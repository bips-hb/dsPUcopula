# usethis::use_package("reticulate", type = "Imports")
#library(desc)
#d <- desc::desc(file = "DESCRIPTION")
#d$set("Config/reticulate",
#      "packages:
#  - package: \"anonymeter\"
#    version: \">=1.0.0\"")
#d$write(file = "DESCRIPTION")

### Server-side functions (dsPUcopulaServer package)

# R/py.R
.get_py_mod <- function() {
  # Try to import lazily; don't install here.
  reticulate::import("anonymeter", delay_load = TRUE)
  reticulate::import("syndat", delay_load = TRUE)
}

#' Set up a dedicated Python env for {yourpkg} (optional)
#' @export
setup_python <- function() {
  # Creates/updates env per Config/reticulate in DESCRIPTION
  reticulate::configure_environment("dsPUcopula")
  invisible(TRUE)
}

# R/wrappers.R
.require_anonymeter <- function() {
  if (!reticulate::py_module_available("anonymeter")) {
    stop(paste(
      "The Python package 'anonymeter' is not available in the active Python env.",
      "Either:",
      "  * run dsPUcopula::setup_python() to auto-create an env, OR",
      "  * set RETICULATE_PYTHON to an env where 'anonymeter' is installed.",
      "Then restart R and try again."
    ), call. = FALSE)
  }
  reticulate::import("anonymeter.evaluators", delay_load = TRUE)
}

.require_syndat <- function() {
  if (!reticulate::py_module_available("syndat")) {
    stop(paste(
      "The Python package 'syndat' is not available in the active Python env.",
      "Either:",
      "  * run dsPUcopula::setup_python() to auto-create an env, OR",
      "  * set RETICULATE_PYTHON to an env where 'syndat' is installed.",
      "Then restart R and try again."
    ), call. = FALSE)
  }
  reticulate::import("syndat", delay_load = TRUE)
}


#' Public R API calling a Python function
#' @param x numeric
#' @export
py_syndat_scores <- function(ori, syn, control=NULL) {
  mod <- .require_syndat()
  # get distribution score

  # temporary workaround because syndat score goes down with to many nas
  oric <- ori
  oric[is.na(oric)] <- -999999
  sync <- syn
  sync[is.na(sync)] <- -999999
  controlc <- control
  controlc[is.na(controlc)] <- -999999

  syndat_scores <- list(
    distribution_ori = mod$distribution(oric, sync), # with recoded nas
    distribution_control = mod$distribution(controlc, sync), # with recoded nas
    discrimination_ori = mod$discrimination(ori, syn),
    discrimination_control = mod$discrimination(control, syn),
    correlation_ori = mod$correlation(ori, syn),
    correlation_control = mod$correlation(control, syn)
  )

  return(syndat_scores)
}


#' Public R API calling a Python function
#' @param x numeric
#' @export
py_anonymeter_SinglingOut <- function(ori, syn, control=NULL, return_evaluator=FALSE) {
  mod <- .require_anonymeter()
  # call e.g. somepy.compute(x)
  #ret <- mod$compute(x)
  #reticulate::py_to_r(ret)
  # get singling out evaluator
  SinglingOut <- mod$SinglingOutEvaluator
  evaluator <- SinglingOut(ori=ori,
                           syn=syn,
                           control=control,
                           n_attacks=as.integer(min(nrow(ori),100L)))
  evaluator$evaluate()
  if (return_evaluator) {
    return(evaluator)
  } else {
    return(evaluator$risk())
  }

}



#' Public R API calling a Python function
#' @param x numeric
#' @export
py_anonymeter_Inference <- function(ori, syn, aux_cols, secret, inference_check_ignore_na=FALSE, control=NULL, return_evaluator=FALSE) {
  mod <- .require_anonymeter()

  if (inference_check_ignore_na) {
    # remove na from ori and control
    rm_ori_id <- which(is.na(ori[[secret]]))
    if (length(rm_ori_id) > 0) ori <- ori[-rm_ori_id,] # without check this would put all data to the trash

    # if na is not removed from control but from ori than we have overestimation of risk
    rm_control_id <- which(is.na(control[[secret]]))
    if (length(rm_control_id) > 0) control <- control[-rm_control_id,]
    #"ValueError: Cannot take a larger sample than population when 'replace=False'\n\033[90mRun \033]8;;rstudio:run:reticulate::py_last_error()\a`reticulate::py_last_error()`\033]8;;\a for details.\033[39m"
    # so maye also:
    #syn <- syn[-rm_ori_id,] #???
  }
  # get inference evaluator
  Inference <- mod$InferenceEvaluator
  evaluator <- Inference(ori=ori,
                           syn=syn,
                           control=control,
                           aux_cols=aux_cols,
                           secret=secret,
                           n_attacks=as.integer(min(nrow(ori),nrow(control),nrow(syn),1000L))) #1000 ,nrow(syn) unnecessary???
  evaluator$evaluate()
  if (return_evaluator) {
    return(evaluator)
  } else {
    return(evaluator$risk())
  }
}


# function to put ranks in bins and smooth them
rank_bin_smooth <- function(x, k) {
  n <- length(x)
  if (sum(!is.na(x)) < k) stop("Not enough non-NA values for chosen k")

  # Compute ranks, keeping NAs in place
  ranks <- rank(x, ties.method = "average", na.last = "keep")

  # Get indices of non-NA ranks
  non_na_idx <- which(!is.na(ranks))
  sorted_non_na_idx <- order(ranks[non_na_idx])
  sorted_indices <- non_na_idx[sorted_non_na_idx]

  # Number of bins
  num_bins <- floor(length(sorted_indices) / k)
  breaks <- floor(seq(1, length(sorted_indices) + 1, length.out = num_bins + 1))

  # Smoothed rank output
  smoothed_ranks <- rep(NA, n)

  for (i in 1:num_bins) {
    start_idx <- breaks[i]
    end_idx <- breaks[i + 1] - 1
    bin_range <- start_idx:end_idx
    bin_indices <- sorted_indices[bin_range]

    avg_rank <- mean(ranks[bin_indices])
    smoothed_ranks[bin_indices] <- avg_rank
  }

  return(smoothed_ranks)
}

# fitPUcopulaDS.R
# fitPUcopulaDS_aggregate_version <- function(data_str, driver_strength_factor =0.5) {
#   if (!requireNamespace("PUcopula", quietly = TRUE)) stop("PUcopula package is required")
#
#   dataTable <- eval(parse(text=data_str), envir = parent.frame())
#
#   driver_strength <- floor(round(nrow(dataTable)*driver_strength_factor))
#   driver_strength <- max(1,driver_strength)
#
#   model <- PUcopula::PUCopula(family="nbinom",
#                               pars.a = driver_strength,
#                               patch="rook",
#                               data=as.matrix(dataTable))
#
#   assign("PU_copula_model", model, envir = parent.frame())
#   return(list(model=model, input=dataTable, class=class(dataTable), length=length(dataTable), dim=dim(dataTable), head=head(dataTable)))
# }
fitPUcopulaDS <- function(data_str, driver_strength_factor = 0.5, bin_size = 3, jitter=FALSE, family="binom") {
  if (!requireNamespace("PUcopula", quietly = TRUE)) stop("PUcopula package is required")

  dataTable <- eval(parse(text=data_str), envir = parent.frame())

  driver_strength <- lapply(driver_strength_factor, function(x) floor(round(nrow(dataTable)*x))) #floor(round(nrow(dataTable)*driver_strength_factor))
  driver_strength <- lapply(driver_strength, function(x) max(1,x) )  #max(1,driver_strength)

  # check for privacy restrictions
  MINIMAL_BIN_SIZE = .get_opt_num("dsPUcopula.minimal_bin_size") # <- this has to be set somewhere in the DS server as option
  #if (bin_size < MINIMAL_BIN_SIZE) stop(paste("The datashield configurations does not allow for bin_size <", MINIMAL_BIN_SIZE))
  if (min(as.numeric(bin_size)) < MINIMAL_BIN_SIZE) stop(paste("The datashield configurations does not allow for bin_size <", MINIMAL_BIN_SIZE))

  #n_bins <- sapply(dataTable, function(x) {
  #  floor(sum(!is.na(x)) / bin_size)
  #})

  # to convert list parameters to vectors, we need...
  varnames <- colnames(dataTable)
  varnames_clean <- sub("(\\.cat\\.[0-9]+|\\.oriname)$", "", varnames)
  varnames_clean_unique <- unique(varnames_clean)

  # if jitter is a list, convert it to vector
  num_jitter <- function(x, factor) {
    return(base::jitter(as.numeric(x), factor=factor))
  }
  if (is.logical(jitter) && jitter==FALSE) {
    #nothing, dataTable unchanged
  } else if (is.numeric(jitter) && length(jitter)==1) {
    dataTable <- lapply(dataTable, num_jitter, factor=jitter)
  } else if (is.list(jitter)) {
    # every list item must be named (unlike for bin_size or driver_strength)
    for (iname in names(jitter)) {
      if (iname %in% varnames_clean) {
        idx <- which(varnames_clean==iname)
        for (id in idx) {
          dataTable[[id]] <- num_jitter(dataTable[[id]], factor=jitter[[iname]])
        }
      }
    }
  }

  # if bin_size is a list, convert it to a vector
  if (is.numeric(bin_size) && length(bin_size)==1) {
    dataTable <- lapply(dataTable, rank_bin_smooth, k=bin_size) |> as.data.frame() |> as.matrix()
    bin_size_list <- NULL
  } else if (is.numeric(bin_size) && length(bin_size)==length(varnames_clean_unique)) {
    bin_size_list <- as.list(bin_size) |> setNames(varnames_clean_unique)

  } else if (is.list(bin_size)) {
    bin_size_list <- bin_size

  } else if (is.numeric(driver_strength) && length(driver_strength)!=length(varnames_clean_unique) && length(driver_strength) != 1) {
    stop("fitPUcopulaDS: driver_strenght must be of length 1 or of the same length as the number of variables in the original data.frame")
  }

  if (!is.null(bin_size_list)) {
    names_in_list <- bin_size_list |> names()
    which_empty <- which(names_in_list=="")

    fill_value <- NA
    if (length(which_empty)>0) fill_value <- bin_size_list[[which_empty]]
    if (length(bin_size_list)==1 && is.null(names(bin_size_list))) fill_value <- bin_size_list[[1]]
    new_bin_size <- rep(fill_value, length(varnames))

    # final driver strength:
    new_bin_size <- bin_size_list[varnames_clean] |> sapply( function(x) if (is.null(x)) fill_value else x) |> as.numeric()
    bin_size <- new_bin_size

    dataTable <- mapply(
      rank_bin_smooth,
      dataTable,
      bin_size,
      SIMPLIFY = FALSE
    ) |>
      as.data.frame() |>
      as.matrix()
  }

  # if driver_strength is a list, convert it to a vector
  if (is.numeric(driver_strength) && length(driver_strength)==1) {
    driver_strength <- as.list(driver_strength) # see if below
  } else if (is.numeric(driver_strength) && length(driver_strength)==length(varnames_clean_unique)) {
    driver_strength_list <- as.list(driver_strength) |> setNames(varnames_clean_unique)
  } else if (is.numeric(driver_strength) && length(driver_strength)!=length(varnames_clean_unique) && length(driver_strength) != 1) {
    stop("fitPUcopulaDS: driver_strenght must be of length 1 or of the same length as the number of variables in the original data.frame")
  }

  if (is.list(driver_strength))
    driver_strength_list <- driver_strength

  names_in_list <- driver_strength_list |> names()
  which_empty <- which(names_in_list=="")

  fill_value <- NA
  if (length(which_empty)>0) fill_value <- driver_strength_list[[which_empty]]
  if (length(driver_strength_list)==1 && is.null(names(driver_strength_list))) fill_value <- driver_strength_list[[1]]
  new_driver_strength <- rep(fill_value, length(varnames))

  # final driver strength:
  driver_strength <- driver_strength_list[varnames_clean] |> sapply( function(x) if (is.null(x)) fill_value else x) |> as.numeric()


  model <- PUcopula::PUCopula(family=family,
                              pars.a = driver_strength,
                              patch="rook", #patch="sample",
                              #patchpar = list(m=n_bins),
                              data=dataTable)

  return(model)
}

save_original_varnamesDS <- function(data_str) {
  dataTable <- eval(parse(text=data_str), envir = parent.frame())

  #assign("D_original_varnames", names(dataTable), envir = parent.frame())

  return(names(dataTable))
}

save_original_classesDS <- function(data_str) {
  dataTable <- eval(parse(text=data_str), envir = parent.frame())

  #assign("D_original_classes", names(dataTable), envir = parent.frame())

  return(sapply(dataTable, class))
}

preprocessDataDS <- function(data_str) {
  dataTable <- eval(parse(text=data_str), envir = parent.frame())

  # Set contrast options globally
  options(contrasts = c("contr.sum","contr.poly"))

  #### UNORDERED FACTORS (.cat.)
  df.unordered <- dplyr::select(
    dataTable,
    tidyselect::where(~ is.factor(.) && !is.ordered(.) && nlevels(.) > 2)
  )
  unordered.names <- names(df.unordered)
  df.unordered <- dplyr::rename_with(df.unordered, ~ paste0(.x, rep(".cat.", length(.x))))
  df.unordered <- dplyr::mutate(df.unordered, dplyr::across(
    .cols = tidyselect::where(is.factor) & tidyselect::matches("\\.cat\\.$"),
    .fns = droplevels
  ))

  #### ORDERED FACTORS (.lev.)
  df.ordered <- dplyr::select(
    dataTable,
    tidyselect::where(~ is.ordered(.) && nlevels(.) > 2)
  )
  ordered.names <- names(df.ordered)
  df.ordered <- dplyr::rename_with(df.ordered, ~ paste0(.x, rep(".lev.", length(.x))))
  df.ordered <- dplyr::mutate(df.ordered, dplyr::across(
    .cols = tidyselect::where(is.ordered) & tidyselect::matches("\\.lev\\.$"),
    .fns = droplevels
  ))


#  # select unordered factor columns with more than 2 levels (no binary variables) and rename
#  df.selected <- dplyr::select(
#    dataTable,
#    tidyselect::where(~ is.factor(.) && !is.ordered(.) && nlevels(.)>2) #&& length(levels(.))>2)
#  )
#
#  df.selected.names <- names(df.selected)  #this names(dmy$lvls) will not do it, because we need the current name withut ".cat."
#
#  df.cat <- dplyr::rename_with(
#    df.selected,
#    ~ paste0(.,".cat.")
#  )
#
#  # remove levels that are unused?
#  df.cat <- dplyr::mutate(df.cat, across(
#      .cols = tidyselect::where(is.factor) & tidyselect::matches("\\.cat\\.$"),
#      .fns  = droplevels
#    ))

  #### Combine for dummy variable creation
  df.catlev <- dplyr::bind_cols(df.unordered, df.ordered)

  dmy <- caret::dummyVars(~.,
                          data = dplyr::select(df.catlev,
                                               tidyselect::where(~ !(is.factor(.) && nlevels(.) <= 2))),
                          sep = "",
                          fullRank = TRUE)

  df.dmy <- data.frame(predict(dmy, newdata = df.catlev))
  df.dmy[] <- lapply(df.dmy, function(x) as.factor(as.integer(x)))


#  # create dummy variables ###(as factor)
#  dmy <- caret::dummyVars(~.,
#                   data = dplyr::select(
#                       df.cat,
#                       # keep everything that is NOT a 1-level factor (will they be lost then?? is this not obsolete??)
#                       tidyselect::where(~ !(is.factor(.) && nlevels(.) <= 2))
#                       ),
#                   sep = "", # not reliable parameter. for factor with levels "0","1" it works, but with levels "e[]?öins"      "z~#...#wößi\"" it is always ""
#                   fullRank = TRUE)
#
#  df.dmy <- data.frame(predict(dmy, newdata = df.cat))
#  df.dmy[] <- lapply(df.dmy, function(x) as.factor(as.integer(x))) # careful when there is somethin else then contr.sum!!! for contr.poly this does not work

  #### Remove original selected variables from main data
  dataTable <- dataTable[, !(names(dataTable) %in% c(unordered.names, ordered.names))]

  #### Track levels and rename other factor variables with ".oriname"
  oriname_levels <- list()
  dataTable <- dataTable |>
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::where(is.factor),
      .fns = ~ {
        oriname_levels[[cur_column()]] <<- levels(.)
        return(.) # you may change to: as.integer(.) - 1 if needed
      }
    )) |>
    dplyr::rename_with(~ paste0(.x, ".oriname"))

#  #
#  dataTable <- dataTable[, !(names(dataTable) %in% df.selected.names)]
#
#  ## put also suffix to the other columns, to make sure no doublettes happen
#  #dataTable <- dplyr::rename_with(
#  #  dataTable, ~ paste0(.,".oriname") )
#  # Initialize a list to collect original levels
#  oriname_levels <- list()
#  # Convert factor columns to numeric (starting from 0), store levels
#  dataTable <- dataTable |>
#    dplyr::mutate(dplyr::across(
#      .cols = tidyselect::where(is.factor),
#      .fns = ~ {
#        oriname_levels[[cur_column()]] <<- levels(.)
#        return(.) #as.integer(.) - 1 # do keep it a factor??
#      }
#    )) |>
#    dplyr::rename_with(~ paste0(.,".oriname"))

  #### Final return
  return(list(
    data = cbind(dataTable, df.dmy),
    original_levels = list(
      dummies = dmy$lvls,
      oriname = oriname_levels
    )
  ))
}

find_closest_cat <- function(..., contrast_tab) {
  vec <- c(...)
  dists <- apply(contrast_tab, 1, function(row) sum((vec - row)^2))
  rs <- names(which.min(dists))
  if (is.null(rs)) rs <- NA
  rs
}

postprocessDataDS <- function(data_str , cat_dummy_levels_str) { # data_str
  dataTable <- eval(parse(text=data_str), envir = parent.frame())
  cat_dummy_levels <- eval(parse(text=cat_dummy_levels_str), envir = parent.frame())

  # Select the relevant dummy columns for each varname
  dummy_cols <- lapply( names(cat_dummy_levels), function(x) { names(dataTable)[startsWith(names(dataTable), x)] } )
  dummy_cols <- setNames(dummy_cols, names(cat_dummy_levels))

  n_dummies <- sapply(dummy_cols, function(x) as.integer(length(x)+1))

  # integers as levels
#  cat_levels <- lapply(n_dummies, function(x) { 1:x } )
  # levels from cat_dummy_levels
  cat_levels <- cat_dummy_levels   #dumb

  ordinal = !endsWith(names(dummy_cols), ".cat.") # if ends with ".cat." then categorical unordered, otherwise ordered

  contrast_cat_tab <- mapply(function(n_dum, is_ord) {if (is_ord) contr.poly(n_dum) else contr.sum(n_dum) }, n_dummies, as.list(ordinal), SIMPLIFY=F)

  contrast_cat_tab <- mapply( function(tab, nam) { rownames(tab) <- nam; return(tab); }, contrast_cat_tab, cat_levels, SIMPLIFY=F )

  new_cols <- mapply( function(cols, contrasts) {
    apply(dataTable[, cols, drop=F], 1, function(row) find_closest_cat(as.numeric(row), contrast_tab=contrasts))
  }, dummy_cols, contrast_cat_tab, SIMPLIFY=F) %>%
    as.data.frame() %>% setNames(names(cat_dummy_levels))

  new_cols <- mapply( factor , new_cols, cat_levels, SIMPLIFY=F) %>% as.data.frame()
  newdf <- dataTable %>% select(-all_of(unlist(dummy_cols)))
  newdf <- newdf %>% bind_cols(new_cols)
#  newdf <- lapply(newdf)
  # remove suffixes
  names(newdf) <- gsub("\\.(oriname|cat\\.)$", "", names(newdf))
  # order as original data
  ordered_names <- unique( gsub("\\.(oriname|cat\\.\\d+)$" ,"", colnames(dataTable)) )
  # order newdf by order given by character vector ordered_names
  newdf <- newdf[, match(ordered_names, names(newdf))]
  newdf
}


check_if_integer <- function(x) {
  is.numeric(x) && all(x %% 1 == 0)
}

check_if_binary <- function(x, ignoreNA=T) {
  if (ignoreNA)
    length(unique(na.omit(x))) == 2
  else
    length(unique(x)) == 2
}

check_if_trivial <- function(x) {
  length(unique(x)) < 2
}

# knn smoothing as in histogramDS1
knnsmoother <- function(x, k=3) {
  ##################################################################
  # CAPTURE THE nfilter SETTINGS                         #
  thr <- dsBase::listDisclosureSettingsDS()                        #
  nfilter.tab <- as.numeric(thr$nfilter.tab)                       #
  #nfilter.glm <- as.numeric(thr$nfilter.glm)                      #
  #nfilter.subset <- as.numeric(thr$nfilter.subset)                #
  #nfilter.string <- as.numeric(thr$nfilter.string)                #
  #nfilter.stringShort <- as.numeric(thr$nfilter.stringShort)      #
  nfilter.kNN <- as.numeric(thr$nfilter.kNN)                       #
  nfilter.noise <- as.numeric(thr$nfilter.noise)                   #
  nfilter.levels.density <- as.numeric(thr$nfilter.levels.density) #
  nfilter.levels.max <- as.numeric(thr$nfilter.levels.max)         #
  ##################################################################

  # remove missing values
  x <- na.omit(x)
  # standardise
  x.standardised <- (x-mean(x))/sd(x)

  # Calculate the length of the variable after ommitting any NAs
  N.data <- length(x)

  # Check if k >= the pre-specified threshold
  # and <= the length of rows of data.complete minus the pre-specified threshold
  if(k < nfilter.kNN | k > (N.data - nfilter.kNN)){
    stop(paste0("k must be greater than or equal to ", nfilter.kNN, " and less than or equal to ", (N.data-nfilter.kNN), "."), call.=FALSE)
  } else {
    neighbours = k
  }

  # to handle vector with only 0s
  if (all(x == x[1], na.rm = TRUE)) return(x)

  # Find the k-1 nearest neighbours of each data point
  nearest <- RANN::nn2(x.standardised, k = neighbours)

  # Calculate the centroid of each n nearest data points
  x.centroid <- matrix()
  for (i in 1:N.data){
    x.centroid[i] <- mean(x.standardised[nearest$nn.idx[i,1:neighbours]])
  }

  # Calculate the scaling factor
  x.scalingFactor <- stats::sd(x.standardised)/stats::sd(x.centroid)

  # Apply the scaling factor to the centroids
  x.masked <- x.centroid * x.scalingFactor

  # Shift the centroids back to the actual position and scale of the original data
  x.new <- (x.masked * stats::sd(x)) + mean(x)

  return(x.new)
}

# estimateMarginalsDS.R
estimateMarginalsDS <- function(data_str, method = "spline", k=3) {
  if (!requireNamespace("logspline", quietly = TRUE)) stop("logspline package is required")

  # checks
  # - check for k >= ???

  # method can be specified for numeric and factor variables
  # if only one method defined,use it for both, otherwise method 1 is for numeric, 2 for factor
  method <- rep_len(method, 2)

  #dataTable <- get(x, envir = parent.frame())
  dataTable <- eval(parse(text=data_str), envir = parent.frame())

  # to convert list parameters to vectors, we need...
  varnames <- colnames(dataTable)
  varnames_clean <- sub("(\\.cat\\.[0-9]+|\\.oriname)$", "", varnames)
  varnames_clean_unique <- unique(varnames_clean)
  # if k is a list, convert it to a vector
  if (is.numeric(k) && length(k)==1) {
    dataTable <- lapply(dataTable,function(x,k) if (is.numeric(x)) knnsmoother(x) else x, k=k ) #apply(dataTable,2,knnsmoother, k=k)
    k_list <- NULL
  } else if (is.numeric(k) && length(k)==length(varnames_clean_unique)) {
    k_list <- as.list(k) |> setNames(varnames_clean_unique)
  } else if (is.list(k)) {
    k_list <- k
  } else if (is.numeric(k) && length(k)!=length(varnames_clean_unique) && length(k) != 1) {
    stop("estimateMarginalsDS: k must be of length 1 or of the same length as the number of variables in the original data.frame or a list")
  }
  if (!is.null(k_list)) {
    names_in_list <- k_list |> names()
    which_empty <- which(names_in_list=="")

    fill_value <- NA
    if (length(which_empty)>0) fill_value <- k_list[[which_empty]]
    if (length(k_list)==1 && is.null(names(k_list))) fill_value <- k_list[[1]]
    new_k <- rep(fill_value, length(varnames))

    # final k vector:
    new_k <- k_list[varnames_clean] |> sapply( function(x) if (is.null(x)) fill_value else x) |> as.numeric()
    k <- new_k

    dataTable <- mapply(
      function(x,k) if (is.numeric(x)) {
        vec_na <- rep(NA, length(x))
        non_na_idx <- which(!is.na(x))
        new_vec <- vec_na
        new_vec[non_na_idx] <- knnsmoother(x)
        return(new_vec)
        } else x, #rank_bin_smooth,
      dataTable,
      k,
      SIMPLIFY = FALSE
    ) |>
      as.data.frame()
  }

  marginals <- mapply(function(col, varname) {
    #if (method == "ecdf") {
    ##  return(stats::ecdf(col))
    #} else if (method == "normal") {
    #  mu <- mean(col); sigma <- sd(col)
    #  return(list(qfun = function(p) stats::qnorm(p, mean = mu, sd = sigma)))
    #} else
    if (method[1] == "spline") {
      mod <- if (check_if_trivial(col)) {
             # message(paste("trivial variable",varname,"cannot be directly fit with logspline"))
              message(paste(varname,"(trivial variable) will be fitted with prop.table"))
              prop.table(table(col))
            } else if (check_if_binary(col)) {
             # message(paste("binary variable",varname,"cannot be directly fit with logspline"))
              message(paste(varname,"(binary) will be fitted with prop.table"))
              prop.table(table(col))
            } else if (is.factor(col)) {
              #message(paste(varname,"hopefully ok")); logspline::logspline(as.numeric(col), maxknots=5) ##????? why 5?
              if (method[2] == "spline") {
                message(paste(varname," (ordered category) will be fitted with logspline"))
                tryCatch(
                  logspline::logspline(as.numeric(as.character(col))),
                  error = function(e) {
                    stop(sprintf("logspline failed for variable '%s': %s", varname, conditionMessage(e)), call. = FALSE)
                  }
                )
              } else {
                message(paste(varname," (ordered category) will be fitted with prop.table"))
                prop.table(table(col))
              }
            }   else {
              message(paste(varname,"will be fitted with logspline"))
              tryCatch(
                logspline::logspline(col),
                error = function(e) {
                  stop(sprintf("logspline failed for variable '%s': %s", varname, conditionMessage(e)), call. = FALSE)
                }
              )
            }
      return(list(qfun = mod))
    } else {
      stop("Unsupported marginal estimation method. For numeric variables only 'spline is supported currently")
    }
  },
  dataTable,
  names(dataTable))

  names(marginals) <- names(dataTable)
  return(marginals)
  #assign("marginals_list", marginals, envir = parent.frame())
  #return(invisible(TRUE))
}

# simulateCopulaDS.R
simulateCopulaDS <- function(n) {
  copula <- get("PU_copula_model", envir = parent.frame())
  u <- copula@rand(n)

  assign("PU_copula_model_u_sims", u, envir = parent.frame())

  return(u) #??
}

# u: vector in [0,1]
# x: sorted target values (length n >= 2), e.g. c(-1, 0, 1)
# p: cumulative probabilities at those x "breaks" (length n), e.g. c(0.05458908, 0.88482304, 1)
#     Interpretation: the i-th p is the u where we want to land at the midpoint between x[i] and x[i+1].
transform_u <- function(u, x, p) {
  stopifnot(length(x) >= 2, length(p) == length(x))
  if (is.unsorted(x, strictly = TRUE)) stop("x must be strictly increasing")
  if (any(p < 0 | p > 1) || is.unsorted(p, strictly = TRUE)) stop("p must be strictly increasing in [0,1]")

  n <- length(x)
  gaps <- diff(x)
  if (any(gaps <= 0)) stop("x must be strictly increasing")

  # knots in u-space: start at 0, then p[1..n-1], then 1
  pk <- c(0, p[1:(n-1)], 1)
  # corresponding y-values: start at left-half extension, then midpoints, then right-half extension
  mids <- x[-n] + gaps/2
  yk <- c(x[1] - 0.5 * gaps[1], mids, x[n] + 0.5 * gaps[n-1])

  # linear interpolation
  approx(pk, yk, xout = u, method = "linear", ties = "ordered", rule = 2)$y
}

.get_opt_num <- function(name, default) {
  val <- getOption(name, getOption(paste0("default.", name), default))
  as.numeric(val)
}

.get_opt_keys <- function(name, default_chr = "sex,age,region,placesize") {
  raw <- getOption(name, getOption(paste0("default.", name), default_chr))
  keys <- trimws(unlist(strsplit(raw, "[,\\s]+", perl = TRUE))) # accept both whitespace and comma#strsplit(raw, " ")))
  keys[nzchar(keys)]
}

.get_opt_str <- function(name, default) {
  raw <- getOption(name, getOption(paste0("default.", name), default))
  tolower(trimws(as.character(raw)))
}

# generateSyntheticDS.R
generateSyntheticDS <- function(n="n_rSynthetic",
                                copula_str="PU_copula_model",
                                marginals_str="marginal_models",
                                training_data="D_ori",
                                control_data="D_control",
                                singling_out_check=TRUE,
                                inference_check=TRUE,
                                inference_check_ignore_na=FALSE,
                                syndat_scores=TRUE,
                                return_scores=FALSE) {

  ## n might be already assigned to an expression
  if (is.character(n))
    n <- get(n, envir = parent.frame())

  if (n==0) return(NULL)

  # get the models
  copula <- get(copula_str, envir = parent.frame())
  marginals <- get(marginals_str, envir = parent.frame())

  u <- copula@rand(n)

  df <- as.data.frame(matrix(nrow = n, ncol = length(marginals)))
  names(df) <- names(marginals)

  for (j in seq_along(marginals)) {
    m <- marginals[[j]]
    if (inherits(m, "ecdf")) {
      # this is untested gpt code....
      xs <- environment(m)$x
      ps <- m(xs)
      df[[j]] <- approx(ps, xs, xout = u[, j], ties = "ordered")$y
    } else if (class(m) %in% c("logspline")) {
      #main code branch
      message(paste("logspline for",names(marginals)[j]))
      df[[j]] <- logspline::qlogspline(u[, j], m)
    } else if (class(m) %in% c("oldlogspline")) {
      message(paste("oldlogspline for",names(marginals)[j]))
      df[[j]] <- logspline::qoldlogspline(u[, j], m)
    } else if (class(m) %in% c("table")) {

      message(paste("discrete probability table for",names(marginals)[j]))
      print(m)

      vbreaks <- cumsum(m)

      # this should be better documented
      if (length(vbreaks)<=2) {
        df[[j]] <- (findInterval(u[, j], vbreaks)+1) %>% as.factor() #as.ordered() # why did we put as ordered instead as.factor here??
        levels(df[[j]]) <- names(m)
      } else {
        df[[j]] <- transform_u(u[,j], as.numeric(names(vbreaks)), vbreaks)
        levels(df[[j]]) <- names(m)
      }
    } else {
      print(class(m))
      print(m)
      message(paste("Unsupported marginal type for",names(marginals)[j]))

      df[[j]] <- u[, j]
    }
  }

  if (exists("is_preprocessed", envir = parent.frame())) {
    is_preprocessed <- get("is_preprocessed", envir = parent.frame())
    if (exists("D_preprocessed", envir = parent.frame()))
      D_preprocessed <- get("D_preprocessed", envir = parent.frame())
  }

  #warning(paste("is_preprocessed existence:", exists("is_preprocessed") ))
  if (exists("is_preprocessed") && is_preprocessed) {
    #warning(paste("is_preprocessed:", is_preprocessed) )
   # assign("D_synthetic_raw", as.data.frame(df), envir=parent.frame())
    assign("D_synthetic_raw", as.data.frame(df)) # current environment
    df <- postprocessDataDS("D_synthetic_raw", "D_preprocessed$original_levels$dummies")
  }

  if (exists("D_original_varnames", envir = parent.frame())) {
    D_original_varnames <- get("D_original_varnames", envir = parent.frame())
    df <- df[D_original_varnames]
  }

  if (exists("D_original_classes", envir = parent.frame())) {
    D_original_classes <- get("D_original_classes", envir = parent.frame())
    df[D_original_classes == "integer"] <- as.data.frame(lapply(round(df[D_original_classes == "integer"]),as.integer) )
  }

  if (!exists(training_data, envir = parent.frame()))
    stop(paste0("training data object '",training_data,"' for not found"))
  if (!exists(control_data, envir = parent.frame()))
    stop(paste0("control data object '",control_data,"' for not found"))

  D_control <- get(control_data, envir = parent.frame())
  D_ori <- get(training_data, envir = parent.frame())

  syndat_scores <- py_syndat_scores(D_ori, df, D_control)
  score_list <- list(quality=syndat_scores, privacy=list(), anonymeter=list())

  if (singling_out_check) {

    so_max  <- .get_opt_num("anonymeter.singlingOut.maxRisk", 0.05)
    so_mode <- .get_opt_str("anonymeter.singlingOut.mode")

    # experimental way. check if value for risk of df (synthetic data) for D_ori
    # is in the confidence interval for the risk of df for D_control (where de control are data that have not been used for fitting the model)
    if (F) {
      so_risk <- py_anonymeter_SinglingOut(D_ori, df)
      so_dummy_risk <- py_anonymeter_SinglingOut(D_control, df)

      message(paste( "singling out risk estimate:",  so_risk$value - so_dummy_risk$value ))

      if (so_risk$value - so_dummy_risk$value > so_max && so_risk$value > so_dummy_risk$ci[[2]]) {
        warning("significant singling out risk recognized")
      }
    } else {
      so_control_risk <- py_anonymeter_SinglingOut(D_ori, df, return_evaluator=T, control=D_control)

      score_list$anonymeter$SinglingOutRisk <- so_control_risk

      score_list$privacy$SinglingOutRisk <- list(
        risk = score_list$anonymeter$SinglingOutRisk$risk()$value,
        ci = unlist( score_list$anonymeter$SinglingOutRisk$risk()$ci )
      )
      #score_list$privacy$SinglingOutRisk <- so_control_risk$risk()#$value
      SinglingOutRisk <- so_control_risk$risk()$value
      message(paste( "singling out risk estimate:",  SinglingOutRisk))

      # error: (so_control_risk$risk()$ci[[2]] - so_control_risk$risk()$value)/1.96
      # 0.077

      attack_rate <- so_control_risk$results()$attack_rate
      control_rate <- so_control_risk$results()$control_rate

      returnDummyRisk=T
      if (returnDummyRisk) {
        score_list$privacy$SinglingOutDummyRisk <- list(
          risk = control_rate$value,
          error = control_rate$error)
      }

      if (SinglingOutRisk > so_max &&  # conservative alternative: so_control_risk$risk()$ci[[2]] > so_max
          attack_rate$value > control_rate$value + control_rate$error) { # attack rate > uci control
        warning("significant singling out risk recognized")
      }
    }
  }

  if (inference_check) {
    inf_max  <- .get_opt_num("anonymeter.inference.maxRisk", 0.05)
    inf_mode <- .get_opt_str("anonymeter.inference.mode")
    keys     <- .get_opt_keys("anonymeter.inference.keys")
    secrets   <- .get_opt_keys("anonymeter.inference.secrets")

    # validate keys exist in the server-side data
    missing_keys <- setdiff(keys, colnames(df))
    if (length(missing_keys)) {
      warning(sprintf("Configured key variables not found in '%s': %s",
                   dataSymbol, paste(missing_keys, collapse=", ")))
    }
    if (length(secrets)==0 || secrets=="") {
      secrets <- setdiff(colnames(df),keys)
    }

    if (TRUE) { #the standard way
      inf_risk_list <- list()
      for (secret_i in secrets) {
        inf_risk_list <- c(inf_risk_list,
                           py_anonymeter_Inference(D_ori, df, aux_cols=keys, return_evaluator=T, secret=secret_i, inference_check_ignore_na=inference_check_ignore_na, D_control)
        )
      }
      names(inf_risk_list) <- secrets
      score_list$anonymeter$InferenceRisksIndividual <- inf_risk_list
#      score_list$privacy$InferenceRisksIndividual <- inf_risk_list
      score_list$privacy$InferenceRisksIndividual <- lapply(inf_risk_list,
                                                            function(x) {
                                                              list(
                                                                risk = x$risk()$value,
                                                                ci   = unlist(x$risk()$ci))
                                                            })

      inferenceRiskValues <- sapply(inf_risk_list, function(x) x$risk()$value)
      inferenceRiskUCLs <-   sapply(inf_risk_list, function(x) x$risk()$ci[[2]])
      maxInfRisk <- inferenceRiskValues |> which.max() #index of highest risk
      message(paste0( "highest inference risk estimate for ", names(maxInfRisk),": ",  inferenceRiskValues[maxInfRisk]))

      # make this compatible with lists
      attack_rate <- lapply(inf_risk_list, function(x) x$results()$attack_rate) #so_control_risk$results()$attack_rate
      attack_rate_values <- sapply(attack_rate, function(x) x$value)
      control_rate <- lapply(inf_risk_list, function(x) x$results()$control_rate) #so_control_risk$results()$control_rate
      control_rate_values <- sapply(control_rate, function(x) x$value)
      control_rate_errors <- sapply(control_rate, function(x) x$error)
    #  names(attack_rate) <- names(control_rate) <- secrets

      #returnDummyRisk=T
      if (returnDummyRisk) {
      score_list$privacy$InferenceDummyRisksIndividual <- lapply(control_rate,
                                                                 function(x) {
                                                                   list(
                                                                     risk = x$value,
                                                                     error = x$error)
                                                                 }) # e.g.: SuccessRate(value=0.5933191326858227, error=0.033298218276823735)
      }
      ifnRiskSignificance <- inferenceRiskValues > inf_max &  # conservative alternative: inferenceRiskUCLs > so_max
        attack_rate_values > control_rate_values + control_rate_errors
      if (any(ifnRiskSignificance)) { # attack rate > ucl control
        warning(paste("significant inference risk recognized for", paste0(names(which(ifnRiskSignificance)), collapse=", ")))
      }

    }
  }

  if (return_scores) {
    return(list(synthetic=df, scores=score_list))
  } else {
    return(df)
  }
}
