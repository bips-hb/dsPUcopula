### Server-side functions (dsPUcopulaServer package)

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
fitPUcopulaDS <- function(data_str, driver_strength_factor = 0.5, bin_size = 3) {
  if (!requireNamespace("PUcopula", quietly = TRUE)) stop("PUcopula package is required")

  dataTable <- eval(parse(text=data_str), envir = parent.frame())

  driver_strength <- floor(round(nrow(dataTable)*driver_strength_factor))
  driver_strength <- max(1,driver_strength)

  # check for privacy restrictions
  MINIMAL_BIN_SIZE = 3 # <- this has to be set somewhere in the DS server as option
  if (bin_size < MINIMAL_BIN_SIZE) stop(paste("The datashield configurations does not allow for bin_size <", MINIMAL_BIN_SIZE))

  n_bins <- sapply(dataTable, function(x) {
    floor(sum(!is.na(x)) / bin_size)
  })

  model <- PUcopula::PUCopula(family="nbinom",
                              pars.a = driver_strength,
                              patch="sample", patchpar = list(m=n_bins),
                              data=as.matrix(dataTable))

  return(model)
}

preprocessDataDS <- function(data_str) {
  dataTable <- eval(parse(text=data_str), envir = parent.frame())

  options(contrasts = c("contr.sum","contr.poly"))

  # select unordered factor columns with more than 2 levels (no binary variables) and rename
  df.selected <- dplyr::select(
    dataTable,
    tidyselect::where(~ is.factor(.) && !is.ordered(.) && nlevels(.)>2) #&& length(levels(.))>2)
  )
  #df.selected.names <- names(dmy$lvls) #this will not do it, because we need the current name withut ".cat."
  df.selected.names <- names(df.selected)
  #df.selected.levels <- lapply(df.selected.names, function(x) {
  #  levs <- levels(df.selected[[x]])
  #  if (is.null(levs)) levs <- character(0)
  #  return(levs)
  #  })
 # df.selected.levels <- setNames(df.selected.levels, df.selected.names)
  df.cat <- dplyr::rename_with(
    df.selected,
    ~ paste0(.,".cat.")
  )

  # remove levels that are unused?
  df.cat <- dplyr::mutate(df.cat, across(
      .cols = tidyselect::where(is.factor) & tidyselect::matches("\\.cat\\.$"),
      .fns  = droplevels
    ))

  # create dummy variables as factor
  dmy <- caret::dummyVars(~.,
                   data = dplyr::select(
                       df.cat,
                       # keep everything that is NOT a 1-level factor (will they be lost then?? is this not obsolete??)
                       tidyselect::where(~ !(is.factor(.) && nlevels(.) <= 2))
                       ),
                   sep = "", # not reliable parameter. for factor with levels "0","1" it works, but with levels "e[]?öins"      "z~#...#wößi\"" it is always ""
                   fullRank = TRUE)
  df.dmy <- data.frame(predict(dmy, newdata = df.cat))
  #df.dmy[] <- lapply(df.dmy, function(x) factor(ifelse(x == 1, 1, 0), levels = c(0, 1))) # can also have negative values or other values
  df.dmy[] <- lapply(df.dmy, function(x) as.factor(as.integer(x))) # careful when there is somethin else then contr.sum!!! for contr.poly this does not work

  #
  dataTable <- dataTable[, !(names(dataTable) %in% df.selected.names)]

  # put also suffix to the other columns, to make sure no doublettes happen
  dataTable <- dplyr::rename_with(
    dataTable, ~ paste0(.,".oriname") )


  return(list(
    data = cbind(dataTable, df.dmy),
    original_levels = dmy$lvls #df.selected.levels
    ))
}

find_closest_cat <- function(..., contrast_tab) {
  vec <- c(...)
  dists <- apply(contrast_tab, 1, function(row) sum((vec - row)^2))
  rs <- names(which.min(dists))
  if (is.null(rs)) rs <- NA
  rs
}

postprocessDataDS <- function(data_str, cat_dummy_levels_str) {
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

  ordinal = F # has to be replaced

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
  newdf
}


check_if_integer <- function(x) {
  is.numeric(x) && all(x %% 1 == 0)
}

check_if_binary <- function(x) {
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

  #dataTable <- get(x, envir = parent.frame())
  dataTable <- eval(parse(text=data_str), envir = parent.frame())

  #dataTable <- as.data.frame(lapply(dataTable, function(x) if (is.factor(x)) as.numeric(as.character(x)) else x)) #apply(dataTable,2,function(x) as.numeric(as.character(x)))
  #dataTable <- lapply(dataTable,knnsmoother, k=k ) #apply(dataTable,2,knnsmoother, k=k)
  dataTable <- lapply(dataTable,function(x,k) if (is.numeric(x)) knnsmoother(x) else x, k=k ) #apply(dataTable,2,knnsmoother, k=k)

  marginals <- mapply(function(col, varname) {
    #if (method == "ecdf") {
    ##  return(stats::ecdf(col))
    #} else if (method == "normal") {
    #  mu <- mean(col); sigma <- sd(col)
    #  return(list(qfun = function(p) stats::qnorm(p, mean = mu, sd = sigma)))
    #} else
    if (method == "spline") {
      mod <- if (check_if_trivial(col)) {
              message(paste("trivial variable",varname,"cannot be directly fit with logspline"))
              prop.table(table(col))
            } else if (check_if_binary(col)) {
              message(paste("binary variable",varname,"cannot be directly fit with logspline"))
              prop.table(table(col))
            } else if (is.factor(col)) {
              #message(paste(varname,"hopefully ok")); logspline::logspline(as.numeric(col), maxknots=5) ##????? why 5?
              message(paste(varname,"ordered category")); prop.table(table(col))
            }   else {
              message(paste(varname,"will be fitted with logspline")); logspline::logspline(col)
            }
      return(list(qfun = mod))
    } else {
      stop("Unsupported marginal estimation method")
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

# generateSyntheticDS.R
generateSyntheticDS <- function(n="n_rSynthetic",
                                copula_str="PU_copula_model",
                                marginals_str="marginals_model") {

  ## n might be already assigned to an expression
  #if (is.null(n)) {
  #  n <- get("n_rSynthetic", envir = parent.frame())
  #}
  if (is.character(n))
    n <- get(n, envir = parent.frame())

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

      df[[j]] <- (findInterval(u[, j], vbreaks)+1) %>% as.ordered()
      levels(df[[j]]) <- names(m)
    } else {
      print(class(m))
      print(m)
      message(paste("Unsupported marginal type for",names(marginals)[j]))

      df[[j]] <- u[, j]
    }
  }

  return(df)
}
