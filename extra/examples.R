#https://isglobal-brge.github.io/resource_bookdown/package-info.html


### example
if (T) {
require('DSI')
require('DSOpal')
require('dsBaseClient')
library(dsPUcopula)
#library(dsPUcopulaClient)
require(dplyr)

require(DSLite)
logindata <- setupCNSIMTest()
conns <- datashield.login(logindata, assign=TRUE)
# retrieve symbol D value from a specific DataSHIELD connection
ori_data <- getDSLiteData(conns$sim1, "D")

#DSI::datashield.aggregate(conns, as.symbol("meanDS(D$LAB_TSC)"))

#dslite.server$aggregateMethods()
#dslite.server$assignMethods()
}

#dslite.server$assignMethod("fitPUcopulaDS","dsPUcopula::fitPUcopulaDS")
#DSI::datashield.assign.expr(conns[[1]], "copula_model", call("fitPUcopulaDS", "D"))
datashield.errors()

# fit copula: fitPUcopulaDS
#dslite.server$aggregateMethod("fitPUcopulaDS_aggregate_version","dsPUcopula::fitPUcopulaDS_aggregate_version")
#foo <- DSI::datashield.aggregate(conns, as.symbol("fitPUcopulaDS_aggregate_version('D')"))
#foo <- DSI::datashield.aggregate(conns, expr=quote(fitPUcopulaDS_aggregate_version("D")))
#foo <- DSI::datashield.aggregate(conns, expr=call('fitPUcopulaDS_aggregate_version','D',0.5))
#foo
dslite.server$assignMethod("fitPUcopulaDS","dsPUcopula::fitPUcopulaDS")
DSI::datashield.assign.expr(conns, "PU_copula_model", as.symbol("fitPUcopulaDS('D', bin_size = 100)"))

# simulate from copula: simulateCopulaDS
dslite.server$aggregateMethod("simulateCopulaDS","dsPUcopula::simulateCopulaDS")
dslite.server$assignMethod("simulateCopulaDS","dsPUcopula::simulateCopulaDS")
rsdf <- DSI::datashield.aggregate(conns, as.symbol("simulateCopulaDS(1000)")) %>% as.data.frame()
plot(rsdf[,1:3])
#dsPUcopulaClient::ds.fitPUcopula("D")


# fit marginals
dslite.server$assignMethod("estimateMarginalsDS","dsPUcopula::estimateMarginalsDS")
DSI::datashield.assign.expr(conns, "marginals_model", as.symbol("estimateMarginalsDS('D')"))

#testD <- getDSLiteData(conns$sim1, "D")
#estimateMarginalsDS('testD')

# simulate synthetic
#set nr of simulations
n_sims <- 100
nrow_by_server <- DSI::datashield.aggregate(conns, as.symbol("dimDS('D')")) %>% sapply(function(x) x[1])
#DSI::datashield.aggregate(conns, as.symbol("lengthDS('D')"))
current_random_state <- .Random.seed
n_servers <- length(conns)
#for (i in 1:length(conns)) DSI::datashield.assign.expr(conns[[i]], "connection_nr", as.symbol(i))
if (n_servers>0) {
  # randomly choose a server for each simulation
  obs_server <- sample(x = 1:n_servers, size = n_sims, replace=T, prob = nrow_by_server) %>%
    factor(levels=1:n_servers) # to keep information for servers with 0 counts
  # total number of simulations per server
  n_obs_server <- table(obs_server) # will then have a "0" count for unused levels (servers)

  # assign number of required simulations on each server (instead of using the same n for all servers)
  for (i in 1:n_servers) DSI::datashield.assign.expr(conns[[i]], "n_rSynthetic", as.symbol(n_obs_server[i]))
}

dslite.server$aggregateMethod("generateSyntheticDS","dsPUcopula::generateSyntheticDS")
syn_data <- DSI::datashield.aggregate(conns, as.symbol("generateSyntheticDS()"))

# works now for
# - continuous
# - ordered factors or binary
# but not yet for:
# - unordered
# => need to create dummies in these cases like in our sythetic data

dslite.server$aggregateMethod("preprocessDataDS","dsPUcopula::preprocessDataDS")
DSI::datashield.aggregate(conns, as.symbol("preprocessDataDS('D')"))

# example pipeline using preprocess
# 1 preprocess - make dummies
dslite.server$assignMethod("preprocessDataDS","dsPUcopula::preprocessDataDS")
DSI::datashield.assign.expr(conns, "D_preprocessed", as.symbol("preprocessDataDS('D')"))
D <- getDSLiteData(conns$sim1, "D")
D_preprocessed <- getDSLiteData(conns$sim1, "D_preprocessed")
preprocessDataDS('D')
# 2 fit copula
dslite.server$assignMethod("fitPUcopulaDS","dsPUcopula::fitPUcopulaDS")
DSI::datashield.assign.expr(conns, "PU_copula_model", as.symbol("fitPUcopulaDS('D_preprocessed$data')"))
PU_copula_model <- getDSLiteData(conns$sim1, "PU_copula_model")
# 3 fit marginals
dslite.server$assignMethod("estimateMarginalsDS","dsPUcopula::estimateMarginalsDS")
DSI::datashield.assign.expr(conns, "marginals_model", as.symbol("estimateMarginalsDS('D_preprocessed$data')"))
marginals_model <- getDSLiteData(conns$sim1, "marginals_model")
estimateMarginalsDS('D_preprocessed$data')
## test becuase marginal names are los
D_preprocessed <- getDSLiteData(conns$sim1, "D_preprocessed")
estimateMarginalsDS('D_preprocessed$data')
# 4 simulate synthetic
#set nr of simulations
n_sims <- 1000
nrow_by_server <- DSI::datashield.aggregate(conns, as.symbol("dimDS('D')")) %>% sapply(function(x) x[1])
n_servers <- length(conns)
if (n_servers>0) {
  # randomly (with probabilities nrow_by_server) choose a server for each simulation
  obs_server <- sample(x = 1:n_servers, size = n_sims, replace=T, prob = nrow_by_server) %>%
    factor(levels=1:n_servers) # to keep information for servers with 0 counts
  # total number of simulations per server
  n_obs_server <- table(obs_server) # will then have a "0" count for unused levels (servers)
  # assign number of required simulations on each server (instead of using the same n for all servers)
  for (i in 1:n_servers) DSI::datashield.assign.expr(conns[[i]], "n_rSynthetic", as.symbol(n_obs_server[i]))
}
dslite.server$aggregateMethod("generateSyntheticDS","dsPUcopula::generateSyntheticDS")
syn_data <- DSI::datashield.aggregate(conns, as.symbol("generateSyntheticDS()"))
dslite.server$assignMethod("generateSyntheticDS","dsPUcopula::generateSyntheticDS")
DSI::datashield.assign.expr(conns, "sim_result", as.symbol("generateSyntheticDS()"))
# needs testing, colnames lost
PU_copula_model <-testMod <- getDSLiteData(conns$sim1, "PU_copula_model")
n_rSynthetic <- getDSLiteData(conns$sim1, "n_rSynthetic")
marginals_model <- getDSLiteData(conns$sim1, "marginals_model")
generateSyntheticDS()
sim_result <- getDSLiteData(conns$sim1, "sim_result")

# 5 postprocess
dslite.server$aggregateMethod("postprocessDataDS","dsPUcopula::postprocessDataDS")
rs <- DSI::datashield.aggregate(conns, as.symbol("postprocessDataDS('sim_result', 'D_preprocessed$original_levels')"))
# has to be tested, bec all factors are 1:(
D_preprocessed <- getDSLiteData(conns$sim1, "D_preprocessed")
postprocessDataDS('sim_result', 'D_preprocessed$original_levels')

#dsPUcopula::postprocessDataDS("syn_data$sim1", getDSLiteData(conns$sim1, "D_preprocessed")$original_levels)

plot(rs[[1]][,c(4:5,7,11)])
plot(D[,c(4:5,7,11)])
rs$sim1$PM_BMI_CATEGORICAL%>% table()
# coding of factor variables is not as input (1,2 instead of 0,1)

getDSLiteData(conns$sim1, "marginals_model")
mydat <- getDSLiteData(conns$sim1, "D")
mydat <- getDSLiteData(conns$sim1, "sim_result")
mylevs <- getDSLiteData(conns$sim1, "D_preprocessed")$original_levels
dsPUcopula::postprocessDataDS("mydat", "mylevs")
#getDSLiteData(conns$sim1, "D_preprocessed$original_levels")


#####

# Create test numerical vector
test_data <- rpois(n = 50, lambda = 10)

# Create virtualized server with DSLite, assign everything needed on it
dslite.server <- newDSLiteServer(tables=list(exposures = test_data),
                                 config = DSLite::defaultDSConfiguration(include=c("dsBase", "dsX")))

builder <- DSI::newDSLoginBuilder()
builder$append(server = "server1", url = "dslite.server", table = "exposures", driver = "DSLiteDriver")
logindata.dslite <- builder$build()

dslite.server$assignMethods(c("fitPUcopulaDS"))

# Login to the virtualized server
conns <- datashield.login(logindata.dslite, assign=T, symbol = "exposures")



######

foo <- getDSLiteData(conns$sim1, "D")
dim(foo)
#foo <- foo[1:4000,]
foomod  <- PUcopula::PUCopula(family="nbinom",
                              pars.a = c(40),
                              patch="sample", patchpar = list(m=2000),
                              data=as.matrix(foo))
foorand <- foomod@rand(2000)
plot(as.data.frame(foorand[,1:3]))
#plot(as.data.frame(apply(foo,2,rank)[,1:3]))

### more tests

foodat <- data.frame( cat = factor(c(rep("a",11),rep("b",4),rep("c",5))),
                      num1 = c(rnorm(11,0),rnorm(4,-1),rnorm(5,1)))
foodat$num2 <- 4-2*foodat$num1*runif(20)
foodat$cat <- as.numeric(foodat$cat)

require(PUcopula)
x <- PUCopula(family="gamma", pars.a=c(40, 43), patch="lFrechet",data=as.matrix(foodat))
plot(x@rand(2000)) #ERROR, lFrechet not valid here!

plot(x@ranks, sub="emp. rank vectors", xlab="", ylab="")
plot(x@rpatch(2000,"uFrechet"), sub="upper Fréchet driver")
plot(x@rpatch(2000,"Bernstein",list(m=20,K=20)), sub="Bernstein copula, m=20, K=20", xlab="", ylab="")
# plot for phi=0 (rook copula)
plot(x@rpatch(2000,"rook"), sub="rook copula, phi=0", xlab="", ylab="")
# plot for phi=0.9
plot(x@rpatch(2000,"Gauss",0.9), sub="phi=0.9", xlab="", ylab="")
plot(x@rpatch(2000,"Gauss",-0.8), sub="phi = -0.8", xlab="", ylab="")
plot(x@rpatch(2000,"Gauss",-0.5), sub="phi = -0.5", xlab="", ylab="")
plot(x@rpatch(2000,"sample",list(m=20)), sub="phi = -0.5")
plot(x@rpatch(2000,"sample",list(m=10)), sub="phi = -0.5")
plot(x@rpatch(2000,"sample",list(m=5)), sub="phi = -0.5")
# why does that fail?
plot(x@rpatch(2000,"sample",list(m=20)), sub="phi = -0.8", xlab="", ylab="")


x <- PUCopula(family="gamma", pars.a=c(40, 43,40), patch="rook",data=as.matrix(foodat))
plot(as.data.frame(x@rand(2000)))
x <- PUCopula(family="gamma", pars.a=c(40, 43,40), patch="sample", patchpar = list(m=20), data=as.matrix(foodat))
plot(as.data.frame(x@rand(2000)))
x <- PUCopula(family="gamma", pars.a=c(40, 43,40), patch="sample", patchpar = list(m=10), data=as.matrix(foodat))
plot(as.data.frame(x@rand(2000)))
x <- PUCopula(family="gamma", pars.a=c(40, 43,40), patch="sample", patchpar = list(m=5), data=as.matrix(foodat))
plot(as.data.frame(x@rand(2000)))
x <- PUCopula(family="nbinom", pars.a=c(40, 43,40), patch="sample", patchpar = list(m=5), data=as.matrix(foodat))
plot(as.data.frame(x@rand(2000)))

#devtools::install_github("amaendle/PUcopula")

# rmm experiments
data <- getDSLiteData(conns$sim1, "D")

ind <- colnames(data)!="DIS_CVA"
data <- data[,ind]

data[] <- lapply(data, function(col) {
  if (is.factor(col)) {
    as.numeric(as.character(col))
  } else {
    col
  }
})

data <- stats::na.omit(data) # unfortunately this removes all the interesting part of this sample dataset...

data.standardised <- as.data.frame(lapply(data, function(x) (x-mean(x, na.rm=T))/sd(x, na.rm=T)))

data <- as.matrix(data)

neighbours=5
nearest <- RANN::nn2(data, k = neighbours)

# Calculate the centroid of each n nearest data points
data.centroid <- matrix(dat=0,nrow=dim(data)[1], ncol=dim(data)[2])
colnames(data.centroid) <- colnames(data)

for (i in 1:neighbours) {
  data.centroid <- data.centroid + data.standardised[nearest$nn.idx[,i],]
}
data.centroid <- data.centroid/neighbours


# Calculate the scaling factor
scalingFactors <- sapply(data.standardised, sd)/sapply(data.centroid, sd)

# Apply the scaling factor to the centroids
data.masked <- sweep(data.centroid,2,scalingFactors,"*")

# Shift the centroids back to the actual position and scale of the original data
newsd <- sapply(as.data.frame(data),sd)
newmean <- sapply(as.data.frame(data),mean)
data.new <- sweep(data.masked,2, newsd,"*")
data.new <- sweep(data.new,   2, newmean,"+")

plot(as.data.frame(data)[,1:3])
plot(as.data.frame(data.centroid)[,1:3])
plot(as.data.frame(data.new)[,1:3])

plot(as.data.frame(apply(data.new,2,rank))[,1:3])
plot(as.data.frame(apply(data,2,rank))[,1:3])

