rm(list=ls())
if(!require("devtools"))
  (install.packages("devtools"))
if(!require("shiny"))
  (install.packages("shiny"))
if(!require("shinyBS"))
  (install.packages("shinyBS"))
if(!require("rCharts"))
  (install.packages("rCharts"))
if(!require("dplyr"))
  (install.packages("dplyr"))
if(!require("data.table"))
  (install.packages("data.table"))
if(!require("lubridate"))
  (install.packages("lubridate"))
if(!require("ggplot2"))
  (install.packages("ggplot2"))
if(!require("rCharts"))
  (install_github('rCharts', 'ramnathv'))
if(!require("mgcv"))
  (install.packages("mgcv"))
if(!require("gbm"))
  (install.packages("gbm"))
if(!require("e1071"))
  (install.packages("e1071"))
if(!require("dtw"))
  (install.packages("dtw"))
if(!require("wavelets"))
  (install.packages("wavelets"))
if(!require("zipcode"))
  (install.packages("zipcode"))
if(!require("cluster"))
  (install.packages("cluster"))
if(!require("fields"))
  (install.packages("fields"))
if(!require("rgdal"))
  (install.packages("rgdal"))
if(!require("ggmap"))
  (install.packages("ggmap"))
if(!require("rgeos"))
  (install.packages("rgeos"))
if(!require("reshape2"))
  (install.packages("reshape2"))
if(!require("RColorBrewer"))
  (install.packages("RColorBrewer"))
if(!require("BayesTree"))
  (install.packages("BayesTree"))
if(!require("mboost"))
  (install.packages("mboost"))


###custom and local packages
if(!require('DataPull')) {
  devtools::install_github(c("albre116/CHR_II/trunk/DataPull"),auth_token="ffcaf9fb4036981ec6022f13d2a1d05df97a5ff3")
}

source("HelperFunctions.R")
####Load in Data####
path <- c('~/CHR_Reference_Data/FullDataSet_AllYearsCombined.txt')  ###set this to file path location for raw data
sample_pct <- 0.1 ###set between [0,1]
RAW <- DataPull::loadData(path,sample_pct)  ### see help file for documentation
RAW <- DataPull::geocodeData(RAW)   ###Geocoding the Data
RAW <- DataPull::tallyDailyVolume(RAW)
save(RAW,file="RAW.RData")###save it so we don't always have to run this



load("RAW.RData")###we can start here most times

######Lets get a cut of the data

DATA <- StateMap(RAW,norig=2,ndest=2)
DATA <- CountyMap(DATA$DATA,DATA$selectedOriginStates,DATA$selectedDestinationStates)





#####do some kriging

###look at volume variation

KrigPlot(RAW,#intput data
         type="Orig",#select either Orig or Dest
         states.model=c("California","Nevada","Arizona","Oregon"),#model area
         states.plot=c("California","Nevada","Arizona","Oregon"),#plot area
         npoints=5000,###krig points,
         dist=50,#the miles aroound each krig point to summarize data over
         DatePlotStart=as.Date("2011-6-1",format="%Y-%m-%d"),#start date for the plot data
         DatePlotEnd=as.Date("2011-6-10",format="%Y-%m-%d"),#end date for the plot data
         summary_fun="n()",##the dplyr function to run : mean(RPM_NormalizedCustomer)
         summary_name="Volume",##the name of the summary measure
         summary_variable="1",###the name of the variable or a number
         low_quantile=0,#the quantile to cut the points at
         high_quantile=100,#the quantile to cut the points at
         theta=20# the kriging exponential decay parameter in miles
)





###look at the RPM variation

KrigPlot(RAW,#intput data
 type="Orig",#select either Orig or Dest
 states.model=c("California","Nevada","Arizona","Oregon"),#model area
 states.plot=c("California"),#plot area
 npoints=5000,###krig points,
 dist=50,#the miles aroound each krig point to summarize data over
 DatePlotStart=as.Date("2011-6-1",format="%Y-%m-%d"),#start date for the plot data
 DatePlotEnd=as.Date("2011-6-10",format="%Y-%m-%d"),#end date for the plot data
 summary_fun="mean(RPM_NormalizedCustomer)",##the dplyr function to run : mean(RPM_NormalizedCustomer)
 summary_name="RPM_NormalizedCustomer",##the name of the summary measure
 summary_variable="DATA$RPM_NormalizedCustomer",###the name of the variable or a number
 low_quantile=5,#the quantile to cut the points at
 high_quantile=95,#the quantile to cut the points at
 theta=20# the kriging exponential decay parameter in miles
)








################################
####stopped here ####
################################



#DWT reference
###http://www.jstatsoft.org/v31/i07/paper
#Wavlet clustering reference
###http://www.rdatamining.com/examples/time-series-clustering-classification


###now filter for display
#grab the Low-High-th percentiles
Low=5
High=95
DATA<-dplyr::filter(RAW,!is.infinite(RPM_NormalizedCustomer) &
                      !is.na(RPM_NormalizedCustomer))
DATA<-dplyr::filter(DATA,ntile(RPM_NormalizedCustomer,100)>Low &
                      ntile(RPM_NormalizedCustomer,100)<=High)



####lets do a simple model here
fit<-mgcv::gam(RPM_NormalizedCustomer~EntryDate+s(Day365,bs="cc")+s(SumOfStops),data=DATA)
summary(fit)
plot(fit,pages=1,all.terms=TRUE)

####make an even spaced set of days to predict over
min_time<-min((DATA$EntryDate))
max_time<-max((DATA$EntryDate))
difftime(max_time,min_time)



####lets look at the data fit
plot<-ggplot(DATA)+geom_point(aes(x=EntryDate,y=RPM_NormalizedCustomer),alpha=0.1)
print(plot)



######try boosting with trees
gbm1 <-
  gbm(RPM_NormalizedCustomer~Day365+SumOfStops,         # formula
      data=DATA,                   # dataset
      var.monotone=c(0,0), # -1: monotone decrease,
      # +1: monotone increase,
      #  0: no monotone restrictions
      distribution="gaussian",     # see the help for other choices
      n.trees=10,                # number of trees
      shrinkage=0.05,              # shrinkage or learning rate,
      # 0.001 to 0.1 usually work
      interaction.depth=3,         # 1: additive model, 2: two-way interactions, etc.
      bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
      train.fraction = 0.5,        # fraction of data for training,
      # first train.fraction*N used for training
      n.minobsinnode = 10,         # minimum total weight needed in each node
      cv.folds = 3,                # do 3-fold cross-validation
      keep.data=TRUE,              # keep a copy of the dataset with the object
      verbose=FALSE,               # don't print out progress
      n.cores=1)                   # use only a single core (detecting #cores is
# error-prone, so avoided here)

# check performance using an out-of-bag estimator
# OOB underestimates the optimal number of iterations
best.iter <- gbm.perf(gbm1,method="OOB")
print(best.iter)

# check performance using a 50% heldout test set
best.iter <- gbm.perf(gbm1,method="test")
print(best.iter)

# check performance using 5-fold cross-validation
best.iter <- gbm.perf(gbm1,method="cv")
print(best.iter)

# plot the performance # plot variable influence
summary(gbm1,n.trees=1)         # based on the first tree
summary(gbm1,n.trees=best.iter) # based on the estimated best number of trees

# compactly print the first and last trees for curiosity
print(pretty.gbm.tree(gbm1,1))
print(pretty.gbm.tree(gbm1,gbm1$n.trees))

# make some new data
N <- 1000
X1 <- runif(N)
X2 <- 2*runif(N)
X3 <- ordered(sample(letters[1:4],N,replace=TRUE))
X4 <- factor(sample(letters[1:6],N,replace=TRUE))
X5 <- factor(sample(letters[1:3],N,replace=TRUE))
X6 <- 3*runif(N) 
mu <- c(-1,0,1,2)[as.numeric(X3)]

Y <- X1**1.5 + 2 * (X2**.5) + mu + rnorm(N,0,sigma)

data2 <- data.frame(Y=Y,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,X6=X6)

# predict on the new data using "best" number of trees
# f.predict generally will be on the canonical scale (logit,log,etc.)
f.predict <- predict(gbm1,data2,best.iter)

# least squares error
print(sum((data2$Y-f.predict)^2))

# create marginal plots
# plot variable X1,X2,X3 after "best" iterations
par(mfrow=c(1,3))
plot(gbm1,1,best.iter)
plot(gbm1,2,best.iter)
plot(gbm1,3,best.iter)
par(mfrow=c(1,1))
# contour plot of variables 1 and 2 after "best" iterations
plot(gbm1,1:2,best.iter)
# lattice plot of variables 2 and 3
plot(gbm1,2:3,best.iter)
# lattice plot of variables 3 and 4
plot(gbm1,3:4,best.iter)

# 3-way plots
plot(gbm1,c(1,2,6),best.iter,cont=20)
plot(gbm1,1:3,best.iter)
plot(gbm1,2:4,best.iter)
plot(gbm1,3:5,best.iter)






