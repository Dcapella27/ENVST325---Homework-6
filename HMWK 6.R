library(caret)
library(randomForest)
library(rgdal)
library(sf)
library(raster)
library(rgdal)

oct <- stack("/cloud/project/activity08/Oct_12.tif")
drStack <- stack("/cloud/project/activity08/May_19.tif",
                 "/cloud/project/activity08/June_10.tif",
                 "/cloud/project/activity08/June_18.tif",
                 "/cloud/project/activity08/Oct_12.tif")
lc <- st_read("/cloud/project/activity08/land_pts.shp")

# Question 1 - Random Forest

# subset to only focus on training pts (60 in each class)
trainPts <- subset(lc, lc$train=="train", drop=FALSE)
# extract pixel data
train <- extract(drStack, trainPts)
# get attribute table from points
trainTable <- st_drop_geometry(trainPts)
# combine into one table that has a y column
# for land cover and all other data are predictions
trainDF <- na.omit(cbind(y=as.factor(trainTable[,3]), train))
#Kfold cross validation
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
nbands <- 20 #20 bands of information
rf.grid <- expand.grid(mtry=1:round(sqrt(nbands))) # number of variables available for splitting at each tree node
# set random seed for algorithm so you can get the same results when
# running multiple times
set.seed(43)

#note that caret:: will make sure we use train from the caret package
rf_model <- caret::train(x = trainDF[,2:21], #digital number data
                         y = as.factor(trainDF[,1]), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trainControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter t
rf_model
#use the model to predict land cover class for the entire raster stack
rf_prediction <- raster::predict(drStack, rf_model )
# plot the land cover class (uses LCID number)
plot(rf_prediction, col= hcl.colors(3, palette = "Harmonic"))
# subset land cover points for validation
validPts <- subset(lc, lc$train=="valid", drop=FALSE)
# convert to data frame
valid_Table <- st_drop_geometry(validPts)

# extract predicted land cover for each point
valid_rf <- extract(rf_prediction, validPts)
# turn into table
validDF_rf <- data.frame(y=valid_Table[,3], rf=valid_rf)
# make a confusion matrix
# LCID 1 = field
# LCID 2 =  tree
# LCID 3 = path
# confusion Matrix, first argument is prediction second is data
rf_errorM = confusionMatrix(as.factor(validDF_rf$rf),as.factor(validDF_rf$y))
# make LCID easier to interpret
colnames(rf_errorM$table) <- c("field","tree","path")
rownames(rf_errorM$table) <- c("field","tree","path")
rf_errorM

# Question 1 - Neural Net
# starting parameters for neural net
nnet.grid <- expand.grid(size = seq(from = 1, to = 10, by = 1), # number of neurons units in the hidden layer 
                         decay = seq(from = 0.001, to = 0.01, by = 0.001)) # regularization parameter to avoid over-fitting 
# train nnet
set.seed(18)
nnet_model <- caret::train(x = trainDF[,c(2:21)], y = as.factor(trainDF[,1]),
                           method = "nnet", metric="Accuracy", 
                           trainControl = tc, tuneGrid = nnet.grid,
                           trace=FALSE)
nnet_model
# predictions
nn_prediction <- raster::predict(drStack, nnet_model)
# map
plot(nn_prediction, col= hcl.colors(3, palette = "Harmonic"))
#cell count neural net
freq(nn_prediction)
#cell count random forest
freq(rf_prediction)
par(mfrow=c(1,2))
plot(nn_prediction, col= hcl.colors(3, palette = "Harmonic"),
     legend=FALSE, axes=FALSE, main="Neural network", box=FALSE)
legend("bottomleft", c("field","tree","path"),
       fill=hcl.colors(3, palette = "Harmonic") ,bty="n")

plot(rf_prediction, col= hcl.colors(3, palette = "Harmonic"),
     legend=FALSE, axes=FALSE, main="Random forest", box=FALSE)
legend("bottomleft", c("field","tree","path"),
       fill=hcl.colors(3, palette = "Harmonic") ,bty="n")

validPts <- subset(lc, lc$train=="valid", drop=FALSE)
# convert to data frame
valid_Table <- st_drop_geometry(validPts)

# extract predicted land cover for each point
valid_nn <- extract(nn_prediction, validPts)
# turn into table
validDF_nn <- data.frame(y=valid_Table[,3], nn=valid_nn)

nn_errorM = confusionMatrix(as.factor(validDF_nn$nn),as.factor(validDF_nn$y))
# make LCID easier to interpret
colnames(nn_errorM$table) <- c("field","tree","path")
rownames(nn_errorM$table) <- c("field","tree","path")
nn_errorM


# Question 2
imageOne <- stack("/cloud/project/activity08/May_19.tif")
imageTwo <- stack("/cloud/project/activity08/June_10.tif")
imageThree <- stack("/cloud/project/activity08/June_18.tif")
imageFour <- stack("/cloud/project/activity08/Oct_12.tif")


#calculate NDVI for the images
imageOneNDVI <- (imageOne[[4]]-imageOne[[3]])/(imageOne[[4]]+imageOne[[3]])
imageTwoNDVI <- (imageTwo[[4]]-imageTwo[[3]])/(imageTwo[[4]]+imageTwo[[3]])
imageThreeNDVI <- (imageThree[[4]]-imageThree[[3]])/(imageThree[[4]]+imageThree[[3]])
imageFourNDVI <- (imageThree[[4]]-imageThree[[3]])/(imageThree[[4]]+imageThree[[3]])

par(mfrow=c(2,2))
plot(imageOneNDVI)
plot(imageTwoNDVI)
plot(imageThreeNDVI)
plot(imageFourNDVI)

# Question 3
set.seed(18)
nnet.grid <- expand.grid(size = seq(from = 1, to = 20, by = 1), # number of neurons units in the hidden layer 
                         decay = seq(from = 0.001, to = 0.01, by = 0.001)) # regularization parameter to avoid over-fitting
nnet_model <- caret::train(x = trainDF[,c(2:21)], y = as.factor(trainDF[,1]),
                           method = "nnet", metric="Accuracy", 
                           trainControl = tc, tuneGrid = nnet.grid,
                           trace=FALSE)
nnet_model
# predictions
nn_prediction <- raster::predict(drStack, nnet_model)
# map
par(mfrow = c(1, 1))
plot(nn_prediction, col= hcl.colors(3, palette = "Harmonic"))
#cell count neural net
freq(nn_prediction)
#cell count random forest
freq(rf_prediction)
par(mfrow=c(1,2))
plot(nn_prediction, col= hcl.colors(3, palette = "Harmonic"),
     legend=FALSE, axes=FALSE, main="Neural network", box=FALSE)
legend("bottomleft", c("field","tree","path"),
       fill=hcl.colors(3, palette = "Harmonic") ,bty="n")

plot(rf_prediction, col= hcl.colors(3, palette = "Harmonic"),
     legend=FALSE, axes=FALSE, main="Random forest", box=FALSE)
legend("bottomleft", c("field","tree","path"),
       fill=hcl.colors(3, palette = "Harmonic") ,bty="n")
