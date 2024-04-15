library(randomForest)
library(randomForestCI)
#library(vcd)
#library(corrgram)
#library(leaps) # for subset selection
#library(glmnet) # for lasso
library(gbm) # for grediant boosting
#library(matrixStats) # for rowSds
library(e1071) # the e1071 library contains implementations for a number of statistical learning methods.
library(boot) # for bootstrap
############################################################
#source("features.R")
#source("Features-ceramic.R")
source("functions.R")
####################################################

data.training = read.csv("data.parameters_features.csv")[-1]

data.training.T0 = data.training[, c("NCT", "ea", "av", "AN", "t", "T0")]
colnames(data.training.T0)[6] = "es"
#data.test.T0 = data.test[, c("NCT", "ea", "av", "AN", "t", "T0")]
#colnames(data.test.T0)[6] = "es"

data.training.a0 = data.training[, c("D", "EVW","AN", "a0")]
colnames(data.training.a0)[4] = "es"
#data.test.a0 = data.test[, c("D", "EVW","AN", "a0")]
#colnames(data.test.a0)[4] = "es"

data.training.a11 = data.training[, c("P", "AN", "evw", "NCT", "av", "t", "a11")]
colnames(data.training.a11)[7] = "es"
#data.test.a11 = data.test[, c("P", "AN", "evw", "NCT", "av", "t", "a11")]
#colnames(data.test.a11)[7] = "es"

data.training.a111 = data.training[, c("P", "AN", "ENMB", "EVW", "D", "a111")]
colnames(data.training.a111)[6] = "es"
#data.test.a111 = data.test[, c("P", "AN", "ENMB", "EVW", "D", "a111")]
#colnames(data.test.a111)[6] = "es"


######################################## SVR mdoel ##############################################
set.seed(20)
############ for T0
plot(fn.model.svr.rbf(data.training.T0, data.training.T0) ~ data.training.T0$es,     #### plot curves after regress by fn.model.svr.rbf
     ylab = "Predicted values", xlab = "Measured values",
     pch = 21, col = "darkblue", cex = 1.5, bg="darkblue",xlim= c(0,150), ylim = c(0,150))
abline(0, 1, lwd =2, col = "red")
points(fn.model.svr.rbf(data.training.T0, data.test.T0) ~ data.test.T0$es, pch = 21, col = "red", cex = 1.5, bg = "red")

model.SVR.rbf.T0 = fn.boot.prop.mean.error(data.training.T0, data.training.T0, seed = 10, model = "svr", B = 1000, ntree = 500, mtry = ceiling(dim(data.training.T0)[2]/3))
#fn.plot.model(model.SVR.rbf.T0$mean, model.SVR.rbf.T0$sd, data.training.T0$es)
plot(model.SVR.rbf.T0$mean ~ data.training.T0$es, xlab = "Measured Values", ylab = "Predicted Values",
     pch = 1, col = "darkblue", cex = 1.5, bg="darkblue", xlim= c(25, 125), ylim = c(25, 125), main = "SVR.rbf.T0")
arrows(data.training.T0$es, model.SVR.rbf.T0$mean - 0.5*model.SVR.rbf.T0$sd, data.training.T0$es, model.SVR.rbf.T0$mean + 0.5*model.SVR.rbf.T0$sd, length = 0.08, angle = 90, code = 3, col = "darkblue")
abline(0,1, col = "red", lwd = 1.0)

set.seed(10)
fn.cv.svr.rbf(data.training.T0)

SVR.rbf.T0 = cbind(model.SVR.rbf.T0, data.training.T0$es)
colnames(SVR.rbf.T0) = c("mean", "sd", "es")
write.csv(SVR.rbf.T0, "svr.rbf.T0.csv")

############## for a0
set.seed(127)
plot(fn.model.svr.rbf(data.training.a0, data.training.a0) ~ data.training.a0$es,     #### plot curves after regress by fn.model.svr.rbf
     ylab = "Predicted values", xlab = "Measured values",
     pch = 21, col = "darkblue", cex = 1.5, bg="darkblue",xlim= c(0,10), ylim = c(0,10))
abline(0, 1, lwd =2, col = "red")
points(fn.model.svr.rbf(data.training.a0, data.test.a0) ~ data.test.a0$es, pch = 21, col = "red", cex = 1.5, bg = "red")

model.SVR.rbf.a0 = fn.boot.prop.mean.error(data.training.a0, data.training.a0, seed = 10, model = "svr", B = 1000, ntree = 500, mtry = ceiling(dim(data.training.T0)[2]/3))
#fn.plot.model(model.SVR.rbf.a0$mean, model.SVR.rbf.a0$sd, data.training.a0$es)
plot(model.SVR.rbf.a0$mean ~ data.training.a0$es, xlab = "Measured Values", ylab = "Predicted Values",
     pch = 1, col = "darkblue", cex = 1.5, bg="darkblue", xlim= c(1, 9), ylim = c(1, 9), main = "SVR.rbf.a0")
arrows(data.training.a0$es, model.SVR.rbf.a0$mean - 0.5*model.SVR.rbf.a0$sd, data.training.a0$es, model.SVR.rbf.a0$mean + 0.5*model.SVR.rbf.a0$sd, length = 0.08, angle = 90, code = 3, col = "darkblue")
abline(0,1, col = "red", lwd = 1.0)

set.seed(10)
fn.cv.svr.rbf(data.training.a0)


SVR.rbf.a0 = cbind(model.SVR.rbf.a0, data.training.a0$es)
colnames(SVR.rbf.a0) = c("mean", "sd", "es")
write.csv(SVR.rbf.a0, "svr.rbf.a0.csv")


############## for a11
set.seed(127)
plot(fn.model.svr.rbf(data.training.a11, data.training.a11) ~ data.training.a11$es,     #### plot curves after regress by fn.model.svr.rbf
     ylab = "Predicted values", xlab = "Measured values",
     pch = 21, col = "darkblue", cex = 1.5, bg="darkblue",xlim= c(-75,20), ylim = c(-75,20))
abline(0, 1, lwd =2, col = "red")
points(fn.model.svr.rbf(data.training.a11, data.test.a11) ~ data.test.a11$es, pch = 21, col = "red", cex = 1.5, bg = "red")

model.SVR.rbf.a11 = fn.boot.prop.mean.error(data.training.a11, data.training.a11, seed = 10, model = "svr", B = 1000, ntree = 500, mtry = ceiling(dim(data.training.T0)[2]/3))
#fn.plot.model(model.SVR.rbf.a0$mean, model.SVR.rbf.a0$sd, data.training.a0$es)
plot(model.SVR.rbf.a11$mean ~ data.training.a11$es, xlab = "Measured Values", ylab = "Predicted Values",
     pch = 1, col = "darkblue", cex = 1.5, bg="darkblue", xlim= c(-20, 2), ylim = c(-20, 2), main = "SVR.rbf.a11")
arrows(data.training.a11$es, model.SVR.rbf.a11$mean - 0.5*model.SVR.rbf.a11$sd, data.training.a11$es, model.SVR.rbf.a11$mean + 0.5*model.SVR.rbf.a11$sd, length = 0.08, angle = 90, code = 3, col = "darkblue")
abline(0,1, col = "red", lwd = 1.0)

set.seed(10)
fn.cv.svr.rbf(data.training.a11)


SVR.rbf.a11 = cbind(model.SVR.rbf.a11, data.training.a11$es)
colnames(SVR.rbf.a11) = c("mean", "sd", "es")
write.csv(SVR.rbf.a11, "svr.rbf.a11.csv")

############## for a111
set.seed(127)
plot(fn.model.svr.rbf(data.training.a111, data.training.a111) ~ data.training.a111$es,     #### plot curves after regress by fn.model.svr.rbf
     ylab = "Predicted values", xlab = "Measured values",
     pch = 21, col = "darkblue", cex = 1.5, bg="darkblue",xlim= c(0, 100), ylim = c(0, 100))
abline(0, 1, lwd =2, col = "red")
points(fn.model.svr.rbf(data.training.a111, data.test.a111) ~ data.test.a111$es, pch = 21, col = "red", cex = 1.5, bg = "red")

model.SVR.rbf.a111 = fn.boot.prop.mean.error(data.training.a111, data.training.a111, seed = 10, model = "svr", B = 1000, ntree = 500, mtry = ceiling(dim(data.training.T0)[2]/3))
#fn.plot.model(model.SVR.rbf.a0$mean, model.SVR.rbf.a0$sd, data.training.a0$es)
plot(model.SVR.rbf.a111$mean ~ data.training.a111$es, xlab = "Measured Values", ylab = "Predicted Values",
     pch = 1, col = "darkblue", cex = 1.5, bg="darkblue", xlim= c(0, 15), ylim = c(0, 15), main = "SVR.rbf.a111")
arrows(data.training.a111$es, model.SVR.rbf.a111$mean - 0.5*model.SVR.rbf.a111$sd, data.training.a111$es, model.SVR.rbf.a111$mean + 0.5*model.SVR.rbf.a111$sd, length = 0.08, angle = 90, code = 3, col = "darkblue")
abline(0,1, col = "red", lwd = 1.0)

set.seed(10)
fn.cv.svr.rbf(data.training.a111)

SVR.rbf.a111 = cbind(model.SVR.rbf.a111, data.training.a111$es)
colnames(SVR.rbf.a111) = c("mean", "sd", "es")
write.csv(SVR.rbf.a111, "svr.rbf.a111.csv")


