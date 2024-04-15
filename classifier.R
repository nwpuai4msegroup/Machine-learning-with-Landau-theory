
###########################traindata
#########################
traindata = read.csv("traindata.csv")[-1]
colnames(traindata)[9] = "type"
###########################functions#######################
library(e1071)
fn.cv.svr.rbf = function(data, formula){
  tuneResult.svr.rbf = tune(svm, formula, data = data,
                            kernel = "radial",
                            ranges = list(gamma = seq(0,1,0.2), cost = seq(20,100,20))
  )
  print(tuneResult.svr.rbf)
  return(tuneResult.svr.rbf$best.parameters)
}
fn.model.svr.rbf = function(data.training, data.virtual, formula){
  parameter.svr.rbf = fn.cv.svr.rbf(data.training, formula)
  gamma = parameter.svr.rbf[1]
  cost = parameter.svr.rbf[2]
  model = svm(formula, data = data.training, kernel = "radial", 
              gamma = gamma, cost = cost)
  prediction = predict(model, data.virtual)
  return (prediction)
}
formula<- type ~ ba + ca + sr + cd + ti + zr + sn + hf

################### check phase counts ###########
dim(traindata[which(traindata[, "type"] == "R"), ])
dim(traindata[which(traindata[, "type"] == "C"), ])
dim(traindata[which(traindata[, "type"] == "T"), ])
traindata[which(traindata[, "type"] == "R"), "type"] = "C" 
dim(traindata[which(traindata[, "type"] == "C"), ])
################# model based on training data #######################
set.seed(123)
y0 = fn.model.svr.rbf(traindata,traindata,formula)
aa = table(y0,traindata$type,dnn=list('predicted','actual'))
train.accuracy = sum(diag(aa))/nrow(traindata)
train.accuracy
plot_table(aa, "Truth", "Prediction", F) #### plot the confusion matrix
################ LOOCV #################
set.seed(322)
data.loov = traindata
n = dim(data.loov)[1]
type = data.loov[, "type"]
type = as.data.frame(type)
for (i in 1:n) {
  type.pre = fn.model.svr.rbf(data.loov[-i, ], data.loov[i, ], formula)
  type[i, "type.P"] = type.pre
}
Error.loov = dim(type[which(type[, 1] == type[, 2]), ])[1]/n

############## two part: test data and train data ###########
set.seed(710072)
boot = sample(n, 30, replace = F)
data.test = traindata[boot, ]
data.train = traindata[-boot, ]
pre.type = fn.model.svr.rbf(data.train, data.test, formula)
data.test = cbind(data.test, pre.type)
Error.test = dim(data.test[which(data.test[, "type"] == data.test[, "pre.type"]), ])[1]/dim(data.test)[1]

Error = cbind(train.accuracy, Error.loov, Error.test)
colnames(Error) = c("Train.183", "Loocv", "Test.30")
write.csv(Error, 'Error.csv')
