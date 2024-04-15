

############ prediction through SVR model ##########
fn.model.svr.rbf = function(data.training, data.virtual){   ######### new function
  
  parameter.svr.rbf = fn.cv.svr.rbf(data.training)     ###### call function, just used in SVR model, not in linear or poly ......
  gamma = parameter.svr.rbf[1]   ##### 
  cost = parameter.svr.rbf[2]    #####
  model = svm(es ~. , data = data.training, kernel = "radial",     ########## regression
              gamma = gamma, cost = cost)
  
  predictions = predict(model,data.virtual)
  return (predictions)   ########## return prediction
}



#########################################################
############# boostrap #############
#########################################################
fn.boot.prop.mean.error = function(data.training,data.virtual, seed, model, B, ntree, mtry) {   ####### new function
  set.seed(seed)
  
  n = dim(data.training)[1]
  R = B-1                               ######## boostrap times
  
  if (model == "svr") {
    predict.data = fn.model.svr.rbf(data.training,data.virtual)
    i=1
    repeat{
      #    boot.sample = c(sample(n, 5, replace = FALSE), sample(n, n-5, replace = TRUE))
      boot.sample = sample(n, n, replace = TRUE)
      t.data.training = data.training[boot.sample,]
      predict.data1 = fn.model.svr.rbf(t.data.training, data.virtual)
      predict.data = cbind(predict.data, predict.data1)
      i=i+1    
      if(i>R) break ()
    }
  }
  
  data.pre = cbind(rowMeans(predict.data), apply(predict.data,1,sd))
  colnames(data.pre) = c("mean","sd")
  return(data.frame(data.pre))
}
