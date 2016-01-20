my_svm <- function(data,spilt=0.1,cost=10,gamma=0.1){
    np = ceiling(spilt*nrow(data))
    test.index = sample(1:nrow(data),np)
    data.test = data[test.index,]
    data.train = data[-test.index,]
    data.model <- svm(lettr ~ .,data = data.train,type='C-classification',kernel='radial',cost=cost,gamma=gamma);
    svm.pred <- predict(data.model,data.test[,-ncol(data)]);
    table.svm.test = table(pred = svm.pred ,true = data.test[,ncol(data)]);
    confusionMatrix(table(pred = svm.pred ,true = data.test[,ncol(data)]));
    correct.svm = sum(diag(table.svm.test)) / sum(table.svm.test)*100;
    return (correct.svm);
}

