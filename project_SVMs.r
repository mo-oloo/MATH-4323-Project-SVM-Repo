data <- read.csv("C:/Users/dvira/Desktop/College/2023_1 Spring/MATH 4323/Group Project/Data/cbb.csv",na.strings="",stringsAsFactors=TRUE)
View(data)
data = data[,c(3:22)]
summary(data)

library(e1071)

set.seed(420)
train = sample(1:nrow(data), 
               round(0.8*nrow(data)))
train_data = as.data.frame(data[train,])
test_data = as.data.frame(data[-train,])

set.seed(420)
tune.out.lin = tune(svm, POSTSEASON~., data=train_data, kernel="linear",
                ranges = list(
                  cost=c(0.001,0.01,0.1,0.5,1:20)
                ))
summary(tune.out.lin)
# best cost = 3
# training error rate = 0.1461489
svmfit.lin = svm(POSTSEASON~., data=train_data, kernel="linear", cost = 3)
yhat.lin = predict(svmfit.lin, test_data)
table(pred=yhat.lin, true=test_data$POSTSEASON)
mean(predict(svmfit.lin, newdata=test_data) != test_data$POSTSEASON)
# test error rate = 0.1608961

set.seed(420)
tune.out.pol = tune(svm, POSTSEASON~., data=train_data, kernel="polynomial",
                    ranges = list(
                      cost = c(0.001, 0.01, 0.1, 0.5, 1:20),
                      degree = c(1:3)
                    ))
summary(tune.out.pol)
# best cost = 17, degree = 1
# training error rate = 0.1497151
svmfit.pol = svm(POSTSEASON~., data=train_data, kernel="polynomial",
                 cost = 17, degree = 1)
yhat.pol = predict(svmfit.pol, test_data)
table(pred=yhat.pol, true=test_data$POSTSEASON)
mean(predict(svmfit.pol, newdata=test_data) != test_data$POSTSEASON)
# test error rate = 0.1486762

set.seed(420)
tune.out.rad = tune(svm, POSTSEASON~., data=train_data, kernel="radial",
                    ranges = list(
                      cost = c(0.001, 0.01, 0.1, 0.5, 1:20),
                      gamma = c(0.01, 0.1, 1, 5)
                    ))
summary(tune.out.rad)
# best cost = 18, gamma = 0.01
# training error rate = 0.1512302
svmfit.rad = svm(POSTSEASON~., data=train_data, kernel="radial",
                 cost = 18, gamma = 0.01)
yhat.rad = predict(svmfit.rad, test_data)
table(pred=yhat.rad, true=test_data$POSTSEASON)
mean(predict(svmfit.rad, newdata=test_data) != test_data$POSTSEASON)
# test error rate = 0.1731161

# run against the entire dataset using the optimal model
yhat.total = predict(svmfit.pol, data)
table(pred=yhat.total, true=data$POSTSEASON)
mean(predict(svmfit.pol, newdata=data) != data$POSTSEASON)
1-mean(predict(svmfit.pol, newdata=data) != data$POSTSEASON)
# 0.8737271 correct prediction rate, which is higher than the optimal KNN model.