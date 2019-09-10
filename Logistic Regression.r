
library('caret')
crashTest_1 = read.csv("crashTest_1.csv", row.names = 1)
crashTest_1_TEST = read.csv("crashTest_1_TEST.csv", row.names = 1)

head(crashTest_1)
head(crashTest_1_TEST)

str(crashTest_1)

summary(crashTest_1)

logisfit = glm(formula = crashTest_1$CarType~., data = crashTest_1, family = "binomial")
logisfit

summary(logisfit)

logisTrain = predict(logisfit, type = "response")
plot(logisTrain)

tapply(logisTrain, crashTest_1$CarType, mean)

logisPred = predict(logisfit, newdata = crashTest_1_TEST, type='response')
plot(logisPred)

crashTest_1_TEST[logisPred<=0.5, "LogisPred"] = "Hatchback"
crashTest_1_TEST[logisPred>0.5, "LogisPred"] = "SUV"
head(crashTest_1_TEST)

confusionMatrix(table(crashTest_1_TEST[,7], crashTest_1_TEST[,6]), positive = 'Hatchback')
