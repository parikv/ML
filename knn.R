library(caret)
library(ggplot2)

glass.df <-read.csv("../input/glass.csv")

dim(glass.df)
names(glass.df)
head(glass.df)
tail(glass.df)

summary(glass.df)
str(glass.df)



glass.df$Type<- as.factor(glass.df$Type) 


set.seed(123)
ind = sample(2, nrow(glass.df), replace = TRUE, prob=c(0.7,0.3))
train.df = glass.df[ind == 1,]
test.df = glass.df[ind == 2,]
dim(train.df)
dim(test.df)

table(train.df$Type)

table(test.df$Type)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

knn_fit <- train(Type ~., data = train.df, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 15)

knn_fit

plot(knn_fit)

test_pred <- predict(knn_fit, newdata = test.df)
test_pred

confusionMatrix(test_pred, test.df$Type )


mean(test_pred == test.df$Type)

