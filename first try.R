#loading dataset 
train <- read.csv("art.csv" , na.strings = c("NULL" , "#VALUE!" , " "))
names(train)
str(train)

#there are way too many levels in Art type, so I'm trying to dilute it
train$artnumeric <- as.numeric(train$Art.Type) #a rough way to dilute
train$Art.Type <- ifelse(train$artnumeric <= 100 ,1 ,ifelse(train$artnumeric <=300 , 2 , ifelse(train$artnumeric <= 600 , 3 , ifelse(train$artnumeric <= 900 ,4 ,5))))

str(train2)

#cleaning
#fixing brush size
train$Brush.Size[is.na(train$Brush.Size)] <- median(train$Brush.Size , na.rm = T)

#fixing brush finesse
library(nnet)
train$Brush.Finesse <- relevel(train$Brush.Finesse , ref = "Coarse")
a <- multinom(Brush.Finesse ~ Brush.Size , data = train )
b <- predict(a , train)
train$Brush.Finesse <- b

#fixin current auction avg price and collectors avg price
train$CurrentAuctionAveragePrice[is.na(train$CurrentAuctionAveragePrice)] <- mean(train$CurrentAuctionAveragePrice , na.rm = T)
train$CollectorsAverageprice[is.na(train$CollectorsAverageprice)] <- mean(train$CollectorsAverageprice , na.rm = T)

#fixing rest of the errors
train2 <- na.omit(train2)

#checking the cleaned set
train2 <- train[,-c(1,6,7,11,12,22,23,25,27)]
str(train2)
library(mice)
md.pattern(train2)


#taking a random sample
set.seed(101)
t1 <- sample(1:nrow(train2), 50000)
fitting_set = train2[t1,]
testing_set = train2[-t1,]
#first trying lasso
attach(train2)
library(glmnet)
X = model.matrix(Critic.Ratings~. , data = fitting_set)
Y = fitting_set$Critic.Ratings
fit.lasso = glmnet(X,Y)
cv.lasso = cv.glmnet(X,Y)
plot(cv.lasso)
# from the graph I'm selecting optimum log(lambda) = -5
exp(-5)
# prediction 
X1 = model.matrix(Critic.Ratings~. , data = testing_set)
Y1 = testing_set$Critic.Ratings
predict(fit.lasso , newx = X1 , s =0.006737 , type = "response" )
