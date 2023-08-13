#Q(i)
rm(list = ls())
attach(iris)
head(iris)
data1=iris[,-5]
p=prcomp(data1,center = F)
a=p$sdev^2;a

s=svd(data1)
b=s$d^2/149;b

all.equal(round(a,3),round(b,3))

#---------------------------------------------------------------
rm(list=ls())
library(pls)
data1=iris[,-5]
#(i)
model=pcr(data1$Sepal.Length~.,data=data1)
summary(model)
model$coefficients

#(or use this)
p=prcomp(iris[2:4])
z=p$x[,1:2]
l=lm(Sepal.Length~z,iris)
a=p$rotation[,1:2]%*%coefficients(l)[-1]
a
pred=0.5398*pc1+0.3647*pc2+0.2345*pc3

##(ii)
predplot(model)

##(iii)
train=iris[1:120,-5]
test=iris[121:150,c(-1,-5)]
y.test=iris$Sepal.Length[121:150]
pcr_model=pcr(Sepal.Length~.,data=train)
pcr_perd=predict(pcr_model,newdata=test,2)
RMSE=sqrt(mean(pcr_perd-y.test)^2)
RMSE
#is the average deviation btw the pred values for sepal length and the obs values for sepal length for observation in the test data set