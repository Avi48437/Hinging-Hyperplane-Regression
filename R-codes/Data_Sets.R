# 1 hinge
x=runif(500,min=-1,max=1)
y=x^2+rnorm(500,0,1/4)
data1=data.frame(x,y)
lm1<-lm(y~x,data=data1)
coef(lm1)
sum(lm1$residuals^2)

x=runif(500,min=0,max=pi)
y=sin(x)+rnorm(500,0,1/20)
data2=data.frame(x,y)
lm2<-lm(y~x,data=data2)
coef(lm2)
sum(lm2$residuals^2)

x=runif(1000,min=2,max=50)
y=log(x)+rnorm(1000,0,1/4)
data3=data.frame(x,y)
lm3<-lm(y~x,data=data3)
coef(lm3)
sum(lm3$residuals^2)

# 2 hinge
x=runif(1000,min=0,max=3)
y=x*(x-1)*(x-3)+rnorm(1000,0,1/4)
data4=data.frame(x,y)
lm4<-lm(y~x,data=data4)
coef(lm4)

x=runif(1000,min = 0,max=2*pi)
y=sin(x)+rnorm(1000,0,1/4)
data5=data.frame(x,y)
lm5<-lm(y~x,data=data5)
coef(lm5)


x=runif(1000,min=2,max=50)
y=log(x)+rnorm(1000,0,1/6)
data7=data.frame(x,y)
lm7<-lm(y~x,data=data1)
coef(lm7)

# 3 hinge
x=runif(1000,min=0,max=5)
y=x*(x-1)*(x-3)*(x-5)+rnorm(1000,0,1)
data6=data.frame(x,y)
lm6<-lm(y~x,data=data6)
coef(lm6)

x=runif(1000,min=-2,max=2)
y=x^2+rnorm(1000,0,1/20)
data61=data.frame(x,y)


#inbalanced data set
x1=runif(500,min=-4,max=-3.5)
x2=runif(50,min=-3.5,max=1)
x3=runif(500,min=1,max=1.5)
x4=runif(100,min=1.5,max=4)
x5=runif(1000,min=4,max=4.5)
x=c(x1,x2,x3,x4,x5)

y=x^2+rnorm(length(x),0,1)
data8=data.frame(x,y)
lm8<-lm(y~x,data=data8)
coef(lm8)
sum(lm8$residuals^2)




#Discrete Data cloud problem
x1=runif(500,min=-4,max=-3.5)
x2=runif(100,min=1,max=3)
x=c(x1,x2)
y=abs(x)+rnorm(600,0,1/3)
data9=data.frame(x,y)
#comment if the function is noiseless then the hinge is robust
#but if the data is with noise then the hinge is not robust and
# the more distance between data cloud the less robust it became


#Non convergence of algorithm for any hinge
x=runif(500,min=-2,max=2)
y=x^2+rnorm(500,0,1/4)
data91=data.frame(x,y)
lm91<-lm(y~x,data=data91)
coef(lm1)
sum(lm1$residuals^2)
# hinge at 3 do not use damped  algo.


#outlier data
#x axis
x=runif(50,min=-1,max=1)
x=sort(x)
y=abs(x)+rnorm(50,0,1/10)
data10=data.frame(x,y)
data10[c(49:50),1]=c(9,10)
data10[50,2]=c(6)
data=data10
plot(data$x,data$y)

