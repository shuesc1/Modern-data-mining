library("ISLR")
fix(Hitters)
names(Hitters)
dim(Hitters)

sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

library("leaps")
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)

regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(2,2))

plot(reg.summary$rss,xlab="Number of Variables", ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables", ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)

#Cp and BIC stats
plot(reg.summary$cp,xlab="number of variables",ylab="Cp", type = 'l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

coef(regfit.full,6)

# forward and backwards stepwise regression
regfit.fwd=regsubsets(Salary~.,data=Hitters ,nvmax =19, method ="forward")
summary(regfit.fwd)

regfit.bwd=regsubsets(Salary~.,data=Hitters ,nvmax =19,method ="backward")
summary(regfit.bwd)

#best subset selction, 7 predictors
coef(regfit.full,7)
#forward stepwise selection, 7 predictors
coef(regfit.fwd,7)
#backward stepwise selection, 7 predictors
coef(regfit.bwd,7)
#all have different coefficients and variables

#choosing a model via VALIDATION SET APPROACH & CROSS-VALIDATION
#must use ONLY TRAINING OBSERVATIONS
# split into training & test data
# random seed set so the user will get same training set/test set split
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)
summary(train)
#dim(train)
summary(test)

#apply regsubsets() to just training set to perform best subset selection
#nvmax = max # of subsets to examine
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
test.mat=model.matrix(Salary~.,data=Hitters[test,])
val.errors=rep(NA,19)
for(i in 1:19){
  coefi = coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
#shows there were 10 variables in the best model

#make predict method for reuse
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
#now perform best subset selection on FULL dataset to get better coeff estimates
regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)
#best 10 var model on full data set != best 10 var model on training set

# now use CROSS-VALIDATION to choose among the models of diff sizes
#do subset selection within each of the k training sets 
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace = TRUE)
cv.errors=matrix(NA,k,19, dimnames = list(NULL,paste(1:19)))
#for loop that performs cross validation
# jth fold-elements equal j are in TEST SET, rest in TRAINING SET
for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}
#gives 10x19 matrix; (i,j)th element == test MSE for the ith cross-val fold for
#the best j-variable model
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')

#shows lowest errors with 11 variables
reg.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(reg.best,11)
# the above command prints the intercept & coeffs for 
# the best 11 var model selected by cross-val

#========RIDGE REGRESSION && LASSO==========p.251
#use glmnet() to do ridge & lasso
sum(is.na(Hitters$Salary))
#checking to make sure that NA values are out
#params of glmnet() are: x matrix & y vector
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
#dim(y)
summary(y)

#RIDGE
#alpha=0 ==> ridge; alpha=1 ==> lasso
install.packages("glmnet")
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda = grid)
dim(coef(ridge.mod))
#20x100 matrix
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

#coeffs when lambda=705, along w their l2 norm
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod,s=50,type = "coefficients")[1:20,]
#now randomly split data into training and test set
#in order to est test error or ridge & lasso
# use set.seed() so results will be reproducible
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda = grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx = x[test,])
mean((ridge.pred-y.test)^2)

mean((mean(y[train])-y.test)^2)
ridge.pred=predict(ridge.mod,s=1e10,newx = x[test,])
mean((ridge.pred-y.test)^2)

ridge.pred=predict(ridge.mod,s=0,newx=x[test,])
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,type="coefficients")[1:20,]

#use cross-validation to choose tuning param lambda
# use cv.glmnet()--default performs 10 fold cross-validation (change via nfolds)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
# value of tuning param lamda with smallest cross-validation error is 399.2

ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
#finally, refit ridge regr model on FULL DATA SET, using lambda from CV
out=glmnet(x,y,alpha = 0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# LASSO
# glmnet() again, now alpha = 1
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda = grid)
plot(lasso.mod)
#now perform CV and get test error
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
#resulting coeff estimates are sparse w/ Lasso
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef != 0]

#==========PCR & PLS regression===========
#install.packages("pls")
library("pls")
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters,scale=TRUE, validation="CV")
summary(pcr.fit)
#plot CV MSE values
validationplot(pcr.fit,val.type = "MSEP")
#smallest CV error occurs when M=16 components used

#now perform PCR on training data & eval its test set perf
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type = "MSEP")
# lowest CV error occurs at M=7~
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
# test set MSE (106868) is competitive w/ results
# from Ridge regr & Lasso

#finally, fit PCR on full data set, using M=7, the num
#of components identified by cross-validation
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

# =====================PARTIAL LEAST SQUARES=============p.258
#plsr() function/library
set.seed(1)
pls.fit=plsr(Salary~., data=Hitters, subset=train,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type = "MSEP")
#lowest CV error occurs when M=2 (according to book)
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)
#The test MSE is comparable to, but slightly higher than, the test MSE
#obtained using ridge regression, the lasso, and PCR.

#finally perform PLS using FULL DATA SET, using M=2, the #of comps id'd by CV
pls.fit=plsr(Salary~., data=Hitters, scale=TRUE,ncomp=2)
summary(pls.fit)

#===============================================================
#========================APPLIED, QUES 8========================
#===============================================================
#p.262 #8

#============================PART A============================
#(a) Use the rnorm() function to generate a predictor X of length
#n = 100, as well as a noise vector epsilon of length n = 100.
#rnorm(N) generates a vector of N pseudo-random normals with mean 0 and
#variance 1.  N must be a positive integer.
set.seed(1)
x <- rnorm(100)
epsilon <- rnorm(100)
# set.seed(1)
# x1=runif(100)
# x2=0.5*x1+rnorm(100)/10
# y1=2+2*x1+0.3*x2+rnorm(100)
# summary(x1)
# summary(x2)
# summary(y1)
# 
# set.seed(1)
# x=rnorm(100)
# noise=
# #y=x-2*x^2+rnorm(100)
# summary(x)
# summary(y)

#=============================PART B============================
#(b) Generate a response vector Y of length n = 100 according to
#the model
b0 <- 5
b1 <- 3
b2 <- 4
b3 <- 7.2

#y=5+ (3*x) + (4*(x^2)) + (7.2*(x^3)) +
y <- b0 + b1*x + b2* x^2 + b3* x^3 + epsilon

#======================ques 8, PART C==========================
# Use the regsubsets() function to perform best subset selection
# in order to choose the best model containing the predictors
# X,X2, . . .,X10. What is the best model obtained according to
# Cp, BIC, and adjusted R2? Show some plots to provide evidence
# for your answer, and report the coefficients of the best model obtained.
# Note you will need to use the data.frame() function to
# create a single data set containing both X and Y .
dataset <- data.frame(y = y, x = x)
summary(dataset)
regfit.fullmodel=regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data=dataset,nvmax=10)
fullmod.sum=summary(regfit.fullmodel)
names(fullmod.sum)
fullmod.sum$rsq
par(mfrow=c(2,2))
plot(fullmod.sum$rss,xlab="number of variables", ylab="RSS",type="l")
plot(fullmod.sum$adjr2,xlab="Number of variables", ylab="Adjusted Rsq",type = "l")
#adjusted Rsq highest at 4 variables
which.max(fullmod.sum$adjr2)
points(4,fullmod.sum$adjr2[4], col="red",cex=2,pch=20)

# #Cp and BIC stats
plot(fullmod.sum$cp,xlab="number of variables",ylab="Cp", type="l")
which.min(fullmod.sum$cp)
points(4,fullmod.sum$cp[4],col="red",cex=2,pch=20)
plot(fullmod.sum$bic,xlab="Number of variables",ylab="BIC",type = "l")
points(which.min(fullmod.sum$bic), fullmod.sum$bic[which.min(fullmod.sum$bic)],col="red",cex=2,pch=20)

plot(regfit.fullmodel,scale="r2")
plot(regfit.fullmodel,scale="adjr2")
plot(regfit.fullmodel,scale = "Cp")
plot(regfit.fullmodel,scale="bic")

#coefficients for 4 variable model
coef(regfit.fullmodel,4)

#======================ques 8, PART D==========================
# Repeat (c), using forward stepwise selection and also using backwards
# stepwise selection. How does your answer compare to the
# results in (c)?

#<<<<<<<<<<<<<<<<<<<<<<FORWARD STEPWISE SELECTION>>>>>>>>>>>>>>>>>>>
regfit.forward=regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data=dataset,nvmax=10,method = "forward")
forwardmod.sum=summary(regfit.forward)
forwardmod.sum

par(mfrow = c(2, 2))
# Cp - 4 vars
plot(forwardmod.sum$cp, xlab = "Number of variables", ylab = "C_p", type = "l")
points(which.min(forwardmod.sum$cp), forwardmod.sum$cp[which.min(forwardmod.sum$cp)], col = "red", cex = 2, pch = 20)
# BIC --indicates 3 vars best
plot(forwardmod.sum$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(forwardmod.sum$bic), forwardmod.sum$bic[which.min(forwardmod.sum$bic)], col = "red", cex = 2, pch = 20)
# adj Rsq - 4 vars
plot(forwardmod.sum$adjr2, xlab = "Number of variables", ylab = "Adjusted Rsq", type = "l")
points(which.max(forwardmod.sum$adjr2), forwardmod.sum$adjr2[which.max(forwardmod.sum$adjr2)], col = "red", cex = 2, pch = 20)

coef(regfit.forward,3)
# (Intercept)           x      I(x^2)      I(x^3) 
# 5.061507    2.975280    3.876209    7.217639 
coef(regfit.forward,4)
# (Intercept)           x      I(x^2)      I(x^3)      I(x^5) 
# 5.07200775  3.38745596  3.84575641  6.75797426  0.08072292 

#<<<<<<<<<<<<<<<<<<<<<<BACKWARDS STEPWISE SELECTION>>>>>>>>>>>>>>>>>>>
regfit.backward=regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data=dataset,nvmax=10,method = "backward")
backwardmod.sum=summary(regfit.backward)
backwardmod.sum

par(mfrow = c(2, 2))
# Cp -4  vars
plot(backwardmod.sum$cp, xlab = "Number of variables", ylab = "C_p", type = "l")
points(which.min(backwardmod.sum$cp), backwardmod.sum$cp[which.min(backwardmod.sum$cp)], col = "red", cex = 2, pch = 20)
# BIC --indicates 3 vars best
plot(backwardmod.sum$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(backwardmod.sum$bic), backwardmod.sum$bic[which.min(backwardmod.sum$bic)], col = "red", cex = 2, pch = 20)
# adj Rsq - 4 vars
plot(backwardmod.sum$adjr2, xlab = "Number of variables", ylab = "Adjusted Rsq", type = "l")
points(which.max(backwardmod.sum$adjr2), backwardmod.sum$adjr2[which.max(backwardmod.sum$adjr2)], col = "red", cex = 2, pch = 20)

coef(regfit.backward,3)
# (Intercept)           x      I(x^2)      I(x^3) 
# 5.061507    2.975280    3.876209    7.217639 
coef(regfit.backward,4)
# (Intercept)           x      I(x^2)      I(x^3)      I(x^9) 
# 5.079236362 3.231905828 3.833494180 7.019555807 0.001290827 


#======================ques 8, PART E==========================
# (e) Now fit a lasso model to the simulated data, again using X,X2,
# . . . , X10 as predictors. Use cross-validation to select the optimal
# value of λ. Create plots of the cross-validation error as a function
# of λ. Report the resulting coefficient estimates, and discuss the
# results obtained.

x_matrix = model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = dataset)[, -1]
lasso.model=glmnet(x_matrix,y,alpha=1,lambda = grid)
#perform CV to get test error
set.seed(1)
crossval.out=cv.glmnet(x_matrix,y,alpha=1)
plot(crossval.out)
#lambda between -2.1 and -1.5
bestlamda=crossval.out$lambda.min
bestlamda
# [1] 0.1213782
lasso.prediction=predict(lasso.model,s=bestlamda,newx = x_matrix)
#mean((lasso.prediction-y.test)^2)
output=glmnet(x_matrix,y,alpha=1,lambda = grid)
lasso.coefficient=predict(output,type="coefficients", s=bestlamda)[1:10,]
lasso.coefficient
# (Intercept)            x       I(x^2)       I(x^3)       I(x^4)       I(x^5)       I(x^6)       I(x^7)       I(x^8) 
# 5.221140e+00 3.033995e+00 3.573236e+00 7.065246e+00 4.755177e-02 1.139770e-02 0.000000e+00 1.880960e-03 0.000000e+00 
# I(x^9) 
# 6.309071e-05 
lasso.coefficient[lasso.coefficient!=0]
# (Intercept)            x       I(x^2)       I(x^3)       I(x^4)       I(x^5)       I(x^7)       I(x^9) 
# 5.221140e+00 3.033995e+00 3.573236e+00 7.065246e+00 4.755177e-02 1.139770e-02 1.880960e-03 6.309071e-05 


#======================ques 8, PART F==========================
# (f) Now generate a response vector Y according to the model
# Y = β0 + β7X7 + ϵ,
# and perform best subset selection and the lasso. Discuss the
# results obtained.
set.seed(1)
x <- rnorm(100)
epsilon <- rnorm(100)

b0 <- 5
b7 <- 7
y <- b0 + b7 * x^7 + epsilon

data.abbrev.set <- data.frame(y = y, x = x)
summary(data.abbrev.set)
#nvmax - largest subset size to examine
#regfit.abbrev=regsubsets(y ~ x + I(x^7), data=data.abbrev.set,nvmax=10) #not sure about this line
regfit.abbrev=regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data=data.abbrev.set,nvmax=10)
abbrevmod.sum=summary(regfit.abbrev)
names(abbrevmod.sum)
abbrevmod.sum$rsq
par(mfrow=c(2,2))
plot(abbrevmod.sum$rss,xlab="number of variables", ylab="RSS",type="l")
plot(abbrevmod.sum$adjr2,xlab="Number of variables", ylab="Adjusted Rsq",type = "l")
which.max(abbrevmod.sum$adjr2)
points(1,abbrevmod.sum$adjr2[1], col="red",cex=2,pch=20)

plot(abbrevmod.sum$cp,xlab="number of variables",ylab="Cp", type="l")
which.min(abbrevmod.sum$cp)
points(1,abbrevmod.sum$cp[1],col="red",cex=2,pch=20)
plot(abbrevmod.sum$bic,xlab="Number of variables",ylab="BIC",type = "l")
points(which.min(abbrevmod.sum$bic), abbrevmod.sum$bic[which.min(abbrevmod.sum$bic)],col="red",cex=2,pch=20)

plot(regfit.abbrev,scale="r2")
plot(regfit.abbrev,scale="adjr2")
plot(regfit.abbrev,scale = "Cp")
plot(regfit.abbrev,scale="bic")

#coefficients for 1 variable model -- results don't make sense
coef(regfit.fullmodel,1)
# clearly not right
# (Intercept)      I(x^3) 
# 8.279046    8.396432
coef(regfit.fullmodel,2)
# (Intercept)      I(x^2)      I(x^3) 
# 5.319997    3.740657    8.050129 
coef(regfit.fullmodel,3)
# (Intercept)           x      I(x^2)      I(x^3) 
# 5.061507    2.975280    3.876209    7.217639 

#lasso - results make sense
x_matrix <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.abbrev.set)[, -1]
crossv.lasso <- cv.glmnet(x_matrix, y, alpha = 1)
best_lamd <- crossv.lasso$lambda.min
best_lamd
lasso.modl <- glmnet(x_matrix, y, alpha = 1)
lasso.coeffs=predict(lasso.modl, s = best_lamd, type = "coefficients")[1:11, ]
lasso.coeffs
lasso.coeffs[lasso.coeffs!=0]
# (Intercept)      I(x^7) 
# 5.904188    6.776797 
