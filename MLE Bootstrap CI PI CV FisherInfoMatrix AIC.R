#Naomi Kaduwela
#Feb 17
#Predictive Analytics 2 - Midterm Notes


# 1. MLE with GLM
# 2. MLE with nlm()
# 3. MLE with nls()
# 4. Bootstrap
# 5. Bootstrap - CI, PI
# 6. Fisher Information Matrix
# 7. Cross Validation
# 8. AIC and CV

rm(list = ls())

#---------------------------------------------------------------------------------------------------------------
# MLE - MAXIMUM LIKELIHOOD ESTIMATOR - with GLM
#---------------------------------------------------------------------------------------------------------------

# Logistic Model Fitting and Illustration of the Likelihood Function   slide 7

# The objective is to model the probability that a household will purchase a new car, 
# as a function of demographic and other available data for the household

CAR<-read.table("Data_for_Lecture_Examples/Car.csv",sep=",",header=TRUE)
CAR
glm1 <- glm(y ~ ., family = binomial(link="logit"), data = CAR) 
#binoimal(link = logit) is logistic function (probabiliy of purchasing a new car)
# Note: Dispersion parameter for binomial family taken to be 1
summary(glm1)
p_hat <- predict(glm1, type="response")
data.frame(CAR, p_hat=round(p_hat,3))
attach(CAR); 
y <- CAR$y
plot(car_age[y==1],income[y==1],col="red", pch=15, xlab="car_age", ylab="income", xlim=c(1,6), ylim=c(10,100))
points(car_age[y==0], income[y==0], col="black", pch=19) 
detach(CAR)
summary(glm1)







#---------------------------------------------------------------------------------------------------------------
# MLE - MAXIMUM LIKELIHOOD ESTIMATOR - with nlm() *Least squares method*
#
# Note: need to know the function of the data when using this, it will only find optimal parameters for that function
#---------------------------------------------------------------------------------------------------------------

#fitting learning curve example using the general optimizer nlm()    slide 19

MLC<-read.table("Data_for_Lecture_Examples/MLC.csv",sep=",",header=TRUE)
x1<-MLC$Location;
x2<-MLC$Week;
y<-MLC$Efficiency
fn <- function(p) {yhat<-p[1]+p[2]*x1+p[4]*exp(p[3]*x2); sum((y-yhat)^2)}  #sum of error squared = least squared

out<-nlm(fn,p=c(1,0,-.5,-.1),hessian=TRUE) #send in parameter guesses, HESSIAN true = compute second derivate at the optimum value - asymptotic value hypothesis testing
theta<-out$estimate  #parameter estimates
theta #optimal values

###we will use the following later, for finding SEs and CIs#######
MSE<-out$minimum/(length(y) - length(theta))  #estimate of the error variance, 
#MSE considering optimal theta values, theta = optimal estimate for parameter
InfoMat<-out$hessian/2/MSE  #observed information matrix, out = output of nlm model
CovTheta<-solve(InfoMat)
SE<-sqrt(diag(CovTheta))  #standard errors of parameter estimates (1 for each parameter)
MSE #0.0001266416
CovTheta

SE #0.7418084 0.7795476





#---------------------------------------------------------------------------------------------------------------
# MLE - MAXIMUM LIKELIHOOD ESTIMATOR - with nls()     *not so good to use this*
#---------------------------------------------------------------------------------------------------------------

#fitting learning curve example using the nonlinear LS function nls()     slide 20 
MLC<-read.table("Data_for_Lecture_Examples/MLC.csv",sep=",",header=TRUE) 
x1<-MLC$Location;
x2<-MLC$Week;
y<-MLC$Efficiency
fn2 <- function(x1,x2,p) p[1]+p[2]*x1+p[4]*exp(p[3]*x2) 
out2<-nls(y~fn2(x1,x2,p),start=list(p=c(1,0,-.5,-.3)),trace=TRUE) 
summary(out2)





#---------------------------------------------------------------------------------------------------------------
# BOOTSTRAP RESAMPLING : BOOTSTRAPPING
# 
# Objective: 
#   - Estimate the sampling distribution of θˆ
#   - SE(theta)- its standard error
#   - a confidence interval for θ, etc.                     slide 32
#         1. Crude ("Normal" in R) CI: using SE(theta), could be too narrow , works well when data is symmetric and centered
#         2. Relfected CI ("Basic" in R): using quantiles, use with NON normal distribution - when data NOT symmetric or centered. includes bias. 
# 
# Why this works: 
#   - Consider making a pretend population that consists of your original sample of n observations, 
#     copied over and over, an infinite number of times. 
#     Each bootstrap sample is equivalent to drawing a random sample of size n from this infinite pretend pop
#
# How to do without bootstrapping?
#   - Simulation: create 100 simulations, calculate the means, then take the SD of all 100 means
#
# Bootstrapping steps:   slide 29 
# 1. generate “bootstrap” sample (with replacement) of n observations (must be the same n, or we change the distribution)
# 2. Fit the same model to the bootstrapped sample. 
# 3. Pick large B and repeat B times
# 4. Construct histogram and average over all samples to get the SE(theta)
#
# When to use Bootstrapping (vs Fisher Information Matrix):
#  - If n is not large enough to invoke asymptotics (the more complex the model, the larger the required n), use bootstrapping
#  - In certain cases in which the conditions other than n required for the asymptotic results are not met (somewhat rare), use bootstrapping
#  - If there is no underlying probabilistic model for the data (and hence no likelihood or MLEs), in which case the Fisher info matrix is irrelevant, use bootstrapping
#  - If the model is nonparametric, in which case the Fisher info matrix is irrelevant, use bootstrapping
#  - For SEs for the predicted response CIs or PIs with complex nonlinear models, use bootstrapping
#---------------------------------------------------------------------------------------------------------------

# bootstrapping parameter SEs/CIs for the manufacturing learning curve      slide 35 

library(boot) #need to load the boot package 
MLC<-read.table("Data_for_Lecture_Examples/MLC.csv",sep=",",header=TRUE) 
MLCfit<-function(Z,i,theta0) {
Zboot<-Z[i,]
x1<-Zboot[[1]];
x2<-Zboot[[2]];
y<-Zboot[[3]]
fn <- function(p) {
  yhat<-p[1]+p[2]*x1+p[4]*exp(p[3]*x2); 
  sum((y-yhat)^2)} #SSE
out<-nlm(fn,p=theta0)
theta<-out$estimate} 
MLCboot<-boot(MLC, MLCfit, R=5000, theta0=c(1,-.05,-.14,-.55)) #R = number of bootstrap samples to create 
                                                              # theta0 are our initial guess for our theta 4 parameters

#covariance - unnormalized correlation
#correlation coeff = covariance(theta1, theta2)/ SD(theta1) * SD(theta2)
#covariance(theta1, theta2) = 1/n SUM (theta2 hat - theta2 bar) x (thetax hat = theta3 bar)
CovTheta<-cov(MLCboot$t) #MLCboot$t each row is the theta values * R (5000) times

#standard error of theta1, theta2, theta3 - parameters for each sample run
SE<-sqrt(diag(CovTheta)) #SE = 0.003070048 0.004077804 0.005867536 0.011475306
#diagonals in covariance matrix are the variance values for each estimated theta parameter above bootstrap sample
#square root to get standard deviation --> which is standard error estimate

MLCboot
CovTheta
SE

#check normal qq plot because it looks a little left skewed —> use reflective CI because there is a bias 
plot(MLCboot,index=1) #index=i calculates results for ith parameter 

# Bootstrap - calculating CI's 

# CI
# type = "norm" gives our crude CI based on the SE and the normal percentiles, 
# but translated by subtracting out the estimated Bias (taken to be the bootstrap average minus the original parameter estimate)
# type = “basic” interval gives the better CI obtained by reflecting the percentiles, which includes the bias
boot.ci(MLCboot,conf=c(.9,.95,.99),index=1,type=c("norm","basic")) #getting CI for your theta value, specify index value associated with the theta value




#---------------------------------------------------------------------------------------------------------------
# Bootstrap - calculating CI's on y prediction
#---------------------------------------------------------------------------------------------------------------

library(boot) #need to load the boot package    slide 48

#R commands for bootstrapping response CIs for the manufacturing learning curve

MLC<-read.table("Data_for_Lecture_Examples/MLC.csv",sep=",",header=TRUE) 
MLCfit<-function(Z,i,theta0,x_pred) { # z is original sample data
                                      # theta 0= initial value estimated from the linear model with optimal G’s used
                                      # x_pred = value you want to predict Y* at
Zboot<-Z[i,]
x1<-Zboot[[1]];
x2<-Zboot[[2]];
y<-Zboot[[3]]
fn <- function(p) {yhat<-p[1]+p[2]*x1+p[4]*exp(p[3]*x2); sum((y-yhat)^2)} # note: general x, not x_pred value here
out<-nlm(fn,p=theta0)
theta<-out$estimate


#add this last line to get y prediction for each bootstrap replicate, otherwise calculating CI for thetas
y_pred<- theta[1]+theta[2]*x_pred[1]+theta[4]*exp(theta[3]*x_pred[2])} #predicted response, 
                                                                      # theta[1] = optimal G0 and G1
                                                                      # y_pred = YHAT

#MLCboot is holding the last line result
MLCboot<-boot(MLC, MLCfit, R=5000, theta0=c(1,-.05,-.14,-.55), x_pred=c(1,15)) #x_pred = the point we want the CI for
MLCboot


VarYhat<-var(MLCboot$t); #find variance 
VarYhat
SEYhat<-sqrt(VarYhat); #sqrt to get SE
SEYhat
plot(MLCboot) 
boot.ci(MLCboot,conf=c(.9,.95,.99),type=c("norm","basic")) #CI 


#---------------------------------------------------------------------------------------------------------------
# Bootstrap - calculating PI's 
#---------------------------------------------------------------------------------------------------------------

library(boot) #need to load the boot package    slide 48

#R commands for bootstrapping response CIs for the manufacturing learning curve

MLC<-read.table("Data_for_Lecture_Examples/MLC.csv",sep=",",header=TRUE) 
MLCfit<-function(Z,i,theta0,x_pred) { # z is original sample data
  # theta 0= initial value estimated from the linear model with optimal G’s used
  # x_pred = value you want to predict Y* at
  Zboot<-Z[i,]
  x1<-Zboot[[1]];
  x2<-Zboot[[2]];
  y<-Zboot[[3]]
  fn <- function(p) {yhat<-p[1]+p[2]*x1+p[4]*exp(p[3]*x2); sum((y-yhat)^2)} # note: general x, not x_pred value here
  out<-nlm(fn,p=theta0)
  theta<-out$estimate
  
  
  #add this last line to get y prediction for each bootstrap replicate
  y_pred<- theta[1]+theta[2]*x_pred[1]+theta[4]*exp(theta[3]*x_pred[2])} #predicted response, 
# theta[1] = optimal G0 and G1
# y_pred = YHAT

#MLCboot is holding the last line result
MLCboot<-boot(MLC, MLCfit, R=5000, theta0=c(1,-.05,-.14,-.55), x_pred=c(1,15)) #x_pred = the point we want the PI for
MLCboot

#simpler PI
SEY<-sqrt(var(Yhatboot)+MSE); 
SEY  #0.01232091
c(g.hat-qnorm(.975)*SEY, g.hat+qnorm(.975)*SEY) #simpler PI =  0.8708515 0.9191485


#more complex PI
Yhat0<-MLCboot$t0
Yhatboot<-MLCboot$t
g.hat <- 0.895; 
MSE <- 0.0001266 #from slide 46 
e<-rnorm(nrow(Yhatboot), mean=0, sd=sqrt(MSE)) 
Yboot<-Yhatboot-e 
Yquant<-quantile(Yboot,prob=c(.025,.975)) 
L<-2*Yhat0-Yquant[2]
U<-2*Yhat0-Yquant[1]
hist(Yboot,100)
c(L,U) #more complex PI





#---------------------------------------------------------------------------------------------------------------
# FISHER INFORMATION MATRIX 
# 
# When to use Fisher (vs Bootstrapping):
#  - If n is large enough to invoke asymptotics, and you want an expression for the SEs 
#     (which provides useful insight into the quality of the data), use the Fisher info matrix
#---------------------------------------------------------------------------------------------------------------

# R commands for finding Fisher Info for learning curve example (repeat of slide #15?)     slide 45

MLC<-read.table("Data_for_Lecture_Examples/MLC.csv",sep=",",header=TRUE) 
x1<-MLC$Location;
x2<-MLC$Week;
y<-MLC$Efficiency
fn <- function(p) {
  yhat<-p[1]+p[2]*x1+p[4]*exp(p[3]*x2); #p1 = intercept, p2 = facility, p3/p4 = week  
  sum((y-yhat)^2)
  } 
out<-nlm(fn,p=c(1,0,-.5,-.1),hessian=TRUE)
theta<-out$estimate #parameter estimates  
theta

###the following calculates the SEs#######
MSE<-out$minimum/(length(y) - length(theta)) #estimate of the error variance - estimate sigma squared with MSE
InfoMat<-out$hessian/2/MSE #observed information matrix 
CovTheta<-solve(InfoMat) # inverse of fisher information matrix
SE<-sqrt(diag(CovTheta)) #standard errors of parameter estimates
MSE
CovTheta
SE # SE of theta





#---------------------------------------------------------------------------------------------------------------
# CROSS VALIDATION 
# 
# When to use CV:
#   - compare SSEcv to see which model is better 
#   - variable selection: does x1 belong in the model or not (bigger model vs smaller model)
#---------------------------------------------------------------------------------------------------------------

# R commands for creating indices of partition for K- fold CV     slide 60

CVInd <- function(n,K) { #n is sample size; K is number of parts; returns K-length list of indices for each part
  m<-floor(n/K) #approximate size of each part 
  r<-n-m*K
  I<-sample(n,n) #random reordering of the indices 
  Ind<-list() #will be list of indices for all K parts 
  length(Ind)<-K
  for (k in 1:K) {
    if (k <= r) kpart <- ((m+1)*(k-1)+1):((m+1)*k)
    else kpart<-((m+1)*r+m*(k-r-1)+1):((m+1)*r+m*(k-r)) 
    Ind[[k]] <- I[kpart] #indices for kth part of data
  }
  Ind }





# Shell for running multiple random replicates of CV      slide 61

Nrep<-20 #number of replicates of CV
K<-10 #K-fold CV on each replicate
n.models = 3 #number of different models to fit and compare 
n=nrow(MLC)
y<-MLC$Efficiency
yhat=matrix(0,n,n.models)
MSE<-matrix(0,Nrep,n.models)
for (j in 1:Nrep) {
  Ind<-CVInd(n,K) 
  for (k in 1:K) {
    out<-lm(Efficiency~.,MLC[-Ind[[k]],]) #the first model to compare , negative because you need to remove that part
    yhat[Ind[[k]],1]<-as.numeric(predict(out,MLC[Ind[[k]],]))
    out<-lm(Efficiency ~ . - Location,MLC[-Ind[[k]],]) #the second model to compare 
    yhat[Ind[[k]],2]<-as.numeric(predict(out,MLC[Ind[[k]],]))
    out<-lm(Efficiency ~ .^2, MLC[-Ind[[k]],]) #the third model to compare
    yhat[Ind[[k]],3]<-as.numeric(predict(out,MLC[Ind[[k]],])) 
    } #end of k loop
  MSE[j,]=apply(yhat,2,function(x) sum((y-x)^2))/n
} #end of j loop
MSE
MSEAve<- apply(MSE,2,mean); 
MSEAve #averaged mean square CV error 
MSEsd <- apply(MSE,2,sd); 
MSEsd #SD of mean square CV error 
r2<-1-MSEAve/var(y); 
r2 #CV r^2


# EXAMPLE 2 of same code above: Using the preceding for K-fold CV for Manu. Learning Curve Data      slide 62


Nrep<-20 #number of replicates of CV
K<-10 #K-fold CV on each replicate
# 10 times on each fold, and 20 replicates so 200 times 
n.models = 2 #number of different models to fit and compare 
FitFun1 <- function(x1,x2,p) p[1]+p[2]*x1+p[4]*exp(p[3]*x2) 
FitFun2 <- function(x1,x2,p) p[1]+p[3]*exp(p[2]*x2) 
n=nrow(MLC)
y<-MLC$Efficiency
yhat=matrix(0,n,n.models)
MSE<-matrix(0,Nrep,n.models)
for (j in 1:Nrep) { # j loop iterates one replicate at a time
  Ind<-CVInd(n,K) 
  for (k in 1:K) { # k loop iterates over each fold
    #first model
    out<-nls(Efficiency~FitFun1(Location,Week,p),data=MLC[-Ind[[k]],],start=list(p=c(1,-.05,-.15,-.55))) 
    yhat[Ind[[k]],1]<-as.numeric(predict(out,MLC[Ind[[k]],])) 
    
    #second model 
    out<-nls(Efficiency~FitFun2(Location,Week,p),data=MLC[-Ind[[k]],],start=list(p=c(1,-.15,-.55))) 
    yhat[Ind[[k]],2]<-as.numeric(predict(out,MLC[Ind[[k]],]))
  } #end of k loop
  MSE[j,]=apply(yhat,2,function(x) sum((y-x)^2))/n
} #end of j loop
MSE # first column is MSEcv, model 1 vs second column is MSEcv, model 2
MSEAve<- apply(MSE,2,mean); 
MSEAve #averaged mean square CV error =  0.0001559811 0.0008391828
MSEsd <- apply(MSE,2,sd); 
MSEsd #SD of mean square CV error  =  1.089592e-05 3.599124e-05
r2<-1-MSEAve/var(y); # r^2 not overfitting for large model since it’s R^2cv
r2 #CV r^2 = 0.9947810 0.9719216





#---------------------------------------------------------------------------------------------------------------
# AIC ad CV - compared two sized logistic regression models
#   - AIC: smaller is better (minimize)
#---------------------------------------------------------------------------------------------------------------


# R commands for AIC and CV for a logistic regression model for the car purchase data        slide 63 

library(boot)
CAR<-read.table("Data_for_Lecture_Examples/Car.csv",sep=",",header=TRUE)
n<-nrow(CAR)
car.fit<-glm(y~income+car_age,family=binomial(link = "logit"),data=CAR) 
summary(car.fit) #smaller model without income, AIC = 42.69, without dividing by n
car.fit$aic/n #1.293625

AIC<- -2*as.numeric(logLik(car.fit))/n+2*3/n
out<-cv.glm(CAR, car.fit, function(y,phat) -mean(log(phat)*y+log(1-phat)*(1-y)), K=11) 
AIC #1.293625


out$delta #0.6572457 0.6521703

car.fit<-glm(y~income+car_age+income:car_age, family=binomial(link = "logit"),data=CAR) #larger model with income 
summary(car.fit) #AIC = 43.404
AIC<- -2*as.numeric(logLik(car.fit))/n+2*4/n
out<-cv.glm(CAR, car.fit, function(y,phat) -mean(log(phat)*y+log(1-phat)*(1-y)), K=11) 
AIC # 1.315276
car.fit$aic/n #1.315276
out$delta # 0.7248305 0.7149087




