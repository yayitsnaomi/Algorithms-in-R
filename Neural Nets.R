#Naomi Kaduwela
#Feb 11
#Predictive Analytics 2 - Midterm Notes

rm(list = ls())

# 1. NN Overview Notes
# 2. NN Exploratory Analysis of Predictor Variables and Stepwise Linear Regression 
# 3. NN Data Prep Steps
# 4. NN Linear output activation function 
# 5. NN Linear output logistic function 

# 6. NN Cross Validation Indexing
# 7. NN Model Comparsion with CV (Regression with logistic activation function vs linear regression)
# 8. NN Interpertation: ALE Plot for variable signficance and 2nd order Interaction Plot 

# 9. NN Model Comparsion with same CV Index Partition (Regression with linear activation function vs linear regression)

# 10. NN Binary Classification with Logistic Activation Function and comparison to logistic models with CV
# 11. NN K Class Classification with Logistic Activation Function
# 12. NN Classification with Numerical Values 
#---------------------------------------------------------------------------------------------------------------
# Neural Nets (NN)
#
# Uses: 
#   - High signal to noise ratio i.e. deep learning/image recognition 
#   - Linear, nonlinear, quadratic, etc can capture any x->y relationship 
# 
# Variables:
#   - Y response. Function of H's, which are a logistic function of X's. 
#   - X's are input layer - each node is a variable 
#   - Hidden layer nodes: internal dummy variables. sigmoid logistic curve, as many number of variables + intercept. This is the tuning parameter and we select how many 
#   - alpha and beta are to be estimated by the model: 
#         - alpha from x--> H: If the coeff is large, it will have bigger impact on hidden unit (close to 1). bigger alpha makes the logistic curve steeper. +/- alpha shift the sigmomid curve left and right/flips it. 
#         - Beta in final H layer -> Y
#
# Tuning Parameters: 
#   - # hidden layers
#   - # hidden nodes 
#   - Final output activation function (linear or logistic)
#   - regularization parameter/shrinkage term (lambda). 
#         - Lambda = 0 will overfit the model. Need to add bias in training to reduce variance in test.
#         - Because there is only one lambda applied to all predictors, that's why you need to standardize so penalty makes sense when you minmize SSE.
# 
# Regression:
#   - Linear activation function (linOut = True)
#
# Classification:
#   - Logistic activation function (linOut = False)
#
# Evaluation:
#   - Cross validation 
#
# Data Prep Steps:
#   1. Standardize each value by subtracting the mean and dividing by the SD. If we do not do this, some predictors will be dominant.
#       - Can use his code: sapply(CRT1[1:k], function(x) (x-mean(x))/sd(x))
#       - Can use scale()
#   2. Standardize the response (Y)
#       - If Linear: standardize the same way as x's
#       - If logistic: standardize to 0-1 scale
#   3. Factor predictors: If there is a 0/1 attribute like gender, apply as.factor()
#   4. Remove rows with NA's
#   5. If there is a skew/long tail in any predictors/response: take log10() or log()
#
#---------------------------------------------------------------------------------------------------------------





#---------------------------------------------------------------------------------------------------------------
# DATA PREPERATION:
#   Read in the concrete data
#   & Standardizing predictors (all x's by subtracting the mean and dividing by the SD)
#   Convert response to the response to [0,1] interval when using logistic output activation function only
#---------------------------------------------------------------------------------------------------------------

library(nnet)
par(mfrow=c(1,1))

#######R code for reading in the concrete data, converting the response to [0,1] interval, & standardizing predictors############## slide 15
CRT <- read.csv("Data_for_Lecture_Examples/concrete.csv",header=TRUE)
k<-ncol(CRT)-1 #number of predictors (8 predictors and 1 response variable)
CRT1 <- CRT #will be standardized and scaled version of data 
CRT1[1:k]<-sapply(CRT1[1:k], function(x) (x-mean(x))/sd(x)) #standardize predictors by subtracting mean and dividing by SD
CRT1[k+1]<-(CRT1[k+1]-min(CRT1[k+1]))/(max(CRT1[k+1])-min(CRT1[k+1])) #Scale Y between 0 & 1
CRT[1:10,]
pairs(CRT, cex=.5, pch=16) #scatter plot matrix of all variables - look for linear vs non linear relationships between variables and multicollinearity





#---------------------------------------------------------------------------------------------------------------
# Neural Nets - EXPLORATORY ANALYSIS OF PREDICTORS & STEPWISE LINEAR REGRESSION MODELS
#---------------------------------------------------------------------------------------------------------------
rm(list = ls())

#slide 46
XX<-read.table("Data_for_Lecture_Examples/adult_train.csv",sep=",",header=TRUE,strip.white=TRUE,na.strings="?") #strip.white = TRUE : recognize missing values so you can replace ?
XX<-na.omit(XX) #omitt rows with NA
INCOME<-XX
INCOME[,c(1,5,11,12,13)]<-scale(INCOME[,c(1,5,11,12,13)]) #standardize the continuous variables using scale()


##EXPLORATORY ANALYSIS: exploring individual variables## slide 47
#Should we be concerned with anything here or do any further cleaning?
summary(INCOME)
par(mfrow = c(2, 3))
for (i in c(1, 5, 11, 12, 13))
  hist(XX[[i]], xlab = names(XX)[i]); #scroll to see all charts 
plot(XX[[15]])

par(mfrow = c(1, 1))
plot(XX[[2]], cex.names = .7)
for (i in c(2, 4, 6, 7, 8, 9, 10, 14, 15))
  print(round(table(XX[[i]]) / nrow(XX), 3)) # what % of data in each category type


##exploring pairwise predictor/response relationships## slide 48 
par(mfrow=c(2,1))
plot(jitter(XX$age,3),jitter(XX$hours.per.week,3),pch=16,cex=.5)
plot(jitter(XX$education.num,3),jitter(XX$hours.per.week,3),pch=16,cex=.5)
par(mfrow=c(1,1))
barplot(tapply(XX$hours.per.week,XX$education.num,mean),ylim=c(30,50),cex.names=.7,xpd=F, xlab="Education.num")
for (i in c(2,4,6,7,8,9,14,15)) {
  print(round(tapply(XX$hours.per.week,XX[[i]],mean),2)); #see mean hours per week (Y) for each predictor category 
  cat("\n")
}


##LINEAR REGRESSION  with all predictors included --> typical next step    slide 51
Inc.lm<-lm(hours.per.week ~ .,data=INCOME[,-3]) # ~ . means include all predictors
summary(Inc.lm)

##LINEAR REGRESSION  including INTERACTIONS (generally NOT a good next step)  slide 51
Inc.lm.full<-lm(hours.per.week ~ .^2,data=INCOME[,-c(3,4,14)]) # ~.^ means include all predicters with interactions 
summary(Inc.lm.full)
#Interaction coeff = you see they change based on a combination of variables because one predictor incfluences another differently based on levels

##STEPWISE LINEAR REGRESSION   slide 51
#run stepwise on just the predcitors (because with interactions it will be too many)
#After you select the predictors, then include interactions to finalize 
#when you have interaction, it’s good to go from full model to smaller model and try every combo to remove non sig. 
#If you don’t do this, you might get a very large model, and don’t know which combo of interactions you want…
#p values change as you change the model, can’t compare p values for variables across models, need to check it for each one
Inc.lm0<-lm(hours.per.week ~ 1,data=INCOME[,-c(3,4)]) 
Inc.lm<-lm(hours.per.week ~ .,data=INCOME[,-c(3,4)]) 
Inc.lm.step<-step(Inc.lm0, scope=formula(Inc.lm), direction="both", trace=0) #stepwise
summary(Inc.lm.step)

#after stepwise you can take the sig variables, plot a linear model
#create a new column in your df with the residuals, take that as resposne into regression tree
#take significant variables, union them with stepwise significant varaibles, 
#then use those consolidated variables in your non linear model 

##Neural network model     slide 52
library(nnet)
library(ALEPlot)
Inc.nn1<-nnet(hours.per.week ~ . ,INCOME[,-c(3,4)], linout=T, skip=F, size=10, decay=5, maxit=100, trace=F)
yhat<-as.numeric(predict(Inc.nn1)) 
y<-INCOME$hours.per.week; 
e<-y-yhat 
1-var(e)/var(y) #training r^2 summary(Inc.nn1) = 0.2926184
#0.1455805
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)) 
ALEPlot(INCOME[,-c(3,4,13)], Inc.nn1, pred.fun=yhat, J=1, K=500, NA.plot = TRUE);




#---------------------------------------------------------------------------------------------------------------
# NEURAL NET: LINEAR ACTIVATION FUNCTION
#---------------------------------------------------------------------------------------------------------------

#############Fit a neural network model to the CRT1 data#################### slide 17
nn1<-nnet(Strength~.,CRT1, linout=T, skip=F, size=10, decay=0.01, maxit=1000, trace=F)
                      # CRT1 = standardized predictor df
                      # set linout = T, when using linear output function
                      # set linout = F, when using logistic output function
                      # size = number of M = # of nodes in hidden layer
                      # maxit = numerical optimization upper bound if it doesn’t converge
                      # decay = lambda

yhat<-as.numeric(predict(nn1)) #get predicted y value from the nn1 model created, using predict function
y<-CRT1[[9]]; #vector of y values
e<-y-yhat #error vector for each value y from neural net on training data
plot(yhat,y) #scatter plot between yhat (x axis) and y (y axis)
c(sd(y),sd(e)) # 0.20811937 0.05396813






#---------------------------------------------------------------------------------------------------------------
# NEURAL NET: LOGISTIC ACTIVATION FUNCTION
#---------------------------------------------------------------------------------------------------------------

#ensure Y is scaled to 0-1 for logistic output 
CRT1[k+1]<-(CRT1[k+1]-min(CRT1[k+1]))/(max(CRT1[k+1])-min(CRT1[k+1])) #Scale Y between 0 & 1

#repeat but using logistic output function, for which the response MUST BE SCALED TO [0,1] RANGE
nn1<-nnet(Strength~.,CRT1, linout=F, skip=F, size=10, decay=0.01, maxit=1000, trace=F)

yhat<-as.numeric(predict(nn1)) 
y<-CRT1[[9]]; 
e<-y-yhat # R^2 —> 1- var(e)/var(y)
plot(yhat,y) 
c(sd(y),sd(e)) #0.20811937 0.05737831
summary(nn1) #all the coefficient weights for alphas(i->H) and betas (H->0), but this is not interpretable 







#---------------------------------------------------------------------------------------------------------------
# NEURAL NET: MODEL TUNING WITH CROSS VALIDATION 
#---------------------------------------------------------------------------------------------------------------

#######A function to determine the indices in a CV partition################## slide 20 
CVInd <- function(n,K) {  #n is sample size; K is number of parts; returns K-length list of indices for each part
  m<-floor(n/K)  #approximate size of each part
  r<-n-m*K  
  I<-sample(n,n)  #random reordering of the indices
  Ind<-list()  #will be list of indices for all K parts
  length(Ind)<-K
  for (k in 1:K) {
    if (k <= r) kpart <- ((m+1)*(k-1)+1):((m+1)*k)  
    else kpart<-((m+1)*r+m*(k-r-1)+1):((m+1)*r+m*(k-r))
    Ind[[k]] <- I[kpart]  #indices for kth part of data
  }
  Ind
}



#---------------------------------------------------------------------------------------------------------------
# NEURAL NET: MODEL COMPARISON WITH CROSS VALIDATION : REGRESSION EXAMPLE (USING LOGISTIC OUTPUT ACTIVATION)
#---------------------------------------------------------------------------------------------------------------


##Now use multiple reps of CV to compare Neural Nets and linear reg models###      # slide 21
Nrep<-4 #number of replicates of CV
K<-3 #K-fold CV on each replicate
n.models = 3 #number of different models to fit
n=nrow(CRT1) 
y<-CRT1$Strength 
yhat=matrix(0,n,n.models) 
MSE<-matrix(0,Nrep,n.models) 

for (j in 1:Nrep) {
  Ind<-CVInd(n,K) 
  for (k in 1:K) {
    # ~. means take all predictors 
    out<-nnet(Strength~.,CRT1[-Ind[[k]],], linout=F, skip=F, size=15, decay=.01, maxit=1000, trace=F) 
    yhat[Ind[[k]],1]<-as.numeric(predict(out,CRT1[Ind[[k]],]))
    
    out<-nnet(Strength~.,CRT1[-Ind[[k]],], linout=F, skip=F, size=30, decay=0, maxit=1000, trace=F) 
    yhat[Ind[[k]],2]<-as.numeric(predict(out,CRT1[Ind[[k]],]))
    
    out<-lm(Strength~.,CRT1[-Ind[[k]],]) #compare against linear model 
    yhat[Ind[[k]],3]<-as.numeric(predict(out,CRT1[Ind[[k]],]))
  } #end of k loop
  MSE[j,]=apply(yhat,2,function(x) sum((y-x)^2))/n #still using MSE because this is regression and Y is continuous 
} #end of j loop
MSE #Mean squared error
    #            [,1]        [,2]       [,3]
    #[1,] 0.004391929 0.007264370 0.01699590
    #[2,] 0.004837410 0.008173886 0.01720605
    #[3,] 0.004448425 0.006003959 0.01698806
    #[4,] 0.005499309 0.079941243 0.01719514
MSEAve<- apply(MSE,2,mean); 
MSEAve #MINIMIZE Averaged mean square CV error = 0.004794268 0.025345864 0.017096288
MSEsd <- apply(MSE,2,sd); 
MSEsd #SD of mean square CV error = 0.0005100422 0.0364077918 0.0001205687
r2<-1-MSEAve/var(y); 
r2 #MAXIMIZE CV r^2 = 0.8893128 0.4148299 0.6052912





#---------------------------------------------------------------------------------------------------------------
# NEURAL NET: MODEL INTERPERTATION: Interpret the effects of predictors, since coeff are not interpertable in a NN
#
# ALE plot and interaction plot
#   - ALE plots work even with multicollinearity!
#   - Interaction plot has main effect subtracted out, so need to sum main effect + interaction
# 
# Main Effect Plots help us Understand: 
#   1. Which predcitors have the largest effect: see which variable has largest Y scale
#   2. What is the nature of the effect: pos, neg, linear, non linear, monotonic, etc 
#
#---------------------------------------------------------------------------------------------------------------


library(ALEPlot)

#############Visualizing the effects of the predictors#################### slide 30
nn1<-nnet(Strength~.,CRT1, linout=F, skip=F, size=15, decay=0.1, maxit=1000, trace=F) 
##From CV, these are about the best tuning parameters
summary(nn1)

## Use ALEPlot package to create accumulated local effects (ALE) plots 
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)) 
par(mfrow=c(2,4))
for (j in 1:8) {
  ALEPlot(CRT1[,1:8], nn1, pred.fun=yhat, J=j, K=50, NA.plot = TRUE) 
  rug(CRT1[,j]) 
  } ## This creates main effect ALE plots for all 8 predictors
par(mfrow=c(1,1))
#ALE PLOT Interpertation
# which predictor variables have biggest effect? largest range on y scale!!!  —> cement (x1), age, slag


#plots 2 predictors together in red/orange heat map
par(mfrow=c(2,2)) 
## This creates 2nd-order interaction ALE plots for x1, x2, x8 
ALEPlot(CRT1[,1:8], nn1, pred.fun=yhat, J=c(1,2), K=50, NA.plot = TRUE) 
ALEPlot(CRT1[,1:8], nn1, pred.fun=yhat, J=c(1,8), K=50, NA.plot = TRUE) 
ALEPlot(CRT1[,1:8], nn1, pred.fun=yhat, J=c(2,8), K=50, NA.plot = TRUE) 
par(mfrow=c(1,1))







#---------------------------------------------------------------------------------------------------------------
# Neural Nets - NN Model Comparsion with same CV Index Partition
# (Regression with linear activation function vs linear regression)
#---------------------------------------------------------------------------------------------------------------


# Prepare the data
#######R code for reading in cpus data set, taking log(response) and then converting to [0,1] interval, and standardizing predictors############## slide 37
CPUS<-read.table("Data_for_Lecture_Examples/cpus.txt",sep="\t")
CPUS1<-CPUS[2:8]
k<-ncol(CPUS1);
CPUS1[c(1:3,k)]<-sapply(CPUS1[c(1:3,k)], log10) #take log of first three predictors and response
CPUS1[1:(k-1)]<-sapply(CPUS1[1:(k-1)], function(x) (x-mean(x))/sd(x)) #standardize predictors
CPUS1[k]<-(CPUS1[k]-min(CPUS1[k]))/(max(CPUS1[k])-min(CPUS1[k])) #standardize to 0-1 scale
CPUS[1:10,]
pairs(CPUS1)

##Now use the same CV partition to compare Neural Net and linear reg models###      slide 42
Ind<-CVInd(n=nrow(CPUS1),10)
K<-length(Ind)
y<-CPUS1$perf
yhat<-y
for (k in 1:K) {
  out<-nnet(perf~.,CPUS1[- Ind[[k]],],linout=T,skip=T,size=10,decay=.05,maxit=1000,trace=F) #linear
  yhat[Ind[[k]]]<-as.numeric(predict(out,CPUS1[Ind[[k]],])) }
e1=y-yhat

#now compare to linear regression with same CV index partition 
for (k in 1:K) {
  out <- lm(perf ~ ., CPUS1[-Ind[[k]], ])
  yhat[Ind[[k]]] <- as.numeric(predict(out, CPUS1[Ind[[k]], ])) #as.numeric() because it's linear prediction
}
e2=y-yhat 
c(sd(e1),sd(e2)) #0.07878177 0.08394187














#---------------------------------------------------------------------------------------------------------------
# Neural Nets - BINARY CLASSIFICATION with LOGISTIC Activation Function
#
# MULTIRESPONSE NEURAL NETWORKS
# K response variables --> include k nodes in the output layer (all in same hidden layer)
# R automatically makes a k-length 0/1 response vector (6 classes = 6 categories)
#
# Model Evaluation: Misclass rate (instead of MSE which is used in regression)
#---------------------------------------------------------------------------------------------------------------


######Read data, convert response to binary, and standardize predictors#####    slide 57
FGL<-read.table("Data_for_Lecture_Examples/fgl.txt",sep="\t")
z<-(FGL$type == "WinF") | (FGL$type == "WinNF")
y<-as.character(FGL$type)
y[z]<-"Win"; 
y[!z]<-"Other"
FGL<-data.frame(FGL,"type_bin"=as.factor(y)) #add a binary factor response column
y[y == "Win"]<-1;
y[y == "Other"]<-0;
FGL<-data.frame(FGL,"type01"=as.numeric(y)) #also add a binary numeric response column
FGL1<-FGL
k<-ncol(FGL)-3; #don't need to standardize the categorical output
FGL1[1:k]<-sapply(FGL1[1:k], function(x) (x-mean(x))/sd(x)) #standardize predictors FGL



#############Fit a neural network classification model to the FGL1 data######   slide 61
#knows it’s binary by looking at response (if it is a factor: as.factor())  
library(nnet)
fgl.nn1<-nnet(type_bin~., FGL1[,c(1:9,11)], linout=F, skip=F, size=10, decay=.05, maxit=1000, trace=F)
#for classification you cannot use linear, have to use logistic…. linout =F 
phat<-as.numeric(predict(fgl.nn1)) #predicted probabilities - vector same length as training rows 
#and each output is predicted prob that the response = 1 for that row 
y<-FGL1[[12]] 
yhat<-as.numeric(phat >= 0.5) #classify as 1 if predicted probability >= 0.5  - threshold .5, if >.5 goes to 1
sum(y != yhat)/length(y) #MISCLASSIFICAION rate = 0.01869159
#decrease the decay and nodes will go outward, less misclassification rate
summary(fgl.nn1)
plot(phat,jitter(y,0.05))


#######A function to determine the indices in a CV partition################## slide 41
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
  Ind 
}


##Now use multiple reps of CV to compare Neural Nets and logistic reg models###       slide 63
Nrep<-20 #number of replicates of CV
K<-3 #K-fold CV on each replicate
n.models = 3 #number of different models to fit
n=nrow(FGL1)
y<-FGL1[[12]] 
yhat=matrix(0,n,n.models) 
CV.rate<-matrix(0,Nrep,n.models) 
for (j in 1:Nrep) {
  Ind<-CVInd(n,K) 
  for (k in 1:K) {
    out<-nnet(type_bin~.,FGL1[-Ind[[k]],c(1:9,11)],linout=F,skip=F,size=10,decay=.3, maxit=1000,trace=F) 
    phat<-as.numeric(predict(out,FGL1[Ind[[k]],c(1:9,11)])); 
    yhat[Ind[[k]],1]<-as.numeric(phat >= 0.5) 
    
    out<-nnet(type_bin~.,FGL1[-Ind[[k]],c(1:9,11)],linout=F,skip=F,size=10,decay=0, maxit=1000,trace=F) 
    phat<-as.numeric(predict(out,FGL1[Ind[[k]],c(1:9,11)])); 
    yhat[Ind[[k]],2]<-as.numeric(phat >= 0.5) 
    
    out<-glm(type_bin~.,FGL1[-Ind[[k]],c(1:9,11)],family=binomial(link="logit")) 
    phat<-as.numeric(predict(out,FGL1[Ind[[k]],c(1:9,11)],type="response")); 
    yhat[Ind[[k]],3]<-as.numeric(phat >= 0.5)
    
  } #end of k loop
  CV.rate[j,]=apply(yhat,2,function(x) sum(y != x)/n)
} #end of j loop
CV.rate
CV.rateAve<- apply(CV.rate,2,mean); 
CV.rateAve #averaged CV misclass rate =  0.1563084 0.1946262 0.1700935
CV.rateSD <- apply(CV.rate,2,sd); 
CV.rateSD #SD of CV misclass rate =  0.01108664 0.01901316 0.00975494


#############Visualizing the effects of the predictors####################   slide 66
fgl.nn1<-nnet(type_bin~., FGL1[,c(1:9,11)], linout=F, skip=F, size=10, decay=.3, maxit=1000, trace=F) ##From CV, these are about the best tuning parameters
summary(fgl.nn1)

## Use ALEPlot package to create accumulated local effects (ALE) plots - check for Significance looking at y axis 
library(ALEPlot)
yhat <- function(X.model, newdata) {p.hat = as.numeric(predict(X.model, newdata, type="raw")); log(p.hat/(1-p.hat))} ## to plot the log-odds
par(mfrow=c(3,3))
for (j in 1:9) {ALEPlot(FGL1[,1:9], fgl.nn1, pred.fun=yhat, J=j, K=50, NA.plot = TRUE)
  rug(FGL1[,j]) } ## This creates main effect ALE plots for all 9 predictors par(mfrow=c(1,1))

par(mfrow=c(2,2)) ## This creates 2nd-order interaction ALE plots for x1, x3, x7 
ALEPlot(FGL1[,1:9], fgl.nn1, pred.fun=yhat, J=c(1,3), K=50, NA.plot = TRUE) 
ALEPlot(FGL1[,1:9], fgl.nn1, pred.fun=yhat, J=c(1,7), K=50, NA.plot = TRUE) 
ALEPlot(FGL1[,1:9], fgl.nn1, pred.fun=yhat, J=c(3,7), K=50, NA.plot = TRUE) 
#ALEPlot(FGL1[,1:9], fgl.nn1, pred.fun=yhat, J=c(2,7), K=50, NA.plot = TRUE) 
par(mfrow=c(1,1))



#---------------------------------------------------------------------------------------------------------------
# NN K Class Classification with Logistic Activation Function
#
# as.factor(Y) so that it knows it's a categorical variable 
#---------------------------------------------------------------------------------------------------------------


#############Same, but use the original 6-category response######   slide 69
library(nnet)
FGL1<-read.table("Data_for_Lecture_Examples/fgl.txt",sep="\t")
FGL1$type <- as.factor(FGL1$type)
fgl.nn1<- nnet(type~.,FGL1[,c(1:10)],linout=F,skip=F,size=10,decay=5,maxit=1000,trace=F)
##output the class probabilities
phat<-predict(fgl.nn1,type="raw")
phat[1:20,]
apply(phat,1,sum) #you can see that the 6 predicted class probabilities sum to 1.0 ##output the class with the largest class probability 
yhat<-predict(fgl.nn1,type="class")
yhat
y<-FGL1$type
sum(y != yhat)/length(y) #training misclassification rate =  0.08411215



##Now use multiple reps of CV to compare Neural Nets and logistic reg models###       slide 63
Nrep<-20 #number of replicates of CV
K<-3 #K-fold CV on each replicate
n.models = 3 #number of different models to fit
n=nrow(FGL1)
y<-FGL1[[10]] 
yhat=matrix(0,n,n.models) 
CV.rate<-matrix(0,Nrep,n.models) 
for (j in 1:Nrep) {
  Ind<-CVInd(n,K) 
  for (k in 1:K) {
    out<-nnet(type~.,FGL1[-Ind[[k]],],linout=F,skip=F,size=10,decay=.05,maxit=1000,trace=F)
    yhat[Ind[[k]],1]<-predict(out,FGL1[Ind[[k]],],type="class") 


    #out<-glm(type_bin~.,FGL1[-Ind[[k]],c(1:9,11)],family=binomial(link="logit")) 
   # phat<-as.numeric(predict(out,FGL1[Ind[[k]],c(1:9,11)],type="response")); 
    #yhat[Ind[[k]],2]<-as.numeric(phat >= 0.5)
    
  } #end of k loop
  CV.rate[j,]=apply(yhat,2,function(x) sum(y != yhat)/length(y))
} #end of j loop
CV.rate
CV.rateAve<- apply(CV.rate,2,mean); 
CV.rateAve #averaged CV misclass rate =  0.1563084 0.1946262 0.1700935
CV.rateSD <- apply(CV.rate,2,sd); 
CV.rateSD #SD of CV misclass rate =  0.01108664 0.01901316 0.00975494



#---------------------------------------------------------------------------------------------------------------
# Neural Nets - CLASSIFICATION of NUMERICAL CONVERTED TO BINARY
#---------------------------------------------------------------------------------------------------------------

#slide 71

XX<-read.table("Data_for_Lecture_Examples/adult_train.csv",sep=",",header=TRUE,strip.white=TRUE,na.strings="?")
XX<-na.omit(XX) #remove NA's
INCOME<-XX
INCOME[,c(1,5,11,12,13)]<-scale(INCOME[,c(1,5,11,12,13)]) #standardize the continuous variables
library(nnet)
Inc.nn1<-nnet(income ~ . ,INCOME[,-c(3,4)], linout=F, skip=F, size=10, decay=5, maxit=100, trace=F) #logistic
y<-INCOME$income #binary
phat<-predict(Inc.nn1, type="raw") #vector of predicted probabilities 
yhat<-predict(Inc.nn1, type="class") #vector of predicted classes 
sum(y != yhat)/length(y) #training misclassification rate  = 0.1339102
plot(phat,jitter(as.numeric(y==">50K"), 1.5))

##compare to a logistic regression model
Inc.glm <- glm(income ~ . ,family = binomial(link = "logit"), data=INCOME[,-c(3,4)]) 
phat <- predict(Inc.glm, type="response")
summary(Inc.glm)
sum((y == ">50K") != (phat > 0.5))/length(y) #0.1510841


#checking for multicollineary    slide 77 
X<-read.table("Data_for_Lecture_Examples/barstock.csv",sep=",",header=T) 
R<-cor(X[2:5]); 
round(R,3)
pairs(X)
round(R,3)

#Linear regression     slide 78
out<-lm(weight~.,data=X) 
summary(out)

attach(X); 
plot(height+width+length,volume); 
detach(X) 
VIF<-diag(solve(R))
sqrt(VIF)




library(MASS) #needed for ridge regression     slide 84
X<-read.table("Data_for_Lecture_Examples/barstock.csv",sep=",",header=T)
X1<-sapply(X, function(x) (x-mean(x))/sd(x)) #standardize predictors and response 
X1<-data.frame(X1)
out<-lm(weight~. -1,data=X1);
summary(out)

lambda<-vector(mode="numeric",length=20)
for (i in 1:20) lambda[21-i]<-1/(1.5^(i-1)) 
outridge<-lm.ridge(weight~. -1,data=X1, lambda=lambda) 
plot(outridge); 
select(outridge)


####### Ridge Regression CV        slide 86
n=30;
K=30; 
Ind<-CVInd(n=n,K=K) 
y<-X1[[1]]
yhat<-y
for (k in 1:K) {
  out<-lm.ridge(weight~. -1,data=X1[-Ind[[k]],], lambda=0.001)
  yhat[Ind[[k]]]<-as.matrix(X1[Ind[[k]],2:5])%*%out$coef 
  }
CVSSE1 = sum((y-yhat)^2) #CV SSE
#now compare to a different shrinkage parameter
yhat<-y
for (k in 1:K) {
  out<-lm.ridge(weight~. -1,data=X1[-Ind[[k]],], lambda=0.667)
  yhat[Ind[[k]]]<-as.matrix(X1[Ind[[k]],2:5])%*%out$coef 
}
CVSSE2 = sum((y-yhat)^2) #CV SSE 
c(CVSSE1,CVSSE2) #  2.068019 2.075369










