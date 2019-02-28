
#---------------------------------------------------------------------------------------------------------------
# K Nearest Neighbor - Flexible, nonlinear tool. Fits training data, no model 
#
# When to use it: Good for data with large n, small k, and no categorical predictors 
#                 Bias / variance trade off - using large k vs small k. Low bias will fall apart when you fit new data. You see isolated pockets in data. 
#                               High bias will have low variance but consistently be higher and/or lower than the mean. Smoother classificaiton boundary. 
# 
# Hot it works:
#   0. Standardize - because need to calculate distances on same scale 
#   1. find closest neighbor to xi using min distance between any point x (new point you want to predict at) and predictors for ith row 
#           = norm || x- xi ||
#           = which is sum of squares of differences element by element of vector  
#   2. take predicted Y to be the response value for that training observation 
#---------------------------------------------------------------------------------------------------------------


# Illustration of K-NN for Gas Mileage data        slide 4
library(scatterplot3d)
library(rgl)
GAS<-read.csv("Data_for_Lecture_Examples/Gas_Mileage.csv",header=TRUE)
GAS1<-GAS
GAS1[,2:12]<-sapply(GAS1[,2:12], function(x) (x-mean(x[!is.na(x)]))/sd(x[!is.na(x)])) 
GAS[1:10,]
attach(GAS1)
GAS1[c(1,2,6)]
plot3d(Displacement,Rear_axle_ratio,Mpg)
###
plot(Displacement,Rear_axle_ratio,type="p")
###
identify(Displacement,Rear_axle_ratio) #click on bullet in scatter plot to see which row it is




#slide 13

library(yaImpute)
CRT<-read.csv("Data_for_Lecture_Examples/concrete.csv", header=TRUE)
CRT1<-CRT
CRT1[1:8]<-sapply(CRT1[1:8], function(x) (x-mean(x))/sd(x)) #standardize predictors 
CRT1[9]<-(CRT1[9]-min(CRT1[9]))/(max(CRT1[9])-min(CRT1[9])) 
train<-as.matrix(CRT1[,1:8]) #training (x's) vs test set array
test<-as.matrix(CRT1[,1:8]) #here it's the same, so like looking at training R^2. For each row in test array, it will find rows in test that are closest
ytrain<-CRT1[,9] #column 9 = response
ytest<-CRT1[,9]
K=3
out<-ann(train,test,K) #approximate nearest neighbors 
ind<-as.matrix(out$knnIndexDist[,1:K])
D<-as.matrix(out$knnIndexDist[,(1+K):(2*K)])
fit<-apply(ind,1,function(x) mean(ytrain[x]))
plot(fit,ytest)
1-var(ytest-fit)/var(ytest) #TRAINING R^2 = 0.8755489

out$knnIndexDist[,1:K] #however many rows in test set - index distance matrix
#fist 3 columns are indexes of 3 closest training data points



#---------------------------------------------------------------------------------------------------------------
# CROSS VALIDATION 
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


#slide 15

Nrep<-50 #number of replicates of CV
K<-10 #K-fold CV on each replicate
n.models = 2 #number of different models to fit 
n=nrow(CRT1)
y<-CRT1$Strength 
yhat=matrix(0,n,n.models) 
MSE<-matrix(0,Nrep,n.models) 
for (j in 1:Nrep) {
  Ind<-CVInd(n,K) 
  
  for (k in 1:K) {
    train<-as.matrix(CRT1[-Ind[[k]],1:8]) 
    test<-as.matrix(CRT1[Ind[[k]],1:8]) 
    ytrain<-CRT1[-Ind[[k]],9]
    
    #y versus yhat for CV with different K
    K1=3; 
    K2=2
    
    out<-ann(train,test,K1,verbose=F) 
    ind<-as.matrix(out$knnIndexDist[,1:K1]) #k1
    yhat[Ind[[k]],1]<-apply(ind,1,function(x) mean(ytrain[x])) 
    
    out<-ann(train,test,K2,verbose=F) 
    ind<-as.matrix(out$knnIndexDist[,1:K2]) #k2
    yhat[Ind[[k]],2]<-apply(ind,1,function(x) mean(ytrain[x]))
    
  } #end of k loop
  MSE[j,]=apply(yhat,2,function(x) sum((y-x)^2))/n
} #end of j loop
MSEAve<- apply(MSE,2,mean); 
MSEAve #averaged mean square CV error 
MSEsd <- apply(MSE,2,sd); 
MSEsd #SD of mean square CV error 
r2<-1-MSEAve/var(y); 
r2 #CV r^2
plot(yhat[,2],y)



#slide 18

CPUS<-read.table("Data_for_Lecture_Examples/cpus.txt",sep="\t")
CPUS1<-CPUS[2:8]
CPUS1[c(1:3,7)]<-sapply(CPUS1[c(1:3,7)], log10) #take log of first three predictors and response
CPUS1[1:6]<-sapply(CPUS1[1:6], function(x) (x-mean(x))/sd(x)) #standardize predictors 
CPUS1[7]<-(CPUS1[7]-min(CPUS1[7]))/(max(CPUS1[7])-min(CPUS1[7])) 
train<-as.matrix(CPUS1[,1:6])
test<-as.matrix(CPUS1[,1:6])
ytrain<-CPUS1[,7]
ytest<-CPUS1[,7]
K=6
out<-ann(train,test,K) 
ind<-as.matrix(out$knnIndexDist[,1:K]) 
D<-as.matrix(out$knnIndexDist[,(1+K):(2*K)]) 
fit<-apply(ind,1,function(x) mean(ytrain[x])) 
plot(fit,ytest)
1-var(ytest-fit)/var(ytest) # 0.8750195



#CV to choose best K     slide 20
Nrep<-10 #number of replicates of CV 
K<-10 #K-fold CV on each replicate 
n=nrow(CPUS1)
y<-CPUS1$perf 
SSE<-matrix(0,Nrep,2)
for (j in 1:Nrep) { 
  Ind<-CVInd(n,K) 
  yhat1<-y; 
  yhat2<-y;
for (k in 1:K) {
  train<-as.matrix(CPUS1[-Ind[[k]],1:6]) 
  test<-as.matrix(CPUS1[Ind[[k]],1:6]) 
  ytrain<-CPUS1[-Ind[[k]],7]
  
  K1=2;
  K2=6
  
  out<-ann(train,test,K1,verbose=F) 
  ind<-as.matrix(out$knnIndexDist[,1:K1]) 
  yhat1[Ind[[k]]]<-apply(ind,1,function(x) mean(ytrain[x])) 
  
  out<-ann(train,test,K2,verbose=F) 
  ind<-as.matrix(out$knnIndexDist[,1:K2]) 
  yhat2[Ind[[k]]]<-apply(ind,1,function(x) mean(ytrain[x]))
} #end of k loop
SSE[j,]=c(sum((y-yhat1)^2),sum((y-yhat2)^2)) } #end of j loop
SSE
SSEAve<-apply(SSE,2,mean);
SSEAve 
plot(yhat2,y)
1-SSEAve/n/var(y)



#---------------------------------------------------------------------------------------------------------------
# K Nearest Neighbor - CLASSIFICATION EXAMPLE
#---------------------------------------------------------------------------------------------------------------


FGL<-read.table("Data_for_Lecture_Examples/fgl.txt",sep="\t")
z<-(FGL$type == "WinF") | (FGL$type == "WinNF")
y<-as.character(FGL$type)
y[z]<-"Win"; y[!z]<-"Other"
FGL<-data.frame(FGL,"type_bin"=as.factor(y)) #add a binary factor response column
y[y == "Win"]<-1;
y[y == "Other"]<-0;
FGL<-data.frame(FGL,"type01"=as.numeric(y)) #also add a binary numeric response column 
FGL1<-FGL
FGL1[1:9]<-sapply(FGL1[1:9], function(x) (x-mean(x))/sd(x)) #standardize predictors 
train<-as.matrix(FGL1[,1:9]); 
test<-as.matrix(FGL1[,1:9])
ytrain<-FGL1[,11]; 
ytest<-FGL1[,11]
K=5
out<-ann(train,test,K)
ind<-as.matrix(out$knnIndexDist[,1:K])
phat<-apply(ind,1,function(x) sum(ytrain[x]=="Win")/length(ytrain[x])) 
plot(phat,jitter(as.numeric(ytest=="Win"),amount=.05))
####can alternatively use the following
library(class)
out<-knn(train, test, ytrain, k = 10, prob = T)
