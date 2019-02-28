#Naomi Kaduwela
#Feb 13
#Predictive Analytics 2 - Midterm Notes

rm(list = ls())


# 1. Regression Trees
# 2. Classification Trees - binary
# 3. Classification Trees - 6 category 
# 4. Take min CP value by code 

#---------------------------------------------------------------------------------------------------------------
# REGRESSION TREES
#     method = anova
#---------------------------------------------------------------------------------------------------------------

CRT <- read.csv("Data_for_Lecture_Examples/concrete.csv",header=TRUE)

#do not have to standardize or transform predictors to fit trees       slide 103
library(rpart)
control <- rpart.control(minbucket = 5, cp = 0.0001, maxsurrogate = 0, usesurrogate = 0, xval = 10)
CRT.tr <- rpart(Strength ~ .,CRT, method = "anova", control = control) #method = anova
plotcp(CRT.tr) #plot of CV r^2 vs. size
printcp(CRT.tr) #same info is in CRT.tr$cptable
#prune back to optimal size, according to plot of CV 1-r^2
CRT.tr1 <- prune(CRT.tr, cp=0.0015) #approximately the best size pruned tree 
CRT.tr1$variable.importance
CRT.tr1$cptable[nrow(CRT.tr1$cptable),] #shows training and CV 1-r^2, and other things #prune and plot a little smaller tree than the optimal one, just for display
CRT.tr2 <- prune(CRT.tr, cp=0.007) #bigger cp gives smaller size tree CRT.tr2
par(cex=.9); plot(CRT.tr2, uniform=F); text(CRT.tr2, use.n = T); par(cex=1) ##
yhat<-predict(CRT.tr1); 
e<-CRT$Strength-yhat
c(1-var(e)/var(CRT$Strength), 1-CRT.tr1$cptable[nrow(CRT.tr1$cptable),3]) #check to see training r^2 agrees with what is in cptable

#---

#slide 131

XX<-read.table("Data_for_Lecture_Examples/adult_train.csv",sep=",",header=TRUE,strip.white=TRUE,na.strings="?")
XX<-na.omit(XX)
INCOME<-XX #there is no need to standardize the predictors with trees (why not)
control <- rpart.control(minbucket = 20, cp = 0.0001, maxsurrogate = 0, usesurrogate = 0, xval = 10)
INC.tr <- rpart(hours.per.week ~ ., INCOME[,-c(3,4)], method = "anova", control = control) 
plotcp(INC.tr)
printcp(INC.tr)
#prune back to optimal size, according to plot of CV r^2
INC.tr1 <- prune(INC.tr, cp=0.001) #approximately the cp corresponding to the best size INC.tr1
par(cex=.9); 
plot(INC.tr1, uniform=F); 
text(INC.tr1, use.n = F); 
par(cex=1) 
INC.tr1$variable.importance
INC.tr1$cptable[nrow(INC.tr1$cptable),] #shows training and CV r^2, and other things




#---------------------------------------------------------------------------------------------------------------
# REGRESSION TREES - using TREE package (use rpart instead) 
#---------------------------------------------------------------------------------------------------------------

CPUS<-read.table("Data_for_Lecture_Examples/cpus.txt",sep="\t")

#do not have to standardize or transform predictors to fit trees    slide 109
library(tree)
control = tree.control(nobs=nrow(CPUS), mincut = 5, minsize = 10, mindev = 0.002)
#default is mindev = 0.01, which only gives a 10-node tree
cpus.tr <- tree(log10(perf) ~ .,CPUS[2:8],control=control) 
cpus.tr
summary(cpus.tr)
plot(cpus.tr,type="u"); 
text(cpus.tr,digits=2) #type="p" plots proportional branch lengths 
######now prune tree and plot deviance vs. complexity parameter 
cpus.tr1<-prune.tree(cpus.tr)
plot(cpus.tr1)
######now plot CV deviance vs complexity parameter
plot(cv.tree(cpus.tr, , prune.tree))
######now find the final tree with the best value of complexity parameter cpus.tr1<-prune.tree(cpus.tr, k=0.4) #can replace replace argument “k=0.4” by “best=11” cpus.tr1
plot(cpus.tr1,type="u");text(cpus.tr1,digits=3)




#---------------------------------------------------------------------------------------------------------------
# CLASSIFICATION TREES - BINARY
#     method = class
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

#need to get other FGL code to convert to binary 

#slide 116
library(rpart)
control <- rpart.control(minbucket = 1, cp = 0.001, maxsurrogate = 0, usesurrogate = 0, xval = 10)
FGL.tr <- rpart(type_bin~., FGL[,c(1:9,11)], method = "class", control = control) #method = class
plotcp(FGL.tr)
printcp(FGL.tr) #same info in FGL.tr$cptable

#prune to optimal size
FGL.tr1 <- prune(FGL.tr, cp=0.055) #approximately the cp corresponding to the optimal size
FGL.tr1
par(cex=1); plot(FGL.tr1, uniform=F); text(FGL.tr1, use.n = F); par(cex=1)
FGL.tr1$variable.importance
FGL.tr1$cptable[nrow(FGL.tr1$cptable),]

#calculate training and CV misclass rates
FGL.tr1$cptable[nrow(FGL.tr1$cptable),c(3,4)]*min(table(FGL$type_bin)/nrow(FGL)) #training and cv misclass rates
yhat<-predict(FGL.tr1, type="class")
sum(yhat != FGL$type_bin)/nrow(FGL) #check the training misclass rate



#---------------------------------------------------------------------------------------------------------------
# CLASSIFICATION TREES - 6 category resposne 
#     method = class
#---------------------------------------------------------------------------------------------------------------

# slide 120 

FGL<-read.table("Data_for_Lecture_Examples/fgl.txt",sep="\t")
library(rpart)
control <- rpart.control(minbucket = 1, cp = 0.00001, maxsurrogate = 0, usesurrogate = 0, xval = 10)
FGL.tr <- rpart(type~.,FGL[,c(1:10)], method = "class", control = control) 
plotcp(FGL.tr)
printcp(FGL.tr) #same info in FGL.tr$cptable
#prune to optimal size
FGL.tr1 <- prune(FGL.tr, cp=0.01) #approximately the cp corresponding to the optimal size
FGL.tr1
par(cex=1); plot(FGL.tr1, uniform=F); 
text(FGL.tr1, use.n = F); 
par(cex=1) 
FGL.tr1$variable.importance
FGL.tr1$cptable[nrow(FGL.tr1$cptable),]
#see what the predicted class probabilities are
yhat<-predict(FGL.tr1, type="prob")
yhat[1:20,]


#---------------------------------------------------------------------------------------------------------------

#take min cp value
min_rel_error_position <- which.min(tr_fit$cptable[,3]) # find position of minimum relative error
cp_value <- tr_fit$cptable[min_rel_error_position,1] # find cp corresponding to minimum relative error
