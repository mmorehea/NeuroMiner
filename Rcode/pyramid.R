library(randomForest)

read.csv("./raw/first_subsets/pyramidal_appended.csv",T)->x
read.csv("./raw/first_subsets/pyramidal_appended.csv",T)->x1 #extra full set for reference
x[1213,64] <- 3477.808 #average total volume for minke whale
 #take out columns with many NA
x$Fractal_Dim <- as.numeric(x$Fractal_Dim)
x$Soma.Surface<-as.numeric(x$Soma.Surface)
colnames(x)[c(36,54,55,56,57,62,65,66,76,78, 83:91)]
x <- x[,-c(36,54,55,56,57,62,65,66,76,78, 83:91)]
nx1<-33:96
nx2<-78:161
ny<-4
dat1 <- data.frame(y=as.factor(x[,ny]),x[,nx1])
#dat<-data.frame(y=as.factor(x[,ny]),x[,c(nx1,nx2)])
dat1<-na.omit(dat1)
coll <- vector(length = dim(x)[1])
 for(i in 1:dim(x)[1])
 coll[i] <- sum(is.na(x[i, 1:96]))
nxx1<-2:65
nxx2<-67:dim(dat)[2]


set.seed(100)
g1<-randomForest(y~.,data=dat1)##,prox=TRUE)
cols=rainbow(length(unique(dat1[,1]))+2)
matplot(g1$err.rate,lwd=2,col=cols,lty=1,type="l", main = "Species Error Rate", ylab = "Error rate", xlab = "Number of Trees")
##MAP cols to dimnames(g$err.rate)[[2]]
varImpPlot(g1, pch = 16, main = "Variable Importance")
sort(g1$confusion[,15])

#take out proechimys
es <- which(dat1[,1] == "proechimys")
datpro <- dat1[-es,]
datpro$y <- factor(datpro$y)#takes out extra level
gpro<-randomForest(y~.,data=datpro)##,prox=TRUE)
cols=rainbow(length(unique(datpro[,1]))+2)
matplot(gpro$err.rate,lwd=2,col=cols,lty=1,type="l")
##MAP cols to dimnames(g$err.rate)[[2]]
varImpPlot(gpro)
sort(gpro$confusion[,14])

#take out proechimys and minke whale
es1 <- which(datpro[,1] == "minke whale")
datmw <- datpro[-es1,]
datmw$y <- factor(datmw$y)
gmw <- randomForest(y~.,data=datmw)##,prox=TRUE)
cols=rainbow(length(unique(datmw[,1]))+2)
matplot(gmw$err.rate,lwd=2,col=cols,lty=1,type="l")
##MAP cols to dimnames(g$err.rate)[[2]]
varImpPlot(gmw)
sort(gmw$confusion[,13])
#humpback whale much better, cat and guinea pig still have exact same error

#Take out proechimys, cat, guinea pig, minke whale
es2 <- which(datmw[,1] == "cat")
es3 <- which(datmw[,1] == "guinea pig")
dat4 <- datmw[-c(es2,es3),]
dat4$y <- factor(dat4$y)
g4 <- randomForest(y~.,data=dat4)##,prox=TRUE)
cols2=rainbow(length(unique(dat4[,1]))+2)
matplot(g4$err.rate,lwd=2,col=cols,lty=1,type="l")
##MAP cols to dimnames(g4$err.rate)[[2]]
varImpPlot(g4)
sort(g4$confusion[,11])
#bottlenose dolphin still high but all below 0.5 now
#taking out predictors with collinearity issue 

#using sholls
set.seed(100)
g2<-randomForest(y~.,data=dat[,c(1,nxx2)])##,prox=TRUE)
cols=rainbow(length(unique(dat[,1]))+2)
matplot(g2$err.rate,lwd=2,col=cols,lty=1,type="l")
##MAP cols to dimnames(g$err.rate)[[2]]
varImpPlot(g2)


set.seed(100)
g3<-randomForest(y~.,data=dat[,-66])##,prox=TRUE)
cols=rainbow(length(unique(dat[,1]))+2)
matplot(g3$err.rate,lwd=2,col=cols,lty=1,type="l")
##MAP cols to dimnames(g$err.rate)[[2]]
varImpPlot(g3)
