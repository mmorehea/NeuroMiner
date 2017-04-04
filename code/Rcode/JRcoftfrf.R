library(cftf)
library(ade4)
#pyramid
read.csv("./raw/first_subsets/pyramidal_appended.csv",T)->pyr
pyr[1213,64] <- 3477.808 #average total volume for minke whale

#for master data
master <- read.csv("NeuronDataMaster.csv", header = T)
#setup from previous analysis
master <- master[master$Archive.Name != "McQuiston",] #all avg. diameter of 1
master$Fractal_Dim <- as.numeric(master$Fractal_Dim)
master$Gstat.total.cable.length <- as.numeric(master$Gstat.total.cable.length)
master$Soma.Surface<-as.numeric(master$Soma.Surface)
colnames(master)[c(36,54,55,56,57,60,61,62,65,66,68,76,78, 83:91, 111, 160)]
master <- master[,-c(36,54,55,56,57,60,61,62,65,66,68,76,78, 83:91, 111, 160)]
nlmeasure <- 33:74
ngstat <- 75:88
master <- na.omit(master)
#remove sholl 1 and sholl 50
nsholl <- 89:136
s1 <- which(master[,4] == "domestic pig")
s2 <- which(master[,4] == "sheep")
both <- master[c(s1, s2),]
y <- both[,4]

#format into new dataframe
dat <- data.frame(y=factor(y),both[,c(nlmeasure,ngstat,nsholl)])
dat <- na.omit(dat)

#are there only 2 levels for response??
rm(y)
y <- dat[,1]
x <- dat[,c(2:46)]
z <- dat[,c(47:60)]
w <- dat[,c(61:dim(dat)[2])] 
lapply(z,  class)
lapply(x,  class)
apply(w, 2, class)
n <- length(y)
#make sure all numeric or integer
#check other two species combinations, make function for it?
#now for co-fitting the fits
## Set L and U  (set p)
set.seed(100)
L=sample(1:n,ceiling(n*.5))
U=setdiff(1:n,L)
y[U]=NA
crf<-cftf(x,z,y,k=5,L,U,learn="RF",type=NA)
crf1<-cftf(x,z,y,k=1,1:n,NULL,learn="RF",type=NA, local = TRUE)
#make a table (type I and type II error)
tabrf <- table(crf$model$yU,dat[,18][U])
tabrf
sum(diag(tabrf))/sum(tabrf)
kapstat(tabrf)
#plot contingency table?
table.cont(tabrf, clabel.r = .9, clabel.c = .9, col.labels = colnames(tabrf))

#can incorporate third view with scholl 
#just run the code in Rstudio??

#in pyramidal csv gstats are the last columns of master 
#use master now, get master larger
#coftfrf.R only predicting "1" when incorporating 'w'

#checking correlations
shmaster <- master[,nsholl]
gmaster <- master[,ngstat]
lmaster <- master[,nlmeasure]
lcov <- cov(lmaster)
lsh <- cov(shmaster)
D1 <- diag(1/sqrt(diag(lsh)))
R1 <- D1 %*% lsh %*% D1
dimnames(R1) <- list(names(lsh), names(lsh))
round(R1, 3)
#need canonical correlations for coftfrf?
#maybe not important
#partial/multiple correlations
library(mult)
coll <- vector(length = dim(gmaster)[2])
coll2 <- vector(length = dim(gmaster)[2])
#test
#coll <- vector(length = dim(x)[2])
#coll2 <- vector(length = dim(x)[2])

for(i in 1:dim(gmaster)[2]){
pp <- mult.corr(shmaster, gmaster[,i])
coll[i] <- pp$mult.corr
coll2[i] <- pp$p.mult
}
#???
#important to find correlation b/w binary and continuous matrix?
#cftf needs independent/orthogonal views? 
#are the lmeasure and gstat views orthogonal? Need test? 
#remember anytime with t-test with absolute value need to double p value
#3/23/17
#write an RMD file soon 

#3/28/17
#coftfrf for brain region
y <- dat[,18]
y <- factor(y)
x <- dat[,nlmeasure]
z <- dat[,c(ngstat, nsholl)]
#need to ask about coftfrf for multinomial
#also ask about coftfrf code where it says type of random forest: regression
#change type in cftf function so it does not weight 0 or 1 anymore than the other. 
