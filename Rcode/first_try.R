library(cftf)

##Load Data

data(pharm)
y=pharm$class
x=pharm$bio
z=pharm$chem
x=na.roughfix(x)  ##fix NA's
n<-length(y)

## Set L and U  (P=60)
set.seed(100)
L=sample(1:n,ceiling(n*0.6))
U=setdiff(1:n,L)
y[U]=NA

## Exectute co-FTF_1( Random Forest)  takes about 5 minutes
crf<-cftf(x,z,y,k=5,L,U,learn="RF",type=1)

## Exectute co-FTF( SVM) takes about 24 secs (ignore warnings)
csvm<-cftf(x,z,y,k=8,L,U,learn="SVM",type=NA)

## Compute Random Forest Unlabeled Error/kappa
y[U]=pharm$class[U]
tabrf=table(crf$model$yU,y[U])
sum(diag(tabrf))/sum(tabrf)
kapstat(tabrf)

## Compute SVM Unlabeled Error/kappa
tabsvm=table(csvm$model$yU,y[U])
sum(diag(tabsvm))/sum(tabsvm)
kapstat(tabsvm)

##Do Variable Importance (RF only)
crf1<-cftf(x,z,y,k=1,1:n,NULL,learn="RF",type=1,local=TRUE)

## Necessary processing for plot
nvar=15  
gx=crf1$model$gx
gz=crf1$model$gz
vecind<-crf1$model$vecind
xvar<-apply(gx$local[,vecind==1],1,sum)
zvar<-apply(gz$local[,vecind==2],1,sum)
rfv<-sort(c(xvar,zvar),dec=TRUE)[nvar:1]

## Make plot
dotchart(rfv,main=expression("Co-FTF"[1]*" with Random Foest"),
         xlab="Variable Score",cex=1.2,pch=16)
