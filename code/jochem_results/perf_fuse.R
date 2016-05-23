############################################################################
##  Purpose: Variable Importance
############################################################################


library(cftf,lib.loc=".")
library(randomForest)
library(rpart)
library(pls)
library(gam)

####
## Function Definition
####
na.mean<-function(x,...){
  handle.na<-function(x){ #plug the mean into missing values
    x[is.na(x)]<-mean(x,na.rm=TRUE)
    return(x)
  }
  p<-dim(x)
  if(is.null(p)){ #special case: `x' is a vector
    return(handle.na(x))
  }
  for(i in 1:p[2]){
    if(!is.factor(x[,i]))# if `x' is a factor ignore it
      x[,i]<-handle.na(x[,i])
  }
  return(x)
}

nipal<-function(x,y,k){
  x<-scale(x,,scale=FALSE)
  y<-scale(y,,scale=FALSE)
  b<-p<-a<-u<-NULL
  for(i in 1:k){
    wt<-t(x)%*%y/as.numeric(t(y)%*%x%*%t(x)%*%y)
    tt<-x%*%wt
    pt<- t(x)%*%tt/as.numeric(t(tt)%*%tt)
    
    npt<-as.numeric(sqrt(t(pt)%*%pt))
    wn<-npt*wt
    tn<-npt*tt
    pn<-pt/npt
    bn<-as.numeric(t(tn)%*%y)/as.numeric(t(tn)%*%tn)
    x<-x-tn%*%t(pn)
    y<-y-bn*tn
    
    b<-c(b,bn)
    p<-cbind(p,pn)
    a<-cbind(a,wn)
    u<-cbind(u,tn)
  }
  dim(x)[2]->ab
  return(structure(list(b=b,p=p,a=a,u=u,d=ab,q=k),class="upls"))
}

vip<-function(obj,y,nm=NULL){
  if(class(obj)!="upls"){
    stop("Object is not of type upls")
  }
  a1<-as.vector(obj$a^2%*%cor(obj$u,y)^2 )
  if(is.null(nm)){
    a2<-1:length(a1)
    names(a1)<-paste(a2)
  }else{
    names(a1)<-nm
  }
  return(a1)##sort(a1,decreasing=TRUE))
}

####
## Read in Data
###


###
## Fit learner and make plots for each view
###

##bio

set.seed(020) ##so RF gets same result as in paper
nu=1:9/10
lnu=length(nu)
L1=50

teacc=matrix(0,nrow=L1*lnu,ncol=6)
tesen=matrix(0,nrow=L1*lnu,ncol=6)
tespec=matrix(0,nrow=L1*lnu,ncol=6)

k3<-1
t1<-proc.time()

for(j in 1:lnu){
  for(i in 1:L1){
    L=sample(1:n,ceiling(nu[j]*n))
    U=setdiff(1:n,L)
    teacc[k3,1]<-nu[j]
    tesen[k3,1]<-nu[j]
    tespec[k3,1]<-nu[j]
    ytr<-y
    ytr[U]=NA
    yval<-factor(y)
    
    crf<-cftf(x,z,ytr,k=5,L,U,learn="RF",type=1)
    v12<-factor(crf$model$yU,levels=c(0,1))
    tab=table(v12,yval[U])
    v1<-diag(tab)/ apply(tab,2,sum)
    teacc[k3,2]<-sum(diag(tab))/sum(tab)
    tesen[k3,2]<-v1[2]
    tespec[k3,2]<-v1[1]

    crf<-cftf(x,z,ytr,k=5,L,U,learn="RF",type=0)
    v12<-factor(crf$model$yU,levels=c(0,1))
    tab=table(v12,yval[U])
    v1<-diag(tab)/ apply(tab,2,sum)
    teacc[k3,3]<-sum(diag(tab))/sum(tab)
    tesen[k3,3]<-v1[2]
    tespec[k3,3]<-v1[1]
    crf<-cftf(x,z,ytr,k=5,L,U,learn="RF",type=NA)
    v12<-factor(crf$model$yU,levels=c(0,1))
    tab=table(v12,yval[U])
    v1<-diag(tab)/ apply(tab,2,sum)
    teacc[k3,4]<-sum(diag(tab))/sum(tab)
    tesen[k3,4]<-v1[2]
    tespec[k3,4]<-v1[1]


    gpls1<-plsr(y~.,data=data.frame(y,x)[L,])
    gpls2<-plsr(y~.,data=data.frame(y,z)[L,])
    dat1=cbind(gpls1$scores[,1:2],gpls2$scores[,1:2])
    g1<-gam(y~.,data=data.frame(y=y[L],x=dat1))
    dat2<-cbind(predict(gpls1,type="scores",newdata=x[U,])[,1:2],
                predict(gpls2,type="scores",newdata=z[U,])[,1:2])
    tab=table(predict(g1,newdata=data.frame(x=dat2))>0.5,y[U])
    v1<-diag(tab)/ apply(tab,2,sum)
    teacc[k3,5]<-sum(diag(tab))/sum(tab)
    tesen[k3,5]<-v1[2]
    tespec[k3,5]<-v1[1]
    k3<-k3+1
  }
  cat("nu=",nu[j],"  k3=",k3,"  time=",(proc.time()-t1)/60,"\n")
  write.table(teacc,"te-acc_fused.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  write.table(tespec,"te-spec_fused.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  write.table(tesen,"te-sen_fused.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
}
q(save="no")
