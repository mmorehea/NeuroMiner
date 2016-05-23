############################################################################
##  Purpose: Variable Importance
############################################################################


library(spa)
library(randomForest)
library(rpart)
library(pls)

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
nu=c(0.2,0.5,0.8)
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
    
    grf<-randomForest(y~.,data=data.frame(y=as.factor(y),x)[L,])
    tab=table(predict(grf,newdata=x)[U],y[U])
    v1<-diag(tab)/ apply(tab,2,sum)
    teacc[k3,2]<-sum(diag(tab))/sum(tab)
    tesen[k3,2]<-v1[2]
    tespec[k3,2]<-v1[1]

    gpls<-plsr(y~.,data=data.frame(y,x)[L,])
    tab=table(predict(gpls,newdata=x,ncomp=2)[U]>0.5,y[U])
    v1<-diag(tab)/ apply(tab,2,sum)
    teacc[k3,3]<-sum(diag(tab))/sum(tab)
    tesen[k3,3]<-v1[2]
    tespec[k3,3]<-v1[1]
   
    
    cat("nu=",nu[j],"  k3=",k3,"  time=",(proc.time()-t1)/60,"\n")
    k3<-k3+1
  }
  write.table(teacc,"te-acc_bio.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  write.table(tespec,"te-spec_bio.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  write.table(tesen,"te-sen_bio.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
}

##Chem

set.seed(020) ##so RF gets same result as in paper
nu=c(0.2,0.5,0.8)
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

    grf<-randomForest(y~.,data=data.frame(y=as.factor(y),z)[L,])
    tab=table(predict(grf,newdata=z)[U],y[U])
    v1<-diag(tab)/ apply(tab,2,sum)
    teacc[k3,2]<-sum(diag(tab))/sum(tab)
    tesen[k3,2]<-v1[2]
    tespec[k3,2]<-v1[1]

    gpls<-plsr(y~.,data=data.frame(y,z)[L,])
    tab=table(predict(gpls,newdata=z,ncomp=2)[U]>0.5,y[U])
    v1<-diag(tab)/ apply(tab,2,sum)
    teacc[k3,3]<-sum(diag(tab))/sum(tab)
    tesen[k3,3]<-v1[2]
    tespec[k3,3]<-v1[1]
     
    cat("nu=",nu[j],"  k3=",k3,"  time=",(proc.time()-t1)/60,"\n")
    k3<-k3+1
  }
   write.table(teacc,"te-acc_chem.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  write.table(tespec,"te-spec_chem.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  write.table(tesen,"te-sen_chem.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
}


##Chem+BIO

set.seed(020) ##so RF gets same result as in paper
nu=c(0.2,0.5,0.8)
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

    grf<-randomForest(y~.,data=data.frame(y=as.factor(y),dat)[L,])
    tab=table(predict(grf,newdata=dat)[U],y[U])
    v1<-diag(tab)/ apply(tab,2,sum)
    teacc[k3,2]<-sum(diag(tab))/sum(tab)
    tesen[k3,2]<-v1[2]
    tespec[k3,2]<-v1[1]

    gpls<-plsr(y~.,data=data.frame(y,dat)[L,])
    tab=table(predict(gpls,newdata=dat,ncomp=2)[U]>0.5,y[U])
    v1<-diag(tab)/ apply(tab,2,sum)
    teacc[k3,3]<-sum(diag(tab))/sum(tab)
    tesen[k3,3]<-v1[2]
    tespec[k3,3]<-v1[1]
    
     cat("nu=",nu[j],"  k3=",k3,"  time=",(proc.time()-t1)/60,"\n")
    k3<-k3+1
  }
  write.table(teacc,"te-acc_both.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  write.table(tespec,"te-spec_both.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  write.table(tesen,"te-sen_both.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
}

