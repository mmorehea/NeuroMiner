############################################################################
##  Purpose: Variable Importance
############################################################################


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
nu=c(0.2,0.8)
lnu=length(nu)
noise=c(0.00,0.05,0.1)
lno=3
L1=50

acc=matrix(0,nrow=L1*lnu,ncol=lno*2+1)
 prior<-as.vector(table(y)/n)


k3<-1
t1<-proc.time()
ytr<-rep(0,n)

for(j in 1:lnu){
  for(i in 1:L1){
    L=sample(1:n,ceiling(nu[j]*n))
    U=setdiff(1:n,L)
    acc[k3,1]<-nu[j]
    
    j12<-lno+2
    nvec<-lno
    for(i1 in 1:lno){
      ytr<-y
      ind1<-sample(L[which(y[L]==0)],prior[1]*ceiling(noise[i1]*length(L)))
      ind2<-sample(L[which(y[L]==1)],prior[2]*ceiling(noise[i1]*length(L)))
      ind1<-c(ind1,ind2)
      if(length(ind1)>1)
        ytr[ind1]=1-ytr[ind1]
      
      grf<-randomForest(y~.,data=data.frame(y=as.factor(ytr),x)[L,])
      tab=table(predict(grf,newdata=x)[U],y[U])
      acc[k3,i1+1]<-sum(diag(tab))/sum(tab)
  
      gpls<-plsr(y~.,data=data.frame(y=ytr,x)[L,])
      tab=table(predict(gpls,newdata=x,ncomp=2)[U]>0.5,y[U])
      acc[k3,j12]<-sum(diag(tab))/sum(tab)
      j12<-j12+1
    }
    k3<-k3+1
  }
  write.table(acc,"acc_noise_bio.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  cat("nu=",nu[j],"  k3=",k3,"  time=",(proc.time()-t1)/60,"\n")
}

##Chem

set.seed(020) ##so RF gets same result as in paper
nu=c(0.2,0.8)
lnu=length(nu)
noise=c(0.00,0.05,0.1)
lno=3
L1=50

acc=matrix(0,nrow=L1*lnu,ncol=lno*2+1)
 prior<-as.vector(table(y)/n)


k3<-1
t1<-proc.time()
ytr<-rep(0,n)

for(j in 1:lnu){
  for(i in 1:L1){
    L=sample(1:n,ceiling(nu[j]*n))
    U=setdiff(1:n,L)
    acc[k3,1]<-nu[j]
    
    j12<-lno+2
    nvec<-lno
    for(i1 in 1:lno){
      ytr<-y
      ind1<-sample(L[which(y[L]==0)],prior[1]*ceiling(noise[i1]*length(L)))
      ind2<-sample(L[which(y[L]==1)],prior[2]*ceiling(noise[i1]*length(L)))
      ind1<-c(ind1,ind2)
      if(length(ind1)>1)
        ytr[ind1]=1-ytr[ind1]
      
      grf<-randomForest(y~.,data=data.frame(y=as.factor(ytr),z)[L,])
      tab=table(predict(grf,newdata=z)[U],y[U])
      acc[k3,i1+1]<-sum(diag(tab))/sum(tab)
  
      gpls<-plsr(y~.,data=data.frame(y=ytr,z)[L,])
      tab=table(predict(gpls,newdata=z,ncomp=2)[U]>0.5,y[U])
      acc[k3,j12]<-sum(diag(tab))/sum(tab)
      j12<-j12+1
    }
    k3<-k3+1
  }
  write.table(acc,"acc_noise_chem.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  cat("nu=",nu[j],"  k3=",k3,"  time=",(proc.time()-t1)/60,"\n")
}

##Boi+chem

set.seed(020) ##so RF gets same result as in paper
nu=c(0.2,0.8)
lnu=length(nu)
noise=c(0.00,0.05,0.1)
lno=3
L1=50

acc=matrix(0,nrow=L1*lnu,ncol=lno*2+1)
 prior<-as.vector(table(y)/n)


k3<-1
t1<-proc.time()
ytr<-rep(0,n)

for(j in 1:lnu){
  for(i in 1:L1){
    L=sample(1:n,ceiling(nu[j]*n))
    U=setdiff(1:n,L)
    acc[k3,1]<-nu[j]
    
    j12<-lno+2
    nvec<-lno
    for(i1 in 1:lno){
      ytr<-y
      ind1<-sample(L[which(y[L]==0)],prior[1]*ceiling(noise[i1]*length(L)))
      ind2<-sample(L[which(y[L]==1)],prior[2]*ceiling(noise[i1]*length(L)))
      ind1<-c(ind1,ind2)
      if(length(ind1)>1)
        ytr[ind1]=1-ytr[ind1]
      
      grf<-randomForest(y~.,data=data.frame(y=as.factor(ytr),dat)[L,])
      tab=table(predict(grf,newdata=dat)[U],y[U])
      acc[k3,i1+1]<-sum(diag(tab))/sum(tab)
  
      gpls<-plsr(y~.,data=data.frame(y=ytr,dat)[L,])
      tab=table(predict(gpls,newdata=dat,ncomp=2)[U]>0.5,y[U])
      acc[k3,j12]<-sum(diag(tab))/sum(tab)
      j12<-j12+1
    }
    k3<-k3+1
  }
  write.table(acc,"acc_noise_both.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  cat("nu=",nu[j],"  k3=",k3,"  time=",(proc.time()-t1)/60,"\n")
}

