library(randomForest)
library(calibrate)
library(gplots)
library(plyr)
library(xtable)
library(VGAM)
library(nnet)
library(e1071)
setwd("~/NeuroMiner/data_sets")
#read.csv("../first_subsets/pyramidal_appended.csv",T)->dump
read.csv("NeuronDataMaster.csv",T)->colmaster


setwd("~/NeuroMiner/data_sets/Neuron_subsets")
temp =list.files(pattern="../*.csv")
myfiles<-lapply(temp, function(x) read.csv(x,header=TRUE))
names(myfiles)<-temp


#### fxns 
#########
mkdirs <- function(fp) {
  if(!file.exists(fp)) {
    mkdirs(dirname(fp))
    dir.create(fp)
  }
}

`cftf` <- function(x,z,w,y,k=8,L,U,learn=c("RF","SVM"),type=c(NA,0,1),verbose=FALSE,...){
  ### test data
  # k=5
  #  learn="RF"
  #  type=1
  #  verbose=FALSE
  ###
  
  ###check data
  
  if(missing(learn))
    learn="RF"
  learn=toupper(learn)
  if(missing(type))
    type=NA
  ctype=type
  if(!is.na(type)){
    type=c("left","right")[type+1] #if type=1 then right
  }
  else {type="none"}
  if(missing(x))
    stop("x view must be supplied")
  if(missing(z))
    stop("z view must be supplied")
  if(missing(w))
    stop("w view must be supplied")
  if(missing(y))
    stop("y response must be supplied")
  
  #   if(is.factor(y)){
  #     if(nlevels(y)>2)
  #       stop("Currently only works for binary response")
  #     y=c(0,1)[as.numeric(y)]
  #   }
  
  
  x=as.data.frame(x)
  z=as.data.frame(z)
  w=as.data.frame(w)
  
  n=dim(x)[1]
  if(n!=dim(z)[1] | n!=dim(w)[1])
    stop("x,z and w must have same number of observations, i.e. dim(x)[1]=dim(z)[1]")
  if(n!=length(y))
    stop("y must have same length as x,z, with y[U]=0 (technically anything)")
  if(length(L)>n)
    if((length(L)+length(U))!=n)
      stop("Error:  length(L)+length(U)!= dim(x)[1]")
  
  if(learn=="SVM"){
    ans=ftf.svm.ctrain(x,z,y,k,L,U,type=type,verbose=verbose,...)
  }
  if(learn=="RF"){
    ans=ftf.rf.ctrain(x,z,w,y,k,L,U,type=type,verbose=verbose,...) #the ellipses make no sense here
    #ans=ftf.rf.ctrain(x,z,y,k,L,U,type=type,verbose=verbose)
  }
  obj=structure(list(model=ans,learn=learn,type=ctype,m=length(L),n=n),class="cftf")
  obj
}

`ftf.rf.ctrain`<-function(x,z,w,y,k=30,L,U,
                          type=c("none","left","right"),
                          verbose=FALSE,...){
  f<-function(a1,a2,a3){
    vec<-apply(cbind(apply(a1,1,max),apply(a2,1,max),apply(a3,1,max)),1,which.max)
    ans<-rep(0,dim(a1)[1])
    ans[vec==1]<-a1[vec==1,2]
    ans[vec==2]<-a2[vec==2,2]
    ans[vec==3]<-a3[vec==3,2]
    list(ans,vec)
  }
  if(type=="left"){
    f<-function(a1,b1){
      vec<-apply(cbind(a1[,1],a2[,1]),1,which.max)
      ans<-rep(0,dim(a1)[1])
      ans[vec==1]<-a1[vec==1,2]
      ans[vec==2]<-a2[vec==2,2]
      list(ans,vec)
    }
  }
  
  #need to make the following work for the three measurements
  # tree, scholl and l-measure
  if(type=="right"){
    f<-function(a1,a2,a3){
      vec<-apply(cbind(a1[,2],a2[,2],a3[,2]),1,which.max)
      ans<-rep(0,dim(a1)[1])
      ans[vec==1]<-a1[vec==1,2]
      ans[vec==2]<-a2[vec==2,2]
      ans[vec==3]<-a3[vec==3,2]
      list(ans,vec)
    }
  }

  
# test data
#####
#   y=y
#   k=5
#   learn="RF"
#   type=1
#   verbose="T"
#    
#   gx<-randomForest(y~.,data=data.frame(y,x)[L,])
#   gz<-randomForest(y~.,data=data.frame(y,z)[L,])
#   gw<-randomForest(y~.,data=data.frame(y,w)[L,])
# 
#   gx<-randomForest(y~.,data=data.frame(y=factor(y,levels=levels(temp1$y)),x)[L,],...)
#   gz<-randomForest(y~.,data=data.frame(y=factor(y,levels=levels(temp1$y)),z)[L,],...)
#   gw<-randomForest(y~.,data=data.frame(y=factor(y,levels=levels(temp1$y)),w)[L,],...)
#   
#   gx<-randomForest(y~.,data=data.frame(y=as.factor(y),x)[L,])
#   gz<-randomForest(y~.,data=data.frame(y=as.factor(y),z)[L,])
#   gw<-randomForest(y~.,data=data.frame(y=as.factor(y),w)[L,])
  #####
  
  




  gx<-randomForest(y~.,data=data.frame(y,x)[L,],...)
  gz<-randomForest(y~.,data=data.frame(y,z)[L,],...)
  gw<-randomForest(y~.,data=data.frame(y,w)[L,],...)
  
  
  a1<-predict(gx,x,type="prob")
  a2<-predict(gz,z,type="prob")
  a3<-predict(gw,w,type="prob")
  
  pa1=f(a1,a2,a3)
  
  pnow=pa1[[1]]
  vecind=pa1[[2]]
  porig<-pnow
  yval<-as.numeric(pnow>=0.5)
  conv=100
  t1<-proc.time()
  if(k>1){
    for(i in 1:k){
      ptemp=pnow
      pnow[L]=y[L]
#      y=factor(pnow,levels=levels(temp1$y))
#       gx<-randomForest(y~.,data=data.frame(y,x),...)
#       gz<-randomForest(y~.,data=data.frame(y,z),...)
#       gw<-randomForest(y~.,data=data.frame(y,w),...)
       gx<-randomForest(y~.,data=data.frame(y=pnow,x),...)
       gz<-randomForest(y~.,data=data.frame(y=pnow,z),...)
       gw<-randomForest(y~.,data=data.frame(y=pnow,w),...)
# gx<-randomForest(y~.,data=data.frame(y=pnow,x))
# gz<-randomForest(y~.,data=data.frame(y=pnow,z))
# gw<-randomForest(y~.,data=data.frame(y=pnow,w))
# 

      a1<-as.vector(predict(gx,x))
      a2<-as.vector(predict(gz,z))
      a3<-as.vector(predict(gw,w))
      
      a1=cbind(1-a1,a1)
      a2=cbind(1-a2,a2)
      a3=cbind(1-a3,a3)
      
      if(any(a1>1))
        a1[a1>1]=1
      if(any(a1<0))
        a1[a1<0]=0
      if(any(a2>1))
        a2[a2>1]=1
      if(any(a2<0))
        a2[a2<0]=0
      if(any(a3>1))
        a3[a3>1]=1
      if(any(a3<0))
        a3[a3<0]=0
      
      pa1=f(a1,a2,a3)
      pnow=pa1[[1]]
      vecind=pa1[[2]]
      yval<-as.numeric(pnow>0.5)
      conv= mean((ptemp-pnow)^2)
      
      if(verbose){
        cat("i=",i," conv=",conv,"  time=",(proc.time()-t1)/60,"\n")
      }
    }
  }
  fvec=list(yL=yval[L],yU=yval[U],px=a1,py=a2,gx=gx,gz=gz,gw=gw,iter=k,
            conv=conv,porig=porig,vecind=vecind)
  return(fvec)
}

`kapstat` <-
  function(tab=diag(2) ){
    if(dim(tab)[1]==1){
      return(0)
    }
    if(dim(tab)[1]==dim(tab)[2]){
      rs<-apply(tab,2,sum)
      cs<-apply(tab,1,sum)
      N<-sum(rs)
      E<-sum(rs*cs)/N^2
      O<-sum(diag(tab))/N
      return( (O-E)/(1-E) )
    }
    return(NA)
  }



# loads a csv into a data.frame and cleans data
process.csv<-function(x)
{ 
  ####testing code
  ##x<-(myfiles[[i]])
  
  ### check for number of species in a dataset
  ###   if dataset contains only 1 species
  ###   the categorical variable is set to the
  ###   primary brain region
  if (nlevels(as.factor(x[,ny]))==1){ny<-15}
  
  ### cleaning the dataset
  x<-x[x$Archive.Name!="McQuiston",]
  dat<-data.frame(y=as.factor(x[,ny]),x[,c(nx1,nx2,nx3)])
  dat<-na.omit(dat)
  dat$Soma.Surface<-as.numeric(dat$Soma.Surface)
  dat$Gstat.total.cable.length<-as.numeric(dat$Gstat.total.cable.length)
  dat$Fractal_Dim<-as.numeric(dat$Fractal_Dim)
  return(dat)  
}  

try.multinom<-function(pdata,ltemp,numvar)
{
  vars<-tail(order(scale(pdata)),numvar)
  index<-append(vars+1,1,0)
  scaletemp<-data.frame(apply(ltemp[,c(index[-1])], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
  scaletemp<-cbind(y=ltemp$y,scaletemp)
  lmeasure<-tryCatch({multinom(y~.,data=scaletemp,maxit=100)},
                     error=function(err){
                       lmeasure<-data.frame(deviance<-0)
                       return<-lmeasure})
  
  return(round(lmeasure$deviance,3))
  
}

parsecolors<-function(cols)
{
  makecolors<-function(x)
  {
    # x<-low
    ctemp<-paste(pal(length(x)),"FF",sep="")
    ###working
    #tf<-levels(dat[,1])%in%x
    #test
    tf<-levels(temp1[,1])%in%x
    tf<-c(FALSE,tf,FALSE)
    cols[tf]<-ctemp
    return(cols)
  }
  low<-c("blowfly","C. elegans","drosophila melanogaster",
         "moth","spiny lobster")
  medium<-c("chicken","frog","goldfish","guinea pig","manatee",
            "mouse","pouched lamprey","proechimys","rabbit",
            "rat","salamander","zebrafish")
  high<-c("bottlenose dolphin","cat","chimpanzee","clouded leopard",
          "domestic pig","elephant","giraffe","human","humpback whale",
          "minke whale","monkey","sheep","Siberian tiger")
  
  pal<-colorRampPalette(c("rosybrown4","red3"))
  cols<-makecolors(low)
  pal<-colorRampPalette(c("grey","blue"))
  cols<-makecolors(medium)
  #pal<-colorRampPalette(c("black","chartreuse"))
  #pal<-colorRampPalette(c("black","chartreuse","black"))
  pal<-colorRampPalette(c("darkgreen","green","palegreen"))
  cols<-makecolors(high)
  return(cols)
}
# prints to screen
print.err<-function(rfor,subj,fname)
{
  last<-end(rfor$err.rate)[1]
  mains<-paste(subj, ", OOB=",round(mean(rfor$err.rate[last,1]),3))  
  matplot(rfor$err.rate,lwd=2,col=cols,lty=1,type="l",main=mains,
          sub=fname, cex.sub=.7)
  #label err rates at 100
  hund<-rep(100,dim(rfor$err.rate)[2])
  textxy(hund,rfor$err.rate[100,],round(rfor$err.rate[100,],2))
  
  legend("bottomright",inset=.05,legend=colnames(rfor$err.rate),
         cex=.7,col=cols,lty=1,lwd=3)
  
  ##MAP cols to dimnames(g$err.rate)[[2]]
  varImpPlot(rfor, main=subj,sub=fname,cex.sub=.7)
}

# prints to pdf
save.err<-function(rfor,subj,fname,counts)
{
  last<-end(rfor$err.rate)[1]
  mains<-paste(subj, ", OOB = ",round(mean(rfor$err.rate[last,1]),3),sep="")
  loc<-paste("~/NeuroMiner/presentations/",format(Sys.time(),"%m-%d-%Y"),"/",sep="")
  mkdirs(loc)
  
  #   png(filename=paste(subj, substr(fname,1,nchar(fname)-4), "1.png"), units="px", width=1024, 
  #       height=768, pointsize=12, res=72)
  
  pdf(file=paste(loc,subj, substr(fname,1,nchar(fname)-4), "1.pdf"), width=11, 
      height=8.5, pointsize=12)
  matplot(rfor$err.rate,lwd=2,col=cols,lty=c(1,2,4,6),type="l",
          main=mains, cex.main=1.2,font.main=1,
          sub=fname, cex.sub=.7,ylab="Error rate (%)",xlab="# of trees")
  
  #   plot(vpls,vrf,main=paste(subj,", PLS vs. RF variable importance"),font.main=1,
  #        cex.main=1.2,sub=fname,cex.sub=.8,
  #        ylab="RF (Variable Score)",xlab="PLS (Variable Score)",
  #        pch=16,bg="transparent")
  #label err rates at 100
  hund<-rep(100,dim(rfor$err.rate)[2])
  textxy(hund,rfor$err.rate[100,],round(rfor$err.rate[100,],2), cex=1.2)
  lname<-paste(colnames(rfor$err.rate),"   n=",counts)
  legend("bottomright",inset=.05,legend=lname,
         cex=1,col=cols,lty=c(1,2,4,6),lwd=2,bg="white")
  dev.off()
  
  ##MAP cols to dimnames(g$err.rate)[[2]]
  #   png(filename=paste(subj,substr(fname,1,nchar(fname)-4), "2.png"), units="px", width=1024, 
  #       height=768, pointsize=12, res=144)
  pdf(file=paste(loc,subj, substr(fname,1,nchar(fname)-4), "2.pdf"), width=11, 
      height=8.5, pointsize=12)
  varImpPlot(rfor, main=paste(subj,", RF variable importance",sep=""),
             cex.main=1.2,font.main=1,sub=fname,cex.sub=.7,bg="transparent")
  dev.off()
}

PLSvRF<-function(vrf,vpls,subj,fname,dat)
{
  vrf<-scale(vrf)
  vpls<-scale(vpls)
  plot(vpls,vrf,main=subj,sub=fname,cex.sub=.7,
       ylab="RF (Variable Score)",xlab="PLS (Variable Score)",pch=16,cex.sub=0.8)
  #x<-as.matrix(dat[,c(nxx1)])
  x<-as.matrix(dat[,])
  indpls<-order(vpls)[dim(x)[2]:(dim(x)[2]-4)]
  indrf<-order(vrf)[dim(x)[2]:(dim(x)[2]-4)]
  points(vpls[indrf],vrf[indrf],pch=16,cex=2,col="white")
  text(vpls[indrf],vrf[indrf],row.names(vrf)[indrf],col=gray(0.6))
  points(vpls[indpls],vrf[indpls],pch=16,cex=2,col="white")
  text(vpls[indpls],vrf[indpls],row.names(vpls)[indpls],col=gray(0.3))
  abline(0,1)
}

save.PLSvRF<-function(vrf,vpls,subj,fname,dat)
{
  #   v1rf,v1pls,"L-measure",names(myfiles[i]),temp1[,c(nxx1)]
  #   vrf<-v1rf
  #   vpls<-v1pls;subj<-"L-measure"
  #   fname<-names(myfiles[i]);dat<-temp1[,c(nxx1)]
  #   
  loc<-paste("~/NeuroMiner/presentations/",format(Sys.time(),"%m-%d-%Y"),"/",sep="")
  mkdirs(loc)
  pdf(file=paste(loc,subj, substr(fname,1,nchar(fname)-4), "PLSvRF.pdf"), width=11, 
      height=8.5, pointsize=12,bg="transparent")
  
  vrf<-scale(vrf)
  vpls<-scale(vpls)
  plot(vpls,vrf,main=paste(subj,", PLS vs. RF variable importance",sep=""),font.main=1,
       cex.main=1.2,sub=fname,cex.sub=.8,
       ylab="RF (Variable Score)",xlab="PLS (Variable Score)",
       pch=16,bg="transparent")
  #x<-as.matrix(dat[,c(nxx1)])
  x<-as.matrix(dat[,])
  
  indpls<-order(vpls)[dim(x)[2]:(dim(x)[2]-4)]
  indrf<-order(vrf)[dim(x)[2]:(dim(x)[2]-4)]
  points(vpls[indrf],vrf[indrf],pch=16,cex=2,col="white")
  text(vpls[indrf],vrf[indrf],row.names(vrf)[indrf],col=gray(0.6))
  points(vpls[indpls],vrf[indpls],pch=16,cex=2,col="white")
  text(vpls[indpls],vrf[indpls],row.names(vpls)[indpls],col=gray(0.3))
  abline(0,1)
  dev.off()
  
  
  varimp<-rbind(varimp,c(row.names(vrf)[indrf],
                         row.names(vpls)[indpls]))
  row.names(varimp)[nrow(varimp)]<-paste(substr(fname,1,nchar(fname)-4),subj)
  
  return(varimp)
}

save.CoFTFRF<-function(rfv,fname)
{
  loc<-paste("~/NeuroMiner/presentations/",format(Sys.time(),"%m-%d-%Y"),"/",sep="")
  mkdirs(loc)
  pdf(file=paste(loc, substr(fname,1,nchar(fname)-4), "CoFTFRF.pdf",sep=""), width=11, 
      height=8.5, pointsize=12,bg="transparent")
  
  dotchart(rfv,main=expression("Co-FTF"[1]*" with Random Forest"),
           xlab="Variable Score",cex=1.2,pch=16)
  
  dev.off()
}



nipal<-function(x,y,k)
{
  x<-scale(x,,scale=FALSE)
  y<-scale(y,,scale=FALSE)
  b<-p<-a<-u<-NULL
  for(i in 1:k){
    wt<-t(x)%*%y/as.numeric(t(y)%*%x%*%t(x)%*%y)
    tt<-x%*%wt
    pt<- t(x)%*%tt/as.numeric(t(tt)%*%tt)
    ## add tuning parameter gamma if above is zero
    # would have to be cross validated
    # example to test
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

vip<-function(obj,y,nm=NULL)
{
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

save.IMP<-function(fname)
{
  loc<-paste("~/NeuroMiner/presentations/",format(Sys.time(),"%m-%d-%Y"),"/",sep="")
  mkdirs(loc)
  pdf(file=paste(loc, substr(fname,1,nchar(fname)-4), "VARIMP.pdf",sep=""), width=11, 
      height=8.5, pointsize=12,bg="transparent")
  
  #  texfile <- paste(loc,substr(fname,1,nchar(fname)-4),'.pdf',sep="")
  cnames<-c("rf","rf","rf","rf","rf",
            "pls","pls","pls","pls","pls")
  colnames(varimp)<-cnames
  #knitr::kable(varimp,  caption = "A table produced by printr.")
  
  
  
  
  par(mfrow=c(2,1))   
  textplot( xtable(varimp[-1,1:5]))
  textplot(xtable(varimp[-1,6:10]))
  
  dev.off()
}


save.time<-function(fname)
{
  loc<-paste("~/NeuroMiner/presentations/",format(Sys.time(),"%m-%d-%Y"),"/",sep="")
  mkdirs(loc)
  pdf(file=paste(loc, substr(fname,1,nchar(fname)-4), "time.pdf",sep=""), width=11, 
      height=8.5, pointsize=12,bg="transparent")
  textplot(xtable(ftemp[-1,]))
  dev.off()
  
}

#####
## batch processing
##############
nx1<-33:96
nx2<-97:110
nx3<-111:159
###try
#ny<-15
###original
ny<-4
#coldat<-process.csv(colmaster)
#cols=rainbow(length(unique(temp1[,1]))+2)
#cols=parsecolors(cols)


index=1:30

#z[-c(4,5,6,7,8,12,13,15,16)]
#z[-c(4)]
### for the time being we omit drosphilia, its a small dataset n=18

varimp<-array(NA,dim=c(1,10))

for (i in index[c(28)])
{
  varimp<-array(NA,dim=c(1,10))
  ftemp<-array(NA,dim=c(1,2))
  dimnames(ftemp)[[2]]<-c("time (s)","OOB")
  
  i=28
  #i=11
  #i=1
  i=9
  temp1<-process.csv(myfiles[[i]])
  
  nxx1<-2:65
  nxx2<-66:79
  nxx3<-81:dim(temp1)[2] #80 is Sholl.1 should always be 1
  
  cols=rainbow(length(unique(temp1[,1]))+2)
  
  ###comment out if running single species
  cols=parsecolors(cols)
  
  n<-length(temp1$y)
  
  ## Set L and U  (P=60)
  set.seed(100)
  y<-temp1$y
  y<- factor(y,levels=levels(temp1$y))
  L=sample(1:n,ceiling(n*0.6))    #60% sample
  U=setdiff(1:n,L)                #the other #%40
  y[U]=NA                         #drops 
#    x=temp1[,c(nxx1)]
#    z=temp1[,c(nxx2)]
#    w=temp1[,c(nxx3)]
#   
  
  ## Exectute co-FTF_1( Random Forest)  takes about 5 minutes
  #change k back to 5
  crf<-cftf(x=temp1[,c(nxx1)],z=temp1[,c(nxx2)],w=temp1[,c(nxx3)],y=y,k=5,L,U,learn="RF",type=1,verbose="T")
  
  
  #   y=pharm$class
  #   x=pharm$bio
  #   z=pharm$chem
  #   x=na.roughfix(x)  ##fix NA's
  #   n<-length(y)
  #   
  
  y[U]=temp1$y[U]
  tabrf=table(crf$model$yU,y[U])
  sum(diag(tabrf))/sum(tabrf)
  kapstat(tabrf)
  
  
  crf1<-cftf(x=temp1[,c(nxx1)],z=temp1[,c(nxx2)],w=temp1[,c(nxx3)],y,k=1,1:n,NULL,learn="RF",type=1,local=TRUE)
  
  ## Necessary processing for plot
  nvar=15  
  gx=crf1$model$gx
  gz=crf1$model$gz
  gw=crf1$model$gw
  vecind<-crf1$model$vecind
  xvar<-apply(gx$local[,vecind==1],1,sum)
  zvar<-apply(gz$local[,vecind==2],1,sum)
  wvar<-apply(gw$local[,vecind==3],1,sum)
  rfv<-sort(c(xvar,zvar,wvar),dec=TRUE)[nvar:1]
  
  
  save.CoFTFRF(rfv,names(myfiles[i]))
  ## Make plot
#   dotchart(rfv,main=expression("Co-FTF"[1]*" with Random Foest"),
#            xlab="Variable Score",cex=1.2,pch=16)
#   
  
  
  

  
}

