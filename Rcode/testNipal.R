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

`cftf` <- function(x,z,y,k=8,L,U,learn=c("RF","SVM"),type=c(NA,0,1),verbose=FALSE,...){
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
  if(missing(y))
    stop("y response must be supplied")
  
#   if(is.factor(y)){
#     if(nlevels(y)>2)
#       stop("Currently only works for binary response")
#     y=c(0,1)[as.numeric(y)]
#   }
    

  x=as.data.frame(x)
  z=as.data.frame(z)

  n=dim(x)[1]
  if(n!=dim(z)[1])
    stop("x,z must have same number of observations, i.e. dim(x)[1]=dim(z)[1]")
  if(n!=length(y))
    stop("y must have same length as x,z, with y[U]=0 (technically anything)")
  if(length(L)>n)
    if((length(L)+length(U))!=n)
      stop("Error:  length(L)+length(U)!= dim(x)[1]")
  
  if(learn=="SVM"){
    ans=ftf.svm.ctrain(x,z,y,k,L,U,type=type,verbose=verbose)
  }
  if(learn=="RF"){
    ans=ftf.rf.ctrain(x,z,y,k,L,U,type=type,verbose=verbose,...) #the ellipses make no sense here
    #ans=ftf.rf.ctrain(x,z,y,k,L,U,type=type,verbose=verbose)
  }
  obj=structure(list(model=ans,learn=learn,type=ctype,m=length(L),n=n),class="cftf")
  obj
}

`ftf.rf.ctrain`<-function(x,z,y,k=30,L,U,
                          type=c("none","left","right"),
                          verbose=FALSE,...){
  f<-function(a1,a2){
    vec<-apply(cbind(apply(a1,1,max),apply(a2,1,max)),1,which.max)
    ans<-rep(0,dim(a1)[1])
    ans[vec==1]<-a1[vec==1,2]
    ans[vec==2]<-a2[vec==2,2]
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
    f<-function(a1,b1){
      vec<-apply(cbind(a1[,2],a2[,2]),1,which.max)
      ans<-rep(0,dim(a1)[1])
      ans[vec==1]<-a1[vec==1,2]
      ans[vec==2]<-a2[vec==2,2]
      list(ans,vec)
    }
  }
  
  #add 3rd here
  gx<-randomForest(y~.,data=data.frame(y=as.factor(y),x)[L,],...)
  gz<-randomForest(y~.,data=data.frame(y=as.factor(y),z)[L,],...)
  
  #gx<-randomForest(y~.,data=data.frame(y=as.factor(y),x)[L,])
  #gz<-randomForest(y~.,data=data.frame(y=as.factor(y),z)[L,])
  
  
  a1<-predict(gx,x,type="prob")
  a2<-predict(gz,z,type="prob")
  
  pa1=f(a1,a2)
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
      gx<-randomForest(y~.,data=data.frame(y=pnow,x),...)
      gz<-randomForest(y~.,data=data.frame(y=pnow,z),...)
      a1<-as.vector(predict(gx,x))
      a2<-as.vector(predict(gz,z))
      a1=cbind(1-a1,a1)
      a2=cbind(1-a2,a2)
      
      if(any(a1>1))
        a1[a1>1]=1
      if(any(a1<0))
        a1[a1<0]=0
      if(any(a2>1))
        a2[a2>1]=1
      if(any(a2<0))
        a2[a2<0]=0
      
      pa1=f(a1,a2)
      pnow=pa1[[1]]
      vecind=pa1[[2]]
      yval<-as.numeric(pnow>0.5)
      conv= mean((ptemp-pnow)^2)
      
      if(verbose){
        cat("i=",i," conv=",conv,"  time=",(proc.time()-t1)/60,"\n")
      }
    }
  }
  fvec=list(yL=yval[L],yU=yval[U],px=a1,py=a2,gx=gx,gz=gz,iter=k,
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
  
  #   tt <- print(xtable(varimp[-1,]), type='latex')
  #   
  #   print(xtable(varimp[-1,]), type='latex')
  #   
  #   texfile <- paste(loc,substr(fname,1,nchar(fname)-4),'.tex',sep="")
  #   cat(
  #     '\\documentclass[12pt]{report}
  #       \\usepackage[landscape]{geometry}
  #       \\date{}
  #       \\begin{document}', tt, '\\end{document}', sep='', 
  #     file=texfile
  #   )
  #   ## pdflatex from texlive package for linux converts .tex to .pdf
  #   
  #   tools::texi2dvi(texfile, pdf = TRUE, clean = TRUE)
  #   tools::texi2pdf(texfile,  clean = TRUE)
  
  
  
  
  
  #   # show R version information
  #   textplot(version)
  #   # show the alphabet as a single string
  #   textplot( paste(letters[1:26], collapse=" ") )
  #   
  #   # show the alphabet as a matrix 
  #   textplot( matrix(letters[1:26], ncol=2))
  #   
  #   ### Make a nice 4 way display with two plots and two text summaries 
  #   data(iris)  
  #   par(mfrow=c(2,2))   
  #   plot( Sepal.Length ~ Species, data=iris, border="blue", col="cyan",   
  #         main="Boxplot of Sepal Length by Species" )    
  #   plotmeans(Sepal.Length ~ Species, data=iris, barwidth=2, connect=FALSE,
  #             main="Means and 95\% Confidence Intervals\nof Sepal Length by Species")
  #   
  #   info <- sapply(split(iris$Sepal.Length, iris$Species),
  #                  function(x) round(c(Mean=mean(x), SD=sd(x), N=gdata::nobs(x)),2))
  #   
  #   textplot( info, valign="top"  )
  #   title("Sepal Length by Species")
  #   
  #   reg <- lm( Sepal.Length ~ Species, data=iris )
  #   textplot( capture.output(summary(reg)), valign="top")
  #   title("Regression of Sepal Length by Species")
  #   
  #   par(mfrow=c(1,1))
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


index=1:18

#z[-c(4,5,6,7,8,12,13,15,16)]
#z[-c(4)]
### for the time being we omit drosphilia, its a small dataset n=18

varimp<-array(NA,dim=c(1,10))

for (i in index[c(11)])
{
  varimp<-array(NA,dim=c(1,10))
  ftemp<-array(NA,dim=c(1,2))
  dimnames(ftemp)[[2]]<-c("time (s)","OOB")
  
  i=11
  #i=1
  #temp1<-process.csv(myfiles[[1]])
  temp1<-process.csv(myfiles[[i]])
  
  
  nxx1<-2:65
  nxx2<-66:79
  nxx3<-81:dim(temp1)[2] #80 is Sholl.1 should always be 1
  
  cols=rainbow(length(unique(temp1[,1]))+2)
  
  ###comment out if running single species
  cols=parsecolors(cols)

  n<-length(temp1$y)
  
  
  #test df
#####
  set.seed(100)
  X1<-runif(20,0,1)
  lX2<-2*X1
  X2<-runif(length(X1),.5,1)
  #    colX2<-X1
  #    colX2[X1>.5]<-X1[X1>.5]-.3
  X3<-rnorm(length(X1),.5,.5)
  X4<-rnorm(length(X1),.5,.5)
  Y1<-rep(0,length(X1))
  Y1[X1>.5]<-1
  Y2<-Y1
  Y2[3]<-0
  
  lindf<-data.frame(Y1,Y2,X1,lX2,X3,X4)
  nldf<-data.frame(Y1,Y2,X1,X2,X3,X4)
  
  #following two pls Y1=1 if X1>.5 and Y1=0 otherwise
  #matrix with collinearities LX2=X1
  set.seed(100)
  lpls<-nipal(as.matrix(lindf[,c(3,4,5,6)]),as.numeric(lindf$Y1),40)
  vlpls<-vip(lpls,as.numeric(lindf$Y1),names(lindf[,c(3,4,5,6)]))
  
  #matrix without lX2 
  set.seed(100)
  lpls<-nipal(as.matrix(lindf[,c(3,5,6)]),as.numeric(lindf$Y1),40)
  vlpls<-vip(lpls,as.numeric(lindf$Y1),names(lindf[,c(3,5,6)]))
  
  #Y2=Y1 except Y2[3]=0 same as first run
  set.seed(100)
  lpls<-nipal(as.matrix(lindf[,c(3,4,5,6)]),as.numeric(lindf$Y2),40)
  vlpls<-vip(lpls,as.numeric(lindf$Y2),names(lindf[,c(3,4,5,6)]))

  # no collinearities fit to Y1
  set.seed(100)
  nlpls<-nipal(as.matrix(nldf[,c(3,4,5,6)]),as.numeric(nldf$Y1),40)
  vnlpls<-vip(nlpls,as.numeric(nldf$Y1),names(nldf[,c(3,4,5,6)]))

  # no collinearities fit to Y2
  set.seed(100)
  nlpls<-nipal(as.matrix(nldf[,c(3,4,5,6)]),as.numeric(nldf$Y2),40)
  vnlpls<-vip(nlpls,as.numeric(nldf$Y2),names(nldf[,c(3,4,5,6)]))
  #####

#   
#   colpls<-nipal(as.matrix(lindf[,-c(3)]),as.numeric(lindf$Y1),40)
#   vcolpls<-vip(lpls,as.numeric(lindf$Y1),names(lindf[,-c(1,3)]))
#   
  

  
  ## Set L and U  (P=60)
  set.seed(100)
  y<-temp1$y
  L=sample(1:n,ceiling(n*0.6))    #60% sample
  U=setdiff(1:n,L)                #the other #%40
  y[U]=NA                         #drops 
  
 
  
  
  

  set.seed(100)
  g3pls<-nipal(as.matrix(temp1[,c(nxx3)]),as.numeric(temp1$y),40)
  v3pls<-vip(g3pls,as.numeric(temp1$y),names(temp1[,c(nxx3)]))
  ftime[6]<-round(proc.time()[3]-ptm,4);names(ftime)[6]<-("Sholl analysis PLS")
  
  
  ftemp<-rbind(ftemp,c(ftime[[6]],NA))
  row.names(ftemp)[nrow(ftemp)]<-names(ftime)[6]
  
  #print.err(g3,"Sholl analysis","NeuronDataMaster")
  #counts<-c(sum(count(temp1$y)[,2]),count(temp1$y)[,2])
  save.err(g3rf,"Sholl analysis",names(myfiles[i]),counts)
  PLSvRF(v3rf,v3pls,"Sholl analysis",names(myfiles[i]),temp1[,c(nxx3)])
  varimp<-save.PLSvRF(v3rf,v3pls,"Sholl analysis",names(myfiles[i]),temp1[,c(nxx3)])
  
  
  
  save.IMP(names(myfiles[i]))
  save.time(names(myfiles[i]))
  
}

