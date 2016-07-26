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

####
## Read in Data
###
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


z=1:28

#z[-c(4,5,6,7,8,12,13,15,16)]
#z[-c(4)]
### for the time being we omit drosphilia, its a small dataset n=18

varimp<-array(NA,dim=c(1,10))

for (i in z[c(28)])
{
  varimp<-array(NA,dim=c(1,10))
  ftemp<-array(NA,dim=c(1,3))
  dimnames(ftemp)[[2]]<-c("time (s)","OOB","R deviance")
  
  #i=28
  #i=1
  #temp1<-process.csv(myfiles[[1]])
  temp1<-process.csv(myfiles[[i]])
  
  
  nxx1<-2:65
  nxx2<-66:79
  nxx3<-81:dim(temp1)[2] #80 is Sholl.1 should always be 1
  
  cols=rainbow(length(unique(temp1[,1]))+2)
  
  ###comment out if running single species
  cols=parsecolors(cols)
  
  
  ptm<-proc.time()[3]
  set.seed(100)
  g1rf<-randomForest(y~.,data=temp1[,c(1,nxx1)])##,prox=TRUE)
  ftime<-round(proc.time()[3]-ptm,4);names(ftime)<-("L-measure forest")
  
  v1rf<-varImpPlot(g1rf)
  #dev<-try.multinom(v1rf,temp1,5)
  dev<-0
  last<-end(g1rf$err.rate)[1]
  OOB<-round(mean(g1rf$err.rate[last,1]),3)
  
  ftemp<-rbind(ftemp,c(ftime[[1]],OOB,dev))
  row.names(ftemp)[nrow(ftemp)]<-names(ftime)
  
  
  #dev<-try.multinom(v1pls,temp1,5)
  ftemp<-rbind(ftemp,c(ftime[[2]],NA,dev))
  row.names(ftemp)[nrow(ftemp)]<-names(ftime)[2]
  
  #print.err(g1rf,"L-measure",names(myfiles[i]))
  counts<-c(sum(count(temp1$y)[,2]),count(temp1$y)[,2])
  save.err(g1rf,"L-measure",names(myfiles[i]),counts)
}

###
## Fit learner and make plots for each view
###

##bio
setwd("~/NeuroMiner/data_sets")
#read.csv("../first_subsets/pyramidal_appended.csv",T)->dump
read.csv("NeuronDataMaster.csv",T)->colmaster


setwd("~/NeuroMiner/data_sets/Neuron_subsets")
temp =list.files(pattern="../*.csv")
myfiles<-lapply(temp, function(x) read.csv(x,header=TRUE))
names(myfiles)<-temp



set.seed(020) ##so RF gets same result as in paper
nu=c(0.2,0.5,0.8)  #train data is 20% 50% and 80% of total data
lnu=length(nu)
L1=3 #default is 50, number of times train sample is created

#following have 150 rows and 6 columns
teacc=matrix(0,nrow=L1*lnu,ncol=6)  #accuracy
tesen=matrix(0,nrow=L1*lnu,ncol=6)  #sensitivity
tespec=matrix(0,nrow=L1*lnu,ncol=6) #specificity

#i=9
i=28
nx1<-33:96
nx2<-97:110
nx3<-111:159
ny<-4
temp1<-process.csv(myfiles[[i]])
nxx1<-2:65

k3<-1
t1<-proc.time()

for(j in 1:lnu){
  for(i in 1:L1){
    
    for(index in 1:nlevels(temp1$y)){
      samp<-which(temp1$y==names(summary(temp1$y))[[index]])
      if (exists("L")){
        L<-append(L,sample(samp,ceiling(nu[j]*summary(temp1$y)[[index]])),length(L))
        U<-append(U,setdiff(samp,L),length(U))
      }
      else{
        L=sample(samp,ceiling(nu[j]*summary(temp1$y)[[index]]))
        U=setdiff(samp,L)
      }
    }

    
#     L=sample(1:n,ceiling(nu[j]*n))  #training data
#     U=setdiff(1:n,L)  #test data
    
    
    #grf<-randomForest(y~.,data=temp1[L,c(1,nxx1)])
    grf<-randomForest(y~.,data=data.frame(y=as.factor(droplevels(temp1[L,1])),temp1[L,c(nxx1)]))
    
    tab=table(predict(grf,newdata=temp1[,nxx1])[U],temp1$y[U])
#    tab=table(predict(grf,newdata=temp1[,c(nxx1)])[U,],temp1$y[U])
#    tab=table(predict(grf,newdata=temp1[,c(nxx1)])[U,],temp1$y[U])
   # grf<-randomForest(y~.,data=data.frame(y=as.factor(y),x)[L,])
   # tab=table(predict(grf,newdata=x)[U],y[U])
    
    v1<-diag(tab)/ apply(tab,2,sum)
    teacc[k3,2]<-sum(diag(tab))/sum(tab)
    tesen[k3,2]<-v1[2]
    tespec[k3,2]<-v1[1]
   
    #gpls<-plsr(y~.,data=temp1[L,c(1,nxx1)])
    #tab=table(predict(gpls,newdata=temp1[,nxx1],ncomp=2)[U]>0.5,temp1$y[U])
    
#     gpls<-plsr(y~.,data=data.frame(y,x)[L,])
#     tab=table(predict(gpls,newdata=x,ncomp=2)[U]>0.5,y[U])
    #v1<-diag(tab)/ apply(tab,2,sum)
    #teacc[k3,3]<-sum(diag(tab))/sum(tab)
    #tesen[k3,3]<-v1[2]
    #tespec[k3,3]<-v1[1]
    
    
    cat("nu=",nu[j],"  k3=",k3,"  time=",(proc.time()-t1)/60,"\n")
    k3<-k3+1
    rm(L)
    rm(U)
  }
  write.table(teacc,"te-acc_4bio.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  write.table(tespec,"te-spec_4bio.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  write.table(tesen,"te-sen_4bio.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
}