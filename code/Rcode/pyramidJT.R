library(randomForest)
library(calibrate)

setwd("~/NeuroMiner/data_sets")
#read.csv("../first_subsets/pyramidal_appended.csv",T)->dump
read.csv("NeuronDataMaster.csv",T)->colmaster


setwd("~/NeuroMiner/data_sets/Neuron_subsets")
temp =list.files(pattern="../*.csv")
myfiles<-lapply(temp, function(x) read.csv(x,header=TRUE))
names(myfiles)<-temp

#########
#### fxns 
mkdirs <- function(fp) {
  if(!file.exists(fp)) {
    mkdirs(dirname(fp))
    dir.create(fp)
  }
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
save.err<-function(rfor,subj,fname)
{
  last<-end(rfor$err.rate)[1]
  mains<-paste(subj, ", OOB=",round(mean(rfor$err.rate[last,1]),3))
  loc<-paste("~/NeuroMiner/presentations/",format(Sys.time(),"%m-%d-%Y"),"/",sep="")
  mkdirs(loc)
  
  #   png(filename=paste(subj, substr(fname,1,nchar(fname)-4), "1.png"), units="px", width=1024, 
  #       height=768, pointsize=12, res=72)
 
  pdf(file=paste(loc,subj, substr(fname,1,nchar(fname)-4), "1.pdf"), width=11, 
      height=8.5, pointsize=12)
  matplot(rfor$err.rate,lwd=2,col=cols,lty=1,type="l",main=mains,
          sub=fname, cex.sub=.7)
  #label err rates at 100
  hund<-rep(100,dim(rfor$err.rate)[2])
  textxy(hund,rfor$err.rate[100,],round(rfor$err.rate[100,],2), cex=1.2)
  legend("bottomright",inset=.05,legend=colnames(rfor$err.rate),
         cex=1,col=cols,lty=1,lwd=3,bg="white")
  dev.off()
  
  ##MAP cols to dimnames(g$err.rate)[[2]]
  #   png(filename=paste(subj,substr(fname,1,nchar(fname)-4), "2.png"), units="px", width=1024, 
  #       height=768, pointsize=12, res=144)
  pdf(file=paste(loc,subj, substr(fname,1,nchar(fname)-4), "2.pdf"), width=11, 
      height=8.5, pointsize=12)
  varImpPlot(rfor, main=subj,sub=fname,cex.sub=.7)
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
  loc<-paste("~/NeuroMiner/presentations/",format(Sys.time(),"%m-%d-%Y"),"/",sep="")
  mkdirs(loc)
  pdf(file=paste(loc,subj, substr(fname,1,nchar(fname)-4), "PLSvRF.pdf"), width=11, 
      height=8.5, pointsize=12)
  
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

##############
#####batch processing
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


z=1:18
#z[-c(4,5,6,7,8,12,13,15,16)]
#z[-c(4)]
### for the time being we omit drosphilia, its a small dataset n=18
for (i in z[-c(4,5,6,7,8,12,13,15,16)])
{


#temp1<-process.csv(myfiles[[1]])
temp1<-process.csv(myfiles[[i]])


nxx1<-2:65
nxx2<-66:79
nxx3<-81:dim(temp1)[2] #80 is Sholl.1 should always be 1

cols=rainbow(length(unique(temp1[,1]))+2)
cols=parsecolors(cols)


ptm<-proc.time()[3]
set.seed(100)
g1rf<-randomForest(y~.,data=temp1[,c(1,nxx1)])##,prox=TRUE)
ftime<-proc.time()[3]-ptm;names(ftime)<-("L-measure forest")

ptm<-proc.time()[3]
set.seed(100)
g1pls<-nipal(as.matrix(temp1[,c(nxx1)]),as.numeric(temp1$y),40)
v1pls<-vip(g1pls,as.numeric(temp1$y),names(temp1[,c(nxx1)]))
ftime[2]<-proc.time()[3]-ptm;names(ftime)[2]<-("L-measure pls")

print.err(g1rf,"L-measure",names(myfiles[i]))
save.err(g1rf,"L-measure",names(myfiles[i]))
v1rf<-varImpPlot(g1rf)
PLSvRF(v1rf,v1pls,"L-measure",names(myfiles[i]),temp1[,c(nxx1)])
save.PLSvRF(v1rf,v1pls,"L-measure",names(myfiles[i]),temp1[,c(nxx1)])

ptm<-proc.time()[3]
set.seed(100)
g2rf<-randomForest(y~.,data=temp1[,c(1,nxx2)])##,prox=TRUE)
ftime[3]<-proc.time()[3]-ptm;names(ftime)[3]<-("gstat forest")

ptm<-proc.time()[3]
set.seed(100)
g2pls<-nipal(as.matrix(temp1[,c(nxx2)]),as.numeric(temp1$y),40)
v2pls<-vip(g2pls,as.numeric(temp1$y),names(temp1[,c(nxx2)]))
ftime[4]<-proc.time()[3]-ptm;names(ftime)[4]<-("gstat pls")

#print.err(g2,"Gstat","NeuronDataMaster")
save.err(g2rf,"Gstat",names(myfiles[i]))
v2rf<-varImpPlot(g2rf)
PLSvRF(v2rf,v2pls,"Gstat",names(myfiles[i]),temp1[,c(nxx2)])
save.PLSvRF(v2rf,v2pls,"Gstat",names(myfiles[i]),temp1[,c(nxx2)])

ptm<-proc.time()[3]
set.seed(100)
g3rf<-randomForest(y~.,data=temp1[,c(1,nxx3)])##,prox=TRUE)
ftime[5]<-proc.time()[3]-ptm;names(ftime)[5]<-("sholl forest")

ptm<-proc.time()[3]
set.seed(100)
g3pls<-nipal(as.matrix(temp1[,c(nxx3)]),as.numeric(temp1$y),40)
v3pls<-vip(g3pls,as.numeric(temp1$y),names(temp1[,c(nxx3)]))
ftime[6]<-proc.time()[3]-ptm;names(ftime)[6]<-("sholl pls")
#print.err(g3,"Sholl","NeuronDataMaster")
save.err(g3rf,"Sholl",names(myfiles[i]))
v3rf<-varImpPlot(g3rf)
PLSvRF(v3rf,v3pls,"Sholl",names(myfiles[i]),temp1[,c(nxx3)])
save.PLSvRF(v3rf,v3pls,"Sholl",names(myfiles[i]),temp1[,c(nxx3)])

ptm<-proc.time()[3]
set.seed(100)
gtotrf<-randomForest(y~.,data=temp1[,c(1,nxx1,nxx2,nxx3)])##,prox=TRUE)
ftime[7]<-proc.time()[3]-ptm;names(ftime)[7]<-("All forest")

ptm<-proc.time()[3]
set.seed(100)
gtotpls<-nipal(as.matrix(temp1[,c(nxx1,nxx2,nxx3)]),as.numeric(temp1$y),40)
vtotpls<-vip(gtotpls,as.numeric(temp1$y),names(temp1[,c(nxx1,nxx2,nxx3)]))
ftime[8]<-proc.time()[3]-ptm;names(ftime)[8]<-("all pls")

#print.err(gtot,"All","NeuronDataMaster")
save.err(gtotrf,"All",names(myfiles[i]))
vtotrf<-varImpPlot(gtotrf)
PLSvRF(vtotrf,vtotpls,"All",names(myfiles[i]),temp1[,c(nxx1,nxx2,nxx3)])
save.PLSvRF(vtotrf,vtotpls,"All",names(myfiles[i]),temp1[,c(nxx1,nxx2,nxx3)])
}

OOB=round(mean(gtotrf$err.rate[last,1])
######end batch
###############




###
###
####
old code
###
###

setwd("~/NeuroMiner/data_sets")
#read.csv("../first_subsets/pyramidal_appended.csv",T)->dump
read.csv("NeuronDataMaster.csv",T)->x

# kilb data showed erro in the Sholl values, these rows are deleted
#x<-x[x$Archive.Name!="Kilb",]
x<-x[x$Archive.Name!="McQuiston",]


nx1<-33:96
nx2<-97:110
nx3<-111:159
ny<-4

dat<-data.frame(y=as.factor(x[,ny]),x[,c(nx1,nx2,nx3)])
dat<-na.omit(dat)
dat$Soma.Surface<-as.numeric(dat$Soma.Surface)
dat$Gstat.total.cable.length<-as.numeric(dat$Gstat.total.cable.length)
dat$Fractal_Dim<-as.numeric(dat$Fractal_Dim)

save(dat, file= "NeuronDataMaster.Rdata")
nxx1<-2:65
nxx2<-66:79
nxx3<-81:dim(dat)[2] #80 is Sholl.1 should always be 1

#which(dat[,80:128]<1,arr.ind=T)
#dat[dat[,80:128]<1]



cols=rainbow(length(unique(dat[,1]))+2)


set.seed(100)
g1<-randomForest(y~.,data=dat[,c(1,nxx1)])##,prox=TRUE)
print.err(g1,"L-measure","NeuronDataMaster")
save.err(g1,"L-measure","NeuronDataMaster")

set.seed(100)
g2<-randomForest(y~.,data=dat[,c(1,nxx2)])##,prox=TRUE)
print.err(g2,"Gstat","NeuronDataMaster")

set.seed(100)
g3<-randomForest(y~.,data=dat[,c(1,nxx3)])##,prox=TRUE)
print.err(g3,"Sholl","NeuronDataMaster")

set.seed(100)
gtot<-randomForest(y~.,data=dat)##,prox=TRUE)
print.err(gtot,"All","NeuronDataMaster")


## -rat
nrow(dat[dat$y=="rat",])
dat2<-dat[dat$y!="rat",]
dat2[,1]<-droplevels(dat2[,1])

set.seed(100)
g2rat<-randomForest(y~.,data=dat2[,c(1,nxx2)])##,prox=TRUE)
cols=rainbow(length(unique(dat2[,1]))+2)
last<-end(g2rat$err.rate)[1]
mains<-paste("Gstat Minus rat , OOB=",round(mean(g2rat$err.rate[last,1]),3))
matplot(g2rat$err.rate[,-26],lwd=2,col=cols,lty=1, type="l", main=mains)
legend("bottomright",inset=.05,legend=colnames(g2rat$err.rate),
       cex=.7,col=cols,lty=1,lwd=3)
##MAP cols to dimnames(g$err.rate)[[2]]
varImpPlot(g2rat,main=mains)



#######
#color testing

bartemp<-c(5,7,6,4,8)
ctemp<-rainbow(5)
ctemp[1:5]<-c("#990000FF","#BB0000FF","#996600FF","#BB6600FF","#EE6600FF")
ctemp<-cm.colors(7)


###more sophisticate way

low<-c("blowfly","C. elegans","drosophila melanogaster",
          "moth","spiny lobster")
medium<-c("chicken","frog","goldfish","guinea pig","manatee",
          "mouse","pouched lamprey","proechimys","rabbit",
          "rat","salamander","zebrafish")
high<-c("bottlenose dolphin","cat","chimpanzee","clouded leopard",
        "domestic pig","elephant","giraffe","human","humpback whale",
        "minke whale","monkey","sheep","siberian tiger")
pal<-colorRampPalette(c("rosybrown4","red4"))
i<-length(medium)
set.seed(100)
bartemp<-runif(i,1,5)
ctemp<-pal(5)
ctemp<-pal(length(low))
barplot(bartemp,col=ctemp)



#value<-c(2, 4, 5, 8, 2, 3, 1)


cols=rainbow(length(unique(dat[,1]))+2)
makecolors<-function(x)
{
#  x<-high
  ctemp<-paste(pal(length(x)),"FF",sep="")
  tf<-levels(dat[,1])%in%x
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

i<-length(cols)
set.seed(100)
bartemp<-runif(i,1,5)
barplot(bartemp,col=cols)



df<-data.frame(cols, tf)




####removing species with less than 100 samples
#nrow(dat[dat$y=="rat",])
#dat2<-dat[dat$y!="rat",]
#dat2[,1]<-droplevels(dat2[,1])
