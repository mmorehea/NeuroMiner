
library(gplots)
library(plyr)
library(xtable)
setwd("~/NeuroMiner/data_sets")
#read.csv("../first_subsets/pyramidal_appended.csv",T)->dump
read.csv("NeuronDataMaster.csv",T)->colmaster

#following three functions clean and trim the dataset
trim.csv<-function(tdata,species)
{ 
  ####testing code
  #x<-funk
  
  ### cleaning the dataset
  #dat<-data.frame(y=as.factor(x[,ny]),x[,-ny])
  #tdata<-rdata
  
  tdata<-na.omit(tdata)
  
  #suppress the warnings due to NAs generated
  
  #rdata[rdata$Species.Name==species][4]
#   dim(tdata[tdata$Species.Name=="C. elegans",][4])
#   dim(tdata[tdata$Species.Name=="mouse",][4])
#   dim(tdata[tdata$Species.Name=="rat",][4])
#   dim(tdata[tdata$Species.Name=="human",][4])


  #rdata[rdata$Species.Name==species,][4]
  #rdata[rdata$Species.Name=="C.elegans"|rdata$Species.Name=="rat"|rdata$Species.Name=="mouse"|rdata$Species.Name=="human",]
  
  #suppressWarnings(tdata<-tdata[tdata$Species.Name==species,])
  #suppressWarnings(tdata<-tdata[tdata$Species.Name=="C.elegans"|tdata$Species.Name=="rat"|tdata$Species.Name=="mouse"|tdata$Species.Name=="human",])
  
  tdata<-tdata[tdata$Species.Name=="C. elegans"|tdata$Species.Name=="rat"|tdata$Species.Name=="mouse"|tdata$Species.Name=="human",]
  tdata$Species.Name<-factor(tdata$Species.Name)

  str(tdata$Species.Name)
  return(tdata)  
}  

trimspec.csv<-function(tdata,species)
{ 
  tdata<-na.omit(tdata)
  
  #rdata[rdata$Species.Name==species][4]
  #rdata[rdata$Species.Name==species,][4]
  #rdata[rdata$Species.Name=="rat"|rdata$Species.Name=="human",]
  
  #suppressWarnings(tdata<-tdata[tdata$Species.Name==species,])
  suppressWarnings(tdata<-tdata[tdata$Species.Name=="rat"|tdata$Species.Name=="human",])
  tdata$Species.Name<-factor(tdata$Species.Name)
  return(tdata)  
}  

wwo.axon<-function(temp,keep)
{ 
  temp<-temp[temp$Structural.Domains==keep,]
  temp$Structural.Domains<-factor(temp$Structural.Domains)
  return(temp)  
} 

process.age<-function(temp,age=18)
{
  temp<-temp[temp$Min.Age!=c("Not reported"),]
  #temp$Min.Age<-factor(temp$Min.Age)
  
  temp$Min.Age<-as.numeric(temp$Min.Age)
  temp<-temp[temp$Min.Age>age,]
  return(temp)
}

process.csv<-function(x,ny)
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

#check this
#####
mkdirs <- function(fp)
{
  if(!file.exists(fp)) {
    mkdirs(dirname(fp))
    dir.create(fp)
  }
}



#####
## batch processing
##############
# nx1<-33:96
# nx2<-97:110
# nx3<-111:159
# ny<-4
rdata<-read.csv("NeuronDataMaster.csv",T)

species<-c("C. elegans","mouse",
           "rat","human")
rdata<-read.csv("NeuronDataMaster.csv",T)
###test shit
#rdata<-rdata[1:20,]



rawdata<-trim.csv(rdata,species)

species<-c("rat","human")

rawdata<-trimspec.csv(rawdata,species)



rawdata<-process.age(rawdata,18)

noaxon<-wwo.axon(rawdata,c("Dendrites, Soma, No Axon"))
waxon<-wwo.axon(rawdata,c("Dendrites, Soma, Axon"))



write.csv(rawdata, "TwoSpecies18over.csv", row.names=FALSE, na="")


write.csv(rawdata, "FourSpecies.csv", row.names=FALSE, na="")
write.csv(noaxon, "FourSpeciesNoAxon18over.csv", row.names=FALSE, na="")
write.csv(waxon, "FourSpeciesWithAxon18over.csv",row.names=FALSE, na="")

#test code
# levels(rawdata$Structural.Domains)
# levels(noaxon$Structural.Domains)
# levels(waxon$Structural.Domains)

alldat<-process.csv(rawdata,ny=1)
noaxon<-process.csv(noaxon,ny=1)
waxon<-process.csv(waxon,ny=1)

