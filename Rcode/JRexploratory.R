axon <- x[,6]
summary(axon)
levels(axon) <- c("DSA", "DSNA")
yesaxon <- which(axon == "DSA")
noaxon <- which(axon == "DSNA")
means<-tapply(x[,40],x[,4],median)
#run g1 random forest from pyramid.R
cbind(summary(x[,4]),g1$confusion[,15],means)->ex
colnames(ex)[3] <- "Avg. Diameter"
colnames(ex)[2] <- "Prediction Error"
colnames(ex)[1] <- "n"
ex[order(ex[,2]),] #table with samples, error, avg. diameter
#It seems that species close in diameter (like cat/proechimys, and ones with
#diamter > 1.5) have higher error rates. Look to make random forest with some
#of these species excluded and see if error rates decrease
#why do mouse and rate differ in length by so much?
#go back and take out rows with "no diameter" under morphological attributes
#there are over 3000 rows with no diameter
#take out proechimys; take out minke or humpack whale; 
#take out guinea pig; take out dolphin or elephant; maybe try taking out
#chimp or rat
abs(outer(ex[,3],ex[,3],"-"))>0 & abs(outer(ex[,3],ex[,3],"-"))<0.06
#this is criteria you used for close avg diameters
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol

#2/17/17
#Took out proechimys, helped rat a bit but not cat
#more notes in pyramid.R
#mouse/rat average diameter is skewed
#3 rats are huge outliers in total volume, one rat huge outlier in avg diameter
boxplot(master$Average.Diameter~master[,4], pch = 16, col = cols)
cols <- rainbow(length(unique(master[,4]))+2)
boxplot(master$Total.Volume~master[,4], pch = 16, col = cols)
bigrats <- which(x[,4] == "rat")
which(x[bigrats,]$Total.Volume>300000)
xout <- x[-c(1353,3542,8718,9063),] #take out outlier rats ??
#Overall Depth and Depth are highly correlated so don't use 
summary(lm(x$Depth~x$Overall.Depth))
summary(lm(x$Surface~x$Total.Surface)) #correlation of 0.99
summary(lm(x$Total.Volume~x$Total.Surface))
summary(lm(x$EucDistance~x$PathDistance))
summary(lm(x$Soma.Surface~ x$Soma_Surface)) #correlation of 1
summary(lm(x$Total.Fragmentation~x$Fragmentation)) #correlation of 1

#2/21/17
master <- read.csv("NeuronDataMaster.csv", header = T)
master <- master[,-c(83,84,85)] #these are pk variables with many NA
master$Fractal_Dim<- as.numeric(master$Fractal_Dim) #Fractal_dim
master$Soma.Surface<-as.numeric(master$Soma.Surface)
isna <- is.na(master$Diameter_pow)
nas <- which(isna == TRUE)
master <- master[-nas,]
cors <- cor(master[,33:93])
cormat <- as.matrix(cors)
colnames(cormat) <- NULL
head(round(cormat,2))
high_cors <- which(cormat > 0.98, arr.ind = T) #maybe get down to lower cor later


same_var <-which(high_cors[,1] == high_cors[,2])


#REMOVE
N_stems or Number.of.Stems
##
plot(master$Number.of.Bifurcations, master$Number.of.Branches, pch = 16)
#factor 'type' variable
plot(master$Diameter, master$Diameter_pow, pch = 16)

#2/22/17
boxplot2(master$Average.Diameter~master[,4], pch = 16, col = cols)
boxplot2(master$Average.Diameter~bymeds, pch = 16, col = cols)
bymeds <- with(master, reorder(master[,4],master$Average.Diameter,median))
#bymeds used to order boxplot
library(gplots)
pdf("orderedbox.pdf",14,8)
boxplot(master$Average.Diameter~bymeds, pch = 16, col = cols, xaxt = "n",main = "Average Diameter by Species", ylab = expression(paste("Diameter (", mu,"m)")))
angleAxis(1,levels(bymeds), at = c(1:length(levels(bymeds))), srt = 35)
dev.off()

#reduced correlation master data frame
#keep soma.surface rm soma_surface
#remove all branches since it is linear combination
colnames(master)[c(36,54,55,56,57,62,65,66,76,78, 83:91)]
master <- master[,-c(36,54,55,56,57,62, 65,66,76,78, 83:91)] 

cors2 <- cor(master[,33:79])
cormat2 <- as.matrix(cors2)
colnames(cormat2) <- NULL
high_cors2 <- which(cormat2 > 0.98, arr.ind = T)
summary(master[,4])
#use removed correlation columns in pyramid.R

#2/23/17
read coftf paper and finally got cftf package running

#2/24/17
#overall depth v depth investigation
collect <- vector(length=dim(x)[1])
#which rows are these 2 not equal
for(i in 1:dim(x)[1])
collect[i] <- sum(x[i,39] != x[i,61])

diffs <- x[which(collect == 1),39] - x[which(collect == 1),61]
x[which(collect == 1)[3545],c(4,33:78)] #big difference over 1k square micron
plot(master$Depth, master$Overall.Depth, pch = 16, xlim = c(0,600), ylim = c(0,600))
which(diffs > 500)
#which of overall depth and depth should be used?
#Type is class integer but has too large of a range

#package nat for plotting neurons on github?
#think of more plots but need to ask Mark Culp about his cftf codes
#2/27/17
#writing Rmd file 
boxplot2(pyramid1$Average.Diameter~pyramid1[,4], pch = 16, col = cols)
boxplot2(pyramid1$Average.Diameter~bymeds, pch = 16, col = cols)
bymeds <- with(pyramid1, reorder(pyramid1[,4],pyramid1$Average.Diameter,median))
#bymeds used to order boxplot
library(gplots)
pdf("orderedbox.pdf",14,8)
boxplot(master$Average.Diameter~bymeds, pch = 16, col = cols, xaxt = "n",main = "Average Diameter by Species", ylab = expression(paste("Diameter (", mu,"m)")))
angleAxis(1,levels(bymeds), at = c(1:length(levels(bymeds))), srt = 35)
dev.off()

