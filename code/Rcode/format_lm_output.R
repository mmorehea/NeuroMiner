
#below takes swc files by 500 (or whatever number) and writes an output text file
aaa <- paste("-f",1:44,",0,0,10.0", collapse =" " , sep ="" ) #for easy lm functions
mtable1 <- NULL
for(j in 62201:length(sam)){
tt <- c(paste("C:\\Users\\einstein\\Desktop\\Lmv5.3\\Lm.exe","-l1,3,8,1.0",aaa,"-sC:\\Users\\einstein\\Desktop\\Lmout.txt",paste("C:\\Users\\einstein\\Desktop\\NeuroMiner\\data_sets\\swc_files\\",sam, sep ="" , collapse =" "  )))
system(tt)
ems <- read.table("C:\\Users\\einstein\\Desktop\\Lmout.txt", fill = T, col.names = paste0("V", seq_len(10)), header = F )
#get rid of swc output in ems
keep <- ems[grep("C", ems[,1]),]
#to format lm output
#read table
#rows need to be condensed
#mm <- read.table("C:\\Users\\einstein\\Desktop\\lmout.txt", fill = TRUE)

ne <- sapply(keep[,1], function(x) gsub("C:\\Users\\einstein\\Desktop\\NeuroMiner\\data_sets\\swc_files\\", "", x, fixed = TRUE))
#remove unnecessary wording
names(ne) <- NULL
n1 <- sapply(ne, function(x) gsub(".swc","",x))
names(n1) <- NULL
keep[,1] <- n1
#now switch format of table
#data is in long form switch to wide

#for loop to obtain correct values 
keep <- keep[,-6]
for(k in 4:7)
keep[,k] <- as.numeric(as.character(keep[,k])) #warning msg is fine
keep2 <- keep[,c(1,2)]
keep2[,2] <- as.character(keep2[,2])

for(i in 1:dim(keep2)[1]){
if(keep2[i,2] == "Diameter" || keep2[i,2] == "Partition_asymmetry" || keep2[i,2] == "Contraction" 
	|| substr(keep2[i,2],1,2) == "Pk" || keep2[i,2] == "Diameter_pow" || keep2[i,2] == "Fractal_Dim" 
	|| substr(keep2[i,2],1,3) == "Bif" || keep2[i,2] == "Helix" || keep2[i,2] == "Rall_Power"
	|| keep2[i,2] == "Parent_Daughter_Ratio" || keep2[i,2] == "Daughter_Ratio") {keep2[i,3] <- keep[i,7]}
 else if(keep2[i,2] == "EucDistance" || keep2[i,2] == "PathDistance" || keep2[i,2] == "Branch_Order"
		|| keep2[i,2] == "Terminal_degree" || keep2[i,2] == "Last_parent_diam") {keep2[i,3] <- keep[i,8]}
else keep2[i,3] <- keep[i,4]
}
keep2[,1] <- factor(keep2[,1])
keep2[,2] <- factor(keep2[,2])
#error is because keep numbers are factors

library(reshape2)
mtable <- dcast(keep2, V1~V2, value.var = "V3")
#check it head(mtable)
mtable <- mtable[,-c(44,45)]
colnames(mtable)[1] <- "Neuron.Name"
mtable1 <- rbind(mtable1,mtable)
write.table(mtable1, paste(C:\\Users\\einstein\\Desktop\\,paste(out,312,.txt,sep=),sep=))
}
#####
#numeric columns may be character class
#mtable[,2:42] <- lapply(mtable[,2:42], function(x) as.numeric(x))
now for neuron info from site
site1 <- read.csv(C:\\Users\\einstein\\Desktop\\neuroData1.csv)
#easy
site1 <- site1[,-1] #dont need this column
#now need to combine all txt files

len <- NULL
dat <- NULL
for(i in 1:length(list.files())){
 dat1 <- read.table(list.files()[i], header = T)
#len <- c(len, dim(dat1)[1])
 dat <- rbind(dat, dat1)
 }
#why losing obervations?? 18332 to 18329 for existing swc files
for(i in 4501:5000){
tocop <- paste(getwd(),/,list.files()[i],sep = )
file.copy(from = tocop, to = "C:/Users/einstein/Desktop/out10swc")
}
#5/3/17
#neurodata1 has 347 neurons from old master that needed data from neuromorpho again
#new master is in data folder
for(i in 33:53)
existing[sav,i] <- site1[sav,i]
#get lmeasure again for 347 obs from site1 to combine with neuromorpho.org data
if(as.character(keep[1:44,2]) == "N_stems"

#5/8/17
#DONT FORGET TO SAVE SCRIPTS!
#still losing neurons
#newmaster has some duplicates....
write.table(dat[,1], names.p, row.names = F, col.names = F, append = T)
#remove duplicates
#brute force
for(i in names(summary(master[,1])))
 print(paste("master[c(",which(master[,1] == i)[1],",",which(master[,1] == i)[2],"),]", sep = "")

rms2 <- c(27143,27142,27146,27145,27148,27149,27352,27353,27375,27376,27381,27382,27384,27385,27548,27549,38177,38178,38180,38181,40427,40429,40433,40434,40965,40966,41552,41553,41771,41772,41775,41776,43113,43114,43189,43190,44412,44414,44415,44417,47983,47984,51760,51761,51763,51764,62643,62644,62646,62647,62650,62651,62652,62653)
rms1 <- c(301,304,306,307,309,312,313,316,318,320,420,567,570,572,573,575,577,579,590,591,594,610,707,710,712,714,715,718,720,722,725,726,737,739,740,743,835,837,839,841,844,845,848,851,853,854,1082,1314,1325,1338,1627,1629,1632,1633,1636,1638,1640,1642,1644,1647,1648,1651,1653,1654,1656,1658,1660,1663,1665,1667,1668,1671)
rms3 <- rems #after removing only rms1 and rms2
rms4 <- rems #remove rms1, rms2, and rms3 first
rms5 <- rems #after all above are rm
rms <- c(rms1, rms2, rms3, rms4, rms5)

library(data.table)#fread is faster
master <- fread("C:\\Users\\einstein\\Desktop\\NeuroMiner\\data_sets\\NoDuplicatesNeuroDataMaster.csv")
master <- master[,-1]
master <- master[-rms,]

write.csv(master, "NoDuplicatesNeuronMaster.csv")
# see below for how to get computer to do it instead

nms <- names(summary(master[,1], maxsum = 500))
#more 5/9/17
 coll <- NULL
for(i in nms)
coll <- c(coll,which(master[,1] == i)[1])
  Ts <- vector(length = length(coll))
for(i in 1:length(Ts))
if((master[coll[i],33] == master[(1+coll[i]),33]) == TRUE) Ts[i] <- "Yes" else Ts[i] <- "NO"

rems <- NULL

for(i in 1:length(Ts)){
	if(Ts[i] == "NO"){
		if((master[coll[i],33] < master[(1 + coll[i]), 33]) == TRUE) rems[i] <- row.names(master[coll[i],]) else rems[i] <- row.names(master[(1 + coll[i]),])}
	else if(Ts[i] == "Yes"){
		if((nchar(as.character(master[c(coll[i], 1+coll[i]), 26]))[1] > nchar(as.character(master[c(coll[i], 1+coll[i]), 26]))[2]) == TRUE){ rems[i] <- row.names(master[(1 + coll[i]),])} else {rems[i] <- row.names(master[coll[i],])}
}
}
rems <- as.numeric(rems)
#5/10/17
#some swc files need standaradized??
ords <- order(master$Width - master$Overall.Width)
master[tail(ords),]
ss[i] <- which(list.files() == paste(master[tail(ords),1][i], ".swc", sep = ""))
library(nat) #to read neuron swc file

#fix differences in swc file line 1 now
ss <- read.neuron(filess[1])
mat <- ss$d[,c(3,4,5)]

lis <- apply(mat, 1,function(x) x - mat[1,])
coll <- NULL
for(j in 1:length(lis))
coll <- rbind(coll, lis[[j]])
ss$d[,c(3,4,5)] <- coll
write.neuron(ss, paste("C:\\Users\\einstein\\Desktop\\fixedswc\\",filess[1], sep = ""))

coll12 <- NULL
samp <- sample(1:62302, 5000)
for(i in 1:5000){
	ss <- read.neuron(sam[samp[i]])
	if(ss$d[1,7] == -1){
		coll12[i] <- sum(ss$d[1,c(3,4,5)] == 0)}
	else {coll12[i] <- NA}
}
library(parallel)
library(foreach)
library(doParallel)
detectCores()
cl <- makeCluster(15)
registerDoParallel(cl)
coll12 <- NULL
sav <- foreach(i = 1:length(sam), .combine = c, .packages = "nat") %dopar% {
tryCatch({
	ss <- read.neuron(sam[i])
		if(ss$d[1,7] == -1){
		return(sum(ss$d[1,c(3,4,5)] == 0))}
	else {return(NA)}}, error = function(e) {cat("Error in",conditionMessage(e), "\n")})
}

head(master[which(sav<3),])
whichs <- master[which(sav < 3),]
whichs <- whichs[-c(6937:7025),]
whichs <- whichs[-c(6933:6936),]

xx <- whichs$Width - whichs$Overall.Width
