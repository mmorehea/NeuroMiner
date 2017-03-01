#pyramidals
library(cftf)

dat<-read.csv("../subsets/pyramidal_appended.csv")
y <- dat$Species.Name
x <- dat[ , 33:96]
z <- dat[ , 97:161]


## Set L and U  (P=60)
set.seed(100)
L=sample(1:n,ceiling(n*0.6))
U=setdiff(1:n,L)
y[U]=NA

## Exectute co-FTF_1( Random Forest)  takes about 5 minutes
crf<-cftf(x,z,y,k=5,L,U,learn="RF",type=1)