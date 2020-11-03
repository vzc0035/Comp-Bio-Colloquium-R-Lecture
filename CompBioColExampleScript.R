#upload your dataset
datum=read.csv(file.choose())

#make sure it uploaded correctly
head(datum)

#load necessary packages
library(lme4)
library(nlme)

#run linear models
resultsmass=lme(Mass~Sex*Treatment+Age*Treatment+HatchOrder+BroodSize,data=datum,random=~1|ï..Nest/Individual,na.action=na.exclude)
resultsrt=lme(RTarsus~Sex*Treatment+Age*Treatment+HatchOrder+BroodSize,data=datum,random=~1|ï..Nest/Individual,na.action=na.exclude)
resultslt=lme(LTarsus~Sex*Treatment+Age*Treatment+HatchOrder+BroodSize,data=datum,random=~1|ï..Nest/Individual,na.action=na.exclude)
resultstasymm=lme(DiffRLTarsus~Sex*Treatment+Age*Treatment+HatchOrder+BroodSize,data=datum,random=~1|ï..Nest/Individual,na.action=na.exclude)
resultsrw=lme(RWing~Sex*Treatment+Age*Treatment+HatchOrder+BroodSize,data=datum,random=~1|ï..Nest/Individual,na.action=na.exclude)
resultslw=lme(LWing~Sex*Treatment+Age*Treatment+HatchOrder+BroodSize,data=datum,random=~1|ï..Nest/Individual,na.action=na.exclude)
resultswasymm=lme(DiffRLWing~Sex*Treatment+Age*Treatment+HatchOrder+BroodSize,data=datum,random=~1|ï..Nest/Individual,na.action=na.exclude)

#look at results
summary(resultsmass)
summary(resultsrt)
summary(resultslt)
summary(resultstasymm)
summary(resultsrw)
summary(resultslw)
summary(resultswasymm)

#models to analyze age separately (right tarsus, left tarsus, and tarsus asymmetry)
resultsrt=lme(RTarsus~Sex*Treatment+HatchOrder+BroodSize,data=datum,subset=c(Age=="5"),random=~1|ï..Nest/Individual,na.action=na.exclude)
resultsrt=lme(RTarsus~Sex*Treatment+HatchOrder+BroodSize,data=datum,subset=c(Age=="10"),random=~1|ï..Nest/Individual,na.action=na.exclude)
resultsrt=lme(RTarsus~Sex*Treatment+HatchOrder+BroodSize,data=datum,subset=c(Age=="16"),random=~1|ï..Nest/Individual,na.action=na.exclude)
resultsrt=lme(RTarsus~Sex*Treatment+HatchOrder+BroodSize,data=datum,subset=c(Age=="25"),random=~1|ï..Nest/Individual,na.action=na.exclude)
resultslt=lme(LTarsus~Sex*Treatment+HatchOrder+BroodSize,data=datum,subset=c(Age=="5"),random=~1|ï..Nest/Individual,na.action=na.exclude)
resultslt=lme(LTarsus~Sex*Treatment+HatchOrder+BroodSize,data=datum,subset=c(Age=="10"),random=~1|ï..Nest/Individual,na.action=na.exclude)
resultslt=lme(LTarsus~Sex*Treatment+HatchOrder+BroodSize,data=datum,subset=c(Age=="16"),random=~1|ï..Nest/Individual,na.action=na.exclude)
resultslt=lme(LTarsus~Sex*Treatment+HatchOrder+BroodSize,data=datum,subset=c(Age=="25"),random=~1|ï..Nest/Individual,na.action=na.exclude)
resultstasymm=lme(DiffRLTarsus~Sex*Treatment+HatchOrder+BroodSize,data=datum,subset=c(Age=="5"),random=~1|ï..Nest/Individual,na.action=na.exclude)
resultstasymm=lme(DiffRLTarsus~Sex*Treatment+HatchOrder+BroodSize,data=datum,subset=c(Age=="10"),random=~1|ï..Nest/Individual,na.action=na.exclude)
resultstasymm=lme(DiffRLTarsus~Sex*Treatment+HatchOrder+BroodSize,data=datum,subset=c(Age=="16"),random=~1|ï..Nest/Individual,na.action=na.exclude)
resultstasymm=lme(DiffRLTarsus~Sex*Treatment+HatchOrder+BroodSize,data=datum,subset=c(Age=="25"),random=~1|ï..Nest/Individual,na.action=na.exclude)

#models to analyze sex separately (tarsus asymmetry 5 dph and 16 dph)
#NOTE: make new .csv file with just 5dph data and another .csv with just 16 dph data
#upload .csv with just 5 dph data
datum=read.csv(file.choose())
head(datum)
resultstasymm=lme(DiffRLTarsus~Treatment+HatchOrder+BroodSize,data=datum,subset=c(Sex=="F"),random=~1|Nest/Individual,na.action=na.exclude)
resultstasymm=lme(DiffRLTarsus~Treatment+HatchOrder+BroodSize,data=datum,subset=c(Sex=="M"),random=~1|Nest/Individual,na.action=na.exclude)
summary(resultstasymm)
#upload .csv with just 16 dph data
datum=read.csv(file.choose())
head(datum)
resultstasymm=lme(DiffRLTarsus~Treatment+HatchOrder+BroodSize,data=datum,subset=c(Sex=="F"),random=~1|Nest/Individual,na.action=na.exclude)
resultstasymm=lme(DiffRLTarsus~Treatment+HatchOrder+BroodSize,data=datum,subset=c(Sex=="M"),random=~1|Nest/Individual,na.action=na.exclude)
summary(resultstasymm)

#plot your data
plot(Mass~Treatment,data=datum)
plot(RTarsus~Treatment,data=datum)
plot(LTarsus~Treatment,data=datum)
plot(DiffRLTarsus~Treatment,data=datum)
plot(RWing~Treatment,data=datum)
plot(LWing~Treatment,data=datum)
plot(DiffRLWing~Treatment,data=datum)

