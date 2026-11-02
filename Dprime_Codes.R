sink("DifferentDprime.txt",split=TRUE)

DiscData <- read.table("discdatawithRT.txt",header=T)

## Counts number of unique subjects in the data

subjects <- unique(DiscData$Participant)
numofsubjects <- length(unique(DiscData$Participant))

## Counts the total number of Same/Different stimuli presented

Same <- vector()
Different <- vector()

## Counts the total number of Same/Different stimuli correctly identified.

DifferentStimDifferentResp <- vector()
#These are HITS.

SameStimDifferentResp <- vector()
##These are false alarms

##Dprime would be

dprimeDifferent <- vector()

##for loop for unique subjects

for (i in 1:numofsubjects) {
	Different[i] <- nrow(DiscData[DiscData$Different=="1" & DiscData$Participant==subjects[i],])
	Same[i] <- nrow(DiscData[DiscData$Different=="0" & DiscData$Participant==subjects[i],])
	
	DifferentStimDifferentResp[i] <- nrow(DiscData[DiscData$Different=="1" & DiscData$Different.resp=="1" & DiscData$Participant==subjects[i],])
	DifferentStimDifferentResp[i] <- ifelse(DifferentStimDifferentResp[i]==0,DifferentStimDifferentResp[i]+0.5,DifferentStimDifferentResp[i])
	DifferentStimDifferentResp[i] <-ifelse(DifferentStimDifferentResp[i]==Different[i],DifferentStimDifferentResp[i]-0.5,DifferentStimDifferentResp[i])
	
	SameStimDifferentResp[i] <- nrow(DiscData[DiscData$Different=="0" & DiscData$Different.resp=="1" & DiscData$Participant==subjects[i],])
	SameStimDifferentResp[i] <- ifelse(SameStimDifferentResp[i]==0,SameStimDifferentResp[i]+0.5,SameStimDifferentResp[i])
	SameStimDifferentResp[i] <- ifelse(SameStimDifferentResp[i]==Same[i],SameStimDifferentResp[i]-0.5,SameStimDifferentResp[i])
	
	dprimeDifferent[i] <- qnorm(DifferentStimDifferentResp[i]/Different[i])-qnorm(SameStimDifferentResp[i]/Same[i])
}

t1 <- t.test(dprimeDifferent)
print(paste("D-prime t-test results: Different correct"))
print(paste("t-test p value = ",t1$p.value))
print(paste("t-test statistic = ",t1$statistic))
print(paste("t-test degrees of freedom = ",t1$parameter))
print(paste("t-test p: ",t1$p.value," t: ",t1$statistic," df: ",t1$parameter))
print(paste("Mean D-Prime: ", mean(dprimeDifferent)))

sink()