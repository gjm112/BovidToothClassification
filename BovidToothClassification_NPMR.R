library(rpart)
library(randomForest)
library(MASS)
library(npmr)

#logLossList <- list()
#load("~/Dropbox/brophyTeeth/ClassificationPaper/confusionList.RData")
#load("~/Dropbox/brophyTeeth/ClassificationPaper/logLossList.RData")

tooth <- c(paste("LM",1:3,sep=""),paste("UM",1:3,sep=""))
for (m in 1:6){
  file <- paste("~/Dropbox/brophyTeeth/data/",tooth[m],".csv",sep="")
  lm1 <- read.csv(file)
  
  lm1$Tribe <- as.character(lm1$Tribe)
  lm1$Tribe <- gsub("Hippotragus","Hippotragini",lm1$Tribe)
  lm1$Tribe <- factor(lm1$Tribe)
  
#lm1<-read.csv("~/Dropbox/brophyTeeth/data/LM1.csv")

#Adding Principal Components using the whole data with known and known
names(lm1)[10:69]<-names(lm1)[10:69]<-paste("Amp",1:60,sep="")
pc<-princomp(lm1[,paste("Amp",c(1:60),sep="")])$scores
lm1<-cbind(lm1,pc[,1:10])
names(lm1)[10:dim(lm1)[2]]<-paste("V",1:(dim(lm1)[2]-9),sep="")
vars <- paste("V",1:(dim(lm1)[2]-9),sep="")

#Using known part for modelling and testing.

lm1$Tribe <- revalue(lm1$Tribe, c("unknown" = "Unknown"))
lm1<-subset(lm1,lm1$Tribe!="Unknown")
lm1[vars] <- scale(lm1[vars])
lm1$Tribe<-factor(as.character(lm1$Tribe))

# lm1$Species<-factor(as.character(lm1$Species))

#Spliting the data into validation sets.  
#Split into 6 groups for cross validation 
set.seed(1234)
groups <- rep(1:6,100)[1:dim(lm1)[1]]
lm1$group <- sample(groups,dim(lm1)[1],replace=FALSE)

#Remove one of our groups from the training data set.  
holdoutList <- list()
holdoutSpeciesList <- list()

for (i in 1:6){
  lm1Train <- lm1[!lm1$group==i,]
  lm1Holdout <- lm1[lm1$group==i,]
  
  
  #################################
  #Tribe Predicton
  #################################

  Y <- as.numeric(lm1Train$Tribe)
  X <- as.matrix(lm1Train[vars])
  nuke <- npmr(X,Y,lambda = 18.507)
  
  #Now predict the hold out sample.  
  Xnew <- as.matrix(lm1Holdout[vars])
  preds <- as.data.frame(predict(nuke,Xnew))
  colnames(preds) <- as.character(sort(unique(lm1Train$Tribe)))
  #################################
  #Species Prediction
  #################################
  predSpeciesList <- list()
  tribes <- as.character(sort(unique(lm1Train$Tribe)))
  for (j in c(1,4:7)){print(j)
    temp <- subset(lm1Train, lm1Train$Tribe == tribes[j])
    Y<-as.factor(as.character(temp$Species))
    X <- as.matrix(temp[vars])
    nuke <- npmr(X,Y,lambda = 4)
    predSpeciesList[[tribes[j]]]  <- data.frame(predict(nuke,Xnew)[,,1])
    colnames(predSpeciesList[[tribes[j]]])<-as.character(levels(Y))
  }
  
  
  
  
  predSpeciesList[[tribes[2]]] <- as.matrix(rep(1,dim(lm1Holdout)[1]))
  colnames(predSpeciesList[[tribes[2]]])<-"5"
  
  predSpeciesList[[tribes[3]]] <- as.matrix(rep(1,dim(lm1Holdout)[1]))
  colnames(predSpeciesList[[tribes[3]]])<-"9"
  
  ##################################################################
  #Combine Species and Tribe Prediction
  ##################################################################
  predSpeciesTribeList<-list()
  for (tribe in tribes){
    predSpeciesTribeList[[tribe]] <- preds[,colnames(preds)==tribe]*predSpeciesList[[tribe]]
  }

  predSpecies <- do.call(cbind,predSpeciesTribeList)
  colnames(predSpecies)<-as.character(c(1:5,9,14:16,10:13,17:20,6:8))
  predSpecies<-predSpecies[,order(as.numeric(as.character(colnames(predSpecies))))]
  
  
  
  
  holdoutList[[i]] <- cbind(lm1Holdout,preds)
  holdoutSpeciesList[[i]] <- cbind(lm1Holdout,predSpecies)
  
  
  
  
}

holdout <- do.call(rbind,holdoutList)
holdoutSpecies <- do.call(rbind,holdoutSpeciesList)


#Calculate log loss
probs<-as.numeric(apply(holdout,1,function(x){x[x["Tribe"]]}))
probsSpecies<-rep(NA,nrow(holdoutSpecies))

for (i in 1:nrow(holdoutSpecies)){
probsSpecies[i]<-holdoutSpecies[i,as.character(holdoutSpecies$Species[i])]
}

#<-as.numeric(apply(holdoutSpecies,1,function(x){x[as.character(x["Species"])]}))

Tribetemp <- holdout[,c("Alcelaphini" , "Antilopini" ,  "Bovini"   ,    "Hippotragini", "Neotragini"  , "Reduncini"  ,  "Tragelaphini")]
Tribepreds <- colnames(Tribetemp)[max.col(Tribetemp,ties.method="first")]
confusionList[["npmr"]][[tooth[m]]][["tribe"]] <- confusionMatrix(Tribepreds,holdout$Tribe)

holdoutSpecies$Species<-factor(holdoutSpecies$Species,levels=c(1:20))
Speciestemp <- holdoutSpecies[,as.character(c(1:20))]
Speciespreds <- as.character(colnames(Speciestemp)[max.col(Speciestemp,ties.method="first")])
Speciespreds <- factor(Speciespreds,levels=c(1:20))
#levels(Speciespreds) <- c(levels(Speciespreds),"12")
confusionList[["npmr"]][[tooth[m]]][["species"]] <- confusionMatrix(Speciespreds,holdoutSpecies$Species)

LogLoss <- mean(-log(probs))
LogLossSpecies <- mean(-log(probsSpecies))

logLossList[["npmr"]][[tooth[m]]] <- c(tribe=mean(-log(probs)),species=mean(-log(probsSpecies)))
}


#save(logLossList,file="~/Dropbox/brophyTeeth/ClassificationPaper/logLossList.RData")
#save(confusionList,file="~/Dropbox/brophyTeeth/ClassificationPaper/confusionList.RData")
