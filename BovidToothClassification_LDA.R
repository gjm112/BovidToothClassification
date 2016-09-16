library(rpart)
library(randomForest)
library(MASS)
library(plyr)
library(caret)

logLossList <- list()
confusionList <- list()
#load("~/Dropbox/brophyTeeth/ClassificationPaper/confusionList.RData")
#load("~/Dropbox/brophyTeeth/ClassificationPaper/logLossList.RData")

tooth <- c(paste("LM",1:3,sep=""),paste("UM",1:3,sep=""))
for (m in 1:6){
  file <- paste("~/Dropbox/brophyTeeth/data/",tooth[m],".csv",sep="")
  lm1 <- read.csv(file)
  
  lm1$Tribe <- as.character(lm1$Tribe)
  lm1$Tribe <- gsub("Hippotragus","Hippotragini",lm1$Tribe)
  lm1$Tribe <- factor(lm1$Tribe)
  

#Adding Principal Components using the whole data with known and known
names(lm1)[10:69]<-names(lm1)[10:69]<-paste("Amp",1:60,sep="")

pc<-princomp(lm1[,paste("Amp",c(1:60),sep="")])$scores
lm1<-cbind(lm1,pc[,1:10])
names(lm1)[10:dim(lm1)[2]]<-paste("V",1:(dim(lm1)[2]-9),sep="")

#Using known part for modelling and testing.
lm1$Tribe <- revalue(lm1$Tribe, c("unknown" = "Unknown"))
lm1<-subset(lm1,lm1$Tribe!="Unknown")
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
for (i in 1:6){print(i)
  lm1Train <- lm1[!lm1$group==i,]
  lm1Holdout <- lm1[lm1$group==i,]
  
  
  #################################
  #Tribe Predicton
  #################################
  
  formula<-as.formula(paste("Tribe~",paste("V",1:70,sep="",collapse="+"),sep=""))
  sampsize <- ceiling(1.0*dim(lm1Train)[1])
  cart <- lda(formula,data=lm1Train)
  
  #Now predict the hold out sample.  
  preds <- predict(cart,lm1Holdout,type="prob")$posterior
  
  #################################
  #Species Prediction
  #################################
  predSpeciesList <- list()
  formula<-as.formula(paste("Species~",paste("V",1:(dim(lm1)[2]-10),sep="",collapse="+"),sep=""))
  tribes <- as.character(sort(unique(lm1Train$Tribe)))
  for (j in c(1,4:7)){print(j)
    temp <- subset(lm1Train, lm1Train$Tribe == tribes[j])
    temp$Species<-as.factor(as.character(temp$Species))
    sampsize <- ceiling(1.0*dim(temp)[1])
    cart <- lda(formula,data=temp)
    predSpeciesList[[tribes[j]]]  <- predict(cart,lm1Holdout,type="prob")$posterior
    predSpeciesList[[tribes[j]]] <- t(apply(predSpeciesList[[tribes[j]]],1,function(x){
      x[x>=0.999]<-0.999;x[x<0.001]<-0.001
      #Now normalize
      x <- x/sum(x)
      return(x)
    }))
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
  predSpecies<-predSpecies[,order(as.numeric(as.character(colnames(predSpecies))))]
  
  
  
  
  holdoutList[[i]] <- cbind(lm1Holdout,preds)
  holdoutSpeciesList[[i]] <- cbind(lm1Holdout,predSpecies)
  
  
  
  
}

holdout <- do.call(rbind,holdoutList)
holdoutSpecies <- do.call(rbind,holdoutSpeciesList)


#Calculate log loss
probs<-as.numeric(apply(holdout,1,function(x){x[x["Tribe"]]}))
#probsSpecies<-as.numeric(apply(holdoutSpecies,1,function(x){x[as.character(x["Species"])]}))

probsSpecies<-rep(NA,nrow(holdoutSpecies))

for (i in 1:nrow(holdoutSpecies)){
  probsSpecies[i]<-holdoutSpecies[i,as.character(holdoutSpecies$Species[i])]
}

Tribetemp <- holdout[,c("Alcelaphini" , "Antilopini" ,  "Bovini"   ,    "Hippotragini", "Neotragini"  , "Reduncini"  ,  "Tragelaphini")]
Tribepreds <- colnames(Tribetemp)[max.col(Tribetemp,ties.method="first")]
confusionList[["lda"]][[tooth[m]]][["tribe"]] <- confusionMatrix(Tribepreds,holdout$Tribe)

holdoutSpecies$Species<-factor(holdoutSpecies$Species,levels=c(1:20))
Speciestemp <- holdoutSpecies[,as.character(c(1:20))]
Speciespreds <- as.character(colnames(Speciestemp)[max.col(Speciestemp,ties.method="first")])
Speciespreds <- factor(Speciespreds,levels=c(1:20))
#levels(Speciespreds) <- c(levels(Speciespreds),"12")
confusionList[["lda"]][[tooth[m]]][["species"]] <- confusionMatrix(Speciespreds,holdoutSpecies$Species)

logLossList[["lda"]][[tooth[m]]] <- c(tribe=mean(-log(probs)),species=mean(-log(probsSpecies)))
}

#save(logLossList,file="~/Dropbox/brophyTeeth/ClassificationPaper/logLossList.RData")
#save(confusionList,file="~/Dropbox/brophyTeeth/ClassificationPaper/confusionList.RData")

#mean(-log(probs))
#mean(-log(probsSpecies))

