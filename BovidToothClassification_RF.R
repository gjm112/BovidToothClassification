library(rpart)
library(randomForest)
library(plyr)

confusionList<-list()
logLossList<-list()

#20 PC
sampSizeVecSpecies<-c(0.8,1,1,1,0.8,0.9)
mtryVecSpecies<- c(40,30,20,30,50,60)

#load("~/Dropbox/brophyTeeth/ClassificationPaper/confusionList.RData")
#load("~/Dropbox/brophyTeeth/ClassificationPaper/logLossList.RData")
tooth <- c(paste("LM",1:3,sep=""),paste("UM",1:3,sep=""))

for (m in 1:6){print(tooth[m])

  file<-paste("~/Dropbox/brophyTeeth/data/",tooth[m],".csv",sep="")
lm2<-read.csv(file)

lm2$Tribe <- as.character(lm2$Tribe)
lm2$Tribe <- gsub("Hippotragus","Hippotragini",lm2$Tribe)
lm2$Tribe <- factor(lm2$Tribe)

#Splitting ther data into known and unknown
lm2$Tribe <- revalue(lm2$Tribe, c("unknown" = "Unknown"))
lm2<-subset(lm2,lm2$Tribe!="Unknown")
lm2Unk<-subset(lm2,lm2$Tribe=="Unknown")
lm2$Tribe<-factor(as.character(lm2$Tribe))
lm2$Species<-factor(as.character(lm2$Species))
names(lm2Unk)[10:69]<-names(lm2)[10:69]<-paste("Amp",1:60,sep="")

pc<-princomp(lm2[,paste("Amp",c(1:60),sep="")])$scores
lm2<-cbind(lm2,pc[,1:20])


#Spliting the data into validation sets.  
#Split into 6 groups for cross validation 
set.seed(1234)
groups <- rep(1:6,100)[1:dim(lm2)[1]]
lm2$group <- sample(groups,dim(lm2)[1],replace=FALSE)

#Remove one of our groups from the training data set.  
holdoutList <- list()
holdoutSpeciesList <- list()
for (i in 1:6){print(i)
  lm2Train <- lm2[!lm2$group==i,]
  lm2Holdout <- lm2[lm2$group==i,]
  
  
  #################################
  #Tribe Predicton
  #################################
  set.seed(1234)
  #cart <- rpart(formula,data=lm2Train,control=rpart.control( cp = 0.02,minsplit=5, minbucket=1))
  formula<-as.formula(paste("Tribe~",paste("Amp",1:60,sep="",collapse="+"),sep=""))
  sampsize <- dim(lm2Train)[1]
  cart <- randomForest(formula,data=lm2Train,ntree=1000,mtry=20,sampsize=sampsize,replace=FALSE)
  #rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,surrogatestyle = 0, maxdepth = 30, ...)
  #Now predict the hold out sample.  
  preds <- predict(cart,lm2Holdout,type="prob")
  preds<-t(apply(preds,1,function(x){
    x[x==1]<-0.999;x[x==0]<-0.001
    #Now normalize
    x <- x/sum(x)
    return(x)
  }))
  
  #################################
  #Species Prediction
  #################################
  predSpeciesList <- list()
  formula<-as.formula(paste("Species~",paste("Amp",1:60,sep="",collapse="+"),sep=""))
  tribes <- as.character(sort(unique(lm2Train$Tribe)))
  for (j in c(1,4:7)){
    temp <- subset(lm2Train, lm2Train$Tribe == tribes[j])
    temp$Species<-as.factor(as.character(temp$Species))
    sampsize <- round(dim(temp)[1]*sampSizeVecSpecies[m])
    cart <- randomForest(formula,data=temp,ntree=1000,mtry=mtryVecSpecies[m],sampsize=sampsize,replace=FALSE)
    predSpeciesList[[tribes[j]]]  <- predict(cart,lm2Holdout,type="prob")
    predSpeciesList[[tribes[j]]]<-t(apply(predSpeciesList[[tribes[j]]],1,function(x){
      x[x==1]<-0.999;x[x==0]<-0.001
      #Now normalize
      x <- x/sum(x)
      return(x)
    }))
  }
  
  predSpeciesList[[tribes[2]]] <- as.matrix(rep(1,dim(lm2Holdout)[1]))
  colnames(predSpeciesList[[tribes[2]]])<-"5"
  
  predSpeciesList[[tribes[3]]] <- as.matrix(rep(1,dim(lm2Holdout)[1]))
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
  
  
  
  
  holdoutList[[i]] <- cbind(lm2Holdout,preds)
  holdoutSpeciesList[[i]] <- cbind(lm2Holdout,predSpecies)
  
  
  
  
}

holdout <- do.call(rbind,holdoutList)
holdoutSpecies <- do.call(rbind,holdoutSpeciesList)
#table(holdout$Tribe,holdout$preds)

#Calculate log loss
probs<-as.numeric(apply(holdout,1,function(x){x[x["Tribe"]]}))
probsSpecies<-as.numeric(apply(holdoutSpecies,1,function(x){x[x["Species"]]}))

Tribetemp <- holdout[,c("Alcelaphini" , "Antilopini" ,  "Bovini"   ,    "Hippotragini", "Neotragini"  , "Reduncini"  ,  "Tragelaphini")]
Tribepreds <- colnames(Tribetemp)[max.col(Tribetemp,ties.method="first")]
confusionList[["rf"]][[tooth[m]]][["tribe"]] <- confusionMatrix(Tribepreds,holdout$Tribe)

holdoutSpecies$Species<-factor(holdoutSpecies$Species,levels=c(1:20))
Speciestemp <- holdoutSpecies[,as.character(c(1:20))]
Speciespreds <- as.character(colnames(Speciestemp)[max.col(Speciestemp,ties.method="first")])
Speciespreds <- factor(Speciespreds,levels=c(1:20))
#levels(Speciespreds) <- c(levels(Speciespreds),"12")
confusionList[["rf"]][[tooth[m]]][["species"]] <- confusionMatrix(Speciespreds,holdoutSpecies$Species)

logLossList[["rf"]][[tooth[m]]] <- c(tribe=mean(-log(probs)),species=mean(-log(probsSpecies)))

}


#save(logLossList,file="~/Dropbox/brophyTeeth/ClassificationPaper/logLossList.RData")
#save(confusionList,file="~/Dropbox/brophyTeeth/ClassificationPaper/confusionList.RData")

######################################################################
#####NOT RUN
######################################################################
#temp <- holdout[,91:97]
#Tribepreds <- colnames(temp)[max.col(temp,ties.method="first")]
#confusionMatrix(holdout$Tribe,Tribepreds)

#temp <- holdoutSpecies[,91:110]
#Speciespreds <- as.factor(colnames(temp)[max.col(temp,ties.method="first")])
#levels(Speciespreds) <- c(levels(Speciespreds),"12")
#confusionMatrix(holdoutSpecies$Species,Speciespreds)




