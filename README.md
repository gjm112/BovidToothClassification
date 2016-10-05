# BovidToothClassification

This repository contains 6 .csv files containing the raw data and 5 .R files, one for each machine learning method examined.

#Data
There are 6 data files, one for each tooth, which are read into and used in each of the 5 .R files: 

 - LM1.csv
 -  LM2.csv
 -  LM3.csv
 - UM1.csv
 -  UM2.csv
 - UM3.csv
 - 
 #R code
There are 5 .R code files, one for each machine learning technique examined here.  Each file is named BovidToothClassification_[Method].  The methods considered here were: 

- Linear Discriminant Analysis (LDA)
- Nuclear Penalized Multinomial Regression (NPMR)
- Random Forests (RF)
- Support Vector Machines (SVM)
- Neural Networks (NNET)


The R code for each method runs 6-fold cross validation for a model classifying the tribe and a model classifying the species conditional on tribe.  Results of cross validation are measured in terms of log-loss and misclassification error.  

Any questions should be sent to: 
Gregory J. Matthews
gjm112@gmail.com

