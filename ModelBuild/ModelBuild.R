library('caret')
library("lattice")
library('tree')
library('randomForest')
library('gbm')


### Decision Tree Analysis
# Builds a classic decision tree using k-fold cross-validation
# Each tree will be displayed to the console as well as a confusion
#   for each fold
# The average accuracy (TPR) will be returned
DecisionTree <- function(folds,train.set) {
  
  acc = c()
  for (i in 1:length(folds)) {
    # Build train and test set from fold
    trainI = unlist(folds[-i])
    tr.dat = train.set[trainI,]
    te.dat = train.set[-trainI,]
    
    # Create tree model
    model = tree(Result~.,data=tr.dat,control=tree.control(mindev = 0,nobs=nrow(tr.dat)))
    print(model)
    
    # Make predictions based on model
    predictions = predict(model, newdata = te.dat)
    n = length(predictions)
    for (i in 1:n){
      if (predictions[i] > 0.5){
        predictions[i] = 1
      }
      else{
        predictions[i] = 0
      }
    }
    
    # Calculate true positive rate
    correct = sum(predictions[which(predictions==1)]==te.dat$Result[which(predictions==1)])
    pred = length(te.dat$Result[which(predictions==1)])
    tpr = correct/pred

    # Create and display confusion matrix
    predDF = data.frame(prediction = as.factor(predictions), true = te.dat$Result)
    print(table(predDF))
    acc = c(acc,tpr)
    
    # Plot Tree
    #plot(fit, uniform=TRUE,
    #     main="Classification Tree for Kyphosis")
    #text(fit, use.n=TRUE, all=TRUE, cex=.8)
  }
  
  return (mean(acc))
}


###  Random Forest
# Try 5 different values for p (size of each bag)
# The best value's TPR will be displayed
doRF <- function(folds, train.set){

  ps = c(5,10,15,20,27)
  bestP = NA
  maxAcc = 0
  accs=c()
  for (p in ps) {
    print(p)
    newAcc = rf(folds, train.set, p)
    accs = c(accs,newAcc)
    if (newAcc>maxAcc) {
      maxAcc = newAcc
      bestP = p
    }
  }
  
  return(maxAcc)
}


# Cross validates on the number of predictors considered at each split
rf <- function(folds,train.set,mtry) {
  
  acc = c()
  for (i in 1:length(folds)) {
    # Build train and test set from fold
    trainI = unlist(folds[-i])
    tr.dat = train.set[trainI,]
    te.dat = train.set[-trainI,]

    # Create RandomForest model
    model = randomForest(Result~.,data=tr.dat, mtry = mtry,n.tree=100)

    # Make predictions based on model
    predictions = predict(model, newdata = te.dat,type='response')
    n = length(predictions)
    for (i in 1:n){
      if (predictions[i] > 0.5){
        predictions[i] = 1
      }
      else{
        predictions[i] = 0
      }
    }
    
    # Calculate true positive rate
    correct = sum(predictions[which(predictions==1)]==te.dat$Result[which(predictions==1)])
    pred = length(te.dat$Result[which(predictions==1)])
    tpr = correct/pred
    
    # Create and display confusion matrix
    predDF = data.frame(prediction = as.factor(predictions), true = te.dat$Result)
    print(table(predDF))
    acc = c(acc,tpr)
  }
  
  return (mean(acc))
}


### Boosted Tree
# Iteratively creates and runs a boosted tree model with the following parameters:
#   bag.fraction=.5
#   shrinkage = .01 (potentially make it smaller if algorithm is fast)
#   Cross validate on a grid of interaction depth and number of iterations.
#   For interaction depth: {1,2,4,6}
#   For number of iterations (trees): {30, 50, 70, 90}
doBoost <- function(folds, train.set){
  lambda=.01
  bag.frac = .5
  ids = c(1,2,4,6)
  n.trees = c(300,500,700,900)
  accs=c()
  bestID = NA
  bestTr = NA
  maxAcc = 0
  
  for (n.tr in n.trees) {
    for (id in ids) {
      print(paste("The interaction depth is: ",id,sep=""))
      print(paste("The number of trees is: ",n.tr,sep=""))
      newAcc = boostTree(folds, train.set, lambda, bag.frac, id, n.tr)
      print(newAcc)
      accs = c(accs,newAcc)
      if (newAcc > maxAcc){
        maxAcc = newAcc
        bestID = id
        bestTr = n.tr
      }
    }
  }
  
  return (list("maxAcc"=maxAcc, "bestID"=bestID, "bestTr"=bestTr))
}

# Creates and runs a boosted tree model
# The smaller the shrinkage parameter, the better predictive performance, but the worse the computational performance
# Bernoulli loss for classification
boostTree <- function(folds, train.set, lambda, bag.frac, inter.depth, n.tr) {
  
  acc = c()
  for (i in 1:length(folds)) {
    # Build train and test set from fold
    trainI = unlist(folds[-i])
    tr.dat = train.set[trainI,]
    te.dat = train.set[-trainI,]
    
    # Create Boosted Tree model
    model = gbm(Result~., data=tr.dat, distribution="bernoulli", n.trees=n.tr,
                shrinkage=lambda, interaction.depth=inter.depth, bag.fraction=bag.frac)
    
    # Make predictions based on model
    predictions = predict(model, newdata = te.dat, n.trees=n.tr, type='response')
    n = length(predictions)
    for (i in 1:n){
      if (predictions[i] > 0.5){
        predictions[i] = 1
      }
      else{
        predictions[i] = 0
      }
    }
    
    # Calculate true positive rate
    correct = sum(predictions[which(predictions==1)]==te.dat$Result[which(predictions==1)])
    pred = length(te.dat$Result[which(predictions==1)])
    tpr = correct/pred
    
    # Create and display confusion matrix
    predDF = data.frame(prediction = as.factor(predictions), true = te.dat$Result)
    print(table(predDF))
    acc = c(acc,tpr)
  }
  
  return (mean(acc))
}


### Logistic Regression
# Cross validates on the training set to select the number of predictors to sample from at each split
LogReg <- function(folds, train.set, formula) {
  
  acc = c()
  for (i in 1:length(folds)) {
    # Build train and test set from fold
    trainI = unlist(folds[-i])
    tr.dat = train.set[trainI,]
    te.dat = train.set[-trainI,]
    
    # Create Logistic Regression Model
    model = glm(formula, data=tr.dat, family=binomial)
    print(summary(model))

    # Make predictions based on model
    predictions = predict(model, newdata = te.dat, type='response')
    n = length(predictions)
    for (i in 1:n){
      if (predictions[i] > 0.5){
        predictions[i] = 1
      }
      else{
        predictions[i] = 0
      }
    }

    # Calculate true positive rate
    correct = sum(predictions[which(predictions==1)]==te.dat$Result[which(predictions==1)])
    pred = length(te.dat$Result[which(predictions==1)])
    tpr = correct/pred

    # Create and display confusion matrix
    predDF = data.frame(prediction = as.factor(predictions), true = te.dat$Result)
    print(table(predDF))
    acc = c(acc,tpr)
  }
  
  return (mean(acc))
}

### Evaluate
# Evaluate predictions from model using a provided threshold. 
# Returns the amount of times the predictions were correct as well as 
#   what was predicted each time
Evaluate <- function(thresh,predictions,test.Result){
  
  # Make predictions based on threshold
  tp = 0
  tn = 0
  no = 0
  yes = 0
  for (i in 1:length(predictions)){
    if (predictions[i] > thresh){
      predictions[i] = 1
    }
    else{
      predictions[i] = 0
    }
    
    # Increment total true and true positive count
    if (test.Result[i] == 1){
      if (predictions[i] == 1){
        tp = tp + 1
      }
      yes = yes + 1
    }
    else{
      if (predictions[i] == 0){
        tn = tn + 1
      }
      no = no + 1
    }
  }
  
  # Create and display confusion matrix
  predDF = data.frame(true = test.Result, prediction = as.factor(predictions))
  print(table(predDF))
  
  return (c(tp, yes, tn, no))
}


# Save dataframe to file
saveDF <- function(df, name){
  # Save matchup table
  saveRDS(df,paste(name," Data.rda",sep=""))
  write.csv(df,paste(name,".csv",sep=""))
}



#####           MAIN           #####
needSplit = TRUE                   # Set to TRUE if dataset needs split
evalModels = TRUE                  # Set to TRUE from model evaluation. Will slow performance
CreateModel = TRUE                  # Set to TRUE if model needs created

# Load final filtered dataset
set.seed(1)
matchups <<- read.csv("./Data/FilteredMatchups2.csv")
matchups$X = NULL

# Split set into train and test
if(needSplit){
  n = nrow(matchups)
  train.set = data.frame(matrix(nrow=0,ncol=65))
  test.set = data.frame(matrix(nrow=0,ncol=65))
  colnames(train.set) = colnames(matchups)
  colnames(test.set) = colnames(matchups)
  for (i in 1:n){
    if (i <= (n*0.66)){
      train.set = rbind(train.set,matchups[i,])
    }
    else{
      test.set = rbind(test.set,matchups[i,])
    }
  }
}

### Begin Model Evaluation

#Create the 7 folds
folds = createFolds(1:nrow(train.set),k=7)
if (evalModels){
  ## Decision Tree
  treeTPR=DecisionTree(folds, train.set)
  
  ## Random Forest
  rfTPR = doRF(folds, train.set)
  
  ## Boosted Tree
  btOpt = doBoost(folds, train.set)
  
  ## Logistic Regression
  train.set$Result = as.factor(train.set$Result)      # make Result a factor
  formula = as.formula(Result~.)
  lrTPR = LogReg(folds, train.set, formula)
}


### Begin Model Creation and Testing
# Best model: Boosted Tree
#   Interaction Depth = 1
#   Bag Fraction = 0.5
#   number of trees = 300
#   lambda (shrinkage) = 0.01
#   TPR = 1.00
#   Bernoulli loss for classification

# Second Best Model: Random Forest
#   number of trees = 100
#   Bag Size (p) = 15

if (isTRUE(CreateModel)){
  # Fit this model on the training set
  final.Model.Boost = gbm(Result~., data=train.set, distribution="bernoulli", n.trees=300,
                    shrinkage=0.01, interaction.depth=1, bag.fraction=0.5)
  final.Model.RF = randomForest(Result~.,data=train.set, mtry = 15, n.tree=100)
  
  tpBoost = c(0,0,0,0,0,0)         #50, 60, 70, 80, 90, 95
  tpRF = c(0,0,0,0,0,0)            #50, 60, 70, 80, 90, 95
  yesBoost = c(0,0,0,0,0,0)        #50, 60, 70, 80, 90, 95
  yesRF = c(0,0,0,0,0,0)           #50, 60, 70, 80, 90, 95
  tnBoost = c(0,0,0,0,0,0)         #50, 60, 70, 80, 90, 95
  tnRF = c(0,0,0,0,0,0)            #50, 60, 70, 80, 90, 95
  noBoost = c(0,0,0,0,0,0)         #50, 60, 70, 80, 90, 95
  noRF = c(0,0,0,0,0,0)            #50, 60, 70, 80, 90, 95
  
  # Predict test set records based on model created from training set
  rownames(test.set) = 1:nrow(test.set)
  
  # Make predictions using models
  predictionsBoost = predict.gbm(final.Model.Boost, newdata=test.set[,(1:length(test.set)-1)], n.trees=300)
  avgBoost = mean(predictionsBoost,1)
  predictionsRF = predict(final.Model.RF, newdata=test.set[,(1:length(test.set)-1)])
  
  
  ## Evaluate model using different thresholds
  threshold = c(0.50, 0.60, 0.70, 0.80, 0.90, 0.95)
  for (i in 1:length(threshold)){
    print(paste("Boost with threshold: ", threshold[i],sep=""))
    evalBoost = Evaluate(avgBoost*threshold[i],predictionsBoost,test.set$Result)
    print(paste("RF with threshold: ", threshold[i],sep=""))
    evalRF = Evaluate(threshold[i],predictionsRF,test.set$Result)
    
    # True positive count
    tpBoost[i] = tpBoost[i]+evalBoost[1]
    tpRF[i] = tpRF[i]+evalRF[1]
    
    # Actual yes count
    yesBoost[i] = yesBoost[i]+evalBoost[2]
    yesRF[i] = yesRF[i]+evalRF[2]
    
    # True negative count
    tnBoost[i] = tnBoost[i] + evalBoost[3]
    tnRF[i] = tnRF[i] + evalRF[3]
    
    # Actual no count
    noBoost[i] = noBoost[i] + evalBoost[4]
    noRF[i] = noRF[i] + evalRF[4]
  }
  
  # Analyzing the results
  threshold = c(50,60,70,80,90,95)
  tprBoost = c(0,0,0,0,0,0)
  tprRF = c(0,0,0,0,0,0)
  specificityBoost = c(0,0,0,0,0,0)
  specificityRF = c(0,0,0,0,0,0)
  for (i in 1:length(threshold)){
    tprBoost[i] = tpBoost[i]/yesBoost[i]
    tprRF[i] = tpRF[i]/yesRF[i]
    specificityBoost[i] = tnBoost[i]/noBoost[i]
    specificityRF[i] = tnRF[i]/noRF[i]
  }
  
  # Create data frame of results and plot
  ResultFrameBoost = data.frame(Threshold = threshold, Specificity=specificityBoost, TPR=tprBoost)
  ResultFrameRF = data.frame(Threshold = threshold, Specificity=specificityRF, TPR=tprRF)
  SaveDF(ResultFrameBoost, "./Results/Boost")
  SaveDF(ResultFrameRF, "./Results/RF")
  
  p1 <- ggplot(data=ResultFrameBoost,
         aes(x=Specificity,y=TPR,group=1))+
    geom_line()+geom_point()+
    xlab("Specificity")+
    ylab("True Positive Rate")+
    ggtitle("ROC Curve for GPM")
  
  p2 <- ggplot(data=ResultFrameRF,
               aes(y=Specificity,x=TPR,group=1))+
    geom_line()+geom_point()+
    xlab("Specificity")+
    ylab("True Positive Rate")+
    ggtitle("ROC Curve for GPM")
  
  p3 <- ggplot(data=ResultFrameRF,
               aes(x=Threshold,y=TPR,group=1,color=TPR))+
    geom_line()+geom_point()+ 
    xlab("Threshold")+ylab("True Positive Rate")+
    ggtitle("TPR vs Prediction Confidence (RF)")
  
  p3 <- ggplot(data=ResultFrameBoost,
               aes(x=Threshold,y=TPR,group=1,color=TPR))+
    geom_line()+geom_point()+ 
    xlab("Threshold")+ylab("True Positive Rate")+
    ggtitle("TPR vs Prediction Confidence (GBM)")
  
  print(p3)
  print(p4)
}

