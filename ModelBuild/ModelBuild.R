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


#####           MAIN           #####

# Load final filtered dataset
set.seed(1)
matchups <<- read.csv("./Data/FilteredMatchups2.csv")
matchups$X = NULL

# Split set into train and test
needSplit = TRUE
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

### Begin Model Creation

#Create the 7 folds
folds = createFolds(1:nrow(train.set),k=7)

evalModels = FALSE
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
