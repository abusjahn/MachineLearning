pacman::p_load(conflicted,
               tidyverse,
               wrappedtools, 
               palmerpenguins,
               ggfortify, GGally, nnet,
               caret,randomForest,kernlab,naivebayes,
               mlbench)
conflict_scout()
#conflict_prefer(name = "filter", winner = "dplyr")
conflicts_prefer(
  dplyr::filter(),
  ggplot2::alpha,
  dplyr::combine,
  dplyr::slice)
rawdata <- penguins |> 
  na.omit()
predvars <- FindVars(c('_mm','_g'))
rawdata <- select(rawdata,
                  species, predvars$names)
# create training/test data ####
set.seed(2001)
trainindex <- createDataPartition(
  y = rawdata$species,
  times = 1,
  p = 0.75,
  list = FALSE
)

traindata <- rawdata |> slice(trainindex[,1])
testdata <- rawdata |> slice(-trainindex[,1])

# cat_desc_stats(traindata$species,singleline = T)
# cat_desc_stats(testdata$species,singleline = T)

# define global modelling options #####
ctrl <- trainControl(method = "repeatedcv", 
                     number=5,  # 5-fold cross-validation
                     repeats = 25) # 25 repeats
# define method-specific options ####
tune <- expand.grid(k=seq(1,9,2))
# tune a model ######
knnfit <- train(form = species~.,
                data = traindata,
                preProcess = c('center','scale'),
                method='knn',
                metric='Accuracy',
                trControl = ctrl,
                tuneGrid=tune)

# define method-specific options ####
tune <- expand.grid(nrounds=c(50,100, 200),
                    max_depth=seq(5,15,5),
                    eta=1,
                    gamma=c(.01,.001),
                    colsample_bytree=1,
                    min_child_weight=1,
                    subsample=1)
xgbfit <- train(form = species~.,
                data = traindata,
                preProcess = c('center','scale'),
                method='xgbTree',
                objective = "multi:softprob",
                metric='Accuracy',
                trControl = ctrl,
                tuneGrid=tune,
                verbosity = 0)

ldafit <- train(form = species~.,
                data = traindata,
                preProcess = c('center','scale'),
                method='lda',
                metric='Accuracy',
                trControl = ctrl)

# define method-specific options ####
tune <- expand.grid(mtry=seq(2,3,1))
rffit <- train(form = species~.,
                data = traindata,
                preProcess = c('center','scale'),
                method='rf',
                metric='Accuracy',
                trControl = ctrl,
               tuneGrid=tune)

svmfit <- train(form = species~.,
                data = traindata,
                preProcess = c('center','scale'),
                method='svmLinear',
                metric='Accuracy',
                trControl = ctrl)

bayesfit <- train(form = species~.,
                data = traindata,
                preProcess = c('center','scale'),
                method='naive_bayes',
                metric='Accuracy',
                trControl = ctrl)

nnfit <- train(form=species~.,
               data=traindata, 
               method="nnet",
               preProcess=c("center","scale"),
               metric="Accuracy",
               trControl=ctrl)

resamps <- resamples(list(knn = knnfit, 
                          lda = ldafit,
                          rf=rffit,
                          xgb=xgbfit,
                          svm=svmfit,
                          bayes=bayesfit,
                          nn=nnfit))
summary(resamps)
resamps$values |> head()
resamps$values |> 
  pivot_longer(contains('~'),
               names_to = c('Model','Measure'),
               names_sep = '~') |> 
  ggplot(aes(Model,value))+
  geom_boxplot(outlier.alpha = .6)+
  facet_wrap(facets = vars(Measure),scales = 'free')
diffs <- diff(resamps)
summary(diffs)
xgbfit
xgbfit[["bestTune"]]
knnfit[["bestTune"]]

# use xgbfit to predict test data ####
xgbpred <- predict(xgbfit, newdata = testdata)
confusionMatrix(xgbpred, testdata$species)
# Importance
importance_xgb <- 
  xgboost::xgb.importance(model=xgbfit[["finalModel"]]) |> 
  arrange(Gain) |> 
  mutate(Feature=fct_inorder(Feature))
importance_xgb
importance_xgb |> 
  ggplot(aes(Feature,Gain))+
  geom_col(aes(fill=Gain))+
  coord_flip()+
  guides(fill="none")
