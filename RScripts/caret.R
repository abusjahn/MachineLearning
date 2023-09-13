pacman::p_load(conflicted,
               tidyverse,
               wrappedtools, 
               palmerpenguins,
               ggfortify, GGally,
               caret,randomForest,kernlab,naivebayes,
               mlbench)
conflict_scout()
conflict_prefer("filter", "dplyr")

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

traindata <- rawdata |> slice(trainindex)
testdata <- rawdata |> slice(-trainindex[,1])

# cat_desc_stats(traindata$species,singleline = T)
# cat_desc_stats(testdata$species,singleline = T)

# define global modelling options #####
ctrl <- trainControl(method = "repeatedcv", 
                     number=10,
                     repeats = 25)
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

resamps <- resamples(list(knn = knnfit, 
                          lda = ldafit,
                          rf=rffit,
                          svm=svmfit,
                          bayes=bayesfit))
summary(resamps)
resamps$values |> head()
resamps$values |> 
  pivot_longer(contains('~'),
               names_to = c('Model','Measure'),
               names_sep = '~') |> 
  ggplot(aes(Model,value))+
  geom_boxplot()+
  facet_wrap(facets = vars(Measure),scales = 'free')
diffs <- diff(resamps)
summary(diffs)
