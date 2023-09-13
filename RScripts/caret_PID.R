pacman::p_load(conflicted,
               tidyverse,
               wrappedtools, 
               palmerpenguins,
               ggfortify, GGally,
               caret,
               mlbench)
conflict_scout()
conflict_prefer("filter", "dplyr")

data("PimaIndiansDiabetes2")
rawdata <- PimaIndiansDiabetes2

predvars <- FindVars(c('.'),exclude = 'diab')
rawdata <- select(rawdata,
                  diabetes, predvars$names) |> 
  na.omit()


# create trainig/test data ####
set.seed(2001)
trainindex <- createDataPartition(
  y = rawdata$diabetes,
  times = 1,
  p = 0.75,
  list = FALSE
)

traindata <- rawdata |> slice(trainindex)
testdata <- rawdata |> slice(-trainindex)

# cat_desc_stats(traindata$diabetes,singleline = T)
# cat_desc_stats(testdata$diabetes,singleline = T)

# define global modelling options #####
ctrl <- trainControl(method = "repeatedcv", 
                     number=10,
                     repeats = 10)
# define method-specific options ####
tune <- expand.grid(k=seq(from=1,to=25,by=2))
# tune a model ######
knnfit <- train(form = diabetes~.,
                data = traindata,
                preProcess = c('center','scale'),
                method='knn',
                metric='Accuracy',
                trControl = ctrl,
                tuneGrid=tune)
ldafit <- train(form = diabetes~.,
                data = traindata,
                preProcess = c('center','scale'),
                method='lda',
                metric='Accuracy',
                trControl = ctrl)
# define method-specific options ####
tune <- expand.grid(mtry=seq(2,predvars$count,1)) # adjusted as appropriate!!
rffit <- train(form = diabetes~.,
                data = traindata,
                preProcess = c('center','scale'),
                method='rf',
                metric='Accuracy',
                trControl = ctrl,
               tuneGrid=tune) # change to unstandardized?
svmfit <- train(form = diabetes~.,
                data = traindata,
                preProcess = c('center','scale'),
                method='svmLinear',
                metric='Accuracy',
                trControl = ctrl)
bayesfit <- train(form = diabetes~.,
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
