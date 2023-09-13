pacman::p_load(conflicted,
               tidyverse,
               wrappedtools,  # just tools 
               ggforce, # for cluster plots, hulls, zoom etc
               ggbeeswarm,
               caret, # Classification and Regression Training
               preprocessCore,  # pre-processing functions
               gmodels, # tools for model fitting
               easystats,
               yardstick)

# conflict_scout()
conflict_prefer('slice','dplyr')
conflict_prefer("filter", "dplyr")
rawdata <- readRDS('Data/cervical.RData') 
#FindVars(c('let','Cand','miR'))
predvars <- FindVars('\\d') # regex for decimal

rawplot <- 
  ggplot(rawdata, 
         aes(!!sym(predvars$names[1]), 
             !!sym(predvars$names[2]), 
             color=Tissuetype))+
  geom_point(alpha=.5)  
rawplot
# Preparation ####
# Scaling
scaled <- rawdata |> 
  select(predvars$names) |> 
  caret::preProcess(method = c("zv","nzv","corr","range"))
rawdata_s <- predict(scaled,rawdata) 

predvars <- FindVars('\\d', allnames = cn(rawdata_s))
set.seed(2023)
traindata <- 
  rawdata_s |> 
  select(SampleID,Tissuetype,predvars$names) |> 
  group_by(Tissuetype) |>
  slice_sample(prop = 2/3) |> 
  ungroup()
testdata <- filter(rawdata_s,
                   !SampleID %in% traindata$SampleID) |> 
  select(SampleID,Tissuetype,predvars$names) 

train_out <- knn3Train(train = traindata |> 
                         select(predvars$names),
          test = testdata |> select(predvars$names),
          cl = traindata$Tissuetype,
          k = 3)
  train_res <- 
  attr(x = train_out,which = 'prob') |> 
  as_tibble() |> 
  mutate(predicted=factor(train_out,
                          levels = levels(rawdata$Tissuetype))) |> 
  cbind(testdata) |> 
  as_tibble()
train_res |> 
  pivot_longer(c(Control,Tumor),
               values_to = 'p',
               names_to = 'Tumorprediction') |> 
  ggplot(aes(Tumorprediction,`p`))+
  geom_violin()+
  geom_beeswarm(cex = .5, alpha=.25)+
  facet_grid(rows = vars(Tissuetype),
             labeller='label_both')

CrossTable(train_res$predicted,train_res$Tissuetype,
                     prop.chisq = F, prop.t = F,
           format = 'SPSS', fisher = T)

yardstick::accuracy(data = train_res,
                    truth=Tissuetype,
                    estimate=predicted,
                    event_level='second')
yardstick::sensitivity(data = train_res,
                    truth=Tissuetype,
                    estimate=predicted,
                    event_level='second')
yardstick::specificity(data = train_res,
                    truth=Tissuetype,
                    estimate=predicted,
                    event_level='second')
yardstick::ppv(data = train_res,
                       truth=Tissuetype,
                       estimate=predicted,
                       event_level='second')

