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
rawdata <- read_csv('Data/diabetes_prediction_dataset.csv') |> 
  mutate(diabetes=factor(diabetes,
         levels=c(0,1), labels=c("no","yes")),
         hypertension=factor(hypertension,
                         levels=c(0,1), labels=c("no","yes")),
         heart_disease=factor(heart_disease,
                         levels=c(0,1), labels=c("no","yes")),
         ID=paste("subject",row_number())) |> 
  na.omit()

predvars <- FindVars(c('age',"Hb","bmi","gluc"))

# rawplot <- 
#   ggplot(rawdata, 
#          aes(!!sym(predvars$names[7]), 
#              !!sym(predvars$names[8]), 
#              color=diabetes))+
#   geom_point(alpha=.5)  
# rawplot+
#   geom_mark_hull(expand = unit(2.5,'mm'))
# Preparation ####
# Scaling
rawdata |> 
  summarize(across(where(is.numeric),ksnormal),
            .by=diabetes)
ggplot(rawdata,aes(bmi,fill=diabetes))+
  geom_density(alpha=.5)
ggplot(rawdata,aes(HbA1c_level,fill=diabetes))+
  geom_density(alpha=.5)

scaled <- rawdata |> 
  select(predvars$names) |> 
  caret::preProcess(method = c("range"))
rawdata_s <- predict(scaled,rawdata) |> 
  na.omit()

# predvars <- FindVars('\\d', allnames = cn(rawdata_s))
set.seed(2023)
traindata <- 
  rawdata_s |> 
  select(diabetes,ID,predvars$names) |> 
  group_by(diabetes) |>
  slice_sample(prop = 2/3) |> 
  ungroup()
testdata <- filter(rawdata_s,
                   !ID %in% traindata$ID) |> 
  select(ID,diabetes,predvars$names) 

train_out <- knn3Train(train = traindata |> 
                         select(predvars$names),
                       test = testdata |> 
                         select(predvars$names),
                       cl = traindata$diabetes,
                       k = 5)
train_res <- 
  attr(x = train_out,which = 'prob') |> 
  as_tibble() |> 
  mutate(predicted=factor(train_out,
                          levels = levels(rawdata$diabetes))) |> 
  cbind(testdata) |> 
  as_tibble()
train_res |> 
  pivot_longer(c(no,yes),
               values_to = 'p',
               names_to = 'diabetes_prediction') |> 
  ggplot(aes(diabetes_prediction,`p`))+
  geom_violin()+
  # geom_beeswarm(cex = .5, alpha=.25)+
  facet_grid(rows = vars(diabetes),
             labeller='label_both')

CrossTable(train_res$predicted,train_res$diabetes,
           prop.chisq = F, prop.t = F,
           format = 'SPSS', fisher = F)

yardstick::accuracy(data = train_res,
                    truth=diabetes,
                    estimate=predicted,
                    event_level='second')
yardstick::sensitivity(data = train_res,
                       truth=diabetes,
                       estimate=predicted,
                       event_level='second')
yardstick::specificity(data = train_res,
                       truth=diabetes,
                       estimate=predicted,
                       event_level='second')
yardstick::ppv(data = train_res,
               truth=diabetes,
               estimate=predicted,
               event_level='second')
yardstick::npv(data = train_res,
               truth=diabetes,
               estimate=predicted,
               event_level='second')

