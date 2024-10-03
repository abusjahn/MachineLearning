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
rawdata <- readRDS('Data/cervical.RDS') 
# rawdata_cleannames <-
#   rawdata |>
#   rename_with(~str_replace_all(.x,
#                                c("-"="_",
#                                  "\\*"="_star")))
# cn(rawdata_cleannames)[1:20]
#FindVars(c('let','Cand','miR'))
predvars <- ColSeeker(namepattern = '\\d') # regex for decimal
# exploratory 1 ####
ks_out <- rawdata |> 
  select(all_of(predvars$names)) |> 
  summarise(across(everything(),
                   ~ksnormal(log(.x+1)))) |> 
  pivot_longer(everything(), values_to="pKS") |> 
  arrange(pKS)
ks_out |> 
  ggplot(aes(pKS))+
  scale_x_continuous(breaks = seq(0,1,.1),
                     minor_breaks=seq(0,1,.01))+
  geom_histogram(binwidth = .01)#bins=20)

ks_out |> 
  group_by(pKS<.05) |> 
  count()
table(ks_out$pKS<.05)

explore <- rawdata |> 
  select(Tissuetype,all_of(predvars$names)) |> 
  group_by(Tissuetype) |> 
  summarise(across(everything(),
                   list(
                     Mean=~mean(.x, na.rm=T),
                     Median=~median(.x, na.rm=T),
                     SD=~sd(.x, na.rm=T)
                   ))) |> 
  pivot_longer(-Tissuetype,
               names_to = c("miRNA",".value"),
               names_sep = "_") 
explore |> 
  ggplot(aes(Median+0.001, fill=Tissuetype))+
  geom_histogram(position='dodge')+
  scale_x_log10()

explore |> 
  ggplot(aes(Median+0.001, fill=Tissuetype))+
  geom_histogram(position='fill')+
  scale_x_log10()+
  scale_y_continuous("Frequency",labels=scales::percent)

explore |> 
  ggplot(aes(Median+0.1, color=Tissuetype))+
  geom_line(stat='bin')+
  scale_x_log10(labels=prettyNum)

explore |> 
  ggplot(aes(y=Median+0.1, x=Tissuetype))+
  geom_boxplot()+
  scale_y_log10(
    breaks=logrange_15,
    minor_breaks=logrange_123456789,
    labels=prettyNum)

ggplot(rawdata, 
       aes(.data[[predvars$names[1]]], 
           .data[[predvars$names[3]]], 
           color=Tissuetype))+
  geom_point(alpha=.5)  

# Preparation ####
# Scaling example with name problem ####
# tempdata <- rawdata_cleannames |>
#   select(matches("\\d")) |>
#   # select(1) |> 
#   as.matrix() |>
#   preprocessCore::normalize.quantiles(keep.names = T) |>
#   as_tibble()
# colnames(tempdata) <- paste0(predvars$name,'_qnorm')
# rawdata0  <- bind_cols(tempdata,rawdata)


# Scaling for real ####
scale_rules <- rawdata |> 
  select(predvars$names) |> 
  caret::preProcess(method = c("nzv",
                               "YeoJohnson",
                               "corr",
                               "range"))
rawdata_s <- predict(scale_rules,rawdata) 
predvars <- ColSeeker(rawdata_s,'\\d')

#Exploratory 2 ####
ks_out <- rawdata_s |> 
  select(all_of(predvars$names)) |> 
  summarise(across(everything(),
                   ~ksnormal(.x))) |> 
  pivot_longer(everything(), values_to="pKS") |> 
  arrange(pKS)
ks_out |> 
  ggplot(aes(pKS))+
  scale_x_continuous(breaks = seq(0,1,.1),
                     minor_breaks=seq(0,1,.01))+
  geom_histogram(binwidth = .01)#bins=20)

ks_out |> 
  group_by(pKS<.05) |> 
  count()

explore_s <- rawdata_s |> 
  select(all_of(predvars$names)) |> 
  summarise(across(everything(),
                   list(
                     Mean=~mean(.x, na.rm=T),
                     Median=~median(.x, na.rm=T),
                     SD=~sd(.x, na.rm=T)
                   ))) |> 
  pivot_longer(everything(),
               names_to = c("miRNA",".value"),
               names_sep = "_") 
ggplot(rawdata_s, 
       aes(!!sym(predvars$names[1]), 
           !!sym(predvars$names[3]), 
           color=Tissuetype))+
  geom_point(alpha=.5)  

# splitting ####
set.seed(2024)
traindata <- 
  rawdata_s |> 
  group_by(Tissuetype) |>
  slice_sample(prop = 2/3) |> 
  ungroup()
testdata <- filter(rawdata_s,
                   !SampleID %in% traindata$SampleID) 

# modelling ####
train_out <- knn3Train(train = traindata |> 
                         select(predvars$names),
                       test = testdata |> select(predvars$names),
                       cl = traindata$Tissuetype,
                       k = 3)
#model testing ####
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

caret::confusionMatrix(train_res$predicted, 
                       reference = train_res$Tissuetype,)

CrossTable(train_res$predicted,train_res$Tissuetype,
           prop.chisq = F, prop.t = F,
           format = 'SPSS', fisher = F)

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
yardstick::npv(data = train_res,
               truth=Tissuetype,
               estimate=predicted,
               event_level='second')
# 
# #623 IVs allowed
testvar <- paste0("DV~",paste0("IDV",1:623, collapse="+")) |> as.formula()
# #not working, too complex for saving in variable
# knn_formula <- paste0(
#   'Tissuetype~',
#   paste(
#     str_replace_all(
#       cn()[-(1:3)],#[1:623],
#       c("-"="_","\\*"="_3p")),
#     collapse = '+')) |> 
#   as.formula()
# #just create and save text, make it a formula later
# knn_formulatext <- paste0(
#   "Tissuetype~",
#   paste(
#     str_replace_all(
#       cn()[-(1:3)],
#       c("-"="_","\\*"="star")),
#     collapse="+"))
# as.formula(knn_formulatext)
# 
# rawdata_renamed <- rawdata_s |> 
#   rename_with(~str_replace_all(
#     .x,
#     c("-"="_","\\*"="star")))
# 
# knn3(paste0(
#   "Tissuetype~",
#   paste(
#     cn(rawdata_renamed)[-(1:3)],
#       collapse="+")
#   ) |> as.formula(),
#   data = rawdata_renamed)
