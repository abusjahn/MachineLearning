pacman::p_load(conflicted,
               tidyverse,
               wrappedtools,  # just tools 
               palmerpenguins, # data
               ggforce, # for cluster plots, hulls, zoom etc
               ggbeeswarm,
               caret, # Classification and Regression Training
               preprocessCore,  # pre-processing functions
               gmodels, # tools for model fitting
               easystats, 
               yardstick)  # model performance

# conflict_scout()
conflict_prefer('slice','dplyr')
conflict_prefer("filter", "dplyr")
rawdata <- penguins |> 
  na.omit()
rawdata <- mutate(rawdata,
                  ID=paste('P', 1:nrow(rawdata))) |> 
  select(ID, everything())
predvars <- ColSeeker(rawdata,'length')

rawplot <- 
  ggplot(rawdata, 
         aes(.data[[predvars$names[1]]], 
             .data[[predvars$names[2]]], 
             color=species))+
  geom_point()  
rawplot
rawplot+
  geom_mark_hull(expand = unit(2.5,'mm'))
rawplot+
  geom_mark_ellipse(expand = unit(2.5,'mm'))

rawplot+
  geom_hline(yintercept = 206)+
  geom_vline(xintercept = 44)
rawplot+
  facet_zoom(xlim = c(40,50),
             ylim = c(190,210))
# Preparation ####
# Scaling
scaled <- rawdata |> 
  select(predvars$names) |> 
  caret::preProcess(method = c('center',"scale"))
rawdata <- predict(scaled,rawdata) |> 
  select(ID,all_of(predvars$names)) |>
  rename_with(.cols=predvars$names,
                     ~paste0(.x,"_std")) |> 
  full_join(select(rawdata,-contains("_std")),
            by='ID') 
  # bind_cols(rawdata)
#  as_tibble() |> 
  # rename(flipper_length_mm_std=flipper_length_mm,
  #        bill_length_mm_std=bill_length_mm) |> 
  # select(contains('rnorm')) |> 
  #cbind(rawdata) |> 
  # as_tibble()

rawdata <- rawdata |> 
  select(predvars$names) |>
  as.matrix() |> 
  preprocessCore::normalize.quantiles(keep.names = TRUE) |> 
  as_tibble() |>
  rename_with(~paste0(.,'_qnorm')) |>
  bind_cols(rawdata) 

rawdata |> 
  pivot_longer(contains('length')) |> 
  ggplot(aes(value,fill=name))+
  geom_density()+
  facet_wrap(facets = vars(name),
             scales='free')+
  guides(fill="none")

rawdata |> 
  ggplot(aes(bill_length_mm,bill_length_mm_std))+
  geom_point()
rawdata |> 
  ggplot(aes(bill_length_mm,bill_length_mm_qnorm))+
  geom_point()

predvars_std <- FindVars('_std')
ggplot(rawdata, 
       aes(.data[[predvars_std$names[1]]], 
           .data[[predvars_std$names[2]]], 
           color=species))+
  geom_point()  

# Set definition
set.seed(2023)
traindata <- 
  rawdata |> 
  select(ID,species,sex,predvars_std$names) |> 
  group_by(species,sex) |> 
  slice_sample(prop = 2/3) |> 
  ungroup() |> 
  select(-sex)
testdata <- filter(rawdata,
                   !ID %in% traindata$ID) |> 
  select(ID,species,predvars_std$names) 

train_out <- knn3Train(
  train = select(traindata,predvars_std$names),
  test = select(testdata, predvars_std$names),
  cl = traindata$species,
  k = 5) # number of neighbors to ask
str(train_out)
head(train_out)
train_res <- 
  attr(x = train_out,which = 'prob') |> 
  as_tibble() |> 
  mutate(predicted=factor(train_out)) |> 
  bind_cols(testdata) 

train_res |> 
  pivot_longer(c(Adelie,Chinstrap,Gentoo),
               values_to = 'p species',
               names_to = 'Species prediction') |> 
  ggplot(aes(`Species prediction`,`p species`))+
  geom_violin()+
  geom_beeswarm(cex = .5, alpha=.25)+
  facet_grid(rows = vars(species),
             labeller='label_both')

CrossTable(train_res$predicted,
           train_res$species,
           prop.chisq = F, prop.t = F,
           format = 'SAS')
caret::confusionMatrix(train_res$predicted, 
                       reference = train_res$species)
yardstick::accuracy(data = train_res,
                    truth=species,
                    estimate=predicted)

knn_formula <- paste0('species~',
                      paste(predvars_std$names, 
                            collapse = '+')) |> 
  as.formula() # how about adding sex in a 2nd run?
knn_out <- knn3(knn_formula, 
                data=rawdata,k = 5)
pred_all <- 
  predict(knn_out,newdata = rawdata) |> 
  as_tibble() |> 
  bind_cols(rawdata) |> 
  mutate(predicted=
           case_when(Adelie>Chinstrap &
                       Adelie>Gentoo ~ 'Adelie',
                     Chinstrap>Adelie &
                       Chinstrap>Gentoo ~ 'Chinstrap',
                     Gentoo>Adelie &
                       Gentoo>Chinstrap ~ 'Gentoo') |> 
           factor()) 

# rowwise() |> 
#   mutate(predicted=case_when(
#     Adelie==max(Adelie,Chinstrap,Gentoo) ~ 'Adelie',
#     Chinstrap==max(Adelie,Chinstrap,Gentoo) ~ 'Chinstrap',
#     Gentoo==max(Adelie,Chinstrap,Gentoo) ~ 'Gentoo') |> 
#       factor()) |> 
#   ungroup()


yardstick::accuracy(data = pred_all,
                    truth=species,
                    estimate=predicted)

pred_all |> 
  pivot_longer(c(Adelie,Chinstrap,Gentoo),
               values_to = 'p species',
               names_to = 'Species_pred') |> 
  ggplot(aes(Species_pred,`p species`))+
  geom_violin()+
  geom_beeswarm(cex = .25, alpha=.25)+
  facet_grid(rows = vars(species))

ggplot(rawdata,
       aes(bill_length_mm_std,flipper_length_mm_std,
           color=sex,shape=species))+
  geom_point()

knn_out <- knn3(sex ~ bill_length_mm_std + flipper_length_mm_std + species, 
                data=rawdata,k = 5)

rawdata <- predict(knn_out,newdata = rawdata) |> 
  as_tibble() |> 
  cbind(rawdata)

rawdata |> 
  pivot_longer(c(female,male),
               values_to = 'p sex',
               names_to = 'Sex') |> 
  ggplot(aes(Sex,`p sex`))+
  geom_violin()+
  geom_beeswarm(cex = .25, alpha=.25)+
  facet_grid(rows = vars(sex))


# cbind(knn_out$learn$y |> as_tibble(), 
#       knn_out$learn$X) |> 
#   as_tibble() |> 
#   ggplot(aes(x=value,y=x))+
#   geom_point()
rawdata <-  mutate(rawdata,
                   pred_sex=case_when(
                     male>=.5 ~ "male",
                     .default = "female"
                   ) |> 
                     as.factor())
yardstick::accuracy(data = rawdata,
                    truth=sex,
                    estimate=pred_sex)
yardstick::sensitivity(data = rawdata,
                    truth=sex,
                    estimate=pred_sex,
                    event_level="second")
yardstick::specificity(data = rawdata,
                       truth=sex,
                       estimate=pred_sex,
                       event_level="second")
yardstick::ppv(data = rawdata,
                       truth=sex,
                       estimate=pred_sex,
                       event_level="second")
CrossTable(rawdata$pred_sex,
           rawdata$sex,
           prop.chisq = F, prop.t = F,
           format = 'SPSS')

