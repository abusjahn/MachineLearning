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
predvars <- FindVars('length')

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
  as_tibble() |> 
  rename(flipper_length_mm_rnorm=flipper_length_mm,
         bill_length_mm_rnorm=bill_length_mm) |> 
  select(contains('rnorm')) |> 
  cbind(rawdata) |> 
  as_tibble()

rawdata <- rawdata |> 
  select(predvars$names) |>
  as.matrix() |> 
  preprocessCore::normalize.quantiles(keep.names = TRUE) |> 
  as_tibble() |>
  rename_with(~paste0(.,'_qnorm')) |>
  cbind(rawdata) |> 
  as_tibble()

rawdata |> 
  pivot_longer(contains('length')) |> 
  ggplot(aes(value,fill=name))+
  geom_density()+
  facet_wrap(facets = vars(name),
             scales='free')

rawdata |> 
  ggplot(aes(bill_length_mm,bill_length_mm_rnorm))+
  geom_point()
rawdata |> 
  ggplot(aes(bill_length_mm,bill_length_mm_qnorm))+
  geom_point()

predvars_rnorm <- FindVars('rnorm')
ggplot(rawdata, 
       aes(!!sym(predvars_rnorm$names[1]), 
           !!sym(predvars_rnorm$names[2]), color=species))+
  geom_point()  

# Set definition
set.seed(2023)
traindata <- 
  rawdata |> 
  select(ID,species,sex,predvars_rnorm$names) |> 
  group_by(species,sex) |> 
  slice_sample(prop = 2/3) |> 
  ungroup() |> 
  select(-sex)
testdata <- filter(rawdata,
                   !ID %in% traindata$ID) |> 
  select(ID,species,predvars_rnorm$names) 

train_out <- knn3Train(train = traindata |> select(predvars_rnorm$names),
          test = testdata |> select(predvars_rnorm$names),
          cl = traindata$species,
          k = 5) # number of neighbors to ask
str(train_out)
train_res <- 
  attr(x = train_out,which = 'prob') |> 
  as_tibble() |> 
  mutate(predicted=factor(train_out)) |> 
  cbind(testdata) |> 
  as_tibble()
train_res |> 
  pivot_longer(c(Adelie,Chinstrap,Gentoo),
               values_to = 'p species',
               names_to = 'Species prediction') |> 
  ggplot(aes(`Species prediction`,`p species`))+
  geom_violin()+
  geom_beeswarm(cex = .5, alpha=.25)+
  facet_grid(rows = vars(species),
             labeller='label_both')

CrossTable(train_res$predicted,train_res$species,
                     prop.chisq = F, prop.t = F,
           format = 'SAS')

yardstick::accuracy(data = train_res,
                    truth=species,
                    estimate=predicted)

knn_formula <- paste0('species~',
                      paste(predvars_rnorm$names, 
                            collapse = '+')) |> 
  as.formula()
knn_out <- knn3(knn_formula, data=rawdata,k = 5)
predict(knn_out,newdata = rawdata) |> 
  as_tibble() |> 
  cbind(rawdata)|> 
  pivot_longer(c(Adelie,Chinstrap,Gentoo),
               values_to = 'p species',
               names_to = 'Species') |> 
  ggplot(aes(Species,`p species`))+
  geom_violin()+
  geom_beeswarm(cex = .25, alpha=.25)+
  facet_grid(rows = vars(species))

ggplot(rawdata,
       aes(bill_length_mm_rnorm,flipper_length_mm_rnorm,
           color=sex,shape=species))+
  geom_point()

knn_out <- knn3(sex ~ bill_length_mm_rnorm + flipper_length_mm_rnorm + species, 
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

