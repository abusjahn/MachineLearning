pacman::p_load(conflicted,
               tidyverse,
               wrappedtools,  # just tools 
               palmerpenguins, # data
               ggforce, # for cluster plots, hulls, zoom etc
               ggbeeswarm,
               caret, # Classification and Regression Training
               preprocessCore,  # pre-processing functions
               gmodels, # tools for model fitting
               easystats)
rawdata <- read_csv("data/diabetes_prediction_dataset.csv") |> 
  mutate(diabetes=factor(diabetes,
                         levels=c(0,1),
                         labels=c('no','yes')))
ggplot(rawdata,
       aes(blood_glucose_level,HbA1c_level,
           color=diabetes))+
  geom_point(alpha=.1, size=.2,
             position=position_jitter(width = .75,
                                      height = .05))+
  geom_mark_hull(expand = unit(2.5,'mm'))
