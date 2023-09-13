pacman::p_load(conflicted,
               wrappedtools, 
               palmerpenguins,
               rpart, rpart.plot,
               randomForest,
               caret)

# conflict_scout()
conflict_prefer('slice','dplyr')
conflict_prefer("filter", "dplyr")
rawdata <- readRDS('Data/cervical.RData') 
#FindVars(c('let','Cand','miR'))
predvars <- FindVars('\\d') # regex for decimal

rawplot <- 
  ggplot(rawdata, 
         aes(!!sym(predvars$names[1]), 
             !!sym(predvars$names[2]), color=Tissuetype))+
  geom_point(alpha=.5)  
rawplot
rpart_formula <- paste('Tissuetype',
                       paste(predvars$bticked[1:600], 
                             collapse='+'),
                       sep='~') |> 
  as.formula()
rpart_out <- rpart(formula = rpart_formula,
                   data = rawdata,
                   control = list(minsplit=2))
prp(rpart_out,
    type = 4,
    extra = 104,
    fallen.leaves = T)

set.seed(2023)
train <- 
  rawdata |> 
  select(SampleID,Tissuetype,predvars$names) |> 
  group_by(Tissuetype) |>
  slice_sample(prop = 2/3) |> 
  ungroup()
test <- filter(rawdata,
                   !SampleID %in% train$SampleID) |> 
  select(SampleID,Tissuetype,predvars$names) 

# forrest_formula <- paste('Tissuetype',
#                          paste(predvars$bticked[1:600], 
#                                collapse='+'),
#                          sep='~') |> 
#   as.formula()
rf_out <- randomForest(x = train[-(1:2)],
                       y=train$Tissuetype,
                       ntree=500,mtry=20)
p1 <- predict(rf_out, train)
confusionMatrix(p1, train$Tissuetype)

p2 <- predict(rf_out, test)
confusionMatrix(p2, test$Tissuetype)

importance(rf_out)

importancedata <- importance(rf_out) |> 
  as_tibble(rownames='Measure') |> 
  mutate(Measure=fct_reorder(.x = MeanDecreaseGini,
                             .f = Measure,
                             .fun=I) |> 
           fct_rev()
  )

# importancedata <- importance(rf_out) |> 
#   as_tibble(rownames='Measure') |> 
#   arrange(desc(MeanDecreaseGini)) |> 
#   mutate(Measure=fct_inorder(Measure)
#   )


importancedata |> 
  ggplot(aes(Measure,MeanDecreaseGini))+
  geom_col()


importancedata|> 
  filter(Measure %in% levels(Measure)[1:20]) |> 
  ggplot(aes(Measure,MeanDecreaseGini))+
  geom_col()+
  coord_flip()

varImpPlot(rf_out ,sort = TRUE, n.var = 10)
