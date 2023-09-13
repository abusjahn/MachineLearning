pacman::p_load(conflicted,
               tidyverse,
               wrappedtools, 
               palmerpenguins,
               rpart, rpart.plot,
               randomForest,
               caret)

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
         aes(!!sym(predvars$names[1]), 
             !!sym(predvars$names[2]), color=species))+
  geom_point()  
rawplot+
  geom_hline(yintercept = 206)+
  geom_vline(xintercept = 44)

rpart_formula <- paste('species',
                       paste(predvars$names, collapse='+'),
                       sep='~') |> 
  as.formula()
rpart_out <- rpart(formula = rpart_formula,
                   data = rawdata)
prp(rpart_out,
    type = 4,
    extra = 104,
    fallen.leaves = T)

predvars <- FindVars(c('_mm','_g'))
set.seed(222)
ind <- sample(2, nrow(rawdata), replace = TRUE, prob = c(0.7, 0.3))
train <- rawdata[ind==1,]
test <- rawdata[ind==2,]

forrest_formula <- paste('species',
                       paste(predvars$names, collapse='+'),
                       sep='~') |> 
  as.formula()
rf_out <- randomForest(forrest_formula,data = train,
                       ntree=500,mtry=2)
p1 <- predict(rf_out, train)
confusionMatrix(p1, train$species)

p2 <- predict(rf_out, test)
confusionMatrix(p2, test$species)

importance(rf_out)

importance(rf_out) |> 
as_tibble(rownames='Measure') |> 
  mutate(Measure=fct_reorder(.x = MeanDecreaseGini,
                             .f = Measure,
                             .fun=median) |> 
           fct_rev()
         ) |> 
  ggplot(aes(Measure,MeanDecreaseGini))+
  geom_col()
