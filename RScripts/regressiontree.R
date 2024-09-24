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
         aes(.data[[predvars$names[1]]], 
             .data[[predvars$names[2]]], color=species))+
  geom_point()  
rawplot+
  geom_hline(yintercept = 206)+
  geom_vline(xintercept = 44)

rpart_formula <- paste('species',
                       paste(predvars$names,
                             collapse='+'),
                       sep='~') |> 
  as.formula()
rpart_out <- rpart(formula = rpart_formula,
                   data = rawdata)
prp(rpart_out,
    type = 4,
    extra = 104,
    fallen.leaves = T)
importance <- tibble(
  Variable=names(rpart_out$variable.importance),
  Score=rpart_out$variable.importance)
ggplot(importance, aes(x=Variable,y=Score))+
  geom_col()+
  coord_flip()


predvars <- FindVars(c('_mm','_g'))
rpart_formula_4 <- paste('species',
                         paste(predvars$names,
                               collapse='+'),
                         sep='~') |> 
  as.formula()
set.seed(222)
# unstratified spliting
ind <- sample(2, nrow(rawdata), 
              replace = TRUE, prob = c(0.7, 0.3))
train <- rawdata[ind==1,]
test <- rawdata[ind==2,]

# tree for training sample
rpart_out_tr <- rpart(formula = rpart_formula_4,
                      data = train)
rpart_out$variable.importance
rpart_out_tr$variable.importance
prp(rpart_out_tr,
    type = 4,
    extra = 104,
    fallen.leaves = T)

test_predicted <- 
  bind_cols(test,
            as_tibble(
              predict(rpart_out_tr, test))) |>   
  mutate(predicted=
           case_when(Adelie>Chinstrap &
                       Adelie>Gentoo ~ 'Adelie',
                     Chinstrap>Adelie &
                       Chinstrap>Gentoo ~ 'Chinstrap',
                     Gentoo>Adelie &
                       Gentoo>Chinstrap ~ 'Gentoo') |> 
           factor())

gmodels::CrossTable(test_predicted$predicted,
           test_predicted$species,
           prop.chisq = F, prop.t = F,
           format = 'SPSS')

forrest_formula <- 
  paste('species',
        paste(predvars$names, collapse='+'),
        sep='~') |> 
  as.formula()
rf_out <- randomForest(forrest_formula,
                       data = train,
                       ntree=500,mtry=2)
p1 <- predict(rf_out, train)
confusionMatrix(p1, train$species)

p2 <- predict(rf_out, test)
confusionMatrix(p2, test$species)

importance(rf_out)

# importance(rf_out) |> 
# as_tibble(rownames='Measure') |> 
#   mutate(Measure=fct_reorder(.x = MeanDecreaseGini,
#                              .f = Measure,
#                              .fun=median) |> 
#            fct_rev()
#          ) |> 
#   ggplot(aes(Measure,MeanDecreaseGini))+
#   geom_col()

importance(rf_out) |> 
  as_tibble(rownames='Measure') |> 
  arrange(#desc(
    MeanDecreaseGini) |> #) |> 
  mutate(Measure=fct_inorder(Measure)) |> 
  ggplot(aes(x=Measure,y=MeanDecreaseGini))+
  geom_col()+
  coord_flip()
