pacman::p_load(conflicted,
               tidyverse,
               wrappedtools, 
               palmerpenguins,
               ggfortify, GGally,
               MASS,
               caret
)

# conflict_scout()
conflict_prefer('select','dplyr')
conflict_prefer("filter", "dplyr")
rawdata <- 
  read_csv('Data/diabetes_prediction_dataset.csv') |> 
  mutate(diabetes=factor(diabetes,
                         levels=c(0,1), labels=c("no","yes")),
         hypertension=factor(hypertension,
                             levels=c(0,1), labels=c("no","yes")),
         heart_disease=factor(heart_disease,
                              levels=c(0,1), labels=c("no","yes")),
         ID=paste("subject",row_number())) |> 
  na.omit()
# predvars <- FindVars(c('_l','age','bmi'))
predvars <- FindVars(rawdata |> select(where(is.numeric)) |> cn())


lda_formula <- paste('diabetes',
                     paste(predvars$names, collapse='+'),
                     sep='~') |> 
  as.formula()

lda_out <- lda(lda_formula, data=rawdata)
lda_out$prior
lda_out$svd^2 / sum(lda_out$svd^2) # explained var
lda_out$scaling
lda_pred <- predict(lda_out)
lda_plotdata <- 
  lda_pred$x |> 
  as_tibble() |> 
  cbind(rawdata |> select(diabetes))
lda_plotdata |> 
  ggplot(aes(diabetes,LD1, color=diabetes))+
  geom_boxplot()+
  geom_hline(yintercept = 1)

confusionMatrix(lda_pred$class,rawdata$diabetes)

