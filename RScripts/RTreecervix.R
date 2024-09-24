pacman::p_load(conflicted,
               tidyverse,
               wrappedtools, 
               gmodels,
               rpart, rpart.plot,
               randomForest,
               caret)

# conflict_scout()
conflict_prefer('slice','dplyr')
conflict_prefer("filter", "dplyr")
rawdata <- readRDS('Data/cervical.RDS') 
#c('let-7d','Candidate-1','miR-10a')
predvars <- ColSeeker(namepattern = '\\d') # regex for decimal

rawplot <- 
  ggplot(rawdata, 
         aes(.data[[predvars$names[1]]], 
             .data[[predvars$names[2]]], 
         color=Tissuetype))+
  geom_point(alpha=.5)  
rawplot
rpart_formula <- paste('Tissuetype',
                       paste(predvars$bticked, 
                             collapse='+'),
                       sep='~') #|> 
  # as.formula()
rpart_out <- rpart(
  formula = as.formula(rpart_formula),
                   data = rawdata,
                   control = list(minsplit=2))

prp(rpart_out,
    type = 4,
    extra = 104,
    fallen.leaves = T)
pred2 <- "miR-200a"
rtplot <- 
  ggplot(rawdata, 
         aes(`miR-10b*`+.1, 
             .data[[pred2]]+.1, 
             color=Tissuetype))+
  geom_point(alpha=.5)+
  scale_y_log10(labels=prettyNum)+
  scale_x_log10(labels=prettyNum)+
  geom_hline(yintercept = 1.1)+
  geom_vline(xintercept = 12.1)
rtplot
rpart_out$variable.importance |> 
  sort(decreasing = TRUE) |> head(10)
importance_rt <- rpart_out$variable.importance |> 
  as_tibble(rownames="miRNA")
importance_rt |> 
  arrange(value) |> 
  mutate(miRNA=fct_inorder(miRNA)) |> 
  ggplot(aes(miRNA,value))+
  geom_col()+
  coord_flip()

test_predicted <- 
  bind_cols(rawdata,
            as_tibble(
              predict(rpart_out, 
                      rawdata))) |>   
  mutate(predicted=
           case_when(Tumor>Control  ~ 'Tumor',
                     .default="Control") |> 
           factor())
CrossTable(test_predicted$predicted,
                    test_predicted$Tissuetype,
                    prop.chisq = F, prop.t = F,
                    format = 'SPSS')


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

# using arguments x and y avoids problem with illegal names,
# alternatively use something like make.names(unique=T)
rf_out <- randomForest(x = train[-(1:2)],
                       y = train$Tissuetype,
                       ntree=10^4,mtry=100)
p1 <- predict(rf_out, train)
confusionMatrix(p1, train$Tissuetype)

p2 <- predict(rf_out, test)
confusionMatrix(p2, test$Tissuetype)

importance(rf_out) |> head()

importancedata <- importance(rf_out) |> 
  as_tibble(rownames='Measure') |> 
  arrange(desc(MeanDecreaseGini)) |> 
  mutate(Measure=fct_inorder(Measure)) 


# importancedata <- importance(rf_out) |> 
#   as_tibble(rownames='Measure') |> 
#   arrange(desc(MeanDecreaseGini)) |> 
#   mutate(Measure=fct_inorder(Measure)
#   )


importancedata |> 
  slice_head(n=20) |>
    ggplot(aes(Measure,MeanDecreaseGini))+
  geom_col()+
  coord_flip()

# alternative filter
importancedata|> 
  filter(MeanDecreaseGini>.2) |> 
  ggplot(aes(Measure,MeanDecreaseGini))+
  geom_col()+
  coord_flip()
# Lollipop plot
importancedata|> 
  filter(MeanDecreaseGini>.1) |> 
  ggplot(aes(x=Measure,y=MeanDecreaseGini))+
  geom_point(color="pink", size=3)+
  geom_point(color="yellow",size=1.5)+
  geom_segment(aes(xend=Measure,yend=0))+
  coord_flip()


varImpPlot(rf_out ,sort = TRUE, n.var = 10)
