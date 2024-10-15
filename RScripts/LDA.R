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
rawdata <- penguins |> 
  na.omit()
rawdata <- mutate(rawdata,
                  ID=paste('P', 1:nrow(rawdata))) |> 
  select(ID, everything())
predvars <- ColSeeker(namepattern = c('_mm','_g'))
scaled <- rawdata |> 
  select(predvars$names) |> 
  caret::preProcess(method = c('center',"scale"))
rawdata <- predict(scaled,rawdata)


lda_formula <- paste('species',
                       paste(predvars$names, collapse='+'),
                       sep='~') |> 
  as.formula()

lda_out <- lda(lda_formula, data=rawdata)
lda_out$prior
lda_out$svd^2 / sum(lda_out$svd^2) # explained variance
lda_out$scaling
lda_pred <- predict(lda_out)
lda_plotdata <- 
  lda_pred$x |> 
  as_tibble() |> 
  cbind(rawdata |> select(species))
lda_plotdata |> 
  ggplot(aes(LD1,LD2, color=species))+
  geom_point()

confusionMatrix(lda_pred$class,rawdata$species)


tdata <- readRDS('Data/cervical.RDS')
predvars <- ColSeeker(tdata,namepattern = "-")
#preProcess
scale_rules <- tdata |> 
  select(predvars$names) |> 
  caret::preProcess(method = c("nzv",
                               "YeoJohnson",
                               "corr",
                               "scale","center"))
tdata <- predict(scale_rules,tdata) 
predvars <- ColSeeker(tdata,namepattern = "-")

lda_out2 <- lda(x = tdata[-(1:3)],
                grouping=tdata$Tissuetype)
lda_out2$prior
lda_out2$svd^2 / sum(lda_out2$svd^2) # explained var
lda_out2$scaling |> head()
lda_pred2 <- predict(lda_out2)
lda_plotdata <- 
  lda_pred2$x |> 
  as_tibble() |> 
  cbind(tdata |> select(Tissuetype))
lda_plotdata |> 
  ggplot(aes(Tissuetype,LD1, color=Tissuetype))+
  geom_boxplot()+
  geom_hline(yintercept = 0, color="darkgreen")+
  geom_hline(yintercept = -2)

confusionMatrix(lda_pred2$class,tdata$Tissuetype)


#data(bordeaux)
bordeaux <- readxl::read_excel('F:/Aktenschrank/Kontakte/CQ_Bildung/Projekte/ABI/RClass2023/Data/bordeaux.xlsx') |> 
  mutate(quality=factor(quality,
                        levels=c('bad','medium', 'good')))
#import from excel!
lda_formula <- paste('quality',
                     paste(c('temperature', 'sun',
                             'heat', 'rain'), collapse='+'),
                     sep='~') |> 
  as.formula()

# lda_out <- lda(lda_formula, data=bordeaux)
# lda_out$prior
# lda_out$svd^2 / sum(lda_out$svd^2) # explained var
# lda_out$scaling
# lda_pred <- predict(lda_out)
# lda_plotdata <- 
#   lda_pred$x |> 
#   as_tibble() |> 
#   cbind(bordeaux |> select(quality))
# lda_plotdata |> 
#   ggplot(aes(LD1,LD2, color=quality))+
#   geom_point()
# 
# confusionMatrix(lda_pred$class,bordeaux$quality)
