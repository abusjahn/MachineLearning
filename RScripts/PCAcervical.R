pacman::p_load(conflicted,
               tidyverse,
               wrappedtools, 
               palmerpenguins,
               ggfortify, GGally,
               rpart,rpart.plot)

# conflict_scout()
conflict_prefer('slice','dplyr')
conflict_prefer("filter", "dplyr")
conflict_prefer('screeplot','stats')
conflict_prefer('biplot','stats')
rawdata <- readRDS('Data/cervical.RData')
predvars <- FindVars(c('-'))
rawdata <- mutate(
  rawdata,
  across(.cols = all_of(predvars$names),
         .fns = ~case_when(.x==0 ~ .x+0.01,
                           .default = .x) |> 
           log()))
# rawdata <- mutate(rawdata,
#                   across(.cols = all_of(predvars$names),
#                          .fns = ~log(.x+0.01)))

pca_out <- prcomp(rawdata |> select(predvars$names),
                  center = T,scale. = T)
summary(pca_out)

# predict(pca_out)

screeplot(pca_out,npcs = 5)

pca_out$rotation[1:10,1:5]

# biplot(pca_out)

autoplot(pca_out)
autoplot(pca_out, data=rawdata, colour='Tissuetype')
autoplot(pca_out, data=rawdata, colour='Tissuetype',
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 4)
#contribution of variables to component
autoplot(pca_out, data=rawdata, colour='Tissuetype',
         x=1,y=3)
autoplot(pca_out, data=rawdata, colour='Tissuetype',
         x=2,y=3)
rawdata <- pca_out$x[,1:14] |> 
  as_tibble() |> 
  cbind(rawdata)
PCAvars <- FindVars('PC')
rpart_formula <- paste('Tissuetype',
                       paste(PCAvars$names, collapse='+'),
                       sep='~') |> 
  as.formula()
rpart_out <- rpart(formula = rpart_formula,
                   data = rawdata)
prp(rpart_out,
    type = 4,
    extra = 104,
    fallen.leaves = T)

loadings <- pca_out$rotation |> 
  as_tibble(rownames = 'RNA') |>
  select(1:14)
  
loadings |> 
  ggplot(aes(abs(PC2)))+
  geom_density()


  loadings |> 
    select(RNA,PC2) |> 
    filter(abs(PC2)>.05)
  
candidates <- 
  loadings |> 
    select(RNA,PC2) |> 
    filter(abs(PC2)>quantile(abs(PC2),probs = .95)) |> 
    arrange(desc(abs(PC2)))
view(candidates)  
rawdata |> 
  select(Tissuetype,all_of(candidates$RNA)) |> 
  pivot_longer(-Tissuetype) |>
  # pivot_longer(cols = all_of(candidates$RNA)) |> 
  ggplot(aes(Tissuetype,value))+
  geom_boxplot()+
  facet_wrap(facets = vars(name),
             scales='free_y')
