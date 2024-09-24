pacman::p_load(conflicted, tidyverse,
               wrappedtools, 
               palmerpenguins,
               ggfortify, GGally,
               factoextra,
               caret,clue
)

# conflict_scout()
conflict_prefer('select','dplyr')
conflict_prefer("filter", "dplyr")
rawdata <- penguins |> 
  na.omit()
rawdata <- mutate(rawdata,
                  ID=paste('P', 1:nrow(rawdata))) |> 
  select(ID, everything())
predvars <- FindVars(c('_mm','_g'))
fviz_nbclust(rawdata |> select(predvars$names),
             FUNcluster = kmeans)
kmeans_out <- kmeans(rawdata |> select(predvars$names),
                     centers = 2)

rawdata <- 
  mutate(rawdata,Cluster_k2var4=kmeans_out$cluster |> as.factor())
rawdata |> 
  ggplot(aes(species,fill=Cluster_k2var4))+
  geom_bar()
rawdata |> 
  ggplot(aes(fill=species,x=Cluster_k2var4))+
  geom_bar(position = 'fill')+
  scale_y_continuous(name = 'Frequency', labels=scales::percent)

rawdata |> 
  ggplot(aes(flipper_length_mm,bill_length_mm,
             shape=species,color=Cluster_k2var4))+
  geom_point()
rawdata |> 
  ggplot(aes(flipper_length_mm,bill_length_mm,
             color=species,shape=Cluster_k2var4))+
  geom_point()

fviz_cluster(kmeans_out,rawdata |> select(predvars$names))

# predict(kmeans_out)
clue::cl_predict(kmeans_out)
