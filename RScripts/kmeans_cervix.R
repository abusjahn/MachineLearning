pacman::p_load(conflicted,
               wrappedtools, 
               palmerpenguins,
               ggfortify, GGally,
               factoextra,
               caret
)

# conflict_scout()
conflict_prefer('select','dplyr')
conflict_prefer("filter", "dplyr")
rawdata <- readRDS('Data/cervical.RData')
predvars <- FindVars('\\d')
fviz_nbclust(rawdata |> select(predvars$names),
             FUNcluster = kmeans)
kmeans_out4 <- kmeans(rawdata |> select(predvars$names),
                     centers = 4, nstart = 10)

rawdata <- 
  mutate(rawdata,Cluster_k4=kmeans_out4$cluster |> as.factor())
rawdata |> 
  ggplot(aes(Tissuetype,fill=Cluster_k4))+
  geom_bar()
rawdata |> 
  ggplot(aes(fill=Tissuetype,x=Cluster_k4))+
  geom_bar(position = 'fill')+
  scale_y_continuous(name = 'Frequency', labels=scales::percent)


fviz_cluster(kmeans_out4,rawdata |> select(predvars$names))


predict(kmeans_out)
