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
predvars <- ColSeeker(namepattern = '-')
scaled <- rawdata |> 
  select(predvars$names) |> 
  caret::preProcess(method = c(
    "zv","nzv",
    "YeoJohnson",#"pca",
    "center","scale"))
rawdata <- predict(scaled,rawdata)
predvars <- ColSeeker(namepattern = '-')

fviz_nbclust(rawdata |> select(predvars$names),
             FUNcluster = kmeans)
kmeans_out2 <- kmeans(rawdata |> select(predvars$names),
                     centers = 2, nstart = 10)

rawdata <- 
  mutate(rawdata,Cluster_k2=kmeans_out2$cluster |> as.factor())
rawdata |> 
  ggplot(aes(Tissuetype,fill=Cluster_k2))+
  geom_bar()
rawdata |> 
  ggplot(aes(fill=Tissuetype,x=Cluster_k2))+
  geom_bar(position = 'fill')+
  scale_y_continuous(name = 'Frequency', labels=scales::percent)


fviz_cluster(kmeans_out2,rawdata |> select(predvars$names))


predict(kmeans_out)
