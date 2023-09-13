pacman::p_load(conflicted,
               tidyverse,
               wrappedtools, 
               palmerpenguins,
               ggfortify, GGally,
               PCAtools, # bioconductor
               FactoMineR)

# conflict_scout()
conflict_prefer('slice','dplyr')
conflict_prefer("filter", "dplyr")
conflict_prefer('screeplot','stats')
conflict_prefer('biplot','stats')
rawdata <- penguins |> 
  na.omit()
rawdata <- mutate(rawdata,
                  ID=paste('P', 1:nrow(rawdata))) |> 
  select(ID, everything())
predvars <- FindVars(c('_mm','_g'))

cortestR(rawdata |> select(predvars$names),
         split = T) |> 
  pluck('corout') |> 
  ggcormat(maxpoint = 10)


ggpairs(rawdata |> select(species, predvars$names),
        aes(color=species,alpha=.5))
pca_out <- prcomp(rawdata |> select(predvars$names),
                  center = T,scale. = T)
summary(pca_out)

screeplot(pca_out,npcs = 5)

pca_out$rotation

biplot(pca_out)

autoplot(pca_out)
autoplot(pca_out, data=rawdata, colour='species')
autoplot(pca_out, data=rawdata, colour='species',
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 4)
#contribution of variables to component
autoplot(pca_out, data=rawdata, colour='species',
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3,
         x=1,y=3)

pca_out$x |> 
  as_tibble() |> 
  cbind(rawdata) |> 
  ggplot(aes(PC1,PC2,color=species))+
  geom_point()


# decathlon
data("decathlon")
cortestR(decathlon |> select(1:10),
         split = T) |> 
  pluck('corout') |> 
  ggcormat(maxpoint = 10)
ggpairs(decathlon |> select(1:10))

pca_out <- prcomp(decathlon |> select(1:10),
                  center = T,scale. = T)
summary(pca_out)
pca_out$rotation |> 
  as_tibble(rownames = 'Exercise') |> 
  mutate(across(-Exercise,
                ~case_when(abs(.)<.25 ~ 0,
                           TRUE ~ .))) |> 
  select(1:6)

autoplot(pca_out, data=decathlon, 
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 4)

# just 2 components
pca_out2 <- prcomp(rawdata |> select(predvars$names),
                   center = T,scale. = T,rank. = 2)
summary(pca_out2)

pca_out2$rotation

#contribution of variables to component
autoplot(pca_out2, data=rawdata, colour='species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3,
         x=1,y=2)

pca_out2$x |> 
  as_tibble() |> 
  cbind(rawdata) |> 
  ggplot(aes(PC1,PC2,color=species))+
  geom_point()


# PCA tools
pca_mat <- rawdata |> select(ID,predvars$names) |> 
  column_to_rownames(var = 'ID') |> 
  as.matrix() |> 
  t()

pca_out3 <- pca(mat = pca_mat,
                center = T,scale = T
)
getVars(pca_out3)
getLoadings(pca_out3)

PCAtools::screeplot(pca_out3)
PCAtools::biplot(pca_out3,
                 showLoadings = TRUE,ntopLoadings = 4,
                 labSize = 5, pointSize = 3, sizeLoadingsNames = 3)
# pairsplot(pca_out3)
# eigencorplot(pca_out3,
#              metavars=predvars$names)
pca_out3$loadings |> 
  as_tibble(rownames='measure') |> 
  ggplot(aes(PC1,PC2,shape=measure))+
  geom_point(data=pca_out3$rotated, color='grey', shape=1)+
  geom_segment(xend=0,yend=0,arrow=arrow(ends='first'))+
  ggrepel::geom_label_repel(aes(label=measure))
