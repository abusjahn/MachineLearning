# BiocManager::install("PCAtools")
pacman::p_load(conflicted,
               tidyverse,
               wrappedtools, 
               palmerpenguins,
               ggfortify, GGally,
               PCAtools, # bioconductor
               FactoMineR,
               ggrepel)

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


ggpairs(rawdata |> select(species, sex, 
                          predvars$names),
        aes(color=species,alpha=.5))
pca_out <- prcomp(rawdata |> 
                    select(predvars$names),
                  center = T,scale. = T)
summary(pca_out)

screeplot(pca_out,npcs = 4)

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
  ggplot(aes(PC1,PC3,color=species))+
  geom_point()

pca_out$rotation |> 
  as_tibble(rownames = "Variable") |> 
  ggplot(aes(PC1,PC3))+
  # geom_label_repel(aes(label=Variable))+
  geom_text(aes(label=Variable),
            hjust=0)+
  geom_segment(aes(xend=0,yend=0),
               arrow = arrow(end='first'))+
  scale_y_continuous(expand=expansion(.2))+
  scale_x_continuous(expand=expansion(
    mult = c(.1,.75)))

pca_loadings <- (pca_out$rotation*2.5) |> 
  as_tibble(rownames = "Variable")
pca_out$x |> 
  as_tibble() |> 
  cbind(rawdata) |> 
  ggplot(aes(PC1,PC3,color=species))+
  geom_point()+
  geom_text(
    data=pca_loadings,
    color="black",
    aes(label=Variable),
            hjust=0)+
  geom_segment(aes(xend=0,yend=0),
               data=pca_loadings,
               color="black",
               arrow = arrow(end='first',
                             length = unit(.05,
                                           'npc')))+
  scale_y_continuous(expand=expansion(.2),
                     breaks=seq(-10,10,1),
                     sec.axis = sec_axis(
                       ~(./2.5), 
                       name = "Loading",
                       breaks = seq(-3,10,1)/2.5))+
  scale_x_continuous(expand=expansion(
    mult = c(.1,.75)),
    breaks=seq(-10,10,1),
    sec.axis = sec_axis(
      ~(./2.5), name = "Loading",
      breaks = seq(-10,10,1)/2.5))

# decathlon
data("decathlon")
cortestR(decathlon |> select(1:10),
         split = T) |> 
  pluck('corout') |> 
  ggcormat(maxpoint = 15)
ggpairs(decathlon |> select(1:10))

pca_out_deca <- prcomp(decathlon |> select(1:10),
                  center = T,scale. = T)
summary(pca_out_deca)
pca_out_deca$rotation |> 
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


pacman::p_load(boot,factoextra)

# Load the decathlon data
data("decathlon")

# Function to perform PCA and extract desired components
pca_func <- function(data, indices) {
  pca <- prcomp(data[indices, 1:10], center = TRUE, scale. = TRUE)
  return(list(pca = pca, loadings = pca$rotation[, 1:2]))
}

# Set the number of bootstrap replicates
B <- 1000

# Perform bootstrapping
boot_out <- boot(decathlon[, 1:10], pca_func, R = B)

# Extract bootstrap estimates
boot_rotations <- sapply(boot_out$t, function(x) x$loadings)

# Calculate the mean and standard deviation of each component's loadings
mean_loadings <- colMeans(boot_rotations)
sd_loadings <- apply(boot_rotations, 2, sd)

# Visualize the stability of the loadings
for (i in 1:B) {
  pca_out <- boot_out$t[[i]]$pca
  fviz_pca_biplot(pca_out, col.var = "cos2", repel = TRUE)
  text(x = mean_loadings[, 1], y = mean_loadings[, 2], labels = colnames(decathlon)[1:10], pos = 4)
  arrows(x0 = 0, y0 = 0, x1 = mean_loadings[, 1], y1 = mean_loadings[, 2], length = 0.1)
  text(x = mean_loadings[, 1] + sd_loadings[, 1], y = mean_loadings[, 2] + sd_loadings[, 2],
       labels = paste0("(SD = ", round(sd_loadings[, 1], 3), ", ", round(sd_loadings[, 2], 3), ")"), pos = 4)
}

# Assess stability using the bootstrap confidence intervals
boot.ci(boot_out, type = "bca")