pacman::p_load(conflicted,
               tidyverse,
               wrappedtools, patchwork,
               palmerpenguins,ggrepel,
               ggfortify, GGally,
               rpart,rpart.plot)

# conflict_scout()
conflict_prefer('slice','dplyr')
conflict_prefer("filter", "dplyr")
conflict_prefer('screeplot','stats')
conflict_prefer('biplot','stats')
rawdata <- readRDS('Data/cervical.RData')
predvars <- FindVars('-')
# rawdata <- mutate(
#   rawdata,
#   across(.cols = all_of(predvars$names),
#          .fns = ~case_when(.x==0 ~ .x+0.01,
#                            .default = .x) |> 
#            log()))
scaled <- rawdata |> 
  select(predvars$names) |> 
  caret::preProcess(method = c(
    "nzv",
    "YeoJohnson",#"pca",
    "center","scale"))
rawdata <- predict(scaled,rawdata)
predvars <- ColSeeker(namepattern = '-')

# rawdata <- mutate(rawdata,
#                   across(.cols = all_of(predvars$names),
#                          .fns = ~log(.x+0.01)))

pca_out <- prcomp(rawdata |> select(predvars$names),
                  center = F,scale. = F)
summary(pca_out)

# predict(pca_out)

screeplot(pca_out,npcs = 20)

load_scaler <- 100 #inflate loads for plotting on 2nd axis

pca_out$rotation[1:10,1:3]
pca_loadings <- 
  (pca_out$rotation[,c(1:2)]*load_scaler) |> 
  as_tibble(rownames="Variable") |> 
  filter(abs(PC1)>7.5 | abs(PC2)>7.5)

pca_out$x[,1:2] |> 
  as_tibble() |> 
  cbind(rawdata) |> 
  ggplot(aes(PC1,PC2,color=Tissuetype))+
  geom_point()+
  geom_text_repel(
    data=pca_loadings,
    color="black",segment.color="darkgreen", 
    max.overlaps = 45,
    aes(label=Variable),
    hjust=0)+
  geom_segment(aes(xend=0,yend=0),
               data=pca_loadings,
               color="black",
               arrow = arrow(end='first',
                             length = unit(.05,
                                           'npc')))+
  scale_y_continuous(
    name="PC2 score",
    expand=expansion(.1),
    breaks=seq(-100,100,2),
    sec.axis = sec_axis(
      ~(./load_scaler), 
      name = "PC2 load",
      breaks = seq(-100,100,2)/load_scaler))+
  scale_x_continuous(
    name="PC1 score",
    expand=expansion(
    mult = c(.1,.1)),
    breaks=seq(-100,100,5),
    sec.axis = sec_axis(
      ~(./100), name = "PC1 load",
      breaks = seq(-100,100,5)/load_scaler))

pca_out$x[,2:3] |> 
  as_tibble() |> 
  cbind(rawdata) |> 
  ggplot(aes(PC2,PC3,color=Tissuetype))+
  geom_point()+
  # geom_text_repel(
  #   data=pca_loadings,
  #   color="black",segment.color="darkgreen", 
  #   max.overlaps = 45,
  #   aes(label=Variable),
  #   hjust=0)+
  # geom_segment(aes(xend=0,yend=0),
  #              data=pca_loadings,
  #              color="black",
  #              arrow = arrow(end='first',
  #                            length = unit(.05,
  #                                          'npc')))+
  scale_y_continuous(
    name="PC2 score",
    expand=expansion(.1),
    breaks=seq(-100,100,2),
    sec.axis = sec_axis(
      ~(./load_scaler), 
      name = "PC2 load",
      breaks = seq(-100,100,2)/load_scaler))+
  scale_x_continuous(
    name="PC1 score",
    expand=expansion(
      mult = c(.1,.1)),
    breaks=seq(-100,100,5),
    sec.axis = sec_axis(
      ~(./100), name = "PC1 load",
      breaks = seq(-100,100,5)/load_scaler))

cor_vars <- pca_loadings$Variable
cortestR(rawdata |> select(all_of(cor_vars)),
         split = T) |> 
  pluck('corout') |> 
  ggcormat(maxpoint = 3)

# biplot(pca_out)

autoplot(pca_out)
ap1 <- autoplot(pca_out, data=rawdata, colour='Tissuetype')
autoplot(pca_out, data=rawdata, colour='Tissuetype',
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 4)
#contribution of variables to component
ap2 <- autoplot(pca_out, data=rawdata, colour='Tissuetype',
         x=1,y=3)
ap3 <- autoplot(pca_out, data=rawdata, colour='Tissuetype',
         x=2,y=3)
ap1|ap2|ap3
rawdata <- pca_out$x[,1:17] |> 
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

importance_rt <- rpart_out$variable.importance |> 
  as_tibble(rownames="miRNA_group")
importance_rt |> 
  arrange(value) |> 
  mutate(miRNA_group=fct_inorder(miRNA_group)) |> 
  ggplot(aes(miRNA_group,value))+
  geom_col()+
  coord_flip()

loadings <- pca_out$rotation |> 
  as_tibble(rownames = 'RNA') |>
  select(1:17)

loadings |> 
  ggplot(aes(PC2))+
  geom_density()


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


# pca_out$x[,c(2,7)] |> 
#   as_tibble() |> 
#   cbind(rawdata) |> 
pca_loadings_27 <- 
  (pca_out$rotation[,c(2,7)]*load_scaler) |> 
  as_tibble(rownames="Variable") |> 
  filter(abs(PC2)>7.5 | abs(PC7)>7.5)

rawdata |> 
  ggplot(aes(PC2,PC7,color=Tissuetype))+
  geom_point()+
  # geom_text_repel(
  #   data=pca_loadings_27,
  #   color="black",segment.color="darkgreen", 
  #   max.overlaps = 45,
  #   aes(label=Variable),
  #   hjust=0)+
  # geom_segment(aes(xend=0,yend=0),
  #              data=pca_loadings_27,
  #              color="black",
  #              arrow = arrow(end='first',
  #                            length = unit(.05,
  #                                          'npc')))+
  scale_y_continuous(
    name="PC7 score",
    expand=expansion(.1),
    breaks=seq(-100,100,2),
    sec.axis = sec_axis(
      ~(./load_scaler), 
      name = "PC7 load",
      breaks = seq(-100,100,2)/load_scaler))+
  scale_x_continuous(
    name="PC2 score",
    expand=expansion(
      mult = c(.1,.1)),
    breaks=seq(-100,100,5),
    sec.axis = sec_axis(
      ~(./100), name = "PC2 load",
      breaks = seq(-100,100,5)/load_scaler))
