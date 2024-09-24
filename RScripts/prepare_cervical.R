# BiocManager::install("MLSeq")
pacman::p_load(wrappedtools, tidyverse,
               MLSeq)
data("cervical")
head(cervical)
str(cervical)
rownames(cervical)
rawdata <- 
  cervical |> 
  rownames_to_column(var = 'sRNA') |> 
  pivot_longer(cols = -sRNA,
               names_to = 'SampleID',
               values_to="value") |> 
  separate(col = SampleID,
           into = c('Tissuetype','PatID'),
           sep = 1, #"(\\D+)(\\d+)"
           remove = FALSE) |> 
  pivot_wider(names_from = sRNA,
              values_from = value)|> 
  mutate(
    Tissuetype=factor(Tissuetype,
                      levels=c('N','T'),
                      labels=c('Control','Tumor')))

unstared_data <- select(rawdata,
                        -contains("*"))
# unstared_data <- select(rawdata,
#                         -matches("\\*"))


saveRDS(rawdata,'Data/cervical.RDS')
saveRDS(unstared_data,
        'Data/cervical_unstared.RDS')
