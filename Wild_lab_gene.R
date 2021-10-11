#install packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("readr")
install.packages("tibble")

#library download
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

#select relevant columns from sota
Sota_gene_expression <- SOTA %>% select(Mouse_ID, eimeriaSpecies, OPG, delta_ct_cewe_MminusE, Gene.Exp.cols) %>%
                                filter(!eimeriaSpecies == "NA")

#how many columns are there?
ncol(Sota_gene_expression)

#reshape to long format
long_gene_expression <- pivot_longer(Sota_gene_expression, 
                                     cols = c("IFNy", "IL.12", "IRG6", "CXCR3", "IL.6", "GBP2",
                                              "IL.10", "IL.13", "IL.10", "IL.13", "IL1RN",
                                              "CXCR3", "CASP1", "CXCL9", "GAPDH", 
                                              "IDO1", "IRGM1", "MPO", "MUC2", "MUC5AC", "MYD88", 
                                              "NCR1", "PPIB", "PRF1", "RETNLB", "SOCS1", "TICAM1", "TNF"), 
                                     names_to = "Gene", values_to = "Gene_Expression_Value")

#remove NA in your data
complete.cases(long_gene_expression)
long_gene_expression2 <- long_gene_expression[complete.cases(long_gene_expression), ]
str(long_gene_expression2)

#creating box plot species vs gene expression
ggplot(long_gene_expression2, aes(x = eimeriaSpecies, y = Gene_Expression_Value, fill = eimeriaSpecies)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(~Gene) +
  labs(x = "Infection status", y = "Gene Expression", 
       title = "Differences in gene expression between uninfected and Eimeria infected mice", 
       subtitle = "Are these differences comparable in the expression of different genes?")

 
#calculate variance 
#where are the eimeria spp. samples? 

#filter out the uninfected samples
long_gene_expression3 <- filter(long_gene_expression, !eimeriaSpecies == "Negative")

#now plot for infection intensity against gene expression
ggplot(long_gene_expression3, aes(x = delta_ct_cewe_MminusE, y = Gene_Expression_Value)) +
  geom_point(aes(color = eimeriaSpecies)) +
  geom_smooth(method=lm) +
  scale_y_log10() +
  facet_wrap(~Gene) +
  labs(x = "Infection Intensity", y = "Gene Expression", 
       title = "How does infection intensity affect Gene expression?", 
       subtitle = "Are there differences between E. ferrisi or E. falciformis?")
                        