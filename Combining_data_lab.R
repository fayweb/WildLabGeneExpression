library(tidyverse)
library(dplyr)

#load all the relevant data
E57_Weight <- read.csv("https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/E57_xxxxx_Eim_record.csv")
E57_infection <- read.csv("https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/E7_112018_Eim_CEWE_qPCR.csv")
E57_gene <- read.csv("https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/E7_112018_Eim_CEWE_RT-qPCR.csv")

P3_Weight <-read.csv("https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P3_112019_Eim_record.csv")
P3_infection <- read.csv("https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P3_112019_Eim_CEWE_qPCR.csv")
P3_gene <- read.csv("https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P3_112019_Eim_CEWE_RTqPCR.csv")


#now join the tables
join_E57_1 <- full_join(E57_gene, E57_infection, by = "EH_ID") 
join_E57 <- full_join(join_E57_1, E57_Weight, by = "EH_ID") 

join_P3_1 <- full_join(P3_gene, P3_infection, P3_Weight, by = "EH_ID") 
join_P3 <- full_join(join_P3_1, P3_Weight, by = "EH_ID") 

#combining both experiment tables
E57_P3 <- bind_rows(join_E57, join_P3)

#select only challenge infection
E57_P3_c <- E57_P3 %>% 
                    filter(infection == "challenge") %>% #filter for challenge infections
                    select(EH_ID, CXCR3, IRG6, IL.12, delta, experiment, relative_weight, dpi, infection)

#have each gene in a row
E57_P3_long  <- pivot_longer(E57_P3_c, cols = c("CXCR3", "IRG6", "IL.12"), names_to = "Gene", values_to = "Gene_Expression_Value")

#remove NA in your data
complete.cases(E57_P3_long)
E57_P3_long2 <- E57_P3_long[complete.cases(E57_P3_long), ]
str(E57_P3_long2)

#now plot for infection intensity against gene expression
ggplot(E57_P3_long2, aes(x = delta, y = Gene_Expression_Value)) +
  geom_point() +
  geom_smooth(method=lm) +
  scale_y_log10() +
  facet_wrap(~Gene) +
  labs(x = "Infection Intensity", y = "Gene Expression", 
       title = "How does infection intensity affect Gene expression in challenge infections?")
