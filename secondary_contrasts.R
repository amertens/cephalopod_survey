
rm(list=ls())
library(tidyverse)
library(here)
library(washb)

d <- readRDS(here("data", "cleaned_octopus_farming_data.RDS"))
colnames(d)
dput(colnames(d))

#outcome variables
table(d$Q7_ban_farming_support)
table(d$Q7_ban_sale_support)
table(d$Q7_ban_subsidies_support)

#convert to numeric
d$Q7_ban_farming_support <- as.numeric(d$Q7_ban_farming_support)
d$Q7_ban_sale_support <- as.numeric(d$Q7_ban_sale_support)
d$Q7_ban_subsidies_support <- as.numeric(d$Q7_ban_subsidies_support)

d$id <- 1:nrow(d)


res_ban_farming_adj = res_ban_sale_adj = res_ban_subsidies_adj = NULL

dput(colnames(d))

Wvars=c("COUNTRY", "URBANRURAL", "income_cat", "Gender", 
        "Age", "Q1_awareness", "Q2_support", "Q4_welfare_knowledge", 
        "Q4_environmental_knowledge", "Q4_economic_knowledge", "Q4_sentience_knowledge", 
        "Q4_public_health_knowledge", "Q5_consumption_frequency", "Q6_impact_on_consumption")

for(i in c("Sentience + Wellfare", "Environmental", "Economics", "Public health")){
  for(j in c("Sentience + Wellfare", "Environmental", "Economics", "Public health")){
    if(i != j){
  res_temp1=washb_glm(Y=as.numeric(d$Q7_ban_farming_support), 
                      tr=d$message, pair = NULL, W=d %>% select(all_of(Wvars)),  id=d$id, contrast=c(i,j), family = "gaussian")$TR
  res_temp1$contrast =paste0(j, " v. ", i," (ref)")
  res_temp1$intervention=j
  res_temp1$ref=i
  res_ban_farming_adj = rbind(res_ban_farming_adj, res_temp1)
  
  res_temp2=washb_glm(Y=as.numeric(d$Q7_ban_sale_support), 
                      tr=d$message, pair = NULL, W=d %>% select(all_of(Wvars)),  id=d$id, contrast=c(i,j), family = "gaussian")$TR
  res_temp2$contrast =paste0(j, " v. ", i," (ref)")
  res_temp2$intervention=j
  res_temp2$ref=i
  res_ban_sale_adj = rbind(res_ban_sale_adj, res_temp2)
  
  res_temp3=washb_glm(Y=as.numeric(d$Q7_ban_subsidies_support), 
                      tr=d$message, pair = NULL, W=d %>% select(all_of(Wvars)),  id=d$id, contrast=c(i,j), family = "gaussian")$TR
  res_temp3$contrast =paste0(j, " v. ", i," (ref)")
  res_temp3$intervention=j
  res_temp3$ref=i
  res_ban_subsidies_adj = rbind(res_ban_subsidies_adj, res_temp3)
}}}



ggplot(res_ban_farming_adj, aes(x=contrast, y=`Coef.`)) + geom_point() +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), width=0.2) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(title="Effect of Message on Support\nfor Ban on Farming Octopuses",
       x="Message Condition",
       y="Estimated Effect (95% CI)") +
  coord_flip() +
  theme_minimal() 

ggplot(res_ban_sale_adj, aes(x=contrast, y=`Coef.`)) + geom_point() +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), width=0.2) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(title="Effect of Message on Support\nfor Ban on Sale of Octopuses",
       x="Message Condition",
       y="Estimated Effect (95% CI)") +
  coord_flip() +
  theme_minimal() 

ggplot(res_ban_subsidies_adj, aes(x=contrast, y=`Coef.`)) + geom_point() +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), width=0.2) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(title="Effect of Message on Support\nfor Ban on Subsidies",
       x="Message Condition",
       y="Estimated Effect (95% CI)") +
  coord_flip() +
  theme_minimal() 

saveRDS(res_ban_farming_adj, file=here("results/res_farming_secondary.rds"))
saveRDS(res_ban_sale_adj, file=here("results/res_sale_secondary.rds"))
saveRDS(res_ban_subsidies_adj, file=here("results/res_subsidies_secondary.rds"))
