
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

#-------------------------------------------------------------------------------
# unadjusted analyses
#-------------------------------------------------------------------------------
table(d$message)
res = washb_glm(Y=d$Q7_ban_farming_support, tr=d$message, pair = NULL, W = NULL, 
          id=d$id, contrast=c("Control","Economics"), family = "gaussian", pval = 0.2)
res$TR


res_ban_farming_unadj = res_ban_sale_unadj = res_ban_subsidies_unadj = NULL

for(i in c("Sentience + Wellfare", "Environmental", "Economics", "Public health")){
  res_temp1=washb_glm(Y=as.numeric(d$Q7_ban_farming_support), 
                      tr=d$message, pair = NULL, W=NULL,  id=d$id, contrast=c("Control",i), family = "gaussian")$TR
  res_ban_farming_unadj = rbind(res_ban_farming_unadj, res_temp1)
  
  res_temp2=washb_glm(Y=as.numeric(d$Q7_ban_sale_support), 
                      tr=d$message, pair = NULL, W=NULL,  id=d$id, contrast=c("Control",i), family = "gaussian")$TR
  res_ban_sale_unadj = rbind(res_ban_sale_unadj, res_temp2)
  
  res_temp3=washb_glm(Y=as.numeric(d$Q7_ban_subsidies_support), 
                      tr=d$message, pair = NULL, W=NULL,  id=d$id, contrast=c("Control",i), family = "gaussian")$TR
  res_ban_subsidies_unadj = rbind(res_ban_subsidies_unadj, res_temp3)
}

res_ban_farming_unadj$Y = rownames(res_ban_farming_unadj)
res_ban_sale_unadj$Y = rownames(res_ban_sale_unadj)
res_ban_subsidies_unadj$Y = rownames(res_ban_subsidies_unadj)



ggplot(res_ban_farming_unadj, aes(x=Y, y=`Coef.`)) + geom_point() +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), width=0.2) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(title="Effect of Message on Support\nfor Ban on Farming Octopuses",
       x="Message Condition",
       y="Estimated Effect (95% CI)") +
  coord_flip() +
  theme_minimal() 

ggplot(res_ban_sale_unadj, aes(x=Y, y=`Coef.`)) + geom_point() +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), width=0.2) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(title="Effect of Message on Support\nfor Ban on Sale of Octopuses",
       x="Message Condition",
       y="Estimated Effect (95% CI)") +
  coord_flip() +
  theme_minimal() 

ggplot(res_ban_subsidies_unadj, aes(x=Y, y=`Coef.`)) + geom_point() +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), width=0.2) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(title="Effect of Message on Support\nfor Ban on Subsidies",
       x="Message Condition",
       y="Estimated Effect (95% CI)") +
  coord_flip() +
  theme_minimal() 

saveRDS(res_ban_farming_unadj, file=here("results/res_farming_unadj.rds"))
saveRDS(res_ban_sale_unadj, file=here("results/res_sale_unadj.rds"))
saveRDS(res_ban_subsidies_unadj, file=here("results/res_subsidies_unadj.rds"))

#-------------------------------------------------------------------------------
# adjusted analyses
#-------------------------------------------------------------------------------

res_ban_farming_adj = res_ban_sale_adj = res_ban_subsidies_adj = NULL

dput(colnames(d))

Wvars=c("COUNTRY", "URBANRURAL", "income_cat", "Gender", 
        "Age", "Q1_awareness", "Q2_support", "Q4_welfare_knowledge", 
        "Q4_environmental_knowledge", "Q4_economic_knowledge", "Q4_sentience_knowledge", 
        "Q4_public_health_knowledge", "Q5_consumption_frequency", "Q6_impact_on_consumption")

for(i in c("Sentience + Wellfare", "Environmental", "Economics", "Public health")){
  res_temp1=washb_glm(Y=as.numeric(d$Q7_ban_farming_support), 
                      tr=d$message, pair = NULL, W=d %>% select(all_of(Wvars)),  id=d$id, contrast=c("Control",i), family = "gaussian")$TR
  res_ban_farming_adj = rbind(res_ban_farming_adj, res_temp1)
  
  res_temp2=washb_glm(Y=as.numeric(d$Q7_ban_sale_support), 
                      tr=d$message, pair = NULL, W=d %>% select(all_of(Wvars)),  id=d$id, contrast=c("Control",i), family = "gaussian")$TR
  res_ban_sale_adj = rbind(res_ban_sale_adj, res_temp2)
  
  res_temp3=washb_glm(Y=as.numeric(d$Q7_ban_subsidies_support), 
                      tr=d$message, pair = NULL, W=d %>% select(all_of(Wvars)),  id=d$id, contrast=c("Control",i), family = "gaussian")$TR
  res_ban_subsidies_adj = rbind(res_ban_subsidies_adj, res_temp3)
}

res_ban_farming_adj$Y = rownames(res_ban_farming_adj)
res_ban_sale_adj$Y = rownames(res_ban_sale_adj)
res_ban_subsidies_adj$Y = rownames(res_ban_subsidies_adj)



ggplot(res_ban_farming_adj, aes(x=Y, y=`Coef.`)) + geom_point() +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), width=0.2) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(title="Effect of Message on Support\nfor Ban on Farming Octopuses",
       x="Message Condition",
       y="Estimated Effect (95% CI)") +
  coord_flip() +
  theme_minimal() 

ggplot(res_ban_sale_adj, aes(x=Y, y=`Coef.`)) + geom_point() +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), width=0.2) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(title="Effect of Message on Support\nfor Ban on Sale of Octopuses",
       x="Message Condition",
       y="Estimated Effect (95% CI)") +
  coord_flip() +
  theme_minimal() 

ggplot(res_ban_subsidies_adj, aes(x=Y, y=`Coef.`)) + geom_point() +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), width=0.2) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(title="Effect of Message on Support\nfor Ban on Subsidies",
       x="Message Condition",
       y="Estimated Effect (95% CI)") +
  coord_flip() +
  theme_minimal() 

saveRDS(res_ban_farming_adj, file=here("results/res_farming_adj.rds"))
saveRDS(res_ban_sale_adj, file=here("results/res_sale_adj.rds"))
saveRDS(res_ban_subsidies_adj, file=here("results/res_subsidies_adj.rds"))
