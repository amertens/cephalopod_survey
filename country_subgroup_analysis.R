
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


res_ban_farming_subgroup = res_ban_sale_subgroup = res_ban_subsidies_subgroup = NULL

dput(colnames(d))

Wvars=c("COUNTRY", "URBANRURAL", "income_cat", "Gender", 
        "Age", "Q1_awareness", "Q2_support", "Q4_welfare_knowledge", 
        "Q4_environmental_knowledge", "Q4_economic_knowledge", "Q4_sentience_knowledge", 
        "Q4_public_health_knowledge", "Q5_consumption_frequency", "Q6_impact_on_consumption")

for(i in c("Sentience + Wellfare", "Environmental", "Economics", "Public health")){
  res_temp1=washb_glm(Y=as.numeric(d$Q7_ban_farming_support), 
                      V="COUNTRY",
                      tr=d$message, pair = NULL, W=d %>% select(all_of(Wvars)),  id=d$id, contrast=c("Control",i), family = "gaussian")$lincom
  colnames(res_temp1)[1] <- "Country"
  res_temp1$tr <- i
  res_ban_farming_subgroup = rbind(res_ban_farming_subgroup, res_temp1)
  
  res_temp2=washb_glm(Y=as.numeric(d$Q7_ban_sale_support), 
                      V="COUNTRY",
                      tr=d$message, pair = NULL, W=d %>% select(all_of(Wvars)),  id=d$id, contrast=c("Control",i), family = "gaussian")$lincom
  colnames(res_temp2)[1] <- "Country"
  res_temp2$tr <- i
  res_ban_sale_subgroup = rbind(res_ban_sale_subgroup, res_temp2)
  
  res_temp3=washb_glm(Y=as.numeric(d$Q7_ban_subsidies_support), 
                      V="COUNTRY",
                      tr=d$message, pair = NULL, W=d %>% select(all_of(Wvars)),  id=d$id, contrast=c("Control",i), family = "gaussian")$lincom
  colnames(res_temp3)[1] <- "Country"
    res_temp3$tr <- i
  res_ban_subsidies_subgroup = rbind(res_ban_subsidies_subgroup, res_temp3)
}

res_ban_farming_subgroup$Y = rownames(res_ban_farming_subgroup)
res_ban_sale_subgroup$Y = rownames(res_ban_sale_subgroup)
res_ban_subsidies_subgroup$Y = rownames(res_ban_subsidies_subgroup)



ggplot(res_ban_farming_subgroup, aes(x=Country, y=est, group=tr, color=tr)) + geom_point(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin=est.lb, ymax=est.ub), width=0.2, position = position_dodge(0.5)) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(title="Effect of Message on Support\nfor Ban on Farming Octopuses",
       x="Message Condition",
       y="Estimated Effect (95% CI)") +
  coord_flip() +
  theme_minimal() 


ggplot(res_ban_farming_subgroup, aes(x=Country, y=est)) + geom_point() +
  geom_errorbar(aes(ymin=est.lb, ymax=est.ub), width=0.2) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  facet_wrap(~tr) +
  labs(title="Effect of Message on Support\nfor Ban on Farming Octopuses",
       x="Message Condition",
       y="Estimated Effect (95% CI)") +
  coord_flip() +
  theme_minimal() 

ggplot(res_ban_sale_subgroup, aes(x=Country, y=est)) + geom_point() +
  geom_errorbar(aes(ymin=est.lb, ymax=est.ub), width=0.2) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  facet_wrap(~tr) +
  labs(title="Effect of Message on Support\nfor Ban on Sale of Octopuses",
       x="Message Condition",
       y="Estimated Effect (95% CI)") +
  coord_flip() +
  theme_minimal() 

ggplot(res_ban_subsidies_subgroup, aes(x=Country, y=est)) + geom_point() +
  geom_errorbar(aes(ymin=est.lb, ymax=est.ub), width=0.2) +
  facet_wrap(~tr) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(title="Effect of Message on Support\nfor Ban on Subsidies",
       x="Message Condition",
       y="Estimated Effect (95% CI)") +
  coord_flip() +
  theme_minimal() 

saveRDS(res_ban_farming_subgroup, file=here("results/res_farming_subgroup.rds"))
saveRDS(res_ban_sale_subgroup, file=here("results/res_sale_subgroup.rds"))
saveRDS(res_ban_subsidies_subgroup, file=here("results/res_subsidies_subgroup.rds"))
