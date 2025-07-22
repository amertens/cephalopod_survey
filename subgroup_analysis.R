
rm(list=ls())
library(tidyverse)
library(here)
library(washb)

d <- readRDS(here("data", "cleaned_octopus_farming_data.RDS"))
colnames(d)
dput(colnames(d))

table(d$Gender)

#outcome variables
table(d$Q7_ban_farming_support)
table(d$Q7_ban_sale_support)
table(d$Q7_ban_subsidies_support)

#convert to numeric
d$Q7_ban_farming_support <- as.numeric(d$Q7_ban_farming_support)
d$Q7_ban_sale_support <- as.numeric(d$Q7_ban_sale_support)
d$Q7_ban_subsidies_support <- as.numeric(d$Q7_ban_subsidies_support)

d$id <- 1:nrow(d)
d$income_cat


dput(colnames(d))

Wvars=c("COUNTRY", "URBANRURAL", "income_cat", "Gender", 
        "Age", "Q1_awareness", "Q2_support", "Q4_welfare_knowledge", 
        "Q4_environmental_knowledge", "Q4_economic_knowledge", "Q4_sentience_knowledge", 
        "Q4_public_health_knowledge", "Q5_consumption_frequency", "Q6_impact_on_consumption")

run_cephalopod_subgroup <- function(d, Vvar){

  res_ban_farming_subgroup = res_ban_sale_subgroup = res_ban_subsidies_subgroup = NULL
  
  for(i in c("Sentience + Wellfare", "Environmental", "Economics", "Public health")){

    res_null1=washb_glm(Y=as.numeric(d$Q7_ban_farming_support), 
                        tr=d$message, pair = NULL, W=d %>% select(all_of(Wvars)),  id=d$id, contrast=c("Control",i), family = "gaussian")    
    res_temp1=washb_glm(Y=as.numeric(d$Q7_ban_farming_support), 
                        V=Vvar,
                        tr=d$message, pair = NULL, W=d %>% select(all_of(Wvars)),  id=d$id, contrast=c("Control",i), family = "gaussian")
    lr_res1=lmtest::lrtest(res_temp1$glmModel, res_null1$glmModel)
    res_temp1=res_temp1$lincom
    colnames(res_temp1)[1] <- Vvar
    res_temp1$tr <- i
    res_temp1$int.pval <- lr_res1$`Pr(>Chisq)`[2]
    res_ban_farming_subgroup = rbind(res_ban_farming_subgroup, res_temp1)

    res_null2=washb_glm(Y=as.numeric(d$Q7_ban_sale_support), 
                        tr=d$message, pair = NULL, W=d %>% select(all_of(Wvars)),  id=d$id, contrast=c("Control",i), family = "gaussian")
    res_temp2=washb_glm(Y=as.numeric(d$Q7_ban_sale_support), 
                        V=Vvar,
                        tr=d$message, pair = NULL, W=d %>% select(all_of(Wvars)),  id=d$id, contrast=c("Control",i), family = "gaussian")
    lr_res2=lmtest::lrtest(res_temp2$glmModel, res_null2$glmModel)
    res_temp2=res_temp2$lincom
    colnames(res_temp2)[1] <- Vvar
    res_temp2$tr <- i
    res_temp2$int.pval <- lr_res2$`Pr(>Chisq)`[2]
    res_ban_sale_subgroup = rbind(res_ban_sale_subgroup, res_temp2)
    
    res_null3=washb_glm(Y=as.numeric(d$Q7_ban_subsidies_support), 
                        tr=d$message, pair = NULL, W=d %>% select(all_of(Wvars)),  id=d$id, contrast=c("Control",i), family = "gaussian")
    res_temp3=washb_glm(Y=as.numeric(d$Q7_ban_subsidies_support), 
                        V=Vvar,
                        tr=d$message, pair = NULL, W=d %>% select(all_of(Wvars)),  id=d$id, contrast=c("Control",i), family = "gaussian")
    lr_res3=lmtest::lrtest(res_temp3$glmModel, res_null3$glmModel)
    res_temp3=res_temp3$lincom
    colnames(res_temp3)[1] <- Vvar
      res_temp3$tr <- i
      res_temp3$int.pval <- lr_res3$`Pr(>Chisq)`[2]
    res_ban_subsidies_subgroup = rbind(res_ban_subsidies_subgroup, res_temp3)
  }
  
  res_ban_farming_subgroup$Y = rownames(res_ban_farming_subgroup)
  res_ban_sale_subgroup$Y = rownames(res_ban_sale_subgroup)
  res_ban_subsidies_subgroup$Y = rownames(res_ban_subsidies_subgroup)
  
  res_int_pval <- bind_rows(
    res_ban_farming_subgroup %>% distinct(int.pval, .keep_all = T) %>% select(!!(Vvar), tr, int.pval) %>% mutate(Y="ban farming"),
    res_ban_sale_subgroup %>% distinct(int.pval, .keep_all = T) %>% select(!!(Vvar), tr, int.pval) %>% mutate(Y="ban sale"),
    res_ban_sale_subgroup %>% distinct(int.pval, .keep_all = T) %>% select(!!(Vvar), tr, int.pval) %>% mutate(Y="ban subsidies"))
  
  p_farming = ggplot(res_ban_farming_subgroup, aes(x=.data[[Vvar]], y=est)) + geom_point() +
    geom_errorbar(aes(ymin=est.lb, ymax=est.ub), width=0.2) +
    geom_hline(yintercept=0, linetype="dashed", color="red") +
    facet_wrap(~tr) +
    labs(title="Effect of Message on Support\nfor Ban on Farming Octopuses",
         x="Message Condition",
         y="Estimated Effect (95% CI)") +
    coord_flip() +
    theme_minimal() 
  
  p_subsidies = ggplot(res_ban_sale_subgroup, aes(x=.data[[Vvar]], y=est)) + geom_point() +
    geom_errorbar(aes(ymin=est.lb, ymax=est.ub), width=0.2) +
    geom_hline(yintercept=0, linetype="dashed", color="red") +
    facet_wrap(~tr) +
    labs(title="Effect of Message on Support\nfor Ban on Sale of Octopuses",
         x="Message Condition",
         y="Estimated Effect (95% CI)") +
    coord_flip() +
    theme_minimal() 
  
  p_sale = ggplot(res_ban_subsidies_subgroup, aes(x=.data[[Vvar]], y=est)) + geom_point() +
    geom_errorbar(aes(ymin=est.lb, ymax=est.ub), width=0.2) +
    facet_wrap(~tr) +
    geom_hline(yintercept=0, linetype="dashed", color="red") +
    labs(title="Effect of Message on Support\nfor Ban on Subsidies",
         x="Message Condition",
         y="Estimated Effect (95% CI)") +
    coord_flip() +
    theme_minimal() 
  
  return(list(res_ban_farming_subgroup=res_ban_farming_subgroup,
              res_ban_sale_subgroup=res_ban_sale_subgroup,
              res_ban_subsidies_subgroup=res_ban_subsidies_subgroup,
              res_int_pval=res_int_pval,
              p_farming=p_farming,
              p_sale=p_sale,
              p_subsidies=p_subsidies))
}

res_country_subgroup <- run_cephalopod_subgroup(d=d, Vvar="COUNTRY")
res_URBANRURAL_subgroup <- run_cephalopod_subgroup(d=d, Vvar="URBANRURAL")
res_income_cat_subgroup <- run_cephalopod_subgroup(d=d, Vvar="income_cat")

d$Gender <- factor(ifelse(d$Gender=="Male","Male","Female+"))
res_Gender_subgroup <- run_cephalopod_subgroup(d=d, Vvar="Gender")

d$Age<-d$dAGE
res_Age_subgroup <- run_cephalopod_subgroup(d=d, Vvar="Age")

res_country_subgroup$res_int_pval %>% arrange(int.pval)
res_URBANRURAL_subgroup$res_int_pval %>% arrange(int.pval)
res_income_cat_subgroup$res_int_pval %>% arrange(int.pval)
res_Gender_subgroup$res_int_pval %>% arrange(int.pval)
res_Age_subgroup$res_int_pval %>% arrange(int.pval)


res_country_subgroup$p_farming
res_URBANRURAL_subgroup$p_farming
res_URBANRURAL_subgroup$p_sale
res_URBANRURAL_subgroup$p_subsidies
res_income_cat_subgroup$p_farming
res_Gender_subgroup$p_farming
res_Age_subgroup$p_farming

   

res_ban_farming_subgroup=res_country_subgroup$res_ban_farming_subgroup
res_ban_subsidies_subgroup=res_country_subgroup$res_ban_subsidies_subgroup
res_ban_sale_subgroup=res_country_subgroup$res_ban_sale_subgroup

ggplot(res_ban_farming_subgroup, aes(x=Country, y=est, group=tr, color=tr)) + geom_point(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin=est.lb, ymax=est.ub), width=0.2, position = position_dodge(0.5)) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(title="Effect of Message on Support\nfor Ban on Farming Octopuses",
       x="Message Condition",
       y="Estimated Effect (95% CI)") +
  coord_flip() +
  theme_minimal() 



saveRDS(res_ban_farming_subgroup, file=here("results/res_farming_subgroup.rds"))
saveRDS(res_ban_sale_subgroup, file=here("results/res_sale_subgroup.rds"))
saveRDS(res_ban_subsidies_subgroup, file=here("results/res_subsidies_subgroup.rds"))
