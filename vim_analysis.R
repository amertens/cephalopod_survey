


rm(list=ls())
library(here)
library(tidyverse)
library(washb)


d <- readRDS(here("data", "cleaned_octopus_farming_data.RDS"))
head(d)

levels(d$Q7_ban_farming_support)
d$Q7_ban_farming_support <- as.numeric(d$Q7_ban_farming_support)
d$Q7_ban_sale_support <- as.numeric(d$Q7_ban_sale_support)
d$Q7_ban_subsidies_support <- as.numeric(d$Q7_ban_subsidies_support)

d$Q1_awareness <- as.numeric(d$Q1_awareness)
d$Q2_support <- as.numeric(d$Q2_support)
d$Q4_welfare_knowledge <- as.numeric(d$Q4_welfare_knowledge)
d$Q4_environmental_knowledge <- as.numeric(d$Q4_environmental_knowledge)
d$Q4_economic_knowledge <- as.numeric(d$Q4_economic_knowledge)
d$Q4_sentience_knowledge <- as.numeric(d$Q4_sentience_knowledge)
d$Q4_public_health_knowledge <- as.numeric(d$Q4_public_health_knowledge)
d$Q5_consumption_frequency <- as.numeric(d$Q5_consumption_frequency)
d$Q6_impact_on_consumption <- as.numeric(d$Q6_impact_on_consumption)
d$Gender <- ifelse(d$Gender=="Male",1, 0) 

Wvars=c("COUNTRY", "URBANRURAL", "INCOME", "Gender", 
        "dAGE", "Q1_awareness", "Q2_support", "Q4_welfare_knowledge", 
        "Q4_environmental_knowledge", "Q4_economic_knowledge", "Q4_sentience_knowledge", 
        "Q4_public_health_knowledge", "Q5_consumption_frequency", "Q6_impact_on_consumption")


Y1equation= paste0("Q7_ban_farming_support ~ message + ", paste(Wvars, collapse="+"))
Y2equation= paste0("Q7_ban_sale_support ~ message + ", paste(Wvars, collapse="+"))
Y3equation= paste0("Q7_ban_subsidies_support ~ message + ", paste(Wvars, collapse="+"))

resY1 = glm(formula=Y1equation, data=d %>% filter(!is.na(Q7_ban_farming_support),!is.infinite(Q7_ban_farming_support)))
resY1=summary(resY1) 
plotdf_Y1= data.frame(est=resY1$coefficients[,1], se=resY1$coefficients[,2], pval=resY1$coefficients[,4])
plotdf_Y1$ci.lb <- plotdf_Y1$est - 1.96*plotdf_Y1$se
plotdf_Y1$ci.ub <- plotdf_Y1$est + 1.96*plotdf_Y1$se

resY2 = glm(formula=Y2equation, data=d %>% filter(!is.na(Q7_ban_sale_support)))
resY2=summary(resY2) 
plotdf_Y2= data.frame(est=resY2$coefficients[,1], se=resY2$coefficients[,2], pval=resY2$coefficients[,4])
plotdf_Y2$ci.lb <- plotdf_Y2$est - 1.96*plotdf_Y2$se
plotdf_Y2$ci.ub <- plotdf_Y2$est + 1.96*plotdf_Y2$se

resY3 = glm(formula=Y3equation, data=d %>% filter(!is.na(Q7_ban_subsidies_support)))
resY3=summary(resY3) 
plotdf_Y3= data.frame(est=resY3$coefficients[,1], se=resY3$coefficients[,2], pval=resY3$coefficients[,4])
plotdf_Y3$ci.lb <- plotdf_Y3$est - 1.96*plotdf_Y3$se
plotdf_Y3$ci.ub <- plotdf_Y3$est + 1.96*plotdf_Y3$se


plotdf_Y1$X = rownames(plotdf_Y1)
plotdf_Y2$X = rownames(plotdf_Y2)
plotdf_Y3$X = rownames(plotdf_Y3)

for(i in Wvars){
  if(is.factor(d[[i]])){
    plotdf_Y1$X=gsub(i,paste0(i,": "),plotdf_Y1$X)
  }
}
plotdf_Y1$X=gsub("\\.\\.",": ",plotdf_Y1$X)
plotdf_Y1$X=gsub("\\."," ",plotdf_Y1$X)

for(i in Wvars){
  if(is.factor(d[[i]])){
    plotdf_Y2$X=gsub(i,paste0(i,": "),plotdf_Y2$X)
  }
}
plotdf_Y2$X=gsub("\\.\\.",": ",plotdf_Y2$X)
plotdf_Y2$X=gsub("\\."," ",plotdf_Y2$X)

for(i in Wvars){
  if(is.factor(d[[i]])){
    plotdf_Y3$X=gsub(i,paste0(i,": "),plotdf_Y3$X)
  }
}
plotdf_Y3$X=gsub("\\.\\.",": ",plotdf_Y3$X)
plotdf_Y3$X=gsub("\\."," ",plotdf_Y3$X)




plotdf_Y1 <- plotdf_Y1 %>% filter(X !="(Intercept)", !grepl("refer not to say", X)) 
plotdf_Y2 <- plotdf_Y2 %>% filter(X !="(Intercept)", !grepl("refer not to say", X)) 
plotdf_Y3 <- plotdf_Y3 %>% filter(X !="(Intercept)", !grepl("refer not to say", X)) 
plotdf_Y1$X[plotdf_Y1$X=="Gender"] <-"GenderMale"
plotdf_Y2$X[plotdf_Y2$X=="Gender"] <-"GenderMale"

colnames(plotdf_Y1)


rownames(plotdf_Y1)=NULL
rownames(plotdf_Y2)=NULL
rownames(plotdf_Y3)=NULL

ggplot(plotdf_Y1, aes(x=X, y=est )) + geom_point() +
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), width=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(#title="Effect of Message Condition on Support for Legislation\nto Stop Sale of Fur from Any Animal",
    x="Covariate",
    y="Estimated Effect (95% CI)") +
  coord_flip() +theme_minimal() 

ggplot(plotdf_Y2, aes(x=X, y=est )) + geom_point() +
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), width=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(#title="Effect of Message Condition on Support for Legislation\nto Stop Sale of Fur from Any Animal",
    x="Covariate",
    y="Estimated Effect (95% CI)") +
  coord_flip() +theme_minimal() 

ggplot(plotdf_Y3, aes(x=X, y=est )) + geom_point() +
  geom_errorbar(aes(ymin=ci.lb, ymax=ci.ub), width=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(#title="Effect of Message Condition on Support for Legislation\nto Stop Sale of Fur from Any Animal",
    x="Covariate",
    y="Estimated Effect (95% CI)") +
  coord_flip() +theme_minimal() 


saveRDS(plotdf_Y1, file=here("results/res_farm_vim_glm.rds"))
saveRDS(plotdf_Y2, file=here("results/res_sale_vim_glm.rds"))
saveRDS(plotdf_Y3, file=here("results/res_subsidies_vim_glm.rds"))
