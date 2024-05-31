# AIM ---------------------------------------------------------------------
# After including LL in the model, the effect on the delta age is taken over by LL. This is because LL and PRL categories are correlated. The consequence is expected as they are correlated.

# Try to build two different models, one with only PRL and one with only LL. compare them and use the root mean square error which one leads with the lowest error is the best model

# keep the LL as continuous is better. even though using it as categorical is easier to interpret.

# libraries ---------------------------------------------------------------
library(tidyverse)

# read in the data --------------------------------------------------------
df <- read_csv("../../data/brain_age_PRL_05262023.csv")

# wrangling ---------------------------------------------------------------
# try to apply a binary separation of the dataset based on the median age
# cut_off <- median(df_full$chronological_age)
# use and arbitrary cut off to divide young and old patient.
cut_off <- 55
# cut_off <- 50
# cut_off <- round(median(df$chronological_age),digits = 0)

df_full <- df %>%
  # add the difference between the estiamted and the actual brain age
  mutate(delta_age = BrainageR - chronological_age) %>%
  # add the category of the PRL
  mutate(PRL_cat = case_when(PRL == 0 ~ "L_0",
                             PRL <= 3 ~ "L_1_3",
                             T ~ "L_4")) %>%
  # add to the dataset the variable for the age cut off
  mutate(age_cat = case_when(chronological_age<=cut_off~"young",
                             T~"old")) %>%
  mutate(age_cat = factor(age_cat,levels = c("young","old"))) %>%
  # make the phenotype variable as category
  mutate(phenotype = factor(phenotype)) %>% 
  # make the gender variable as category
  mutate(sex = factor(sex))

# show the distribution of the variables in the model ---------------------
df_full %>%
  # pivot_longer(names_to = "var",values_to = "value",c(id_sample,chronological_age,BrainageR,LL,Brain_volume,gARMSS,MSSS,delta_age))
  pivot_longer(names_to = "var",values_to = "value",-c(id_sample,center,sex,phenotype,PRL_cat,age_cat)) %>%
  ggplot(aes(x=value))+geom_density()+facet_wrap(~var,scales = "free")+theme_bw()+theme(strip.background = element_blank())

# build the model ---------------------------------------------------------
# model with PRL
model_PRL <- lm(df_full,formula = delta_age~center+sex+age_cat*PRL_cat)
summary(model_PRL)

# main plot
df_full %>%
  ggplot(aes(y=delta_age,x=PRL_cat,fill = age_cat)) + 
  # geom_violin()+
  # geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9),width=0.1)+
  geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9))+
  geom_point(position=position_jitterdodge(dodge.width=0.9,jitter.width = 0.4),alpha=0.2) +
  theme_bw()

# model with LL leave it as numerical
model_LL <- lm(df_full,formula = delta_age~center+sex+age_cat*LL)
summary(model_LL)

model_LL2 <- lm(df_full,formula = delta_age~center+sex+age_cat+LL)
summary(model_LL2)

# main plot
df_full %>%
  ggplot(aes(y=delta_age,x=LL,group = age_cat,color = age_cat)) + 
  # geom_violin()+
  # geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9),width=0.1)+
  # geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9))+
  geom_point(alpha=0.2) +
  scale_x_log10()+
  theme_bw()

# try to use it categorical
df_full2 <- df_full %>% 
  mutate(LL_cat = case_when(LL<median(LL,na.rm = T)~"low",
                            T~"high")) %>% 
  mutate(LL_cat = factor(LL_cat,levels = c("low","high")))
  
model_LLcat <- lm(df_full2,formula = delta_age~center+sex+age_cat*LL_cat)
summary(model_LLcat)

# main plot
df_full2 %>%
  ggplot(aes(y=delta_age,x=LL_cat,fill = age_cat)) + 
  # geom_violin()+
  # geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9),width=0.1)+
  # geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9))+
  geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9))+
  geom_point(position=position_jitterdodge(dodge.width=0.9,jitter.width = 0.4),alpha=0.2) +
  theme_bw()

# -------------------------------------------------------------------------
# try Celine suggestion for the stepwise selction
# select the variables
test <- df_full2 %>% 
  select(center,sex,phenotype,EDSS,disease_duration,Brain_volume,gARMSS,MSSS,delta_age,PRL_cat,age_cat,LL_cat) %>% 
  # make sure all the value are defined
  filter(complete.cases(.))

# Forward Stepwise Selection

#define intercept-only model
intercept_only <- lm(test,formula = delta_age ~ 1)

#define model with all predictors
all <- lm(test,formula = delta_age~.)

#perform forward stepwise regression
forward <- step(intercept_only, direction='forward', scope=formula(all), trace=0)

#view results of forward stepwise regression
# forward$anova
summary(forward)

