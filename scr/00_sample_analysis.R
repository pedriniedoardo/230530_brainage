# libraries ---------------------------------------------------------------
library(tidyverse)
library(contrast)
library(GGally)
library(patchwork)

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
  mutate(sex = factor(sex)) %>%
  mutate(LL_mL = LL/1000)
  
# are there missing points
df_full %>%
  filter(is.na(delta_age))

# see the distribuiotn of the grouping 
df_full %>%
  group_by(PRL_cat) %>%
  summarise(n = n())

df_full %>%
  group_by(age_cat) %>%
  summarise(n = n())
#
table(df_full$PRL,df_full$PRL_cat)
table(df_full$age_cat,df_full$PRL_cat)
table(df_full$sex)
table(df_full$PRL_cat)

# EDA ---------------------------------------------------------------------
# distribution of the delta_age by PRL_cat
df_full %>%
  ggplot(aes(y=delta_age,x=PRL_cat)) + 
  # geom_violin() + 
  # geom_boxplot(width = 0.1,outlier.shape = NA) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2,alpha=0.1) + theme_bw()
ggsave("../../out/R_analysis/image/boxplot_PRL_deltaAge_full.pdf",width = 4,height = 4)

# is there any batch efect for the center variable?
df_full %>%
  ggplot(aes(y=delta_age,x=PRL_cat,fill=center)) + geom_boxplot() + theme_bw()
# maybe a small batch effect on the NIH data in L_0 I can boc for center in the linear model
df_full %>%
  ggplot(aes(y=delta_age,x=PRL_cat,fill=sex)) + geom_boxplot() + theme_bw()+scale_fill_manual(values = c("pink","blue"))

# -------------------------------------------------------------------------
# stat for the comparison using as ref the L_0 category
# model_01 <- lm(df_full,formula = delta_age~PRL_cat)
model_01 <- lm(df_full,formula = delta_age~PRL_cat)
summary(model_01)

# -------------------------------------------------------------------------
# model blocking for center
model_02 <- lm(df_full,formula = delta_age~center+PRL_cat)
summary(model_02)

# pull the specific contrast of interest
#
id_center <- unique(df_full$center)
id_comparision <- list(L_4.L_1_3 = c("L_4","L_1_3"),
                    L_4.L_0 = c("L_4","L_0"),
                    L_1_3.L_0 = c("L_1_3","L_0"))

df_model_02 <- crossing(center = id_center,comp_vec = id_comparision) %>% 
  pmap(function(center,comp_vec){
    test1 <- contrast(model_02,
                      list(PRL_cat=comp_vec[1],center = center),
                      list(PRL_cat=comp_vec[2],center = center))
    
    test10 <- data.frame(
      Comparison=paste0(comp_vec[1],".",comp_vec[2]),
      Contrast=test1$Contrast,
      SE=test1$SE,
      Lower=test1$Lower,
      Upper=test1$Upper,
      testStat=test1$testStat,
      df=test1$df,
      Pvalue=test1$Pvalue,
      id_center=center
    )
    return(test10)
  }) %>% 
  bind_rows()

# -------------------------------------------------------------------------
# model blocking for center with interaction between center and category
model_03 <- lm(df_full,formula = delta_age~center*PRL_cat)
summary(model_03)

# id_center <- unique(df_full$center)
# id_comparision <- list(L_4.L_1_3 = c("L_4","L_1_3"),
#                        L_4.L_0 = c("L_4","L_0"),
#                        L_1_3.L_0 = c("L_1_3","L_0"))


df_model_03 <- crossing(center = id_center,comp_vec = id_comparision) %>% 
  pmap(function(center,comp_vec){
    test1 <- contrast(model_03,
                      list(PRL_cat=comp_vec[1],center = center),
                      list(PRL_cat=comp_vec[2],center = center))
    
    test10 <- data.frame(
      Comparison=paste0(comp_vec[1],".",comp_vec[2]),
      Contrast=test1$Contrast,
      SE=test1$SE,
      Lower=test1$Lower,
      Upper=test1$Upper,
      testStat=test1$testStat,
      df=test1$df,
      Pvalue=test1$Pvalue,
      id_center=center
    )
    return(test10)
  }) %>% 
  bind_rows()

# contrast(model_03,list(PRL_cat="L_4",center = "Bruxelles"),list(PRL_cat="L_1_3",center = "Bruxelles"))
# contrast(model_03,list(PRL_cat="L_4",center = "NIH"),list(PRL_cat="L_1_3",center = "NIH"))
# contrast(model_03,list(PRL_cat="L_4",center = "JHU"),list(PRL_cat="L_1_3",center = "JHU"))
# 
# contrast(model_03,list(PRL_cat="L_4",center = "Bruxelles"),list(PRL_cat="L_0",center = "Bruxelles"))
# contrast(model_03,list(PRL_cat="L_4",center = "NIH"),list(PRL_cat="L_0",center = "NIH"))
# contrast(model_03,list(PRL_cat="L_4",center = "JHU"),list(PRL_cat="L_0",center = "JHU"))
# 
# contrast(model_03,list(PRL_cat="L_1_3",center = "Bruxelles"),list(PRL_cat="L_0",center = "Bruxelles"))

# -------------------------------------------------------------------------
# explore correlation between the delta age and chronological age cat per PRL_cat
df_full %>%
  ggplot(aes(y=delta_age,x=PRL_cat,fill = age_cat)) + 
  # geom_violin()+
  # geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9),width=0.1)+
  geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9))+
  geom_point(position=position_jitterdodge(dodge.width=0.9,jitter.width = 0.4),alpha=0.2) +
  theme_bw()
ggsave("../../out/R_analysis/image/boxplot_PRL_AgeCat_deltaAge_full.pdf",width = 5,height = 4)

# count the numerosity per group type
df_full %>%
  group_by(PRL_cat,age_cat) %>%
  summarise(n = n())

# stat
model_04 <- lm(df_full,formula = delta_age~center+PRL_cat*age_cat)
summary(model_04)

# id_center <- unique(df_full$center)
# id_comparision_treat <- list(L_4.L_1_3 = c("L_4","L_1_3"),
#                              L_4.L_0 = c("L_4","L_0"),
#                              L_1_3.L_0 = c("L_1_3","L_0"))
# id_comparision_age <- list(young_old = c("young","old"),
#                            old_young = c("old","young"),
#                            young_young = c("young","young"),
#                            old_old = c("old","old"))
id_treat <- c("L_4","L_1_3","L_0")
id_age <- c("young","old")

df_model_04 <- crossing(center = id_center,ref_treat = id_treat,alt_treat = id_treat,ref_age = id_age,alt_age=id_age) %>% 
  filter(!(ref_treat==alt_treat&ref_age==alt_age)) %>% 
  filter(!(ref_treat==alt_treat&ref_age=="old"&alt_age=="young")) %>% 
  pmap(function(center,ref_treat,alt_treat,ref_age,alt_age){
    test1 <- contrast(model_04,
                      list(PRL_cat=alt_treat,age_cat = alt_age,center = center),
                      list(PRL_cat=ref_treat,age_cat = ref_age,center = center))
    
    test10 <- data.frame(
      Comparison_treat=paste0(alt_treat,".",ref_treat),
      Comparison_age=paste0(alt_age,".",ref_age),
      Contrast=test1$Contrast,
      SE=test1$SE,
      Lower=test1$Lower,
      Upper=test1$Upper,
      testStat=test1$testStat,
      df=test1$df,
      Pvalue=test1$Pvalue,
      id_center=center
    )
    return(test10)
  }) %>% 
  bind_rows()

df_model_04 %>% 
  filter(id_center=="Bruxelles") %>% 
  arrange(Pvalue)

# pull the contrast of interest
# contrast(model_04,list(PRL_cat="L_4",age_cat = "old",center = "Bruxelles"),list(PRL_cat="L_4",age_cat = "young",center = "Bruxelles"))
# contrast(model_04,list(PRL_cat="L_1_3",age_cat = "old",center = "Bruxelles"),list(PRL_cat="L_1_3",age_cat = "young",center = "Bruxelles"))
# contrast(model_04,list(PRL_cat="L_0",age_cat = "old",center = "Bruxelles"),list(PRL_cat="L_0",age_cat = "young",center = "Bruxelles"))
# 
# contrast(model_04,list(PRL_cat="L_4",age_cat = "old",center = "Bruxelles"),list(PRL_cat="L_0",age_cat = "old",center = "Bruxelles"))
# contrast(model_04,list(PRL_cat="L_4",age_cat = "young",center = "Bruxelles"),list(PRL_cat="L_0",age_cat = "young",center = "Bruxelles"))

# -------------------------------------------------------------------------
# explore the delta age versus chronological age
df_full %>%
  ggplot(aes(y=delta_age,x=chronological_age,col=PRL_cat,group=PRL_cat)) + 
  geom_point(alpha=0.5) + theme_bw() + 
  geom_smooth(method = "lm")
ggsave("../../out/R_analysis/image/scatter_PRL_AgeCat_deltaAge_full.pdf",width = 5,height = 4)

df_full %>%
  ggplot(aes(y=delta_age,x=chronological_age,col=PRL_cat,group=PRL_cat)) + 
  geom_point(alpha=0.5) + theme_bw() + 
  geom_smooth(method = "lm") + facet_wrap(~PRL_cat)+theme(strip.background = element_blank())
ggsave("../../out/R_analysis/image/scatter_PRL_AgeCat_deltaAge_split_full.pdf",width = 6,height = 4)
# there seem to be some correlation between chronologicla and and delta age splitted by PRL_cat

# try to measure the correlation between the variables
list_cor <- df_full %>%
  split(f = .$PRL_cat) %>%
  lapply(function(x){
    cor.test(x$chronological_age, x$delta_age, method = "pearson")
  })

# try the same with linear models
list_cor2 <- df_full %>%
  split(f = .$PRL_cat) %>%
  lapply(function(x){
    mod <- lm(x,formula = delta_age~chronological_age)
  })

list_cor2 %>%
  lapply(function(x){
    summary(x)
  })

# -------------------------------------------------------------------------
# explore more varaible in the dataset
# are the delta age and phenotype correlated?
df_full %>%
  ggplot(aes(y=delta_age,x=PRL_cat,fill = phenotype)) + 
  geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9))+
  geom_point(position=position_jitterdodge(dodge.width=0.9,jitter.width = 0.4),alpha=0.2) +
  theme_bw()

# model and stat
# stat
model_05 <- lm(df_full,formula = delta_age~center + PRL_cat*phenotype)
summary(model_05)
# pull the contrast of interest
contrast(model_05,list(PRL_cat="L_0",phenotype = "2",center = "Bruxelles"),list(PRL_cat="L_0",phenotype = "1",center = "Bruxelles"))

# test the gender
df_full %>%
  ggplot(aes(y=delta_age,x=PRL_cat,fill = sex)) + 
  facet_wrap(~center)+
  geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9))+
  geom_point(position=position_jitterdodge(dodge.width=0.9,jitter.width = 0.4),alpha=0.2) +
  theme_bw()+
  theme(strip.background = element_blank())

# model and stat
# stat
model_06_1 <- lm(df_full,formula = delta_age~center + sex +PRL_cat)
summary(model_06_1)

model_06_2 <- lm(df_full,formula = delta_age~center * sex +PRL_cat)
summary(model_06_2)

model_06 <- lm(df_full,formula = delta_age~center+sex+PRL_cat*age_cat)
summary(model_06)

p1 <- data.frame(residual = model_06$residuals,index = 1:length(model_06$residuals)) %>% 
  ggplot(aes(x=index,y=residual))+geom_point()+theme_bw()+geom_hline(yintercept = 0,linetype="dashed",col="red")
p2 <- data.frame(residual = model_06$residuals,index = 1:length(model_06$residuals)) %>% 
  ggplot(aes(x=residual))+geom_histogram(bins = 20)+theme_bw()

p1+p2

# pull the contrast of interest
contrast(model_06_1,list(PRL_cat="L_1_3",sex = "0",center = "Bruxelles"),list(PRL_cat="L_1_3",sex = "1",center = "Bruxelles"))

# martina asked to try to block for LL and see the effect on the model
# stat
model_07 <- lm(df_full,formula = delta_age~center+LL+PRL_cat*age_cat)
summary(model_07)

# express the lesion load as mL rather than mm^3
model_07_2 <- lm(df_full,formula = delta_age~center+LL_mL+PRL_cat*age_cat)
summary(model_07_2)

model_07_gender <- lm(df_full,formula = delta_age~center+sex+LL+PRL_cat*age_cat)
summary(model_07_gender)

# model_08 <- lm(df_full,formula = delta_age~center+LL+chronological_age+PRL_cat)
# summary(model_08)
model_08 <- lm(df_full,formula = delta_age~center+sex+chronological_age*PRL_cat)
summary(model_08)

# comapre it with the model without LL
# model_04 <- lm(df_full,formula = delta_age~center+PRL_cat*age_cat)
summary(model_04)

df_full %>%
  ggplot(aes(y=LL,x=age_cat,fill = age_cat)) + 
  # geom_violin()+
  # geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9),width=0.1)+
  geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9))+
  geom_point(position=position_jitterdodge(dodge.width=0.9,jitter.width = 0.4),alpha=0.2) +
  theme_bw()

df_full %>%
  ggplot(aes(y=LL,x=PRL_cat,fill = age_cat)) + 
  # geom_violin()+
  # geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9),width=0.1)+
  geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9))+
  geom_point(position=position_jitterdodge(dodge.width=0.9,jitter.width = 0.4),alpha=0.2) +
  theme_bw()
ggsave("../../out/R_analysis/image/boxplot_PRL_AgeCat_LL_full.pdf",width = 5,height = 4)

df_full %>%
  ggplot(aes(y=delta_age,x=LL)) + 
  geom_point(alpha=0.5) + theme_bw() + 
  geom_smooth(method = "lm")
ggsave("../../out/R_analysis/image/scatter_LL_DeltaAge.pdf",width = 5,height = 4)

df_full %>%
  ggplot(aes(y=delta_age,x=LL,col=PRL_cat,group=PRL_cat)) + 
  geom_point(alpha=0.5) + theme_bw() + 
  geom_smooth(method = "lm")
ggsave("../../out/R_analysis/image/scatter_LL_DeltaAge_PRL.pdf",width = 5,height = 4)

df_full %>%
  ggplot(aes(y=delta_age,x=LL,col=PRL_cat,group=PRL_cat)) + 
  geom_point(alpha=0.5) + theme_bw() + 
  geom_smooth(method = "lm")+facet_wrap(~PRL_cat)+theme(strip.background = element_blank())
ggsave("../../out/R_analysis/image/scatter_LL_DeltaAge_PRL_split.pdf",width = 10,height = 4)

# df_full %>%
#   ggplot(aes(y=delta_age,x=LL,col=age_cat,group=age_cat)) + 
#   geom_point(alpha=0.5) + theme_bw() + 
#   geom_smooth(method = "lm")
# 
# df_full %>%
#   ggplot(aes(y=delta_age,x=LL,col=PRL_cat,group=PRL_cat)) + 
#   geom_point(alpha=0.5) + theme_bw() + 
#   geom_smooth(method = "lm")+facet_wrap(~PRL_cat)+theme(strip.background = element_blank())
# 
# df_full %>%
#   ggplot(aes(y=LL,x=delta_age,col = age_cat)) + geom_point()
#   # geom_violin()+
#   # geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9),width=0.1)+
#   geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9))+
#   geom_point(position=position_jitterdodge(dodge.width=0.9,jitter.width = 0.4),alpha=0.2) +
#   theme_bw()

# pull the contrast of interest
contrast(model_05,list(PRL_cat="L_0",phenotype = "2",center = "Bruxelles"),list(PRL_cat="L_0",phenotype = "1",center = "Bruxelles"))

# try to categorize LL to explain better the effect
# martina asked to try to block for LL and see the effect on the model
# stat
df_full_test <- df_full %>% 
  mutate(LL_class = case_when(LL>median(LL,na.rm = T)~"high",
                              T~"low")) %>% 
  mutate(LL_class = factor(LL_class,levels = c("low","high")))
model_09 <- lm(df_full_test,formula = delta_age~center+sex+LL_class+PRL_cat*age_cat)
summary(model_09)

df_full_test %>%
  ggplot(aes(y=delta_age,x=PRL_cat,fill = age_cat)) + 
  # geom_violin()+
  # geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9),width=0.1)+
  geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9))+
  geom_point(position=position_jitterdodge(dodge.width=0.9,jitter.width = 0.4),alpha=0.2) +
  theme_bw()+facet_wrap(~LL_class)+theme(strip.background = element_blank())
ggsave("../../out/R_analysis/image/boxplot_PRL_AgeCat_LL_full_split.pdf",width = 8,height = 4)

df_full_test %>% 
  summarise(med = median(LL,na.rm = T))

df_full_test %>%
  ggplot(aes(x=LL)) + geom_histogram() +geom_vline(aes(xintercept = median(LL,na.rm = T)),col="red",linetype = "dashed")
  # # geom_violin()+
  # # geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9),width=0.1)+
  # geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9))+
  # geom_point(position=position_jitterdodge(dodge.width=0.9,jitter.width = 0.4),alpha=0.2) +
  # theme_bw()+facet_wrap(~LL_class)+theme(strip.background = element_blank())

# -------------------------------------------------------------------------
# try to correlate all the variables in the dataset
df_full %>%
  dplyr::select(-c(id_sample,PRL,sex,center)) %>%
  ggpairs(ggplot2::aes(colour=PRL_cat,alpha=0.6))+ theme_bw()+theme(strip.background = element_blank())
ggsave("../../out/R_analysis/image/corr_plot_full.pdf",width = 25,height = 25)
