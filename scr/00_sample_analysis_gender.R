# # libraries ---------------------------------------------------------------
# library(tidyverse)
# library(contrast)
# library(GGally)
# 
# # read in the data --------------------------------------------------------
# df <- read_csv("../../data/brain_age_PRL.csv")
# 
# # wrangling ---------------------------------------------------------------
# # try to apply a binary separation of the dataset based on the median age
# # cut_off <- median(df_full$chronological_age)
# # use and arbitrary cut off to divide young and old patient.
# cut_off <- 55
# 
# df_full <- df %>%
#   # filter only the data where there is also the gender specification
#   filter(!is.na(sex)) %>%
#   mutate(sex = factor(sex)) %>%
#   # add the difference between the estiamted and the actual brain age
#   mutate(delta_age = BrainageR - chronological_age) %>%
#   # add the category of the PRL
#   mutate(PRL_cat = case_when(PRL == 0 ~ "L_0",
#                              PRL <= 3 ~ "L_1_3",
#                              T ~ "L_4")) %>%
#   # add to the dataset the variable for the age cut off
#   mutate(age_cat = case_when(chronological_age<=cut_off~"young",
#                              T~"old")) %>%
#   mutate(age_cat = factor(age_cat,levels = c("young","old"))) %>%
#   # make the phenotype variable as category
#   mutate(phenotype = factor(phenotype))
# 
# # are there missing points
# df_full %>%
#   filter(is.na(delta_age))
# 
# # see the distribuiotn of the grouping 
# df_full %>%
#   group_by(PRL_cat) %>%
#   summarise(n = n())
# #
# table(df_full$PRL,df_full$PRL_cat)
# 
# # EDA ---------------------------------------------------------------------
# # distribution of the delta_age by PRL_cat
# df_full %>%
#   ggplot(aes(y=delta_age,x=PRL_cat)) + 
#   # geom_violin() + 
#   # geom_boxplot(width = 0.1,outlier.shape = NA) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_jitter(width = 0.2,alpha=0.1) + theme_bw()
# # ggsave("images/boxplot_PRL_deltaAge.pdf",width = 4,height = 4)
# 
# # is there any batch efect for the center variable?
# df_full %>%
#   ggplot(aes(y=delta_age,x=PRL_cat,fill=center)) + geom_boxplot() + theme_bw()
# # maybe a small batch effect on the NIH data in L_0 I can boc for center in the linear model
# 
# # is there any batch efect for the gender variable?
# df_full %>%
#   ggplot(aes(y=delta_age,x=PRL_cat,fill=sex)) + geom_boxplot() + theme_bw()
# # there doesn't seems to be any bias for the gender in terms of delta age per number of lesions
# 
# # stat for the comparison using as ref the L_0 category
# # model_01 <- lm(df_full,formula = delta_age~PRL_cat)
# model_01 <- lm(df_full,formula = delta_age~sex + center + PRL_cat)
# summary(model_01)
# 
# # df_full %>%
# #   ggplot(aes(y=delta_age,x=center,fill=sex)) + geom_boxplot() + theme_bw() + facet_wrap(~PRL_cat)
# 
# # pull the specific contrast of interest
# # 
# contrast(model_01,list(PRL_cat = "L_4",sex = "1",center = "JHU"),list(PRL_cat="L_0",sex = "1",center = "JHU"))
# # contrast(model_01,list(PRL_cat = "L_4",sex = "0",center = "JHU"),list(PRL_cat="L_0",sex = "0",center = "JHU"))
# contrast(model_01,list(PRL_cat = "L_1_3",sex = "1",center = "JHU"),list(PRL_cat="L_0",sex = "1",center = "JHU"))
# contrast(model_01,list(PRL_cat = "L_4",sex = "1",center = "JHU"),list(PRL_cat="L_1_3",sex = "1",center = "JHU"))
# # contrast(model_01,list(PRL_cat="L_4",center = "JHU"),list(PRL_cat="L_0",center = "JHU"))
# # contrast(model_01,list(PRL_cat="L_4",center = "NIH"),list(PRL_cat="L_0",center = "NIH"))
# 
# # -------------------------------------------------------------------------
# # explore correlation between the delta age and chronological age cat per PRL_cat
# df_full %>%
#   ggplot(aes(y=delta_age,x=sex,fill = age_cat)) + 
#   # geom_violin()+
#   # geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9),width=0.1)+
#   geom_boxplot(outlier.shape = NA,position = position_dodge(width=0.9))+
#   geom_point(position=position_jitterdodge(dodge.width=0.9,jitter.width = 0.4),alpha=0.2) +
#   theme_bw() + facet_wrap(~PRL_cat)
# # ggsave("images/boxplot_PRL_AgeCat_deltaAge.pdf",width = 5,height = 4)
# 
# # count the numerosity per group type
# df_full %>%
#   group_by(PRL_cat,age_cat,sex) %>%
#   summarise(n = n())
# 
# # stat
# model_02 <- lm(df_full,formula = delta_age~sex + center + PRL_cat * age_cat)
# summary(model_02)
# # pull the contrast of interest
# contrast(model_02,list(PRL_cat = "L_4",sex = "1",center = "JHU",age_cat = "old"),list(PRL_cat="L_4",sex = "1",center = "JHU",age_cat = "young"))
# # contrast(model_02,list(PRL_cat = "L_4",sex = "0",center = "JHU",age_cat = "old"),list(PRL_cat="L_4",sex = "0",center = "JHU",age_cat = "young"))
# 
# contrast(model_02,list(PRL_cat = "L_1_3",sex = "0",center = "JHU",age_cat = "old"),list(PRL_cat="L_1_3",sex = "0",center = "JHU",age_cat = "young"))
# 
# contrast(model_02,list(PRL_cat = "L_0",sex = "0",center = "JHU",age_cat = "old"),list(PRL_cat="L_0",sex = "0",center = "JHU",age_cat = "young"))
# 
# # -------------------------------------------------------------------------
# # try to correlate all teh varibles in the dataset
# df_full %>%
#   dplyr::select(-c(PRL,center)) %>%
#   ggpairs(ggplot2::aes(colour=PRL_cat,alpha=0.6))+ theme_bw()
