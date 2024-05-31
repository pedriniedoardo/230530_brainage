# libraries ---------------------------------------------------------------
library(randomForest)
library(tidyverse)

# read in the data --------------------------------------------------------
set.seed(4543)
data(mtcars)

# build the model ---------------------------------------------------------
rf.fit <- randomForest(mpg ~ ., data=mtcars, ntree=1000,
                       keep.forest=FALSE, importance=TRUE)

# I will specifically focus on understanding the performance and variable importance. So after we run the piece of code above, we can check out the results by simply running rf.fit.
rf.fit

# Notice that the function ran random forest regression, and we didn’t need to specify that. It will perform nonlinear multiple regression as long as the target variable is numeric (in this example, it is Miles per Gallon - mpg). But, if it makes you feel better, you can add type= “regression”.

# The mean of squared residuals and % variance explained indicate how well the model fits the data. Residuals are a difference between prediction and the actual value. In our example, 5.6 means that we were wrong by 5.6 miles/gallon on average.

# You can experiment with, i.e. increase or decrease, the number of trees (ntree) or the number of variables tried at each split (mtry) and see whether the residuals or % variance change.

# If you also want to understand what the model has learnt, make sure that you do importance = TRUE as in the code above.

# Random forest regression in R provides two outputs: decrease in mean square error (MSE) and node purity. Prediction error described as MSE is based on permuting out-of-bag sections of the data per individual tree and predictor, and the errors are then averaged. In the regression context, Node purity is the total decrease in residual sum of squares when splitting on a variable averaged over all trees (i.e. how well a predictor decreases variance). MSE is a more reliable measure of variable importance. If the two importance metrics show different results, listen to MSE. If all of your predictors are numerical, then it shouldn’t be too much of an issue - read more here.

# The built-in varImpPlot() will visualize the results, but we can do better. Here, we combine both importance measures into one plot emphasizing MSE results.

### Visualize variable importance ----------------------------------------------

# Get variable importance from the model fit
ImpData <- as.data.frame(importance(rf.fit))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

# Conclusion
# In terms of assessment, it always comes down to some theory or logic behind the data. Do the top predictors make sense? If not, investigate why.
# Modeling is an iterative process. You can get a better idea about the predictive error of your random forest regression when you save some data for performance testing only. You might also want to try out other methods.

# test on brainage dataset ------------------------------------------------
df <- read_csv("../../data/brain_age_PRL_05262023.csv")
cut_off <- 55
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

df_full2 <- df_full %>% 
  mutate(LL_cat = case_when(LL<median(LL,na.rm = T)~"low",
                            T~"high")) %>% 
  mutate(LL_cat = factor(LL_cat,levels = c("low","high")))

test <- df_full2 %>% 
  select(center,sex,phenotype,EDSS,disease_duration,Brain_volume,gARMSS,MSSS,delta_age,PRL_cat,age_cat,LL_cat) %>% 
  # make sure all the value are defined
  filter(complete.cases(.))

rf.fit2 <- randomForest(delta_age ~ ., data=test, ntree=1000,
                       keep.forest=FALSE, importance=TRUE)

rf.fit2

# Get variable importance from the model fit
ImpData2 <- as.data.frame(importance(rf.fit2))
ImpData2$Var.Names <- row.names(ImpData2)

ImpData2 %>%
  ggplot(aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )
