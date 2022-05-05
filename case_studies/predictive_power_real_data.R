####################################################################################################
###########     Practical Methods for Measuring Algorithmic Fairness with Proxy Data      ##########
####################################################################################################

source("load_libraries_utilities.R")

## NOTE: Code is not fully reproducible as data is the property of UCI and is not hosted 
## on this repo

#########################################     Warfarin     #########################################

## Adult data (adult) comes from: https://archive.ics.uci.edu/ml/datasets/adult
## Adult occupation map (adult.map) comes from: https://www2.census.gov/programs-surveys/cps/techdocs/cpsmar94.pdf

adult <- left_join(adult, adult.map)

## Calculate demographic disparity (DD)
high.income.rate.female <- mean((adult %>% filter(sex =="Female"))$Income_Over_50k)
high.income.rate.male <- mean((adult %>% filter(sex =="Male"))$Income_Over_50k)
dd <- high.income.rate.female - high.income.rate.male

## Repeat check 100 times
sex.predictive.accuracies <- c()
est.dds <-c()
for (i in 1:100) {
  
  ## Randomly assign individuals to be male or female based on their probability of being female
  adult$Female_Probability <- runif(nrow(adult))
  adult <- adult %>%
    mutate(Predicted_Sex = as.factor(
      ifelse(Female_Probability < occupation.share.female, "Female", "Male")))
  
  ## Calculate DD
  est.high.income.rate.female <- mean((adult %>% filter(Predicted_Sex =="Female"))$Income_Over_50k)
  est.high.income.rate.male <- mean((adult %>% filter(Predicted_Sex =="Male"))$Income_Over_50k)
  est.dd <- est.high.income.rate.female - est.high.income.rate.male
  est.dds <- c(est.dd, est.dds)
  
  ## Calculate predictive accuracy of occupation proxy for sex
  conf <- confusionMatrix(data=adult$Predicted_Sex, reference=adult$sex)
  sex.predictive.accuracy <- conf$overall[which(names(conf$overall) == "Accuracy")]
  sex.predictive.accuracies <- c(sex.predictive.accuracy, sex.predictive.accuracies)
  
}
