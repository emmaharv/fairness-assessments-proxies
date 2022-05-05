####################################################################################################
###########     Practical Methods for Measuring Algorithmic Fairness with Proxy Data      ##########
####################################################################################################

source("load_libraries_utilities.R")

## NOTE: Code is not fully reproducible as data is the property of UCI, PharmGKB, and ForeverData
## and is not hosted on this repo

#########################################     Warfarin     #########################################

## Warfarin data (warfarin) comes from: https://www.pharmgkb.org/downloads
## Warfarin gene map comes from: https://pubmed.ncbi.nlm.nih.gov/20504253/
## Warfarin population split comes from: https://pubmed.ncbi.nlm.nih.gov/28672100/

## Generate outcome variable (high dose) and uses CYP2C9 gene as a high-accuracy proxy for being 
## white vs. Black. Adjust high dose rate for CYP2C9 because CYP2C9 gene is directly associated with
## a lower dose, regardless of race. 
warfarin <- warfarin %>% 
  filter(Race_OMB %in% c("White", "Black or African American")) %>%
  mutate(High_Dose = ifelse(CYP2C9_consensus == "*2/*2", Therapeutic_Dose_of_Warfarin > 35 - 8.7, 
                            Therapeutic_Dose_of_Warfarin > 35),
         Predicted_Race = ifelse(CYP2C9_consensus == "*2/*2", "White", "Black")) %>%
  filter(!is.na(High_Dose) & !is.na(Predicted_Race))

## Get true demographic disparity (DD) between high dose rates of white and Black patients 
true.result <- spread(warfarin %>% group_by(Race_OMB) %>% summarise(outcome = mean(High_Dose)), 
                      key=Race_OMB, value=outcome) %>%
  mutate(true.dd = White - `Black or African American`) %>%
  select(White, `Black or African American`, true.dd)

## Estimate the rate of high-dose white patients based on the proxy
est.result <- spread(warfarin %>% group_by(Predicted_Race) %>% summarise(outcome = mean(High_Dose)), 
                     key=Predicted_Race, value=outcome) 
est.high.dose.rate.white <- est.result$White

## Calculate the estimated DD at different assumed shares of the population that is Black
est.pop.share.black <- seq(0, 1, 0.01)
out <- data.frame(
  est.pop.share.black = est.pop.share.black, 
  est.high.dose.rate.white = est.high.dose.rate.white, 
  est.high.dose.rate.black = 
    ((mean(warfarin$High_Dose)) - (est.high.dose.rate.white * (1 - est.pop.share.black))) / (est.pop.share.black))

out <- out %>% 
  mutate(est.high.dose.rate.black = ifelse(est.high.dose.rate.black < 0 | est.high.dose.rate.black > 1, NA, est.high.dose.rate.black), 
         est.dd = est.high.dose.rate.white - est.high.dose.rate.black)

#########################################     Heritage     #########################################

## Heritage data (heritage) comes from: https://foreverdata.org/1015/index.html

## Generate outcome variable in the full dataset
heritage.full <- heritage %>% 
  group_by(MemberID, sex, AgeAtFirstClaim) %>%
  summarise(Hospitalized = ifelse(sum(DaysInHospital_Y2) > 0, 1, 0)) %>% 
  ungroup()

## Filter the dataset to remove correlation between OBGYN visits and hospitalizations. Remove 
## individuals who are not of the age to go to the OBGYN regularly and individuals who are 
## pregnant (their OBGYN visits are direct predictors of a future hospital visit)
heritage.filtered <- heritage %>% 
  group_by(MemberID, sex, AgeAtFirstClaim) %>%
  summarise(
    Predicted_Sex = as.factor(ifelse(any(specialty %in% c("Obstetrics and Gynecology")), "F", "M")), 
    Is_Pregnant = ifelse(any(Description %in% c("Pregnancy")), T, F), 
    Hospitalized = ifelse(sum(DaysInHospital_Y2) > 0, 1, 0)) %>% 
  ungroup() %>%
  filter(AgeAtFirstClaim %in% c("20-29", "30-39", "40-49", "50-59", "60-69")) %>%
  filter(!Is_Pregnant)

## Get true DD between hospitalization rates of males and females in the unfiltered dataset
true.result <- spread(heritage.full %>% group_by(sex) %>% summarise(outcome = mean(Hospitalized)), 
                      key=sex, value=outcome) %>%
  mutate(true.dd = `F` - `M`) %>%
  select(`F`, `M`, true.dd)

## Get true DD between hospitalization rates of males and females in the filtered dataset
filtered.true.result <- spread(heritage.filtered %>% group_by(sex) %>% summarise(outcome = mean(Hospitalized)), 
                               key=sex, value=outcome) %>%
  mutate(true.dd = `F` - `M`) %>%
  select(`F`, `M`, true.dd)

## Get the ratio of the unfiltered to filtered DDs to adjust the estimated DD with later 
## (the estimated DD will initially be of the same population as the filtered DD, but we want to 
## convert to unfiltered)
ratio <- mean(heritage.full$Hospitalized) / mean(heritage.filtered$Hospitalized)

## Estimate the rate of high-dose white patients based on the proxy
est.result <- spread(heritage.filtered %>% group_by(Predicted_Sex) %>% summarise(outcome = mean(Hospitalized)), 
                     key=Predicted_Sex, value=outcome)

## Adjust to apply to the full, vs. the filtered, dataset
est.hosp.rate.female <- est.result$`F` * ratio

## Calculate the estimated DD at different assumed shares of the population that is male
est.pop.share.male = seq(0, 1, 0.01)
out <- data.frame(
  est.pop.share.male = est.pop.share.male, 
  est.hosp.rate.female = est.hosp.rate.female, 
  est.hosp.rate.male = 
    ((mean(heritage.full$Hospitalized)) - (est.hosp.rate.female * (1 - est.pop.share.male))) / (est.pop.share.male))

out <- out %>% 
  mutate(est.hosp.rate.male = ifelse(est.hosp.rate.male < 0 | est.hosp.rate.male > 1, NA, est.hosp.rate.male), 
         est.dd = est.hosp.rate.female - est.hosp.rate.male)


########################################     Marketing     #########################################

## Marketing data (marketing) comes from: https://archive.ics.uci.edu/ml/datasets/bank+marketing
## Marketing population split comes from: http://data.un.org/Data.aspx

## Generate outcome variable in the full dataset. Use whether an individual is retired as a proxy
## for whether they are elderly. 
marketing <- marketing %>%
  mutate(Invests = ifelse(y == "yes", 1, 0), 
         Predicted_Age = ifelse(job == "retired", "elderly", "nonelderly"), 
         Actual_Age = ifelse(age >= 65, "elderly", "nonelderly"))

## Get true demographic disparity (DD) between investment rates of elderly and nonelderly customers
true.result <- spread(marketing %>% group_by(Actual_Age) %>% summarise(outcome = mean(Invests)), 
                      key=Actual_Age, value=outcome) %>%
  mutate(true.dd = elderly - nonelderly) %>%
  select(elderly, nonelderly, true.dd)

## Estimate the rate of elderly customers who invest based on the proxy
est.result <- spread(marketing %>% group_by(Predicted_Age) %>% summarise(outcome = mean(Invests)), 
                     key=Predicted_Age, value=outcome)
est.investment.rate.elderly <- est.result$elderly

## Calculate the estimated DD at different assumed shares of the population that is nonelderly
est.pop.share.nonelderly = seq(0, 1, 0.01)
out <- data.frame(
  est.pop.share.nonelderly = est.pop.share.nonelderly, 
  est.investment.rate.elderly = est.investment.rate.elderly, 
  est.investment.rate.nonelderly = 
    ((mean(marketing$Invests)) - (est.investment.rate.elderly * (1 - est.pop.share.nonelderly))) / (est.pop.share.nonelderly))

out <- out %>% 
  mutate(est.investment.rate.nonelderly = ifelse(est.investment.rate.nonelderly < 0 | est.investment.rate.nonelderly > 1, NA, est.investment.rate.nonelderly), 
         est.dd = est.investment.rate.elderly - est.investment.rate.nonelderly)
