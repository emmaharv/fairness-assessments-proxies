####################################################################################################
###########     Practical Methods for Measuring Algorithmic Fairness with Proxy Data      ##########
####################################################################################################

source("load_libraries_utilities.R")

## NOTE: Code is not fully reproducible as data is the property of PharmGKB and is not hosted 
## on this repo

#########################################     Warfarin     #########################################

## Warfarin data (warfarin) comes from: https://www.pharmgkb.org/downloads
## Warfarin gene map (warfarin.map) comes from: https://pubmed.ncbi.nlm.nih.gov/20504253/

## Create outcome variable
warfarin <- warfarin %>% 
  mutate(High_Dose = ifelse(Therapeutic_Dose_of_Warfarin > 35, 1, 0))

## Restrict pool to only look at individuals with a white and Asian indidiauls with a sequenced 
## VKORC1 gene
warfarin <- warfarin %>% 
  filter(!is.na(VKORC1_1639_consensus)) %>%
  filter(Race_OMB %in% c("White", "Asian") & 
           (Ethnicity_OMB == "Not Hispanic or Latino" | Ethnicity_OMB == "not Hispanic or Latino"))

## Calculate share of white patients with high dose
high.dose.rate.white <- nrow(warfarin %>% filter(High_Dose == 1 & Race_OMB == "White")) / 
  nrow(warfarin %>% filter(Race_OMB == "White"))

## Calculate share of Asian patients with high dose
high.dose.rate.asian <- nrow(warfarin %>% filter(High_Dose == 1 & Race_OMB != "White")) / 
  nrow(warfarin %>% filter(Race_OMB != "White"))

## Calculate demographic disparity (DD)
dd <- high.dose.rate.white - high.dose.rate.asian

## Measure accuracy of estimated DD at different assumed proportions of Asian individuals in the 
## population
dd.delta <- data.frame()
for (est.pop.share.white in seq(0, 1, 0.01)) {
  
  print(est.pop.share.white)
  
  ## Use the estimated % of the population that is white to construct a single-variable proxy table
  ## from the provided demographic summary proxy table
  priors <- data.frame("Prior"=c(0 + est.pop.share.white, 1 - est.pop.share.white), 
                       "Race"= c("White", "Asian"))
  
  conditionals <- left_join(warfarin.map, priors, by="Race")
  conditionals <- conditionals %>% 
    group_by(Genotype) %>%
    mutate(Weighted_Avg = weighted.mean(Proportion, Prior)) %>%
    ungroup() %>%
    mutate(Conditional = (Prior * Proportion) / Weighted_Avg)
  conditionals <- conditionals %>% filter(Race == "White")
  
  dd.accuracy.check <- warfarin %>% left_join(conditionals, by=c("VKORC1_1639_consensus"="Genotype"))
  
  ## Repeat each check 10 times
  for (i in 1:10) {
    
    ## Randomly assign individuals to be white or Asian based on their probability of being white
    dd.accuracy.check$White_Probability <- runif(nrow(dd.accuracy.check))
    dd.accuracy.check <- dd.accuracy.check %>%
      mutate(Predicted_Race = ifelse(White_Probability < Conditional, "White", "Asian"))
    
    ## Calculate estimated share of white patients with high dose
    est.high.dose.rate.white <- nrow(dd.accuracy.check %>% filter(High_Dose == 1 & Predicted_Race == "White")) / 
      nrow(dd.accuracy.check %>% filter(Predicted_Race == "White"))
    
    ## Calculate estimated share of Asian patients with high dose
    est.high.dose.rate.asian <- nrow(dd.accuracy.check %>% filter(High_Dose == 1 & Predicted_Race != "White")) / 
      nrow(dd.accuracy.check %>% filter(Predicted_Race != "White"))
    
    ## Calculate estimated disparity (DD)
    est.dd <- est.high.dose.rate.white - est.high.dose.rate.asian
    
    dd.delta <- rbind(data.frame(est.pop.share.white=est.pop.share.white, est.dd=est.dd), dd.delta)
  }
}
  