####################################################################################################
###########     Practical Methods for Measuring Algorithmic Fairness with Proxy Data      ##########
####################################################################################################

source("load_libraries_utilities.R")

########################################     Simulation     ########################################

## SETUP

## Set population size & share RED
pop.size <- 100000
pop.share.red = 0.5

## Set the predicted & actual probability of a "good" outcome for members of the GREEN group: 
## 40% and 50%, respectively
pred.green <- 0.4
actual.green <- 0.5

## Set the predicted & actual probability of a "good" outcome for members of the RED group: 
## 70% and 60%, respectively
pred.red <- 0.7
actual.red <- 0.6

## Define situations to assess
situations <- list(
  list(name="Randomly Assigned Perfect Identifier", red.share.x1=0.03, green.share.x1=0),
  list(name="Randomly Assigned Near-Perfect Identifier", red.share.x1=0.03, green.share.x1=0.0003),
  list(name="Randomly Assigned Less-Perfect Identifier", red.share.x1=0.03, green.share.x1=0.003), 
  list(name="Perfect Identifier Associated with a True Positive", red.share.x1=0.03, green.share.x1=0),
  list(name="Perfect Identifier Associated with a False Negative", red.share.x1=0.03, green.share.x1=0))

situations <- as.data.frame(do.call(rbind, situations))
situations <- situations %>% mutate_all(unlist)

## Run for each situation
for (i in 1:nrow(situations)) {
  
  name <- situations[i, "name"]
  red.share.x1 <- situations[i, "red.share.x1"] ## share of the RED group with characteristic X1
  green.share.x1 <- situations[i, "green.share.x1"] ## share of the GREEN group with characteristic X1
  
  # GENERATE DATA
  
  ## Create a demographic summary table (share of individuals with each trait)
  demographic.summary.table <- data.frame(Color=c("Red", "Green"), X1=c(red.share.x1, green.share.x1))
  
  ## Create a pool of green individuals
  green <- data.frame(ID = 1:pop.size, Color="Green", Characteristic_Probability=runif(pop.size)) %>% 
    left_join(demographic.summary.table) %>%
    mutate(Characteristic = ifelse(Characteristic_Probability < X1, "X1", "X2")) %>%
    select(-Characteristic_Probability, -X1) %>%
    mutate(Good_Outcome_Prob = runif(pop.size))
  
  predicted.yes <- sample(1:nrow(green), pred.green * nrow(green), prob=green$Good_Outcome_Prob)
  actual.yes <- sample(1:nrow(green), actual.green * nrow(green), prob=green$Good_Outcome_Prob)
  
  green <- green %>% 
    mutate(Pred_Outcome = ifelse(ID %in% predicted.yes, 1, 0), 
           Actual_Outcome = ifelse(ID %in% actual.yes, 1, 0))
  
  ## Create a pool of red individuals
  red <- data.frame(ID = 1:pop.size, Color="Red", Characteristic_Probability=runif(pop.size)) %>% 
    left_join(demographic.summary.table) %>%
    mutate(Characteristic = ifelse(Characteristic_Probability < X1, "X1", "X2")) %>%
    select(-Characteristic_Probability, -X1) %>%
    mutate(Good_Outcome_Prob = runif(pop.size))
  
  predicted.yes <- sample(1:nrow(red), pred.red * nrow(red), prob=red$Good_Outcome_Prob)
  actual.yes <- sample(1:nrow(red), actual.red * nrow(red), prob=red$Good_Outcome_Prob)
  
  red <- red %>% 
    mutate(Pred_Outcome = ifelse(ID %in% predicted.yes, 1, 0), 
           Actual_Outcome = ifelse(ID %in% actual.yes, 1, 0))
  
  ## Correlate the characteristic with a true positive according to the scenario
  if (name == "Perfect Identifier Associated with a False Negative") {
    red = red %>%
      mutate(Characteristic = ifelse(Pred_Outcome == 0 & Actual_Outcome == 1, Characteristic, 
                                     ifelse(runif(pop.size) < 0.3, Characteristic, "X2")))
  } else if (name == "Perfect Identifier Associated with a True Positive") {
    red = red %>%
      mutate(Characteristic = ifelse(Pred_Outcome == 1 & Actual_Outcome == 1, Characteristic, 
                                     ifelse(runif(pop.size) < 0.3, Characteristic, "X2")))
  }
  
  ## Combine to create full population
  pop <- rbind(green[sample(1:nrow(green), nrow(green) * (1 - pop.share.red)),], 
               red[sample(1:nrow(red), nrow(red) * (pop.share.red)),])
  
  ## Calculate the true TPRD (True Positive Rate Disparity)
  true.tpr.red <- nrow(pop %>% filter(Actual_Outcome == 1 & Pred_Outcome == 1 & Color == "Red")) / 
    nrow(pop %>% filter(Actual_Outcome == 1 & Color == "Red"))
  
  true.tpr.green <- nrow(pop %>% filter(Actual_Outcome == 1 & Pred_Outcome == 1 & Color == "Green")) / 
    nrow(pop %>% filter(Actual_Outcome == 1 & Color == "Green"))
  
  true.tprd <- true.tpr.red - true.tpr.green
  
  out.dat <- pop %>% mutate(
    tp = Actual_Outcome == 1 & Pred_Outcome == 1,
    pred.color = ifelse(Characteristic == "X1", "red", "green"))
  
  write.csv(out.dat, paste0("Data/Simulated/", name, ".csv"), row.names=F)
}
