####################################################################################################
###########     Practical Methods for Measuring Algorithmic Fairness with Proxy Data      ##########
####################################################################################################

source("load_libraries_utilities.R")

########################################     Simulation     ########################################

## SETUP

## Set population size
pop.size <- 5000

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
  list(name="Very Small, Balanced Disparity", pop.share.red=0.5, red.share.x1=0.99, green.share.x1=0.01),
  list(name="Small, Balanced Disparity with Unequal Group Sizes", pop.share.red=0.2, red.share.x1=0.9, green.share.x1=0.1),
  list(name="Medium, Unbalanced Disparity", pop.share.red=0.5, red.share.x1=0.99, green.share.x1=0.5))

situations <- as.data.frame(do.call(rbind, situations))
situations <- situations %>% mutate_all(unlist)

## Run for each situation
for (i in 1:nrow(situations)) {
  
  name <- situations[i, "name"]
  pop.share.red <- situations[i, "pop.share.red"] ## share of the population in the RED group
  red.share.x1 <- situations[i, "red.share.x1"] ## share of the RED group with characteristic X1
  green.share.x1 <- situations[i, "green.share.x1"] ## share of the GREEN group with characteristic X1
  
  ## GENERATE DATA
  
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
  
  ## Combine to create full population
  pop <- rbind(green[sample(1:nrow(green), nrow(green) * (1 - pop.share.red)),], 
               red[sample(1:nrow(red), nrow(red) * (pop.share.red)),])
  
  ## Calculate the true TPRD (True Positive Rate Disparity)
  true.tpr.red <- nrow(pop %>% filter(Actual_Outcome == 1 & Pred_Outcome == 1 & Color == "Red")) / 
    nrow(pop %>% filter(Actual_Outcome == 1 & Color == "Red"))
  
  true.tpr.green <- nrow(pop %>% filter(Actual_Outcome == 1 & Pred_Outcome == 1 & Color == "Green")) / 
    nrow(pop %>% filter(Actual_Outcome == 1 & Color == "Green"))
  
  true.tprd <- true.tpr.red - true.tpr.green
  
  ## Complete the demographic summary table - all individuals who don't display characteristic X1 
  ## display characteristic X2
  demographic.summary.table$X2 <- 1 - demographic.summary.table$X1
  demographic.summary.table <- gather(demographic.summary.table, key="Characteristic", value="Proportion", 2:3)
  
  ## Measure accuracy of estimated TPRD at different assumed proportions of RED individuals in the 
  ## population
  tprd.delta <- data.frame()
  for (est.pop.share.red in seq(0, 1, 0.01)) {
    
    print(est.pop.share.red)
    
    ## Create priors 
    priors <- data.frame("Prior"=c(0 + est.pop.share.red, 1 - est.pop.share.red), "Color"= c("Red", "Green"))
    
    ## Use demographic summary table + priors to create conditionals
    conditionals <- left_join(demographic.summary.table, priors, by="Color")
    conditionals <- conditionals %>% 
      group_by(Characteristic) %>%
      mutate(Weighted_Avg = weighted.mean(Proportion, Prior)) %>%
      ungroup() %>%
      mutate(Conditional = (Prior * Proportion) / Weighted_Avg)
    
    conditionals <- conditionals %>% filter(Color == "Red") %>% select(Characteristic, Conditional)
    
    tprd.accuracy.check <- pop %>% left_join(conditionals, by=c("Characteristic"))
    
    ## Repeat each check 10 times
    for (j in 1:10) {
      
      ## Randomly assign individuals to be RED or GREEN based on their probability of being RED
      tprd.accuracy.check$Red_Probability <- runif(nrow(tprd.accuracy.check))
      tprd.accuracy.check <- tprd.accuracy.check %>%
        mutate(Predicted_Color = ifelse(Red_Probability < Conditional, "Red", "Green"))
      
      est.tpr.red <- nrow(tprd.accuracy.check %>% filter(Actual_Outcome == 1 & Pred_Outcome == 1 & Predicted_Color == "Red")) / 
        nrow(tprd.accuracy.check %>% filter(Actual_Outcome == 1 & Predicted_Color == "Red"))
      
      est.tpr.green <- nrow(tprd.accuracy.check %>% filter(Actual_Outcome == 1 & Pred_Outcome == 1 & Predicted_Color == "Green")) / 
        nrow(tprd.accuracy.check %>% filter(Actual_Outcome == 1 & Predicted_Color == "Green"))
      
      color.accuracy <- nrow(tprd.accuracy.check %>% filter(Color == Predicted_Color)) / nrow(tprd.accuracy.check)
      
      ## Calculate estimated TPRD
      est.tprd <- est.tpr.red - est.tpr.green
      
      tprd.delta <- rbind(data.frame(est.pop.share.red=est.pop.share.red, est.tprd=est.tprd, 
                                     color.accuracy=color.accuracy), 
                          tprd.delta)
    }
  }
  write.csv(tprd.delta, paste0("Data/Simulated/", name, ".csv"), row.names=F)
}

