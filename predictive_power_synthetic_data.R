####################################################################################################
###########     Practical Methods for Measuring Algorithmic Fairness with Proxy Data      ##########
####################################################################################################

source("load_libraries_utilities.R")

########################################     Simulation     ########################################

## SETUP

## Set population size & share RED
pop.size <- 5000
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
  list(name="when X1 is equally predictive for both subgroups"),
  list(name="when X1 is more predictive for red individuals"),
  list(name="when X1 is correlated with outcome"))

situations <- as.data.frame(do.call(rbind, situations))
situations <- situations %>% mutate_all(unlist)

## Run for each situation
for (i in 1:nrow(situations)) {
  
  name <- situations[i, "name"]
  
  ## Measure accuracy of estimated TPRD at different assumed proportions of RED individuals with 
  ## trait X1
  tprd.delta <- data.frame()
  for (red.share.x1 in c(1, 0.99, 0.95, 0.9, 0.8, 0.7,  0.6, 0.55, 0.5)) {
    
    print(red.share.x1)
    
    ## Set value of green.share.x1 according to the scenario
    if (name == "when X1 is equally predictive for both subgroups" | 
        name == "when X1 is correlated with outcome") {
      green.share.x1 <- 1 - red.share.x1
    } else {
      green.share.x1 = 0.5
    }
    
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
    if (name == "when X1 is correlated with outcome") {
      red = red %>%
        mutate(Characteristic = ifelse(Pred_Outcome == 0 & Actual_Outcome == 1, Characteristic, 
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
    
    ## Complete the demographic summary table - all individuals who don't display characteristic X1 
    ## display characteristic X2
    demographic.summary.table$X2 <- 1 - demographic.summary.table$X1
    demographic.summary.table <- gather(demographic.summary.table, key="Characteristic", value="Proportion", 2:3)
    
    ## Create priors 
    priors <- data.frame("Prior"=c(0 + pop.share.red, 1 - pop.share.red), "Color"= c("Red", "Green"))
    
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
      
      tprd.delta <- rbind(data.frame(true.tprd=true.tprd, est.tprd=est.tprd, 
                                     color.accuracy=color.accuracy, red.share.x1=red.share.x1), 
                          tprd.delta)
    }
  }
  
  tprd.delta <- tprd.delta %>%
    group_by(red.share.x1) %>%
    summarise_all(function(x) round(mean(x), 2)) %>%
    mutate(tprd.disparity = true.tprd - est.tprd)
  
  write.csv(tprd.delta, paste0("Data/Simulated/", name, ".csv"), row.names=F)
}
