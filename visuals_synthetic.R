####################################################################################################
###########     Practical Methods for Measuring Algorithmic Fairness with Proxy Data      ##########
####################################################################################################

source("load_libraries_utilities.R")

true.tprd <- 0.318

####################################     Prior Probabilities     ###################################

## Define situations to graph
situations <- list(
  list(name="Very Small, Balanced Disparity", pop.share.red=0.5, peak.accuracy=0.5, title="Near-perfect proxy", subtitle="with equally-sized subgroups"),
  list(name="Small, Balanced Disparity with Unequal Group Sizes", pop.share.red=0.2, peak.accuracy=0.2, title="Less-perfect proxy", subtitle="with unequally-sized subgroups"),
  list(name="Medium, Unbalanced Disparity", pop.share.red=0.5, peak.accuracy=0.98, title="Less-perfect proxy", subtitle="that is more predictive for red individuals"))

situations <- as.data.frame(do.call(rbind, situations))
situations <- situations %>% mutate_all(unlist)

## Run for each situation
for (i in 1:nrow(situations)) {
  
  name <- situations[i, "name"]
  pop.share.red <- situations[i, "pop.share.red"] ## true share of the population in the RED group
  peak.accuracy <- situations[i, "peak.accuracy"] ## estimated share of the population in the RED group with peak accuracy in estimating TPRD
  title <- situations[i, "title"] 
  subtitle <- situations[i, "subtitle"] 
  
  ## Read data
  dat <- read.csv(paste0("Data/Simulated/", name, ".csv"))
  dat <- dat %>% group_by(est.pop.share.red) %>% summarise_all(mean) %>% ungroup() 
  
  ## Set intercept values for graph
  intercepts1 <- data.frame(y1=true.tprd, x1 = pop.share.red, x2 = peak.accuracy)
  
  ## Plot
  plot1 <- ggplot() +
    geom_hline(data=intercepts1, aes(yintercept = y1, color = "Actual TPRD"), size = 2) +
    geom_vline(data=intercepts1, aes(xintercept = x2, color = "Peak TPRD accuracy"), size = 1) +
    geom_vline(data=intercepts1, aes(xintercept = x1, color = "Actual % red"), size = 1, linetype="dashed") +
    geom_point(data=dat, aes(x=est.pop.share.red, y=est.tprd, color = "Estimated TPRD"), size=3) +
    ylim(0, 0.4) + 
    xlim(0, 1) + 
    scale_color_manual(values = c("darkgray", "#A6CEE3", "#1F78B4", "#6A3D9A")) +
    labs(title=title, subtitle=subtitle, x ="\nEstimated % red", y = "TPRD", color = "") +
    theme_minimal() + 
    theme(legend.position = "bottom") + 
    guides(color = guide_legend(override.aes = list(linetype = 0, size = 4, shape = 15, alpha = 1), 
                                nrow=2, byrow=T)) +
    theme(plot.title=element_text(size=13, face="bold"),
          plot.subtitle = element_text(size=11, face="italic"),
          axis.title = element_text(size=11, colour="#5F5F5F"),
          axis.text = element_text(size=10), 
          legend.text = element_text(size=10))
  
  ggsave(filename=paste0("Visuals/", name, ".pdf"), plot=plot1, device="pdf", height=4, width=4, 
         units="in", dpi=500)
}

#####################################     Predictive Power     #####################################

## Define situations to graph
situations <- list(
  list(name="when X1 is equally predictive for both subgroups", title="Dropoff in predictive power", subtitle="when X1 is equally predictive for both subgroups", ymin=0, ymax=0.4),
  list(name="when X1 is more predictive for red individuals", title="Dropoff in predictive power", subtitle="when X1 is more predictive for red individuals", ymin=0, ymax=0.4),
  list(name="when X1 is correlated with outcome", title="Dropoff in predictive power", subtitle="when X1 is correlated with outcome", ymin=-0.2, ymax=0.4))

situations <- as.data.frame(do.call(rbind, situations))
situations <- situations %>% mutate_all(unlist)

## Run for each situation
for (i in 1:nrow(situations)) {
  
  name <- situations[i, "name"]
  ymax <- situations[i, "ymax"] 
  ymin <- situations[i, "ymin"] 
  title <- situations[i, "title"] 
  subtitle <- situations[i, "subtitle"] 
  
  ## Read data
  dat <- read.csv(paste0("Data/Simulated/", name, ".csv"))
  
  ## Set intercept values for graph
  intercepts1 <- data.frame(y1=true.tprd)
  
  ## Plot
  plot1 <- ggplot() +
    geom_hline(data=intercepts1, aes(yintercept = y1, color = "Actual TPRD"), size = 2) +
    geom_point(data=dat, aes(x=red.share.x1, y=est.tprd, color = "Estimated TPRD"), size=3) +
    ylim(ymin, ymax) + 
    scale_color_manual(values = c("#A6CEE3", "#1F78B4")) +
    labs(title=title, subtitle=subtitle,
         x ="\n% red individuals with trait X1", y = "TPRD", color = "") +
    theme_minimal() + 
    theme(legend.position = "bottom") + 
    guides(color = guide_legend(override.aes = list(linetype = 0, size = 4, shape = 15, alpha = 1))) +
    theme(plot.title=element_text(size=13, face="bold"),
          plot.subtitle = element_text(size=11, face="italic"),
          axis.title = element_text(size=11, colour="#5F5F5F"),
          axis.text = element_text(size=10), 
          legend.text = element_text(size=10))
  
  ggsave(filename=paste0("Visuals/", name, ".pdf"), plot=plot1, device="pdf", height=4, width=4, 
         units="in", dpi=500)
}

###################################     High Accuracy Method     ###################################

## Define situations to assess
situations <- list(
  list(name="Randomly Assigned Perfect Identifier"),
  list(name="Randomly Assigned Near-Perfect Identifier"),
  list(name="Randomly Assigned Less-Perfect Identifier"), 
  list(name="Perfect Identifier Associated with a True Positive"),
  list(name="Perfect Identifier Associated with a False Negative"))

situations <- as.data.frame(do.call(rbind, situations))
situations <- situations %>% mutate_all(unlist)

## Run for each situation
for (i in 1:nrow(situations)) {
  
  true.share.pop.green <- 0.5
  name <- situations[i, "name"]
  
  ## Read data
  dat <- read.csv(paste0("Data/Simulated/", name, ".csv"))
  
  ## Calculate TPRD metrics
  tpr.red = nrow(dat %>% filter(Actual_Outcome == 1 & Pred_Outcome == 1 & Color == "Red")) / 
    nrow(dat %>% filter(Actual_Outcome == 1 & Color == "Red"))
  
  tpr.green = nrow(dat %>% filter(Actual_Outcome == 1 & Pred_Outcome == 1 & Color == "Green")) / 
    nrow(dat %>% filter(Actual_Outcome == 1 & Color == "Green"))
  
  true.trpd = tpr.red - tpr.green
  
  est.tpr.red <- mean((dat %>% filter(pred.color == "red"))$tp) / mean((dat %>% filter(pred.color == "red"))$Actual_Outcome)
  
  pop.share.green = seq(0, 1, 0.01)
  out <- data.frame(pop.share.green = pop.share.green, 
                    est.tpr.red = est.tpr.red, 
                    est.tpr.green = ((mean(dat$tp) / mean(dat$Actual_Outcome)) - (est.tpr.red * (1 - pop.share.green))) / (pop.share.green))
  out <- out %>% 
    mutate(est.tpr.green = ifelse(est.tpr.green < 0 | est.tpr.green > 1, NA, est.tpr.green), 
           tprd = est.tpr.red - est.tpr.green)
  
  ## Set intercept values for graph
  intercepts1 <- data.frame(y1=true.trpd, y2=est.tpr.red, y3=tpr.red, x1=true.share.pop.green)
  
  ylim_bottom = ifelse(name == "Perfect Identifier Associated with a False Negative", -0.5, 0)
  
  ## Plot
  plot1 <- ggplot() +
    geom_hline(data=intercepts1, aes(yintercept = y1, color = "Actual TPRD"), size = 2) +
    geom_hline(data=intercepts1, aes(yintercept = y2, color = "Est. red TPR"), size = 1) +
    geom_hline(data=intercepts1, aes(yintercept = y3, color = "Actual red TPR"), size = 1) +
    geom_vline(data=intercepts1, aes(xintercept = x1, color = "Actual % green"), size = 1, linetype="dashed") +
    geom_point(data=out, aes(x=pop.share.green, y=tprd, color = "Est. TPRD"), size=3) +
    ylim(ylim_bottom, 1) + 
    xlim(0, 1) + 
    scale_color_manual(values = c("darkgray", "#A6CEE3", "#33A02C", "#1F78B4", "#6A3D9A")) +
    labs(title=name, 
         x ="\nEstimated % green", y = "TPR", color = "") +
    theme_minimal() + 
    theme(legend.position = "bottom") + 
    guides(color = guide_legend(override.aes = list(linetype = 0, size = 4, shape = 15, alpha = 1), 
                                nrow=2, byrow=T)) +
    theme(plot.title=element_text(size=13, face="bold"),
          plot.subtitle = element_text(size=11, face="italic"),
          axis.title = element_text(size=11, colour="#5F5F5F"),
          axis.text = element_text(size=10), 
          legend.text = element_text(size=10))
  
  ggsave(filename=paste0("Visuals/", name, ".pdf"), plot=plot1, device="pdf", height=5, width=5, 
         units="in", dpi=500)
  
}
