library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(brms)
library(lme4)
library(lmtest)
library(sandwich)
source("function/marascuillo.R")


### Data import ###

# Tier
tier_longer_loc <- "data/processed/tier_longer.csv"
tier_long <- read.csv2(tier_longer_loc, header=TRUE, sep=",")

# Faculty
fac_loc <- "data/processed/faculty.csv"
fac <- read.csv2(fac_loc, header=TRUE, sep=",")
fac$X[1] <- "eng"



### Data processing ###
tier_long$tier <- seq(1,8)
tier_long$med <- c(0,0,0,0,0,0,1,1)
tier_long$mid <- c(0,0,0,1,1,1,0,0)
tier_long$high<- c(1,1,1,0,0,0,0,0)

tier_long$rank <- c(1,1,1,2,2,2,3,3)
tier_long$study <- c(1,1,1,1,1,1,0,0)


### Analysis ###

# Chi-square test of proportions
fac_prop_test <- prop.test(fac$Female, fac$Total)


# Post-hoc adjusted z-score test
z_output <- matrix(0, nrow(fac), nrow(fac))
effective_or_not <- matrix(0, nrow(fac), nrow(fac))

for(i in 1:nrow(fac)){
  for(j in 1:nrow(fac)){
    if(i != j){
      
      ss_estimate <- power.prop.test(p1 = fac$Female[i]/fac$Total[i], p2 = fac$Female[j]/fac$Total[j], power=0.8)
      effective_or_not[i,j] <-fac$Total[i] > ss_estimate$n & fac$Total[j] > ss_estimate$n
      
      test <- prop.test(c(fac$Female[i], fac$Female[j]), c(fac$Total[i], fac$Total[j]))
      z_output[i,j] = test$p.value
    }
  }
}


# Diagnonal removed from bonferroni
z_diag_removed <- z_output
diag(z_diag_removed) = NA


p_adj <- p.adjust(z_diag_removed, method="bonferroni")
p_adj <- matrix(p_adj, nrow=14, ncol=14)

p_adj_df <- data.frame(p_adj)



# Generate heatmap
heatmap_df <- as.data.frame(expand.grid(1:14, 1:14))
heatmap_df$vals <- p_adj[cbind(heatmap_df$Var1, heatmap_df$Var2)]
heatmap_df$Var1 = fac$X[heatmap_df$Var1]
heatmap_df$Var2 = fac$X[heatmap_df$Var2]

heatmap_df$sig <- heatmap_df$vals < 0.05


# ggplot(heatmap_df, aes(x = Var1, y = Var2, fill= vals)) +
#   geom_tile()+
#   geom_text(aes(label = round(vals,2)), color = "black", size = 3) +
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white",
#                        midpoint = 0.05, limit = c(0,1), space = "Lab",
#                        name="Marascuillo ") +
#   theme_minimal()+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


ggplot(heatmap_df, aes(x = Var1, y = Var2, fill= vals)) +
  geom_tile()+
  geom_text(aes(label = round(vals,4)), color = "black", size = 3) +
  scale_fill_gradient2(low = "white", high = "#808080", mid = "white",
                       midpoint = 0.05, limit = c(0,1), space = "Lab",
                       name="p-values ") +
  theme_ipsum()+
  ggtitle("Adjusted P-value heatmap")+
  xlab("Department")+
  ylab("Department")



### Analyzing tiers ###

ggplot(tier_long, aes(x=tier, y =Female/Total))+
  geom_line()

### Mixed effect model, not used anymore 
# fit_tier_me <- glmer(cbind(Female,Total) ~ tier + (1|rank), family=binomial, data=tier_long)



### Clustered adjusted regression
fit_tier_lm <- glm(cbind(Female, Total) ~ tier + study, family=binomial, data=tier_long)
vcov_tier <- vcovCL(fit_tier_lm, cluster = ~study)
cluster_adj_coeff_test <- coeftest(fit_tier_lm, vcov=vcov_tier, cluster = ~study)


