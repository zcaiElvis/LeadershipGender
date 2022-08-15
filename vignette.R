library(lmtest)
library(brms)
library(sandwich)

##############################################
### Code for: "Organizational leadership 
### gender differences in medical schools and
### affiliated universities." 
##############################################


### Data import ###

# Importing tier list data
tier_loc <- "data/processed/tier_longer.csv"
tier <- read.csv2(tier_loc, header=TRUE, sep=",")

# Importing faculty list data
fac_loc <- "data/processed/faculty.csv"
fac <- read.csv2(fac_loc, header=TRUE, sep=",")
fac$X[1] <- "eng"

### Data processing ###
tier$tier <- seq(1,8)
tier$study <- c(2,2,2,1,1,1,0,0)

##############################################
### Horizontal analysis across departments ###
##############################################
# Proportion test
fac_prop_test <- prop.test(fac$Female, fac$Total)


# Post-hoc adjusted z-score test
p_unadj <- matrix(NA, nrow(fac), nrow(fac))

# Calculating unadjusted p values
for(i in 1:nrow(fac)){
  for(j in 1:nrow(fac)){
    if(i != j){
      test <- prop.test(c(fac$Female[i], fac$Female[j]), c(fac$Total[i], fac$Total[j]))
      p_unadj[i,j] = test$p.value
    }
  }
}

# Reformatting matrix
p_unadj_diag_rm <- p_unadj
diag(p_unadj_diag_rm) = NA

# Adjust p-value with Holm's procedure
p_adj <- p.adjust(p_unadj_diag_rm, method="holm")
p_adj <- matrix(p_adj, nrow=14, ncol=14)
p_adj_df <- data.frame(p_adj)


#############################################
### Verticle analysis across leadership #####
#############################################

# Fitting GLM
fit_tier_lm <- glm(cbind(Female, Total) ~ tier + study, family=binomial, data=tier)

# Calculate the clustered covariance matrix
vcov_tier <- vcovCL(fit_tier_lm, cluster = ~study)

# Test coefficient with clustered covariance matrix
cluster_adj_coeff_test <- coeftest(fit_tier_lm, vcov=vcov_tier, cluster = ~study)


