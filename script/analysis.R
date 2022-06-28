library(dplyr)
library(tidyverse)
library(brms)
library(lme4)


### Read Data ###

#TIER
tier_loc <- "data/processed/tier.csv"
tier <- read.csv2(tier_loc, header=TRUE, sep=",")
tier <- tier[seq(1,6)]

#FACULTY
fac_loc <- "data/processed/faculty.csv"
fac <- read.csv2(fac_loc, header=TRUE, sep=",")



### Data Processing ###
tier$GENDER <- sapply(tier$GENDER, gsub, pattern=" ", replacement="")
rplace <- list("PRESIDENT/CHANCELLOR"=1, "VP/VICE CHANCELLOR"= 2, "OTHER" = 3,
               "DEAN" = 4, "ASSOCIATE DEAN" = 5, "ASSISTANT DEAN"= 6)
tier$TIER <- unname(sapply(tier$POSITION, FUN=function(x){return(rplace[[x]])}))




## Analysis - GENDER vs TIER ###
tier %>%
  group_by(POSITION, TIER, GENDER) %>%
  summarise(n=n()) %>%
  mutate(f_per = n/sum(n)) %>%
  filter(GENDER == "F") %>%
  ggplot(aes(x=TIER, y=f_per))+
  geom_line()
  
tier_gender <- tier %>%
  group_by(POSITION) %>%
  mutate(total = n()) %>%
  group_by(POSITION, TIER, GENDER, total) %>%
  summarise(n=n()) %>%
  filter(GENDER == "F")


fit_tier_gender <- glm(cbind(n, total) ~ TIER, data = tier_gender, family=binomial)


### Analysis - FACULTY ###

fac_prop_test <- prop.test(x=fac$Female, n = fac$Total)
remove_nurse <- filter(fac, X != "nur")
fac_prop_nurs_removed <- prop.test(x=remove_nurse$Female, n = remove_nurse$Total)
# Use the following instead
# fac_prop_test <- pairwise.prop.test(x=fac$Female, n = fac$Total)


marascuillo <- function(lp, n, alpha=0.05){
  k <- length(lp)
  lp_comb <- combn(k, m=2)
  
  sqrt_chisq <- sqrt(qchisq(p=1-alpha, df=k-1))
  output <- data.frame(pi = lp_comb[1,], pj = lp_comb[2,], rij = seq(1,ncol(lp_comb)),
                       pipj = seq(1,ncol(lp_comb)))
  
  for(i in 1:ncol(lp_comb)){
    ith <- lp_comb[1,i]
    jth <- lp_comb[2,i]
    
    pi <- lp[ith]
    ni <- n[ith]
    pj <- lp[jth]
    nj <- n[jth]

    rij <- sqrt_chisq* sqrt((pi*(1-pi))/ni + (pj*(1-pj))/nj)
    pipj <- abs(pi-pj)

    output[i,3] = rij
    output[i,4] = pipj
  }
  return(output)
}

lp <- fac$Female/fac$Total
mar_result <- marascuillo(lp, fac$Total)

mar_result<- mar_result %>%
  mutate(sig = pipj > rij) %>%
  mutate(diff = pipj-rij)


mar_result %>%
  filter(sig==TRUE)

##### Faculty nursing removed ####
lp_rmn <- remove_nurse$Female/remove_nurse$Total
mar_rmn_result <- marascuillo(lp_rmn, remove_nurse$Total)

mar_rmn_result <- mar_rmn_result %>%
  mutate(sig = pipj > rij) %>%
  mutate(diff = pipj-rij)

mar_rmn_result %>%
  filter(sig==TRUE)


##### Med vs other ####

fac_nomed <- fac%>%
  filter(X != "med") %>%
  select(-c("X"))
  
Female_nomed <- sum(fac_nomed$Female)
Total_nomed <- sum(fac_nomed$Total)

Female_med <- filter(fac, X== "med")$Female
Total_med <- filter(fac, X=="med")$Total

med_others <- prop.test(x=c(Female_nomed, Female_med), n= c(Total_nomed, Total_med))

#### Med vs others by tier ####
dean<- prop.test(c(9,168), c(40,411))
asso_dean <- prop.test(c(157, 610), c(360, 1166))
assi_dean <- prop.test(c(74, 365), c(141, 552))

# Asso dean and Assi dean significantly different


### Heatmap marascuillo matrix ###
mar_other <- mar_result %>%
  mutate(pj_sub = pi) %>%
  mutate(pi_sub = pj) %>%
  mutate(pi = pi_sub) %>%
  mutate(pj = pj_sub)

mar_longer <- rbind(mar_result, mar_other[, 1:6])


ggplot(data= mar_longer, aes(x=pi, y=pj, fill=diff))+
  geom_tile(color="white")+
  geom_text(aes(label = round(diff,2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-0.8,1), space = "Lab", 
                       name="Marascuillo ") +
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())




### Analysis - Longer tier ###
tier_longer_loc <- "data/processed/tier_longer.csv"
tier_long <- read.csv2(tier_longer_loc, header=TRUE, sep=",")
# tier_long$tier <- order(seq(1,8),decreasing = TRUE)
tier_long$tier <- seq(1,8)
tier_long$med <- c(0,0,0,0,0,0,1,1)

fit_tier_longer <- glm(cbind(Female,Total) ~ tier, family=binomial, data=tier_long)
fit_tier_me <- glmer(cbind(Female,Total) ~ tier + (1|med), family=binomial, data=tier_long)




### Others ###
gender %>%
  distinct(POSITION)

gender %>%
  select(GENDER == "F")