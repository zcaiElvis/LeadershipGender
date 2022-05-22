library(dplyr)
library(tidyverse)
library(brms)


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
  mutate(sig = pipj > rij)


mar_result %>%
  filter(sig==TRUE)




### Others ###
gender %>%
  distinct(POSITION)

gender %>%
  select(GENDER == "F")