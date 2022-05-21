library(dplyr)
library(tidyverse)
library(brms)


### Read Data ###
data_loc <- "data/processed/data.csv"
gender <- read.csv2(data_loc, header=TRUE, sep=",")
gender <- gender[seq(1,6)]


### Data Processing ###
gender$GENDER <- sapply(gender$GENDER, gsub, pattern=" ", replacement="")
rplace <- list("PRESIDENT/CHANCELLOR"=1, "VP/VICE CHANCELLOR"= 2, "OTHER" = 3,
               "DEAN" = 4, "ASSOCIATE DEAN" = 5, "ASSISTANT DEAN"= 6)
gender$TIER <- unname(sapply(gender$POSITION, FUN=function(x){return(rplace[[x]])}))




## Analysis - GENDER vs TIER ###
gender %>%
  group_by(POSITION, TIER, GENDER) %>%
  summarise(n=n()) %>%
  mutate(f_per = n/sum(n)) %>%
  filter(GENDER == "F") %>%
  ggplot(aes(x=TIER, y=f_per))+
  geom_line()
  
tier_gender <- gender %>%
  group_by(POSITION) %>%
  mutate(total = n()) %>%
  group_by(POSITION, TIER, GENDER, total) %>%
  summarise(n=n()) %>%
  filter(GENDER == "F")


fit_tier_gender <- glm(cbind(n, total) ~ TIER, data = tier_gender, family=binomial)


### Analysis - SCHOOL




gender %>%
  distinct(POSITION)

gender %>%
  select(GENDER == "F")