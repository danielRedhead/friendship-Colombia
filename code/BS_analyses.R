########################################.
#
#   EHB Friendship Analyses  
#   Daniel Redhead
#
########################################.

# Clear working space
rm(list = ls())

# Set the working directory
# You will need to specify the correct file path for your computer
#setwd("/Users/danielredhead/friendship-Colombia")
setwd("./friendship-colombia")       # working directory for apple

# Load function
normalize <- function(y) {
    x<-y[!is.na(y)]
    x<-(x - min(x)) / (max(x) - min(x))
    y[!is.na(y)]<-x
    return(y)
}

# Load libraries
library(STRAND)
library(reshape2)

friends <- as.matrix(read.table("./data/BS_friends.csv", sep = ",", row.names = 1, header = TRUE))
relatedness <- as.matrix(read.table("./data/BS_kinship.csv", sep = ",", row.names = 1, header = TRUE))
sharing <- as.matrix(read.table("./data/BS_exchange.csv", sep = ",", row.names = 1, header = TRUE))
work <- as.matrix(read.table("./data/BS_working.csv", sep = ",", row.names = 1, header = TRUE))
attractiveness <- as.matrix(read.table("./data/BS_attractiveness.csv", sep = ",", row.names = 1, header = TRUE))
distance <- as.matrix(read.table("./data/BS_physical_distance.csv", sep = ",", row.names = 1, header = TRUE))
pol_distance <- as.matrix(read.table("./data/BS_political_distance.csv", sep = ",", row.names = 1, header = TRUE))
wealth_distance <- as.matrix(read.table("./data/BS_wealth_distance.csv", sep = ",", row.names = 1, header = TRUE))
age_distance <- as.matrix(read.table("./data/BS_age_distance.csv", sep = ",", row.names = 1, header = TRUE))


att <- read.csv("./data/BS_individuals.csv", sep = ",")
att <- att[att$PID %in% rownames(friends),]
att$Sex[att$Sex == "F"] <- 1
att$Sex[att$Sex == "M"] <- 0 

att$Religion[att$Religion %in% c("CHRISTIAN", "EVANGELICAL","PENTECOSTAL", "SEVENTH DAY ADVENTIST")] <- "OTHER"
att$Religion[att$Religion == "SPIRITUAL"] <- "NONE"

# Create the STRAND data object
nets <- list( Friends = friends)

dyad <- list( Relatedness = relatedness, Sharing = sharing, 
              Political = pol_distance, Proximity = distance,
              Attractiveness = attractiveness, Age_dist = age_distance, 
              Wealth_dist = wealth_distance)

group_ids <- data.frame(Ethnicity = as.factor(att$Ethnicity), 
                        Sex = as.factor(ifelse(att$Sex == 1, "FEMALE", "MALE")),
                        Religion = as.factor(att$Religion))

indiv <-  data.frame(Age = center(att$Age), Grip = center(att$Grip), 
					Wealth = center(log(att$hh_wealth+20)))

model_dat <- make_strand_data(self_report = nets,
                              block_covariates = group_ids, 
                              individual_covariates = indiv, 
                              dyadic_covariates = dyad)

fit <- fit_block_plus_social_relations_model( data=model_dat,
      block_regression = ~ Religion + Sex + Ethnicity,
      focal_regression = ~ Age + Grip + Wealth,
      target_regression = ~  1,
      dyad_regression = ~ Relatedness + Sharing + Attractiveness + Proximity + Age_dist + Wealth_dist + Political,
       mode="mcmc",
       stan_mcmc_parameters = list(chains = 1, parallel_chains = 1, refresh = 1,
    iter_warmup = 1000, iter_sampling = 1000,
     max_treedepth = NULL, adapt_delta = .98)
       )

res <- summarize_strand_results(fit)


