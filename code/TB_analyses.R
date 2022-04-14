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
#setwd("./friendship-colombia")       # working directory for apple
#setwd("~/Desktop/friendship paper")   # Augusto's working directory

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

#import data
friends <- as.matrix(read.table("./data/TB_friends.csv", sep = ",", row.names = 1, header = TRUE))
relatedness <- as.matrix(read.table("./data/TB_kinship.csv", sep = ",", row.names = 1, header = TRUE))
sharing <- as.matrix(read.table("./data/TB_exchange.csv", sep = ",", row.names = 1, header = TRUE))
work <- as.matrix(read.table("./data/TB_working.csv", sep = ",", row.names = 1, header = TRUE))
attractiveness <- as.matrix(read.table("./data/TB_attractiveness.csv", sep = ",", row.names = 1, header = TRUE))
distance <- as.matrix(read.table("./data/TB_physical_distance.csv", sep = ",", row.names = 1, header = TRUE))
pol_distance <- as.matrix(read.table("./data/TB_political_distance.csv", sep = ",", row.names = 1, header = TRUE))
wealth_distance <- as.matrix(read.table("./data/TB_wealth_distance.csv", sep = ",", row.names = 1, header = TRUE))
age_distance <- as.matrix(read.table("./data/TB_age_distance.csv", sep = ",", row.names = 1, header = TRUE))
atrakt_distance <- as.matrix(read.table("./data/TB_atrakt_dist.csv", sep = ",", row.names = 1, header = TRUE))
edu_distance <- as.matrix(read.table("./data/TB_edu_distance.csv", sep = ",", row.names = 1, header = TRUE))
bmi_distance <- as.matrix(read.table("./data/TB_bmi_distance.csv", sep = ",", row.names = 1, header = TRUE))
pv_distance <- as.matrix(read.table("./data/TB_pv_distance.csv", sep = ",", row.names = 1, header = TRUE))
dl_distance <- as.matrix(read.table("./data/TB_dl_distance.csv", sep = ",", row.names = 1, header = TRUE))
al_distance <- as.matrix(read.table("./data/TB_al_distance.csv", sep = ",", row.names = 1, header = TRUE))
qm_distance <- as.matrix(read.table("./data/TB_qm_distance.csv", sep = ",", row.names = 1, header = TRUE))

#import individual data
att <- read.csv("./data/TB_individuals.csv", sep = ",")
att <- att[att$PID %in% rownames(friends),]
att$Sex[att$Sex == "F"] <- 1
att$Sex[att$Sex == "M"] <- 0 

#code religion
att$Religion[att$Religion %in% c("CHRISTIAN", "PENTECOSTAL")] <- "OTHER"
att$Religion[att$Religion == "SPIRITUAL"] <- "NONE"

# Subset to individuals who played the RICH games
N <- length(att$LeaveOther[!is.na(att$LeaveOther)])

# Create the STRAND data object
nets <- list( Friends = friends[1:N,1:N])

sharing2 <- ifelse( (sharing + t(sharing)) > 0, 1, 0) #line to be added to all scripts

dyad <- list( Relatedness = relatedness[1:N,1:N], 
              Sharing = sharing[1:N,1:N], 
              Polit_dist = pol_distance[1:N,1:N], 
              Phys_dist = distance[1:N,1:N],
              Age_dist = age_distance[1:N,1:N], 
              Wealth_dist = wealth_distance[1:N,1:N],
              Edu_dist = edu_distance[1:N,1:N],
              BMI_dist = bmi_distance[1:N,1:N],
              PV = pv_distance[1:N,1:N],
              DL = dl_distance[1:N,1:N],
              AL = al_distance[1:N,1:N],
              QM = qm_distance[1:N,1:N],
              Atrakt_dist = atrakt_distance[1:N, 1:N]
)

group_ids <- data.frame(Ethnicity = as.factor(att$Ethnicity[1:N]), 
                        Sex = as.factor(ifelse(att$Sex[1:N] == 1, "FEMALE", "MALE")),
                        Religion = as.factor(att$Religion[1:N]))

indiv <-  data.frame(Age = center(att$Age[1:N]), 
                     Grip = center(att$Grip[1:N]),
                     BMI = center(att$BMI[1:N]),  
                     Wealth = center(log(att$hh_wealth[1:N]+20)),
                     Give = center(att$GiveOther[1:N]),
                     Leave = center(att$LeaveOther[1:N]),
                     Punish = center(att$ReduceOther[1:N]),
                     Edu = center(att$EducationYears[1:N]),
                     RS = att$ChildrenAlive[1:N],
                     Attractiveness = att$A_S[1:N])

model_dat <- make_strand_data(self_report = nets,
                              block_covariates = group_ids, 
                              individual_covariates = indiv, 
                              dyadic_covariates = dyad)

#model
fit <- fit_block_plus_social_relations_model( data=model_dat,
                                              block_regression = ~ Sex + Religion + Ethnicity,
                                              focal_regression = ~ 1,
                                              target_regression = ~ Give + Leave + Punish + Attractiveness + RS +  Grip + Wealth + Age + Edu + BMI,
                                              dyad_regression = ~ Sharing + Relatedness  + Phys_dist  + Polit_dist +  Wealth_dist + Age_dist + Edu_dist + BMI_dist + Atrakt_dist,
                                              mode="mcmc",
                                              stan_mcmc_parameters = list(chains = 1, parallel_chains = 1, refresh = 1,
                                                                          iter_warmup = 1000, iter_sampling = 1000,
                                                                          max_treedepth = NULL, adapt_delta = .98)
)

res <- summarize_strand_results(fit)

