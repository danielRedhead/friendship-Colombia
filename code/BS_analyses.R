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

#distance <- as.matrix(read.table("./analyses/preliminary-analyses/su_distance.csv", sep = ","))
friends <- as.matrix(read.table("./data/BS_friends.csv", sep = ",", row.names = 1, header = TRUE))
relatedness <- as.matrix(read.table("./data/BS_kinship.csv", sep = ",", row.names = 1, header = TRUE))
sharing <- as.matrix(read.table("./data/BS_exchange.csv", sep = ",", row.names = 1, header = TRUE))
work <- as.matrix(read.table("./data/BS_working.csv", sep = ",", row.names = 1, header = TRUE))

att <- read.csv("./data/BS_individuals.csv", sep = ",")
att <- att[att$PID %in% rownames(friends),]
att$Sex[att$Sex == "F"] <- 1
att$Sex[att$Sex == "M"] <- 0 

# Creat binary matrix of whether individuals are the same religion
matrix <- data.frame('i' = att$PID, 'rel_i' = att$Religion)
matrix1 <- data.frame('j' = att$PID, 'rel_j' = att$Religion)
rel_df <- merge(matrix, matrix1)
rel_df <- rel_df[which(rel_df$i != rel_df$j),]
rel_df$value <- ifelse((rel_df$rel_i == rel_df$rel_j), 1, 0)
cleaned_df <- rel_df[,-2]
cleaned_df <- cleaned_df[,-3]
same_religion <- acast(cleaned_df, i ~ j , value.var='value')
same_religion[is.na(same_religion)] <- 1
same_religion <- same_religion[match(rownames(friends), rownames(same_religion)), match(colnames(friends), colnames(same_religion))]
diag(same_religion) <- 0

# Create the STRAND data object
nets <- list( Friends = friends)

dyad <- list( Relatedness = relatedness, Sharing = sharing, Same_Religion = same_religion)

group_ids <- as.factor(att$Ethnicity)

indiv <-  data.frame( Age = center(att$Age), Female = att$Sex, Grip = center(att$Grip), 
					Religion = as.factor(att$Religion), Wealth = center(att$hh_wealth))

model_dat <- make_strand_data( self_report = nets,
group_ids = group_ids, individual_covariates = indiv, dyadic_covariates = dyad
)

fit <- fit_block_plus_social_relations_model( data=model_dat,
      focal_regression = ~ Age + Female + Grip + Religion + Wealth,
      target_regression = ~ Age + Female + Grip + Religion + Wealth,
      dyad_regression = ~ Relatedness + Sharing + Same_Religion,
       mode="mcmc",
       stan_mcmc_parameters = list(chains = 1, parallel_chains = 1, refresh = 1,
    iter_warmup = 1000, iter_sampling = 4000,
     max_treedepth = NULL, adapt_delta = .98)
       )

res <- summarize_strand_results(fit)


