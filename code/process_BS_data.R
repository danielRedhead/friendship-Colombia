# Set working directory
# setwd("~/Desktop/Colombian data")
setwd("/Users/danielredhead/Dropbox/Augusto")
# Load packages
library(kinship2)
library(reshape2)
library(geosphere)
library(igraph)
library(tidyverse)

# Load data
indiv <- read.csv("Data/PartnerChoice-Ross-Site1-Individual.csv",
                  header = TRUE,
                  as.is = TRUE)

su <- read.csv("Data/PartnerChoice-Ross-Site1-Household.csv", 
               header = TRUE, 
               as.is = TRUE)

nl <- read.csv("Data/PartnerChoice-Ross-Site1-Networks.csv",
               na = "", 
               header = TRUE,
               as.is = TRUE)

kin <- read.csv("Data/PartnerChoice-Ross-Site1-Kinship.csv")

gl <- read.csv(file = "Data/PartnerChoice-Ross-Site1-Games.csv",
               na = "",
               header = TRUE,
               as.is = TRUE)

#################################################
# SPECIFY KINSHIP DATA
#################################################

kin$gender[kin$Sex == "M"] <- 1
kin$gender[kin$Sex == "F"] <- 2
kin$gender[is.na(kin$Sex)] <- 3
kin$gender[kin$Sex == "NA"] <- 3

ped <- pedigree(id = kin$PID, 
                dadid = kin$FID, 
                momid = kin$MID, 
                sex = kin$gender,
                missid = 0)

rel <- melt(2*(as.matrix(kinship(ped))),
            varnames = c('i', 'j'),
            value.name = "r",
            na.rm = TRUE)

# remove focal to focal (same ind)
rel1 <- subset(rel, i!=j) 
# halve data, i.e. remove duplicated pairs
rel2 <- subset(rel1, as.character(rel1$i) < as.character(rel1$j))

# add the sharing unit code for each i and j
# sharing unit code is the household id
rel2$i_su <- indiv$HHID[match(rel2$i, indiv$PID)]
rel2$j_su <- indiv$HHID[match(rel2$j, indiv$PID)]

# remove inds that aren't in the study community
# ie household unknown
rel3 <- subset(rel2, !(is.na(i_su) | is.na(j_su)))

# make a household dyad variable
# so alphabetize the HHID 
rel3$first <- ifelse(as.character(rel3$i_su) < as.character(rel3$j_su), 
                     paste(rel3$i_su), 
                     paste(rel3$j_su))

rel3$second <- ifelse(as.character(rel3$i_su) > as.character(rel3$j_su),
                      paste(rel3$i_su), 
                      paste(rel3$j_su))

rel3$su_dyad <- paste(rel3$first,
                      "_", 
                      rel3$second, 
                      sep = "")

# calc average inter-household relatedness

su_r <- tapply(rel3$r, rel3$su_dyad, mean)
su_r <- data.frame(su_dyad = names(su_r), 
                   avg_r = su_r, 
                   row.names = NULL)

#################################################
# SPECIFY DISTANCE MATRIX
#################################################

su_distance <- distm(cbind(su$X/50, su$Y/50))

rownames(su_distance) = su$HHID
colnames(su_distance) = su$HHID

# "ul" is for unit list (as in "edgelist")
ul1 <- melt(su_distance,
            varnames = c('ui', 'uj'), 
            value.name = "distance")

# create the paired dyad name, to match relatedness data
ul1$first <- ifelse(as.character(ul1$ui) < as.character(ul1$uj),
                    paste(ul1$ui),
                    paste(ul1$uj))

ul1$second <- ifelse(as.character(ul1$ui) > as.character(ul1$uj),
                     paste(ul1$ui),
                     paste(ul1$uj))

ul1$su_dyad <- paste(ul1$first, 
                     "_",
                     ul1$second, 
                     sep = "")

# combine kinship & distance data

unit_dyads <- merge(ul1[, c("su_dyad", "distance")],
                    su_r[, c("su_dyad", "avg_r")],
                    by = "su_dyad")

unit_dyads$distance <- 6000*(unit_dyads$distance / max(unit_dyads$distance, na.rm = TRUE))



#################################################
names <- unique(nl$Question) # question numbers
el <- vector("list", length(names))
gel <- vector("list", length(names))

# network list
for(i in 1:length(names)){
  el[[i]]<- nl[which(nl$Question == i), ]
}

# game list
for(i in 1:6){
  gel[[i]]<- gl[which(gl$Question == i), ]
}

elGive <- rbind(gel[[1]])

G <- as.matrix(elGive)
G <- G[complete.cases(G), ]
labels <- unique(c(G[, 1], G[, 2]))
A <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[G[, 1:2]] <- as.numeric(G[, 3])
A_Give <- A

G <- as.matrix(gel[[4]])
G <- G[complete.cases(G), ]
labels <- unique(c(G[, 1], G[, 2]))
A <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[G[, 1:2]] <- G[, 3]
A_Atrakt <- ifelse(A==0, 0,1)

# Loan money
elExchange <- rbind(el[[3]], el[[4]])
snExchange <- graph.data.frame(d = elExchange, directed = TRUE)

# Work with
elBehavioral <- rbind(el[[2]])
snBehavioral <- graph.data.frame(d = elBehavioral, directed = TRUE) 

# Friends
elInformation <- rbind(el[[1]])
snInformation <- graph.data.frame(d = elInformation, directed = TRUE)

### process network data for modelling

G <- as.matrix(elInformation)
G <- G[which((G[, 1] %in% colnames(A_Give)) & G[, 2] %in% colnames(A_Give)),]
A <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[G[, 1:2]] <- rep(1, length(G[, 3]))
A_Friends <- A

G <- as.matrix(elExchange)
G <- G[which((G[, 1] %in% colnames(A_Give)) & G[, 2] %in% colnames(A_Give)),]
A <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[G[, 1:2]] <- rep(1, length(G[, 3]))
A_Exchange <- A

G <- as.matrix(elBehavioral)
G <- G[which((G[, 1] %in% colnames(A_Give)) & G[, 2] %in% colnames(A_Give)),]
A <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[G[, 1:2]] <- rep(1, length(G[, 3]))
A_Work <- A

G <- as.matrix(rel1)
G <- G[which((G[, 1] %in% colnames(A_Give)) & G[, 2] %in% colnames(A_Give)),]
A <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[G[ ,1:2]] <-  as.numeric(G[ ,3])
A_Kin <- A

#################################################
# SPECIFY INDIVIDUAL-LEVEL DATA
#################################################
# CHECK TO SEE WHETHER THERE ARE ANY OTHER VARIABLES TO INCLUDE

indiv <- select(indiv, PID, HHID, Sex, Ethnicity, Age, 
                BMI, Grip, Religion, ReligionPublic,
                ReligionPrivate, GodInequality,
                EducationYears, PeaceVote, AbortionLegal, QueerMarriageOK, DrugsLegal)

# Add total household wealth to the indiv table
indiv$hh_wealth <- su$TotalValue[match(indiv$HHID, su$HHID)]

# Check for missing values
apply(indiv, 2, anyNA)

# Impute missing values for each variable
indiv$Grip[which(is.na(indiv$Grip))] <- mean(indiv$Grip, na.rm = TRUE)
indiv$EducationYears[which(is.na(indiv$EducationYears))] <- median(indiv$EducationYears,na.rm=TRUE)
indiv$Age[which(is.na(indiv$Age))] <- median(indiv$Age, na.rm=TRUE) #some other missing data
indiv$BMI[which(is.na(indiv$BMI))] <- median(indiv$BMI, na.rm=TRUE) 

# Impute missing values of categorical variables with "unknown"
indiv <- indiv %>% mutate_at(vars(HHID, 3:4, 8:11), ~replace(., is.na(.), "UNKNOWN"))

# Create compposite religiousness variable
indiv$RelPub <- ifelse(indiv$ReligionPublic == "AFEWTIMESPERWEEK" | indiv$ReligionPublic=="MORETHANONCEPERWEEK" | indiv$ReligionPublic == "ONCEPERWEEK", 1, 2) 
indiv$RelPri <- ifelse(indiv$ReligionPrivate == "EVERYDAY" | indiv$ReligionPrivate == "MORETHANONCEPERDAY", 1, 2)
indiv$GodIneq <- ifelse(indiv$GodInequality == "YES", 1, 2)

# Specify political opinions 
indiv$PeaceVote[which(indiv$PeaceVote == "STRONGLYDISAGREE")] <- "DISAGREE"
indiv$PeaceVote[which(indiv$PeaceVote == "STRONGLYAGREE")] <- "AGREE"
indiv$PeaceVote[is.na(indiv$PeaceVote)] <- "DONTKNOW"

indiv$AbortionLegal[which(indiv$AbortionLegal == "STRONGLYDISAGREE")] <- "DISAGREE"
indiv$AbortionLegal[which(indiv$AbortionLegal == "STRONGLYAGREE")] <- "AGREE"
indiv$AbortionLegal[is.na(indiv$AbortionLegal)] <- "DONTKNOW"

indiv$QueerMarriageOK[which(indiv$QueerMarriageOK == "STRONGLYDISAGREE")] <- "DISAGREE"
indiv$QueerMarriageOK[which(indiv$QueerMarriageOK == "STRONGLYAGREE")] <- "AGREE"
indiv$QueerMarriageOK[is.na(indiv$QueerMarriageOK)] <- "DONTKNOW"

indiv$DrugsLegal[which(indiv$DrugsLegal == "STRONGLYDISAGREE")] <- "DISAGREE"
indiv$DrugsLegal[which(indiv$DrugsLegal == "STRONGLYAGREE")] <- "AGREE"
indiv$DrugsLegal[is.na(indiv$DrugsLegal)] <- "DONTKNOW"

N = nrow(indiv)
phys_dist = pol_dist <- matrix( NA, nrow = N, ncol = N)
age_dist = wealth_dist <- matrix( NA, nrow = N, ncol = N)
R = c("AGREE","DISAGREE","DONTKNOW")

colnames(phys_dist) = colnames(pol_dist) = colnames(age_dist) = colnames(wealth_dist) = indiv$PID
rownames(phys_dist) = rownames(pol_dist) = rownames(age_dist) = rownames(wealth_dist) = indiv$PID

M = matrix(0, nrow=3, ncol=3)
M[1,1] = 1
M[1,2] = -1

M[2,1] = -1
M[2,2] = 1

indiv$hh_wealth <- (indiv$hh_wealth +20)

for( i in 1:N){
  for(j in 1:N){
  bob=c()
  bob[1] = M[which(indiv$PeaceVote[i]==R),which(indiv$PeaceVote[j]==R)]
  bob[2] = M[which(indiv$AbortionLegal[i]==R),which(indiv$AbortionLegal[j]==R)]
  bob[3] = M[which(indiv$QueerMarriageOK[i]==R),which(indiv$QueerMarriageOK[j]==R)]
  bob[4] = M[which(indiv$DrugsLegal[i]==R),which(indiv$DrugsLegal[j]==R)]
  pol_dist[i,j]=sum(bob)  
  phys_dist[i,j] = su_distance[which(indiv$HHID[i]==colnames(su_distance)),which(indiv$HHID[j]==colnames(su_distance))]
  age_dist[i,j] = abs(indiv$Age[i] - indiv$Age[j])
  wealth_dist[i,j] = abs(log(indiv$hh_wealth[i]) - log(indiv$hh_wealth[j]))
    }
}

phys_dist <- phys_dist/max(phys_dist, na.rm = TRUE)

# Filter individual dataframe by the IDs present in the friendship matrix
indiv <- indiv %>% filter(PID %in% rownames(A_Friends))

# Filter matrix elements by the IDs present in the individual dataframe
A_Friends <- A_Friends[which(rownames(A_Friends) %in% indiv$PID), which(rownames(A_Friends) %in% indiv$PID)]
A_Exchange <- A_Exchange[which(rownames(A_Exchange) %in% indiv$PID), which(rownames(A_Exchange) %in% indiv$PID)]
A_Work <- A_Work[which(rownames(A_Work) %in% indiv$PID), which(rownames(A_Work) %in% indiv$PID)]
A_Kin <- A_Kin[which(rownames(A_Kin) %in% indiv$PID), which(rownames(A_Kin) %in% indiv$PID)]
A_Atrakt <- A_Atrakt[which(rownames(A_Atrakt) %in% indiv$PID), which(rownames(A_Atrakt) %in% indiv$PID)]

# A_Exchange <- A_Exchange[match(rownames(A_Friends), rownames(A_Exchange)), match(colnames(A_Friends), colnames(A_Exchange))]

indiv <- indiv[match(rownames(A_Friends), indiv$PID),]

all(rownames(A_Exchange) == rownames(A_Friends))
all(rownames(A_Work) == rownames(A_Friends))
all(rownames(A_Kin) == rownames(A_Friends))
all(indiv$PID == rownames(A_Friends))


pol_dist <- pol_dist[which(rownames(pol_dist) %in% indiv$PID), which(colnames(pol_dist) %in% indiv$PID)]
phys_dist <- phys_dist[which(rownames(phys_dist) %in% indiv$PID), which(colnames(phys_dist) %in% indiv$PID)]
age_dist <- age_dist[which(rownames(age_dist) %in% indiv$PID), which(colnames(age_dist) %in% indiv$PID)]
wealth_dist <- wealth_dist[which(rownames(wealth_dist) %in% indiv$PID), which(colnames(wealth_dist) %in% indiv$PID)]


pol_dist <- pol_dist[match(indiv$PID, rownames(pol_dist)),match(indiv$PID, colnames(pol_dist))]
phys_dist <- phys_dist[match(indiv$PID, rownames(phys_dist)),match(indiv$PID, colnames(phys_dist))]
age_dist <- age_dist[match(indiv$PID, rownames(age_dist)),match(indiv$PID, colnames(age_dist))]
wealth_dist <- wealth_dist[match(indiv$PID, rownames(wealth_dist)),match(indiv$PID, colnames(wealth_dist))]



sum(colnames(A_Atrakt)!=colnames(A_Friends))
sum(colnames(pol_dist)!=colnames(A_Friends))
sum(colnames(phys_dist)!=colnames(A_Friends))
sum(colnames(age_dist)!=colnames(A_Friends))
sum(colnames(wealth_dist)!=colnames(A_Friends))


# Write out data
write.csv(A_Friends, "data/BS_friends.csv")
write.csv(A_Work, "data/BS_working.csv")
write.csv(A_Exchange, "data/BS_exchange.csv")
write.csv(A_Kin, "data/BS_kinship.csv")
write.csv(indiv, "data/BS_individuals.csv")
write.csv(pol_dist, "data/BS_political_distance.csv")
write.csv(A_Atrakt, "data/BS_attractiveness.csv")
write.csv(phys_dist, "data/BS_physical_distance.csv")
write.csv(age_dist, "data/BS_age_distance.csv")
write.csv(wealth_dist, "data/BS_wealth_distance.csv")

