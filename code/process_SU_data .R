# Set working directory
setwd("~/Desktop/friendship paper") # Augusto's directory
# setwd("/Users/danielredhead/Dropbox/Augusto") # Dan's directory

# Load packages
library(kinship2)
library(reshape2)
library(geosphere)
library(igraph)
library(tidyverse)

# Load data
indiv <- read.csv("Data/PartnerChoice-Ross-Site4-Individual.csv",
                  header = TRUE,
                  as.is = TRUE)

su <- read.csv("Data/PartnerChoice-Ross-Site4-Household.csv", 
               header = TRUE, 
               as.is = TRUE)

nl <- read.csv("Data/PartnerChoice-Ross-Site4-Networks.csv",
               na = "", 
               header = TRUE,
               as.is = TRUE)

kin <- read.csv("Data/PartnerChoice-Ross-Site4-Kinship.csv")

gl <- read.csv(file = "Data/PartnerChoice-Ross-Site4-Games.csv",
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

#create kinship tree
ped <- pedigree(id = kin$PID, 
                dadid = kin$FID, 
                momid = kin$MID, 
                sex = kin$gender,
                missid = 0)

#edge list of relatedness coefficients
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

#household proximity matrix
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

#take the first element of the list (giving game) and covert it to a matrix
elGive <- rbind(gel[[1]])
G <- as.matrix(elGive)
G[which(G == "   ")] <- NA
G[which(G == "BZJ")] <- NA
G[which(G == "EZA")] <- NA
G[which(G == "GK6")] <- NA
G[which(G == "W7S")] <- NA
G <- G[complete.cases(G), ]
labels <- unique(c(G[, 1], G[, 2]))
A <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[G[, 1:2]] <- as.numeric(G[, 3])
A_Give <- A

#take the fourth element of the gel list (attactiveness) and convert it to a matrix
G <- as.matrix(gel[[4]])
G[which(G == "   ")] <- NA
G[which(G == "BZJ")] <- NA
G[which(G == "EZA")] <- NA
G[which(G == "GK6")] <- NA
G[which(G == "W7S")] <- NA
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
elWork <- rbind(el[[2]])
snWork <- graph.data.frame(d = elWork, directed = TRUE) 

# Friends
elFriends <- rbind(el[[1]])
snFriends <- graph.data.frame(d = elFriends, directed = TRUE)

### process network data for modelling

#friendship
G <- as.matrix(elFriends) #create matrix of friendship nominations
G <- G[which((G[, 1] %in% colnames(A_Give)) & G[, 2] %in% colnames(A_Give)),] #filter out IDs who are not in the giving game data
A <- matrix(0, length(labels), length(labels)) #create blank matrix
rownames(A) <- colnames(A) <- labels           #label blank matrix
A[G[, 1:2]] <- rep(1, length(G[, 3]))          #filling the matrix with the friendship nominations (1 for existence)
A_Friends <- A                                 #name friendship matrix

#exchange
G <- as.matrix(elExchange)
G <- G[which((G[, 1] %in% colnames(A_Give)) & G[, 2] %in% colnames(A_Give)),]
A <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[G[, 1:2]] <- rep(1, length(G[, 3]))
A_Exchange <- A

#work
G <- as.matrix(elWork)
G <- G[which((G[, 1] %in% colnames(A_Give)) & G[, 2] %in% colnames(A_Give)),]
A <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[G[, 1:2]] <- rep(1, length(G[, 3]))
A_Work <- A

#relatedness (individual)
G <- as.matrix(rel1)
G <- G[which((G[, 1] %in% colnames(A_Give)) & G[, 2] %in% colnames(A_Give)),]
A <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[G[ ,1:2]] <-  as.numeric(G[ ,3])
A_Kin <- A

#select relevant individual variables
indiv <- select(indiv, PID, HHID, Sex, Ethnicity, Age, 
                BMI, Grip, Religion, ReligionPublic,
                ReligionPrivate, GodInequality,
                EducationYears, PeaceVote, AbortionLegal, QueerMarriageOK, DrugsLegal,
                GiveOther, LeaveOther, ReduceOther, ChildrenAlive
                )

# Add total household wealth to the indiv table
indiv$hh_wealth <- su$TotalValue[match(indiv$HHID, su$HHID)]

# Check for missing values
apply(indiv, 2, anyNA)

# Impute missing values for each variable
indiv$EducationYears[which(is.na(indiv$EducationYears))] <- median(indiv$EducationYears,na.rm=TRUE)
indiv$Age[which(is.na(indiv$Age))] <- median(indiv$Age, na.rm=TRUE) 
indiv$BMI[which(is.na(indiv$BMI))] <- median(indiv$BMI, na.rm=TRUE) 

# Impute missing values of categorical variables with "unknown"
indiv <- indiv %>% mutate_at(vars(HHID, 3:4, Grip, 8:11), ~replace(., is.na(.), "UNKNOWN")) #replace NAs with unknowns

# Create composite religiousness variable
indiv$RelPub <- ifelse(indiv$ReligionPublic == "AFEWTIMESPERWEEK" | indiv$ReligionPublic=="MORETHANONCEPERWEEK" | indiv$ReligionPublic == "ONCEPERWEEK", 1, 2) 
indiv$RelPri <- ifelse(indiv$ReligionPrivate == "EVERYDAY" | indiv$ReligionPrivate == "MORETHANONCEPERDAY", 1, 2)
indiv$GodIneq <- ifelse(indiv$GodInequality == "YES", 1, 2)

## Specify political opinions
#opinions on peace vote
indiv$PeaceVote[which(indiv$PeaceVote == "STRONGLYDISAGREE")] <- "DISAGREE"
indiv$PeaceVote[which(indiv$PeaceVote == "STRONGLYAGREE")] <- "AGREE"
indiv$PeaceVote[is.na(indiv$PeaceVote)] <- "DONTKNOW"

#opinions on abortion
indiv$AbortionLegal[which(indiv$AbortionLegal == "STRONGLYDISAGREE")] <- "DISAGREE"
indiv$AbortionLegal[which(indiv$AbortionLegal == "STRONGLYAGREE")] <- "AGREE"
indiv$AbortionLegal[is.na(indiv$AbortionLegal)] <- "DONTKNOW"

#opinions on queer marriage
indiv$QueerMarriageOK[which(indiv$QueerMarriageOK == "STRONGLYDISAGREE")] <- "DISAGREE"
indiv$QueerMarriageOK[which(indiv$QueerMarriageOK == "STRONGLYAGREE")] <- "AGREE"
indiv$QueerMarriageOK[is.na(indiv$QueerMarriageOK)] <- "DONTKNOW"

#opinions on drugs legalization
indiv$DrugsLegal[which(indiv$DrugsLegal == "STRONGLYDISAGREE")] <- "DISAGREE"
indiv$DrugsLegal[which(indiv$DrugsLegal == "STRONGLYAGREE")] <- "AGREE"
indiv$DrugsLegal[is.na(indiv$DrugsLegal)] <- "DONTKNOW"

N <- nrow(indiv) #number of individuals
phys_dist <- pol_dist <- matrix( NA, nrow = N, ncol = N) #creates NxN matrices with NA as default cell value
age_dist <- wealth_dist <- matrix( NA, nrow = N, ncol = N)
edu_dist <- bmi_dist <- matrix( NA, nrow = N, ncol = N)
R <- c("AGREE","DISAGREE","DONTKNOW") #political responses

#create set of NxN empty matrices with N = individual IDs
AL_dist <- QM_dist <- DL_dist <- PV_dist <-  matrix( NA, nrow = N, ncol = N)
colnames(AL_dist) = colnames(QM_dist) = colnames(DL_dist) = colnames(PV_dist) = indiv$PID
rownames(AL_dist) = rownames(QM_dist) = rownames(DL_dist) = rownames(PV_dist)  = indiv$PID
colnames(edu_dist) = colnames(bmi_dist) = colnames(phys_dist) = colnames(pol_dist) = colnames(age_dist) = colnames(wealth_dist) = indiv$PID
rownames(edu_dist) = rownames(bmi_dist) = rownames(phys_dist) = rownames(pol_dist) = rownames(age_dist) = rownames(wealth_dist) = indiv$PID

# "Payoff" values for political opinions
M = matrix(0, nrow=3, ncol=3)
M[1,1] = 1  #both agree: 1
M[1,2] = -1 #discordant opinions: -1
M[2,1] = -1 #discordant opinions: -1
M[2,2] = 1  #both disagree: 1

#minimum non-zero value of the wealth variable
min_wealth <- indiv %>% 
  select(hh_wealth) %>%     #select wealth column
  filter(hh_wealth > 0) %>% #filter strictly positive values
  min()                     #get minimum among those
indiv$hh_wealth <- (indiv$hh_wealth + min_wealth) #modify wealth variable to make it "loggable"

# Distance matrix
for( i in 1:N){
  for(j in 1:N){
    bob=c()
    PV_dist[i,j] = bob[1] = M[which(indiv$PeaceVote[i]==R),which(indiv$PeaceVote[j]==R)]
    AL_dist[i,j] = bob[2] = M[which(indiv$AbortionLegal[i]==R),which(indiv$AbortionLegal[j]==R)]
    QM_dist[i,j] = bob[3] = M[which(indiv$QueerMarriageOK[i]==R),which(indiv$QueerMarriageOK[j]==R)]
    DL_dist[i,j] = bob[4] = M[which(indiv$DrugsLegal[i]==R),which(indiv$DrugsLegal[j]==R)]
    pol_dist[i,j]=length(which(bob==-1))  
    phys_dist[i,j] = su_distance[which(indiv$HHID[i]==colnames(su_distance)),which(indiv$HHID[j]==colnames(su_distance))]*ifelse(indiv$Ethnicity[i]==indiv$Ethnicity[j],1,0)
    age_dist[i,j] = abs(indiv$Age[i] - indiv$Age[j])
    wealth_dist[i,j] = abs(log(indiv$hh_wealth[i]) - log(indiv$hh_wealth[j]))
    bmi_dist[i,j] = abs(indiv$BMI[i] - indiv$BMI[j])
    edu_dist[i,j] = abs(indiv$EducationYears[i] - indiv$EducationYears[j])
  }
}

# Normalize physical distance (proximity)
phys_dist <- phys_dist/max(phys_dist, na.rm = TRUE)

# Filter individual dataframe by the IDs present in the friendship matrix
indiv <- indiv %>% filter(PID %in% rownames(A_Friends))

# Filter matrix elements by the IDs present in the individual dataframe
A_Friends <- A_Friends[which(rownames(A_Friends) %in% indiv$PID), which(rownames(A_Friends) %in% indiv$PID)]
A_Exchange <- A_Exchange[which(rownames(A_Exchange) %in% indiv$PID), which(rownames(A_Exchange) %in% indiv$PID)]
A_Work <- A_Work[which(rownames(A_Work) %in% indiv$PID), which(rownames(A_Work) %in% indiv$PID)]
A_Kin <- A_Kin[which(rownames(A_Kin) %in% indiv$PID), which(rownames(A_Kin) %in% indiv$PID)]
A_Atrakt <- A_Atrakt[which(rownames(A_Atrakt) %in% indiv$PID), which(rownames(A_Atrakt) %in% indiv$PID)]
indiv <- indiv[match(rownames(A_Friends), indiv$PID),]

#check that names match
all(rownames(A_Exchange) == rownames(A_Friends))
all(rownames(A_Work) == rownames(A_Friends))
all(rownames(A_Kin) == rownames(A_Friends))
all(rownames(A_Atrakt) == rownames(A_Friends))
all(indiv$PID == rownames(A_Friends))

#subsetting (probably redundant)
pol_dist <- pol_dist[which(rownames(pol_dist) %in% indiv$PID), which(colnames(pol_dist) %in% indiv$PID)]
phys_dist <- phys_dist[which(rownames(phys_dist) %in% indiv$PID), which(colnames(phys_dist) %in% indiv$PID)]
age_dist <- age_dist[which(rownames(age_dist) %in% indiv$PID), which(colnames(age_dist) %in% indiv$PID)]
wealth_dist <- wealth_dist[which(rownames(wealth_dist) %in% indiv$PID), which(colnames(wealth_dist) %in% indiv$PID)]
edu_dist <- edu_dist[which(rownames(edu_dist) %in% indiv$PID), which(colnames(edu_dist) %in% indiv$PID)]
bmi_dist <- bmi_dist[which(rownames(bmi_dist) %in% indiv$PID), which(colnames(bmi_dist) %in% indiv$PID)]

#reordering
pol_dist <- pol_dist[match(indiv$PID, rownames(pol_dist)),match(indiv$PID, colnames(pol_dist))]
phys_dist <- phys_dist[match(indiv$PID, rownames(phys_dist)),match(indiv$PID, colnames(phys_dist))]
age_dist <- age_dist[match(indiv$PID, rownames(age_dist)),match(indiv$PID, colnames(age_dist))]
wealth_dist <- wealth_dist[match(indiv$PID, rownames(wealth_dist)),match(indiv$PID, colnames(wealth_dist))]
edu_dist <- edu_dist[match(indiv$PID, rownames(edu_dist)),match(indiv$PID, colnames(edu_dist))]
bmi_dist <- bmi_dist[match(indiv$PID, rownames(bmi_dist)),match(indiv$PID, colnames(bmi_dist))]
PV_dist <- PV_dist[match(indiv$PID, rownames(PV_dist)),match(indiv$PID, colnames(PV_dist))]
AL_dist <- AL_dist[match(indiv$PID, rownames(AL_dist)),match(indiv$PID, colnames(AL_dist))]
QM_dist <- QM_dist[match(indiv$PID, rownames(QM_dist)),match(indiv$PID, colnames(QM_dist))]
DL_dist <- DL_dist[match(indiv$PID, rownames(DL_dist)),match(indiv$PID, colnames(DL_dist))]

##check whether colnames do not match
sum(colnames(A_Atrakt)!=colnames(A_Friends))
sum(colnames(pol_dist)!=colnames(A_Friends))
sum(colnames(phys_dist)!=colnames(A_Friends))
sum(colnames(age_dist)!=colnames(A_Friends))
sum(colnames(wealth_dist)!=colnames(A_Friends))
sum(colnames(edu_dist)!=colnames(A_Friends))
sum(colnames(bmi_dist)!=colnames(A_Friends))
sum(colnames(PV_dist)!=colnames(A_Friends))
sum(colnames(AL_dist)!=colnames(A_Friends))
sum(colnames(QM_dist)!=colnames(A_Friends))
sum(colnames(DL_dist)!=colnames(A_Friends))

#build attractiveness scores
attractiveness_scores <- colSums(A_Atrakt) #number of attractiveness nominations received per individual

#create matrix of attractiveness distance
atrakt_dist <-matrix(NA, nrow = nrow(A_Friends), ncol = nrow(A_Friends)) #create NxN matrix with N = elements of friends matrix
#loop over the elements of the empty matrix to get attractiveness distance
for(i in 1:nrow(A_Friends)){
  for(j in 1:nrow(A_Friends)){
    atrakt_dist[i,j] = abs(attractiveness_scores[i] - attractiveness_scores[j])
  }
}
colnames(atrakt_dist) = rownames(atrakt_dist) = colnames(A_Atrakt)

#add attractiveness scores to the indiv dataframe
temp_df <- data.frame(PID = names(attractiveness_scores), A_S = attractiveness_scores) #make attractiveness vector into a temporary df
indiv <- left_join(indiv, temp_df, by = "PID") #merge with the indiv df based on personal IDs

# Write out data
write.csv(atrakt_dist, "data/SC_atrakt_dist.csv")
write.csv(A_Friends, "data/SC_friends.csv")
write.csv(A_Work, "data/SC_working.csv")
write.csv(A_Exchange, "data/SC_exchange.csv")
write.csv(A_Kin, "data/SC_kinship.csv")
write.csv(indiv, "data/SC_individuals.csv")
write.csv(pol_dist, "data/SC_political_distance.csv")
write.csv(A_Atrakt, "data/SC_attractiveness.csv")
write.csv(phys_dist, "data/SC_physical_distance.csv")
write.csv(age_dist, "data/SC_age_distance.csv")
write.csv(wealth_dist, "data/SC_wealth_distance.csv")
write.csv(edu_dist, "data/SC_edu_distance.csv")
write.csv(bmi_dist, "data/SC_bmi_distance.csv")
write.csv(PV_dist, "data/SC_pv_distance.csv")
write.csv(AL_dist, "data/SC_al_distance.csv")
write.csv(QM_dist, "data/SC_qm_distance.csv")
write.csv(DL_dist, "data/SC_dl_distance.csv")

# Write out data
#write.csv(A_Friends, "/Users/danielredhead/friendship-Colombia/data/SU_friends.csv")
#write.csv(A_Work, "/Users/danielredhead/friendship-Colombia/data/SU_working.csv")
#write.csv(A_Exchange, "/Users/danielredhead/friendship-Colombia/data/SU_exchange.csv")
#write.csv(A_Kin, "/Users/danielredhead/friendship-Colombia/data/SU_kinship.csv")
#write.csv(indiv, "/Users/danielredhead/friendship-Colombia/data/SU_individuals.csv")
