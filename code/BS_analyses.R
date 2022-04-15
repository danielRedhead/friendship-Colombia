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
#setwd("/Users/danielredhead/friendship-Colombia")                     # Dan's working directory
#setwd("C:\\Users\\Mind Is Moving\\Desktop\\friendship-Colombia-main") # Cody's working directory
#setwd("~/Desktop/friendship paper")                                   # Augusto's working directory
setwd("C:\\Users\\Mind Is Moving\\Desktop\\friendship-Colombia-main")       # working directory for apple

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
atrakt_distance <- as.matrix(read.table("./data/BS_atrakt_dist.csv", sep = ",", row.names = 1, header = TRUE))
edu_distance <- as.matrix(read.table("./data/BS_edu_distance.csv", sep = ",", row.names = 1, header = TRUE))
bmi_distance <- as.matrix(read.table("./data/BS_bmi_distance.csv", sep = ",", row.names = 1, header = TRUE))
pv_distance <- as.matrix(read.table("./data/BS_pv_distance.csv", sep = ",", row.names = 1, header = TRUE))
dl_distance <- as.matrix(read.table("./data/BS_dl_distance.csv", sep = ",", row.names = 1, header = TRUE))
al_distance <- as.matrix(read.table("./data/BS_al_distance.csv", sep = ",", row.names = 1, header = TRUE))
qm_distance <- as.matrix(read.table("./data/BS_qm_distance.csv", sep = ",", row.names = 1, header = TRUE))
att <- read.csv("./data/BS_individuals.csv", sep = ",")

att <- att[att$PID %in% rownames(friends),]
att$Sex[att$Sex == "F"] <- 1
att$Sex[att$Sex == "M"] <- 0 

att$Religion[att$Religion %in% c("CHRISTIAN", "EVANGELICAL","PENTECOSTAL", "SEVENTH DAY ADVENTIST")] <- "OTHER"
att$Religion[att$Religion == "SPIRITUAL"] <- "NONE"

att$Ethnicity[att$Ethnicity == "AFROEMBERA"] <- "AFROCOLOMBIAN"

# Subset to individuals who played the RICH games
N <- length(att$LeaveOther[!is.na(att$LeaveOther)])

# Create the STRAND data object
nets <- list( Friends = friends[1:N,1:N])

sharing2 <- ifelse( (sharing + t(sharing)) > 0, 1, 0) #line to be added to all scripts

dyad <- list( Relatedness = relatedness[1:N,1:N], 
              Sharing = sharing2[1:N,1:N], 
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
fit_BS <- fit_block_plus_social_relations_model( data=model_dat,
      block_regression = ~ Sex + Religion + Ethnicity,
      focal_regression = ~ 1,
      target_regression = ~ Give + Leave + Punish + Attractiveness + RS +  Grip + Wealth + Age + Edu + BMI,
      dyad_regression = ~ Sharing + Relatedness  + Phys_dist  + Polit_dist +  Wealth_dist + Age_dist + Edu_dist + BMI_dist + Atrakt_dist,
       mode="mcmc",
       stan_mcmc_parameters = list(chains = 1, parallel_chains = 1, refresh = 1,
    iter_warmup = 1000, iter_sampling = 1000,
     max_treedepth = NULL, adapt_delta = .98)
       )

res_BS <- summarize_strand_results(fit_BS)

#plotting results
strand_caterpillar_plot = function(results, submodels=NULL, normalized=FALSE, only_slopes=TRUE, only_technicals=FALSE, site="BOB"){
  dat = vector("list",length(results$summary_list))

  for(k in 1:length(results$summary_list)){
   dat[[k]] = data.frame(results$summary_list[[k]])
   dat[[k]]$SubModel = names(results$summary_list)[k]
   colnames(dat[[k]]) = c("Variable", "Median", "LI", "HI", "Mean","SD", "SubModel")
   for(j in 2:6)
   dat[[k]][,j] = as.numeric(dat[[k]][,j])
  }


df = do.call(rbind, dat)

colnames(df) = c("Variable", "Median", "LI", "HI", "Mean","SD", "SubModel")


df$Submodel = factor(df$SubModel)
df$Submodel = factor(df$SubModel, levels=c("False positive rate", "Recall of true ties","Theta: question-order effects",
                                           "Focal efffects: Out-degree","Target effects: In-degree","Dyadic effects", "Other estimates" ))

if(only_slopes==TRUE){
exclude=c("false positive rate intercept, layer 1",               
"false positive rate intercept, layer 2",               
"false positive rate sd, layer 1",                     
"false positive rate sd, layer 2",                            
"recall rate of true ties intercept, layer 1",          
"recall rate of true ties intercept, layer 2",          
"recall rate of true ties sd, layer 1",                 
"recall rate of true ties sd, layer 2",                 
"theta intercept, layer 1 to 2",                        
"theta sd, layer 1 to 2",                               
"focal effects sd",                                            
"target effects sd",                                            
"dyadic effects sd",                                                         
"focal-target effects rho (generalized recipocity)",    
"dyadic effects rho (dyadic recipocity)")   

df = df[which(!df$Variable %in% exclude),]
}

if(only_technicals==TRUE){
include=c("false positive rate intercept, layer 1",               
"false positive rate intercept, layer 2",               
"false positive rate sd, layer 1",                     
"false positive rate sd, layer 2",                            
"recall rate of true ties intercept, layer 1",          
"recall rate of true ties intercept, layer 2",          
"recall rate of true ties sd, layer 1",                 
"recall rate of true ties sd, layer 2",                 
"theta intercept, layer 1 to 2",                        
"theta sd, layer 1 to 2",                               
"focal effects sd",                                            
"target effects sd",                                            
"dyadic effects sd",                                                         
"focal-target effects rho (generalized recipocity)",    
"dyadic effects rho (dyadic recipocity)")   

unit=c("false positive rate intercept, layer 1",               
"false positive rate intercept, layer 2",                                  
"recall rate of true ties intercept, layer 1",          
"recall rate of true ties intercept, layer 2",                          
"theta intercept, layer 1 to 2",                                                                                                             
"focal-target effects rho (generalized recipocity)",    
"dyadic effects rho (dyadic recipocity)")

df = df[which(df$Variable %in% include),]

df$Scaling = ifelse(df$Variable %in% unit, "Rates", "Dispersion")
}


if(!is.null(submodels))
df = df[which(df$SubModel %in% submodels),]

df$Diff = df$HI-df$LI   

if(normalized==TRUE) {
  df$Median = df$Median/df$Diff
  df$LI = df$LI/df$Diff
  df$HI =  df$HI/df$Diff
}
 
 df$Site = site

p <- ggplot(df,aes(x=Variable,y=Median,ymin=LI,ymax=HI))+ 
     geom_linerange(size=1)+
     geom_point(size=2)+
     facet_grid( SubModel~., scales = "free", space='free')+
       #facet_wrap(vars(SubModel), ncol=1,scales = "free")+
       geom_hline(aes(yintercept=0),color="black",linetype="dashed")+
     labs(y="Regression parameters", x="") + theme(strip.text.x = element_text(size=12,face="bold"), 
     strip.text.y = element_text(size=12,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360)) + coord_flip() + theme(panel.spacing = unit(1, "lines")) 

p2 <- ggplot(df,aes(x=Variable,y=Median,ymin=LI,ymax=HI))+ 
     geom_linerange(size=1)+
     geom_point(size=2)+
     facet_grid( SubModel~Scaling, scales = "free")+
       #facet_wrap(vars(SubModel), ncol=1,scales = "free")+
       geom_hline(aes(yintercept=0),color="black",linetype="dashed")+
     labs(y="Regression parameters", x="") + theme(strip.text.x = element_text(size=8,face="bold"), 
     strip.text.y = element_text(size=8,face="bold"),axis.text=element_text(size=8),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360)) + coord_flip() + theme(panel.spacing = unit(1, "lines")) 


if(only_technicals==TRUE){
 return(p2)} else{
    return(df)
 }
 }

  df_BS = strand_caterpillar_plot(res_BS, submodel=c("Focal efffects: Out-degree","Target effects: In-degree","Dyadic effects","Other estimates"), normalized=FALSE, site="BS")
 save(df_BS, file="df_BS.RData")

  df_BSs = strand_caterpillar_plot(res_BS, submodel=c("Focal efffects: Out-degree","Target effects: In-degree","Dyadic effects","Other estimates"), normalized=TRUE, site="BS")
 save(df_BSs, file="df_BSs.RData")

