########################################.
#
#   EHB Friendship Analyses  
#
########################################.

# Clear working space
rm(list = ls())

# Set the working directory
# You will need to specify the correct file path for your computer
#setwd("/Users/danielredhead/friendship-Colombia")
setwd("C:\\Users\\Mind Is Moving\\Desktop\\friendship-Colombia-main")       # working directory for apple
#setwd("~/Desktop/friendship paper")   # Augusto's working directory

# Load function
normalize = function(y) {
    x = y[!is.na(y)]
    x = (x - min(x)) / (max(x) - min(x))
    y[!is.na(y)] = x
    return(y)
}

# Load libraries
library(STRAND)
library(reshape2)

#import data
friends = as.matrix(read.table("./data/TB_friends.csv", sep = ",", row.names = 1, header = TRUE))
relatedness = as.matrix(read.table("./data/TB_kinship.csv", sep = ",", row.names = 1, header = TRUE))
sharing = as.matrix(read.table("./data/TB_exchange.csv", sep = ",", row.names = 1, header = TRUE))
work = as.matrix(read.table("./data/TB_working.csv", sep = ",", row.names = 1, header = TRUE))
attractiveness = as.matrix(read.table("./data/TB_attractiveness.csv", sep = ",", row.names = 1, header = TRUE))
distance = as.matrix(read.table("./data/TB_physical_distance.csv", sep = ",", row.names = 1, header = TRUE))
pol_distance = as.matrix(read.table("./data/TB_political_distance.csv", sep = ",", row.names = 1, header = TRUE))
wealth_distance = as.matrix(read.table("./data/TB_wealth_distance.csv", sep = ",", row.names = 1, header = TRUE))
age_distance = as.matrix(read.table("./data/TB_age_distance.csv", sep = ",", row.names = 1, header = TRUE))
atrakt_distance = as.matrix(read.table("./data/TB_atrakt_dist.csv", sep = ",", row.names = 1, header = TRUE))
edu_distance = as.matrix(read.table("./data/TB_edu_distance.csv", sep = ",", row.names = 1, header = TRUE))
bmi_distance = as.matrix(read.table("./data/TB_bmi_distance.csv", sep = ",", row.names = 1, header = TRUE))
pv_distance = as.matrix(read.table("./data/TB_pv_distance.csv", sep = ",", row.names = 1, header = TRUE))
dl_distance = as.matrix(read.table("./data/TB_dl_distance.csv", sep = ",", row.names = 1, header = TRUE))
al_distance = as.matrix(read.table("./data/TB_al_distance.csv", sep = ",", row.names = 1, header = TRUE))
qm_distance = as.matrix(read.table("./data/TB_qm_distance.csv", sep = ",", row.names = 1, header = TRUE))

giveA = as.matrix(read.table("./data/TB_give.csv", sep = ",", row.names = 1, header = TRUE))
leaveA = as.matrix(read.table("./data/TB_leave.csv", sep = ",", row.names = 1, header = TRUE))
punishA = as.matrix(read.table("./data/TB_punish.csv", sep = ",", row.names = 1, header = TRUE))

#import individual data
att = read.csv("./data/TB_individuals.csv", sep = ",")
att = att[att$PID %in% rownames(friends),]
att$Sex[att$Sex == "F"] = 1
att$Sex[att$Sex == "M"] = 0 

#code religion
att$Religion[att$Religion %in% c("CHRISTIAN", "PENTECOSTAL")] = "OTHER"
att$Religion[att$Religion == "SPIRITUAL"] = "NONE"

# Subset to individuals who played the RICH games
N = length(att$LeaveOther[!is.na(att$LeaveOther)])

# Create the STRAND data object
nets = list( Friends = friends[1:N,1:N])

sharing2 = ifelse( (sharing + t(sharing)) > 0, 1, 0) #line to be added to all scripts

giveA2 = ifelse( (giveA + t(giveA)) > 0, 1, 0) #line to be added to all scripts
leaveA2 = ifelse( (leaveA + t(leaveA)) > 0, 1, 0) #line to be added to all scripts
punishA2 = ifelse( (punishA + t(punishA)) > 0, 1, 0) #line to be added to all scripts

dyad = list( Relatedness = relatedness[1:N,1:N], 
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
              Atrakt_dist = atrakt_distance[1:N, 1:N],
              Give_Dyadic = giveA2[1:N, 1:N],
              Leave_Dyadic = leaveA2[1:N, 1:N]
)

group_ids = data.frame(Ethnicity = as.factor(att$Ethnicity[1:N]), 
                        Sex = as.factor(ifelse(att$Sex[1:N] == 1, "FEMALE", "MALE")),
                        Religion = as.factor(att$Religion[1:N]))

indiv =  data.frame(Age = center(att$Age[1:N]), 
                     Grip = center(att$Grip[1:N]),
                     BMI = center(att$BMI[1:N]),  
                     Wealth = center(log(att$hh_wealth[1:N]+20)),
                     Give = center(att$GiveOther[1:N]),
                     Leave = center(att$LeaveOther[1:N]),
                     Edu = center(att$EducationYears[1:N]),
                     RS = att$ChildrenAlive[1:N],
                     Attractiveness = att$A_S[1:N])

model_dat = make_strand_data(self_report = nets,
                              block_covariates = group_ids, 
                              individual_covariates = indiv, 
                              dyadic_covariates = dyad)

#model
fit_TB = fit_block_plus_social_relations_model( data=model_dat,
                                              block_regression = ~ Sex + Religion + Ethnicity,
                                              focal_regression = ~ 1,
                                              target_regression = ~ Give + Leave + Attractiveness + RS +  Grip + Wealth + Age + Edu + BMI,
                                              dyad_regression = ~ Sharing + Relatedness  + Phys_dist  + Polit_dist +  Wealth_dist + Age_dist + Edu_dist + BMI_dist + Atrakt_dist + Give_Dyadic + Leave_Dyadic,
                                              mode="mcmc",
                                              stan_mcmc_parameters = list(chains = 2, parallel_chains = 1, refresh = 1,
                                                                          iter_warmup = 1500, iter_sampling = 1500,
                                                                          max_treedepth = NULL, adapt_delta = .98)
)

res_TB = summarize_strand_results(fit_TB)


# Plotting results
strand_caterpillar_plot = function(results, submodels=NULL, normalized=FALSE, only_slopes=FALSE, only_technicals=FALSE, site="BOB"){
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

  df_TB = strand_caterpillar_plot(res_TB, submodel=c("Focal efffects: Out-degree","Target effects: In-degree","Dyadic effects","Other estimates"), normalized=FALSE, site="TB")
save(df_TB, file="df_TB.RData")

  df_TBs = strand_caterpillar_plot(res_TB, submodel=c("Focal efffects: Out-degree","Target effects: In-degree","Dyadic effects","Other estimates"), normalized=TRUE, site="TB")
save(df_TBs, file="df_TBs.RData")


#################################################################### Contrasts
####################### Sex
intercepts = res_TB$samples$srm_model_samples$block_parameters

sex_int2 = sex_int = intercepts[[2]]

nsamps = dim(sex_int)[1]

for(i in 1:nsamps){
  sex_int2[i,,] = sex_int[i,,] - sex_int[i,1,1]
}

sex_M = apply(sex_int2, 2:3, mean)
sex_L = apply(sex_int2, 2:3, HPDI)[1,,]
sex_H = apply(sex_int2, 2:3, HPDI)[2,,]
sex_Labs = matrix(c("offset, FEMALE to FEMALE", "offset, FEMALE to MALE", "offset, MALE to FEMALE", "offset, MALE to MALE"), nrow=2, ncol=2, byrow = TRUE)

sex_df_diff_tb = data.frame(Variable = c(sex_Labs),
                         Mean = c(sex_M),     
                         LI = c(sex_L),    
                         HI = c(sex_H),
                         Site = "TB"   
                         )

save(sex_df_diff_tb, file="df_TB_sex.RData")

####################### Religion
rel_int2 = rel_int = intercepts[[3]]

nsamps = dim(rel_int)[1]

for(i in 1:nsamps){
  rel_int2[i,,] = rel_int[i,,] - rel_int[i,1,1]
}

rel_M = apply(rel_int2, 2:3, mean)
rel_L = apply(rel_int2, 2:3, HPDI)[1,,]
rel_H = apply(rel_int2, 2:3, HPDI)[2,,]
rel_Labs = matrix(c("offset, CATHOLIC to CATHOLIC", "offset, CATHOLIC to NONE", "offset, CATHOLIC to OTHER",
                    "offset, NONE to CATHOLIC", "offset, NONE to NONE", "offset, NONE to OTHER",
                    "offset, OTHER to CATHOLIC", "offset, OTHER to NONE", "offset, OTHER to OTHER"), nrow=3, ncol=3, byrow = TRUE)

rel_df_diff_tb = data.frame(Variable = c(rel_Labs),
                         Mean = c(rel_M),     
                         LI = c(rel_L),    
                         HI = c(rel_H),
                         Site = "TB"   
                         )

save(rel_df_diff_tb, file="df_TB_rel.RData")


####################### Ethnicity
eth_int2 = eth_int = intercepts[[4]]

nsamps = dim(eth_int)[1]

for(i in 1:nsamps){
  eth_int2[i,,] = eth_int[i,,] - eth_int[i,1,1]
}

eth_M = apply(eth_int2, 2:3, mean)
eth_L = apply(eth_int2, 2:3, HPDI)[1,,]
eth_H = apply(eth_int2, 2:3, HPDI)[2,,]
eth_Labs = matrix(c("offset, AFROCOLOMBIAN to AFROCOLOMBIAN", "offset, AFROCOLOMBIAN to EMBERA", "offset, AFROCOLOMBIAN to MESTIZO",
                    "offset, EMBERA to AFROCOLOMBIAN", "offset, EMBERA to EMBERA", "offset, EMBERA to MESTIZO",
                    "offset, MESTIZO to AFROCOLOMBIAN", "offset, MESTIZO to EMBERA", "offset, MESTIZO to MESTIZO"), nrow=3, ncol=3, byrow = TRUE)

eth_df_diff_tb = data.frame(Variable = c(eth_Labs),
                         Mean = c(eth_M),     
                         LI = c(eth_L),    
                         HI = c(eth_H),
                         Site = "TB"   
                         )

save(eth_df_diff_tb, file="df_TB_eth.RData")



