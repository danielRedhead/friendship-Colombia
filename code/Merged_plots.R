

# + theme(text=element_text(family="Comic Sans MS"))

setwd("C:\\Users\\Mind Is Moving\\Desktop\\friendship-Colombia-main")       # working directory for apple

################################################################################# Standardized here
load("df_SUs.RData")
load("df_SCs.RData")
load("df_BSs.RData")
load("df_TBs.RData")


df = rbind(df_TBs,df_BSs,df_SUs,df_SCs)

 df$Location = factor(df$Site)
 levels(df$Location) = c("Coastal", "Lowland", "Altiplano", "Highland")
 df$Location = factor(df$Location, levels=c("Coastal", "Lowland", "Highland", "Altiplano"))




############################################################## Dyadic effects
df_D = df[which((df$SubModel  %in% c("Dyadic effects")) & df$Variable!="dyadic effects sd" ),]

df_D$Variable = factor(df_D$Variable)

levels(df_D$Variable) = c(
"Age distance",    "Attractiveness distance",
"BMI distance",    "Education distance",  
"RICH Giving", "RICH Leaving",
"Spatial distance",   "Political distance", 
"Relatedness", "Sharing ties",    
"Wealth distance")

   

df_D$Variable = factor(df_D$Variable, 
     levels=c( rev(c("RICH Giving", "RICH Leaving",
               "Sharing ties",         
               "Relatedness")), 
               rev(c("Age distance",           
               "Attractiveness distance",
               "BMI distance",           
               "Education distance",              
               "Spatial distance",                 
               "Political distance",            
               "Wealth distance"))
               ))

df_D$Variable2 = ifelse(df_D$Variable %in% c("Sharing ties", "RICH Giving", "RICH Leaving",
               "Relatedness"), "Network overlap", "Homophily" )

p <- ggplot(df_D ,aes(x=Variable,y=Median,ymin=LI,ymax=HI,color=Location))+ 
     geom_linerange(size=1.5,aes(color=Location), position = position_dodge(-.5))+geom_point(size=2,aes(color=Location), position = position_dodge(-.5))+
     facet_grid(Variable2 ~ . ,scales="free",space = "free_y")+geom_hline(aes(yintercept=0),color="black",linetype="dashed")+ labs(x=" ") +
     labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"), 
     strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines")) +
     scale_colour_manual(values = c("Coastal" = "#8ca8c4", "Lowland"="#385438", "Highland"="#8c3800", "Altiplano"="#e0a81c")) + 
     theme(legend.text=element_text(size=12),legend.title=element_text(size=14))

 p  

ggsave("Dyadic_results.pdf", p, width=10, height=4.6, device = cairo_pdf)




############################################################# Target effects
df_T = df[which((df$SubModel  %in% c("Target effects: In-degree")) & df$Variable!="target effects sd"),]
df_T$SubModel2 = "Target effects"

df_T$Variable = factor(df_T$Variable)

levels(df_T$Variable) = c(
 "Age",           
 "Attractiveness",
 "BMI",           
 "Education",           
 "Giving score",          
 "Grip strength",          
 "Leaving score",         
 "RS",            
 "Wealth"
)



df_T$Variable = factor(df_T$Variable, 
     levels=c(
               "Leaving score",         
               "Giving score", 
               rev(c("Age",           
               "Attractiveness",
               "BMI",           
               "Education",              
               "Grip strength",                 
               "RS",            
               "Wealth"))
               ))

df_T$Variable2 = ifelse(df_T$Variable %in% c("Punishing score", 
               "Leaving score",         
               "Giving score"), "Propensities", "Attributes" )

p <- ggplot(df_T ,aes(x=Variable,y=Median,ymin=LI,ymax=HI,color=Location))+ 
     geom_linerange(size=1.5,aes(color=Location), position = position_dodge(-.5))+geom_point(size=2,aes(color=Location), position = position_dodge(-.5))+
     facet_grid(Variable2 ~ . ,scales="free",space = "free_y")+geom_hline(aes(yintercept=0),color="black",linetype="dashed")+ labs(x=" ") +
     labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"), 
     strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines")) +
     scale_colour_manual(values = c("Coastal" = "#8ca8c4", "Lowland"="#385438", "Highland"="#8c3800", "Altiplano"="#e0a81c")) + 
     theme(legend.text=element_text(size=12),legend.title=element_text(size=14))

 p 

ggsave("Target_results.pdf", p, width=10, height=4.4, device = cairo_pdf)

################################################################################# Unstandardized here
load("df_SU.RData")
load("df_SC.RData")
load("df_BS.RData")
load("df_TB.RData")


df = rbind(df_TB,df_SC,df_BS,df_SU)

 df$Location = factor(df$Site)
 levels(df$Location) = c("Coastal", "Lowland", "Altiplano", "Highland")
 df$Location = factor(df$Location, levels=c("Coastal", "Lowland", "Highland", "Altiplano"))



############################################################## SRM effects

exclude=c(                    
"focal effects sd",                                            
"target effects sd",                                            
"dyadic effects sd",                                                         
"focal-target effects rho (generalized recipocity)",    
"dyadic effects rho (dyadic recipocity)") 

exclude2=c(                    
"focal effects sd",                                            
"target effects sd",                                            
"dyadic effects sd"
)


df_M = df[which(df$Variable %in% exclude ),]

df_M$Variable = factor(df_M$Variable, levels=exclude)


df_M$Mode = ifelse(df_M$Variable %in% exclude2, "Variation", "Correlation")

levels(df_M$Variable) = c(                    
"Focal effects, σ",                                            
"Target effects, σ",                                            
"Dyadic effects, σ",                                                         
"Focal-target effects, ρ",    
"Dyadic effects, ρ") 

p <- ggplot(df_M ,aes(x=Variable,y=Median,ymin=LI,ymax=HI,color=Location))+ 
     geom_linerange(size=1.5,aes(color=Location), position = position_dodge(-.5))+geom_point(size=2,aes(color=Location), position = position_dodge(-.5))+
     facet_grid(. ~ Mode  ,scales="free",space = "free_y")+geom_hline(aes(yintercept=0),color="black",linetype="dashed")+ labs(x=" ") +
     labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"), 
     strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines")) +
     scale_colour_manual(values = c("Coastal" = "#8ca8c4", "Lowland"="#385438", "Highland"="#8c3800", "Altiplano"="#e0a81c")) + 
     theme(legend.text=element_text(size=12),legend.title=element_text(size=14))

 p  

ggsave("Corr_results.pdf", p, width=10, height=3.05, device = cairo_pdf)




############################################################# Sex effects
load("df_BS_sex.RData")
load("df_TB_sex.RData")
load("df_SU_sex.RData")
load("df_SC_sex.RData")

 df_so = rbind(sex_df_diff_tb, sex_df_diff_bs, sex_df_diff_su, sex_df_diff_sc)

 df_so$Location = factor(df_so$Site)
 levels(df_so$Location) = c("Coastal", "Lowland", "Altiplano", "Highland")
 df_so$Location = factor(df_so$Location, levels=c("Coastal", "Lowland", "Highland", "Altiplano"))

 df_so$Median = NA 
 df_so$SD = NA        
 df_so$SubModel = "Other estimates"    
 df_so$Submodel = "Other estimates" 
 df_so$Diff = NA      
 df_so$SubModel2 = "Sex effects"
 df_so$Type = "Contrast"

df_S = df[which(df$Variable  %in%  c("offset, MALE to MALE", "offset, MALE to FEMALE", "offset, FEMALE to MALE", "offset, FEMALE to FEMALE")),]
df_S$SubModel2 = "Sex effects"

df_S$Variable = factor(df_S$Variable)
df_S$Type = "Estimates"

df_S = rbind(df_so,df_S)

df_S$Variable = factor(df_S$Variable)

levels(df_S$Variable) = c("Female to Female", "Female to Male",   "Male to Female",   "Male to Male" ) 

 data_hline =  data.frame(Type = c("Contrast","Estimates"), 
                          hline = c(0, NA))                                             
  

p <- ggplot(df_S ,aes(x=Variable,y=Mean,ymin=LI,ymax=HI,color=Location))+ 
     geom_linerange(size=1.5,aes(color=Location), position = position_dodge(-.5))+
     geom_point(size=2, aes(color=Location), position = position_dodge(-.5))+
     facet_grid(Location ~ Type ,scales="free",space = "free_y")+ labs(x=" ") +
     labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"), 
     strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines")) +
     scale_colour_manual(values = c("Coastal" = "#8ca8c4", "Lowland"="#385438", "Highland"="#8c3800", "Altiplano"="#e0a81c")) + 
     theme(legend.text=element_text(size=12),legend.title=element_text(size=14)) + theme(legend.position = "none") +
     geom_hline(data = data_hline,  aes(yintercept = hline),linetype="dashed") #+geom_hline(aes(yintercept=0),color="black",linetype="dashed")

 p 

ggsave("Sex_results.pdf", p, width=6.7, height=4, device = cairo_pdf)


############################################################# Religion effects

load("df_BS_rel.RData")
load("df_TB_rel.RData")
load("df_SU_rel.RData")
load("df_SC_rel.RData")

 df_re = rbind(rel_df_diff_tb, rel_df_diff_bs, rel_df_diff_su, rel_df_diff_sc)

 df_re$Location = factor(df_re$Site)
 levels(df_re$Location) = c("Coastal", "Lowland", "Altiplano", "Highland")
 df_re$Location = factor(df_re$Location, levels=c("Coastal", "Lowland", "Highland", "Altiplano"))

 df_re$Median = NA 
 df_re$SD = NA        
 df_re$SubModel = "Other estimates"    
 df_re$Submodel = "Other estimates" 
 df_re$Diff = NA      
 df_re$SubModel2 = "Sex effects"
 df_re$Type = "Contrast"

df_R = df[which(df$Variable  %in%  c( "offset, CATHOLIC to CATHOLIC", "offset, CATHOLIC to NONE", "offset, CATHOLIC to OTHER",
                                      "offset, NONE to CATHOLIC", "offset, NONE to NONE","offset, NONE to OTHER",
                                      "offset, OTHER to CATHOLIC", "offset, OTHER to NONE", "offset, OTHER to OTHER")),]
df_R$SubModel2 = "Religion effects"

df_R$Variable = factor(df_R$Variable)

df_R$Type = "Estimates"

df_R = rbind(df_re,df_R)

df_R$Variable = factor(df_R$Variable)

levels(df_R$Variable) = c("Catholic to Catholic", "Catholic to None", "Catholic to Other",                               
                          "None to Catholic", "None to None", "None to Other",
                          "Other to Catholic", "Other to None", "Other to Other"   ) 

 data_hline =  data.frame(Type = c("Contrast","Estimates"), 
                          hline = c(0, NA))   

p <- ggplot(df_R ,aes(x=Variable,y=Mean,ymin=LI,ymax=HI,color=Location))+ 
     geom_linerange(size=1.5,aes(color=Location), position = position_dodge(-.5))+geom_point(size=2,aes(color=Location), position = position_dodge(-.5))+
     facet_grid(Location ~ Type ,scales="free",space = "free_y")+ labs(x=" ") +
     labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"), 
     strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines")) +
     scale_colour_manual(values = c("Coastal" = "#8ca8c4", "Lowland"="#385438", "Highland"="#8c3800", "Altiplano"="#e0a81c")) + 
     theme(legend.text=element_text(size=12),legend.title=element_text(size=14)) + theme(legend.position = "none") + 
     geom_hline(data = data_hline,  aes(yintercept = hline),linetype="dashed") #+geom_hline(aes(yintercept=0),color="black",linetype="dashed")

 p  
ggsave("Religion_results.pdf", p, width=6.2, height=6.7, device = cairo_pdf)



###################################################### Ethnicity effects


load("df_BS_eth.RData")
load("df_TB_eth.RData")
load("df_SU_eth.RData")
load("df_SC_eth.RData")

 df_et = rbind(eth_df_diff_tb, eth_df_diff_bs, eth_df_diff_su, eth_df_diff_sc)

 df_et$Location = factor(df_et$Site)
 levels(df_et$Location) = c("Coastal", "Lowland", "Altiplano", "Highland")
 df_et$Location = factor(df_et$Location, levels=c("Coastal", "Lowland", "Highland", "Altiplano"))

 df_et$Median = NA 
 df_et$SD = NA        
 df_et$SubModel = "Other estimates"    
 df_et$Submodel = "Other estimates" 
 df_et$Diff = NA      
 df_et$SubModel2 = "Sex effects"
 df_et$Type = "Contrast"

df_E = df[which(df$Variable  %in%  c( "offset, AFROCOLOMBIAN to AFROCOLOMBIAN", "offset, AFROCOLOMBIAN to EMBERA", "offset, AFROCOLOMBIAN to MESTIZO",                 
                                      "offset, COLOMBIAN to COLOMBIAN", "offset, COLOMBIAN to VENEZUELAN",                  
                                      "offset, EMBERA to AFROCOLOMBIAN", "offset, EMBERA to EMBERA", "offset, EMBERA to MESTIZO",                        
                                      "offset, MESTIZO to AFROCOLOMBIAN", "offset, MESTIZO to EMBERA", "offset, MESTIZO to MESTIZO",
                                      "offset, VENEZUELAN to COLOMBIAN", "offset, VENEZUELAN to VENEZUELAN")),]
df_E$SubModel2 = "Ethnicity effects"

df_E$Variable = factor(df_E$Variable)

df_E$Type = "Estimates"

df_E = rbind(df_et,df_E)

df_E$Variable = factor(df_E$Variable)

levels(df_E$Variable) = c("Afro. to Afro.", "Afro. to Embera", "Afro. to Mestizo",                 
                          "Colomb. to Colomb.", "Colomb. to Venez.",                  
                          "Embera to Afro.", "Embera to Embera", "Embera to Mestizo",                        
                          "Mestizo to Afro.", "Mestizo to Embera", "Mestizo to Mestizo",
                          "Venez. to Colomb.", "Venez. to Venez."  ) 

 data_hline =  data.frame(Type = c("Contrast","Estimates"), 
                          hline = c(0, NA))  

p <- ggplot(df_E ,aes(x=Variable,y=Mean,ymin=LI,ymax=HI,color=Location))+ 
     geom_linerange(size=1.5,aes(color=Location), position = position_dodge(-.5))+geom_point(size=2,aes(color=Location), position = position_dodge(-.5))+
     facet_grid(Location ~ Type ,scales="free",space = "free_y")+ labs(x=" ") +
     labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"), 
     strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines")) +
     scale_colour_manual(values = c("Coastal" = "#8ca8c4", "Lowland"="#385438", "Highland"="#8c3800", "Altiplano"="#e0a81c")) + 
     theme(legend.text=element_text(size=12),legend.title=element_text(size=14))+ theme(legend.position = "none") + 
     geom_hline(data = data_hline,  aes(yintercept = hline),linetype="dashed") #+geom_hline(aes(yintercept=0),color="black",linetype="dashed")

 p  
ggsave("Ethnicity_results.pdf", p, width=6, height=6.9, device = cairo_pdf)



