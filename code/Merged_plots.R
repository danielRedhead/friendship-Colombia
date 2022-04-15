



setwd("C:\\Users\\Mind Is Moving\\Desktop\\friendship-Colombia-main")       # working directory for apple

################################################################################# Standardized here
load("df_SUs.RData")
load("df_SCs.RData")
load("df_BSs.RData")
load("df_TBs.RData")


df = rbind(df_TBs,df_BSs,df_SUs,df_SCs)

 df$Location = factor(df$Site)
 levels(df$Location) = c("Coast", "Lowland", "Altiplano", "Highland")


############################################################## Dyadic effects
df_D = df[which(df$SubModel  %in% c("Dyadic effects")),]

df_D$Variable = factor(df_D$Variable)

levels(df_D$Variable) = c(
"Age distance",    "Attactivness distance",
"BMI distance",    "Education distance",  
"Spatial distance",   "Political distance", 
"Relatedness", "Sharing ties",    
"Wealth distance")

p <- ggplot(df_D ,aes(x=Variable,y=Median,ymin=LI,ymax=HI,color=Location))+ 
     geom_linerange(size=1.5,aes(color=Location), position = position_dodge(-.5))+geom_point(size=2,aes(color=Location), position = position_dodge(-.5))+
     facet_grid(SubModel ~ . ,scales="free",space = "free_y")+geom_hline(aes(yintercept=0),color="black",linetype="dashed")+ labs(x=" ") +
     labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"), 
     strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines")) +
     scale_colour_manual(values = c("Coast" = "#8ca8c4", "Lowland"="#385438", "Highland"="#8c3800", "Altiplano"="#e0a81c")) + 
     theme(legend.text=element_text(size=12),legend.title=element_text(size=14))

 p  
ggsave("Dyadic_results.pdf", p, width=10, height=4.5)




############################################################# Target effects
df_T = df[which(df$SubModel  %in% c("Target effects: In-degree")),]
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
 "Punishing score",        
 "RS",            
 "Wealth"
)

p <- ggplot(df_T ,aes(x=Variable,y=Median,ymin=LI,ymax=HI,color=Location))+ 
     geom_linerange(size=1.5,aes(color=Location), position = position_dodge(-.5))+geom_point(size=2,aes(color=Location), position = position_dodge(-.5))+
     facet_grid(SubModel2 ~ . ,scales="free",space = "free_y")+geom_hline(aes(yintercept=0),color="black",linetype="dashed")+ labs(x=" ") +
     labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"), 
     strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines")) +
     scale_colour_manual(values = c("Coast" = "#8ca8c4", "Lowland"="#385438", "Highland"="#8c3800", "Altiplano"="#e0a81c")) + 
     theme(legend.text=element_text(size=12),legend.title=element_text(size=14))

 p  
ggsave("Target_results.pdf", p, width=10, height=4.5)

################################################################################# Unstandardized here
load("df_SU.RData")
load("df_SC.RData")
load("df_BS.RData")
load("df_TB.RData")


df = rbind(df_TB,df_BS,df_SU,df_SC)

 df$Location = factor(df$Site)
 levels(df$Location) = c("Coast", "Lowland", "Altiplano", "Highland")


############################################################# Sex effects
df_S = df[which(df$Variable  %in%  c("offset, MALE to MALE", "offset, MALE to FEMALE", "offset, FEMALE to MALE", "offset, FEMALE to FEMALE")),]
df_S$SubModel2 = "Sex effects"

df_S$Variable = factor(df_S$Variable)

levels(df_S$Variable) = c("Female to Female", "Female to Male",   "Male to Female",   "Male to Male" ) 

p <- ggplot(df_S ,aes(x=Variable,y=Median,ymin=LI,ymax=HI,color=Location))+ 
     geom_linerange(size=1.5,aes(color=Location), position = position_dodge(-.5))+geom_point(size=2,aes(color=Location), position = position_dodge(-.5))+
     facet_grid(Location ~ . ,scales="free",space = "free_y")+geom_hline(aes(yintercept=0),color="black",linetype="dashed")+ labs(x=" ") +
     labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"), 
     strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines")) +
     scale_colour_manual(values = c("Coast" = "#8ca8c4", "Lowland"="#385438", "Highland"="#8c3800", "Altiplano"="#e0a81c")) + 
     theme(legend.text=element_text(size=12),legend.title=element_text(size=14))

 p  
ggsave("Sex_results.pdf", p, width=8, height=4.5)


############################################################# Religion effects
df_R = df[which(df$Variable  %in%  c( "offset, CATHOLIC to CATHOLIC", "offset, CATHOLIC to NONE", "offset, CATHOLIC to OTHER",
                                      "offset, NONE to CATHOLIC", "offset, NONE to NONE","offset, NONE to OTHER",
                                      "offset, OTHER to CATHOLIC", "offset, OTHER to NONE", "offset, OTHER to OTHER")),]
df_R$SubModel2 = "Religion effects"

df_R$Variable = factor(df_R$Variable)

levels(df_R$Variable) = c("Catholic to Catholic", "Catholic to None", "Catholic to Other",                               
                          "None to Catholic", "None to None", "None to Other",
                          "Other to Catholic", "Other to None", "Other to Other"   ) 

p <- ggplot(df_R ,aes(x=Variable,y=Median,ymin=LI,ymax=HI,color=Location))+ 
     geom_linerange(size=1.5,aes(color=Location), position = position_dodge(-.5))+geom_point(size=2,aes(color=Location), position = position_dodge(-.5))+
     facet_grid(Location ~ . ,scales="free",space = "free_y")+geom_hline(aes(yintercept=0),color="black",linetype="dashed")+ labs(x=" ") +
     labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"), 
     strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines")) +
     scale_colour_manual(values = c("Coast" = "#8ca8c4", "Lowland"="#385438", "Highland"="#8c3800", "Altiplano"="#e0a81c")) + 
     theme(legend.text=element_text(size=12),legend.title=element_text(size=14))

 p  
ggsave("Religion_results.pdf", p, width=8, height=6)



###################################################### Ethnicity effects
df_E = df[which(df$Variable  %in%  c( "offset, AFROCOLOMBIAN to AFROCOLOMBIAN", "offset, AFROCOLOMBIAN to EMBERA", "offset, AFROCOLOMBIAN to MESTIZO",                 
                                      "offset, COLOMBIAN to COLOMBIAN", "offset, COLOMBIAN to VENEZUELAN",                  
                                      "offset, EMBERA to AFROCOLOMBIAN", "offset, EMBERA to EMBERA", "offset, EMBERA to MESTIZO",                        
                                      "offset, MESTIZO to AFROCOLOMBIAN", "offset, MESTIZO to EMBERA", "offset, MESTIZO to MESTIZO",
                                      "offset, VENEZUELAN to COLOMBIAN", "offset, VENEZUELAN to VENEZUELAN")),]
df_E$SubModel2 = "Ethnicity effects"

df_E$Variable = factor(df_E$Variable)

levels(df_E$Variable) = c("Afrocolombian to Afrocolombian", "Afrocolombian to Embera", "Afrocolombian to Mestizo",                 
                          "Colombian to Colombian", "Colombian to Venezuelan",                  
                          "Embera to Afrocolombian", "Embera to Embera", "Embera to Mestizo",                        
                          "Mestizo to Afrocolombian", "Mestizo to Embera", "Mestizo to Mestizo",
                          "Venezuelan to Colombian", "Venezuelan to Venezuelan"  ) 

p <- ggplot(df_E ,aes(x=Variable,y=Median,ymin=LI,ymax=HI,color=Location))+ 
     geom_linerange(size=1.5,aes(color=Location), position = position_dodge(-.5))+geom_point(size=2,aes(color=Location), position = position_dodge(-.5))+
     facet_grid(Location ~ . ,scales="free",space = "free_y")+geom_hline(aes(yintercept=0),color="black",linetype="dashed")+ labs(x=" ") +
     labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"), 
     strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines")) +
     scale_colour_manual(values = c("Coast" = "#8ca8c4", "Lowland"="#385438", "Highland"="#8c3800", "Altiplano"="#e0a81c")) + 
     theme(legend.text=element_text(size=12),legend.title=element_text(size=14))

 p  
ggsave("Ethnicity_results.pdf", p, width=8, height=6)



