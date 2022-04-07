
##################################################### Load packages
library(igraph)
library(qgraph)
library(rethinking)
library(Rlab)
library(parallel)
library(cmdstanr)
library(reshape2)
library(plyr)
library(kinship2)
library(geosphere)
library(GGally)
library(network)
library(ggplot2)
library(rethinking)
library(colorspace)
library(parallel)
library(Cairo)
library(infotheo)
library(dirmult)
library(STRAND)

setwd("./network-measurement/Code/")
#setwd("C:\\Users\\cody_ross\\Dropbox\\Open Papers\\Network Measurements\\Code")
setwd("C:\\Users\\Mind Is Moving\\Dropbox\\Completed and Published Projects\\1-papers\\Network Measurements\\Code")

####
# Load data
load("ColombianDataWithImputations.RData")
set.seed(1)

# Prepare for Stan
N = length(Age)
group_ids = data.frame(Ethnicity=factor(Ethnicity), Sex=factor(ifelse(Male==1,"Male","Female")))

net = list(TransferOut=Friends*magic)

ind = data.frame(Age=center(Age), GoodsValues=center(GoodsValues), Education=center(Education), Male=Male,
                                  CantWork=CantWork, GripStrength=center(Grip), NoFood=NoFood)
dyad = list(Relatedness=Relatedness)

model_dat = make_strand_data(self_report=net, outcome_mode="binomial", ground_truth=NULL, block_covariates=group_ids, individual_covariates=ind, dyadic_covariates=dyad)

fit1 = fit_block_model(data=model_dat,
                       block_regression = ~ Ethnicity + Sex,
                       focal_regression = ~ Age + GoodsValues + NoFood + CantWork + GripStrength,
                       target_regression = ~ Age + GoodsValues + NoFood + CantWork + GripStrength,
                       dyad_regression = ~ Relatedness,
                       mode="mcmc",
                       return_latent_network = TRUE,
                       stan_mcmc_parameters = list(seed = 1, chains = 1, parallel_chains = 1, refresh = 1, iter_warmup = 1000,
                       iter_sampling = 1000, max_treedepth = NULL, adapt_delta = NULL)
                                     )

res1 = summarize_strand_results(fit1)

###################################### Plotting
diag(Outgoing_Transfers) = 0
diag(Incoming_Transfers) = 0

outgoing = graph_from_adjacency_matrix(Outgoing_Transfers, mode = c("directed"))
incoming = graph_from_adjacency_matrix(Incoming_Transfers, mode = c("directed"))

reported_either <- ifelse(Outgoing_Transfers==1 | t(Incoming_Transfers)==1, 1,0)
reported_both <- ifelse(Outgoing_Transfers==1 & t(Incoming_Transfers)==1, 1,0)

union <- graph_from_adjacency_matrix(reported_either, mode = c("directed"))
intersection <- graph_from_adjacency_matrix(reported_both, mode = c("directed"))

latent_res = graph_from_adjacency_matrix(round(apply(res1$samples$latent_network_sample,2:3,median )), mode = c("directed"))

coords <- layout_with_kk(union)

V(latent_res)$color <-V(intersection)$color <- V(union)$color <- V(outgoing)$color <- V(incoming)$color <- c("gray13","turquoise4",  "goldenrod3")[group_ids]
V(latent_res)$frame.color <-V(intersection)$frame.color <- V(union)$frame.color <- V(outgoing)$frame.color <- V(incoming)$frame.color <- c("gray13", "turquoise4", "goldenrod3")[group_ids]


# plot true out network
Cairo(file="colombia_outgoing.png", 
      type="png",
      units="in", 
      width=6, 
      height=6, 
      pointsize=36, 
      dpi=150)

par(mar=c(1,1,1,1)+0)
plot(outgoing, edge.arrow.size =0.1, edge.curved = 0.3,
                  vertex.label=NA, vertex.size = 3, #vertex.color="#252525", vertex.frame.color="#252525",
                  layout = coords, seed = 3)

dev.off()

# plot true out network
Cairo(file="colombia_incoming.png", 
      type="png",
      units="in", 
      width=6, 
      height=6, 
      pointsize=36, 
      dpi=150)

par(mar=c(1,1,1,1)+0)
plot(incoming, edge.arrow.size =0.1, edge.curved = 0.3,
                  vertex.label=NA, vertex.size = 3, #vertex.color="#252525", vertex.frame.color="#252525",
                  layout = coords, seed = 3)

dev.off()

# plot true out network
Cairo(file="colombia_intersection.png", 
      type="png",
      units="in", 
      width=6, 
      height=6, 
      pointsize=36, 
      dpi=150)

par(mar=c(1,1,1,1)+0)
plot(intersection, edge.arrow.size =0.1, edge.curved = 0.3,
                  vertex.label=NA, vertex.size = 3, #vertex.color="#252525", vertex.frame.color="#252525",
                  layout = coords, seed = 3)
dev.off()


# plot true out network
Cairo(file="colombia_union.png", 
      type="png",
      units="in", 
      width=6, 
      height=6, 
      pointsize=36, 
      dpi=150)

par(mar=c(1,1,1,1)+0)
plot(union, edge.arrow.size =0.1, edge.curved = 0.3,
                  vertex.label=NA, vertex.size = 3, #vertex.color="#252525", vertex.frame.color="#252525",
                  layout = coords, seed = 3)
dev.off()

# plot true out network
Cairo(file="colombia_latent.png", 
      type="png",
      units="in", 
      width=6, 
      height=6, 
      pointsize=36, 
      dpi=150)

par(mar=c(1,1,1,1)+0)
plot(latent_res, edge.arrow.size =0.1, edge.curved = 0.3,
                  vertex.label=NA, vertex.size = 3, #vertex.color="#252525", vertex.frame.color="#252525",
                  layout = coords, seed = 3)
dev.off()




  vis1 = strand_caterpillar_plot(res1, submodel=c("Focal efffects: Out-degree","Target effects: In-degree","Dyadic effects"), normalized=TRUE)
  ggsave("Colombia_slopes_latent.pdf", vis1, width=8,height=8)

  vis2 = strand_caterpillar_plot(res1, submodel=c("False positive rate", "Recall of true ties","Theta: question-order effects"), normalized=TRUE)
  ggsave("Colombia_slopes_measurement.pdf", vis2, width=8,height=8)

  vis3 = strand_caterpillar_plot(res1, submodel=c("False positive rate", "Recall of true ties","Theta: question-order effects"), only_slopes=FALSE, 
                                  normalized=FALSE, only_technicals=TRUE)
  ggsave("Colombia_intercepts_measurement.pdf", vis3, width=8,height=4)

            












data=model_dat
                          block_regression= ~ Ethnicity + Sex
                          focal_regression = ~ Age + GoodsValues + NoFood + CantWork + GripStrength + Depressed
                          target_regression = ~ Age + GoodsValues + NoFood + CantWork + GripStrength + Depressed
                          dyad_regression = ~ Relatedness + Friends
                          mode="mcmc"
                          return_latent_network=FALSE
                          stan_mcmc_parameters = list(seed = 1, chains = 1, parallel_chains = 1, refresh = 1, iter_warmup = NULL,
                                                       iter_sampling = NULL, max_treedepth = NULL, adapt_delta = NULL)
                          priors=NULL










                          fit = fit_block_plus_social_relations_model( data=model_dat,
                                             block_regression = ~ Ethnicity,
                                             focal_regression = ~ 1,
                                             target_regression = ~ 1,
                                             dyad_regression = ~ 1,
                                             mode="mcmc",
                                             stan_mcmc_parameters = list(seed = 1, chains = 1, parallel_chains = 1, refresh = 1, iter_warmup = 600,
                                                                         iter_sampling = 600, max_treedepth = NULL, adapt_delta = NULL)
                                             )