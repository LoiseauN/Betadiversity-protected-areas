rm(list = ls())

## Library ----

library(stringr)
library(ggplot2)
library(grid)
library(png)
## Source multiplot ----

source(here::here("Function", "multiplot.R"))
## Import Icons ----

paths <- list.files(path = here::here("Outputs", "Figure_S2"), pattern = "*.png$", 
                    full.names = TRUE)

files <- list.files(path = here::here("Outputs", "Figure_S2"), pattern = "*.png$", 
                    full.names = FALSE)

all_im <- lapply(paths, png::readPNG)
names(all_im) <- gsub(".png", "", files)

## Prepare Data ----

filenames <- list.files(path = here::here("Outputs", "Figure_S2"), pattern = ".RData$",
                        full.names = FALSE)

files <- list.files(path = here::here("Outputs", "Figure_S2"), pattern = ".RData$",
                    full.names = TRUE)

lapply(files, load, environment())



#################################FISH##################################################################################################### 
Closed_NP50km<-as.data.frame(c(apply(SES_Fish_BetaClose_NonP_TOT_50km,1,mean),apply(SES_Fish_BetaClose_NonP_TUR_50km,1,mean),apply(SES_Fish_BetaClose_NonP_NES_50km,1,mean)))
Closed_NP50km$Components<-factor(c(rep("Total",dim(SES_Fish_BetaClose_NonP_TOT_50km)[1]),rep("Turnover",dim(SES_Fish_BetaClose_NonP_TOT_50km)[1]),rep("Nestedness",dim(SES_Fish_BetaClose_NonP_TOT_50km)[1])))
colnames(Closed_NP50km)<-c("beta","Components")

data<-Closed_NP50km
data$scale <-factor(c(rep("Strict vs OA",dim(Closed_NP50km)[1])))

#Pour conserver l'ordre des boxplot
data$scale<- factor(data$scale,levels = c('Strict vs OA'),ordered = TRUE)
data$Components<- factor(data$Components,levels = c('Total','Turnover','Nestedness'),ordered = TRUE)

data$dist_cat_n[data$Components == "Total"] <- -0.25
data$dist_cat_n[data$Components == "Turnover"] <- 0
data$dist_cat_n[data$Components == "Nestedness"] <- 0.25

data$scat_adj[data$Components == "Total"] <- 1.25
data$scat_adj[data$Components == "Turnover"] <- 2
data$scat_adj[data$Components == "Nestedness"] <- 2.75

p1<-ggplot(data, aes(Components,beta,colour=Components)) +
  ylim(-4,4)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  geom_hline(yintercept=1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=- 1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=0,linetype="dotted",size=1.4) +
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "50 km",
       x = "",
       y = "SPA vs NPA",
       colour = "Components") +
  theme_bw() +  annotation_custom(rasterGrob(all_im$Fish),  xmin = 2.7, xmax = 3.7, ymin = 3, ymax = 4.1)




#################################BIRD##################################################################################################### 

Closed_NP50km<-as.data.frame(c(apply(ses.tot_50km_Bird_Close_NonP,1,mean),apply(ses.tur_50km_Bird_Close_NonP,1,mean),apply(ses.nes_50km_Bird_Close_NonP,1,mean)))
Closed_NP50km$Components<-factor(c(rep("Total",dim(ses.tot_50km_Bird_Close_NonP)[1]),rep("Turnover",dim(ses.tot_50km_Bird_Close_NonP)[1]),rep("Nestedness",dim(ses.tot_50km_Bird_Close_NonP)[1])))
colnames(Closed_NP50km)<-c("beta","Components")

data<-Closed_NP50km
data$scale <-factor(c(rep("Strict vs OA",dim(Closed_NP50km)[1])))

#Pour conserver l'ordre des boxplot
data$scale<- factor(data$scale,levels = c('Strict vs OA'),ordered = TRUE)
data$Components<- factor(data$Components,levels = c('Total','Turnover','Nestedness'),ordered = TRUE)

data$dist_cat_n[data$Components == "Total"] <- -0.25
data$dist_cat_n[data$Components == "Turnover"] <- 0
data$dist_cat_n[data$Components == "Nestedness"] <- 0.25

data$scat_adj[data$Components == "Total"] <- 1.25
data$scat_adj[data$Components == "Turnover"] <- 2
data$scat_adj[data$Components == "Nestedness"] <- 2.75

p2<-ggplot(data, aes(Components,beta,colour=Components)) +
  ylim(-4,4)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  geom_hline(yintercept=1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=- 1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=0,linetype="dotted",size=1.4) +
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "",
       colour = "Components") +
  theme_bw() +  annotation_custom(rasterGrob(all_im$Bird),  xmin = 2.7, xmax = 3.7, ymin = 3, ymax = 4.1)

#+
#theme(panel.grid.major = element_blank(),
#      panel.grid.minor = element_blank())

#################################PLANT##################################################################################################### 

Closed_NP50km<-as.data.frame(c(apply(ses.tot_Close_NonP_50km,1,mean),apply(ses.tur_Close_NonP_50km,1,mean),apply(ses.nes_Close_NonP_50km,1,mean)))
Closed_NP50km$Components<-factor(c(rep("Total",dim(ses.tot_Close_NonP_50km)[1]),rep("Turnover",dim(ses.tot_Close_NonP_50km)[1]),rep("Nestedness",dim(ses.tot_Close_NonP_50km)[1])))
colnames(Closed_NP50km)<-c("beta","Components")

data<-Closed_NP50km
data$scale <-factor(c(rep("Strict vs OA",dim(Closed_NP50km)[1])))

#Pour conserver l'ordre des boxplot
data$scale<- factor(data$scale,levels = c('Strict vs OA'),ordered = TRUE)
data$Components<- factor(data$Components,levels = c('Total','Turnover','Nestedness'),ordered = TRUE)

data$dist_cat_n[data$Components == "Total"] <- -0.25
data$dist_cat_n[data$Components == "Turnover"] <- 0
data$dist_cat_n[data$Components == "Nestedness"] <- 0.25

data$scat_adj[data$Components == "Total"] <- 1.25
data$scat_adj[data$Components == "Turnover"] <- 2
data$scat_adj[data$Components == "Nestedness"] <- 2.75

p3<-ggplot(data, aes(Components,beta,colour=Components)) +
  ylim(-4,4)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  geom_hline(yintercept=1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=- 1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=0,linetype="dotted",size=1.4) +
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "",
       colour = "Components") +
  theme_bw() +  annotation_custom(rasterGrob(all_im$Plant),  xmin = 2.7, xmax = 3.7, ymin = 3, ymax = 4.1)

###################################################################################################################################### 
###################################################################################################################################### 
#################################RESTRICTED VS UNPROTECTED#############################################################################
###################################################################################################################################### 
######################################################################################################################################


#################################FISH##################################################################################################### 
Closed_NP50km<-as.data.frame(c(apply(SES_Fish_Betarestricte_NonP_TOT_50km,1,mean),apply(SES_Fish_Betarestricte_NonP_TUR_50km,1,mean),apply(SES_Fish_Betarestricte_NonP_NES_50km,1,mean)))
Closed_NP50km$Components<-factor(c(rep("Total",dim(SES_Fish_Betarestricte_NonP_TOT_50km)[1]),rep("Turnover",dim(SES_Fish_Betarestricte_NonP_TOT_50km)[1]),rep("Nestedness",dim(SES_Fish_Betarestricte_NonP_TOT_50km)[1])))
colnames(Closed_NP50km)<-c("beta","Components")

data<-Closed_NP50km
data$scale <-factor(c(rep("Restrict vs OA",dim(Closed_NP50km)[1])))

#Pour conserver l'ordre des boxplot
data$scale<- factor(data$scale,levels = c('Restrict vs OA'),ordered = TRUE)
data$Components<- factor(data$Components,levels = c('Total','Turnover','Nestedness'),ordered = TRUE)

data$dist_cat_n[data$Components == "Total"] <- -0.25
data$dist_cat_n[data$Components == "Turnover"] <- 0
data$dist_cat_n[data$Components == "Nestedness"] <- 0.25

data$scat_adj[data$Components == "Total"] <- 1.25
data$scat_adj[data$Components == "Turnover"] <- 2
data$scat_adj[data$Components == "Nestedness"] <- 2.75

p4<-ggplot(data, aes(Components,beta,colour=Components)) +
  ylim(-4,4)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  geom_hline(yintercept=1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=- 1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=0,linetype="dotted",size=1.4) +
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "RA vs NPA",
       colour = "Components") +
  theme_bw()





#################################BIRD##################################################################################################### 
Closed_NP50km<-as.data.frame(c(apply(ses.tot_50km_Bird_restricte_NonP,1,mean),apply(ses.tur_50km_Bird_restricte_NonP,1,mean),apply(ses.nes_50km_Bird_restricte_NonP,1,mean)))
Closed_NP50km$Components<-factor(c(rep("Total",dim(ses.tot_50km_Bird_restricte_NonP)[1]),rep("Turnover",dim(ses.tot_50km_Bird_restricte_NonP)[1]),rep("Nestedness",dim(ses.tot_50km_Bird_restricte_NonP)[1])))
colnames(Closed_NP50km)<-c("beta","Components")

data<-Closed_NP50km
data$scale <-factor(c(rep("Strict vs OA",dim(Closed_NP50km)[1])))

#Pour conserver l'ordre des boxplot
data$scale<- factor(data$scale,levels = c('Strict vs OA'),ordered = TRUE)
data$Components<- factor(data$Components,levels = c('Total','Turnover','Nestedness'),ordered = TRUE)

data$dist_cat_n[data$Components == "Total"] <- -0.25
data$dist_cat_n[data$Components == "Turnover"] <- 0
data$dist_cat_n[data$Components == "Nestedness"] <- 0.25

data$scat_adj[data$Components == "Total"] <- 1.25
data$scat_adj[data$Components == "Turnover"] <- 2
data$scat_adj[data$Components == "Nestedness"] <- 2.75

p5<-ggplot(data, aes(Components,beta,colour=Components)) +
  ylim(-4,4)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  geom_hline(yintercept=1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=- 1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=0,linetype="dotted",size=1.4) +
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "",
       colour = "Components") +
  theme_bw()

#################################PLANT##################################################################################################### 

Closed_NP50km<-as.data.frame(c(apply(ses.tot_Restricted_NonP_50km,1,mean),apply(ses.tur_Restricted_NonP_50km,1,mean),apply(ses.nes_Restricted_NonP_50km,1,mean)))
Closed_NP50km$Components<-factor(c(rep("Total",dim(ses.tot_Restricted_NonP_50km)[1]),rep("Turnover",dim(ses.tot_Restricted_NonP_50km)[1]),rep("Nestedness",dim(ses.tot_Restricted_NonP_50km)[1])))
colnames(Closed_NP50km)<-c("beta","Components")

data<-Closed_NP50km
data$scale <-factor(c(rep("Strict vs OA",dim(Closed_NP50km)[1])))

#Pour conserver l'ordre des boxplot
data$scale<- factor(data$scale,levels = c('Strict vs OA'),ordered = TRUE)
data$Components<- factor(data$Components,levels = c('Total','Turnover','Nestedness'),ordered = TRUE)

data$dist_cat_n[data$Components == "Total"] <- -0.25
data$dist_cat_n[data$Components == "Turnover"] <- 0
data$dist_cat_n[data$Components == "Nestedness"] <- 0.25

data$scat_adj[data$Components == "Total"] <- 1.25
data$scat_adj[data$Components == "Turnover"] <- 2
data$scat_adj[data$Components == "Nestedness"] <- 2.75

p6<-ggplot(data, aes(Components,beta,colour=Components)) +
  ylim(-4,4)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  geom_hline(yintercept=1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=- 1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=0,linetype="dotted",size=1.4) +
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "",
       colour = "Components") +
  theme_bw() 
###################################################################################################################################### 
###################################################################################################################################### 
#################################PROTECTED VS RESTRICTED #############################################################################
###################################################################################################################################### 
######################################################################################################################################

#################################FISH##################################################################################################### 
Closed_RESTRICT50km<-as.data.frame(c(apply(SES_Fish_BetaClose_RESTRICT_TOT_50km,1,mean),apply(SES_Fish_BetaClose_RESTRICT_TUR_50km,1,mean),apply(SES_Fish_BetaClose_RESTRICT_NES_50km,1,mean)))
Closed_RESTRICT50km$Components<-factor(c(rep("Total",dim(SES_Fish_BetaClose_RESTRICT_TOT_50km)[1]),rep("Turnover",dim(SES_Fish_BetaClose_RESTRICT_TOT_50km)[1]),rep("Nestedness",dim(SES_Fish_BetaClose_RESTRICT_TOT_50km)[1])))
colnames(Closed_RESTRICT50km)<-c("beta","Components")

data<-Closed_RESTRICT50km
data$scale <-factor(c(rep("Strict vs OA",dim(Closed_RESTRICT50km)[1])))

#Pour conserver l'ordre des boxplot
data$scale<- factor(data$scale,levels = c('Strict vs OA'),ordered = TRUE)
data$Components<- factor(data$Components,levels = c('Total','Turnover','Nestedness'),ordered = TRUE)

data$dist_cat_n[data$Components == "Total"] <- -0.25
data$dist_cat_n[data$Components == "Turnover"] <- 0
data$dist_cat_n[data$Components == "Nestedness"] <- 0.25

data$scat_adj[data$Components == "Total"] <- 1.25
data$scat_adj[data$Components == "Turnover"] <- 2
data$scat_adj[data$Components == "Nestedness"] <- 2.75

p7<-ggplot(data, aes(Components,beta,colour=Components)) +
  ylim(-4,4)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  geom_hline(yintercept=1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=- 1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=0,linetype="dotted",size=1.4) +
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "SPA vs RA",
       colour = "Components") +
  theme_bw() 





#################################BIRD##################################################################################################### 

Closed_RESTRICT50km<-as.data.frame(c(apply(ses.tot_50km_Bird_Close_RESTRICT,1,mean),apply(ses.tur_50km_Bird_Close_RESTRICT,1,mean),apply(ses.nes_50km_Bird_Close_RESTRICT,1,mean)))
Closed_RESTRICT50km$Components<-factor(c(rep("Total",dim(ses.tot_50km_Bird_Close_RESTRICT)[1]),rep("Turnover",dim(ses.tot_50km_Bird_Close_RESTRICT)[1]),rep("Nestedness",dim(ses.tot_50km_Bird_Close_RESTRICT)[1])))
colnames(Closed_RESTRICT50km)<-c("beta","Components")

data<-Closed_RESTRICT50km
data$scale <-factor(c(rep("Strict vs OA",dim(Closed_RESTRICT50km)[1])))

#Pour conserver l'ordre des boxplot
data$scale<- factor(data$scale,levels = c('Strict vs OA'),ordered = TRUE)
data$Components<- factor(data$Components,levels = c('Total','Turnover','Nestedness'),ordered = TRUE)

data$dist_cat_n[data$Components == "Total"] <- -0.25
data$dist_cat_n[data$Components == "Turnover"] <- 0
data$dist_cat_n[data$Components == "Nestedness"] <- 0.25

data$scat_adj[data$Components == "Total"] <- 1.25
data$scat_adj[data$Components == "Turnover"] <- 2
data$scat_adj[data$Components == "Nestedness"] <- 2.75

p8<-ggplot(data, aes(Components,beta,colour=Components)) +
  ylim(-4,4)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  geom_hline(yintercept=1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=- 1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=0,linetype="dotted",size=1.4) +
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "",
       colour = "Components") +
  theme_bw() 


#################################PLANT##################################################################################################### 

Closed_NP50km<-as.data.frame(c(apply(ses.tot_Close_Restrict_50km,1,mean),apply(ses.tur_Close_Restrict_50km,1,mean),apply(ses.nes_Close_Restrict_50km,1,mean)))
Closed_NP50km$Components<-factor(c(rep("Total",dim(ses.tot_Close_Restrict_50km)[1]),rep("Turnover",dim(ses.tot_Close_Restrict_50km)[1]),rep("Nestedness",dim(ses.tot_Close_Restrict_50km)[1])))
colnames(Closed_NP50km)<-c("beta","Components")

data<-Closed_NP50km
data$scale <-factor(c(rep("Strict vs OA",dim(Closed_NP50km)[1])))

#Pour conserver l'ordre des boxplot
data$scale<- factor(data$scale,levels = c('Strict vs OA'),ordered = TRUE)
data$Components<- factor(data$Components,levels = c('Total','Turnover','Nestedness'),ordered = TRUE)

data$dist_cat_n[data$Components == "Total"] <- -0.25
data$dist_cat_n[data$Components == "Turnover"] <- 0
data$dist_cat_n[data$Components == "Nestedness"] <- 0.25

data$scat_adj[data$Components == "Total"] <- 1.25
data$scat_adj[data$Components == "Turnover"] <- 2
data$scat_adj[data$Components == "Nestedness"] <- 2.75

p9<-ggplot(data, aes(Components,beta,colour=Components)) +
  ylim(-4,4)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  geom_hline(yintercept=1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=- 1.96,linetype="dotted",size=0.7) +
  geom_hline(yintercept=0,linetype="dotted",size=1.4) +
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "",
       colour = "Components") +
  theme_bw() 



multiplot(p1,p7,p4,p2,p8,p5,p3,p9,p6,cols=3)
