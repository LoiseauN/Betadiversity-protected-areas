rm(list = ls())

## Library ----

library(stringr)
library(ggplot2)
library(grid)
library(png)
## Source multiplot ----

source(here::here("Function", "multiplot.R"))
## Import Icons ----

paths <- list.files(path = here::here("Outputs", "Figure_5"), pattern = "*.png$", 
                    full.names = TRUE)

files <- list.files(path = here::here("Outputs", "Figure_5"), pattern = "*.png$", 
                    full.names = FALSE)

all_im <- lapply(paths, png::readPNG)
names(all_im) <- gsub(".png", "", files)


## Prepare Data ----

filenames <- list.files(path = here::here("Outputs", "Figure_5"), pattern = ".RData$",
                        full.names = FALSE)

files <- list.files(path = here::here("Outputs", "Figure_5"), pattern = ".RData$",
                    full.names = TRUE)

lapply(files, load, environment())



#--- Plot
###################################################################################################################################### 
###################################################################################################################################### 
#################################PROTECTED VS UNPROTECTED#############################################################################
###################################################################################################################################### 
######################################################################################################################################
#################################FISH##################################################################################################### 
Closed_NP50km<-as.data.frame(c(apply(Fish_BetaClose_NonP_TOT_50km,1,mean),apply(Fish_BetaClose_NonP_TUR_50km,1,mean),apply(Fish_BetaClose_NonP_NES_50km,1,mean)))
Closed_NP50km$Components<-factor(c(rep("Total",dim(Fish_BetaClose_NonP_TOT_50km)[1]),rep("Turnover",dim(Fish_BetaClose_NonP_TOT_50km)[1]),rep("Nestedness",dim(Fish_BetaClose_NonP_TOT_50km)[1])))
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
  ylim(0,1)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "SPA vs NPA",
       colour = "Components") +
  theme_bw() +  annotation_custom(rasterGrob(all_im$Fish), xmin = 2.7, xmax = 3.7, ymin = 0.8, ymax = 0.97)+
  annotate("text", x=1, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=2, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=3, y=0.973, label= "*",size=6.5,fontface=2,color="gray")+theme(axis.text.x = element_text(size=12))



#################################BIRD##################################################################################################### 

Closed_NP50km<-as.data.frame(c(apply(Bird_BetaClose_NonP_TOT_50km,1,mean),apply(Bird_BetaClose_NonP_TUR_50km,1,mean),apply(Bird_BetaClose_NonP_NES_50km,1,mean)))
Closed_NP50km$Components<-factor(c(rep("Total",dim(Bird_BetaClose_NonP_TOT_50km)[1]),rep("Turnover",dim(Bird_BetaClose_NonP_TOT_50km)[1]),rep("Nestedness",dim(Bird_BetaClose_NonP_TOT_50km)[1])))
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
  ylim(0,1)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "",
       colour = "Components") +
  theme_bw() +  annotation_custom(rasterGrob(all_im$Bird),  xmin = 2.7, xmax = 3.7, ymin = 0.8, ymax = 0.97)+
  annotate("text", x=1, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=2, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=3, y=1, label= "ns",size=4)+theme(axis.text.x = element_text(size=12))

#theme(panel.grid.major = element_blank(),
#      panel.grid.minor = element_blank())

#################################PLANT##################################################################################################### 

Closed_NP50km<-as.data.frame(c(apply(Plant_BetaClose_NonP_TOT_50km,1,mean),apply(Plant_BetaClose_NonP_TUR_50km,1,mean),apply(Plant_BetaClose_NonP_NES_50km,1,mean)))
Closed_NP50km$Components<-factor(c(rep("Total",dim(Plant_BetaClose_NonP_TOT_50km)[1]),rep("Turnover",dim(Plant_BetaClose_NonP_TOT_50km)[1]),rep("Nestedness",dim(Plant_BetaClose_NonP_TOT_50km)[1])))
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
  ylim(0,1)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "",
       colour = "Components") +
  theme_bw() +  annotation_custom(rasterGrob(all_im$Plant),  xmin = 2.7, xmax = 3.7, ymin = 0.8, ymax = 0.97)+
annotate("text", x=1, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=2, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=3, y=1, label= "ns",size=4)+theme(axis.text.x = element_text(size=12))

###################################################################################################################################### 
###################################################################################################################################### 
#################################RESTRICTED VS UNPROTECTED#############################################################################
###################################################################################################################################### 
######################################################################################################################################


#################################FISH##################################################################################################### 
Closed_NP50km<-as.data.frame(c(apply(Fish_Betarestricte_NonP_TOT_50km,1,mean),apply(Fish_Betarestricte_NonP_TUR_50km,1,mean),apply(Fish_Betarestricte_NonP_NES_50km,1,mean)))
Closed_NP50km$Components<-factor(c(rep("Total",dim(Fish_Betarestricte_NonP_TOT_50km)[1]),rep("Turnover",dim(Fish_Betarestricte_NonP_TOT_50km)[1]),rep("Nestedness",dim(Fish_Betarestricte_NonP_TOT_50km)[1])))
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
  ylim(0,1)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "RA vs NPA",
       colour = "Components") +
  theme_bw() +  
  annotate("text", x=1, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=2, y=1, label= "ns",size=4,color="black")+
  annotate("text", x=3, y=1, label= "ns",size=4)+theme(axis.text.x = element_text(size=12))



#################################BIRD##################################################################################################### 
Closed_NP50km<-as.data.frame(c(apply(Bird_Betarestricte_NonP_TOT_50km,1,mean),apply(Bird_Betarestricte_NonP_TUR_50km,1,mean),apply(Bird_Betarestricte_NonP_NES_50km,1,mean)))
Closed_NP50km$Components<-factor(c(rep("Total",dim(Bird_Betarestricte_NonP_TOT_50km)[1]),rep("Turnover",dim(Bird_Betarestricte_NonP_TOT_50km)[1]),rep("Nestedness",dim(Bird_Betarestricte_NonP_TOT_50km)[1])))
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
  ylim(0,1)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "",
       colour = "Components") +
  theme_bw()+
  annotate("text", x=1, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=2, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=3, y=0.973, label= "***",size=6.5,fontface=2,color="black")+theme(axis.text.x = element_text(size=12))


#################################PLANT##################################################################################################### 

Closed_NP50km<-as.data.frame(c(apply(Plant_BetaRestricted_NonP_TOT_50km,1,mean),apply(Plant_BetaRestricted_NonP_TUR_50km,1,mean),apply(Plant_BetaRestricted_NonP_NES_50km,1,mean)))
Closed_NP50km$Components<-factor(c(rep("Total",dim(Plant_BetaRestricted_NonP_TOT_50km)[1]),rep("Turnover",dim(Plant_BetaRestricted_NonP_TOT_50km)[1]),rep("Nestedness",dim(Plant_BetaRestricted_NonP_TOT_50km)[1])))
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
  ylim(0,1)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "",
       colour = "Components") +
  theme_bw()+
  annotate("text", x=0.75, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=1.75, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=2.75, y=0.973, label= "***",size=6.5,fontface=2,color="gray")+theme(axis.text.x = element_text(size=12))   
###################################################################################################################################### 
###################################################################################################################################### 
#################################PROTECTED VS RESTRICTED #############################################################################
###################################################################################################################################### 
######################################################################################################################################

#################################FISH##################################################################################################### 
Closed_RESTRICT50km<-as.data.frame(c(apply(Fish_BetaClose_RESTRICT_TOT_50km,1,mean),apply(Fish_BetaClose_RESTRICT_TUR_50km,1,mean),apply(Fish_BetaClose_RESTRICT_NES_50km,1,mean)))
Closed_RESTRICT50km$Components<-factor(c(rep("Total",dim(Fish_BetaClose_RESTRICT_TOT_50km)[1]),rep("Turnover",dim(Fish_BetaClose_RESTRICT_TOT_50km)[1]),rep("Nestedness",dim(Fish_BetaClose_RESTRICT_TOT_50km)[1])))
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
  ylim(0,1)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "SPA vs RA",
       colour = "Components") +
  theme_bw()+
  annotate("text", x=1, y=0.973, label= "*",size=6.5,fontface=2,color="black")+
  annotate("text", x=2, y=1, label= "ns",size=4)+
  annotate("text", x=3, y=1, label= "ns",size=4)+theme(axis.text.x = element_text(size=12))


#################################BIRD##################################################################################################### 

Closed_RESTRICT50km<-as.data.frame(c(apply(Bird_BetaClose_RESTRICT_TOT_50km,1,mean),apply(Bird_BetaClose_RESTRICT_TUR_50km,1,mean),apply(Bird_BetaClose_RESTRICT_NES_50km,1,mean)))
Closed_RESTRICT50km$Components<-factor(c(rep("Total",dim(Bird_BetaClose_RESTRICT_TOT_50km)[1]),rep("Turnover",dim(Bird_BetaClose_RESTRICT_TOT_50km)[1]),rep("Nestedness",dim(Bird_BetaClose_RESTRICT_TOT_50km)[1])))
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
  ylim(0,1)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "",
       colour = "Components") +
  theme_bw()+
  annotate("text", x=1, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=2, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=3, y=0.973, label= "***",size=6.5,fontface=2,color="black")+theme(axis.text.x = element_text(size=12))

#################################PLANT##################################################################################################### 

Closed_NP50km<-as.data.frame(c(apply(Plant_BetaClose_Restrict_TOT_50km,1,mean),apply(Plant_BetaClose_Restrict_TUR_50km,1,mean),apply(Plant_BetaClose_Restrict_NES_50km,1,mean)))
Closed_NP50km$Components<-factor(c(rep("Total",dim(Plant_BetaClose_Restrict_TOT_50km)[1]),rep("Turnover",dim(Plant_BetaClose_Restrict_TOT_50km)[1]),rep("Nestedness",dim(Plant_BetaClose_Restrict_TOT_50km)[1])))
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
  ylim(0,1)+
  geom_boxplot(outlier.size=0,alpha=0,show_guide=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj,beta),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show_guide=FALSE) +
  
  #facet_wrap(~ plot_type) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "",
       colour = "Components") +
  theme_bw()+
  annotate("text", x=1, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=2, y=1, label= "ns",size=4)+
  annotate("text", x=3, y=0.973, label= "***",size=6.5,fontface=2,color="black")+theme(axis.text.x = element_text(size=12))

grDevices::pdf(file = here::here("Figures", "Figure5.pdf"), 
               width = 10, height = 10) 
print(multiplot(p1,p7,p4,p2,p8,p5,p3,p9,p6,cols=3))
      dev.off()
      
      
      