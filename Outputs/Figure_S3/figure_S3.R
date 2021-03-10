rm(list = ls())

## Library ----

library(stringr)
library(ggplot2)
library(grid)
library(png)
## Source multiplot ----

source(here::here("Function", "multiplot.R"))
## Import Icons ----

paths <- list.files(path = here::here("Outputs", "Figure_S3"), pattern = "*.png$", 
                    full.names = TRUE)

files <- list.files(path = here::here("Outputs", "Figure_S3"), pattern = "*.png$", 
                    full.names = FALSE)

all_im <- lapply(paths, png::readPNG)
names(all_im) <- gsub(".png", "", files)

## Prepare Data ----

filenames <- list.files(path = here::here("Outputs", "Figure_S3"), pattern = ".RData$",
                        full.names = FALSE)

files <- list.files(path = here::here("Outputs", "Figure_S3"), pattern = ".RData$",
                    full.names = TRUE)

lapply(files, load, environment())






#############################################################################################################################


#############################################10 KM######################################################################## 



#############################################################################################################################




###################################################################################################################################### 
###################################################################################################################################### 
#################################PROTECTED VS UNPROTECTED#############################################################################
###################################################################################################################################### 
######################################################################################################################################
#################################FISH##################################################################################################### 
Closed_NP10km<-as.data.frame(c(apply(Fish_BetaClose_NonP_TOT_10km,1,mean),apply(Fish_BetaClose_NonP_TUR_10km,1,mean),apply(Fish_BetaClose_NonP_NES_10km,1,mean)))
Closed_NP10km$Components<-factor(c(rep("Total",dim(Fish_BetaClose_NonP_TOT_10km)[1]),rep("Turnover",dim(Fish_BetaClose_NonP_TOT_10km)[1]),rep("Nestedness",dim(Fish_BetaClose_NonP_TOT_10km)[1])))
colnames(Closed_NP10km)<-c("beta","Components")

data<-Closed_NP10km
data$scale <-factor(c(rep("Strict vs OA",dim(Closed_NP10km)[1])))

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
  labs(title = "10 km",
       x = "",
       y = "SPA vs NPA",
       colour = "Components") +
  theme_bw() +  annotation_custom(rasterGrob(img), xmin = 2.7, xmax = 3.7, ymin = 0.8, ymax = 0.97)#+
annotate("text", x=1, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=2, y=0.973, label= "*",size=6.5,fontface=2,color="black")+
  annotate("text", x=3, y=1, label= "ns",size=4)



#################################BIRD##################################################################################################### 

Closed_NP10km<-as.data.frame(c(apply(Bird_BetaClose_NonP_TOT_10km,1,mean),apply(Bird_BetaClose_NonP_TUR_10km,1,mean),apply(Bird_BetaClose_NonP_NES_10km,1,mean)))
Closed_NP10km$Components<-factor(c(rep("Total",dim(Bird_BetaClose_NonP_TOT_10km)[1]),rep("Turnover",dim(Bird_BetaClose_NonP_TOT_10km)[1]),rep("Nestedness",dim(Bird_BetaClose_NonP_TOT_10km)[1])))
colnames(Closed_NP10km)<-c("beta","Components")

data<-Closed_NP10km
data$scale <-factor(c(rep("Strict vs OA",dim(Closed_NP10km)[1])))

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
  theme_bw() +  annotation_custom(rasterGrob(img2),  xmin = 2.7, xmax = 3.7, ymin = 0.8, ymax = 0.97)#+
annotate("text", x=1, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=2, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=3, y=1, label= "ns",size=4)
#+
#theme(panel.grid.major = element_blank(),
#      panel.grid.minor = element_blank())

#################################PLANT##################################################################################################### 

Closed_NP10km<-as.data.frame(c(apply(Plant_BetaClose_NonP_TOT_10km,1,mean),apply(Plant_BetaClose_NonP_TUR_10km,1,mean),apply(Plant_BetaClose_NonP_NES_10km,1,mean)))
Closed_NP10km$Components<-factor(c(rep("Total",dim(Plant_BetaClose_NonP_TOT_10km)[1]),rep("Turnover",dim(Plant_BetaClose_NonP_TOT_10km)[1]),rep("Nestedness",dim(Plant_BetaClose_NonP_TOT_10km)[1])))
colnames(Closed_NP10km)<-c("beta","Components")

data<-Closed_NP10km
data$scale <-factor(c(rep("Strict vs OA",dim(Closed_NP10km)[1])))

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
  theme_bw() +  annotation_custom(rasterGrob(img3),  xmin = 2.7, xmax = 3.7, ymin = 0.8, ymax = 0.97)#+
annotate("text", x=1, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=2, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=3, y=1, label= "ns",size=4)

###################################################################################################################################### 
###################################################################################################################################### 
#################################RESTRICTED VS UNPROTECTED#############################################################################
###################################################################################################################################### 
######################################################################################################################################


#################################FISH##################################################################################################### 
Closed_NP10km<-as.data.frame(c(apply(Fish_Betarestricte_NonP_TOT_10km,1,mean),apply(Fish_Betarestricte_NonP_TUR_10km,1,mean),apply(Fish_Betarestricte_NonP_NES_10km,1,mean)))
Closed_NP10km$Components<-factor(c(rep("Total",dim(Fish_Betarestricte_NonP_TOT_10km)[1]),rep("Turnover",dim(Fish_Betarestricte_NonP_TOT_10km)[1]),rep("Nestedness",dim(Fish_Betarestricte_NonP_TOT_10km)[1])))
colnames(Closed_NP10km)<-c("beta","Components")

data<-Closed_NP10km
data$scale <-factor(c(rep("Restrict vs OA",dim(Closed_NP10km)[1])))

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
  theme_bw()#+ +  annotation_custom(rasterGrob(img),  xmin = 2.7, xmax = 3.7, ymin = 0.8, ymax = 0.97)+
annotate("text", x=1, y=1, label= "ns",size=4)+
  annotate("text", x=2, y=1, label= "ns",size=4)+
  annotate("text", x=3, y=1, label= "ns",size=4)




#################################BIRD##################################################################################################### 
Closed_NP10km<-as.data.frame(c(apply(Bird_Betarestricte_NonP_TOT_10km,1,mean),apply(Bird_Betarestricte_NonP_TUR_10km,1,mean),apply(Bird_Betarestricte_NonP_NES_10km,1,mean)))
Closed_NP10km$Components<-factor(c(rep("Total",dim(Bird_Betarestricte_NonP_TOT_10km)[1]),rep("Turnover",dim(Bird_Betarestricte_NonP_TOT_10km)[1]),rep("Nestedness",dim(Bird_Betarestricte_NonP_TOT_10km)[1])))
colnames(Closed_NP10km)<-c("beta","Components")

data<-Closed_NP10km
data$scale <-factor(c(rep("Strict vs OA",dim(Closed_NP10km)[1])))

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
  theme_bw()#+
annotate("text", x=1, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=2, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=3, y=0.973, label= "***",size=6.5,fontface=2,color="black")
# +  annotation_custom(rasterGrob(img2),  xmin = 2.7, xmax = 3.7, ymin = 0.8, ymax = 0.97)


#################################PLANT##################################################################################################### 

Closed_NP10km<-as.data.frame(c(apply(Plant_BetaRestricted_NonP_TOT_10km,1,mean),apply(Plant_BetaRestricted_NonP_TUR_10km,1,mean),apply(Plant_BetaRestricted_NonP_NES_10km,1,mean)))
Closed_NP10km$Components<-factor(c(rep("Total",dim(Plant_BetaRestricted_NonP_TOT_10km)[1]),rep("Turnover",dim(Plant_BetaRestricted_NonP_TOT_10km)[1]),rep("Nestedness",dim(Plant_BetaRestricted_NonP_TOT_10km)[1])))
colnames(Closed_NP10km)<-c("beta","Components")

data<-Closed_NP10km
data$scale <-factor(c(rep("Strict vs OA",dim(Closed_NP10km)[1])))

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
  theme_bw()#+
annotate("text", x=0.75, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=1.75, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=2.75, y=0.973, label= "***",size=6.5,fontface=2,color="gray") #+  annotation_custom(rasterGrob(img3),  xmin = 2.7, xmax = 3.7, ymin = 0.8, ymax = 0.97)

###################################################################################################################################### 
###################################################################################################################################### 
#################################PROTECTED VS RESTRICTED #############################################################################
###################################################################################################################################### 
######################################################################################################################################

#################################FISH##################################################################################################### 
Closed_RESTRICT10km<-as.data.frame(c(apply(Fish_BetaClose_RESTRICT_TOT_10km,1,mean),apply(Fish_BetaClose_RESTRICT_TUR_10km,1,mean),apply(Fish_BetaClose_RESTRICT_NES_10km,1,mean)))
Closed_RESTRICT10km$Components<-factor(c(rep("Total",dim(Fish_BetaClose_RESTRICT_TOT_10km)[1]),rep("Turnover",dim(Fish_BetaClose_RESTRICT_TOT_10km)[1]),rep("Nestedness",dim(Fish_BetaClose_RESTRICT_TOT_10km)[1])))
colnames(Closed_RESTRICT10km)<-c("beta","Components")

data<-Closed_RESTRICT10km
data$scale <-factor(c(rep("Strict vs OA",dim(Closed_RESTRICT10km)[1])))

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
  theme_bw()#+
annotate("text", x=1, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=2, y=1, label= "ns",size=4)+
  annotate("text", x=3, y=1, label= "ns",size=4)# +  annotation_custom(rasterGrob(img),  xmin = 2.7, xmax = 3.7, ymin = 0.8, ymax = 0.97)




#################################BIRD##################################################################################################### 

Closed_RESTRICT10km<-as.data.frame(c(apply(Bird_BetaClose_RESTRICT_TOT_10km,1,mean),apply(Bird_BetaClose_RESTRICT_TUR_10km,1,mean),apply(Bird_BetaClose_RESTRICT_NES_10km,1,mean)))
Closed_RESTRICT10km$Components<-factor(c(rep("Total",dim(Bird_BetaClose_RESTRICT_TOT_10km)[1]),rep("Turnover",dim(Bird_BetaClose_RESTRICT_TOT_10km)[1]),rep("Nestedness",dim(Bird_BetaClose_RESTRICT_TOT_10km)[1])))
colnames(Closed_RESTRICT10km)<-c("beta","Components")

data<-Closed_RESTRICT10km
data$scale <-factor(c(rep("Strict vs OA",dim(Closed_RESTRICT10km)[1])))

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
  theme_bw()#+
annotate("text", x=1, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=2, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=3, y=0.973, label= "***",size=6.5,fontface=2,color="black")# +  annotation_custom(rasterGrob(img2),  xmin = 2.7, xmax = 3.7, ymin = 0.8, ymax = 0.97)


#################################PLANT##################################################################################################### 

Closed_NP10km<-as.data.frame(c(apply(Plant_BetaClose_Restrict_TOT_10km,1,mean),apply(Plant_BetaClose_Restrict_TUR_10km,1,mean),apply(Plant_BetaClose_Restrict_NES_10km,1,mean)))
Closed_NP10km$Components<-factor(c(rep("Total",dim(Plant_BetaClose_Restrict_TOT_10km)[1]),rep("Turnover",dim(Plant_BetaClose_Restrict_TOT_10km)[1]),rep("Nestedness",dim(Plant_BetaClose_Restrict_TOT_10km)[1])))
colnames(Closed_NP10km)<-c("beta","Components")

data<-Closed_NP10km
data$scale <-factor(c(rep("Strict vs OA",dim(Closed_NP10km)[1])))

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
  theme_bw()#+
annotate("text", x=1, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=2, y=0.973, label= "***",size=6.5,fontface=2,color="black")+
  annotate("text", x=3, y=1, label= "ns",size=4)# +  annotation_custom(rasterGrob(img3),  xmin = 2.7, xmax = 3.7, ymin = 0.8, ymax = 0.97)



multiplot(p1,p7,p4,p2,p8,p5,p3,p9,p6,cols=3)

