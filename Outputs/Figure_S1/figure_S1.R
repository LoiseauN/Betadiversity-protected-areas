rm(list = ls())

## Library ----

library(stringr)
library(ggplot2)
library(grid)
library(png)
## Source multiplot ----

source(here::here("Function", "multiplot.R"))
## Import Icons ----

paths <- list.files(path = here::here("Outputs", "Figure_S1"), pattern = "*.png$", 
                    full.names = TRUE)

files <- list.files(path = here::here("Outputs", "Figure_S1"), pattern = "*.png$", 
                    full.names = FALSE)

all_im <- lapply(paths, png::readPNG)
names(all_im) <- gsub(".png", "", files)

## Prepare Data ----

filenames <- list.files(path = here::here("Outputs", "Figure_S1"), pattern = ".RData$",
                        full.names = FALSE)

files <- list.files(path = here::here("Outputs", "Figure_S1"), pattern = ".RData$",
                    full.names = TRUE)

lapply(files, load, environment())



##FOR SUP

# ---
fish_dbrda <- data.frame(Rsquare = c(fish_dbrda$RsquareAdj_all,fish_dbrda$RsquareAdj_partial_PA,fish_dbrda$RsquareAdj_partial_hab),
                        Var = c(rep("All",nrow(fish_dbrda)),
                                rep("Protection",nrow(fish_dbrda)),
                                rep("Habitat",nrow(fish_dbrda))))

#Pour conserver l'ordre des boxplot
fish_dbrda$Var<- factor(fish_dbrda$Var,levels = c('All','Protection','Habitat'),ordered = TRUE)
fish_dbrda$dist_cat_n[fish_dbrda$Var == "All"] <- -0.25
fish_dbrda$dist_cat_n[fish_dbrda$Var == "Protection"] <- 0.25
fish_dbrda$dist_cat_n[fish_dbrda$Var == "Habitat"] <-  0 

fish_dbrda$scat_adj[fish_dbrda$Var == "All"] <- 1.25
fish_dbrda$scat_adj[fish_dbrda$Var == "Protection"] <- 1.75
fish_dbrda$scat_adj[fish_dbrda$Var == "Habitat"] <- 3

###fish_dbrda

p4<- ggplot(fish_dbrda, aes(Var,Rsquare,colour=Var)) +
  geom_boxplot(alpha=0,show.legend=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj ,Rsquare),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show.legend=FALSE) +
  scale_colour_manual(values=c("darkblue", "cyan4", "cyan3"))+
  labs(title = "",
       x = "",
       y = "Rsquare",
       colour = "",size=15) + ylim(0,0.4)+
  theme_bw() +  annotation_custom(rasterGrob(all_im$Fish),  xmin = 2.8, xmax = 3.5, ymin = 0.35, ymax = 0.45) +
  theme(axis.title.y = element_text(size=16),axis.text.x = element_text(size=14)) 


# ---
bird_dbrda <- data.frame(Rsquare = c(bird_dbrda$RsquareAdj_all,bird_dbrda$RsquareAdj_partial_PA,bird_dbrda$RsquareAdj_partial_hab),
                        Var = c(rep("All",nrow(bird_dbrda)),
                                rep("Protection",nrow(bird_dbrda)),
                                rep("Habitat",nrow(bird_dbrda))))

#Pour conserver l'ordre des boxplot
bird_dbrda$Var<- factor(bird_dbrda$Var,levels = c('All','Protection','Habitat'),ordered = TRUE)
bird_dbrda$dist_cat_n[bird_dbrda$Var == "All"] <- -0.25
bird_dbrda$dist_cat_n[bird_dbrda$Var == "Protection"] <- 0.25
bird_dbrda$dist_cat_n[bird_dbrda$Var == "Habitat"] <-  0 

bird_dbrda$scat_adj[bird_dbrda$Var == "All"] <- 1.25
bird_dbrda$scat_adj[bird_dbrda$Var == "Protection"] <- 1.75
bird_dbrda$scat_adj[bird_dbrda$Var == "Habitat"] <- 3

###bird_dbrda

p5<- ggplot(bird_dbrda, aes(Var,Rsquare,colour=Var)) +
  geom_boxplot(alpha=0,show.legend=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj ,Rsquare),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show.legend=FALSE) +
  scale_colour_manual(values=c("coral4", "chocolate1", "coral1"))+
  labs(title = "",
       x = "",
       y = " ",
       colour = "",size=15) + ylim(0,0.4)+
  theme_bw() +  annotation_custom(rasterGrob(all_im$Bird),  xmin = 2.8, xmax = 3.5, ymin = 0.35, ymax = 0.45)  +
  theme(axis.title.y = element_text(size=16),axis.text.x = element_text(size=14))


# ---
plant_dbrda <- data.frame(Rsquare = c(plant_dbrda$RsquareAdj_all,plant_dbrda$RsquareAdj_partial_PA,plant_dbrda$RsquareAdj_partial_hab),
                         Var = c(rep("All",nrow(plant_dbrda)),
                                 rep("Protection",nrow(plant_dbrda)),
                                 rep("Habitat",nrow(plant_dbrda))))

#Pour conserver l'ordre des boxplot
plant_dbrda$Var<- factor(plant_dbrda$Var,levels = c('All','Protection','Habitat'),ordered = TRUE)
plant_dbrda$dist_cat_n[plant_dbrda$Var == "All"] <- -0.25
plant_dbrda$dist_cat_n[plant_dbrda$Var == "Protection"] <- 0.25
plant_dbrda$dist_cat_n[plant_dbrda$Var == "Habitat"] <-  0 

plant_dbrda$scat_adj[plant_dbrda$Var == "All"] <- 1.25
plant_dbrda$scat_adj[plant_dbrda$Var == "Protection"] <- 1.75
plant_dbrda$scat_adj[plant_dbrda$Var == "Habitat"] <- 3

###plant_dbrda

p6<- ggplot(plant_dbrda, aes(Var,Rsquare,colour=Var)) +
  geom_boxplot(alpha=0,show.legend=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj ,Rsquare),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show.legend=FALSE) +
  scale_colour_manual(values=c("aquamarine4", "chartreuse3", "aquamarine2"))+
  labs(title = "",
       x = "",
       y = " ",
       colour = "",size=15) + ylim(0,0.4)+
  theme_bw() +  annotation_custom(rasterGrob(all_im$Plant),  xmin = 2.8, xmax = 3.5, ymin = 0.35, ymax = 0.45)  +
  theme(axis.title.y = element_text(size=16),axis.text.x = element_text(size=14))

grDevices::pdf(file = here::here("Figures", "FigureS1.pdf"), 
               width = 10, height = 10) 
print(multiplot(p4,p5,p6,cols=3))
dev.off()



