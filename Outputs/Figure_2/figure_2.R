rm(list = ls())
## Library ----

library(stringr)
library(ggplot2)
library(grid)
library(png)
## Source multiplot ----

source(here::here("Function", "multiplot.R"))

## Import Icons ----

paths <- list.files(path = here::here("Outputs", "Figure_2"), pattern = "*.png$", 
                    full.names = TRUE)

files <- list.files(path = here::here("Outputs", "Figure_2"), pattern = "*.png$", 
                    full.names = FALSE)

all_im <- lapply(paths, png::readPNG)
names(all_im) <- gsub(".png", "", files)


## Prepare Data ----

filenames <- list.files(path = here::here("Outputs", "Figure_2"), pattern = ".RData$",
                        full.names = FALSE)

files <- list.files(path = here::here("Outputs", "Figure_2"), pattern = ".RData$",
                    full.names = TRUE)

lapply(files, load, environment())


#--bird dbrda
Intersect_bird     <- bird_dbrda$RsquareAdj_all-bird_dbrda$RsquareAdj_partial_PA-bird_dbrda$RsquareAdj_partial_hab
Pa_perc_bird          <- bird_dbrda$RsquareAdj_partial_PA/(bird_dbrda$RsquareAdj_all+Intersect_bird)
Hab_perc_bird         <- bird_dbrda$RsquareAdj_partial_hab/(bird_dbrda$RsquareAdj_all+Intersect_bird)
Intersect_perc_bird   <- Intersect_bird  /(bird_dbrda$RsquareAdj_all+Intersect_bird)

metaRNASeq::fishercomb(bird_dbrda$Pval_mod_partial_PA) 
metaRNASeq::fishercomb(bird_dbrda$Pval_mod_partial_hab) 

#--fish dbrda
Intersect_fish     <- fish_dbrda$RsquareAdj_all-fish_dbrda$RsquareAdj_partial_PA-fish_dbrda$RsquareAdj_partial_hab
Pa_perc_fish          <- fish_dbrda$RsquareAdj_partial_PA/(fish_dbrda$RsquareAdj_all+Intersect_fish)
Hab_perc_fish         <- fish_dbrda$RsquareAdj_partial_hab/(fish_dbrda$RsquareAdj_all+Intersect_fish)
Intersect_perc_fish   <- Intersect_fish  /(fish_dbrda$RsquareAdj_all+Intersect_fish)

metaRNASeq::fishercomb(fish_dbrda$Pval_mod_partial_PA) 
metaRNASeq::fishercomb(fish_dbrda$Pval_mod_partial_hab) 


#--plant dbrda
Intersect_plant     <- plant_dbrda$RsquareAdj_all-plant_dbrda$RsquareAdj_partial_PA-plant_dbrda$RsquareAdj_partial_hab
Pa_perc_plant          <- plant_dbrda$RsquareAdj_partial_PA/(plant_dbrda$RsquareAdj_all+Intersect_plant)
Hab_perc_plant         <- plant_dbrda$RsquareAdj_partial_hab/(plant_dbrda$RsquareAdj_all+Intersect_plant)
Intersect_perc_plant   <- Intersect_plant  /(plant_dbrda$RsquareAdj_all+Intersect_plant)

metaRNASeq::fishercomb(plant_dbrda$Pval_mod_partial_PA) 
metaRNASeq::fishercomb(plant_dbrda$Pval_mod_partial_hab) 

# --- plot
data_dbrda_fish <- data.frame(Percentage = c(Pa_perc_fish*100,
                                       Hab_perc_fish*100),
                        Var = c(rep("Protection",nrow(fish_dbrda)),
                                rep("Habitat",nrow(fish_dbrda))))

#Pour conserver l'ordre des boxplot
data_dbrda_fish$Var<- factor(data_dbrda_fish$Var,levels = c('Protection','Habitat'),ordered = TRUE)

###Fish
my_grob1 = grobTree(textGrob("***", x=0.22,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=24,fontface=2)))
my_grob2 = grobTree(textGrob("***", x=0.67,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=24,fontface=2)))
p1 <- ggplot(data_dbrda_fish, aes(Var,Percentage,colour=Var)) +
  geom_boxplot(alpha=0,show.legend=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(Var,Percentage),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show.legend=FALSE) +
  scale_colour_manual(values=c("darkblue", "cyan4"))+
  labs(title = "",
       x = "",
       y = "Percentage",
       colour = "",size=15) + ylim(0,100)+
  theme_bw() +  annotation_custom(rasterGrob(all_im$Fish),  xmin = 2, xmax = 2.5, ymin = 92, ymax = 102) +
  theme(axis.title.y = element_text(size=16),axis.text.x = element_text(size=14))+
  annotation_custom(my_grob1)+
  annotation_custom(my_grob2)


# ---
my_grob1 = grobTree(textGrob("***", x=0.22,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=24,fontface=2)))
my_grob2 = grobTree(textGrob("***", x=0.67,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=24,fontface=2)))
data_dbrda_bird <- data.frame(Percentage = c(Pa_perc_bird*100,
                                       Hab_perc_bird*100),
                        Var = c(rep("Protection",nrow(bird_dbrda)),
                                rep("Habitat",nrow(bird_dbrda))))

#Pour conserver l'ordre des boxplot
data_dbrda_bird$Var<- factor(data_dbrda_bird$Var,levels = c('Protection','Habitat'),ordered = TRUE)

###bird
p2 <- ggplot(data_dbrda_bird, aes(Var,Percentage,colour=Var)) +
  geom_boxplot(alpha=0,show.legend=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(Var,Percentage),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show.legend=FALSE) +
  scale_colour_manual(values=c("coral4", "chocolate1"))+
  labs(title = "",
       x = "",
       y = " ",
       colour = "",size=15) + ylim(0,100)+
  theme_bw() +  annotation_custom(rasterGrob(all_im$Bird),  xmin = 2.1, xmax = 2.5, ymin = 92, ymax = 102) +
  theme(axis.title.y = element_text(size=16),axis.text.x = element_text(size=14))+
  annotation_custom(my_grob1)+
  annotation_custom(my_grob2)


# ---
data_dbrda_plant <- data.frame(Percentage = c(Pa_perc_plant*100,
                                        Hab_perc_plant*100),
                         Var = c(rep("Protection",nrow(plant_dbrda)),
                                 rep("Habitat",nrow(plant_dbrda))))

#Pour conserver l'ordre des boxplot
data_dbrda_plant$Var<- factor(data_dbrda_plant$Var,levels = c('Protection','Habitat'),ordered = TRUE)

###plant
my_grob1 = grobTree(textGrob("***", x=0.22,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=24,fontface=2)))
my_grob2 = grobTree(textGrob("***", x=0.67,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=24,fontface=2)))
p3<- ggplot(data_dbrda_plant, aes(Var,Percentage,colour=Var)) +
  geom_boxplot(alpha=0,show.legend=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(Var,Percentage),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show.legend=FALSE) +
  scale_colour_manual(values=c("aquamarine4", "chartreuse3"))+
  labs(title = "",
       x = "",
       y = " ",
       colour = "",size=15) + ylim(0,100)+
  theme_bw() +  annotation_custom(rasterGrob(all_im$Plant),  xmin = 2.1, xmax = 2.5, ymin = 92, ymax = 102) +
  theme(axis.title.y = element_text(size=16),axis.text.x = element_text(size=14))+
  annotation_custom(my_grob1)+
  annotation_custom(my_grob2)




grDevices::pdf(file = here::here("Figures", "Figure2.pdf"), 
               width = 12, height = 8) #SAVE A4
multiplot(p1,p2,p3,cols=3)
dev.off()

#PNG
png(
  file      = here::here("Figures", "Figure2.png"),
  width     = 12,
  height    = 8,
  units     = "in",
  res       = 600,
  pointsize = 38
)
multiplot(p1,p2,p3,cols=3)
dev.off()

grDevices::postscript(file = here::here("Figures", "Figure2.eps"),
           width     = 12,
           height    = 8,
           units     = "in",
           res       = 600,
           pointsize = 38
)
multiplot(p1,p2,p3,cols=3)
dev.off()
#save 8*12