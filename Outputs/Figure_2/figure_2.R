bird <- na.omit(data.frame(dbrda_BIRDS_close_50km))

Intersect_bird     <- bird$RsquareAdj_all-bird$RsquareAdj_partial_PA-bird$RsquareAdj_partial_hab
Pa_perc_bird          <- bird$RsquareAdj_partial_PA/(bird$RsquareAdj_all+Intersect_bird)
Hab_perc_bird         <- bird$RsquareAdj_partial_hab/(bird$RsquareAdj_all+Intersect_bird)
Intersect_perc_bird   <- Intersect_bird  /(bird$RsquareAdj_all+Intersect_bird)

metaRNASeq::fishercomb(bird$Pr..F._mod_partial_PA) 
metaRNASeq::fishercomb(bird$Pr..F._mod_partial_hab) 

fish <- data.frame(rbind(dbrda_FISH_RLS_50km,dbrda_fish_close_50km_SERF))

Intersect_fish     <- fish$RsquareAdj_all-fish$RsquareAdj_partial_PA-fish$RsquareAdj_partial_hab
Pa_perc_fish          <- fish$RsquareAdj_partial_PA/(fish$RsquareAdj_all+Intersect_fish)
Hab_perc_fish         <- fish$RsquareAdj_partial_hab/(fish$RsquareAdj_all+Intersect_fish)
Intersect_perc_fish   <- Intersect_fish  /(fish$RsquareAdj_all+Intersect_fish)

metaRNASeq::fishercomb(fish$Pr..F._mod_partial_PA) 
metaRNASeq::fishercomb(fish$Pr..F._mod_partial_hab) 



plant <- data.frame(dbrda_PLANTS_close_50km)

Intersect_plant     <- plant$RsquareAdj_all-plant$RsquareAdj_partial_PA-plant$RsquareAdj_partial_hab
Pa_perc_plant          <- plant$RsquareAdj_partial_PA/(plant$RsquareAdj_all+Intersect_plant)
Hab_perc_plant         <- plant$RsquareAdj_partial_hab/(plant$RsquareAdj_all+Intersect_plant)
Intersect_perc_plant   <- Intersect_plant  /(plant$RsquareAdj_all+Intersect_plant)

metaRNASeq::fishercomb(plant$Pr..F._mod_partial_PA) 
metaRNASeq::fishercomb(plant$Pr..F._mod_partial_hab) 


df_res <- data.frame(Percentage = c(Pa_perc_fish*100,Pa_perc_bird*100,Pa_perc_plant*100,
                                    Hab_perc_fish*100,Hab_perc_bird*100,Hab_perc_plant*100),
                     Var = c(rep("PA",nrow(fish)),rep("PA",nrow(bird)),rep("PA",nrow(plant)),
                             rep("HAB",nrow(fish)),rep("HAB",nrow(bird)),rep("HAB",nrow(plant))),
                     taxa= c(rep("fish",nrow(fish)),rep("bird",nrow(bird)),rep("plant",nrow(plant)),
                             rep("fish",nrow(fish)),rep("bird",nrow(bird)),rep("plant",nrow(plant)))) 

ggplot(data=df_res,
       aes(x= taxa, y = Percentage,color=Var))+
  geom_boxplot()


df_res_Rsquare <- data.frame(Rsquare = c(fish$RsquareAdj_all,fish$RsquareAdj_partial_PA,fish$RsquareAdj_partial_hab,
                                         bird$RsquareAdj_all,bird$RsquareAdj_partial_PA,bird$RsquareAdj_partial_hab,
                                         plant$RsquareAdj_all,plant$RsquareAdj_partial_PA,plant$RsquareAdj_partial_hab),
                             Var = c(rep("ALL",nrow(fish)),rep("PA",nrow(fish)),rep("HAB",nrow(fish)),
                                     rep("ALL",nrow(bird)),rep("PA",nrow(bird)),rep("HAB",nrow(bird)),
                                     rep("ALL",nrow(plant)),rep("PA",nrow(plant)),rep("HAB",nrow(plant))),
                             taxa= c(rep("fish",nrow(fish)*3),rep("bird",nrow(bird)*3),rep("plant",nrow(plant)*3))) 


ggplot(data=df_res_Rsquare,
       aes(x= taxa, y = Rsquare,color=Var))+
  geom_boxplot()



df_res <- data.frame(Percentage = c(Pa_perc_fish*100,Pa_perc_bird*100,Pa_perc_plant*100,
                                    Hab_perc_fish*100,Hab_perc_bird*100,Hab_perc_plant*100),
                     Var = c(rep("PA",nrow(fish)),rep("PA",nrow(bird)),rep("PA",nrow(plant)),
                             rep("HAB",nrow(fish)),rep("HAB",nrow(bird)),rep("HAB",nrow(plant))),
                     taxa= c(rep("fish",nrow(fish)),rep("bird",nrow(bird)),rep("plant",nrow(plant)),
                             rep("fish",nrow(fish)),rep("bird",nrow(bird)),rep("plant",nrow(plant)))) 

# ---
data_fish <- data.frame(Percentage = c(Pa_perc_fish*100,
                                       Hab_perc_fish*100),
                        Var = c(rep("Protection",nrow(fish)),
                                rep("Habitat",nrow(fish))))

#Pour conserver l'ordre des boxplot
data_fish$Var<- factor(data_fish$Var,levels = c('Protection','Habitat'),ordered = TRUE)

###Fish
my_grob1 = grobTree(textGrob("***", x=0.22,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=24,fontface=2)))
my_grob2 = grobTree(textGrob("***", x=0.67,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=24,fontface=2)))
p1 <- ggplot(data_fish, aes(Var,Percentage,colour=Var)) +
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
  theme_bw() +  annotation_custom(rasterGrob(img),  xmin = 2, xmax = 2.5, ymin = 92, ymax = 102) +
  theme(axis.title.y = element_text(size=16),axis.text.x = element_text(size=14))+
  annotation_custom(my_grob1)+
  annotation_custom(my_grob2)
p1


# ---
my_grob1 = grobTree(textGrob("***", x=0.22,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=24,fontface=2)))
my_grob2 = grobTree(textGrob("***", x=0.67,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=24,fontface=2)))
data_bird <- data.frame(Percentage = c(Pa_perc_bird*100,
                                       Hab_perc_bird*100),
                        Var = c(rep("Protection",nrow(bird)),
                                rep("Habitat",nrow(bird))))

#Pour conserver l'ordre des boxplot
data_bird$Var<- factor(data_bird$Var,levels = c('Protection','Habitat'),ordered = TRUE)

###bird
p2 <- ggplot(data_bird, aes(Var,Percentage,colour=Var)) +
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
  theme_bw() +  annotation_custom(rasterGrob(img2),  xmin = 2.1, xmax = 2.5, ymin = 92, ymax = 102) +
  theme(axis.title.y = element_text(size=16),axis.text.x = element_text(size=14))+
  annotation_custom(my_grob1)+
  annotation_custom(my_grob2)
p2

# ---
data_plant <- data.frame(Percentage = c(Pa_perc_plant*100,
                                        Hab_perc_plant*100),
                         Var = c(rep("Protection",nrow(plant)),
                                 rep("Habitat",nrow(plant))))

#Pour conserver l'ordre des boxplot
data_plant$Var<- factor(data_plant$Var,levels = c('Protection','Habitat'),ordered = TRUE)

###plant
my_grob1 = grobTree(textGrob("***", x=0.22,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=24,fontface=2)))
my_grob2 = grobTree(textGrob("***", x=0.67,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=24,fontface=2)))
p3<- ggplot(data_plant, aes(Var,Percentage,colour=Var)) +
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
  theme_bw() +  annotation_custom(rasterGrob(img3),  xmin = 2.1, xmax = 2.5, ymin = 92, ymax = 102) +
  theme(axis.title.y = element_text(size=16),axis.text.x = element_text(size=14))+
  annotation_custom(my_grob1)+
  annotation_custom(my_grob2)
p3

multiplot(p1,p2,p3,cols=3)

#save 8*12






##FOR SUP

# ---
data_fish <- data.frame(Rsquare = c(fish$RsquareAdj_all,fish$RsquareAdj_partial_PA,fish$RsquareAdj_partial_hab),
                        Var = c(rep("All",nrow(fish)),
                                rep("Protection",nrow(fish)),
                                rep("Habitat",nrow(fish))))

#Pour conserver l'ordre des boxplot
data_fish$Var<- factor(data_fish$Var,levels = c('All','Protection','Habitat'),ordered = TRUE)
data_fish$dist_cat_n[data_fish$Var == "All"] <- -0.25
data_fish$dist_cat_n[data_fish$Var == "Protection"] <- 0.25
data_fish$dist_cat_n[data_fish$Var == "Habitat"] <-  0 

data_fish$scat_adj[data_fish$Var == "All"] <- 1.25
data_fish$scat_adj[data_fish$Var == "Protection"] <- 1.75
data_fish$scat_adj[data_fish$Var == "Habitat"] <- 3

###Fish

p4<- ggplot(data_fish, aes(Var,Rsquare,colour=Var)) +
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
  theme_bw() +  annotation_custom(rasterGrob(img),  xmin = 2.8, xmax = 3.5, ymin = 0.35, ymax = 0.45) +
  theme(axis.title.y = element_text(size=16),axis.text.x = element_text(size=14)) 


# ---
data_bird <- data.frame(Rsquare = c(bird$RsquareAdj_all,bird$RsquareAdj_partial_PA,bird$RsquareAdj_partial_hab),
                        Var = c(rep("All",nrow(bird)),
                                rep("Protection",nrow(bird)),
                                rep("Habitat",nrow(bird))))

#Pour conserver l'ordre des boxplot
data_bird$Var<- factor(data_bird$Var,levels = c('All','Protection','Habitat'),ordered = TRUE)
data_bird$dist_cat_n[data_bird$Var == "All"] <- -0.25
data_bird$dist_cat_n[data_bird$Var == "Protection"] <- 0.25
data_bird$dist_cat_n[data_bird$Var == "Habitat"] <-  0 

data_bird$scat_adj[data_bird$Var == "All"] <- 1.25
data_bird$scat_adj[data_bird$Var == "Protection"] <- 1.75
data_bird$scat_adj[data_bird$Var == "Habitat"] <- 3

###bird

p5<- ggplot(data_bird, aes(Var,Rsquare,colour=Var)) +
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
  theme_bw() +  annotation_custom(rasterGrob(img2),  xmin = 2.8, xmax = 3.5, ymin = 0.35, ymax = 0.45)  +
  theme(axis.title.y = element_text(size=16),axis.text.x = element_text(size=14))


# ---
data_plant <- data.frame(Rsquare = c(plant$RsquareAdj_all,plant$RsquareAdj_partial_PA,plant$RsquareAdj_partial_hab),
                         Var = c(rep("All",nrow(plant)),
                                 rep("Protection",nrow(plant)),
                                 rep("Habitat",nrow(plant))))

#Pour conserver l'ordre des boxplot
data_plant$Var<- factor(data_plant$Var,levels = c('All','Protection','Habitat'),ordered = TRUE)
data_plant$dist_cat_n[data_plant$Var == "All"] <- -0.25
data_plant$dist_cat_n[data_plant$Var == "Protection"] <- 0.25
data_plant$dist_cat_n[data_plant$Var == "Habitat"] <-  0 

data_plant$scat_adj[data_plant$Var == "All"] <- 1.25
data_plant$scat_adj[data_plant$Var == "Protection"] <- 1.75
data_plant$scat_adj[data_plant$Var == "Habitat"] <- 3

###plant

p6<- ggplot(data_plant, aes(Var,Rsquare,colour=Var)) +
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
  theme_bw() +  annotation_custom(rasterGrob(img3),  xmin = 2.8, xmax = 3.5, ymin = 0.35, ymax = 0.45)  +
  theme(axis.title.y = element_text(size=16),axis.text.x = element_text(size=14))


multiplot(p4,p5,p6,cols=3)
