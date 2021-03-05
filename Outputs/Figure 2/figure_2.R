rm(list = ls())

## Library ----

library(stringr)
library(ggplot2)
library(grid)
library(png)

## Import Icons ----

paths <- list.files(path = here::here("Outputs", "Figure 2"), pattern = "*.png$", 
                    full.names = TRUE)

files <- list.files(path = here::here("Outputs", "Figure 2"), pattern = "*.png$", 
                    full.names = FALSE)

all_im <- lapply(paths, png::readPNG)
names(all_im) <- gsub(".png", "", files)


## Prepare Data ----

filenames <- list.files(path = here::here("Outputs", "Figure 2"), pattern = ".RData$",
                        full.names = FALSE)

files <- list.files(path = here::here("Outputs", "Figure 2"), pattern = ".RData$",
                    full.names = TRUE)

lapply(files, load, environment())


## FONCTION  multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#--- Plot


Closed_NP50km<-data.frame(c((apply(S_fish_closed_50vsNP,1,mean)-apply(S_fish_NP_50vsclosed,1,mean)),
                            (apply(S_plant_closed_50vsNP,1,mean)-apply(S_plant_NP_50vsclosed,1,mean)),
                            (apply(S_bird_closed_50vsNP,1,mean)-apply(S_bird_NP_50vsclosed,1,mean))))
Closed_NP50km$Components<-factor(c(rep("Fish",dim(S_fish_closed_50vsNP)[1]),rep("Plant",dim(S_plant_closed_50vsNP)[1]),rep("Bird",dim(S_bird_closed_50vsNP)[1])))
colnames(Closed_NP50km)<-c("RS","Species")

restricted_NP50km<-data.frame(c((apply(S_fish_restricted_50vsNP,1,mean)-apply(S_fish_NP_50vsrestr,1,mean)),
                                (apply(S_plant_restricted_50vsNP,1,mean)-apply(S_plant_NP_50vsrestr,1,mean)),
                                (apply(S_bird_restricted_50vsNP,1,mean)-apply(S_bird_NP_50vsrestr,1,mean))))
restricted_NP50km$Components<-factor(c(rep("Fish",dim(S_fish_restricted_50vsNP)[1]),rep("Plant",dim(S_plant_restricted_50vsNP)[1]),rep("Bird",dim(S_bird_restricted_50vsNP)[1])))
colnames(restricted_NP50km)<-c("RS","Species")

Closed_RESTRICT50km<-data.frame(c((apply(S_fish_closed_50vsrester,1,mean)-apply(S_fish_restricted_50vsclose,1,mean)),
                                  (apply(S_plant_closed_50vsrester,1,mean)-apply(S_plant_restricted_50vsclose,1,mean)),
                                  (apply(S_bird_closed_50vsrester,1,mean)-apply(S_bird_restricted_50vsclose,1,mean))))
Closed_RESTRICT50km$Components<-factor(c(rep("Fish",dim(S_fish_closed_50vsrester)[1]),rep("Plant",dim(S_plant_closed_50vsrester)[1]),rep("Bird",dim(S_bird_closed_50vsrester)[1])))
colnames(Closed_RESTRICT50km)<-c("RS","Species")

data<-rbind(Closed_NP50km,restricted_NP50km,Closed_RESTRICT50km)
data$scale <-factor(c(rep("SPA vs NPA",dim(Closed_NP50km)[1]),rep("RA vs NPA",dim(restricted_NP50km)[1]),rep("SPA vs RA",dim(Closed_RESTRICT50km)[1])))

#Pour conserver l'ordre des boxplot
data$scale<- factor(data$scale,levels = c('SPA vs NPA','SPA vs RA','RA vs NPA'),ordered = TRUE)
data$dist_cat_n[data$scale == "SPA vs NPA"] <- -0.25
data$dist_cat_n[data$scale == "RA vs NPA"] <- 0.25
data$dist_cat_n[data$scale == "SPA vs RA"] <-  0 

data$scat_adj[data$scale == "SPA vs NPA"] <- 1.25
data$scat_adj[data$scale == "RA vs NPA"] <-2.75
data$scat_adj[data$scale == "SPA vs RA"] <- 2

###Fish
Fish<-data[data$Species=="Fish",]
my_grob1 = grobTree(textGrob("ns", x=0.12,  y=0.9845, hjust=0, gp=gpar(col="black", fontsize=20)))
my_grob2 = grobTree(textGrob("ns", x=0.43,  y=0.9845, hjust=0, gp=gpar(col="black", fontsize=20)))
my_grob3 = grobTree(textGrob("ns", x=0.74,  y=0.9845, hjust=0, gp=gpar(col="black", fontsize=20)))

p1<- ggplot(Fish, aes(scale,RS,colour=scale)) +
  geom_boxplot(alpha=0,show.legend=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj ,RS),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show.legend=FALSE) +
  scale_colour_manual(values=c("darkblue", "cyan4", "cyan3"))+
  labs(title = "",
       x = "",
       y = "Difference number of species",
       colour = "",size=15) +
  theme_bw() +  annotation_custom(rasterGrob(all_im$Fish),  xmin = 2.8, xmax = 3.6, ymin = -65, ymax = -90) +
  annotation_custom(my_grob1)+annotation_custom(my_grob2)+annotation_custom(my_grob3)+
  theme(axis.title.y = element_text(size=16),axis.text.x = element_text(size=14))

wilcox.test(apply(S_fish_closed_50vsNP,1,mean),apply(S_fish_NP_50vsclosed,1,mean))
wilcox.test(apply(S_fish_closed_50vsrester,1,mean),apply(S_fish_restricted_50vsclose,1,mean))
wilcox.test(apply(S_fish_restricted_50vsNP,1,mean),apply(S_fish_NP_50vsrestr,1,mean))


###Bird
bird<-data[data$Species=="Bird",]

my_grob1 = grobTree(textGrob("ns", x=0.12,  y=0.9845, hjust=0, gp=gpar(col="black", fontsize=20)))
my_grob2 = grobTree(textGrob("ns", x=0.43,  y=0.9845, hjust=0, gp=gpar(col="black", fontsize=20)))
my_grob3 = grobTree(textGrob("ns", x=0.74,  y=0.9845, hjust=0, gp=gpar(col="black", fontsize=20)))


p2<-ggplot(bird, aes(scale,RS,colour=scale)) +
  geom_boxplot(alpha=0,show.legend=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj ,RS),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show.legend=FALSE) +
  scale_colour_manual(values=c("coral4", "chocolate1", "coral1"))+
  labs(title = "",
       x = "",
       y = "",
       colour = "") +
  theme_bw() +  annotation_custom(rasterGrob(all_im$Bird),  xmin = 2.8, xmax = 3.6, ymin = -120, ymax = -75)+
  annotation_custom(my_grob1)+annotation_custom(my_grob2)+annotation_custom(my_grob3)+
  theme(axis.title.y = element_text(size=16),axis.text.x = element_text(size=14))


wilcox.test(apply(S_bird_closed_50vsNP,1,mean),apply(S_bird_NP_50vsclosed,1,mean))
wilcox.test(apply(S_bird_closed_50vsrester,1,mean),apply(S_bird_restricted_50vsclose,1,mean))
wilcox.test(apply(S_bird_restricted_50vsNP,1,mean),apply(S_bird_NP_50vsrestr,1,mean))


###Plant
Plant<-data[data$Species=="Plant",]
my_grob1 = grobTree(textGrob("*", x=0.16,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=24,fontface=2)))
my_grob2 = grobTree(textGrob("***", x=0.43,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=24,fontface=2)))
my_grob3 = grobTree(textGrob("*", x=0.79,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=24,fontface=2)))
p3<-ggplot(Plant, aes(scale,RS,colour=scale)) +
  geom_boxplot(alpha=0,show.legend=FALSE)+  #mettre T pour afficher la legend
  geom_jitter(aes(dist_cat_n + scat_adj ,RS),
              position=position_jitter(width=0.05,height=0),
              alpha=0.55, size=2,
              show.legend=FALSE) +
  scale_colour_manual(values=c("aquamarine4", "chartreuse3", "aquamarine2"))+
  labs(title = "",
       x = "",
       y = "",
       colour = "") +
  theme_bw() +  annotation_custom(rasterGrob(all_im$Plant),  xmin = 2.8, xmax = 3.6, ymin = -980, ymax = -850)+
  annotation_custom(my_grob1)+annotation_custom(my_grob2)+annotation_custom(my_grob3)+
  theme(axis.title.y = element_text(size=16),axis.text.x = element_text(size=14))

wilcox.test(apply(S_plant_closed_50vsNP,1,mean),apply(S_plant_NP_50vsclosed,1,mean))
wilcox.test(apply(S_plant_closed_50vsrester,1,mean),apply(S_plant_restricted_50vsclose,1,mean))
wilcox.test(apply(S_plant_restricted_50vsNP,1,mean),apply(S_plant_NP_50vsrestr,1,mean))




grDevices::pdf(file = here::here("figures", "Figure2.pdf"), 
               width = 11.7, height = 8.3) #SAVE A4
print(multiplot(p1,p2,p3,cols=3))
dev.off()


