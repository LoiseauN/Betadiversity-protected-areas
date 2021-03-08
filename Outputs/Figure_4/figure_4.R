rm(list=ls())
## Library ----

library(stringr)
library(ggplot2)
library(grid)
library(png)
## Source multiplot ----

source(here::here("Function", "multiplot.R"))

## Import Icons ----

paths <- list.files(path = here::here("Outputs", "Figure_4"), pattern = "*.png$", 
                    full.names = TRUE)

files <- list.files(path = here::here("Outputs", "Figure_4"), pattern = "*.png$", 
                    full.names = FALSE)

all_im <- lapply(paths, png::readPNG)
names(all_im) <- gsub(".png", "", files)


## Prepare Data ----

filenames <- list.files(path = here::here("Outputs", "Figure_4"), pattern = ".RData$",
                        full.names = FALSE)

files <- list.files(path = here::here("Outputs", "Figure_4"), pattern = ".RData$",
                    full.names = TRUE)

lapply(files, load, environment())




summary(lm(Env_deltaS_fish_SPA_NPA$Envi~Env_deltaS_fish_SPA_NPA$deltaS))
a <- ggplot(Env_deltaS_fish_SPA_NPA, aes(x = Envi, y = deltaS))+geom_point( color = "darkblue") + xlim(0,1)+
  #geom_smooth(color = "darkblue", method = lm, fullrange = FALSE,se=F) + ylim(-70,100) +xlim(0,1)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.8, y=-75, label="R2 = 0.02 ns", color="darkblue",size=4)  +
  labs(title = "", x = "",y = "SPA vs NPA") +
  theme_bw() + annotation_custom(rasterGrob(all_im$Fish),  xmin = 0.75, xmax = 1, ymin = 90, ymax = 120)    



summary(lm(Env_deltaS_fish_SPA_RA$Envi~Env_deltaS_fish_SPA_RA$deltaS))
b <- ggplot(Env_deltaS_fish_SPA_RA, aes(x = Envi, y = deltaS))+xlim(0,1)+geom_point( color = "cyan4") +
  #geom_smooth(color = "cyan4", method = lm, fullrange = FALSE,se=F) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.8, y=-5, label="R2 = -0.07 ns", color="cyan4",size=4)  +
  labs(title = "", x = "",y = "SPA vs RA") +
  theme_bw() 


summary(lm(Env_deltaS_fish_RA_NPA$Envi~Env_deltaS_fish_RA_NPA$deltaS))
c <- ggplot(Env_deltaS_fish_RA_NPA, aes(x = Envi, y = deltaS)) +xlim(0,1)+geom_point( color = "cyan3") +
  #geom_smooth(color = " cyan3", method = lm, fullrange = FALSE,se=F) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.8, y=-53, label="R2 = -0.11 ns", color="cyan3",size=4)  +
  labs(title = "", x = "Habitat dissimilarity",y = "RA vs NPA") +
  theme_bw() 


#######################################


summary(lm(Env_deltaS_bird_SPA_NPA$Envi~Env_deltaS_bird_SPA_NPA$deltaS))
d <- ggplot(Env_deltaS_bird_SPA_NPA, aes(x = Envi, y = deltaS)) +xlim(0,1)+geom_point( color = "coral4") +
  # geom_smooth(color = "coral4", method = lm, fullrange = FALSE,se=F) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.8, y=-43, label="R2 = -0.007 ns", color="coral4",size=4)  +
  labs(title = "", x = "",y = "") +
  theme_bw() +  annotation_custom(rasterGrob(all_im$Bird), xmin = 0.75, xmax = 1, ymin = 60, ymax = 80)



summary(lm(Env_deltaS_bird_SPA_RA$Envi~Env_deltaS_bird_SPA_RA$deltaS))
e <- ggplot(Env_deltaS_bird_SPA_RA, aes(x = Envi, y = deltaS))+geom_point( color = "chocolate1") +xlim(0,1)+
  geom_smooth(color = "chocolate1", method = lm, fullrange = FALSE,se=F) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.8, y=-100, label="R2 = 0.07 *", color="chocolate1",size=4)  +
  labs(title = "", x = "",y = "") +
  theme_bw() 



summary(lm(Env_deltaS_bird_RA_NPA$Envi~Env_deltaS_bird_RA_NPA$deltaS))
f <- ggplot(Env_deltaS_bird_RA_NPA, aes(x = Envi, y = deltaS))+xlim(0,1)+geom_point( color = "coral1") +
  #geom_smooth(color = " coral1", method = lm, fullrange = FALSE,se=F) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.8, y=-50, label="R2 = 0.01 ns", color="coral1",size=4)  +
  labs(title = "", x = "Habitat dissimilarity",y = "") +
  theme_bw() 




summary(lm(Env_deltaS_plant_SPA_NPA$Envi~Env_deltaS_plant_SPA_NPA$deltaS))
g <- ggplot(Env_deltaS_plant_SPA_NPA, aes(x = Envi, y = deltaS))+ xlim(0,1) + geom_point( color = "aquamarine4") +
  # geom_smooth(color = "aquamarine4", method = lm, fullrange = FALSE,se=F) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.8, y=-840, label="R2 = -0.14 ns", color="aquamarine4",size=4)  +
  labs(title = "", x = "",y = "") +
  theme_bw() +  annotation_custom(rasterGrob(all_im$Plant), xmin = 0.75, xmax = 1, ymin = -90, ymax = 0)



summary(lm(Env_deltaS_plant_SPA_RA$Envi~Env_deltaS_plant_SPA_RA$deltaS))
h <- ggplot(Env_deltaS_plant_SPA_RA, aes(x = Envi, y = deltaS))+ geom_point( color = "chartreuse3") +xlim(0,1)+
  # geom_smooth(color = "chartreuse3", method = lm, fullrange = FALSE,se=F) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.8, y=-875, label="R2 = 0.14 ns", color="chartreuse3",size=4)  +
  labs(title = "", x = "",y = "") +
  theme_bw() 




summary(lm(Env_deltaS_plant_RA_NPA$Envi~Env_deltaS_plant_RA_NPA$deltaS))
i <- ggplot(Env_deltaS_plant_RA_NPA, aes(x = Envi, y = deltaS))+ geom_point( color = "aquamarine2") +xlim(0,1)+
  geom_smooth(color = " aquamarine2", method = lm, fullrange = FALSE,se=F) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.8, y=-800, label="R2 = 0.25 ***", color="aquamarine2",size=4)  +
  labs(title = "", x = "Habitat dissimilarity",y = "") +
  theme_bw() 



grDevices::pdf(file = here::here("Figures", "Figure4.pdf"), 
               width = 10, height = 10) 
multiplot(a,b,c,d,e,f,g,h,i,cols=3)
dev.off()



