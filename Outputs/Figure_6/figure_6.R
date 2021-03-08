
## Library ----

library(stringr)
library(ggplot2)
library(grid)
library(png)
## Source multiplot ----

source(here::here("Function", "multiplot.R"))

## Import Icons ----

paths <- list.files(path = here::here("Outputs", "Figure_6"), pattern = "*.png$", 
                    full.names = TRUE)

files <- list.files(path = here::here("Outputs", "Figure_6"), pattern = "*.png$", 
                    full.names = FALSE)

all_im <- lapply(paths, png::readPNG)
names(all_im) <- gsub(".png", "", files)


#######################################

Env_turnover_fish_SPA_NPA <- data.frame(Envi=apply(Fish_BetaClose_NonP_DissimEnv_50km,1,mean),
                                        Turnover=apply(Fish_BetaClose_NonP_TUR_50km,1,mean))
a <- ggplot(Env_turnover_fish_SPA_NPA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "darkblue") +
  # geom_smooth(color = "darkblue", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.80, y=0.05, label="R2 = 0.12 ns", color="darkblue",size=4)  +
  labs(title = "", x = "",y = "SPA vs NPA") +
  theme_bw() +  annotation_custom(rasterGrob(img), xmin = 0.75, xmax = 1, ymin = 0.8, ymax = 0.97)
a


Env_turnover_fish_SPA_RA <- data.frame(Envi=apply(Fish_BetaClose_RESTRICT_DissimEnv_50km,1,mean),
                                       Turnover=apply(Fish_BetaClose_RESTRICT_TUR_50km,1,mean))
b <- ggplot(Env_turnover_fish_SPA_RA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "cyan4") +
  # geom_smooth(color = "cyan4", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.80, y=0.05, label="R2 = 0.15 ns", color="cyan4",size=4)  +
  labs(title = "", x = "",y = "SPA vs RA") +
  theme_bw() 
b

Env_turnover_fish_RA_NPA <- data.frame(Envi=apply(Fish_Betarestricte_NonP_DissimEnv_50km,1,mean),
                                       Turnover=apply(Fish_Betarestricte_NonP_TUR_50km,1,mean))
c <- ggplot(Env_turnover_fish_RA_NPA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "cyan3") +
  #  geom_smooth(color = " cyan3", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.80, y=0.05, label="R2 = 0.06 ns", color="cyan3",size=4)  +
  labs(title = "", x = "Habitat dissimilarity",y = "RA vs NPA") +
  theme_bw() 

c


#######################################



Env_turnover_bird_SPA_NPA <- data.frame(Envi=apply(Dissim_habSPA_NPA_TOT_50km_Bird,1,mean),
                                        Turnover=apply(Bird_BetaClose_NonP_TUR_50km,1,mean))
d <- ggplot(Env_turnover_bird_SPA_NPA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "coral4") +
  geom_smooth(color = "coral4", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.80, y=0.05, label="R2 = 0.18 ***", color="coral4",size=4)  +
  labs(title = "", x = "",y = "") +
  theme_bw() +  annotation_custom(rasterGrob(img2), xmin = 0.75, xmax = 1, ymin = 0.8, ymax = 0.97)
d


Env_turnover_bird_SPA_RA <- data.frame(Envi=apply(Dissim_habSPA_RA_TOT_50km_Bird,1,mean),
                                       Turnover=apply(Bird_BetaClose_RESTRICT_TUR_50km,1,mean))
e <- ggplot(Env_turnover_bird_SPA_RA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "chocolate1") +
  geom_smooth(color = "chocolate1", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.80, y=0.05, label="R2 = 0.09 *", color="chocolate1",size=4)  +
  labs(title = "", x = "",y = "") +
  theme_bw() 
e

Env_turnover_bird_RA_NPA <- data.frame(Envi=apply(Dissim_habRA_NPA_TOT_50km_Bird,1,mean),
                                       Turnover=apply(Bird_Betarestricte_NonP_TUR_50km,1,mean))
f <- ggplot(Env_turnover_bird_RA_NPA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "coral1") +
  geom_smooth(color = " coral1", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.80, y=0.05, label="R2 = 0.24 ***", color="coral1",size=4)  +
  labs(title = "", x = "Habitat dissimilarity",y = "") +
  theme_bw() 

f


#######################################


Env_turnover_plant_SPA_NPA <- data.frame(Envi=apply(Dissim_habSPA_NPA_TOT_50km_Plant,1,mean),
                                         Turnover=apply(Plant_BetaClose_NonP_TUR_50km,1,mean))
g <- ggplot(Env_turnover_plant_SPA_NPA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "aquamarine4") +
  #geom_smooth(color = "aquamarine4", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ 
  geom_text(x=0.80, y=0.05, label="R2 = 0.12 ns", color="aquamarine4",size=4)  +
  labs(title = "", x = "",y = "") +
  theme_bw() +  annotation_custom(rasterGrob(img3), xmin = 0.75, xmax = 1, ymin = 0.8, ymax = 0.97)
g

Env_turnover_plant_SPA_RA <- data.frame(Envi=apply(Dissim_habSPA_RA_TOT_50km_Plant,1,mean),
                                        Turnover=apply(Plant_BetaClose_Restrict_TUR_50km,1,mean))
h <- ggplot(Env_turnover_plant_SPA_RA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "chartreuse3") +
  #geom_smooth(color = "chartreuse3", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ 
  geom_text(x=0.80, y=0.05, label="R2 = 0.30 ns", color="chartreuse3",size=4)  +
  labs(title = "", x = "",y = "") +
  theme_bw() 
h

Env_turnover_plant_RA_NPA <- data.frame(Envi=apply(Dissim_habRA_NPA_TOT_50km_Plant,1,mean),
                                        Turnover=apply(Plant_BetaRestricted_NonP_TUR_50km,1,mean))
i <- ggplot(Env_turnover_plant_RA_NPA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "aquamarine2") +
  geom_smooth(color = " aquamarine2", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.80, y=0.05, label="R2 = 0.40 ***", color="aquamarine2",size=4)  +
  labs(title = "", x = "Habitat dissimilarity",y = "") +
  theme_bw() 

i

multiplot(a,b,c,d,e,f,g,h,i,cols=3)





