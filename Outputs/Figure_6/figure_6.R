rm(list=ls())

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

## Prepare Data ----

filenames <- list.files(path = here::here("Outputs", "Figure_6"), pattern = ".RData$",
                        full.names = FALSE)

files <- list.files(path = here::here("Outputs", "Figure_6"), pattern = ".RData$",
                    full.names = TRUE)

lapply(files, load, environment())

#######################################


a <- ggplot(Env_turnover_fish_SPA_NPA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "darkblue") +
  # geom_smooth(color = "darkblue", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.80, y=0.05, label="R2 = 0.12 ns", color="darkblue",size=4)  +
  labs(title = "", x = "",y = "SPA vs NPA") +
  theme_bw() +  annotation_custom(rasterGrob(all_im$Fish), xmin = 0.75, xmax = 1, ymin = 0.8, ymax = 0.97)



b <- ggplot(Env_turnover_fish_SPA_RA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "cyan4") +
  # geom_smooth(color = "cyan4", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.80, y=0.05, label="R2 = 0.15 ns", color="cyan4",size=4)  +
  labs(title = "", x = "",y = "SPA vs RA") +
  theme_bw() 

c <- ggplot(Env_turnover_fish_RA_NPA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "cyan3") +
  #  geom_smooth(color = " cyan3", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.80, y=0.05, label="R2 = 0.06 ns", color="cyan3",size=4)  +
  labs(title = "", x = "Habitat dissimilarity",y = "RA vs NPA") +
  theme_bw() 


#######################################


d <- ggplot(Env_turnover_bird_SPA_NPA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "coral4") +
  geom_smooth(color = "coral4", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.80, y=0.05, label="R2 = 0.18 ***", color="coral4",size=4)  +
  labs(title = "", x = "",y = "") +
  theme_bw() +  annotation_custom(rasterGrob(all_im$Bird), xmin = 0.75, xmax = 1, ymin = 0.8, ymax = 0.97)

e <- ggplot(Env_turnover_bird_SPA_RA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "chocolate1") +
  geom_smooth(color = "chocolate1", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.80, y=0.05, label="R2 = 0.09 *", color="chocolate1",size=4)  +
  labs(title = "", x = "",y = "") +
  theme_bw() 


f <- ggplot(Env_turnover_bird_RA_NPA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "coral1") +
  geom_smooth(color = " coral1", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.80, y=0.05, label="R2 = 0.24 ***", color="coral1",size=4)  +
  labs(title = "", x = "Habitat dissimilarity",y = "") +
  theme_bw() 


#######################################


g <- ggplot(Env_turnover_plant_SPA_NPA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "aquamarine4") +
  #geom_smooth(color = "aquamarine4", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ 
  geom_text(x=0.80, y=0.05, label="R2 = 0.12 ns", color="aquamarine4",size=4)  +
  labs(title = "", x = "",y = "") +
  theme_bw() +  annotation_custom(rasterGrob(all_im$Plant), xmin = 0.75, xmax = 1, ymin = 0.8, ymax = 0.97)

h <- ggplot(Env_turnover_plant_SPA_RA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "chartreuse3") +
  #geom_smooth(color = "chartreuse3", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ 
  geom_text(x=0.80, y=0.05, label="R2 = 0.30 ns", color="chartreuse3",size=4)  +
  labs(title = "", x = "",y = "") +
  theme_bw() 

i <- ggplot(Env_turnover_plant_RA_NPA, aes(x = Envi, y = Turnover))+ ylim(0,1) +xlim(0,1)+geom_point( color = "aquamarine2") +
  geom_smooth(color = " aquamarine2", method = lm, fullrange = FALSE,se=F) +
  theme_bw() + theme(legend.position = "none")+ geom_text(x=0.80, y=0.05, label="R2 = 0.40 ***", color="aquamarine2",size=4)  +
  labs(title = "", x = "Habitat dissimilarity",y = "") +
  theme_bw() 


grDevices::pdf(file = here::here("Figures", "Figure6.pdf"), 
               width = 10, height = 10) 
print(multiplot(a,b,c,d,e,f,g,h,i,cols=3))
dev.off()


#PNG
png(
  file      = here::here("Figures", "Figure6.png"),
  width     = 10,
  height    = 10,
  units     = "in",
  res       = 600,
  pointsize = 38
)
multiplot(a,b,c,d,e,f,g,h,i,cols=3)
dev.off()