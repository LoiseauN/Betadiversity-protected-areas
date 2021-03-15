rm(list = ls())

## Library ----

library(stringr)
library(ggplot2)
library(grid)
library(png)
library(ggpubr)
library(gridExtra)
## Source multiplot ----

source(here::here("Function", "multiplot.R"))
## Import Icons ----

paths <- list.files(path = here::here("Outputs", "Figure_S5"), pattern = "*.png$", 
                    full.names = TRUE)

files <- list.files(path = here::here("Outputs", "Figure_S5"), pattern = "*.png$", 
                    full.names = FALSE)

all_im <- lapply(paths, png::readPNG)
names(all_im) <- gsub(".png", "", files)

## Prepare Data ----

filenames <- list.files(path = here::here("Outputs", "Figure_S5"), pattern = ".RData$",
                        full.names = FALSE)

files <- list.files(path = here::here("Outputs", "Figure_S5"), pattern = ".RData$",
                    full.names = TRUE)

lapply(files, load, environment())


THR_fish$Protection_status<- factor(THR_fish$Protection_status,levels = c('SPA','RA','NPA'),ordered = TRUE)

THR_fish$dist_cat_n[THR_fish$Protection_status == "SPA"] <- -0.25
THR_fish$dist_cat_n[THR_fish$Protection_status == "RA"] <- 0
THR_fish$dist_cat_n[THR_fish$Protection_status == "NPA"] <- 0.25



a<-ggplot(THR_fish, aes(x=Protection_status, y=Probability, colour=Protection_status)) + geom_violin(show_guide=FALSE)  +
  geom_jitter(width = 0.05,size=2,show_guide=FALSE,alpha=0.55) +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1),  geom="pointrange", size = 1,show_guide=FALSE,colour = "black")+ 
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "THR",
       colour = "Protection_status") +   stat_compare_means(comparisons = list(c("SPA", "NPA"),c("SPA", "RA"),c("RA", "NPA")), tip.length=0.01,
                                                            label = "p.signif",method = "wilcox.test", #label.y = c(100,80,75),
                                                            symnum.args = list(cutpoints = c(0.001, 0.01, 0.05, 0.01, 1), 
                                                                               symbols = c("***", "**", "*", "ns"))) +
  annotation_custom(rasterGrob(all_im$Fish),  xmin = 2.7, xmax = 3.7, ymin = 110, ymax = 120)



LR_fish$Protection_status<- factor(LR_fish$Protection_status,levels = c('SPA','RA','NPA'),ordered = TRUE)

LR_fish$dist_cat_n[LR_fish$Protection_status == "SPA"] <- -0.25
LR_fish$dist_cat_n[LR_fish$Protection_status == "RA"] <- 0
LR_fish$dist_cat_n[LR_fish$Protection_status == "NPA"] <- 0.25


b <- ggplot(LR_fish, aes(x=Protection_status, y=Probability, colour=Protection_status)) + geom_violin(show_guide=FALSE)  +
  geom_jitter(width = 0.05,size=2,show_guide=FALSE,alpha=0.55) +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1),  geom="pointrange", size = 1,show_guide=FALSE,colour = "black")+ 
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "LR",
       colour = "Protection_status") +   stat_compare_means(comparisons = list(c("SPA", "NPA"),c("SPA", "RA"),c("RA", "NPA")), tip.length=0.01,
                                                            label = "p.signif",method = "wilcox.test", #label.y = c(100,80,75),
                                                            symnum.args = list(cutpoints = c(0.001, 0.01, 0.05, 0.01, 1), 
                                                                               symbols = c("***", "**", "*", "ns"))) 



NE_fish$Protection_status<- factor(NE_fish$Protection_status,levels = c('SPA','RA','NPA'),ordered = TRUE)

NE_fish$dist_cat_n[NE_fish$Protection_status == "SPA"] <- -0.25
NE_fish$dist_cat_n[NE_fish$Protection_status == "RA"] <- 0
NE_fish$dist_cat_n[NE_fish$Protection_status == "NPA"] <- 0.25


c <- ggplot(NE_fish, aes(x=Protection_status, y=Probability, colour=Protection_status)) + geom_violin(show_guide=FALSE)  +
  geom_jitter(width = 0.05,size=2,show_guide=FALSE,alpha=0.55) +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1),  geom="pointrange", size = 1,show_guide=FALSE,colour = "black")+ 
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "NE",
       colour = "Protection_status") +   stat_compare_means(comparisons = list(c("SPA", "NPA"),c("SPA", "RA"),c("RA", "NPA")), tip.length=0.01,
                                                            label = "p.signif",method = "wilcox.test", #label.y = c(100,80,75),
                                                            symnum.args = list(cutpoints = c(0.001, 0.01, 0.05, 0.01, 1), 
                                                                               symbols = c("***", "**", "*", "ns"))) 



THR_bird$Protection_status<- factor(THR_bird$Protection_status,levels = c('SPA','RA','NPA'),ordered = TRUE)

THR_bird$dist_cat_n[THR_bird$Protection_status == "SPA"] <- -0.25
THR_bird$dist_cat_n[THR_bird$Protection_status == "RA"] <- 0
THR_bird$dist_cat_n[THR_bird$Protection_status == "NPA"] <- 0.25


d <- ggplot(THR_bird, aes(x=Protection_status, y=Probability, colour=Protection_status)) + geom_violin(show_guide=FALSE)  +
  geom_jitter(width = 0.05,size=2,show_guide=FALSE,alpha=0.55) +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1),  geom="pointrange", size = 1,show_guide=FALSE,colour = "black")+ 
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "THR",
       colour = "Protection_status") +   stat_compare_means(comparisons = list(c("SPA", "NPA"),c("SPA", "RA"),c("RA", "NPA")), tip.length=0.01,
                                                            label = "p.signif",method = "wilcox.test", #label.y = c(100,80,75),
                                                            symnum.args = list(cutpoints = c(0.001, 0.01, 0.05, 0.01, 1), 
                                                                               symbols = c("***", "**", "*", "ns"))) +
  annotation_custom(rasterGrob(all_im$Bird),  xmin = 2.7, xmax = 3.7, ymin = 110, ymax = 120)



LR_bird$Protection_status<- factor(LR_bird$Protection_status,levels = c('SPA','RA','NPA'),ordered = TRUE)

LR_bird$dist_cat_n[LR_bird$Protection_status == "SPA"] <- -0.25
LR_bird$dist_cat_n[LR_bird$Protection_status == "RA"] <- 0
LR_bird$dist_cat_n[LR_bird$Protection_status == "NPA"] <- 0.25


e <- ggplot(LR_bird, aes(x=Protection_status, y=Probability, colour=Protection_status)) + geom_violin(show_guide=FALSE)  +
  geom_jitter(width = 0.05,size=2,show_guide=FALSE,alpha=0.55) +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1),  geom="pointrange", size = 1,show_guide=FALSE,colour = "black")+ 
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "",
       colour = "Protection_status") +   stat_compare_means(comparisons = list(c("SPA", "NPA"),c("SPA", "RA"),c("RA", "NPA")), tip.length=0.01,
                                                            label = "p.signif",method = "wilcox.test", #label.y = c(100,80,75),
                                                            symnum.args = list(cutpoints = c(0.001, 0.01, 0.05, 0.01, 1), 
                                                                               symbols = c("***", "**", "*", "ns"))) 



NE_bird$Protection_status<- factor(NE_bird$Protection_status,levels = c('SPA','RA','NPA'),ordered = TRUE)

NE_bird$dist_cat_n[NE_bird$Protection_status == "SPA"] <- -0.25
NE_bird$dist_cat_n[NE_bird$Protection_status == "RA"] <- 0
NE_bird$dist_cat_n[NE_bird$Protection_status == "NPA"] <- 0.25


f <- ggplot(NE_bird, aes(x=Protection_status, y=Probability, colour=Protection_status)) + geom_violin(show_guide=FALSE)  +
  geom_jitter(width = 0.05,size=2,show_guide=FALSE,alpha=0.55) +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1),  geom="pointrange", size = 1,show_guide=FALSE,colour = "black")+ 
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "",
       colour = "Protection_status") +   stat_compare_means(comparisons = list(c("SPA", "NPA"),c("SPA", "RA"),c("RA", "NPA")), tip.length=0.01,
                                                            label = "p.signif",method = "wilcox.test", #label.y = c(100,80,75),
                                                            symnum.args = list(cutpoints = c(0.001, 0.01, 0.05, 0.01, 1), 
                                                                               symbols = c("***", "**", "*", "ns"))) 


THR_plant$Protection_status<- factor(THR_plant$Protection_status,levels = c('SPA','RA','NPA'),ordered = TRUE)

THR_plant$dist_cat_n[THR_plant$Protection_status == "SPA"] <- -0.25
THR_plant$dist_cat_n[THR_plant$Protection_status == "RA"] <- 0
THR_plant$dist_cat_n[THR_plant$Protection_status == "NPA"] <- 0.25


g <- ggplot(THR_plant, aes(x=Protection_status, y=Probability, colour=Protection_status)) + geom_violin(show_guide=FALSE)  +
  geom_jitter(width = 0.05,size=2,show_guide=FALSE,alpha=0.55) +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1),  geom="pointrange", size = 1,show_guide=FALSE,colour = "black")+ 
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "THR",
       colour = "Protection_status") +   stat_compare_means(comparisons = list(c("SPA", "NPA"),c("SPA", "RA"),c("RA", "NPA")), tip.length=0.01,
                                                            label = "p.signif",method = "wilcox.test", #label.y = c(100,80,75),
                                                            symnum.args = list(cutpoints = c(0.001, 0.01, 0.05, 0.01, 1), 
                                                                               symbols = c("***", "**", "*", "ns"))) +
  annotation_custom(rasterGrob(all_im$Plant),  xmin = 2.7, xmax = 3.7, ymin = 110, ymax = 120)



LR_plant$Protection_status<- factor(LR_plant$Protection_status,levels = c('SPA','RA','NPA'),ordered = TRUE)

LR_plant$dist_cat_n[LR_plant$Protection_status == "SPA"] <- -0.25
LR_plant$dist_cat_n[LR_plant$Protection_status == "RA"] <- 0
LR_plant$dist_cat_n[LR_plant$Protection_status == "NPA"] <- 0.25


h <- ggplot(LR_plant, aes(x=Protection_status, y=Probability, colour=Protection_status))+ geom_violin(show_guide=FALSE)  +
  geom_jitter(width = 0.05,size=2,show_guide=FALSE,alpha=0.55) +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1),  geom="pointrange", size = 1,show_guide=FALSE,colour = "black")+ 
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "",
       colour = "Protection_status") +   stat_compare_means(comparisons = list(c("SPA", "NPA"),c("SPA", "RA"),c("RA", "NPA")), tip.length=0.01,
                                                            label = "p.signif",method = "wilcox.test", #label.y = c(100,80,75),
                                                            symnum.args = list(cutpoints = c(0.001, 0.01, 0.05, 0.01, 1), 
                                                                               symbols = c("***", "**", "*", "ns"))) 



NE_plant$Protection_status<- factor(NE_plant$Protection_status,levels = c('SPA','RA','NPA'),ordered = TRUE)

NE_plant$dist_cat_n[NE_plant$Protection_status == "SPA"] <- -0.25
NE_plant$dist_cat_n[NE_plant$Protection_status == "RA"] <- 0
NE_plant$dist_cat_n[NE_plant$Protection_status == "NPA"] <- 0.25


i <- ggplot(NE_plant, aes(x=Protection_status, y=Probability, colour=Protection_status)) + geom_violin(show_guide=FALSE)  +
  geom_jitter(width = 0.05,size=2,show_guide=FALSE,alpha=0.55) +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1),  geom="pointrange", size = 1,show_guide=FALSE,colour = "black")+ 
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "",
       x = "",
       y = "",
       colour = "Protection_status") +   stat_compare_means(comparisons = list(c("SPA", "NPA"),c("SPA", "RA"),c("RA", "NPA")), tip.length=0.01,
                                                            label = "p.signif",method = "wilcox.test", #label.y = c(100,80,75),
                                                            symnum.args = list(cutpoints = c(0.001, 0.01, 0.05, 0.01, 1), 
                                                                               symbols = c("***", "**", "*", "ns"))) 

grDevices::pdf(file = here::here("Figures", "FigureS5.pdf"), 
               width =12 , height = 12) 
grid.arrange(a,d,g,
             b,e,h,
             c,f,i, ncol=3)
dev.off()

