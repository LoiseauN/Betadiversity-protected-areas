rm(list=ls())
## Library ----

library(stringr)
library(ggplot2)
library(grid)
library(png)
library(gridExtra)
library(eulerr)
## Source multiplot ----

source(here::here("Function", "multiplot.R"))


## Prepare Data ----

filenames <- list.files(path = here::here("Outputs", "Figure_7"), pattern = ".RData$",
                        full.names = FALSE)

files <- list.files(path = here::here("Outputs", "Figure_7"), pattern = ".RData$",
                    full.names = TRUE)

lapply(files, load, environment())





grDevices::pdf(file = here::here("Figures", "Figure7.pdf"), 
               width = 11.7, height = 8.3) #SAVE A4
grid.arrange(plot(venn_fish_all, quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","cadetblue3","steelblue3"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             plot(venn_bird_all,quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","chocolate1","coral1"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             plot(venn_plant_all, quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","chartreuse2","chartreuse4"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             plot(venn_fish_THR, quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","cadetblue3","steelblue3"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             plot(venn_bird_THR,quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","chocolate1","coral1"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             plot(venn_plant_THR, quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","chartreuse2","chartreuse4"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             plot(venn_fish_LR, quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","cadetblue3","steelblue3"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             plot(venn_bird_LR,quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","chocolate1","coral1"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             plot(venn_plant_LR, quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","chartreuse2","chartreuse4"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             nrow=3)
dev.off()


#PNG
png(
  file      = here::here("Figures", "Figure7.png"),
  width     = 11.7,
  height    = 8.3,
  units     = "in",
  res       = 600,
  pointsize = 38
)
grid.arrange(plot(venn_fish_all, quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","cadetblue3","steelblue3"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             plot(venn_bird_all,quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","chocolate1","coral1"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             plot(venn_plant_all, quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","chartreuse2","chartreuse4"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             plot(venn_fish_THR, quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","cadetblue3","steelblue3"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             plot(venn_bird_THR,quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","chocolate1","coral1"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             plot(venn_plant_THR, quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","chartreuse2","chartreuse4"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             plot(venn_fish_LR, quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","cadetblue3","steelblue3"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             plot(venn_bird_LR,quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","chocolate1","coral1"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             plot(venn_plant_LR, quantities = list(col = "black", cex = 1),lty = c(3,2,1),fill = c("grey","chartreuse2","chartreuse4"),labels = list(col = "black", cex = 1),alpha=c(0.1,0.3,0.6)),
             nrow=3)
dev.off()
