###########################################################################################################
# Analyse beta diversity between MPA and non MPA
#
# Author : Nicolas Loiseau, 
# Date : 02-10-2017
# 
# R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
# Copyright (C) 2016 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
#
###########################################################################################################
###########################################################################################################

##
# library
library(ecodist)
library(betapart)
library(ade4)
library(vegan)
library(sp)
library(stringr)
library(ggplot2)
library(cluster)
library(rgeos)
library(sp)
library(rgdal)
library(parallel)

'%!in%' <- function(x,y)!('%in%'(x,y))


load("occ.RData")
load("InfoTOTAL2.RData")

listMPA_SPA <- unique(rbind(subset(InfoTOTAL2,InfoTOTAL2$V2 =="Integral")))
listMPA_SPA<-unique(listMPA_SPA$namesMPA)

listMPA_RA <- unique(rbind(subset(InfoTOTAL2,InfoTOTAL2$V2 =="Restreint")))
listMPA_RA2<-unique(rownames(listMPA_RA))
listMPA_RA<-unique(listMPA_RA$namesMPA)
listMPA_RA<-listMPA_RA[-3]

listMPA_nonprotect<- unique(rbind(subset(InfoTOTAL2,InfoTOTAL2$V2 =="NoPro")))
listMPA_nonprotect<-unique(rownames(listMPA_nonprotect))

load("IDTransect100km_SPAvsNPA.RData")
load("IDTransect100km_SPAvsRA.RData")
load("IDTransect100km_RAvsNPA.RData")

load("IDTransect50km_SPAvsNPA.RData")
load("IDTransect50km_SPAvsRA.RData")
load("IDTransect50km_RAvsNPA.RData")

load("IDTransect10km_SPAvsNPA.RData")
load("IDTransect10km_SPAvsRA.RData")
load("IDTransect10km_RAvsNPA.RData")


reps=1000 # Rep modelnull
repboot=1000# Rep boostrap
core=50# number of core

# SPA VS NPA --- 

Plant_BetaSPA_NPA_TOT_50km<-matrix(NA,length(listMPA_SPA),repboot)
Plant_BetaSPA_NPA_TUR_50km<-Plant_BetaSPA_NPA_TOT_50km
Plant_BetaSPA_NPA_NES_50km<-Plant_BetaSPA_NPA_TOT_50km

Plant_S_50km_Unique_SPA.SPA_NPA<-matrix(NA,length(listMPA_SPA),repboot)
Plant_S_50km_Unique_NPA.SPA_NPA<-Plant_S_50km_Unique_SPA.SPA_NPA
Plant_S_50km_Shared_SPA_NPA.SPA_NPA<-Plant_S_50km_Unique_SPA.SPA_NPA

ses.tot_SPA_NPA_50km <- Plant_BetaSPA_NPA_TOT_50km
ses.tur_SPA_NPA_50km <- Plant_BetaSPA_NPA_TOT_50km
ses.nes_SPA_NPA_50km<- Plant_BetaSPA_NPA_TOT_50km

for (i in 1: length(listMPA_SPA)) {# For each SPA
  
  #Transect SPA
  IDReserveSPA<-subset(InfoTOTAL2,InfoTOTAL2$namesMPA==listMPA_SPA[i])
  IDReserveSPA<-subset(IDReserveSPA,IDReserveSPA$V2 =="Integral")
  
  if (dim(IDReserveSPA)[1]==1)  { Transect_SPA<-t(as.data.frame(occ[rownames(occ)%in%rownames(IDReserveSPA),]))
  rownames(Transect_SPA)<-rownames(IDReserveSPA)  } 
  if (dim(IDReserveSPA)[1]>1) {  Transect_SPA<-as.data.frame(occ[rownames(occ)%in%rownames(IDReserveSPA),])} 
  
  #Transect NPA
  Transect50km_NPA<-occ[rownames(occ)%in%IDTransect50km_SPAvsNPA[[i]],]
  
  for (k in 1:repboot){ #Bootstrap due to different sampling effort
    
    if(dim(Transect_SPA)[1]==1){ 
      Group_NPA_Sample<-as.matrix(Transect50km_NPA[sample(1:nrow(Transect50km_NPA),dim(Transect_SPA)[1],replace=F),])
      Group_SPA2<-apply(Transect_SPA, 2,sum)
      Group_NPA2<-apply(Group_NPA_Sample, 2,sum)
      
      nullbeta<-matrix(NA,3,500)
      for (l in 1:reps){
        nullbeta[1,l]<-NA
        nullbeta[2,l]<-NA
        nullbeta[3,l]<-NA
        print(paste ("l",l))}
    }
    
    if (dim(Transect_SPA)[1]>dim(Transect50km_NPA)[1]){
      
      Group_SPA_Sample<-as.matrix(Transect_SPA[sample(1:nrow(Transect_SPA),dim(Transect50km_NPA)[1],replace=F),])
      Group_SPA2<-apply(Group_SPA_Sample,2,sum)
      Group_NPA2<-apply(Transect50km_NPA,2,sum)
      
      Compa_NULL<-rbind(Group_SPA_Sample,Transect50km_NPA)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Group_SPA_Sample)[1],],2,sum),apply(random_mat[,,e][(dim(Transect50km_NPA)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    if (dim(Transect_SPA)[1]<dim(Transect50km_NPA)[1]){
      Group_NPA_Sample<-as.matrix(Transect50km_NPA[sample(1:nrow(Transect50km_NPA),dim(Transect_SPA)[1],replace=F),])
      Group_SPA2<-apply(Transect_SPA, 2,sum)
      Group_NPA2<-apply(Group_NPA_Sample, 2,sum)
      
      Compa_NULL<-rbind(Transect_SPA,Group_NPA_Sample)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Transect_SPA)[1],],2,sum),apply(random_mat[,,e][(dim(Group_NPA_Sample)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    nullbeta <- t( do.call(rbind.data.frame, beta_null))
    
    Compa<-rbind(Group_SPA2,Group_NPA2)
    Compa[Compa>0]<-1
    BETAX<-beta.pair(Compa, index.family="jaccard")
    
    Plant_BetaSPA_NPA_TOT_50km[i,k]<-BETAX$beta.jac
    Plant_BetaSPA_NPA_TUR_50km[i,k]<-BETAX$beta.jtu
    Plant_BetaSPA_NPA_NES_50km[i,k]<-BETAX$beta.jne
    
    # SES=(obs-mean(rand))/sd(rand)
    ses.tot_SPA_NPA_50km[i,k] = (BETAX$beta.jac - mean(nullbeta[3,]))/sd(nullbeta[3,])
    ses.tur_SPA_NPA_50km[i,k] = (BETAX$beta.jtu - mean(nullbeta[1,]))/sd(nullbeta[1,])
    ses.nes_SPA_NPA_50km[i,k] = (BETAX$beta.jne - mean(nullbeta[2,]))/sd(nullbeta[2,])
    
    print(paste ("k",k))
    
    Plant_S_50km_Unique_SPA.SPA_NPA[i,k]<-sum(Compa[1,])
    Plant_S_50km_Unique_NPA.SPA_NPA[i,k]<-sum(Compa[2,])
    Plant_S_50km_Shared_SPA_NPA.SPA_NPA[i,k]<-length(Compa[,apply(Compa,2,sum)==2])
    
    save(Plant_BetaSPA_NPA_TOT_50km,file="Plant_BetaSPA_NPA_TOT_50km.RData")
    save(Plant_BetaSPA_NPA_TUR_50km,file="Plant_BetaSPA_NPA_TUR_50km.RData")
    save(Plant_BetaSPA_NPA_NES_50km,file="Plant_BetaSPA_NPA_NES_50km.RData")
    
    save(ses.tot_SPA_NPA_50km,file="ses.tot_SPA_NPA_50km.RData")
    save(ses.tur_SPA_NPA_50km,file="ses.tur_SPA_NPA_50km.RData")
    save(ses.nes_SPA_NPA_50km,file="ses.nes_SPA_NPA_50km.RData")

    save(Plant_S_50km_Unique_SPA.SPA_NPA,file="Plant_S_50km_Unique_SPA.SPA_NPA.RData")
    save(Plant_S_50km_Unique_NPA.SPA_NPA,file="Plant_S_50km_Unique_NPA.SPA_NPA.RData")
    save(Plant_S_50km_Shared_SPA_NPA.SPA_NPA,file="Plant_S_50km_Shared_SPA_NPA.SPA_NPA.RData")
    
  }
  
  print(paste ("i",i))
}



# SPA VS RA ---
Plant_BetaSPA_RA_TOT_50km<-matrix(NA,length(listMPA_SPA),50)
Plant_BetaSPA_RA_TUR_50km<-Plant_BetaSPA_RA_TOT_50km
Plant_BetaSPA_RA_NES_50km<-Plant_BetaSPA_RA_TOT_50km

Plant_S_50km_Unique_SPA.SPA_RA<-matrix(NA,length(listMPA_SPA),50)
Plant_S_50km_Unique_RA.SPA_RA<-Plant_S_50km_Unique_SPA.SPA_RA
Plant_S_50km_Shared_SPA_RA.SPA_RA<-Plant_S_50km_Unique_SPA.SPA_RA


ses.tot_SPA_RA_50km <- Plant_BetaSPA_RA_TOT_50km
ses.tur_SPA_RA_50km <- Plant_BetaSPA_RA_TOT_50km
ses.nes_SPA_RA_50km<- Plant_BetaSPA_RA_TOT_50km


for (i in 1: length(listMPA_SPA)) {# Pour chaque r??serve
  
  #Transect SPA
  IDReserveSPA<-subset(InfoTOTAL2,InfoTOTAL2$namesMPA==listMPA_SPA[i])
  IDReserveSPA<-subset(IDReserveSPA,IDReserveSPA$V2 =="Integral")

  if (dim(IDReserveSPA)[1]==1)  { Transect_SPA<-t(as.data.frame(occ[rownames(occ)%in%rownames(IDReserveSPA),]))
  rownames(Transect_SPA)<-rownames(IDReserveSPA)  } 
  if (dim(IDReserveSPA)[1]>1) {  Transect_SPA<-as.data.frame(occ[rownames(occ)%in%rownames(IDReserveSPA),])} 
  
  #Transect RA
  Transect50km_RA<-occ[rownames(occ)%in%IDTransect50km_SPAvsRA[[i]],]
  
  for (k in 1:repboot){ #Bootstrap due to different sampling effort
    
    if(dim(Transect_SPA)[1]==1){ 
      Group_RA_Sample<-as.matrix(Transect50km_RA[sample(1:nrow(Transect50km_RA),dim(Transect_SPA)[1],replace=F),])
      Group_SPA2<-apply(Transect_SPA, 2,sum)
      Group_RA2<-apply(Group_RA_Sample, 2,sum)
      
      nullbeta<-matrix(NA,3,reps)
      for (l in 1:reps){
        nullbeta[1,l]<-NA
        nullbeta[2,l]<-NA
        nullbeta[3,l]<-NA
        print(paste ("l",l))}
    }
    
    if (dim(Transect_SPA)[1]>dim(Transect50km_RA)[1]){
      
      Group_SPA_Sample<-as.matrix(Transect_SPA[sample(1:nrow(Transect_SPA),dim(Transect50km_RA)[1],replace=F),])
      Group_SPA2<-apply(Group_SPA_Sample,2,sum)
      Group_RA2<-apply(Transect50km_RA,2,sum)
      
      Compa_NULL<-rbind(Group_SPA_Sample,Transect50km_RA)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Group_SPA_Sample)[1],],2,sum),apply(random_mat[,,e][(dim(Transect50km_RA)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    if (dim(Transect_SPA)[1]<dim(Transect50km_RA)[1]){
      Group_RA_Sample<-as.matrix(Transect50km_RA[sample(1:nrow(Transect50km_RA),dim(Transect_SPA)[1],replace=F),])
      Group_SPA2<-apply(Transect_SPA, 2,sum)
      Group_RA2<-apply(Group_RA_Sample, 2,sum)
      
      Compa_NULL<-rbind(Transect_SPA,Group_RA_Sample)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Transect_SPA)[1],],2,sum),apply(random_mat[,,e][(dim(Group_RA_Sample)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    nullbeta <- t( do.call(rbind.data.frame, beta_null))
    
    Compa<-rbind(Group_SPA2,Group_RA2)
    Compa[Compa>0]<-1
    BETAX<-beta.pair(Compa, index.family="jaccard")
    
    Plant_BetaSPA_RA_TOT_50km[i,k]<-BETAX$beta.jac
    Plant_BetaSPA_RA_TUR_50km[i,k]<-BETAX$beta.jtu
    Plant_BetaSPA_RA_NES_50km[i,k]<-BETAX$beta.jne
    
    # SES=(obs-mean(rand))/sd(rand)
    ses.tot_SPA_RA_50km[i,k] = (BETAX$beta.jac - mean(nullbeta[3,]))/sd(nullbeta[3,])
    ses.tur_SPA_RA_50km[i,k] = (BETAX$beta.jtu - mean(nullbeta[1,]))/sd(nullbeta[1,])
    ses.nes_SPA_RA_50km[i,k] = (BETAX$beta.jne - mean(nullbeta[2,]))/sd(nullbeta[2,])
    
     print(paste ("k",k))
  }
  
  save(Plant_BetaSPA_RA_TOT_50km,file="Plant_BetaSPA_RA_TOT_50km.RData")
  save(Plant_BetaSPA_RA_TUR_50km,file="Plant_BetaSPA_RA_TUR_50km.RData")
  save(Plant_BetaSPA_RA_NES_50km,file="Plant_BetaSPA_RA_NES_50km.RData")
  
  save(ses.tot_SPA_RA_50km,file="ses.tot_SPA_RA_50km.RData")
  save(ses.tur_SPA_RA_50km,file="ses.tur_SPA_RA_50km.RData")
  save(ses.nes_SPA_RA_50km,file="ses.nes_SPA_RA_50km.RData")
  
  save(Plant_S_50km_Unique_SPA.SPA_RA,file="Plant_S_50km_Unique_SPA.SPA_RA.RData")
  save(Plant_S_50km_Unique_RA.SPA_RA,file="Plant_S_50km_Unique_RA.SPA_RA.RData")
  save(Plant_S_50km_Shared_SPA_RA.SPA_RA,file="Plant_S_50km_Shared_SPA_RA.SPA_RA.RData")
  
  print(paste ("i",i))
}



#RA VS NPA----

Plant_BetaRA_NPA_TOT_50km<-matrix(NA,length(listMPA_RA),repboot)
Plant_BetaRA_NPA_TUR_50km<-Plant_BetaRA_NPA_TOT_50km
Plant_BetaRA_NPA_NES_50km<-Plant_BetaRA_NPA_TOT_50km

Plant_S_50km_Unique_RA.RA_NPA<-matrix(NA,length(listMPA_RA),repboot)
Plant_S_50km_Unique_NPA.RA_NPA<-Plant_S_50km_Unique_RA.RA_NPA
Plant_S_50km_Shared_RA_NPA.RA_NPA<-Plant_S_50km_Unique_RA.RA_NPA

ses.tot_RA_NPA_50km <- Plant_BetaRA_NPA_TOT_50km
ses.tur_RA_NPA_50km <- Plant_BetaRA_NPA_TOT_50km
ses.nes_RA_NPA_50km<- Plant_BetaRA_NPA_TOT_50km

for (i in 1:length(listMPA_RA)) {# Pour chaque r??serve
  
  #Transect RA
  IDReserveRA<-subset(InfoTOTAL2,InfoTOTAL2$namesMPA==listMPA_RA[i])
  IDReserveRA<-subset(IDReserveRA,IDReserveRA$V2 =="Restreint")
  
  if (dim(IDReserveRA)[1]==1)  { IDReserveRA<-t(as.data.frame(occ[rownames(occ)%in%rownames(IDReserveRA),]))
  rownames(IDReserveRA)<-rownames(IDReserveRA)  } 
  if (dim(IDReserveRA)[1]>1) {  Transect_RA<-as.data.frame(occ[rownames(occ)%in%rownames(IDReserveRA),])} 
  
  #Transect NPA
  Transect50km_NPA<-occ[rownames(occ)%in%IDTransect50km_RAvsNPA[[i]],]
  
  for (k in 1:repboot){ #Bootstrap due to different sampling effort
    
    if(dim(Transect_RA)[1]==1){ 
      Group_NPA_Sample<-as.matrix(Transect50km_NPA[sample(1:nrow(Transect50km_NPA),dim(Transect_RA)[1],replace=F),])
      Group_RA2<-apply(Transect_RA, 2,sum)
      Group_NPA2<-apply(Group_NPA_Sample, 2,sum)
      
      nullbeta<-matrix(NA,3,50)
      for (l in 1:reps){
        nullbeta[1,l]<-NA
        nullbeta[2,l]<-NA
        nullbeta[3,l]<-NA
        print(paste ("l",l))}
    }
    
    
    if (dim(Transect_RA)[1]>dim(Transect50km_NPA)[1]){
      
      Group_RA_Sample<-as.matrix(Transect_RA[sample(1:nrow(Transect_RA),dim(Transect50km_NPA)[1],replace=F),])
      Group_RA2<-apply(Group_RA_Sample,2,sum)
      Group_NPA2<-apply(Transect50km_NPA,2,sum)
      
      Compa_NULL<-rbind(Group_RA_Sample,Transect50km_NPA)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
     
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Group_RA_Sample)[1],],2,sum),apply(random_mat[,,e][(dim(Transect50km_NPA)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    if (dim(Transect_RA)[1]<dim(Transect50km_NPA)[1]){
      Group_NPA_Sample<-as.matrix(Transect50km_NPA[sample(1:nrow(Transect50km_NPA),dim(Transect_RA)[1],replace=F),])
      Group_RA2<-apply(Transect_RA, 2,sum)
      Group_NPA2<-apply(Group_NPA_Sample, 2,sum)
      
      Compa_NULL<-rbind(Transect_RA,Group_NPA_Sample)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Transect_RA)[1],],2,sum),apply(random_mat[,,e][(dim(Group_NPA_Sample)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    
    nullbeta <- t( do.call(rbind.data.frame, beta_null))
    
    Compa<-rbind(Group_RA2,Group_NPA2)
    Compa[Compa>0]<-1
    BETAX<-beta.pair(Compa, index.family="jaccard")
    
    Plant_BetaRA_NPA_TOT_50km[i,k]<-BETAX$beta.jac
    Plant_BetaRA_NPA_TUR_50km[i,k]<-BETAX$beta.jtu
    Plant_BetaRA_NPA_NES_50km[i,k]<-BETAX$beta.jne
    
    # SES=(obs-mean(rand))/sd(rand)
    ses.tot_RA_NPA_50km[i,k] = (BETAX$beta.jac - mean(nullbeta[3,]))/sd(nullbeta[3,])
    ses.tur_RA_NPA_50km[i,k] = (BETAX$beta.jtu - mean(nullbeta[1,]))/sd(nullbeta[1,])
    ses.nes_RA_NPA_50km[i,k] = (BETAX$beta.jne - mean(nullbeta[2,]))/sd(nullbeta[2,])

    Plant_S_50km_Unique_RA.RA_NPA[i,k]<-sum(Compa[1,])
    Plant_S_50km_Unique_NPA.RA_NPA[i,k]<-sum(Compa[2,])
    Plant_S_50km_Shared_RA_NPA.RA_NPA[i,k]<-length(Compa[,apply(Compa,2,sum)==2])
    
    print(paste ("k",k))
    
    save(Plant_BetaRA_NPA_TOT_50km,file="Plant_BetaRA_NPA_TOT_50km.RData")
    save(Plant_BetaRA_NPA_TUR_50km,file="Plant_BetaRA_NPA_TUR_50km.RData")
    save(Plant_BetaRA_NPA_NES_50km,file="Plant_BetaRA_NPA_NES_50km.RData")
    
    save(ses.tot_RA_NPA_50km,file="ses.tot_RA_NPA_50km.RData")
    save(ses.tur_RA_NPA_50km,file="ses.tur_RA_NPA_50km.RData")
    save(ses.nes_RA_NPA_50km,file="ses.nes_RA_NPA_50km.RData")

    save(Plant_S_50km_Unique_RA.RA_NPA,file="Plant_S_50km_Unique_RA.RA_NPA.RData")
    save(Plant_S_50km_Unique_NPA.RA_NPA,file="Plant_S_50km_Unique_NPA.RA_NPA.RData")
    save(Plant_S_50km_Shared_RA_NPA.RA_NPA,file="Plant_S_50km_Shared_RA_NPA.RA_NPA.RData")
    
  }
  
  print(paste ("i",i))
}

