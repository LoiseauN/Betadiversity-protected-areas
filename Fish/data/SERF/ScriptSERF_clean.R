#Algae      Coral      Other

###########################################################################################################
setwd("~/Documents/Postdoc MARBEC/BETA PROTECTED AREA")
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
require("maptools")
require("rgdal")
require("sp")
library("plyr")
library("dplyr")
library("sf")
library(ggthemes)
library(parallel)
########################################################################
# Load data ------

# All fish recorded
record<-read.csv2("ALL FISH INDIV RECORDS_GOOD - Kulbicki changed.csv")
# Environmental variable
env<-read.csv2("Environmental sites data.csv")
# Famille
fam<-read.csv2("Families data.csv")
# Species data
trait<-read.csv2("Species data.csv")
# Social site
site<-read.csv2("Social sites data.csv",sep=",")
# Country
country<-read.csv2("Country data.csv",sep=",")

# Travailler que sur les transect dont les uniqueSite sont dans DATA
DATA<-read.csv("SERF DATA FOR GEORGIE_2015_11_18.csv",sep=";")

# Information de protection
df.protec<-read.csv("df.protec.csv",sep=";",row.names=1)
# Information de Lat/lon
df.geo<-read.csv("df_geo.csv",row.names=1,sep=",")
# Information habitat
df.hab<-read.csv("df.hab.csv",row.names=1,sep=",")

load("geodist.RData")
load("habdist.RData")
load("protecdist.RData")




#env<-rbind(subset(env,env$DepthCategory=="4-10m"),
#           subset(env,env$DepthCategory=="0-4m"),
#           subset(env,env$DepthCategory==">10m"))


#transformer data en tableau de contingence UniqueSite/Esp
record<-record[record$UniqueSite%in%env$UniqueSite,]
record<-record[record$UniqueSite%in%DATA$UniqueSite,]
env<-env[env$UniqueSite  %in%record$UniqueSite,]
DATA<-DATA[DATA$UniqueSite%in%record$UniqueSite,]

table1<-xtabs(Number ~ UniqueTransect + FullSpecies , data = record)
table1<-table1[rownames(table1) %in% record$UniqueTransect,]

occ<-as.data.frame.matrix(table1)
occ[occ>0]<-1
occ<-occ[,apply(occ,2,sum)>0]

#Enl??ve les requins
Shark<-unique(rbind(subset(record,record$Family=="Ginglymostomatidae")[17], subset(record,record$Family=="Heterodontidae")[17], 
                    subset(record,record$Family=="Carcharhinidae")[17] , subset(record,record$Family=="Sphyrnidae")[17]))


'%!in%' <- function(x,y)!('%in%'(x,y))
occ<-occ[,colnames(occ)%!in%Shark[,1]]

#Enl??ve les "sp."
occ<-occ[,-grep("sp.", colnames(occ), ignore.case = TRUE)]

# Prepare data----

#Tableau d'information transect Transect + LatLong+ Unique Site +Protection+habitat
Infotrans<-data.frame(record$UniqueTransect,record$UniqueSite)
Infotrans<-Infotrans[!duplicated(Infotrans),]
Infotrans<-Infotrans[Infotrans$record.UniqueTransect%in%rownames(table1),]
Infotrans<- merge(Infotrans,df.geo,by.x="record.UniqueSite",by.y="row.names",all=F)               
Infotrans<- merge(Infotrans,df.protec,by.x="record.UniqueSite",by.y="row.names",all=F)      
Infotrans<- merge(Infotrans,df.hab,by.x="record.UniqueSite",by.y="row.names",all=F)      


#Cluster pour regrouper les sites entre eux par ??chelle spatial
df.geo.Transect <- data.frame(name = Infotrans$record.UniqueTransect,
                              lat  = Infotrans$lat,
                              lon  =Infotrans$lon)


source(file.path("~/Documents/Fonction R utile/FromGPStoDISTANCE.R"))
df_geo_fish<-GeoDistanceInMetresMatrix(df.geo.Transect)
geodist.Transect <- df_geo_fish/1000
save(geodist.Transect, file= "geodist.Transect.RData")

#load("geodist.Transect.RData")


# IUCN_CAT EXTRACTION ----

setwd("~/Documents/Postdoc MARBEC/BETA PROTECTED AREA/WDPA_Feb2018_marine-shapefile")
#Send MAP
mapPA<-readOGR("WDPA_Feb2018_marine-shapefile-polygons.shp")

pointtoplot <- data.frame(SITE=df.geo.Transect$name,x=df.geo.Transect$lon , y=df.geo.Transect$lat)
pointtoplot <- unique(pointtoplot)
rownames(pointtoplot)<-pointtoplot[,1]
pointtoplot<-pointtoplot[,-1]

coordinates(pointtoplot) <- ~ x + y 
proj4string(pointtoplot) <- CRS("+proj=longlat +ellps=WGS84")
mapPA<-spTransform(mapPA,CRS("+proj=longlat +ellps=WGS84"))
Info_Reserve<-over(pointtoplot, mapPA)


df.geo.Transect2<-data.frame(df.geo.Transect,Info_Reserve$NAME)
names(df.geo.Transect2)[4]<-"namesMPA"
rownames(df.geo.Transect2)<-df.geo.Transect2[,1]

df.geo.Transect2<-df.geo.Transect2[rownames(df.geo.Transect2)%in%rownames(occ),]
setwd("~/Documents/Postdoc MARBEC/BETA PROTECTED AREA")
########################################################################
#RECUPER LE NOM DES RESERVES
#V??rifier que toutes les r??serves ont un nom -----

InfoTOTAL<- data.frame(df.geo.Transect2,Infotrans$protec,Infotrans$hab)
rownames(InfoTOTAL)<-InfoTOTAL[,1]
InfoTOTAL<-InfoTOTAL[,-1]

InfoTOTAL$Infotrans.protec <- as.character(InfoTOTAL$Infotrans.protec)
InfoTOTAL$Infotrans.protec[InfoTOTAL$Infotrans.protec == "UnfishedLow"] <- "Unfished"
InfoTOTAL$Infotrans.protec[InfoTOTAL$Infotrans.protec == "UnfishedHigh"] <- "Unfished"

InfoTOTAL<-InfoTOTAL[rownames(InfoTOTAL)%in%rownames(occ),]
listMPA_close<-rbind(subset(InfoTOTAL,InfoTOTAL$Infotrans.protec=="Unfished"))
listMPA_close<-na.omit(unique(listMPA_close$namesMPA))
#Enl??ve celle qui n'a pas de nom


listMPA_restricted<-subset(InfoTOTAL,InfoTOTAL$Infotrans.protec=="Restricted")
listMPA_restricted2<-unique(rownames(listMPA_restricted))
listMPA_restricted<-na.omit(unique(listMPA_restricted$namesMPA))
#Enl??ve celle qui n'a pas de nom
listMPA_restricted<-listMPA_restricted[-7]


listMPA_nonprotect<-subset(InfoTOTAL,InfoTOTAL$Infotrans.protec=="Fished")
listMPA_nonprotect<-unique(rownames(listMPA_nonprotect))


#Eliminer les r??serves ou il y a moins de 2 suivi de part et d'autres ----
# Prendre les transects de cette r??serve & qui sont unfished
NBsurvey_IN<-NULL 
NBsurvey_OUT<-NULL 
NBsurvey_RES<-NULL

for (i in 1:length(listMPA_close)){
  IDReserveClose<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_close[i])
  #Transect Closed
  Transect_Closed<-occ[rownames(occ)%in%rownames(IDReserveClose),]
  NBsurvey_IN<-c(NBsurvey_IN,dim(Transect_Closed)[1])
  
  IDTransect50km<-NULL 
  for (j in 1:dim(Transect_Closed)[1]){
    if(dim(Transect_Closed)[1]==1){#strucure of the dataframe is different if dim(Transect_Closed)[1]==1.
      distGeo<-data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_Closed),])
      rownames(distGeo)<-colnames(geodist.Transect)}
    else{
      distGeo<-t(data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_Closed),]))
      rownames(distGeo)<-colnames(geodist.Transect)
    }
    
    distGeo2<-subset(distGeo,distGeo[,j]<50) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>=0)  
    distGeo2<-unique(rownames(distGeo2))
    IDTransect50km<-c(IDTransect50km,distGeo2)
  }
  IDTransect50km<-unique(IDTransect50km)
  
  #Transect non protected
  IDTransect50km_NP<- IDTransect50km[IDTransect50km%in%listMPA_nonprotect]
  Transect50km_NP<-occ[rownames(occ)%in%IDTransect50km_NP,]
  NBsurvey_OUT<-c(NBsurvey_OUT,dim(Transect50km_NP)[1]) 
  
  #Transect restricted
  IDTransect50km_restricted<- IDTransect50km[IDTransect50km%in%listMPA_restricted2]
  Transect50km_restricted<-occ[rownames(occ)%in%IDTransect50km_restricted,]
  NBsurvey_RES<-c(NBsurvey_RES,dim(Transect50km_restricted)[1]) 
}

MPAselect<-data.frame(listMPA_close,NBsurvey_IN,NBsurvey_OUT,NBsurvey_RES)




####MEME CHOSE POUR LES RESTRICTED  ----                          

NBsurvey_OUT<-NULL 
NBsurvey_RES<-NULL
for (i in 1:length(listMPA_restricted)){
  IDReserverestricted<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_restricted[i])
  #Transect restricted
  Transect_restricted<-occ[rownames(occ)%in%rownames(IDReserverestricted),]
  NBsurvey_RES<-c(NBsurvey_RES,dim(Transect_restricted)[1])
  
  IDTransect50km<-NULL 
  for (j in 1:dim(Transect_restricted)[1]){
    if(dim(Transect_restricted)[1]==1){#strucure of the dataframe is different if dim(Transect_restricted)[1]==1.
      distGeo<-data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_restricted),])
      rownames(distGeo)<-colnames(geodist.Transect)}
    else{
      distGeo<-t(data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_restricted),]))
      rownames(distGeo)<-colnames(geodist.Transect)
    }
    
    distGeo2<-subset(distGeo,distGeo[,j]<50) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDTransect50km<-c(IDTransect50km,distGeo2)
  }
  IDTransect50km<-unique(IDTransect50km)
  
  #Transect non protected
  IDTransect50km_NP<- IDTransect50km[IDTransect50km%in%listMPA_nonprotect]
  Transect50km_NP<-occ[rownames(occ)%in%IDTransect50km_NP,]
  NBsurvey_OUT<-c(NBsurvey_OUT,dim(Transect50km_NP)[1]) 
}

RESselect<-data.frame(listMPA_restricted,NBsurvey_RES,NBsurvey_OUT)


#50km
listMPA_close_compaNP <- MPAselect[MPAselect$NBsurvey_IN>2 &   MPAselect$NBsurvey_OUT>2,]$listMPA_close
listMPA_close_comparestricted<-MPAselect[MPAselect$NBsurvey_IN>2 &   MPAselect$NBsurvey_RES>2,]$listMPA_close
listMPA_restricted_NP<-RESselect[RESselect$NBsurvey_RES>2 & RESselect$NBsurvey_OUT>2,]$listMPA_restricted


yala<-InfoTOTAL[InfoTOTAL$namesMPA %in% c(as.character(listMPA_close_compaNP),as.character(listMPA_close_comparestricted),as.character(listMPA_restricted_NP)),]

yolo <- occ[rownames(occ)%in%rownames(yala),]
yolo <- yolo[,apply(yolo,2,sum)>0]
uniqueSP_SERF <- colnames(yolo)
save(uniqueSP_SERF,file="uniqueSP_SERF.RData")

occSERF<- yolo
save(occSERF,file="occSERF.RData")

surveySERF <- yala
save(surveySERF,file="surveySERF.RData")
##########################################################################
#reps model null
reps=200
core=2
setwd("~/Documents/Postdoc MARBEC/BETA PROTECTED AREA_CLEAN/Fish/Reducedset/SERF")
# NO TAKE VS OUTSIDE ----

total_notake_outside_50km_SERF<-matrix(NA,length(listMPA_close_compaNP),200)
turnover_notake_outside_50km_SERF<-total_notake_outside_50km_SERF
nested_notake_outside_50km_SERF<-total_notake_outside_50km_SERF

Fish_S_50km_Unique_Close.Close_NP_SERF<-matrix(NA,length(listMPA_close_compaNP),200)
Fish_S_50km_Unique_NP.Close_NP_SERF<-Fish_S_50km_Unique_Close.Close_NP_SERF
Fish_S_50km_Shared_Close_NP_SERF<-Fish_S_50km_Unique_Close.Close_NP_SERF


ses.tot_50km_SERF_notake_outside <- total_notake_outside_50km_SERF
ses.tur_50km_SERF_notake_outside <- total_notake_outside_50km_SERF
ses.nes_50km_SERF_notake_outside<- total_notake_outside_50km_SERF

# p-value =quantile.obs/(total.interation+1)
p.val.tot_50km_SERF_notake_outside<-total_notake_outside_50km_SERF
p.val.tur_50km_SERF_notake_outside<- total_notake_outside_50km_SERF
p.val.nes_50km_SERF_notake_outside<-total_notake_outside_50km_SERF

for (i in 1: length(listMPA_close_compaNP)) {# Pour chaque r??serve
  
  # Prendre les transects de cette r??serve & qui sont unfished
  IDReserveClose<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_close_compaNP[i])
  IDReserveClose<-subset(IDReserveClose,IDReserveClose$Infotrans.protec=="Unfished")
  #Transect Closed
  Transect_Closed<-occ[rownames(occ)%in%rownames(IDReserveClose),]
  
  #Transect un protected
  IDTransect50km<-NULL 
  for (j in 1:dim(Transect_Closed)[1]){
    
    if(dim(Transect_Closed)[1]==1){#strucure of the dataframe is different if dim(Transect_Closed)[1]==1.
      distGeo<-data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_Closed),])
      rownames(distGeo)<-colnames(geodist.Transect)}
    
    else{
      distGeo<-t(data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_Closed),]))
      rownames(distGeo)<-colnames(geodist.Transect)
    }
    
    
    distGeo2<-subset(distGeo,distGeo[,j]<50) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDTransect50km<-c(IDTransect50km,distGeo2)
  }
  IDTransect50km<-unique(IDTransect50km)
  
  
  
  #Transect non protected
  IDTransect50km_NP<- IDTransect50km[IDTransect50km%in%listMPA_nonprotect]
  Transect50km_NP<-occ[rownames(occ)%in%IDTransect50km_NP,]
  
  if(dim(Transect50km_NP)[1]==0){next}
  
  #Calcul de la beta, du nombre d'esp partag?? et unique 
  for (k in 1:reps){#bootstrap car pas le mm nombre de site
    
    if (dim(Transect_Closed)[1]>dim(Transect50km_NP)[1]){
      
      Group_CLOSED_Sample<-as.matrix(Transect_Closed[sample(1:nrow(Transect_Closed),dim(Transect50km_NP)[1],replace=F),])
      Group_CLOSED2<-apply(Group_CLOSED_Sample,2,sum)
      Group_NP2<-apply(Transect50km_NP,2,sum)
      
      Compa_NULL<-rbind(Group_CLOSED_Sample,Transect50km_NP)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Group_CLOSED_Sample)[1],],2,sum),apply(random_mat[,,e][(dim(Transect50km_NP)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
      
    }
    
    else {
      
      Group_NP_Sample<-as.matrix(Transect50km_NP[sample(1:nrow(Transect50km_NP),dim(Transect_Closed)[1],replace=F),])
      Group_CLOSED2<-apply(Transect_Closed, 2,sum)
      Group_NP2<-apply(Group_NP_Sample, 2,sum)
      
      Compa_NULL<-rbind(Transect_Closed,Group_NP_Sample)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Transect_Closed)[1],],2,sum),apply(random_mat[,,e][(dim(Group_NP_Sample)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    nullbeta <- t( do.call(rbind.data.frame, beta_null))
    
    Compa<-rbind(Group_CLOSED2,Group_NP2)
    Compa[Compa>0]<-1
    BETAX<-beta.pair(Compa, index.family="jaccard")
    
    total_notake_outside_50km_SERF[i,k]<-BETAX$beta.jac
    turnover_notake_outside_50km_SERF[i,k]<-BETAX$beta.jtu
    nested_notake_outside_50km_SERF[i,k]<-BETAX$beta.jne


    if(length(dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2])==0){Fish_S_50km_Unique_Close.Close_NP_SERF[i,k]<-0
    }else{Fish_S_50km_Unique_Close.Close_NP_SERF[i,k]<-dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2]} 
    
    if(length(dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2])==0){Fish_S_50km_Unique_NP.Close_NP_SERF[i,k]<-0
    }else{ Fish_S_50km_Unique_NP.Close_NP_SERF[i,k]<-dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2]} 
    
    if(length(dim(Compa[,apply(Compa,2,sum)==2])[2])==0){Fish_S_50km_Shared_Close_NP_SERF[i,k]<-0
    }else{ Fish_S_50km_Shared_Close_NP_SERF[i,k]<-dim(Compa[,apply(Compa,2,sum)==2])[2]} 
    

    # SES=(obs-mean(rand))/sd(rand)
    ses.tot_50km_SERF_notake_outside[i,k] = (BETAX$beta.jac - mean(nullbeta[3,]))/sd(nullbeta[3,])
    ses.tur_50km_SERF_notake_outside[i,k] = (BETAX$beta.jtu - mean(nullbeta[1,]))/sd(nullbeta[1,])
    ses.nes_50km_SERF_notake_outside[i,k] = (BETAX$beta.jne - mean(nullbeta[2,]))/sd(nullbeta[2,])
    
    # p-value = proportion of null value inferior to obs Beta (+1)
    p.val.tot_50km_SERF_notake_outside[i,k] =   length(which(nullbeta[3,]<(BETAX$beta.jac))) / (length(nullbeta[3,])+1)
    p.val.tur_50km_SERF_notake_outside[i,k] =   length(which(nullbeta[1,]<(BETAX$beta.jtu))) / (length(nullbeta[1,])+1)
    p.val.nes_50km_SERF_notake_outside[i,k] =   length(which(nullbeta[2,]<(BETAX$beta.jne))) / (length(nullbeta[2,])+1)
    
    save(total_notake_outside_50km_SERF,file="total_notake_outside_50km_SERF.RData")
    save(turnover_notake_outside_50km_SERF,file="turnover_notake_outside_50km_SERF.RData")
    save(nested_notake_outside_50km_SERF,file="nested_notake_outside_50km_SERF.RData")
    save(Fish_S_50km_Unique_Close.Close_NP_SERF,file="Fish_S_50km_Unique_Close.Close_NP_SERF.RData")
    save(Fish_S_50km_Unique_NP.Close_NP_SERF,file="Fish_S_50km_Unique_NP.Close_NP_SERF.RData")
    save(Fish_S_50km_Shared_Close_NP_SERF,file="Fish_S_50km_Shared_Close_NP_SERF.RData")
    
    save(ses.tot_50km_SERF_notake_outside,file="ses.tot_50km_SERF_notake_outside.RData")
    save(ses.tur_50km_SERF_notake_outside,file="ses.tur_50km_SERF_notake_outside.RData")
    save(ses.nes_50km_SERF_notake_outside,file="ses.nes_50km_SERF_notake_outside.RData")
    
    save(p.val.tot_50km_SERF_notake_outside,file="p.val.tot_50km_SERF_notake_outside.RData")
    save(p.val.tur_50km_SERF_notake_outside,file="p.val.tur_50km_SERF_notake_outside.RData")
    save(p.val.nes_50km_SERF_notake_outside,file="p.val.nes_50km_SERF_notake_outside.RData")
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}



# NO TAKE VS RESTRICTED ----
total_notake_restricted_50km_SERF<-matrix(NA,length(listMPA_close_comparestricted),reps)
turnover_notake_restricted_50km_SERF<-total_notake_restricted_50km_SERF
nested_notake_restricted_50km_SERF<-total_notake_restricted_50km_SERF

Fish_S_50km_Unique_Close.Close_Rest_SERF<-matrix(NA,length(listMPA_close_comparestricted),reps)
Fish_S_50km_Unique_RESTRICT.Close_Rest_SERF<-Fish_S_50km_Unique_Close.Close_Rest_SERF
Fish_S_50km_Shared_Close_RESTRICT.Close_Rest_SERF<-Fish_S_50km_Unique_Close.Close_Rest_SERF


ses.tot_50km_SERF_notake_restricted <- total_notake_restricted_50km_SERF
ses.tur_50km_SERF_notake_restricted <- total_notake_restricted_50km_SERF
ses.nes_50km_SERF_notake_restricted<- total_notake_restricted_50km_SERF

#p-value =quantile.obs/(total.interation+1)
p.val.tot_50km_SERF_notake_restricted<-total_notake_restricted_50km_SERF
p.val.tur_50km_SERF_notake_restricted<- total_notake_restricted_50km_SERF
p.val.nes_50km_SERF_notake_restricted<-total_notake_restricted_50km_SERF

reps=200
for (i in 1: length(listMPA_close_comparestricted)) {# Pour chaque r??serve
  
  # Prendre les transects de cette r??serve & qui sont unfished
  IDReserveClose<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_close_comparestricted[i])
  IDReserveClose<-subset(IDReserveClose,IDReserveClose$Infotrans.protec=="Unfished")
  #Transect Closed
  Transect_Closed<-occ[rownames(occ)%in%rownames(IDReserveClose),]
  
  #Transect restreicted
  IDTransect50km<-NULL 
  for (j in 1:dim(Transect_Closed)[1]){
    
    if(dim(Transect_Closed)[1]==1){#strucure of the dataframe is different if dim(Transect_Closed)[1]==1.
      distGeo<-data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_Closed),])
      rownames(distGeo)<-colnames(geodist.Transect)}
    
    else{
      distGeo<-t(data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_Closed),]))
      rownames(distGeo)<-colnames(geodist.Transect)
    }
    
    
    distGeo2<-subset(distGeo,distGeo[,j]<50) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDTransect50km<-c(IDTransect50km,distGeo2)
  }
  IDTransect50km<-unique(IDTransect50km)
  
  #Transect non protected
  IDTransect50km_RESTRICT<- IDTransect50km[IDTransect50km%in%listMPA_restricted2]
  Transect50km_RESTRICT<-occ[rownames(occ)%in%IDTransect50km_RESTRICT,]
  
  if(dim(Transect50km_RESTRICT)[1]==0){next}
  
  
  #Calcul de la beta, du nombre d'esp partag?? et unique 
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (dim(Transect_Closed)[1]>dim(Transect50km_RESTRICT)[1]){
      
      Group_CLOSED_Sample<-as.matrix(Transect_Closed[sample(1:nrow(Transect_Closed),dim(Transect50km_RESTRICT)[1],replace=F),])
      Group_CLOSED2<-apply(Group_CLOSED_Sample,2,sum)
      Group_RESTRICT2<-apply(Transect50km_RESTRICT,2,sum)
      
      Compa_NULL<-rbind(Group_CLOSED_Sample,Transect50km_RESTRICT)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Group_CLOSED_Sample)[1],],2,sum),apply(random_mat[,,e][(dim(Transect50km_RESTRICT)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
      
    }
    
    else {
      
      Group_RESTRICT_Sample<-as.matrix(Transect50km_RESTRICT[sample(1:nrow(Transect50km_RESTRICT),dim(Transect_Closed)[1],replace=F),])
      Group_CLOSED2<-apply(Transect_Closed, 2,sum)
      Group_RESTRICT2<-apply(Group_RESTRICT_Sample, 2,sum)
      
      Compa_NULL<-rbind(Transect_Closed,Group_RESTRICT_Sample)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Transect_Closed)[1],],2,sum),apply(random_mat[,,e][(dim(Group_RESTRICT_Sample)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
      
    }
    nullbeta <- t( do.call(rbind.data.frame, beta_null))
    
    Compa<-rbind(Group_CLOSED2,Group_RESTRICT2)
    Compa[Compa>0]<-1
    BETAX<-beta.pair(Compa, index.family="jaccard")
    
    total_notake_restricted_50km_SERF[i,k]<-BETAX$beta.jac
    turnover_notake_restricted_50km_SERF[i,k]<-BETAX$beta.jtu
    nested_notake_restricted_50km_SERF[i,k]<-BETAX$beta.jne
    

    if(length(dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2])==0){Fish_S_50km_Unique_Close.Close_Rest_SERF[i,k]<-0
    }else{Fish_S_50km_Unique_Close.Close_Rest_SERF[i,k]<-dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2]} 
    
    if(length(dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2])==0){Fish_S_50km_Unique_RESTRICT.Close_Rest_SERF[i,k]<-0
    }else{ Fish_S_50km_Unique_RESTRICT.Close_Rest_SERF[i,k]<-dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2]} 
    
    if(length(dim(Compa[,apply(Compa,2,sum)==2])[2])==0){Fish_S_50km_Shared_Close_RESTRICT.Close_Rest_SERF[i,k]<-0
    }else{ Fish_S_50km_Shared_Close_RESTRICT.Close_Rest_SERF[i,k]<-dim(Compa[,apply(Compa,2,sum)==2])[2]} 
    
    # SES=(obs-mean(rand))/sd(rand)
    ses.tot_50km_SERF_notake_restricted[i,k] = (BETAX$beta.jac - mean(nullbeta[3,]))/sd(nullbeta[3,])
    ses.tur_50km_SERF_notake_restricted[i,k] = (BETAX$beta.jtu - mean(nullbeta[1,]))/sd(nullbeta[1,])
    ses.nes_50km_SERF_notake_restricted[i,k] = (BETAX$beta.jne - mean(nullbeta[2,]))/sd(nullbeta[2,])
    
    # p-value = proportion of null value inferior to obs Beta (+1)
    p.val.tot_50km_SERF_notake_restricted[i,k] =   length(which(nullbeta[3,]<(BETAX$beta.jac))) / (length(nullbeta[3,])+1)
    p.val.tur_50km_SERF_notake_restricted[i,k] =   length(which(nullbeta[1,]<(BETAX$beta.jtu))) / (length(nullbeta[1,])+1)
    p.val.nes_50km_SERF_notake_restricted[i,k] =   length(which(nullbeta[2,]<(BETAX$beta.jne))) / (length(nullbeta[2,])+1)
    
    print(paste ("k",k))
    
    save(total_notake_restricted_50km_SERF,file="total_notake_restricted_50km_SERF.RData")
    save(turnover_notake_restricted_50km_SERF,file="turnover_notake_restricted_50km_SERF.RData")
    save(nested_notake_restricted_50km_SERF,file="nested_notake_restricted_50km_SERF.RData")
    save(Fish_S_50km_Unique_Close.Close_Rest_SERF,file="Fish_S_50km_Unique_Close.Close_Rest_SERF.RData")
    save(Fish_S_50km_Unique_RESTRICT.Close_Rest_SERF,file="Fish_S_50km_Unique_RESTRICT.Close_Rest_SERF.RData")
    save(Fish_S_50km_Shared_Close_RESTRICT.Close_Rest_SERF,file="Fish_S_50km_Shared_Close_RESTRICT.Close_Rest_SERF.RData")
    
    save(ses.tot_50km_SERF_notake_restricted,file="ses.tot_50km_SERF_notake_restricted.RData")
    save(ses.tur_50km_SERF_notake_restricted,file="ses.tur_50km_SERF_notake_restricted.RData")
    save(ses.nes_50km_SERF_notake_restricted,file="ses.nes_50km_SERF_notake_restricted.RData")
    
    save(p.val.tot_50km_SERF_notake_restricted,file="p.val.tot_50km_SERF_notake_restricted.RData")
    save(p.val.tur_50km_SERF_notake_restricted,file="p.val.tur_50km_SERF_notake_restricted.RData")
    save(p.val.nes_50km_SERF_notake_restricted,file="p.val.nes_50km_SERF_notake_restricted.RData")
  }
  print(i)
}

##
# RESTRICTED VS OUTSIDE  ----
total_restricted_outside_50km_SERF<-matrix(NA,length(listMPA_restricted_NP),reps)
turnover_restricted_outside_50km_SERF<-total_restricted_outside_50km_SERF
nested_restricted_outside_50km_SERF<-total_restricted_outside_50km_SERF

Fish_S_50km_Unique_restricte.Rest_NP_SERF<-matrix(NA,length(listMPA_restricted_NP),reps)
Fish_S_50km_Unique_NP.Rest_NP_SERF<-Fish_S_50km_Unique_restricte.Rest_NP_SERF
Fish_S_50km_Shared_restricte_NP.Rest_NP_SERF<-Fish_S_50km_Unique_restricte.Rest_NP_SERF

# SES=(obs-mean(rand))/sd(rand)
ses.tot_50km_SERF_restricted_outside<-total_restricted_outside_50km_SERF
ses.tur_50km_SERF_restricted_outside<-total_restricted_outside_50km_SERF
ses.nes_50km_SERF_restricted_outside<-total_restricted_outside_50km_SERF

# p-value = proportion of null value inferior to obs Beta (+1)
p.val.tot_50km_SERF_restricted_outside<-total_restricted_outside_50km_SERF
p.val.tur_50km_SERF_restricted_outside<-total_restricted_outside_50km_SERF
p.val.nes_50km_SERF_restricted_outside<-total_restricted_outside_50km_SERF

for (i in 1: length(listMPA_restricted_NP)) {# Pour chaque r??serve
  
  # Prendre les transects de cette r??serve & qui sont unfished
  IDReserverestricte<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_restricted_NP[i])
  IDReserverestricte<-subset(IDReserverestricte,IDReserverestricte$Infotrans.protec=="Restricted")
  
  
  #Transect restricted
  Transect_restricted<-occ[rownames(occ)%in%rownames(IDReserverestricte),]
  
  #Transect un protected
  IDTransect50km<-NULL 
  for (j in 1:dim(Transect_restricted)[1]){
    
    if(dim(Transect_restricted)[1]==1){#strucure of the dataframe is different if dim(Transect_restricted)[1]==1.
      distGeo<-data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_restricted),])
      rownames(distGeo)<-colnames(geodist.Transect)}
    
    else{
      distGeo<-t(data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_restricted),]))
      rownames(distGeo)<-colnames(geodist.Transect)
    }
    
    
    distGeo2<-subset(distGeo,distGeo[,j]<50) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDTransect50km<-c(IDTransect50km,distGeo2)
  }
  IDTransect50km<-unique(IDTransect50km)
  
  
  
  #Transect non protected
  IDTransect50km_NP<- IDTransect50km[IDTransect50km%in%listMPA_nonprotect]
  Transect50km_NP<-occ[rownames(occ)%in%IDTransect50km_NP,]
  
  
  if(dim(Transect50km_NP)[1]==0){next}
  
  #Calcul de la beta, du nombre d'esp partag?? et unique 
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (dim(Transect_restricted)[1]>dim(Transect50km_NP)[1]){
      
      Group_restricted_Sample<-as.matrix(Transect_restricted[sample(1:nrow(Transect_restricted),dim(Transect50km_NP)[1],replace=F),])
      Group_restricted2<-apply(Group_restricted_Sample,2,sum)
      Group_NP2<-apply(Transect50km_NP,2,sum)
      
      Compa_NULL<-rbind(Group_restricted_Sample,Transect50km_NP)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Group_restricted_Sample)[1],],2,sum),apply(random_mat[,,e][(dim(Transect50km_NP)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
      
    }
    
    else {
      
      Group_NP_Sample<-as.matrix(Transect50km_NP[sample(1:nrow(Transect50km_NP),dim(Transect_restricted)[1],replace=F),])
      Group_restricted2<-apply(Transect_restricted, 2,sum)
      Group_NP2<-apply(Group_NP_Sample, 2,sum)
      
      Compa_NULL<-rbind(Transect_restricted,Group_NP_Sample)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Transect_restricted)[1],],2,sum),apply(random_mat[,,e][(dim(Group_NP_Sample)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    nullbeta <- t( do.call(rbind.data.frame, beta_null))
    
    
    Compa<-rbind(Group_restricted2,Group_NP2)
    Compa[Compa>0]<-1
    BETAX<-beta.pair(Compa, index.family="jaccard")
    
    total_restricted_outside_50km_SERF[i,k]<-BETAX$beta.jac
    turnover_restricted_outside_50km_SERF[i,k]<-BETAX$beta.jtu
    nested_restricted_outside_50km_SERF[i,k]<-BETAX$beta.jne
    
    if(length(dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2])==0){Fish_S_50km_Unique_restricte.Rest_NP_SERF[i,k]<-0
    }else{Fish_S_50km_Unique_restricte.Rest_NP_SERF[i,k]<-dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2]} 
    
    if(length(dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2])==0){Fish_S_50km_Unique_NP.Rest_NP_SERF[i,k]<-0
    }else{ Fish_S_50km_Unique_NP.Rest_NP_SERF[i,k]<-dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2]} 
    
    if(length(dim(Compa[,apply(Compa,2,sum)==2])[2])==0){Fish_S_50km_Shared_restricte_NP.Rest_NP_SERF[i,k]<-0
    }else{ Fish_S_50km_Shared_restricte_NP.Rest_NP_SERF[i,k]<-dim(Compa[,apply(Compa,2,sum)==2])[2]} 

    
    
    # SES=(obs-mean(rand))/sd(rand)
    ses.tot_50km_SERF_restricted_outside[i,k] = (BETAX$beta.jac - mean(nullbeta[3,]))/sd(nullbeta[3,])
    ses.tur_50km_SERF_restricted_outside[i,k] = (BETAX$beta.jtu - mean(nullbeta[1,]))/sd(nullbeta[1,])
    ses.nes_50km_SERF_restricted_outside[i,k] = (BETAX$beta.jne - mean(nullbeta[2,]))/sd(nullbeta[2,])
    
    # p-value = proportion of null value inferior to obs Beta (+1)
    p.val.tot_50km_SERF_restricted_outside[i,k] =   length(which(nullbeta[3,]<(BETAX$beta.jac))) / (length(nullbeta[3,])+1)
    p.val.tur_50km_SERF_restricted_outside[i,k] =   length(which(nullbeta[1,]<(BETAX$beta.jtu))) / (length(nullbeta[1,])+1)
    p.val.nes_50km_SERF_restricted_outside[i,k] =   length(which(nullbeta[2,]<(BETAX$beta.jne))) / (length(nullbeta[2,])+1)
    
    print(paste ("k",k))
    
    
    save(total_restricted_outside_50km_SERF,file="total_restricted_outside_50km_SERF.RData")
    save(turnover_restricted_outside_50km_SERF,file="turnover_restricted_outside_50km_SERF.RData")
    save(nested_restricted_outside_50km_SERF,file="nested_restricted_outside_50km_SERF.RData")
    
    save(Fish_S_50km_Unique_restricte.Rest_NP_SERF,file="Fish_S_50km_Unique_restricte.Rest_NP_SERF.RData")
    save(Fish_S_50km_Unique_NP.Rest_NP_SERF,file="Fish_S_50km_Unique_NP.Rest_NP_SERF.RData")
    save(Fish_S_50km_Shared_restricte_NP.Rest_NP_SERF,file="Fish_S_50km_Shared_restricte_NP.Rest_NP_SERF.RData")
    
    save(ses.tot_50km_SERF_restricted_outside,file="ses.tot_50km_SERF_restricted_outside.RData")
    save(ses.tur_50km_SERF_restricted_outside,file="ses.tur_50km_SERF_restricted_outside.RData")
    save(ses.nes_50km_SERF_restricted_outside,file="ses.nes_50km_SERF_restricted_outside.RData")
    
    save(p.val.tot_50km_SERF_restricted_outside,file="p.val.tot_50km_SERF_restricted_outside.RData")
    save(p.val.tur_50km_SERF_restricted_outside,file="p.val.tur_50km_SERF_restricted_outside.RData")
    save(p.val.nes_50km_SERF_restricted_outside,file="p.val.nes_50km_SERF_restricted_outside.RData")
  }
  print(i)
}
