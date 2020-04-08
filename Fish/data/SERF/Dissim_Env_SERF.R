setwd("~/Documents/Postdoc MARBEC/BETA PROTECTED AREA_CLEAN/Fish/Reducedset")
load("~/Documents/Postdoc MARBEC/BETA PROTECTED AREA_CLEAN/Fish/Reducedset/SERF/SERF_chla_sst.RData")

survey <- unique(data.frame(UniqueTransect=record$UniqueTransect, UniqueSite=record$UniqueSite))
Hab_all <- merge(SERF_chla_sst,survey,by.x="name",by.y="UniqueSite",all.x=T)

env_survey <- unique(data.frame(UniqueSite=env$UniqueSite,coralcover=env$GoodHardCoralCover,CleanHabitat=env$CleanHabitat,TotalAlgalCover=env$TotalAlgalCover,Depth=env$Depth))
env_survey[env_survey==-999]<-NA
Hab_all <- merge(Hab_all,env_survey,by.x="name",by.y="UniqueSite",all.x=TRUE)


Hab_all<- Hab_all[Hab_all$UniqueTransect %in% rownames(occ),]
rownames(Hab_all)<-Hab_all$UniqueTransect
Hab_all <- Hab_all[,-c(1,2,3,6)]

Hab_all$NumHab<- NA
drop.levels(Hab_all$CleanHabitat)
for (i in 1:nrow(Hab_all)){
  if(Hab_all$CleanHabitat[i]=="Flat") Hab_all$NumHab[i]<-1
  if(Hab_all$CleanHabitat[i]=="Lagoon_Back reef") Hab_all$NumHab[i]<-2
  if(Hab_all$CleanHabitat[i]=="Crest") Hab_all$NumHab[i]<-3
  if(Hab_all$CleanHabitat[i]=="Slope") Hab_all$NumHab[i]<-4
}

Hab_all <- Hab_all[,-4]
Hab_all$coralcover <- as.numeric(as.character(Hab_all$coralcover))
Hab_all$TotalAlgalCover <- as.numeric(as.character(Hab_all$TotalAlgalCover))
Hab_all$Depth <- as.numeric(as.character(Hab_all$Depth))


Dissim_habSPA_NPA_TOT_50km_SERF <- matrix(NA,length(listMPA_close_compaNP),200)


for (i in 1: length(listMPA_close_compaNP)) {# Pour chaque r??serve
  
  # Prendre les transects de cette r??serve & qui sont unfished
  IDReserveClose<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_close_compaNP[i])
  IDReserveClose<-subset(IDReserveClose,IDReserveClose$Infotrans.protec=="Unfished")
  #Transect Closed
  Transect_Closed<-Hab_all[rownames(Hab_all)%in%rownames(IDReserveClose),]
  
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
  Transect50km_NP<-Hab_all[rownames(Hab_all)%in%IDTransect50km_NP,]
  
  if(dim(Transect50km_NP)[1]==0){next}
  
  #Calcul de la beta, du nombre d'esp partag?? et unique 
  for (k in 1:reps){#bootstrap car pas le mm nombre de site
    
    if (dim(Transect_Closed)[1]>dim(Transect50km_NP)[1]){
      
      Group_CLOSED_Sample<-as.matrix(Transect_Closed[sample(1:nrow(Transect_Closed),dim(Transect50km_NP)[1],replace=F),])
      Group_CLOSED2<-apply(Group_CLOSED_Sample,2,mean,na.rm=TRUE)
      Group_NP2<-apply(Transect50km_NP,2,mean,na.rm=TRUE)
      
    }
    
    else {
      
      Group_NP_Sample<-as.matrix(Transect50km_NP[sample(1:nrow(Transect50km_NP),dim(Transect_Closed)[1],replace=F),])
      Group_CLOSED2<-apply(Group_CLOSED_Sample,2,mean,na.rm=TRUE)
      Group_NP2<-apply(Transect50km_NP,2,mean,na.rm=TRUE)
      
    }
    
    
    Compa<-rbind(Group_CLOSED2,Group_NP2)
   
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habSPA_NPA_TOT_50km_SERF[i,k] <- Dissim_hab[1]
    
    save(Dissim_habSPA_NPA_TOT_50km_SERF,file="Dissim_habSPA_NPA_TOT_50km_SERF.RData")

    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}




#############################################################################################################################
Dissim_habSPA_RA_TOT_50km_SERF <- matrix(NA,length(listMPA_close_comparestricted),200)

for (i in 1: length(listMPA_close_comparestricted)) {# Pour chaque r??serve
  
  # Prendre les transects de cette r??serve & qui sont unfished
  IDReserveClose<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_close_comparestricted[i])
  IDReserveClose<-subset(IDReserveClose,IDReserveClose$Infotrans.protec=="Unfished")
  #Transect Closed
  Transect_Closed<-Hab_all[rownames(Hab_all)%in%rownames(IDReserveClose),]
  
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
  Transect50km_RESTRICT<-Hab_all[rownames(Hab_all)%in%IDTransect50km_RESTRICT,]
  
  if(dim(Transect50km_RESTRICT)[1]==0){next}
  
  
  #Calcul de la beta, du nombre d'esp partag?? et unique 
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (dim(Transect_Closed)[1]>dim(Transect50km_RESTRICT)[1]){
      
      Group_CLOSED_Sample<-as.matrix(Transect_Closed[sample(1:nrow(Transect_Closed),dim(Transect50km_RESTRICT)[1],replace=F),])
      Group_CLOSED2<-apply(Group_CLOSED_Sample,2,mean,na.rm=TRUE)
      Group_RESTRICT2<-apply(Transect50km_RESTRICT,2,mean,na.rm=TRUE)
      
    }
    
    else {
      
      Group_RESTRICT_Sample<-as.matrix(Transect50km_RESTRICT[sample(1:nrow(Transect50km_RESTRICT),dim(Transect_Closed)[1],replace=F),])
      Group_CLOSED2<-apply(Transect_Closed, 2,mean,na.rm=TRUE)
      Group_RESTRICT2<-apply(Group_RESTRICT_Sample, 2,mean,na.rm=TRUE)
      
    }
    
    Compa<-rbind(Group_CLOSED2,Group_RESTRICT2)
    
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habSPA_RA_TOT_50km_SERF[i,k] <- Dissim_hab[1]
    
    save(Dissim_habSPA_RA_TOT_50km_SERF,file="Dissim_habSPA_RA_TOT_50km_SERF.RData")
    
  }
  print(i)
}








#############################################################################################################################

Dissim_habRA_NPA_TOT_50km_SERF <- matrix(NA,length(listMPA_restricted_NP),200)

for (i in 1: length(listMPA_restricted_NP)) {# Pour chaque r??serve
  
  # Prendre les transects de cette r??serve & qui sont unfished
  IDReserverestricte<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_restricted_NP[i])
  IDReserverestricte<-subset(IDReserverestricte,IDReserverestricte$Infotrans.protec=="Restricted")
  
  
  #Transect restricted
  Transect_restricted<-Hab_all[rownames(Hab_all)%in%rownames(IDReserverestricte),]
  
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
  Transect50km_NP<-Hab_all[rownames(Hab_all)%in%IDTransect50km_NP,]
  
  
  if(dim(Transect50km_NP)[1]==0){next}
  
  #Calcul de la beta, du nombre d'esp partag?? et unique 
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (dim(Transect_restricted)[1]>dim(Transect50km_NP)[1]){
      
      Group_restricted_Sample<-as.matrix(Transect_restricted[sample(1:nrow(Transect_restricted),dim(Transect50km_NP)[1],replace=F),])
      Group_restricted2<-apply(Group_restricted_Sample,2,sum)
      Group_NP2<-apply(Transect50km_NP,2,sum)
      
    }
    
    else {
      
      Group_NP_Sample<-as.matrix(Transect50km_NP[sample(1:nrow(Transect50km_NP),dim(Transect_restricted)[1],replace=F),])
      Group_restricted2<-apply(Transect_restricted, 2,sum)
      Group_NP2<-apply(Group_NP_Sample, 2,sum)
      
    }
    
    
    Compa<-rbind(Group_restricted2,Group_NP2)
    
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habRA_NPA_TOT_50km_SERF[i,k] <- Dissim_hab[1]
    
    save(Dissim_habRA_NPA_TOT_50km_SERF,file="Dissim_habRA_NPA_TOT_50km_SERF.RData")
    
  }
  print(i)
}


