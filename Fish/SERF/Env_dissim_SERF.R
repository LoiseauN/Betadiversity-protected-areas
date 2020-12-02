load("Hab_all_SERF.RData")

reps=1

Dissim_habSPA_NPA_TOT_50km_SERF <- matrix(NA,length(listMPA_SPA_NPA),reps)

for (i in 1: length(listMPA_SPA_NPA)) {# For each PA
  
  #Transect SPA
  IDReserveSPA<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_SPA_NPA[i])
  IDReserveSPA<-subset(IDReserveSPA,IDReserveSPA$Infotrans.protec=="Unfished")
  
  Transect_SPA<-Hab_all_SERF[rownames(Hab_all_SERF)%in%rownames(IDReserveSPA),]
  
  #Transect non protected
  IDTransect50km<-NULL 
  for (j in 1:dim(Transect_SPA)[1]){
    
    if(dim(Transect_SPA)[1]==1){#strucure of the dataframe is different if dim(Transect_SPA)[1]==1.
      distGeo<-data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_SPA),])
      rownames(distGeo)<-colnames(geodist.Transect)}
    
    else{
      distGeo<-t(data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_SPA),]))
      rownames(distGeo)<-colnames(geodist.Transect)
    }
    
    distGeo2<-subset(distGeo,distGeo[,j]<50) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDTransect50km<-c(IDTransect50km,distGeo2)
  }
  IDTransect50km<-unique(IDTransect50km)
  
  #Transect non protected
  IDTransect50km_NPA<- IDTransect50km[IDTransect50km%in%listMPA_NPA]
  Transect50km_NPA<-Hab_all_SERF[rownames(Hab_all_SERF)%in%IDTransect50km_NPA,]
  
  if(dim(Transect50km_NPA)[1]==0){next}
  
  
  for (k in 1:reps){#Bootstrap due to different sampling effort
    
    if (dim(Transect_SPA)[1]>dim(Transect50km_NPA)[1]){
      
      Group_SPA_Sample<-as.matrix(Transect_SPA[sample(1:nrow(Transect_SPA),dim(Transect50km_NPA)[1],replace=F),])
      Group_SPA2<-apply(Group_SPA_Sample,2,mean,na.rm=TRUE)
      Group_NPA2<-apply(Transect50km_NPA,2,mean,na.rm=TRUE)
      
    }
    
    else {
      
      Group_NPA_Sample<-as.matrix(Transect50km_NPA[sample(1:nrow(Transect50km_NPA),dim(Transect_SPA)[1],replace=F),])
      Group_SPA2<-apply(Group_SPA_Sample,2,mean,na.rm=TRUE)
      Group_NPA2<-apply(Transect50km_NPA,2,mean,na.rm=TRUE)
      
    }
    
    
    Compa<-rbind(Group_SPA2,Group_NPA2)
    
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habSPA_NPA_TOT_50km_SERF[i,k] <- Dissim_hab[1]
    
    save(Dissim_habSPA_NPA_TOT_50km_SERF,file="Dissim_habSPA_NPA_TOT_50km_SERF.RData")
    
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}




#############################################################################################################################
Dissim_habSPA_RA_TOT_50km_SERF <- matrix(NA,length(listMPA_SPA_RA),reps)

for (i in 1: length(listMPA_SPA_RA)) {# Pour chaque r??serve
  
  # #Transect SPA
  IDReserveSPA<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_SPA_RA[i])
  IDReserveSPA<-subset(IDReserveSPA,IDReserveSPA$Infotrans.protec=="Unfished")
 
  Transect_SPA<-Hab_all_SERF[rownames(Hab_all_SERF)%in%rownames(IDReserveSPA),]
  
  #Transect RA
  IDTransect50km<-NULL 
  for (j in 1:dim(Transect_SPA)[1]){
    
    if(dim(Transect_SPA)[1]==1){#strucure of the dataframe is different if dim(Transect_SPA)[1]==1.
      distGeo<-data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_SPA),])
      rownames(distGeo)<-colnames(geodist.Transect)}
    
    else{
      distGeo<-t(data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_SPA),]))
      rownames(distGeo)<-colnames(geodist.Transect)
    }
    
    
    distGeo2<-subset(distGeo,distGeo[,j]<50) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDTransect50km<-c(IDTransect50km,distGeo2)
  }
  IDTransect50km<-unique(IDTransect50km)
  
  #Transect non protected
  IDTransect50km_RA<- IDTransect50km[IDTransect50km%in%listMPA_RA2]
  Transect50km_RA<-Hab_all_SERF[rownames(Hab_all_SERF)%in%IDTransect50km_RA,]
  
  if(dim(Transect50km_RA)[1]==0){next}
  
  

  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (dim(Transect_SPA)[1]>dim(Transect50km_RA)[1]){
      
      Group_SPA_Sample<-as.matrix(Transect_SPA[sample(1:nrow(Transect_SPA),dim(Transect50km_RA)[1],replace=F),])
      Group_SPA2<-apply(Group_SPA_Sample,2,mean,na.rm=TRUE)
      Group_RA2<-apply(Transect50km_RA,2,mean,na.rm=TRUE)
      
    }
    
    else {
      
      Group_RA_Sample<-as.matrix(Transect50km_RA[sample(1:nrow(Transect50km_RA),dim(Transect_SPA)[1],replace=F),])
      Group_SPA2<-apply(Transect_SPA, 2,mean,na.rm=TRUE)
      Group_RA2<-apply(Group_RA_Sample, 2,mean,na.rm=TRUE)
      
    }
    
    Compa<-rbind(Group_SPA2,Group_RA2)
    
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habSPA_RA_TOT_50km_SERF[i,k] <- Dissim_hab[1]
    
    save(Dissim_habSPA_RA_TOT_50km_SERF,file="Dissim_habSPA_RA_TOT_50km_SERF.RData")
    
  }
  print(i)
}




#############################################################################################################################

Dissim_habRA_NPA_TOT_50km_SERF <- matrix(NA,length(listMPA_RA_NPA),reps)

for (i in 1: length(listMPA_RA_NPA)) {# Pour chaque r??serve
  
  #Transect RA
  IDReserveRA<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_RA_NPA[i])
  IDReserveRA<-subset(IDReserveRA,IDReserveRA$Infotrans.protec=="Restricted")
  Transect_RA<-Hab_all_SERF[rownames(Hab_all_SERF)%in%rownames(IDReserveRA),]
  
  #Transect non protected
  IDTransect50km<-NULL 
  for (j in 1:dim(Transect_RA)[1]){
    
    if(dim(Transect_RA)[1]==1){#strucure of the dataframe is different if dim(Transect_RA)[1]==1.
      distGeo<-data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_RA),])
      rownames(distGeo)<-colnames(geodist.Transect)}
    
    else{
      distGeo<-t(data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_RA),]))
      rownames(distGeo)<-colnames(geodist.Transect)
    }
    
    
    distGeo2<-subset(distGeo,distGeo[,j]<50) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDTransect50km<-c(IDTransect50km,distGeo2)
  }
  IDTransect50km<-unique(IDTransect50km)
  
    #Transect non protected
  IDTransect50km_NPA<- IDTransect50km[IDTransect50km%in%listMPA_NPA]
  Transect50km_NPA<-Hab_all_SERF[rownames(Hab_all_SERF)%in%IDTransect50km_NPA,]
  
  
  if(dim(Transect50km_NPA)[1]==0){next}
  
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (dim(Transect_RA)[1]>dim(Transect50km_NPA)[1]){
      
      Group_RA_Sample<-as.matrix(Transect_RA[sample(1:nrow(Transect_RA),dim(Transect50km_NPA)[1],replace=F),])
      Group_RA2<-apply(Group_RA_Sample,2,sum)
      Group_NPA2<-apply(Transect50km_NPA,2,sum)
      
    }
    
    else {
      
      Group_NPA_Sample<-as.matrix(Transect50km_NPA[sample(1:nrow(Transect50km_NPA),dim(Transect_RA)[1],replace=F),])
      Group_RA2<-apply(Transect_RA, 2,sum)
      Group_NPA2<-apply(Group_NPA_Sample, 2,sum)
      
    }
    
    Compa<-rbind(Group_RA2,Group_NPA2)
    
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habRA_NPA_TOT_50km_SERF[i,k] <- Dissim_hab[1]
    
    save(Dissim_habRA_NPA_TOT_50km_SERF,file="Dissim_habRA_NPA_TOT_50km_SERF.RData")
    
  }
  print(i)
}


