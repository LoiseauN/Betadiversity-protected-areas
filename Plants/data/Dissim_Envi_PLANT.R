
setwd("~/Documents/Postdoc MARBEC/BETA PROTECTED AREA/PLANT/CESBIO")
rast = raster::raster("OCS_2016_CESBIO_Alps.img")

load("listMPA_nonprotect.RData")
load("listMPA_restricted.RData")
load("listMPA_restricted2.RData")
load("listMPA_close.RData")
load("InfoTOTAL2.RData")
 load("~/Documents/Postdoc MARBEC/BETA PROTECTED AREA/BetaPlant180718/IDTransect50km_CLOSEVSRESTR.RData")
 load("~/Documents/Postdoc MARBEC/BETA PROTECTED AREA/BetaPlant180718/IDTransect50km_NP.RData")
 load("~/Documents/Postdoc MARBEC/BETA PROTECTED AREA/BetaPlant180718/IDTransect50km_RestrVSoutside.RData")

load("IDTransect50km_CLOSEVSRESTR.RData")
load("IDTransect50km_RestrVSoutside.RData")
load("ValLC_081118.RData")
load("~/Documents/Postdoc MARBEC/BETA PROTECTED AREA_CLEAN/Plant/Sensitivity analyses/Plants_temp_prec.RData")

require(plyr)


ValLC2 <- lapply(ValLC,function(y){t(as.matrix(y))})
ValLC3 <- rbind.fill(lapply(ValLC2,function(y){as.data.frame((y),stringsAsFactors=FALSE)}))
hab_mat <- ValLC3
hab_mat[is.na(hab_mat)]<-0

for(i in 1:nrow(hab_mat)){ 
  print(i)
  hab_mat[i,]<-(hab_mat[i,]/sum(hab_mat[i,]))*100}
  
  
rownames(hab_mat)<-rownames(occ)

hab_all<-merge(hab_mat,Plants_temp_prec,by.x="row.names",by.y="names")
rownames(hab_all)<-hab_all[,1]
hab_all<-hab_all[,-c(1,20,21)]
hab_all$Precipitation <-log(hab_all$Precipitation)


Dissim_habSPA_NPA_TOT_50km_Plant<-matrix(NA,5,200)

for (i in 1: 5) {# Pour chaque r??serve
  
  # Prendre les transects de cette r??serve & qui sont unfished
  IDReserveClose<-subset(InfoTOTAL2,InfoTOTAL2$namesMPA==listMPA_close[i])
  IDReserveClose<-subset(IDReserveClose,IDReserveClose$V2 =="Integral")
  
  
  #Transect Closed
  if (dim(IDReserveClose)[1]==1)  { Transect_Closed<-t(as.data.frame(hab_all[rownames(hab_all)%in%rownames(IDReserveClose),]))
  rownames(Transect_Closed)<-rownames(IDReserveClose)  } 
  if (dim(IDReserveClose)[1]>1) {  Transect_Closed<-as.data.frame(hab_all[rownames(hab_all)%in%rownames(IDReserveClose),])} 
  
  Transect50km_NP<-hab_all[rownames(hab_all)%in%IDTransect50km_NP[[i]],]
  
  for (k in 1:repboot){ #Bootstrap due to different sampling effort
    
    if(dim(Transect_Closed)[1]==1){ 
      Group_NP_Sample<-as.matrix(Transect50km_NP[sample(1:nrow(Transect50km_NP),dim(Transect_Closed)[1],replace=F),])
      Group_CLOSED2<-apply(Transect_Closed, 2,mean)
      Group_NP2<-apply(Group_NP_Sample, 2,mean)
    }
    
    
    if (dim(Transect_Closed)[1]>dim(Transect50km_NP)[1]){
      
      Group_CLOSED_Sample<-as.matrix(Transect_Closed[sample(1:nrow(Transect_Closed),dim(Transect50km_NP)[1],replace=F),])
      Group_CLOSED2<-apply(Group_CLOSED_Sample,2,mean)
      Group_NP2<-apply(Transect50km_NP,2,mean)
      
    }
    
    if (dim(Transect_Closed)[1]<dim(Transect50km_NP)[1]){
      Group_NP_Sample<-as.matrix(Transect50km_NP[sample(1:nrow(Transect50km_NP),dim(Transect_Closed)[1],replace=F),])
      Group_CLOSED2<-apply(Transect_Closed, 2,mean)
      Group_NP2<-apply(Group_NP_Sample, 2,mean)
      
    }
    
    Compa<-rbind(Group_CLOSED2,Group_NP2)

    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habSPA_NPA_TOT_50km_Plant[i,k] <- Dissim_hab[1]
    
    save(Dissim_habSPA_NPA_TOT_50km_Plant,file="Dissim_habSPA_NPA_TOT_50km_Plant.RData")
    
    print(paste0("k",k))
    
  }
  
  print(paste ("i",i))
}




##################################################################################################################################


Dissim_habSPA_RA_TOT_50km_Plant<-matrix(NA,5,200)


for (i in 1: 5) {# Pour chaque r??serve
  
  # Prendre les transects de cette r??serve & qui sont unfished
  IDReserveClose<-subset(InfoTOTAL2,InfoTOTAL2$namesMPA==listMPA_close[i])
  IDReserveClose<-subset(IDReserveClose,IDReserveClose$V2 =="Integral")
  
  
  #Transect Closed
  if (dim(IDReserveClose)[1]==1)  { Transect_Closed<-t(as.data.frame(hab_all[rownames(hab_all)%in%rownames(IDReserveClose),]))
  rownames(Transect_Closed)<-rownames(IDReserveClose)  } 
  if (dim(IDReserveClose)[1]>1) {  Transect_Closed<-as.data.frame(hab_all[rownames(hab_all)%in%rownames(IDReserveClose),])} 
  
  Transect50km_Restrict<-hab_all[rownames(hab_all)%in%IDTransect50km_CLOSEVSRESTR[[i]],]
  
  for (k in 1:repboot){ #Bootstrap due to different sampling effort
    
    if(dim(Transect_Closed)[1]==1){ 
      Group_Restrict_Sample<-as.matrix(Transect50km_Restrict[sample(1:nrow(Transect50km_Restrict),dim(Transect_Closed)[1],replace=F),])
      Group_CLOSED2<-apply(Transect_Closed, 2,mean)
      Group_Restrict2<-apply(Group_Restrict_Sample, 2,mean)
    }
    
    if (dim(Transect_Closed)[1]>dim(Transect50km_Restrict)[1]){
      
      Group_CLOSED_Sample<-as.matrix(Transect_Closed[sample(1:nrow(Transect_Closed),dim(Transect50km_Restrict)[1],replace=F),])
      Group_CLOSED2<-apply(Group_CLOSED_Sample,2,mean)
      Group_Restrict2<-apply(Transect50km_Restrict,2,mean)
      
    }
    
    if (dim(Transect_Closed)[1]<dim(Transect50km_Restrict)[1]){
      Group_Restrict_Sample<-as.matrix(Transect50km_Restrict[sample(1:nrow(Transect50km_Restrict),dim(Transect_Closed)[1],replace=F),])
      Group_CLOSED2<-apply(Transect_Closed, 2,mean)
      Group_Restrict2<-apply(Group_Restrict_Sample, 2,mean)
      
      
    }
    
    
    Compa<-rbind(Group_CLOSED2,Group_Restrict2)
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habSPA_RA_TOT_50km_Plant[i,k] <- Dissim_hab[1]
    
    save(Dissim_habSPA_RA_TOT_50km_Plant,file="Dissim_habSPA_RA_TOT_50km_Plant.RData")
    
    print(paste0("k",k))
    
  }
  
  print(paste ("i",i))
}


##################################################################################################################################


Dissim_habRA_NPA_TOT_50km_Plant<-matrix(NA,length(listMPA_restricted),200)


for (i in 1:length(listMPA_restricted)) {# Pour chaque r??serve
  
  # Prendre les transects de cette r??serve & qui sont unfished
  IDReserveRestricted<-subset(InfoTOTAL2,InfoTOTAL2$namesMPA==listMPA_restricted[i])
  IDReserveRestricted<-subset(IDReserveRestricted,IDReserveRestricted$V2 =="Restreint")
  
  
  #Transect Restricted
  if (dim(IDReserveRestricted)[1]==1)  { IDReserveRestricted<-t(as.data.frame(hab_all[rownames(hab_all)%in%rownames(IDReserveRestricted),]))
  rownames(IDReserveRestricted)<-rownames(IDReserveRestricted)  } 
  if (dim(IDReserveRestricted)[1]>1) {  Transect_Restricted<-as.data.frame(hab_all[rownames(hab_all)%in%rownames(IDReserveRestricted),])} 
  
  Transect50km_NP<-hab_all[rownames(hab_all)%in%IDTransect50km_RestrVSoutside[[i]],]
  
  for (k in 1:repboot){ #Bootstrap due to different sampling effort
    
    if(dim(Transect_Restricted)[1]==1){ 
      Group_NP_Sample<-as.matrix(Transect50km_NP[sample(1:nrow(Transect50km_NP),dim(Transect_Restricted)[1],replace=F),])
      Group_Restricted2<-apply(Transect_Restricted, 2,mean)
      Group_NP2<-apply(Group_NP_Sample, 2,mean)
      
    }
    
    
    if (dim(Transect_Restricted)[1]>dim(Transect50km_NP)[1]){
      
      Group_Restricted_Sample<-as.matrix(Transect_Restricted[sample(1:nrow(Transect_Restricted),dim(Transect50km_NP)[1],replace=F),])
      Group_Restricted2<-apply(Group_Restricted_Sample,2,mean)
      Group_NP2<-apply(Transect50km_NP,2,mean)
      
      
    }
    
    if (dim(Transect_Restricted)[1]<dim(Transect50km_NP)[1]){
      Group_NP_Sample<-as.matrix(Transect50km_NP[sample(1:nrow(Transect50km_NP),dim(Transect_Restricted)[1],replace=F),])
      Group_Restricted2<-apply(Transect_Restricted, 2,mean)
      Group_NP2<-apply(Group_NP_Sample, 2,mean)
      
      
    }
    
    Compa<-rbind(Group_Restricted2,Group_NP2)
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habRA_NPA_TOT_50km_Plant[i,k] <- Dissim_hab[1]
    
    save(Dissim_habRA_NPA_TOT_50km_Plant,file="Dissim_habRA_NPA_TOT_50km_Plant.RData")
    
    print(paste0("k",k))
    
  }
  
  print(paste ("i",i))
}

