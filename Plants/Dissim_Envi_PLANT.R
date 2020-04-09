
load("hab_all_plant.RData")

repboot=1000
Dissim_habSPA_NPA_TOT_50km_Plant<-matrix(NA,5,repboot)

for (i in 1: length(listMPA_SPA)) {# For each PA
  #Transect SPA
  IDReserveSPA<-subset(InfoTOTAL2,InfoTOTAL2$namesMPA==listMPA_SPA[i])
  IDReserveSPA<-subset(IDReserveSPA,IDReserveSPA$V2 =="Integral")
  
 
  if (dim(IDReserveSPA)[1]==1)  { Transect_SPAd<-t(as.data.frame(hab_all_plant[rownames(hab_all_plant)%in%rownames(IDReserveSPA),]))
  rownames(Transect_SPAd)<-rownames(IDReserveSPA)  } 
  if (dim(IDReserveSPA)[1]>1) {  Transect_SPAd<-as.data.frame(hab_all_plant[rownames(hab_all_plant)%in%rownames(IDReserveSPA),])} 
  
  Transect50km_NPA<-hab_all_plant[rownames(hab_all_plant)%in%IDTransect50km_SPAvsNPA[[i]],]
  
  for (k in 1:repboot){ #Bootstrap due to different sampling effort
    
    if(dim(Transect_SPAd)[1]==1){ 
      Group_NPA_Sample<-as.matrix(Transect50km_NPA[sample(1:nrow(Transect50km_NPA),dim(Transect_SPAd)[1],replace=F),])
      Group_SPAD2<-apply(Transect_SPAd, 2,mean)
      Group_NPA2<-apply(Group_NPA_Sample, 2,mean)
    }
    
    
    if (dim(Transect_SPAd)[1]>dim(Transect50km_NPA)[1]){
      
      Group_SPAD_Sample<-as.matrix(Transect_SPAd[sample(1:nrow(Transect_SPAd),dim(Transect50km_NPA)[1],replace=F),])
      Group_SPAD2<-apply(Group_SPAD_Sample,2,mean)
      Group_NPA2<-apply(Transect50km_NPA,2,mean)
      
    }
    
    if (dim(Transect_SPAd)[1]<dim(Transect50km_NPA)[1]){
      Group_NPA_Sample<-as.matrix(Transect50km_NPA[sample(1:nrow(Transect50km_NPA),dim(Transect_SPAd)[1],replace=F),])
      Group_SPAD2<-apply(Transect_SPAd, 2,mean)
      Group_NPA2<-apply(Group_NPA_Sample, 2,mean)
      
    }
    
    Compa<-rbind(Group_SPAD2,Group_NPA2)
    
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habSPA_NPA_TOT_50km_Plant[i,k] <- Dissim_hab[1]
    
    save(Dissim_habSPA_NPA_TOT_50km_Plant,file="Dissim_habSPA_NPA_TOT_50km_Plant.RData")
    
    print(paste0("k",k))
    
  }
  
  print(paste ("i",i))
}




##################################################################################################################################


Dissim_habSPA_RA_TOT_50km_Plant<-matrix(NA,5,repboot)


for (i in 1: length(listMPA_SPA)) {# Pour chaque r??serve
  
  # Prendre les transects de cette r??serve & qui sont unfished
  IDReserveSPA<-subset(InfoTOTAL2,InfoTOTAL2$namesMPA==listMPA_SPA[i])
  IDReserveSPA<-subset(IDReserveSPA,IDReserveSPA$V2 =="Integral")
  
  
  #Transect SPAd
  if (dim(IDReserveSPA)[1]==1)  { Transect_SPAd<-t(as.data.frame(hab_all_plant[rownames(hab_all_plant)%in%rownames(IDReserveSPA),]))
  rownames(Transect_SPAd)<-rownames(IDReserveSPA)  } 
  if (dim(IDReserveSPA)[1]>1) {  Transect_SPAd<-as.data.frame(hab_all_plant[rownames(hab_all_plant)%in%rownames(IDReserveSPA),])} 
  
  Transect50km_RA<-hab_all_plant[rownames(hab_all_plant)%in%IDTransect50km_SPAvsRA[[i]],]
  
  for (k in 1:repboot){ #Bootstrap due to different sampling effort
    
    if(dim(Transect_SPAd)[1]==1){ 
      Group_RA_Sample<-as.matrix(Transect50km_RA[sample(1:nrow(Transect50km_RA),dim(Transect_SPAd)[1],replace=F),])
      Group_SPAD2<-apply(Transect_SPAd, 2,mean)
      Group_RA2<-apply(Group_RA_Sample, 2,mean)
    }
    
    if (dim(Transect_SPAd)[1]>dim(Transect50km_RA)[1]){
      
      Group_SPAD_Sample<-as.matrix(Transect_SPAd[sample(1:nrow(Transect_SPAd),dim(Transect50km_RA)[1],replace=F),])
      Group_SPAD2<-apply(Group_SPAD_Sample,2,mean)
      Group_RA2<-apply(Transect50km_RA,2,mean)
      
    }
    
    if (dim(Transect_SPAd)[1]<dim(Transect50km_RA)[1]){
      Group_RA_Sample<-as.matrix(Transect50km_RA[sample(1:nrow(Transect50km_RA),dim(Transect_SPAd)[1],replace=F),])
      Group_SPAD2<-apply(Transect_SPAd, 2,mean)
      Group_RA2<-apply(Group_RA_Sample, 2,mean)
      
      
    }
    
    
    Compa<-rbind(Group_SPAD2,Group_RA2)
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habSPA_RA_TOT_50km_Plant[i,k] <- Dissim_hab[1]
    
    save(Dissim_habSPA_RA_TOT_50km_Plant,file="Dissim_habSPA_RA_TOT_50km_Plant.RData")
    
    print(paste0("k",k))
    
  }
  
  print(paste ("i",i))
}


##################################################################################################################################
Dissim_habRA_NPA_TOT_50km_Plant<-matrix(NA,length(listMPA_RA),repboot)

for (i in 1:length(listMPA_RA)) {# Pour chaque r??serve
  
  # Prendre les transects de cette r??serve & qui sont unfished
  IDReserveRA<-subset(InfoTOTAL2,InfoTOTAL2$namesMPA==listMPA_RA[i])
  IDReserveRA<-subset(IDReserveRA,IDReserveRA$V2 =="Restreint")
  
  
  #Transect RA
  if (dim(IDReserveRA)[1]==1)  { IDReserveRA<-t(as.data.frame(hab_all_plant[rownames(hab_all_plant)%in%rownames(IDReserveRA),]))
  rownames(IDReserveRA)<-rownames(IDReserveRA)  } 
  if (dim(IDReserveRA)[1]>1) {  Transect_RA<-as.data.frame(hab_all_plant[rownames(hab_all_plant)%in%rownames(IDReserveRA),])} 
  
  Transect50km_NPA<-hab_all_plant[rownames(hab_all_plant)%in%IDTransect50km_RAvsNPA[[i]],]
  
  for (k in 1:repboot){ #Bootstrap due to different sampling effort
    
    if(dim(Transect_RA)[1]==1){ 
      Group_NPA_Sample<-as.matrix(Transect50km_NPA[sample(1:nrow(Transect50km_NPA),dim(Transect_RA)[1],replace=F),])
      Group_RA2<-apply(Transect_RA, 2,mean)
      Group_NPA2<-apply(Group_NPA_Sample, 2,mean)
      
    }
    
    
    if (dim(Transect_RA)[1]>dim(Transect50km_NPA)[1]){
      
      Group_RA_Sample<-as.matrix(Transect_RA[sample(1:nrow(Transect_RA),dim(Transect50km_NPA)[1],replace=F),])
      Group_RA2<-apply(Group_RA_Sample,2,mean)
      Group_NPA2<-apply(Transect50km_NPA,2,mean)
      
      
    }
    
    if (dim(Transect_RA)[1]<dim(Transect50km_NPA)[1]){
      Group_NPA_Sample<-as.matrix(Transect50km_NPA[sample(1:nrow(Transect50km_NPA),dim(Transect_RA)[1],replace=F),])
      Group_RA2<-apply(Transect_RA, 2,mean)
      Group_NPA2<-apply(Group_NPA_Sample, 2,mean)
      
      
    }
    
    Compa<-rbind(Group_RA2,Group_NPA2)
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habRA_NPA_TOT_50km_Plant[i,k] <- Dissim_hab[1]
    
    save(Dissim_habRA_NPA_TOT_50km_Plant,file="Dissim_habRA_NPA_TOT_50km_Plant.RData")
    
    print(paste0("k",k))
    
  }
  
  print(paste ("i",i))
}
