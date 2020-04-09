load("Hab_all.RData")
reps=1000
core=30
scale=50 #10 or 50 or 100km

Dissim_habSPA_NPA_TOT_50km_Bird<-matrix(NA,length(listMPA_SPA),reps)

for (i in 1: length(listMPA_SPA)) {# For each PA
  
  #survey protected
  IDReserveSPA<-subset(info_survey,info_survey$namesMPA==listMPA_SPA[i])

  IDsurvey50km<-NULL 
  for (j in 1: dim(IDReserveSPA)[1]){
    
    if(dim(IDReserveSPA)[1]==1){#strucure of the dataframe is different if dim(survey_SPAd)[1]==1.
      distGeo<-data.frame(geodist.Bird[rownames(geodist.Bird)%in%IDReserveSPA$NewRouteID,])
      rownames(distGeo)<-colnames(geodist.Bird)}
    
    else{
      distGeo<-t(data.frame(geodist.Bird[rownames(geodist.Bird)%in%IDReserveSPA$NewRouteID,]))
      rownames(distGeo)<-colnames(geodist.Bird)
    }
    distGeo2<-subset(distGeo,distGeo[,j]<50) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDsurvey50km<-c(IDsurvey50km,distGeo2)
  }
  IDsurvey50km<-unique(IDsurvey50km)
  
  #survey non protected
  IDsurvey50km_NPA<- IDsurvey50km[IDsurvey50km%in%listMPA_NPA]
  
  #Calcul de la beta, du nombre d'esp partag?? et unique 
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (length(IDsurvey50km_NPA) == 0){
      next
    }
    
    if (dim(IDReserveSPA)[1]>length(IDsurvey50km_NPA)){
      
      #survey NPA
      
      survey50km_NPA <-   Hab_all[rownames(Hab_all)%in%IDsurvey50km_NPA,]
      
      #survey SPAd
      names_SPAd_Sample<-sample(IDReserveSPA$NewRouteID,length(IDsurvey50km_NPA),replace=F)
      survey_SPAd <-   Hab_all[rownames(Hab_all) %in% names_SPAd_Sample,]
      
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_SPAd, survey50km_NPA)
      rownames(Compa)<-c(rownames(survey_SPAd),rownames(survey50km_NPA))
      
      
    }
    
    else {
      
      #survey NPA
      names_NPA_Sample<-sample(IDsurvey50km_NPA,dim(IDReserveSPA)[1],replace=F)
      survey50km_NPA <-   Hab_all[rownames(Hab_all)%in%names_NPA_Sample,]
      
      
      #survey SPAd
      survey_SPAd<-Hab_all[rownames(Hab_all)  %in%IDReserveSPA$NewRouteID,]
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_SPAd, survey50km_NPA)
      rownames(Compa)<-c(rownames(survey_SPAd),rownames(survey50km_NPA))
      
    }
    
    Compa<-rbind(apply(Compa[1:dim(survey_SPAd)[1],],2,mean,na.rm=T),apply(Compa[(dim(survey50km_NPA)[1]+1):dim(Compa)[1],],2,mean,na.rm=T))
    
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habSPA_NPA_TOT_50km_Bird[i,k] <- Dissim_hab[1]
    
    save(Dissim_habSPA_NPA_TOT_50km_Bird,file="Dissim_habSPA_NPA_TOT_50km_Bird.RData")
    
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}



####################################################################################################################################################################################

Dissim_habSPA_RA_TOT_50km_Bird<-matrix(NA,length(listMPA_SPA),reps)


for (i in 1: length(listMPA_SPA)) {# Pour chaque r??serve
  
  #survey protected
  IDReserveSPA<-subset(info_survey,info_survey$namesMPA==listMPA_SPA[i])
  
  IDsurvey50km<-NULL 
  for (j in 1: dim(IDReserveSPA)[1]){
    
    if(dim(IDReserveSPA)[1]==1){#strucure of the dataframe is different if dim(survey_SPAd)[1]==1.
      distGeo<-data.frame(geodist.Bird[rownames(geodist.Bird)%in%IDReserveSPA$NewRouteID,])
      rownames(distGeo)<-colnames(geodist.Bird)}
    
    else{
      distGeo<-t(data.frame(geodist.Bird[rownames(geodist.Bird)%in%IDReserveSPA$NewRouteID,]))
      rownames(distGeo)<-colnames(geodist.Bird)
    }
    distGeo2<-subset(distGeo,distGeo[,j]<50) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDsurvey50km<-c(IDsurvey50km,distGeo2)
  }
  IDsurvey50km<-unique(IDsurvey50km)
  
  #survey non protected
  IDsurvey50km_RESTRICT<- IDsurvey50km[IDsurvey50km%in%listMPA_RA2]
  
  #Calcul de la beta, du nombre d'esp partag?? et unique 
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (length(IDsurvey50km_RESTRICT) == 0){
      next
    }
    
    if (dim(IDReserveSPA)[1]>length(IDsurvey50km_RESTRICT)){
      
      #survey RESTRICT
      survey50km_RESTRICT<-Hab_all[rownames(Hab_all) %in%IDsurvey50km_RESTRICT,]
      
      #survey SPAd
      names_SPAd_Sample<-sample(IDReserveSPA$NewRouteID,length(IDsurvey50km_RESTRICT),replace=F)
      survey_SPAd<-Hab_all[rownames(Hab_all) %in%names_SPAd_Sample,]
      
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_SPAd, survey50km_RESTRICT)
      rownames(Compa)<-c(rownames(survey_SPAd),rownames(survey50km_RESTRICT))
      
    }
    
    
    else {
      
      #survey RESTRICT
      names_RESTRICT_Sample<-sample(IDsurvey50km_RESTRICT,dim(IDReserveSPA)[1],replace=F)
      survey50km_RESTRICT<-Hab_all[rownames(Hab_all) %in%names_RESTRICT_Sample,]
      
      #survey SPAd
      survey_SPAd<-Hab_all[rownames(Hab_all) %in%IDReserveSPA$NewRouteID,]
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_SPAd, survey50km_RESTRICT)
      rownames(Compa)<-c(rownames(survey_SPAd),rownames(survey50km_RESTRICT))
      
    }
    
    Compa<-rbind(apply(Compa[1:dim(survey_SPAd)[1],],2,mean,na.rm=T),apply(Compa[(dim(survey50km_RESTRICT)[1]+1):dim(Compa)[1],],2,mean,na.rm=T))
    
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habSPA_RA_TOT_50km_Bird[i,k] <- Dissim_hab[1]
    
    save(Dissim_habSPA_RA_TOT_50km_Bird,file="Dissim_habSPA_RA_TOT_50km_Bird.RData")
    
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}







####################################################################################################################################################################################

Dissim_habRA_NPA_TOT_50km_Bird<-matrix(NA,length(listMPA_RA),reps)

for (i in 1: length(listMPA_RA)) {# Pour chaque r??serve
  
  #survey protected
  IDReserveRA<-subset(info_survey,info_survey$namesMPA==listMPA_RA[i])
  
  IDTransect50km<-NULL 
  for (j in 1:dim(IDReserveRA)[1]){
    
    if(dim(IDReserveRA)[1]==1){
      distGeo<-data.frame(geodist.Bird[rownames(geodist.Bird)%in%rownames(IDReserveRA),])
      rownames(distGeo)<-colnames(geodist.Bird)}
    
    else{
      distGeo<-t(data.frame(geodist.Bird[rownames(geodist.Bird)%in%rownames(IDReserveRA),]))
      rownames(distGeo)<-colnames(geodist.Bird)
    }
    
    
    distGeo2<-subset(distGeo,distGeo[,j]<50) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDTransect50km<-c(IDTransect50km,distGeo2)
  }
  IDTransect50km<-unique(IDTransect50km)
  
  #Transect non protected
  IDsurvey50km_NPA<- IDTransect50km[IDTransect50km%in%listMPA_NPA]
  
  
  #Calcul de la beta, du nombre d'esp partag?? et unique 
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (length(IDsurvey50km_NPA) == 0){
      next
    }
    
    if (dim(IDReserveRA)[1]>length(IDsurvey50km_NPA)){
      
      #survey NPA
      survey50km_NPA<-Hab_all[rownames(Hab_all)%in%IDsurvey50km_NPA,]
      
      
      #survey RA
      names_RA_Sample<-sample(IDReserveRA$NewRouteID,length(IDsurvey50km_NPA),replace=F)
      survey_RA<-Hab_all[rownames(Hab_all)%in%names_RA_Sample,]
      
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_RA, survey50km_NPA)
      rownames(Compa)<-c(rownames(survey_RA),rownames(survey50km_NPA))
      
      
    }
    
    else {
      
      #survey NPA
      names_NPA_Sample<-sample(IDsurvey50km_NPA,dim(IDReserveRA)[1],replace=F)
      survey50km_NPA<-Hab_all[rownames(Hab_all)%in%names_NPA_Sample,]
      
      #survey RA
      survey_RA<-Hab_all[rownames(Hab_all)%in%IDReserveRA$NewRouteID,]
      
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_RA, survey50km_NPA)
      rownames(Compa)<-c(rownames(survey_RA),rownames(survey50km_NPA))
      
    }
    
    Compa<-rbind(apply(Compa[1:dim(survey_RA)[1],],2,mean,na.rm=T),apply(Compa[(dim(survey50km_NPA)[1]+1):dim(Compa)[1],],2,mean,na.rm=T))
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habRA_NPA_TOT_50km_Bird[i,k] <- Dissim_hab[1]
    
    save(Dissim_habRA_NPA_TOT_50km_Bird,file="Dissim_habRA_NPA_TOT_50km_Bird.RData")
    
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}
