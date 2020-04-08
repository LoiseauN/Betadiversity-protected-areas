load("~/Documents/Postdoc MARBEC/BETA PROTECTED AREA_CLEAN/Birds/Data/Environmental_data/Map habitat/nlcd_2011_landcover_2011_edition_2014_10_10/Habitatdiversity_Birds2.RData")
load("~/Documents/Postdoc MARBEC/BETA PROTECTED AREA GITHUB/data/Birds/Temp_prec/Birds_temp_prec.RData")

Hab_all <- merge(Habitatdiversity_Birds2,Birds_temp_prec,by.x="row.names",by.y="name" )
rownames(Hab_all) <- Hab_all[,1]
Hab_all <- Hab_all[,-c(1,17,18)]
Hab_all$Precipitation<-log(Hab_all$Precipitation)
setwd("~/Documents/Postdoc MARBEC/BETA PROTECTED AREA_CLEAN/Birds/Data/Results")

reps=200
core=3
scale=50 #50 or 50 or 500km

Dissim_habSPA_NPA_TOT_50km_Bird<-matrix(NA,length(listMPA_close),reps)

for (i in 1: length(listMPA_close)) {# Pour chaque r??serve
  
  # Prendre les surveys de cette r??serve
  IDReserveClose<-subset(routes3,routes3$namesMPA==listMPA_close[i])
  
  #survey un protected
  #survey un protected
  IDsurvey50km<-NULL 
  for (j in 1: dim(IDReserveClose)[1]){
    
    if(dim(IDReserveClose)[1]==1){#strucure of the dataframe is different if dim(survey_Closed)[1]==1.
      distGeo<-data.frame(geodist.Bird[rownames(geodist.Bird)%in%IDReserveClose$NewRouteID,])
      rownames(distGeo)<-colnames(geodist.Bird)}
    
    else{
      distGeo<-t(data.frame(geodist.Bird[rownames(geodist.Bird)%in%IDReserveClose$NewRouteID,]))
      rownames(distGeo)<-colnames(geodist.Bird)
    }
    distGeo2<-subset(distGeo,distGeo[,j]<50) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDsurvey50km<-c(IDsurvey50km,distGeo2)
  }
  IDsurvey50km<-unique(IDsurvey50km)
  
  #survey non protected
  IDsurvey50km_NP<- IDsurvey50km[IDsurvey50km%in%listMPA_nonprotect]
  
  #Calcul de la beta, du nombre d'esp partag?? et unique 
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (length(IDsurvey50km_NP) == 0){
      next
    }
    
    if (dim(IDReserveClose)[1]>length(IDsurvey50km_NP)){
      
      #survey NP
      
      survey50km_NP <-   Hab_all[rownames(Hab_all)%in%IDsurvey50km_NP,]
     
      #survey Closed
      names_Closed_Sample<-sample(IDReserveClose$NewRouteID,length(IDsurvey50km_NP),replace=F)
      survey_Closed <-   Hab_all[rownames(Hab_all) %in% names_Closed_Sample,]

      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_Closed, survey50km_NP)
      rownames(Compa)<-c(rownames(survey_Closed),rownames(survey50km_NP))


    }
    
  else {
      
      #survey NP
      names_NP_Sample<-sample(IDsurvey50km_NP,dim(IDReserveClose)[1],replace=F)
      survey50km_NP <-   Hab_all[rownames(Hab_all)%in%names_NP_Sample,]

      
      #survey Closed
      survey_Closed<-Hab_all[rownames(Hab_all)  %in%IDReserveClose$NewRouteID,]
   
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_Closed, survey50km_NP)
      rownames(Compa)<-c(rownames(survey_Closed),rownames(survey50km_NP))
 
    }
    
    Compa<-rbind(apply(Compa[1:dim(survey_Closed)[1],],2,mean,na.rm=T),apply(Compa[(dim(survey50km_NP)[1]+1):dim(Compa)[1],],2,mean,na.rm=T))

     Dissim_hab <- daisy(Compa, metric = "euclidean")
    
     Dissim_habSPA_NPA_TOT_50km_Bird[i,k] <- Dissim_hab[1]
    
    save(Dissim_habSPA_NPA_TOT_50km_Bird,file="Dissim_habSPA_NPA_TOT_50km_Bird.RData")
    
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}



####################################################################################################################################################################################

Dissim_habSPA_RA_TOT_50km_Bird<-matrix(NA,length(listMPA_close),reps)


for (i in 1: length(listMPA_close)) {# Pour chaque r??serve
  
  # Prendre les surveys de cette r??serve
  IDReserveClose<-subset(routes3,routes3$namesMPA==listMPA_close[i])
  
  #survey un protected
  #survey un protected
  IDsurvey50km<-NULL 
  for (j in 1: dim(IDReserveClose)[1]){
    
    if(dim(IDReserveClose)[1]==1){#strucure of the dataframe is different if dim(survey_Closed)[1]==1.
      distGeo<-data.frame(geodist.Bird[rownames(geodist.Bird)%in%IDReserveClose$NewRouteID,])
      rownames(distGeo)<-colnames(geodist.Bird)}
    
    else{
      distGeo<-t(data.frame(geodist.Bird[rownames(geodist.Bird)%in%IDReserveClose$NewRouteID,]))
      rownames(distGeo)<-colnames(geodist.Bird)
    }
    distGeo2<-subset(distGeo,distGeo[,j]<50) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDsurvey50km<-c(IDsurvey50km,distGeo2)
  }
  IDsurvey50km<-unique(IDsurvey50km)
  
  #survey non protected
  IDsurvey50km_RESTRICT<- IDsurvey50km[IDsurvey50km%in%listMPA_restricted2]
  
  #Calcul de la beta, du nombre d'esp partag?? et unique 
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (length(IDsurvey50km_RESTRICT) == 0){
      next
    }
    
    if (dim(IDReserveClose)[1]>length(IDsurvey50km_RESTRICT)){
      
      #survey RESTRICT
      survey50km_RESTRICT<-Hab_all[rownames(Hab_all) %in%IDsurvey50km_RESTRICT,]
      
      #survey Closed
      names_Closed_Sample<-sample(IDReserveClose$NewRouteID,length(IDsurvey50km_RESTRICT),replace=F)
      survey_Closed<-Hab_all[rownames(Hab_all) %in%names_Closed_Sample,]
      
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_Closed, survey50km_RESTRICT)
      rownames(Compa)<-c(rownames(survey_Closed),rownames(survey50km_RESTRICT))
     
    }
    
    
    else {
      
      #survey RESTRICT
      names_RESTRICT_Sample<-sample(IDsurvey50km_RESTRICT,dim(IDReserveClose)[1],replace=F)
      survey50km_RESTRICT<-Hab_all[rownames(Hab_all) %in%names_RESTRICT_Sample,]

      #survey Closed
      survey_Closed<-Hab_all[rownames(Hab_all) %in%IDReserveClose$NewRouteID,]
 
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_Closed, survey50km_RESTRICT)
      rownames(Compa)<-c(rownames(survey_Closed),rownames(survey50km_RESTRICT))

    }
    
    Compa<-rbind(apply(Compa[1:dim(survey_Closed)[1],],2,mean,na.rm=T),apply(Compa[(dim(survey50km_RESTRICT)[1]+1):dim(Compa)[1],],2,mean,na.rm=T))
    
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habSPA_RA_TOT_50km_Bird[i,k] <- Dissim_hab[1]
    
    save(Dissim_habSPA_RA_TOT_50km_Bird,file="Dissim_habSPA_RA_TOT_50km_Bird.RData")
    
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}







####################################################################################################################################################################################

Dissim_habRA_NPA_TOT_50km_Bird<-matrix(NA,length(listMPA_restricted),reps)



for (i in 1: length(listMPA_restricted)) {# Pour chaque r??serve
  
  
  # Prendre les surveys de cette r??serve
  IDReserverestricte<-subset(routes3,routes3$namesMPA==listMPA_restricted[i])
  
  
  #Transect un protected
  IDTransect50km<-NULL 
  for (j in 1:dim(IDReserverestricte)[1]){
    
    if(dim(IDReserverestricte)[1]==1){
      distGeo<-data.frame(geodist.Bird[rownames(geodist.Bird)%in%rownames(IDReserverestricte),])
      rownames(distGeo)<-colnames(geodist.Bird)}
    
    else{
      distGeo<-t(data.frame(geodist.Bird[rownames(geodist.Bird)%in%rownames(IDReserverestricte),]))
      rownames(distGeo)<-colnames(geodist.Bird)
    }
    
    
    distGeo2<-subset(distGeo,distGeo[,j]<50) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDTransect50km<-c(IDTransect50km,distGeo2)
  }
  IDTransect50km<-unique(IDTransect50km)
  
  #Transect non protected
  IDsurvey50km_NP<- IDTransect50km[IDTransect50km%in%listMPA_nonprotect]
  
  
  #Calcul de la beta, du nombre d'esp partag?? et unique 
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (length(IDsurvey50km_NP) == 0){
      next
    }
    
    if (dim(IDReserverestricte)[1]>length(IDsurvey50km_NP)){
      
      #survey NP
      survey50km_NP<-Hab_all[rownames(Hab_all)%in%IDsurvey50km_NP,]
     
      
      #survey restricte
      names_restricte_Sample<-sample(IDReserverestricte$NewRouteID,length(IDsurvey50km_NP),replace=F)
      survey_restricte<-Hab_all[rownames(Hab_all)%in%names_restricte_Sample,]

      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_restricte, survey50km_NP)
      rownames(Compa)<-c(rownames(survey_restricte),rownames(survey50km_NP))
   
  
    }
    
    
    else {
      
      #survey NP
      names_NP_Sample<-sample(IDsurvey50km_NP,dim(IDReserverestricte)[1],replace=F)
      survey50km_NP<-Hab_all[rownames(Hab_all)%in%names_NP_Sample,]

      #survey restricte
      survey_restricte<-Hab_all[rownames(Hab_all)%in%IDReserverestricte$NewRouteID,]
  
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_restricte, survey50km_NP)
      rownames(Compa)<-c(rownames(survey_restricte),rownames(survey50km_NP))
   
      
    }
    
    Compa<-rbind(apply(Compa[1:dim(survey_restricte)[1],],2,mean,na.rm=T),apply(Compa[(dim(survey50km_NP)[1]+1):dim(Compa)[1],],2,mean,na.rm=T))
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habRA_NPA_TOT_50km_Bird[i,k] <- Dissim_hab[1]
    
    save(Dissim_habRA_NPA_TOT_50km_Bird,file="Dissim_habRA_NPA_TOT_50km_Bird.RData")
    
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}
