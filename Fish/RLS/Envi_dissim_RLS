
load(Hab_all_RLS,file="Hab_all_RLS.RData")
#####################SPA VS NPA############################
reps=1000
core=3
scale=50 #50 or 50 or 500km

Dissim_habSPA_NPA_TOT_50km_RLS<-matrix(NA,length(listMPA_SPA),reps)
for (i in 1:length(listMPA_SPA)) {# For each mpa
  
  # Prendre les surveys de cette r??serve
  
  # Prendre les surveys de cette r??serve
  IDReserveSPA<-subset(RLS_data,RLS_data$MPA==listMPA_SPA[i])
  IDReserveSPA<-unique(subset(IDReserveSPA,IDReserveSPA$MPA_CAT=="SPA"))
  IDReserveSPA<-unique(IDReserveSPA[,c("MPA","SurveyID","MPA_CAT")])
  
  survey50km_SPA<- Hab_all_RLS[rownames(Hab_all_RLS)%in%IDReserveSPA$SurveyID,]
  if (nrow(survey50km_SPA)==0){next}   
  
  
  #survey uneprotected
  IDsurvey50km<-NULL 
  for (j in 1: dim(IDReserveSPA)[1]){
    
    if(dim(IDReserveSPA)[1]==1){#strucure of the dataframe is different if dim(survey_SPA)[1]==1.
      distGeo<-data.frame(df_geo_fish[rownames(df_geo_fish)%in%IDReserveSPA$SurveyID,])
      rownames(distGeo)<-colnames(df_geo_fish)}
    
    else{
      distGeo<-t(data.frame(df_geo_fish[rownames(df_geo_fish)%in%IDReserveSPA$SurveyID,]))
      rownames(distGeo)<-colnames(df_geo_fish)
    }
    distGeo2<-subset(distGeo,distGeo[,j]<scale) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDsurvey50km<-c(IDsurvey50km,distGeo2)
  }
  IDsurvey50km<-unique(IDsurvey50km)
  
  
  if (length(IDsurvey50km)==0){next}   
  
  #survey non protected
  IDsurvey50km_NPA<- IDsurvey50km[IDsurvey50km%in%listMPA_NPA]
  survey50km_NPA<-   Hab_all_RLS[rownames(Hab_all_RLS)%in%IDsurvey50km_NPA,]

  
  if (length(IDsurvey50km_NPA)==0){next}  
  
    #Calcul de la beta, du nombre d'esp partag?? et unique 
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (dim(IDReserveSPA)[1]>length(IDsurvey50km_NPA)){
      
      #survey SPA
      names_SPA_Sample<-sample(rownames(survey50km_SPA),dim(survey50km_NPA)[1],replace=F)
      survey50km_SPA<- survey50km_SPA[rownames(survey50km_SPA)%in%names_SPA_Sample,]
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey50km_SPA, survey50km_NPA)
      rownames(Compa)<-c(rownames(survey50km_SPA),rownames(survey50km_NPA))

          }else {
      
      #survey NPA
      names_NPA_Sample<-sample(rownames(survey50km_NPA),dim(survey50km_SPA)[1],replace=F)
      survey50km_NPA<- survey50km_NPA[rownames(survey50km_NPA)%in%names_NPA_Sample,]
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey50km_SPA, survey50km_NPA)
      rownames(Compa)<-c(rownames(survey50km_SPA),rownames(survey50km_NPA))

    }
    
    Compa<-rbind(apply(Compa[1:dim(survey50km_SPA)[1],],2,mean,na.rm=T),apply(Compa[(dim(survey50km_NPA)[1]+1):dim(Compa)[1],],2,mean,na.rm=T))
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habSPA_NPA_TOT_50km_RLS[i,k] <- Dissim_hab[1]

    save(Dissim_habSPA_NPA_TOT_50km_RLS,file="Dissim_habSPA_NPA_TOT_50km_RLS.RData")
    
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}






#####################SPA VS RA############################
reps=200
core=3
scale=50 #50 or 50 or 500km

Dissim_habSPA_RA_TOT_50km_RLS<-matrix(NA,length(listMPA_SPA),reps)
for (i in 1:length(listMPA_SPA)) {# For each mpa
  
  # Prendre les surveys de cette r??serve
  
  # Prendre les surveys de cette r??serve
  IDReserveSPA<-subset(RLS_data,RLS_data$MPA==listMPA_SPA[i])
  IDReserveSPA<-unique(subset(IDReserveSPA,IDReserveSPA$MPA_CAT=="SPA"))
  IDReserveSPA<-unique(IDReserveSPA[,c("MPA","SurveyID","MPA_CAT")])
  
  survey50km_SPA<- Hab_all_RLS[rownames(Hab_all_RLS)%in%IDReserveSPA$SurveyID,]
  if (nrow(survey50km_SPA)==0){next}   
  
  
  #survey uneprotected
  IDsurvey50km<-NULL 
  for (j in 1: dim(IDReserveSPA)[1]){
    
    if(dim(IDReserveSPA)[1]==1){#strucure of the dataframe is different if dim(survey_SPA)[1]==1.
      distGeo<-data.frame(df_geo_fish[rownames(df_geo_fish)%in%IDReserveSPA$SurveyID,])
      rownames(distGeo)<-colnames(df_geo_fish)}
    
    else{
      distGeo<-t(data.frame(df_geo_fish[rownames(df_geo_fish)%in%IDReserveSPA$SurveyID,]))
      rownames(distGeo)<-colnames(df_geo_fish)
    }
    distGeo2<-subset(distGeo,distGeo[,j]<scale) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDsurvey50km<-c(IDsurvey50km,distGeo2)
  }
  IDsurvey50km<-unique(IDsurvey50km)
  
  
  if (length(IDsurvey50km)==0){next}   
  
  #survey non protected
  IDsurvey50km_RA<- IDsurvey50km[IDsurvey50km%in%listMPA_RA2]
  survey50km_RA<-   Hab_all_RLS[rownames(Hab_all_RLS)%in%IDsurvey50km_RA,]
  
  
  if (length(IDsurvey50km_RA)==0){next}  
  
  #Calcul de la beta, du nombre d'esp partag?? et unique 
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (dim(IDReserveSPA)[1]>length(IDsurvey50km_RA)){
      
      #survey SPA
      names_SPA_Sample<-sample(rownames(survey50km_SPA),dim(survey50km_RA)[1],replace=F)
      survey50km_SPA<- survey50km_SPA[rownames(survey50km_SPA)%in%names_SPA_Sample,]
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey50km_SPA, survey50km_RA)
      rownames(Compa)<-c(rownames(survey50km_SPA),rownames(survey50km_RA))
      
    }else {
      
      #survey RA
      names_RA_Sample<-sample(rownames(survey50km_RA),dim(survey50km_SPA)[1],replace=F)
      survey50km_RA<- survey50km_RA[rownames(survey50km_RA)%in%names_RA_Sample,]
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey50km_SPA, survey50km_RA)
      rownames(Compa)<-c(rownames(survey50km_SPA),rownames(survey50km_RA))
      
    }
    
    Compa<-rbind(apply(Compa[1:dim(survey50km_SPA)[1],],2,mean,na.rm=T),apply(Compa[(dim(survey50km_RA)[1]+1):dim(Compa)[1],],2,mean,na.rm=T))
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habSPA_RA_TOT_50km_RLS[i,k] <- Dissim_hab[1]
    
    save(Dissim_habSPA_RA_TOT_50km_RLS,file="Dissim_habSPA_RA_TOT_50km_RLS.RData")
    
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}








#####################RA VS NPA############################


Dissim_habRA_NPA_TOT_50km_RLS<-matrix(NA,length(listMPA_RA),reps)
for (i in 1:length(listMPA_RA)) {# For each mpa
  
  # Prendre les surveys de cette r??serve
  
  # Prendre les surveys de cette r??serve
  IDReserveRA<-subset(RLS_data,RLS_data$MPA==listMPA_RA[i])
  IDReserveRA<-unique(subset(IDReserveRA,IDReserveRA$MPA_CAT=="RA"))
  IDReserveRA<-unique(IDReserveRA[,c("MPA","SurveyID","MPA_CAT")])
  
  survey50km_RA<- Hab_all_RLS[rownames(Hab_all_RLS)%in%IDReserveRA$SurveyID,]
  if (nrow(survey50km_RA)==0){next}   
  
  
  #survey uneprotected
  IDsurvey50km<-NULL 
  for (j in 1: dim(IDReserveRA)[1]){
    
    if(dim(IDReserveRA)[1]==1){#strucure of the dataframe is different if dim(survey_RA)[1]==1.
      distGeo<-data.frame(df_geo_fish[rownames(df_geo_fish)%in%IDReserveRA$SurveyID,])
      rownames(distGeo)<-colnames(df_geo_fish)}
    
    else{
      distGeo<-t(data.frame(df_geo_fish[rownames(df_geo_fish)%in%IDReserveRA$SurveyID,]))
      rownames(distGeo)<-colnames(df_geo_fish)
    }
    distGeo2<-subset(distGeo,distGeo[,j]<scale) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDsurvey50km<-c(IDsurvey50km,distGeo2)
  }
  IDsurvey50km<-unique(IDsurvey50km)
  
  
  if (length(IDsurvey50km)==0){next}   
  
  #survey non protected
  IDsurvey50km_NPA<- IDsurvey50km[IDsurvey50km%in%listMPA_NPA]
  survey50km_NPA<-   Hab_all_RLS[rownames(Hab_all_RLS)%in%IDsurvey50km_NPA,]
  
  
  if (length(IDsurvey50km_NPA)==0){next}  
  
  #Calcul de la beta, du nombre d'esp partag?? et unique 
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (dim(IDReserveRA)[1]>length(IDsurvey50km_NPA)){
      
      #survey RA
      names_RA_Sample<-sample(rownames(survey50km_RA),dim(survey50km_NPA)[1],replace=F)
      survey50km_RA<- survey50km_RA[rownames(survey50km_RA)%in%names_RA_Sample,]
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey50km_RA, survey50km_NPA)
      rownames(Compa)<-c(rownames(survey50km_RA),rownames(survey50km_NPA))
      
    }else {
      
      #survey NPA
      names_NPA_Sample<-sample(rownames(survey50km_NPA),dim(survey50km_RA)[1],replace=F)
      survey50km_NPA<- survey50km_NPA[rownames(survey50km_NPA)%in%names_NPA_Sample,]
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey50km_RA, survey50km_NPA)
      rownames(Compa)<-c(rownames(survey50km_RA),rownames(survey50km_NPA))
      
    }
    
    Compa<-rbind(apply(Compa[1:dim(survey50km_RA)[1],],2,mean,na.rm=T),apply(Compa[(dim(survey50km_NPA)[1]+1):dim(Compa)[1],],2,mean,na.rm=T))
    Dissim_hab <- daisy(Compa, metric = "euclidean")
    
    Dissim_habRA_NPA_TOT_50km_RLS[i,k] <- Dissim_hab[1]
    
    save(Dissim_habRA_NPA_TOT_50km_RLS,file="Dissim_habRA_NPA_TOT_50km_RLS.RData")
    
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}

