#--------------------------------------------------------------Library
library(rgdal)
library(betapart)
library(reshape)
library(gtools)
library(vegan)
library(parallel)
#--------------------------------------------------------------Load data
setwd("~/Documents/Postdoc MARBEC/BETA PROTECTED AREA_CLEAN/Fish/Reducedset/RLS")
RLS_data <- read.csv2("RLSdata.csv", sep=";",header =T)

df_fish <- data.frame(sp=RLS_data$CURRENT_SPECIES_NAME, SurveyID=RLS_data$SurveyID, Num=RLS_data$Abundance)
df_fish<-df_fish[-grep("sp.", df_fish$sp, ignore.case = TRUE),]

uniqueSP_RLS <- unique(df_fish$sp)

#--------------------------------------------------------------Transform data
#source("FromGPStoDISTANCE.R")
#df_geo_fish <- unique(data.frame(name = RLS_data$SurveyID,
#                          lat  = RLS_data$SiteLat,
#                          lon  =RLS_data$SiteLong))
#df_geo_fish[,2]<-as.numeric(as.character(df_geo_fish[,2]))
#df_geo_fish[,3]<-as.numeric(as.character(df_geo_fish[,3]))


#df_geo_fish<-GeoDistanceInMetresMatrix(df_geo_fish)
#df_geo_fish <- df_geo_fish/10000
#save(df_geo_fish, file= "df_geo_fish.RData")

load("df_geo_fish.RData")

#--------------------------------------------------------------Extract Name of MPA to group survey 
RLS_data$MPA.zone.type<- tolower(RLS_data$MPA.zone.type)


Info_Reserve <-data.frame(SurveyID=RLS_data$SurveyID,Graham_CAT=RLS_data$MPA.zone.type,NamesMPA=RLS_data$MPA)
Info_Reserve$MPA_CAT <- NA
for (i in 1:nrow(Info_Reserve)){
  if(Info_Reserve$Graham_CAT[i]=="no take (part of multi-zoned mpa)")  {Info_Reserve$MPA_CAT[i] <-"SPA"
  }   else if(Info_Reserve$Graham_CAT[i]=="no take (stand alone)")  {Info_Reserve$MPA_CAT[i] <-"SPA"
  }  else if(Info_Reserve$Graham_CAT[i]=="special management area (pre-existing sanctuary)")  {Info_Reserve$MPA_CAT[i] <-"SPA"
  }  else if(Info_Reserve$Graham_CAT[i]=="restricted (stand alone)") {Info_Reserve$MPA_CAT[i] <-"RA"
  }  else if(Info_Reserve$Graham_CAT[i]=="restricted (part of multizoned mpa)")  {Info_Reserve$MPA_CAT[i] <-"RA"
  }   else if (Info_Reserve$Graham_CAT[i]=="outside") {Info_Reserve$MPA_CAT[i] <-"NPA"}
}


RLS_data$MPA_CAT <- NA
for (i in 1:nrow(RLS_data)){
  if(RLS_data$MPA.zone.type[i]=="no take (part of multi-zoned mpa)")  {RLS_data$MPA_CAT[i] <-"SPA"
  }   else if(RLS_data$MPA.zone.type[i]=="no take (stand alone)")  {RLS_data$MPA_CAT[i] <-"SPA"
  }  else if(RLS_data$MPA.zone.type[i]=="special management area (pre-existing sanctuary)")  {RLS_data$MPA_CAT[i] <-"SPA"
  }  else if(RLS_data$MPA.zone.type[i]=="restricted (stand alone)") {RLS_data$MPA_CAT[i] <-"RA"
  }  else if(RLS_data$MPA.zone.type[i]=="restricted (part of multizoned mpa)")  {RLS_data$MPA_CAT[i] <-"RA"
  }   else if (RLS_data$MPA.zone.type[i]=="outside") {RLS_data$MPA_CAT[i] <-"NPA"}
}

listMPA_SPA<-subset(RLS_data,RLS_data$MPA_CAT=="SPA")
listMPA_SPA<-unique(listMPA_SPA$MPA)

listMPA_RA<-subset(RLS_data,RLS_data$MPA_CAT=="RA")
listMPA_RA2<-unique(listMPA_RA$SurveyID)
listMPA_RA<-unique(listMPA_RA$MPA)

listMPA_NPA<-unique(subset(RLS_data,RLS_data$MPA_CAT=="NPA")$SurveyID)

#--------------------------------------------------------------Prepare Environnemental data
Hab_data <- read.csv2("Habdata.csv", sep=";",header =T)
Hab_data  <- Hab_data[Hab_data$SurveyID %in% RLS_data$SurveyID,]

Habtransform <- reshape2::dcast(Hab_data, SurveyID ~ MajorCategory,value.var="Percent.Coverage",  fun.aggregate=sum)    # CATEGORY OR MAJOR CAT
depth <- unique(data.frame(SurveyID=Hab_data$SurveyID, Depth=Hab_data$Depth))
Habtransform <- merge(Habtransform,depth,by="SurveyID",all.x=T)
rownames(Habtransform) <- Habtransform[,1]
Habtransform<- Habtransform[,-1]

load("RLS_chla_sst.RData")

Hab_all <- merge(RLS_chla_sst,Habtransform,by.x="name",by.y="row.names",all.x=T) 
rownames(Hab_all) <- Hab_all[,1]
Hab_all <- Hab_all[,-c(1:3)]


mydata <- na.omit(Habtransform)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")




#####################SPA VS NPA############################
reps=200
core=3
scale=50 #50 or 50 or 500km

Dissim_habSPA_NPA_TOT_50km_RLS<-matrix(NA,length(listMPA_SPA),reps)
for (i in 1:length(listMPA_SPA)) {# For each mpa
  
  # Prendre les surveys de cette r??serve
  
  # Prendre les surveys de cette r??serve
  IDReserveSPA<-subset(RLS_data,RLS_data$MPA==listMPA_SPA[i])
  IDReserveSPA<-unique(subset(IDReserveSPA,IDReserveSPA$MPA_CAT=="SPA"))
  IDReserveSPA<-unique(IDReserveSPA[,c("MPA","SurveyID","MPA_CAT")])
  
  survey50km_SPA<- Hab_all[rownames(Hab_all)%in%IDReserveSPA$SurveyID,]
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
  survey50km_NPA<-   Hab_all[rownames(Hab_all)%in%IDsurvey50km_NPA,]

  
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
  
  survey50km_SPA<- Hab_all[rownames(Hab_all)%in%IDReserveSPA$SurveyID,]
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
  survey50km_RA<-   Hab_all[rownames(Hab_all)%in%IDsurvey50km_RA,]
  
  
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
  
  survey50km_RA<- Hab_all[rownames(Hab_all)%in%IDReserveRA$SurveyID,]
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
  survey50km_NPA<-   Hab_all[rownames(Hab_all)%in%IDsurvey50km_NPA,]
  
  
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

