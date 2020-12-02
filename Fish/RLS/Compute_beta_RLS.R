###########################################################################################################
# Analyse beta diversity between MPA and non MPA
#
# Author : Nicolas Loiseau, 
# R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
# Copyright (C) 2016 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
#
###########################################################################################################


#--------------------------------------------------------------Library
library(rgdal)
library(betapart)
library(reshape)
library(gtools)
library(vegan)
library(parallel)
#--------------------------------------------------------------Load data


load("df_geo_fish.RData")
load("RLS_data.RData")
df_fish <- data.frame(sp=RLS_data$CURRENT_SPECIES_NAME, SurveyID=RLS_data$SurveyID, Num=RLS_data$Abundance)
df_fish<-df_fish[-grep("sp.", df_fish$sp, ignore.case = TRUE),]


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

#--------------------------------------------------------------Compute Beta and Null Model
#####################SPA VS NPA############################

reps=1000
core=30
scale=50 #10 or 50 or 100km

Fish_BetaSPA_NPA_TOT_50km<-matrix(NA,length(listMPA_SPA),reps)
Fish_BetaSPA_NPA_TUR_50km<-Fish_BetaSPA_NPA_TOT_50km
Fish_BetaSPA_NPA_NES_50km<-Fish_BetaSPA_NPA_TOT_50km

Fish_S_50km_Unique_SPA.SPA_NPA<-matrix(NA,length(listMPA_SPA),reps)
Fish_S_50km_Unique_NPA.SPA_NPA<-Fish_S_50km_Unique_SPA.SPA_NPA
Fish_S_50km_Shared_SPA_NPA.SPA_NPA<-Fish_S_50km_Unique_SPA.SPA_NPA

ses.tot_50km_Fish_SPA_NPA<-Fish_BetaSPA_NPA_TOT_50km
ses.tur_50km_Fish_SPA_NPA<-Fish_BetaSPA_NPA_TOT_50km
ses.nes_50km_Fish_SPA_NPA<-Fish_BetaSPA_NPA_TOT_50km



for (i in 1: length(listMPA_SPA)) {# For each mpa
  
  #survey SPA
  IDReserveSPA<-subset(RLS_data,RLS_data$MPA==listMPA_SPA[i])
  IDReserveSPA<-unique(subset(IDReserveSPA,IDReserveSPA$MPA_CAT=="SPA"))
  IDReserveSPA<-unique(IDReserveSPA[,c("MPA","SurveyID","MPA_CAT")])
  
 
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
  if (length(IDsurvey50km_NPA)==0){next}  
  
  #Beta
  for (k in 1:reps){ #Bootstrap due to different sampling effort
 
    if (dim(IDReserveSPA)[1]>length(IDsurvey50km_NPA)){
      
      #survey NPA
      survey50km_NPA<-df_fish[df_fish$SurveyID%in%IDsurvey50km_NPA,]
      survey50km_NPA <- dcast(survey50km_NPA, SurveyID ~ sp,value.var="Num",  fun.aggregate=sum)
      rownames(survey50km_NPA)<-survey50km_NPA[,1]
      survey50km_NPA<-survey50km_NPA[,-1]      
      
      #survey SPA
      names_SPA_Sample<-sample(IDReserveSPA$SurveyID,length(IDsurvey50km_NPA),replace=F)
      survey_SPA<-df_fish[df_fish$SurveyID%in%names_SPA_Sample,]
      survey_SPA <- dcast(survey_SPA, SurveyID ~ sp,value.var="Num",  fun.aggregate=sum)
      rownames(survey_SPA)<-survey_SPA[,1]
      survey_SPA<-survey_SPA[,-1]
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_SPA, survey50km_NPA)
      rownames(Compa)<-c(rownames(survey_SPA),rownames(survey50km_NPA))
      Compa[is.na(Compa)]<-0
      Compa<-Compa[,apply(Compa,2,sum)>0]
      
      #create random matrices
      Compa_NULL<-Compa
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      if(min(c(dim(survey_SPA)[1],dim(survey50km_NPA)[1]))==1){
        random_pool<-mclapply(1:reps,function(e){ rbind(random_mat[,,e][1:dim(survey_SPA)[1],],random_mat[,,e][(dim(survey50km_NPA)[1]+1):dim(Compa_NULL)[1],])},mc.cores=core)
      }else{ 
       random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(survey_SPA)[1],],2,sum),apply(random_mat[,,e][(dim(survey50km_NPA)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)}
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    
    else {
      
      #survey NPA
      names_NPA_Sample<-sample(IDsurvey50km_NPA,dim(IDReserveSPA)[1],replace=F)
      survey50km_NPA<-df_fish[df_fish$SurveyID%in%names_NPA_Sample,]
      survey50km_NPA <- dcast(survey50km_NPA, SurveyID ~ sp,value.var="Num",  fun.aggregate=sum)
      rownames(survey50km_NPA)<-survey50km_NPA[,1]
      survey50km_NPA<-survey50km_NPA[,-1]      
      
      #survey SPA
      survey_SPA<-df_fish[df_fish$SurveyID%in%IDReserveSPA$SurveyID,]
      survey_SPA <- dcast(survey_SPA, SurveyID ~ sp,value.var="Num",  fun.aggregate=sum)
      rownames(survey_SPA)<-survey_SPA[,1]
      survey_SPA<-survey_SPA[,-1]
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_SPA, survey50km_NPA)
      rownames(Compa)<-c(rownames(survey_SPA),rownames(survey50km_NPA))
      Compa[is.na(Compa)]<-0
      Compa<-Compa[,apply(Compa,2,sum)>0]
      
      #create random matrices
      Compa_NULL<-Compa
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      if(min(c(dim(survey_SPA)[1],dim(survey50km_NPA)[1]))==1){
        random_pool<-mclapply(1:reps,function(e){ rbind(random_mat[,,e][1:dim(survey_SPA)[1],],random_mat[,,e][(dim(survey50km_NPA)[1]+1):dim(Compa_NULL)[1],])},mc.cores=core)
      }else{ 
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(survey_SPA)[1],],2,sum),apply(random_mat[,,e][(dim(survey50km_NPA)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)}
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    Compa<-rbind(apply(Compa[1:dim(survey_SPA)[1],],2,sum),apply(Compa[(dim(survey50km_NPA)[1]+1):dim(Compa)[1],],2,sum))
    Compa[Compa>0]<-1
    BETAX<-beta.pair(Compa, index.family="jaccard")
    
    Fish_BetaSPA_NPA_TOT_50km[i,k]<-BETAX$beta.jac
    Fish_BetaSPA_NPA_TUR_50km[i,k]<-BETAX$beta.jtu
    Fish_BetaSPA_NPA_NES_50km[i,k]<-BETAX$beta.jne

    Fish_S_50km_Unique_SPA.SPA_NPA[i,k]<-dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2]
    Fish_S_50km_Unique_NPA.SPA_NPA[i,k]<-dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2]
    Fish_S_50km_Shared_SPA_NPA.SPA_NPA[i,k]<-dim(Compa[,apply(Compa,2,sum)==2])[2]
    
    nullbeta <- t( do.call(rbind.data.frame, beta_null))
    
    # SES=(obs-mean(rand))/sd(rand)
    ses.tot_50km_Fish_SPA_NPA[i,k] = (BETAX$beta.jac - mean(nullbeta[3,]))/sd(nullbeta[3,])
    ses.tur_50km_Fish_SPA_NPA[i,k] = (BETAX$beta.jtu - mean(nullbeta[1,]))/sd(nullbeta[1,])
    ses.nes_50km_Fish_SPA_NPA[i,k] = (BETAX$beta.jne - mean(nullbeta[2,]))/sd(nullbeta[2,])
    
    save(Fish_BetaSPA_NPA_TOT_50km,file="Fish_BetaSPA_NPA_TOT_50km.RData")
    save(Fish_BetaSPA_NPA_TUR_50km,file="Fish_BetaSPA_NPA_TUR_50km.RData")
    save(Fish_BetaSPA_NPA_NES_50km,file="Fish_BetaSPA_NPA_NES_50km.RData")
   
    save(Fish_S_50km_Unique_SPA.SPA_NPA,file="Fish_S_50km_Unique_SPA.SPA_NPA.RData")
    save(Fish_S_50km_Unique_NPA.SPA_NPA,file="Fish_S_50km_Unique_NPA.SPA_NPA.RData")
    save(Fish_S_50km_Shared_SPA_NPA.SPA_NPA,file="Fish_S_50km_Shared_SPA_NPA.SPA_NPA.RData")
   
    save(ses.tot_50km_Fish_SPA_NPA,file="ses.tot_50km_Fish_SPA_NPA.RData")
    save(ses.tur_50km_Fish_SPA_NPA,file="ses.tur_50km_Fish_SPA_NPA.RData")
    save(ses.nes_50km_Fish_SPA_NPA,file="ses.nes_50km_Fish_SPA_NPA.RData")

    
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}







#####################SPA VS RA############################


Fish_BetaSPA_RA_TOT_50km<-matrix(NA,length(listMPA_SPA),reps)
Fish_BetaSPA_RA_TUR_50km<-Fish_BetaSPA_RA_TOT_50km
Fish_BetaSPA_RA_NES_50km<-Fish_BetaSPA_RA_TOT_50km

Fish_S_50km_Unique_SPA.SPA_RA<-matrix(NA,length(listMPA_SPA),reps)
Fish_S_50km_Unique_RA.SPA_RA<-Fish_S_50km_Unique_SPA.SPA_RA
Fish_S_50km_Shared_SPA_RA.SPA_RA<-Fish_S_50km_Unique_SPA.SPA_RA

ses.tot_50km_Fish_SPA_RA<-Fish_BetaSPA_RA_TOT_50km
ses.tur_50km_Fish_SPA_RA<-Fish_BetaSPA_RA_TOT_50km
ses.nes_50km_Fish_SPA_RA<-Fish_BetaSPA_RA_TOT_50km

for (i in 1: length(listMPA_SPA)) {# For each mpa
  
  #survey SPA
  IDReserveSPA<-subset(RLS_data,RLS_data$MPA==listMPA_SPA[i])
  IDReserveSPA<-unique(subset(IDReserveSPA,IDReserveSPA$MPA_CAT=="SPA"))
  IDReserveSPA<-unique(IDReserveSPA[,c("MPA","SurveyID","MPA_CAT")])
  
  
  #survey non-protected
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
  
  #survey RA
  IDsurvey50km_RA<- IDsurvey50km[IDsurvey50km%in%listMPA_RA2]
  if (length(IDsurvey50km_RA)==0){next}  
  
  #Beta
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (dim(IDReserveSPA)[1]>length(IDsurvey50km_RA)){
      
      #survey RA
      survey50km_RA<-df_fish[df_fish$SurveyID%in%IDsurvey50km_RA,]
      survey50km_RA <- dcast(survey50km_RA, SurveyID ~ sp,value.var="Num",  fun.aggregate=sum)
      rownames(survey50km_RA)<-survey50km_RA[,1]
      survey50km_RA<-survey50km_RA[,-1]      
      
      #survey SPA
      names_SPA_Sample<-sample(IDReserveSPA$SurveyID,length(IDsurvey50km_RA),replace=F)
      survey_SPA<-df_fish[df_fish$SurveyID%in%names_SPA_Sample,]
      survey_SPA <- dcast(survey_SPA, SurveyID ~ sp,value.var="Num",  fun.aggregate=sum)
      rownames(survey_SPA)<-survey_SPA[,1]
      survey_SPA<-survey_SPA[,-1]
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_SPA, survey50km_RA)
      rownames(Compa)<-c(rownames(survey_SPA),rownames(survey50km_RA))
      Compa[is.na(Compa)]<-0
      Compa<-Compa[,apply(Compa,2,sum)>0]
      
      #create random matrices
      Compa_NULL<-Compa
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      if(min(c(dim(survey_SPA)[1],dim(survey50km_RA)[1]))==1){
        random_pool<-mclapply(1:reps,function(e){ rbind(random_mat[,,e][1:dim(survey_SPA)[1],],random_mat[,,e][(dim(survey50km_RA)[1]+1):dim(Compa_NULL)[1],])},mc.cores=core)
      }else{ 
        random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(survey_SPA)[1],],2,sum),apply(random_mat[,,e][(dim(survey50km_RA)[1]+1):dim(Compa_NULL)[1],],2,sum))
        },mc.cores=core)}
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    
    else {
      
      #survey RA
      names_RA_Sample<-sample(IDsurvey50km_RA,dim(IDReserveSPA)[1],replace=F)
      survey50km_RA<-df_fish[df_fish$SurveyID%in%names_RA_Sample,]
      survey50km_RA <- dcast(survey50km_RA, SurveyID ~ sp,value.var="Num",  fun.aggregate=sum)
      rownames(survey50km_RA)<-survey50km_RA[,1]
      survey50km_RA<-survey50km_RA[,-1]      
      
      #survey SPA
      survey_SPA<-df_fish[df_fish$SurveyID%in%IDReserveSPA$SurveyID,]
      survey_SPA <- dcast(survey_SPA, SurveyID ~ sp,value.var="Num",  fun.aggregate=sum)
      rownames(survey_SPA)<-survey_SPA[,1]
      survey_SPA<-survey_SPA[,-1]
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_SPA, survey50km_RA)
      rownames(Compa)<-c(rownames(survey_SPA),rownames(survey50km_RA))
      Compa[is.na(Compa)]<-0
      Compa<-Compa[,apply(Compa,2,sum)>0]
      
      #create random matrices
      Compa_NULL<-Compa
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      if(min(c(dim(survey_SPA)[1],dim(survey50km_RA)[1]))==1){
        random_pool<-mclapply(1:reps,function(e){ rbind(random_mat[,,e][1:dim(survey_SPA)[1],],random_mat[,,e][(dim(survey50km_RA)[1]+1):dim(Compa_NULL)[1],])},mc.cores=core)
      }else{ 
        random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(survey_SPA)[1],],2,sum),apply(random_mat[,,e][(dim(survey50km_RA)[1]+1):dim(Compa_NULL)[1],],2,sum))
        },mc.cores=core)}
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    Compa<-rbind(apply(Compa[1:dim(survey_SPA)[1],],2,sum),apply(Compa[(dim(survey50km_RA)[1]+1):dim(Compa)[1],],2,sum))
    Compa[Compa>0]<-1
    BETAX<-beta.pair(Compa, index.family="jaccard")
    
    Fish_BetaSPA_RA_TOT_50km[i,k]<-BETAX$beta.jac
    Fish_BetaSPA_RA_TUR_50km[i,k]<-BETAX$beta.jtu
    Fish_BetaSPA_RA_NES_50km[i,k]<-BETAX$beta.jne
    
    Fish_S_50km_Unique_SPA.SPA_RA[i,k]<-dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2]
    Fish_S_50km_Unique_RA.SPA_RA[i,k]<-dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2]
    Fish_S_50km_Shared_SPA_RA.SPA_RA[i,k]<-dim(Compa[,apply(Compa,2,sum)==2])[2]
    
    nullbeta <- t( do.call(rbind.data.frame, beta_null))
    
    # SES=(obs-mean(rand))/sd(rand)
    ses.tot_50km_Fish_SPA_RA[i,k] = (BETAX$beta.jac - mean(nullbeta[3,]))/sd(nullbeta[3,])
    ses.tur_50km_Fish_SPA_RA[i,k] = (BETAX$beta.jtu - mean(nullbeta[1,]))/sd(nullbeta[1,])
    ses.nes_50km_Fish_SPA_RA[i,k] = (BETAX$beta.jne - mean(nullbeta[2,]))/sd(nullbeta[2,])

    save(Fish_BetaSPA_RA_TOT_50km,file="Fish_BetaSPA_RA_TOT_50km.RData")
    save(Fish_BetaSPA_RA_TUR_50km,file="Fish_BetaSPA_RA_TUR_50km.RData")
    save(Fish_BetaSPA_RA_NES_50km,file="Fish_BetaSPA_RA_NES_50km.RData")
    
    save(Fish_S_50km_Unique_SPA.SPA_RA,file="Fish_S_50km_Unique_SPA.SPA_RA.RData")
    save(Fish_S_50km_Unique_RA.SPA_RA,file="Fish_S_50km_Unique_RA.SPA_RA.RData")
    save(Fish_S_50km_Shared_SPA_RA.SPA_RA,file="Fish_S_50km_Shared_SPA_RA.SPA_RA.RData")
    
    save(ses.tot_50km_Fish_SPA_RA,file="ses.tot_50km_Fish_SPA_RA.RData")
    save(ses.tur_50km_Fish_SPA_RA,file="ses.tur_50km_Fish_SPA_RA.RData")
    save(ses.nes_50km_Fish_SPA_RA,file="ses.nes_50km_Fish_SPA_RA.RData")
    
    
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}









#####################RA VS NPA############################

Fish_BetaRA_NPA_TOT_50km<-matrix(NA,length(listMPA_RA),reps)
Fish_BetaRA_NPA_TUR_50km<-Fish_BetaRA_NPA_TOT_50km
Fish_BetaRA_NPA_NES_50km<-Fish_BetaRA_NPA_TOT_50km

Fish_S_50km_Unique_RA.RA_NPA<-matrix(NA,length(listMPA_RA),reps)
Fish_S_50km_Unique_NPA.RA_NPA<-Fish_S_50km_Unique_RA.RA_NPA
Fish_S_50km_Shared_RA_NPA.RA_NPA<-Fish_S_50km_Unique_RA.RA_NPA

ses.tot_50km_Fish_RA_NPA<-Fish_BetaRA_NPA_TOT_50km
ses.tur_50km_Fish_RA_NPA<-Fish_BetaRA_NPA_TOT_50km
ses.nes_50km_Fish_RA_NPA<-Fish_BetaRA_NPA_TOT_50km


for (i in 1: length(listMPA_RA)) {# For each mpa
  
  #survey RA
  IDReserveRA<-subset(RLS_data,RLS_data$MPA==listMPA_RA[i])
  IDReserveRA<-unique(subset(IDReserveRA,IDReserveRA$MPA_CAT=="RA"))
  IDReserveRA<-unique(IDReserveRA[,c("MPA","SurveyID","MPA_CAT")])
  
  #survey non-protected
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
  if (length(IDsurvey50km_NPA)==0){next}  
  #Beta
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (dim(IDReserveRA)[1]>length(IDsurvey50km_NPA)){
      
      #survey NPA
      survey50km_NPA<-df_fish[df_fish$SurveyID%in%IDsurvey50km_NPA,]
      survey50km_NPA <- dcast(survey50km_NPA, SurveyID ~ sp,value.var="Num",  fun.aggregate=sum)
      rownames(survey50km_NPA)<-survey50km_NPA[,1]
      survey50km_NPA<-survey50km_NPA[,-1]      
      
      #survey RA
      names_RA_Sample<-sample(IDReserveRA$SurveyID,length(IDsurvey50km_NPA),replace=F)
      survey_RA<-df_fish[df_fish$SurveyID%in%names_RA_Sample,]
      survey_RA <- dcast(survey_RA, SurveyID ~ sp,value.var="Num",  fun.aggregate=sum)
      rownames(survey_RA)<-survey_RA[,1]
      survey_RA<-survey_RA[,-1]
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_RA, survey50km_NPA)
      rownames(Compa)<-c(rownames(survey_RA),rownames(survey50km_NPA))
      Compa[is.na(Compa)]<-0
      Compa<-Compa[,apply(Compa,2,sum)>0]
      
      #create random matrices
      Compa_NULL<-Compa
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      if(min(c(dim(survey_RA)[1],dim(survey50km_NPA)[1]))==1){
        random_pool<-mclapply(1:reps,function(e){ rbind(random_mat[,,e][1:dim(survey_RA)[1],],random_mat[,,e][(dim(survey50km_NPA)[1]+1):dim(Compa_NULL)[1],])},mc.cores=core)
      }else{ 
        random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(survey_RA)[1],],2,sum),apply(random_mat[,,e][(dim(survey50km_NPA)[1]+1):dim(Compa_NULL)[1],],2,sum))
        },mc.cores=core)}
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    
    else {
      
      #survey NPA
      names_NPA_Sample<-sample(IDsurvey50km_NPA,dim(IDReserveRA)[1],replace=F)
      survey50km_NPA<-df_fish[df_fish$SurveyID%in%names_NPA_Sample,]
      survey50km_NPA <- dcast(survey50km_NPA, SurveyID ~ sp,value.var="Num",  fun.aggregate=sum)
      rownames(survey50km_NPA)<-survey50km_NPA[,1]
      survey50km_NPA<-survey50km_NPA[,-1]      
      
      #survey RA
      survey_RA<-df_fish[df_fish$SurveyID%in%IDReserveRA$SurveyID,]
      survey_RA <- dcast(survey_RA, SurveyID ~ sp,value.var="Num",  fun.aggregate=sum)
      rownames(survey_RA)<-survey_RA[,1]
      survey_RA<-survey_RA[,-1]
      
      #Merge matrix for NULL MODEL
      Compa<-smartbind(survey_RA, survey50km_NPA)
      rownames(Compa)<-c(rownames(survey_RA),rownames(survey50km_NPA))
      Compa[is.na(Compa)]<-0
      Compa<-Compa[,apply(Compa,2,sum)>0]
      
      #create random matrices
      Compa_NULL<-Compa
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      if(min(c(dim(survey_RA)[1],dim(survey50km_NPA)[1]))==1){
        random_pool<-mclapply(1:reps,function(e){ rbind(random_mat[,,e][1:dim(survey_RA)[1],],random_mat[,,e][(dim(survey50km_NPA)[1]+1):dim(Compa_NULL)[1],])},mc.cores=core)
      }else{ 
        random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(survey_RA)[1],],2,sum),apply(random_mat[,,e][(dim(survey50km_NPA)[1]+1):dim(Compa_NULL)[1],],2,sum))
        },mc.cores=core)}
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    Compa<-rbind(apply(Compa[1:dim(survey_RA)[1],],2,sum),apply(Compa[(dim(survey50km_NPA)[1]+1):dim(Compa)[1],],2,sum))
    Compa[Compa>0]<-1
    BETAX<-beta.pair(Compa, index.family="jaccard")
    
    Fish_BetaRA_NPA_TOT_50km[i,k]<-BETAX$beta.jac
    Fish_BetaRA_NPA_TUR_50km[i,k]<-BETAX$beta.jtu
    Fish_BetaRA_NPA_NES_50km[i,k]<-BETAX$beta.jne
    
    Fish_S_50km_Unique_RA.RA_NPA[i,k]<-dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2]
    Fish_S_50km_Unique_NPA.RA_NPA[i,k]<-dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2]
    Fish_S_50km_Shared_RA_NPA.RA_NPA[i,k]<-dim(Compa[,apply(Compa,2,sum)==2])[2]
    
    
    nullbeta <- t( do.call(rbind.data.frame, beta_null))
    
    # SES=(obs-mean(rand))/sd(rand)
    ses.tot_50km_Fish_RA_NPA[i,k] = (BETAX$beta.jac - mean(nullbeta[3,]))/sd(nullbeta[3,])
    ses.tur_50km_Fish_RA_NPA[i,k] = (BETAX$beta.jtu - mean(nullbeta[1,]))/sd(nullbeta[1,])
    ses.nes_50km_Fish_RA_NPA[i,k] = (BETAX$beta.jne - mean(nullbeta[2,]))/sd(nullbeta[2,])
    

    save(Fish_BetaRA_NPA_TOT_50km,file="Fish_BetaRA_NPA_TOT_50km.RData")
    save(Fish_BetaRA_NPA_TUR_50km,file="Fish_BetaRA_NPA_TUR_50km.RData")
    save(Fish_BetaRA_NPA_NES_50km,file="Fish_BetaRA_NPA_NES_50km.RData")
    
    save(Fish_S_50km_Unique_RA.RA_NPA,file="Fish_S_50km_Unique_RA.RA_NPA.RData")
    save(Fish_S_50km_Unique_NPA.RA_NPA,file="Fish_S_50km_Unique_NPA.RA_NPA.RData")
    save(Fish_S_50km_Shared_RA_NPA.RA_NPA,file="Fish_S_50km_Shared_RA_NPA.RA_NPA.RData")
    
    save(ses.tot_50km_Fish_RA_NPA,file="ses.tot_50km_Fish_RA_NPA.RData")
    save(ses.tur_50km_Fish_RA_NPA,file="ses.tur_50km_Fish_RA_NPA.RData")
    save(ses.nes_50km_Fish_RA_NPA,file="ses.nes_50km_Fish_RA_NPA.RData")
    
    
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}


