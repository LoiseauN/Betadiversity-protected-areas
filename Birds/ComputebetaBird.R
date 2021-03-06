###########################################################################################################
# Computing Beta diversity BIRD
#
# Author : Nicolas Loiseau, 
# 
#R version 3.4.1 (2017-06-30) -- "Single Candle"
#Copyright (C) 2017 The R Foundation for Statistical Computing
#Platform: x86_64-apple-darwin15.6.0 (64-bit)
#
###########################################################################################################
`%notin%` <- Negate(`%in%`)
########################################################################
# Load data on Birds Life Surveys
########################################################################

load("geodist.Bird.RData")
load("info_survey.RData")
#Data are cut to reduce size of files for github
load("df_bird1.RData")
load("df_bird2.RData")
load("df_bird3.RData")

load("geodist.Bird1.RData")
load("geodist.Bird2.RData")
load("geodist.Bird3.RData")
load("geodist.Bird4.RData")

df_bird <- rbind(df_bird1,df_bird2,df_bird3)
geodist.Bird <- rbind(geodist.Bird1,geodist.Bird2,geodist.Bird3,geodist.Bird4)



listMPA_SPA<-rbind(subset(info_survey,info_survey$Access=="Closed"), subset(info_survey,info_survey$Access=="Restricted Access")
                     ,subset(info_survey,info_survey$IUCN_CAT=="Ia"),subset(info_survey,info_survey$IUCN_CAT=="Ib"))
listMPA_SPA<-na.omit(unique(listMPA_SPA$namesMPA))

listMPA_RA<-subset(info_survey,info_survey$Access=="Open Access")
listMPA_RA<-listMPA_restricted[listMPA_RA%notin%listMPA_SPA]
listMPA_RA2<-unique(rownames(listMPA_RA))
listMPA_RA<-unique(listMPA_RA$namesMPA)


listMPA_NPA<-unique(subset(info_survey,is.na(info_survey$Access))$NewRouteID)


reps=1000
core=50

#PA VS NPA

Bird_BetaSPA_NPA_TOT_50km<-matrix(NA,length(listMPA_SPA),reps)
Bird_BetaSPA_NPA_TUR_50km<-Bird_BetaSPA_NPA_TOT_50km
Bird_BetaSPA_NPA_NES_50km<-Bird_BetaSPA_NPA_TOT_50km

Bird_S_50km_Unique_SPA.SPA_NPA<-matrix(NA,length(listMPA_SPA),reps)
Bird_S_50km_Unique_NPA.SPA_NPA<-Bird_S_50km_Unique_SPA.SPA_NPA
Bird_S_50km_Shared_SPA_NPA.SPA_NPA<-Bird_S_50km_Unique_SPA.SPA_NPA

ses.tot_50km_Bird_SPA_NPA<-Bird_BetaSPA_NPA_TOT_50km
ses.tur_50km_Bird_SPA_NPA<-Bird_BetaSPA_NPA_TOT_50km
ses.nes_50km_Bird_SPA_NPA<-Bird_BetaSPA_NPA_TOT_50km

for (i in 1: length(listMPA_SPA)) {# For each PA
  
  #survey protected
  IDReserveSPA<-subset(info_survey,info_survey$namesMPA==listMPA_SPA[i])
  
  IDsurvey50km<-NULL 
  for (j in 1: dim(IDReserveSPA)[1]){
    
    if(dim(IDReserveSPA)[1]==1){#strucure of the dataframe is different if dim(survey_SPA)[1]==1.
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
  
  #Compute BEta
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (length(IDsurvey50km_NPA) == 0){
      next
    }
    
    if (dim(IDReserveSPA)[1]>length(IDsurvey50km_NPA)){
      
      #survey NPA
      survey50km_NPA<-df_bird[df_bird$NewRouteID%in%IDsurvey50km_NPA,]
      survey50km_NPA <- dcast(survey50km_NPA, IDcomplete ~ AOU,value.var="Abun",  fun.aggregate=sum)
      rownames(survey50km_NPA)<-survey50km_NPA[,1]
      survey50km_NPA<-survey50km_NPA[,-1]      
      
      #survey SPA
      names_SPA_Sample<-sample(IDReserveSPA$NewRouteID,length(IDsurvey50km_NPA),replace=F)
      survey_SPA<-df_bird[df_bird$NewRouteID%in%names_SPA_Sample,]
      survey_SPA <- dcast(survey_SPA, IDcomplete ~ AOU,value.var="Abun",  fun.aggregate=sum)
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
      ##create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(survey_SPA)[1],],2,sum),apply(random_mat[,,e][(dim(survey50km_NPA)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    
    else {
      
      #survey NPA
      names_NPA_Sample<-sample(IDsurvey50km_NPA,dim(IDReserveSPA)[1],replace=F)
      survey50km_NPA<-df_bird[df_bird$NewRouteID%in%names_NPA_Sample,]
      survey50km_NPA <- dcast(survey50km_NPA, IDcomplete ~ AOU,value.var="Abun",  fun.aggregate=sum)
      rownames(survey50km_NPA)<-survey50km_NPA[,1]
      survey50km_NPA<-survey50km_NPA[,-1]      
      
      #survey SPA
      survey_SPA<-df_bird[df_bird$NewRouteID%in%IDReserveSPA$NewRouteID,]
      survey_SPA <- dcast(survey_SPA, IDcomplete ~ AOU,value.var="Abun",  fun.aggregate=sum)
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
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(survey_SPA)[1],],2,sum),apply(random_mat[,,e][(dim(survey50km_NPA)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    Compa<-rbind(apply(Compa[1:dim(survey_SPA)[1],],2,sum),apply(Compa[(dim(survey50km_NPA)[1]+1):dim(Compa)[1],],2,sum))
    Compa[Compa>0]<-1
    BETAX<-beta.pair(Compa, index.family="jaccard")
    
    Bird_BetaSPA_NPA_TOT_50km[i,k]<-BETAX$beta.jac
    Bird_BetaSPA_NPA_TUR_50km[i,k]<-BETAX$beta.jtu
    Bird_BetaSPA_NPA_NES_50km[i,k]<-BETAX$beta.jne
    
    if(length(dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2])==0){Bird_S_50km_Unique_SPA.SPA_NPA[i,k]<-0
    }else{Bird_S_50km_Unique_SPA.SPA_NPA[i,k]<-dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2]} 
    
    if(length(dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2])==0){Bird_S_50km_Unique_NPA.SPA_NPA[i,k]<-0
    }else{ Bird_S_50km_Unique_NPA.SPA_NPA[i,k]<-dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2]} 
    
    Bird_S_50km_Shared_SPA_NPA.SPA_NPA[i,k]<-dim(Compa[,apply(Compa,2,sum)==2])[2]
    
    nullbeta <- t( do.call(rbind.data.frame, beta_null))
    # SES=(obs-mean(rand))/sd(rand)
    ses.tot_50km_Bird_SPA_NPA[i,k] = (BETAX$beta.jac - mean(nullbeta[3,]))/sd(nullbeta[3,])
    ses.tur_50km_Bird_SPA_NPA[i,k] = (BETAX$beta.jtu - mean(nullbeta[1,]))/sd(nullbeta[1,])
    ses.nes_50km_Bird_SPA_NPA[i,k] = (BETAX$beta.jne - mean(nullbeta[2,]))/sd(nullbeta[2,])
    
    
    #save(Bird_BetaSPA_NPA_TOT_50km,file="Bird_BetaSPA_NPA_TOT_50km.RData")
    #save(Bird_BetaSPA_NPA_TUR_50km,file="Bird_BetaSPA_NPA_TUR_50km.RData")
    #save(Bird_BetaSPA_NPA_NES_50km,file="Bird_BetaSPA_NPA_NES_50km.RData")
    
    #save(Bird_S_50km_Unique_SPA.SPA_NPA,file="Bird_S_50km_Unique_SPA.SPA_NPA.RData")
    #save(Bird_S_50km_Unique_NPA.SPA_NPA,file="Bird_S_50km_Unique_NPA.SPA_NPA.RData")
    #save(Bird_S_50km_Shared_SPA_NPA.SPA_NPA,file="Bird_S_50km_Shared_SPA_NPA.SPA_NPA.RData")
    
    #save(ses.tot_50km_Bird_SPA_NPA,file="ses.tot_50km_Bird_SPA_NPA.RData")
    #save(ses.tur_50km_Bird_SPA_NPA,file="ses.tur_50km_Bird_SPA_NPA.RData")
    #save(ses.nes_50km_Bird_SPA_NPA,file="ses.nes_50km_Bird_SPA_NPA.RData")
    
    
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}



##SPA vs RA 

Bird_BetaSPA_RA_TOT_50km<-matrix(NA,length(listMPA_SPA),reps)
Bird_BetaSPA_RA_TUR_50km<-Bird_BetaSPA_RA_TOT_50km
Bird_BetaSPA_RA_NES_50km<-Bird_BetaSPA_RA_TOT_50km

Bird_S_50km_Unique_SPA.SPA_RA<-matrix(NA,length(listMPA_SPA),reps)
Bird_S_50km_Unique_RA.SPA_RA<-Bird_S_50km_Unique_SPA.SPA_RA
Bird_S_50km_Shared_SPA_RA.SPA_RA<-Bird_S_50km_Unique_SPA.SPA_RA

ses.tot_50km_Bird_SPA_RA<-Bird_BetaSPA_RA_TOT_50km
ses.tur_50km_Bird_SPA_RA<-Bird_BetaSPA_RA_TOT_50km
ses.nes_50km_Bird_SPA_RA<-Bird_BetaSPA_RA_TOT_50km

for (i in 1: length(listMPA_SPA)) {# For each PA
  
  #survey protected
  IDReserveSPA<-subset(info_survey,info_survey$namesMPA==listMPA_SPA[i])
  
  
  IDsurvey50km<-NULL 
  for (j in 1: dim(IDReserveSPA)[1]){
    
    if(dim(IDReserveSPA)[1]==1){#strucure of the dataframe is different if dim(survey_SPA)[1]==1.
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
  IDsurvey50km_RA<- IDsurvey50km[IDsurvey50km%in%listMPA_RA2]
  
  #Beta
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (length(IDsurvey50km_RA) == 0){
      next
    }
    
    if (dim(IDReserveSPA)[1]>length(IDsurvey50km_RA)){
      
      #survey RA
      survey50km_RA<-df_bird[df_bird$NewRouteID%in%IDsurvey50km_RA,]
      survey50km_RA <- dcast(survey50km_RA, IDcomplete ~ AOU,value.var="Abun",  fun.aggregate=sum)
      rownames(survey50km_RA)<-survey50km_RA[,1]
      survey50km_RA<-survey50km_RA[,-1]      
      
      #survey SPA
      names_SPA_Sample<-sample(IDReserveSPA$NewRouteID,length(IDsurvey50km_RA),replace=F)
      survey_SPA<-df_bird[df_bird$NewRouteID%in%names_SPA_Sample,]
      survey_SPA <- dcast(survey_SPA, IDcomplete ~ AOU,value.var="Abun",  fun.aggregate=sum)
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
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(survey_SPA)[1],],2,sum),apply(random_mat[,,e][(dim(survey50km_RA)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    
    else {
      
      #survey RA
      names_RA_Sample<-sample(IDsurvey50km_RA,dim(IDReserveSPA)[1],replace=F)
      survey50km_RA<-df_bird[df_bird$NewRouteID%in%names_RA_Sample,]
      survey50km_RA <- dcast(survey50km_RA, IDcomplete ~ AOU,value.var="Abun",  fun.aggregate=sum)
      rownames(survey50km_RA)<-survey50km_RA[,1]
      survey50km_RA<-survey50km_RA[,-1]      
      
      #survey SPA
      survey_SPA<-df_bird[df_bird$NewRouteID%in%IDReserveSPA$NewRouteID,]
      survey_SPA <- dcast(survey_SPA, IDcomplete ~ AOU,value.var="Abun",  fun.aggregate=sum)
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
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(survey_SPA)[1],],2,sum),apply(random_mat[,,e][(dim(survey50km_RA)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    Compa<-rbind(apply(Compa[1:dim(survey_SPA)[1],],2,sum),apply(Compa[(dim(survey50km_RA)[1]+1):dim(Compa)[1],],2,sum))
    Compa[Compa>0]<-1
    BETAX<-beta.pair(Compa, index.family="jaccard")
    
    Bird_BetaSPA_RA_TOT_50km[i,k]<-BETAX$beta.jac
    Bird_BetaSPA_RA_TUR_50km[i,k]<-BETAX$beta.jtu
    Bird_BetaSPA_RA_NES_50km[i,k]<-BETAX$beta.jne
    
    
    if(length(dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2])==0){Bird_S_50km_Unique_SPA.SPA_RA[i,k]<-0
    }else{Bird_S_50km_Unique_SPA.SPA_RA[i,k]<-dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2]} 
    
    if(length(dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2])==0){Bird_S_50km_Unique_RA.SPA_RA[i,k]<-0
    }else{ Bird_S_50km_Unique_RA.SPA_RA[i,k]<-dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2]} 
    
    Bird_S_50km_Shared_SPA_RA.SPA_RA[i,k]<-dim(Compa[,apply(Compa,2,sum)==2])[2]
    
    
    
    
    nullbeta <- t( do.call(rbind.data.frame, beta_null))
    # SES=(obs-mean(rand))/sd(rand)
    ses.tot_50km_Bird_SPA_RA[i,k] = (BETAX$beta.jac - mean(nullbeta[3,]))/sd(nullbeta[3,])
    ses.tur_50km_Bird_SPA_RA[i,k] = (BETAX$beta.jtu - mean(nullbeta[1,]))/sd(nullbeta[1,])
    ses.nes_50km_Bird_SPA_RA[i,k] = (BETAX$beta.jne - mean(nullbeta[2,]))/sd(nullbeta[2,])
    
    
    save(Bird_BetaSPA_RA_TOT_50km,file="Bird_BetaSPA_RA_TOT_50km.RData")
    save(Bird_BetaSPA_RA_TUR_50km,file="Bird_BetaSPA_RA_TUR_50km.RData")
    save(Bird_BetaSPA_RA_NES_50km,file= "Bird_BetaSPA_RA_NES_50km.RData")
    
    save(Bird_S_50km_Unique_SPA.SPA_RA,file="Bird_S_50km_Unique_SPA.SPA_RA.RData")
    save(Bird_S_50km_Unique_RA.SPA_RA,file="Bird_S_50km_Unique_RA.SPA_RA.RData")
    save(Bird_S_50km_Shared_SPA_RA.SPA_RA,file="Bird_S_50km_Shared_SPA_RA.SPA_RA.RData")
    
    save(ses.tot_50km_Bird_SPA_RA,file="ses.tot_50km_Bird_SPA_RA.RData")
    save(ses.tur_50km_Bird_SPA_RA,file="ses.tur_50km_Bird_SPA_RA.RData")
    save(ses.nes_50km_Bird_SPA_RA,file="ses.nes_50km_Bird_SPA_RA.RData")
    
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}


##RA vs NPA
Bird_BetaRA_NPA_TOT_50km<-matrix(NA,length(listMPA_RA),50)
Bird_BetaRA_NPA_TUR_50km<-Bird_BetaRA_NPA_TOT_50km
Bird_BetaRA_NPA_NES_50km<-Bird_BetaRA_NPA_TOT_50km

Bird_S_50km_Unique_RA.RA_NPA<-matrix(NA,length(listMPA_RA),50)
Bird_S_50km_Unique_NPA.RA_NPA<-Bird_S_50km_Unique_RA.RA_NPA
Bird_S_50km_Shared_RA_NPA.RA_NPA<-Bird_S_50km_Unique_RA.RA_NPA

ses.tot_50km_Bird_RA_NPA<-Bird_BetaRA_NPA_TOT_50km
ses.tur_50km_Bird_RA_NPA<-Bird_BetaRA_NPA_TOT_50km
ses.nes_50km_Bird_RA_NPA<-Bird_BetaRA_NPA_TOT_50km

for (i in 1: length(listMPA_RA)) {# Pour chaque r??serve
  
  
  #Transect RA
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
  
  
  #Beta
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (length(IDsurvey50km_NPA) == 0){
      next
    }
    
    if (dim(IDReserveRA)[1]>length(IDsurvey50km_NPA)){
      
      #survey NPA
      survey50km_NPA<-df_bird[df_bird$NewRouteID%in%IDsurvey50km_NPA,]
      survey50km_NPA <- dcast(survey50km_NPA, IDcomplete ~ AOU,value.var="Abun",  fun.aggregate=sum)
      rownames(survey50km_NPA)<-survey50km_NPA[,1]
      survey50km_NPA<-survey50km_NPA[,-1]      
      
      #survey RA
      names_RA_Sample<-sample(IDReserveRA$NewRouteID,length(IDsurvey50km_NPA),replace=F)
      survey_RA<-df_bird[df_bird$NewRouteID%in%names_RA_Sample,]
      survey_RA <- dcast(survey_RA, IDcomplete ~ AOU,value.var="Abun",  fun.aggregate=sum)
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
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(survey_RA)[1],],2,sum),apply(random_mat[,,e][(dim(survey50km_NPA)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    else {
      
      #survey NPA
      names_NPA_Sample<-sample(IDsurvey50km_NPA,dim(IDReserveRA)[1],replace=F)
      survey50km_NPA<-df_bird[df_bird$NewRouteID%in%names_NPA_Sample,]
      survey50km_NPA <- dcast(survey50km_NPA, IDcomplete ~ AOU,value.var="Abun",  fun.aggregate=sum)
      rownames(survey50km_NPA)<-survey50km_NPA[,1]
      survey50km_NPA<-survey50km_NPA[,-1]      
      
      #survey RA
      survey_RA<-df_bird[df_bird$NewRouteID%in%IDReserveRA$NewRouteID,]
      survey_RA <- dcast(survey_RA, IDcomplete ~ AOU,value.var="Abun",  fun.aggregate=sum)
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
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(survey_RA)[1],],2,sum),apply(random_mat[,,e][(dim(survey50km_NPA)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    Compa<-rbind(apply(Compa[1:dim(survey_RA)[1],],2,sum),apply(Compa[(dim(survey50km_NPA)[1]+1):dim(Compa)[1],],2,sum))
    Compa[Compa>0]<-1
    BETAX<-beta.pair(Compa, index.family="jaccard")
    
    Bird_BetaRA_NPA_TOT_50km[i,k]<-BETAX$beta.jac
    Bird_BetaRA_NPA_TUR_50km[i,k]<-BETAX$beta.jtu
    Bird_BetaRA_NPA_NES_50km[i,k]<-BETAX$beta.jne
    
    if(length(dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2])==0){Bird_S_50km_Unique_RA.RA_NPA[i,k]<-0
    }else{Bird_S_50km_Unique_RA.RA_NPA[i,k]<-dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2]} 
    
    if(length(dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2])==0){Bird_S_50km_Unique_NPA.RA_NPA[i,k]<-0
    }else{ Bird_S_50km_Unique_NPA.RA_NPA[i,k]<-dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2]} 
    
    Bird_S_50km_Shared_RA_NPA.RA_NPA[i,k]<-dim(Compa[,apply(Compa,2,sum)==2])[2]
    
    nullbeta <- t( do.call(rbind.data.frame, beta_null))
    # SES=(obs-mean(rand))/sd(rand)
    ses.tot_50km_Bird_RA_NPA[i,k] = (BETAX$beta.jac - mean(nullbeta[3,]))/sd(nullbeta[3,])
    ses.tur_50km_Bird_RA_NPA[i,k] = (BETAX$beta.jtu - mean(nullbeta[1,]))/sd(nullbeta[1,])
    ses.nes_50km_Bird_RA_NPA[i,k] = (BETAX$beta.jne - mean(nullbeta[2,]))/sd(nullbeta[2,])
    
    
    save(Bird_BetaRA_NPA_TOT_50km,file="Bird_BetaRA_NPA_TOT_50km.RData")
    save(Bird_BetaRA_NPA_TUR_50km,file="Bird_BetaRA_NPA_TUR_50km.RData")
    save(Bird_BetaRA_NPA_NES_50km,file="Bird_BetaRA_NPA_NES_50km.RData")
    
    save(Bird_S_50km_Unique_RA.RA_NPA,file="Bird_S_50km_Unique_RA.RA_NPA.RData")
    save(Bird_S_50km_Unique_NPA.RA_NPA,file="Bird_S_50km_Unique_NPA.RA_NPA.RData")
    save(Bird_S_50km_Shared_RA_NPA.RA_NPA,file="Bird_S_50km_Shared_RA_NPA.RA_NPA.RData")
    
    save(ses.tot_50km_Bird_RA_NPA,file="ses.tot_50km_Bird_RA_NPA.RData")
    save(ses.tur_50km_Bird_RA_NPA,file="ses.tur_50km_Bird_RA_NPA.RData")
    save(ses.nes_50km_Bird_RA_NPA,file="ses.nes_50km_Bird_RA_NPA.RData")
    
    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}
