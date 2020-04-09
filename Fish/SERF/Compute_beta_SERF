load("InfoTOTAL.RData")
load("occSERF.RData")
load("surveySERF.RData")
load("geodist.Transect1.RData")
load("geodist.Transect2.RData")
load("geodist.Transect3.RData")
load("geodist.Transect4.RData")

geodist.Transect <-rbind(geodist.Transect1,geodist.Transect2,geodist.Transect3,geodist.Transect4)

listMPA_SPA<-rbind(subset(InfoTOTAL,InfoTOTAL$Infotrans.protec=="Unfished"))
listMPA_SPA<-na.omit(unique(listMPA_SPA$namesMPA))

listMPA_RA<-subset(InfoTOTAL,InfoTOTAL$Infotrans.protec=="Restricted")
listMPA_RA2<-unique(rownames(listMPA_RA))
listMPA_RA<-na.omit(unique(listMPA_RA$namesMPA))

listMPA_NPA<-subset(InfoTOTAL,InfoTOTAL$Infotrans.protec=="Fished")
listMPA_NPA<-unique(rownames(listMPA_NPA))

#keep only MPA with 2 surveys outside and inside
NBsurvey_IN<-NULL 
NBsurvey_OUT<-NULL 
NBsurvey_RES<-NULL

for (i in 1:length(listMPA_SPA)){
  IDReserveSPA<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_SPA[i])
  #Transect SPA
  Transect_SPA<-occSERF[rownames(occSERF)%in%rownames(IDReserveSPA),]
  NBsurvey_IN<-c(NBsurvey_IN,dim(Transect_SPA)[1])
  
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
    distGeo2<-subset(distGeo2,distGeo2[,j]>=0)  
    distGeo2<-unique(rownames(distGeo2))
    IDTransect50km<-c(IDTransect50km,distGeo2)
  }
  IDTransect50km<-unique(IDTransect50km)
  
  #Transect non protected
  IDTransect50km_NPA<- IDTransect50km[IDTransect50km%in%listMPA_NPA]
  Transect50km_NPA<-occSERF[rownames(occSERF)%in%IDTransect50km_NPA,]
  NBsurvey_OUT<-c(NBsurvey_OUT,dim(Transect50km_NPA)[1]) 
  
  #Transect RA
  IDTransect50km_RA<- IDTransect50km[IDTransect50km%in%listMPA_RA2]
  Transect50km_RA<-occSERF[rownames(occSERF)%in%IDTransect50km_RA,]
  NBsurvey_RES<-c(NBsurvey_RES,dim(Transect50km_RA)[1]) 
}

MPAselect<-data.frame(listMPA_SPA,NBsurvey_IN,NBsurvey_OUT,NBsurvey_RES)




####MEME CHOSE POUR LES RA  ----                          

NBsurvey_OUT<-NULL 
NBsurvey_RES<-NULL
for (i in 1:length(listMPA_RA)){
  IDReserveRA<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_RA[i])
  #Transect RA
  Transect_RA<-occSERF[rownames(occSERF)%in%rownames(IDReserveRA),]
  NBsurvey_RES<-c(NBsurvey_RES,dim(Transect_RA)[1])
  
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
  Transect50km_NPA<-occSERF[rownames(occSERF)%in%IDTransect50km_NPA,]
  NBsurvey_OUT<-c(NBsurvey_OUT,dim(Transect50km_NPA)[1]) 
}

RESselect<-data.frame(listMPA_RA,NBsurvey_RES,NBsurvey_OUT)


#50km
listMPA_SPA_NPA <- MPAselect[MPAselect$NBsurvey_IN>2 &   MPAselect$NBsurvey_OUT>2,]$listMPA_SPA
listMPA_SPA_RA<-MPAselect[MPAselect$NBsurvey_IN>2 &   MPAselect$NBsurvey_RES>2,]$listMPA_SPA
listMPA_RA_NPA<-RESselect[RESselect$NBsurvey_RES>2 & RESselect$NBsurvey_OUT>2,]$listMPA_RA

#########################################################################
#reps model null and bootstrap
reps=1000
core=30

# SPA VS NPA ----

Fish_BetaSPA_NPA_TOT_50km_SERF<-matrix(NA,length(listMPA_SPA_NPA),reps)
Fish_BetaSPA_NPA_TUR_50km_SERF<-Fish_BetaSPA_NPA_TOT_50km_SERF
Fish_BetaSPA_NPA_NES_50km_SERF<-Fish_BetaSPA_NPA_TOT_50km_SERF

Fish_S_50km_Unique_SPA.SPA_NP_SERF<-matrix(NA,length(listMPA_SPA_NPA),reps)
Fish_S_50km_Unique_NP.SPA_NP_SERF<-Fish_S_50km_Unique_SPA.SPA_NP_SERF
Fish_S_50km_Shared_SPA_NP_SERF<-Fish_S_50km_Unique_SPA.SPA_NP_SERF

ses.tot_50km_SERF_SPA_NPA <- Fish_BetaSPA_NPA_TOT_50km_SERF
ses.tur_50km_SERF_SPA_NPA <- Fish_BetaSPA_NPA_TOT_50km_SERF
ses.nes_50km_SERF_SPA_NPA<- Fish_BetaSPA_NPA_TOT_50km_SERF


for (i in 1: length(listMPA_SPA_NPA)) {# Pour chaque r??serve
  
  # Prendre les transects de cette r??serve & qui sont unfished
  IDReserveSPA<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_SPA_NPA[i])
  IDReserveSPA<-subset(IDReserveSPA,IDReserveSPA$Infotrans.protec=="Unfished")
  #Transect SPA
  Transect_SPA<-occSERF[rownames(occSERF)%in%rownames(IDReserveSPA),]
  
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
  Transect50km_NPA<-occSERF[rownames(occSERF)%in%IDTransect50km_NPA,]
  
  if(dim(Transect50km_NPA)[1]==0){next}
  
  #Beta
  for (k in 1:reps){#bootstrap 
    
    if (dim(Transect_SPA)[1]>dim(Transect50km_NPA)[1]){
      
      Group_SPA_Sample<-as.matrix(Transect_SPA[sample(1:nrow(Transect_SPA),dim(Transect50km_NPA)[1],replace=F),])
      Group_SPA2<-apply(Group_SPA_Sample,2,sum)
      Group_NP2<-apply(Transect50km_NPA,2,sum)
      
      Compa_NULL<-rbind(Group_SPA_Sample,Transect50km_NPA)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Group_SPA_Sample)[1],],2,sum),apply(random_mat[,,e][(dim(Transect50km_NPA)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
      
    }
    
    else {
      
      Group_NP_Sample<-as.matrix(Transect50km_NPA[sample(1:nrow(Transect50km_NPA),dim(Transect_SPA)[1],replace=F),])
      Group_SPA2<-apply(Transect_SPA, 2,sum)
      Group_NP2<-apply(Group_NP_Sample, 2,sum)
      
      Compa_NULL<-rbind(Transect_SPA,Group_NP_Sample)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Transect_SPA)[1],],2,sum),apply(random_mat[,,e][(dim(Group_NP_Sample)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    nullbeta <- t( do.call(rbind.data.frame, beta_null))
    
    Compa<-rbind(Group_SPA2,Group_NP2)
    Compa[Compa>0]<-1
    BETAX<-beta.pair(Compa, index.family="jaccard")
    
    Fish_BetaSPA_NPA_TOT_50km_SERF[i,k]<-BETAX$beta.jac
    Fish_BetaSPA_NPA_TUR_50km_SERF[i,k]<-BETAX$beta.jtu
    Fish_BetaSPA_NPA_NES_50km_SERF[i,k]<-BETAX$beta.jne
    
    
    if(length(dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2])==0){Fish_S_50km_Unique_SPA.SPA_NP_SERF[i,k]<-0
    }else{Fish_S_50km_Unique_SPA.SPA_NP_SERF[i,k]<-dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2]} 
    
    if(length(dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2])==0){Fish_S_50km_Unique_NP.SPA_NP_SERF[i,k]<-0
    }else{ Fish_S_50km_Unique_NP.SPA_NP_SERF[i,k]<-dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2]} 
    
    if(length(dim(Compa[,apply(Compa,2,sum)==2])[2])==0){Fish_S_50km_Shared_SPA_NP_SERF[i,k]<-0
    }else{ Fish_S_50km_Shared_SPA_NP_SERF[i,k]<-dim(Compa[,apply(Compa,2,sum)==2])[2]} 
    
    # SES=(obs-mean(rand))/sd(rand)
    ses.tot_50km_SERF_SPA_NPA[i,k] = (BETAX$beta.jac - mean(nullbeta[3,]))/sd(nullbeta[3,])
    ses.tur_50km_SERF_SPA_NPA[i,k] = (BETAX$beta.jtu - mean(nullbeta[1,]))/sd(nullbeta[1,])
    ses.nes_50km_SERF_SPA_NPA[i,k] = (BETAX$beta.jne - mean(nullbeta[2,]))/sd(nullbeta[2,])
  
     save(Fish_BetaSPA_NPA_TOT_50km_SERF,file="Fish_BetaSPA_NPA_TOT_50km_SERF.RData")
     save(Fish_BetaSPA_NPA_TUR_50km_SERF,file="Fish_BetaSPA_NPA_TUR_50km_SERF.RData")
     save(Fish_BetaSPA_NPA_NES_50km_SERF,file="Fish_BetaSPA_NPA_NES_50km_SERF.RData")
    
     save(Fish_S_50km_Unique_SPA.SPA_NP_SERF,file="Fish_S_50km_Unique_SPA.SPA_NP_SERF.RData")
     save(Fish_S_50km_Unique_NP.SPA_NP_SERF,file="Fish_S_50km_Unique_NP.SPA_NP_SERF.RData")
     save(Fish_S_50km_Shared_SPA_NP_SERF,file="Fish_S_50km_Shared_SPA_NP_SERF.RData")
    
     save(ses.tot_50km_SERF_SPA_NPA,file="ses.tot_50km_SERF_SPA_NPA.RData")
     save(ses.tur_50km_SERF_SPA_NPA,file="ses.tur_50km_SERF_SPA_NPA.RData")
     save(ses.nes_50km_SERF_SPA_NPA,file="ses.nes_50km_SERF_SPA_NPA.RData")

    print(paste0("k",k))
  }
  
  print(paste0("i",i))
}



# SPA VS RA ----
Fish_BetaSPA_RA_TOT_50km_SERF<-matrix(NA,length(listMPA_SPA_RA),reps)
Fish_BetaSPA_RA_TUR_50km_SERF<-Fish_BetaSPA_RA_TOT_50km_SERF
Fish_BetaSPA_RA_NES_50km_SERF<-Fish_BetaSPA_RA_TOT_50km_SERF

Fish_S_50km_Unique_SPA.SPA_RA_SERF<-matrix(NA,length(listMPA_SPA_RA),reps)
Fish_S_50km_Unique_RA.SPA_RA_SERF<-Fish_S_50km_Unique_SPA.SPA_RA_SERF
Fish_S_50km_Shared_SPA_RA.SPA_RA_SERF<-Fish_S_50km_Unique_SPA.SPA_RA_SERF

ses.tot_50km_SERF_SPA_RA <- Fish_BetaSPA_RA_TOT_50km_SERF
ses.tur_50km_SERF_SPA_RA <- Fish_BetaSPA_RA_TOT_50km_SERF
ses.nes_50km_SERF_SPA_RA<- Fish_BetaSPA_RA_TOT_50km_SERF

for (i in 1: length(listMPA_SPA_RA)) {# Pour chaque r??serve
  
  #Transect SPA
  IDReserveSPA<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_SPA_RA[i])
  IDReserveSPA<-subset(IDReserveSPA,IDReserveSPA$Infotrans.protec=="Unfished")

  Transect_SPA<-occSERF[rownames(occSERF)%in%rownames(IDReserveSPA),]
  
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
  Transect50km_RA<-occSERF[rownames(occSERF)%in%IDTransect50km_RA,]
  
  if(dim(Transect50km_RA)[1]==0){next}
  
  
  #Beta
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (dim(Transect_SPA)[1]>dim(Transect50km_RA)[1]){
      
      Group_SPA_Sample<-as.matrix(Transect_SPA[sample(1:nrow(Transect_SPA),dim(Transect50km_RA)[1],replace=F),])
      Group_SPA2<-apply(Group_SPA_Sample,2,sum)
      Group_RA2<-apply(Transect50km_RA,2,sum)
      
      Compa_NULL<-rbind(Group_SPA_Sample,Transect50km_RA)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Group_SPA_Sample)[1],],2,sum),apply(random_mat[,,e][(dim(Transect50km_RA)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
      
    }
    
    else {
      
      Group_RA_Sample<-as.matrix(Transect50km_RA[sample(1:nrow(Transect50km_RA),dim(Transect_SPA)[1],replace=F),])
      Group_SPA2<-apply(Transect_SPA, 2,sum)
      Group_RA2<-apply(Group_RA_Sample, 2,sum)
      
      Compa_NULL<-rbind(Transect_SPA,Group_RA_Sample)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Transect_SPA)[1],],2,sum),apply(random_mat[,,e][(dim(Group_RA_Sample)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
      
    }
    nullbeta <- t( do.call(rbind.data.frame, beta_null))
    
    Compa<-rbind(Group_SPA2,Group_RA2)
    Compa[Compa>0]<-1
    BETAX<-beta.pair(Compa, index.family="jaccard")
    
    Fish_BetaSPA_RA_TOT_50km_SERF[i,k]<-BETAX$beta.jac
    Fish_BetaSPA_RA_TUR_50km_SERF[i,k]<-BETAX$beta.jtu
    Fish_BetaSPA_RA_NES_50km_SERF[i,k]<-BETAX$beta.jne
    
    
    if(length(dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2])==0){Fish_S_50km_Unique_SPA.SPA_RA_SERF[i,k]<-0
    }else{Fish_S_50km_Unique_SPA.SPA_RA_SERF[i,k]<-dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2]} 
    
    if(length(dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2])==0){Fish_S_50km_Unique_RA.SPA_RA_SERF[i,k]<-0
    }else{ Fish_S_50km_Unique_RA.SPA_RA_SERF[i,k]<-dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2]} 
    
    if(length(dim(Compa[,apply(Compa,2,sum)==2])[2])==0){Fish_S_50km_Shared_SPA_RA.SPA_RA_SERF[i,k]<-0
    }else{ Fish_S_50km_Shared_SPA_RA.SPA_RA_SERF[i,k]<-dim(Compa[,apply(Compa,2,sum)==2])[2]} 
    
    # SES=(obs-mean(rand))/sd(rand)
    ses.tot_50km_SERF_SPA_RA[i,k] = (BETAX$beta.jac - mean(nullbeta[3,]))/sd(nullbeta[3,])
    ses.tur_50km_SERF_SPA_RA[i,k] = (BETAX$beta.jtu - mean(nullbeta[1,]))/sd(nullbeta[1,])
    ses.nes_50km_SERF_SPA_RA[i,k] = (BETAX$beta.jne - mean(nullbeta[2,]))/sd(nullbeta[2,])
 
    print(paste ("k",k))
    
     save(Fish_BetaSPA_RA_TOT_50km_SERF,file="Fish_BetaSPA_RA_TOT_50km_SERF.RData")
     save(Fish_BetaSPA_RA_TUR_50km_SERF,file="Fish_BetaSPA_RA_TUR_50km_SERF.RData")
     save(Fish_BetaSPA_RA_NES_50km_SERF,file="Fish_BetaSPA_RA_NES_50km_SERF.RData")
    
     save(Fish_S_50km_Unique_SPA.SPA_RA_SERF,file="Fish_S_50km_Unique_SPA.SPA_RA_SERF.RData")
     save(Fish_S_50km_Unique_RA.SPA_RA_SERF,file="Fish_S_50km_Unique_RA.SPA_RA_SERF.RData")
     save(Fish_S_50km_Shared_SPA_RA.SPA_RA_SERF,file="Fish_S_50km_Shared_SPA_RA.SPA_RA_SERF.RData")
    
     save(ses.tot_50km_SERF_SPA_RA,file="ses.tot_50km_SERF_SPA_RA.RData")
     save(ses.tur_50km_SERF_SPA_RA,file="ses.tur_50km_SERF_SPA_RA.RData")
     save(ses.nes_50km_SERF_SPA_RA,file="ses.nes_50km_SERF_SPA_RA.RData")

  }
  print(i)
}

##
# RA VS NPA  ----
Fish_BetaRA_NPA_TOT_50km_SERF<-matrix(NA,length(listMPA_RA_NPA),reps)
Fish_BetaRA_NPA_TUR_50km_SERF<-Fish_BetaRA_NPA_TOT_50km_SERF
Fish_BetaRA_NPA_NES_50km_SERF<-Fish_BetaRA_NPA_TOT_50km_SERF

Fish_S_50km_Unique_RA.RA_NP_SERF<-matrix(NA,length(listMPA_RA_NPA),reps)
Fish_S_50km_Unique_NP.RA_NP_SERF<-Fish_S_50km_Unique_RA.RA_NP_SERF
Fish_S_50km_Shared_RA_NP.RA_NP_SERF<-Fish_S_50km_Unique_RA.RA_NP_SERF

# SES=(obs-mean(rand))/sd(rand)
ses.tot_50km_SERF_RA_NPA<-Fish_BetaRA_NPA_TOT_50km_SERF
ses.tur_50km_SERF_RA_NPA<-Fish_BetaRA_NPA_TOT_50km_SERF
ses.nes_50km_SERF_RA_NPA<-Fish_BetaRA_NPA_TOT_50km_SERF


for (i in 1: length(listMPA_RA_NPA)) {# For each MPA
  
  #Transect RA
  IDReserveRA<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_RA_NPA[i])
  IDReserveRA<-subset(IDReserveRA,IDReserveRA$Infotrans.protec=="Restricted")
  Transect_RA<-occSERF[rownames(occSERF)%in%rownames(IDReserveRA),]
  
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
  Transect50km_NPA<-occSERF[rownames(occSERF)%in%IDTransect50km_NPA,]
  
  
  if(dim(Transect50km_NPA)[1]==0){next}
  
  #Beta
  for (k in 1:reps){ #Bootstrap due to different sampling effort
    
    if (dim(Transect_RA)[1]>dim(Transect50km_NPA)[1]){
      
      Group_RA_Sample<-as.matrix(Transect_RA[sample(1:nrow(Transect_RA),dim(Transect50km_NPA)[1],replace=F),])
      Group_RA2<-apply(Group_RA_Sample,2,sum)
      Group_NP2<-apply(Transect50km_NPA,2,sum)
      
      Compa_NULL<-rbind(Group_RA_Sample,Transect50km_NPA)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Group_RA_Sample)[1],],2,sum),apply(random_mat[,,e][(dim(Transect50km_NPA)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
      
    }
    
    else {
      
      Group_NP_Sample<-as.matrix(Transect50km_NPA[sample(1:nrow(Transect50km_NPA),dim(Transect_RA)[1],replace=F),])
      Group_RA2<-apply(Transect_RA, 2,sum)
      Group_NP2<-apply(Group_NP_Sample, 2,sum)
      
      Compa_NULL<-rbind(Transect_RA,Group_NP_Sample)
      Compa_NULL<-Compa_NULL[,apply(Compa_NULL,2,sum)>0]
      #create random matrices
      null.algo<-nullmodel(Compa_NULL, "curveball")
      random_mat <- simulate(null.algo, nsim=reps,seed=1871)
      #create random matrices
      random_pool<-mclapply(1:reps,function(e){ rbind(apply(random_mat[,,e][1:dim(Transect_RA)[1],],2,sum),apply(random_mat[,,e][(dim(Group_NP_Sample)[1]+1):dim(Compa_NULL)[1],],2,sum))
      },mc.cores=core)
      random_pool<- rapply(random_pool,function(x) ifelse(x>=1,1,x), how = "replace")
      beta_null<-mclapply(1:reps,function(k) beta.pair(random_pool[[k]],index.family = "jaccard"),mc.cores=core)  
    }
    
    nullbeta <- t( do.call(rbind.data.frame, beta_null))
    
    
    Compa<-rbind(Group_RA2,Group_NP2)
    Compa[Compa>0]<-1
    BETAX<-beta.pair(Compa, index.family="jaccard")
    
    Fish_BetaRA_NPA_TOT_50km_SERF[i,k]<-BETAX$beta.jac
    Fish_BetaRA_NPA_TUR_50km_SERF[i,k]<-BETAX$beta.jtu
    Fish_BetaRA_NPA_NES_50km_SERF[i,k]<-BETAX$beta.jne
    
    if(length(dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2])==0){Fish_S_50km_Unique_RA.RA_NP_SERF[i,k]<-0
    }else{Fish_S_50km_Unique_RA.RA_NP_SERF[i,k]<-dim(Compa[,Compa[1,]==1 & Compa[2,]==0])[2]} 
    
    if(length(dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2])==0){Fish_S_50km_Unique_NP.RA_NP_SERF[i,k]<-0
    }else{ Fish_S_50km_Unique_NP.RA_NP_SERF[i,k]<-dim(Compa[,Compa[2,]==1 & Compa[1,]==0])[2]} 
    
    if(length(dim(Compa[,apply(Compa,2,sum)==2])[2])==0){Fish_S_50km_Shared_RA_NP.RA_NP_SERF[i,k]<-0
    }else{ Fish_S_50km_Shared_RA_NP.RA_NP_SERF[i,k]<-dim(Compa[,apply(Compa,2,sum)==2])[2]} 
    
    # SES=(obs-mean(rand))/sd(rand)
    ses.tot_50km_SERF_RA_NPA[i,k] = (BETAX$beta.jac - mean(nullbeta[3,]))/sd(nullbeta[3,])
    ses.tur_50km_SERF_RA_NPA[i,k] = (BETAX$beta.jtu - mean(nullbeta[1,]))/sd(nullbeta[1,])
    ses.nes_50km_SERF_RA_NPA[i,k] = (BETAX$beta.jne - mean(nullbeta[2,]))/sd(nullbeta[2,])
    
    print(paste ("k",k))
   
     save(Fish_BetaRA_NPA_TOT_50km_SERF,file="Fish_BetaRA_NPA_TOT_50km_SERF.RData")
     save(Fish_BetaRA_NPA_TUR_50km_SERF,file="Fish_BetaRA_NPA_TUR_50km_SERF.RData")
     save(Fish_BetaRA_NPA_NES_50km_SERF,file="Fish_BetaRA_NPA_NES_50km_SERF.RData")
    
     save(Fish_S_50km_Unique_RA.RA_NP_SERF,file="Fish_S_50km_Unique_RA.RA_NP_SERF.RData")
     save(Fish_S_50km_Unique_NP.RA_NP_SERF,file="Fish_S_50km_Unique_NP.RA_NP_SERF.RData")
     save(Fish_S_50km_Shared_RA_NP.RA_NP_SERF,file="Fish_S_50km_Shared_RA_NP.RA_NP_SERF.RData")
    
     save(ses.tot_50km_SERF_RA_NPA,file="ses.tot_50km_SERF_RA_NPA.RData")
     save(ses.tur_50km_SERF_RA_NPA,file="ses.tur_50km_SERF_RA_NPA.RData")
     save(ses.nes_50km_SERF_RA_NPA,file="ses.nes_50km_SERF_RA_NPA.RData")
 
  }
  print(i)
}
