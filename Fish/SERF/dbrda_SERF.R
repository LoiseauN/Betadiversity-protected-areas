
survey <- unique(data.frame(UniqueTransect=record$UniqueTransect, UniqueSite=record$UniqueSite))
env_survey <- unique(data.frame(UniqueSite=env$UniqueSite,CleanHabitat=env$CleanHabitat,Depth=env$Depth))
env_survey[env_survey==-999]<-NA

Hab_data <- merge(survey,env_survey,by="UniqueSite")
Hab_data <- merge(Hab_data,SERF_chla_sst,by.x="UniqueSite",by.y= "name")
Hab_data<- Hab_data[Hab_data$UniqueTransect %in% rownames(occSERF),]
rownames(Hab_data)<-Hab_data$UniqueTransect
Hab_data <- Hab_data[,-c(1,2)]


Hab_data$NumHab <- NA
for (i in 1:nrow(Hab_data)){
  if(Hab_data$CleanHabitat[i]=="Flat") Hab_data$NumHab[i]<-1
  if(Hab_data$CleanHabitat[i]=="Lagoon_Back reef") Hab_data$NumHab[i]<-2
  if(Hab_data$CleanHabitat[i]=="Crest") Hab_data$NumHab[i]<-3
  if(Hab_data$CleanHabitat[i]=="Slope") Hab_data$NumHab[i]<-4
}

Hab_data <- Hab_data[,-c(1,3,4)]


for(i in 1:4){ Hab_data[,i] <- as.numeric(as.character(Hab_data[,i]))}


Hab_data <-log10(Hab_data+1)

mydata <- Hab_data[,-1]
mydata <- na.omit(mydata)

#Cluster approach not used for dbrda
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

grouphab<-data.frame(SurveyID=rownames(mydata), group=kmeans(mydata,6, nstart = 30)$cluster)

#PC Approach
dist_hab <- cluster::daisy(mydata,"euclidean")
pcoa_hab <- ape::pcoa(dist_hab)
PC <- pcoa_hab$vectors[,1:4]


data_hab_prot <- merge(grouphab,PC,by="row.names")
rownames(data_hab_prot) <- data_hab_prot[,1]
data_hab_prot <- data_hab_prot[,-1]
colnames(data_hab_prot)[1:6]<- c("SurveyID","grouphab","PC1","PC2","PC3","PC4")



scale=50
########################################################################
dbrda_fish_close_50km_SERF <- matrix(NA,length(listMPA_close),10)
colnames(dbrda_fish_close_50km_SERF) <- c("F_mod_all","Pr(>F)_mod_all",
                                   "RsquareAdj_all",
                                   "F_mod_partial_PA","Pr(>F)_mod_partial_PA",
                                   "RsquareAdj_partial_PA",
                                   "F_mod_partial_hab","Pr(>F)_mod_partial_hab",
                                   "RsquareAdj_partial_hab",
                                   "Is Info")



InfoMPAcompared <-  matrix(NA,length(listMPA_close),3)
colnames(InfoMPAcompared) <- c("SPA","NPA","RA")


for (i in 1: length(listMPA_close) ) {# Pour chaque r??serve
  print(i)
  
  # Prendre les transects de cette r??serve & qui sont unfished
  IDReserveClose<-subset(InfoTOTAL,InfoTOTAL$namesMPA==listMPA_close[i])
  IDReserveClose<-subset(IDReserveClose,IDReserveClose$Infotrans.protec=="Unfished")
  #Transect Closed
  Transect_Closed<-occSERF[rownames(occSERF)%in%rownames(IDReserveClose),]
  if (nrow(Transect_Closed)==0){next}    
  #Transect un protected
  IDTransect50km<-NULL 
  for (j in 1:dim(Transect_Closed)[1]){
    
    if(dim(Transect_Closed)[1]==1){#strucure of the dataframe is different if dim(Transect_Closed)[1]==1.
      distGeo<-data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_Closed),])
      rownames(distGeo)<-colnames(geodist.Transect)}
    
    else{
      distGeo<-t(data.frame(geodist.Transect[rownames(geodist.Transect)%in%rownames(Transect_Closed),]))
      rownames(distGeo)<-colnames(geodist.Transect)
    }
    
    
    distGeo2<-subset(distGeo,distGeo[,j]<50) 
    distGeo2<-subset(distGeo2,distGeo2[,j]>0)  
    distGeo2<-unique(rownames(distGeo2))
    IDTransect50km<-c(IDTransect50km,distGeo2)
  }
  IDTransect50km<-unique(IDTransect50km)
  
  if (length(IDTransect50km)==0){next}    
  
  #Transect non protected
  IDTransect50km_NP<- IDTransect50km[IDTransect50km%in%rownames(subset(InfoTOTAL,InfoTOTAL$Infotrans.protec=="Fished"))]
  Transect50km_NP<-occSERF[rownames(occSERF)%in%IDTransect50km_NP,]
  
  
  #Transect restricted
  IDTransect50km_restricted<- IDTransect50km[IDTransect50km%in%rownames(subset(InfoTOTAL,InfoTOTAL$Infotrans.protec=="Restricted"))]
  Transect50km_restricted<-occSERF[rownames(occSERF)%in%IDTransect50km_restricted,]
  
  if (dim(Transect50km_NP)[1]==0  & dim(Transect50km_restricted)[1]==0){next}       
  
  Transect_Closed <- Transect_Closed[apply(Transect_Closed,1,sum)>0,]
  if (dim(Transect50km_NP)[1]>0) Transect50km_NP <- Transect50km_NP[apply(Transect50km_NP,1,sum)>0,]
  if (dim(Transect50km_restricted)[1]>0)  Transect50km_restricted <- Transect50km_restricted[apply(Transect50km_restricted,1,sum)>0,]   
  
  data_pool <-  gtools::smartbind(Transect_Closed,Transect50km_NP,Transect50km_restricted)
  data_pool[is.na(data_pool)] <- 0
  rownames(data_pool)<- c(rownames(Transect_Closed),rownames(Transect50km_NP),rownames(Transect50km_restricted))
  
  Management <- data.frame(SurveyID=rownames(data_pool),
                           Management=c(rep("Close",dim(Transect_Closed)[1]),rep("Unpro",dim(Transect50km_NP)[1])
                                        ,rep("Restricted",dim(Transect50km_restricted)[1])))
  
  #Hab <- grouphab[rownames(grouphab) %in% rownames(data_pool),]
  
  Hab <- data_hab_prot[rownames(data_hab_prot) %in% rownames(data_pool),]
  
  if (dim(Hab)[1]==0) {
    beta_pool <- beta.pair(data_pool, index.family="jaccard")
  mod_complete <- capscale(formula= beta_pool$beta.jtu ~ Management, data = Management)
  anov_complete <- anova(mod_complete, permutations = 9999)
  
  dbrda_fish_close_50km_SERF[i,1] <- anov_complete$'F'[1]
  dbrda_fish_close_50km_SERF[i,2] <- anov_complete$'Pr(>F)'[1]
  dbrda_fish_close_50km_SERF[i,3] <- RsquareAdj(mod_complete)$r.squared
  
  dbrda_fish_close_50km_SERF[i,10] <- 0
  
  
  }else {
    #Hab$dominanthab <- colnames(Hab)[apply(Hab,1,which.max)]
    #Management <-merge(Management,Hab,by.x="rownames",by.y="row.names")
    InfoMana_hab <-merge(Management,Hab,by="SurveyID",all.x=T)
    rownames(InfoMana_hab) <- InfoMana_hab[,1]
    InfoMana_hab <- InfoMana_hab[,-1]
   # colnames(InfoMana_hab) <- c("Management","grouphab")
    InfoMana_hab <- na.omit(InfoMana_hab)
    
    if (length(unique(InfoMana_hab$Management))==1) {
      beta_pool <- beta.pair(data_pool, index.family="jaccard")
      mod_complete <- capscale(formula= beta_pool$beta.jtu ~ Management, data = Management)
      anov_complete <- anova(mod_complete, permutations = 9999)
      
      dbrda_fish_close_50km_SERF[i,1] <- anov_complete$'F'[1]
      dbrda_fish_close_50km_SERF[i,2] <- anov_complete$'Pr(>F)'[1]
      dbrda_fish_close_50km_SERF[i,3] <- RsquareAdj(mod_complete)$r.squared
    
    }else {
      
      
      data_pool<-data_pool[rownames(data_pool) %in% rownames(InfoMana_hab),]
      InfoMana_hab <- InfoMana_hab[match(rownames(data_pool), rownames(InfoMana_hab)), ]
      
      beta_pool <- beta.pair(data_pool, index.family="jaccard")
      mod_complete <- capscale(formula= beta_pool$beta.jtu ~ Management + PC1 + PC2, data = InfoMana_hab)
      mod_partial_PA  <- capscale(formula= beta_pool$beta.jtu ~ Management + Condition(PC1 + PC2), data = InfoMana_hab)
      mod_partial_hab  <- capscale(formula= beta_pool$beta.jtu ~ Condition(Management) + PC1 + PC2, data = InfoMana_hab)
      
      anov_complete <- anova(mod_complete, permutations = 9999)
      anov_partial_PA  <- anova(mod_partial_PA, permutations = 9999)
      anov_partial_hab  <- anova(mod_partial_hab, permutations = 9999)
      
      dbrda_fish_close_50km_SERF[i,1] <- anov_complete$'F'[1]
      dbrda_fish_close_50km_SERF[i,2] <- anov_complete$'Pr(>F)'[1]
      dbrda_fish_close_50km_SERF[i,3] <- RsquareAdj(mod_complete)$r.squared
      
      dbrda_fish_close_50km_SERF[i,4] <- anov_partial_PA$'F'[1]
      dbrda_fish_close_50km_SERF[i,5] <- anov_partial_PA$'Pr(>F)'[1]
      dbrda_fish_close_50km_SERF[i,6] <- RsquareAdj(mod_partial_PA)$r.squared
      
      dbrda_fish_close_50km_SERF[i,7] <- anov_partial_hab$'F'[1]
      dbrda_fish_close_50km_SERF[i,8] <- anov_partial_hab$'Pr(>F)'[1]
      dbrda_fish_close_50km_SERF[i,9] <- RsquareAdj(mod_partial_hab)$r.squared
      
      dbrda_fish_close_50km_SERF[i,10] <- 1
    } 
    
    
    
  } 
  if(dim(Transect_Closed)[1]>0) InfoMPAcompared[i,1]<- 1
  if(dim(Transect50km_NP)[1]>0) InfoMPAcompared[i,2]<- 1
  if(dim(Transect50km_restricted)[1]>0) InfoMPAcompared[i,3]<- 1
}

save(dbrda_fish_close_50km_SERF,file="dbrda_fish_close_50km_SERF.RData")
