library(vegan)
library(reshape2)

load("distpair_birds.RData")
load("data_birds.RData")
load("Hab_all.RData")

data_birds <- data_birds[,-c(19:33,35)]


PCApproach
dist_hab <- cluster::daisy(Hab_all,"gower")
pcoa_hab <- ape::pcoa(dist_hab)
PC <- pcoa_hab$vectors[,1:4]

data_birds <- merge(data_birds,grouphab,by.x= "NewRouteID",by="row.names",all.x = T)
data_birds <- merge(data_birds,PC,by.x= "NewRouteID",by="row.names",all.x = T)
colnames(data_birds)[20:24]<- c("grouphab","PC1","PC2","PC3","PC4")
#'-------------------------------------------------------------------------------------------------- DBRDA
dbrda_BIRDS_close_50km <-matrix(NA,length(listMPA_SPA),10)

colnames(dbrda_BIRDS_close_50km) <- c("F_mod_all","Pr(>F)_mod_all",
                                      "RsquareAdj_all",
                                      "F_mod_partial_PA","Pr(>F)_mod_partial_PA",
                                      "RsquareAdj_partial_PA",
                                      "F_mod_partial_hab","Pr(>F)_mod_partial_hab",
                                      "RsquareAdj_partial_hab",
                                      "Is Info")

InfoMPAcompared <-  matrix(NA,length(listMPA_SPA),3)
colnames(InfoMPAcompared) <- c("SPA","NPA","RA")

for (i in 1: length(listMPA_SPA) ) {# Pour chaque r??serve
  print(i)
  
  # Prendre les transects de cette r??serve & qui sont unfished
  # Prendre les surveys de cette r??serve
  IDReserveSPA<-subset(data_birds,data_birds$namesMPA==listMPA_SPA[i])
  
  #survey SPA
  survey_SPA<-df_bird[df_bird$NewRouteID%in%IDReserveSPA$NewRouteID,]
  survey_SPA <- dcast(survey_SPA, IDcomplete ~ AOU,value.var="Abun",  fun.aggregate=sum)
  rownames(survey_SPA)<-survey_SPA[,1]
  survey_SPA<-survey_SPA[,-1]
  
  #survey un protected
  distGeo<-data.frame(rbind(distpair_birds[distpair_birds$iso1 %in% IDReserveSPA$NewRouteID,],distpair_birds[distpair_birds$iso2 %in% IDReserveSPA$NewRouteID,]))
  distGeo2<-subset(distGeo,distGeo$dist<50) 
  
  IDsurvey50km<-unique(c(distGeo2$iso1,distGeo2$iso2))
  
  if (length(IDsurvey50km)==0){next}       
  
  #survey non protected
  IDsurvey50km_NPA <- IDsurvey50km[IDsurvey50km %in% data_birds[data_birds$catPA=="NPA",]$NewRouteID]
  
  survey50km_NPA<-df_bird[df_bird$NewRouteID%in%IDsurvey50km_NPA,]
  if (dim(survey50km_NPA)[1]>0) { survey50km_NPA <- dcast(survey50km_NPA, IDcomplete ~ AOU,value.var="Abun",  fun.aggregate=sum)
  rownames(survey50km_NPA)<-survey50km_NPA[,1]
  survey50km_NPA<-survey50km_NPA[,-1] }    
  
  #survey restricted
  IDsurvey50km_RA<- IDsurvey50km[IDsurvey50km %in% data_birds[data_birds$catPA=="RA",]$NewRouteID]
  
  survey50km_RA<-df_bird[df_bird$NewRouteID%in%IDsurvey50km_RA,]
  if (dim(survey50km_RA)[1]>0) {survey50km_RA <- dcast(survey50km_RA, IDcomplete ~ AOU,value.var="Abun",  fun.aggregate=sum)
  rownames(survey50km_RA)<-survey50km_RA[,1]
  survey50km_RA<-survey50km_RA[,-1] }   
  
  if (dim(survey50km_NPA)[1]==0  & dim(survey50km_RA)[1]==0){next}       
  
  survey_SPA <- survey_SPA[apply(survey_SPA,1,sum)>0,]
  if (dim(survey50km_NPA)[1]>0) survey50km_NPA <- survey50km_NPA[apply(survey50km_NPA,1,sum)>0,]
  if (dim(survey50km_RA)[1]>0)  survey50km_RA <- survey50km_RA[apply(survey50km_RA,1,sum)>0,]   
  
  data_pool <-  smartbind(survey_SPA,survey50km_NPA,survey50km_RA)
  data_pool[is.na(data_pool)] <- 0
  rownames(data_pool)<- c(rownames(survey_SPA),rownames(survey50km_NPA),rownames(survey50km_RA))
  
  Management <- data.frame(row.names=rownames(data_pool),
                           Management=c(rep("SPA",dim(survey_SPA)[1]),rep("NPA",dim(survey50km_NPA)[1])
                                        ,rep("RPA",dim(survey50km_RA)[1])))
  
  #Hab <- unique(data_birds[data_birds$IDcomplete %in% rownames(Management),][,c("IDcomplete","grouphab")])
 Hab <- unique(data_birds[data_birds$IDcomplete %in% rownames(Management),][,c("IDcomplete","PC1","PC2","PC3","PC4")])

  data_pool[data_pool>0]<-1
  beta_pool <- beta.pair(data_pool, index.family="jaccard")
  
  if (dim(na.omit(Hab))[1]==0) { 
    mod_complete <- capscale(beta_pool$beta.jtu ~ Management, data = Management)
    anov_complete <- anova(mod_complete, permutations = 9999)
   
  dbrda_BIRDS_close_50km[i,1] <- anov_complete$'F'[1]
  dbrda_BIRDS_close_50km[i,2] <- anov_complete$'Pr(>F)'[1]
  dbrda_BIRDS_close_50km[i,3] <- RsquareAdj(mod_complete)$r.squared
  
  dbrda_BIRDS_close_50km[i,10] <- 0

  }else {
    #Hab$dominanthab <- colnames(Hab)[apply(Hab,1,which.max)]
    #Management <-merge(Management,Hab,by.x="rownames",by.y="row.names")
    InfoMana_hab <- unique(merge(Management,Hab,by.x="row.names",by.y="IDcomplete"))
    rownames(InfoMana_hab) <- InfoMana_hab[,1]
    InfoMana_hab <- InfoMana_hab[,-1]
    #colnames(InfoMana_hab) <- c("Management","grouphab")
    InfoMana_hab <- InfoMana_hab[match(rownames(data_pool), rownames(InfoMana_hab)), ]
    
    if(length(unique(InfoMana_hab$grouphab))==1) { 
      mod_complete <- capscale(beta_pool$beta.jtu ~ Management, data = Management)
      anov_complete <- anova(mod_complete, permutations = 9999)
     
      dbrda_BIRDS_close_50km[i,1] <- anov_complete$'F'[1]
      dbrda_BIRDS_close_50km[i,2] <- anov_complete$'Pr(>F)'[1]
      dbrda_BIRDS_close_50km[i,3] <- RsquareAdj(mod_complete)$r.squared
      
      dbrda_BIRDS_close_50km[i,10] <- 0
      
    }else {
      mod_complete    <- capscale(formula= beta_pool$beta.jtu ~ PC1 + PC2 + Management , data = InfoMana_hab)
      mod_partial_PA  <- capscale(formula= beta_pool$beta.jtu  ~  Condition(PC1 + PC2 ) + Management , data = InfoMana_hab)
      mod_partial_hab <- capscale(formula= beta_pool$beta.jtu ~  Condition(Management) + PC1 + PC2 , data = InfoMana_hab)
      

      anov_complete    <- anova(mod_complete, permutations = 9999)
      anov_partial_PA  <- anova(mod_partial_PA, permutations = 9999)
      anov_partial_hab <- anova(mod_partial_hab, permutations = 9999)
      
      dbrda_BIRDS_close_50km[i,1] <- anov_complete$'F'[1]
      dbrda_BIRDS_close_50km[i,2] <- anov_complete$'Pr(>F)'[1]
      dbrda_BIRDS_close_50km[i,3] <- RsquareAdj(mod_complete)$r.squared
        
      dbrda_BIRDS_close_50km[i,4] <- anov_partial_PA$'F'[1]
      dbrda_BIRDS_close_50km[i,5] <- anov_partial_PA$'Pr(>F)'[1]
      dbrda_BIRDS_close_50km[i,6] <- RsquareAdj(mod_partial_PA)$r.squared
      
      dbrda_BIRDS_close_50km[i,7] <- anov_partial_hab$'F'[1]
      dbrda_BIRDS_close_50km[i,8] <- anov_partial_hab$'Pr(>F)'[1]
      dbrda_BIRDS_close_50km[i,9] <- RsquareAdj(mod_partial_hab)$r.squared
      
      dbrda_BIRDS_close_50km[i,10] <- 1
      
      if(dim(survey_SPA)[1]>0) InfoMPAcompared[i,1]<- 1
      if(dim(survey50km_NPA)[1]>0) InfoMPAcompared[i,2]<- 1
      if(dim(survey50km_RA)[1]>0) InfoMPAcompared[i,3]<- 1
  
    }
  } 
}
