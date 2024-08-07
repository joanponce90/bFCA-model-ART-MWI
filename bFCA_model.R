require(ggplot2)
require(matrixStats)
library(RColorBrewer)
require(scales)
library(tidyverse)
library(readxl)
source("/Users/joanponce/Desktop/FCA/FILES FOR JOAN/Libraries.R")

#Distance decay
##-----------------------DISTANCE DECAY-----------------------
#1.WALK BIKE DRIVE SCENARIO-----
#importing HCF info
HCF<-read.csv("MWI_HCF_max.csv") #HCF
#import EA info
EA_pop_cent_mod1<-read.csv("pop_cent_MWI.csv")

#Read the OD matrix
#OD matrix: cols=HCF, row=EACODE
OD_mat_wbd<-as.data.frame(read.csv("OD_WBD_3h_altered.csv"))

#renaming rows
rownames(OD_mat_wbd.df)<-as.vector(OD_mat_wbd.df[,1])
OD_mat_wbd.df<-OD_mat_wbd.df[,-1]

#counting which rows have all NAs (isolated EAs)
missing_rows_OD_mat_wbd<-which(Reduce(`&`,as.data.frame(is.na(OD_mat_wbd.df))))
isolated_EA_codes_WBD<-row.names(OD_mat_wbd.df)[missing_rows_OD_mat_wbd]

#Min distance to HCFs from EAs
rowmins_ODmat_wbd<-apply(OD_mat_wbd.df, 1, function(x){min(x, na.rm = TRUE)})

#removing Nas in rows and cols from the OD matrix
OD_mat_wbd0<-OD_mat_wbd.df[ , colSums(is.na(OD_mat_wbd.df)) != nrow(OD_mat_wbd.df)]

#OD MAT without any isolated rows/cols
OD_mat_wbd1<-OD_mat_wbd0[rowSums(is.na(OD_mat_wbd0)) != ncol(OD_mat_wbd0), ]

rowmins_ODmat_WBD1<-apply(OD_mat_wbd1, 1, function(x){min(x, na.rm = TRUE)})

Access_hourly<-function(max_hours, supply_quarterly){
  OD_mat_wbd <-OD_mat_wbd1
  OD_mat_wbd[OD_mat_wbd >= max_hours] <- NA
  rowmins_ODmat_WBD<-apply(OD_mat_wbd, 1, function(x){min(x, na.rm = TRUE)})
  
  #Distance decay function
  OD_mat_wbd_hours<-OD_mat_wbd/60
  DDF_WBD<-((1+exp(-0.7097641/0.5287538))/(1+exp((OD_mat_wbd_hours-0.7097641)/0.5287538)))

  #WALK weights
  W_ij<-DDF_WBD
  
  #remove Nas
  W_ij[is.na(W_ij)]<-0
  
  #Normalize
  Wi_ij <- W_ij*(1/rowSums(W_ij))
  Wj_ij <- t(t(W_ij)*(1/colSums(W_ij)))
  
  #substituting NaNs:
  Wi_ij0 <- Wi_ij
  Wi_ij0[is.na(Wi_ij0)]<-0
  
  Wj_ij0 <- Wj_ij
  Wj_ij0[is.na(Wj_ij0)]<-0
  
  Wi_ij<-Wi_ij0
  Wj_ij<-Wj_ij0
  
  # 3. Create the Dj vectors----
  #Obtain the population data and distribute the percentages to each mode of transportation
  p_i0<-readOGR("pop_cent.shp")
  P_i<-as.data.frame(p_i0@data)
  
  #Accessibility of the population WBD----
  Pi_WBD<-left_join(data.frame(id=row.names(Wi_ij)), P_i, by=c("id"="EACODE"))
  
  #Dj vector WBD
  Dj.vect_wbd <- t(Wi_ij) %*% Pi_WBD$NUM_PLHIV
  
  # 4. Read the Sj vector----
  Swbd_j0 <- as.data.frame(read.csv("MWI_HCF_max.csv"))
  Swbd_j1<-left_join(data.frame(id1=colnames(Wi_ij)), Swbd_j0, by=c("id1"="Code"))
  
  Access_t<-function(supplyq){
    Sj.vect_wbd <- as.numeric(Swbd_j1[[supplyq]])
    Sj.vect_wbd[is.na(Sj.vect_wbd)]<-0
    
    # 5. Create the Lj vector----
    Lj.vect_wbd <- Sj.vect_wbd/Dj.vect_wbd
    Lj.vect_wbd[is.infinite(Lj.vect_wbd)]<-0
    
    # 6. Accessibility------
    A.tot_WBD <- Wj_ij %*% Lj.vect_wbd
    #plot
    EA_file<-readOGR("ECHOS_prioritization_mdf_fixedgeom.shp")
    A.tot_WBD.df<-data.frame(EACODE=row.names(A.tot_WBD), access=A.tot_WBD[,1])
    EA_file_Access_WBD<-merge(EA_file, A.tot_WBD.df, by.x = "EACODE", by.y = "EACODE", all.x=T)
    
    HCF_shp<-readOGR("MWI_HCF.shp")
    Lj_WBD.df<-data.frame(HCF_code=row.names(Lj.vect_wbd), Lj=Lj.vect_wbd[,1], Dj=Dj.vect_wbd[,1])
    HCF_file_Lj_WBD<-merge(HCF_shp, Lj_WBD.df, by.x = "Code", by.y = "HCF_code", all.x=T)
    
    return(list(HCF_file_Lj_WBD, EA_file_Access_WBD))
  }
  temp_access<-Access_t(supply_quarterly)
  return(temp_access)
}

access_1h_all_WBD<-Access_hourly(60,'max_supply_2020')
Lj_1h<-access_1h_all_WBD[[1]]
access_1h<-access_1h_all_WBD[[2]]
