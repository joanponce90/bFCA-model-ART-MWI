# Load necessary library
library(dplyr)
library(readr)
library(stats)

#1.WALK BIKE DRIVE SCENARIO-----

# Origin-destination file with healthcare facilities as columns (HCF) and population centroids as rows (Population_centroid_code)
#The origin-destination file contains the number of minutes it takes to travel from the population centroids to the HCFs
OD_mat_wbd.df<-data <- read.csv("Example_OD_1.csv", stringsAsFactors = FALSE)
#Naming rows
row.names(OD_mat_wbd.df)<-OD_mat_wbd.df$X
#removing first column
OD_mat_wbd.df$X<-NULL

Access_hourly<-function(max_hours, supply_quarterly){
  OD_mat_wbd <-OD_mat_wbd.df
  OD_mat_wbd[OD_mat_wbd >= max_hours] <- NA
  OD_mat_wbd_hours<-OD_mat_wbd/60
  #Distance decay function for the walk, bike, drive scenario
  DDF_WBD<-((1+exp(-0.7097641/0.5287538))/(1+exp((OD_mat_wbd_hours-0.7097641)/0.5287538)))
  
  # 2. W_ij=f(d_{ij})-----
  #WALK weights
  W_ij<-DDF_WBD
  
  #remove Nas
  W_ij[is.na(W_ij)]<-0
  
  #Normalize
  Wi_ij <- W_ij*(1/rowSums(W_ij))
  Wj_ij <- t(t(W_ij)*(1/colSums(W_ij)))
  
  #substituting NaNs: We use the normalized matrices
  Wi_ij0 <- Wi_ij
  Wi_ij0[is.na(Wi_ij0)]<-0
  
  Wj_ij0 <- Wj_ij
  Wj_ij0[is.na(Wj_ij0)]<-0
  
  Wi_ij<-Wi_ij0
  Wj_ij<-Wj_ij0
  
  # 3. Create the Dj vectors----
  #read in the population centroids and number of people data
  P_i<-as.data.frame(read.csv("PC_PLHIV.csv"))
  
  #Accessibility of the population WBD----
  Pi_WBD<-left_join(data.frame(id=row.names(Wi_ij)), P_i, by=c("id"="Population_centroid_code"))
  
  #Dj (demand) vector WBD
  Dj.vect_wbd <- t(Wi_ij) %*% Pi_WBD$Number_PLHIV
  
  # 4. Read the Sj vector (supply of each HCF)----
  Swbd_j0 <- as.data.frame(read_csv("HCF_supply.csv"))
  Swbd_j1<-left_join(data.frame(HCF_code=colnames(Wi_ij)), Swbd_j0, by=c("HCF_code"="HCF_code"))
  
  Access_t<-function(supplyq){
    Sj.vect_wbd <- as.numeric(Swbd_j1$ART_supply)
    Sj.vect_wbd[is.na(Sj.vect_wbd)]<-0
    
    # 5. Create the Lj (level of service) vector----
    Lj.vect_wbd <- Sj.vect_wbd/Dj.vect_wbd
    Lj.vect_wbd[is.infinite(Lj.vect_wbd)]<-0
    
    # 6. Accessibility------
    A.tot_WBD <- Wj_ij %*% Lj.vect_wbd
    A.tot_WBD.df<-data.frame(population_centroid_code=P_i[,1], number_PLHIV=P_i[,2],access=A.tot_WBD[,1])
    
    HCF_file_WBD.df<-data.frame(HCF_code=Swbd_j0[,1],ART_supply=Swbd_j0[,2], Lj=Lj.vect_wbd[,1], Dj=Dj.vect_wbd[,1])
    return(list(A.tot_WBD.df, HCF_file_WBD.df))
  }
  temp_access<-Access_t(supply_quarterly)
  return(temp_access)
}

#Accessibility index of population centroids to ART within 180 minutes of walking/biking or driving.
access_1h_all_WBD<-Access_hourly(180,'ART_supply')
