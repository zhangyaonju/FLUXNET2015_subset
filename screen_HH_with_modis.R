#initialize MODIS vegetation dataset

#This is the New Eddy flux analysis code for FLUXNET2015 first release (2016 Jan).
#Only Subset dataset was used
#altogether 137 SITES
#developed by Yao ZHANG
#APRIL 19th, 2016

screen_HH<-function(HH_fluxdata,DD_fluxdata,WK_fluxdata,modisdata){
  #data screening based on data quality
  VALID_R<-DD_fluxdata$SW_IN_F_QC>0.75
  VALID_C<-DD_fluxdata$NEE_VUT_REF_QC>0.75
  VALID_HH<-rep(VALID_R&VALID_C,each=48)
  
  #merge wk_data with MODISdata
  date<-WK_fluxdata$YEAR*1000+WK_fluxdata$WEEK*8-7
  WK_data<-merge(cbind(date,WK_fluxdata),modisdata,by="date",all.x=T)
  
  #cloudness
  CL_HH<-HH_fluxdata$SW_IN_F/HH_fluxdata$SW_IN_POT
  CL_HH<-ifelse(CL_HH>1,1,CL_HH)
  
  #temperature
  TE<-(DD_fluxdata$TA_F>20)&(DD_fluxdata$TA_F<35)
  TE_HH<-rep(TE,each=48)
  
  #Water
  WA<-DD_fluxdata$VPD_F<10
  WA_HH<-rep(WA,each=48)
  
  ##altogether
  NS<-TE_HH&WA_HH&VALID_HH
  
  ##
  
  #GPP from DT and NT
  #consistency of DT and NT method, only 8day GPP within 20% difference are used
  DIFF_DT_NT<-abs((WK_fluxdata$GPP_NT_VUT_REF-WK_fluxdata$GPP_DT_VUT_REF)/
                    (WK_fluxdata$GPP_NT_VUT_REF+WK_fluxdata$GPP_DT_VUT_REF))
  DIFF_DT_NT[is.nan(DIFF_DT_NT)]<-NA
  DIFF_GPP_WK<-data.frame(WK_fluxdata$YEAR,DIFF_DT_NT,WK_data$bise_evi,WK_data$bise_ndvi,WK_data$bise_fpar)
  names(DIFF_GPP_WK)<-c("YEAR","DIFF_DT_NT","BISE_EVI","BISE_NDVI","BISE_FPAR")
  years<-unique(WK_fluxdata$YEAR)
  DIFF_GPP<-array(NA,dim = c(length(CL_HH)))
  BISE_EVI<-array(NA,dim = c(length(CL_HH)))
  BISE_NDVI<-array(NA,dim = c(length(CL_HH)))
  BISE_FPAR<-array(NA,dim = c(length(CL_HH)))
  HH_time<-data.frame(HH_fluxdata$YEAR,DIFF_GPP,BISE_EVI,BISE_NDVI,BISE_FPAR)
  
  names(HH_time)<-c("YEAR","DIFF_GPP","BISE_EVI","BISE_NDVI","BISE_FPAR")
  for (i in 1:(length(years))){
    HH_time$DIFF_GPP[HH_time$YEAR==years[i]][1:17280]=
      rep(DIFF_GPP_WK$DIFF_DT_NT[DIFF_GPP_WK$YEAR==years[i]][1:45],each=384)
    HH_time$BISE_EVI[HH_time$YEAR==years[i]][1:17280]=
      rep(DIFF_GPP_WK$BISE_EVI[DIFF_GPP_WK$YEAR==years[i]][1:45],each=384)
    HH_time$BISE_NDVI[HH_time$YEAR==years[i]][1:17280]=
      rep(DIFF_GPP_WK$BISE_NDVI[DIFF_GPP_WK$YEAR==years[i]][1:45],each=384)
    HH_time$BISE_FPAR[HH_time$YEAR==years[i]][1:17280]=
      rep(DIFF_GPP_WK$BISE_FPAR[DIFF_GPP_WK$YEAR==years[i]][1:45],each=384)
    w46<-length(HH_time$YEAR[HH_time$YEAR==years[i]])
    HH_time$DIFF_GPP[HH_time$YEAR==years[i]][17281:w46]=
      rep(DIFF_GPP_WK$DIFF_DT_NT[DIFF_GPP_WK$YEAR==years[i]][46],each=w46-17280)
    HH_time$BISE_EVI[HH_time$YEAR==years[i]][17281:w46]=
      rep(DIFF_GPP_WK$BISE_EVI[DIFF_GPP_WK$YEAR==years[i]][46],each=w46-17280)
    HH_time$BISE_NDVI[HH_time$YEAR==years[i]][17281:w46]=
      rep(DIFF_GPP_WK$BISE_NDVI[DIFF_GPP_WK$YEAR==years[i]][46],each=w46-17280)
    HH_time$BISE_FPAR[HH_time$YEAR==years[i]][17281:w46]=
      rep(DIFF_GPP_WK$BISE_FPAR[DIFF_GPP_WK$YEAR==years[i]][46],each=w46-17280)
  }
  
  DIFF_GPP_HH<-HH_time$DIFF_GPP
  BISE_EVI<-HH_time$BISE_EVI
  BISE_NDVI<-HH_time$BISE_NDVI
  BISE_FPAR<-HH_time$BISE_FPAR
  
  HH_AVA<-cbind(HH_fluxdata,CL_HH,NS,VALID_HH,TE_HH,WA_HH,DIFF_GPP_HH,BISE_EVI,BISE_NDVI,BISE_FPAR)
  HH_GRAPH<-subset(HH_AVA,select=c("PDATE",
                                   "WEEK",
                                   "YEAR",
                                   "TIME",
                                   "SW_IN_F",
                                   "NIGHT",
                                   "GPP_NT_VUT_REF",
                                   "CL_HH",
                                   "NS",
                                   "VALID_HH",
                                   "TE_HH",
                                   "WA_HH",
                                   "DIFF_GPP_HH",
                                   "BISE_EVI",
                                   "BISE_NDVI",
                                   "BISE_FPAR"))
  return(HH_GRAPH)
}

