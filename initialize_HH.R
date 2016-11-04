#This is the New Eddy flux processing code for FLUXNET2015 first release (2016 Jan).
#Only Subset dataset was used
#altogether 137 SITES
#developed by Yao ZHANG
#March 10th, 2016

initialize_HH<-function(rawdata){
  #get the year
  fdata<-rawdata
  YEAR<-floor(rawdata$TIMESTAMP_START/1e8)
  MONTH<-floor(rawdata$TIMESTAMP_START/1e6)-YEAR*100
  DATE<-floor(rawdata$TIMESTAMP_START/1e4)-YEAR*10000-MONTH*100
  TIME<-round(rawdata$TIMESTAMP_START%%1e4/50)/2
  PDATE<-ISOdate(YEAR,MONTH,DATE,floor(TIME),TIME%%1*30,00)
  WEEK<-ceiling((as.POSIXlt(PDATE)$yday+1)/8)
  
  fdata[fdata==-9999]<-NA
  
  CLOUD<-rawdata$SW_IN_F/rawdata$SW_IN_POT
  fdata<-cbind(YEAR,MONTH,WEEK,DATE,TIME,PDATE,CLOUD,fdata)
  
  
  data<-subset(fdata,select=c("YEAR",
                             "MONTH",
                             "WEEK",
                             "DATE",
                             "TIME",
                             "PDATE",
                             "TA_F",
                             "P_F",
                             "SW_IN_POT",
                             "SW_IN_F",
                             "SW_IN_F_QC",
                             "VPD_F",
                             "H_F_MDS",
                             "H_CORR",
                             "LE_F_MDS",
                             "LE_CORR",
                             "NIGHT",
                             "NEE_VUT_REF",
                             "NEE_VUT_REF_QC",
                             "NEE_VUT_25",
                             "NEE_VUT_50",
                             "NEE_VUT_75",
                             "RECO_NT_VUT_REF",
                             "RECO_NT_VUT_25",
                             "RECO_NT_VUT_50",
                             "RECO_NT_VUT_75",
                             "GPP_NT_VUT_REF",
                             "GPP_NT_VUT_25",
                             "GPP_NT_VUT_50",
                             "GPP_NT_VUT_75",
                             "GPP_DT_VUT_REF",
                             "GPP_DT_VUT_25",
                             "GPP_DT_VUT_50",
                             "GPP_DT_VUT_75")
  )
  return(data)
}
