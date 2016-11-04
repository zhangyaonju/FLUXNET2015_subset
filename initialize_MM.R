#This is the New Eddy flux processing code for FLUXNET2015 first release (2016 Jan).
#Only Subset dataset was used
#altogether 137 SITES
#developed by Yao ZHANG
#June 29th, 2016

initialize_MM<-function(rawdata){
  #get the year
  fdata<-rawdata
  YEAR<-floor(rawdata$TIMESTAMP/100)
  MONTH<-rawdata$TIMESTAMP-YEAR*100
  
  fdata[fdata==-9999]<-NA
  
  CLOUD<-rawdata$SW_IN_F/rawdata$SW_IN_POT
  EF<-rawdata$LE_CORR/(rawdata$H_CORR+rawdata$LE_CORR)
  fdata<-cbind(YEAR,MONTH,CLOUD,EF,fdata)
  
  data<-subset(fdata,select=c("YEAR",
                              "MONTH",
                              "TA_F",
                              "P_F",
                              "SW_IN_POT",
                              "SW_IN_F",
                              "SW_IN_F_QC",
                              "CLOUD",
                              "VPD_F",
                              "H_F_MDS",
                              "H_CORR",
                              "LE_F_MDS",
                              "LE_CORR",
                              "EF",
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





