#This is the New Eddy flux processing code for FLUXNET2015 first release (2016 Jan).
#Only Subset dataset was used
#altogether 137 SITES
#developed by Yao ZHANG
#March 15th, 2016

initialize_DD<-function(rawdata){
  #get the year
  fdata<-rawdata
  YEAR<-floor(rawdata$TIMESTAMP/1e4)
  MONTH<-floor(rawdata$TIMESTAMP/1e2)-YEAR*100
  DATE<-rawdata$TIMESTAMP%%100
  PDATE<-ISOdate(YEAR,MONTH,DATE)
  WEEK<-ceiling((as.POSIXlt(PDATE)$yday+1)/8)
  
  fdata[fdata==-9999]<-NA
  
  CLOUD<-rawdata$SW_IN_F/rawdata$SW_IN_POT
  EF<-rawdata$LE_CORR/(rawdata$H_CORR+rawdata$LE_CORR)
  fdata<-cbind(YEAR,MONTH,WEEK,DATE,PDATE,CLOUD,EF,fdata)
  if (length(fdata$SWC_F_MDS_1)==0){
    SWC_F_MDS_1<-array(0,dim=c(dim(fdata)[1],1))
    fdata<-cbind(fdata,SWC_F_MDS_1)
  }
  
  DD_data<-subset(fdata,select=c("YEAR",
                                 "MONTH",
                                 "WEEK",
                                 "DATE",
                                 "PDATE",
                                 "TA_F",
                                 "P_F",
                                 "SW_IN_POT",
                                 "SW_IN_F",
                                 "SW_IN_F_QC",
                                 "CLOUD",
                                 "VPD_F",
                                 "SWC_F_MDS_1",
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
  return(DD_data)
}
  
  