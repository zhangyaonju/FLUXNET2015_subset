#This is the New Eddy flux analysis code for FLUXNET2015 first release (2016 Jan).
#Only Subset dataset was used
#altogether 137 SITES
#developed by Yao ZHANG
#April 19th, 2016

screen_WK<-function(WK_fluxdata,modisVI){
  date<-WK_fluxdata$YEAR*1000+WK_fluxdata$WEEK*8-7
  #data screening based on data quality
  VALID_R<-WK_fluxdata$SW_IN_F_QC>0.75
  VALID_C<-WK_fluxdata$NEE_VUT_REF_QC>0.75
  
  #cloudness
  CL<-WK_fluxdata$COULD>0.65
  
  #temperature
  TE<-(WK_fluxdata$TA_F>20)&(WK_fluxdata$TA_F<35)

  #Water
  WA<-WK_fluxdata$VPD_F<10
  
  ##altogether
  NS<-TE&WA&VALID_R&VALID_C
  
  DIFF_DT_NT<-abs((WK_fluxdata$GPP_NT_VUT_REF-WK_fluxdata$GPP_DT_VUT_REF)/
                    (WK_fluxdata$GPP_NT_VUT_REF+WK_fluxdata$GPP_DT_VUT_REF))
  DIFF_DT_NT[is.nan(DIFF_DT_NT)]<-NA
  GPP<-WK_fluxdata$GPP_NT_VUT_REF+WK_fluxdata$GPP_DT_VUT_REF
  
  #the nominated LUE
  
  LUE_nominated<-(WK_fluxdata$GPP_NT_VUT_REF+WK_fluxdata$GPP_DT_VUT_REF)/2/WK_fluxdata$SW_IN_F
  
  WK_flux<-cbind(date,WK_fluxdata,VALID_C,VALID_R,TE,WA,NS,DIFF_DT_NT,LUE_nominated,GPP)
  
  WK_data<-merge(WK_flux,modisVI,by="date",all.x=T)
}
