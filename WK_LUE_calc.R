
gt1<-function(x){
  if(!is.na(x)&x<0.1)
    x<-NA
  return(x)
}

calculate_LUE<-function(SC_WK_data,site_name,site_IGBP){
  adj_fpar<-0.2
  adj_evi<-0.1
  adj_ndvi<-0.2
  
  years<-unique(SC_WK_data$YEAR)
  
  n<-dim(SC_WK_data)[1]
  
  SC_WK_data$GPP_NT_VUT_REF[!SC_WK_data$NS|SC_WK_data$DIFF_DT_NT<0.15]<-NA
  
  ndviqa<-is.na(SC_WK_data$bise_ndvi)
  ndviqc<-ndviqa|c(ndviqa[n],ndviqa[1:(n-1)])|c(ndviqa[2:n],ndviqa[1])
  
  eviqa<-is.na(SC_WK_data$bise_evi)
  eviqc<-eviqa|c(eviqa[n],eviqa[1:(n-1)])|c(eviqa[2:n],eviqa[1])
  
  fparqa<-is.na(SC_WK_data$bise_fpar)
  fparqc<-fparqa|c(fparqa[n],fparqa[1:(n-1)])|c(fparqa[2:n],fparqa[1])
  
  SC_WK_data$gfbise_fpar[!fparqc]<-NA
  SC_WK_data$gfbise_ndvi[!ndviqc]<-NA
  SC_WK_data$gfbise_evi[!eviqc]<-NA
  SC_WK_data$GPP_NT_VUT_REF[SC_WK_data$GPP_NT_VUT_REF<0]<-NA
  SC_WK_data$SW_IN_F[SC_WK_data$SW_IN_F<0]<-NA
  LUE_fpar<-SC_WK_data$GPP_NT_VUT_REF/SC_WK_data$SW_IN_F/gt1(SC_WK_data$gfbise_fpar)
  adj_LUE_fpar<-SC_WK_data$GPP_NT_VUT_REF/SC_WK_data$SW_IN_F/gt1(SC_WK_data$gfbise_fpar-adj_fpar)
  
  LUE_ndvi<-SC_WK_data$GPP_NT_VUT_REF/SC_WK_data$SW_IN_F/gt1(SC_WK_data$gfbise_ndvi)
  adj_LUE_ndvi<-SC_WK_data$GPP_NT_VUT_REF/SC_WK_data$SW_IN_F/gt1(SC_WK_data$gfbise_ndvi-adj_ndvi)
  
  LUE_evi<-SC_WK_data$GPP_NT_VUT_REF/SC_WK_data$SW_IN_F/gt1(SC_WK_data$gfbise_evi)
  adj_LUE_evi<-SC_WK_data$GPP_NT_VUT_REF/SC_WK_data$SW_IN_F/gt1(SC_WK_data$gfbise_evi-adj_evi)
  
  data<-cbind(SC_WK_data[,c(1:3,10)], LUE_fpar,adj_LUE_fpar,LUE_ndvi, adj_LUE_ndvi, LUE_evi, adj_LUE_evi)
  write.csv(data,paste("F:/Project/2016/fpar/data/WK_LUE/",site_name,'_',site_IGBP,'.csv',sep=""))
}