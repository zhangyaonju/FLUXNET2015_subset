#This is the New Eddy flux processing code for FLUXNET2015 first release (2016 Jan).
#Only Subset dataset was used
#altogether 137 SITES
#developed by Yao ZHANG
#March 15th, 2016

aggregate_DD2WK<-function(DD_data){
  years<-unique(DD_data$YEAR)
  n_years<-length(years)
  WK_data<-as.data.frame(array(NA,dim=c(n_years*46,33)))
  WK_data[,3]<-as.Date(WK_data[,3])
  
  for (i in 1:n_years){
    yr_data<-DD_data[DD_data$YEAR==years[i],]
    days<-dim(yr_data)[1]
    NA_fill<-array(NA,dim = c(368-days,dim(yr_data)[2]))
    yr_data[(days+1):368,]<-NA_fill
    yr_array<-as.numeric(as.matrix(yr_data))
    dim(yr_array)<-c(8,46,dim(yr_data)[2])
    yr_8d_data<-apply(yr_array,c(2,3),mean,na.rm=T)
    yr_8d_data<-as.data.frame(yr_8d_data)
    names(yr_8d_data)<-names(yr_data)
    yr_8d_data<-yr_8d_data[,-c(2,4)]
    yr_8d_data$PDATE<-yr_data$PDATE[1:46*8-7]
    WK_data[(i*46-45):(i*46),]<-yr_8d_data
  }
  names(WK_data)<-names(yr_8d_data)
  WK_data[,3]<-as.POSIXct(WK_data[,3])
  return(WK_data)
}









