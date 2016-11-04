#This is the New Eddy flux processing code for FLUXNET2015 first release (2016 Jan).
#Only Subset dataset was used
#altogether 137 SITES
#developed by Yao ZHANG
#July 4th, 2016

aggregate_DD2DK<-function(DD_data){
  years<-unique(DD_data$YEAR)
  n_years<-length(years)
  DD_data<-DD_data[,-5]
  DK_data<-as.data.frame(array(NA,dim=c(n_years*36,34)))
  #DK_data[,3]<-as.Date(DK_data[,3])
  
  for (i in 1:n_years){
    yr_data<-DD_data[DD_data$YEAR==years[i],]
    dekad<-ceiling(yr_data$DATE/10)
    dekad[dekad>3]<-3
    for (m in 1:12){
      mn_data<-yr_data[yr_data$MONTH==m,]
      for (d in 1:3){
        de_data<-as.matrix(mn_data[dekad==d,])
        DK_data[(i-1)*36+(m-1)*3+d,]<-apply(de_data,2,mean,na.rm=T)
      }
    }
  }
  names(DK_data)<-names(DD_data)
  return(DK_data)
}


