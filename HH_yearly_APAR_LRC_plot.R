#This is the New Eddy flux GRAPHING code for FLUXNET2015 first release (2016 Jan).
#plot the light response curve for screened HH data
#Only Subset dataset was used
#altogether 137 SITES
#developed by Yao ZHANG
#April 19th, 2016

nls_plot<-function(wk_graph,p){

  #####for cloud
  nc_data<-wk_graph[!is.na(wk_graph$CL_HH)&wk_graph$CL_HH>0.65,]
  cl_data<-wk_graph[!is.na(wk_graph$CL_HH)&wk_graph$CL_HH<=0.65,]
  #####regression
  s <- seq(from = 0, to = 1200, length = 50)
  
  resul<-array(NA,dim=c(8,1))
  plot(wk_graph$APAR,wk_graph$GPP_NT_VUT_REF,type="n",
       xlab="",ylab="GPP",main = wk_graph$WEEK[1], xlim=c(0,1200),ylim=c(0,60))
  points(cl_data$APAR,cl_data$GPP_NT_VUT_REF,col='blue')
  points(nc_data$APAR,nc_data$GPP_NT_VUT_REF,col='red')
  
  remove(reg)
  if (length(nc_data$APAR)>30){
    tryCatch({
      reg<-nls(GPP_NT_VUT_REF~e0*APAR*gppm/(e0*APAR+gppm),
               data=nc_data,
               start=list(e0=0.025,gppm=30),
               model=TRUE,
               control = nls.control(minFactor = 1/1024),
               trace=TRUE)
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
    if (exists('reg')){
      resul[1]<-summary(reg)$parameters[1,1]
      resul[2]<-summary(reg)$parameters[2,1]
      resul[3]<-summary(reg)$parameters[1,4]
      resul[4]<-summary(reg)$parameters[2,4]
      lines(s, predict(reg, list(APAR = s)), col = "red")
      text(0,58,pos=4,paste('e0=',round(resul[1],digits = 4),'  gppm=',round(resul[2],digits = 4),sep=''),col='red')
    }
  }
  
  remove(reg)
  if (length(cl_data$APAR)>30){
    tryCatch({
      reg<-nls(GPP_NT_VUT_REF~e0*APAR*gppm/(e0*APAR+gppm),
               data=cl_data,
               start=list(e0=0.025,gppm=30),
               model=TRUE,
               control = nls.control(minFactor = 1/1024),
               trace=TRUE)
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
    if (exists('reg')){
      resul[5]<-summary(reg)$parameters[1,1]
      resul[6]<-summary(reg)$parameters[2,1]
      resul[7]<-summary(reg)$parameters[1,4]
      resul[8]<-summary(reg)$parameters[2,4]
      lines(s, predict(reg, list(APAR = s)), col = "blue")
      text(0,54,pos=4,paste('e0=',round(resul[5],digits = 4),'  gppm=',round(resul[6],digits = 4),sep=''),col='blue')
    }
  }
  return(resul)
}




graph_HH<-function(SC_HH_data,site_name,site_IGBP){
  prod<-c("evi","ndvi","fpar")
  for (p in 1:3){
    years<-unique(SC_HH_data$YEAR)
    reg_data<-as.data.frame(array(NA,dim=c(length(years)*46,8)))
    year<-rep(years,each=46)
    week<-rep(1:46,length(years))
    names(reg_data)<-c("e0_no_cloud","gppm_no_cloud","p1_no_cloud","p2_no_cloud",
                       "e0_cloud","gppm_cloud","p1_cloud","p2_cloud")
    
    for (y in 1:length(years)){
      valid_HH<-SC_HH_data[SC_HH_data$YEAR==years[y],]
      if(sum(valid_HH$NS==1&!is.na(valid_HH[,13+p]),na.rm=T)<384){
        next
      }
      pdf(paste('F:/data/FLUXNET_subset_HH_GRAPH/',prod[p],'/',site_name,'_',site_IGBP,' ',years[y],'.pdf',sep=''),
          width=13,height=20)
      par(mar=c(2,4,3,1)+.1)
      layout(matrix(c(rep(1,6),rep(2,6),3:50), 10, 6, byrow = TRUE), 
             widths=c(rep(1,6)), heights=c(1,1,rep(2,8)))
      
      
      ##data quality
      plot(valid_HH$PDATE,valid_HH$NS,type='s',col='black',
           xlab="",ylab="data quality",main = paste(site_name,'-',site_IGBP),ylim=c(-0.2,1.2))
      lines(valid_HH$PDATE,valid_HH$VALID_HH,col='red',lty=2)
      lines(valid_HH$PDATE,valid_HH$TE_HH,col='orange',lty=3)
      lines(valid_HH$PDATE,valid_HH$WA_HH,col='blue',lty=4)
      plot(valid_HH$PDATE,valid_HH$DIFF_GPP_HH,type='s',col='black',
           xlab="",ylab="GPP DT NT difference",ylim=c(-0.1,0.5))
      lines(valid_HH$PDATE[c(1,length(valid_HH$PDATE))],c(0.1,0.1),col='red',lty=2)
      APAR<-valid_HH$SW_IN_F*valid_HH[,13+p]
      valid_HH<-cbind(valid_HH,APAR)
      #plot 8-day
      for (w in 1:46){
        wk_valid<-valid_HH[valid_HH$WEEK==w,]
        flag<-wk_valid$NS&(wk_valid$DIFF_GPP_HH<0.1)&!(wk_valid$NIGHT)&(!is.na(wk_valid[,13+p]))
        wk_graph<-wk_valid[ifelse(is.na(flag),FALSE,flag),]
        
        if (dim(wk_graph)[1]<100){
          plot(wk_graph$SW_IN_F,wk_graph$GPP_NT_VUT_REF,type='n',col='black',
               xlab="",ylab="",xlim=c(0,1200),ylim=c(0,60),main = w)
        }else{
          ####different cloud no-cloud
          reg_data[46*y-46+w,]<-nls_plot(wk_graph,p)
        }
      }
      dev.off()
    }
    reg_data<-cbind(year,week,reg_data)
    write.csv(reg_data,paste('F:/data/FLUXNET_subset_HH_stat/',prod[p],'/',site_name,'_',site_IGBP,' ','.csv',sep='')) 
  }
  
}




