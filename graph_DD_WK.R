#This is the New Eddy flux graphing code for FLUXNET2015 first release (2016 Jan).
#Only Subset dataset was used
#altogether 137 SITES
#developed by Yao ZHANG
#March 15th, 2016

graph_DD_WK<-function(DD_data,WK_data,name,IGBP){
  pdf(paste('F:/data/FLUXNET_subset_GRAPH/',name,'_',IGBP,'.pdf',sep=''),width=9,height=15)
  par(mfrow=c(6,1),mar=c(1,4,2,5)+.1)
  
  #T&P
  plot(DD_data$PDATE,DD_data$TA_F,cex=0.5,col='red',
       xlab="",ylab="Temperature [deg C]",main = paste(name,'-',IGBP))
  lines(WK_data$PDATE,WK_data$TA_F,col='red')
  lines(WK_data$PDATE,rep(20,times=length(WK_data$PDATE)),lty=2,col='grey')
  lines(WK_data$PDATE,rep(35,times=length(WK_data$PDATE)),lty=2,col='grey')
  par(new=TRUE)
  barplot(DD_data$P_F,col="blue",border = NA,xaxt="n",yaxt="n",xlab="",ylab="")
  axis(4)
  mtext("Precipitation [mm/day]",side=4,line=3,cex=0.7)
  
  #radiation
  plot(DD_data$PDATE,DD_data$SW_IN_F,cex=0.5,col='red',xlab="",ylab="Radiation [W m-2]")
  lines(WK_data$PDATE,WK_data$SW_IN_F,col='red')
  par(new=TRUE)
  plot(WK_data$PDATE,WK_data$SW_IN_F_QC,col="grey",xaxt="n",yaxt="n",type = 's',
          xlab="",ylab="",ylim=c(0,3))
  lines(DD_data$PDATE,DD_data$CLOUD,col="blue")
  lines(WK_data$PDATE,rep(0.7,times=length(WK_data$PDATE)),lty=2,col='grey')
  axis(4)
  mtext("Data quality",side=4,line=3,cex=0.7)
  legend("topleft",col=c("red","blue","grey"),lty=1,legend=c("Radiation","Cloud","data quality"))
  
  #VPD
  plot(DD_data$PDATE,DD_data$VPD_F,cex=0.5,col='red',xlab="",ylab="VPD [hPa]")
  lines(WK_data$PDATE,WK_data$VPD_F,col='red')
  par(new=TRUE)
  plot(WK_data$PDATE,WK_data$SWC_F_MDS_1,col="blue",xaxt="n",yaxt="n",type = 'l',
       xlab="",ylab="",ylim=c(0,120))
  axis(4)
  mtext("Soil water content",side=4,line=3,cex=0.7)
  legend("topleft",col=c("red","blue"),lty=1,legend=c("VPD","SWC"))
  
  #LE
  plot(DD_data$PDATE,DD_data$LE_F_MDS,cex=0.5,col='red',xlab="",ylab="LE [W m-2]")
  lines(WK_data$PDATE,WK_data$LE_F_MDS,col='red')
  points(DD_data$PDATE,DD_data$LE_CORR,cex=0.5,col='blue')
  lines(WK_data$PDATE,WK_data$LE_CORR,col='blue')
  par(new=TRUE)
  plot(WK_data$PDATE,WK_data$EF,col='black',xaxt="n",yaxt="n",type = 'l',
       xlab="",ylab="",ylim=c(0,2))
  axis(4)
  mtext("EF",side=4,line=3,cex=0.7)
  legend("topleft",col=c("red","blue","black"),lty=1,legend=c("LE_MDS","LE_CORR","EF"))
  
  #GPP
  plot(DD_data$PDATE,DD_data$GPP_NT_VUT_REF,cex=0.5,col='red',xlab="",ylab="GPP [g C m-2 d-1]")
  lines(WK_data$PDATE,WK_data$GPP_NT_VUT_REF,col='red')
  points(DD_data$PDATE,DD_data$GPP_DT_VUT_REF,cex=0.5,col='blue')
  lines(WK_data$PDATE,WK_data$GPP_DT_VUT_REF,col='blue')
  par(new=TRUE)
  plot(WK_data$PDATE,WK_data$NEE_VUT_REF_QC,col="grey",xaxt="n",yaxt="n",type = 's',
       xlab="",ylab="",ylim=c(0,3))
  axis(4)
  mtext("Data quality",side=4,line=3,cex=0.7)
  legend("topleft",col=c("red","blue","grey"),lty=1,legend=c("GPP_NT","GPP_DT","data quality"))
  
  
  #GPP
  plot(WK_data$PDATE,WK_data$GPP_NT_VUT_REF,type='l',col='red',xlab="",ylab="GPP [g C m-2 d-1]")
  lines(WK_data$PDATE,WK_data$GPP_NT_VUT_25,col='blue')
  lines(WK_data$PDATE,WK_data$GPP_NT_VUT_50,col='brown')
  lines(WK_data$PDATE,WK_data$GPP_NT_VUT_75,col='green')
  lines(WK_data$PDATE,WK_data$GPP_DT_VUT_REF,lty=2,col='red')
  lines(WK_data$PDATE,WK_data$GPP_DT_VUT_25,lty=2,col='blue')
  lines(WK_data$PDATE,WK_data$GPP_DT_VUT_50,lty=2,col='brown')
  lines(WK_data$PDATE,WK_data$GPP_DT_VUT_75,lty=2,col='green')
  
  legend("topleft",col=c("red","blue",'brown','green','black'),
         lty=c(1,1,1,1,2),legend=c("GPP_REF","GPP_25","GPP_50","GPP_75","GPP_DT"))
  
  dev.off()
}




