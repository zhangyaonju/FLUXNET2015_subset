####FLUXNET_subset
####get data
####plot data
####main
####

#library(lattice)
#library(latticeExtra)

source('C:/Users/eomf/Dropbox/YAOZHANG/code/R-code/tools/FLUXNET_SUBSET/initialize_DD.R')
source('C:/Users/eomf/Dropbox/YAOZHANG/code/R-code/tools/FLUXNET_SUBSET/aggregate_DD2WK.R')
source('C:/Users/eomf/Dropbox/YAOZHANG/code/R-code/tools/FLUXNET_SUBSET/aggregate_DD2DK.R')
source('C:/Users/eomf/Dropbox/YAOZHANG/code/R-code/tools/FLUXNET_SUBSET/screen_WK.R')
source('C:/Users/eomf/Dropbox/YAOZHANG/code/R-code/tools/FLUXNET_SUBSET/WK_LUE_calc.R')

setwd('F:/data/FLUXNET_subset/')

dirs<-list.dirs('./',full.names = T)
n<-length(dirs)-2
site_list<-read.csv('./site.csv',stringsAsFactors = F)
site_list$SITE_ID<-substr(site_list$SITE_ID,1,6)

for (i in 1:n){
  #get the name and IGBP type
  site_name<-substr(dirs[i+1],8,13)
  site_IGBP<-site_list$IGBP[site_list$SITE_ID==site_name]
  
  modfile<-list.files("F:/Project/2016/fpar/data/gapfilled/",pattern=site_name,full.names = T)
  if(length(modfile)==0){
    next
  }
  #daily data
  DD_file<-list.files(dirs[i+1],pattern = '_DD_',full.names = T)
  DD_rawdata<-read.csv(DD_file)
  DD_fluxdata<-initialize_DD(DD_rawdata)
  
  #8-day data
  WK_fluxdata<-aggregate_DD2WK(DD_fluxdata)
  
  #10-day data
  #this is only for the MTCI comparison, but we use another programme to
  #do the data screening in code/project/2016/fpar/MTCI_EC_VI_screen
  #DK_fluxdata<-aggregate_DD2DK(DD_fluxdata)
  
  #modis data
  modisVI<-read.csv(modfile)
  ###################################################
  
  #data screen for WK data
  SC_WK_data<-screen_WK(WK_fluxdata,modisVI)
  
  write.csv(SC_WK_data,paste("F:/Project/2016/fpar/data/WK_data/",site_name,'_',site_IGBP,'.csv',sep=""))
  #calculate_LUE(SC_WK_data,site_name,site_IGBP)
  #write.csv(DK_fluxdata,paste("F:/Project/2016/fpar/data/DK_data/",site_name,'_',site_IGBP,'.csv',sep=""))
}


