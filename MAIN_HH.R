####FLUXNET_subset
####get data
####plot data
####main
####

#library(lattice)
#library(latticeExtra)
source('C:/Users/eomf/Dropbox/YAOZHANG/code/R-code/tools/FLUXNET_SUBSET/initialize_HH.R')
source('C:/Users/eomf/Dropbox/YAOZHANG/code/R-code/tools/FLUXNET_SUBSET/initialize_DD.R')
source('C:/Users/eomf/Dropbox/YAOZHANG/code/R-code/tools/FLUXNET_SUBSET/aggregate_DD2WK.R')
source('C:/Users/eomf/Dropbox/YAOZHANG/code/R-code/tools/FLUXNET_SUBSET/screen_HH.R')
source('C:/Users/eomf/Dropbox/YAOZHANG/code/R-code/tools/FLUXNET_SUBSET/graph_DD_WK.R')
source('C:/Users/eomf/Dropbox/YAOZHANG/code/R-code/tools/FLUXNET_SUBSET/HH_yearly_LRC_plot.R')

setwd('F:/data/FLUXNET_subset/')

dirs<-list.dirs('./',full.names = T)
n<-length(dirs)-2
site_list<-read.csv('./site.csv',stringsAsFactors = F)
site_list$SITE_ID<-substr(site_list$SITE_ID,1,6)

for (i in 1:n){
  #get the name and IGBP type
  site_name<-substr(dirs[i+1],8,13)
  site_IGBP<-site_list$IGBP[site_list$SITE_ID==site_name]
  
  #half-hour data
  HH_file<-list.files(dirs[i+1],pattern = '_HH_',full.names = T)
  if(length(HH_file)==0){
    next
  }
  HH_rawdata<-read.csv(HH_file)
  HH_fluxdata<-initialize_HH(HH_rawdata)
  
  #daily data
  DD_file<-list.files(dirs[i+1],pattern = '_DD_',full.names = T)
  DD_rawdata<-read.csv(DD_file)
  DD_fluxdata<-initialize_DD(DD_rawdata)
  
#   #7-day data
#   WW_file<-list.files(dirs[i+1],pattern = '_WW_',full.names = T)
#   WW_rawdata<-read.csv(WW_file)
#   WW_fluxdata<-initialize_WW(WW_rawdata)
  
  #8-day data
  WK_fluxdata<-aggregate_DD2WK(DD_fluxdata)
  
   ###################################################
  #start the graphing WK data
  #graph_DD_WK(DD_fluxdata,WK_fluxdata,site_name,site_IGBP)

  #data screen for HH data
  SC_HH_data<-screen_HH(HH_fluxdata,DD_fluxdata,WK_fluxdata)
  graph_HH(SC_HH_data,site_name,site_IGBP)
}


