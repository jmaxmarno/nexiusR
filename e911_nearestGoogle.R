# Can't figure out how to get geocoder to run consecutively for all iterations (perhaps exceeding limit per minute/second???)



# MAX MARNO
# 7/126/2017
# WORK FOR NEXIUS/SUNDELL/Liz
# closest google searchable address
# 
rm(list=ls())
require(ggmap)
require(sp)
require(readxl)
require(dplyr)
require(stringi)
#devtools::install_github("dkahle/ggmap")
projws= "C:/Users/jmaxm/Documents/Projects/Nexius/nexiusR"
setwd(projws)
gclientid<-"REMOVED"
gsecret<- "REMOVED"
register_google(key = 'REMOVED')

sites.w.revgeo<- read.csv("C:/Users/jmaxm/Documents/Projects/Nexius/nexiusR/sites.df.wRevGeo.csv", stringsAsFactors = FALSE)
sites.df<- read.csv("C:/Users/jmaxm/Documents/Projects/Nexius/LizPeterson_07_27/e911datacopy.csv", stringsAsFactors = FALSE)
#sites.df<- read.csv("C:/Users/jmaxm/Documents/Projects/Nexius/nexiusR/workinprogress.csv", stringsAsFactors = FALSE)
#sites.df<- sites.df[10:nrow(sites.df)] #DON'T FORGET COMMA!!!
for(i in 1:nrow(sites.df)){
  sites.df$RevGeoAddr[i]<- stri_trans_general(revgeocode(location = c(sites.df$longitude[i], sites.df$latitude[i]), override_limit = TRUE, nameType='long'), "Latin-ASCII")
  #Sys.sleep(.5)
  AddrCoords<- geocode(location = toString(sites.df$RevGeoAddr[i]), override_limit = TRUE, nameType = 'long')
  #Sys.sleep(.5)
  sites.df$RevGeoAddrLat[i]<- AddrCoords$lat
  sites.df$RevGeoAddrLon[i]<- AddrCoords$lon
  #MEASURE DIST BETWEEEN REAL SITE COORDINATES, AND REVGEO>GEOCODED ADDRESS COORDINATES
  coords<- matrix(c(sites.df$longitude[i], sites.df$latitude[i]), nrow = 1)
  addcoords<- matrix(c(sites.df$RevGeoAddrLon[i], sites.df$RevGeoAddrLat[i]), nrow = 1)
  ddist<- spDistsN1(addcoords, coords, longlat = TRUE)
  sites.df$DIST2RevGeo[i]<- 1000*ddist
  print(toString(i))
}
for(i in 1:nrow(sites.df)){
  # sites.df$RevGeoAddr[i]<- stri_trans_general(revgeocode(location = c(sites.df$longitude[i], sites.df$latitude[i]), override_limit = TRUE, nameType='long'), "Latin-ASCII")
  # #Sys.sleep(.5)
  # AddrCoords<- geocode(location = toString(sites.df$RevGeoAddr[i]), override_limit = TRUE, nameType = 'long')
  # #Sys.sleep(.5)
  # sites.df$RevGeoAddrLat[i]<- AddrCoords$lat
  # sites.df$RevGeoAddrLon[i]<- AddrCoords$lon
  
  # MEASURE DIST BETWEEEN REAL SITE COORDINATES, AND REVGEO>GEOCODED ADDRESS COORDINATES
  coords<- matrix(c(sites.df$longitude[i], sites.df$latitude[i]), nrow = 1)
  addcoords<- matrix(c(sites.df$RevGeoAddrLon[i], sites.df$RevGeoAddrLat[i]), nrow = 1)
  ddist<- spDistsN1(addcoords, coords, longlat = TRUE)
  sites.df$DIST2RevGeo[i]<- 1000*ddist
  print(toString(i))
}



########################################################################################
# LOOP TO RETURN SINGLE ADDRESS INSTEAD OF RANGE
# try with already created revgeo csv


sites.df<- sites.w.revgeo
for (i in 1:nrow(sites.df)){
  asite<- sites.df[i,]
  aaddr<- asite$RevGeoAddr
  odist<- asite$DIST2RevGeo
  coords<- matrix(c(asite$longitude, asite$latitude), nrow = 1)
  splitaddr<- unlist(strsplit(aaddr, ' '))
  rootaddr<- splitaddr[2:length(splitaddr)]
  rootaddr<- paste(rootaddr, collapse = " ")
  print(rootaddr)
  if (grepl("-",splitaddr[1])){
    addrange<- unlist(strsplit(splitaddr[1], "-"))
    print(addrange)
    rmin<-  as.numeric(addrange[1])
    rmax<- as.numeric(addrange[2])
    rdiff<- rmax-rmin
    rrange<- c(rmin:rmax)
    radds<- sapply(rrange, function(x) stri_trans_general(paste(toString(x), rootaddr), "Latin-ASCII"))
    multi.fun<- function(x){
      # NOT SURE IF SLEEP/DELAY IS REALLY NECESSARY...
      #Sys.sleep(.3)
      unlist(geocode(location = x, override_limit = TRUE))
    }
    rrevgeo<- lapply(radds, multi.fun)
    #rrevgeo<- lapply(radds, function(x) unlist(geocode(location = x, override_limit = TRUE)))
    # rcoords<- lapply(rrevgeo, function(x) c(unlist(x)[1], unlist(x)[2]))
    ddists<- unlist(lapply(rrevgeo, function(x) spDistsN1(matrix(c(unlist(x)[1], unlist(x)[2]), nrow=1), coords, longlat = TRUE )))
    # ddists<- lapply(rcoords, function(x) spDistsN1(matrix(c(x[1], x[2]), nrow=1), coords, longlat = TRUE ))
    range.df<- data.frame(addressess=radds, distances = ddists)
    mindistaddress<- range.df[which(range.df$distances==min(range.df$distances)),]$addressess
    mindistdist<- range.df[which(range.df$distances==min(range.df$distances)),]$distances
    sites.df$e911address[i]<- toString(mindistaddress)
    sites.df$e911dist[i]<- mindistdist*1000
  } else {
    sites.df$e911address[i]<- NA
    sites.df$e911dist[i]<- NA
  }
}


"4922 Stonehaven Dr, Yorba Linda, CA 92887, USA"
"5097 Village Center Dr, Yorba Linda, CA 92886, USA" 
"5070 Village Center Dr, Yorba Linda, CA 92886, USA"


## Loop through again to find ones that the first missed for some fucking reason 
sites.df<- read.csv('workinprogress.csv', stringsAsFactors = FALSE)
for (i in 1:nrow(sites.df)){
  asite<- sites.df[i,]
  aaddr<- asite$RevGeoAddr
  odist<- asite$DIST2RevGeo
  coords<- matrix(c(asite$longitude, asite$latitude), nrow = 1)
  splitaddr<- unlist(strsplit(aaddr, ' '))
  rootaddr<- splitaddr[2:length(splitaddr)]
  rootaddr<- paste(rootaddr, collapse = " ")
  print(rootaddr)
  if (grepl("-",splitaddr[1]) & is.na(asite$e911address)==TRUE){
    addrange<- unlist(strsplit(splitaddr[1], "-"))
    print(addrange)
    rmin<-  as.numeric(addrange[1])
    rmax<- as.numeric(addrange[2])
    rdiff<- rmax-rmin
    rrange<- c(rmin:rmax)
    radds<- sapply(rrange, function(x) stri_trans_general(paste(toString(x), rootaddr), "Latin-ASCII"))
    multi.fun<- function(x){
      #Sys.sleep(.3)
      unlist(geocode(location = x, override_limit = TRUE))
    }
    rrevgeo<- lapply(radds, multi.fun)
    #rrevgeo<- lapply(radds, function(x) unlist(geocode(location = x, override_limit = TRUE)))
    # rcoords<- lapply(rrevgeo, function(x) c(unlist(x)[1], unlist(x)[2]))
    for (i in 1:length(rrevgeo)){
      if (is.numeric(unlist(rrevgeo[i])[1])==TRUE){
        rrevgeo[i]<-NULL
      }
    }
    ddists<- unlist(lapply(rrevgeo, function(x) spDistsN1(matrix(c(unlist(x)[1], unlist(x)[2]), nrow=1), coords, longlat = TRUE )))
    # ddists<- lapply(rcoords, function(x) spDistsN1(matrix(c(x[1], x[2]), nrow=1), coords, longlat = TRUE ))
    range.df<- data.frame(addressess=radds, distances = ddists)
    mindistaddress<- range.df[which(range.df$distances==min(range.df$distances)),]$addressess
    mindistdist<- range.df[which(range.df$distances==min(range.df$distances)),]$distances
    sites.df$e911address[i]<- toString(mindistaddress)
    sites.df$e911dist[i]<- mindistdist
  }
}



