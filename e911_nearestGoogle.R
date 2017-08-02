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
projws= "C:/Users/jmaxm/Documents/Projects/Nexius/nexiusR"
setwd(projws)
gclientid<-"386710922427-jrd05o1hhvmh6f7l8rmb801esmbpejrs.apps.googleusercontent.com"
gsecret<- "YVFiEA6_iqZRX7glI4F5C1d7"

sites.df<- read.csv("C:/Users/jmaxm/Documents/Projects/Nexius/LizPeterson_07_27/e911datacopy.csv", stringsAsFactors = FALSE)
sites.df<- sites.df[3,]
for(i in 1:nrow(sites.df)){
  sites.df$RevGeoAddr[i]<- revgeocode(location = c(sites.df$longitude[i], sites.df$latitude[i]))
  AddrCoords<- geocode(location = toString(sites.df$RevGeoAddr[i]))
  sites.df$RevGeoAddrLat[i]<- AddrCoords$lat
  sites.df$RevGeoAddrLon[i]<- AddrCoords$lon
  # MEASURE DIST BETWEEEN REAL SITE COORDINATES, AND REVGEO>GEOCODED ADDRESS COORDINATES
  coords<- matrix(c(sites.df$longitude[i], sites.df$latitude[i]), nrow = 1)
  addcoords<- matrix(c(sites.df$RevGeoAddrLon[i], sites.df$RevGeoAddrLat[i]), nrow = 1)
  ddist<- spDistsN1(addcoords, coords, longlat = TRUE)
  sites.df$DIST2RevGeo[i]<- 1000*ddist
}




########################################################################################
# LOOP TO RETURN SINGLE ADDRESS INSTEAD OF RANGE

for (i in 1:nrow(sites.df)){
  asite<- sites.df[i,]
  aaddr<- asite$RevGeoAddr
  odist<- asite$DIST2RevGeo
  coords<- matrix(c(asite$longitude[i], asite$latitude[i]), nrow = 1)
  splitaddr<- unlist(strsplit(aaddr, ' '))
  rootaddr<- splitaddr[2:length(splitaddr)]
  if (grepl("-",splitaddr[1])){
    addrange<- unlist(strsplit(splitaddr[1], "-"))
    rmin<- addrange[1]
    rmax<- addrange[2]
    #rdiff<- rmax-rmin
    rrange<- c(rmin:rmax)
    radds<- sapply(rrange, function(x) paste(toString(x), rootaddr))
    rrevgeo<- lapply(radds, function(x) unlist(geocode(location = x)))
    rcoords<- lapply(rrevgeo, function(x) c(unlist(x)[1], unlist(x)[2]))
    ddists<- lapply(rcoords, function(x) spDistsN1(matrix(c(x[1], x[2]), nrow=1), coords, longlat = TRUE ))
    
    range.df<- data.frame(addressess=radds, distances = ddists)
    print(range.df)

    }
    
    print(splitaddr[1])
    print(rmin)
  }






for(i in 1:nrow(OaklandPoints)){
  coords<- matrix(c(OaklandPoints$longitude[i], OaklandPoints$latitude[i]), nrow = 1)
  acoords<- matrix(c(OaklandPoints$AddrLon[i], OaklandPoints$AddrLat[i]), nrow = 1)
  ddist<- spDistsN1(acoords, coords, longlat = TRUE)
  OaklandPoints$DIST2RevGeo[i]<- 1000*ddist
}


