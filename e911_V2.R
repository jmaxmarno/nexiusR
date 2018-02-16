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
# gclientid<-"REMOVED"
# gsecret<- "REMOVED"
register_google(key = 'REMOVED')

sites.w.revgeo<- read.csv("C:/Users/jmaxm/Documents/Projects/Nexius/nexiusR/sites.df.wRevGeo.csv", stringsAsFactors = FALSE)
sites.df<- read.csv("C:/Users/jmaxm/Documents/Projects/Nexius/LizPeterson_07_27/e911datacopy.csv", stringsAsFactors = FALSE)
#sites.df<- read.csv("C:/Users/jmaxm/Documents/Projects/Nexius/nexiusR/workinprogress.csv", stringsAsFactors = FALSE)
#sites.df<- sites.df[10:nrow(sites.df)] #DON'T FORGET COMMA!!!
sites.df<- sites.df[which(sites.df$fulcrum_id=="a110b163-2519-4060-b021-474edcffd735" | sites.df$fulcrum_id=="ac6dd6af-6a27-40b4-95f1-cb457f99a6fe" ),]
for(i in 1:nrow(sites.df)){
  geo1<- revgeocode(location = c(sites.df$longitude[i], sites.df$latitude[i]), override_limit = TRUE, nameType='long', output = 'all')
  if (geo1$status=="OK"){
    sites.df$RevGeoAddr[i]<- toString(geo1$results[[1]]$formatted_address)
    sites.df$geo1status[i]<- toString(geo1$status)
  }else{
    sites.df$geo1status[i]<- NA
    sites.df$RevGeoAddr[i]<- NA
  }
}
# write.csv(sites.df, "sitesWrevgeo.csv", row.names = FALSE)
sites.df<- read.csv("sitesWrevgeo.csv", stringsAsFactors = FALSE)
for(i in 1:nrow(sites.df)){
  if (is.na(sites.df$geo1status[i])){
    geo1<- revgeocode(location = c(sites.df$longitude[i], sites.df$latitude[i]), override_limit = TRUE, nameType='long', output = 'all')
    if (geo1$status=="OK"){
      sites.df$RevGeoAddr[i]<- toString(geo1$results[[1]]$formatted_address)
      sites.df$geo1status[i]<- toString(geo1$status)
    }else{
      sites.df$geo1status[i]<- NA
      sites.df$RevGeoAddr[i]<- NA
    }
  }
}
write.csv(sites.df, "sitesWrevgeo2pt0.csv", row.names = FALSE)
#Sys.sleep(.5)
sites.df<- read.csv("sitesWrevgeo2pt0.csv", stringsAsFactors = FALSE)

for(i in 1:nrow(sites.df)){
  AddrCoords<- geocode(location = stri_trans_general(sites.df$RevGeoAddr[i], id="Latin-ASCII"), override_limit = TRUE, nameType = 'long', output = 'all')
  if (AddrCoords$status=='OK'){
    sites.df$georevstatus[i]<- toString(AddrCoords$status)
    sites.df$RevGeoAddrLat[i]<- AddrCoords$results[[1]]$geometry$location$lat
    sites.df$RevGeoAddrLon[i]<- AddrCoords$results[[1]]$geometry$location$lng
    # MEASURE DIST BETWEEEN REAL SITE COORDINATES, AND REVGEO>GEOCODED ADDRESS COORDINATES
    coords<- matrix(c(sites.df$longitude[i], sites.df$latitude[i]), nrow = 1)
    addcoords<- matrix(c(sites.df$RevGeoAddrLon[i], sites.df$RevGeoAddrLat[i]), nrow = 1)
    ddist<- spDistsN1(addcoords, coords, longlat = TRUE)
    sites.df$DIST2RevGeo[i]<- 1000*ddist
    print(toString(i))
    }else{
      sites.df$RevGeoAddrLat[i]<- NA
      sites.df$RevGeoAddrLon[i]<- NA
      sites.df$DIST2RevGeo[i]<- NA
      sites.df$georevstatus[i]<- NA
    }
}

for(i in 1:nrow(sites.df)){
  if (is.na(sites.df$georevstatus[i])){
    AddrCoords<- geocode(location = stri_trans_general(sites.df$RevGeoAddr[i], id="Latin-ASCII"), override_limit = TRUE, nameType = 'long', output = 'all')
    if (AddrCoords$status=='OK'){
      sites.df$georevstatus[i]<- toString(AddrCoords$status)
      sites.df$RevGeoAddrLat[i]<- AddrCoords$results[[1]]$geometry$location$lat
      sites.df$RevGeoAddrLon[i]<- AddrCoords$results[[1]]$geometry$location$lng
      # MEASURE DIST BETWEEEN REAL SITE COORDINATES, AND REVGEO>GEOCODED ADDRESS COORDINATES
      coords<- matrix(c(sites.df$longitude[i], sites.df$latitude[i]), nrow = 1)
      addcoords<- matrix(c(sites.df$RevGeoAddrLon[i], sites.df$RevGeoAddrLat[i]), nrow = 1)
      ddist<- spDistsN1(addcoords, coords, longlat = TRUE)
      sites.df$DIST2RevGeo[i]<- 1000*ddist
      print(toString(i))
    }else{
      sites.df$RevGeoAddrLat[i]<- NA
      sites.df$RevGeoAddrLon[i]<- NA
      sites.df$DIST2RevGeo[i]<- NA
    }
  }
}



########################################################################################
# LOOP TO RETURN SINGLE ADDRESS INSTEAD OF RANGE
# try with already created revgeo csv


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
      xx<-geocode(location = x, override_limit = TRUE, nameType = 'long', output = 'all')
      while(xx$status != "OK"){
        Sys.sleep(10)
        xx<-geocode(location = x, override_limit = TRUE, nameType = 'long', output = 'all')
      }
      lat<- xx$results[[1]]$geometry$location$lat
      lon<- xx$results[[1]]$geometry$location$lng
      return(c(lon, lat))
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


## ROUND 2
########################################################################################
# LOOP TO RETURN SINGLE ADDRESS INSTEAD OF RANGE
# try with already created revgeo csv
#write.csv(sites.df, "SITES.DF_6.csv", row.names = FALSE)


sites.df<- read.csv("SITES.DF_6.csv", stringsAsFactors = FALSE)
for (i in 1:nrow(sites.df)){
  asite<- sites.df[i,]
  aaddr<- asite$RevGeoAddr
  odist<- asite$DIST2RevGeo
  coords<- matrix(c(asite$longitude, asite$latitude), nrow = 1)
  splitaddr<- unlist(strsplit(aaddr, ' '))
  rootaddr<- splitaddr[2:length(splitaddr)]
  rootaddr<- paste(rootaddr, collapse = " ")
  #print(rootaddr)
  if (grepl("-",splitaddr[1]) & is.na(asite$e911address)){
    addrange<- unlist(strsplit(splitaddr[1], "-"))
    print(addrange)
    rmin<-  as.numeric(addrange[1])
    rmax<- as.numeric(addrange[2])
    rdiff<- rmax-rmin
    rrange<- c(rmin:rmax)
    if (rdiff<2000){
      print(asite)
      radds<- sapply(rrange, function(x) stri_trans_general(paste(toString(x), rootaddr), "Latin-ASCII"))
      multi.fun<- function(x){
        # NOT SURE IF SLEEP/DELAY IS REALLY NECESSARY...
        #Sys.sleep(.3)
        xx<-geocode(location = x, override_limit = TRUE, nameType = 'long', output = 'all')
        while(xx$status != "OK"){
          Sys.sleep(10)
          xx<-geocode(location = x, override_limit = TRUE, nameType = 'long', output = 'all')
        }
        lat<- xx$results[[1]]$geometry$location$lat
        lon<- xx$results[[1]]$geometry$location$lng
        return(c(lon, lat))
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
      sites.df$e911dist[i]<- mindistdist
    } else {
      sites.df$e911address[i]<- NA
      sites.df$e911dist[i]<- NA
    }
      
  }
    
}

