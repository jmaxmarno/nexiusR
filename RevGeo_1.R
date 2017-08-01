# MAX MARNO
# 7/19/2017
# WORK FOR NEXIUS/SUNDELL
# REVERSE GEOCODE SITE COORDINATES TO GET ADDRESSES, THEN PLOT AND EXPORT MAPS
require(ggmap)
require(sp)
require(readxl)
require(dplyr)
projws= "C:/Users/jmaxm/Documents/Projects/Nexius/RevGeo"
setwd(projws)

sites<- read.csv("E911 address mapping.csv", stringsAsFactors = FALSE)
NexiusSites<- read.csv("NexiusSites.csv", stringsAsFactors = FALSE)
AlamedaAssessors<- read.csv("AlamedaAssessors_17_18.csv", stringsAsFactors = FALSE)
OaklandPoints<-read_excel("~/Projects/Nexius/RevGeo/Oakland_Sites_Proj_SpJ_addrS1_tble_TableToExcel.xls")
OaklandPoints<- as.data.frame(OaklandPoints)
OaklandPoints$AlamedaAssessors_Situs_Zip<- OaklandPoints$geocoded_address_postal_code
OaklandPoints$AlamedaAssessors_Situs_Street_Number<- gsub(",", "", OaklandPoints$AlamedaAssessors_Situs_Street_Number)

# for(i in 1:nrow(sites)){
#   sites$RevGeoAddress[i]<- revgeocode(location = c(sites$longitude[i], sites$latitude[i]))
#   AddrCoords<- geocode(location = toString(sites$RevGeoAddress[i]))
#   sites$AddrLat[i]<- AddrCoords$lat
#   sites$AddrLon[i]<- AddrCoords$lon
# }

# pt1<-matrix(c(sites$longitude[2], sites$latitude[2]), nrow=1, ncol=2)
# pt2<- matrix(c(sites$AddrLon[2], sites$AddrLat[2]), nrow=1)
# spDistsN1(pt1, pt2, longlat = TRUE)
########################################################################################
# NEED TO TURN THIS INTO A FUNCTION AND VECTORIZE
# THIS IS A KEEPER
# geocode the assess
#OaklandPoints<- OaklandPoints[3:10,]
for(i in 1:nrow(OaklandPoints)){
  trow<- OaklandPoints[i,]
  stloc<- paste0(toString(trow$AlamedaAssessors_Situs_Street_Number), 
                 " ", toString(trow$AlamedaAssessors_Situs_Street_Name), 
                 " ", toString(trow$AlamedaAssessors_Situs_City),
                 ", CA ", 
                 toString(trow$AlamedaAssessors_Situs_Zip))
  OaklandPoints$ASSADDR[i]<- stloc
  AddrCoords<- geocode(location = stloc)
  OaklandPoints$ASSAddrLat[i]<- AddrCoords$lat
  OaklandPoints$ASSAddrLon[i]<- AddrCoords$lon
}


for(i in 1:nrow(OaklandPoints)){
  coords<- matrix(c(OaklandPoints$longitude[i], OaklandPoints$latitude[i]), nrow = 1)
  acoords<- matrix(c(OaklandPoints$ASSAddrLon[i], OaklandPoints$ASSAddrLat[i]), nrow = 1)
  ddist<- spDistsN1(acoords, coords, longlat = TRUE)
  OaklandPoints$DIST2ASSESSORS[i]<- 1000*ddist
}

for(i in 1:nrow(OaklandPoints)){
  coords<- matrix(c(OaklandPoints$longitude[i], OaklandPoints$latitude[i]), nrow = 1)
  acoords<- matrix(c(OaklandPoints$AddrLon[i], OaklandPoints$AddrLat[i]), nrow = 1)
  ddist<- spDistsN1(acoords, coords, longlat = TRUE)
  OaklandPoints$DIST2RevGeo[i]<- 1000*ddist
}

write.csv(OaklandPoints, "NexiusSites_wDist.csv", row.names = FALSE)

#write.csv(sites, "NexiusSites.csv", row.names = FALSE)





mapgilbert <- get_map(location = c(lon = sites$longitude[3], lat = sites$latitude[3]), zoom = 20,
                      maptype = "satellite", scale = 2)


#####################################################################################################

for(i in 1:nrow(OaklandPoints)){
  streetnum<- toString(OaklandPoints$RevGeoAddress[i])
  streetnum<- unlist(strsplit(streetnum, " "))
  streetnum<- streetnum[1]
  
}

for(i in 1:nrow(OaklandPoints)){
  OaklandPoints$ASSADDR[i]<- grep(",", OaklandPoints$ASSADDR[i])
}




for(i in 1:nrow(NexiusSites)){
  coords<- matrix(c(NexiusSites$longitude[i], NexiusSites$latitude[i]), nrow = 1)
  acoords<- matrix(c(NexiusSites$AddrLon[i], NexiusSites$AddrLat[i]), nrow = 1)
  ddist<- spDistsN1(acoords, coords, longlat = TRUE)
  NexiusSites$DistanceInMeters[i]<- 1000*ddist
}


# i HAVE ENDED UP DOING SOME MANUAL MANIPULATION OF THE FINAL FIELDS IN EXCEL, NEED TO ADD THESE STEPS TOTHE SCRIPT
# FINAL NEAREST ADDRESS FIELD
# FINAL 'SOURCE' FIELD
# FINAL DISTANCE BETWEEN SITE AND ADDRESS POINT FIELD



