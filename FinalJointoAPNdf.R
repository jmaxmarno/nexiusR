require(dplyr)

sites.df<- read.csv("SITES.DF_6.csv", stringsAsFactors = FALSE)
sites.apn<- read.csv("Riverside_OC_nexius_sites.csv", stringsAsFactors = FALSE)

justapn<- sites.apn[,c('fulcrum_id', 'COUNTY', 'ROW_APN', 'NEAREST_APN') ]

tester<- full_join(sites.df, justapn, by='fulcrum_id')
write.csv(tester, "nexius_feasibility_OCandRiverside.csv", row.names = FALSE)

tester$e911dist<- lapply(tester$e911dist, function(x) 
  if (is.na(x)==FALSE){
  as.numeric(x)*1000
}else{NULL}
)
write.csv(tester, "nexius_feasibility_OCandRiverside.csv", row.names = FALSE)
tester$FinalAddress<-