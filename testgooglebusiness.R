
##############################3
#
#   USE GOOGLE MAPS GEOCODE API FROM BUSINESS ACCOUNT
# 
rm(list=ls())
require(ggmap)
require(sp)
require(readxl)
require(dplyr)
projws= "C:/Users/jmaxm/Documents/Projects/Nexius/nexiusR"
setwd(projws)

testadd<- ""

gclientid<-""
gsecret<- ""

sites.df<- read.csv("C:/Users/jmaxm/Documents/Projects/Nexius/LizPeterson_07_27/e911datacopy.csv", stringsAsFactors = FALSE)
sites.df<- sites.df[6,] #DON'T FORGET COMMA!!!


secrettosig <- function(secret)
{
  #secret.safe <- chartr("-_", "+/", secret)
  decoded_key <- base64enc::base64decode(secret)
  # break up the url
  #signature <- digest::hmac(decoded_key, url_to_sign, algo="sha1", raw=TRUE)
  signature<- base64enc::base64encode(decoded_key)
  return(signature)
}
Gsignature<- secrettosig(gsecret)

geocode(location = testadd, client = gclientid, signature = Gsignature)

