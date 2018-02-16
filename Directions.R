require(googleway)

originaddress = "2785 Mitchell Dr., Bldg #9, Walnut Creek, 94598"


route<- googleway::google_directions(units = 'imperial', key = "YourAPIKey", origin = "Breckenridge, CO", destination = "Denver, CO", mode = 'driving')

legs<- as.data.frame(route$routes$legs)
directions.df<- as.data.frame(legs$steps)
HTMLclean <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))}
directions.df$html_instructions<- sapply(directions.df$html_instructions, HTMLclean)
