library(httr)
library(dplyr)

getwd()
download.file("https://polizei.nrw/sites/default/files/2019-11/10_2019_Open_Gov.csv",destfile="CrimeStatistic.csv",method="libcurl")

data <- read.csv("CrimeStatistic.csv")

#Filter only city names
t = grep("Kreis", data$Behörde)
data = data[-t,]
t = grep("kreis", data$Behörde)
data = data[-t,]
t = grep("-", data$Behörde)
data = data[-t,]
cities <- as.vector(unique(data$Behörde))

#Add longitude and latitude
data$longitude <- NA
data$latitude <- NA

for(i in 1:length(cities)) {
  url <- 'http://nominatim.openstreetmap.org/search'
  params = list(city = cities[i],
                countrycodes = 'DEU',
                format = 'json')
  res <- httr::RETRY('GET', url, query = params, encode='json') %>% httr::content()
  
  for(j in 1:length(res)) {
    if(grepl("Nordrhein-Westfalen", res[[j]]$display_name)) {
      data$longitude[which(data$Behörde == cities[i])] <- res[[j]]$lon
      data$latitude[which(data$Behörde == cities[i])] <- res[[j]]$lat
      break
    }
  }
}

#Add coordinates for Essen, Köln and Bielefeld
data$longitude[which(data$Behörde == "Essen")] <- 7.0115552
data$latitude[which(data$Behörde == "Essen")] <- 51.4556432
data$longitude[which(data$Behörde == "Köln")] <- 6.958281
data$latitude[which(data$Behörde == "Köln")] <- 50.941278
data$longitude[which(data$Behörde == "Bielefeld")] <- 8.5324708
data$latitude[which(data$Behörde == "Bielefeld")] <- 52.0302285

colnames(data) <- c("Behoerde", "Delikt", "Monat", "bekanntgewordeneFaelle", "davonVersuche", "Aufklaerungsquote", "longitude", "latitude")
write.csv(data, file="CrimeStatistic.csv", row.names = FALSE)
