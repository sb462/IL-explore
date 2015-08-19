install.packages("ggmap")
library(ggmap)
?geocode
ghome <- geocode("1117 W Hawthorne St, Arlington Heights, IL 60005")
toString(ghome$lat)
ghome$lon
ghome$lat
toString(ghome$lat)
toString(ghome$lon)
sample.url <- "http://www.broadbandmap.gov/broadbandmap/demographic/2013/coordinates?latitude=42.0893375&longitude=-87.9978056&format=json"
sample.data3 <- fromJSON(sample.url)
str(sample.data3$Results$blockFips)

sample.url.county <- "http://www.broadbandmap.gov/broadbandmap/demographic/jun2014/county/ids/17081?format=json"
sample.county.data <- fromJSON(sample.url.county)
str(sample.county.data)


# function to return geolocation of a street address
# requires the package ggmap
# get.Geolocation<- function(street, city, state, zip){
#   address.String <- paste0(c(street, city, state, zip),collapse=",") 
#   print(address.String)
#   geo.Location <- geocode(address.String)
#   geo.lat <- toString(geo.Location$lat)
#   geo.lon <- toString(geo.Location$lon)
#   return(list(lat=geo.lat,lon= geo.lon))
# }

# better write my own function for geocoding
# requires jsonlite package
get.Geolocation <- function(house, street, city, state, zip){
  #https://maps.googleapis.com/maps/api/geocode/json?address=1600+Amphitheatre+Parkway,+Mountain+View,+CA&key=API_KEY
  google.head <- "https://maps.googleapis.com/maps/api/geocode/json?address="
  address <- paste0(house," ",street,", ",city,", " ,state," ",zip)
  en.address <- gsub(" ","+",address)
  API_KEY <- "AIzaSyBwq3EZ5KjeOKVeP3eLJwTFQ0mvu1jbcTg"
  google.url <- paste0(google.head,en.address,"&key=",API_KEY)
  google.response <- fromJSON(google.url)
  if (toupper(google.response$status) == "OK"){
    lat <- toString(google.response$results$geometry$location$lat)
    lon <- toString(google.response$results$geometry$location$lng)
    geo.list <- list("lat"= lat,"lon" = lon)
    return(geo.list)
  }
}

house = "1117"
street = "W Hawthorne St"
city = "Arlington Heights"
state = "IL"
zip = "60005"
get.Geolocation(house,street,city,state,zip)
ghome

paste0("latitude=", toString(42.0893375))

# requires jsonlite package
get.Block.Demography <- function(lat,lon){
  url.head <- "http://www.broadbandmap.gov/broadbandmap/demographic/"
  url.year <- toString(2014)
  url.mid <- "/coordinates?"
  url.geolocation <- paste0("latitude=", toString(lat),"&", "longitude=",toString(lon))
  url.tail <- "&format=json"  
  get.url <- paste0(url.head,url.year,url.mid,url.geolocation,url.tail)
  response.url <- fromJSON(get.url)$Results
  return(response.url)
}

get.County.Demography <- function(county.fips){
  url.head <- "http://www.broadbandmap.gov/broadbandmap/demographic/"
  url.year <- toString("jun2014")
  url.mid <- "/county/ids/"
  url.id <- toString(county.fips)
  url.tail <- "?format=json"  
  get.url <- paste0(url.head,url.year,url.mid,url.id,url.tail)
  response.url <- fromJSON(get.url)$Results
  return(response.url)
}

substr("abcde", start = 1, stop =9)
get.County.Demography(17031)
?substr
library(magrittr)
?round
## requires magrittr
compare.With.County <- function(lat,lon){
  block.demography <- get.Block.Demography(lat,lon)
  str(block.demography)
  county.fips <- block.demography$blockFips %>% substr(.,start=1, stop=5)
  print(county.fips)
  county.demography <- get.County.Demography(county.fips)
  str(county.demography)
  diff.median.income <-  ((block.demography$medianIncome - county.demography$medianIncome)/county.demography$medianIncome*100) 
  diff.below.poverty <- ((block.demography$incomeBelowPoverty - county.demography$incomeBelowPoverty)/county.demography$incomeBelowPoverty*100)
  diff.college.graduate <- ((block.demography$educationBachelorOrGreater - county.demography$educationBachelorOrGreater)/county.demography$educationBachelorOrGreater*100)
  
  comparison.list <- list("medianIncome"= round(diff.median.income, digits=2), 
                          "belowPoverty"= round(diff.below.poverty, digits =2),
                          "collegeGraduate" = round(diff.college.graduate, digits =2))
  
  return(comparison.list)
}
compare.With.County(42.130042,-88.013641)

AIzaSyBwq3EZ5KjeOKVeP3eLJwTFQ0mvu1jbcTg  

# final flow of demography comparison

property.compare <- function(house, street, city,state, zip){
  geocode.property <- get.Geolocation(house, street, city,state, zip)
  print(geocode.property)
  compare.with.County <- compare.With.County(geocode.property$lat,geocode.property$lon)
  return(compare.with.County)
}

house <-  "1341"
street <- "E Evergreen Dr"
city <- "Palatine"
state <- "IL"
zip <- "60074"

property.compare(house,street,city,state,zip)
get.Block.Demography(42.130042,-88.013641)


