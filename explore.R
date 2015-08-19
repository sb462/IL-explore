list.files()
library(data.table)
?fread
FIPS.df <- fread("FIPS.txt", stringsAsFactors = FALSE, colClasses = c("character","character","character","character","character"))
head(FIPS.df)
library(dplyr)
library(magrittr)
?drop
?filter
FIPS.df.new <- FIPS.df %>% 
            setnames(., c("State", "State.Fips","County.FIPS", "County.Name", "County.Type")) %>%
              select(., -County.Type) %>%
                mutate(.,County.Code = paste0(State.Fips,County.FIPS))
FIPS.code <- FIPS.df.new$County.Code

# Now we access the census API for all the counties in IL

#API http://www.broadbandmap.gov/developer/api/demographics-api-by-geography-type-and-geography-id

# GET 10 counties together

# API structure

#http://www.broadbandmap.gov/broadbandmap/demographic/jun2014/county/ids/17081,17083?format=json

library(jsonlite)
sample.data <- fromJSON("http://www.broadbandmap.gov/broadbandmap/demographic/jun2014/county/ids/17081,17083?format=json")
sample.res <- sample.data$Results

# build a list of URLs

yr.census <- c("jun2011", "dec2011", "jun2012", "dec2012", "jun2013", "dec2013", "jun2014")

url.head <- "http://www.broadbandmap.gov/broadbandmap/demographic/"
url.yr <- "jun2014"
url.mid <- "/county/ids/"
url.tail <- "?format=json"
FIPS.matrix <- matrix(FIPS.code[1:100], nrow = 10)

FIPS.list <- list()
url.FIPS <- list()
for (i in 1:11){
  if (i %in% c(1:10)){
  FIPS.list[[i]] <- FIPS.matrix[,i]
  url.FIPS[[i]] <- paste0(FIPS.list[[i]],sep=",", collapse = "")
  }
  else{
  FIPS.list[[i]] = FIPS.code[101:102]
  url.FIPS[[i]] = paste0(FIPS.list[[i]],sep=",", collapse = "")
  }
}

a.list <- c("a","b")

vec <- c("a","b","c")
paste0(vec,sep=",", collapse= "")

paste0(a.list, sep=",", collapse="")

paste0(vec, collapse=",")
?paste0
?paste0
# build list of 11 URLs

build.url <- function(fips.code){
  url.head <- "http://www.broadbandmap.gov/broadbandmap/demographic/"
  url.yr <- "jun2014"
  url.mid <- "/county/ids/"
  url.tail <- "?format=json"
  url.vector <- c(url.head,url.yr,url.mid,fips.code,url.tail)
  url <- paste0(url.vector, sep="", collapse ="")
}

list.of.urls.jun2014 <- lapply(url.FIPS, FUN=build.url)

list.of.urls.jun2014[[1]]

sample.url <- "http://www.broadbandmap.gov/broadbandmap/demographic/2014/coordinates?latitude=42.456&longitude=-74.987&format=json"
sample.data2 <- fromJSON(sample.url)
str(sample.data2$Results)

