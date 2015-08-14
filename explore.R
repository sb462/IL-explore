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
               