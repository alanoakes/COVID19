# load library
library(rvest)
library(stringr)
library(readr)
library(data.table)
library(magrittr)

# load raw urls
URL_WORLD              <- read_html("https://www.worldometers.info/coronavirus/#countries")
BLS_KEY                <- read.csv("C:/Users/oakespar/OneDrive - CLK Multifamily Management, LLC/02 R Pipeline - 01 Import/COVID19_CIVIL_20200415/BLS_IDs-20200415.csv")
BLS_DATA               <- fread("https://download.bls.gov/pub/time.series/la/la.data.64.County")
COVID_COUNTY           <- fread("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv")

# combine BLS Data
BLS_Month              <- format(Sys.Date(), "%m")
BLS_Month              <- as.numeric(BLS_Month) - 2   # BLS is always 2mos behind (2020-04 data is from 2020-02)
BLS_Month              <- paste("M0", BLS_Month, sep = "")
BLS_Year               <- format(Sys.Date(), "%Y")
BLS_Merge              <- merge(x = BLS_KEY, y = BLS_DATA, by.x = 'BLS_ID', by.y = 'series_id')
BLS_Merge              <- BLS_Merge[BLS_Merge$year == BLS_Year  &  BLS_Merge$period == BLS_Month, -c(length(BLS_Merge)-2, length(BLS_Merge)) ]

# format and merge covid cases
COVID_COUNTY           <- as.data.frame(COVID_COUNTY)
COVID_COUNTY           <- COVID_COUNTY[ , c(1:3,length(COVID_COUNTY)) ]
COVID_COUNTY           <- merge(x = BLS_Merge, y = COVID_COUNTY, by.x = 'FIPS', by.y = 'countyFIPS')
COVID_COUNTY           <- COVID_COUNTY[, -c(9,11,12)]
colnames(COVID_COUNTY) <- c("County_FIPS","BLS_ID","US_COUNTY","ST","STATE","LATITUDE","LONGITUDE","COUNTY_CARD","UN_EMP_RATE","COVID_CASES")
COVID_COUNTY           <- COVID_COUNTY[,c("County_FIPS","BLS_ID","US_COUNTY","ST","STATE","UN_EMP_RATE","COVID_CASES","LATITUDE","LONGITUDE","COUNTY_CARD")]

# extract and format COVID19 data from worldometers
COVID_WORLD            <- URL_WORLD %>% html_nodes(xpath = "//tr") %>% html_text()
COVID_WORLD            <- str_split(COVID_WORLD, "\n")
WORLD_NAMES            <- COVID_WORLD[[1]][1:12]
COVID_WORLD            <- data.frame(Reduce(rbind, COVID_WORLD))
WORLD_Sub_A            <- grep("All",COVID_WORLD$X13)               # locate countries rows to keep
WORLD_Sub_B            <- grep("Total:", COVID_WORLD$X1)            # locate total rows to remove
WORLD_Sub_1            <- WORLD_Sub_A[1] + 1                        # first position
WORLD_Sub_2            <- WORLD_Sub_B[1] - 1                        # last position
COVID_WORLD            <- COVID_WORLD[ WORLD_Sub_1 : WORLD_Sub_2, 1:12]
colnames(COVID_WORLD)  <- WORLD_NAMES
COVID_WORLD$TotalCases <- as.character(COVID_WORLD$TotalCases)
COVID_WORLD$TotalCases <- gsub(" ", "", COVID_WORLD$TotalCases)
COVID_WORLD$TotalCases <- gsub(",", "", COVID_WORLD$TotalCases)
COVID_WORLD$TotalCases <- as.numeric(COVID_WORLD$TotalCases)
COVID_WORLD            <- COVID_WORLD[ order(COVID_WORLD$TotalCases,decreasing = T),]
COVID_WORLD            <- COVID_WORLD[1:5,]
colnames(COVID_WORLD)  <- c("Country","TotalCases","NewCases","TotalDeaths","NewDeaths","TotalRecovered","ActiveCases","SeriousCritical","Tot Cases/1M pop","Deaths/1M pop","TotalTests","Tests/1M pop")

# write files to onedrive
setwd("C:/Users/oakespar/OneDrive - CLK Multifamily Management, LLC/02 R Pipeline - 02 Tidy/COVID19-20200416")
write.csv(COVID_COUNTY, "COVID_COUNTY.csv", file = , row.names = F)
write.csv(COVID_WORLD,  "COVID_WORLD - COPY.csv",  file = , row.names = F)