
######################################################
# ENTER SHELBY COUNTY HEALTH DEPT DAILY TESTS COUNT

	Tested    <- 20315   	

#		Manual Entry on 2020-04-22
######################################################


# Data Sources
# 01 Johns Hopkins COVID-19 gitgub repo
# 	https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
# 02 Shelby County, TN Health Department
# 	https://insight.livestories.com/s/v2/covid-19-data-page/8a6ba562-bc6f-4e58-bdcc-c211b6be539c

# Load Libraries
library(rvest)
library(stringr)
library(readr)
library(tidyr)
library(data.table)

# Load Data
URL       <- read_html("https://insight.livestories.com/s/v2/covid-19-data-page/8a6ba562-bc6f-4e58-bdcc-c211b6be539c")
Deaths    <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
Time      <- Sys.time()

# subset JHU data for shelby county
Deaths    <- as.data.frame(Deaths)
Deaths    <- Deaths[Deaths$FIPS == "47157", length(Deaths)][1]

# format and clean SHD data
COVID  <- URL %>% html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "ezdhjma0", " " ))]', ) %>% html_text()
COVID  <- COVID[3]
COVID  <- str_split(COVID, "-")
COVID1 <- str_split(COVID[[1]][6], "Data Source: ")
COVID1 <- COVID1[[1]][1]
COVID  <- list(c(COVID[[1]][2:5], COVID1[[1]][1]))

COVID  <- as.data.frame(COVID)
colnames(COVID) <- "Shelby County Data"
COVID  <- separate(data = COVID,
		col = "Shelby County Data",
		into = c("Shelby County Data", "Value"), 
		sep = ": ")

COVID$Value <- gsub(",", "", COVID$Value)
COVID$Value <- as.numeric(COVID$Value)

# Make Rel_f data
Tested_p <- round(Tested / Tested, 4)
Deaths_p <- round(Deaths / Tested, 4)
df_p     <- round(COVID$Value / Tested, 4)
COVID_p  <- c(Tested_p, df_p, Deaths_p)

# Merge all Data and make Rel_f col
Tested  <- c("Tested", Tested)
Deaths  <- c("Deaths", Deaths)
COVID   <- rbind(Tested, COVID, Deaths)
COVID$Rel_f <- COVID_p
COVID$Value <- as.numeric(COVID$Value)

# Calculate Formulas and make footnote
Cases  <- COVID[2, 2]
Tested <- COVID[1, 2]
Deaths <- COVID[7, 2]
Fatality_Rate      = round(( Deaths / Cases  ) * 100, 2)
Real_Fatality_Rate = round(( Deaths / Tested ) * 100, 2)


# Format Message
Footnote1 <- paste("\n","Footnote^1:","Percentage values in table are based on 'Tested' amount","\n")
Footnote2 <- paste("Footnote^2:","Data Sources are Shelby County Health Dept. and JHU","\n")
Footnote3 <- paste("Footnote^3:","This data is as of ", Time,"\n")
Footnote <- c(Footnote1,Footnote2,Footnote3)

Line1  <- paste("Summary^1:",COVID[1,2],"people have been tested.","\n")
Line2  <- paste("Summary^2:",COVID[7,3]*100,"% (",COVID[7,2],"people) of the people tested, have died\n related to COVID-19.","\n")
Line3  <- paste("Summary^3:","Out of the ",COVID[2,3]*100,"% of people tested (confirmed cases)","\n", Fatality_Rate,"% have died related to COVID-19 (possible other reasons too).")
COVIDMessage <- c(Line1,Line2,Line3)


# Print Message
print(COVID, right=F); writeLines(Footnote, sep="\n");writeLines(COVIDMessage, sep="\n")

