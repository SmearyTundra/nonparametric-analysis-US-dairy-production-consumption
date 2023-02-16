library(readxl)
library(tidyr)
rm(list=ls())
all_sales_county <- read.table("data_updated_2021/all_dairy_sales_county.csv", 
                               sep =",", header = T)

all_sales_county[which(all_sales_county$Value == " (D)"), ]$Value = NA
all_sales_county$Value = as.numeric(gsub(",", "", all_sales_county$Value))
all_sales_county <- all_sales_county[-c(1,3,4,5,7,8,9,12,13,14,15,16,17,18,19,21)]
all_sales_county$County.ANSI = as.numeric(all_sales_county$County.ANSI)
all_sales_county$County = tolower(all_sales_county$County)
all_sales_county$State = tolower(all_sales_county$State)



county_coord <- read_excel("data_updated_2021/us-county-boundaries.xlsx")
county_coord$COUNTYFP_NOZERO <- as.numeric(county_coord$COUNTYFP_NOZERO)

county_coord <- county_coord[c(1,3,6,9,10)]
names(county_coord)[2] <- "fips"
names(county_coord)[5] <- "County.ANSI"
names(county_coord)[3] <- "County"
names(county_coord)[4] <- "State"
county_coord$County = tolower(county_coord$County)
county_coord$State = tolower(county_coord$State)


population_county <- read.table("data_updated_2021/co-est2007-alldata.csv", 
                            sep =",", header = T)
population_county <- population_county[c(5,6,17)]
population_county$STNAME <- tolower(population_county$STNAME)
names(population_county) <- c("County.ANSI", "State", "Population")

temp <- merge(all_sales_county,county_coord, by=c("State","County", "County.ANSI"), all.x=TRUE, all.y=FALSE)
temp2 <- merge(temp,population_county, by=c("State","County.ANSI"), all.x=TRUE, all.y = FALSE)

temp2 <- temp2[temp2$State != "hawaii",]

#cows_inventory_county <- read.table("data_updated_2021/all_cows_county.csv", 
#                                    sep =",", header = T)
#cows_inventory_county$Value = as.numeric(gsub(",", "", cows_inventory_county$Value))
#cows_inventory_county <- cows_inventory_county[-c(1,3,4,5,7,8,9,12,13,14,15,16,17,18,19,21)]
#cows_inventory_county$County.ANSI = as.numeric(cows_inventory_county$County.ANSI)
#cows_inventory_county$County = tolower(cows_inventory_county$County)
#cows_inventory_county$State = tolower(cows_inventory_county$State)
#names(cows_inventory_county)[5] = "Cows"
#
#
##inner_join(milk_sales_county, county_coord, by="County.ANSI")
#temp <- merge(all_sales_county,county_coord, by=c("State","County", "County.ANSI"), all.x=TRUE, all.y=FALSE)
#temp2 <- merge(temp,cows_inventory_county, by=c("Year","State","County", "County.ANSI"), all.x=TRUE, all.y = FALSE)
#


loc <- strsplit(as.character(temp2$Geo_Point),",", fixed=TRUE)
loc <- do.call(rbind.data.frame, loc)
names(loc) <- c("x","y")

loc$x <- as.double(loc$x)
loc$y <- as.double(loc$y)

temp2 <- cbind(temp2,loc)


final_df <- split(temp2,list(temp2$Year))
all_sales_county_1997 <- final_df$"1997"
all_sales_county_2002 <- final_df$"2002"
all_sales_county_2007 <- final_df$"2007"

all_sales_county_1997 <- drop_na(all_sales_county_1997)
all_sales_county_2002 <- drop_na(all_sales_county_2002)
all_sales_county_2007 <- drop_na(all_sales_county_2007)
all_sales_county_1997<- all_sales_county_1997[-c(4,6)]
all_sales_county_2002 <- all_sales_county_2002[-c(4,6)]
all_sales_county_2007 <- all_sales_county_2007[-c(4,6)]
rm(temp)
rm(temp2)
rm(loc)
rm(county_coord)
rm(final_df)
rm(all_sales_county)
rm(population_county)
#rm(cows_inventory_county)
