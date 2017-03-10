library(dplyr)
library(datapkg)

##################################################################
#
# Processing Script for Substance-Abuse
# Created by Jenna Daly
# On 03/06/17
#
##################################################################

#all blanks should be suppressed
#all backfilled should be NAs

sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path <- (paste0(getwd(), "/", data_location))
all_csvs <- dir(path, recursive=T, pattern = ".csv") 
raw_file <- read.csv(paste0(path, "/", all_csvs), stringsAsFactors=F, header=T)

#subset raw data
substance_abuse <- raw_file[,c('Town', 'AdmMonth', 'AdmYear', 'PrimaryDrug', 'AdmCount')]

#recode month column
month_digits <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
months <- factor(c("January", "February", "March", "April", "May", "June", "July", 
                   "August", "September", "October", "November", "December"))
substance_abuse$Month <- months[match(substance_abuse$AdmMonth, month_digits)]

substance_abuse$AdmMonth <- NULL

substance_abuse$Month <- factor(substance_abuse$Month, levels = months)
substance_abuse <- arrange(substance_abuse, Month, AdmYear)

#setting NAs to 0, so we can calculate the aggregate (there are no true 0 entries)
substance_abuse$AdmCount[is.na(substance_abuse$AdmCount)] <- 0

#creating total admission count for each year
substance_abuse <- substance_abuse %>% 
  group_by(Town,AdmYear,Month) %>% 
  mutate(AdmCount_Month = sum(AdmCount))

#order data
substance_abuse <- arrange(substance_abuse, Town, Month, AdmYear)

#select pertinent columns
substance_abuse <- substance_abuse[,c('Town', 'AdmYear', 'AdmCount', 'Month', 'AdmCount_Month')]

#create Connecticut totals for all years
total_CT <- substance_abuse [,c('Town', 'AdmYear', 'Month', 'AdmCount_Month')]
total_CT <- total_CT[!duplicated(total_CT), ]

total_CT <- total_CT %>% 
  group_by(AdmYear, Month) %>% 
  mutate(AdmCount_Year = sum(AdmCount_Month))

total_CT <- total_CT [,c('Town', 'AdmYear', 'Month', 'AdmCount_Year')]

total_CT$'Town' <- "Connecticut"

#add Connecticut totals to substance abuse 
substance_abuse <- substance_abuse [,c('Town', 'AdmYear', 'Month', 'AdmCount_Month')]
substance_abuse <- substance_abuse[!duplicated(substance_abuse), ]

colnames(substance_abuse) <- c('Town', 'Year', 'Month', 'AdmCount')
colnames(total_CT) <- c('Town', 'Year', 'Month', 'AdmCount')
substance_abuse <- as.data.frame(substance_abuse, stringsAsFactors=F)
total_CT <- as.data.frame(total_CT, stringsAsFactors=F)

combine <- rbind(total_CT, substance_abuse)

#bring in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

#backfill years and months
years <- c("2013",
           "2014",
           "2015",
           "2016")

backfill <- expand.grid(
  `Town` = unique(fips$`Town`),
  `Year` = years, 
  `Month` = months
)

backfill$Town <- as.character(backfill$Town)
backfill$Month <- as.character(backfill$Month)
backfill$Year <- as.character(backfill$Year)

backfill <- arrange(backfill, Town)

town_list <- merge(combine, backfill, all=T)

town_list_with_FIPS <- merge(town_list, fips, by = "Town", all=T)

#setting 0's back to -9999 to account for suppressed values
town_list_with_FIPS$AdmCount[town_list_with_FIPS$AdmCount == 0] <- -9999

#Create filtering columns
town_list_with_FIPS$Variable <- NA
town_list_with_FIPS$Variable <- "DMHAS Admissions"

town_list_with_FIPS$`Admission Type` <- NA
town_list_with_FIPS$`Admission Type` <- "Substance Abuse"

town_list_with_FIPS$`Measure Type` <- NA
town_list_with_FIPS$`Measure Type` <- "Number"

#Select and reorder columns
town_list_with_FIPS <- town_list_with_FIPS %>% 
  select(`Town`, `FIPS`, `Year`, `Month`, `Variable`, `Admission Type`, `Measure Type`, `AdmCount`)

#Rename columns
colnames(town_list_with_FIPS) <- c("Town", "FIPS", "Year", "Month", "Variable", "Admission Type", "Measure Type", "Value")

#Remove duplicates
town_list_with_FIPS <- town_list_with_FIPS[!duplicated(town_list_with_FIPS), ]

#Write CSV
write.table(
  town_list_with_FIPS,
  file.path(getwd(), "data", "dmhas-admissions_substance-abuse.csv"),
  sep = ",",
  na = "0",
  row.names = F
)
