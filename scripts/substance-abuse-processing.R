library(dplyr)
library(datapkg)

##################################################################
#
# Processing Script for Substance-Abuse
# Created by Jenna Daly
# On 03/06/17
#
##################################################################

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
  group_by(Town,AdmYear) %>% 
  mutate(AdmCount_Year = sum(AdmCount))

#order data
substance_abuse <- arrange(substance_abuse, Town, AdmYear)

#select pertinent columns
substance_abuse <- substance_abuse[,c('Town', 'AdmYear', 'AdmCount', 'AdmCount_Year')]

#create Connecticut totals for all years
total_CT <- substance_abuse 

total_CT <- total_CT %>% 
  group_by(AdmYear) %>% 
  mutate(AdmCount_Year = sum(AdmCount))

total_CT$Town <- "Connecticut"
total_CT <- total_CT[!duplicated(total_CT), ]

#add Connecticut totals to substance abuse 
substance_abuse <- substance_abuse[!duplicated(substance_abuse), ]
combine <- rbind(substance_abuse, total_CT)

#bring in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

#backfill years
years <- c("2013",
           "2014",
           "2015",
           "2016")

backfill_years <- expand.grid(
  `Town` = unique(fips$`Town`),
  `AdmYear` = years
)

complete_town_list <- merge(combine, backfill_years, all=T)
complete_town_list_with_FIPS <- merge(complete_town_list, fips, by = "Town", all=T)

#setting 0's back to NAs
complete_town_list_with_FIPS$AdmCount_Year[complete_town_list_with_FIPS$AdmCount_Year == 0] <- NA

#suppressing towns where values are less than 15
complete_town_list_with_FIPS$AdmCount_Year[complete_town_list_with_FIPS$AdmCount_Year < 15] <- -9999

#Create filtering columns
complete_town_list_with_FIPS$Variable <- NA
complete_town_list_with_FIPS$Variable <- "DMHAS Admissions"

complete_town_list_with_FIPS$`Admission Type` <- NA
complete_town_list_with_FIPS$`Admission Type` <- "Substance Abuse"

complete_town_list_with_FIPS$`Measure Type` <- NA
complete_town_list_with_FIPS$`Measure Type` <- "Number"

#Select and reorder columns
complete_town_list_with_FIPS <- complete_town_list_with_FIPS %>% 
  select(`Town`, `FIPS`, `AdmYear`, `Variable`, `Admission Type`, `Measure Type`, `AdmCount_Year`)

#Rename columns
colnames(complete_town_list_with_FIPS) <- c("Town", "FIPS", "Year", "Variable", "Admission Type", "Measure Type", "Value")

#Remove duplicates
complete_town_list_with_FIPS <- complete_town_list_with_FIPS[!duplicated(complete_town_list_with_FIPS), ]

#Write CSV
write.table(
  complete_town_list_with_FIPS,
  file.path(getwd(), "data", "dmhas-admissions_substance-abuse.csv"),
  sep = ",",
  na = "-6666",
  row.names = F
)
