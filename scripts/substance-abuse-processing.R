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
all_csvs <- dir(path, recursive=T, pattern = "Month.csv") 
raw_file <- read.csv(paste0(path, "/", all_csvs), stringsAsFactors=F, header=T)

#subset raw data
substance_abuse <- raw_file %>% 
  select('Town', 'AdmMonth', 'AdmYear', 'MonthTotal') %>% 
  rename(Year = AdmYear) %>% 
  filter(!is.na(Town))

#recode month column
month_digits <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
months <- factor(c("January", "February", "March", "April", "May", "June", "July", 
                   "August", "September", "October", "November", "December"))
substance_abuse$Month <- months[match(substance_abuse$AdmMonth, month_digits)]

substance_abuse$AdmMonth <- NULL

substance_abuse$Month <- factor(substance_abuse$Month, levels = months)
substance_abuse <- arrange(substance_abuse, Town, Month, Year)

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

backfill <- backfill %>% 
  filter(Town != "Connecticut") %>% 
  arrange(Town)

substance_abuse_backfill <- merge(substance_abuse, backfill, by = c("Town", "Year", "Month"), all=T)

substance_abuse_backfill_fips <- merge(substance_abuse_backfill, fips, by = "Town", all.x=T)

#recode NA to 0 to account for FY overlap
substance_abuse_backfill_fips <- substance_abuse_backfill_fips %>% 
  mutate(MonthTotal = ifelse(Month %in% c("January", "February", "March", "April", "May", "June") & Year == "2013", 0, MonthTotal))
substance_abuse_backfill_fips <- substance_abuse_backfill_fips %>% 
  mutate(MonthTotal = ifelse(Month %in% c("July", "August", "September", "October", "November", "December") & Year == "2016", 0, MonthTotal))

#Create filtering columns
substance_abuse_backfill_fips$Variable <- "DMHAS Admissions"
substance_abuse_backfill_fips$`Admission Type` <- "Substance Abuse"
substance_abuse_backfill_fips$`Measure Type` <- "Number"

#Select, reorder, rename columns
substance_abuse_backfill_fips <- substance_abuse_backfill_fips %>% 
  select(Town, FIPS, Year, Month, `Admission Type`, `Measure Type`, Variable, MonthTotal) %>% 
  rename(Value = MonthTotal) %>% 
  arrange(Town, Year, Month)

#Write CSV
write.table(
  substance_abuse_backfill_fips,
  file.path(getwd(), "data", "dmhas-admissions_substance-abuse_2016.csv"),
  sep = ",",
  na = "-9999",
  row.names = F
)
