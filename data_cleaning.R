library(tidyverse)  

setwd("C://Users/adeak/Dropbox (University of Oregon)/EWP/AFRI/data/Raw data/New data")

data_7 <- read.csv(file = "upto2002.csv", header = TRUE, ",")
data_8 <- read.csv(file = "upto2006.csv", header = TRUE, ",")
data_10 <-  read.csv(file = "upto2010.csv", header = TRUE, ",")
data_13 <- read.csv(file = "upto2013.csv", header = TRUE, ",")
data_16 <- read.csv(file = "upto2016.csv", header = TRUE, ",")
data_19 <- read.csv(file = "upto2019.csv", header = TRUE, ",")
data_21 <-read.csv(file = "upto2021.csv", header = TRUE, ",")

colnames(data_16) <- colnames(data_19)
combined_data <- rbind(data_7, data_8, data_10, data_13, data_16, data_19, data_21)

#filter data for year 2001-2020

data_00_20 <- filter(combined_data, Fiscal.Year >= 2001)
data_00_20 <- filter(data_00_20, Fiscal.Year <= 2020)
data_00_20

unique(data_00_20[c("Fiscal.Year")])
length(unique(data_00_20$Fiscal.Year))
names(combined_data)

# filter FSC codes
restoration_data = data_00_20[data_00_20$Product.or.Service.Code %in% c('F003','F004','F005','F006','F008', 'F009', 'F010','F012','F013', 'F014', 'F018', 'F019', 'F021','F099'), ]

# Now using these codes to filter the data

F3 <- restoration_data %>% 
  filter(Product.or.Service.Code == "F003") %>%
  filter(NAICS.Code %in% c("238990", "561210", "562910", "115310", "561990", "532412", "561730", "113310", "238910", "115310", "113110", "NA"))

F4 <- restoration_data %>% 
  filter(Product.or.Service.Code == "F004") %>%
  filter(NAICS.Code %in% c("113210", "238990", "561730", "113310", "238910", "115112", "115310","NA"))

F5<- restoration_data %>% 
  filter(Product.or.Service.Code == "F005") %>%
  filter(NAICS.Code %in% c("238910", "238990", "113210", "561730", "111421", "562910", "115112", "115310","113110", "NA"))

F6<- restoration_data %>% 
  filter(Product.or.Service.Code == "F006") %>%
  filter(NAICS.Code %in% c("235930", "561210", "235990", "532412", "113210", "561730", "113310", "238910", "115112", "115310","113110", "NA"))

F8<- restoration_data %>% 
  filter(Product.or.Service.Code == "F008") %>%
  filter(NAICS.Code %in% c("238990", "561990", "113210", "561730", "113310", "238910", "115310", "NA", "561210", "111421","115310"))

F9<- restoration_data %>% 
  filter(Product.or.Service.Code == "F009") %>%
  filter(NAICS.Code %in% c("113210","561730", "113310", "111421", "115112", "115310", "NA", "113310", "111421", "115112", "115310"))

F10<- restoration_data %>% 
  filter(Product.or.Service.Code == "F010") %>%
  filter(NAICS.Code %in% c("238990","113210","561730", "111421", "321113", "115112", "115310", "NA", "238910", "113110"))

F12<- restoration_data %>% 
  filter(Product.or.Service.Code == "F012") %>%
  filter(NAICS.Code %in% c("541310", "111421", "115310", "113110", "NA"))

F13<- restoration_data %>% 
  filter(Product.or.Service.Code == "F013") %>%
  filter(NAICS.Code %in% c("113210", "111421", "115310", "NA", "561730", "113310"))

F14 <-restoration_data %>% 
  filter(Product.or.Service.Code == "F014") %>%
  filter(NAICS.Code %in% c("238990","561990","113210", "561730", "113310", "238910", "115112", "115310", "113110", "NA", "111421"))

F18<- restoration_data %>% 
  filter(Product.or.Service.Code == "F018") %>%
  filter(NAICS.Code %in% c("235990", "238990","561990","532412", "321912", "561210", "113210", "561730", "113310","111421","562910", "238160", "321113", "238910","115112", "115310", "113110", "321114", "NA", "423310"))

F19<- restoration_data %>% 
  filter(Product.or.Service.Code == "F019") %>%
  filter(NAICS.Code %in% c("561730", "113310","238910","115112", "115310", "113110", "NA", "561210"))

F21<- restoration_data %>% 
  filter(Product.or.Service.Code == "F021") %>%
  filter(NAICS.Code %in% c("235990", "113210", "561730", "113310", "423310", "111421","562910", "238910","115112", "115310", "113110", "NA"))

F99<- restoration_data %>% 
  filter(Product.or.Service.Code == "F099") %>%
  filter(NAICS.Code %in% c("238990", "532412","235930", "561210", "113210","561730", "113310","111421","562910","321113", "238910", "115112", "115310", "113110", "NA", "238190", "561990"))

restoration_contracting_data <- rbind(F3, F4, F5, F6, F8, F9, F10, F12, F13, F14, F18, F19, F21, F99)

head(restoration_contracting_data)
restoration_contracting_data$statename <- restoration_contracting_data$Vendor.Address.State
restoration_contracting_data$zip<- str_extract(restoration_contracting_data$Vendor.Address.Zip.Code, "^\\d{5}")

#add fips and merged based on state and county. Got this data from https://www.unitedstateszipcodes.org/zip-code-database/
state <- read.csv(file = "C://Users/adeak/Dropbox (University of Oregon)/EWP/AFRI/data/State_FIPS_codes.csv", header = TRUE, ",")
head(state)

colnames(state)[1] <- "statename"
state$statename = toupper(state$statename)

add_statename <- left_join(restoration_contracting_data, state)
head(add_statename)
  
#introduce crosswalk

zipcode <- read.csv(file = "C://Users/adeak/Dropbox (University of Oregon)/EWP/AFRI/data/Raw data/Zipcode.csv", header = TRUE, ",")
colnames(zipcode)[1] <- "zip"
zipcode$zip <- str_pad(zipcode$zip, width = 5, side = "left", pad = "0")
names(zipcode)
head(zipcode)
zipcode<- zipcode[c("state", "county", "zip")]
head(zipcode)


restoration_final1 <- dplyr::left_join(restoration_contracting_data, zipcode, by = c ("zip"), all.x = TRUE)
str(restoration_final1$Dollars.Obligated)

restoration_final1$Dollars.Obligated <- as.numeric(gsub('[$,]', '', restoration_final1$Dollars.Obligated))
restoration_final1

national_data <- restoration_final1 %>% 
  group_by(PIID, Unique.Entity.ID)%>%
  dplyr::mutate(total_contract_val = sum(Dollars.Obligated), contract_last_fiscal_yr = max(Fiscal.Year)) %>%
  filter(total_contract_val > 0, 
         Funding.Office.Name != "NTL INTERAGENCY FIRE CENTER",
         Funding.Office.Name !=  "NATIONAL INTERAGENCY FIRE CENTER", 
         !str_detect(Legal.Business.Name, regex('shower', ignore_case = TRUE)), 
         !str_detect(Legal.Business.Name, regex('retardent', ignore_case = TRUE)),
         !str_detect(Legal.Business.Name, regex('cater', ignore_case = TRUE)), 
         !str_detect(Legal.Business.Name, regex('sanitary', ignore_case = TRUE)),
         !str_detect(Legal.Business.Name, regex('jet', ignore_case = TRUE)), 
         !str_detect(Legal.Business.Name, regex('airways', ignore_case = TRUE)),
         !str_detect(Legal.Business.Name, regex('helicopter', ignore_case = TRUE)), 
         !str_detect(Legal.Business.Name, regex('mini storage', ignore_case = TRUE)),
         !str_detect(Legal.Business.Name, regex('rent-a', ignore_case = TRUE)), 
         !str_detect(Legal.Business.Name, regex('medic', ignore_case = TRUE)),
         !str_detect(Legal.Business.Name, regex('towing', ignore_case = TRUE)), 
         !str_detect(Legal.Business.Name, regex('telephone', ignore_case = TRUE)),
         !str_detect(Legal.Business.Name, regex('oil', ignore_case = TRUE)), 
         !str_detect(Legal.Business.Name, regex('gas', ignore_case = TRUE)),
         !str_detect(Legal.Business.Name, regex('transport', ignore_case = TRUE)), 
         !str_detect(Legal.Business.Name, regex('trucking', ignore_case = TRUE)),
         !str_detect(Legal.Business.Name, regex('food', ignore_case = TRUE)), 
         !str_detect(Legal.Business.Name, regex('aerial', ignore_case = TRUE)),
         !str_detect(Legal.Business.Name, regex('mill', ignore_case = TRUE)), 
         !str_detect(Legal.Business.Name, regex('turf', ignore_case = TRUE)),
         !str_detect(Legal.Business.Name, regex('emergency', ignore_case = TRUE)), 
         !str_detect(Legal.Business.Name, regex('equipment', ignore_case = TRUE)),
         !str_detect(Legal.Business.Name, regex('road', ignore_case = TRUE)), 
         !str_detect(Legal.Business.Name, regex('traffic', ignore_case = TRUE)),
         !str_detect(Legal.Business.Name, regex('safety', ignore_case = TRUE)), 
         !str_detect(Legal.Business.Name, regex('mobile', ignore_case = TRUE)),
         !str_detect(Legal.Business.Name, regex('sewer', ignore_case = TRUE)), 
         !str_detect(Legal.Business.Name, regex('drain', ignore_case = TRUE)),
         !str_detect(Legal.Business.Name, regex('petroleum', ignore_case = TRUE)), 
         !str_detect(Funding.Office.Name, regex('Fire', ignore_case = TRUE)),
         !str_detect(Funding.Office.Name, regex('aviation', ignore_case = TRUE)), 
         !str_detect(Contracting.Office.Name, regex('fire', ignore_case = TRUE)),
         !str_detect(Contracting.Office.Name, regex('truck', ignore_case = TRUE)),
         !str_detect(Contracting.Office.Name, regex('aviation', ignore_case = TRUE))) %>%
  ungroup()

national_data %>% distinct(PIID, Unique.Entity.ID) %>% nrow 

write.csv(national_data, "C://Users/adeak/Dropbox (University of Oregon)/EWP/AFRI/data/national-data_cleaned.csv")

