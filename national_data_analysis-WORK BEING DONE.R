library(tidyverse)
library(sf)
library(scales)
library(MetBrewer)

wd <- "C://Users/adeak/Dropbox (University of Oregon)/EWP/AFRI/data/"
out_dir <- "C://Users/adeak/Dropbox (University of Oregon)/EWP/AFRI/outputs/"
setwd(wd)

##----------------------------
## INFLATION RATE USING PRODUCER PRICE INDEX (PPI)
##----------------------------
ppi <- read.csv(paste0(wd, "Monthly_PPI_data_2020-21.csv")) %>%
  mutate(DATE = as.Date(DATE, tryFormats = "%m/%d/%Y")) %>%
  mutate(month = format(DATE, "%m"), year = format(DATE, "%Y")) %>%
  group_by(year) %>%
  summarise(average_ppi = mean(PPI, trim = 0.1)) %>%
  mutate(new_ppi = ((195-average_ppi)/average_ppi)) %>%
  mutate(year = as.integer(year))

##----------------------------
## NATIONAL DATASET - CLEANING
##----------------------------

cleaned <- read.csv(paste0(wd, "national-data_cleaned.csv")) %>% 
  rename("Vendor.County" = county, "Vendor.State" = state, "Vendor.Zip.Code" = zip, "contract_last_fy" = contract_last_fiscal_yr, 
         "PSC" = Product.or.Service.Code, "pop.state" = Principal.Place.of.Performance.State.Code) %>%
  left_join(ppi, by = c(contract_last_fy = "year")) %>%
  distinct() %>%
  mutate(adjusted_contract_val = (total_contract_val + (total_contract_val * new_ppi)))

head(cleaned)
  

##----------------------------
## MOST COMMON WORK BEING DONE
##----------------------------
PSC_codes <- read.csv(paste0(wd, "PSC_CODES.csv"))
names(PSC_codes)[1] <- "PSC"

total_n_contracts <- cleaned %>% select(PIID, Unique.Entity.ID) %>% distinct() %>% nrow() # 69,698

# Total number of contracts awarded by PSC
work_US <- cleaned %>%
  select(PIID, Unique.Entity.ID, PSC) %>%
  distinct() %>% 
  group_by(PSC) %>%
  summarise(n_contracts = n()) %>%
  arrange(desc(n_contracts)) %>%
  left_join(PSC_codes) %>%
  ungroup() %>%
  mutate(percent = n_contracts/total_n_contracts * 100)
work_US

# Work stratified by year completed and PSC
work_US_by_fy <- cleaned %>%
  select(PIID, Unique.Entity.ID, PSC, contract_last_fy) %>%
  distinct() %>% 
  group_by(PSC, contract_last_fy) %>%
  summarise(n_contracts = n()) %>%
  arrange(desc(n_contracts)) %>%
  left_join(PSC_codes) %>%
  arrange(PSC, contract_last_fy) %>%
  ungroup()
work_US_by_fy

# write.csv(work_US_by_fy, paste0(wd,"numberofPSCs_by_fy.csv"))
work_US_by_fy <- read.csv(paste0(wd,"numberofPSCs_by_fy.csv"))

sum(work_US$n_contracts) # 61,701

ggplot(work_US_by_fy, aes(x=contract_last_fy, y = n_contracts, fill = Product.or.Service.Description)) +
  geom_area(position = "fill", color = "black", size = 0.2, alpha = 0.75) +
  scale_fill_manual(values = met.brewer("Redon", 14)) + 
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(title = "Proportion of contracts by service code, 2001-2020",
       xlab = "Last fiscal year of contract",
       ylab = "Percent of contracts")
# ggsave(paste0(out_dir, "figures/proportion_of_pscs_by_fy.pdf"))

ggplot(work_US_by_fy, aes(x=contract_last_fy, y = n_contracts, fill = Product.or.Service.Description)) +
  geom_area(color = "black", size = 0.2, alpha = 0.75) +
  scale_fill_manual(values = met.brewer("Redon", 14)) + 
  theme_classic() +
  labs(title = "Change in number of contracts by service code, 2001-2020",
       xlab = "Last fiscal year of contract",
       ylab = "Number of contracts")
# ggsave(paste0(out_dir, "figures/number_of_pscs_by_fy.pdf"))

##----------------------------
## RELATIONSHIP BETWEEN PLACE AND WORK BEING DONE - BY STATE
##----------------------------
# PSCs contracted by state
work_by_state <- cleaned %>%
  select(PIID, Unique.Entity.ID, PSC, pop.state) %>%
  distinct() %>% 
  group_by(PSC, pop.state) %>%
  summarise(n_contracts = n()) %>%
  arrange(desc(n_contracts)) %>%
  left_join(PSC_codes) %>%
  filter(pop.state != "")
work_by_state
sum(work_by_state$n_contracts) # 67,401

most_work_by_state <- work_by_state %>%
  group_by(pop.state) %>%
  slice(which.max(n_contracts))

work_done_by_state_shp <- st_read(paste0(wd, "GIS/tl_2021_us_state/tl_2021_us_state.shp")) %>%
  select(STUSPS) %>% 
  left_join(most_work_by_state, by=c(STUSPS ="pop.state")) %>%
  st_cast("POLYGON") %>%
  st_transform(crs=st_crs(5070))

ggplot(work_done_by_state_shp) +
  geom_sf(aes(fill = Product.or.Service.Description)) +
  theme_minimal() +
  ggtitle("Most common product or service code contracted by state, 2001-2020")
# ggsave(paste0(out_dir, "most_PSCs_by_state.png"))


##----------------------------
## RELATIONSHIP BETWEEN PLACE AND WORK BEING DONE - BY REGION
##----------------------------
# Step one, in ArcGIS found centroid of each county and spatially joined to region shapefile
state_fips <- read.csv(paste0(wd, "State_FIPS_codes.csv"))
usfs_regions <- st_read(paste0(wd, "GIS/USFS Regional Boundaries/S_USA.AdministrativeRegion.shp")) %>% st_transform(crs=st_crs(5070))
counties_by_reg <- st_read(paste0(wd, "GIs/national_data/counties_by_FS_region.shp")) %>% 
  as.data.frame() %>%
  mutate(NAME = toupper(NAME)) %>%
  select(STATEFP, NAME, NAMELSAD,REGIONNAME, REGION) %>%
  mutate(STATEFP = as.numeric(STATEFP)) %>%
  left_join(state_fips,by = c(STATEFP = "st"))

POP_regions <- counties_by_reg %>%
  left_join(cleaned, by = c(NAME = "Principal.Place.of.Performance.County.Name", stusps = "pop.state")) 
  
top_pscs_by_reg <- POP_regions%>%
  select(PIID, Unique.Entity.ID, PSC,REGION, REGIONNAME) %>%
  filter(!is.na(PSC) | !is.na(REGIONNAME)) %>%
  select(PIID, Unique.Entity.ID, PSC, REGION, REGIONNAME) %>%
  distinct() %>%
  group_by(PSC, REGIONNAME) %>%
  summarise(n_contracts = n()) %>%
  arrange(desc(n_contracts)) %>%
  left_join(PSC_codes) %>% 
  filter(!is.na(PSC) | !is.na(REGIONNAME)) %>%
  group_by(REGIONNAME) %>%
  slice(which.max(n_contracts))
# write.csv(top_pscs_by_reg, paste0("Most_PSCs_by_Region.csv"))

work_by_region <- st_read(paste0(wd, "GIS/USFS Regional Boundaries/S_USA.AdministrativeRegion.shp")) %>% 
  st_transform(crs=st_crs(5070)) %>%
  select(REGION, REGIONNAME) %>%
  left_join(top_pscs_by_reg)

# st_write(work_by_region, paste0("GIS/national_data/PSCS_by_FS_region.shp"))

ggplot(work_by_region) +
  geom_sf(aes(fill = Product.or.Service.Description)) +
  theme_minimal() +
  ggtitle("Most common product or service code contracted by USFS region, 2001-2020")
# ggsave(paste0(out_dir, "most_PSCs_by_region.png"))


## ALLUVIAL PLOT -----------------------------------------------------------------

library(ggalluvial)
library(scales)
library(MetBrewer)

POP_regions %>%
  select(Vendor.County, Vendor.State, stusps, NAME, NAMELSAD, PIID, Unique.Entity.ID, REGIONNAME) %>%
  na.omit() %>%
  distinct() %>%
  rename("pop.county" = NAME, "pop.state" = stusps, "pop.region" = REGIONNAME) %>%
  group_by(Vendor.County, Vendor.State, pop.state, pop.county) %>%
  mutate(n_contracts = n()) %>%
  select(Vendor.County, Vendor.State, pop.state, pop.county, pop.region, n_contracts, NAMELSAD) %>%
  ungroup() %>%
  distinct() %>%
  left_join(counties_by_reg, by = c(Vendor.County = "NAMELSAD", Vendor.State = "stusps")) %>%
  rename("vendor.region" = REGIONNAME) %>%
  select(vendor.region, pop.region, n_contracts) %>%
  distinct() %>%
  group_by(vendor.region, pop.region) %>%
  summarise(n_contracts = sum(n_contracts)) %>%
  arrange(n_contracts) %>%
  na.omit() %>%
  top_frac(0.5) %>%
  ggplot(aes(axis1 = vendor.region, axis2=pop.region, y = n_contracts)) +
  geom_alluvium(aes(fill = vendor.region), alpha = 0.75, decreasing = F) +
  geom_stratum(decreasing = F) +
  geom_text(stat = "stratum", decreasing = F, aes(label = after_stat(stratum))) +
  labs(title = "Top 50 percent of labor-intensive forestry work\ntaking place by USFS region, 2001-2020") +
  theme_void() +
  scale_fill_manual(values = met.brewer("Redon", 16)) +
  theme(legend.position = "none")
# ggsave(paste0(out_dir, "figures/alluvial_plot_regions.png"))


##-----------------------------------
## NUMBER & VALUE OF CONTRACTS BY REGION
##-----------------------------------
CONTRACTS_BY_REGION <- POP_regions %>%
  select(PIID, Unique.Entity.ID, REGION, REGIONNAME) %>%
  filter(!is.na(REGION)) %>%
  select(PIID, Unique.Entity.ID, REGION, REGIONNAME) %>%
  distinct() %>%
  group_by(REGIONNAME) %>%
  summarise(n_contracts = n()) %>%
  arrange(desc(n_contracts)) 
# write.csv(CONTRACTS_BY_REGION, paste0("Most_PSCs_by_Region.csv"))
sum(CONTRACTS_BY_REGION$n_contracts)

contract_dolls_by_reg <- POP_regions %>%
  select(PIID, Unique.Entity.ID, adjusted_contract_val, REGION, REGIONNAME) %>%
  na.omit() %>%
  distinct() %>%
  group_by(REGIONNAME) %>%
  summarise(sum_contracts = sum(adjusted_contract_val)) %>%
  arrange(desc(sum_contracts))
contract_dolls_by_reg

sum_contract_dolls <- sum(contract_dolls_by_reg$sum_contracts)
percent_contract_dolls <- contract_dolls_by_reg %>%
  mutate(percent_contracts = sum_contracts/sum_contract_dolls)
percent_contract_dolls


##-----------------------------------
## LOCATIONS BUSINESSES ARE BEING CONTRACTED FROM FOR EACH PSC
##-----------------------------------
pscs_by_state_businesses <- cleaned %>%
  select(PIID, Unique.Entity.ID, PSC, Vendor.State) %>%
  distinct() %>%
  group_by(PSC, Vendor.State) %>%
  summarise(n_contracts = n()) %>%
  group_by(PSC) %>%
  slice(which.max(n_contracts))
pscs_by_state_businesses 

pscs <- cleaned %>%
  select(PIID, Unique.Entity.ID, PSC) %>%
  distinct() %>%
  group_by(PSC) %>%
  summarise(n_total_contracts = n()) %>%
  left_join(pscs_by_state_businesses) %>%
  mutate(percent_contracts = n_contracts/n_total_contracts*100) %>%
  arrange(n_contracts)
pscs

