# READ ME ----

# As with the herpetofauna data, Melanie had on occassion moved the SRBP bird
# sampling locations and logged those movements. Those changes were recorded in
# an Excel spreadsheet, and needed to be incorporated into the DB for storage
# and reference. Changes to the herpetofauna sampling locations are detailed in
# the herpetofauna schema on stegosaurus (postgres). The data here document
# changes to the bird sampling locations. Note that changes to bird and herp
# sampling locations may coincide if, for example, an entire transect is moved.
# These changes should be reflected in both locations. Unlike the herpetofuna,
# however, the SRBP birds are treated as a part of the lter34 long-term bird
# monitoring program. In light of that, all bird monitoring sites (SRBP,
# survey200, riparian, NDV) are included in this location histories data set as
# well. This is something that we should have done long ago but did not, and is
# fortuitious now as many of these locations will move (or more appopriately be
# discontinued and new ones added) in light of the Smart Sampling program
# associated with CAP IV. Note that the core (non-SRBP) sampling locations are
# from Shero Holland's 2013 GPS'ing of the sites - any site movements (and there
# were some, e.g., F8) prior to that time are NOT reflected in these data. There
# is an earlier shapefile detailing the location of these sites (dataset # 160);
# most sites overlap but for the few where a locational discrepency is evident
# (e.g., F8), it is unclear if the site truly moved, the reason if so, and the
# timing of the move. Dataset # 160 is in PASTA so those data will remain
# available, but these data will be presented as of 2013.

# target fields:
# id,
# site_id,
# lat,
# long,
# begin_date,
# end_date,
# location_histories_notes

# libraries ----
library(stringr)
library(stringi)

# DB connections ----

# pg <- dbConnect(dbDriver("PostgreSQL"),
local <- dbConnect(MySQL(),
                   user='srearl',
                   password=.rs.askForPassword("Enter password:"),
                   dbname='lter34birds',
                   host='127.0.0.1')

# data acquisition ----

bird_sites <- dbGetQuery(con, "
SELECT
  sites.site_id,
  sites.site_code,
  sites.sample AS location_type,
  sites.description,
  sites.address
FROM lter34birds.sites;")

# SRBP Bird_Jan2013_May2014
birds_locations_1 <- read_excel('~/Dropbox (ASU)/localRepos/SaltRiverBiologicalProject/SRBP_BirdsHerps_latlongWchanges_072516.xlsx', sheet = 'Bird_Jan2013_May2014', skip = 1)

birds_locations_1 <- birds_locations_1  %>%
  filter(!is.na(name)) %>% 
  mutate(begin_date = NA) %>%
  mutate(begin_date_month = 1) %>%
  mutate(begin_date_year = 2013) %>%
  mutate(end_date = NA) %>%
  mutate(end_date_month = NA) %>%
  mutate(end_date_year = NA) %>%
  mutate(location_histories_notes = NA)
  
# SRBP Bird_May2014_April2016
birds_locations_2 <- read_excel('~/Dropbox (ASU)/localRepos/SaltRiverBiologicalProject/SRBP_BirdsHerps_latlongWchanges_072516.xlsx', sheet = 'Bird_May2014_April2016', skip = 3)

birds_locations_2 <- birds_locations_2 %>%
  filter(!is.na(Note)) %>% 
  select(-`Old Name                 (if applicable)`) %>% 
  mutate(name = stri_replace_first_regex(name, "[0-9]+", "")) %>% # take out Melanie's 2 for new location
  mutate(begin_date = NA) %>%
  mutate(begin_date_month = 6) %>%
  mutate(begin_date_year = 2014) %>%
  mutate(end_date = NA) %>%
  mutate(end_date_month = NA) %>%
  mutate(end_date_year = NA) %>%
  mutate(location_histories_notes = Note) %>% 
  select(-Note)

end_dates_1_2 <- birds_locations_2$name # get the plot ids of those that have changed to update herps 1
birds_locations_1[birds_locations_1$name %in% end_dates_1_2,]$end_date_month <- 5
birds_locations_1[birds_locations_1$name %in% end_dates_1_2,]$end_date_year <- 2014

# SRBP Bird_April2016_current
birds_locations_3 <- read_excel('~/Dropbox (ASU)/localRepos/SaltRiverBiologicalProject/SRBP_BirdsHerps_latlongWchanges_072516.xlsx', sheet = 'Bird_April2016_current', skip = 3)

colnames(birds_locations_3)[5] <- "note"

birds_locations_3 <- birds_locations_3 %>%
  filter(!is.na(note)) %>% 
  select(-`Old Name                 (if applicable)`) %>% 
  mutate(name = stri_replace_first_regex(name, "2", "")) %>% # take out Melanie's 2 for new location
  mutate(begin_date = NA) %>%
  mutate(begin_date_month = 4) %>%
  mutate(begin_date_year = 2016) %>%
  mutate(end_date = NA) %>%
  mutate(end_date_month = NA) %>%
  mutate(end_date_year = NA) %>%
  mutate(location_histories_notes = note) %>% 
  select(-note)

# careful here, updating end dates should work okay this way UNLESS the same plot is updated multiple times
end_dates_2_3 <- birds_locations_3$name # get the plot ids of those that have changed to update herps 1
birds_locations_1[birds_locations_1$name %in% end_dates_2_3,]$end_date_month <- 3
birds_locations_1[birds_locations_1$name %in% end_dates_2_3,]$end_date_year <- 2016

bird_site_to_join <- bird_sites %>% 
  filter(location_type == "SRBP") %>% 
  mutate(site_code = gsub("_CORE", "", site_code)) %>% 
  select(site_id, site_code) %>% 
  mutate(site_code = toupper(site_code))

birds_locations_histories <- bind_rows(birds_locations_1, birds_locations_2, birds_locations_3) %>% 
  mutate(name = toupper(name))

birds_locations_histories <- left_join(birds_locations_histories, bird_site_to_join, by = c("name" = "site_code"))

# add core birds ----

core_birds_gps <- read_csv('~/Desktop/core_birds_spatial_merged/core_birds_survey_locations.csv') 

core_site_to_join <- bird_sites %>% 
  filter(location_type %in% c("200 point", "Riparian", "North Desert Village")) %>% 
  mutate(site_code = gsub("_CORE", "", site_code)) %>% 
  select(site_id, site_code)

core_birds_gps <- full_join(core_birds_gps, core_site_to_join, by = c("site_code")) %>% 
  mutate(id = NA) %>% 
  select(site_id, lat = Y, long = X)
  
birds_locations_histories <- bind_rows(core_birds_gps, birds_locations_histories)
  
birds_locations_histories <- birds_locations_histories %>% 
  mutate(id = NA) %>% 
  select(id, site_id, lat, long, begin_date:location_histories_notes) %>% 
  mutate(begin_date = as.Date(begin_date)) %>% 
  mutate(end_date = as.Date(end_date)) %>% 
  mutate(end_date_month = replace(end_date_month, site_id == 30, 8)) %>% 
  mutate(end_date_year = replace(end_date_year, site_id == 30, 2001))

# add data to MySQL ----

# write to database
if (dbExistsTable(con, 'birds_location_histories')) dbRemoveTable(con, 'birds_location_histories') # make sure tbl does not exist
dbWriteTable(con, 'birds_location_histories', value = birds_locations_histories, row.names = F)

# add primary key, modify columns as needed (site_id to INT to match
# sites.site_id type), note that MySQL will auto-populate the auto-increment
# primary key
dbSendQuery(con, '
ALTER TABLE birds_location_histories
  MODIFY id INTEGER AUTO_INCREMENT PRIMARY KEY,
  MODIFY COLUMN begin_date DATE,
  MODIFY COLUMN end_date DATE,
  MODIFY COLUMN site_id INTEGER NOT NULL
')

# add foreign key to sites table
dbSendQuery(con, '
ALTER TABLE lter34birds.birds_location_histories
  ADD CONSTRAINT birds_location_histories_fk_sites
  FOREIGN KEY (site_id)
  REFERENCES sites(site_id);')