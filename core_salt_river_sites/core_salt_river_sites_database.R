
# README ------------------------------------------------------------------

# The backstory is that one of the SRBP sites at the Ave67 reach that we
# consider a core site had changed from mid to down in April 2016 but this went
# unnoticed until summer 2019. Previous data entries were corrected using the
# web app by Sally and Shero, but to avoid this in the future, we will remove
# the core desigation from the site name (in the database, datasheets, and
# otherwise), and keep a record of which SRBP site is considered the core site
# in the database. The workflow documents creating a table to track that
# information.

# The workflow here is to create and populate the table to catalog which SRBP
# site is the core site; please see the database change log for documentation as
# to removing the text "_CORE" from the site names.

# To Sally: So, just to be sure, I am going to: 1. remove the text "_CORE" from
# all of the site names 2. store in another location in the database which sites
# are core, which is derived from the name (e.g., Rio_mid_B2 is the Rios core
# site for that location) 3. the exception to #2 will be that Ave67_mid_B1 was
# the Ave67 core site to April 2016, but Ave67_dwn_B1 was and is the Ave67 core
# site beginning April 2016


# libraries ---------------------------------------------------------------

library(tidyverse)


# connections -------------------------------------------------------------

source('mysql_prod.R')
mysql <- mysql_prod_connect('lter34birds')

source('mysql_local.R')
mysql <- mysql_local_connect('lter34birds')


# develop core site designation table -------------------------------------

salt_river_core_sites <- dbGetQuery(mysql,
                                    'SELECT * FROM lter34birds.sites;') %>% 
  filter(grepl("srbp", sample, ignore.case = T)) %>% 
  select(site_id, site_code) %>% 
  mutate(
    id = as.integer(NA),
    reach = str_extract(site_code, "^[^_]+(?=_)"),
    begin_date = as.Date(NA),
    end_date = as.Date(NA)
  ) %>% 
  mutate(
    begin_date = replace(begin_date, site_code == "Ave67_dwn_B1", "2016-04-01"),
    end_date = replace(end_date, site_code == "Ave67_mid_B1_CORE", "2016-03-31")
  ) %>% 
  filter(grepl("core", site_code, ignore.case = T) | !is.na(begin_date)) %>% 
  select(-site_code) %>% 
  select(id, everything())


# write core sites table to database --------------------------------------

if (dbExistsTable(mysql, 'salt_river_core_sites')) dbRemoveTable(mysql, 'salt_river_core_sites')
dbWriteTable(mysql, 'salt_river_core_sites', value = salt_river_core_sites, row.names = F)

# add primary key, modify columns as needed (site_id to INT to match
# sites.site_id type), note that MySQL will auto-populate the auto-increment
# primary key
dbSendQuery(mysql, '
ALTER TABLE lter34birds.salt_river_core_sites
  MODIFY id INTEGER AUTO_INCREMENT PRIMARY KEY,
  MODIFY COLUMN begin_date DATE,
  MODIFY COLUMN end_date DATE,
  MODIFY COLUMN site_id INTEGER NOT NULL
')

# add foreign key to sites table
dbSendQuery(mysql, '
ALTER TABLE lter34birds.salt_river_core_sites
  ADD CONSTRAINT salt_river_core_sites_fk_sites
  FOREIGN KEY (site_id)
  REFERENCES sites(site_id);')