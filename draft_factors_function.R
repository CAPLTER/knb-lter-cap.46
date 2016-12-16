# questions for Susannah:



# reml slots ----
getSlots("dataset")
  getSlots("distribution")
  getSlots("keywordSet")
    getSlots("keyword")
getSlots("dataTable")
getSlots("physical")
  getSlots("size")
  getSlots("distribution")
    getSlots("online")
      getSlots("url")
getSlots("additionalInfo")
  getSlots("section")
  getSlots("para")
getSlots("metadataProvider")
  getSlots("individualName")
  getSlots("userId")
getSlots("creator")
  getSlots("individualName")
  getSlots("userId")

# libraries ----
library("EML")
library('RPostgreSQL')
library('RMySQL')
library('tidyverse')
library("tools")
library("readr")
library("readxl")

# functions and working dir ----
source('~/Dropbox (ASU)/localRepos/dataPublishing/writeAttributesFn.R')
source('~/Dropbox (ASU)/localRepos/dataPublishing/createKMLFn.R')
source('~/Dropbox (ASU)/localRepos/dataPublishing/createdataTableFn.R')
source('~/Dropbox (ASU)/localRepos/dataPublishing/createDataTableFromFileFn.R')
source('~/Dropbox (ASU)/localRepos/dataPublishing/address_publisher_contact_language_rights.R')
source('~/Dropbox (ASU)/localRepos/dataPublishing/createOtherEntityFn.R')
setwd("~/Dropbox (ASU)/localRepos/lterBirdMonitoring/")

# DB connections ----
con <- dbConnect(MySQL(),
                 user='srearl',
                 password=.rs.askForPassword("Enter password:"),
                 dbname='',
                 host='stegosaurus.gios.asu.edu')

pg <- dbConnect(dbDriver("PostgreSQL"),
                 user="srearl",
                 dbname="working",
                 host="localhost",
                 password=.rs.askForPassword("Enter password:"))

pg <- dbConnect(dbDriver("PostgreSQL"),
                 user="srearl",
                 dbname="caplter",
                 host="stegosaurus.gios.asu.edu",
                 password=.rs.askForPassword("Enter password:"))

# dataset details to set first ----
projectid <- 640
packageIdent <- 'knb-lter-cap.640.1'
pubDate <- '2016-11-30'

# data processing ----

# species_density

HOA_diversity <- read_excel('~/Dropbox (ASU)/localRepos/lterBirdMonitoring/SBL_HOAs_diversity/HOAs and urban diversity.xlsx', sheet = 'HOA_diversity')

HOA_diversity <- HOA_diversity %>% 
  mutate(HOA = as.factor(HOA)) %>% 
  mutate(`CCR no HOA` = as.factor(`CCR no HOA`)) %>% 
  mutate(CCnR = as.factor(CCnR)) %>% 
  mutate(ARC = as.factor(ARC)) %>% 
  mutate(M_Weed = as.factor(M_Weed)) %>% 
  mutate(M_Trim = as.factor(M_Trim)) %>% 
  mutate(M_Pest = as.factor(M_Pest)) %>% 
  mutate(M_Disease = as.factor(M_Disease)) %>% 
  mutate(Encroach = as.factor(Encroach)) %>% 
  mutate(P_Height = as.factor(P_Height)) %>% 
  mutate(SpeciesP = as.factor(SpeciesP)) %>% 
  mutate(SpeciesR = as.factor(SpeciesR)) %>% 
  mutate(`Species L` = as.factor(`Species L`)) %>% 
  mutate(TurfP = as.factor(TurfP)) %>% 
  mutate(TurfR = as.factor(TurfR)) %>% 
  mutate(Drainage = as.factor(Drainage)) %>% 
  mutate(Topography = as.factor(Topography)) %>% 
  mutate(Irrigation = as.factor(Irrigation)) %>% 
  mutate(`Trash R` = as.factor(`Trash R`)) %>% 
  mutate(`Trash C` = as.factor(`Trash C`)) %>% 
  mutate(`Perch A` = as.factor(`Perch A`)) %>% 
  mutate(`Perch C` = as.factor(`Perch C`)) %>% 
  mutate(`Perch S` = as.factor(`Perch S`)) %>% 
  mutate(`Temp Structures` = as.factor(`Temp Structures`)) %>% 
  mutate(`Animals D` = as.factor(`Animals D`)) %>% 
  mutate(`Animals L` = as.factor(`Animals L`)) %>% 
  mutate(Noise = as.factor(Noise)) %>% 
  mutate(`SHAN PLNT` = as.numeric(`SHAN PLNT`)) %>% 
  mutate(`SHAN BUG` = as.numeric(`SHAN BUG`)) %>% 
  select(-contains("COORD")) # remove s200 plot location data
  
writeAttributes(HOA_diversity) # write data frame attributes to a csv in current dir to edit metadata
HOA_diversity_desc <- "characteristics of HOAs in the greater Phoenix metropolitan area, and corresponding animal (arthropod) and plant diversity within those communities"

# `Yard Type` <- c(Mesic = "yard characterized by turf and exotic plants",
#                  Xeric = "yard characterized by crushed gravel and drought tolerant vegetation")
# Treatment <- c(Bush = "seed tray positioned next to plants",
#                Open = "seed tray positioned 3 meters away from plants")

# HOA_diversity_factors <- rbind(
#   data.frame(
#     attributeName = "HOA",
#     code = names(HOA),
#     definition = unname(HOA)
#   ))

# lots of factors here, and they are all the same (i.e., 1=yes, 2=no), so
# instead of typing all that out, let's just create all of those objects in a
# loop
listOfFactors <- sapply(HOA_diversity, is.factor)
trueList <- which(listOfFactors)
for(i in 1:length(trueList)) assign(names(trueList)[i], c(`1` = "yes", `2` = "no"))

# instead of typing out all of those factors into separate data frames, lets loop through them - and this will be the basis of a new function to handle this in the future
HOA_diversity_factors <- data.frame()
for(i in 1:length(trueList)) {
factor_elements <- get(names(trueList)[i])
temp_frame <- rbind(
  data.frame(
    attributeName = names(trueList)[i],
    code = names(factor_elements),
    definition = unname(factor_elements)
  ))
HOA_diversity_factors <- rbind(HOA_diversity_factors, temp_frame)
}

HOA_diversity_DT <- createDTFF(dfname = HOA_diversity,
                               factors = HOA_diversity_factors,
                               description = HOA_diversity_desc)


# title and abstract ----
title <- 'Homeowner Associations as a vehicle for promoting native urban biodiversity'
abstract <- 'The loss of habitat due to suburban and urban development represents one of the greatest threats to biodiversity. Conservation developments have emerged as a key player for reconciling new ex-urban residential development with ecosystem services. However, since more than half of the world population live in urban and suburban developments, identifying conservation partners to facilitate with retrofitting existing residential neighborhoods becomes paramount. Homeowner Associations (HOA) manage a significant proportion of residential developments in the United States, which includes the landscape design for yards and gardens. These areas have the potential to mitigate the loss of urban biodiversity when they provide habitat for native wildlife. Therefore the conditions and restrictions imposed upon the homeowner by the HOA could have profound effects on the local wildlife habitat. We explored the potential of HOAs to promote conservation by synthesizing research from three monitoring programs from Phoenix, AZ. We compared native bird diversity, arthropod diversity, and plant diversity between neighborhoods with and without an HOA. Neighborhoods belonging to HOAs had significantly greater bird and plant diversity, though insect diversity did not differ. The institutional framework structuring HOAs including sanctions for enforcement coupled with a predictable maintenance regime that introduces regular disturbance might explain why neighborhoods with an HOA had greater bird diversity. For neighborhoods with an HOA, we analyzed landscape form and management practices. We linked these features with ecological function and suggested how to modify management practices by adopting strategies from the Sustainable Sites Initiative, an international sustainable landscaping program, to help support biodiversity in current and future residential landscapes.'


# people ----

ASU <- "Arizona State University"

slb_name <- new('individualName',
                givenName = 'Susannah',
                surName = 'Lerman')

susannahLerman <- new('creator',
                       individualName = slb_name,
                       organizationName = 'USDA Forest Service Northern Research Station',
                       electronicMailAddress = "slerman@cns.umass.edu")

creators <- as(susannahLerman, 'creator')

susannahLerman <- new('metadataProvider',
                       individualName = slb_name,
                       organizationName = 'USDA Forest Service Northern Research Station',
                       electronicMailAddress = "slerman@cns.umass.edu")

metadataProvider <- as(susannahLerman, 'metadataProvider')


# keywords ----
keywordSet <-
  c(new("keywordSet",
        keywordThesaurus = "LTER controlled vocabulary",
        keyword =  c("urban",
                     "biodiversity",
                     "community structure",
                     "conservation")),
    new("keywordSet",
        keywordThesaurus = "LTER core areas",
        keyword =  c("disturbance patterns",
                     "population studies")),
    new("keywordSet",
        keywordThesaurus = "Creator Defined Keyword Set",
        keyword =  c("residential landscapes",
                     "Homeowner Associations",
                     "development",
                     "Sustainable Sites Initiative")),
    new("keywordSet",
        keywordThesaurus = "CAPLTER Keyword Set List",
        keyword =  c("cap lter",
                     "cap",
                     "caplter",
                     "central arizona phoenix long term ecological research",
                     "arizona",
                     "az",
                     "arid land"))
    )

# Homeowner Associations
# residential landscapes
# urban biodiversity
# CAP LTER
# Conservation development
Sustainable Sites Initiative	

# methods and coverages ----
methodsFile <- '~/Dropbox (ASU)/localRepos/lterBirdMonitoring/SBL_HOAs_diversity/SBL_HOA_diversity_methods.md'
methods <- set_methods(methods_file = methodsFile)

begindate <- "2009-10-01"
enddate <- "2012-06-01"
geographicDescription <- "CAP LTER study area"
coverage <- set_coverage(begin = begindate,
                         end = enddate,
                         geographicDescription = geographicDescription,
                         west = -113.34, east = -111.59,
                         north = +34.01, south = +32.91)

# construct the dataset ----

# address, publisher, contact, and rights come from a sourced file

# XML DISTRUBUTION
  xml_url <- new("online",
                 onlineDescription = "CAPLTER Metadata URL",
                 url = paste0("https://sustainability.asu.edu/caplter/data/data-catalog/view/", packageIdent, "/xml/"))
metadata_dist <- new("distribution",
                 online = xml_url)

# DATASET
dataset <- new("dataset",
               title = title,
               creator = creators,
               pubDate = pubDate,
               metadataProvider = metadataProvider,
               intellectualRights = rights,
               abstract = abstract,
               keywordSet = keywordSet,
               coverage = coverage,
               contact = contact,
               methods = methods,
               distribution = metadata_dist,
               dataTable = c(HOA_diversity_DT)) 

# construct the eml ----

# ACCESS
allow_cap <- new("allow",
                 principal = "uid=CAP,o=LTER,dc=ecoinformatics,dc=org",
                 permission = "all")
allow_public <- new("allow",
                    principal = "public",
                    permission = "read")
lter_access <- new("access",
                   authSystem = "knb",
                   order = "allowFirst",
                   scope = "document",
                   allow = c(allow_cap,
                             allow_public))

# CUSTOM UNITS
# standardUnits <- get_unitList()
# unique(standardUnits$unitTypes$id) # unique unit types

custom_units <- rbind(
  data.frame(id = "microsiemenPerCentimeter",
             unitType = "conductance",
             parentSI = "siemen",
             multiplierToSI = 0.000001,
             description = "electric conductance of lake water in the units of microsiemenPerCentimeter"),
data.frame(id = "nephelometricTurbidityUnit",
           unitType = "unknown",
           parentSI = "unknown",
           multiplierToSI = 1,
           description = "(NTU) ratio of the amount of light transmitted straight through a water sample with the amount scattered at an angle of 90 degrees to one side"))
unitList <- set_unitList(custom_units)

eml <- new("eml",
           packageId = packageIdent,
           scope = "system",
           system = "knb",
           access = lter_access,
           dataset = dataset)

# write the xml to file ----
write_eml(eml, "knb-lter-cap.640.1.xml")
