
prod <- dbConnect(MySQL(),
                 user='srearl',
                 password=.rs.askForPassword("Enter password:"),
                 dbname='gios2_production',
                 host='mysql.prod.aws.gios.asu.edu')

eyal <- dbGetQuery(prod, "
SELECT
	people.first_name,
	people.last_name,
	people.email,
	people_address.institution,
	people_address.department
FROM gios2_production.people
JOIN gios2_production.people_address ON (people.person_id = people_address.person_id)
WHERE people.last_name LIKE 'shochat'
;")

eyal_name <- new('individualName',
                 givenName = eyal$first_name,
                 surName = eyal$last_name)

eyalOrcid <- new('userId',
                 'http://orcid.org',
                 directory = 'orcid.org')

eyalShochat <- new('creator',
                   individualName = eyal_name,
                   organizationName = eyal$institution,
                   electronicMailAddress = eyal$email,
                   userId = eyalOrcid)


# functionalize

# checking for presence of orcid not feasible because the field does not exist,
# need to add this to the DB before we can develop this fn.
addCreator <- function(lastName) {
  
  personDetails <- dbGetQuery(prod, paste0("
  SELECT
  	people.first_name,
  	people.last_name,
  	people.email,
  	people_address.institution,
  	people_address.department
  FROM gios2_production.people
  JOIN gios2_production.people_address ON (people.person_id = people_address.person_id)
  WHERE people.last_name LIKE '", lastName, "';"))
  
  creator_name <- new('individualName',
                      givenName = personDetails$first_name,
                      surName = personDetails$last_name)
  
  # if(!is.na(personDetails$orcid)) {
    creator_orcid <- new('userId',
                         personDetails$orcid,
                         directory = 'orcid.org')
  # }
  
  # if(!is.na(personDetails$orcid)) {
  newCreator <- new('creator',
                     individualName = creator_name,
                     organizationName = personDetails$institution,
                     electronicMailAddress = personDetails$email,
                     userId = creator_orcid) 
  # } else {
  # newCreator <- new('creator',
  #                    individualName = creator_name,
  #                    organizationName = personDetails$institution,
  #                    electronicMailAddress = personDetails$email)
  #   
  # }

}



# personDetails <- function(lastName) {  
#   
#   dbGetQuery(prod, paste0("
#     SELECT
#     	people.first_name,
#     	people.last_name,
#     	people.email,
#     	people_address.institution,
#     	people_address.department
#     FROM gios2_production.people
#     JOIN gios2_production.people_address ON (people.person_id = people_address.person_id)
#     WHERE people.last_name LIKE '", lastName, "';"))
# }

############# EXISTING

heh_name <- new('individualName',
                givenName = 'Hilairy',
                surName = 'Hartnett')

heh_orcid <- new('userId',
                 'http://orcid.org/0000-0003-0736-7844',
                 directory = 'orcid.org')

hilairyHartnett <- new('creator',
                       individualName = heh_name,
                       organizationName = ASU,
                       electronicMailAddress = "h.hartnett@asu.edu",
                       userId = heh_orcid)

# WE CAN ADD @id ATTRIBUTE !
eyalTest <- new('creator',
                   individualName = eyal_name,
                   organizationName = eyal$institution,
                   electronicMailAddress = eyal$email,
                id = eyal$last_name)

getSlots("keyword") - off topic here but keywordType is a value sensu directory in userId above...hmmm