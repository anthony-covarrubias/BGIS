library("chron")
library("dplyr")
library("httr")
library("jsonlite")

source("C:/Users/anthony.covarrubias/EnerNOC, Inc/BOP - Documents/Projects/TD Bank/Building Model/Enablement/Parameter Updates/New Uploader/UploaderFunctions.R")
source("http://github.bdap.enernoc.net/raw/Labs-Tools/Building_Model_R/master/AFD_Functions.R")

# Pull system_id, measures_enabled, date_enabled, date_added, disabled_reason

####################
# # Basic Inputs # #
####################

# # Specify Customer
customer <- "TD Bank"
#customer <- "CIBC"

# # List of desired metadata parameters to include from TITAN (excluding system_id, name, class, type, and building_name)
property_list <- c("date_added",
                   "measures_enabled",    
                   "date_enabled",
                   "disabled_reason",
                   "entouch_space_type")

#  Subsetting of data to useful data frames
all_systems_raw <- GetSystems(customer, property_list)
all_systems <- all_systems_raw %>% filter(!is.na(all_systems_raw$date_added))
todays_date <- as.character(Sys.Date())

#################
# # Processes # #
#################

# # New Systems
# Find disabled measures due to new systems (in order to provide system IDs to TW)
disabled_systems <- all_systems %>% filter(measures_enabled == "FALSE")
new_systems <- disabled_systems %>% filter(disabled_reason == "new_system")
not_gateway <- new_systems %>% filter(!entouch_space_type == "Gateway Only" | is.na(entouch_space_type))

write.csv(not_gateway$branch_code, paste0("C:/Users/anthony.covarrubias/EnerNOC, Inc/BOP - Documents/Projects/TD Bank/Building Model/Enablement/System Queries/All/", gsub("-", "", todays_date),"_new_sites.csv"), row.names = FALSE)

# Once enabled, update properties
property_list <- list(measures_enabled = "TRUE",
                      disabled_reason = "NA",
                      date_enabled = todays_date)
for (i in 1:nrow(not_gateway)) {
    system_id <- not_gateway[[i,6]]
    edit_properties("production", system_id, property_list)
    print(paste0(i, "-", system_id))
}

# # Update new gateway systems
new_gateway_systems <- new_systems %>% filter(entouch_space_type == "Gateway Only")
for (j in 1:nrow(new_gateway_systems)) {
    system_id <- new_gateway_systems[[j,6]]
    edit_property("production", system_id, "disabled_reason", "gateway_system")
    print(paste0(j, "-", system_id))
}

# # Other
enabled_systems <- all_systems %>% filter(measures_enabled == "TRUE")
gateway_systems <- all_systems %>% filter(disabled_reason == "gateway_system")

