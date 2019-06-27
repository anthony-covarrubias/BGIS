library("chron")
library("dplyr")
library("httr")
library("jsonlite")

source("C:/Users/anthony.covarrubias/EnerNOC, Inc/BOP - Documents/Projects/TD Bank/Building Model/Enablement/Parameter Updates/New Uploader/UploaderFunctions.R")
source("http://github.bdap.enernoc.net/raw/Labs-Tools/Building_Model_R/master/AFD_Functions.R")

####################
# # Basic Inputs # #
####################

# # Specify Customer
customer <- "TD Bank"
#customer <- "CIBC"

# # List of desired metadata parameters to include from TITAN (excluding system_id, name, class, type, and building_name)
property_list <- c("template",
                   "entouch_unit_id",    
                   "entouch_space_type",
                   "entouch_meter_type",
                   "entouch_branch",
                   "entouch_facility_id")

#  Subsetting of data to useful data frames
all_systems_raw <- GetSystems(customer, property_list)
all_systems <- all_systems_raw %>% filter(!is.na(template))
    conf_systems <- all_systems_raw %>% filter(!is.na(template))
    unconf_systems <- all_systems_raw %>% filter(is.na(template))

fan_systems <- conf_systems %>% filter(class == "fan_systems")
submeter_systems <- conf_systems %>% filter(class == "submeters")
    #error_submeter_systems <- submeter_systems %>% filter(is.na(entouch_branch)) # ones that need branch #
    #submeter_systems <- submeter_systems %>% filter(!is.na(entouch_branch))


#############################################
# # Read and Format Data from ENTOUCH API # #
#############################################

# Read in entouch_category/AFD template mapping file
AFD_mappings <- read.csv("https://github.bdap.enernoc.net/raw/Labs-Tools/TD_Bank_Setup.R/master/category_AFD_mappings.csv")

# Read in CSV files generated from TD_Bank_Setup.R\Entouch_Information (must have been pushed to Github)
entouch_units <- read.csv("https://github.bdap.enernoc.net/raw/Labs-Tools/TD_Bank_Setup.R/master/Entouch_Information/Entouch_Unit_Hours.csv", row.names = NULL, stringsAsFactors = FALSE)
entouch_bldgs <- read.csv("https://github.bdap.enernoc.net/raw/Labs-Tools/TD_Bank_Setup.R/master/Entouch_Information/Entouch_Building_Hours.csv", row.names = NULL, stringsAsFactors = FALSE)
entouch_meters <-  read.csv("https://github.bdap.enernoc.net/raw/Labs-Tools/TD_Bank_Setup.R/master/Entouch_Information/entouch_meter_info.csv", row.names = NULL, stringsAsFactors = FALSE)

# Format data into R-friendly forms
schedule_df <- FormatBldgSchedule(entouch_bldgs, customer)
offset_df <- FormatOffsets(entouch_units, customer)
setpoints_df <- FormatSetpoints(entouch_units, customer)
meters_df <- FormatMeters(entouch_meters, customer)

# TODO: bring in API functions into this script

###################
# # Fan Systems # #
###################

fan_systems_slim <- SlimFanSystems(fan_systems)
fan_schedules <- merge(fan_systems_slim, offset_df[,c(4:8)], all.x = FALSE, by = c("entouch_facility_id", "entouch_unit_id"))
fan_schedules <- merge(fan_schedules, schedule_df[,c(3:18)], all.x = FALSE, by = c("entouch_facility_id", "entouch_customer_id"))
fan_schedules_woffset <- ApplyOffsets(fan_schedules, smart_recovery_time = "01:00:00")

fan_setpoints <- merge(fan_systems_slim[,c(1:4,6)], setpoints_df[,c(1,4,7,8)], by = c("entouch_unit_id", "branch_code"))
    colnames(fan_setpoints)[c(6,7)] <- c("Para1848", "Para1847")
    # to account for user adjustment, add 2.0 F to Para1847 and subtract 2.0 F from Para1848 before uploading setpoint values
    fan_setpoints$Para1847 <- as.character(fan_setpoints$Para1847 + 2)
    fan_setpoints$Para1848 <- as.character(fan_setpoints$Para1848 - 2)

fan_names <- merge(fan_systems_slim, offset_df[,c(1,3:5)], by = c("branch_code", "entouch_facility_id", "entouch_unit_id"))  # TODO: remove when facility id is fixed


#################
# # Submeters # #
#################

submeter_systems_slim <- SlimSubmeters(submeter_systems)

meter_cat <- merge(submeter_systems_slim, meters_df[,c(3:9)], by = c("entouch_facility_id", "entouch_unit_id", "entouch_branch"))
    colnames(meter_cat)[c(6,9,13)] <- c("old_name","old_meter_type", "new_meter_type")
    meter_cat$new_name <- paste0(meter_cat$entouch_panel_name,"-",meter_cat$entouch_branch,"-",meter_cat$entouch_branch_name)
    meter_cat$new_meter_type <- gsub(" ", "_", meter_cat$new_meter_type)
    meter_cat$new_meter_type <- toupper(meter_cat$new_meter_type)

bldg_offsets <- offset_df[,c(1,2,5:8)] %>% distinct(entouch_facility_id, .keep_all = TRUE)

submeter_schedules <- merge(submeter_systems_slim, bldg_offsets, by = c("entouch_facility_id", "branch_code"))
submeter_schedules <- merge(submeter_schedules, schedule_df[,c(3:18)], all.x = FALSE, by = c("entouch_facility_id", "entouch_customer_id"))

# Current templates used: 1, 3, 5, 6, 9, 12, 13
# Assumption: offsets are unique to each unit but need to assume they're all equal since we don't know which BBH is tied to which AHU

# 12.Schedule_and_Season (Baseboard Heaters, Duct Heaters, Heaters) - includes offsets but not smart recovery period
submeter_schedules_12 <- submeter_schedules %>% filter(template == "12.Schedule_and_Season")
submeter_schedules_12 <- ApplyOffsets(submeter_schedules_12, smart_recovery_time = "00:00:00")

# 13.Scheduled_HVAC
submeter_schedules_13 <- submeter_schedules %>% filter(template == "13.Scheduled_HVAC")
submeter_schedules_13 <- ApplyOffsets(submeter_schedules_13, smart_recovery_time = "01:00:00")

# 5.Scheduled_Occasionally_Off
submeter_schedules_5 <- submeter_schedules %>% filter(template == "5.Scheduled_Occasionally_Off")
submeter_schedules_5 <- ApplyOffsets(submeter_schedules_5, smart_recovery_time = "01:00:00")

# 3.Scheduled_Default
submeter_schedules_3 <- submeter_schedules %>% filter(template == "3.Scheduled_Default")
    # for interior lighting, assume 1 hour open offset and 2 hour close offset
    submeter_schedules_3$offset_open <- "01:00:00"
    submeter_schedules_3$offset_open <- chron(times. = submeter_schedules_3$offset_open, format = "hh:mm:ss")
    submeter_schedules_3$offset_close <- "02:00:00"
    submeter_schedules_3$offset_close <- chron(times. = submeter_schedules_3$offset_close, format = "hh:mm:ss")
submeter_schedules_3 <- ApplyOffsets(submeter_schedules_3, smart_recovery_time = "00:00:00")

# 9.Scheduled_No_Cycling (exterior lighting)
submeter_schedules_9 <- Type9Format(submeter_schedules, 
                                    Start1 = "00:00:00", 
                                    Stop1 = "08:00:00", 
                                    Start2 = "18:00:00", 
                                    Stop2 = "23:59:59")


# 6.Scheduled_With_Baseload_Check
submeter_schedules_6 <- submeter_schedules %>% filter(template == "6.Scheduled_With_Baseload_Check")
    # Add 2 close offset to account for branch shutdown, etc. 
    submeter_schedules_6$offset_close <- "02:00:00"
    submeter_schedules_6$offset_close <- chron(times. = submeter_schedules_6$offset_close, format = "hh:mm:ss")
submeter_schedules_6 <- ApplyOffsets(submeter_schedules_6, smart_recovery_time = "01:00:00")
    
# Combine all submeter schedules
submeter_schedules_combined <- rbind(submeter_schedules_3, 
                                  submeter_schedules_5, 
                                  submeter_schedules_6,
                                  submeter_schedules_12, 
                                  submeter_schedules_13)


###############
# # Uploads # #
###############
todays_date <- as.character(Sys.Date())

# Prepare data
fan_schedules_final <- fan_schedules_woffset[,c(grep("system_id", colnames(fan_schedules_woffset)),grep("SSPara001", colnames(fan_schedules_woffset)):ncol(fan_schedules_woffset))] %>% mutate_all(as.character)
fan_setpoints_final <- fan_setpoints[,c(grep("system_id", colnames(fan_setpoints)), grep("Para1847", colnames(fan_setpoints)), grep("Para1848", colnames(fan_setpoints)))] %>% mutate_all(as.character)
fan_names_final <- fan_names[,c(grep("system_id", colnames(fan_names)), grep("entouch_unit_name", colnames(fan_names)))] %>% mutate_all(as.character)
    colnames(fan_names_final)[2] <- "name"
fans <- merge(fan_schedules_final, fan_setpoints_final)
    fans$date_last_updated <- todays_date

submeter_cat_final <- meter_cat[,c(grep("system_id", colnames(meter_cat)), grep("new_meter_type", colnames(meter_cat)), grep("new_name", colnames(meter_cat)))] %>% mutate_all(as.character)
    colnames(submeter_cat_final)[2:3] <- c("entouch_meter_type", "name")
    submeter_cat_final$date_last_updated <- todays_date
submeter_schedules_final <- submeter_schedules_combined[,c(grep("system_id", colnames(submeter_schedules_combined)),grep("SSPara001", colnames(submeter_schedules_combined)):ncol(submeter_schedules_combined))] %>% mutate_all(as.character)
submeter_schedules_final_9 <- submeter_schedules_9
#submeters <- merge(submeter_schedules_final, submeter_cat_final[,c(1:2)])

all_names <- rbind(fan_names_final, submeter_cat_final[,c(1,3)])

fan_mismatch <- merge(fan_systems, fans, by = "system_id", all = TRUE)
fan_mismatch <- fan_mismatch %>% filter(is.na(Para1847))
if (nrow(fan_mismatch) > 0) {
    print("There are mismatched fan systems.")
}

# Upload 
#result <- BMUpload(fans, submeters, all_names, submeter_schedules_final_9)
result <- BMUpload(fans, all_names, submeter_cat_final[,c(1:2,4)], submeter_schedules_final, submeter_schedules_final_9)


###################
# # To-do Tasks # #
###################
# TODO: optimize so we're only updating values that are different than what's already there (i.e. efficiency)
# TODO: separate script to update AFD based on entouch_category
# TODO: space_type should be updated via trimkeys

#####################
# # Misc. Scripts # #
#####################
# mains <- submeter_systems %>% filter(entouch_meter_type == "TOTAL_ENERGY")
# write.csv(mains, "C:/Users/anthony.covarrubias/EnerNOC, Inc/BOP - Documents/Projects/TD Bank/Building Model/Enablement/Parameter Updates/20190523-td_submeter_mains.csv")
# meter_mismatch <- merge(submeter_systems, submeter_schedules_final, by = "system_id", all = TRUE)
# meter_mismatch <- meter_mismatch %>% filter(is.na(SSPara010))
# meter_mismatch <- meter_mismatch %>% filter(!template == "9.Scheduled_No_Cycling")
# meter_mismatch <- meter_mismatch %>% filter(!entouch_meter_type == "TOTAL_ENERGY")
# write.csv(fan_systems, "C:/Users/anthony.covarrubias/EnerNOC, Inc/BOP - Documents/Projects/TD Bank/Building Model/Enablement/Parameter Updates/20190523-td_fan_systems.csv")
# to_save <- submeter_schedules_13 %>% filter(entouch_category == "HVAC")
# write.csv(to_save[,6], "C:/Users/anthony.covarrubias/EnerNOC, Inc/BOP - Documents/Projects/TD Bank/Building Model/Enablement/Parameter Updates/20190401-td_submeters_type13.csv")
# write.csv(fan_systems_slim, "C:/Users/anthony.covarrubias/EnerNOC, Inc/BOP - Documents/Projects/TD Bank/Building Model/Enablement/Parameter Updates/20190401-td_fan_systems.csv")
# write.csv(error_submeter_systems[,6], "C:/Users/anthony.covarrubias/EnerNOC, Inc/BOP - Documents/Projects/TD Bank/Building Model/Enablement/Parameter Updates/20190411-td_error_submeters.csv")
# error_fan_systems <- as.data.frame(setdiff(fan_systems[,6], fans[,1]))
# colnames(error_fan_systems) <- "system_id"
# write.csv(error_fan_systems, "C:/Users/anthony.covarrubias/EnerNOC, Inc/BOP - Documents/Projects/TD Bank/Building Model/Enablement/Parameter Updates/20190411-td_error_fans.csv")
# 

# # Notes
# unit_id in building model corresponds to tstat_id from ENTOUCH API