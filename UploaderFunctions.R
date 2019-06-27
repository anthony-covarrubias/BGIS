GetCustomerID <- function(customer) {
  if (customer == "TD Bank") {
    customer_id <- "188867437" 
  } else if (customer == "CIBC") {
    customer_id <- "f3c67baf-1678-400b-854b-bdd9c3ba71af"
  }
  return(customer_id)
}


GetSystems <- function(customer, property_list){
  customer_id <- GetCustomerID(customer)
  
  # Pull systems from TITAN
  reqstring <- paste0("http://production-building-model-service.fo.enernoc.net:8080/building-model-service/api/v2/systems/search/findByCustomer?customerId=",customer_id)
  mycall <- GET(reqstring)
  full_response <- fromJSON(content(mycall, "text"), flatten=TRUE)
  json_response <- full_response[[1]]
  json_systems_full <- json_response$system
  
  # Utilize property list to subset data frame to desired columns
  pos_list <- c()
  for (i in 1:length(property_list)) {
    position <- grep(paste0("properties.",property_list[[i]]), colnames(json_systems_full))
        pos_list <- c(pos_list, assign(paste0("pos.",property_list[[i]]), as.numeric(position)))
  }
  #pos_list <- c(1,2,3,4,5, pos_list)
  pos_list <- c(5,2,3,4,1, pos_list)
  
  json_systems <- json_systems_full[,pos_list]
  json_systems <- mutate(json_systems, branch_code = substr(building_name,1,7))
  colnum <- ncol(json_systems)
  colnum_1 <- colnum-1
  json_systems <- json_systems[,c(colnum,1:colnum_1)]
  names(json_systems)[7:colnum] <- substring(names(json_systems)[7:colnum],12)
  
  return(json_systems)
}


FormatBldgSchedule <- function(entouch_bldgs, customer){
  schedule_df <- mutate(entouch_bldgs, branch_code = substr(Name,1,7))
  if (customer == "TD Bank") {
    schedule_df <- schedule_df %>% filter(!grepl("CM0", schedule_df$branch_code))
  } else if (customer == "CIBC") {
    schedule_df <- schedule_df %>% filter(grepl("CM0", schedule_df$branch_code))
  }
  
  schedule_df <- schedule_df[,c(26,3,2,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25)]
  schedule_df[is.na(schedule_df)] <- as.character("00:00")
  for(i in 5:ncol(schedule_df)) {
    schedule_df[,i] <- paste0(schedule_df[,i],":00")
    schedule_df[,i] <- chron::chron(times. =  schedule_df[,i], format = "hh:mm:ss")
  }
  colnames(schedule_df)[2:18] <- c("entouch_facility_name", "entouch_facility_id", "entouch_customer_id", "SSPara001", "SSPara002", "SSPara003", "SSPara004", "SSPara005", "SSPara006", "SSPara007", "SSPara008", "SSPara009", "SSPara010", "SSPara011", "SSPara012", "SSPara013", "SSPara014")
  return(schedule_df)
} 

FormatOffsets <- function(entouch_units, customer){
  
  offset_df <- entouch_units[,c(2:8)]
  offset_df <- mutate(offset_df,branch_code = substr(building_name,1,7))
  if (customer == "TD Bank") {
    offset_df <- offset_df %>% filter(!grepl("CM0", offset_df$branch_code))
  } else if (customer == "CIBC") {
    offset_df <- offset_df %>% filter(grepl("CM0", offset_df$branch_code))
  }
  offset_df <- offset_df[,c(8,3,2,1,4:7)]
  offset_df$offset_open <- abs(offset_df$offset_open)
  colnames(offset_df)[2:6] <- c("entouch_facility_name", "entouch_unit_name", "entouch_unit_id", "entouch_facility_id", "entouch_customer_id")
  
  # turn offsets (in minutes) to hh:mm:ss format
  offset_df$offset_open <- chron(times. = paste0(substr(times((offset_df$offset_open%/%60+offset_df$offset_open%%60/60)/24), 1, 5),":00"), format = "hh:mm:ss")
  offset_df$offset_close <- chron(times. = paste0(substr(times((offset_df$offset_close%/%60+offset_df$offset_close%%60/60)/24), 1, 5),":00"), format = "hh:mm:ss")

  return(offset_df)
}

FormatSetpoints <- function(entouch_units, customer){
  setpoints_df <- entouch_units[,c(2:6,9:12)]
  setpoints_df <- mutate(setpoints_df, branch_code = substr(building_name,1,7))
  if (customer == "TD Bank") {
    setpoints_df <- setpoints_df %>% filter(!grepl("CM0", setpoints_df$branch_code))
  } else if (customer == "CIBC") {
    setpoints_df <- setpoints_df %>% filter(grepl("CM0", setpoints_df$branch_code))
  }
  setpoints_df <- setpoints_df[,c(10,3,2,1,4:9)]
  colnames(setpoints_df)[2:6] <- c("entouch_facility_name", "entouch_unit_name", "entouch_unit_id", "entouch_facility_id", "entouch_customer_id")
  return(setpoints_df)
}

FormatMeters <- function(entouch_meters, customer){
  meters_df <- entouch_meters[,c(2:9)]
  meters_df <- mutate(meters_df, branch_code = substr(facility_name,1,7))
  if (customer == "TD Bank") {
    meters_df <- meters_df %>% filter(!grepl("CM0", meters_df$branch_code))
  } else if (customer == "CIBC") {
    meters_df <- meters_df %>% filter(grepl("CM0", meters_df$branch_code))
  }
  meters_df <- meters_df[,c(9,2,1,3:8)]
  meters_df$energy_link <- substring(meters_df$energy_link, regexpr("monitors", meters_df$energy_link)+9)
  #substring(string, regexpr(":", string) + 1)

  colnames(meters_df)[2:9] <- c("entouch_facility_name", "entouch_facility_id", "entouch_unit_id","entouch_panel_name", "entouch_branch_number", "entouch_branch_name", "entouch_meter_type", "entouch_branch")
  return(meters_df)
}

SlimFanSystems <- function(fan_systems){
  slim_list <- c("entouch_facility_id", "entouch_unit_id", "entouch_space_type")
  pos_list <- c()
  for (i in 1:length(slim_list)) {
    position <- grep(slim_list[[i]], colnames(fan_systems))
    pos_list <- c(pos_list, assign(paste0("pos.",slim_list[[i]]), as.numeric(position)))
  }
  pos_list <- c(1,2,3,6, pos_list)
  fan_systems_slim <- fan_systems[,pos_list]
  return(fan_systems_slim)
}

SlimSubmeters <- function(submeter_systems){
  slim_list <- c("entouch_meter_type", "entouch_branch", "entouch_facility_id", "entouch_unit_id")
  pos_list <- c()
  for (i in 1:length(slim_list)) {
    position <- grep(slim_list[[i]], colnames(submeter_systems))
    pos_list <- c(pos_list, assign(paste0("pos.",slim_list[[i]]), as.numeric(position)))
  }
  pos_list <- c(1,2,3,6,7, pos_list)
  submeter_systems_slim <- submeter_systems[,pos_list]
  return(submeter_systems_slim)
}


ApplyOffsets <- function(schedules, smart_recovery_time){
  pos.sunday_open <- grep("SSPara001", colnames(schedules))
  for (i in 1:nrow(schedules)) {
    # Open times
    pos.open_offset <- grep("offset_open", colnames(schedules))
    open_offset <- schedules[[i,pos.open_offset]]
    for (j in seq(pos.sunday_open,ncol(schedules),2)) {
      if (schedules[[i,j]] != "00:00:00") {
        # Extra 1 hour to account for Smart Recovery period
        smart_recovery <- chron::chron(times. = smart_recovery_time, format = "hh:mm:ss")
        schedules[[i,j]] <- schedules[[i,j]] - open_offset - smart_recovery
      }
    }
    # Close times
    pos.close_offset <- grep("offset_close", colnames(schedules))
    close_offset <- schedules[[i,pos.close_offset]]
    for (k in seq(pos.sunday_open+1,ncol(schedules),2)) {
      if (schedules[[i,k]] != "00:00:00") {
        schedules[[i,k]] <- schedules[[i,k]] + close_offset
      }
    }
  }
  return(schedules)
}




UploadFan <- function(fans) {
    for (m in 1:nrow(fans)) {
        system_id <- fans[m,1]
        reqstring <- paste0("http://production-building-model-service.fo.enernoc.net:8080/building-model-service/api/v2/systems/",system_id)
        json_payload <- gsub("\\[|\\]","",toJSON(list("properties" = fans[m,2:ncol(fans)]), pretty = TRUE))
        my_call <- PATCH(reqstring, body = json_payload)
        print(paste0("UploadFan-", m,"-",system_id))
    }
}

UploadSubmeter <- function(submeters) {
    for (n in 1:nrow(submeters)) {
        system_id <- submeters[n,1]
        reqstring <- paste0("http://production-building-model-service.fo.enernoc.net:8080/building-model-service/api/v2/systems/",system_id)
        json_payload <- gsub("\\[|\\]","",toJSON(list("properties" = submeters[n,2:ncol(submeters)]), pretty = TRUE))
        my_call <- PATCH(reqstring, body = json_payload)
        print(paste0("UploadSubmeter-", n,"-",system_id))
    }
}

UploadNames <- function(all_names) {
    for (p in 1:nrow(all_names)) {
        system_id <- all_names[p,1]
        new_name <- all_names[p,2]
        reqstring <- paste0("http://production-building-model-service.fo.enernoc.net:8080/building-model-service/api/v2/systems/",system_id)
        json_payload <- gsub("\\[|\\]","",toJSON(list("name" = new_name), pretty = TRUE))
        my_call <- PATCH(reqstring, body = json_payload)
        print(paste0("UploadNames-", p,"-",system_id))
    }
}

Type9Format <- function(submeter_schedules, Start1, Stop1, Start2, Stop2) {
    submeter_schedules_9 <- submeter_schedules[,c(1:11)] %>% filter(template == "9.Scheduled_No_Cycling")
    
    DOW <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    Start1_Time <- chron(times. = Start1, format = "hh:mm:ss")
    Stop1_Time <- chron(times. = Stop1, format = "hh:mm:ss")
    Start2_Time <- chron(times. = Start2, format = "hh:mm:ss")
    Stop2_Time <- chron(times. = Stop2, format = "hh:mm:ss")
    
    type9_list <- c()
    for (j in 1:length(DOW)) {
        assign(paste0(DOW[j], "_Start1_Time"), Start1_Time)
        assign(paste0(DOW[j], "_Stop1_Time"), Stop1_Time)
        assign(paste0(DOW[j], "_Start2_Time"), Start2_Time)
        assign(paste0(DOW[j], "_Stop2_Time"), Stop2_Time)
        type9_list <- c(type9_list, 
                        paste0(DOW[j], "_Start1_Time"), 
                        paste0(DOW[j], "_Stop1_Time"),
                        paste0(DOW[j], "_Start2_Time"),
                        paste0(DOW[j], "_Stop2_Time"))
    }
    #type9_df <- matrix(type9_list)
    
    submeter_9 <- as.data.frame(matrix(nrow = nrow(submeter_schedules_9), ncol = 29))
    colnames(submeter_9) <- c("system_id", type9_list)
    submeter_9$system_id <- submeter_schedules_9$system_id
    
    for (h in 1:length(type9_list)) {
        property <- type9_list[[h]]
        property_value <- eval(as.name(property))
        submeter_9[,which(colnames(submeter_9) == property)] <- as.character(property_value)
    }
    
    return(submeter_9)
}

UploadType9 <- function(submeter_schedules_final_9) {
    for (r in 1:nrow(submeter_schedules_final_9)) {
        system_id <- submeter_schedules_final_9[r,1]
        reqstring <- paste0("http://production-building-model-service.fo.enernoc.net:8080/building-model-service/api/v2/systems/",system_id)
        json_payload <- gsub("\\[|\\]","",toJSON(list("properties" = submeter_schedules_final_9[r,2:ncol(submeter_schedules_final_9)]), pretty = TRUE))
        my_call <- PATCH(reqstring, body = json_payload)
        print(paste0("UploadType9-", r,"-",system_id))
    }
    
}

BMUpload <- function(fans, all_names, submeter_cat_final, submeter_schedules_final, submeter_schedules_final_9) {
    upload_fan <- UploadFan(fans)
    upload_all_names <- UploadNames(all_names)
    upload_submeter_cat <- UploadSubmeter(submeter_cat_final)
    upload_submeter_schedules <- UploadSubmeter(submeter_schedules_final)
    upload_type9 <- UploadType9(submeter_schedules_final_9)
}

