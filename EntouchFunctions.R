library("dplyr")
library("httr")
library("jsonlite")


GetMeterInfo <- function(session_token) {
    facilities_1<-get_facilities_from_customerID(session_token,"4616","1")
    facilities_2<-get_facilities_from_customerID(session_token,"4616","2")
    facilities_3<-get_facilities_from_customerID(session_token,"4616","3")
    facilities_4<-get_facilities_from_customerID(session_token,"7039","1")
    
    facility_list <- rbind(facilities_1, 
                           facilities_2,
                           facilities_3, 
                           facilities_4)
    
    facility_list<-facility_list%>%
        mutate(customer_id=substr(facility_list$`_links.self`,12,15))
    
    facility_length<-nrow(facility_list)
    branch_list<-c("Main","Branch1","Branch2","Branch3","Branch4","Branch5","Branch6","Branch7","Branch8")
    
    df <- data.frame(facility_id=integer(),
                     facility_name=character(),
                     energy_link=character(),
                     energy_name=character(),
                     branch_number=character(),
                     branch_name=character(),
                     branch_category=character(),
                     stringsAsFactors=FALSE) 
    track<-1
    
    for (i in 1:facility_length){
        facility_id <- facility_list[[i,"Id"]]
        facility_name <- facility_list[[i,"Name"]]
        customer_id <- facility_list[[i,"customer_id"]]
        energy <- get_facility_energy_from_id(session_token,customer_id,facility_id)
        energy_list <- energy$`_embedded`$`resource:energy-monitors`
        energy_length <- nrow(energy_list)
        #print(energy_length)
        print(paste0("GetMeterInfo-", facility_name,"-",energy_length))
        if (!(is.null(energy_length))){
            for (k in 1:energy_length){
                energy_link<-energy_list[[k,"_links.self"]]
                energy_name<-energy_list[[k,"Name"]]
                energy_info<-get_facility_energy_info_by_link(session_token,energy_link)
                for (z in 1:9){
                    branch_number<-branch_list[z]
                    branch_name<-energy_info[[branch_number]]$Name
                    branch_category<-energy_info[[branch_number]]$Category
                    df[[track,"facility_id"]]<-facility_id
                    df[[track,"facility_name"]]<-facility_name
                    df[[track,"energy_link"]]<-energy_link
                    df[[track,"energy_name"]]<-energy_name
                    df[[track,"branch_number"]]<-branch_number
                    df[[track,"branch_name"]]<-branch_name
                    if (!(is.null(branch_category))){
                        df[[track,"branch_category"]]<-branch_category
                    }
                    track<-track+1
                }  
            }
        }
    }
    df<-df%>%
        mutate(branch_description=ifelse(branch_number=="Main", "MAIN",paste("B",substr(branch_number,nchar(branch_number),nchar(branch_number)),sep="")))
    write.csv(df, "BGIS/entouch_meter_info.csv")
    return(df)
}

GetBuildingInfo <- function(session_token) {
    facilities_1<-get_facilities_from_customerID(session_token,"4616","1")
    facilities_1<-facilities_1%>%
        mutate(customer_id="4616")
    
    facilities_2<-get_facilities_from_customerID(session_token,"4616","2")
    facilities_2<-facilities_2%>%
        mutate(customer_id="4616")
    
    facilities_3<-get_facilities_from_customerID(session_token,"4616","3")
    facilities_3<-facilities_3%>%
        mutate(customer_id="4616")
    
    facilities_4<-get_facilities_from_customerID(session_token,"7039","1")
    facilities_4<-facilities_4%>%
        mutate(customer_id="7039")
    
    facilities <- rbind(facilities_1, facilities_2,facilities_3, facilities_4)
    facilities <- facilities[,c("Id","Name", "customer_id")]
    facility_length <- nrow(facilities)
    facilities$SundayClosed<-NA
    facilities$SundayOn<-NA
    facilities$SundayOff<-NA
    facilities$MondayClosed<-NA
    facilities$MondayOn<-NA
    facilities$MondayOff<-NA
    facilities$TuesdayClosed<-NA
    facilities$TuesdayOn<-NA
    facilities$TuesdayOff<-NA
    facilities$WednesdayClosed<-NA
    facilities$WednesdayOn<-NA
    facilities$WednesdayOff<-NA
    facilities$ThursdayClosed<-NA
    facilities$ThursdayOn<-NA
    facilities$ThursdayOff<-NA
    facilities$FridayClosed<-NA
    facilities$FridayOn<-NA
    facilities$FridayOff<-NA
    facilities$SaturdayClosed<-NA
    facilities$SaturdayOn<-NA
    facilities$SaturdayOff<-NA
    
    for (i in 1:facility_length){
        fac_id<-facilities[[i,"Id"]]
        customer_id<-facilities[[i,"customer_id"]]
        facility_name <- facilities[[i, "Name"]]
        print(paste0("GetBuildingInfo-", facility_name))
        reqstring<-paste("https://api.entouchgo.com/customers/",customer_id,"/facilities/",fac_id, sep="")
        #reqstring<-paste("https://api.entouchgo.com/customers/7039/facilities/",fac_id, sep="")
        mycall<-GET(reqstring, add_headers('API-Session-Token' = session_token))
        full_response<-fromJSON(content(mycall, "text"), flatten=TRUE)
        hours<-full_response$BusinessHours
        for (k in 1:7){
            facilities[[i,3*(k-1)+4]]<-hours[[k]]$IsClosed
            if (facilities[[i,3*(k-1)+4]]==FALSE) {
                facilities[[i,3*(k-1)+5]]<-hours[[k]]$Open
                facilities[[i,3*(k-1)+6]]<-hours[[k]]$Close
            }
        }
    }
    write.csv(facilities, "BGIS/entouch_building_info.csv")
    return(facilities)
}

GetUnitInfo <- function(session_token) {
    facilities_1<-get_facilities_from_customerID(session_token,"4616","1")
    facilities_1<-facilities_1%>%
        mutate(customer_id="4616")
    
    facilities_2<-get_facilities_from_customerID(session_token,"4616","2")
    facilities_2<-facilities_2%>%
        mutate(customer_id="4616")
    
    facilities_3<-get_facilities_from_customerID(session_token,"4616","3")
    facilities_3<-facilities_3%>%
        mutate(customer_id="4616")
    
    facilities_4<-get_facilities_from_customerID(session_token,"7039","1")
    facilities_4<-facilities_4%>%
        mutate(customer_id="7039")
    
    facilities <- rbind(facilities_1, facilities_2,facilities_3, facilities_4)
    facility_length<-nrow(facilities)
    
    thermostats<-data.frame(tstat_id=character(),
                            tstat_name=character(), 
                            facility_id=character(), 
                            facility_name=character(),
                            customer_name=character(),
                            offset_open=integer(),
                            offset_close=integer(),
                            vacant_heat=integer(),
                            vacant_cool=integer(),
                            occupied_heat=integer(),
                            occupied_cool=integer(),
                            stringsAsFactors = FALSE) 
    
    for (i in 1:facility_length){
        building_id<-facilities[[i,"Id"]]
        building_name<-facilities[[i,"Name"]]
        customer_id<-facilities[[i,"customer_id"]]
        reqstring<-paste("https://api.entouchgo.com/customers/",customer_id,"/facilities/",building_id,"/thermostats", sep="")
        mycall<-GET(reqstring, add_headers('API-Session-Token' = session_token))
        full_response<-fromJSON(content(mycall, "text"), flatten=TRUE)
        tstat<-full_response$`_embedded`$`resource:thermostats`
        tstat_length<-nrow(tstat)
        print(paste0("GetUnitInfo-", building_name, "-", tstat_length))
        if (!is.null(tstat_length)){
            for (k in 1:tstat_length){
                reqstring<-paste("https://api.entouchgo.com/customers/",customer_id,"/facilities/",building_id,"/thermostats/",tstat[[k,"Id"]] ,"/hvac_schedule",sep="")
                mycall<-GET(reqstring, add_headers('API-Session-Token' = session_token))
                full_response<-fromJSON(content(mycall, "text"), flatten=TRUE)
                tstat_open_offset<-full_response$Advanced$OffsetMinutesOpen
                tstat_close_offset<-full_response$Advanced$OffsetMinutesClose
                vacant_heat<-full_response$Advanced$VacancySetPoints$Heat
                vacant_cool<-full_response$Advanced$VacancySetPoints$Cool
                occupied_heat<-full_response$Advanced$OccupancySetPoints$Heat
                occupied_cool<-full_response$Advanced$OccupancySetPoints$Cool
                thermostats <- rbind(thermostats, data.frame("tstat_id"=tstat[[k,"Id"]],"tstat_name"=tstat[[k,"Name"]],"building_name"=building_name,
                                                             "building_id"=building_id,"customer_id"=customer_id,
                                                             "offset_open"=tstat_open_offset,"offset_close"=tstat_close_offset,
                                                             "vacant_heat"=vacant_heat,"vacant_cool"=vacant_cool,
                                                             "occupied_heat"=occupied_heat,"occupied_cool"=occupied_cool))
                
            }
        }
        
        
    }
    write.csv(thermostats, "BGIS/entouch_unit_info.csv")
    return(thermostats)
}