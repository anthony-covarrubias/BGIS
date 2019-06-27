library("httr")
library("jsonlite")
library("dplyr")

# get_entouch_token<-function(api_key){
#   reqstring<-"https://api1.entouchgo.com/tokens"
#   json_payload<-paste('{"ApiKey": "',api_key,'",','"RememberMe" : true,','"DeviceName" : "enoc_cb"}',sep='')
#   mycall<-POST(reqstring, add_headers('Content-Type' = 'application/json'),body=json_payload)
#   full_response<-fromJSON(content(mycall, "text"), flatten=TRUE)
#   session_token<-full_response$SessionToken
#   return (session_token)
# }

get_customers<-function(session_token){
  reqstring<-"https://api.entouchgo.com/customers"
  mycall<-GET(reqstring, add_headers('API-Session-Token' = session_token))
  full_response<-fromJSON(content(mycall, "text"), flatten=TRUE)
  return_value<-full_response$`_embedded`$`resource:customers`
  return (return_value)
}

get_facilities_from_customerID<-function(session_token, CustomerID, page){
  reqstring<-paste("https://api.entouchgo.com/customers/",CustomerID,"/facilities?page=",page,sep="")
  mycall<-GET(reqstring, add_headers('API-Session-Token' = session_token))
  full_response<-fromJSON(content(mycall, "text"), flatten=TRUE)
  return_value<-full_response$`_embedded`$`resource:facilities`
  return (return_value)
}

get_facility_from_id<-function(session_token, CustomerID,FacilityId){
  reqstring<-paste("https://api.entouchgo.com/customers/",CustomerID,"/facilities/",FacilityId,sep="")
  mycall<-GET(reqstring, add_headers('API-Session-Token' = session_token))
  full_response<-fromJSON(content(mycall, "text"), flatten=TRUE)
  #return_value<-full_response$`_embedded`$`resource:facilities`
  return (full_response)
}

get_facility_hours_from_id<-function(session_token, CustomerID,FacilityId){
  reqstring<-paste("https://api.entouchgo.com/customers/",CustomerID,"/facilities/",FacilityId,sep="")
  mycall<-GET(reqstring, add_headers('API-Session-Token' = session_token))
  full_response<-fromJSON(content(mycall, "text"), flatten=TRUE)
  return_value<-full_response$BusinessHours
  return (return_value)
}

get_facility_dimmers_from_id<-function(session_token, CustomerID,FacilityId){
  reqstring<-paste("https://api.entouchgo.com/customers/",CustomerID,"/facilities/",FacilityId,"/dimmers",sep="")
  mycall<-GET(reqstring, add_headers('API-Session-Token' = session_token))
  full_response<-fromJSON(content(mycall, "text"), flatten=TRUE)
  #return_value<-full_response$BusinessHours
  return (full_response)
}

get_facility_lights_from_id<-function(session_token, CustomerID,FacilityId){
  reqstring<-paste("https://api.entouchgo.com/customers/",CustomerID,"/facilities/",FacilityId,"/lights",sep="")
  mycall<-GET(reqstring, add_headers('API-Session-Token' = session_token))
  full_response<-fromJSON(content(mycall, "text"), flatten=TRUE)
  #return_value<-full_response$BusinessHours
  return (full_response)
}

get_facility_remote_from_id<-function(session_token, CustomerID,FacilityId){
  reqstring<-paste("https://api.entouchgo.com/customers/",CustomerID,"/facilities/",FacilityId,"/remote-sensors",sep="")
  mycall<-GET(reqstring, add_headers('API-Session-Token' = session_token))
  full_response<-fromJSON(content(mycall, "text"), flatten=TRUE)
  #return_value<-full_response$BusinessHours
  return (full_response)
}

get_facility_energy_from_id<-function(session_token, CustomerID,FacilityId){
  reqstring<-paste("https://api.entouchgo.com/customers/",CustomerID,"/facilities/",FacilityId,"/energy-monitors",sep="")
  mycall<-GET(reqstring, add_headers('API-Session-Token' = session_token))
  full_response<-fromJSON(content(mycall, "text"), flatten=TRUE)
  #return_value<-full_response$BusinessHours
  return (full_response)
}

get_facility_energy_info<-function(session_token, CustomerID,FacilityId, monitor_id){
  reqstring<-paste("https://api.entouchgo.com/customers/",CustomerID,"/facilities/",FacilityId,"/energy-monitors/",monitor_id,sep="")
  mycall<-GET(reqstring, add_headers('API-Session-Token' = session_token))
  full_response<-fromJSON(content(mycall, "text"), flatten=TRUE)
  #return_value<-full_response$BusinessHours
  return (full_response)
}

get_facility_thermostat_from_id<-function(session_token, CustomerID,FacilityId){
  reqstring<-paste("https://api.entouchgo.com/customers/",CustomerID,"/facilities/",FacilityId,"/thermostats",sep="")
  mycall<-GET(reqstring, add_headers('API-Session-Token' = session_token))
  full_response<-fromJSON(content(mycall, "text"), flatten=TRUE)
  #return_value<-full_response$BusinessHours
  return (full_response)
}

get_facility_energy_info_by_link<-function(session_token, link){
  reqstring<-paste("https://api.entouchgo.com",link,sep="")
  mycall<-GET(reqstring, add_headers('API-Session-Token' = session_token))
  full_response<-fromJSON(content(mycall, "text"), flatten=TRUE)
  #return_value<-full_response$BusinessHours
  return (full_response)
}

get_facility_tstat_info_by_link<-function(session_token, link){
  reqstring<-paste("https://api.entouchgo.com",link,sep="")
  mycall<-GET(reqstring, add_headers('API-Session-Token' = session_token))
  full_response<-fromJSON(content(mycall, "text"), flatten=TRUE)
  #return_value<-full_response$BusinessHours
  return (full_response)
}
