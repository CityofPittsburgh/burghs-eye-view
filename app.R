# Burgh's Eye View Public
# Organization: City of Pittsburgh
# Dept: Innovation & Performance
# Team: Analytics & Strategy
# Author: Geoffrey Arnold

# Load required packages
library(shiny)
library(shinythemes)
library(xml2)
#"Dogfooding" Packages
library(httr)
library(jsonlite)
library(readr)
library(curl)
# Visuals Libraries
library(leaflet)
library(DT)
library(maptools)
library(htmltools)
# Data Transform
library(plyr)
library(zoo)
library(lubridate)

ckan_api <- jsonlite::fromJSON("key.json")$ckan_api

#Function to read backslashes correctly
chartr0 <- function(foo) chartr('\\','\\/',foo)

getWidth <- '$(document).on("shiny:connected", function(e) {
  var jsWidth = screen.width;
  Shiny.onInputChange("GetScreenWidth",jsWidth);
});'

#Make it work when Downloading stuff
httr::set_config(config(ssl_verifypeer = 0L))

dollarsComma <- function(x){
  x <- round(x, 2)
  x <- prettyNum(x, big.mark = ",")
  x <- paste0("$", x)
  return(x)
}

# Function to download WPRDC Data
ckan <- function(id) {
  x <- paste0("https://data.wprdc.org/datastore/dump/", id)
  r <- GET(x, add_headers(Authorization = ckan_api))
  content(r)
}

#Function to Query WPRDC Data on Time Frame
ckanQuery  <- function(id, days, column) {
  today <- as.character(format(Sys.Date(), "%m-%d-%Y"))
  query <- as.character(format(Sys.Date() - days, "%m-%d-%Y"))
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22", id, "%22%20WHERE%20%22", column,"%22%20%3E=%20%27", query, "%27%20AND%20%22", column, "%22%20%3C=%20%27", today,"%27")
  r <- GET(url, add_headers(Authorization = ckan_api))
  c <- content(r, "text")
  json <- gsub('NaN', '""', c, perl = TRUE)
  jsonlite::fromJSON(json)$result$records
}

cleanCouncil <- function(data) {
  data <- transform(data, COUNCIL_DISTRICT = as.factor(mapvalues(COUNCIL_DISTRICT, c(0:9),
                                                                   c(NA, "1: Harris", "2: Kail-Smith", "3: Kraus", "4: Rudiak", "5: O'Connor", "6: Lavelle", "7: Gross", "8: Gilman", "9: Burgess"))))
  return(data)
}

cleanDPW <-function(data) {
  data <- transform(data, PUBLIC_WORKS_DIVISION = as.factor(mapvalues(PUBLIC_WORKS_DIVISION, c(0:6), 
                                                                      c( NA, "1: North Side", "2: East End (North)", "3: The Hill, East End (South) & South Side", "4: South Side", "5: West End & South Hills", "6: Downtown, Strip & North Shore"))))
  return(data)
}

cleanZone <- function(data) {
  data <- transform(data, POLICE_ZONE = as.factor(mapvalues(POLICE_ZONE, c(append( c("OSC"), 0:6)), 
                                                            c(NA, NA, "1: North Side", "2: Downtown, Hill & Strip", "3: South Side", "4: East End (South) & South Side", "5: East End (North)", "6: West End & South Hills"))))
  return(data)
}
#Function to clean Geographies
cleanGeo <- function(data) {
  data <- cleanCouncil(data)
  data <- cleanDPW(data)
  data <- cleanZone(data)
  return(data)
}

#Load Boundary Files
#Neighborhoods
load.hoods <- readShapeSpatial("boundaries/Pittsburgh_Neighborhoods/Pittsburgh_Neighborhoods.shp")
#Council
load.council <- readShapeSpatial("boundaries/Pittsburgh_City_Council_Districts/Pittsburgh_City_Council_Districts.shp")
load.council$COUNCIL_DISTRICT <- load.council$council
load.council@data <- cleanCouncil(load.council@data)
#DPW
load.dpw <- readShapeSpatial("boundaries/Pittsburgh_DPW_Divisions/Pittsburgh_DPW_Divisions.shp")
load.dpw$PUBLIC_WORKS_DIVISION <- load.dpw$division
load.dpw@data <- cleanDPW(load.dpw@data)
#Zone
load.zones <- readShapeSpatial("boundaries/Pittsburgh_Police_Zones/Pittsburgh_Police_Zones.shp")
load.zones$POLICE_ZONE <- load.zones$zone
load.zones@data <- cleanZone(load.zones@data)

#Load Marker Files
#Load 311 Requests
load311 <- ckanQuery("40776043-ad00-40f5-9dc8-1fde865ff571", 365, "CREATED_ON")
load311$CREATED_ON <- as.POSIXct(load311$CREATED_ON, format = '%Y-%m-%dT%H:%M:%S')
#Clean Geographies
load311 <- subset(load311, select = -REQUEST_ID)
load311 <- cleanGeo(load311)
load311$date <- as.Date(load311$CREATED_ON)
load311$CREATED_ON <- as.POSIXct(load311$CREATED_ON, tz = "EST")
load311$icon <- as.character(load311$REQUEST_TYPE)
load311$REQUEST_TYPE <- ifelse(load311$REQUEST_TYPE == "Potholes - 4th Div", "Potholes", load311$REQUEST_TYPE)
#Prepare for Icons
requests311 <-c("Abandoned Vehicle (parked on street)", "Building Maintenance", "Building Without a Permit", "Drug Enforcement", "Fire Department", "Fire Lane", "Fire Prevention", "Gang Activity", "Graffiti, Documentation", "Graffiti, Removal", "Hydrant - Fire Admin", "Illegal Dumping", "Illegal Parking", "Litter","Noise", "Missed Pick Up", "Panhandling", "Patrol", "Paving Request", "Potholes", "Pruning (city tree)", "Refuse Violations", "Replace/Repair a Sign", "Request New Sign", "Rodent control", "Sidewalk Obstruction", "Sinkhole", "Smoke detectors", "Snow/Ice removal", "Street Cleaning/Sweeping", "Street Light - Repair", "Traffic", "Traffic or Pedestrian Signal, Repair", "Vacant Building", "Weeds/Debris")
#Set Icon to Other
load311$icon <- ifelse(load311$icon %in% requests311, load311$icon, "Other")
load311$icon <- as.factor(load311$icon)
load311$REQUEST_TYPE <- as.factor(load311$REQUEST_TYPE)
load311 <- transform(load311, icon = as.factor(mapvalues(icon, c("Abandoned Vehicle (parked on street)", "Building Maintenance", "Building Without a Permit", "Drug Enforcement", "Fire Department", "Fire Lane", "Fire Prevention", "Gang Activity", "Graffiti, Documentation", "Graffiti, Removal", "Hydrant - Fire Admin", "Illegal Dumping", "Illegal Parking", "Litter","Noise", "Other", "Missed Pick Up", "Panhandling", "Patrol", "Paving Request", "Potholes", "Pruning (city tree)", "Refuse Violations", "Replace/Repair a Sign", "Request New Sign", "Rodent control", "Sidewalk Obstruction", "Sinkhole", "Smoke detectors", "Snow/Ice removal", "Street Cleaning/Sweeping", "Street Light - Repair", "Traffic", "Traffic or Pedestrian Signal, Repair", "Vacant Building", "Weeds/Debris"),
                                                         c("abandoned_vehicle", "building_maintenance", "building_nopermit", "drug_enforcement", "fire_dept", "fire_lane", "fire_prevention",  "gang_activity", "graffiti", "graffiti", "hydrant", "illegal_dumping", "illegal_parking", "litter", "noise","other311", "missed_pickup","panhandling", "patrol", "paving_request", "pothole", "pruning", "refuse_violation", "replace_sign", "request_sign", "rodent_control", "sidewalk_obstruction", "sinkhole", "smoke_detectors", "snow_removal", "street_sweeper", "streetlight_repair", "traffic", "trafficlight_repair", "vacant_building", "weeds_debris"))))
#Origin Clean
load311 <- transform(load311, REQUEST_ORIGIN = as.factor(mapvalues(REQUEST_ORIGIN, c("Report2Gov Android", "Report2Gov iOS", "Report2Gov Website"),
                                                                   c("myBurgh (Android)", "myBurgh (iOS)", "Website"))))
load311 <- transform(load311, REQUEST_ORIGIN2 = as.factor(mapvalues(REQUEST_ORIGIN, c("myBurgh (Android)", "myBurgh (iOS)", "Website"),
                                                                    c('<a href="https://play.google.com/store/apps/details?id=com.qscend.report2gov.myburgh&hl=en" target="_blank">myBurgh (Android)</a>','<a href="https://itunes.apple.com/us/app/myburgh/id1021606996?mt=8" target="_blank">myBurgh (iOS)</a>', '<a href="http://pittsburghpa.gov/311/form" target="_blank">Website</a>'))))
load311$DEPARTMENT <- ifelse(is.na(load311$DEPARTMENT), "Other", load311$DEPARTMENT)
load311$DEPARTMENT <- as.factor(load311$DEPARTMENT)
load311$NEIGHBORHOOD <- as.factor(load311$NEIGHBORHOOD)

icons_311 <- iconList(
  abandoned_vehicle = makeIcon("./icons/311/abandoned_vehicle.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  building_maintenance = makeIcon("./icons/311/building_maintenance.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  building_nopermit = makeIcon("./icons/311/building_nopermit.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  drug_enforcement = makeIcon("./icons/311/drug_enforcement.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_dept = makeIcon("./icons/311/fire_dept.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_lane = makeIcon("./icons/311/fire_truck.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_prevention = makeIcon("./icons/311/fire_ex.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  gang_activity = makeIcon("./icons/311/gang_activity.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  graffiti = makeIcon("./icons/311/graffiti.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 18, popupAnchorY = -48),
  hydrant = makeIcon("./icons/311/fire_hydrant.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  illegal_dumping = makeIcon("./icons/311/illegal_dumping.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  illegal_parking = makeIcon("./icons/311/illegal_parking.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 18, popupAnchorY = -48),
  litter = makeIcon("./icons/311/litter.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 18, popupAnchorY = -48),
  missed_pickup = makeIcon("./icons/311/missed_pickup.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  noise = makeIcon("./icons/311/noise.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  other311 = makeIcon("./icons/311/other311.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  panhandling = makeIcon("./icons/311/panhandling.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  patrol = makeIcon("./icons/311/patrol.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  paving_request = makeIcon("./icons/311/paving_request.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  pothole = makeIcon("./icons/311/pothole.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  pruning = makeIcon("./icons/311/pruning.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  refuse_violation = makeIcon("./icons/311/refuse_violation.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  replace_sign = makeIcon("./icons/311/replace_sign.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  request_sign = makeIcon("./icons/311/request_sign.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  rodent_control = makeIcon("./icons/311/rodent_control.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  sidewalk_obstruction = makeIcon("./icons/311/sidewalk_obstruction.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  sinkhole = makeIcon("./icons/311/sinkhole.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  smoke_detectors = makeIcon("./icons/311/smoke_detector.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  snow_removal = makeIcon("./icons/311/snow_removal.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  street_sweeper = makeIcon("./icons/311/street_sweeper.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  streetlight_repair = makeIcon("./icons/311/streetlight_repair.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  traffic = makeIcon("./icons/311/traffic.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  trafficlight_repair = makeIcon("./icons/311/trafficlight_repair.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  vacant_building = makeIcon("./icons/311/vacant_building.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  weeds_debris = makeIcon("./icons/311/weeds_debris.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

# Load facilities
load.facilities <- ckan("9a5a52fd-fbe5-45b3-b6b2-c3bdaf6a2e04")
#Clean Geographies
load.facilities <- cleanGeo(load.facilities)
#Create Adress Column (checks to see if Address No. is valid, to add number and add space between street name)
load.facilities$address <- paste0(ifelse(is.na(load.facilities$AddressNumber), "", paste0(load.facilities$AddressNumber, " ")), ifelse(is.na(load.facilities$Street), "", load.facilities$Street))
#Clean NA's in Facility Type
load.facilities$FacilityType <- ifelse(load.facilities$ID == "Southside Park Third Base Dugout" | load.facilities$ID == "Josh Gibson  1 Third Base Dugout", "DUGOUT",load.facilities$FacilityType)
load.facilities$FacilityType <- ifelse(load.facilities$ID =="Herschel Upper  Building", "DUGOUT", load.facilities$FacilityType)
load.facilities$FacilityType <- ifelse(load.facilities$ID =="Martin Luther King Field Field House" | load.facilities$ID =="Frick Park Entrance Gate", "ACTIVITY", load.facilities$FacilityType)
load.facilities$FacilityType <- as.factor(load.facilities$FacilityType)
#Clean Facility Type for humans
load.facilities <- transform(load.facilities, usage = as.factor(mapvalues(FacilityType, c("ACTIVITY", "CABIN", "COMMUNITY", "CONCESSION", "DUGOUT", "FIREHOUSE" , "MEDIC STATION", "OFFICE", "POLICE", "POOL", "POOL CLOSED", "POOL/REC", "REC", "RECYCLING", "RESTROOMS", "SALT DOME", "SENIOR", "SERVICE", "SHELTER", "STORAGE", "TRAINING", "UTILITY", "VACANT", NA),
                                                                          c("Activity", "Cabin", "Community", "Concession", "Dugout", "Firehouse", "Medic Station", "Office", "Police", "Pool", "Pool - Closed", "Pool/Recreation", "Recreation", "Recycling", "Restrooms", "Salt Dome", "Senior Center", "Service", "Shelter", "Storage", "Training", "Utility", "Vacant", "STORAGE"))))
#Create Icon set
load.facilities <- transform(load.facilities, icon = as.factor(mapvalues(FacilityType, c("ACTIVITY", "CABIN", "COMMUNITY", "CONCESSION", "DUGOUT", "FIREHOUSE" , "MEDIC STATION", "OFFICE", "POLICE", "POOL", "POOL CLOSED", "POOL/REC", "REC", "RECYCLING", "RESTROOMS", "SALT DOME", "SENIOR", "SERVICE", "SHELTER", "STORAGE", "TRAINING", "UTILITY", "VACANT"),
                                                                         c("ACTIVITY", "CABIN", "COMMUNITY", "CONCESSION", "DUGOUT", "FIREHOUSE" , "MEDIC_STATION", "OFFICE", "POLICE", "POOL", "POOL_CLOSED", "POOL_REC", "REC", "RECYCLING", "RESTROOMS", "SALT_DOME", "SENIOR", "SERVICE", "SHELTER", "STORAGE", "TRAINING", "UTILITY", "VACANT"))))

load.facilities$url <- ifelse(load.facilities$usage %in% c("Shelter", "Community", "Senior Center", "Activity"), '<br><center><a href="https://registerparks.pittsburghpa.gov/" target="_blank">Rent this facility</a></center>', "")
load.facilities <- cleanGeo(load.facilities)

# Icons for Facilities
icons_facilities <- iconList(
  ACTIVITY = makeIcon("./icons/facilities/activity.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  CABIN = makeIcon("./icons/facilities/cabin.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  COMMUNITY = makeIcon("./icons/facilities/community.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  CONCESSION = makeIcon("./icons/facilities/concession.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  DUGOUT = makeIcon("./icons/facilities/dugout.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  FIREHOUSE = makeIcon("./icons/facilities/firehouse.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  MEDIC_STATION = makeIcon("./icons/facilities/medic_station.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  OFFICE = makeIcon("./icons/facilities/office.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  POLICE = makeIcon("./icons/facilities/police.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  POOL = makeIcon("./icons/facilities/pool.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  POOL_CLOSED = makeIcon("./icons/facilities/pool_closed.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  POOL_REC = makeIcon("./icons/facilities/pool_recreation.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  REC = makeIcon("./icons/facilities/recreation.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  RECYCLING = makeIcon("./icons/facilities/recycling.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  RESTROOMS = makeIcon("./icons/facilities/restrooms.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  SALT_DOME= makeIcon("./icons/facilities/salt_dome.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  SENIOR = makeIcon("./icons/facilities/healthy_living.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  SERVICE = makeIcon("./icons/facilities/service.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  SHELTER = makeIcon("./icons/facilities/shelter.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  STORAGE = makeIcon("./icons/facilities/storage.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  TRAINING = makeIcon("./icons/facilities/training.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  UTILITY = makeIcon("./icons/facilities/utility.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  VACANT = makeIcon("./icons/facilities/vacant.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

# Load Permit Layer
load.permits <- ckan("95d69895-e58d-44de-a370-fec6ad2b332e")
load.permits$date <- as.Date(load.permits$intake_date)
#Full address clean
load.permits$full_address <- paste0(ifelse(is.na(load.permits$street_address) | is.null(load.permits$street_address), "", paste0(as.character(load.permits$street_address), " ")),
                                    ifelse(is.na(load.permits$city) | is.null(load.permits$city), "",  paste0(load.permits$city, ", ")),
                                    ifelse(is.na(load.permits$state) | is.null(load.permits$state), "",  paste0(load.permits$state, " ")),
                                    ifelse(is.na(load.permits$zip) | is.null(load.permits$zip), "",  paste0(load.permits$zip, " ")))
types <- as.data.frame(do.call(rbind, strsplit(load.permits$permit_type, " - ", fixed = FALSE)))
load.permits$primary_type <- types$V1
load.permits$record_category <- ifelse(is.na(load.permits$record_category), "None", load.permits$record_category)
load.permits$current_status <- as.factor(load.permits$current_status)
load.permits$lat <- as.numeric(load.permits$lat)
load.permits$lon <- as.numeric(load.permits$lon)
load.permits$permit_type <- as.factor(load.permits$permit_type)
load.permits$neighborhood <- as.factor(load.permits$neighborhood)

#Create County Parcel viewer link
load.permits$url <-  paste0('<a href="http://www2.county.allegheny.pa.us/RealEstate/GeneralInfo.aspx?ParcelID=',load.permits$parcel_id, '" target="_blank">', load.permits$parcel_id, '</a>')

load.permits <- transform(load.permits, icon = as.factor(mapvalues(primary_type, c("Board of Appeals Application", "Building Permit", "Communication Tower", "Demolition Permit", "Electrical Permit", "Fire Alarm Permit", "HVAC Permit", "Land Operations Permit", "Occupancy Only", "Occupant Load Placard", "Sign Permit", "Sprinkler Permit", "Temporary Occupancy", "Temporary Occupancy Commercial"),
                                                                   c('appeals', 'building_permit', 'communication_tower', 'demolition_permit', 'electrical_permit', 'fire_alarm', 'HVAC_permit', 'land_operations', 'occupancy', 'occupant_load_placard', 'sign_permit', 'sprinkler_permit', 'temp_occupancy', 'temp_occupancy'))))
#Clean Geograhies
names(load.permits)[names(load.permits)=="police_zone"] <- "POLICE_ZONE"
names(load.permits)[names(load.permits)=="council_district"] <- "COUNCIL_DISTRICT"
names(load.permits)[names(load.permits)=="public_works_division"] <- "PUBLIC_WORKS_DIVISION"
load.permits <- cleanGeo(load.permits)

load.workflow <- ckan("7e0bf4bf-c7f5-48cd-8177-86f5ce776dfa")
load.workflow$tool <- paste0("<dt>", load.workflow$status_date, ": ", load.workflow$action_by_dept, "</dt>", "<dd>", load.workflow$task, " - ", load.workflow$status, "</dd>")

# Icons for Permit
icons_permits <- iconList(
  appeals = makeIcon("./icons/PLI/appeals.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  building_permit = makeIcon("./icons/PLI/building.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  communication_tower = makeIcon("./icons/PLI/comm_tower.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  demolition_permit = makeIcon("./icons/PLI/demolition.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  electrical_permit = makeIcon("./icons/PLI/electrical.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_alarm = makeIcon("./icons/PLI/fire_alarm.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  HVAC_permit = makeIcon("./icons/PLI/hvac.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  land_operations = makeIcon("./icons/PLI/land_operations.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  occupancy = makeIcon("./icons/PLI/occupancy.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  occupant_load_placard = makeIcon("./icons/PLI/occupant_load.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  sign_permit = makeIcon("./icons/PLI/sign.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  sprinkler_permit = makeIcon("./icons/PLI/sprinkler.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  temp_occupancy = makeIcon("./icons/PLI/temp_occupancy_commercial.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

# Load Violations
load.violations <- ckan("4e5374be-1a88-47f7-afee-6a79317019b4")
load.violations$date <- as.Date(load.violations$INSPECTION_DATE)
load.violations$INSPECTION_RESULT <- as.factor(load.violations$INSPECTION_RESULT)
load.violations <- transform(load.violations, icon = as.factor(mapvalues(INSPECTION_RESULT, c('Abated','Violations Found','Voided'),
                                                                         c('violations_abated', 'violations_found', 'violations_void'))))
load.violations$FullAddress <- paste(ifelse(is.na(load.violations$STREET_NUM) | is.null(load.violations$STREET_NUM) | load.violations$STREET_NUM == 0, "", load.violations$STREET_NUM) , ifelse(is.na(load.violations$STREET_NAME) | is.null(load.violations$STREET_NAME), "", load.violations$STREET_NAME))
#Create Parcel URL
load.violations$full_address <- paste(load.violations$STREET_NUM, load.violations$STREET_NAME)
load.violations$url <-  paste0('<a href="http://www2.county.allegheny.pa.us/RealEstate/GeneralInfo.aspx?ParcelID=',load.violations$PARCEL, '" target="_blank">', load.violations$PARCEL, '</a>')
#Prepare 
violations1 <- ncol(load.violations) + 1
list <- as.data.frame(do.call(rbind, strsplit(load.violations$VIOLATION, ":: ", fixed = FALSE)))
load.violations <- cbind(load.violations, list)
violationsCol <- ncol(load.violations)
load.violations$VIOLATION <- as.character(load.violations$VIOLATION)
load.violations$VIOLATION <- gsub("::", "/", load.violations$VIOLATION)
load.violations$CORRECTIVE_ACTION <- gsub("::", "/", load.violations$CORRECTIVE_ACTION)

#Clean Geographies
load.violations <- cleanGeo(load.violations)

icons_violations <- iconList(
  violations_abated = makeIcon("./icons/PLI/violations_abated.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  violations_found = makeIcon("./icons/PLI/violations_found.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  violations_void = makeIcon("./icons/PLI/violations_void.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

# Blotter
archive <- ckanQuery("044f2016-1dfd-4ab0-bc1e-065da05fca2e", 365, "INCIDENTTIME")
# Clean for merge
archive$X <- as.numeric(archive$X)
archive$Y <- as.numeric(archive$Y)
archive$INCIDENTTIME <- as.POSIXct(archive$INCIDENTTIME, format = '%Y-%m-%dT%H:%M:%S')
# Load Thirty Day Blotter
thirty <- ckan("1797ead8-8262-41cc-9099-cbc8a161924b")
# Clean for Merge
thirty <- thirty[,1:ncol(thirty)]
archive <- archive[,c(colnames(thirty))]
# Merge
load.blotter <- rbind(archive, thirty)
# Prepare for Mapping
load.blotter$date <- as.Date(load.blotter$INCIDENTTIME)
# Reform Hierarchy
load.blotter$HIERARCHY_Num <- load.blotter$HIERARCHY
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 1, "01 Murder", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 2, "02 Rape", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 3, "03 Robbery", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 4, "04 Assault", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 5, "05 Burglary", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 6, "06 Theft", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 7, "07 Vehicle Theft", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 8, "08 Arson", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 9, "09 Forgery", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 10, "10 Simple Assault", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 11, "11 Fraud", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 12, "12 Embezzlement", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 13, "13 Receiving Stolen Prop", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 14, "14 Vandalism", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 15, "15 Carrying Weapon", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 16, "16 Prostitution", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 17, "17 Sex Offense", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 18, "18 Drug Offense", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 19, "19 Gambling", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 20, "20 Endangering Children", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 21, "21 DUI", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 22, "22 Liquor Laws", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 23, "23 Public Drunkenness", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 24, "24 Disorderly Conduct", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 25, "25 Vagrancy", load.blotter$HIERARCHY)
load.blotter$HIERARCHY <- ifelse(load.blotter$HIERARCHY_Num == 0 | load.blotter$HIERARCHY_Num > 25,	"26 Other", load.blotter$HIERARCHY)
# Transform to Factors for icons
load.blotter$HIERARCHY <- as.factor(load.blotter$HIERARCHY)
load.blotter$HIERARCHY_Num <-as.numeric(load.blotter$HIERARCHY_Num)
#Unify Neighborhoods
load.blotter <- transform(load.blotter, INCIDENTNEIGHBORHOOD = as.factor(mapvalues(INCIDENTNEIGHBORHOOD, c("Golden Triangle/Civic Arena", "Central Northside", "Mt. Oliver Neighborhood", "Troy Hill-Herrs Island"),
                                                                                   c("Central Business District", "Central North Side", "Mount Oliver", "Troy Hill"))))

load.blotter <- transform(load.blotter, icon = as.factor(mapvalues(HIERARCHY, c("08 Arson", "04 Assault", "05 Burglary", "15 Carrying Weapon", "24 Disorderly Conduct", "18 Drug Offense", "21 DUI", "20 Endangering Children", "12 Embezzlement", "09 Forgery", "11 Fraud", "19 Gambling", "22 Liquor Laws", "01 Murder", "26 Other", "16 Prostitution", "23 Public Drunkenness", "02 Rape", "13 Receiving Stolen Prop", "03 Robbery", "17 Sex Offense", "10 Simple Assault", "06 Theft", "14 Vandalism", "14 Vagrancy", "07 Vehicle Theft"), 
                                                                   c("arson", "assault",  "burglary", "carrying_weapon", "disorderly_conduct", "drug_offense", "DUI", "endangering_children", "embezzlement", "forgery", "fraud", "gambling", "liquor_laws", "murder", "other", "prostitution", "public_drunkenness", "rape", "receiving_stolen_property", "robbery", "sex_offense", "simple_assault", "theft", "vandalism", "vagrancy", "vehicle_theft"))))
#Clean Geographies
load.blotter$HIERARCHY <- as.factor(load.blotter$HIERARCHY)
names(load.blotter)[names(load.blotter)=="INCIDENTZONE"] <- "POLICE_ZONE"
load.blotter <- cleanGeo(load.blotter)
#Clean Flag
load.blotter$CLEAREDFLAG <- ifelse(load.blotter$CLEAREDFLAG == "Y", "Yes", "No")

#Offenses Columns
incidents <- as.data.frame(do.call(rbind, strsplit(load.blotter$OFFENSES, " / ", fixed = FALSE)))
load.blotter <- cbind(load.blotter, incidents)
offensesCol <- as.numeric(ncol(load.blotter))
offenses1 <- as.numeric(which(colnames(load.blotter)=="V1"))

# Icons for Blotter
icons_blotter <- iconList(
  murder = makeIcon("./icons/police/murder.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  rape = makeIcon("./icons/police/rape.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  robbery = makeIcon("./icons/police/robbery.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  assault = makeIcon("./icons/police/assault.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  burglary = makeIcon("./icons/police/burglary.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48), 
  theft = makeIcon("./icons/police/theft.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  vehicle_theft = makeIcon("./icons/police/vehicle_theft.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  arson = makeIcon("./icons/police/arson.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  forgery = makeIcon("./icons/police/forgery.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  simple_assault = makeIcon("./icons/police/simple_assault.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fraud = makeIcon("./icons/police/fraud.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  embezzlement = makeIcon("./icons/police/embezzlement.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  receiving_stolen_property = makeIcon("./icons/police/receiving_stolen_property.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  vandalism = makeIcon("./icons/police/vandalism.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  carrying_weapon = makeIcon("icons/police/carrying_weapon.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  prostitution = makeIcon("./icons/police/prostitution.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  sex_offense = makeIcon("./icons/police/sex_offense.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  drug_offense = makeIcon("./icons/police/drug_offense.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  gambling = makeIcon("./icons/police/gambling.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  endangering_children = makeIcon("./icons/police/endangering_children.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  DUI = makeIcon("./icons/police/DUI.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  liquor_laws = makeIcon("./icons/police/liquor_laws.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  public_drunkenness = makeIcon("./icons/police/public_drunkeness.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  disorderly_conduct= makeIcon("icons/police/disorderly_conduct.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  vagrancy = makeIcon("./icons/police/vagrancy.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  other = makeIcon("./icons/police/other.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

#Capital Projects
load.cproj <- read.csv("cgCapitalProjects.csv")
load.cproj$year <- as.numeric(format(as.Date(load.cproj$StopDateField, format = "%m/%d/%Y"), "%Y"))
load.cproj$TotalCostActualField <- dollarsComma(load.cproj$TotalCostActualField)
load.cproj$COUNCIL_DISTRICT <- ""
load.cproj$POLICE_ZONE <- ""
load.cproj$NEIGHBORHOOD <- ""
load.cproj$PUBLIC_WORKS_DIVISION <- ""

load.cproj <- cleanGeo(load.cproj)

icons_cproj <- iconList(
  administration = makeIcon("./icons/omb/administration.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  engineering_construction = makeIcon("./icons/omb/engineering_construction.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  facility_improvement = makeIcon("./icons/omb/facility_improvement.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  neighborhood_development = makeIcon("./icons/omb/community_development.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  public_safety = makeIcon("./icons/omb/public_safety.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  vehicles_equipment = makeIcon("./icons/omb/vehicles_equipment.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

#this_year
this_year <- format(Sys.Date(), format="%Y")

if(Sys.Date() <= as.Date(paste0(this_year,"-10-31")) & Sys.Date() >= as.Date(paste0(this_year,"-10-01"))) {
  # Egg
  X <- c(-79.9573738, -79.9796721, -79.9892566, -79.9814719, -79.9517155, -79.9128181, -79.9272001, -79.983961, -79.9948964, -79.9933058, -80.0217265, -80.0215099, -79.9851465)
  Y <- c(40.4611634, 40.4671619, 40.4667157, 40.472155, 40.4684005, 40.4401088, 40.4161835, 40.4186422, 40.4066441, 40.4012173, 40.4737751, 40.4636383, 40.4289496)
  title <- c("Allegheny", "Voegtly", "Ridgelawn", "St. Pauls", "St. Mary", "Smithfield East", "Calvary Catholic", "St Michaels", "St John Vianney", "South Side", "Highwood", "Union Dale", "Prince of Peace")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "halloween"
  load.egg$tt <- "Yarr! There be nuttin' to be found with that search term matey."
} else if (Sys.Date() <= as.Date(paste0(this_year,"-11-08")) & Sys.Date() >= as.Date(paste0(this_year,"-11-01"))) {
  load.egg <- ckan("e17e6a67-2bba-4a1a-aa36-87beb2cd0a3b")
  load.egg <- subset(load.egg, MuniName == "PITTSBURGH")
  load.egg$icon <- "election"
  load.egg$tt <- paste0("<font color='black'>No matter who you Vote for, make sure you Vote!
                        <br><b>Location: </b>", load.egg$LocName,
                        "<br><b>Ward: </b>", load.egg$Ward,
                        "<br><b>District: </b>", load.egg$District,
                        "<br><b>Address: </b>", load.egg$NewAddress,
                        '<br><center><a href="https://www.pavoterservices.state.pa.us/pages/pollingplaceinfo.aspx" target="_blank">Find your polling place!</a></center>
                        Clear the search bar to go back to the regular Burgh&#39;s Eye View!</font>'
  )
} else if (Sys.Date() <= as.Date(paste0(this_year,"-11-30")) & Sys.Date() >= as.Date(paste0(this_year,"-11-09"))) {
  X <- c(-79.9773187, -80.0096757, -80.0109521)
  Y <- c(40.4644031, 40.4406418, 40.4416163)
  title <- c("Herr's Island", "Fort Pitt", "Fort Duquesne")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "thanksgiving"
  load.egg$tt <- "*Gobble gobble* <br> No Results this time. Search again and have a Happy Thanksgiving!"
} else if (Sys.Date() >= as.Date(paste0(this_year,"-12-30")) | Sys.Date() <= as.Date(paste0(this_year,"-1-02"))) {
  X <- c(-80.00383, -80.003981)
  Y <- c(40.441558, 40.442340)
  title <- c("Liberty & Stanwix", "Penn & Stanwix")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "new_year"
  load.egg$tt <- "3... 2... 1... Happy New Years! <br>Looks like a fresh start to the New Year, and a fresh blank map! Try something else in the search bar!"
} else if (Sys.Date() >= as.Date(paste0(this_year,"-02-01")) & Sys.Date() <= as.Date(paste0(this_year,"-02-15"))) {
  X <-  c(-80.002398,  -80.017794, -79.964644, -79.964708, -79.983140, -79.991428)
  Y <- c(40.440397, 40.437650, 40.428210, 40.461866, 40.452217, 40.456897)
  title <- c("Market Square", "Mt. Washington", "SouthSide Works", " Church Brew Works", "The Strip", "Penn Brewery")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "valentine"
  load.egg$tt <- "Love is in the air, but doesn't look like any results are! <br>Would you be my Valentine?"
} else {
  X <- c(-79.9968604, -80.004055)
  Y <- c(40.4381098, 40.440631)
  title <- c("City County Building", "Market Square")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "snow"
  load.egg$tt <- "Burrr!! The app's not frozen, there's just nothing that fits that description here!"
}

icons_egg <- iconList(
  halloween = makeIcon("./icons/egg/pirate.png", iconAnchorX = 9, iconAnchorY = 12.5, popupAnchorX = 0, popupAnchorY = -12.5),
  election = makeIcon("./icons/egg/vote.png", iconAnchorX = 9, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13),
  thanksgiving = makeIcon("./icons/egg/thanksgiving.png", iconAnchorX = 9, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13),
  snow = makeIcon("./icons/egg/snowboard.png", iconAnchorX = 9, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13),
  new_year = makeIcon("./icons/egg/new_year.png", iconAnchorX = 9, iconAnchorY = 13.5, popupAnchorX = 0, popupAnchorY = -13.5),
  valentine = makeIcon("./icons/egg/valentine.png", iconAnchorX = 40, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5)
)

# Non-Traffic Citations
load.citations <- ckan("6b11e87d-1216-463d-bbd3-37460e539d86")
load.citations$date <- as.Date(load.citations$CITEDTIME)
load.citations$icon <- "citation"
#Unify Neighborhoods
load.citations <- transform(load.citations, NEIGHBORHOOD = as.factor(mapvalues(NEIGHBORHOOD, c("Golden Triangle/Civic Arena", "Central Northside", "Mt. Oliver Neighborhood", "Troy Hill-Herrs Island"),
                                                                               c("Central Business District", "Central North Side", "Mount Oliver", "Troy Hill"))))
#Clean Geographies
names(load.citations)[names(load.citations)=="ZONE"] <- "POLICE_ZONE"
load.citations <- cleanGeo(load.citations)

#Offenses Columns
incidents2 <- as.data.frame(do.call(rbind, strsplit(load.citations$OFFENSES, " / ", fixed = FALSE)))
load.citations <- cbind(load.citations, incidents2)
offensesColCit <- as.numeric(ncol(load.citations))
offensesCit1 <- as.numeric(which(colnames(load.citations)=="V1"))

icons_citations <- iconList(
  citation = makeIcon("./icons/police/nontraffic_citation.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

# Arrests
load.arrests <- ckan("e03a89dd-134a-4ee8-a2bd-62c40aeebc6f")
load.arrests$icon <- "arrest_made"
load.arrests$date <- as.Date(load.arrests$ARRESTTIME)
#Unify Neighborhoods
load.arrests <- transform(load.arrests, INCIDENTNEIGHBORHOOD = as.factor(mapvalues(INCIDENTNEIGHBORHOOD, c("Golden Triangle/Civic Arena", "Central Northside", "Mt. Oliver Neighborhood", "Troy Hill-Herrs Island"),
                                                                                   c("Central Business District", "Central North Side", "Mount Oliver", "Troy Hill"))))
#Clean Geographies
names(load.arrests)[names(load.arrests)=="INCIDENTZONE"] <- "POLICE_ZONE"
load.arrests <- cleanGeo(load.arrests)

#Offenses Columns
incidents2 <- as.data.frame(do.call(rbind, strsplit(load.arrests$OFFENSES, " / ", fixed = FALSE)))
load.arrests <- cbind(load.arrests, incidents2)
offensesColAr <- as.numeric(ncol(load.arrests))
offensesAr1 <- as.numeric(which(colnames(load.arrests)=="V1"))

icons_arrests <- iconList(
  arrest_made = makeIcon("./icons/police/arrest_made.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

# UI for application
ui <- navbarPage(windowTitle = "Burgh's Eye View", 
                 collapsible = TRUE,
                 fluid = TRUE,
                 theme = shinytheme("flatly"),
                 title = HTML('<img src="burghs_eyeview_logo_small.png" alt="Burghs Eye View" height="85%">'),
                 position = "static-top",
                 tabPanel('Map', class = "Map",
                          #Run script to determine if user is loading from a mobile device
                          tags$script(getWidth),
                          #Google Tag Manager Script to Head
                          tags$head(includeScript("tag-manager-head.js")),
                          #Set favicon
                          tags$head(tags$link(rel = "icon", type = "image/png", href="favicon.png")),
                          tags$head(HTML('<link rel="apple-touch-icon-precomposed" href="apple-touch-icon-precomposed.png" />
                                         <link rel="apple-touch-icon" sizes="76x76" href="apple-icon-76x76-precomposed.png" />
                                         <link rel="apple-touch-icon" sizes="114x114" href="apple-icon-120x120-precomposed.png" />
                                         <link rel="apple-touch-icon" sizes="152x152" href="apple-icon-152x152-precomposed.png" />')),
                          tags$head(HTML('<!-- You can use Open Graph tags to customize link previews.
                                         Learn more: https://developers.facebook.com/docs/sharing/webmasters -->
                                         <meta property="og:url"           content="http://www.your-domain.com/your-page.html" />
                                         <meta property="og:type"          content="website" />
                                         <meta property="og:title"         content="Burgh&#39;s Eye View" />
                                         <meta property="og:description"   content="Pittsburgh&#39;s one stop shop for geographic City Data" />
                                         <meta property="og:image"         content="http://apps.pittsburghpa.gov/cis/burgh-seye-icon.png" />')),
                          #Add Google Analytics Script to page
                          tags$head(includeScript("google-analytics.js")),
                          #Add Tag Manager Script to Body
                          tags$body(tags$noscript(tags$iframe(src='https://www.googletagmanager.com/ns.html?id=GTM-TCTCQVD', height = 0, width = 0, style="display:none;visibility:hidden"))),
                          #Hide error codes that may appear
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"),
                          #Add background image
                          tags$head(tags$style(type="text/css", '.Map {
                                               background-image: url("loading.png");
                                               background-repeat: no-repeat;
                                               background-position: center;
                                               background-size: contain;
                                               }')),
                          #Background of report.table
                          tags$style(type="text/css", '.report.table {background-color: #fff;}'),
                          #Remove unwanted padding and margins
                          tags$style(type="text/css", ".container-fluid {padding:0;}"),
                          tags$style(type="text/css", ".navbar-header {margin:auto;"),
                          tags$style(type="text/css", ".navbar-static-top {margin-bottom:0;}"),
                          tags$style(type="text/css", ".navbar-brand {height:60px; padding:0;}"),
                          tags$style(type="text/css", ".navbar {border-right-width: 20px;
                                                                border-left-width: 65px;}"),
                          #Set max height for pop-ups
                          tags$style(type="text/css", ".leaflet-popup-content {overflow-y: auto; max-height: 400px !important;}"),
                          #Edit top bar
                          tags$style(type= "text/css", ".form-group {
                                     margin-bottom: 0px;
                                     }"),
                          #Generate search & layer panel & Map (checks for mobile devices)
                          uiOutput("mapPanel")
                          ),
                 tabPanel('Data', class = "Data",
                          #Select Dataset for Export
                          inputPanel(
                            selectInput("report_select", 
                                        tagList(shiny::icon("map-marker"), "Select Layer:"),
                                        choices = c("311 Requests", "Arrests", "Blotter", "Building Permits", "Capital Projects", "City Facilities", "Code Violations", "Non-Traffic Citations"), #
                                        selected= "311 Requests"),
                            #Define Button Position
                            uiOutput("buttonStyle")
                          ),
                          #Clean up the Data Table CSS
                          tags$style(type = "text/css", ".dataTables_length {margin-left: 10px;}"),
                          tags$style(type = "text/css", ".dataTables_info {margin-left: 10px;}"),
                          tags$style(type = "text/css", ".dataTables_filter {margin-right: 5px;}"),
                          dataTableOutput("report.table")
                 ),
                 tabPanel('About', class = "About",
                          includeHTML('about.html'),
                          #Twitter Button
                          tags$script(HTML("var header = $('.navbar > .container-fluid > .navbar-collapse');
                                           header.append('<div class =\"twit\" style=\"float:right;margin-top: 15px;\"><a href=\"https://twitter.com/share\" class=\"twitter-share-button\" align=\"middle\" data-url=\"data.pittsburghpa.gov/BurghsEyeView\" data-text=\"Check out Burgh&#39;s Eye View! A new tool to view city data in Pittsburgh: https://goo.gl/C4ySqW\" data-size=\"large\">Tweet</a></div>');
                                           console.log(header)")),
                          tags$script(HTML("!function(d,s,id){
                                           var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';
                                           if(!d.getElementById(id)){
                                           js=d.createElement(s);
                                           js.id=id;
                                           js.src=p+'://platform.twitter.com/widgets.js';
                                           fjs.parentNode.insertBefore(js,fjs);
                                           }
                          }(document, 'script', 'twitter-wjs');")),
                #Facebook Button
                HTML('<div id="fb-root"></div>'),
                tags$script(HTML("(function(d, s, id) {
                                 var js, fjs = d.getElementsByTagName(s)[0];
                                 if (d.getElementById(id)) return;
                                 js = d.createElement(s); js.id = id;
                                 js.src = \"//connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.8\";
                                 fjs.parentNode.insertBefore(js, fjs);
                          }(document, 'script', 'facebook-jssdk'));")),
                tags$script(HTML('header.append(\'<div class="fb-share-button" style="float:right;margin-top: 15px;margin-right: 5px;" data-href="http://data.pittsburghpa.gov/BurghsEyeView/#utm_source=facebook_button&amp;utm_campaign=facebook_button&amp;utm_medium=facebook%2Fsocial\" data-layout="button" data-size="large" data-mobile-iframe="true"><a class="fb-xfbml-parse-ignore" target="_blank" href="https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Fdata.pittsburghpa.gov%2FBurghsEyeView%2F%23utm_source%3Dfacebook_button%26utm_campaign%3Dfacebook_button%26utm_medium%3Dfacebook%252Fsocial&amp;src=sdkpreparse">Share</a></div>\');
                                 console.log(header)'))
                )
                 )

# Define server
server <- shinyServer(function(input, output, session) {
  #Observe changes to the dates function, if not default include in bookmark/url
  observeEvent(input$dates,  {
    if (input$dates[1] != Sys.Date()-10 | input$dates[2] != Sys.Date()){
      setBookmarkExclude("GetScreenWidth")
    } else {
      setBookmarkExclude(c("GetScreenWidth", "dates"))
    }
  })
  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  #Update page URL
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  output$buttonStyle <- renderUI({
    #Generate search & layer panel & Map (checks for mobile devices)
    if (as.numeric(input$GetScreenWidth) > 800) {
      div(style="margin-top: 20px", downloadButton("downloadData", paste("Export" , input$report_select), class = "dlBut"))
    } else {
      div(downloadButton("downloadData", paste("Export" , input$report_select), class = "dlBut"))
    }
  })
  #Map Tab UI
  output$mapPanel <- renderUI({
    #UI for Desktop Users
    if (as.numeric(input$GetScreenWidth) > 800) {
      tagList(
        #Generate Map
        leafletOutput("map"),
        #Map size for Desktop CSS
        tags$style(type = "text/css", "#map {height: calc(100vh - 60px) !important;}"),
        absolutePanel(
          #Input panel for Desktops (alpha'd)
          top = 70, left = 50, width = '300px',
          wellPanel(id = "tPanel", style = "overflow-y:auto; max-height: calc(100vh - 85px) !important;",
                    textInput("search",
                              value = ifelse(Sys.Date() == as.Date(paste0(this_year,"-11-08")), "Election Day!", ""),
                              label = NULL, 
                              placeholder = "Search"),
                    HTML('<small style="font-size:11px;margin-left:3px">Locations are not exact. (See &rsquo;About&rsquo; for details.)</small><br><br>'),
                    dateRangeInput("dates",
                                   label = NULL,
                                   start = Sys.Date()-10,
                                   end = Sys.Date(),
                                   startview = "day"),
                    HTML('<font color="#F47B25">'),
                    checkboxInput("toggle311",
                                  label = "311 Requests",
                                  value = TRUE),
                    HTML('</font>'),
                    uiOutput("request_UI"),
                    selectInput("dept_select",
                                label = NULL,
                                c(`Department`='', levels(load311$DEPARTMENT)),
                                multiple = TRUE,
                                selectize=TRUE),
                    selectInput("origin_select",
                                label = NULL,
                                c(`Request Origin`='', levels(load311$REQUEST_ORIGIN)),
                                multiple = TRUE,
                                selectize=TRUE),
                    HTML('<font color="#3663AD">'),
                    checkboxInput("toggleBlotter",
                                  label = "Police Blotter",
                                  value= TRUE),
                    HTML('</font>'),
                    selectInput("hier",
                                label = NULL,
                                c(`Hierarchy`='', levels(load.blotter$HIERARCHY)),
                                multiple = TRUE,
                                selectize = TRUE),
                    uiOutput("offense_UI"),
                    HTML('<font color="#A91622">'),
                    checkboxInput("toggleArrests",
                                  label = "Arrests",
                                  value = TRUE),
                    HTML('</font>'),
                    HTML('<font color="#ED2393">'),
                    checkboxInput("toggleCitations",
                                  label = "Non-Traffic Citations",
                                  value = TRUE),
                    HTML('</font>'),
                    HTML('<font color="#009FE1">'),
                    checkboxInput("togglePermits",
                                  label = "Building Permits",
                                  value = TRUE),
                    HTML('</font>'),
                    selectInput("permit_select",
                                label = NULL,
                                c(`Permit Type`='', levels(load.permits$permit_type)),
                                multiple = TRUE,
                                selectize=TRUE),
                    selectInput("status_select",
                                label = NULL,
                                c(`Permit Status`='', levels(load.permits$current_status)),
                                multiple = TRUE,
                                selectize=TRUE),
                    HTML('<font color="#0B9444">'),
                    checkboxInput("toggleViolations",
                                  label = "Code Violations", 
                                  value = TRUE),
                    HTML('</font>'),
                    uiOutput("violations_UI"),
                    selectInput("result_select",
                                label = NULL,
                                c(`Inspection Result`='', levels(load.violations$INSPECTION_RESULT)),
                                multiple = TRUE,
                                selectize=TRUE),
                    HTML('<font color="#b9a5c1">'),
                    checkboxInput("toggleCproj",
                                  label = "Capital Projects",
                                  value = TRUE),
                    HTML('</font>'),
                    selectInput("funcarea_select",
                                label = NULL,
                                c(`Functional Area`='', levels(load.cproj$CapitalProjectFunctionalAreaField)),
                                multiple = TRUE,
                                selectize=TRUE),
                    HTML('<font color="#474545">'),
                    checkboxInput("toggleFacilities",
                                  label = "City Facilities",
                                  value = TRUE),
                    HTML('</font>'),
                    selectInput("usage_select",
                                label = NULL,
                                c(`Facility Usage`='', levels(load.facilities$usage)),
                                multiple = TRUE,
                                selectize=TRUE),
                    selectInput("filter_select",
                                "Filter by Area",
                                c(`Area Type`='', c("Neighborhood", "Council District", "Police Zone", "Public Works Division")),
                                selectize = TRUE,
                                selected = ""),
                    uiOutput("filter_UI")
          ), style = "opacity: 0.88"
        )
      )
    } else {
      tagList(
        #Input panel for Mobile (stationary at top)
        sidebarPanel(id = "tPanel",
                     #Remove padding from Search Bar
                     tags$style(type= "text/css", "#tPanel {margin-bottom:0px; padding:0px;}"),
                     #Set background color to match panels
                     tags$style(type = "text/css", "body {background-color: #ecf0f1}"),
                     tags$style(type= "text/css", "{width:100%;
                                margin-bottom:5px;
                                text-align: center;}
                                .inner
                                {display: inline-block;}"),
                     HTML('<div id="outer">'),
                     #Set Searchvar width optimal for device
                     tags$style(type = "text/css", paste0('#search {width: ', input$GetScreenWidth - 84, 'px}')),
                     #Inputs
                     div(style="display:inline-block", 
                         textInput("search", 
                                   value = ifelse(Sys.Date() == as.Date(paste0(this_year,"-11-08")), "Election Day!", ""),
                                   label = NULL, 
                                   placeholder = "Search")),
                     tags$style(style="text/css", chartr0('#mapPanel button .fa:before { content: "\\f056";  }
                                                          #mapPanel button.collapsed .fa:before { content: "\\f055";  }')),
                     HTML('<button class="btn collapsed" data-toggle="collapse" data-target="#mobile"><i class="fa fa-search-plus" aria-hidden="true"></i></button></div><div id="mobile" class="collapse">
                          <small style="font-size:11px;margin-left:3px">Not locations are exact. (See &rsquo;About&rsquo; for details.)</small>
                          <br>
                          <br>'),
                     dateRangeInput("dates",
                                    label = NULL,
                                    start = Sys.Date()-10,
                                    end = Sys.Date(),
                                    startview = "day"),
                     HTML('<font color="#F47B25">'),
                     checkboxInput("toggle311",
                                   label = "311 Requests",
                                   value = TRUE),
                     HTML('</font>'),
                     uiOutput("request_UI"),
                     selectInput("dept_select",
                                 label = NULL,
                                 c(`Department`='', levels(load311$DEPARTMENT)),
                                 multiple = TRUE,
                                 selectize=TRUE),
                     selectInput("origin_select",
                                 label = NULL,
                                 c(`Request Origin`='', levels(load311$REQUEST_ORIGIN)),
                                 multiple = TRUE,
                                 selectize=TRUE),
                     HTML('<font color="#3663AD">'),
                     checkboxInput("toggleBlotter",
                                   label = "Police Blotter",
                                   value= TRUE),
                     HTML('</font>'),
                     selectInput("hier",
                                 label = NULL,
                                 c(`Hierarchy`='', levels(load.blotter$HIERARCHY)),
                                 multiple = TRUE,
                                 selectize = TRUE),
                     uiOutput("offense_UI"),
                     HTML('<font color="#A91622">'),
                     checkboxInput("toggleArrests",
                                   label = "Arrests",
                                   value = TRUE),
                     HTML('</font>'),
                     HTML('<font color="#ED2393">'),
                     checkboxInput("toggleCitations",
                                   label = "Non-Traffic Citations",
                                   value = TRUE),
                     HTML('</font>'),
                     HTML('<font color="#009FE1">'),
                     checkboxInput("togglePermits",
                                   label = "Building Permits",
                                   value = TRUE),
                     HTML('</font>'),
                     selectInput("permit_select",
                                 label = NULL,
                                 c(`Permit Type`='', levels(load.permits$permit_type)),
                                 multiple = TRUE,
                                 selectize=TRUE),
                     selectInput("status_select",
                                 label = NULL,
                                 c(`Permit Status`='', levels(load.permits$current_status)),
                                 multiple = TRUE,
                                 selectize=TRUE),
                     HTML('<font color="#0B9444">'),
                     checkboxInput("toggleViolations",
                                   label = "Code Violations", 
                                   value = TRUE),
                     HTML('</font>'),
                     uiOutput("violations_UI"),
                     selectInput("result_select",
                                 label = NULL,
                                 c(`Inspection Result`='', levels(load.violations$INSPECTION_RESULT)),
                                 multiple = TRUE,
                                 selectize=TRUE),
                     HTML('<font color="#b9a5c1">'),
                     checkboxInput("toggleCproj",
                                   label = "Capital Projects",
                                   value = TRUE),
                     HTML('</font>'),
                     selectInput("funcarea_select",
                                 label = NULL,
                                 c(`Functional Area`='', levels(load.cproj$CapitalProjectFunctionalAreaField)),
                                 multiple = TRUE,
                                 selectize=TRUE),
                     HTML('<font color="#474545">'),
                     checkboxInput("toggleFacilities",
                                   label = "City Facilities",
                                   value = TRUE),
                     HTML('</font>'),
                     selectInput("usage_select",
                                 label = NULL,
                                 c(`Facility Usage`='', levels(load.facilities$usage)),
                                 multiple = TRUE,
                                 selectize=TRUE),
                     selectInput("filter_select",
                                 "Filter by Area",
                                 c(`Area Type`='', c("Neighborhood", "Council District", "Police Zone", "Public Works Division")),
                                 selectize = TRUE,
                                 selected = ""),
                     uiOutput("filter_UI"),
                     HTML('</div>')
                     ),
        #Generate Map
        leafletOutput("map"),
        #Set map to style for Mobile
        tags$style(type = "text/css", "#map {height: calc(100vh - 110px) !important;}")
                     )
  }
})
  #Filter by Area Display Options
  output$filter_UI <- renderUI({
    if (input$filter_select == "Neighborhood"){
      selectInput("hood_select",
                  label = NULL,
                  c(`Neighborhood`='', levels(load311$NEIGHBORHOOD)),
                  multiple = TRUE,
                  selectize=TRUE)
    } else if (input$filter_select == "Public Works Division") {
      selectInput("DPW_select",
                  label = NULL,
                  c(`Public Works Division`='', levels(load311$PUBLIC_WORKS_DIVISION)),
                  multiple = TRUE,
                  selectize=TRUE)
    } else if (input$filter_select == "Police Zone") {
      selectInput("zone_select",
                  label = NULL,
                  c(`Police Zone`='', levels(load311$POLICE_ZONE)),
                  multiple = TRUE,
                  selectize=TRUE)
    } else if (input$filter_select == "Council District") {
      selectInput("council_select",
                  label = NULL,
                  c(`Council District`='', levels(load311$COUNCIL_DISTRICT)),
                  multiple = TRUE,
                  selectize=TRUE)
    }
  })
  #311 Request Type UI
  output$request_UI <- renderUI({
    dat311 <- load311
    if (length(input$dept_select) > 0) {
      dat311 <- dat311[dat311$DEPARTMENT %in% input$dept_select,]
      
      dat311$REQUEST_TYPE <- as.character(dat311$REQUEST_TYPE)
      dat311$REQUEST_TYPE <- as.factor(dat311$REQUEST_TYPE)
    }
    
    selectInput("req.type",
                label = NULL,
                c(`Request Type`='', levels(dat311$REQUEST_TYPE)),
                multiple = TRUE,
                selectize=TRUE
    )
  })
  #Blotter Offense UI
  output$offense_UI <- renderUI({
    blotter <- load.blotter
    
    #Hierarchy Filter
    if (length(input$hier) > 0){
      blotter <- blotter[blotter$HIERARCHY %in% input$hier,]
    } 
    
    #Search Filter
    if (!is.null(input$search) && input$search != "") {
      blotter <- blotter[apply(blotter, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    #Geographic Filters
    if (length(input$zone_select) > 0 & input$filter_select == "Police Zone"){
      blotter <- blotter[blotter$POLICE_ZONE %in% input$zone_select,]
    } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
      blotter <- blotter[blotter$INCIDENTNEIGHBORHOOD %in% input$hood_select,]
    } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
      blotter <- blotter[blotter$PUBLIC_WORKS_DIVISION %in% input$DPW_select,]
    } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
      blotter <- blotter[blotter$COUNCIL_DISTRICT %in% input$council_select,]
    }
    
    blotter <- subset(blotter, date >= input$dates[1] & date <= input$dates[2])
    
    #Select offense rows
    offenses <- blotter[,offenses1:offensesCol]
    
    for (i in 1:ncol(offenses)) {
      #Apply character
      lvls <- as.character(offenses[,i])
      
      if (i == 1) {
        #Create list beginning
        offenseList <- as.data.frame(lvls)
      } else {
        #Append List
        offenseList <- rbind(lvls, offenseList)
      }
      #Trim list
      offenseList <- unique(offenseList)
    } 
    
    #Apply factor
    offenseList$lvls <- as.factor(offenseList$lvls)
    
    selectInput("offense_select",
                label = NULL,
                c(`Offense Type`='',levels(offenseList$lvls)),
                multiple = TRUE,
                selectize=TRUE
    )
  })
  #Code Violations UI
  output$violations_UI <- renderUI({
    violations <- load.violations
    
    #Date Filter
    violations <- subset(violations, date >= input$dates[1] & date <= input$dates[2])
    
    violations <- violations[,violations1:violationsCol]
    
    for (i in 1:ncol(violations)) {
      #Apply character
      lvls <- as.character(violations[,i])
      
      if (i == 1) {
        #Create list beginning
        violationList <- as.data.frame(lvls)
      } else {
        #Append List
        violationList <- rbind(lvls, violationList)
      }
      #Trim list
      violationList <- unique(violationList)
    } 
    
    #Apply factor
    violationList$lvls <- as.factor(violationList$lvls)
    
    selectInput("violation_select",
                label = NULL,
                c(`Violation`='', levels(violationList$lvls)),
                multiple = TRUE,
                selectize=TRUE)
  })
  #Boundary Data
  #Neighborhoods
  hoodsInput <- reactive({
    hoods <- load.hoods
    
    if (length(input$hood_select) > 0){
      hoods <- hoods[hoods$hood %in% input$hood_select,]
    }
    
    hoods
  })
  #Council District
  councilInput <- reactive({
    council <- load.council
    
    if (length(input$council_select) > 0){
      council <- council[council$COUNCIL_DISTRICT %in% input$council_select,]
    }
    
    council
  })
  #DPW Divisions
  dpwInput <- reactive({
    dpw <- load.dpw
    
    if (length(input$DPW_select) > 0){
      dpw <- dpw[dpw$PUBLIC_WORKS_DIVISION %in% input$DPW_select,]
    }
    
    dpw
  })
  #Police Zones
  zonesInput <- reactive({
    zones <- load.zones
    
    if (length(input$zone_select) > 0){
      zones <- zones[zones$POLICE_ZONE %in% input$zone_select,]
    }
    
    zones
  })
  #Point Data
  #311 data with filters
  dat311Input <- reactive({
    dat311 <- load311
    
    #Sort
    dat311 <- dat311[rev(order(as.Date(dat311$date, format="%d/%m/%Y"))),]
    
    #311 Filters
    if (length(input$dept_select) > 0){
      dat311 <- dat311[dat311$DEPARTMENT %in% input$dept_select,]
    }
    if (length(input$req.type) > 0){
      dat311 <- dat311[dat311$REQUEST_TYPE %in% input$req.type,]
    }
    if (length(input$origin_select) > 0){
      dat311 <- dat311[dat311$REQUEST_ORIGIN %in% input$origin_select,]
    }
    
    #Geographic Filters
    if (length(input$zone_select) > 0 & input$filter_select == "Police Zone") {
      dat311 <- dat311[dat311$POLICE_ZONE %in% input$zone_select,]
    } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
      dat311 <- dat311[dat311$NEIGHBORHOOD %in% input$hood_select,]
    } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
      dat311 <- dat311[dat311$PUBLIC_WORKS_DIVISION %in% input$DPW_select,]
    } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
      dat311 <- dat311[dat311$COUNCIL_DISTRICT %in% input$council_select,]
    } else if (length(input$firez_select) > 0 & input$filter_select == "Fire Zone") {
      dat311 <- dat311[dat311$FIRE_ZONE %in% input$firez_select,]
    }
    
    dat311 <- subset(dat311, date >= input$dates[1] & date <= input$dates[2])
    
    #Search Filter
    if (!is.null(input$search) && input$search != "") {
      dat311 <- dat311[apply(dat311, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(dat311)
  })
  #Police Blotter data with filters
  blotterInput <- reactive({
    blotter <- load.blotter
    
    #Date filter
    blotter <- subset(blotter, date >= input$dates[1] & date <= input$dates[2])
    
    #Sort
    blotter <- blotter[rev(order(as.Date(blotter$date, format="%d/%m/%Y"))),]
    
    #Hierarchy Filter
    if (length(input$hier) > 0){
      blotter <- blotter[blotter$HIERARCHY %in% input$hier,]
    } 
    
    #Geographic Filters
    if (length(input$zone_select) > 0 & input$filter_select == "Police Zone"){
      blotter <- blotter[blotter$POLICE_ZONE %in% input$zone_select,]
    } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
      blotter <- blotter[blotter$INCIDENTNEIGHBORHOOD %in% input$hood_select,]
    } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
      blotter <- blotter[blotter$PUBLIC_WORKS_DIVISION %in% input$DPW_select,]
    } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
      blotter <- blotter[blotter$COUNCIL_DISTRICT %in% input$council_select,]
    }
    
    #Prepare Filter
    if (length(input$offense_select) > 0) { 
      for (i in offenses1:offensesCol) {
        if (i ==offenses1) {
          out <- blotter[blotter[,i] %in% input$offense_select,]
        } else {
          new <- blotter[blotter[,i] %in% input$offense_select,]
          out <- rbind(out, new)
        }
      }
      blotter <- unique(out)
    }
    
    #Search Filter
    if (!is.null(input$search) && input$search != "") {
      blotter <- blotter[apply(blotter, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(blotter)
  })
  #Arrest data with filters
  arrestsInput <- reactive({
    arrests <- load.arrests
    
    #Date filter
    arrests <- subset(arrests, date >= input$dates[1] & date <= input$dates[2])
    
    #Sort
    arrests <- arrests[rev(order(as.Date(arrests$date, format="%d/%m/%Y"))),]
    
    #Geographic Filters
    if (length(input$zone_select) > 0 & input$filter_select == "Police Zone"){
      arrests <- arrests[arrests$POLICE_ZONE %in% input$zone_select,]
    } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
      arrests <- arrests[arrests$INCIDENTNEIGHBORHOOD %in% input$hood_select,]
    } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
      arrests <- arrests[arrests$PUBLIC_WORKS_DIVISION %in% input$DPW_select,]
    } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
      arrests <-arrests[arrests$COUNCIL_DISTRICT %in% input$council_select,]
    } 
    
    #Offense Filter
    if (length(input$offense_select) > 0) { 
      for (i in offensesAr1:offensesColAr) {
        if (i == offensesAr1) {
          out <- arrests[arrests[,i] %in% input$offense_select,]
        } else {
          new <- arrests[arrests[,i] %in% input$offense_select,]
          out <- rbind(out, new)
        }
      }
      arrests <- unique(out)
    }
    
    #Search Filter
    if (!is.null(input$search) && input$search != "") {
      arrests <- arrests[apply(arrests, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(arrests)
  })
  #Citations data with filters
  citationsInput <- reactive({
    citations <- load.citations
    
    #Date filter
    citations <- subset(citations, date >= input$dates[1] & date <= input$dates[2])
    
    #Sort
    citations <- citations[rev(order(as.Date(citations$date, format="%d/%m/%Y"))),]
    
    #Geographic Filters
    if (length(input$zone_select) > 0 & input$filter_select == "Police Zone"){
      citations <- citations[citations$POLICE_ZONE %in% input$zone_select,]
    } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
      citations <- citations[citations$INCIDENTNEIGHBORHOOD %in% input$hood_select,]
    } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
      citations <- citations[citations$PUBLIC_WORKS_DIVISION %in% input$DPW_select,]
    } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
      citations <-citations[citations$COUNCIL_DISTRICT %in% input$council_select,]
    } 
    
    #Offense Filter
    if (length(input$offense_select) > 0) { 
      for (i in offensesCit1:offensesColCit) {
        if (i == offensesCit1) {
          out <- citations[citations[,i] %in% input$offense_select,]
        } else {
          new <- citations[citations[,i] %in% input$offense_select,]
          out <- rbind(out, new)
        }
      }
      citations <- unique(out)
    }
    
    #Search Filter
    if (!is.null(input$search) && input$search != "") {
      citations <- citations[apply(citations, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(citations)
  })
  #Code Violations data with filters
  violationsInput <- reactive({
    violations <- load.violations
    
    #Date Filter
    violations <- subset(violations, date >= input$dates[1] & date <= input$dates[2])
    
    #Sort
    violations <- violations[rev(order(as.Date(violations$date, format="%d/%m/%Y"))),]
    
    #Violation Filter
    if (length(input$violation_select) > 0) { 
      for (i in violations1:violationsCol) {
        if (i == violations1) {
          out <- violations[violations[,i] %in% input$violation_select,]
        } else {
          new <- violations[violations[,i] %in% input$violation_select,]
          out <- rbind(out, new)
        }
      }
      violations <- unique(out)
    }
    
    #Result Filter
    if (length(input$result_select) > 0){
      violations <- violations[violations$INSPECTION_RESULT %in% input$result_select,]
    }
    
    if (length(input$zone_select) > 0 & input$filter_select == "Police Zone"){
      violations <- violations[violations$POLICE_ZONE %in% input$zone_select,]
    } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
      violations <- violations[violations$NEIGHBORHOOD %in% input$hood_select,]
    } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
      violations <- violations[violations$PUBLIC_WORKS_DIVISION %in% input$DPW_select,]
    } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
      violations <-violations[violations$COUNCIL_DISTRICT %in% input$council_select,]
    }
    
    #Search Filter
    if (!is.null(input$search) && input$search != "") {
      violations <- violations[apply(violations, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(violations)
  })
  #Building Permits data with filters
  permitsInput <- reactive({
    permits <- load.permits
    
    #Date Filter
    permits <- subset(permits, date >= input$dates[1] &  date <= input$dates[2])
    
    #Sort
    permits <- permits[rev(order(as.Date(permits$date, format="%d/%m/%Y"))),]
    
    #Permit Filters
    if (length(input$permit_select) > 0) {
      permits <- permits[permits$permit_type %in% input$permit_select,]
    } 
    if (length(input$status_select) > 0) {
      permits <- permits[permits$current_status %in% input$status_select,]
    } 
    if (length(input$category_select) > 0) {
      permits <- permits[permits$record_category %in% input$category_select,]
    }
    
    #Geographic Filters
    if (length(input$zone_select) > 0 & input$filter_select == "Police Zone"){
      permits <- permits[permits$POLICE_ZONE %in% input$zone_select,]
    } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
      permits <- permits[permits$neighborhood %in% input$hood_select,]
    } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
      permits <- permits[permits$PUBLIC_WORKS_DIVISION %in% input$DPW_select,]
    } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
      permits <-permits[permits$COUNCIL_DISTRICT %in% input$council_select,]
    }
    
    #Search Filter
    if (!is.null(input$search) && input$search != "") {
      permits <- permits[apply(permits, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    } 
    
    # Append Workflows
    workflow <- load.workflow
    # Remove Old Workflows
    workflow <- workflow[workflow$permit_id %in% permits$permit_id,]
    workflow$permit_id <- as.factor(workflow$permit_id)
    
    # Loop which aggregates appropriate Workflows
    for (i in  levels(workflow$permit_id)){
      temp <- subset(workflow, permit_id == i)
      temp <- temp[order(temp$history_seq_nbr),]
      temp <- subset(workflow, permit_id == i, select = c(tool))
      tt <- paste0('<br><b>Workflow:</b><br><dl style="margin-bottom: 0px; margin-left:10px";>', toString(temp), "</dl>")
      tt <- gsub(",", "", tt)
      df <- data.frame(i,tt)
      if (i == levels(workflow$permit_id)[1]){
        tt.df <- df
      } else {
        tt.df <- rbind(tt.df, df)
      }
    }
    
    # Rename Columns for Merge
    colnames(tt.df) <- c("permit_id", "tt")
    # Merge Workflow Tooltip to Permits
    permits <- merge(permits, tt.df, by = "permit_id", all.x = TRUE)
    
    return(permits)
  })
  #City Facilities data with filters
  facilitiesInput <- reactive({
    facilities <- load.facilities
    
    #Usage Filter
    if (length(input$usage_select) > 0) {
      facilities <- facilities[facilities$usage %in% input$usage_select,]
    }
    
    #Geographic Filters
    if (length(input$zone_select) > 0 & input$filter_select == "Police Zone"){
      facilities <- facilities[facilities$POLICE_ZONE %in% input$zone_select,]
    } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
      facilities <- facilities[facilities$NEIGHBORHOOD %in% input$hood_select,]
    } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
      facilities <- facilities[facilities$PUBLIC_WORKS_DIVISION %in% input$DPW_select,]
    } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
      facilities <- facilities[facilities$COUNCIL_DISTRICT %in% input$council_select,]
    } 
    
    #Search Filter
    if (!is.null(input$search) && input$search != "") {
      facilities <- facilities[apply(facilities, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(facilities)
  })
  #Capital Projects data with filters
  cprojInput <- reactive({
    cproj <- load.cproj
    
    #Year filter
    cproj <- subset(cproj, year >= this_year)
    
    if (length(input$funcarea_select) > 0) {
      cproj <- cproj[cproj$CapitalProjectFunctionalAreaField %in% input$funcarea_select,]
    }
    
    #Geographic Filters
    if (length(input$zone_select) > 0 & input$filter_select == "Police Zone"){
      cproj <- cproj[cproj$POLICE_ZONE %in% input$zone_select,]
    } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
      cproj <- cproj[cproj$NEIGHBORHOOD %in% input$hood_select,]
    } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
      cproj <- cproj[cproj$PUBLIC_WORKS_DIVISION %in% input$DPW_select,]
    } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
      cproj <- cproj[cproj$COUNCIL_DISTRICT %in% input$council_select,]
    } 
    
    #Search Filter
    if (!is.null(input$search) && input$search != "") {
      cproj <- cproj[apply(cproj, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(cproj)
  })
  #Generate table for Data page and export
  #Note all reports do same data process comments only exist for 311
  reportInput <- reactive({
    if (input$report_select == "311 Requests") {
      #Load dataset
      dat311 <- dat311Input()
      
      #Select display columns
      dat311 <- subset(dat311, select = c(REQUEST_TYPE, DEPARTMENT, CREATED_ON, NEIGHBORHOOD, COUNCIL_DISTRICT, POLICE_ZONE, PUBLIC_WORKS_DIVISION))
      
      #Rename columns for humans
      colnames(dat311) <- c("Request Type", "Dept", "Create Date", "Neighborhood", "Council District", "Police Zone",  "Public Works Division")
      
      #Set report data
      report <- dat311
    } else if (input$report_select == "Blotter"){
      #Select Columns of Interest
      blotter <- blotterInput()
      
      blotter <- subset(blotter, select = c(HIERARCHY, OFFENSES, INCIDENTTIME, CLEAREDFLAG, INCIDENTLOCATION, INCIDENTNEIGHBORHOOD, COUNCIL_DISTRICT, POLICE_ZONE, PUBLIC_WORKS_DIVISION, CCR))
      
      colnames(blotter)  <- c("Hierarchy", "Offenses", "Date/Time", "Cleared", "Location", "Neighborhood", "Council District", "Police Zone", "Public Works Division", "CCR")
      
      report <- blotter
    } else if (input$report_select == "Arrests") {
      arrests <- arrestsInput()
      
      arrests <- subset(arrests, select = c(OFFENSES,  AGE, GENDER, RACE, ARRESTTIME, ARRESTLOCATION, INCIDENTLOCATION, INCIDENTNEIGHBORHOOD, COUNCIL_DISTRICT, POLICE_ZONE, PUBLIC_WORKS_DIVISION, CCR))
      
      colnames(arrests)  <- c("Offense(s)", "Age", "Gender", "Race", "Date/Time", "Arrest Location", "Incident Location", "Incident Neighborhood", "Incident Council District","Incident Police Zone", "Incident Public Works Division", "CCR")
      
      report <- arrests
    } else if (input$report_select == "Non-Traffic Citations") {
      citations <- citationsInput()
      
      citations <- subset(citations, select = c(OFFENSES, AGE, GENDER, RACE, CITEDTIME, INCIDENTLOCATION, NEIGHBORHOOD, COUNCIL_DISTRICT, POLICE_ZONE, PUBLIC_WORKS_DIVISION, CCR))
      
      colnames(citations)  <- c("Offense(s)", "Age", "Gender", "Race", "Date/Time", "Incident Location", " Neighborhood", "Council District", "Police Zone", "Public Works Division", "CCR")
      
      report <- citations
    } else if (input$report_select == "Building Permits") {
      permits <- permitsInput()
      
      permits <- subset(permits, select = c(permit_type, current_status, intake_date, issued_date, full_address, neighborhood, COUNCIL_DISTRICT, POLICE_ZONE, PUBLIC_WORKS_DIVISION, permit_id, url))
      
      colnames(permits) <- c("Type",  "Status", "Intake Date",  "Issued Date", "Address", "Neighborhood", "Council District", "Police Zone", "Public Works Division", "Permit ID", "Parcel ID")
      
      report <- permits
    } else if (input$report_select == "Code Violations"){
      violations <- violationsInput()
      
      violations <- subset(violations, select = c(VIOLATION, INSPECTION_RESULT, INSPECTION_DATE, full_address, NEIGHBORHOOD, COUNCIL_DISTRICT, POLICE_ZONE, PUBLIC_WORKS_DIVISION, CASE_NUMBER, url))
      
      colnames(violations) <- c("Violation", "Result", "Inspection Date", "Address", "Neighborhood", "Council District", "Police Zone", "Public Works Division", "Case #", "Parcel ID")
      report <- violations
    } else if (input$report_select == "City Facilities") {
      facilities <- facilitiesInput()
      
      facilities <- subset(facilities, select = c(usage, ID, PrimaryUser, address, NEIGHBORHOOD, COUNCIL_DISTRICT, PUBLIC_WORKS_DIVISION, POLICE_ZONE))
      
      colnames(facilities) <- c("Usage", "Name", "Dept", "Address", "Neighborhood", "Council", "Public Works Division", "Police Zone")
      
      report <- facilities
    } else if (input$report_select == "Capital Projects") {
      cproj <- cprojInput()
      
      cproj <- subset(cproj, select = c(CapitalProjectFunctionalAreaField, CapitalProjectNameField, StatusField, BudgetedAmountField, TotalCostActualField, cgAssetandIDField, StartDateField, StopDateField, TaskDescriptionField, NEIGHBORHOOD, COUNCIL_DISTRICT, PUBLIC_WORKS_DIVISION, POLICE_ZONE))
      
      colnames(cproj) <- c("Functional Area", "Name", "Status", "Amount Budgeted", "Amount Spent", "Asset", "Start Date", "Stop Date", "Description", "Neighborhood", "Council", "Public Works Division", "Police Zone")
      
      report <- cproj
    }
    #Return Data
    report
  })
  downloadInput <- reactive({
    report <- reportInput()
    
    #Report Table Search Filter
    if (!is.null(input$report.table_search) && input$report.table_search != "") {
      download <- report[apply(report, 1, function(row){any(grepl(input$report.table_search, row, ignore.case = TRUE))}), ]
    }
    
    return(download)
  })
  #Generate Report Table
  output$report.table <- DT::renderDataTable({
    #Load Report dataset
    reportInput()
  }, escape = FALSE, options = list(scrollX = TRUE), rownames= FALSE)
  #Execute download function
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$report_select, ".csv", sep="") },
    content = function(file) {
      write.csv(downloadInput(), file)
    }
  )
  #Build main map
  output$map <- renderLeaflet({
    recs <- 0
    layerCount <- 0
    map <- leaflet() %>% 
      addProviderTiles("OpenStreetMap.HOT", options = providerTileOptions(noWrap = TRUE))
    #Boundary Layers
    #Neighborhood
    if (input$filter_select == "Neighborhood") {
      hoods <- hoodsInput()
      if (nrow(hoods) > 0) {
        map <- addPolygons(map, data = hoods,
                           stroke = TRUE, smoothFactor = 0, weight = 1, color = "#000000", opacity = 0.6,
                           fill = TRUE, fillColor = "#00FFFFFF", fillOpacity = 0, 
                           popup = ~paste("<font color='black'><b>Neighborhood:</b> ", htmlEscape(hood), "</font>")
        )
      }
    } else if (input$filter_select == "Council District"){
      council <- councilInput()
      if (nrow(council) > 0) {
        map <- addPolygons(map, data = council,
                           stroke = TRUE, smoothFactor = 0, weight = 1, color = "#000000", opacity = 0.6,
                           fill = TRUE, fillColor = "#00FFFFFF", fillOpacity = 0, 
                           popup = ~paste("<font color='black'><b>District:</b> ", htmlEscape(COUNCIL_DISTRICT),
                                          "<br><b>Phone #:</b>", htmlEscape(phone),
                                          "<br><b>Committee:</b>", htmlEscape(committee), "</font>")
        )
      }
    } else if (input$filter_select == "Public Works Division"){
      dpw <- dpwInput()
      if (nrow(dpw) > 0) {
        map <- addPolygons(map, data = dpw,
                           stroke = TRUE, smoothFactor = 0, weight = 1, color = "#000000", opacity = 0.6,
                           fill = TRUE, fillColor = "#00FFFFFF", fillOpacity = 0, 
                           popup = ~paste("<font color='black'><b>Zone:</b> ", htmlEscape(PUBLIC_WORKS_DIVISION), "</font>")
        )
      }
    } else if (input$filter_select == "Police Zone"){
      zones <- zonesInput()
      if (nrow(zones) > 0) {
        map <- addPolygons(map, data = zones,
                           stroke = TRUE, smoothFactor = 0, weight = 1, color = "#000000", opacity = 0.6,
                           fill = TRUE, fillColor = "#00FFFFFF", fillOpacity = 0, 
                           popup = ~paste("<font color='black'><b>Zone:</b> ", htmlEscape(POLICE_ZONE), "</font>")
        )
      }
    }  
    
    #Point Layers
    #311 Data
    if (input$toggle311) {
      dat311 <- dat311Input()
      dat311 <- dat311[!(is.na(dat311$X)),] 
      dat311 <- dat311[!(is.na(dat311$Y)),]
      dat311 <- subset(dat311, X > -80.242767 & X < -79.660492 & Y < 40.591014 & Y > 40.266428)
      if (nrow(dat311) > 0){
        layerCount <- layerCount + 1
        map <- addMarkers(map, data = dat311,
                          clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                                                                                      var childCount = cluster.getChildCount();  
                                                                                      if (childCount < 10) {  
                                                                                      c = 'rgba(252, 236, 214, 1);'
                                                                                      } else if (childCount < 100) {  
                                                                                      c = 'rgba(252, 188, 101, 1);'  
                                                                                      } else { 
                                                                                      c = 'rgba(248, 155, 59, 1);'  
                                                                                      }    
                                                                                      return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
      }")), ~X, ~Y, icon = ~icons_311[icon],
                        popup = ~(paste("<font color='black'><b>Request:</b>", dat311$REQUEST_TYPE,
                                        "<br><b>Location Accuracy:</b>", dat311$GEO_ACCURACY,
                                        "<br><b>Open Date/Time:</b>", dat311$CREATED_ON,
                                        "<br><b>Dept:</b>", dat311$DEPARTMENT,
                                        "<br><b>Origin:</b>", dat311$REQUEST_ORIGIN2,
                                        "<br><b>Neighborhood:</b>", dat311$NEIGHBORHOOD,
                                        "<br><b>Council District:</b>", dat311$COUNCIL_DISTRICT,
                                        "<br><b>Police Zone:</b>", dat311$POLICE_ZONE,
                                        "<br><b>Public Works Division:</b>", dat311$PUBLIC_WORKS_DIVISION,
                                        '<br><center><a href="http://pittsburghpa.gov/311/form" target="_blank">Submit a 311 Request!</a></center></font>'))
        )
        recs <- recs + nrow(dat311)
  }
}
    #Arrests Layer
    if(input$toggleArrests) {
      arrests <- arrestsInput()
      #Remove unmappables
      arrests <- arrests[!(is.na(arrests$X)),] 
      arrests <- arrests[!(is.na(arrests$Y)),]
      arrests <- subset(arrests, X > -80.242767 & X < -79.660492 & Y < 40.591014 & Y > 40.266428)
      if (nrow(arrests) > 0) {
        layerCount <- layerCount + 1
        map <- addMarkers(map, data=arrests,
                          clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                                                                                      var childCount = cluster.getChildCount();  
                                                                                      if (childCount < 10) {  
                                                                                      c = 'rgba(178, 102, 102, 0.95);'
                                                                                      } else if (childCount < 100) {  
                                                                                      c = 'rgba(204, 152, 152, 0.95);'  
                                                                                      } else { 
                                                                                      c = 'rgba(128, 0, 0, 0.95);'  
                                                                                      }    
                                                                                      return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
      }")), ~X, ~Y, icon = ~icons_arrests[icon],
                          popup = ~(paste("<font color='black'><b>Offense(s):</b>", arrests$OFFENSES,
                                          "<br><b>Date/Time:</b>", arrests$ARRESTTIME,
                                          "<br><b>Age:</b>", arrests$AGE,
                                          "<br><b>Gender:</b>", arrests$GENDER,
                                          "<br><b>Race:</b>", arrests$RACE,
                                          "<br><b>Arrest Location:</b>", arrests$ARRESTLOCATION,
                                          "<br><b>Incident Location:</b>", arrests$INCIDENTLOCATION,
                                          "<br><b>Incident Neighborhood:</b>", arrests$INCIDENTNEIGHBORHOOD,
                                          "<br><b>Council District:</b>", arrests$COUNCIL_DISTRICT,
                                          "<br><b>Incident Police Zone:</b>", arrests$POLICE_ZONE,
                                          "<br><b>CCR:</b>", arrests$CCR, "</font>"))
        )
        recs <- recs + nrow(arrests)
                }
      }
    #Non-Traffic Citations
    if (input$toggleCitations) {
      citations <- citationsInput()
      #Remove unmappables
      citations <- citations[!(is.na(citations$X)),] 
      citations <- citations[!(is.na(citations$Y)),]
      citations <- subset(citations, X > -80.242767 & X < -79.660492 & Y < 40.591014 & Y > 40.266428)
      if (nrow(citations) > 0) {
        layerCount <- layerCount + 1
        map <- addMarkers(map, data=citations,
                          clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                                                                                      var childCount = cluster.getChildCount();  
                                                                                      if (childCount < 10) {  
                                                                                      c = 'rgba(255, 224, 229, 0.95);'
                                                                                      } else if (childCount < 100) {  
                                                                                      c = 'rgba(255, 167, 182, 0.95);'  
                                                                                      } else { 
                                                                                      c = 'rgba(255, 204, 213, 0.95);'  
                                                                                      }    
                                                                                      return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
      }")), ~X, ~Y, icon = ~icons_citations[icon],
                          popup = ~(paste("<font color='black'><b>Offense(s):</b>", citations$OFFENSES,
                                          "<br><b>Date/Time:</b>", citations$CITEDTIME,
                                          "<br><b>Age:</b>", citations$AGE,
                                          "<br><b>Gender:</b>", citations$GENDER,
                                          "<br><b>Race:</b>", citations$RACE,
                                          "<br><b>Incident Location:</b>", citations$INCIDENTLOCATION,
                                          "<br><b>Neighborhood:</b>", citations$NEIGHBORHOOD,
                                          "<br><b>Council District:</b>", citations$COUNCIL_DISTRICT,
                                          "<br><b>Police Zone:</b>", citations$POLICE_ZONE,
                                          "<br><b>CCR:</b>", citations$CCR, "</font>"))
        )
        recs <- recs + nrow(citations)
        }
      }
    #Police Blotter Layer
    if (input$toggleBlotter) {
      blotter <- blotterInput()
      #Remove unmappables
      blotter <- blotter[!(is.na(blotter$X)),] 
      blotter <- blotter[!(is.na(blotter$Y)),]
      blotter <- subset(blotter, X >= -80.242767 & X <= -79.660492 & Y <= 40.591014 & Y >= 40.266428)
      #Remove non-pittsburgh incidents for map (these incidents still appear in data extracts)
      blotter <- subset(blotter, POLICE_ZONE != "OSC" | INCIDENTNEIGHBORHOOD != "Outside City" | INCIDENTNEIGHBORHOOD != "Outside County")
      #Remove Citation instances from Blotter data (if layer is)
      if (input$toggleCitations) {
        blotter <- blotter[!(blotter$CCR %in% citations$CCR),]
      }
      if (nrow(blotter) > 0) {
        layerCount <- layerCount + 1
        map <- addMarkers(map, data=blotter,
                          clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                                                                                      var childCount = cluster.getChildCount();  
                                                                                      if (childCount < 10) {  
                                                                                      c = 'rgba(222, 229, 242, 0.95);'
                                                                                      } else if (childCount < 100) {  
                                                                                      c = 'rgba(140, 165, 210, 0.95);'  
                                                                                      } else { 
                                                                                      c = 'rgba(67, 109, 179, 0.95);'  
                                                                                      }    
                                                                                      return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
      }")), ~X, ~Y, icon = ~icons_blotter[icon],
                          popup = ~(paste("<font color='black'><b>Hierarchy:</b>", blotter$HIERARCHY,
                                          "<br><b>Offense(s):</b>", blotter$OFFENSES,
                                          "<br><b>Cleared Flag:</b>", blotter$CLEAREDFLAG,
                                          "<br><b>Date/Time:</b>", blotter$INCIDENTTIME,
                                          "<br><b>Location:</b>", blotter$INCIDENTLOCATION,
                                          "<br><b>Neighborhood:</b>", blotter$INCIDENTNEIGHBORHOOD,
                                          "<br><b>Council District:</b>", blotter$COUNCIL_DISTRICT,
                                          "<br><b>Police Zone:</b>", blotter$POLICE_ZONE,
                                          "<br><b>CCR:</b>", blotter$CCR, "</font>"))
        )
        recs <- recs + nrow(blotter)
        }
      }
    #Building Permits Layer
    if(input$togglePermits) {
      permits <- permitsInput()
      #Remove unmappables
      permits <- permits[!(is.na(permits$lat)),]
      permits <- permits[!(is.na(permits$lon)),]
      permits <- subset(permits, lon > -80.242767 & lon < -79.660492 & lat < 40.591014 & lat > 40.266428)
      if (nrow(permits) > 0) {
        layerCount <- layerCount + 1
        map <- addMarkers(map, data=permits,
                          clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                                                                                      var childCount = cluster.getChildCount();  
                                                                                      if (childCount < 10) {  
                                                                                      c = 'rgba(207, 242, 252, 0.95);'
                                                                                      } else if (childCount < 100) {  
                                                                                      c = 'rgba(117, 214, 247, 0.95);'  
                                                                                      } else { 
                                                                                      c = 'rgba(0, 150, 219, 0.95);'
                                                                                      }
                                                                                      return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
      }")), ~lon, ~lat, icon = ~icons_permits[icon],
               popup = ~(paste("<font color='black'><b>Type:</b>", permits$permit_type,
                               "<br><b>Status:</b>", permits$current_status,
                               "<br><b>Address:</b>", permits$full_address,
                               "<br><b>Neighborhood:</b>", permits$neighborhood,
                               "<br><b>Council District:</b>", permits$COUNCIL_DISTRICT,
                               "<br><b>Police Zone:</b>", permits$POLICE_ZONE,
                               "<br><b>Public Works Division:</b>", permits$PUBLIC_WORKS_DIVISION,
                               "<br><b>Parcel ID:</b>", permits$url,
                               "<br><b>Permit ID:</b>", permits$permit_id,
                               permits$tt,
                               '<br><center><a href="https://pittsburghpa.buildingeye.com/building" target="_blank">Search Permits on Building Eye!</a></center></font></font>'))
        )
        recs <- recs + nrow(permits)
        }
      }
    #Building Code Violations
    if(input$toggleViolations) {
      violations <- violationsInput()
      #Remove unmappables
      violations <- violations[!(is.na(violations$X)),]
      violations <- violations[!(is.na(violations$Y)),]
      violations <- subset(violations, X > -80.242767 & X < -79.660492 & Y < 40.591014 & Y > 40.266428)
      if (nrow(violations) > 0) {
        layerCount <- layerCount + 1
        map <- addMarkers(map, data=violations, 
                          clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                                                                                      var childCount = cluster.getChildCount();  
                                                                                      if (childCount < 10) {  
                                                                                      c = 'rgba(115, 201, 158, 1);'
                                                                                      } else if (childCount < 100) {  
                                                                                      c = 'rgba(57, 168, 113, 1);'  
                                                                                      } else { 
                                                                                      c = 'rgba(0, 136, 68, 1);'  
                                                                                      }    
                                                                                      return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
      }")), ~X, ~Y, icon = ~icons_violations[icon],
                        popup = ~(paste("<font color='black'><b>Violation (Result):</b>", violations$VIOLATION,
                                        "<br><b>Inspection Result:</b>", violations$INSPECTION_RESULT,
                                        "<br><b>Date Inspected:</b>", violations$date,
                                        "<br><b>Corrective Action(s):</b>", violations$CORRECTIVE_ACTION,
                                        "<br><b>Address:</b>", violations$FullAddress,
                                        "<br><b>Neighborhood:</b>", violations$NEIGHBORHOOD,
                                        "<br><b>Council District:</b>", violations$COUNCIL_DISTRICT,
                                        "<br><b>Public Works Division:</b>", violations$PUBLIC_WORKS_DIVISION,
                                        "<br><b>Police Zone:</b>", violations$POLICE_ZONE,
                                        "<br><b>Parcel ID:</b>", violations$url,
                                        "<br><b>Case #:</b>", violations$CASE_NUMBER,
                                        '<br><center><a href="https://pittsburghpa.buildingeye.com/enforcement" target="_blank">Search Violations on Building Eye!</a></center></font>'))
        )
        recs <- recs + nrow(violations)
        }
      }
    #City Facilities Layer
    if (input$toggleFacilities) {
      facilities <- facilitiesInput()
      #Remove unmappables
      facilities <- facilities[!(is.na(facilities$Lng)),]
      facilities <- facilities[!(is.na(facilities$Lat)),]
      facilities <- subset(facilities, Lng > -80.242767 & Lng < -79.660492 & Lat < 40.591014 & Lat > 40.266428)
      if (nrow(facilities) > 0) {
        layerCount <- layerCount + 1
        map <- addMarkers(map, data=facilities,
                          clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                                                                                      var childCount = cluster.getChildCount();  
                                                                                      if (childCount < 10) {  
                                                                                      c = 'rgba(217, 217, 224, 0.95);'
                                                                                      } else if (childCount < 100) {  
                                                                                      c = 'rgba(171, 171, 182, 0.95);'  
                                                                                      } else { 
                                                                                      c = 'rgba(150, 150, 163, 0.95);'  
                                                                                      }    
                                                                                      return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
      }")), ~Lng, ~Lat, icon = ~icons_facilities[icon],
                 popup = ~(paste("<font color='black'><b>Usage:</b>", facilities$usage,
                                 "<br><b>Name:</b>", facilities$ID,
                                 "<br><b>Dept:</b>", facilities$PrimaryUser,
                                 "<br><b>Address:</b>", facilities$address,
                                 "<br><b>Neighborhood:</b>", facilities$NEIGHBORHOOD,
                                 "<br><b>Council District:</b>", facilities$COUNCIL_DISTRICT,
                                 "<br><b>Public Works Division:</b>", facilities$PUBLIC_WORKS_DIVISION,
                                 "<br><b>Police Zone:</b>", facilities$POLICE_ZONE, 
                                 facilities$url, "</font>"))
        )
        recs <- recs + nrow(facilities)
      }
    }
    #Capital Projects Layer
    if (input$toggleCproj) {
      cproj <- cprojInput()
      #Remove unmappables
      cproj <- cproj[!(is.na(cproj$Lng)),]
      cproj <- cproj[!(is.na(cproj$Lat)),]
      cproj <- subset(cproj, Lng > -80.242767 & Lng < -79.660492 & Lat < 40.591014 & Lat > 40.266428)
      if (nrow(cproj) > 0) {
        layerCount <- layerCount + 1
        map <- addMarkers(map, data=cproj,
                          clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                                                                                      var childCount = cluster.getChildCount();  
                                                                                      if (childCount < 10) {  
                                                                                      c = 'rgba(220, 210, 224, 0.95);'
                                                                                      } else if (childCount < 100) {  
                                                                                      c = 'rgba(208, 195, 213, 0.95);'  
                                                                                      } else { 
                                                                                      c = 'rgba(184, 165, 192, 0.95);'  
                                                                                      }    
                                                                                      return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
      }")), ~Lng, ~Lat, icon = ~icons_cproj[icon],
                 popup = ~(paste("<font color='black'><b>Functional Area:</b>", cproj$CapitalProjectFunctionalAreaField, 
                                 "<br><b>Name:</b>", cproj$CapitalProjectNameField,
                                 "<br><b>Statu:s</b>",  cproj$StatusField,
                                 "<br><b>Amount Budgeted:</b>", cproj$BudgetedAmountField,
                                 "<br><b>Amount Spent:</b>", cproj$TotalCostActualField,
                                 "<br><b>Asset:</b>", cproj$cgAssetandIDField,
                                 "<br><b>Start Date:</b>", cproj$StartDateField,
                                 "<br><b>Stop Date:</b>", cproj$StopDateField,
                                 "<br><b>Description:</b>", cproj$TaskDescriptionField,
                                 "<br><b>Neighborhood:</b>", cproj$NEIGHBORHOOD,
                                 "<br><b>Council District:</b>", cproj$COUNCIL_DISTRICT,
                                 "<br><b>Public Works Division:</b>", cproj$PUBLIC_WORKS_DIVISION,
                                 "<br><b>Police Zone:</b>", cproj$POLICE_ZONE
                          ))
        )
        recs <- recs + nrow(cproj)
      }
    }
    print(recs)
    if (layerCount < 1) {
      if (Sys.Date() >= as.Date(paste0(this_year,"-11-01")) & Sys.Date() <= as.Date(paste0(this_year,"-11-08"))) {
        egg <- load.egg
      } else {
        egg <- load.egg[sample(1:nrow(load.egg),1),]
      }
      map <-addMarkers(map, data=egg, ~X, ~Y, icon = ~icons_egg[icon], popup = ~tt) %>% 
        setView(-79.9959, 40.4406, zoom = 10)
    }
    map
    })
  })

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")