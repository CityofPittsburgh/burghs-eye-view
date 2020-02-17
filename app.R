# Burgh's Eye View Points
# Organization: City of Pittsburgh
# Dept: Innovation & Performance
# Team: Analytics & Strategy
# Author: Geoffrey Arnold

# Load required packages
library(shiny)
library(shinythemes)
library(htmltools)

#"Dogfooding" Packages
library(httr)
library(jsonlite)

# Visuals Libraries
library(leaflet)
library(leaflet.extras)
library(DT)
library(sp)
library(rgdal)

# Data Transform
library(plyr)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(stringi)
library(stringr)

# Turn off Scientific Notation
options(scipen = 999)

httr::set_config(config(ssl_verifypeer = 0L))

# Function to read backslashes correctly
chartr0 <- function(foo) chartr('\\','\\/',foo)

# Function to Check Screenwidth
getWidth <- '$(document).on("shiny:connected", function(e) {
var jsWidth = screen.width;
Shiny.onInputChange("GetScreenWidth",jsWidth);
});'

# Make it work when Downloading stuff
httr::set_config(config(ssl_verifypeer = 0L))

dollarsComma <- function(x){
  x <- round(x, 2)
  x <- prettyNum(x, big.mark = ",")
  x <- paste0("$", x)
  return(x)
}

# Function to download WPRDC Data
ckan <- function(id) {
  url <- paste0("https://data.wprdc.org/datastore/dump/", id)
  r <- RETRY("GET", url)
  content(r)
}

# Function to Query WPRDC Data on Time Frame
ckanQueryDates <- function(id, start, end, column) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22", id, "%22%20WHERE%20%22", column, "%22%20%3E=%20%27", start, "%27%20AND%20%22", column, "%22%20%3C=%20%27", end, "%27")
  r <- RETRY("GET", url)
  c <- content(r, "text")
  json <- gsub('NaN', '""', c, perl = TRUE)
  if (length(jsonlite::fromJSON(json)$result$records) == 0) {
    fields <- jsonlite::fromJSON(json)$result$fields$id
    data.frame(t(data.frame(1:length(fields), row.names = fields))[0,])
  } else {
    data.frame(jsonlite::fromJSON(json)$result$records)
  }
}

ckanSQL <- function(url) {
  r <- RETRY("GET", url)
  c <- content(r, "text")
  json <- gsub('NaN', '""', c, perl = TRUE)
  data.frame(jsonlite::fromJSON(json)$result$records)
}

ckanQuery2 <- function(id, query, column, arg, query2, column2) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22", id, "%22%20WHERE%20%22", column,"%22%20=%20%27", query, "%27%20", arg, "%20%22", column2, "%22%20=%20%27", query2, "%27")
  r <- RETRY("GET", url)
  c <- content(r, "text")
  json <- gsub('NaN', '""', c, perl = TRUE)
  if (length(jsonlite::fromJSON(json)$result$records) == 0) {
    fields <- jsonlite::fromJSON(json)$result$fields$id
    data.frame(t(data.frame(1:length(fields), row.names = fields))[0,])
  } else {
    jsonlite::fromJSON(json)$result$records
  }
}

# Query Crash Dataset
ckanQueryCrashes <- function(start_date, end_date) {
  start_month <- format(as.Date(start_date), "%Y-%m-01")
  end_month <- format(as.Date(end_date), "%Y-%m-01")
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%222c13021f-74a9-4289-a1e5-fe0472c89881%22%20WHERE%20%22MUNICIPALITY%22%20LIKE%20%27%%252301%27%20AND%20TO_DATE(%22CRASH_YEAR%22%20||%20%27-%27%20||%20%22CRASH_MONTH%22%20||%20%27-01%27,%20%27YYYY-MM-DD%27)%20BETWEEN%20%27", start_month, "%27%20AND%27", end_month, "%27")
  r <- RETRY("GET", url)
  c <- content(r, "text")
  json <- gsub('NaN', '""', c, perl = TRUE)
  if (length(jsonlite::fromJSON(json)$result$records) == 0) {
    fields <- jsonlite::fromJSON(json)$result$fields$id
    df <- data.frame(t(data.frame(1:length(fields), row.names = fields))[0,])
  } else {
    df <- jsonlite::fromJSON(json)$result$records
  }
  return(df)
}

# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(url))
}

# Get ID's
getIds <- function(phrase) {
  url <- paste0("http://data.wprdc.org/api/action/package_search?q=", gsub(" ", "%20", phrase))
  r <- GET(url)
  raw <- content(r, "text")
  df <- jsonlite::fromJSON(raw)$result$results
  tib <- tibble(df$resources) %>%
    unnest() %>%
    filter(format == "CSV")
  final <- df %>%
    select(id, name) %>%
    right_join(tib, by = c("id" = "package_id"))
  
  return(final)
}

# List for Clean Function
# council_list <- selectGet("council_list", selection_conn)
council_list <- c("1: Bobby Wilson", "2: Theresa Kail-Smith", "3: Bruce Kraus", "4: Anthony Coghill", "5: Corey O'Connor", "6: R. Daniel Lavelle", "7: Deb Gross", "8: Erika Strassburger", "9: Rev. Ricky Burgess")

# Council Clean
cleanCouncil <- function(data, upper) {
  upper <- ifelse(missing(upper), FALSE, upper)
  if (upper) {
    data <- transform(data, COUNCIL_DISTRICT = as.factor(mapvalues(COUNCIL_DISTRICT, c(0:9),
                                                                   c(NA, council_list))))
  } else {
    data <- transform(data, council_district = as.factor(mapvalues(council_district, c(0:9),
                                                                   c(NA, council_list))))
  }
  return(data)
}

# DPW Clean
cleanDPW <-function(data, upper) {
  upper <- ifelse(missing(upper), FALSE, upper)
  if (upper) {
    data <- transform(data, PUBLIC_WORKS_DIVISION = as.factor(mapvalues(PUBLIC_WORKS_DIVISION, c(0:6),
                                                                        c( NA, "1: North Side", "2: East End (North)", "3: The Hill, East End (South) & South Side", "4: South Side", "5: West End & South Hills", "6: Downtown, Strip & North Shore"))))
  } else {
    data <- transform(data, public_works_division = as.factor(mapvalues(public_works_division, c(0:6),
                                                                        c( NA, "1: North Side", "2: East End (North)", "3: The Hill, East End (South) & South Side", "4: South Side", "5: West End & South Hills", "6: Downtown, Strip & North Shore"))))
  }
  return(data)
}

# Police Zone Clean
cleanZone <- function(data, upper) {
  upper <- ifelse(missing(upper), FALSE, upper)
  if (upper) {
    data <- transform(data, POLICE_ZONE = as.factor(mapvalues(POLICE_ZONE, c(append( c("OSC"), 0:6)),
                                                              c(NA, NA, "1: North Side", "2: Downtown, Hill & Strip", "3: South Side", "4: East End (South) & South Side", "5: East End (North)", "6: West End & South Hills"))))
  } else {
    data <- transform(data, police_zone = as.factor(mapvalues(police_zone, c(append( c("OSC"), 0:6)),
                                                              c(NA, NA, "1: North Side", "2: Downtown, Hill & Strip", "3: South Side", "4: East End (South) & South Side", "5: East End (North)", "6: West End & South Hills"))))
  }
  return(data)
}

# Function to clean all Geographies
cleanGeo <- function(data, upper) {
  data <- cleanCouncil(data, upper)
  data <- cleanDPW(data, upper)
  data <- cleanZone(data, upper)
  return(data)
}

# Load Boundary Files from Pittsburgh Shems server. This process may cause the error screen to appear before the application UI loads.
# Neighborhoods
load.hoods <- readOGR("https://opendata.arcgis.com/datasets/dbd133a206cc4a3aa915cb28baa60fd4_0.geojson")
# DPW
load.dpw <- readOGR("https://opendata.arcgis.com/datasets/524ecda73d354ca0aa4a0640bd6b8bd5_0.geojson")
load.dpw$PUBLIC_WORKS_DIVISION <- load.dpw$division
load.dpw@data <- cleanDPW(load.dpw@data, TRUE)
# Zone
load.zones <- readOGR("https://opendata.arcgis.com/datasets/230d80a6f1a2479faf501025f10ba903_0.geojson")
load.zones$POLICE_ZONE <- load.zones$zone
load.zones@data <- cleanZone(load.zones@data, TRUE)
# Fire Zone
load.firez <- readOGR("https://opendata.arcgis.com/datasets/da92100723d1400cb7e68753a505d2d3_0.geojson")
# Council
load.council <- readOGR("https://opendata.arcgis.com/datasets/019101970961451890680bcc1862cb68_0.geojson")
load.council$COUNCIL_DISTRICT <- load.council$council
load.council@data <- cleanCouncil(load.council@data, TRUE)

# 311 Input & Icons
request_types <- ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "REQUEST_TYPE")
request_types <- levels(as.factor(request_types$REQUEST_TYPE))
status_types <- c("New", "Open", "Closed")

requests311 <-c("Abandoned Vehicle (parked on street)", "Building Maintenance", "Building Without a Permit", "Drug Enforcement", "Fire Department", "Fire Lane", "Fire Prevention", "Gang Activity", "Graffiti, Documentation", "Graffiti, Removal", "Hydrant - Fire Admin", "Illegal Dumping", "Illegal Parking", "Litter","Noise", "Missed Pick Up", "Panhandling", "Patrol", "Paving Request", "Potholes", "Pruning (city tree)", "Refuse Violations", "Replace/Repair a Sign", "Request New Sign", "Rodent control", "Sidewalk Obstruction", "Sinkhole", "Smoke detectors", "Snow/Ice removal", "Street Cleaning/Sweeping", "Street Light - Repair", "Traffic", "Traffic or Pedestrian Signal, Repair", "Vacant Building", "Weeds/Debris")

departments <- ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "DEPARTMENT")
departments <- levels(as.factor(departments$DEPARTMENT))
origins <- ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "REQUEST_ORIGIN")
origins <- levels(as.factor(origins$REQUEST_ORIGIN))
origins <- levels(as.factor(mapvalues(origins, c("Report2Gov Android", "Report2Gov iOS", "Report2Gov Website"),
                                      c("myBurgh (Android)", "myBurgh (iOS)", "Website"))))

# 311 Selections
icons_311 <- iconList(
  abandoned_vehicle = makeIcon("./icons/311/abandoned_vehicle.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  building_maintenance = makeIcon("./icons/311/building_maintenance.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  building_nopermit = makeIcon("./icons/311/building_nopermit.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  drug_enforcement = makeIcon("./icons/311/drug_enforcement.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  fire_dept = makeIcon("./icons/311/fire_dept.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  fire_lane = makeIcon("./icons/311/fire_truck.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  fire_prevention = makeIcon("./icons/311/fire_ex.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  gang_activity = makeIcon("./icons/311/gang_activity.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  graffiti = makeIcon("./icons/311/graffiti.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 18, popupAnchorY = -48),
  hydrant = makeIcon("./icons/311/fire_hydrant.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  illegal_dumping = makeIcon("./icons/311/illegal_dumping.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  illegal_parking = makeIcon("./icons/311/illegal_parking.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 18, popupAnchorY = -48),
  litter = makeIcon("./icons/311/litter.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 18, popupAnchorY = -48),
  missed_pickup = makeIcon("./icons/311/missed_pickup.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  noise = makeIcon("./icons/311/noise.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  other311 = makeIcon("./icons/311/other311.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  panhandling = makeIcon("./icons/311/panhandling.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  patrol = makeIcon("./icons/311/patrol.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  paving_request = makeIcon("./icons/311/paving_request.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  pothole = makeIcon("./icons/311/pothole.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  pruning = makeIcon("./icons/311/pruning.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  refuse_violation = makeIcon("./icons/311/refuse_violation.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  replace_sign = makeIcon("./icons/311/replace_sign.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  request_sign = makeIcon("./icons/311/request_sign.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  rodent_control = makeIcon("./icons/311/rodent_control.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  sidewalk_obstruction = makeIcon("./icons/311/sidewalk_obstruction.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  sinkhole = makeIcon("./icons/311/sinkhole.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  smoke_detectors = makeIcon("./icons/311/smoke_detector.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  snow_removal = makeIcon("./icons/311/snow_removal.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  street_sweeper = makeIcon("./icons/311/street_sweeper.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  streetlight_repair = makeIcon("./icons/311/streetlight_repair.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  traffic = makeIcon("./icons/311/traffic.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  trafficlight_repair = makeIcon("./icons/311/trafficlight_repair.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  vacant_building = makeIcon("./icons/311/vacant_building.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  weeds_debris = makeIcon("./icons/311/weeds_debris.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34)
)

# Building Code Violations
violations <- as.factor(trimws(ckanSQL("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(SPLIT_PART(%22VIOLATION%22,%27::%27,1))%20FROM%20%224e5374be-1a88-47f7-afee-6a79317019b4%22")$split_part))

inspect_results <- c('Abated','Violations Found','Voided')

# Icons for Building Code Violations
icons_violations <- iconList(
  violations_abated = makeIcon("./icons/PLI/violations_abated.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  violations_found = makeIcon("./icons/PLI/violations_found.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  violations_void = makeIcon("./icons/PLI/violations_void.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34)
)

# Blotter Input & Icons
hierarchies <- as.factor(c("01 Murder", "02 Rape", "03 Robbery", "04 Assault", "05 Burglary", "06 Theft", "07 Vehicle Theft", "08 Arson", "09 Forgery", "10 Simple Assault", "11 Fraud", "12 Embezzlement", "13 Receiving Stolen Prop", "14 Vandalism", "15 Carrying Weapon", "16 Prostitution", "17 Sex Offense", "18 Drug Offense", "19 Gambling", "20 Endangering Children", "21 DUI", "22 Liquor Laws", "23 Public Drunkenness", "24 Disorderly Conduct", "25 Vagrancy", "26 Other"))

offenses <- as.factor(trimws(ckanSQL("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(SPLIT_PART(%22OFFENSES%22,%27%20/%20%27,1))%20FROM%20%22044f2016-1dfd-4ab0-bc1e-065da05fca2e%22")$split_part))

# Icons for Blotter
icons_blotter <- iconList(
  murder = makeIcon("./icons/police/murder.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  rape = makeIcon("./icons/police/rape.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  robbery = makeIcon("./icons/police/robbery.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  assault = makeIcon("./icons/police/assault.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  burglary = makeIcon("./icons/police/burglary.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34), 
  theft = makeIcon("./icons/police/theft.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  vehicle_theft = makeIcon("./icons/police/vehicle_theft.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  arson = makeIcon("./icons/police/arson.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  forgery = makeIcon("./icons/police/forgery.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  simple_assault = makeIcon("./icons/police/simple_assault.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  fraud = makeIcon("./icons/police/fraud.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  embezzlement = makeIcon("./icons/police/embezzlement.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  receiving_stolen_property = makeIcon("./icons/police/receiving_stolen_property.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  vandalism = makeIcon("./icons/police/vandalism.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  carrying_weapon = makeIcon("icons/police/carrying_weapon.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  prostitution = makeIcon("./icons/police/prostitution.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  sex_offense = makeIcon("./icons/police/sex_offense.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  drug_offense = makeIcon("./icons/police/drug_offense.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  gambling = makeIcon("./icons/police/gambling.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  endangering_children = makeIcon("./icons/police/endangering_children.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  DUI = makeIcon("./icons/police/DUI.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  liquor_laws = makeIcon("./icons/police/liquor_laws.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  public_drunkenness = makeIcon("./icons/police/public_drunkeness.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  disorderly_conduct= makeIcon("icons/police/disorderly_conduct.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  vagrancy = makeIcon("./icons/police/vagrancy.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  other = makeIcon("./icons/police/other.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34)
)

# Capital Projects Inputs & Icons
functional_areas <- c("Administration/Sub-Award", "Engineering and Construction", "Facility Improvement", "Neighborhood and Community Development", "Public Safety","Vehicles and Equipment")

icons_cproj <- iconList(
  administration = makeIcon("./icons/omb/administration.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  engineering_construction = makeIcon("./icons/omb/engineering_construction.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  facility_improvement = makeIcon("./icons/omb/facility_improvement.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  neighborhood_development = makeIcon("./icons/omb/community_development.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  public_safety = makeIcon("./icons/omb/public_safety.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  vehicles_equipment = makeIcon("./icons/omb/vehicles_equipment.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34)
)

# Collisions
crash_types <- c("Automobile", "Bicycle", "Bus", "Hit Deer", "Intoxicated Driver", "Motorcycle", "Pedestrian", "Train/Trolley", "Fixed Object")

circumstances_values <- c("LANE_CLOSED", "TAILGATING", "AGGRESSIVE_DRIVING", "SPEEDING_RELATED", "UNLICENSED", "WET_ROAD", "SNOW_SLUSH_ROAD", "ICY_ROAD", "REAR_END", "OVERTURNED", "CELL_PHONE", "VEHICLE_TOWED", "RUNNING_RED_LT", "RUNNING_STOP_SIGN", "FATIGUE_ASLEEP", "WORK_ZONE", "DISTRACTED", "SCH_BUS_IND")

circumstances_types <- c("Lane Closed", "Tailgating", "Aggressive Driving", "Speeding Related", "Unlicensed", "Wet Road", "Snow/Slushy Road", "Icy Road", "Rear Ended", "Overturned Vehicle", "Cellphone Related", "Vehicle Towed", "Ran Red Light", "Ran Stop Sign", "Fatigued/Asleep", "Work Zone", "Distracted", "School Bus")

icons_crashes <- iconList(
  crash = makeIcon("./icons/crashes/crash.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  crash_bike = makeIcon("./icons/crashes/crash_bike.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  crash_bus = makeIcon("./icons/crashes/crash_bus.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  crash_deer = makeIcon("./icons/crashes/crash_deer.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  crash_dui = makeIcon("./icons/crashes/crash_dui.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  crash_motorcycle = makeIcon("./icons/crashes/crash_motorcycle.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  crash_pedestrian = makeIcon("./icons/crashes/crash_pedestrian.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  crash_trolley = makeIcon("./icons/crashes/crash_trolley.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  crash_single = makeIcon("./icons/crashes/crash_single.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34)
)

# Fires
fire_desc <- ckanSQL("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22incident_type%22||%20%27%20%27||%20%22type_description%22)%20from%20%228d76ac6b-5ae8-4428-82a4-043130d17b02%22")
fire_desc <- levels(as.factor(fire_desc$X.column.))

# Icons for Fires
icons_fires <- iconList(
  fire = makeIcon("./icons/fire/fire.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  fire_brush = makeIcon("./icons/fire/fire_brush.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  fire_building = makeIcon("./icons/fire/fire_building.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  fire_cooking = makeIcon("./icons/fire/fire_cooking.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  fire_trash = makeIcon("./icons/fire/fire_trash.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  fire_vehicle = makeIcon("./icons/fire/fire_vehicle.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34)
)

# ROW Stuff
row_types <- levels(as.factor(ckanSQL(paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22type%22)FROM%20%22cc17ee69-b4c8-4b0c-8059-23af341c9214%22%20WHERE%20%22open_date%22>%27", Sys.Date() - 365, "%27"))$type))


# ROW Icons
# Icons for Fires
icons_row <- iconList(
  barricade = makeIcon("./icons/domi/barricade.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  bridge_permit = makeIcon("./icons/domi/bridge_permit.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  cafe = makeIcon("./icons/domi/cafe.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  curb_cut = makeIcon("./icons/domi/curb_cut.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  dumpster = makeIcon("./icons/domi/dumpster.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  machinery = makeIcon("./icons/domi/machinery.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  opening_permit = makeIcon("./icons/domi/opening_permit.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  sidewalk_repair = makeIcon("./icons/domi/sidewalk_repair.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  tele_pole = makeIcon("./icons/domi/tele_pole.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  traffic_obstruction = makeIcon("./icons/domi/traffic_obstruction2.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  valet_parking = makeIcon("./icons/domi/valet_parking.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34),
  other = makeIcon("./icons/domi/other.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34)
)

# this_year
this_year <- format(Sys.Date(), format="%Y")
last_year <- as.numeric(this_year) -  1

# Presidential Years
presidential_years <- seq(2016, 3000, 4)

# Election Day
nov <- ymd(as.Date(paste0(this_year, "-11-01")))
dow <- sapply(seq(0,7),function(x) format(nov+x, "%a"))
eDay <- nov + which(dow=="Mon")[1]

# Primary Day
if (this_year %in% presidential_years) {
  april <- ymd(as.Date(paste0(this_year, "-04-01")))
  dow <- sapply(seq(0,7),function(x) format(april+x, "%a"))
  firstTuesday <- april + which(dow=="Tue")[1]
  # In Presidential Years PA Primaries are on the 4th Tuesday of April
  pDay <- firstTuesday + 20
} else {
  may <- ymd(as.Date(paste0(this_year, "-05-01")))
  dow <- sapply(seq(0,7),function(x) format(may+x, "%a"))
  firstTuesday <- may + which(dow=="Tue")[1]
  # In Non-Presidential Years PA Primaries are on the 3rd Tuesay of May
  pDay <- firstTuesday + 13
}

icons_egg <- iconList(
  halloween = makeIcon("./icons/egg/pirate.png", iconAnchorX = 31, iconAnchorY = 12.5, popupAnchorX = 0, popupAnchorY = -12.5, iconWidth = 72),
  election = makeIcon("./icons/egg/vote.png", iconAnchorX = 31, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13, iconWidth = 72),
  thanksgiving = makeIcon("./icons/egg/thanksgiving.png", iconAnchorX = 31, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13, iconWidth = 72),
  snow = makeIcon("./icons/egg/snowboard.png", iconAnchorX = 31, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13, iconWidth = 72),
  new_year = makeIcon("./icons/egg/new_year.png", iconAnchorX = 31, iconAnchorY = 13.5, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72),
  valentine = makeIcon("./icons/egg/valentine.png", iconAnchorX = 31, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72),
  patrick = makeIcon("./icons/egg/patrick.png", iconAnchorX = 31, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72),
  easter_egg = makeIcon("./icons/egg/easter.png", iconAnchorX = 31, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72),
  summer = makeIcon("./icons/egg/summer.png", iconAnchorX = 31, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72),
  july_4 = makeIcon("./icons/egg/july_4.png", iconAnchorX = 31, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72)      
)

# Non-Traffic Citations Icons
icons_citations <- iconList(
  citation = makeIcon("./icons/police/nontraffic_citation.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34)
)

# Arrests Icon
icons_arrests <- iconList(
  arrest_made = makeIcon("./icons/police/arrest_made.png", iconAnchorX = 18, iconAnchorY = 41, popupAnchorX = 1, popupAnchorY = -34)
)

# UI for application
ui <- ui <- function(request) {
      navbarPage(id = "navTab",
                 windowTitle = "Burgh's Eye View Points",
                 selected = "Points",
                 collapsible = TRUE,
                 fluid = TRUE,
                 theme = shinytheme("flatly"),
                 title = HTML('<img src="burghs_eyeview_logo_small.png" alt="Burghs Eye View" height="85%">'),
                 position = "static-top",
                 tabPanel('Points', class = "Points", value = "Points", id = "Points",
                          # Run script to determine if user is loading from a mobile device
                          tags$script(getWidth),
                          # Google Tag Manager Script to Head
                          tags$head(includeScript("tag-manager-head.js")),
                          # Notification Centered and Color Fix
                          tags$head(tags$style(HTML(type = "text/css", 
                                               ".shiny-notification {
                                                    position: fixed;
                                                    background: #2c3e50;
                                                    top: calc(50%);;
                                                    left: calc(50%);;
                                                    width: calc(25%);;
                                                    min-width: 200px;
                                                    transform: translate(-50%, 0);}"))),
                          tags$head(tags$style(HTML(".shiny-notification-close { color: white; }"))),
                          # Set favicon
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
                          # Add Google Analytics Script to page
                          tags$head(includeScript("google-analytics.js")),
                          # Add Tag Manager Script to Body
                          tags$body(tags$noscript(tags$iframe(src='https://www.googletagmanager.com/ns.html?id=GTM-TCTCQVD', height = 0, width = 0, style="display:none;visibility:hidden"))),
                          # Layout CSS
                          tags$style(type="text/css", ".shiny-output-error { visibility: hidden;}
                                                       .shiny-output-error:before { visibility: hidden; }
                                                       .container-fluid { padding:0; }
                                                       .navbar-header {margin:auto;}
                                                       .navbar-static-top {margin-bottom:0;}
                                                       .navbar-brand {height:60px; 
                                                                      padding:0;}
                                                       .navbar {border-right-width: 20px;
                                                                border-left-width: 65px;}
                                                       .leaflet-popup-content {overflow-y: auto; 
                                                                               max-height: 400px !important;}
                                                       .form-group {margin-bottom: 0px;}
                                                       @media only screen and (min-width: 600px) {
                                                         #map {height: calc(100vh - 55px) !important; 
                                                               z-index: 0;}
                                                         #tPanel {opacity: 0.88;
                                                                  max-height: calc(100vh - 90px);}
                                                         .btn.collapsed {display: none;}
                                                         #mobile {display: initial;}
                                                         #outer {position: relative; padding-bottom: 0px;}
                                                         #search {width: 275px;}
                                                       }
                                                       @media only screen and (max-width: 600px) {
                                                         #map {height: calc(100vh - 115px) !important;
                                                               position: absolute !important;
                                                               top: 60px;
                                                               z-index: 0;}
                                                         .mapBack {height: calc(100vh);}
                                                         #aPanel {top: 60px !important; 
                                                                  left: 0px !important; 
                                                                  width: 100% !important;}
                                                         .assetsBack {position: absolute;
                                                                      width: 100%;
                                                                      z-index: -1;
                                                                      left: 0px;
                                                                      top: 55px;}
                                                         #tPanel {margin-bottom:0px; 
                                                                  padding:0px !important; 
                                                                  overflow-y:scroll !important; 
                                                                  max-height: calc(100vh - 65) !important; 
                                                                  min-height: 55px !important; 
                                                                  padding-left: 10px !important; 
                                                                  padding-right: 10px !important;
                                                                  border: none;
                                                                  width: 100%;
                                                                  opacity: 1 !important;}
                                                         #search {width: calc(100vw - 85px) !important; margin-left:10px !important;}
                                                         #outer {margin-top: 5px !important; position: absolute;}
                                                         .btn.collapsed {display: in-line !important;}
                                                       }"),
                          # Generate Map
                          div(class="mapBack", style='position: absolute;
                                                      background-image: url("loading.png");
                                                      background-repeat: no-repeat;
                                                      background-position: center;
                                                      background-size: contain;
                                                      width: 100%;
                                                      z-index: -1;
                                                      left: 0px;
                                                      top: 55px', 
                              leafletOutput("map")),
        absolutePanel(
          # Input panel for Desktops (alpha'd)
          top = 70, left = 50, width = '325px', style = "z-index: 1000", id = "aPanel",
          wellPanel(id = "tPanel", style = "overflow-y:auto; min-height: 65px;",
                    HTML('<div id="outer" style="z-index: 9; background-color:#ecf0f1;">'),
                    div(style="display:inline-block;", 
                        textInput("search", 
                                  value = ifelse(eDay == Sys.Date() | pDay == Sys.Date(), "Vote!", ""),
                                  label = NULL, 
                                  placeholder = "Search")),
                    tags$style(style="text/css", chartr0('#tPanel #outer .btn .fa:before { content: "\\f056";  }
                                                         #tPanel #outer .btn.collapsed .fa:before { content: "\\f055";  }')),
                    HTML('<button class="btn collapsed" data-toggle="collapse" data-target="#mobile" stye="display: block;"><i class="fa fa-search-plus" aria-hidden="true"></i></button></div>
                         <div id="mobile" class="collapse" style="margin-top:55px;">'),
                    HTML('<small style="font-size:11px;margin-left:3px">Locations are not exact. (See &rsquo;About&rsquo; for details.)</small><br><br>'),
                    dateRangeInput("dates",
                                   label = NULL,
                                   start = Sys.Date()-10,
                                   end = Sys.Date(),
                                   min = as.Date("2004-01-01"),
                                   max = Sys.Date() + 30,
                                   startview = "day"),
                    tags$br(),
                    actionButton("heatVision",
                                 label = "Enable Heat Map",
                                 icon = icon("eye")),
                    HTML('<font color="#F47B25">'),
                    checkboxInput("toggle311",
                                  label = "311 Requests",
                                  value = TRUE),
                    HTML('</font>'),
                    selectInput("req.type",
                                label = NULL,
                                c(`Request Type`='', request_types),
                                multiple = TRUE,
                                selectize = TRUE),
                    selectInput("status_type",
                                label = NULL,
                                c(`Request Status`='', status_types),
                                multiple = TRUE,
                                selectize=TRUE),
                    selectInput("dept_select",
                                label = NULL,
                                c(`Department`='', departments),
                                multiple = TRUE,
                                selectize=TRUE),
                    selectInput("origin_select",
                                label = NULL,
                                c(`Request Origin`='', origins),
                                multiple = TRUE,
                                selectize=TRUE),
                    HTML('<font color="#3663AD">'),
                    checkboxInput("toggleBlotter",
                                  label = "Police Blotter",
                                  value= TRUE),
                    HTML('</font>'),
                    selectInput("hier",
                                label = NULL,
                                c(`Hierarchy`='', levels(hierarchies)),
                                multiple = TRUE,
                                selectize = TRUE),
                    selectInput("offense_select",
                                label = NULL,
                                c(`Offense Type`='', levels(offenses)),
                                multiple = TRUE,
                                selectize = TRUE),
                    HTML('<font color="#474545">'),
                    checkboxInput("toggleArrests",
                                  label = "Arrests",
                                  value = TRUE),
                    HTML('</font>'),
                    HTML('<font color="#ED2393">'),
                    checkboxInput("toggleCitations",
                                  label = "Non-Traffic Citations",
                                  value = TRUE),
                    HTML('</font>'),
                    HTML('<font color="#BA1924">'),
                    checkboxInput("toggleFires",
                                  label = "Fire Incidents",
                                  value = TRUE),
                    HTML('</font>'),
                    selectInput("fire_desc_select",
                                label = NULL,
                                c(`Fire Type` = '', fire_desc),
                                multiple = TRUE,
                                selectize = TRUE),
                    HTML('<font color="#0B9444">'),
                    checkboxInput("toggleViolations",
                                  label = "Code Violations", 
                                  value = TRUE),
                    HTML('</font>'),
                    selectInput("violation_select",
                                label = NULL,
                                c(`Violation`='', levels(violations)),
                                multiple = TRUE,
                                selectize=TRUE),
                    selectInput("result_select",
                                label = NULL,
                                c(`Inspection Result`='', inspect_results),
                                multiple = TRUE,
                                selectize=TRUE),
                    HTML('<font color="#b9a5c1">'),
                    checkboxInput("toggleCproj",
                                  label = "Capital Projects",
                                  value = TRUE),
                    HTML('</font>'),
                    selectInput("funcarea_select",
                                label = NULL,
                                c(`Functional Area`='', functional_areas),
                                multiple = TRUE,
                                selectize=TRUE),
                    HTML('<font color="#2F9997">'),
                    checkboxInput("toggleROW",
                                  label = "Right of Way Permits",
                                  value = FALSE),
                    HTML('</font>'),
                    selectInput("row_select",
                                label = NULL,
                                c(`ROW Permit Type`='', row_types),
                                multiple = TRUE,
                                selectize=TRUE),
                    HTML('<font color="#F9C13D">'),
                    checkboxInput("toggleCrashes",
                                  label = "Traffic Collisions",
                                  value = FALSE),
                    HTML('</font>'),
                    selectInput("crash_select",
                                label = NULL,
                                c(`Collision Type`='', crash_types),
                                multiple = TRUE,
                                selectize=TRUE),
                    selectInput("circumstances_select",
                                label = NULL,
                                c(`Special Circumstances`='', circumstances_types),
                                multiple = TRUE,
                                selectize = TRUE),
                    selectInput("dow_select",
                                label = NULL,
                                c(`Day of the Week` = '', c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                                multiple = TRUE,
                                selectize = TRUE),
                    sliderInput("times",
                                label = "Collision Time (24-hour clock)",
                                min = 0,
                                max = 24,
                                value = c(0,24),
                                step = 1),
                    selectInput("basemap_select",
                                label = "Basemap",
                                choices = c(`OSM Mapnik` = "OpenStreetMap.Mapnik", `Code for Pittsburgh` = "mapStack", `OSM France` = "OpenStreetMap.France", `OSM Humanitarian` = "OpenStreetMap.HOT", `Stamen Toner` = "Stamen.Toner", `Esri Satellite` = "Esri.WorldImagery", Esri = "Esri.WorldStreetMap", `OSM Dark Matter` = "CartoDB.DarkMatter", `OSM Positron` = "CartoDB.Positron"),
                                selected = "OpenStreetMap.Mapnik"),
                    selectInput("filter_select",
                                "Filter by Area",
                                c(`Area Type`='', c("Neighborhood", "Council District", "Police Zone", "Fire Zone", "Public Works Division")),
                                selectize = TRUE,
                                selected = ""),
                    # Conditional Filter Panels
                    conditionalPanel("input.filter_select == 'Neighborhood'",
                                     selectInput("hood_select",
                                                 label = NULL,
                                                 c(`Neighborhood`='', levels(load.hoods$hood)),
                                                 multiple = TRUE,
                                                 selectize=TRUE)
                    ),
                    conditionalPanel("input.filter_select == 'Public Works Division'",
                                     selectInput("DPW_select",
                                                 label = NULL,
                                                 c(`Public Works Division`='', levels(load.dpw$PUBLIC_WORKS_DIVISION)),
                                                 multiple = TRUE,
                                                 selectize=TRUE)
                    ),
                    conditionalPanel("input.filter_select == 'Police Zone'",
                                     selectInput("zone_select",
                                                 label = NULL,
                                                 c(`Police Zone`='', levels(load.zones$POLICE_ZONE)),
                                                 multiple = TRUE,
                                                 selectize=TRUE)
                    ),
                    conditionalPanel("input.filter_select == 'Council District'",
                                     selectInput("council_select",
                                                 label = NULL,
                                                 c(`Council District`='', levels(load.council$COUNCIL_DISTRICT)),
                                                 multiple = TRUE,
                                                 selectize=TRUE)
                    ),
                    conditionalPanel("input.filter_select == 'Fire Zone'",
                                     selectInput("firez_select",
                                                 label = NULL,
                                                 c(`Fire Zone`='', levels(load.firez$dist_zone)),
                                                 multiple = TRUE,
                                                 selectize=TRUE)
                    ),
                    HTML("</div>")
                    )
                  )
               ),
               tabPanel(a("Places", href="https://pittsburghpa.shinyapps.io/BurghsEyeViewPlaces/", style = "padding-top: 0px;
                          padding-bottom: 0px; bottom: 19; top: -19; bottom: 19px")),
               tabPanel(a("Parcels", href="https://pittsburghpa.shinyapps.io/BurghsEyeViewParcels/", style = "padding-top: 0px; padding-bottom: 0px; bottom: 19; top: -19; bottom: 19px")),
               tabPanel('Data: Points', class = "Data: Points", value = "Data: Points",
                        # Select Dataset for Export
                        inputPanel(
                          selectInput("report_select", 
                                      tagList(shiny::icon("map-marker"), "Select Layer:"),
                                      choices = c("311 Requests", "Arrests", "Blotter", "Capital Projects", "Code Violations", "Collisions", "Fire Incidents", "Non-Traffic Citations", "Right of Way Permits"),
                                      selected= "311 Requests"),
                          # Define Button Position
                          uiOutput("buttonStyle")
                        ),
                        # Clean up the Data Table CSS
                        tags$style(type = "text/css", ".dataTables_length {margin-left: 10px;}"),
                        tags$style(type = "text/css", ".dataTables_info {margin-left: 10px;}"),
                        tags$style(type = "text/css", ".dataTables_filter {margin-right: 5px;}"),
                        dataTableOutput("report.table")
               ),
               tabPanel('About', class = "About", value = "About",
                        includeHTML('about.html'),
                        # Twitter Button
                        tags$script(HTML("var header = $('.navbar > .container-fluid > .navbar-collapse');
                                         header.append('<div class =\"twit\" style=\"float:right;margin-top: 15px;\"><a href=\"https://twitter.com/share\" class=\"twitter-share-button\" align=\"middle\" data-url=\"data.pittsburghpa.gov/BurghsEyeView\" data-text=\"Check out Burgh&#39;s Eye View! A new tool to view city data in Pittsburgh: https://goo.gl/z4cZ30\" data-size=\"large\">Tweet</a></div>');
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
              # Facebook Button
              HTML('<div id="fb-root"></div>'),
              tags$script(HTML("(function(d, s, id) {
                               var js, fjs = d.getElementsByTagName(s)[0];
                               if (d.getElementById(id)) return;
                               js = d.createElement(s); js.id = id;
                               js.src = \"//connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.8\";
                               fjs.parentNode.insertBefore(js, fjs);
                               }(document, 'script', 'facebook-jssdk'));")),
              tags$script(HTML('header.append(\'<div class="fb-share-button" style="float:right;margin-top: 15px;margin-right: 5px;" data-href="http://pittsburghpa.shinyapps.io/BurghsEyeView/?utm_source=facebook_button&amp;utm_campaign=facebook_button&amp;utm_medium=facebook%2Fsocial\" data-layout="button" data-size="large" data-mobile-iframe="true"><a class="fb-xfbml-parse-ignore" target="_blank" href="https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Fpittsburghpa.shinyapps.io%2FBurghsEyeView%2F%23utm_source%3Dfacebook_button%26utm_campaign%3Dfacebook_button%26utm_medium%3Dfacebook%252Fsocial&amp;src=sdkpreparse">Share</a></div>\');
                               console.log(header)'))
              )
             )
}

# Define server
server <- shinyServer(function(input, output, session) {
  # Observe changes to the dates function, if not default include in bookmark/url
  observeEvent(input$dates,  {
    if (input$dates[1] != Sys.Date()-10 | input$dates[2] != Sys.Date()){
      setBookmarkExclude(c("GetScreenWidth", "report.table_rows_all", "report.table_rows_current"))
    } else {
      setBookmarkExclude(c("GetScreenWidth", "dates", "report.table_rows_all", "report.table_rows_current"))
    }
  })
  # Crashes Event Message
  observeEvent(input$toggleCrashes, {
    if (input$toggleCrashes & format(input$dates[1], "%Y") == this_year) {
      showNotification(HTML(paste0('<center><font color="white">Because Traffic Collision are reported by the State of Pennslyvania annually the Date Range has been set to last year.</font></center>')), type = "message", duration = 10, id = "crashmessage")
      updateDateRangeInput(session = session,
                           inputId = "dates",
                           start = as.Date(input$dates[1]) - 365,
                           end = as.Date(input$dates[2]) - 365,
                           min = as.Date("2004-01-01"),
                           max = Sys.Date())
    }
  })
  # Night Vision Button Text
  observeEvent(input$heatVision, {
    if (input$heatVision %% 2){
      updateActionButton(session = session,
                         inputId = "heatVision",
                         label = "Disable Heat Map",
                         icon = icon("low-vision"))
      updateSelectInput(session = session,
                        inputId = "basemap_select",
                        label = "Basemap",
                        choices = c(`OSM Mapnik` = "OpenStreetMap.Mapnik", `Code for Pittsburgh` = "mapStack", `OSM France` = "OpenStreetMap.France", `OSM Humanitarian` = "OpenStreetMap.HOT", `Stamen Toner` = "Stamen.Toner", `Esri Satellite` = "Esri.WorldImagery", Esri = "Esri.WorldStreetMap", `OSM Dark Matter` = "CartoDB.DarkMatter", `OSM Positron` = "CartoDB.Positron"),
                        selected = "CartoDB.DarkMatter")
    } else {
      updateActionButton(session = session,
                         inputId = "heatVision",
                         label = "Enable Heat Map",
                         icon = icon("eye"))
      updateSelectInput(session = session,
                        inputId = "basemap_select",
                        label = "Basemap",
                        choices = c(`OSM Mapnik` = "OpenStreetMap.Mapnik", `Code for Pittsburgh` = "mapStack", `OSM France` = "OpenStreetMap.France", `OSM Humanitarian` = "OpenStreetMap.HOT", `Stamen Toner` = "Stamen.Toner", `Esri Satellite` = "Esri.WorldImagery", Esri = "Esri.WorldStreetMap", `OSM Dark Matter` = "CartoDB.DarkMatter", `OSM Positron` = "CartoDB.Positron"),
                        selected = "OpenStreetMap.Mapnik")
    }
  })
  # Tracking Info
  sessionStart <- as.character(Sys.time())
  names(sessionStart) <- "sessionStart"
  sessionID <- paste(stri_rand_strings(1, 5), gsub("\\.", "-", sessionStart) , "points", sep="-")
  userName <- Sys.getenv("SHINYPROXY_USERNAME")
  names(userName) <- "userName"
  names(sessionID) <- "sessionID"
  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  # Update page URL
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  output$buttonStyle <- renderUI({
    # Generate search & layer panel & Map (checks for mobile devices)
    if (as.numeric(input$GetScreenWidth) > 800) {
      div(style="margin-top: 20px", downloadButton("downloadData", paste("Export" , input$report_select), class = "dlBut"))
    } else {
      div(downloadButton("downloadData", paste("Export" , input$report_select), class = "dlBut"))
    }
  })
  # Boundary Data
  # Neighborhoods
  hoodsInput <- reactive({
    hoods <- load.hoods
    
    if (length(input$hood_select) > 0){
      hoods <- hoods[hoods$hood %in% input$hood_select,]
    }
    
    hoods
  })
  # Council District
  councilInput <- reactive({
    council <- load.council
    
    if (length(input$council_select) > 0){
      council <- council[council$COUNCIL_DISTRICT %in% input$council_select,]
    }
    
    council
  })
  # DPW Divisions
  dpwInput <- reactive({
    dpw <- load.dpw
    
    if (length(input$DPW_select) > 0){
      dpw <- dpw[dpw$PUBLIC_WORKS_DIVISION %in% input$DPW_select,]
    }
    
    dpw
  })
  # Police Zones
  zonesInput <- reactive({
    zones <- load.zones
    
    if (length(input$zone_select) > 0){
      zones <- zones[zones$POLICE_ZONE %in% input$zone_select,]
    }
    
    zones
  })
  firezInput <- reactive({
    firez <- load.firez
    
    if (length(input$firez_select) > 0){
      firez <- firez[firez$dist_zone %in% input$firez_select,]
    }
    
    firez
  })
  # Point Data
  # Load Right of Way Data
  rowLoad <- reactive({
    query <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT*%20FROM%20%22cc17ee69-b4c8-4b0c-8059-23af341c9214%22%20WHERE%20((%22from_date%22%20BETWEEN%20%27", input$dates[1], "%27%20AND%20%27", input$dates[2], "%27)%20OR%20(%22to_date%22%20BETWEEN%20%27",  input$dates[1], "%27%20AND%20%27", input$dates[2], "%27)%20OR%20(%22from_date%22<=%27", input$dates[1], "%27%20AND%20%22to_date%22>=%27", input$dates[2], "%27)%20OR%20(%22restoration_date%22%20BETWEEN%20%27", input$dates[1], "%27%20AND%20%27", input$dates[2], "%27))%20AND%20%22open_date%22>=%27", as.Date(input$dates[1]) - 365, "%27")
    
    row <- ckanSQL(query) %>%
      mutate(icon = as.factor(case_when(type == "Barricade Permit" ~ "barricade",
                                        type == "Annual Bridge Permit" ~ "bridge_permit",
                                        type == "Sidewalk Cafe Permit" ~ "cafe",
                                        grepl("curb cut", type, ignore.case = T) ~ "curb_cut",
                                        grepl("dumpster", type, ignore.case = T) ~ "dumpster",
                                        grepl("machinery", type, ignore.case = T) ~ "machinery",
                                        type == "Opening Permit" ~ "opening_permit",
                                        grepl("sidewalk repair", type, ignore.case = T) ~ "sidewalk_repair",
                                        type == "Pole Permit" ~ "tele_pole",
                                        type == "Traffic Obstruction Permit" ~ "traffic_obstruction",
                                        type == "Valet Parking Permit" ~ "valet_parking",
                                        TRUE ~ "other")),
             business_name = ifelse(business_name == "NA", "", business_name),
             license_type = ifelse(license_type == "NA", "", license_type),
             to_street = ifelse(to_street == "NA", "", to_street),
             from_street = ifelse(from_street == "NA", "", from_street),
             map_lat = case_when(!is.na(from_lat) ~ from_lat,
                                 !is.na(to_lat) ~ to_lat,
                                 TRUE ~ address_lat),
             map_lon = case_when(!is.na(from_lon) ~ from_lon,
                                 !is.na(to_lon) ~ to_lon,
                                 TRUE ~ address_lon)) %>%
      select(-description, -X_full_text)
    
    return(row)
  })
  rowInput <- reactive({
    row <- rowLoad()
    
    # ROW Filters
    if (length(input$row_select) > 0){
      row <- row[row$type %in% input$row_select,]
    }
    
    # Geographic Filters
    if (length(input$zone_select) > 0 & input$filter_select == "Police Zone") {
      row <- row[row$police_zone %in% input$zone_select,]
    } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
      row <- row[row$neighborhood %in% input$hood_select,]
    } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
      row <- row[row$public_works_division %in% input$DPW_select,]
    } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
      row <- row[row$council_district %in% input$council_select,]
    } else if (length(input$firez_select) > 0 & input$filter_select == "Fire Zone") {
      row <- row[row$fire_zone %in% input$firez_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) && input$search != "") {
      row <- row[apply(row, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(row)
  })
  # Crash Data
  crashesLoad <- reactive({
    crashes <- ckanQueryCrashes(input$dates[1], input$dates[2])
    
    # Clean
    crashes <- subset(crashes, !is.na(DEC_LONG) & !is.na(DEC_LAT))
    crashes$DEC_LONG <- as.numeric(crashes$DEC_LONG)
    crashes$DEC_LAT <- as.numeric(crashes$DEC_LAT)
    crashes <- subset(crashes, DEC_LONG > -80.242767 & DEC_LONG < -79.660492 & DEC_LAT < 40.591014 & DEC_LAT > 40.266428)
    
    return(crashes)
  })
  crashesInput <- reactive({
    # Load Crashes
    crashes <- crashesLoad()
    
    if (length(input$circumstances_select) > 0) {
      cols <- circumstances_values[which(circumstances_types %in% input$circumstances_select)]
      
      count <- 1
      for (i in cols) {
        temp <- crashes[c(crashes[i] >= 1),]
        #Create DF
        if (count == 1) {
          count <- 2
          crashes <- temp
        } else {
          #Bind DF
          crashes <- rbind(crashes, temp)
        }
      }
    }
    
    # Icons
    if (nrow(crashes) > 0){
      crashes$icon <- as.factor(case_when(
        crashes$BICYCLE == "1" ~ "crash_bike",
        crashes$BUS_COUNT >= 1 ~ "crash_bus",
        crashes$MOTORCYCLE == "1" ~ "crash_motorcycle",
        crashes$PEDESTRIAN == "1" ~ "crash_pedestrian",
        crashes$ALCOHOL_RELATED == "1" | crashes$DRUGGED_DRIVER == "1" ~ "crash_dui",
        crashes$TRAIN_TROLLEY == "1" ~ "crash_trolley",
        crashes$HIT_DEER == "1" ~ "crash_deer",
        crashes$HIT_FIXED_OBJECT == "1" | crashes$HIT_POLE  == "1" | crashes$HIT_GDRAIL  == "1" | crashes$HIT_BARRIER  == "1" | crashes$HIT_TREE_SHRUB == "1" | crashes$HIT_PARKED_VEHICLE  == "1" | crashes$HIT_GDRAIL_END == "1" ~ "crash_single",
        TRUE ~ "crash"))
      
      crashes <- transform(crashes, type = as.factor(mapvalues(icon, c("crash", "crash_bike", "crash_bus", "crash_deer", "crash_dui", "crash_motorcycle", "crash_pedestrian", "crash_trolley", "crash_single"),
                                                               crash_types)))
      # Type Select
      if (length(input$crash_select) > 0){
        crashes <- crashes[crashes$type %in% input$crash_select,]
      }
      if (nrow(crashes) > 0) {
        # Clean
        crashes$CRASH_MONTH <- str_pad(as.character(crashes$CRASH_MONTH), 2, pad = "0")
        crashes$time <- str_pad(crashes$TIME_OF_DAY, 4, pad = "0")
        crashes$date <- paste0(crashes$CRASH_YEAR, crashes$CRASH_MONTH, "01")
        crashes$date_time <- as.POSIXct(paste(crashes$date, crashes$time), format = "%Y%m%d %H%M")
        crashes$time <- format(crashes$date_time, "%H:%M")
        crashes$hour <- as.integer(format(crashes$date_time, "%H"))
        crashes$date <- format(as.Date(crashes$date, format = "%Y%m%d"), "%B %Y")
        crashes$day <- as.factor(case_when(
          crashes$DAY_OF_WEEK == "1" ~ "Sunday",
          crashes$DAY_OF_WEEK == "2" ~ "Monday",
          crashes$DAY_OF_WEEK == "3" ~ "Tuesday",
          crashes$DAY_OF_WEEK == "4" ~ "Wednesday",
          crashes$DAY_OF_WEEK == "5" ~ "Thursday",
          crashes$DAY_OF_WEEK == "6" ~ "Friday",
          crashes$DAY_OF_WEEK == "7" ~ "Saturday"
        ))
        
        crashes <- subset(crashes, time >= input$times[1] & time <= input$times[2])
        
        # Day of the Week Filter
        if (length(input$dow_select) > 0) {
          crashes <- crashes[crashes$day %in% input$dow_select,]
        }
        
        # Spatial for filtering
        coords <- cbind(as.numeric(crashes$DEC_LONG), as.numeric(crashes$DEC_LAT))
        points <- SpatialPoints(coords)
        crashes_sp <- SpatialPointsDataFrame(points, crashes)
        proj4string(crashes_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        
        # Geographic Filters
        if (length(input$zone_select) > 0 & input$filter_select == "Police Zone") {
          crashes_sp$POLICE_ZONE <- sp::over(crashes_sp, load.zones)$POLICE_ZONE
          crashes_sp <- crashes_sp[crashes_sp$POLICE_ZONE %in% input$zone_select,]
        } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
          crashes_sp$hood <- sp::over(crashes_sp, load.hoods)$hood
          crashes_sp <- crashes_sp[crashes_sp$hood %in% input$hood_select,]
        } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
          crashes_sp$PUBLIC_WORKS_DIVISION <- sp::over(crashes_sp, load.dpw)$PUBLIC_WORKS_DIVISION
          crashes_sp <- crashes_sp[crashes_sp$PUBLIC_WORKS_DIVISION %in% input$DPW_select,]
        } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
          crashes_sp$COUNCIL_DISTRICT <- sp::over(crashes_sp, load.council)$COUNCIL_DISTRICT
          crashes_sp <- crashes_sp[crashes_sp$COUNCIL_DISTRICT %in% input$council_select,]
        } else if (length(input$firez_select) > 0 & input$filter_select == "Fire Zone") {
          crashes_sp$firez <- sp::over(crashes_sp, load.firez)$dist_zone
          crashes_sp <- crashes_sp[crashes_sp$firez %in% input$firez_select,]
        }
        crashes <- crashes_sp
      } else {
        crashes <- read.table(text = "", col.names =  c(names(crashes), c("date", "day", "time")))
      }
    } else {
      crashes <- read.table(text = "", col.names =  c(names(crashes), c("type", "date", "day", "time")))
    }
    
    return(crashes)
  })
  dat311Load <- reactive({
    dat311 <- ckanQueryDates("76fda9d0-69be-4dd5-8108-0de7907fc5a4", input$dates[1], input$dates[2], "CREATED_ON")
    
    
    # Date cleaning when there's data
    if (nrow(dat311) > 0) {
      dat311$CREATED_ON <- as.POSIXct(dat311$CREATED_ON, format = '%Y-%m-%dT%H:%M:%S', tz = "EST")
      dat311$date <- as.Date(dat311$CREATED_ON)
    } else {
      dat311 <- dat311 %>%
        mutate(date = CREATED_ON)
    }
    
    # Clean Geographies
    dat311 <- subset(dat311, select = -REQUEST_ID)
    dat311 <- cleanGeo(dat311, TRUE)
    dat311$icon <- as.character(dat311$REQUEST_TYPE)
    dat311$REQUEST_TYPE <- ifelse(dat311$REQUEST_TYPE == "Potholes - 4th Div", "Potholes", dat311$REQUEST_TYPE)
    
    # Clean Status
    dat311$STATUS <- ifelse(dat311$STATUS == 0, "New", dat311$STATUS)
    dat311$STATUS <- ifelse(dat311$STATUS == 3, "Open", dat311$STATUS)
    dat311$STATUS <- ifelse(dat311$STATUS == 1, "Closed", dat311$STATUS)
    
    # Set Icon to Other
    dat311$icon <- ifelse(dat311$icon %in% requests311, dat311$icon, "Other")
    dat311$icon <- as.factor(dat311$icon)
    dat311$REQUEST_TYPE <- as.factor(dat311$REQUEST_TYPE)
    dat311 <- transform(dat311, icon = as.factor(mapvalues(icon, c("Abandoned Vehicle (parked on street)", "Building Maintenance", "Building Without a Permit", "Drug Enforcement", "Fire Department", "Fire Lane", "Fire Prevention", "Gang Activity", "Graffiti, Documentation", "Graffiti, Removal", "Hydrant - Fire Admin", "Illegal Dumping", "Illegal Parking", "Litter","Noise", "Other", "Missed Pick Up", "Panhandling", "Patrol", "Paving Request", "Potholes", "Pruning (city tree)", "Refuse Violations", "Replace/Repair a Sign", "Request New Sign", "Rodent control", "Sidewalk Obstruction", "Sinkhole", "Smoke detectors", "Snow/Ice removal", "Street Cleaning/Sweeping", "Street Light - Repair", "Traffic", "Traffic or Pedestrian Signal, Repair", "Vacant Building", "Weeds/Debris"),
                                                           c("abandoned_vehicle", "building_maintenance", "building_nopermit", "drug_enforcement", "fire_dept", "fire_lane", "fire_prevention",  "gang_activity", "graffiti", "graffiti", "hydrant", "illegal_dumping", "illegal_parking", "litter", "noise","other311", "missed_pickup","panhandling", "patrol", "paving_request", "pothole", "pruning", "refuse_violation", "replace_sign", "request_sign", "rodent_control", "sidewalk_obstruction", "sinkhole", "smoke_detectors", "snow_removal", "street_sweeper", "streetlight_repair", "traffic", "trafficlight_repair", "vacant_building", "weeds_debris"))))
    # Origin Clean
    dat311 <- transform(dat311, REQUEST_ORIGIN = as.factor(mapvalues(REQUEST_ORIGIN, c("Report2Gov Android", "Report2Gov iOS", "Report2Gov Website"),
                                                                     c("myBurgh (Android)", "myBurgh (iOS)", "Website"))))
    dat311 <- transform(dat311, REQUEST_ORIGIN2 = as.factor(mapvalues(REQUEST_ORIGIN, c("myBurgh (Android)", "myBurgh (iOS)", "Website"),
                                                                      c('<a href="https://play.google.com/store/apps/details?id=com.qscend.report2gov.myburgh&hl=en" target="_blank">myBurgh (Android)</a>','<a href="https://itunes.apple.com/us/app/myburgh/id1021606996?mt=8" target="_blank">myBurgh (iOS)</a>', '<a href="http://pittsburghpa.gov/311/form" target="_blank">Website</a>'))))
    dat311$DEPARTMENT <- ifelse(is.na(dat311$DEPARTMENT), "Other", dat311$DEPARTMENT)
    dat311$DEPARTMENT <- as.factor(dat311$DEPARTMENT)
    dat311$NEIGHBORHOOD <- as.factor(dat311$NEIGHBORHOOD)
    # Sort
    dat311 <- dat311[rev(order(as.Date(dat311$date, format="%d/%m/%Y"))),]
    
    return(dat311)
  })
  # 311 data with filters
  dat311Input <- reactive({
    # Load 311 Requests
    dat311 <- dat311Load()
    
    # 311 Filters
    if (length(input$dept_select) > 0){
      dat311 <- dat311[dat311$DEPARTMENT %in% input$dept_select,]
    }
    if (length(input$req.type) > 0){
      dat311 <- dat311[dat311$REQUEST_TYPE %in% input$req.type,]
    }
    if (length(input$status_type) > 0){
      dat311 <- dat311[dat311$STATUS %in% input$status_type,]
    }
    if (length(input$origin_select) > 0){
      dat311 <- dat311[dat311$REQUEST_ORIGIN %in% input$origin_select,]
    }
    
    # Geographic Filters
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
    
    # Search Filter
    if (!is.null(input$search) && input$search != "") {
      dat311 <- dat311[apply(dat311, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(dat311)
  })
  # Police Blotter data with filters
  blotterLoad <- reactive({
    # Blotter
    archive <- ckanQueryDates("044f2016-1dfd-4ab0-bc1e-065da05fca2e", input$dates[1], input$dates[2], "INCIDENTTIME")
    
    # Clean for merge
    archive$X <- as.numeric(archive$X)
    archive$Y <- as.numeric(archive$Y)
    # Load Thirty Day Blotter
    thirty <- ckanQueryDates("1797ead8-8262-41cc-9099-cbc8a161924b", input$dates[1], input$dates[2], "INCIDENTTIME")
    
    # Merge
    archive <- archive[,c(colnames(thirty))]
    blotter <- rbind(archive, thirty)
    
    # Prepare for Mapping
    blotter$date <- as.Date(blotter$INCIDENTTIME)
    
    if (nrow(blotter) > 0) {
      # Reform Hierarchy
      blotter$HIERARCHY_Num <- blotter$HIERARCHY
      blotter$HIERARCHY <- case_when(
        blotter$HIERARCHY_Num == 1 ~ "01 Murder",
        blotter$HIERARCHY_Num == 2 ~ "02 Rape", 
        blotter$HIERARCHY_Num == 4 ~ "04 Assault",
        blotter$HIERARCHY_Num == 5 ~ "05 Burglary",
        blotter$HIERARCHY_Num == 6 ~ "06 Theft",
        blotter$HIERARCHY_Num == 7 ~ "07 Vehicle Theft",
        blotter$HIERARCHY_Num == 8 ~ "08 Arson",
        blotter$HIERARCHY_Num == 9 ~ "09 Forgery",
        blotter$HIERARCHY_Num == 10 ~ "10 Simple Assault",
        blotter$HIERARCHY_Num == 11 ~ "11 Fraud",
        blotter$HIERARCHY_Num == 12 ~ "12 Embezzlement",
        blotter$HIERARCHY_Num == 13 ~ "13 Receiving Stolen Prop",
        blotter$HIERARCHY_Num == 14 ~ "14 Vandalism",
        blotter$HIERARCHY_Num == 15 ~ "15 Carrying Weapon",
        blotter$HIERARCHY_Num == 16 ~ "16 Prostitution",
        blotter$HIERARCHY_Num == 17 ~ "17 Sex Offense",
        blotter$HIERARCHY_Num == 18 ~ "18 Drug Offense",
        blotter$HIERARCHY_Num == 19 ~ "19 Gambling",
        blotter$HIERARCHY_Num == 20 ~ "20 Endangering Children",
        blotter$HIERARCHY_Num == 21 ~ "21 DUI",
        blotter$HIERARCHY_Num == 22 ~ "22 Liquor Laws",
        blotter$HIERARCHY_Num == 23 ~ "23 Public Drunkenness",
        blotter$HIERARCHY_Num == 24 ~ "24 Disorderly Conduct",
        blotter$HIERARCHY_Num == 25 ~ "25 Vagrancy",
        TRUE ~ "26 Other"
      )
      blotter$HIERARCHY <- as.factor(blotter$HIERARCHY)
    }
    
    # Unify Neighborhoods
    blotter <- transform(blotter, INCIDENTNEIGHBORHOOD = as.factor(mapvalues(INCIDENTNEIGHBORHOOD, c("Golden Triangle/Civic Arena", "Central Northside", "Mt. Oliver Neighborhood", "Troy Hill-Herrs Island"),
                                                                             c("Central Business District", "Central North Side", "Mount Oliver", "Troy Hill"))))
    
    blotter <- transform(blotter, icon = as.factor(mapvalues(HIERARCHY, levels(hierarchies), 
                                                             c("murder",  "rape", "robbery", "assault", "burglary", "theft", "vehicle_theft", "arson", "forgery", "simple_assault", "fraud", "embezzlement",  "receiving_stolen_property", "vandalism", "carrying_weapon", "prostitution", "sex_offense", "drug_offense", "gambling", "endangering_children", "DUI", "liquor_laws", "public_drunkenness", "disorderly_conduct", "vagrancy", "other"))))
    
    # Clean Geographies
    blotter$HIERARCHY <- as.factor(blotter$HIERARCHY)
    names(blotter)[names(blotter)=="INCIDENTZONE"] <- "POLICE_ZONE"
    blotter <- cleanGeo(blotter, TRUE)
    # Clean Flag
    blotter$CLEAREDFLAG <- ifelse(blotter$CLEAREDFLAG == "Y", "Yes", "No")
    
    return(blotter)
  })
  blotterInput <- reactive({
    # Load Blotter
    blotter <- blotterLoad()
    
    # Hierarchy Filter
    if (length(input$hier) > 0){
      blotter <- blotter[blotter$HIERARCHY %in% input$hier,]
    } 
    
    # Geographic Filters
    if (length(input$zone_select) > 0 & input$filter_select == "Police Zone"){
      blotter <- blotter[blotter$POLICE_ZONE %in% input$zone_select,]
    } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
      blotter <- blotter[blotter$INCIDENTNEIGHBORHOOD %in% input$hood_select,]
    } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
      blotter <- blotter[blotter$PUBLIC_WORKS_DIVISION %in% input$DPW_select,]
    } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
      blotter <- blotter[blotter$COUNCIL_DISTRICT %in% input$council_select,]
    } else if (length(input$firez_select) > 0 & input$filter_select == "Fire Zone") {
      blotter <- blotter[blotter$FIRE_ZONE %in% input$firez_select,]
    }
    
    # Prepare Filter
    if (length(input$offense_select) > 0) { 
      search_string <- paste(input$offense_select, collapse="|")
      blotter <- blotter[which(grepl(search_string, blotter$OFFENSES, ignore.case = TRUE)),]
    }
    
    # Search Filter
    if (!is.null(input$search) && input$search != "") {
      blotter <- blotter[apply(blotter, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(blotter)
  })
  arrestsLoad <- reactive({
    arrests <- ckanQueryDates("e03a89dd-134a-4ee8-a2bd-62c40aeebc6f", input$dates[1], input$dates[2], "ARRESTTIME")
    
    # Date Col
    arrests$date <- as.Date(arrests$ARRESTTIME)
    # Unify Neighborhoods
    arrests <- transform(arrests, INCIDENTNEIGHBORHOOD = as.factor(mapvalues(INCIDENTNEIGHBORHOOD, c("Golden Triangle/Civic Arena", "Central Northside", "Mt. Oliver Neighborhood", "Troy Hill-Herrs Island"),
                                                                             c("Central Business District", "Central North Side", "Mount Oliver", "Troy Hill"))))
    # Clean Geographies
    names(arrests)[names(arrests)=="INCIDENTZONE"] <- "POLICE_ZONE"
    arrests <- cleanGeo(arrests, TRUE)
    
    return(arrests)
  })
  # Arrest data with filters
  arrestsInput <- reactive({
    # Load Arrests
    arrests <- arrestsLoad()
    
    # Offenses Columns
    arrests$OFFENSES <- as.character(arrests$OFFENSES)
    incidents2 <- as.data.frame(do.call(rbind, strsplit(arrests$OFFENSES, " / ", fixed = FALSE)))
    arrests <- cbind(arrests, incidents2)
    offensesColAr <- as.numeric(ncol(arrests))
    offensesAr1 <- as.numeric(which(colnames(arrests)=="V1"))
    
    # Sort
    arrests <- arrests[rev(order(as.Date(arrests$date, format="%d/%m/%Y"))),]
    
    # Geographic Filters
    if (length(input$zone_select) > 0 & input$filter_select == "Police Zone"){
      arrests <- arrests[arrests$POLICE_ZONE %in% input$zone_select,]
    } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
      arrests <- arrests[arrests$INCIDENTNEIGHBORHOOD %in% input$hood_select,]
    } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
      arrests <- arrests[arrests$PUBLIC_WORKS_DIVISION %in% input$DPW_select,]
    } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
      arrests <-arrests[arrests$COUNCIL_DISTRICT %in% input$council_select,]
    } else if (length(input$firez_select) > 0 & input$filter_select == "Fire Zone") {
      arrests <- arrests[arrests$FIRE_ZONE %in% input$firez_select,]
    }
    
    # Offense Filter
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
    
    # Search Filter
    if (!is.null(input$search) && input$search != "") {
      arrests <- arrests[apply(arrests, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(arrests)
  })
  citationsLoad <- reactive({
    citations <- ckanQueryDates("6b11e87d-1216-463d-bbd3-37460e539d86", input$dates[1], input$dates[2], "CITEDTIME")
    
    #Fix Date
    citations$date <- as.Date(citations$CITEDTIME)
    # Unify Neighborhoods
    citations <- transform(citations, NEIGHBORHOOD = as.factor(mapvalues(NEIGHBORHOOD, c("Golden Triangle/Civic Arena", "Central Northside", "Mt. Oliver Neighborhood", "Troy Hill-Herrs Island"),
                                                                         c("Central Business District", "Central North Side", "Mount Oliver", "Troy Hill"))))
    # Clean Geographies
    names(citations)[names(citations)=="ZONE"] <- "POLICE_ZONE"
    citations <- cleanGeo(citations, TRUE)
    
    return(citations)
  })
  # Citations data with filters
  citationsInput <- reactive({
    citations <- citationsLoad()
    
    # Offenses Columns
    citations$OFFENSES <- as.character(citations$OFFENSES)
    incidents2 <- as.data.frame(do.call(rbind, strsplit(citations$OFFENSES, " / ", fixed = FALSE)))
    citations <- cbind(citations, incidents2)
    offensesColCit <- as.numeric(ncol(citations))
    offensesCit1 <- as.numeric(which(colnames(citations)=="V1"))
    
    # Geographic Filters
    if (length(input$zone_select) > 0 & input$filter_select == "Police Zone"){
      citations <- citations[citations$POLICE_ZONE %in% input$zone_select,]
    } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
      citations <- citations[citations$INCIDENTNEIGHBORHOOD %in% input$hood_select,]
    } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
      citations <- citations[citations$PUBLIC_WORKS_DIVISION %in% input$DPW_select,]
    } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
      citations <-citations[citations$COUNCIL_DISTRICT %in% input$council_select,]
    } else if (length(input$firez_select) > 0 & input$filter_select == "Fire Zone") {
      citations <- citations[citations$FIRE_ZONE %in% input$firez_select,]
    }
    
    # Offense Filter
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
    
    # Search Filter
    if (!is.null(input$search) && input$search != "") {
      citations <- citations[apply(citations, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(citations) 
  })
  firesLoad <- reactive({
    fires <- ckanQueryDates("8d76ac6b-5ae8-4428-82a4-043130d17b02", input$dates[1], input$dates[2], "alarm_time")
    
    # Clean
    fires$fire_desc <- paste(fires$incident_type, fires$type_description)
    fires$fire_desc <- as.factor(fires$fire_desc)
    
    return(fires)
  })
  # Fire data with filters
  firesInput <-reactive({
    # Load Fires
    fires <- firesLoad()
    
    # Type Description Filter
    if (length(input$fire_desc_select) > 0) {
      fires <- fires[fires$fire_desc %in% input$fire_desc_select,]
    }
    
    # Clean Geographies
    fires <- cleanGeo(fires)
    
    # Geographic Filters
    if (length(input$zone_select) > 0 & input$filter_select == "Police Zone"){
      fires <- fires[fires$police_zone %in% input$zone_select,]
    } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
      fires <- fires[fires$neighborhood %in% input$hood_select,]
    } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
      fires <- fires[fires$public_works_division %in% input$DPW_select,]
    } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
      fires <-fires[fires$council_district %in% input$council_select,]
    } else if (length(input$firez_select) > 0 & input$filter_select == "Fire Zone") {
      fires <- fires[fires$fire_zone %in% input$firez_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) && input$search != "") {
      fires <- fires[apply(fires, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    # Icons
    if (nrow(fires) > 0) {
      fires$icon <- case_when(fires$incident_type %in% c(111, 112) ~ "fire_building",
                              fires$incident_type %in% c(113, 123) ~ "fire_cooking",
                              fires$incident_type %in% c(130, 131, 132, 133, 134, 137) ~ "fire_vehicle",
                              fires$incident_type %in% c(140, 141, 142, 143, 171, 173) ~ "fire_brush",
                              fires$incident_type %in% c(118, 117, 150, 151, 152, 154, 155) ~ "fire_trash",
                              TRUE ~ "fire")
    } else {
      fires <- read.table(text = "", col.names =  c(names(fires), "icon"))
    }
    
    return(fires)
  })
  violationsLoad <- reactive({
    violations <- ckanQueryDates("4e5374be-1a88-47f7-afee-6a79317019b4", input$dates[1], input$dates[2], "INSPECTION_DATE")
    
    violations <- violations %>%
      mutate(date = as.Date(INSPECTION_DATE),
             url = paste0('<a href="http://www2.county.allegheny.pa.us/RealEstate/GeneralInfo.aspx?ParcelID=', PARCEL, '" target="_blank">', PARCEL, '</a>'),
             INSPECTION_RESULT = as.factor(INSPECTION_RESULT),
             full_address = paste(STREET_NUM, STREET_NAME)) %>%
      transform(icon = as.factor(mapvalues(INSPECTION_RESULT, c('Abated','Violations Found','Voided'),
                                           c('violations_abated', 'violations_found', 'violations_void'))))
    violations$FullAddress <- paste(ifelse(is.na(violations$STREET_NUM) | is.null(violations$STREET_NUM) | violations$STREET_NUM == 0, "", violations$STREET_NUM) , ifelse(is.na(violations$STREET_NAME) | is.null(violations$STREET_NAME), "", violations$STREET_NAME))
    
    # Clean Geographies
    violations <- cleanGeo(violations, TRUE)
    
    return(violations)
  })
  # Code Violations data with filters
  violationsInput <- reactive({
    # Load Violations
    violations <- violationsLoad()
    
    if (nrow(violations) > 0) {
      # Prepare 
      violations1 <- ncol(violations) + 1
      list <- as.data.frame(do.call(rbind, strsplit(violations$VIOLATION, ":: ", fixed = FALSE)))
      violations <- cbind(violations, list)
      violationsCol <- ncol(violations)
      violations$VIOLATION <- as.character(violations$VIOLATION)
      violations$VIOLATION <- gsub("::", "/", violations$VIOLATION)
      violations$CORRECTIVE_ACTION <- gsub("::", "/", violations$CORRECTIVE_ACTION)
      
      # Violation Filter
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
    }
    
    # Result Filter
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
    } else if (length(input$firez_select) > 0 & input$filter_select == "Fire Zone") {
      violations <- violations[violations$FIRE_ZONE %in% input$firez_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) && input$search != "") {
      violations <- violations[apply(violations, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(violations)
  })
  # Capital Projects data with filters
  cprojLoad <- reactive({
    # Capital Projects
    year1 <- format(as.Date(input$dates[1]), "%Y")
    year2 <- format(as.Date(input$dates[2]), "%Y")
    cproj <- ckanSQL(paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT*%20FROM%20%222fb96406-813e-4031-acfe-1a82e78dc33c%22%20WHERE%20%22fiscal_year%22=%27", year1, "%27%20OR%20%22fiscal_year%22=%27", year2, "%27%20OR%20%22status%22=%27Planned%27%20OR%20%22status%22=%27In%20Progress%27"))
    
    cproj <- transform(cproj, icon = as.factor(mapvalues(area, functional_areas, c("administration", "engineering_construction", "facility_improvement", "neighborhood_development", "public_safety", "vehicles_equipment"))))
    
    # Formatting
    cproj$budgeted_amount <- dollarsComma(as.numeric(cproj$budgeted_amount))
    
    return(cproj)
  })
  cprojInput <- reactive({
    cproj <- cprojLoad()
    
    if (nrow(cproj) > 0){
      # Clean Hood
      cproj$neighborhood <- gsub("\\|", ", ", cproj$neighborhood)
      # Clean Zones
      for (i in 1:length(levels(load.zones$POLICE_ZONE))) {
        rep <- as.character(levels(load.zones$POLICE_ZONE)[i])
        cproj$police_zone <- gsub(as.character(i), rep, cproj$police_zone)
      }
      cproj$police_zone <- gsub("\\|", ", ", cproj$police_zone)
      # Clean Council
      for (i in 1:length(levels(load.council$COUNCIL_DISTRICT))) {
        rep <- as.character(levels(load.council$COUNCIL_DISTRICT)[i])
        cproj$council_district <- gsub(as.character(i), rep, cproj$council_district)
      }
      cproj$council_district <- gsub("\\|", ", ", cproj$council_district)
      # Clean DPW
      for (i in 1:length(levels(load.dpw$PUBLIC_WORKS_DIVISION))) {
        rep <- as.character(levels(load.dpw$PUBLIC_WORKS_DIVISION)[i])
        cproj$public_works_division <- gsub(as.character(i), rep, cproj$public_works_division)
      }
      cproj$public_works_division <- gsub("\\|", ", ", cproj$public_works_division)
      
      if (length(input$funcarea_select) > 0) {
        cproj <- cproj[cproj$area %in% input$funcarea_select,]
      }
      
      # Geographic Filters
      if (length(input$zone_select) > 0 & input$filter_select == "Police Zone"){
        for (i in 1:length(input$zone_select)) {
          if (i == 1) {
            cproj.temp <- cproj[grepl(as.character(input$zone_select[i]), cproj$police_zone), ]
          } else {
            temp <- cproj[grepl(input$zone_select[i], cproj$police_zone), ]
            cproj.temp <- rbind(cproj.temp, temp)
          }
        }
        cproj <- unique(cproj.temp)
      } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
        for (i in 1:length(input$hood_select)) {
          if (i == 1) {
            cproj.temp <- cproj[grepl(as.character(input$hood_select[i]), cproj$neighborhood), ]
          } else {
            temp <- cproj[grepl(as.character(input$hood_select[i]), cproj$neighborhood), ]
            cproj.temp <- rbind(cproj.temp, temp)
          }
        }
        cproj <- unique(cproj.temp)
      } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
        for (i in 1:length(input$DPW_select)) {
          if (i == 1) {
            cproj.temp <- cproj[grepl(input$DPW_select[i], cproj$public_works_division), ]
          } else {
            temp <- cproj[grepl(input$DPW_select[i], cproj$public_works_division), ]
            cproj.temp <- rbind(cproj.temp, temp)
          }
        }
        cproj <- unique(cproj.temp)
      } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
        for (i in 1:length(input$council_select)) {
          if (i == 1) {
            cproj.temp <- cproj[grepl(input$council_select[i], cproj$council_district), ]
          } else {
            temp <- cproj[grepl(input$council_select[i], cproj$council_district), ]
            cproj.temp <- rbind(cproj.temp, temp)
          }
        }
        cproj <- unique(cproj.temp)
      } else if (length(input$firez_select) > 0 & input$filter_select == "Fire Zone") {
        for (i in 1:length(input$firez_select)) {
          if (i == 1) {
            cproj.temp <- cproj[grepl(input$firez_select[i], cproj$council_district), ]
          } else {
            temp <- cproj[grepl(input$firez_select[i], cproj$firez_district), ]
            cproj.temp <- rbind(cproj.temp, temp)
          }
        }
        cproj <- unique(cproj.temp)
      }
      
      # Search Filter
      if (!is.null(input$search) && input$search != "") {
        cproj <- cproj[apply(cproj, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
      }
    }
    
    return(cproj)
  })
  # Generate table for Data page and export
  # Note all reports do same data process comments only exist for 311
  reportInput <- reactive({
    if (input$report_select == "311 Requests") {
      # Load dataset
      dat311 <- dat311Input()
      
      # Select display columns
      dat311 <- subset(dat311, select = c(REQUEST_TYPE, DEPARTMENT, CREATED_ON, STATUS, NEIGHBORHOOD, COUNCIL_DISTRICT, POLICE_ZONE, PUBLIC_WORKS_DIVISION))
      
      # Rename columns for humans
      colnames(dat311) <- c("Request Type", "Dept", "Create Date", "Status", "Neighborhood", "Council District", "Police Zone",  "Public Works Division")
      
      # Set report data
      report <- dat311
    } else if (input$report_select == "Blotter"){
      # Select Columns of Interest
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
    } else if (input$report_select == "Fire Incidents") {
      fires <- firesInput()
      
      fires <- subset(fires, select = c(call_no, fire_desc, alarm_time, primary_unit, alarms, address, fire_zone, neighborhood, council_district))
      colnames(fires) <- c("Call #", "Type", "Alarm Time", "Primary Unit", "Alarms", "Location", "Fire Zone", "Neighborhood", "Council District")
      
      report <- fires
    } else if (input$report_select == "Code Violations"){
      violations <- violationsInput()
      
      violations <- subset(violations, select = c(VIOLATION, INSPECTION_RESULT, INSPECTION_DATE, full_address, NEIGHBORHOOD, COUNCIL_DISTRICT, POLICE_ZONE, PUBLIC_WORKS_DIVISION, CASE_NUMBER, url))
      
      colnames(violations) <- c("Violation", "Result", "Inspection Date", "Address", "Neighborhood", "Council District", "Police Zone", "Public Works Division", "Case #", "Parcel ID")
      
      report <- violations
    } else if (input$report_select == "Capital Projects") {
      cproj <- cprojInput()
      
      cproj <- subset(cproj, select = c(name, asset_id, task_description, area, status, budgeted_amount, fiscal_year, neighborhood, council_district, public_works_division, police_zone))
      
      colnames(cproj) <- c("Project Name", "Asset", "Description", "Functional Area", "Status", "Budgeted Amount", "Fiscal Year", "Neighborhood", "Council", "Public Works Division", "Police Zone")
      
      report <- cproj
    } else if (input$report_select == "Collisions") {
      crashes <- crashesInput()
      
      if (class(crashes) == "SpatialPointsDataFrame") {
        crashes <- crashes@data
      }
      
      crashes <- subset(crashes, select = c(type, day, time, STREET_NAME, SPEED_LIMIT, VEHICLE_COUNT, PERSON_COUNT, INJURY_COUNT, FATAL_COUNT, LANE_CLOSED, TAILGATING, AGGRESSIVE_DRIVING, SPEEDING_RELATED, UNLICENSED, WET_ROAD, SNOW_SLUSH_ROAD, ICY_ROAD, REAR_END, OVERTURNED, CELL_PHONE, VEHICLE_TOWED, RUNNING_RED_LT, RUNNING_STOP_SIGN, FATIGUE_ASLEEP, WORK_ZONE, DISTRACTED, SCH_BUS_IND))
      
      colnames(crashes) <- c("Type", "Day of the Week", "Time (24-hour clock)", "Street", "Speed Limit", "Vehicles", "People", "Injuries", "Deaths", "Lane Closed", "Tailgating", "Agreesive Driving", "Speeding", "Unlicensed, Wet Road", "Snow/Slush", "Ice", "Rear Ended", "OVerturned", "Cellphone", "Towed", "Ran Red Light", "Ran Stop Sign", "Fatigue/Asleep", "Work Zone", "Distracted", "School Bus")
      
      report <- crashes
    } else if (input$report_select == "Right of Way Permits") {
      report <- rowInput() %>%
        rename(`Permit ID` = id,
               `Permit Type` = type,
               `Open Date` = open_date,
               `Valid From` = from_date,
               `Valid To` = to_date,
               `Restoration By` = restoration_date,
               `Primary Address` = address,
               `Street/Location` = street_or_location,
               `From Street` = from_street,
               `To Street` = to_street,
               `Contractor/Utility` = business_name,
               License = license_type,
               Neighborhood = neighborhood,
               `Council District` = council_district,
               `Public Works Division` = public_works_division) %>%
        select(`Permit ID`, `Permit Type`, `Open Date`, `Valid From`, `Valid To`, `Restoration By`, `Primary Address`, `Street/Location`, `From Street`, `To Street`, `Contractor/Utility`, License, Neighborhood, `Council District`, `Public Works Division`)
    }
    # Return Data
    return(report)
  })
  downloadInput <- reactive({
    report <- reportInput()
    
    # Report Table Search Filter
    if (!is.null(input$report.table_search) && input$report.table_search != "") {
      report <- report[apply(report, 1, function(row){any(grepl(input$report.table_search, row, ignore.case = TRUE))}), ]
    }
    
    return(report)
  })
  # Generate Report Table
  output$report.table <- DT::renderDataTable({
    # Load Report dataset
    reportInput()
  }, escape = FALSE, options = list(scrollX = TRUE), rownames= FALSE)
  # Execute download function
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$report_select, ".csv", sep="") },
    content = function(file) {
      write.csv(downloadInput(), file)
    }
  )
  easterEgg <- reactive({
    if (Sys.Date() == eDay | Sys.Date() == pDay | input$search == "Vote!") {
      month <- as.numeric(format(Sys.Date(), "%m")) 
      
      if (month >= 10) {
        yearQ <- format(Sys.Date(), "*%Y")
        monthQ <- "*november*"
      } else if (month > 3 ) {
        yearQ <- as.character(as.numeric(format(Sys.Date(), "*%Y")) - 1)
        monthQ <- "*november*"
      } else {
        yearQ <- format(Sys.Date(), "*%Y")
        monthQ <- "*may"
      }
      
      ids <- getIds("Allegheny County Polling Place Locations") %>%
        filter(grepl(yearQ, name.x, ignore.case = T) & grepl(monthQ, name.x, ignore.case = T))
      
      id <- ids$id.y[1]
      
      load.egg <- ckanSQL(paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22", id, "%22%20WHERE%22MuniName%22%20=%20%27PITTSBURGH%27")) %>%
        mutate(icon = "election",
               X = as.numeric(X),
               Y = as.numeric(Y),
               tt = paste0("<font color='black'>No matter who you Vote for, make sure you Vote!
                            <br><br><b>Location: </b>", LocName,
                            "<br><b>Ward: </b>", Ward,
                            "<br><b>District: </b>", District,
                            "<br><b>Address: </b>", NewAddress,
                            '<br><center><a href="https://alleghenycounty.civicengine.com/" target="_blank">Find your polling place!</a></center>'
      ))
    } else if(Sys.Date() <= as.Date(paste0(this_year,"-10-31")) & Sys.Date() >= as.Date(paste0(this_year,"-10-01"))) {
      # Egg
      X <- c(-79.9573738, -79.9796721, -79.9892566, -79.9814719, -79.9517155, -79.9128181, -79.9272001, -79.983961, -79.9948964, -79.9933058, -80.0217265, -80.0215099, -79.9851465)
      Y <- c(40.4611634, 40.4671619, 40.4667157, 40.472155, 40.4684005, 40.4401088, 40.4161835, 40.4186422, 40.4066441, 40.4012173, 40.4737751, 40.4636383, 40.4289496)
      title <- c("Allegheny", "Voegtly", "Ridgelawn", "St. Pauls", "St. Mary", "Smithfield East", "Calvary Catholic", "St Michaels", "St John Vianney", "South Side", "Highwood", "Union Dale", "Prince of Peace")
      load.egg <- data.frame(X,Y,title)
      load.egg$icon <- "halloween"
      load.egg$tt <- "Yarr! There be nuttin' to be found with that search term matey."
    } else if (Sys.Date() <= as.Date(paste0(this_year,"-11-30")) & Sys.Date() >= as.Date(paste0(this_year,"-11-01"))) {
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
    } else if (Sys.Date() >= as.Date(paste0(this_year,"-03-01")) & Sys.Date() <= as.Date(paste0(this_year,"-03-31"))){
      X <- c(-79.9968604, -80.004055)
      Y <- c(40.4381098, 40.440631)
      title <- c("City County Building", "Market Square")
      load.egg <- data.frame(X,Y,title)
      load.egg$icon <- "patrick"
      load.egg$tt <- "<i>Your search didn't turn up anything, not even my Pot-o-Gold!</i>"
    } else if (Sys.Date() >= as.Date(paste0(this_year,"-04-01")) & Sys.Date() <= as.Date(paste0(this_year,"-04-30"))) {
      load.egg <- read.csv("boundaries/Parks/parks.csv")
      load.egg$icon <- "easter_egg"
      load.egg$tt <- "<i>You couldn't find any results, but maybe you can find my eggs.</i>"
    } else if (Sys.Date() >= as.Date(paste0(this_year,"-07-01")) & Sys.Date() <= as.Date(paste0(this_year,"-07-07"))) {
      load.egg <- read.csv("boundaries/Parks/parks.csv")
      load.egg$icon <- "july_4"
      load.egg$tt <- "<i>Happy Independence Day! Looks like you need to try another search term.</i>"
    } else if (Sys.Date() >= as.Date(paste0(this_year,"-05-01")) & Sys.Date() <= as.Date(paste0(this_year,"-08-31"))) {
      load.pools <- readOGR("https://data.wprdc.org/dataset/f7067c4e-0c1e-420c-8c31-f62769fcd29a/resource/77288f26-54a1-4c0c-bc59-7873b1109e76/download/poolsimg.geojson")
      load.egg <- data.frame(coordinates(load.pools))
      colnames(load.egg) <- c("X","Y")
      load.egg$icon <- "summer"
      load.egg$tt <- "<i>Ah... Summer! Chill out, relax and grab some rays with me. Or if you'd like try another search term.</i>"
    } else {
      X <- c(-79.9968604, -80.004055)
      Y <- c(40.4381098, 40.440631)
      title <- c("City County Building", "Market Square")
      load.egg <- data.frame(X,Y,title)
      load.egg$icon <- "snow"
      load.egg$tt <- "Burrr!! The app's not frozen, there's just nothing that fits that description here!"
    }
    return(load.egg)
  })
  # Build main map
  output$map <- renderLeaflet({
    recs <- 0
    map <- leaflet() %>% 
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    # Code for Pittsburgh Basemaps
    if (input$basemap_select == "mapStack") {
      map <- addTiles(map, urlTemplate = "http://{s}.sm.mapstack.stamen.com/((terrain-background,$000[@30],$fff[hsl-saturation@80],$1b334b[hsl-color],mapbox-water[destination-in]),(watercolor,$fff[difference],$000000[hsl-color],mapbox-water[destination-out]),(terrain-background,$000[@40],$000000[hsl-color],mapbox-water[destination-out])[screen@60],(streets-and-labels,$fedd9a[hsl-color])[@50])/{z}/{x}/{y}.png", attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, &copy; <a href="https://cartodb.com/attributions">CARTO</a>')
    } else {
      map <- addProviderTiles(map, input$basemap_select,
                              options = providerTileOptions(noWrap = TRUE))
    }
    # Boundary Layers
    # Neighborhoods
    if (input$filter_select == "Neighborhood") {
      hoods <- hoodsInput()
      if (nrow(hoods) > 0) {
        map <- addPolygons(map, data = hoods,
                           stroke = TRUE, smoothFactor = 0, weight = 1, color = "#000000", opacity = 0.6,
                           fill = TRUE, fillColor = "#00FFFFFF", fillOpacity = 0, 
                           popup = ~paste("<font color='black'><b>Neighborhood:</b> ", htmlEscape(hood), "</font>")
        )
      }
      # Council Districts
    } else if (input$filter_select == "Council District"){
      council <- councilInput()
      if (nrow(council) > 0) {
        map <- addPolygons(map, data = council,
                           stroke = TRUE, smoothFactor = 0, weight = 1, color = "#000000", opacity = 0.6,
                           fill = TRUE, fillColor = "#00FFFFFF", fillOpacity = 0, 
                           popup = ~paste("<font color='black'><b>District:</b> ", htmlEscape(COUNCIL_DISTRICT), "</font>")
        )
      }
      # DPW Divisions
    } else if (input$filter_select == "Public Works Division"){
      dpw <- dpwInput()
      if (nrow(dpw) > 0) {
        map <- addPolygons(map, data = dpw,
                           stroke = TRUE, smoothFactor = 0, weight = 1, color = "#000000", opacity = 0.6,
                           fill = TRUE, fillColor = "#00FFFFFF", fillOpacity = 0, 
                           popup = ~paste("<font color='black'><b>Division:</b> ", htmlEscape(PUBLIC_WORKS_DIVISION), "</font>")
        )
      }
      # Police Zones
    } else if (input$filter_select == "Police Zone"){
      zones <- zonesInput()
      if (nrow(zones) > 0) {
        map <- addPolygons(map, data = zones,
                           stroke = TRUE, smoothFactor = 0, weight = 1, color = "#000000", opacity = 0.6,
                           fill = TRUE, fillColor = "#00FFFFFF", fillOpacity = 0, 
                           popup = ~paste("<font color='black'><b>Police Zone:</b> ", htmlEscape(POLICE_ZONE), "</font>")
        )
      }
    } else if (input$filter_select == "Fire Zone"){
      firez <- firezInput()
      if (nrow(firez) > 0) {
        map <- addPolygons(map, data = firez,
                           stroke = TRUE, smoothFactor = 0, weight = 1, color = "#000000", opacity = 0.6,
                           fill = TRUE, fillColor = "#00FFFFFF", fillOpacity = 0, 
                           popup = ~paste("<font color='black'><b>Fire Zone:</b> ", htmlEscape(dist_zone), "</font>")
        )
      }
    } 
    # Heat Vision Check
    if (input$heatVision %% 2) {
      allData <- NULL
      # 311 Layer
      if (input$toggle311) {
        dat311 <- dat311Input()
        dat311 <- dat311[!(is.na(dat311$X)),] 
        dat311 <- dat311[!(is.na(dat311$Y)),]
        dat311 <- subset(dat311, X > -80.242767 & X < -79.660492 & Y < 40.591014 & Y > 40.266428)
        
        allData <- rbind(dat311[,c("X", "Y")], allData)
      }
      # Arrests Layer
      if(input$toggleArrests) {
        arrests <- arrestsInput()
        # Remove unmappables
        arrests <- arrests[!(is.na(arrests$X)),] 
        arrests <- arrests[!(is.na(arrests$Y)),]
        arrests <- subset(arrests, X > -80.242767 & X < -79.660492 & Y < 40.591014 & Y > 40.266428)
        
        allData <- rbind(arrests[,c("X", "Y")], allData)
      }
      # Non-Traffic Citations Layer
      if (input$toggleCitations) {
        citations <- citationsInput()
        # Remove unmappables
        citations <- citations[!(is.na(citations$X)),] 
        citations <- citations[!(is.na(citations$Y)),]
        citations <- subset(citations, X > -80.242767 & X < -79.660492 & Y < 40.591014 & Y > 40.266428)
        
        allData <- rbind(citations[,c("X", "Y")], allData)
      }
      # Police Blotter Layer
      if (input$toggleBlotter) {
        blotter <- blotterInput()
        # Remove unmappables
        blotter <- blotter[!(is.na(blotter$X)),] 
        blotter <- blotter[!(is.na(blotter$Y)),]
        blotter <- subset(blotter, X >= -80.242767 & X <= -79.660492 & Y <= 40.591014 & Y >= 40.266428)
        # Remove non-pittsburgh incidents for map (these incidents still appear in data extracts)
        blotter <- subset(blotter, POLICE_ZONE != "OSC" | INCIDENTNEIGHBORHOOD != "Outside City" | INCIDENTNEIGHBORHOOD != "Outside County")
        
        allData <- rbind(blotter[,c("X", "Y")], allData) 
      }
      # Fire Incidents Layer
      if (input$toggleFires) {
        fires <- firesInput()
        # Remove unmappables        
        fires <- fires[!(is.na(fires$longitude)),]
        fires <- fires[!(is.na(fires$latitude)),]
        fires <- subset(fires, longitude >= -80.242767 & longitude <= -79.660492 & latitude <= 40.591014 & latitude >= 40.266428)
        
        fires <- fires[,c("longitude", "latitude")]
        colnames(fires) <- c("X", "Y")
        
        allData <- rbind(fires, allData)
      }
      # Building Code Violations
      if(input$toggleViolations) {
        violations <- violationsInput()
        # Remove unmappables
        violations <- violations[!(is.na(violations$X)),]
        violations <- violations[!(is.na(violations$Y)),]
        violations <- subset(violations, X > -80.242767 & X < -79.660492 & Y < 40.591014 & Y > 40.266428)
        
        allData <- rbind(violations[,c("X", "Y")], allData)  
      }
      # Capital Projects Layer
      if (input$toggleCproj) {
        cproj <- cprojInput()
        # Remove unmappables
        cproj <- cproj[!(is.na(cproj$longitude)),]
        cproj <- cproj[!(is.na(cproj$latitude)),]
        cproj <- subset(cproj, longitude > -80.242767 & longitude < -79.660492 & latitude < 40.591014 & latitude > 40.266428)
        
        cproj <- cproj[,c("longitude", "latitude")]
        colnames(cproj) <- c("X", "Y")
        
        allData <- rbind(cproj, allData)
      }
      if (input$toggleCrashes) {
        crashes <- crashesInput()
        if (!is.data.frame(crashes)) {
          crashes <- crashes@data
          crashes <- crashes[,c("DEC_LONG","DEC_LAT")]
          colnames(crashes) <- c("X", "Y")
          
          allData <- rbind(crashes, allData)
        }
      }
      if (input$toggleROW) {
        row <- rowInput() %>%
          filter(!is.na(map_lat) & !is.na(map_lon)) %>%
          rename(Y = map_lat,
                 X = map_lon) %>%
          select(X, Y)
        
        allData <- rbind(row, allData)
      }
      # Create Heat Map
      recs <- ifelse(is.null(allData), 0, nrow(allData))
      if (recs > 0) {
        map <- addHeatmap(map, data = allData, lng = ~X, lat = ~Y, radius = 8)
      }
      # Point Layers  
    } else {
      # 311 Data
      if (input$toggle311) {
        dat311 <- dat311Input()
        dat311 <- dat311[!(is.na(dat311$X)),] 
        dat311 <- dat311[!(is.na(dat311$Y)),]
        dat311 <- subset(dat311, X > -80.242767 & X < -79.660492 & Y < 40.591014 & Y > 40.266428)
        if (nrow(dat311) > 0){
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
                                          "<br><b>Status:</b>", dat311$STATUS,
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
      # Arrests Layer
      if(input$toggleArrests) {
        arrests <- arrestsInput()
        # Remove unmappables
        arrests <- arrests[!(is.na(arrests$X)),] 
        arrests <- arrests[!(is.na(arrests$Y)),]
        arrests <- subset(arrests, X > -80.242767 & X < -79.660492 & Y < 40.591014 & Y > 40.266428)
        if (nrow(arrests) > 0) {
          arrests$icon <- "arrest_made"
          map <- addMarkers(map, data=arrests,
                            clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                                                                                        var childCount = cluster.getChildCount();  
                                                                                        if (childCount < 10) {  
                                                                                        c = 'rgba(217, 217, 224, 1);'
                                                                                        } else if (childCount < 100) {  
                                                                                        c = 'rgba(171, 171, 182, 1);'  
                                                                                        } else { 
                                                                                        c = 'rgba(150, 150, 163, 1);'  
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
      # Non-Traffic Citations
      if (input$toggleCitations) {
        citations <- citationsInput()
        # Remove unmappables
        citations <- citations[!(is.na(citations$X)),] 
        citations <- citations[!(is.na(citations$Y)),]
        citations <- subset(citations, X > -80.242767 & X < -79.660492 & Y < 40.591014 & Y > 40.266428)
        if (nrow(citations) > 0) {
          citations$icon <- "citation"
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
      # Police Blotter Layer
      if (input$toggleBlotter) {
        blotter <- blotterInput()
        # Remove unmappables
        blotter <- blotter[!(is.na(blotter$X)),] 
        blotter <- blotter[!(is.na(blotter$Y)),]
        blotter <- subset(blotter, X >= -80.242767 & X <= -79.660492 & Y <= 40.591014 & Y >= 40.266428)
        # Remove non-pittsburgh incidents for map (these incidents still appear in data extracts)
        blotter <- subset(blotter, POLICE_ZONE != "OSC" | INCIDENTNEIGHBORHOOD != "Outside City" | INCIDENTNEIGHBORHOOD != "Outside County")
        # Remove Citation instances from Blotter data (if layer is)
        if (input$toggleCitations) {
          blotter <- blotter[!(blotter$CCR %in% citations$CCR),]
        }
        if (nrow(blotter) > 0) {
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
                                            "<br><b>Fire Zone:</b>", blotter$FIRE_ZONE,
                                            "<br><b>DPW Division:</b>", blotter$PUBLIC_WORKS_DIVISION,
                                            "<br><b>CCR:</b>", blotter$CCR, "</font>"))
          )
          recs <- recs + nrow(blotter)
          }
        }
      # Fire Incidents Layer
      if (input$toggleFires) {
        fires <- firesInput()
        # Remove unmappables        
        fires <- fires[!(is.na(fires$longitude)),]
        fires <- fires[!(is.na(fires$latitude)),]
        fires <- subset(fires, longitude >= -80.242767 & longitude <= -79.660492 & latitude <= 40.591014 & latitude >= 40.266428)
        if(nrow(fires) > 0) {
          map <- addMarkers(map, data=fires,
                            clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
                                                                                        var childCount = cluster.getChildCount();  
                                                                                        if (childCount < 10) {  
                                                                                        c = 'rgba(249, 208, 209, 1);'
                                                                                        } else if (childCount < 100) {  
                                                                                        c = 'rgba(230, 83, 92, 1);'  
                                                                                        } else { 
                                                                                        c = 'rgba(212, 30, 39, 1);'  
                                                                                        }    
                                                                                        return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
        }")), ~longitude, ~latitude, icon = ~icons_fires[icon],
                            popup = ~(paste("<font color='black'><b>Fire Description:</b>", fires$fire_desc,
                                            "<br><b>Alarm Time:</b>", fires$alarm_time,
                                            "<br><b>Primary Unit:</b>", fires$primary_unit,
                                            "<br><b># of Alarms:</b>", fires$alarms,
                                            "<br><b>Location:</b>", fires$address,
                                            "<br><b>Neighborhood:</b>", fires$neighborhood,
                                            "<br><b>Council District:</b>", fires$council_district,
                                            "<br><b>Police Zone:</b>", fires$police_zone,
                                            "<br><b>Fire Zone:</b>", fires$fire_zone,
                                            "<br><b>DPW Division:</b>", fires$public_works_division,
                                            "<br><b>Call #:</b>", fires$call_no, "</font>"))
          )
          recs <- recs + nrow(fires)                 
          }
        }
      # Building Code Violations
      if(input$toggleViolations) {
        violations <- violationsInput()
        # Remove unmappables
        violations <- violations[!(is.na(violations$X)),]
        violations <- violations[!(is.na(violations$Y)),]
        violations <- subset(violations, X > -80.242767 & X < -79.660492 & Y < 40.591014 & Y > 40.266428)
        if (nrow(violations) > 0) {
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
      # Capital Projects Layer
      if (input$toggleCproj) {
        cproj <- cprojInput()
        # Remove unmappables
        cproj <- cproj[!(is.na(cproj$longitude)),]
        cproj <- cproj[!(is.na(cproj$latitude)),]
        cproj <- subset(cproj, longitude > -80.242767 & longitude < -79.660492 & latitude < 40.591014 & latitude > 40.266428)
        if (nrow(cproj) > 0) {
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
        }")), ~longitude, ~latitude, icon = ~icons_cproj[icon],
                   popup = ~(paste("<font color='black'><b>Name:</b>", cproj$name,
                                   ifelse(is.na(cproj$asset_id), "", paste("<br><b>Asset:</b>", cproj$asset_id)),
                                   "<br><b>Description:</b>", cproj$task_description,
                                   "<br><b>Functional Area:</b>", cproj$area,
                                   "<br><b>Status:</b>",  cproj$status,
                                   "<br><b>Budgeted Amount:</b>", cproj$budgeted_amount,
                                   "<br><b>Fiscal Year:</b>", cproj$fiscal_year,
                                   "<br><b>Neighborhood:</b>", cproj$neighborhood,
                                   "<br><b>Council District:</b>", cproj$council_district,
                                   "<br><b>Public Works Division:</b>", cproj$public_works_division,
                                   "<br><b>Police Zone:</b>", cproj$police_zone, '</font>'
                            ))
          )
          recs <- recs + nrow(cproj)
          }
}
      # Crashes
      if (input$toggleCrashes) {
        crashes <- crashesInput()
        if (!is.data.frame(crashes)) {
          map <- addMarkers(map, data=crashes,
                            clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {
                                                                                        var childCount = cluster.getChildCount();
                                                                                        if (childCount < 10) {  
                                                                                        c = 'rgba(252, 247, 220, 1);'
                                                                                        } else if (childCount < 100) {  
                                                                                        c = 'rgba(251, 227, 136, 1);'  
                                                                                        } else { 
                                                                                        c = 'rgba(246, 205, 57, 1);'  
                                                                                        }   
                                                                                        return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
        }")), icon = ~icons_crashes[icon],
                   popup = ~(paste("<font color='black'><b>Collision Type:</b>", crashes$type,
                                   "<br><b>When:</b>", crashes$date,
                                   "<br><b>Day:</b>", crashes$day,
                                   "<br><b>Time <i>(24-hour clock)</i>:</b>", crashes$time,
                                   "<br><b>Street:</b>", crashes$STREET_NAME,
                                   "<br><b>Speed Limit:</b>", crashes$SPEED_LIMIT,
                                   "<br><b>Vehicles:</b>", crashes$VEHICLE_COUNT,
                                   "<br><b>People:</b>", crashes$PERSON_COUNT,
                                   "<br><b>Injuries:</b>", crashes$INJURY_COUNT,
                                   "<br><b>Deaths:</b>", crashes$FATAL_COUNT,
                                   "<br><br><b>Special Circumstances:</b><ul>",
                                   ifelse(crashes$LANE_CLOSED == 1, "<li>Lane Closed", ""),
                                   ifelse(crashes$TAILGATING== 1, "<li>Tailgating", ""),
                                   ifelse(crashes$AGGRESSIVE_DRIVING == 1, "<li>Aggressive Driving", ""),
                                   ifelse(crashes$SPEEDING_RELATED == 1, "<li>Speeding Related", ""),
                                   ifelse(crashes$UNLICENSED == 1, "<li>Unlicensed", ""),
                                   ifelse(crashes$WET_ROAD == 1, "<li>Wet Road", ""),
                                   ifelse(crashes$SNOW_SLUSH_ROAD == 1, "<li>Snow/Slushy Road", ""),
                                   ifelse(crashes$ICY_ROAD == 1, "<li>Icy Road", ""),
                                   ifelse(crashes$REAR_END == 1, "<li>Rear Ended", ""),
                                   ifelse(crashes$OVERTURNED == 1, "<li>Overturned Vehicle", ""),
                                   ifelse(crashes$CELL_PHONE == 1, "<li>Cellphone Related", ""),
                                   ifelse(crashes$VEHICLE_TOWED == 1, "<li>Vehicle Towed", ""),
                                   ifelse(crashes$RUNNING_RED_LT == 1, "<li>Ran Red Light", ""),
                                   ifelse(crashes$RUNNING_STOP_SIGN == 1, "<li>Ran Stop Sign", ""),
                                   ifelse(crashes$FATIGUE_ASLEEP == 1, "<li>Fatigued/Asleep", ""),
                                   ifelse(crashes$WORK_ZONE == 1, "<li>Work Zone", ""),
                                   ifelse(crashes$DISTRACTED == 1, "<li>Distracted", ""),
                                   ifelse(crashes$SCH_BUS_IND == 1, "<li>School Bus", ""),
                                   "</ul>"))
          )
          recs <- recs + nrow(crashes@data)
  }
    }
      if (input$toggleROW) {
        row <- rowInput()
        if (nrow(row) > 0) {
          map <- addMarkers(map, data=row,
                            clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {
                                                                                        var childCount = cluster.getChildCount();
                                                                                        if (childCount < 10) {  
                                                                                        c = 'rgba(138, 219, 218, 1);'
                                                                                        } else if (childCount < 100) {  
                                                                                        c = 'rgba(60, 195, 193, 1);'  
                                                                                        } else { 
                                                                                        c = 'rgba(47, 153, 151, 1);'  
                                                                                        }   
                                                                                        return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
        }")), ~map_lon, ~map_lat, icon = ~icons_row[icon],
                            popup = ~paste("<font color='black'><b>Permit Type:</b>", row$type,
                                           "<br><b>Permit ID:</b>", row$id,
                                           ifelse(is.na(row$business_name), "", paste("<br><b>Contractor/Utility:</b>", row$business_name)),
                                           ifelse(is.na(row$license_type), "", paste("<br><b>License:</b>", row$license_type)),
                                           "<br><b>Open Date:</b>", row$open_date,
                                           ifelse(row$from_date == row$to_date, paste("<br><b>Valid:</b>", row$from_date), paste("<br><b>Valid:</b>", row$from_date,  "-", row$to_date)),
                                           ifelse(is.na(row$restoration_date), "", paste("<br><b>Restoration By:</b>", row$restoration_date)),
                                           "<br><b>Primary Address:</b>", row$address,
                                           ifelse(is.na(row$street_or_location), "", paste("<br><b>Street/Location:</b>", row$street_or_location)),
                                           ifelse(is.na(row$from_street),  "", paste("<br><b>From Street:</b>", row$from_street)),
                                           ifelse(is.na(row$to_street),  "", paste("<br><b>To Street:</b>", row$to_street)),
                                           "<br><b>Neighborhood:</b>", row$neighborhood,
                                           "<br><b>Council District:</b>", row$council_district,
                                           "<br><b>Public Works Division:</b>", row$public_works_division)
                            )
          recs <- recs + nrow(row)
        }
      }
    }
    print(recs)
    if (recs < 1) {
      if (input$search == "Vote!") {
        egg <- easterEgg()
      } else {
        egg <- easterEgg()
        egg <- egg[sample(1:nrow(egg),1),]
      }
      
      map <- addMarkers(map, data = egg, lng= ~X, lat= ~Y, icon = ~icons_egg[icon], popup = ~tt) %>% 
        setView(-79.9959, 40.4406, zoom = 12)
    }
    #Generate Map
    map
  })
})

enableBookmarking("url")

# Run the application 
shinyApp(ui = ui, server = server)
