# Burgh's Eye View Points
# Organization: City of Pittsburgh
# Dept: Innovation & Performance
# Team: Analytics & Strategy
# Author: Geoffrey Arnold

# Load required packages
library(shiny)
library(shinythemes)
library(shinyTime)
library(xml2)

#"Dogfooding" Packages
library(httr)
library(jsonlite)
library(readr)
library(curl)
library(R4CouchDB)

# Visuals Libraries
library(leaflet)
library(leaflet.extras)
library(DT)
library(maptools)
library(htmltools)
library(htmlwidgets)
library(rgeos)
library(geojsonio)
library(sp)

# Data Transform
library(plyr)
library(dplyr)
library(zoo)
library(lubridate)
library(stringi)
library(stringr)

# Turn off Scientific Notation
options(scipen = 999)

ckan_api <- jsonlite::fromJSON("key.json")$ckan_api
couchdb_un <- jsonlite::fromJSON("key.json")$couchdb_un
couchdb_pw <- jsonlite::fromJSON("key.json")$couchdb_pw
couchdb_url <- jsonlite::fromJSON("key.json")$couchdb_url

selection_conn <- cdbIni(serverName = couchdb_url, port = "5984", uname = couchdb_un, pwd = couchdb_pw, DBName = "bev-inputs")

# Input Selection Function
selectGet <- function(id, conn) {
  conn$id <- id
  r <- cdbGetDoc(conn)$res
  vals <- unlist(r)[3:length(r)]
  levels(as.factor(vals))
}

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
  x <- paste0("https://data.wprdc.org/datastore/dump/", id)
  r <- GET(x, add_headers(Authorization = ckan_api))
  content(r)
}

# Function to Query WPRDC Data on Time Frame
ckanQueryDates <- function(id, start, end, column) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22", id, "%22%20WHERE%20%22", column, "%22%20%3E=%20%27", start, "%27%20AND%20%22", column, "%22%20%3C=%20%27", end, "%27")
  r <- GET(url, add_headers(Authorization = ckan_api))
  c <- content(r, "text")
  json <- gsub('NaN', '""', c, perl = TRUE)
  if (length(jsonlite::fromJSON(json)$result$records) == 0) {
    fields <- jsonlite::fromJSON(json)$result$fields$id
    data.frame(t(data.frame(1:length(fields), row.names = fields))[0,])
  } else {
    data.frame(jsonlite::fromJSON(json)$result$records)
  }
}

ckanQuery2 <- function(id, query, column, arg, query2, column2) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22", id, "%22%20WHERE%20%22", column,"%22%20=%20%27", query, "%27%20", arg, "%20%22", column2, "%22%20=%20%27", query2, "%27")
  r <- GET(url, add_headers(Authorization = ckan_api))
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
  start_year <- as.numeric(format(as.Date(start_date), "%Y"))
  end_year <- as.numeric(format(as.Date(end_date), "%Y"))
  start_month <- as.numeric(format(as.Date(start_date), "%m"))
  end_month <- as.numeric(format(as.Date(end_date), "%m"))
  if (start_year == end_year) {
    months <- start_month:end_month
    if (length(months) == 1) {
      url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%222c13021f-74a9-4289-a1e5-fe0472c89881%22%20WHERE%20%22MUNICIPALITY%22%20=%20%272301%27%20AND%20%22CRASH_YEAR%22%20=%27", start_year,"%27%20AND%20%22CRASH_MONTH%22%20=%20%27", start_month, "%27")
    } else {
      url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%222c13021f-74a9-4289-a1e5-fe0472c89881%22%20WHERE%20%22MUNICIPALITY%22%20=%20%272301%27%20AND%20%22CRASH_YEAR%22%20=%27", start_year,"%27%20AND%20(%22CRASH_MONTH%22%20BETWEEN%20%27", start_month,"%27%20AND%20%27", end_month, "%27)")
    }
    r <- GET(url, add_headers(Authorization = ckan_api))
    c <- content(r, "text")
    json <- gsub('NaN', '""', c, perl = TRUE)
    if (length(jsonlite::fromJSON(json)$result$records) == 0) {
      fields <- jsonlite::fromJSON(json)$result$fields$id
      df <- data.frame(t(data.frame(1:length(fields), row.names = fields))[0,])
    } else {
      df <- jsonlite::fromJSON(json)$result$records
    }
  } else {
    years <- start_year:end_year
    url_start <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%222c13021f-74a9-4289-a1e5-fe0472c89881%22%20WHERE%20%22MUNICIPALITY%22%20=%20%272301%27%20AND%20%22CRASH_YEAR%22%20=%27", start_year,"%27%20AND%20(%22CRASH_MONTH%22%20BETWEEN%20%27", start_month,"%27%20AND%20%2712%27)")
    r <- GET(url_start, add_headers(Authorization = ckan_api))
    c <- content(r, "text")
    json <- gsub('NaN', '""', c, perl = TRUE)
    if (length(jsonlite::fromJSON(json)$result$records) == 0) {
      fields <- jsonlite::fromJSON(json)$result$fields$id
      df1 <- data.frame(t(data.frame(1:length(fields), row.names = fields))[0,])
    } else {
      df1 <- jsonlite::fromJSON(json)$result$records
    }
    url_end <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%222c13021f-74a9-4289-a1e5-fe0472c89881%22%20WHERE%20%22MUNICIPALITY%22%20=%20%272301%27%20AND%20%22CRASH_YEAR%22%20=%27", end_year,"%27%20AND%20(%22CRASH_MONTH%22%20BETWEEN%20%271%27%20AND%20%27", end_month, "%27)")
    r <- GET(url_end, add_headers(Authorization = ckan_api))
    c <- content(r, "text")
    json <- gsub('NaN', '""', c, perl = TRUE)
    if (length(jsonlite::fromJSON(json)$result$records) == 0) {
      fields <- jsonlite::fromJSON(json)$result$fields$id
      df2 <- data.frame(t(data.frame(1:length(fields), row.names = fields))[0,])
    } else {
      df2 <- jsonlite::fromJSON(json)$result$records
    }
    df <- rbind(df1, df2)
    if (length(years) > 2) {
      for (i in years[2]:years[length(years)-1]) {
        url<- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%222c13021f-74a9-4289-a1e5-fe0472c89881%22%20WHERE%20%22MUNICIPALITY%22%20=%20%272301%27%20AND%20%22CRASH_YEAR%22%20=%27", i,"%27")
        r <- GET(url, add_headers(Authorization = ckan_api))
        c <- content(r, "text")
        json <- gsub('NaN', '""', c, perl = TRUE)
        if (length(jsonlite::fromJSON(json)$result$records) == 0) {
          fields <- jsonlite::fromJSON(json)$result$fields$id
          df_new <- data.frame(t(data.frame(1:length(fields), row.names = fields))[0,])
        } else {
          df_new <- jsonlite::fromJSON(json)$result$records
        }
        df <- rbind(df, df_new)
      }
    }
  }
  return(df)
}

#GEO CKAN Json
ckanGEO <- function(url) {
  r<- GET(url, add_headers(Authorization = ckan_api))
  c <- content(r, as ="text")
  rgdal::readOGR(c, "OGRGeoJSON", verbose = F)
}

# List for Clean Function
council_list <- selectGet("council_list", selection_conn)

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
load.hoods <- geojson_read("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/87a7e06c5d8440f280ce4b1e4f75cc84_0.geojson", what = "sp")
# DPW
load.dpw <- geojson_read("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/2d2c30d9633647ddab2f918afc38c35b_0.geojson", what = "sp")
load.dpw$PUBLIC_WORKS_DIVISION <- load.dpw$division
load.dpw@data <- cleanDPW(load.dpw@data, TRUE)
# Zone
load.zones <- geojson_read("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/7e95f0914283472e83e8000c0af33110_0.geojson", what = "sp")
load.zones$POLICE_ZONE <- load.zones$zone
load.zones@data <- cleanZone(load.zones@data, TRUE)
# Fire Zone
load.firez <- geojson_read("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/324584a643a743afba24149a304cc6d3_0.geojson", what = "sp")
# Council
load.council <- geojson_read("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/9ba815225b1a4d0eada5a00715344095_0.geojson", what = "sp")
load.council$COUNCIL_DISTRICT <- load.council$council
load.council@data <- cleanCouncil(load.council@data, TRUE)

# 311 Input & Icons
request_types <- selectGet("request_types", selection_conn)

requests311 <-c("Abandoned Vehicle (parked on street)", "Building Maintenance", "Building Without a Permit", "Drug Enforcement", "Fire Department", "Fire Lane", "Fire Prevention", "Gang Activity", "Graffiti, Documentation", "Graffiti, Removal", "Hydrant - Fire Admin", "Illegal Dumping", "Illegal Parking", "Litter","Noise", "Missed Pick Up", "Panhandling", "Patrol", "Paving Request", "Potholes", "Pruning (city tree)", "Refuse Violations", "Replace/Repair a Sign", "Request New Sign", "Rodent control", "Sidewalk Obstruction", "Sinkhole", "Smoke detectors", "Snow/Ice removal", "Street Cleaning/Sweeping", "Street Light - Repair", "Traffic", "Traffic or Pedestrian Signal, Repair", "Vacant Building", "Weeds/Debris")

departments <- selectGet("departments", selection_conn)
origins <- selectGet("origins", selection_conn)

# 311 Selections
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

# Building Permit Input & Icons
permit_types <- c("Board of Appeals Application", "Building Permit", "Communication Tower", "Demolition Permit", "Electrical Permit", "Fire Alarm Permit", "HVAC Permit", "Land Operations Permit", "Occupancy Only", "Occupant Load Placard", "Sign Permit", "Sprinkler Permit", "Temporary Occupancy", "Temporary Occupancy Commercial")

permit_status <- selectGet("permit_status", selection_conn)

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

# Load Workflow
# load.workflow <- ckan("7e0bf4bf-c7f5-48cd-8177-86f5ce776dfa")
# load.workflow$tool <- paste0("<dt>", load.workflow$status_date, ": ", load.workflow$action_by_dept, "</dt>", "<dd>", load.workflow$task, " - ", load.workflow$status, "</dd>")

violations <- selectGet("violations", selection_conn)

inspect_results <- c('Abated','Violations Found','Voided')

icons_violations <- iconList(
  violations_abated = makeIcon("./icons/PLI/violations_abated.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  violations_found = makeIcon("./icons/PLI/violations_found.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  violations_void = makeIcon("./icons/PLI/violations_void.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

# Blotter Input & Icons
hierarchies <- as.factor(c("01 Murder", "02 Rape", "03 Robbery", "04 Assault", "05 Burglary", "06 Theft", "07 Vehicle Theft", "08 Arson", "09 Forgery", "10 Simple Assault", "11 Fraud", "12 Embezzlement", "13 Receiving Stolen Prop", "14 Vandalism", "15 Carrying Weapon", "16 Prostitution", "17 Sex Offense", "18 Drug Offense", "19 Gambling", "20 Endangering Children", "21 DUI", "22 Liquor Laws", "23 Public Drunkenness", "24 Disorderly Conduct", "25 Vagrancy", "26 Other"))

offenses <- selectGet("offenses", selection_conn)

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

# Capital Projects Inputs & Icons
functional_areas <- c("Administration/Sub-Award", "Engineering and Construction", "Facility Improvement", "Neighborhood and Community Development", "Public Safety","Vehicles and Equipment")

icons_cproj <- iconList(
  administration = makeIcon("./icons/omb/administration.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  engineering_construction = makeIcon("./icons/omb/engineering_construction.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  facility_improvement = makeIcon("./icons/omb/facility_improvement.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  neighborhood_development = makeIcon("./icons/omb/community_development.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  public_safety = makeIcon("./icons/omb/public_safety.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  vehicles_equipment = makeIcon("./icons/omb/vehicles_equipment.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

# Collisions
crash_types <- c("Automobile", "Bicycle", "Bus", "Hit Deer", "Intoxicated Driver", "Motorcycle", "Pedestrian", "Train/Trolley", "Fixed Object")

circumstances_values <- c("LANE_CLOSED", "TAILGATING", "AGGRESSIVE_DRIVING", "SPEEDING_RELATED", "UNLICENSED", "WET_ROAD", "SNOW_SLUSH_ROAD", "ICY_ROAD", "REAR_END", "OVERTURNED", "CELL_PHONE", "VEHICLE_TOWED", "RUNNING_RED_LT", "RUNNING_STOP_SIGN", "FATIGUE_ASLEEP", "WORK_ZONE", "DISTRACTED", "SCH_BUS_IND")

circumstances_types <- c("Lane Closed", "Tailgating", "Aggressive Driving", "Speeding Related", "Unlicensed", "Wet Road", "Snow/Slushy Road", "Icy Road", "Rear Ended", "Overturned Vehicle", "Cellphone Related", "Vehicle Towed", "Ran Red Light", "Ran Stop Sign", "Fatigued/Asleep", "Work Zone", "Distracted", "School Bus")

icons_crashes <- iconList(
  crash = makeIcon("./icons/crashes/crash.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  crash_bike = makeIcon("./icons/crashes/crash_bike.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  crash_bus = makeIcon("./icons/crashes/crash_bus.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  crash_deer = makeIcon("./icons/crashes/crash_deer.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  crash_dui = makeIcon("./icons/crashes/crash_dui.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  crash_motorcycle = makeIcon("./icons/crashes/crash_motorcycle.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  crash_pedestrian = makeIcon("./icons/crashes/crash_pedestrian.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  crash_trolley = makeIcon("./icons/crashes/crash_trolley.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  crash_single = makeIcon("./icons/crashes/crash_single.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

# Fires
fire_desc <- selectGet("fire_desc", selection_conn)

icons_fires <- iconList(
  fire = makeIcon("./icons/fire/fire.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_brush = makeIcon("./icons/fire/fire_brush.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_building = makeIcon("./icons/fire/fire_building.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_cooking = makeIcon("./icons/fire/fire_cooking.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_trash = makeIcon("./icons/fire/fire_trash.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48),
  fire_vehicle = makeIcon("./icons/fire/fire_vehicle.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

# this_year
this_year <- format(Sys.Date(), format="%Y")
last_year <- as.numeric(this_year) -  1

# Election Day
nov <- ymd(as.Date(paste0(this_year, "-11-01")))
dow <- sapply(seq(0,6),function(x) wday(nov+days(x)))
eDay <- nov + days(which(dow==2))

# CouchDB Connection
# couchDB <- cdbIni(serverName = couchdb_url, uname = couchdb_un, pwd = couchdb_pw, DBName = "burghs-eye-view-points")
couchDB <- cdbIni(serverName = couchdb_url, uname = couchdb_un, pwd = couchdb_pw, DBName = "burghs-eye-view-points-dev")

if (Sys.Date() == eDay) {
  load.egg <- ckan("51efa73c-d4b8-4ac0-b65a-9c9b1f904372")
  load.egg <- subset(load.egg, MuniName == "PITTSBURGH")
  load.egg$icon <- "election"
  load.egg$tt <- paste0("<font color='black'>No matter who you Vote for, make sure you Vote!
                        <br><b>Location: </b>", load.egg$LocName,
                        "<br><b>Ward: </b>", load.egg$Ward,
                        "<br><b>District: </b>", load.egg$District,
                        "<br><b>Address: </b>", load.egg$NewAddress,
                        '<br><center><a href="https://www.pavoterservices.state.pa.us/pages/pollingplaceinfo.aspx" target="_blank">Find your polling place!</a></center>'
  )
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
  load.pools <- ckanGEO("https://data.wprdc.org/dataset/8186cabb-aa90-488c-b894-2d4a1b019155/resource/6f836153-ada7-4b18-b9c9-7a290c569ea9/download/pools.geojson")
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

icons_egg <- iconList(
  halloween = makeIcon("./icons/egg/pirate.png", iconAnchorX = 9, iconAnchorY = 12.5, popupAnchorX = 0, popupAnchorY = -12.5),
  election = makeIcon("./icons/egg/vote.png", iconAnchorX = 9, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13),
  thanksgiving = makeIcon("./icons/egg/thanksgiving.png", iconAnchorX = 9, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13),
  snow = makeIcon("./icons/egg/snowboard.png", iconAnchorX = 9, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13),
  new_year = makeIcon("./icons/egg/new_year.png", iconAnchorX = 9, iconAnchorY = 13.5, popupAnchorX = 0, popupAnchorY = -13.5),
  valentine = makeIcon("./icons/egg/valentine.png", iconAnchorX = 40, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5),
  patrick = makeIcon("./icons/egg/patrick.png", iconAnchorX = 40, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5),
  easter_egg = makeIcon("./icons/egg/easter.png", iconAnchorX = 45, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5),
  summer = makeIcon("./icons/egg/summer.png", iconAnchorX = 45, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5),
  july_4 = makeIcon("./icons/egg/july_4.png", iconAnchorX = 45, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5)               
)

# Non-Traffic Citations Icons
icons_citations <- iconList(
  citation = makeIcon("./icons/police/nontraffic_citation.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

# Arrests Icon
icons_arrests <- iconList(
  arrest_made = makeIcon("./icons/police/arrest_made.png", iconAnchorX = 18, iconAnchorY = 48, popupAnchorX = 0, popupAnchorY = -48)
)

# UI for application
ui <- navbarPage(id = "navTab",
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
                          tags$head(tags$style(HTML(".shiny-notification {
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
                          # Hide error codes that may appear
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"),
                          # Background of report.table
                          tags$style(type="text/css", '.report.table {background-color: #fff;}'),
                          # Remove unwanted padding and margins
                          tags$style(type="text/css", ".container-fluid {padding:0;}"),
                          tags$style(type="text/css", ".navbar-header {margin:auto;"),
                          tags$style(type="text/css", ".navbar-static-top {margin-bottom:0;}"),
                          tags$style(type="text/css", ".navbar-brand {height:60px; padding:0;}"),
                          tags$style(type="text/css", ".navbar {border-right-width: 20px;
                                                                border-left-width: 65px;}"),
                          # Set max height for pop-ups
                          tags$style(type="text/css", ".leaflet-popup-content {overflow-y: auto; max-height: 400px !important;}"),
                          # Edit top bar
                          tags$style(type= "text/css", ".form-group {
                                     margin-bottom: 0px;
                                     }"),
                          # Generate search & layer panel & Map (checks for mobile devices)
                          uiOutput("mapPanel")
                          ),
                 tabPanel(a("Places", href="https://pittsburghpa.shinyapps.io/BurghsEyeViewPlaces/", style = "padding-top: 0px;
    padding-bottom: 0px; bottom: 19; top: -19; bottom: 19px")),
                 tabPanel(a("Parcels", href="https://pittsburghpa.shinyapps.io/BurghsEyeViewParcels/", style = "padding-top: 0px; padding-bottom: 0px; bottom: 19; top: -19; bottom: 19px")),
                 tabPanel('Data: Points', class = "Data: Points", value = "Data: Points",
                          # Select Dataset for Export
                          inputPanel(
                            selectInput("report_select", 
                                        tagList(shiny::icon("map-marker"), "Select Layer:"),
                                        choices = c("311 Requests", "Arrests", "Blotter", "Capital Projects", "Code Violations", "Collisions", "Fire Incidents", "Non-Traffic Citations"), #  , "Building Permits"
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
                        choices = c(`OSM Mapnik` = "OpenStreetMap.Mapnik", `OSM France` = "OpenStreetMap.France", `OSM Humanitarian` = "OpenStreetMap.HOT", `Stamen Toner` = "Stamen.Toner", `Esri Satellite` = "Esri.WorldImagery", Esri = "Esri.WorldStreetMap", `OSM Dark Matter` = "CartoDB.DarkMatter", `OSM Positron` = "CartoDB.Positron"),
                        selected = "CartoDB.DarkMatter")
    } else {
      updateActionButton(session = session,
                         inputId = "heatVision",
                         label = "Enable Heat Map",
                         icon = icon("eye"))
      updateSelectInput(session = session,
                        inputId = "basemap_select",
                        label = "Basemap",
                        choices = c(`OSM Mapnik` = "OpenStreetMap.Mapnik", `OSM France` = "OpenStreetMap.France", `OSM Humanitarian` = "OpenStreetMap.HOT", `Stamen Toner` = "Stamen.Toner", `Esri Satellite` = "Esri.WorldImagery", Esri = "Esri.WorldStreetMap", `OSM Dark Matter` = "CartoDB.DarkMatter", `OSM Positron` = "CartoDB.Positron"),
                        selected = "OpenStreetMap.Mapnik")
    }
  })
  # Tracking Info
  sessionStart <- as.numeric(Sys.time())
  names(sessionStart) <- "sessionStart"
  sessionID <- paste(stri_rand_strings(1, 5), gsub("\\.", "-", sessionStart) , "points", sep="-")
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
  # Map Tab UI
  output$mapPanel <- renderUI({
    # UI for Desktop Users
    #
    if (as.numeric(input$GetScreenWidth) > 800) {
      tagList(
        # Generate Map
        div(class="mapBack", style="position: absolute;
                                    width: 100%;
                                    z-index: -1;
                                    left: 0px;
                                    top: 55px;", leafletOutput("map")),
        # Map size for Desktop CSS
        tags$style(type = "text/css", "#map {height: calc(100vh - 55px) !important;}"),
        absolutePanel(
          # Input panel for Desktops (alpha'd)
          top = 70, left = 50, width = '300px',
          wellPanel(id = "tPanel", style = "overflow-y:auto; max-height: calc(100vh - 90px) !important;",
                    textInput("search",
                              value = ifelse(Sys.Date() == eDay, "Election Day!", ""),
                              label = NULL, 
                              placeholder = "Search"),
                    # Add background image
                    tags$head(tags$style(type="text/css", '.mapBack {
                                               background-image: url("loading.png");
                                               background-repeat: no-repeat;
                                               background-position: center;
                                               background-size: contain;
                                               }')),
                    HTML('<small style="font-size:11px;margin-left:3px">Locations are not exact. (See &rsquo;About&rsquo; for details.)</small><br><br>'),
                    dateRangeInput("dates",
                                   label = NULL,
                                   start = Sys.Date()-10,
                                   end = Sys.Date(),
                                   min = as.Date("2004-01-01"),
                                   max = Sys.Date(),
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
                                c(`Offense Type`='', offenses),
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
                    # HTML('<font color="#009FE1">'),
                    # checkboxInput("togglePermits",
                    #               label = "Building Permits",
                    #               value = TRUE),
                    # HTML('</font>'),
                    # selectInput("permit_select",
                    #             label = NULL,
                    #             c(`Permit Type`='', permit_types),
                    #             multiple = TRUE,
                    #             selectize=TRUE),
                    # selectInput("status_select",
                    #             label = NULL,
                    #             c(`Permit Status`='', permit_status),
                    #             multiple = TRUE,
                    #             selectize=TRUE),
                    HTML('<font color="#0B9444">'),
                    checkboxInput("toggleViolations",
                                  label = "Code Violations", 
                                  value = TRUE),
                    HTML('</font>'),
                    selectInput("violation_select",
                                label = NULL,
                                c(`Violation`='', violations),
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
                                choices = c(`OSM Mapnik` = "OpenStreetMap.Mapnik", `OSM France` = "OpenStreetMap.France", `OSM Humanitarian` = "OpenStreetMap.HOT", `Stamen Toner` = "Stamen.Toner", `Esri Satellite` = "Esri.WorldImagery", Esri = "Esri.WorldStreetMap", `OSM Dark Matter` = "CartoDB.DarkMatter", `OSM Positron` = "CartoDB.Positron"),
                                selected = "OpenStreetMap.Mapnik"),
                    selectInput("filter_select",
                                "Filter by Area",
                                c(`Area Type`='', c("Neighborhood", "Council District", "Police Zone", "Fire Zone", "Public Works Division")),
                                selectize = TRUE,
                                selected = ""),
                    uiOutput("filter_UI")
          ), style = "opacity: 0.88"
        )
      )
    } else {
      tagList(
        # Input panel for Mobile (stationary at top)
        absolutePanel(top = 65, left = 0, width = '100%' ,
                      wellPanel(id = "tPanel", style ="padding-left: 5px; padding-right: 5px;",
                     # Remove padding from Search Bar
                     tags$style(type= "text/css", "#tPanel {margin-bottom:0px; padding:0px; overflow-y:scroll; max-height: calc(100vh - 60px); !important; min-height: 55px;}"),
                     # Set background color to match panels
                     tags$style(type = "text/css", "body {background-color: #ecf0f1}"),
                     tags$style(type= "text/css", "{width:100%;
                                margin-bottom:5px;
                                text-align: center;}
                                .inner
                                {display: inline-block;}"),
                     # Div for Search Bar and Expansion
                     HTML('<div id="outer" style="position:absolute;z-index: 9; background-color:#ecf0f1; width:100%;">'),
                     # Set Searchvar width optimal for device
                     tags$style(type = "text/css", paste0('#search {width: calc(100vw - 85px); margin-left:10px;}')),
                     # Inputs
                     div(style="display:inline-block;", 
                         textInput("search", 
                                   value = ifelse(Sys.Date() == eDay, "Election Day!", ""),
                                   label = NULL, 
                                   placeholder = "Search")),
                     tags$style(style="text/css", chartr0('#mapPanel #outer .btn .fa:before { content: "\\f056";  }
                                                          #mapPanel #outer .btn.collapsed .fa:before { content: "\\f055";  }')),
                     HTML('<button class="btn collapsed" data-toggle="collapse" data-target="#mobile"><i class="fa fa-search-plus" aria-hidden="true"></i></button></div>
                          <div id="mobile" class="collapse" style="margin-top:55px;">
                            <small style="font-size:11px;margin-left:3px">Not all locations are exact. (See &rsquo;About&rsquo; for details.)</small>
                          <br>'),
                     dateRangeInput("dates",
                                    label = NULL,
                                    start = Sys.Date()-10,
                                    end = Sys.Date(),
                                    min = as.Date("2004-01-01"),
                                    max = Sys.Date(),
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
                                 c(`Offense Type`='', offenses),
                                 multiple = TRUE,
                                 selectize=TRUE),
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
                     # HTML('<font color="#009FE1">'),
                     # checkboxInput("togglePermits",
                     #               label = "Building Permits",
                     #               value = TRUE),
                     # HTML('</font>'),
                     # selectInput("permit_select",
                     #             label = NULL,
                     #             c(`Permit Type`='', permit_types),
                     #             multiple = TRUE,
                     #             selectize=TRUE),
                     # selectInput("status_select",
                     #             label = NULL,
                     #             c(`Permit Status`='', permit_status),
                     #             multiple = TRUE,
                     #             selectize=TRUE),
                     HTML('<font color="#0B9444">'),
                     checkboxInput("toggleViolations",
                                   label = "Code Violations", 
                                   value = TRUE),
                     HTML('</font>'),
                     selectInput("violation_select",
                                 label = NULL,
                                 c(`Violation`='', violations),
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
                                 choices = c(`OSM Mapnik` = "OpenStreetMap.Mapnik", `OSM France` = "OpenStreetMap.France", `OSM Humanitarian` = "OpenStreetMap.HOT", `Stamen Toner` = "Stamen.Toner", `Esri Satellite` = "Esri.WorldImagery", Esri = "Esri.WorldStreetMap", `OSM Dark Matter` = "CartoDB.DarkMatter", `OSM Positron` = "CartoDB.Positron"),
                                 selected = "OpenStreetMap.Mapnik"),
                     uiOutput("filter_UI"),
                     selectInput("filter_select",
                                 "Filter by Area",
                                 c(`Area Type`='', c("Neighborhood", "Council District", "Police Zone", "Fire Zone", "Public Works Division")),
                                 selectize = TRUE,
                                 selected = ""),
                     
                     HTML('</div>')
                     ),
                  # Generate Map
                  div(class="mapBack", style="position: absolute;
                                              width: 100%;z-index: -1;
                                              left: 0px;
                                              top: 55px;", leafletOutput("map")),
                  # Set map to style for Mobile
                  tags$style(type = "text/css", "#map {height: calc(100vh - 115px) !important;}"),
                  tags$head(tags$style(type="text/css", '.mapBack {
                                             background-image: url("loading.png");
                                             background-repeat: no-repeat;
                                             background-position: center;
                                             background-size: contain;}'))
        )
      )
  }
})
  # Filter by Area Display Options
  output$filter_UI <- renderUI({
    if (input$filter_select == "Neighborhood"){
      selectInput("hood_select",
                  label = NULL,
                  c(`Neighborhood`='', levels(load.hoods$hood_1)),
                  multiple = TRUE,
                  selectize=TRUE)
    } else if (input$filter_select == "Public Works Division") {
      selectInput("DPW_select",
                  label = NULL,
                  c(`Public Works Division`='', levels(load.dpw$PUBLIC_WORKS_DIVISION)),
                  multiple = TRUE,
                  selectize=TRUE)
    } else if (input$filter_select == "Police Zone") {
      selectInput("zone_select",
                  label = NULL,
                  c(`Police Zone`='', levels(load.zones$POLICE_ZONE)),
                  multiple = TRUE,
                  selectize=TRUE)
    } else if (input$filter_select == "Council District") {
      selectInput("council_select",
                  label = NULL,
                  c(`Council District`='', levels(load.council$COUNCIL_DISTRICT)),
                  multiple = TRUE,
                  selectize=TRUE)
    } else if (input$filter_select == "Fire Zone") {
      selectInput("firez_select",
                  label = NULL,
                  c(`Fire Zone`='', levels(load.firez$dist_zone)),
                  multiple = TRUE,
                  selectize=TRUE)
    }
  })
  # Boundary Data
  # Neighborhoods
  hoodsInput <- reactive({
    hoods <- load.hoods
    
    if (length(input$hood_select) > 0){
      hoods <- hoods[hoods$hood_1 %in% input$hood_select,]
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
          crashes_sp$hood <- sp::over(crashes_sp, load.hoods)$hood_1
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
    dat311 <- ckanQueryDates("40776043-ad00-40f5-9dc8-1fde865ff571", input$dates[1], input$dates[2], "CREATED_ON")
    
    return(dat311)
  })
  # 311 data with filters
  dat311Input <- reactive({
    # Load 311 Requests
    dat311 <- dat311Load()
    dat311$CREATED_ON <- as.POSIXct(dat311$CREATED_ON, format = '%Y-%m-%dT%H:%M:%S')
    # Clean Geographies
    dat311 <- subset(dat311, select = -REQUEST_ID)
    dat311 <- cleanGeo(dat311, TRUE)
    dat311$date <- as.Date(dat311$CREATED_ON)
    dat311$CREATED_ON <- as.POSIXct(dat311$CREATED_ON, tz = "EST")
    dat311$icon <- as.character(dat311$REQUEST_TYPE)
    dat311$REQUEST_TYPE <- ifelse(dat311$REQUEST_TYPE == "Potholes - 4th Div", "Potholes", dat311$REQUEST_TYPE)
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
    
    # 311 Filters
    if (length(input$dept_select) > 0){
      dat311 <- dat311[dat311$DEPARTMENT %in% input$dept_select,]
    }
    if (length(input$req.type) > 0){
      dat311 <- dat311[dat311$REQUEST_TYPE %in% input$req.type,]
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
    
    dat311 <- subset(dat311, date >= input$dates[1] & date <= input$dates[2])
    
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
  })
  blotterInput <- reactive({
    # Load Blotter
    blotter <- blotterLoad()
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
    
    # Offenses Columns
    blotter$OFFENSES <- as.character(blotter$OFFENSES)
    incidents <- as.data.frame(do.call(rbind, strsplit(blotter$OFFENSES, " / ", fixed = FALSE)))
    blotter <- cbind(blotter, incidents)
    offensesCol <- as.numeric(ncol(blotter))
    offenses1 <- as.numeric(which(colnames(blotter)=="V1"))
    
    # Date filter
    blotter <- subset(blotter, date >= input$dates[1] & date <= input$dates[2])
    
    # Sort
    blotter <- blotter[rev(order(as.Date(blotter$date, format="%d/%m/%Y"))),]
    
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
    
    # Search Filter
    if (!is.null(input$search) && input$search != "") {
      blotter <- blotter[apply(blotter, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(blotter)
  })
  arrestsLoad <- reactive({
    arrests <- ckanQueryDates("e03a89dd-134a-4ee8-a2bd-62c40aeebc6f", input$dates[1], input$dates[2], "ARRESTTIME")
    
    return(arrests)
  })
  # Arrest data with filters
  arrestsInput <- reactive({
    # Load Arrests
    arrests <- arrestsLoad()
    arrests$date <- as.Date(arrests$ARRESTTIME)
    # Unify Neighborhoods
    arrests <- transform(arrests, INCIDENTNEIGHBORHOOD = as.factor(mapvalues(INCIDENTNEIGHBORHOOD, c("Golden Triangle/Civic Arena", "Central Northside", "Mt. Oliver Neighborhood", "Troy Hill-Herrs Island"),
                                                                                       c("Central Business District", "Central North Side", "Mount Oliver", "Troy Hill"))))
    # Clean Geographies
    names(arrests)[names(arrests)=="INCIDENTZONE"] <- "POLICE_ZONE"
    arrests <- cleanGeo(arrests, TRUE)
    
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
    
    return(citations)
  })
  # Citations data with filters
  citationsInput <- reactive({
    citations <- citationsLoad()
    #Fix Date
    citations$date <- as.Date(citations$CITEDTIME)
    # Unify Neighborhoods
    citations <- transform(citations, NEIGHBORHOOD = as.factor(mapvalues(NEIGHBORHOOD, c("Golden Triangle/Civic Arena", "Central Northside", "Mt. Oliver Neighborhood", "Troy Hill-Herrs Island"),
                                                                         c("Central Business District", "Central North Side", "Mount Oliver", "Troy Hill"))))
    # Clean Geographies
    names(citations)[names(citations)=="ZONE"] <- "POLICE_ZONE"
    citations <- cleanGeo(citations, TRUE)

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
    
    return(fires)
  })
  # Fire data with filters
  firesInput <-reactive({
    # Load Fires
    fires <- firesLoad()
    # Clean
    fires$fire_desc <- paste(fires$incident_type, fires$type_description)
    fires$fire_desc <- as.factor(fires$fire_desc)
    
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
    
    return(violations)
  })
  # Code Violations data with filters
  violationsInput <- reactive({
    # Load Violations
    violations <- violationsLoad()
    # Clean
    violations$date <- as.Date(violations$INSPECTION_DATE)
    violations$INSPECTION_RESULT <- as.factor(violations$INSPECTION_RESULT)
    violations <- transform(violations, icon = as.factor(mapvalues(INSPECTION_RESULT, c('Abated','Violations Found','Voided'),
                                                                             c('violations_abated', 'violations_found', 'violations_void'))))
    violations$FullAddress <- paste(ifelse(is.na(violations$STREET_NUM) | is.null(violations$STREET_NUM) | violations$STREET_NUM == 0, "", violations$STREET_NUM) , ifelse(is.na(violations$STREET_NAME) | is.null(violations$STREET_NAME), "", violations$STREET_NAME))
    # Create Parcel URL
    violations$full_address <- paste(violations$STREET_NUM, violations$STREET_NAME)
    violations$url <-  paste0('<a href="http://www2.county.allegheny.pa.us/RealEstate/GeneralInfo.aspx?ParcelID=',violations$PARCEL, '" target="_blank">', violations$PARCEL, '</a>')
    # Prepare 
    violations1 <- ncol(violations) + 1
    list <- as.data.frame(do.call(rbind, strsplit(violations$VIOLATION, ":: ", fixed = FALSE)))
    violations <- cbind(violations, list)
    violationsCol <- ncol(violations)
    violations$VIOLATION <- as.character(violations$VIOLATION)
    violations$VIOLATION <- gsub("::", "/", violations$VIOLATION)
    violations$CORRECTIVE_ACTION <- gsub("::", "/", violations$CORRECTIVE_ACTION)
    
    # Clean Geographies
    violations <- cleanGeo(violations, TRUE)

    # Sort
    violations <- violations[rev(order(as.Date(violations$date, format="%d/%m/%Y"))),]
    
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
  # Building Permits data with filters
  permitsLoad <- reactive({
    permits <- ckanQueryDates("95d69895-e58d-44de-a370-fec6ad2b332e", input$dates[1], input$dates[2], "status_date")
    
    return(permits)
  })
  permitsInput <- reactive({
    # Load Permit Layer
    permits <- permitsLoad()
    # Clean Date
    permits$date <- as.Date(permits$intake_date)
    # Full address clean
    permits$full_address <- paste0(ifelse(is.na(permits$street_address) | is.null(permits$street_address), "", paste0(as.character(permits$street_address), " ")),
                                        ifelse(is.na(permits$city) | is.null(permits$city), "",  paste0(permits$city, ", ")),
                                        ifelse(is.na(permits$state) | is.null(permits$state), "",  paste0(permits$state, " ")),
                                        ifelse(is.na(permits$zip) | is.null(permits$zip), "",  paste0(permits$zip, " ")))
    types <- as.data.frame(do.call(rbind, strsplit(permits$permit_type, " - ", fixed = FALSE)))
    permits$primary_type <- types$V1
    permits$record_category <- ifelse(is.na(permits$record_category), "None", permits$record_category)
    permits$current_status <- as.factor(permits$current_status)
    permits$lat <- as.numeric(permits$lat)
    permits$lon <- as.numeric(permits$lon)
    permits$permit_type <- as.factor(permits$permit_type)
    permits$neighborhood <- as.factor(permits$neighborhood)
    
    # Create County Parcel viewer link
    permits$url <-  paste0('<a href="http://www2.county.allegheny.pa.us/RealEstate/GeneralInfo.aspx?ParcelID=',permits$parcel_id, '" target="_blank">', permits$parcel_id, '</a>')
    
    permits <- transform(permits, icon = as.factor(mapvalues(primary_type, permit_types,
                                                                       c('appeals', 'building_permit', 'communication_tower', 'demolition_permit', 'electrical_permit', 'fire_alarm', 'HVAC_permit', 'land_operations', 'occupancy', 'occupant_load_placard', 'sign_permit', 'sprinkler_permit', 'temp_occupancy', 'temp_occupancy'))))
    # Clean Geograhies
    permits <- cleanGeo(permits)

    # Sort
    permits <- permits[rev(order(as.Date(permits$date, format="%d/%m/%Y"))),]

    # Permit Filters
    if (length(input$permit_select) > 0) {
      permits <- permits[permits$permit_type %in% input$permit_select,]
    }
    if (length(input$status_select) > 0) {
      permits <- permits[permits$current_status %in% input$status_select,]
    }
    if (length(input$category_select) > 0) {
      permits <- permits[permits$record_category %in% input$category_select,]
    }

    # Geographic Filters
    if (length(input$zone_select) > 0 & input$filter_select == "Police Zone"){
      permits <- permits[permits$police_zone %in% input$zone_select,]
    } else if (length(input$hood_select) > 0 & input$filter_select == "Neighborhood") {
      permits <- permits[permits$neighborhood %in% input$hood_select,]
    } else if (length(input$DPW_select) > 0 & input$filter_select == "Public Works Division") {
      permits <- permits[permits$public_works_division %in% input$DPW_select,]
    } else if (length(input$council_select) > 0 & input$filter_select == "Council District") {
      permits <-permits[permits$council_district %in% input$council_select,]
    } else if (length(input$firez_select) > 0 & input$filter_select == "Fire Zone") {
      permits <- permits[permits$fire_zone %in% input$firez_select,]
    }

    # Search Filter
    if (!is.null(input$search) && input$search != "") {
      permits <- permits[apply(permits, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }

    # Append Workflows
    workflow <- load.workflow
    # Select Workflows
    workflow <- workflow[workflow$permit_id %in% permits$permit_id,]
    workflow$permit_id <- as.factor(workflow$permit_id)

    if (nrow(permits) > 0) {
      # Loop which aggregates appropriate Workflows
      for (i in  levels(workflow$permit_id)){
        # Isolate Workflows for Permit ID
        temp <- subset(workflow, permit_id == i)
        # Sort workflow to correct order
        temp <- temp[order(temp$history_seq_nbr),]
        # Isolate only tooltip
        temp <- temp[,"tool"]
        # Create DT string from list
        tt <- paste0('<br><b>Workflow:</b><br><dl style="margin-bottom: 0px; margin-left:10px";>', toString(temp), "</dl>")
        # Remove Junk characters from unlisting
        tt <- gsub(",", "", tt)
        tt <- gsub('" "', "", tt)
        tt <- gsub('c\\("', "", tt)
        tt <- gsub('"\\)', "", tt)
        # Create Columns for bind
        df <- data.frame(i,tt)
        # Check for first Workflow
        if (i == levels(workflow$permit_id)[1]){
          tt.df <- df
        # Merge to other tooltips
        } else {
          tt.df <- rbind(tt.df, df)
        }
      }

    # Rename Columns for Merge
    colnames(tt.df) <- c("permit_id", "tt")
    # Merge Workflow Tooltip to Permits
    permits <- merge(permits, tt.df, by = "permit_id", all.x = TRUE)
    # Make Unsuccessful tooltips blank instead of NA
    permits$tt <- as.character(permits$tt)
    permits$tt[is.na(permits$tt)] <- ""
    }

    return(permits)
  })
  # Capital Projects data with filters
  cprojLoad <- reactive({
    # Capital Projects
    year1 <- format(as.Date(input$dates[1]), "%Y")
    year2 <- format(as.Date(input$dates[2]), "%Y")
    cproj <- ckanQuery2("2fb96406-813e-4031-acfe-1a82e78dc33c", year1, "fiscal_year", "OR", year2, "fiscal_year")
    
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
      
      # Formatting
      cproj$budgeted_amount <- dollarsComma(as.numeric(cproj$budgeted_amount))
      
      cproj <- transform(cproj, icon = as.factor(mapvalues(area, functional_areas, c("administration", "engineering_construction", "facility_improvement", "neighborhood_development", "public_safety", "vehicles_equipment"))))
      
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
      dat311 <- subset(dat311, select = c(REQUEST_TYPE, DEPARTMENT, CREATED_ON, NEIGHBORHOOD, COUNCIL_DISTRICT, POLICE_ZONE, PUBLIC_WORKS_DIVISION))
      
      # Rename columns for humans
      colnames(dat311) <- c("Request Type", "Dept", "Create Date", "Neighborhood", "Council District", "Police Zone",  "Public Works Division")
      
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

      fires <- subset(fires, select = c(call_no, fire_desc, alarm_time, arrival_time, primary_unit, alarms, address, fire_zone, neighborhood, council_district))
      colnames(fires) <- c("Call #", "Type", "Alarm Time", "Arrival Time", "Primary Unit", "Alarms", "Location", "Fire Zone", "Neighborhood", "Council District")
      
      report <- fires
    } else if (input$report_select == "Building Permits") {
      permits <- permitsInput()
      
      permits <- subset(permits, select = c(permit_type, current_status, intake_date, issued_date, full_address, neighborhood, council_district, police_zone, public_works_division, permit_id, url))
      
      colnames(permits) <- c("Type",  "Status", "Intake Date",  "Issued Date", "Address", "Neighborhood", "Council District", "Police Zone", "Public Works Division", "Permit ID", "Parcel ID")
      
      report <- permits
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
    }
    # Return Data
    report
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
    if (url.exists(paste0(couchdb_url, ":5984/_utils/"))){
      dateTime <- Sys.time()
      names(dateTime) <- "dateTime"
      inputs <- isolate(reactiveValuesToList(input))
      couchDB$dataList <- c(inputs, sessionID, dateTime, sessionStart)
      cdbAddDoc(couchDB)
    }
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
  # Build main map
  output$map <- renderLeaflet({
    recs <- 0
    map <- leaflet() %>% 
      addProviderTiles(input$basemap_select,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    # Boundary Layers
    # Neighborhoods
    if (input$filter_select == "Neighborhood") {
      hoods <- hoodsInput()
      if (nrow(hoods) > 0) {
        map <- addPolygons(map, data = hoods,
                           stroke = TRUE, smoothFactor = 0, weight = 1, color = "#000000", opacity = 0.6,
                           fill = TRUE, fillColor = "#00FFFFFF", fillOpacity = 0, 
                           popup = ~paste("<font color='black'><b>Neighborhood:</b> ", htmlEscape(hood_1), "</font>")
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
      # Building Permits Layer
      # if(input$togglePermits) {
      #   permits <- permitsInput()
      #   # Remove unmappables
      #   permits <- permits[!(is.na(permits$lat)),]
      #   permits <- permits[!(is.na(permits$lon)),]
      #   permits <- subset(permits, lon > -80.242767 & lon < -79.660492 & lat < 40.591014 & lat > 40.266428)
      #   
      #   permits <- permits[,c("lon", "lat")]
      #   colnames(permits) <- c("X", "Y")
      #   allData <- rbind(permits, allData)
      # }
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
                                            "<br><b>Arrival Time:</b>", fires$arrival_time,
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
      # Building Permits Layer
      # if(input$togglePermits) {
      #   permits <- permitsInput()
      #   # Remove unmappables
      #   permits <- permits[!(is.na(permits$lat)),]
      #   permits <- permits[!(is.na(permits$lon)),]
      #   permits <- subset(permits, lon > -80.242767 & lon < -79.660492 & lat < 40.591014 & lat > 40.266428)
      #   if (nrow(permits) > 0) {
      #     map <- addMarkers(map, data=permits,
      #                       clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {
      #                                                                                   var childCount = cluster.getChildCount();
      #                                                                                   if (childCount < 10) {
      #                                                                                   c = 'rgba(207, 242, 252, 0.95);'
      #                                                                                   } else if (childCount < 100) {
      #                                                                                   c = 'rgba(117, 214, 247, 0.95);'
      #                                                                                   } else {
      #                                                                                   c = 'rgba(0, 150, 219, 0.95);'
      #                                                                                   }
      #                                                                                   return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
      #   }")), ~lon, ~lat, icon = ~icons_permits[icon],
      #            popup = ~(paste("<font color='black'><b>Type:</b>", permits$permit_type,
      #                            "<br><b>Status:</b>", permits$current_status,
      #                            "<br><b>Address:</b>", permits$full_address,
      #                            "<br><b>Neighborhood:</b>", permits$neighborhood,
      #                            "<br><b>Council District:</b>", permits$council_district,
      #                            "<br><b>Police Zone:</b>", permits$police_zone,
      #                            "<br><b>Public Works Division:</b>", permits$public_works_division,
      #                            "<br><b>Parcel ID:</b>", permits$url,
      #                            "<br><b>Permit ID:</b>", permits$permit_id,
      #                            permits$tt,
      #                            '<br><center><a href="https://pittsburghpa.buildingeye.com/building" target="_blank">Search Permits on Building Eye!</a></center></font></font>'))
      #     )
      #   recs <- recs + nrow(permits)
      #   }
      # }
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
    }
    print(recs)
    if (recs < 1) {
      
      if (Sys.Date() == eDay) {
        egg <- load.egg
      } else {
        egg <- load.egg[sample(1:nrow(load.egg),1),]
      }
      
      map <- addMarkers(map, data=egg, ~X, ~Y, icon = ~icons_egg[icon], popup = ~tt) %>% 
          setView(-79.9959, 40.4406, zoom = 10)
    }
    #Write inputs to Couch
    if (url.exists(paste0(couchdb_url, ":5984/_utils/"))){
      dateTime <- Sys.time()
      names(dateTime) <- "dateTime"
      inputs <- isolate(reactiveValuesToList(input))
      couchDB$dataList <- c(inputs, sessionID, dateTime, sessionStart)
      cdbAddDoc(couchDB)
    }
    #Generate Map
    map
    })
  })

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
