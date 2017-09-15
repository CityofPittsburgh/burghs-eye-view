require(httr)
require(jsonlite)
require(R4CouchDB)
require(curl)

httr::set_config(config(ssl_verifypeer = 0L))

ckan_api <- jsonlite::fromJSON("key_webhost.json")$ckan_api
couchdb_un <- jsonlite::fromJSON("key_webhost.json")$couchdb_un
couchdb_pw <- jsonlite::fromJSON("key_webhost.json")$couchdb_pw

# Function to download WPRDC Data
ckan <- function(id) {
  x <- paste0("https://data.wprdc.org/datastore/dump/", id)
  r <- GET(x, add_headers(Authorization = ckan_api))
  content(r)
}

addUpdateDoc <- function (id, data) {
  conn <- cdbIni(serverName = "webhost.pittsburghpa.gov", port = "5984", uname = couchdb_un, pwd = couchdb_pw, DBName = "bev-inputs")
  names(data) <- 1:length(data)
  conn$dataList <- data
  conn$id <- id
  rurl <- paste0("http://webhost.pittsburghpa.gov:5984/bev-inputs/", conn$id)
  rg <- GET(rurl, authenticate(couchdb_un, couchdb_pw), timeout(60))
  if (is.null(rg$header$etag)) {
    cdbAddDoc(conn)
  } else {
    cdbUpdateDoc(conn)
  }
}

# Load 311 Data
load311 <- ckan("40776043-ad00-40f5-9dc8-1fde865ff571")

# 311 Inputs List
request_types <- levels(load311$REQUEST_TYPE)
addUpdateDoc("request_types", request_types)
departments <- levels(load311$DEPARTMENT)
addUpdateDoc("departments", departments)
origins <- levels(load311$REQUEST_ORIGIN)
addUpdateDoc("origins", origins)

# Police Blotter & Archive
archive <- ckan("044f2016-1dfd-4ab0-bc1e-065da05fca2e")
thirty <- ckan("1797ead8-8262-41cc-9099-cbc8a161924b")
# Clean for Merge
thirty <- thirty[,1:ncol(thirty)]
archive <- archive[,c(colnames(thirty))]
# Merge
blotter <- rbind(archive, thirty)

# Offenses Selection
incidents <- as.data.frame(do.call(rbind, strsplit(blotter$OFFENSES, " / ", fixed = FALSE)))
blotter <- cbind(blotter, incidents)
offensesCol <- as.numeric(ncol(blotter))
offenses1 <- as.numeric(which(colnames(blotter)=="V1"))

offenses <- blotter[,offenses1:offensesCol]

for (i in 1:ncol(offenses)) {
  # Apply character
  lvls <- as.character(offenses[,i])
  
  if (i == 1) {
    # Create list beginning
    offenseList <- as.data.frame(lvls)
  } else {
    # Append List
    offenseList <- rbind(lvls, offenseList)
  }
  # Trim list
  offenseList <- unique(offenseList)
}

addUpdateDoc("offenses", levels(offenseList$lvls))

# Violations
violations <- ckan("4e5374be-1a88-47f7-afee-6a79317019b4")
violations1 <- ncol(violations) + 1
list <- as.data.frame(do.call(rbind, strsplit(violations$VIOLATION, " :: ", fixed = FALSE)))
violations <- cbind(violations, list)
violationsCol <- ncol(violations)

violations <- violations[,violations1:violationsCol]

for (i in 1:ncol(violations)) {
  # Apply character
  lvls <- as.character(violations[,i])
  
  if (i == 1) {
    # Create list beginning
    violationList <- as.data.frame(lvls)
  } else {
    # Append List
    violationList <- rbind(lvls, violationList)
  }
  # Trim list
  violationList <- unique(violationList)
} 

addUpdateDoc("violations", levels(violationList$lvls))

# Building Permits
permits <- ckan("95d69895-e58d-44de-a370-fec6ad2b332e")

permit_status <- unique(permits$current_status)

addUpdateDoc("permit_status", permit_status)

# Fire Incidents
fires <- ckan("8d76ac6b-5ae8-4428-82a4-043130d17b02")

fires$fire_desc <- paste(fires$incident_type, fires$type_description)

fire_desc <- unique(fires$fire_desc)

addUpdateDoc("fire_desc", fire_desc)

council_list <- c("1: Darlene Harris", "2: Theresa Kail-Smith", "3: Bruce Kraus", "4: Natalia Rudiak", "5: Corey O'Connor", "6: R. Daniel Lavelle", "7: Deb Gross", "8: Dan Gilman", "9: Reverend Ricky Burgess")

addUpdateDoc("council_list", council_list)

# Places Updates

# Load facilities
facilities <- ckanGEO("https://data.wprdc.org/dataset/e33e12d9-1268-45ed-ae47-ae3a76dcc0aa/resource/fd532423-b0ec-4028-98ff-5d414c47e01a/download/facilitiesimg.geojson")
# Create Adress Column (checks to see if Address No. is valid, to add number and add space between street name)
facilities@data$address <- paste0(ifelse(is.na(facilities@data$address_number), "", paste0(as.character(as.integer(facilities@data$address_number)), " ")), ifelse(is.na(facilities@data$street), "", as.character(facilities@data$street)))
facilities@data$type <- as.factor(facilities@data$type)
# Clean Facility Type for humans
facilities@data <- transform(facilities@data, usage = as.factor(mapvalues(type, c("ACTIVITY", "CABIN", "COMMUNITY", "CONCESSION", "DUGOUT", "FIREHOUSE" , "MEDIC STATION", "OFFICE", "POLICE", "POOL", "POOL CLOSED", "POOL/REC", "REC", "RECYCLING", "RESTROOMS", "SALT DOME", "SENIOR", "SERVICE", "SHELTER", "STORAGE", "TRAINING", "UTILITY", "VACANT", NA),
                                                                                    c("Activity", "Cabin", "Community", "Concession", "Dugout", "Firehouse", "Medic Station", "Office", "Police", "Pool", "Pool - Closed", "Pool/Recreation", "Recreation", "Recycling", "Restrooms", "Salt Dome", "Senior Center", "Service", "Shelter", "Storage", "Training", "Utility", "Vacant", "Storage"))))
# Create Tooltip
facilities@data$rentable <- as.factor(facilities@data$rentable)
facilities@data$url <- ifelse(facilities@data$rentable == 1, '<br><center><a href="https://registerparks.pittsburghpa.gov/" target="_blank">Rent this facility</a></center>', "")
facilities@data$rentable <- ifelse(facilities@data$rentable == 1, "Yes", "No")

facilities <- facilities[facilities$inactive == 0,]
facilities@data$usage <-as.character(facilities@data$usage)

# Rec Centers
recfacilities <- facilities[facilities@data$usage %in%  c("Activity", "Recreation", "Dugout", "Pool/Recreation", "Concession"),]
recfacilities@data$usage <- as.factor(recfacilities@data$usage)

# Pools Facilities
poolsfacilities <- facilities[facilities@data$usage %in%  c("Pool", "Pool - Closed"),]
poolsfacilities@data$usage <- as.factor(poolsfacilities@data$usage)

# Remove Stuff
facilities <- facilities[!facilities@data$usage %in%  c("Activity", "Recreation", "Dugout", "Pool/Recreation", "Pool", "Pool - Closed"),]
facilities@data$usage <- as.character(facilities@data$usage)
facilities@data$usage <- as.factor(facilities@data$usage)

# Rec Types
courts <- ckanGEO("https://data.wprdc.org/dataset/8da92664-22a4-42b8-adae-1950048d70aa/resource/96d327a8-fb12-4174-a30d-7ec9a9920237/download/courtsimg.geojson")
fields <- ckanGEO("https://data.wprdc.org/dataset/87c77ec3-db98-4b2a-8891-d9b577b4c44d/resource/d569b513-44c0-4b65-9241-cc3d5c506760/download/fieldsimg.geojson")
parks <- geojson_read("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/e95593cb0a2d4ff194be9694b40614dc_0.geojson", what = "sp")

# Rec Tyoes
rec_types <- sort(c("Greenway", levels(courts@data$type), levels(fields@data$field_usage), levels(recfacilities@data$usage), levels(parks$final_cat), "Playground"))
addUpdateDoc("rec_types", rec_types)

# Load Pools
# Load Water Features
wf <- ckanGEO("https://data.wprdc.org/dataset/fe7cfb11-9f33-4590-a5ee-04419f3f974a/resource/f7c252a5-28be-43ab-95b5-f3eb0f1eef67/download/wfimg.geojson")
# Remove Inactive Water Features
wf <- wf[wf$inactive == 0,]
# Prepare for Merge to Facilities
wf@data <- transform(wf@data, feature_type = as.factor(mapvalues(feature_type, c("Spray", "Decorative"), c("Spray Fountain", "Decorative Water Fountain"))))
wf$feature_type <- as.character(wf$feature_type)

# Load Spray
spray <- wf[wf@data$feature_type == "Spray Fountain",]
spray@data$feature_type <- as.factor(spray@data$feature_type)

# Remove Spray
wf <- wf[wf@data$feature_type != "Spray Fountain",]
wf@data$feature_type <- as.factor(wf@data$feature_type)

# Facility Usage
facility_usage <- sort(c(levels(wf$feature_type), levels(facilities$usage)))
addUpdateDoc("facility_usage", facility_usage)

# Load Pools
pools <- ckanGEO("https://data.wprdc.org/dataset/f7067c4e-0c1e-420c-8c31-f62769fcd29a/resource/77288f26-54a1-4c0c-bc59-7873b1109e76/download/poolsimg.geojson")

# Pool Categories
pool_cat <- sort(unique(c(levels(pools$type) ,levels(poolsfacilities@data$usage), levels(spray$feature_type))))
addUpdateDoc("pool_cat", pool_cat)

# Intersections
# Load Signalized Intersections
si <- ckanGEO("https://data.wprdc.org/dataset/f470a3d5-f5cb-4209-93a6-c974f7d5a0a4/resource/82ce557f-2388-489f-87e0-0d9d052633c4/download/siimg.geojson")
# Clean
si@data$description <- gsub("_", " ", si@data$description)
si@data$description <- toTitleCase(tolower(si@data$description))
si@data$description <- gsub("Osm", "OSM", si@data$description, ignore.case = TRUE)
si@data$flash_yellow <- ifelse(is.na(si@data$flash_yellow), NA, toTitleCase(tolower(si@data$flash_yellow)))
si@data$operation_type <- as.character(si@data$operation_type)
si@data$operation_type <- paste("Traffic Signal -", si@data$operation_type)
si@data$operation_type <- ifelse(si@data$operation_type == "Traffic Signal - ", "Traffic Signal - Other", si@data$operation_type)
si@data$operation_type <- as.factor(si@data$operation_type)
si@data$flash_time <- as.factor(si@data$flash_time)

# Load Crosswalks
cw <- ckanGEO("https://data.wprdc.org/dataset/31ce085b-87b9-4ffd-adbb-0a9f5b3cf3df/resource/f86f1950-3b73-46f9-8bd4-2991ea99d7c4/download/crosswalksimg.geojson")

intersection_type <- sort(c(levels(cw$type), levels(si$operation_type)))
addUpdateDoc("intersection_type", intersection_type)

flash_times <- levels(si$flash_time)
addUpdateDoc("flash_times", flash_times)

# Steps Walls

# Load City Steps
steps <- ckanGEO("https://data.wprdc.org/dataset/e9aa627c-cb22-4ba4-9961-56d9620a46af/resource/ff6dcffa-49ba-4431-954e-044ed519a4d7/download/stepsimg.geojson")
steps@data$installed<-  as.numeric(format(as.Date(steps@data$installed), "%Y"))

# Load Retaining Walls
walls <- ckanGEO("https://data.wprdc.org/dataset/5e77546c-f1e1-432a-b556-9ccf29db9b2c/resource/b126d855-d283-4875-aa29-3180099090ec/download/retainingwallsimg.geojson")
walls$image <- as.character(walls$image)

ft_max <- max(c(steps$length, walls$length), na.rm = TRUE)
ft_min <- min(c(steps$length, walls$length), na.rm = TRUE)

ft_select <- c(ft_min, ft_max)
addUpdateDoc("ft_select", ft_select)
