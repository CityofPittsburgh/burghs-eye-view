require(httr)
require(jsonlite)
require(R4CouchDB)
require(curl)
require(rgdal)
require(geojsonio)
require(plyr)
require(tools)

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

ckanGEO <- function(url) {
  r <- GET(url, add_headers(Authorization = ckan_api))
  c <- content(r, as ="text")
  readOGR(c, "OGRGeoJSON", verbose = F)
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

addUpdateDoc("offenses", levels(as.factor(offenseList$lvls)))

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

addUpdateDoc("violations", levels(as.factor(violationList$lvls)))

# Building Permits
permits <- ckan("95d69895-e58d-44de-a370-fec6ad2b332e")

permit_status <- unique(permits$current_status)

addUpdateDoc("permit_status", permit_status)

# Council List
council_list <- c("1: Darlene Harris", "2: Theresa Kail-Smith", "3: Bruce Kraus", "4: Anthony Coghill", "5: Corey O'Connor", "6: R. Daniel Lavelle", "7: Deb Gross", "8: Vacant", "9: Reverend Ricky Burgess")

addUpdateDoc("council_list", council_list)

# Places Updates

# Park Types
parks <- geojson_read("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/e95593cb0a2d4ff194be9694b40614dc_0.geojson", what = "sp")
park_types <- levels(as.factor(parks$final_cat))
addUpdateDoc("park_types", park_types)

#Trees
all <- ckanGEO("https://data.wprdc.org/dataset/9ce31f01-1dfa-4a14-9969-a5c5507a4b40/resource/d876927a-d3da-44d1-82e1-24310cdb7baf/download/treesimg.geojson")
all <- all[all@data$scientific_name != "_CALLS",]

# Make Icon
all$icon <- tolower(as.character(all$common_name))

sites <- all[grepl("vacant", all$icon) | all$icon == "non-sufficient space" | all$icon == "stump",]
all$common_name <- as.character(all$common_name)
all$common_name <- as.factor(all$common_name)

trees <- all[!grepl("vacant", all$scientific_name, ignore.case = T),]
trees <- trees[trees$icon != "non-sufficient space",]
trees <- trees[trees$icon != "stump",]
trees@data$common_name <- as.character(trees@data$common_name)
trees@data$common_name <- as.factor(trees@data$common_name)
trees$scientific_name <- as.character(trees$scientific_name)
trees$scientific_name <- as.factor(trees$scientific_name)
trees$overhead_utilities <- as.character(trees$overhead_utilities)
trees$overhead_utilities <- as.factor(trees$overhead_utilities)

comname_select <- levels(trees$common_name)
addUpdateDoc("comname_select", comname_select)

sciname_select <- levels(trees$scientific_name)
addUpdateDoc("sciname_select", sciname_select)

grow_select <- levels(trees$growth_space_type)
addUpdateDoc("grow_select", grow_select)

landuse_select <- levels(trees$land_use)
addUpdateDoc("landuse_select", landuse_select)

utils_select <- levels(trees$overhead_utilities)
addUpdateDoc("utils_select", utils_select)

type_select <- levels(sites$common_name)
addUpdateDoc("type_select", type_select)