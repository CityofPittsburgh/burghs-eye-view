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
load311 <- read.csv("311data.csv")

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