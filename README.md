# Burghs Eye View - Points
<p align="center"><img src="http://apps.pittsburghpa.gov/cis/burgh-seye-icon.png"></p>
Burgh's Eye View is the core application used by Pittsburgh I&P's Analytics & Strategy team for mapping open data for point based data.

## Introduction 

### Authors: 
* Geoffrey Arnold
* Nicholas Hall
* Maxwell Cercone
  
### Collaborators:
* Tara Matthews
* Robert Burrack
* Dee Jones
  
### Acknowledgements & Thanks
This application was made possible by a wide number of individuals within and throughout the City of Pittsburgh. First, Laura Meixell for her leadership in seeing this application through to public release and beyond. To Former Chief of Police Cameron McLay for his commitment to Open Data and ensuring the Police are one of our biggest contributors. To Former Chief Innovation Officer for the City of Pittsburgh Debra Lam for her executive sponsorship. To Director Finance Paul Leger for his trasnformative leadership and vision. To our primary funder, the Heinz Endowements for their ongoing support. To all of our friends and family for their compassion and understanding through our late nights and frenzied mornings. To Mayor William Peduto for making the City of Pittsburgh a place where innovative work like this is possible. Finally to the Citizens of Pittsburgh for their generous support of this application since release and their feedback through the 32 Community Meetings our team was able to attend.
  
## Installation & Configuration
Burgh's Eye View is a Shiny application built on a single app file, and therefore only requires R Shiny Server and its dependencies to run. It is hosted through RStudio's shinyapps.io platform but can be run on a local device. The repository as currently configured is ready to deploy minus a few necessary variables supplied by the user.

## Contents
* boundaries/Parks/parks.csv: Centroid locations of City parks for easter egg icons.
* icons/: All subfolders contain the icons for their respective layer on Burgh's Eye View
* key_example.json: Example file for users to input their own credentials for the WPRDC and a CouchDB of their choice.
* app.R: Main application
* MISSING: tag-manager-header/body.js and google-analytics.js
