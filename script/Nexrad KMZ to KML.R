#Nexrad locations


# extract long-lat coordinates from a bunch of kmz files
# first unzips the KMZ-s then reads coordinates from each KML with getKMLcoordinates {maptools}
# (worth checking this as well: https://gist.github.com/holstius/6631918 )

library(maptools)

# list the kmz files in a given folder path
KMZs <- list.files(path="./data/", pattern="*.kmz", full.names=FALSE)

# unzip each KMZ file and read coordinates with getKMLcoordinates()
# select only the matrix element of the list returned by getKMLcoordinates(), 
# therefore mention the index [[1]]
LonLat <- sapply(KMZs, 
                 function(x) 
                   getKMLcoordinates(kmlfile= unzip(zipfile = paste0("./data/", x),
                                                            exdir   = "./data/KML/"), 
                                     ignoreAltitude = TRUE)[[1]])
library(sf)
nexrad <- st_read("./data/KML/nexrad.kml")


### NOT RUN BELOW ####
# To get altitude from KML, then mention ignoreAltitude=FALSE                

# delete the .kmz part from the column names in LonLat matrix
colnames(LonLat) <- gsub(pattern=".kmz", replacement="", x=colnames(LonLat))

# give names to rows
rownames(LonLat) <- c("Longitude", "Latitude")

# transpose the matrix for readability reasons
LonLat <- t(LonLat)