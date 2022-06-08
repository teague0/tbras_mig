library(tidyverse)

bracken <- c("bracken", -98.35252860731364,29.68707713634115)
frio <- c("frio", -99.66489740325156,29.445546581914538)
bamberger <- c("bamberger", -98.45303505940065,30.199141208878444)
selman <- c("selman", -99.2581519596492,36.71265289494823)
pinegrove <- c("pinegrove", -90.75445949746045,30.708510385509776)
cdelguano <- c("cueva del guano",-103.532333, 25.374667) #Cueva del Guano, Durango, López-González et al 2010

tbrasSites <- rbind(bracken,
                    frio,
                    bamberger,
                    selman,
                    pinegrove,
                    cdelguano)
tbrasSites <- as.data.frame(tbrasSites)
names(tbrasSites) <- c("cave", "long", "lat")
tbrasSites$long <- as.numeric(tbrasSites$long)
tbrasSites$lat <- as.numeric(tbrasSites$lat)

russel2005 <- read.csv("./data/Russel2005Caves.csv")

angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

russel2005$lat <- angle2dec(russel2005$lat)
russel2005$long <- angle2dec(russel2005$long)

motus <- read.csv("./data/MOTUS_receiver-deployments.csv")
ctt <- motus %>% filter(receiverType == "SENSORSTATION")

library(rnaturalearth)
library(sf)

states <- rnaturalearth::ne_states(c("united states of america", "mexico"))
states <- st_as_sf(states)

states <- states %>% filter(name != c("Alaska", "Hawaii"))
states_s <- st_simplify(states, preserveTopology = FALSE, dTolerance = 10000)
studyStates <- states %>% filter(name %in% c("Louisiana", "Oklahoma", "Texas", "Sonora", "Chihuahua", "Coahuila", "Nuevo Leon", "Tamaulipas", "San Luis Potosí", "Zacatecas", "Durango", "Sinaloa", "Nayarit", "Jalisco", "Aguascalientes", "Guanajuato", "Querétaro", "Hidalgo", "Veracruz","Michoacán"))
studyStates_s <- st_simplify(studyStates, preserveTopology = FALSE, dTolerance = 10000)
usSites <- states %>% filter(name %in% c("Louisiana", "Oklahoma", "Texas"))
nexrad <- st_read("./data/KML/nexrad.kml")
states_bbox <- st_bbox(studyStates)
xlim <- c(states_bbox[1],states_bbox[3])
ylim <- c(states_bbox[2],states_bbox[4])

ggplot()+
  geom_sf(data = studyStates_s, fill = NA)+
  geom_sf(data = nexrad)+
  geom_point(data = russel2005, aes(x = long, y= lat), color = "red", size = 4, alpha = 0.6)+
  geom_point(data = tbrasSites, aes(x = long, y = lat), color = "red", size = 4, alpha = 0.6)+
  coord_sf(xlim = xlim, ylim = ylim)+
  theme_bw()

#read in bat ranges to extract TABR range
library(sf)
# batDists <- st_read("~/ownCloud/projects/bat migration phylogeny/Bat Species Range Maps/MDD_Chiroptera/MDD_Chiroptera.gpkg") 
# tabr <- batDists %>% filter(sciname == "Tadarida brasiliensis")
# rm(batDists)
# st_write(tabr, dsn = "tabr.shp", layer = "tabr.shp", driver = "ESRI Shapefile")
# st_write(tabr, dsn = "tabr.gpkg", driver = "GPKG")


library(tidyverse)
library(sf)
tabr <- st_read("./data/tabrRange/tabr.shp") #sum(rapply(st_geometry(tabr), nrow)) = 3277130
#Simplify the giant multipolygons to 10 km
tabr_s <- st_simplify(tabr, preserveTopology = FALSE, dTolerance = 10000) #sum(rapply(st_geometry(tabr_s), nrow)) = 2324
rm(tabr)


sum(rapply(st_geometry(tabr_s), nrow))
sum(rapply(st_geometry(tabr), nrow))


states_bbox <- st_bbox(states_s)
xlim <- c(states_bbox[1],states_bbox[3])
ylim <- c(states_bbox[2],states_bbox[4])

ggplot()+
  geom_sf(data = tabr_s, fill = "red", alpha = 0.2)+
  geom_sf(data = states_s, fill = NA)+
  coord_sf(xlim = xlim, ylim = ylim)+
  theme_bw()

ggsave("./output/tabr_rangeMap.png", range)


ggplot()+
  geom_sf(data = tabr, fill = "red", alpha = 0.5)
