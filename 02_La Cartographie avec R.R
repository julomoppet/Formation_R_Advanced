###############################################################################
#################### Déclaration des librairies
###############################################################################
library(sf)
library(tidyverse)
library(raster)
library(sp)
library(geojsonsf)
library(banR)
library(mapview)
library(tmap)
library(leaflet)
library(raster)

setwd("~/R_Advanced/")

###############################################################################
#################### Exemple 1 : Représentation des magasins Norauto 
###############################################################################

france <- raster::getData(country="FRA", level="0")
lim_bbox <- sp::bbox(france)

# utiliser cette bbox sur https://overpass-turbo.eu/
paste(lim_bbox[2,1],lim_bbox[1,1],lim_bbox[2,2],lim_bbox[1,2], sep = ",")

# magasins Norauto --------------------------------------

#   [out:json][timeout:200];
# (
#   node["brand:wikidata"="Q3317698"]["shop"="car_repair"](41.33375,-5.143751,51.0894,9.560416);
#   way["brand:wikidata"="Q3317698"]["shop"="car_repair"](41.33375,-5.143751,51.0894,9.560416);
#   relation["brand:wikidata"="Q3317698"]["shop"="car_repair"](41.33375,-5.143751,51.0894,9.560416);
# );
# out body;
# >;
# out skel qt;


# puis exporter / importer en geojson
# norauto <- geojson_sf("data/norauto.geojson")
norauto <- readRDS("norauto.rds")

norauto <- norauto %>% dplyr::select(geometry)
# on ne conserve que les centroides
norauto <- sf::st_centroid(norauto)

# on va faire du reverse geocoding pour retrouver les adresses
norauto_df <- st_coordinates(norauto) %>% 
  as.data.frame() %>%
  dplyr::rename(lon=X, lat=Y)
norauto_addresses <- norauto_df %>% 
  reverse_geocode_tbl(lon, lat)
glimpse(norauto_addresses)

# fusion dans l'objet sf original
keep.vars <- c("result_id",
               "result_housenumber","result_name",
               "result_postcode","result_city")
norauto_addresses <- norauto_addresses %>%
  dplyr::select(one_of(keep.vars))
norauto <- norauto %>% 
  bind_cols(norauto_addresses) %>%
  filter(!is.na(result_id))


# carte avec mapview
mapview(norauto)




###############################################################################
#################### Exemple 2 : Carte du monde interactive avec tmap
###############################################################################
#tmap apporte quelques fonctionnalités int?ressantes par rapport à ggplot2, 
# notamment l'interactivité et l'affichage en fonction des modalités d'une variable

#Carte interactive
#Les cartes deviennent interactives facilement avec tmap_mode("view")
#Exemple : carte interactive du monde.
#La variable "economy" est utilis?e pour la couleur des pays en utilisant la fonction tm_fill().
data("World")
tmap_mode("view")
tm_shape(World) + tm_fill(col = "economy") + tm_borders("black")


#Représentation en fonction de plusieurs variables
#On peut afficher plusieurs versions d'une carte en mettant plusieurs variables dans polygons()
#Exemple : Représentation des pays du monde en fonction des variables "HPI" et "economy"
#La fonction tm_facets() permet de juxtaposer des cartes
tmap_mode("view")
tm_shape(World) + tm_polygons(c("HPI", "economy")) + tm_facets(sync = TRUE, ncol = 2)



#Créer plusieurs cartes en fonction d'une modalité
#La fonction tm_facets() va permettre de créer une carte par modalité d'une variable choisie
#Exemple : creation d'un graphique par continent
tmap_mode("plot")
tm_shape(World) + tm_polygons("inequality", palette = "RdYlBu") + tm_facets(by = "continent")





###############################################################################
#################### Leaflet - des cartes interactives pouss?es
###############################################################################
#Exemple pour la Tour Eiffel
leaflet() %>%
  addTiles() %>%
  addMarkers(lng = 2.2945, lat = 48.858255, popup = "La Tour Eiffel")



# Autre exemple

#Les icones sont customisables avec addAwesomeMarkers()
#quakes jeu de données intégré au package
#Extraction des 20 premières lignes
df.20 <- quakes[1:20, ]

#Icones customisées en fonction de la variable mag
#Fonction qui permet de d?terminer la couleur
getColor <- function(quakes) {
  sapply(quakes$mag, function(mag) {
    if (mag <= 4) {
      "green"
    } else if (mag <= 5) {
      "orange"
    } else {
      "red"
    }
  })
}

#Customisation des icones
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(df.20)
)

#Visualisation
leaflet(df.20) %>% addTiles() %>%
  addAwesomeMarkers( ~ long,
                     ~ lat,
                     icon = icons,
                     label =  ~ as.character(mag))

#Le paramètre clusterOptions de la fonction addMarkers() est utilis? lorsque beaucoup d'icones de points se trouvent sur une carte.
#En vue macro, il va agr?ger les icones de points en un icone global de points (indiquant le nombre de sous icones de points).
#En zoomant, il est possible d'avoir une vision détaillée de toutes les icones de points.
l <- leaflet(quakes)
leaflet(quakes) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())

