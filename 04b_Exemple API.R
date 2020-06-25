# Exemple 1 : OpenFoodFacts -----------
library(tidyverse)
# https://www.ean-search.org/?q=jardin+bio

jardin_bio <- read_delim("data/jardin_bio.txt", 
                         delim = "\t", 
                         col_names = c("ean13","produit"),
                         col_types = cols(ean13=col_character(),
                                          produit=col_character()))

# https://fr.openfoodfacts.org/data
get_json <- function(ean) {
  paste0("https://world.openfoodfacts.org/api/v0/product/",ean,".json")
}

nom <- list()
sucres <- list()
sel <- list()

for (i in 1:nrow(jardin_bio)) {
  json_data <- jsonlite::fromJSON(get_json(jardin_bio[i,1]))
  nom[i] <- json_data$product$product_name
  sucres[i] <- json_data$product$nutriments$sugars_100g
  sel[i] <- json_data$product$nutriments$salt_100g
 }


unlist_prods <- function(liste) {
  liste <- lapply(liste, function(x) if (length(x) == 0) {0} else {x})
  liste <- unlist(liste)
  liste <- as.vector(liste)
  return(liste)
}

mydf <- data.frame(nom = unlist_prods(nom),
                   sucres = unlist_prods(sucres),
                   sel = unlist_prods(sel)) %>%
  filter(!nom %in% c("0","")) %>%
  arrange(desc(sucres),desc(sel))



# Exemple 2 : Spotify -----------
#devtools::install_github('charlie86/spotifyr')
library(spotifyr)

id <- "70298a02eeb341e181310b1554e79a3a"
secret <- "e439fc6b45c945749ba40d17533d0531"
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()

source <- get_artist_audio_features('serge gainsbourg')

