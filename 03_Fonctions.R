# Boucle for

library(tidyverse)

setwd("~/R_Advanced/data")
download.file(
  "https://www.insee.fr/fr/statistiques/fichier/3720946/communes-01012019-csv.zip",
  destfile = "communes.zip"
)
unzip("communes.zip")

communes <- readr::read_delim("communes-01012019.csv", delim = ",")
communes <- communes %>%
  dplyr::select(libelle) %>%
  t() %>% as.character()

saint <- vector()

for (i in 1:length(communes)) {
  saint[i] <- str_detect(communes[i], "Saint")
}

table(saint)
100 * prop.table(table(saint))







# Exercice 1 ----------------

fizzbuzz <- function(x) {
  #vérifier que l’argument est un argument valide
  stopifnot(length(x) == 1)
  stopifnot(is.numeric(x))
  
  if (!(x %% 3) && !(x %% 5)) {
    #Nombre divisible par 3 et 5 ?
    "fizzbuzz"
  } else if (!(x %% 3)) {
    #Si non, nombre divisible par 3 ?
    "fizz"
  } else if (!(x %% 5)) {
    #Si non, nombre divisible par 5 ?
    "buzz"
  } else {
    #Si non, retourner le nombre initial
    as.character(x)
  }
}

# fizzbuzz(6)
# fizzbuzz(10)
# fizzbuzz(15)
# fizzbuzz(2)

# Exercice 2 ----------------------------------
iris_SD <-
  iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]

#Fonction permettant de discrétiser les variables quantitatives
DISCRETISE <- function(DF, bornes) {
  nbvar <- dim(DF)[2]
  for (i in 1:nbvar) {
    nom <- paste(colnames(DF)[i], "_D", sep = "")
    nom_var <- colnames(DF)
    q <- quantile(DF[, i], bornes)
    breaks <- c(min(DF[, i]), q[1], q[2], max(DF[, i]))
    var_ <- cut(DF[, i], unique(breaks), include.lowest = TRUE)
    DF <- cbind(DF, var_)
    names(DF) <- c(nom_var, nom)
  }
  return(DF)
}

# iris_DISCRET <- DISCRETISE(DF = iris_SD, bornes = c(1 / 3, 2 / 3))
