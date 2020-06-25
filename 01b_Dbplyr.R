setwd("~/R_Advanced")

library(tidyverse)
library(lubridate)
library(jsonlite)
library(RSQLite)
library(DBI)
library(connections)
library(dbplyr)

bigtable <- readRDS("data/validations-sur-le-reseau-ferre-ratp.rds")


# JOURS --------------
# jours <- bigtable %>% distinct(JOUR) %>% arrange(JOUR)

jours <- seq.Date(
    from = as.Date("2018/01/01"),
    to = as.Date("2018/12/31"),
    by = "day"
) %>% as.data.frame()
names(jours) <- "JOUR"

jours$MOIS <- lubridate::month(jours$JOUR)
jours$ANNEE <- lubridate::year(jours$JOUR)
jours$JOUR_SEM <- weekdays(jours$JOUR)

# questionr::irec(jours$JOUR_SEM)

jours$WEEKEND <- recode(
    jours$JOUR_SEM,
    "lundi" = "0",
    "mardi" = "0",
    "mercredi" = "0",
    "jeudi" = "0",
    "vendredi" = "0",
    "samedi" = "1",
    "dimanche" = "1"
)
jours$WEEKEND <- as.numeric(jours$WEEKEND)

# ARRETS -------------------------
arrets <- bigtable %>% distinct(CODE_STIF_ARRET, LIBELLE_ARRET)
arrets <- arrets %>%
    mutate(CODE_STIF_ARRET  = as.character(CODE_STIF_ARRET))

# VALIDATIONS -----------------
valid <- bigtable %>% dplyr::select(1:4, 6:7)

valid_id <- as.character(seq(1, nrow(valid), 1))

valid <- valid %>%
    mutate_at(c("CODE_STIF_TRNS", "CODE_STIF_RES", "CODE_STIF_ARRET"),
              as.character) %>%
    arrange(JOUR,
            CODE_STIF_TRNS,
            CODE_STIF_RES,
            CODE_STIF_ARRET,
            CATEGORIE_TITRE) %>%
    mutate(VALID_ID = valid_id) %>%
    dplyr::select(VALID_ID, everything())
glimpse(valid)

# CREATION DE LA BASE DE DONNEES -----------------

# connexion à la BDD
drv <- SQLite()
db <- dbConnect(drv, dbname = "data/stif.sqlite")

# creation de 3 tables

# calendrier
sapply(jours, function(x)
    dbDataType(db, x))
dbSendQuery(
    conn = db,
    "CREATE TABLE CALENDRIER (
            JOUR  TEXT,
            MOIS  INTEGER,
            ANNEE  INTEGER,
            JOUR_SEM TEXT,
            WEEKEND INTEGER,
            PRIMARY KEY (JOUR))"
)

dbWriteTable(
    conn = db,
    name = "CALENDRIER",
    value = jours,
    row.names = FALSE,
    append = TRUE
)
dbListFields(db, "CALENDRIER")
jours_db <- dbReadTable(db, "CALENDRIER")

# arrets
sapply(arrets, function(x)
    dbDataType(db, x))
dbSendQuery(
    conn = db,
    "CREATE TABLE ARRETS (
            CODE_STIF_ARRET  TEXT,
            LIBELLE_ARRET  TEXT,
            PRIMARY KEY (CODE_STIF_ARRET))"
)
dbWriteTable(
    conn = db,
    name = "ARRETS",
    value = arrets,
    row.names = FALSE,
    append = TRUE
)

# validations
sapply(valid, function(x)
    dbDataType(db, x))
dbSendQuery(
    conn = db,
    "CREATE TABLE VALIDATIONS (
            VALID_ID TEXT,
            JOUR  TEXT,
            CODE_STIF_TRNS   TEXT,
            CODE_STIF_RES TEXT,
            CODE_STIF_ARRET TEXT,
            CATEGORIE_TITRE TEXT,
            NB_VALD REAL,
            PRIMARY KEY (VALID_ID))"
)
dbWriteTable(
    conn = db,
    name = "VALIDATIONS",
    value = valid,
    row.names = FALSE,
    append = TRUE
)

# vérifications des tables
dbListTables(db)


# Requêtes SQL --------------------------------------------------
# requête basique
test <- dbGetQuery(db, "SELECT distinct CATEGORIE_TITRE FROM VALIDATIONS")

# requête paramétrée
CODE_STIF_ARRET <-  "10"
CATEGORIE_TITRE <- c("NAVIGO")	

result <- dbGetQuery(db, 
                     'SELECT * FROM VALIDATIONS WHERE CODE_STIF_ARRET = ? AND CATEGORIE_TITRE = ?', 
                     params = c(CODE_STIF_ARRET,CATEGORIE_TITRE))
head(result)


# Deconnexion --------------------------------------------------------
dbDisconnect(db)

# exploration avec dbplyr --------------------
con <- connection_open(SQLite(), "data/stif.sqlite")

validations <-tbl(con, "VALIDATIONS") 
validations %>% head()

navigo <- validations %>%
    filter(CODE_STIF_ARRET == "10" & CATEGORIE_TITRE == "NAVIGO" )
navigo %>% collect()

arrets <- tbl(con,"ARRETS")
navigo2 <- left_join(validations, arrets) %>%
    filter(CODE_STIF_ARRET == "10" & CATEGORIE_TITRE == "NAVIGO" )
navigo2 %>% collect()

connection_close(con)
