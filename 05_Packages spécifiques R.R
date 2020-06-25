#----------------------------------------------------------------------------------#
#                            Packages spécifiques R                                #
#----------------------------------------------------------------------------------#

#### Librairies --------------

# install.packages(c("eurostat", "questionr", "data.table", "esquisse"))
# install.packages("simputation", dependencies=TRUE)

library(eurostat)
library(tidyverse)
library(questionr)
library(simputation)
library(data.table)
library(esquisse)
library(fs)
library(lubridate)
library(caret)


if (!require(httr)) {
    install.packages("httr")
}
library(httr)

#### Open Data -----------------

countries_list <- eurostat::eu_countries

search_eurostat <- eurostat::search_eurostat("unemploy")

eurostat_code <- "une_ltu_a"

my_eurostat <- eurostat::get_eurostat(eurostat_code)


#### Le RMarkdown  --------------


# Démo dans "demo.Rmd"


#### Programmation fonctionnelle  --------------


# Exemple 1
#Dans le dataset iris, calculer le minimum de chaque ligne sur les colonnes Sepal.Length, Sepal.Width, Petal.Length, Petal.Width

#Appliquer la fonction min() sur les colonnes s?lectionn?es du dataframe iris
iris[, 1:4] %>% pmap(function(Sepal.Length,
                              Sepal.Width,
                              Petal.Length,
                              Petal.Width) {
    min(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
})




# Exemple 2
#Appliquer par cylindr? une r?gression lin?aire et afficher le r carr?, sur les donn?es du dataset mtcars (contient des donn?es sur des v?hicules, notamment le cylindr?)
#Etapes :
#  S?paration du dataframe en trois ? partir de la valeur de cylindr? (trois valeurs 4, 6 ou 8) avec la fonction split()
#  R?gression lin?aire par cylindr? de la variable mpg par la variable wt, en utilisant la fonction map()
#  Le R carr? par cylindr? est calcul? en utilisant les fonctions map(summary) et map_dbl(?r.squared?)

mtcars %>% split(.$cyl) %>%
    map( ~ lm(mpg ~ wt, data = .)) %>%
    map(summary) %>% map_dbl("r.squared")



#Exemple 3
#Import de plusieurs fichiers csv de format identique
#data_dir : objet contenant le chemin et le nom du r?pertoire
#dir_ls : permet de lister tous les fichiers de type csv du r?pertoire
#map_dfr : permet de lire tous les fichiers csv en les concat?nant dans un dataframe. Le param?tre id permet de cr?er une colonne dans le dataframe qui indique le nom du fichier d'o? proviennent les donn?es
#mutate : permet de retravailler la variable de date pour avoir une date au format Ann?e - Mois
data_dir <- "pack_purrr/"

exemple3 <- data_dir %>%
    dir_ls(regexp = "\\.csv$") %>%
    map_dfr(read_csv, .id = "source") %>%
    mutate(Month_Year = myd(Month_Year, truncated = 1))



#### package questionr --------------


# D?monstration sur le dataframe hdv2003
data(hdv2003)

# irec() pour recoder un facteur via une interface interactive
irec(hdv2003, "qualif")

# iorder() : pour r?ordonner ? ? la main ? des facteurs via une interface interactive
iorder(hdv2003, "qualif")

#icut() : pour cr?er des variables en tranches, via une interface interactive, avec la possibilit? d'utiliser plusieurs m?thodologies diff?rentes
icut(hdv2003, "age")




#### imputation de données manquantes avec simputation --------------


#A partir du jeu de donn?es iris, imputation avec une r?gression lin?aire et un arbre de d?cision des donn?es manquantes.
dat <- iris


#Le jeu de donn?es ne comportant pas de valeurs manquantes, il va falloir en simuler.
dat[1:3, 1] <- dat[3:7, 2] <- dat[8:10, 5] <- NA
head(dat, 10)


# Imputer Sepal.Width et Sepal.Length par une regression lin?aire sur Petal.Width et Species.
# Puis imputer Species par un arbre de d?cision (CART) utilisant toutes les autres variables
dat %>%
    impute_lm(Sepal.Length + Sepal.Width ~ Petal.Length + Species) %>%
    impute_cart(Species ~ .) %>%
    head(10)


#### import de fichiers volumineux avec data.table  --------------


# Importation du fichier ??big_df?? avec readr() du tidyverse et fread(), et comparaison de la vitesse d'import

datalink <- "big_df.csv"

system.time(DF1 <- read.csv(datalink, stringsAsFactors = FALSE))
# 4 sec

system.time(DT <- fread(datalink))
# 0.08 sec



#### Graphiques ggplot2 en interactif -----------------------------


# démo sur le dataset iris
esquisse::esquisser(iris)


#### machine learning avec caret --------------


mydata <- fread("Telco-Customer-Churn.csv")

glimpse(mydata)

ids <- mydata$customerID
df <- mydata[, 2:ncol(mydata)]

# variable à prédire
df$Churn <- as.factor(df$Churn)
glimpse(df)

colSums(is.na(df))
# pour simplifier, on supprime la colonne avec trop de données manquantes
df$TotalCharges <- NULL

#   Pre-processing
#------------------------------------------------------------------------------#

# Echantillon apprentissage et validation
# Split des données : 70% pour l'apprentissage (et 100%-70% = 30% pour la validation)
trainIndex <- createDataPartition(df$Churn, p = .7, list = FALSE)

# création des échantillons train et test
churnTrain <- df[trainIndex, ]
churnTest  <- df[-trainIndex, ]


# Centrer réduire des données
preProcValues <- preProcess(churnTrain, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, churnTrain)
testTransformed <- predict(preProcValues, churnTest)


#   Modélisation
#------------------------------------------------------------------------------#



# Cross validation
fitControl <- trainControl(method = "cv", number = 5)


# Premier modèle : GBM
gbmFit <- train(
    Churn ~ .,
    data = churnTrain,
    method = "gbm",
    trControl = fitControl,
    verbose = FALSE)

# précision sur l'échantillon d'apprentissage
predict(gbmFit, newdata = churnTrain, type = "prob")
pred <- predict(gbmFit, newdata = churnTrain)
pred

truth <- churnTrain$Churn
xtab <- table(pred, truth)
xtab
caret::confusionMatrix(xtab)

# précision sur l'échantillon de test
pred <- predict(gbmFit, newdata = churnTest)
truth <- churnTest$Churn
xtab <- table(pred, truth)
xtab
caret::confusionMatrix(xtab)


# Modèle 2 : SVM

svmFit <- train(
    Churn ~ .,
    data = churnTrain,
    method = "svmRadial",
    trControl = fitControl,
    verbose = FALSE
)

pred <- predict(svmFit, newdata = churnTest)
truth <- churnTest$Churn
caret::confusionMatrix(table(pred, truth))

# Modèle 3 : Random Forest

rfFit <- train(
    Churn ~ .,
    data = churnTrain,
    method = "rf",
    trControl = fitControl,
    verbose = FALSE
)

pred <- predict(rfFit, newdata = churnTest)
truth <- churnTest$Churn
caret::confusionMatrix(table(pred, truth))


# Comparaison des 3 modèles
results <- resamples(list(GBM = gbmFit, SVM = svmFit, RF = rfFit))
results$values
summary(results, metric = "Accuracy")
bwplot(results, metric = "Accuracy")
