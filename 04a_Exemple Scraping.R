library(tidyverse)
library(xml2)
library(rvest)
library(stringr)

setwd("~/R_Advanced/")

# page test
url <- 'http://www.xsilence.net/disque-2.htm'
webpage <- read_html(url)
artist <- html_text(html_nodes(webpage, '.i_1_1_4T'))[1]
album_title <- html_text(html_nodes(webpage, '.i_5T'))[1]
chronique_date <- html_text(html_nodes(webpage, '.i_5T'))[2]
review <- html_text(html_nodes(webpage, 'div.normal'))[1]
note_chroniqueur <- html_text(html_nodes(webpage, 'span.title'))[1]
appreciation <- html_text(html_nodes(webpage, 'span.title3'))[1]
chroniqueur <- html_text(html_nodes(webpage, 'a.normal'))

source <- tibble(url,
                 artist,
                 album_title,
                 chronique_date,
                 review,
                 note_chroniqueur,
                 appreciation,
                 chroniqueur)

nb_disques <- 100
links <-paste0('http://www.xsilence.net/disque-',
               seq(3,nb_disques,1),
               '.htm')
for (i in 2:nb_disques) {
  url<-  links[i]
  webpage <- read_html(url)
  source[i,1] <- links[i]
  source[i,2] <- html_text(html_nodes(webpage, '.i_1_1_4T'))[1]
  source[i,3] <- html_text(html_nodes(webpage, '.i_5T'))[1]
  source[i,4] <- html_text(html_nodes(webpage, '.i_5T'))[2]
  source[i,5] <- html_text(html_nodes(webpage, 'div.normal'))[1]
  source[i,6] <- html_text(html_nodes(webpage, 'span.title'))[1]
  source[i,7] <- html_text(html_nodes(webpage, 'span.title3'))[1]
  source[i,8] <- html_text(html_nodes(webpage, 'a.normal'))[1]
}


mydata <- source %>% 
  mutate(unique_album = paste0(artist," / ",album_title))
mydata <- mydata[!duplicated(mydata$unique_album), ]
saveRDS(mydata,"data/xsilence.rds")



