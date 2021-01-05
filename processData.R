library(dplyr)
library(tidyr)
library(sp)
library(geojsonio)
library(leaflet)

#read world map in
world.dat <- geojson_read("https://raw.githubusercontent.com/datasets/geo-countries/master/data/countries.geojson",  what = "sp")

#read goodreads Library data in
calibre.dat <- read.csv("data/calibre_books.csv", stringsAsFactors = FALSE)

#split nationality column to deal with dual nationalities
calibre.dat <- calibre.dat %>%
  separate(X.nationality, c("primary.nat", "secondary.nat"), "-", fill = "right")

#fix country names to match with world map
calibre.dat = calibre.dat %>% 
  mutate(primary.nat = gsub("USA", "United States of America", primary.nat), 
         primary.nat = gsub("UK", "United Kingdom", primary.nat),
         primary.nat = gsub("Serbia", "Republic of Serbia", primary.nat),
         primary.nat = gsub("Republic of the Congo", "Republic of Congo", primary.nat),
         secondary.nat = gsub("USA", "United States of America", secondary.nat),
         secondary.nat = gsub("UK", "United Kingdom", secondary.nat),
         secondary.nat = gsub("Serbia", "Republic of Serbia", secondary.nat),
         secondary.nat = gsub("Republic of the Congo", "Republic of Congo", secondary.nat)
  )

#create dataframes of countries with counts of owned and read books
nat.own <- calibre.dat %>% 
  count(primary.nat) %>% 
  rename(admin = "primary.nat", own.count = "n")

nat.read <- calibre.dat %>% 
  filter(X.read == "True") %>% 
  count(primary.nat) %>% 
  rename(admin = "primary.nat", read.count = "n") 

#merge the owned and read count datframes and set owned but not read countries to 0
nat.join <- left_join(nat.own, nat.read, by = c("admin")) %>% 
  mutate(read.count = replace_na(read.count, 0))

#merge the owned and read count datframes
world.dat.new <- merge(world.dat, nat.join, by.x="ADMIN", by.y="admin")


#set countries with no owned books to 0 counts
world.dat.new@data <- world.dat.new@data %>% 
  mutate(own.count = replace_na(own.count, 0)) %>% 
  mutate(read.count = replace_na(read.count, 0))

# add binary owned/not owned and read/not read values for each country and set as factors
world.dat.new@data <- world.dat.new@data %>%                        
  mutate(own.bin = factor(ifelse(own.count > 0, 1, 0), levels = c(0, 1), labels = c("No", "Yes"))) %>% 
  mutate(read.bin = factor(ifelse(read.count > 0, 1, 0), levels = c(0, 1), labels = c("No", "Yes"))) %>% 
  mutate(all.bin = factor(ifelse(read.count > 0 , 3, ifelse(own.count > 0, 2, 1)), 
                          levels = c(1, 2, 3), labels = c("Not owned", "Owned", "Read")))

#remove antartica from the map to make plots look better
world.dat.new <- world.dat.new[!world.dat.new$ADMIN== "Antarctica", ]

rm(calibre.dat, nat.join, nat.own, nat.read, world.dat)