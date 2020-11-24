library(sp)
library(geojsonio)
library(leaflet)
library(dplyr)

world.dat <- geojson_read("https://raw.githubusercontent.com/datasets/geo-countries/master/data/countries.geojson",  what = "sp")

#read goodreads Library data in
book.dat <- read.csv("data/book_data.csv", stringsAsFactors = FALSE)

book.dat$Nationality <- gsub("USA", "United States of America", book.dat$Nationality)
book.dat$Nationality <- gsub("UK", "United Kingdom", book.dat$Nationality)
book.dat$Nationality <- gsub("Serbia", "Republic of Serbia", book.dat$Nationality)
book.dat$Nationality <- gsub("Republic of the Congo", "Republic of Congo", book.dat$Nationality)


#cretae dataframes of countries with counts of owned and read books
nat.own  <- as.data.frame(table(book.dat$Nationality))
nat.read <- as.data.frame(table(book.dat$Nationality[book.dat$Exclusive.Shelf == "read"]))

#rename columns
names(nat.own)  <- c("admin", "own.count")
names(nat.read) <- c("admin", "read.count")

#set country name to character instead of factor for matching purposes
nat.own$admin  <- as.character(nat.own$admin)
nat.read$admin <- as.character(nat.read$admin)

#cretae dataframes of countries with counts of owned and read books
nat.own  <- as.data.frame(table(book.dat$Nationality))
nat.read <- as.data.frame(table(book.dat$Nationality[book.dat$Exclusive.Shelf == "read"]))

#rename columns
names(nat.own)  <- c("admin", "own.count")
names(nat.read) <- c("admin", "read.count")

#set country name to character instead of factor for matching purposes
nat.own$admin  <- as.character(nat.own$admin)
nat.read$admin <- as.character(nat.read$admin)

#merge the owned and read count datframes
nat.join <- left_join(nat.own, nat.read, by = c("admin")) 

#merge the owned and read count datframes
world.dat.new <- merge(world.dat, nat.join, by.x="ADMIN", by.y="admin")

world.dat.new@data$own.count[is.na(world.dat.new@data$own.count)]   <- 0
world.dat.new@data$read.count[is.na(world.dat.new@data$read.count)] <- 0

world.dat.new@data$own.bin  <- ifelse(world.dat.new@data$own.count > 0, 1, 0)
world.dat.new@data$read.bin <- ifelse(world.dat.new@data$read.count > 0, 1, 0)

world.dat.new@data$own.bin  <- factor(world.dat.new$own.bin, levels = c(0,1), labels = c("No", "Yes"))
world.dat.new@data$read.bin <- factor(world.dat.new$read.bin, levels = c(0,1), labels = c("No", "Yes"))

#remove antartica from the map to make plots look better
world.dat.new <- world.dat.new[!world.dat.new$ADMIN== "Antarctica", ]

rm(book.dat, nat.join, nat.own, nat.read, world.dat)