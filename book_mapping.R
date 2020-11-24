library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(gridExtra)
library(ggplot2)
theme_set(theme_bw())

#load world map
world <- ne_countries(scale = "medium", returnclass = "sf")
#class(world)

#read goodreads Library data in
book.dat <- read.csv("//userfs/cjw509/w2k/Downloads/book_data.csv", stringsAsFactors = FALSE)

#standardise some country names
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

#merge the owned and read count datframes
nat.join <- left_join(nat.own, nat.read, by = c("admin")) 

#add empty columns for book counts into world map dataframe 
world$own.count  <- 0
world$read.count <- 0


world.join <- left_join(world, nat.join, by = c("admin")) 

world.join$own.count  <- coalesce(world.join$own.count.y, world.join$own.count.x)
world.join$read.count <- coalesce(world.join$read.count.y, world.join$read.count.x)

world.join <- world.join[, c(9, 55, 68:70)]

world.join$own.bin  <- ifelse(world.join$own.count > 0, 1, 0)
world.join$read.bin <- ifelse(world.join$read.count > 0, 1, 0)

world.join$own.bin  <- factor(world.join$own.bin, levels = c(0,1), labels = c("No", "Yes"))
world.join$read.bin <- factor(world.join$read.bin, levels = c(0,1), labels = c("No", "Yes"))

#remove antartica from the map to make plots look better
world.join <- world.join[!world.join$admin == "Antarctica", ]


##### plots
### world
#plot world map of owned books (yes/no)
ggplot(data = world.join) +
  geom_sf(aes(fill = own.bin)) +
  scale_fill_manual("Owned book" ,values=c("#8d96a3","#00798c")) +
  theme(legend.position = "bottom")

#plot world map of read books (yes/no)
ggplot(data = world.join) +
  geom_sf(aes(fill = read.bin)) +
  scale_fill_manual("Read book" ,values=c("#8d96a3","#00798c")) +
  theme(legend.position = "bottom")

#plot world map of count of books read
ggplot(data = world.join) +
  geom_sf(aes(fill = read.count)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")


### individual continent mapping
#europe
eur.own  <- ggplot(data = world.join[world.join$continent == "Europe", ]) +
              geom_sf(aes(fill = own.bin)) +
              xlim(-23, 50) +
              ylim(35, 72) +
              scale_fill_manual("Owned book" ,values=c("#8d96a3","#00798c")) +
              theme(legend.position = "bottom")

eur.read <- ggplot(data = world.join[world.join$continent == "Europe", ]) +
              geom_sf(aes(fill = read.bin)) +
              xlim(-23, 50) +
              ylim(35, 72) +
              scale_fill_manual("Read book" ,values=c("#8d96a3","#00798c")) +
              theme(legend.position = "bottom")

#Asia
asia.own  <- ggplot(data = world.join[world.join$continent == "Asia", ]) +
              geom_sf(aes(fill = own.bin)) +
              xlim(20, 150) +
              ylim(-15, 70) +
              scale_fill_manual("Owned book" ,values=c("#8d96a3","#00798c")) +
              theme(legend.position = "bottom")

asia.read <- ggplot(data = world.join[world.join$continent == "Asia", ]) +
              geom_sf(aes(fill = read.bin)) +
              xlim(20, 150) +
              ylim(-15, 70) +
              scale_fill_manual("Read book" ,values=c("#8d96a3","#00798c")) +
              theme(legend.position = "bottom")

#North America
na.own   <- ggplot(data = world.join[world.join$continent == "North America", ]) +
              geom_sf(aes(fill = own.bin)) +
              xlim(-170, -15) +
              ylim(5, 85) +
              scale_fill_manual("Owned book" ,values=c("#8d96a3","#00798c")) +
              theme(legend.position = "bottom")

na.read  <- ggplot(data = world.join[world.join$continent == "North America", ]) +
              geom_sf(aes(fill = read.bin)) +
              xlim(-170, -15) +
              ylim(5, 85) +
              scale_fill_manual("Read book" ,values=c("#8d96a3","#00798c")) +
              theme(legend.position = "bottom")
  
#South America
sa.own  <- ggplot(data = world.join[world.join$continent == "South America", ]) +
                  geom_sf(aes(fill = own.bin)) +
                  xlim(-100, -30) +
                  ylim(-55, 15) +
                  scale_fill_manual("Owned book" ,values=c("#8d96a3","#00798c")) +
                  theme(legend.position = "bottom")


sa.read <- ggplot(data = world.join[world.join$continent == "South America", ]) +
                  geom_sf(aes(fill = read.bin)) +
                  xlim(-100, -30) +
                  ylim(-55, 15) +
                  scale_fill_manual("Read book" ,values=c("#8d96a3","#00798c")) +
                  theme(legend.position = "bottom")

#Oceania
oce.own  <- ggplot(data = world.join[world.join$continent == "Oceania", ]) +
                    geom_sf(aes(fill = own.bin)) +
                    xlim(100, 180) +
                    ylim(-60, 20) +
                    scale_fill_manual("Owned book" ,values=c("#8d96a3","#00798c")) +
                    theme(legend.position = "bottom")

oce.read <- ggplot(data = world.join[world.join$continent == "Oceania", ]) +
                    geom_sf(aes(fill = read.bin)) +
                    xlim(100, 180) +
                    ylim(-60, 20) +
                    scale_fill_manual("Read book" ,values=c("#8d96a3","#00798c")) +
                    theme(legend.position = "bottom")

#Africa
afr.own  <- ggplot(data = world.join[world.join$continent == "Africa", ]) +
                    geom_sf(aes(fill = own.bin)) +
                    xlim(-25, 50) +
                    ylim(-40, 40) +
                    scale_fill_manual("Owned book" ,values=c("#8d96a3","#00798c")) +
                    theme(legend.position = "bottom")

afr.read <- ggplot(data = world.join[world.join$continent == "Africa", ]) +
                    geom_sf(aes(fill = read.bin)) +
                    xlim(100, 180) +
                    ylim(-60, 30) +
                    scale_fill_manual("Read book" ,values=c("#8d96a3","#00798c")) +
                    theme(legend.position = "bottom")

# Create user-defined function, which extracts legends from ggplots
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

owned_legend <- extract_legend(eur.own)
read_legend  <- extract_legend(eur.read)


#plot all continents
layout_matrix <- matrix(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
                          4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6), nrow = 2, byrow = TRUE)

# grid.arrange(arrangeGrob(eur.own + theme(legend.position = "none"), 
#              na.own + theme(legend.position = "none"), 
#              sa.own + theme(legend.position = "none"), 
#              afr.own + theme(legend.position = "none"), 
#              asia.own + theme(legend.position = "none"), 
#              oce.own + theme(legend.position = "none"),
#              ncol = 3), owned_legend, nrow = 2, heights = c(10, 1))



