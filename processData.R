library(tidyverse)
library(lubridate)
library(googlesheets4)
library(sp)
library(geojsonio)

#read library data in
#calibre.dat <- read.csv("data/calibre_books.csv", stringsAsFactors = FALSE)
gs4_auth(email = TRUE)
calibre.dat <- read_sheet('1aq35ICEY5cQSLPcPIs-NNAypsLKIk9GLCf_Hw55FYNI')

colClean <- function(x){ colnames(x) <- gsub("#", "", colnames(x)); x } 
calibre.dat <- colClean(calibre.dat) 

#remove columns not useful to stats
drop.cols <- c('cover', 'comments', 'identifiers', 'library_name', 'review', 
               'size', 'uuid', 'isbn')
calibre.dat <- calibre.dat %>% 
                select(-one_of(drop.cols))

#split nationality column to deal with dual nationalities
calibre.dat <- calibre.dat %>%
  separate(nationality, c("primary.nat", "secondary.nat"), "-", fill = "right")

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

#unlist weird imports from google sheets
calibre.dat <- calibre.dat %>% 
                mutate(title = as.character(unlist(title)),
                       title_sort = as.character(unlist(title_sort)))

#set gender column to factor
calibre.dat <- calibre.dat %>% 
                mutate(gender = factor(gender, levels = c('Female', 'Male'), 
                                         labels = c('Female', 'Male')))

#convert date columns to correct date format instead of character type
calibre.dat <- calibre.dat %>% 
                mutate(dateread =  ymd_hms(dateread),
                       pubdate =  ymd_hms(pubdate))


##### Book Mapping
#create dataframes of countries with counts of owned and read books
nat.own <- calibre.dat %>% 
  count(primary.nat) %>% 
  rename(admin = "primary.nat", own.count = "n")

nat.read <- calibre.dat %>% 
  filter(read == "TRUE") %>% 
  count(primary.nat) %>% 
  rename(admin = "primary.nat", read.count = "n") 

#merge the owned and read count datframes and set owned but not read countries to 0
nat.join <- left_join(nat.own, nat.read, by = c("admin")) %>% 
  mutate(read.count = replace_na(read.count, 0))

#read world map in
world.dat <- geojson_read("https://raw.githubusercontent.com/datasets/geo-countries/master/data/countries.geojson",  what = "sp")

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

world.dat.df <- world.dat.new@data

# Create dataframe summarising numbe rof owned/read books per country
country.summary <- world.dat.df %>% 
                    select(ADMIN, own.count, read.count) %>% 
                    filter(own.count > 0) %>% 
                    arrange(desc(own.count))

#Calculate prortions of owned/read books for each coutry and read to owned ratio
country.summary <- country.summary %>% 
                    mutate(prop.own = own.count / sum(own.count),
                           prop.read = read.count / sum(read.count),
                           read.own.ratio = read.count / own.count)

#Calculate year each country was first read
# country.year <- calibre.dat %>%
#                   filter(read == "TRUE") %>% 
#                   arrange(dateread) %>%
#                   group_by(primary.nat) %>%
#                   slice(1L) %>% 
#                   select(primary.nat, dateread) %>% 
#                   mutate(year.read = year(dateread))

####Books read statistics
#create dataframe for use in book stats plotting
read.dat <- calibre.dat %>% 
  filter(year(dateread) >= 1900) #add filter to remove any books with errors in read dates (defaults to year 0)
  
author.summary <- read.dat %>% 
  select(authors, gender, primary.nat) %>% 
  count(authors, gender, primary.nat, name = 'read.count')

gender.count.all <- read.dat %>% 
  count(gender)

gender.count.auth <- author.summary %>% 
  count(gender) 


rm(drop.cols, nat.join, nat.own, nat.read, world.dat)
