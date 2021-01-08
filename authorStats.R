# Libraries
library(ggplot2)
library(waffle)
source("processData.R")

read.dat <- calibre.dat %>% 
              filter(year(X.dateread) > 1990) %>% 
               mutate(X.gender = factor(X.gender, levels = c('Female', 'Male'), labels = c('Female', 'Male')))


author.summary <- read.dat %>% 
                    select(authors, X.gender, primary.nat) %>% 
                    count(authors, X.gender, primary.nat, name = 'read.count')

gender.count.all <- read.dat %>% 
                      count(X.gender)

gender.count.auth <- author.summary %>% 
                      count(X.gender) 

plot(read.dat$X.dateread, read.dat$pubdate)

labs <- c("Male", "Female")
slices <- c(gender.count.all$n[gender.count.all$X.gender == 'Male'], 
            gender.count.all$n[gender.count.all$X.gender == 'Female'])
pie(slices, labels = labs, main="Total books read by gender of author")

slices.single <- c(gender.count.auth$n[gender.count.auth$X.gender == 'Male'], 
                   gender.count.auth$n[gender.count.auth$X.gender == 'Female'])
pie(slices.single, labels = labs, main="Authors read by gender (any number of books read)")



iron(
waffle(gender.count.all, rows = 12, colors = c("#fb8072", "#8dd3c7", "white")),
waffle(gender.count.auth, rows = 12, colors = c("#fb8072", "#8dd3c7", "white"), pad = 16)
)
