# Libraries
library(ggplot2)
library(tidyverse)
source("processData.R")

read.dat <- calibre.dat %>% 
              filter(year(X.dateread) > 2015)

plot(read.dat$X.dateread, read.dat$pubdate)

# slices <- c(sum(read.dat$Gender == 'male'), sum(read.dat$Gender == 'female'))
# labs <- c("Male", "Female")
# pie(slices, labels = labs, main="Gender of author of read books")
