library(ggplot2)
library(waffle)
source("processData.R")


list.300.read <- calibre.dat %>% 
                  filter(grepl("List Challenge 300", collection) & read == "TRUE")

list.bbc.read <- calibre.dat %>% 
                   filter(grepl("BBC Shaped World", collection) & read == "TRUE")

list.300.read.table <- data.frame(status = c("Read", "Not read"), n = c(nrow(list.300.read), 300 - nrow(list.300.read)))
list.bbc.read.table <- data.frame(status = c("Read", "Not read"), n = c(nrow(list.bbc.read), 100 - nrow(list.bbc.read)))

list.300.read.vec <- list.300.read.table$n
names(list.300.read.vec) <- list.300.read.table$status

list.bbc.read.vec <- list.bbc.read.table$n
names(list.bbc.read.vec) <- list.bbc.read.table$status

iron(
  waffle(list.300.read.vec, 
         rows = 20, colors = c("#69b3a2", "grey", "white"), flip = TRUE,
         xlab = "1 square = 1 read book", title = "List Challenge 300"),
  waffle(list.bbc.read.vec, 
         rows = 10, colors = c("#69b3a2", "grey", "white"), flip = TRUE,
         xlab = "1 square = 1 read book",   title = "BBC Shaped the World")
)


list.300.read.table.prop <- c(round((nrow(list.300.read)/300), 2)*100, round((300 - nrow(list.300.read))/300, 2)*100)
names(list.300.read.table.prop) <- c("Read", "Not read")


waffle(list.300.read.table.prop, 
       rows = 10, colors = c("#69b3a2", "grey", "white"), flip = TRUE,
       xlab = "1 square = 1 read book", title = "List Challenge 300")
