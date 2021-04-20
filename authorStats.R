# Libraries
library(ggplot2)
library(waffle)
source("processData.R")

plot(read.dat$dateread, read.dat$pubdate)

labs <- c("Male", "Female")
slices <- c(gender.count.all$n[gender.count.all$gender == 'Male'], 
            gender.count.all$n[gender.count.all$gender == 'Female'])
pie(slices, labels = labs, main="Total books read by gender of author")

slices.single <- c(gender.count.auth$n[gender.count.auth$gender == 'Male'], 
                   gender.count.auth$n[gender.count.auth$gender == 'Female'])
pie(slices.single, labels = labs, main="Authors read by gender (any number of books read)")




waffle(gender.count.all, rows = 20, colors = c("#69b3a2", "grey", "white"), flip = TRUE,
       xlab = "1 square = 1 read book")

# 
# ggplot(gender.count.all, aes(x= gender, y=n)) +
#   geom_segment(aes(x=reorder(gender, n), xend=gender, y=0, yend=n), color="grey") +
#   geom_point(size=3, color="#69b3a2") +
#   theme_light() +
#   coord_flip() +
#   xlab("Author Gender") + 
#   ylab("Number of books read") +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks.y = element_blank()
#   )


