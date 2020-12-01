
#read goodreads Library data in
book.dat <- read.csv("data/book_data.csv", stringsAsFactors = FALSE)


read.dat <- book.dat[book.dat$Exclusive.Shelf == "read",]


read.dat$Date.Read <- as.Date(read.dat$Date.Read, "%d/%m/%Y")


read.dat$Date.Read.year <- format(read.dat$Date.Read,"%Y")


plot(read.dat$Date.Read.year, read.dat$Original.Publication.Year)



slices <- c(sum(read.dat$Gender == 'male'), sum(read.dat$Gender == 'female'))
labs <- c("Male", "Female")
pie(slices, labels = labs, main="Gender of author of read books")


