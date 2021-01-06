library(tidyverse)
library(sp)
library(geojsonio)
library(leaflet)
library(ggplot2)
library(treemap)

source("processData.R")

pal <- colorFactor(palette = c("#8d96a3","#00798c"),
                   domain = world.dat.new@data$own.bin)

labels.owned <- sprintf(
  "<strong>%s</strong><br/>Books Owned - %g ",
  world.dat.new@data$ADMIN, world.dat.new@data$own.count
) %>% lapply(htmltools::HTML)

labels.read <- sprintf(
  "<strong>%s</strong><br/>Books Read - %g ",
  world.dat.new@data$ADMIN, world.dat.new@data$read.count
) %>% lapply(htmltools::HTML)


leaflet(data = world.dat.new) %>%
  addPolygons(fillColor = ~pal(own.bin),
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              label = labels.owned,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal, values = ~own.bin, opacity = 0.7, title = "Book Owned",
            position = "bottomright")



leaflet(data = world.dat.new) %>%
  addPolygons(fillColor = ~pal(read.bin),
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              label = labels.read,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal, values = ~read.bin, opacity = 0.7, title = "Book Read",
            position = "bottomright")


##all

pal.all <- colorFactor(palette = c("#8d96a3", "#fdbb84", "#e34a33"),
                        domain = world.dat.new@data$all.bin)
                   
labels.all <- sprintf(
  "<strong>%s</strong><br/>Books Owned - %g <br />Books Read - %g",
  world.dat.new@data$ADMIN, world.dat.new@data$own.count, world.dat.new@data$read.count
) %>% lapply(htmltools::HTML)


leaflet(data = world.dat.new) %>%
  addPolygons(fillColor = ~pal.all(all.bin),
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              label = labels.all,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal.all, values = ~all.bin, opacity = 0.7, title = "Read Status",
            position = "bottomright")

# Horizontal version
ggplot(country.summary, aes(x= ADMIN, y=read.count)) +
  geom_segment(aes(x=reorder(ADMIN, read.count), xend=ADMIN, y=0, yend=read.count), color="grey") +
  geom_point(size=3, color="#69b3a2") +
  theme_light() +
  coord_flip() +
  xlab("Number of books read") + 
  ylab("Country") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

# Plot
treemap(country.summary,
        # data
        index="ADMIN",
        vSize="read.count",
        type="index",
        
        # Main
        title="",
        palette="Dark2",
        
        # Borders:
        border.col=c("black"),             
        border.lwds=1,                         
        
        # Labels
        fontsize.labels=0.5,
        fontcolor.labels="white",
        fontface.labels=1,            
        bg.labels=c("transparent"),              
        align.labels=c("center", "center"),                                  
        overlap.labels=0.5,
        inflate.labels=T                        # If true, labels are bigger when rectangle is bigger.
        
        
)
