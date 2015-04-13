#####
#####
## NAM Parental Origins
#####
#####

library(ggplot2)
library(ggmap)
library(mapproj)

NAM <- read.csv("NAM_lat_long_data.csv")
head(NAM)

nam_map <- get_map(location= c(lon = 30, lat = 35),
                zoom=3,
               maptype = 'satellite')

ggmap(nam_map) +
geom_point(colour='white', size= 4, aes(x=long, y=lat), data=NAM) +
geom_text(data = NAM, aes(x = long, y = lat, label = Ecotype_name),
          size = 9, vjust = 0, hjust = -0.25, colour='white') +
theme(aspect.ratio = .9) +
labs(title="Representative Arabidopsis Ecotype Origins",
            x="Longitude", y="Latitude")