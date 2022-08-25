#Make a map of the DarkCideS data (https://darkcides.org/database/)

library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(sf)

#Data taken from: https://figshare.com/articles/dataset/Metadata_for_DarkCideS_1_0_a_global_database_for_bats_in_karsts_and_caves/16413405?file=34091939


caves <- read.csv("./data/darkCideS/Dataset 2.csv")
caves.g <- caves %>% group_by(Country.record, Cave.site, Lattitude, Longitude) %>% 
  summarize(batRichness = n())

caves.countryRich <- caves %>% group_by(Country.record) %>% 
  summarize(countryBatRichness = length(unique(Species.name)))

world <- ne_countries(scale = "large", returnclass = "sf")
class(world)

worldBatCaveRich <- world %>% left_join(caves.countryRich, by = c("name" = "Country.record"))

library(RColorBrewer)
bluecols <- brewer.pal(9, 'BuPu')
newcol <- colorRampPalette(bluecols)
ncols <- max(caves.countryRich$countryBatRichness)
bluecols2 <- newcol(ncols)#apply the function to get 100 colours
pie(rep(1, ncols), col = bluecols2, border = NA, labels = NA)

p <- ggplot()+
  geom_sf(data = worldBatCaveRich, fill = "white")+
  geom_sf(data = worldBatCaveRich, aes(fill = sqrt(countryBatRichness)))+
  scale_fill_distiller(direction = 1)+
  geom_sf(data = worldBatCaveRich %>% filter(is.na(countryBatRichness)), fill = "white")+
  geom_point(data = caves.g, aes(x = Longitude, y = Lattitude, size = batRichness), alpha = 0.1, pch = 21, stroke = 1, color = "red")+
  theme_bw()+
  theme(legend.position = "none")
p

pdf("./globeCaveMapDiversity.pdf", width = 10, height = 8)
print(p)
dev.off()
  
png("./globeCaveMapDiversity.png", width = 1600, height = 1280)
print(p)
dev.off()

  
