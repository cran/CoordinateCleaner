## ---- warning = F, message = F, collapse = T, fig.width=8, fig.height=6-------
library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
library(rgbif)
library(sp)
library(viridis)


#download data from GBIF
dat <- rgbif::occ_search(scientificName = "Avicennia", limit = 1000,
         hasCoordinate = T)

dat <- dat$data

dat <-  dat %>% 
  dplyr::select(species = name, decimallongitude = decimalLongitude, 
         decimallatitude = decimalLatitude, countryCode)

# run with default gazetteer
outl <- cc_sea(dat, value = "flagged")

plo <- data.frame(dat, outlier =  as.factor(!outl))

#plot results
ggplot()+
  borders(fill = "grey60")+
  geom_point(data = plo, 
             aes(x = decimallongitude, y = decimallatitude, col = outlier))+
  scale_color_viridis(discrete = T, name = "Flagged outlier")+
  coord_fixed()+
  theme_bw()+
  theme(legend.position = "bottom")


## ---- warning = F, message = F, collapse = T, fig.width=8, fig.height=6-------
# The buffered custom gazetteer
data("buffland")
plot(buffland)

# run with custom gazetteer
outl <- cc_sea(dat, value = "flagged", ref = buffland)

plo <- data.frame(dat, outlier =  as.factor(!outl))

#plot results
ggplot()+
  borders(fill = "grey60")+
  geom_point(data = plo, 
             aes(x = decimallongitude, y = decimallatitude, col = outlier))+
  scale_color_viridis(discrete = T, name = "Flagged outlier")+
  coord_fixed()+
  theme_bw()+
  theme(legend.position = "bottom")

