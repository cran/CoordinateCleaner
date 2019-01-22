## ---- echo = F, eval = T-------------------------------------------------
#LOCAL <- identical(Sys.getenv("TRUE"), "TRUE") # this defines if the vignette is build TRUEly using a large TRUE dataset or not, in which case the compilation will take very long
knitr::opts_chunk$set(fig.width = 7)

## ---- eval = F-----------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  
#  install_github("ropensci/CoordinateCleaner")

## ---- echo = F, eval = !nzchar(Sys.getenv("vignette_local")), warning = F, message = F----
#  library(dplyr)
#  library(ggplot2)
#  library(rgbif)
#  library(sp)
#  library(countrycode)
#  library(CoordinateCleaner)
#  
#  #Simulate data for CRAN to speed up vignette testing
#  
#  #obtain data from GBIF via rgbif
#  dat <- data.frame(
#    species = "Panthera leo",
#    decimalLongitude = runif(min = -23, max = 83, n = 10000),
#    decimalLatitude = runif(min = -60, max = 35, n = 10000),
#    countryCode = c("NA", "CG"),
#    individualCount = 1,
#    gbifID = sample(1:100000, size = 10000),
#    family = "Felidae",
#    taxonRnak = "species",
#    coordinateUncertaintyInMeters = rnorm(mean = 100000, sd = 10000, n= 10000),
#    year = sample(1960:2018, size = 10000, replace = TRUE),
#    basisOfRecord = "OBSERVATION",
#    institutionCode = "UU",
#    datasetName = sample(letters, size = 10000, replace = TRUE)
#    )
#  
#  # remove records without coordinates
#  dat <- dat%>%
#    filter(!is.na(decimalLongitude))%>%
#    filter(!is.na(decimalLatitude))
#  

## ---- echo = T, eval = nzchar(Sys.getenv("vignette_local")), warning = F, message = F----
library(dplyr)
library(ggplot2)
library(rgbif)
library(sp)
library(countrycode)
library(CoordinateCleaner)

#obtain data from GBIF via rgbif
dat <- occ_search(scientificName = "Panthera leo", limit = 5000,
                  return = "data", hasCoordinate = T)

# names(dat) #a lot of columns

#select columns of interest
dat <- dat %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, individualCount,
         gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
         basisOfRecord, institutionCode, datasetName)

# remove records without coordinates
dat <- dat%>%
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude))


## Visualize the data on a map

## ---- eval = TRUE, message = F, warning = F, fig.cap = "\\label{fig:al}Occurrence records for Panthera leo obtained from GBIF."----
#plot data to get an overview
wm <- borders("world", colour="gray50", fill="gray50")
ggplot()+ coord_fixed()+ wm +
  geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
  theme_bw()


## ---- eval = TRUE, warning = F, results = 'hold', collapse = T, fig.cap = "\\label{fig:automated}Records flagged by the automated cleaning."----
#convert country code from ISO2c to ISO3c
dat$countryCode <-  countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c')

#flag problems
dat <- data.frame(dat)
flags <- clean_coordinates(x = dat, lon = "decimalLongitude", lat = "decimalLatitude",
                          countries = "countryCode", 
                          species = "species",
                          tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                    "zeros", "countries")) # most test are on by default
summary(flags)
plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

## ---- eval = TRUE--------------------------------------------------------
#Exclude problematic records
dat_cl <- dat[flags$.summary,]

#The flagged records
dat_fl <- dat[!flags$.summary,]

## ---- warning= F, message = F, eval = F----------------------------------
#   #to avoid specifying it in each function
#  names(dat)[2:3] <- c("decimallongitude", "decimallatitude")
#  
#  clean <- dat%>%
#    cc_val()%>%
#    cc_equ()%>%
#    cc_cap()%>%
#    cc_cen()%>%
#    cc_coun(iso3 = "countryCode")%>%
#    cc_gbif()%>%
#    cc_inst()%>%
#    cc_sea()%>%
#    cc_zero()%>%
#    cc_outl()%>%
#    cc_dupl()

## ---- eval = FALSE-------------------------------------------------------
#  dat %>%
#      as_tibble() %>%
#      mutate(val = cc_val(., value = "flagged"),
#             sea = cc_sea(., value = "flagged"))

## ---- eval = TRUE, collapse = TRUE---------------------------------------
flags <- cf_age(x = dat_cl,
                lon = "decimalLongitude",
                lat = "decimalLatitude",
                taxon = "species", 
                min_age = "year", 
                max_age = "year", 
                value = "flagged")

dat_cl[!flags, "year"]

dat_cl <- dat_cl[flags, ]


## ---- eval = TRUE, collapse = T, fig.cap = "\\label{fig:automated}A histogram of the coordinate precision in the dataset.."----
#Remove records with low coordinate precision
hist(dat_cl$coordinateUncertaintyInMeters / 1000, breaks = 20)

dat_cl <- dat_cl %>%
  filter(coordinateUncertaintyInMeters / 1000 <= 100 | is.na(coordinateUncertaintyInMeters))

#Remove unsuitable data sources, especially fossils 
#which are responsible for the majority of problems in this case
table(dat$basisOfRecord)

dat_cl <- filter(dat_cl, basisOfRecord == "HUMAN_OBSERVATION" | 
                         basisOfRecord == "OBSERVATION" |
                         basisOfRecord == "PRESERVED_SPECIMEN")

## ---- eval = TRUE--------------------------------------------------------
#Individual count
table(dat_cl$individualCount)

dat_cl <- dat_cl%>%
  filter(individualCount > 0 | is.na(individualCount))%>%
  filter(individualCount < 99 | is.na(individualCount)) # high counts are not a problem

## ---- eval = TRUE--------------------------------------------------------
#Age of records
table(dat_cl$year)

dat_cl <- dat_cl%>%
  filter(year > 1945) # remove records from before second world war

## ---- collapse = T, eval = TRUE------------------------------------------
table(dat_cl$family) #that looks good
dat_cl <- dat_cl%>%
  filter(family == 'Felidae')

table(dat_cl$taxonRank) # this is also good


## ---- eval = TRUE--------------------------------------------------------
#exclude based on study area
dat_fin <- filter(dat_cl, decimalLatitude < 40)

## ---- eval = TRUE, collapse = T------------------------------------------

#create simple natural range for lions
range <- Polygon(cbind(c(-23, -7, 31, 71, 83, 42, 41, 24, -23), c(14, 37, 32, 27, 18, 0, -16, -38, 14)))
range <- Polygons(list(range), ID = c("A"))
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
range <- SpatialPolygons(list(range), proj4string = CRS(wgs84))
df <- data.frame(species = c("Panthera leo"), row.names = c("A"))
nat_range <- SpatialPolygonsDataFrame(range, data = as.data.frame(df))

# Visualize range
plo <- fortify(nat_range)

ggplot() +
  borders("world", colour="gray50", fill="gray50")+
  geom_polygon(data = plo, aes(x = long, y = lat, group = group))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_blank())

# run cc_iucn()
range_flags <- cc_iucn(x = dat_cl,
                       range = nat_range,
                       lon = "decimalLongitude",
                       lat = "decimalLatitude",
                       value = "flagged")

dat_fin <- dat_cl[range_flags, ]

## ---- eval = TRUE, echo = F, warning = F, fig.height = 10, fig.cap = "\\label{fig:final}The dataset of occurrence of lions after different cleaning phases."----
dat$cleaning = "Raw data"
dat_cl$cleaning = "Automatic cleaning"
dat_fin$cleaning = "Manual polishing"

plo <- bind_rows(dat, dat_cl, dat_fin)
plo$cleaning <- factor(plo$cleaning, levels = c("Raw data", "Automatic cleaning", "Manual polishing"))

wm <- borders("world", colour="gray50", fill="gray50")
ggplot() +
  coord_fixed()+
  wm +
  geom_point(data = plo, aes(x = decimalLongitude, y = decimalLatitude, colour = cleaning),
             size = 0.5)+
  scale_colour_discrete(c("darkred", "darkblue", "darkgreen"))+
  facet_wrap(~cleaning, ncol = 1)+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_blank())


## ---- eval = TRUE, warnings = F, message = F, fig.height = 6-------------
out.ddmm <- cd_ddmm(dat_cl, lon = "decimalLongitude", lat = "decimalLatitude", 
                    ds = "species", diagnostic = T, diff = 1,
                    value = "dataset")


## ---- eval = TRUE, echo = T, warning = F, eval = T, collapse = T, fig.height = 6, fig.cap = "\\label{fig:final}Diagnostic plots testing for rasterized sampling or excessive rounding. The left panel shows histograms of the record distribution, the right panel shows the autoorrelation plots. The upper panel shows longitude, the lower panel shows latitude. The logical flag in the heading of the right panel indicates the binary flag."----
par(mfrow = c(2,2), mar = rep(2, 4))
out.round <- cd_round(dat_fin, lon = "decimalLongitude", 
                      lat = "decimalLatitude", 
                      ds = "species",
                      value = "dataset",
                      T1 = 7,
                      graphs = T)

