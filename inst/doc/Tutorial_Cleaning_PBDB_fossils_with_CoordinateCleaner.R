## ---- echo = F, eval = T-------------------------------------------------
#LOCAL <- identical(Sys.getenv("TRUE"), "TRUE")
knitr::opts_chunk$set(warning = FALSE, fig.width = 7)

## ---- eval = F-----------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  
#  install_github("ropensci/CoordinateCleaner")

## ---- echo = T, warning = F, message = F, eval = TRUE--------------------
library(dplyr)
library(ggplot2)
library(CoordinateCleaner)
library(countrycode)
library(paleobioDB)

## ---- eval = F, collapse = TRUE------------------------------------------
#  #load data
#  dat <- paleobioDB::pbdb_occurrences(base_name = "Magnoliopsida", vocab = "pbdb", limit = 5000,
#                          show = c("coords", "phylo", "attr", "loc", "time", "rem"))
#  rownames(dat) <- NULL

## ---- eval = TRUE, collapse = TRUE, echo = FALSE-------------------------
data(pbdb_example)
dat <- pbdb_example
rownames(dat) <- NULL

## ---- echo = F, eval = F, message = F------------------------------------
#  dat <- read_csv("tutorial_s3_example_input_data",  guess_max = 10000)

## ---- eval = TRUE, collapse = T------------------------------------------
#plot data to get an overview
wm <- borders("world", colour="gray50", fill="gray50")
ggplot()+ coord_fixed()+ wm +
  geom_point(data = dat, aes(x = lng, y = lat),
             colour = "darkred", size = 0.5)+
  theme_bw()


## ---- eval = TRUE--------------------------------------------------------
cl <- cc_val(dat, lat = "lat", lon = "lng")

## ---- eval = TRUE--------------------------------------------------------
cl <- cc_equ(cl, lat = "lat", lon = "lng")

## ---- eval = TRUE, collapse = T------------------------------------------
fl <- cc_equ(dat, value = "flagged", lat = "lat", lon = "lng")

# extract and check the flagged records
fl_rec <- dat[!fl,] 
head(fl_rec)

## ---- collapse = T, eval = TRUE, collapse = T----------------------------
fl <- cc_cen(cl, lat = "lat", lon = "lng", value = "flagged")
fl_rec <- cl[!fl, ]
unique(fl_rec$cc)
cl <- cl[fl, ]

## ---- eval = TRUE, collapse = T------------------------------------------
#adapt country code to ISO3, for country test
cs_ma <- "GBR"
names(cs_ma) <- "UK"
cl$cc_iso3 <- countrycode(cl$cc, origin = "iso2c", destination = "iso3c", custom_match = cs_ma)

cl <- cc_coun(cl, lat = "lat", lon = "lng", iso3 = "cc_iso3")

## ---- eval = TRUE, collapse = T------------------------------------------
cl <- cc_inst(cl, lat = "lat", lon = "lng")
cl <- cc_gbif(cl, lat = "lat", lon = "lng")

## ---- eval = TRUE, collapse = T------------------------------------------
cl <- cc_zero(cl, lat = "lat", lon = "lng")

## ---- eval = TRUE, collapse = T------------------------------------------
cl <- cl[!is.na(cl$late_age),]
cl <- cl[!is.na(cl$early_age),]
cl <- cf_equal(cl, min_age = "late_age", max_age = "early_age")

## ---- eval = TRUE, collapse = T------------------------------------------
rang <- cl$early_age - cl$late_age
hist(rang, breaks = 40, xlab = "Date range [max age - min age]", main = "")

## ---- eval = TRUE, collapse = T------------------------------------------
# Outlier dataset
cl <- cf_range(cl, taxon = "", min_age = "late_age", max_age = "early_age")

# Outlier per taxon
cl <- cf_range(cl, taxon = "taxon_name", min_age = "late_age", max_age = "early_age")

# Absolute age limit
cl <- cf_range(cl, taxon = "taxon_name", min_age = "late_age", 
               max_age = "early_age", method = "time", max_range = 35)

rang <- cl$early_age - cl$late_age
hist(rang, breaks = 40, xlab = "Date range [max age - min age]", main = "")


## ---- eval = TRUE, collapse = T------------------------------------------
# Outlier dataset
cl <- cf_outl(cl, taxon = "", lat = "lat", lon = "lng",
              min_age = "late_age", max_age = "early_age")

# Outlier taxon
cl <- cf_outl(cl, taxon = "taxon_name", lat = "lat", lon = "lng",
              min_age = "late_age", max_age = "early_age")

## ---- eval = TRUE--------------------------------------------------------
nrow(dat) - nrow(cl)

## ---- eval = F, collapse = T---------------------------------------------
#  cl <- dat%>%
#    cc_val(lat = "lat", lon = "lng")%>%
#    cc_equ(lat = "lat", lon = "lng")%>%
#    cc_cen(lat = "lat", lon = "lng")%>%
#    cc_coun(lat = "lat", lon = "lng", iso3 = "cc")%>%
#    cc_gbif(lat = "lat", lon = "lng")%>%
#    cc_inst(lat = "lat", lon = "lng")%>%
#    cc_zero(lat = "lat", lon = "lng")%>%
#    cf_equal(min_age = "late_age", max_age = "early_age")%>%
#    cf_range(taxon = "taxon_name",
#             min_age = "late_age", max_age = "early_age")%>%
#    cf_outl(taxon = "taxon_name",
#            lat = "lat", lon = "lng",
#            min_age = "late_age", max_age = "early_age")

## ---- eval = F, collapse = T---------------------------------------------
#  #adapt country code to ISO3, for country test
#  cs_ma <- "GBR"
#  names(cs_ma) <- "UK"
#  dat$cc <- countrycode(dat$cc, origin = "iso2c", destination = "iso3c", custom_match = cs_ma)
#  
#  #run automated testing
#  flags <- clean_fossils(x = dat,
#                               taxon = "taxon_name",
#                               min_age = "late_age", max_age = "early_age",
#                               value = "spatialvalid")
#  
#  head(flags)
#  cl <- dat[flags$.summary,] #the cleaned records
#  fl_rec <- dat[!flags$.summary,] # the flagged records for verification

## ---- eval = TRUE, collapse = T------------------------------------------
#1. This looks OK
table(cl$phylum)

#2. Taxonomic level of identification
table(cl$taxon_rank)

## ---- eval = TRUE, collapse = T------------------------------------------
cl <- cl %>%
  filter(taxon_rank %in% c("species", "genus"))

## ---- eval = TRUE--------------------------------------------------------
table(cl$geogscale)

## ---- collapse = T, fig.height = 4, eval = TRUE--------------------------
#minimum ages
tail(table(cl$late_age))

ggplot(cl)+
  geom_histogram(aes(x = late_age))

#maximum ages
tail(table(cl$early_age))

ggplot(cl)+
  geom_histogram(aes(x = early_age))

## ---- eval = F, collapse = T---------------------------------------------
#  
#  table(cl$latlng_basis)
#  table(cl$latlng_precision)
#  
#  
#  table(cl$ref_pubyr)
#  table(cl$collection_type)
#  table(cl$preservation_quality)
#  table(cl$plant_organ)
#  
#  cl <- filter(cl, preservation_quality != "very poor")

## ---- echo = F, fig.height = 4, eval = TRUE, fig.height = 6--------------
wm <- borders("world", colour="gray50", fill="gray50")

ggplot()+ coord_fixed()+ wm +
  geom_point(data = dat, aes(x = lng, y = lat),
             colour = "darkred", size = 0.5)+
  theme_bw()

ggplot()+ coord_fixed()+ wm +
  geom_point(data = cl, aes(x = lng, y = lat),
             colour = "darkgreen", size = 0.5)+
  theme_bw()

## ---- message = F, warning = F, eval = F, fig.height = 6-----------------
#  # replace  blanks in taxon names
#  cl$taxon_name <- gsub("[[:blank:]]{1,}","_", cl$taxon_name)
#  
#  #simulated current status, soley for demonstration purposes, replace with your own data
#  mock_status <- data.frame(taxon_name = unique(cl$taxon_name),
#                            status = sample(c("extinct", "extant"),
#                                            size = length(unique(cl$taxon_name)),
#                                            replace = TRUE))
#  
#  #add current status to fossils
#  cl2 <- inner_join(cl, mock_status, by = "taxon_name")
#  
#  #Write PyRate input to disk
#  write_pyrate(cl, fname = "paleobioDB_angiosperms", status = cl2$status,
#              taxon = "taxon_name", min_age = "late_age", max_age = "early_age")

