# Script for Riaz et al "Seal-fishery interactions in the Falkland Islands - operational and environmental factors drive resource competition" 
# Author: Javed Riaz

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
# Read in relevant packages

library(ggplot2)
library(tidyverse)
library(hms)
library(viridis)
library(scales)
library(readr)
library(rgdal)
library(grid)
library(raster)
library(stringr)
library(lubridate)
library(maptools)
library(sp)
library(sf)
library(rnaturalearthhires)
library(data.table)
library(parallel)
library(ggpubr)
library(raadtools)
library(rasterVis)
library(RColorBrewer)
library(devtools)
library(wesanderson)
library(geosphere)
library(mapview)
library(rerddapXtracto)
library(ggOceanMaps)
library(janitor)


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
# Some initial mapping stuff

setwd("P:/Projects/DPLUS168_pinniped bycatch/GIS shapefiles/FICZ")

FICZ<-readOGR("ukho_ficz_focz_limits.shp")
str(FICZ)
FICZ 

proj4string(FICZ) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

FICZ <- spTransform(FICZ,
                    CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


FICZ_df <- ggplot2::fortify(FICZ)

names(FICZ_df)[names(FICZ_df)=="long"]<-"x"
names(FICZ_df)[names(FICZ_df)=="lat"]<-"y"


Falklandsmap_1 <- ggplot() +
  geom_polygon(data = FICZ_df, 
               aes(x = x, y = y, group = group), colour = "black", fill = NA, linetype = "solid", linewidth = 1) + theme_bw() +
  scale_linetype_manual(values=c("dashed", "solid")) +
  theme(legend.position = "none")
Falklandsmap_1


# ##############################################################################################################################################
# ##############################################################################################################################################

setwd("P:/Projects/DPLUS168_pinniped bycatch/GIS shapefiles/Loligo Box/New/")

Loligo<-readOGR("LOL Box (without land).shp")

str(Loligo)


Loligo <- spTransform(Loligo,
                      CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


Loligo_df <- ggplot2::fortify(Loligo)

names(Loligo_df)[names(Loligo_df)=="long"]<-"x"
names(Loligo_df)[names(Loligo_df)=="lat"]<-"y"


Loligomap <- ggplot() +
  geom_polygon(data = Loligo_df, 
               aes(x = x, y = y, group = group), colour = "darkgrey", fill = "lightgrey", linetype = "dashed", linewidth = 1) + theme_bw()
Loligomap


Falklandsmap_2 <-  Falklandsmap_1 +
  geom_polygon(data = Loligo_df, aes(x = x, y = y, group = group), colour = "darkgrey", fill = "grey90",   linetype = "solid",linewidth = 1) + theme_bw()
Falklandsmap_2


# ##############################################################################################################################################
# ##############################################################################################################################################

setwd("P:/Projects/DPLUS168_pinniped bycatch/GIS shapefiles/")

Falklands<-readOGR("southamerica_adm0.shp")

Falklands <- crop(Falklands, extent(c(-70, -40, -58, -40))) 

proj4string(Falklands) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

Falklands <- spTransform(Falklands,
                         CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


Falklands_df <- ggplot2::fortify(Falklands)

names(Falklands_df)[names(Falklands_df)=="long"]<-"x"
names(Falklands_df)[names(Falklands_df)=="lat"]<-"y"


Falklandsmap <- ggplot() +
  geom_polygon(data = Falklands_df, 
               aes(x = x, y = y, group = group)) + theme_bw()
Falklandsmap


# ##############################################################################################################################################
# ##############################################################################################################################################

setwd("P:/Projects/DPLUS168_pinniped bycatch/JR_Code and Data/Bycatch data and code/")


BycatchMap <- basemap(limits = c(-62.5, -56, -53.3, -50), bathy.style = "rcb" , rotate = TRUE, grid.col = NA) + 
  theme(legend.position = "bottom")  +
  theme(legend.background = element_rect(fill = "white", colour = "black"),
        legend.key = element_rect(fill = "white")) 
BycatchMap

pdf("Basemap.pdf", width = 10, height = 6, onefile = FALSE)
BycatchMap
dev.off()


BycatchMap1 <- basemap(limits = c(-82, -32, -57, 10), bathymetry = FALSE , rotate = FALSE, grid.col = NA) + 
  theme(legend.position = "none")  +
  geom_polygon(data = FICZ_df, 
               aes(x = x, y = y, group = group), colour = "red", fill = NA, linetype = "solid", linewidth = 1) + theme_bw() +
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(
    # legend.spacing.y = unit(0, "mm"), 
    panel.border = element_rect(colour = "black", fill=NA),
    # aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")) +
  theme(panel.background = element_rect(fill = "white")) 
BycatchMap1


pdf("Fig1_Inset_MS.pdf", width = 6, height = 6, onefile = FALSE)
BycatchMap1
dev.off()


# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ###############################################################################################################################################
# # OBSERVER DATA 

setwd("P:/Projects/DPLUS168_pinniped bycatch/JR_Code and Data/Bycatch data and code/")


## Summary of Bycatch data

BycatchEvents <- read_csv("Just_BycatchEvents.csv")

sum(BycatchEvents$`No Mortalities`)

sum(BycatchEvents$`No Live Escapes (from water)`) + sum(BycatchEvents$`No Live Releases (from deck)`) + sum(BycatchEvents$`No Recaptured Carcasses`)

BycatchSummary <- BycatchEvents %>%
  group_by(Species, Year) %>%
  replace_na(list(Species = "SXX")) %>%
  summarise(No.Captures = sum(`No Captures`, na.rm = TRUE),
            No.Escaped_Water = sum(`No Live Escapes (from water)`, na.rm = TRUE),
            RealsedDeck = sum(`No Live Releases (from deck)`, na.rm = TRUE),
            No.Mortalities = sum(`No Mortalities`, na.rm = TRUE) + sum(`No Recaptured Carcasses`, na.rm = TRUE),
            Recaptures = sum(`No Recaptured Carcasses`, na.rm = TRUE)) %>% 
  mutate(CaptureRelease =  No.Escaped_Water + RealsedDeck ) %>%
  dplyr::select(Species, Year, CaptureRelease,No.Mortalities ) %>%
  adorn_totals("row")


BycatchSummary_Trawls <- BycatchEvents %>%
  dplyr::select(Species, Year, Vessel_ID, date, lon, lat) %>%
  distinct()


## Read in Observer Data and classify behaviours 

Observerdata <- read_csv("Cleaned_Observer.csv")

unique(Observerdata$Behaviour) 

## Classify interactions
No_InteractSeals <- Observerdata %>% ## 
  filter(Behaviour == "No sightings"|
           Behaviour == "Not observed") %>%
  mutate(Interactions = "No Interaction") %>%
  mutate(PresAbs = 0)



InteractSeals <- Observerdata %>% ## 10474 interactions
  filter(Behaviour == "Foraging around the net"|
           Behaviour ==  "Eating from the net and climbing"|
           Behaviour ==  "Eating from the net"|
           Behaviour ==  "Following the vessel"|
           Behaviour ==  "Swimming astern"|
           Behaviour ==  "Foraging in the discard chute area"|
           Behaviour ==  "Eating from the discard chute"|
           Behaviour ==  "Bycatch"|
           Behaviour ==  "Other") %>%
  mutate(Interactions = "Interaction") %>%
  mutate(PresAbs = 1)



Observerdata <- No_InteractSeals %>% ## Rejoin these data 
  full_join(InteractSeals) %>%
  arrange(date)

Observerdata$id <- paste(Observerdata$Vessel_ID, Observerdata$Trawl_ID) 


# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ###############################################################################################################################################
# # Read in fisheries catch data 

FisheriesData <- read_csv("FisheriesData.csv")

FisheriesData$date <- FisheriesData$Start_Date

FisheriesData$lon <- FisheriesData$Start_long

FisheriesData$lat <- FisheriesData$Start_lat


BulkFish <- FisheriesData %>% 
  ungroup() %>%
  dplyr::group_by(Vessel_ID, Trawl_ID, date, Year, lon, lat, End_lat, End_long, Month ) %>%
  summarise(Trawl_Duration = max(Trawl_Duration),
            Quantity = sum(Quantity)) %>%
  mutate(CPUE = Quantity/Trawl_Duration) %>%
  filter(Year == 2018 |
           Year == 2019|
           Year == 2020|
           Year == 2021|
           Year == 2022)


# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ###############################################################################################################################################
# # MERGE FISHERIES AND OBSERVER DATA - based on date and vessel ID


Observerdata <- Observerdata %>%
  left_join(BulkFish)

BycatchMap + ggspatial::geom_spatial_point(data = Observerdata, aes(x = lon, y = lat)) 

SpatialTracks <- Observerdata

unique(SpatialTracks$id)

sf_locs <- sf::st_as_sf(SpatialTracks, coords = c("lon","lat")) %>% 
  sf::st_set_crs(4326)
sf_locs

sf_lines <- sf_locs %>% 
  dplyr::arrange(id) %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(do_union = FALSE) %>% 
  sf::st_cast("MULTILINESTRING")


dat_sf <- st_as_sf(SpatialTracks, coords = c("lon", "lat")) %>% st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
dat_sd <- as(dat_sf, 'Spatial')


res2 <- over(dat_sd, Loligo)
Loligo_Points <- na.omit(res2)
pts2 = dat_sd[!is.na(over(dat_sd,as(Loligo,"SpatialPolygons"))),]
plot(pts2)


LoligoZone_Trawls <- as.data.frame(pts2)

names(LoligoZone_Trawls)[names(LoligoZone_Trawls) == 'coords.x1'] <- 'lon'
names(LoligoZone_Trawls)[names(LoligoZone_Trawls) == 'coords.x2'] <- 'lat'


Observerdata <- LoligoZone_Trawls 

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## Get unique observer events and merge records that had to be examined manually

Observerdata$Month <- lubridate::month(Observerdata$date)


Interactions <- Observerdata %>% ## Just single interaction events per trawl. 
  ungroup() %>%
  filter(PresAbs == 1) %>%
  dplyr::select(1, 11, 13:27) %>%
  distinct()

No_Interactions <- Observerdata %>% ## ## Just single non-interaction events per trawl. 
  ungroup() %>%
  filter(PresAbs == 0) %>%
  dplyr::select(1, 11, 13:27) %>%
  distinct()

Merged <- Interactions %>% 
  full_join(No_Interactions)

JustTrawls_unique <- Merged %>% #### Unique trawls
  dplyr::select(- Interactions, -PresAbs) %>%
  dplyr:: distinct() %>%
  ungroup()


JustInteractionEvents <- JustTrawls_unique %>% ## Joining the trawls with interaction events. All NA means no interactions recorded
  left_join(Interactions)

JustInteractionEvents$Interactions[is.na(JustInteractionEvents$Interactions)] <- "No Interaction"

JustInteractionEvents$PresAbs[is.na(JustInteractionEvents$PresAbs)] <- 0

JustInteractionEvents$count <- 1


NoInteractionCheck <- JustInteractionEvents %>% ### 
  filter(Interactions == "No Interaction")

InteractionCheck <- JustInteractionEvents %>% ### 
  filter(Interactions == "Interaction")


Model_Events <- JustInteractionEvents


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## A few data tweaks and summaries

Model_Events <- Model_Events %>%
  mutate(Season = case_when(Month == '2'| Month == '3'| Month == '4'| Month == '5'~ 'Season 1',
                            Month == '7' | Month == '8' | Month == '9'| Month == '10' ~ 'Season 2'))

unique(Model_Events$Month)


Model_Events <- Model_Events %>% ## Reset unrealistically small trawl duration to NA
  mutate(Trawl_Duration = ifelse(Trawl_Duration <= 30, NA, Trawl_Duration))  

Season_Summary <- Model_Events %>%
  group_by(Season) %>%
  summarise(n = sum(count),
            Prop = n/21097,
            Quant = mean(Quantity))


InteractionsSummary <- Model_Events %>%
  group_by(Season,PresAbs) %>%
  summarise(n = sum(count))


# ##############################################################################################################################################
# ##############################################################################################################################################
# ###############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ###############################################################################################################################################
# EXTRACT ENVIRONMENTAL VARIABLES   


Model_Events$ShortDate <- as.Date(Model_Events$date, format = "%Y-%m-%d", "GMT" )

# Bathymetry
bath <- raadtools::readderivaadc("bathymetry")
bath
Model_Events$Bathymetry  <- extract(bath, Model_Events[, c("lon", "lat")])
ggplot(Model_Events, aes(Bathymetry)) + geom_histogram(bins = 100) + theme_bw()
range(Model_Events$Bathymetry)

# Bathymetry slope
bathyslope <- raadtools::readderivaadc("bathymetry_slope")
bathyslope
Model_Events$BathymetrySlope  <- extract(bathyslope, Model_Events[, c("lon", "lat")])
ggplot(Model_Events, aes(BathymetrySlope)) + geom_histogram(bins = 100) + theme_bw()
range(Model_Events$BathymetrySlope)

# SST
Model_Events$SST <- extract(raadtools::readghrsst, Model_Events[, c("lon", "lat", "date")])
range(Model_Events$SST)
Model_Events$SST <- Model_Events$SST - 273.15 ## Convert Kelvin to C for GRHSST

# SSH
Model_Events$SSH <- extract(raadtools::readssh, Model_Events[, c("lon", "lat", "date")], latest = FALSE)
ggplot(Model_Events, aes(date, SSH, colour = lat)) + geom_point() + theme_bw()
range(Model_Events$SSH)


ENV_Model_Frame <- Model_Events  %>%
  dplyr::select(id, lon, lat, ShortDate, SST, SSH, Bathymetry, BathymetrySlope) %>%
  distinct()
# write_rds(ENV_Model_Frame, "Env_Data.rds")
ENV_Model_Frame <- read_rds("Env_Data.rds")


## Rejoin and save ENV-Observer

Model_Events <- Model_Events %>%
  left_join(ENV_Model_Frame)


# write_rds(Model_Events, "ENV-Observer.rds")

# ##############################################################################################################################################
# ##############################################################################################################################################
# ###############################################################################################################################################
# ##############################################################################################################################################
# ##############################################################################################################################################
# ###############################################################################################################################################
# CALCULATE SPATIAL VARIABLES 


library(geosphere)

count_points_within_distance_and_time <- function(df, distance_threshold, time_threshold) {
  num_points <- nrow(df)
  results <- numeric(num_points)
  for (i in 1:num_points) {
    # Get lat, lon, and timestamp
    current_lat <- df$lat[i]
    current_lon <- df$lon[i]
    current_time <- df$date[i]
    

    distances <- distGeo(matrix(c(current_lon, current_lat), ncol = 2),
                         matrix(c(df$lon, df$lat), ncol = 2))
    

    within_distance_and_time <- distances <= distance_threshold &
      abs(current_time - df$date) <= time_threshold * 3600  
    
    results[i] <- sum(within_distance_and_time)
  }
  
  return(results)
}

# Time lag
distance_threshold <- 20000  
time_threshold <- 24         
Model_Events$Radius <- count_points_within_distance_and_time(Model_Events, distance_threshold, time_threshold)

# Real-time
distance_threshold <- 20000  
time_threshold <- 5       

Model_Events$Realtime_Radius <- count_points_within_distance_and_time(Model_Events, distance_threshold, time_threshold)


Model_Events$Year <- lubridate::year(Model_Events$date)

Model_Events$Vessel_ID <- word(Model_Events$id, 1)

Model_Events <- Model_Events %>%
  dplyr::mutate(doy = lubridate::yday(date))


# write_rds(Model_Events, "ModelEvents_Cl.rds")


### Calculate distance from land

vessel_locations <- as.matrix(Model_Events[,c("lon", "lat")])
landmask_locations <- as.matrix(Falklands_df[,c("x", "y")])

Model_Events$distance_to_land <- apply(vessel_locations, 1, function(x) {
  min(distm(x, landmask_locations, fun = distHaversine))
})


write_rds(Model_Events, "Javed Data.rds")

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
# Summary discussion figure of fisheries catch trend
## Figure 4 of manuscript

library(bbplot)
library(scico)

CatchTable <- read_csv("CatchsummaryTable.csv")


wide_data <- read_csv("CatchsummaryTable.csv",
                      col_types = cols(YEAR = col_character(), 
                                       `1999` = col_double(), 
                                       `2000` = col_double(), 
                                       `2001` = col_double(), 
                                       `2002` = col_double(), 
                                       `2003` = col_double(), 
                                       `2004` = col_double(), 
                                       `2005` = col_double(), 
                                       `2006` = col_double(), 
                                       `2007` = col_double(), 
                                       `2008` = col_double(), 
                                       `2009` = col_double(), 
                                       `2010` = col_double(), 
                                       `2011` = col_double(), 
                                       `2012` = col_double(), 
                                       `2013` = col_double(), 
                                       `2014` = col_double(), 
                                       `2015` = col_double(), 
                                       `2016` = col_double(), 
                                       `2017` = col_double(), 
                                       `2018` = col_double(), 
                                       `2019` = col_double(), 
                                       `2020` = col_double(), 
                                       `2021` = col_double(), 
                                       `2022` = col_double()))

long_data <- pivot_longer(wide_data, cols = -YEAR, names_to = "Year", values_to = "Value")

unique(long_data$YEAR)

long_data$Species <- long_data$YEAR

FF_data <- long_data %>%
  filter(Species == "BLU"|
           Species == "HAK"|
           Species == "COX") %>%
  mutate(Group = "Finfish")

LL_data <- long_data %>%
  filter(Species == "LOL") %>%
  mutate(Group = "Loligo")

PlotData <- FF_data %>%
  full_join(LL_data)

PlotData$Year <- as.numeric(PlotData$Year)


JZR_style <- function() {
  font <- "Helvetica"
  ggplot2::theme(
    #Text format:
    plot.title = ggplot2::element_text(family=font,
                                       size=22,
                                       face="bold",
                                       color="#222222"),
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=22,
                                          margin=ggplot2::margin(9,0,9,0)),
    plot.caption = ggplot2::element_blank(),
    #Legend format
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=16,
                                        color="#222222"),
    
    #Axis format
    # axis.title = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(family=font,
                                       size=18,
                                       color="#222222"),
    axis.text = ggplot2::element_text(family=font,
                                      size=12,
                                      color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    panel.background = ggplot2::element_blank(),
    
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}


DiscussionFigure <- ggplot(PlotData, aes(x = Year, y = Value, colour = Species, fill = Species), group = Species) +
  geom_bar(stat = "identity", position = position_dodge2(width = 1, preserve = "single"), colour = "black") +
  labs(title = "Annual trend in catch species over time",
       x = "Year",
       y = "Catch quantity (tonnes)") +
  facet_wrap(~Group) +
  geom_smooth(method = "gam", se = FALSE, fullrange = FALSE) + 
  theme_bw() +
  # scale_fill_manual(values=c("#69b3a8", "#404080")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 101168)) +
  
  scale_x_continuous(breaks = c(2000,2005, 2010, 2015, 2020)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = c("#b8627db2",  "#f9a242b2", "#04233b", "#593d9cb2", "#1F968BFF"), labels = c("Blue whiting", "Rock cod", "Hake", "Loligo")) +
  scale_fill_manual(values = c("#b8627db2",  "#f9a242b2", "#04233b", "#593d9cb2", "#1F968BFF"), labels = c("Blue whiting", "Rock cod", "Hake", "Loligo")) +
  JZR_style ()

DiscussionFigure



## Create Figure 4

tiff("Discussion_Figure.tiff", width = 8, height = 6, units = 'in', res = 300)
DiscussionFigure
dev.off()


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
# Start analysing integrated observer, operational, spatiotemporal and environmental datasets

setwd("P:/Projects/DPLUS168_pinniped bycatch/JR_Code and Data/Bycatch data and code/")

Model_Frame <- read_rds("Javed Data.rds")

Model_Frame$distance_to_land <- Model_Frame$distance_to_land/1000

sum(Model_Frame$Quantity)/1000

Table2 <- Model_Frame %>%
  group_by(Season) %>%
  summarise(Freq = sum(count),
            meanDuration = mean(Trawl_Duration, na.rm = TRUE),
            sdDuration = sd(Trawl_Duration, na.rm = TRUE),
            minDuration = min(Trawl_Duration, na.rm = TRUE), 
            masDuration = max(Trawl_Duration, na.rm = TRUE),
            
            
            sumQuant = sum(Quantity), 
            meanQuant = mean(Quantity), 
            sdQuant = sd(Quantity), 
            minQuant = min(Quantity), 
            masQuant = max(Quantity),
            
            meanCPUE = mean(CPUE), 
            sdCPUE = sd(CPUE), 
            minCPUE = min(CPUE), 
            masCPUE = max(CPUE),
            
            meanSST = mean(SST), 
            sdSST = sd(SST), 
            minSST = min(SST), 
            masSST = max(SST),
            
            meanSSH = mean(SSH), 
            sdSSH = sd(SSH), 
            minSSH = min(SSH), 
            masSSH = max(SSH),
            
            meanBath = mean(Bathymetry), 
            sdBath = sd(Bathymetry), 
            minBath = min(Bathymetry), 
            masBath = max(Bathymetry),
            
            meanBSlop = mean(BathymetrySlope), 
            sdBSlop = sd(BathymetrySlope), 
            minBSlop = min(BathymetrySlope), 
            masBSlop = max(BathymetrySlope))



Model_Frame$PresAbs <- as.numeric(Model_Frame$PresAbs)


Check <- BycatchMap + ggspatial::geom_spatial_point(data = Model_Frame, aes(x = lon, y = lat, colour = distance_to_land)) +
  scale_colour_viridis_c() + theme(legend.position = "right")
Check


## Figure 1

PlotFrameA <- Model_Frame %>%
  ungroup() %>%
  mutate(Lat_BIN = as.character(cut(lat, breaks = seq(-54,-44, by = 0.25)))) %>% ## 6km
  mutate(Lon_BIN = as.character(cut(lon, breaks = seq(-64,-56, by = 0.50))))  %>% ## 3km
  mutate(Lon_MAX =  plyr::round_any(lon, 0.50, f = ceiling)) %>%
  mutate(Lon_MIN =  plyr::round_any(lon, 0.50, f = floor)) %>%
  mutate(Lat_MAX =  plyr::round_any(lat, 0.25, f = ceiling)) %>%
  mutate(Lat_MIN =  plyr::round_any(lat, 0.25, f = floor)) %>%
  mutate(count = 1) %>%
  group_by(Lon_MAX, Lon_MIN, Lat_MAX, Lat_MIN, Season) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(popvalue = (count - min(count))/(max(count) - min(count))) 
PlotFrameA


PlotFrameA1 <- Model_Frame %>%
  ungroup() %>%
  mutate(Lat_BIN = as.character(cut(lat, breaks = seq(-54,-44, by = 0.25)))) %>% ## 6km
  mutate(Lon_BIN = as.character(cut(lon, breaks = seq(-64,-56, by = 0.50))))  %>% ## 3km
  mutate(Lon_MAX =  plyr::round_any(lon, 0.50, f = ceiling)) %>%
  mutate(Lon_MIN =  plyr::round_any(lon, 0.50, f = floor)) %>%
  mutate(Lat_MAX =  plyr::round_any(lat, 0.25, f = ceiling)) %>%
  mutate(Lat_MIN =  plyr::round_any(lat, 0.25, f = floor)) %>%
  mutate(count = 1) %>%
  group_by(Lon_MAX, Lon_MIN, Lat_MAX, Lat_MIN, Season, Year) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(popvalue = (count - min(count))/(max(count) - min(count))) 
PlotFrameA1

PlotFrameB <- Model_Frame %>%
  ungroup() %>%
  mutate(Lat_BIN = as.character(cut(lat, breaks = seq(-54,-44, by = 0.25)))) %>% ## 6km
  mutate(Lon_BIN = as.character(cut(lon, breaks = seq(-64,-56, by = 0.50))))  %>% ## 3km
  mutate(Lon_MAX =  plyr::round_any(lon, 0.50, f = ceiling)) %>%
  mutate(Lon_MIN =  plyr::round_any(lon, 0.50, f = floor)) %>%
  mutate(Lat_MAX =  plyr::round_any(lat, 0.25, f = ceiling)) %>%
  mutate(Lat_MIN =  plyr::round_any(lat, 0.25, f = floor)) %>%
  mutate(count = 1) %>%
  group_by(Lon_MAX, Lon_MIN, Lat_MAX, Lat_MIN, Season, PresAbs, Interactions) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  group_by(PresAbs) %>%
  mutate(popvalue = (count - min(count))/(max(count) - min(count))) 


JustInteractions <- PlotFrameB %>%
  filter(Interactions == "Interaction")


PlotFrameB1 <- Model_Frame %>%
  ungroup() %>%
  mutate(Lat_BIN = as.character(cut(lat, breaks = seq(-54,-44, by = 0.25)))) %>% ## 6km
  mutate(Lon_BIN = as.character(cut(lon, breaks = seq(-64,-56, by = 0.50))))  %>% ## 3km
  mutate(Lon_MAX =  plyr::round_any(lon, 0.50, f = ceiling)) %>%
  mutate(Lon_MIN =  plyr::round_any(lon, 0.50, f = floor)) %>%
  mutate(Lat_MAX =  plyr::round_any(lat, 0.25, f = ceiling)) %>%
  mutate(Lat_MIN =  plyr::round_any(lat, 0.25, f = floor)) %>%
  mutate(count = 1) %>%
  group_by(Lon_MAX, Lon_MIN, Lat_MAX, Lat_MIN, Season, PresAbs, Interactions, Year) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  group_by(PresAbs) %>%
  mutate(popvalue = (count - min(count))/(max(count) - min(count))) 


JustInteractions1 <- PlotFrameB1 %>%
  filter(Interactions == "Interaction")

Seal_PlotA <- BycatchMap + 
    ggspatial::geom_spatial_polygon(data = Falklands_df, aes(x = x, y = y, group = group), fill = "grey25" ) +
  guides(fill ="none")  +   
  ggnewscale::new_scale_fill()+
  ggspatial::geom_spatial_rect(data = PlotFrameA, aes(xmin = Lon_MIN, xmax = Lon_MAX, ymin = Lat_MIN, ymax = Lat_MAX, fill = count)) +
  facet_wrap(~ Season, scales = "fixed") +
  scico::scale_fill_scico(palette = "tokyo", direction = -1)+
  labs(fill = "Total number\nof fishing\ntrawls") +
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) +
  guides(linetype ="none") + 
  # guides(fill ="none")  +   
  theme(legend.position = "right") + labs(colour = "Seal-fishery interaction", y = "Latitude")  +
  guides(colour = guide_legend(title.position="top", title.hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        # aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggspatial::geom_spatial_polygon(data = Loligo_df, aes(x = x, y = y, group = group), colour = "red", fill = NA,   linetype = "solid",linewidth = 0.3, alpha = 1) 
Seal_PlotA


Seal_PlotB <- BycatchMap + 
    ggspatial::geom_spatial_polygon(data = Falklands_df, aes(x = x, y = y, group = group), fill = "grey25" ) + 
  guides(fill ="none")  +   
  ggnewscale::new_scale_fill()+
  ggspatial::geom_spatial_rect(data = JustInteractions, aes(xmin = Lon_MIN, xmax = Lon_MAX, ymin = Lat_MIN, ymax = Lat_MAX, fill = count)) +
  facet_wrap(~ Season, scales = "fixed") +
  scale_fill_viridis_c(option = "A") +
  labs(fill = "Number of\nseal-fishery\ninteractions") +
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) +
  guides(linetype ="none") + 
  # guides(fill ="none")  +   
  labs(colour = "Seal-fishery interaction", x = "Longitude", y = "Latitude") +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        # aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) +
  ggspatial::geom_spatial_polygon(data = Loligo_df, aes(x = x, y = y, group = group), colour = "red", fill = NA,   linetype = "solid",linewidth = 0.3, alpha = 1) 

Seal_PlotB


## Save Fig 1A
pdf("Fig1_CH2_MS.pdf", width = 7, height = 5, onefile = FALSE)
ggarrange(Seal_PlotA, Seal_PlotB, ncol = 1, legend = "right", common.legend = FALSE, align = "hv")
dev.off()




Seal_PlotC <- BycatchMap + 
  ggspatial::geom_spatial_polygon(data = Falklands_df, aes(x = x, y = y, group = group), fill = "grey25" ) +
  guides(fill ="none")  +   
  ggnewscale::new_scale_fill()+
  ggspatial::geom_spatial_rect(data = PlotFrameA1, aes(xmin = Lon_MIN, xmax = Lon_MAX, ymin = Lat_MIN, ymax = Lat_MAX, fill = count)) +
  facet_grid(Season ~ Year, scales = "fixed") +
  scico::scale_fill_scico(palette = "tokyo", direction = -1)+
  labs(fill = "Total number\nof fishing\ntrawls") +
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) +
  guides(linetype ="none") + 
  # guides(fill ="none")  +   
  theme(legend.position = "right") + labs(colour = "Seal-fishery interaction", y = "Latitude")  +
  guides(colour = guide_legend(title.position="top", title.hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        # aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  theme(legend.key = element_rect(fill = "white")) +
  # theme(legend.position = c(0.15, 0.9190), legend.direction = "vertical")  +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(
    strip.background = element_blank(),
  ) +
  ggspatial::geom_spatial_polygon(data = Loligo_df, aes(x = x, y = y, group = group), colour = "red", fill = NA,   linetype = "solid",linewidth = 0.1, alpha = 0.5) 
Seal_PlotC




Seal_PlotD <- BycatchMap + 
  ggspatial::geom_spatial_polygon(data = Falklands_df, aes(x = x, y = y, group = group), fill = "grey25" ) +
  guides(fill ="none")  +   
  ggnewscale::new_scale_fill()+
  ggspatial::geom_spatial_rect(data = JustInteractions1, aes(xmin = Lon_MIN, xmax = Lon_MAX, ymin = Lat_MIN, ymax = Lat_MAX, fill = count)) +
  facet_grid(Season~ Year, scales = "fixed") +
  scale_fill_viridis_c(option = "A") +
  labs(fill = "Number of\nseal-fishery\ninteractions") +
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) +
  guides(linetype ="none") + 
  # guides(fill ="none")  +   
  labs(colour = "Seal-fishery interaction", x = "Longitude", y = "Latitude") +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        # aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) +
  ggspatial::geom_spatial_polygon(data = Loligo_df, aes(x = x, y = y, group = group), colour = "red", fill = NA,   linetype = "solid",linewidth = 0.1, alpha = 0.5) 

Seal_PlotD


## Save Fig 1B

pdf("Fig_S2_MS.pdf", width = 9, height = 6, onefile = FALSE)
ggarrange(Seal_PlotC, Seal_PlotD, ncol = 1, legend = "right", common.legend = FALSE, align = "hv")
dev.off()



##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
# Modelling with GAMMs

library(lme4)
library(performance)
library(mgcv)
library(corrplot)
library(DHARMa)
library(mgcViz)
library(ggcorrplot)
library(gratia)


Season1 <- Model_Frame %>%
  filter(Season == "Season 1")
unique(Season1$Month)

Season2 <- Model_Frame %>%
  filter(Season == "Season 2")
unique(Season2$Month)

##############################################################################################################################################
##############################################################################################################################################
### Correlation Plots

grob <- grobTree(textGrob("A) Season 1", x=0.05,  y=0.95, hjust=0,
                          gp=gpar(col="black", fontsize=15, fontface="bold")))


X1 <-Season1[ , c("Quantity", "Trawl_Duration", "CPUE", "lat", "Radius", "Realtime_Radius", "distance_to_land", "SST", "SSH", "Bathymetry", "BathymetrySlope")] 
colnames(X1) <- c("Quantity", "Duration", "CPUE", "Latitude", "Vclust (< 24hrs)", "Vclust (< 5hrs)", "Dist land", "SST", "SSH", "Bathy", "BSlope")


cor1 <- cor(X1, method = "spearman")
cor1

Cor1_Plot <- ggcorrplot(cor1, hc.order = FALSE, type = "lower",
                        outline.col = "white",
                        lab = FALSE,
                        ggtheme = ggplot2::theme_gray,
                        colors = c("#6D9EC1", "white", "#E46726")) +
  annotation_custom(grob)




grob1 <- grobTree(textGrob("B) Season 2", x=0.05,  y=0.95, hjust=0,
                           gp=gpar(col="black", fontsize=15, fontface="bold")))

X2 <-Season2[ , c("Quantity", "Trawl_Duration", "CPUE", "lat", "Radius", "Realtime_Radius", "distance_to_land", "SST", "SSH", "Bathymetry", "BathymetrySlope")] 
colnames(X2) <- c("Quantity", "Duration", "CPUE", "Latitude", "Vclust (< 24hrs)", "Vclust (< 5hrs)", "Dist land", "SST", "SSH", "Bathy", "BSlope")



cor2 <- cor(X2, method = "spearman")
cor2

Cor2_Plot <- ggcorrplot(cor2, hc.order = FALSE, type = "lower",
                        outline.col = "white",
                        lab = FALSE,
                        ggtheme = ggplot2::theme_gray,
                        colors = c("#6D9EC1", "white", "#E46726")) +
  annotation_custom(grob1)

FigS1_Cor <- Cor1_Plot + Cor2_Plot
FigS1_Cor

pdf("FigS1.pdf", width = 9, height = 6, onefile = FALSE)
FigS1_Cor
dev.off()

##############################################################################################################################################
##############################################################################################################################################

Model_Frame$Year <- as.factor(Model_Frame$Year)
Model_Frame$Vessel_ID <- as.factor(Model_Frame$Vessel_ID)
Model_Frame$Season <- as.factor(Model_Frame$Season)
Model_Frame$YearID <- paste(Model_Frame$Year, Model_Frame$Vessel_ID)
Model_Frame$YearID <- as.factor(Model_Frame$YearID)


Model_Frame$Scaled_Quantity <- datawizard::standardise(Model_Frame$Quantity)
Model_Frame$Scaled_TrawlDuration <- datawizard::standardise(Model_Frame$Trawl_Duration)


Season1 <- Model_Frame %>%
  filter(Season == "Season 1")
unique(Season1$Month)
Season1 <- within(Season1, Yearre <- relevel(Year, ref = "2020"))

Season2 <- Model_Frame %>%
  filter(Season == "Season 2")
unique(Season1$Month)
Season2 <- within(Season2, Yearre <- relevel(Year, ref = "2020"))


options(scipen=999)


##############################################################################################################################################
## Season 1 model
##############################################################################################################################################

Season_mod1 <- gam(PresAbs ~ s(lat, k = 10) + te(Month, SST, bs = c("cc", "tp"), k=c(5,10)) + s(SSH, k = 10) + s(Bathymetry, k = 10)+ s(BathymetrySlope, k = 10)+ Scaled_Quantity + Scaled_TrawlDuration + Radius + s(YearID, bs = "re"), data = Season1, method = "REML", family =  "binomial", knots=list(Month=c(0, 4)), na.action = "na.fail", select = TRUE)
summary(Season_mod1)
gam.check(Season_mod1)
draw(Season_mod1)
concurvity(Season_mod1, full=TRUE)
concurvity(Season_mod1, full=FALSE)
mgcv.helper::vif.gam(Season_mod1) 


## Refit the model based on VIF and concurvity
Season_mod1 <- gam(PresAbs ~  s(lat, k = 10) + te(Month, SST, bs = c("cc", "tp"), k=c(5,10)) + s(SSH, k = 12)  + s(BathymetrySlope, k = 10)+ Scaled_Quantity + s(YearID, bs = "re"), data = Season1, method = "REML", family =  "binomial", knots=list(Month=c(0, 4)), na.action = "na.fail", select = TRUE)
summary(Season_mod1)
gam.check(Season_mod1)
draw(Season_mod1)
concurvity(Season_mod1, full=TRUE)
concurvity(Season_mod1, full=FALSE)


plot1 <- draw(smooth_estimates(Season_mod1, smooth = c("s(lat)"))) +
  labs(title = NULL) + xlab("Latitude") + 
  labs(caption = NULL) + 
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) 
# theme(axis.title.x=element_blank(),
#       axis.text.x=element_blank(),
#       axis.ticks.x=element_blank()) 

plot1

plot2 <- draw(smooth_estimates(Season_mod1, smooth = c("te(Month,SST)"))) +
  labs(title = NULL) + labs(caption = NULL) + 
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) 
# theme(axis.title.x=element_blank(),
#       axis.text.x=element_blank(),
#       axis.ticks.x=element_blank()) 

plot2

plot3 <- draw(smooth_estimates(Season_mod1, smooth = c("s(BathymetrySlope)"))) +
  labs(title = NULL) + 
  labs(caption = NULL) + 
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) 
# theme(axis.title.x=element_blank(),
#       axis.text.x=element_blank(),
#       axis.ticks.x=element_blank()) 


plot3

plot4 <- draw(evaluate_parametric_term(Season_mod1, term = "Scaled_Quantity"), rug = FALSE) +
  # geom_rug(colour = "white") + 
  labs(title = NULL) + ylab("Partial effect") + xlab("Catch quantity [scaled]") + labs(caption = NULL) + 
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) 
# theme(axis.title.x=element_blank(),
#       axis.text.x=element_blank(),
#       axis.ticks.x=element_blank()) 

plot4


##############################################################################################################################################
## Season 2 model
##############################################################################################################################################

Season_mod2 <- gam(PresAbs ~ s(lat, k = 10) + te(Month, SST, bs = c("cc", "tp"), k=c(5,10)) + s(SSH, k = 10)  + s(Bathymetry, k = 10)+ s(BathymetrySlope, k = 10)+ Scaled_Quantity + Scaled_TrawlDuration + Radius + s(YearID, bs = "re"), data = Season1, method = "REML", family =  "binomial", knots=list(Month=c(0, 4)), na.action = "na.fail", select = TRUE)
summary(Season_mod2)
gam.check(Season_mod2)
draw(Season_mod2)
concurvity(Season_mod2)
mgcv.helper::vif.gam(Season_mod2)

## Refit the model based on VIF and concurvity
Season_mod2 <- gam(PresAbs ~ s(lat, k = 10) + te(Month, SST, bs = c("cc", "tp"), k=c(5,10)) + s(SSH, k = 10) + s(BathymetrySlope, k = 10)+ Scaled_Quantity + s(YearID, bs = "re"), data = Season2, method = "REML", family =  "binomial", knots=list(Month=c(0, 4)), na.action = "na.fail", select = TRUE)
summary(Season_mod2)
gam.check(Season_mod2)
draw(Season_mod2)
concurvity(Season_mod2, full=TRUE)
concurvity(Season_mod2, full=FALSE)


plot1_B <- draw(smooth_estimates(Season_mod2, smooth = c("s(lat)"))) +
  labs(title = NULL) + xlab("Latitude") + 
  labs(caption = NULL) + 
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) 

plot1_B

plot2_B <- draw(smooth_estimates(Season_mod2, smooth = c("te(Month,SST)"))) +
  labs(title = NULL) + labs(caption = NULL) + 
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) 
# theme(legend.direction = "horizontal") 
# guides(fill = guide_legend(direction = "horizontal")) 

plot2_B

plot3_B <- draw(smooth_estimates(Season_mod2, smooth = c("s(SSH)"))) +
  labs(title = NULL) + 
  labs(caption = NULL) + 
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) 


plot3_B

plot4_B <- draw(smooth_estimates(Season_mod2, smooth = c("s(BathymetrySlope)"))) +
  labs(title = NULL) + 
  labs(caption = NULL) +
  labs(x = "Bathymetry slope (°)") +
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) 


plot4_B

plot5_B <- draw(evaluate_parametric_term(Season_mod2, term = "Scaled_Quantity"), rug = FALSE) +
  # geom_rug(colour = "white") + 
  labs(title = NULL) + ylab("Partial effect") + xlab("Catch quantity [scaled]") + labs(caption = NULL) + 
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) 

plot5_B

##############################################################################################################################################
##############################################################################################################################################
# Plotting things to combine figures for Fig 2 of manuscript

Blankplot <- ggplot() +theme_void()


Fig2_CH2A <- ggarrange(plot4, plot1,  Blankplot, plot3 ,plot2,
                       nrow = 1, ncol = 5, common.legend = FALSE, legend = "none")
Fig2_CH2A



Fig2_CH2B <- ggarrange(plot5_B, plot1_B, plot3_B, plot4_B, plot2_B,
                       nrow = 1, ncol = 5, common.legend = FALSE, legend = "none")
Fig2_CH2B




Fig2_CH2 <- ggarrange(Fig2_CH2A, Fig2_CH2B,
                      nrow = 2, ncol = 1, common.legend = FALSE, legend = "none", align = "hv")
Fig2_CH2


## Save Figure 2
pdf("ModelResults.pdf", width = 9, height = 5)
Fig2_CH2
dev.off()




##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
# Mapping and data wrangling for catch distribution figures (Fig 3 and supplementary figs)
# Code provided by Tobi Buring

catchfile <- readRDS("P:/Projects/DPLUS168_pinniped bycatch/JR_Code and Data/Bycatch data and code/Tobi's stuff/Catchfile.rds")
setwd("P:/Projects/DPLUS168_pinniped bycatch/JR_Code and Data/Bycatch data and code/Tobi's stuff/")

LolBox <- catchfile %>% filter(Latitude <= -50, Longitude <= -56, Longitude >= -61, LicenceUsed %in% c("C", "X", "c", "x"))

ggplot(LolBox, aes(x = Longitude, y=Latitude))+
  geom_point()

Hake <- LolBox %>% filter(SpeciesCode == "HAK") %>% 
  group_by(Year, Month = Mont) %>% 
  summarise(Weight = sum(CatchWeight)/1000)

Hake$Date <- paste(Hake$Year, Hake$Month, "01", sep = "-")


library(scales)
ggplot(Hake, aes(x = as.Date(Date), y = Weight))+
  geom_bar(stat = "identity")+
  geom_smooth(method = "lm")+
  theme_bw()+
  scale_x_date(date_breaks = "2 year",
               date_labels = "%Y")

Hake <- LolBox %>% filter(SpeciesCode == "HAK") %>% 
  group_by(Year) %>% 
  summarise(Weight = sum(CatchWeight)/1000)

#Hake$Date <- paste(Hake$Year, Hake$Month, "01", sep = "-")

ggplot(Hake, aes(x = Year, y = Weight))+
  geom_bar(stat = "identity")+
  geom_smooth(method = "lm")+
  theme_bw()+
  ggtitle("Hake catches in LOL Box")+
  ylab("Catch Weight [t]")

# ggsave("Hake Catches.png", width = 12, height = 8)

# saveRDS(LolBox, "Catch File Lol Box.rds")

######

LOL<- LolBox %>% filter(SpeciesCode == "LOL", Year <= 2022) %>% 
  mutate(Week = as.numeric(strftime(ReportDate, format = "%V"))) %>% 
  mutate(Season = ifelse(Week %in% c(5:22), "First Season",
                         ifelse(Week %in% c(26:43), "Second Season", "Out of Season"))) %>% 
  group_by(Year, Season) %>% 
  summarise(Weight = sum(CatchWeight)/1000) 



ggplot(LOL, aes(x = Year, y = Weight))+
  geom_bar(stat = "identity")+
  geom_smooth(method = "lm")+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  ggtitle("Loligo catches in LOL Box")+
  ylab("Catch Weight [t]")+
  facet_wrap(~Season)

# ggsave("Loligo Catches per Seaspm.png", width = 12, height = 8)

#############
unique(catchfile$UnitEffortType)

Summary <- catchfile %>% 
  filter(UnitEffortType %in% c("Trawl Time", "Trawl time")) %>% 
  filter(SpeciesCode %in% c("HAK", "BLU", "PAR", "LOL")) %>% 
  mutate(Period = case_when(
    Year %in% c(2003, 2004, 2005, 2006, 2007) ~ "2003-2007",
    Year %in% c(2008, 2009, 2010, 2011, 2012) ~ "2008-2012",
    Year %in% c(2013, 2014, 2015, 2016, 2017) ~ "2013-2017",
    Year %in% c(2018, 2019, 2020, 2021, 2022) ~ "2018-2022"
  )) %>% 
  group_by(Period, GS, SpeciesCode) %>% 
  summarise(Weight = sum(CatchWeight), Latitude = mean(Latitude, na.rm = T), 
            Longitude = mean(Longitude, na.rm = T), Effort = sum(UnitEffort)) %>% ungroup() %>% 
  mutate(CPUE = Weight/Effort)

ggplot(Summary %>% filter(!is.na(Period)) %>% ungroup(), aes(x = Longitude, y = Latitude, color = Weight))+
  geom_point(size = 2)+
  facet_grid(SpeciesCode~Period)+
  scale_color_viridis_c(option = "H")

#############


Summary <- catchfile %>% 
  filter(UnitEffortType %in% c("Trawl Time", "Trawl time")) %>% 
  filter(SpeciesCode %in% c("HAK", "BLU", "PAR", "LOL")) %>% 
  mutate(Period = case_when(
    Year %in% c(2003, 2004, 2005, 2006, 2007) ~ "2003-2007",
    Year %in% c(2008, 2009, 2010, 2011, 2012) ~ "2008-2012",
    Year %in% c(2013, 2014, 2015, 2016, 2017) ~ "2013-2017",
    Year %in% c(2018, 2019, 2020, 2021, 2022) ~ "2018-2022"
  )) %>% 
  group_by(Period,Latitude,  Longitude, SpeciesCode) %>% 
  summarise(CatchWeight = sum(CatchWeight)/1000, Effort = sum(UnitEffort)/60) %>% ungroup() %>% 
  mutate(CPUE = CatchWeight/Effort)

Summary$ID <- paste(Summary$SpeciesCode, Summary$Period)

for(i in unique(Summary$ID)){
  temp <- Summary %>% filter(ID == i)
  write.csv(temp, paste(i, ".csv"), row.names = F)
}


ggplot() +geom_point(data = Summary, aes(x = Longitude, y = Latitude, colour = CatchWeight)) +
  facet_grid(SpeciesCode~Period)


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
#############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
#############################################################################################################################################
# END OF SCRIPT
# END OF SCRIPT
# END OF SCRIPT
# END OF SCRIPT


