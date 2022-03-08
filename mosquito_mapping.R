library(here)

mosquito_sampling_sites <- read.csv(here('data', 'input', 'mentor_mosquito_sampling_sites.csv'))

library(sf)
library(ggsn)
mosquito_sampling_sf <- mosquito_sampling_sites %>% 
  st_as_sf(coords=c('latitude', 'longitude'))


angola_oncho_mosquito_map <- angola_commune_oncho_noloaloa_collapse %>% 
  filter(adm1_en %in% c('benguela', 'cuanza_sul')) %>% 
  #filter(adm2_en %in% c('benguela', 'ganda', 'sumbe', 'quibala'))
  ggplot() +
  geom_sf(aes(fill = oncho_mean_aw)) +
  geom_sf(fill = "transparent", color = "black", size = 1.5,
          data = adm1_shp) +
  geom_point(
    data = mosquito_sampling_sites, aes(x = longitude, y = latitude),
    fill = "red", color = "red", alpha =1, size=4
  ) +
  labs(fill="Environmental suitability (Mean)") +
  theme(legend.position = "top") +
  commune_fill

province_oncho_mosquito_map <- angola_commune_oncho_collapse %>% 
  filter(adm1_en %in% c('benguela', 'cuanza_sul')) %>% 
  #filter(adm2_en %in% c('benguela', 'ganda', 'sumbe', 'quibala'))
  ggplot() +
  geom_sf(aes(fill = oncho_mean_aw)) +
  geom_sf_label_repel(aes(label = adm2_en)) +
  geom_point(
    data = mosquito_sampling_sites, aes(x = longitude, y = latitude),
    fill = "red", color = "red", alpha =1, size=4
  ) +
  geom_text(
    data = mosquito_sampling_sites,
    aes(x = longitude, y = latitude,label=sentinel.site),color="red", 
    nudge_x = 0.15, nudge_y = 0.15, 
    check_overlap = T
  ) +
  labs(fill="Environmental suitability (Mean)") +
  theme(legend.position = "top") +
  commune_fill

# Focus on Quibala & Ganda
angola_commune_oncho_collapse %>% 
  filter(adm3_en %in% c('kibala', 'ganda')) %>% 
  ggplot() +
  geom_sf(aes(fill = oncho_mean_aw)) +
  geom_sf_label_repel(aes(label = adm3_en)) +
  geom_point(
    data = mosquito_sampling_sites %>% filter(sentinel.site %in% c('Quibala', 'Ganda')), aes(x = longitude, y = latitude),
    fill = "red", color = "red", alpha =1, size=4
  ) +
  labs(fill="Environmental suitability (Mean)") +
  theme(legend.position = "top") +
  commune_fill

kib_gan_shp <- angola_commune_oncho_collapse %>% 
  filter(adm3_en %in% c('kibala', 'ganda'))

#Add raster data
africa_oncho_raster <- raster::raster(here('data', 'IHME_AFRICA_ONCHO_ENV_SUIT_MEAN_Y2020M08D26.TIF'))

kib_gan_raster_crop <- mask(africa_oncho_raster, kib_gan_shp)

kib_gan_raster_crop <- crop(africa_oncho_raster, kib_gan_raster_crop)

kib_gan_raster_point_df  <-  rasterToPoints(kib_gan_raster_crop) %>% as_tibble() 
colnames(kib_gan_raster_point_df) = c("lon", "lat", "suitability")

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(1, 100))

kib_gan_map <- ggplot() +
  #geom_sf() +
  geom_tile(data = kib_gan_raster_point_df %>% filter(suitability>=50), aes(lon, lat, fill = suitability), alpha = .7) +
  geom_sf(fill = "transparent", color = "black", size = 1.5, 
          data = kib_gan_shp) +
  geom_sf_label_repel(data=kib_gan_shp,aes(label = adm3_en)) +
  geom_point(
    data = mosquito_sampling_sites %>% filter(sentinel.site %in% c('Quibala', 'Ganda')), aes(x = longitude, y = latitude),
    fill = "red", color = "red", shape=8, alpha =1, size=4
  ) +
  labs(fill='Environmental suitability') +
  theme(legend.position='top') +
  sc +
  north(kib_gan_shp) +
  scalebar(kib_gan_shp, dist = 25, dist_unit = "km",
           transform = TRUE, model = "WGS84")


#Read in lakes and format file
rivers <-
  sf::st_read(here('data','input','rivers', 'afrivs.shp'))

rivers_interest <- sf::st_intersection(rivers,kib_gan_shp)

kib_gan_rivers_map <- ggplot() +
  #geom_sf() +
  geom_tile(data = kib_gan_raster_point_df %>% filter(suitability>=50), aes(lon, lat, fill = suitability), alpha = .7) +
  geom_sf(fill = "transparent", color = "black", size = 1.5,
          data = kib_gan_shp) +
  geom_sf_label_repel(data=kib_gan_shp,aes(label = adm3_en)) +
  geom_sf(data=rivers_interest, col="blue", lwd=0.4) +
  geom_point(
    data = mosquito_sampling_sites %>% filter(sentinel.site %in% c('Quibala', 'Ganda')), aes(x = longitude, y = latitude),
    fill = "red", color = "red", shape=8, alpha =1, size=4
  ) +
  labs(fill='Environmental suitability') +
  theme(legend.position='top') +
  sc +
  north(kib_gan_shp) +
  scalebar(kib_gan_shp, dist = 25, dist_unit = "km",
           transform = TRUE, model = "WGS84")

library(httr)

get_flowlines <- function(streamorder, mapRange){
  postURL <- "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows"
  
  filterXML <- paste0('<?xml version="1.0"?>',
                      '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="shape-zip" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                      '<wfs:Query xmlns:feature="https://gov.usgs.cida/nhdplus" typeName="feature:nhdflowline_network" srsName="EPSG:4326">',
                      '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
                      '<ogc:And>',
                      '<ogc:PropertyIsGreaterThan>',
                      '<ogc:PropertyName>streamorde</ogc:PropertyName>',
                      '<ogc:Literal>',streamorder-1,'</ogc:Literal>',
                      '</ogc:PropertyIsGreaterThan>',
                      '<ogc:BBOX>',
                      '<ogc:PropertyName>the_geom</ogc:PropertyName>',
                      '<gml:Envelope>',
                      '<gml:lowerCorner>',mapRange[3]," ",mapRange[1],'</gml:lowerCorner>',
                      '<gml:upperCorner>',mapRange[4]," ",mapRange[2],'</gml:upperCorner>',
                      '</gml:Envelope>',
                      '</ogc:BBOX>',
                      '</ogc:And>',
                      '</ogc:Filter>',
                      '</wfs:Query>',
                      '</wfs:GetFeature>')
  
  destination = file.path(tempdir(),"nhdflowline_network.zip")
  file <- POST(postURL, body = filterXML, write_disk(destination, overwrite=T))
  
  filePath <- tempdir()
  print("unzipping...")
  unzip(destination, exdir = filePath)
  
  flowLines <- st_read(filePath, layer = 'nhdflowline_network')
  
  return(flowLines)
}

mapRange <- c(range(st_coordinates(kib_gan_shp)[,1]),range(st_coordinates(kib_gan_shp)[,2]))
rivers4 <- get_flowlines(4, mapRange)
  