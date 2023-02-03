library(eurostat)
library(sf)
library(tmap)
library(cartogram)
library(tidyverse)
library(ofce)
library(tmap)
# eurostat------------------
box <- c(left=-25,bottom=34,right=48,top=72)
bbox <-st_bbox( c(xmin=-25,ymin=30,xmax=48,ymax=72), crs = 4326)
bbox3035 <- c(xmin=1094760,ymin=2047400,xmax=5506230,ymax=5765620)

# We get spatail data at a low resolution, sufficient here
sf <- get_eurostat_geospatial(output_class = "sf",   resolution = "60", nuts_level = "all") 
st_crs(sf) <- st_crs(4326)
# checking data, NUTS level, bbox
bbox3035 <- st_crop(sf %>% filter(LEVL_CODE==1), bbox) %>% st_transform(3035) %>% st_bbox()
fdc3035 <-maptiles::get_tiles(bbox3035, zoom=5, provider = "Esri.WorldShadedRelief" )
fdc <- maptiles::get_tiles(bbox, crop = TRUE, zoom=5, provider = "Esri.WorldShadedRelief" )
fdc_p <- projectRaster(from=fdc, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
tm_shape(fdc3035, bbox=bbox3035, projection=st_crs(3035))+tm_rgb()+tm_shape(sf %>% st_transform(3035) %>% filter(LEVL_CODE==1))+tm_borders()

# We download pib and pop on eurostat
pib <- ofce::sna_get("nama_10r_3gdp")
pop <- ofce::sna_get("nama_10r_3popgdp")
# and labels
pib <- pib |> mutate(
  geo_label = label_eurostat(geo, dic="geo", fix_duplicated = TRUE) 
)
pib <- pib  |> 
  filter(geo!="EU27_2020", geo!="EU28") |>  
  complete(unit, geo, time, fill=list(values=NA))  |> 
  mutate(pays=str_sub(geo,1,2), 
         nuts3 = str_length(str_sub(geo,3,5))==2,
         nuts2 = str_length(str_sub(geo,3,5))==1,
         nuts1 = str_length(str_sub(geo,3,5))==0) %>% 
  left_join(pop %>% dplyr::select(geo, time, pop=THS), by=c("geo","time"))


pib.sf <- left_join(
  sf  |> dplyr::select(geo=NUTS_ID, LEVL_CODE),
  pib |> 
    filter(time=="2017-01-01", unit=="PPS_HAB_EU27_2020")
  , by="geo") |>
  st_transform(3035)

mb <- mapboxapi::get_static_tiles(
  location = pib2 |> st_transform(4326),
  zoom=2, 
  style_id = "ckjka0noe1eg819qrhuu1vigs", 
  username="xtimbeau") 

pib_map <- tm_shape(mb, raster.downsample = FALSE, bbox = bbox)+
  tm_rgb(alpha=1)+tm_shape(pib.sf  |>  filter(LEVL_CODE==1), bbox=bbox)+
  tm_fill(col="values", style="cont", palette="Reds", alpha=0.8)+
  tm_shape(pib.sf |>  filter(LEVL_CODE==0))+tm_borders()
tmap_save(pib2_map, filename = "GDP per capita map.svg", width=24, height=17, units="cm")

# we calculate a cartogram to represent correctly the chloropleth map
pib2_cart_0 <- cartogram_cont(pib2 %>% filter(LEVL_CODE==1), weight="pop_2017")
cont <- tm_shape(osm, raster.downsample = FALSE)+tm_rgb(alpha=0.25)+
  tm_shape(pib2_cart_0, bbox=bbox)+
  tm_polygons(col="gdp_2018", style="cont", palette="Reds", alpha=0.8)
tmap_save(cont, filename="GDP per capita cont map.svg", width=24, height=17, units="cm")
pib2_cart_1 <- cartogram_dorling(pib2 %>% filter(LEVL_CODE==1), weight="pop_2017", m_weight=0.1, k=0.33)
dorling <- tm_shape(osm, raster.downsample = FALSE)+tm_rgb(alpha=0.25)+
  tm_shape(pib2_cart_1, bbox=bbox)+
  tm_polygons(col="gdp_2018", style="cont", palette="Reds", alpha=0.8)+
  tm_shape(pib2 %>% filter(LEVL_CODE==0))+tm_borders()
tmap_save(dorling, filename = "GDP per capita dorling map.svg", width=24, height=17, units="cm")

pib2_cart_2 <- cartogram_ncont(pib2 %>% filter(LEVL_CODE==1), weight="pop_2017",  k=1)
ncont <- tm_shape(osm, raster.downsample = FALSE)+tm_rgb(alpha=0.25)+
  tm_shape(pib2_cart_2, bbox=bbox)+
  tm_polygons(col="gdp_2018", style="cont", palette="Reds", alpha=0.8)+
  tm_shape(pib2 %>% filter(LEVL_CODE==0))+tm_borders()
tmap_save(ncont, filename = "GDP per capita ncont map.svg", width=24, height=17, units="cm")