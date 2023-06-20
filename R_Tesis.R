#------------------------------TESIS_CODE--------------------------------------

rm(list = ls()) #Clear the work space

#Load required packages

library(readxl) #Allows to read xls or xlsx files
library(openxlsx) #Read, write and edit xlsx files
library(xts) #Extensible Time Series
library(data.table) #Fast aggregation of large data
library(tidytable) #A tidy interface to 'data.table'
library(lubridate) #Make dealing with dates a little easier
library(tidyverse) #Set of packages that work in harmony
library(sf) #Support for simple features, a standardized way to encode
#spatial vector data
library(raster) #Provides classes and functions to manipulate geographic
#(spatial) data in 'raster' format
library(spdplyr) #Methods for 'dplyr' verbs for 'sp' 'Spatial' classes
library(sp) #Classes and methods for spatial data; the classes document
#where the spatial location information resides, for 2D or 3D data
library(maptools) #Set of tools for manipulating geographic data
library(dplyr) #A fast, consistent tool for working with data frame like
#objects, both in memory and out of memory
library(terra) #Methods for spatial data analysis with vector (points,
#lines, polygons) and raster (grid) data
library(rgdal) #Provides bindings to the 'Geospatial' Data Abstraction 
#Library ('GDAL') (>= 1.6.3) and access to projection/transformation
#operations from the 'PROJ.4' library
library(phylin) #It has functions for IDW interpolation using genetic data
#and midpoints
library(viridis) #Viridis color palette
library(lattice)
library(rasterVis)
library(gridExtra)
library(extrafont)
library(showtext)
library(patchwork)
library(latticeExtra)
library(ggpubr)
library(rnaturalearth)
library(rnaturalearthdata)

#Defining the working directory
setwd("C:/Users/David Gomez/Desktop/TESIS/DATOS")

#Function to be able to discard stations without data - precipitation
agg <- function(x) {
  if (sum(is.na(x)) >= 0.25 * length(x)) { #defined percentage
    return(NA)
  }
  else {
    return(sum(x, na.rm = TRUE))
  }
}

#Reading precipitation information
data_ppt <- read.xlsx("DatosIDW_221205.xlsx", "PTPM_CON")

#Sorting the precipitation data by dates
data_ppt <- as.xts(data_ppt[ , -1], 
                   order.by = as.Date(data_ppt[ , 1],
                                      format = "%Y-%m-%d"))

#Obtaining the annual precipitation of each station
data_y_ppt <- aggregate(data_ppt, by = year(index(data_ppt)), agg) %>% 
  data.frame(check.names = FALSE)

#Creating a variable that combines year and month
year_month_ppt <- format(index(data_ppt), "%Y-%m")

#Obtaining the monthly precipitation of each year
data_ym_ppt <- aggregate(data_ppt, by = list(year_month_ppt), agg) %>% 
  data.frame(check.names = FALSE)

#Function to be able to discard stations without data - temperature
agg_mt <- function(x) {
  if (sum(is.na(x)) >= 0.25 * length(x)) { #defined percentage
    return(NA)
  }
  else {
    return(mean(x, na.rm = TRUE))
  }
}

#Reading mean temperature information
data_mt <- read.xlsx("DatosIDW_221205.xlsx", "TSSM_D")

#Sorting the temperature data by dates 
data_mt <- as.xts(data_mt[ , -1],
                  order.by = as.Date(data_mt[ , 1],
                                     format = "%Y-%m-%d"))

#Obtaining the annual temperature of each station
data_y_mt <- aggregate(data_mt, by = year(index(data_mt)), agg_mt) %>% 
  data.frame(check.names = FALSE)

#Creating a variable that combines year and month
year_month_mt <- format(index(data_mt), "%Y-%m")

#Obtaining the average monthly temperature for each year 
data_ym_mt <- aggregate(data_mt, by = list(year_month_mt), agg_mt) %>% 
  data.frame(check.names = FALSE)

#Reading maximum temperature information
data_maxt <- read.xlsx("DatosIDW_221205.xlsx", "TMX_CON")

#Sorting the maximum temperature data by dates 
data_maxt <- as.xts(data_maxt[ , -1],
                    order.by = as.Date(data_maxt[ , 1],
                                       format = "%Y-%m-%d"))

#Creating a variable that combines year and month
year_month_maxt <- format(index(data_maxt), "%Y-%m")

#Obtaining the maximum temperature for each year
data_ym_maxt <- aggregate(data_maxt, by = list(year_month_maxt), agg_mt) %>% 
  data.frame(check.names = FALSE)

#Reading minimum temperature information
data_mint <- read.xlsx("DatosIDW_221205.xlsx", "TMN_CON")

#Sorting the minimum temperature data by dates
data_mint <- as.xts(data_mint[ , -1],
                    order.by = as.Date(data_mint[ , 1],
                                       format = "%Y-%m-%d"))

#Creating a variable that combines year and month
year_month_mint <- format(index(data_mint), "%Y-%m")

#Obtaining the minimum temperature for each year
data_ym_mint <- aggregate(data_mint, by = list(year_month_mint), agg_mt) %>% 
  data.frame(check.names = FALSE)

#The vector is loaded with the precpitation stations
v_cln_ppt <- colnames(data_ppt)

#The vector is loaded with the mean temperature
v_cln_mt <- colnames(data_mt)

#The vector is loaded with the maximum temperature
v_cln_maxt <- colnames(data_maxt)

#The vector is loaded with the minimum temperature
v_cln_mint <- colnames(data_mint)

#The shapefile containing all the IDEAM stations is loaded and the
#projection of the coordinates is made to the Colombian code
sta_all <- vect("Shapes/CNE_IDEAM.shp") %>% terra::project("epsg:9377") 

#The shapefile is filtered by the precipitation stations of the study area
sta_ppt <- sta_all[sta_all$CODIGO %in% v_cln_ppt , ]

#The result is exported in a shapefile
writeVector(sta_ppt, "RESULTS/sta_ppt.shp", overwrite = TRUE)

#The dataframe with the annual precipitation data is ordered according to 
#the stations in the area, this for subsequent interpolation
data_y_ppt <- data_y_ppt[ , sta_ppt$CODIGO]

#The dataframe with the monthly precipitation data is ordered according to 
#the stations in the area, this for subsequent interpolation
data_ym_ppt <- data_ym_ppt[ , sta_ppt$CODIGO]

#The x & y plane coordinates of the precipitation stations are obtained and
#saved as a dataframe, in order to perform the interpolation
sta_ppt <- terra::geom(sta_ppt)[ , c("x" , "y")]
sta_ppt <- as.data.frame(sta_ppt)

#The shapefile is filtered by the mean temperature stations of the study 
#area
sta_mt <- sta_all[sta_all$CODIGO %in% v_cln_mt , ]

#The result is exported in a shapefile
writeVector(sta_mt, "RESULTS/sta_mt.shp", overwrite = TRUE)

#The dataframe with the mean annual temperature data is ordered according to 
#the stations in the area, this for subsequent interpolation
data_y_mt <- data_y_mt[ , sta_mt$CODIGO]

#The dataframe with the mean monthly temperature data is ordered according to 
#the stations in the area, this for subsequent interpolation
data_ym_mt <- data_ym_mt[ , sta_mt$CODIGO]

#The x & y plane coordinates of the mean temperature stations are obtained 
#and saved as a dataframe, in order to perform the interpolation
sta_mt <- terra::geom(sta_mt)[ , c("x" , "y")]
sta_mt <- as.data.frame(sta_mt)

#A shapefile is loaded with the temperature stations with an attribute of 
#elevation
sta_mt_z <- vect("RESULTS/sta_mt_z.shp") %>% terra::project("epsg:9377")

#A dataframe is created where the name of the temperature stations is stored
#with their respective elevation
mt_z_df <- data.frame(CODIGO = sta_mt_z$CODIGO, z = sta_mt_z$z1)

#Temperatur correction factor due to elevation is applied
mt_z_df <- mt_z_df %>% mutate(corr_t = z * 0.0065)

#data_y_mt <- data_y_mt %>%
#  mutate(across(all_of(mt_z_df$CODIGO), ~ . + 
#                  mt_z_df$corr_t[match(cur_column(), mt_z_df$CODIGO)]))

#'cols_mtch' contains the positions of the column in 'data_y_mt' that
#correspond to the values in the 'CODIGO' column of 'mt_z_df'
cols_mtch <- match(mt_z_df$CODIGO, colnames(data_y_mt))

#'data_y_mtz' is a modified version of 'data_y_mt' where the values in the
##columns that match 'cols_mtch' have been incremented by the corresponding
#values of 'corr_t' from 'mt_zd_df'. In this line of code the temperature
#correction is applied to all the annual data of the stations due to the
#altitude
data_y_mtz <- t(apply(data_y_mt[,cols_mtch], 1, 
                      function(x) x + (mt_z_df$corr_t)))[,names(data_y_mt)] %>% 
  as.data.frame()

#'cols_mtch_ym_my' contains the positions of the column in 'data_ym_mt' that
#correspond to the values in the 'CODIGO' column of 'mt_z_df'
cols_mtch_ym_mt <- match(mt_z_df$CODIGO, colnames(data_ym_mt))

#'data_ym_mtz' is a modified version of 'data_ym_mt' where the values in the
##columns that match 'cols_mtch_ym_mt' have been incremented by the 
#corresponding values of 'corr_t' from 'mt_zd_df'. In this line of code the 
#temperature correction is applied to all the monthly data of the stations due 
#to the altitude
data_ym_mtz <- t(apply(data_ym_mt[,cols_mtch_ym_mt], 1, 
                      function(x) x + (mt_z_df$corr_t)))[,names(data_ym_mt)] %>% 
  as.data.frame()

###-------------MAX--------
#The shapefile containing all the IDEAM stations is loaded and the
#projection of the coordinates is made to the Colombian code
#sta_maxt <- vect("Shapes/CNE_IDEAM.shp") %>% terra::project("epsg:9377") 

#The shapefile is filtered by the maximum temperature stations of the study 
#area
sta_maxt <- sta_all[sta_all$CODIGO %in% v_cln_maxt , ]

#The result is exported in a shapefile
writeVector(sta_maxt, "RESULTS/sta_maxt.shp", overwrite = TRUE)

#The dataframe with the monthly maximum temperature data is ordered according to 
#the stations in the area, this for subsequent interpolation
data_ym_maxt <- data_ym_maxt[ , sta_maxt$CODIGO]

#The x & y plane coordinates of the maximum temperature stations are obtained 
#and saved as a dataframe, in order to perform the interpolation
sta_maxt <- terra::geom(sta_maxt)[ , c("x" , "y")]
sta_maxt <- as.data.frame(sta_maxt)

#A shapefile is loaded with the maximum temperature stations with an attribute 
#of elevation
sta_maxt_z <- vect("RESULTS/sta_maxt_z.shp") %>% terra::project("epsg:9377")

#A dataframe is created where the name of the maximum temperature stations is 
#stored with their respective elevation
maxt_z_df <- data.frame(CODIGO = sta_maxt_z$CODIGO, z = sta_maxt_z$z1)

#Temperature correction factor due to elevation is applied
maxt_z_df <- maxt_z_df %>% mutate(corr_t = z * 0.0065)

#data_y_mt <- data_y_mt %>%
#  mutate(across(all_of(mt_z_df$CODIGO), ~ . + 
#                  mt_z_df$corr_t[match(cur_column(), mt_z_df$CODIGO)]))

#'cols_mtch_maxt' contains the positions of the column in 'data_ym_maxt' that
#correspond to the values in the 'CODIGO' column of 'maxt_z_df'
cols_mtch_maxt <- match(maxt_z_df$CODIGO, colnames(data_ym_maxt))

#'data_ym_maxtz' is a modified version of 'data_ym_maxt' where the values in the
##columns that match 'cols_mtch_maxt' have been incremented by the corresponding
#values of 'corr_t' from 'maxt_zd_df'. In this line of code the temperature
#correction is applied to all the monthly data of the stations due to the
#altitude
data_ym_maxtz <- t(apply(data_ym_maxt[ , cols_mtch_maxt], 1, 
                         function(x) x + 
                           (maxt_z_df$corr_t)))[ , names(data_ym_maxt)] %>% 
  as.data.frame()

###-------------MIN--------
#The shapefile is filtered by the minimum temperature stations of the study 
#area
sta_mint <- sta_all[sta_all$CODIGO %in% v_cln_mint , ]

#The result is exported in a shapefile
writeVector(sta_mint, "RESULTS/sta_mint.shp", overwrite = TRUE)

#The dataframe with the monthly minimum temperature data is ordered according to 
#the stations in the area, this for subsequent interpolation
data_ym_mint <- data_ym_mint[ , sta_mint$CODIGO]

#The x & y plane coordinates of the minimum temperature stations are obtained 
#and saved as a dataframe, in order to perform the interpolation
sta_mint <- terra::geom(sta_mint)[ , c("x" , "y")]
sta_mint <- as.data.frame(sta_mint)

#A shapefile is loaded with the minimum temperature stations with an attribute 
#of elevation
sta_mint_z <- vect("RESULTS/sta_mint_z.shp") %>% terra::project("epsg:9377")

#A dataframe is created where the name of the minimum temperature stations is 
#stored with their respective elevation
mint_z_df <- data.frame(CODIGO = sta_mint_z$CODIGO, z = sta_mint_z$z1)

#Temperature correction factor due to elevation is applied
mint_z_df <- mint_z_df %>% mutate(corr_t = z * 0.0065)

#data_y_mt <- data_y_mt %>%
#  mutate(across(all_of(mt_z_df$CODIGO), ~ . + 
#                  mt_z_df$corr_t[match(cur_column(), mt_z_df$CODIGO)]))

#'cols_mtch_mint' contains the positions of the column in 'data_ym_mint' that
#correspond to the values in the 'CODIGO' column of 'mint_z_df'
cols_mtch_mint <- match(mint_z_df$CODIGO, colnames(data_ym_mint))

#'data_ym_mintz' is a modified version of 'data_ym_mint' where the values in the
##columns that match 'cols_mtch_mint' have been incremented by the corresponding
#values of 'corr_t' from 'mint_zd_df'. In this line of code the temperature
#correction is applied to all the monthly data of the stations due to the
#altitude
data_ym_mintz <- t(apply(data_ym_mint[ , cols_mtch_mint], 1, 
                         function(x) x + 
                           (mint_z_df$corr_t)))[ , names(data_ym_mint)] %>% 
  as.data.frame()
###------------------------

#The study area raster is loaded
dem <- rast("Raster/DEM_LR_120_F.tif") %>% terra::project("epsg:9377")

#Coordinates are assigned to the raster to create a grid and thus be able
#to interpolate
grd <- crds(dem, df = TRUE, na.rm = TRUE) 

#The temperature correction factor due to the elevation within the grid is also
#applied to interpolate
grd <- cbind(grd, values(dem, na.rm = T) * 0.0065) 
# grid2 <- cbind(grid, values(dem, na.rm = T)) otra alternativa


#-----PRECIPITATION INTERPOLATION-----
#Path where the results are to be saved
path_ppt <- 'C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/PPT/'

#Assigned name for precipitation interpolation results
name_ppt <- "PPT"

#Extension required to save interpolation results
exten <- ".tif"

#A vector is created with the range of years to be analyzed 
years <- (2000:2020)

#A vector is created with the range of months to be analyzed 
months <- (1:12)

#A for loop is used to perform precipitation interpolation
year_month <- c()
for (i in years) {
  for (j in months) {
    year_month <- c(year_month, paste0(i, "-", sprintf("%02d", j)))
  }
}

#A for loop is used to perform precipitation interpolation
for (i in years) {
  j <- as.character(i)
  #The as.vector function is used to extract the data and it is nested with
  #the as.numeric function to only extract the numeric values
  data_it_ppt <- as.vector(as.numeric(data_y_ppt[j , ]))
  
  #NA values are excluded
  nnv_ppt <- which(!is.na(data_it_ppt))
  
  #av <- colnames(data_y_ppt)[qv]
  
  #NA values of precipitation stations are also excluded
  sta_nnv_ppt <- sta_ppt[nnv_ppt , ]
  
  #The IDW interpolation method is applied using the idw function, from
  #the phylin library. In this function, the values to be interpolated, 
  #the coordinates of these values, the coordinate grid where you want to
  #interpolate and the exponent p are used as input data. In this case,
  #the exponent 1 is used, which is recommended to avoid smoothing the
  #interpolaton data. Additionally, only the first two columns f the grid that
  #correspond to the x and y coordinates are selected, since the z coordinate
  #is not needed in this case
  itrpl_ppt <- idw(data_it_ppt[nnv_ppt], sta_nnv_ppt, grd[ , 1:2], p = 1)
  
  #grid <- data.frame(grid, valores = it_data_mt[ , 1]) CHANGED
  
  #The interpolated values are assigned to the grid
  raster_grid <- data.frame(grd, valores = itrpl_ppt[ , 1])
  
  #Coordinates are assigned to the grid. It is specified that 'x' and
  #'y' are the coordinates
  coordinates(raster_grid) <- ~x+y 
  
  #It is indicated that it is a network of points
  gridded(raster_grid) <- T
  
  #Raster is created
  raster_grid <- raster(raster_grid, "valores")
  
  #The coordinate system in which you are working is specified
  projection(raster_grid) <- crs("+init=epsg:9377")
  
  #Numbering for each interpolation result
  #numeration <- as.character(j)
  
  #Export interpolation result as raster file
  writeRaster(raster_grid, paste0(path_ppt, j, "-", name_ppt, exten), 
              overwrite = T)
}

#-----------------

#-----PRECIPITATION INTERPOLATION PER MONTH OF EACH YEAR-----

#Path where the results are to be saved
path_ym_ppt <- 'C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/S_PPT/'

for (j in year_month) {
  #The as.vector function is used to extract the data and it is nested with
  #the as.numeric function to only extract the numeric values
  data_it_ym_ppt <- as.vector(as.numeric(data_ym_ppt[j , ]))
  
  #NA values are excluded
  nnv_ym_ppt <- which(!is.na(data_it_ym_ppt))
  
  #av <- colnames(data_y_ppt)[qv]
  
  #NA values of precipitation stations are also excluded
  sta_nnv_ym_ppt <- sta_ppt[nnv_ym_ppt , ]
  
  #The IDW interpolation method is applied using the idw function, from
  #the phylin library. In this funcion, the values to be interpolated, 
  #the coordinates of these values, the coordinate grid where you want to
  #interpolate and the exponent p are used as input data. In this case,
  #the exponent 1 is used, which is recommended to avoid smoothing the
  #interpolaton data. Additionally, only the first two columns f the grid that
  #correspond to the x and y coordinates are selected, since the z coordinate
  #is not needed in this case
  itrpl_ym_ppt <- idw(data_it_ym_ppt[nnv_ym_ppt], sta_nnv_ym_ppt, 
                      grd[ , 1:2], p = 1)
  
  #grid <- data.frame(grid, valores = it_data_mt[ , 1]) CHANGED
  
  #The interpolated values are assigned to the grid
  raster_grid <- data.frame(grd, valores = itrpl_ym_ppt[ , 1])
  
  #Coordinates are assigned to the grid. It is specified that 'x' and
  #'y' are the coordinates
  coordinates(raster_grid) <- ~x+y 
  
  #It is indicated that it is a network of points
  gridded(raster_grid) <- T
  
  #Raster is created
  raster_grid <- raster(raster_grid, "valores")
  
  #The coordinate system in which you are working is specified
  projection(raster_grid) <- crs("+init=epsg:9377")
  
  #Numbering for each interpolation result
  #numeration <- as.character(j)
  
  #Export interpolation result as raster file
  writeRaster(raster_grid, paste0(path_ym_ppt, j, "-", name_ppt, exten), 
              overwrite = T)
}
#-----MEAN TEMPERATURE INTERPOLATION-----
#Path where the results are to be saved
path_mt <- 'C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/MTEMP/'

#Assigned name for mean temperature interpolation results
name_mt <- "MT"

#A for loop is used to perform mean temperature interpolation
for (i in years) {
  j <- as.character(i)
  #The as.vector function is used to extract the data and it is nested with
  #the as.numeric function to only extract the numeric values
  data_it_mt <- as.vector(as.numeric(data_y_mtz[j , ]))
  
  #NA values are excluded
  nnv_mt <- which(!is.na(data_it_mt))
  
  #NA values of mean temperature stations are also excluded
  sta_nnv_mt <- sta_mt[nnv_mt , ]
  
  #The IDW interpolation method is applied using the idw function, from
  #the phylin library. In this function, the values to be interpolated, 
  #the coordinates of these values, the coordinate grid where you want to
  #interpolate and the exponent p are used as input data. In this case,
  #the exponent 1 is used, which is recommended to avoid smoothing the
  #interpolaton data. To interpolate, only the x and y coordinates of the grid
  #are selected. In addition, the third column of the grid that corresponds to
  #the elevation is substrated, to obtain the interpolation with the real
  #temperature data due to the correction factor for the elevation
  itrpl_mt <- idw(data_it_mt[nnv_mt], sta_nnv_mt, grd[ , 1:2], p = 1) - 
    grd[ , 3]
  
  #The interpolated values are assigned to the grid
  raster_grid <- data.frame(grd, valores = itrpl_mt[ , 1])
  
  #Coordinates are assigned to the grid. It is specified that 'x' and
  #'y' are the coordinates
  coordinates(raster_grid) <- ~x+y 
  
  #It is indicated that it is a network of points
  gridded(raster_grid) <- T
  
  #grid <- raster(grid, "valores") CHANGED
  
  #Raster is created
  raster_grid <- raster(raster_grid, "valores")
  
  #The coordinate system in which you are working is specified
  projection(raster_grid) <- crs("+init=epsg:9377")
  
  #Numbering for each interpolation result
  #numeration <- as.character(i)
  
  #Export interpolation result as raster file
  writeRaster(raster_grid, paste0(path_mt, j, "-", name_mt, exten), 
              overwrite = T)
}


#-----MEAN TEMPERATURE INTERPOLATION PER MONTH OF EACH YEAR-----

#Path where the results are to be saved
path_ym_mt <- 'C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/S_MTEMP/'

#A for loop is used to perform mean temperature interpolation
for (j in year_month) {
  #j <- as.character(i)
  #The as.vector function is used to extract the data and it is nested with
  #the as.numeric function to only extract the numeric values
  data_it_ym_mt <- as.vector(as.numeric(data_ym_mtz[j , ]))
  
  #NA values are excluded
  nnv_ym_mt <- which(!is.na(data_it_ym_mt))
  
  #NA values of mean temperature stations are also excluded
  sta_nnv_ym_mt <- sta_mt[nnv_ym_mt , ]
  
  #The IDW interpolation method is applied using the idw function, from
  #the phylin library. In this funcion, the values to be interpolated, 
  #the coordinates of these values, the coordinate grid where you want to
  #interpolate and the exponent p are used as input data. In this case,
  #the exponent 1 is used, which is recommended to avoid smoothing the
  #interpolaton data. To interpolate, only the x and y coordinates of the grid
  #are selected. In addition, the third column of the grid that corresponds to
  #the elevation is substrated, to obtain the interpolation with the real
  #temperature data due to the correction factor for the elevation
  itrpl_ym_mt <- idw(data_it_ym_mt[nnv_ym_mt], sta_nnv_ym_mt, grd[ , 1:2], 
                     p = 1) - grd[ , 3]
  
  #The interpolated values are assigned to the grid
  raster_grid <- data.frame(grd, valores = itrpl_ym_mt[ , 1])
  
  #Coordinates are assigned to the grid. It is specified that 'x' and
  #'y' are the coordinates
  coordinates(raster_grid) <- ~x+y 
  
  #It is indicated that it is a network of points
  gridded(raster_grid) <- T
  
  #grid <- raster(grid, "valores") CHANGED
  
  #Raster is created
  raster_grid <- raster(raster_grid, "valores")
  
  #The coordinate system in which you are working is specified
  projection(raster_grid) <- crs("+init=epsg:9377")
  
  #Numbering for each interpolation result
  #numeration <- as.character(i)
  
  #Export interpolation result as raster file
  writeRaster(raster_grid, paste0(path_ym_mt, j, "-", name_mt, exten), 
              overwrite = T)
}

#Path where the results are to be saved
path_maxt <- 'C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/S_MAXT/'

#Assigned name for maximum temperature interpolation results
name_maxt <- "MAXT"

#A for loop is used to perform maximum temperature interpolation
for (j in year_month) {
  #j <- as.character(i)
  #The as.vector function is used to extract the data and it is nested with
  #the as.numeric function to only extract the numeric values
  data_it_maxt <- as.vector(as.numeric(data_ym_maxtz[j , ]))
  
  #NA values are excluded
  nnv_maxt <- which(!is.na(data_it_maxt))
  
  #NA values of mean temperature stations are also excluded
  sta_nnv_maxt <- sta_maxt[nnv_maxt , ]
  
  #The IDW interpolation method is applied using the idw function, from
  #the phylin library. In this function, the values to be interpolated, 
  #the coordinates of these values, the coordinate grid where you want to
  #interpolate and the exponent p are used as input data. In this case,
  #the exponent 1 is used, which is recommended to avoid smoothing the
  #interpolaton data. To interpolate, only the x and y coordinates of the grid
  #are selected. In addition, the third column of the grid that corresponds to
  #the elevation is substrated, to obtain the interpolation with the real
  #temperature data due to the correction factor for the elevation
  itrpl_maxt <- idw(data_it_maxt[nnv_maxt], sta_nnv_maxt, grd[ , 1:2], p = 1) - 
    grd[ , 3]
  
  #The interpolated values are assigned to the grid
  raster_grid <- data.frame(grd, valores = itrpl_maxt[ , 1])
  
  #Coordinates are assigned to the grid. It is specified that 'x' and
  #'y' are the coordinates
  coordinates(raster_grid) <- ~x+y 
  
  #It is indicated that it is a network of points
  gridded(raster_grid) <- T
  
  #grid <- raster(grid, "valores") CHANGED
  
  #Raster is created
  raster_grid <- raster(raster_grid, "valores")
  
  #The coordinate system in which you are working is specified
  projection(raster_grid) <- crs("+init=epsg:9377")
  
  #Numbering for each interpolation result
  #numeration <- as.character(i)
  
  #Export interpolation result as raster file
  writeRaster(raster_grid, paste0(path_maxt, j, "-", name_maxt, exten), 
              overwrite = T)
}

#Path where the results are to be saved
path_mint <- 'C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/S_MINT/'

#Assigned name for minimum temperature interpolation results
name_mint <- "MINT"

#A for loop is used to perform minimum temperature interpolation
for (j in year_month) {
  #j <- as.character(i)
  #The as.vector function is used to extract the data and it is nested with
  #the as.numeric function to only extract the numeric values
  data_it_mint <- as.vector(as.numeric(data_ym_mintz[j , ]))
  
  #NA values are excluded
  nnv_mint <- which(!is.na(data_it_mint))
  
  #NA values of mean temperature stations are also excluded
  sta_nnv_mint <- sta_mint[nnv_mint , ]
  
  #The IDW interpolation method is applied using the idw function, from
  #the phylin library. In this function, the values to be interpolated, 
  #the coordinates of these values, the coordinate grid where you want to
  #interpolate and the exponent p are used as input data. In this case,
  #the exponent 1 is used, which is recommended to avoid smoothing the
  #interpolaton data. To interpolate, only the x and y coordinates of the grid
  #are selected. In addition, the third column of the grid that corresponds to
  #the elevation is substrated, to obtain the interpolation with the real
  #temperature data due to the correction factor for the elevation
  itrpl_mint <- idw(data_it_mint[nnv_mint], sta_nnv_mint, grd[ , 1:2], p = 1) - 
    grd[ , 3]
  
  #The interpolated values are assigned to the grid
  raster_grid <- data.frame(grd, valores = itrpl_mint[ , 1])
  
  #Coordinates are assigned to the grid. It is specified that 'x' and
  #'y' are the coordinates
  coordinates(raster_grid) <- ~x+y 
  
  #It is indicated that it is a network of points
  gridded(raster_grid) <- T
  
  #grid <- raster(grid, "valores") CHANGED
  
  #Raster is created
  raster_grid <- raster(raster_grid, "valores")
  
  #The coordinate system in which you are working is specified
  projection(raster_grid) <- crs("+init=epsg:9377")
  
  #Numbering for each interpolation result
  #numeration <- as.character(i)
  
  #Export interpolation result as raster file
  writeRaster(raster_grid, paste0(path_mint, j, "-", name_mint, exten), 
              overwrite = T)
}
#BIEN HASTA ACÁ

#Obtaining the raster of latitudes

#An interpoalted temperature raster (it can be mean, minimum, maximum
#temperature) is loaded
mt_lat <- rast("RESULTS/S_MTEMP/2000-01-MT.tif") %>% 
  terra::project("epsg:9377")

#The study area raster is loaded
dem_l <- rast("Raster/DEM_LR_120_F.tif") %>% terra::project("epsg:9377")

#The coordinate grid of the study area is created
grd_l <- crds(dem_l, df = TRUE, na.rm = TRUE) 

#The coordinate grid of the study area is assigned to a new variable
lat <- grd_l

latp <- terra::project(as.matrix(grd_l), "epsg:9377", "epsg:4686")

#The coordinate grid for the temperature raster is created
grd_mt <- crds(mt_lat, df = T)

#The values of the coordinate grid for the temperature is projected
lat_mt <- terra::project(as.matrix(grd_mt), "epsg:9377", "epsg:4686")

#The results of the projection are converted to a dataframe, and column names
#are assigned
df_lat_mt <- data.frame(lat_mt) %>% setNames(c("x","y"))

#A new column called "z" is created, and the "y" column from the projected 
#dataframe is assigned to it. This column corresponds to the latitude value
lat$z <- df_lat_mt$y

#The latitude values are converted to raster format
lat <- rast(lat, type = "xyz", crs = "epsg:9377")

#Path where the results are to be saved
path_lat <- 'C:/Users/David Gomez/Desktop/TESIS/DATOS/Raster/'

#Export latitude result as raster file
writeRaster(lat, paste0(path_lat, "lat.tif"), overwrite = T)

#Calculation of evapotranspiration

#Empty lists are created to perform the calculation
pat_etp <- list()
tssm <- list()
tmx <- list()
tmn <- list()
tssm_s <- list()
tmx_s <- list()
tmn_s <- list()
etpm <- list()

#Path where the results are to be saved
path_etpm <- 'C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/ETPm/'

#Assigned name for monthly evapotranspiration results
name_etpm <- 'ETP'

#A for loop is created to obtain the monthly evapotranspiration
for (x in year_month) {
  #Filename pattern list is created for uploading 
  pat_etp[[x]] <- glob2rx(paste0(x, "*.tif$"))
  
  #Mean temperature data is listed
  tssm[[x]] <- list.files("RESULTS/S_MTEMP/", 
                           pattern = pat_etp[[x]], full.names = TRUE)
  
  #Maximum temperature data is listed
  tmx[[x]] <- list.files("RESULTS/S_MAXT/", 
                            pattern = pat_etp[[x]], full.names = TRUE)
  
  #Minimum temperature data is listed
  tmn[[x]] <- list.files("RESULTS/S_MINT/", 
                            pattern = pat_etp[[x]], full.names = TRUE)
  
  #The stack function is used to store each monthly data of the temperature
  #raster
  tssm_s[[x]] <- stack(tssm[[x]])
  
  tmx_s[[x]] <- stack(tmx[[x]])
  
  tmn_s[[x]] <- stack(tmn[[x]])
  
  #The 15th day of each month is obtained 
  dv <- as.Date(paste0(x, "-15"))
  
  #The corresponding day of the year is obtained
  j <- yday(dv)
  
  #The number of days in the month is obtained
  ym <- days_in_month(ym(x))
  
  #Solar constant
  Gr <- 0.082 #MJ*m^{-2}*min^{-1}
  
  #dr
  dr <- 1 + 0.033 * cos(2 * pi / 365 * j)
  
  #delta
  delta <- 23.45 * pi / 180 * sin((360 / 365 * (284 + j)) * pi / 180)
  
  #Omega s
  ws <- acos(-tan(lat * pi / 180) * tan(delta))
  
  #Extraterrestrial solar radiation
  ra <- 24 * 60 / pi * Gr * dr * (ws * sin(lat * pi / 180) * sin(delta) + 
                                    cos(lat * pi / 180) * cos(delta) * sin(ws))
  
  #The extraterrestrial solar radiation is converted from a SpatRaster file to
  #a RasterLayer
  ra <- raster(ra)
  
  #ETP month
  etpm[[x]] <- (0.0023 * (ra / 2.45) * (tssm_s[[x]] + 17.78) * 
                  (tmx_s[[x]] - tmn_s[[x]]) ^ 0.5) * ym
  
  #Export evapotranspiration result as raster file
  writeRaster(etpm[[x]], paste0(path_etpm, x, "-", name_etpm, ".tif"), 
              overwrite = T)
  
}

#-----RECHARGE CALCULATION-----
#Empty lists are created to work with the data

pat <- list() #List of patterns
grids <- list() #List of information by year
ppt_s <- list() #Precipitation stack
rec <- list() #First aquifer recharge methodology
rec_2 <- list() #Second aquifer recharge methodology
rec_3 <- list() #Third aquifer recharge methodology
pat_temp <- list() #List of temperature patterns
grids_temp <- list() #List of temperature information by year
temp_s <- list() #Temperature stack
temp_m <- list() #Mean temperature
l_temp <- list() #Parameter L
ppt_in <- list() #Precipitation in inches

path_rec <- c('C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/REC_A/REC_1/',
              'C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/REC_A/REC_2/',
              'C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/REC_A/REC_3/')

name_rec <- c("REC_1", "REC_2", "REC_3")

system.time(
  for (i in years) {
    j <- as.character(i)
    #Filename pattern list is created for uploading 
    pat[[j]] <- glob2rx(paste0(i, "*.tif$"))

    #All precipitation files are listed
    grids[[j]] <- list.files("RESULTS/PPT/", 
                             pattern = pat[[j]], full.names = TRUE)
    
    #The stack function is used to store each annual precipitation raster
    ppt_s[[j]] <- stack(grids[[j]]) 
    #A RasterStack is a collection of RasterLayer objects with the same 
    #spatial extent and resolution
    
    #The units of eaach precipitation raster are converted from milimeters
    #to inches
    ppt_in[[j]] <- ppt_s[[j]] / 25.4
    
    #-----------------------RECHARGE - FIRST METHODOLOGY------------------------
    
    #SGC - CHEETURVEDI (SINHA & SHARMA, 1988)
    
    #After calculating the recharge, the result is converted to milimeters
    #for better analysis
    recb <- ( 1.35 * (ppt_in[[j]] - 14)^(0.5) ) * 25.4
    
    #recb[[1L]] is used to convert the file type from RasterBrick to
    #RasterLayer, to avoid any inconvenience after
    rec[[j]] <- recb[[1L]]
    
    writeRaster(rec[[j]], paste0(path_rec[1], j, "-", name_rec[1], exten),
                overwrite = TRUE)
    
    #----------------------RECHARGE - SECOND METHODOLOGY------------------------
    
    #SGC - SEHGAL (1973)
    
    #After calculating the recharge, the result is converted to milimeters
    #fro better analysis
    rec_2b <- ( 2.5 * (ppt_in[[j]] - 16)^(0.5) ) * 25.4
    
    #rec_2b[[1L]] is used to convert the file type from RasterBrick to
    #RasterLayer, to avoid any inconvenience after
    rec_2[[j]] <- rec_2b[[1L]]
    
    writeRaster(rec_2[[j]], paste0(path_rec[2], j, "-", name_rec[2], exten),
                overwrite = TRUE)
    
    #-----------------------RECHARGE - THIRD METHODOLOGY------------------------
    
    #SGC - TURC (1954)
    
    #Filename pattern list is created for uploading 
    pat_temp[[j]] <- glob2rx(paste0(i, "*.tif$"))
    
    #All temperature files are listed
    grids_temp[[j]] <- list.files("RESULTS/MTEMP/", 
                                  pattern = pat_temp[[j]], full.names = TRUE)
    
    #The stack function is used to store each mean annual temperature raster
    temp_s[[j]] <- stack(grids_temp[[j]])
    
    #Parameter L is calculated
    l_temp[[j]] <- 300 + (25 * temp_s[[j]]) + (0.05 * (temp_s[[j]])^(2))
    
    #Recharge calculation
    rec_3b <- ppt_s[[j]] * (1 - (0.9 + ((ppt_s[[j]]^(2))) / 
                                     ((l_temp[[j]]^(2))))^(-0.5)) 
    
    #recb[[1L]] is used to convert the file type from RasterBrick to
    #RasterLayer, to avoid any inconvenience after
    rec_3[[j]] <- rec_3b[[1L]]
    
    writeRaster(rec_3[[j]], paste0(path_rec[3], j, "-", name_rec[3], exten),
                overwrite = TRUE)
  }
)

#Creation of the stacks for each methodology to calculate the average
stk <- stack()
stk_2 <- stack()
stk_3 <- stack()

#Creation of the precipitation stack to calculate the average precipitation
stk_ppt <- stack()

#Creation of the for loop to add the recharge results to each stack, by means
#of the addLayer function
#addLayer function add a layer to a Raster* object
for (i in 1:length(years)) {
  stk_ppt <- addLayer(stk_ppt, ppt_s[[i]]) #Precipitation stack
  stk <- addLayer(stk, rec[[i]])
  stk_2 <- addLayer(stk_2, rec_2[[i]])
  stk_3 <- addLayer(stk_3, rec_3[[i]])
}


#The calc function is used to calculate the average precipitation and recharge.
#In addition, null values are removed by na.rm = TRUE. 
ppt_t <- calc(stk_ppt, fun = mean, na.rm = TRUE)

rec_t <- calc(stk, fun = mean, na.rm =TRUE)

rec_t_2 <- calc(stk_2, fun = mean, na.rm =TRUE)

rec_t_3 <- calc(stk_3, fun = mean, na.rm = TRUE)

#The percentage of precipitation that is converted to recharge is calculated
#to discard methodologies. According to empirical results, the maximum 
#percentage of precipitation that can be converted to recharge is 30%. 
#Therefore, methodologies with a higher percentage than this value will be
#discarded.
#The values function is used to extract the pixel values from the raster to
#calculate the average.
r_per <- 100 - mean(values( ( (ppt_t - rec_t) / ppt_t) * 100), 
                    na.rm = TRUE) 

r_per_2 <- 100 - mean(values( ( (ppt_t - rec_t_2) / ppt_t) * 100), 
                      na.rm = TRUE)

r_per_3 <- 100 - mean(values( ( (ppt_t - (rec_t_3)) / ppt_t) * 100),
                      na.rm = TRUE)

#r_df <- as.data.frame(rec_t, xy=TRUE)

writeRaster(rec_t, filename = 
              "C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/REC/REC_T_1",
            format = "GTiff", overwrite = TRUE)

writeRaster(rec_t_2, filename = 
              "C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/REC/REC_T_2",
            format = "GTiff", overwrite = TRUE)

writeRaster(rec_t_3, filename = 
              "C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/REC/REC_T_3",
            format = "GTiff", overwrite = TRUE)

writeRaster(rec_3[["2020"]], filename = 
              "C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/p2020",
            format = "GTiff", overwrite = TRUE)

#-----SCHOSINSKY----
#The agrology shapefile is loaded to work with soil texture
agro <- vect("Shapes/AGROLOGIA_F.shp") %>% terra::project("epsg:9377")

#The texture is extracted from the attribute table of the shapefile by
#applying special text patterns using the stringr library
agro$texture <- trimws(str_extract(agro$CARACT,
                                   "(?<=textura?(?:[^a-zA-Z]|s)).*?(?=\\;)"))

agro$texture2 <- trimws(str_extract(agro$CARACT, 
                                    "(?<=textura?(?:[^a-zA-Z]|s)).*?(?=\\.)"))

agro$texture1 <- trimws(str_extract(agro$texture, "^[^,y]+"))

agro$texture1_1 <- trimws(str_extract(agro$texture1, ".*?(?=\\s+a\\s|$)"))

agro$texture2_2 <- trimws(str_extract(agro$texture2, ".*?(?=\\s+a\\s|$)"))

agro$texture3 <- trimws(str_extract(agro$texture2_2, "^[^,y]+"))

#The NA values in text format are converted to simple NAs
agro$texture1_1 <- gsub("NA", NA, agro$texture1_1)

agro$texture3 <- gsub("NA", NA, agro$texture3)

#The results obtained from the different textures are grouped into a single
#column
agro$texturef <- coalesce(agro$texture1_1, agro$texture3)

#A dataframe is created to store the values of fc (basic soil infiltration),
#CC (field capacity), and PM (permanent wilting point) based on soil texture
val <- data.frame(
  text = c("arcillo arenosa", "arcillosa", "arcillosa con fragmentos de roca", 
           "arcillosa con gravilla", "arenosa franca", "franca", 
           "franca arenosa", "franco", "franco arcillo arenosa", 
           "franco arcillo arenosa gravillosa", "franco arcillosa", 
           "franco arenosa", "franco arenosa gravillosa", 
           "franco gravillosa", "franco limosa", "variadas"),
  fc = c(96, 60, 2400, 2400, 600, 312, 600, 312, 192, 192, 192, 600, 600, 
         600, 312, NA),
  CC = c(302.3, 437.5, 437.5, 437.5, 63, 123.2, 63, 123.2, 182.3, 182.3, 182.3,
         63, 63, 63, 63, NA),
  PM = c(146.3, 212.5, 212.5, 212.5, 27, 56, 27, 56, 87.8, 87.8, 87.8, 27, 27,
         27, 27, NA)
)

#The match function is used to associate the textures in the shapefile with
#their respective fc values
matched_values <- val[match(agro$texturef, val$text), "fc"]

#A field is created in the shapefile with the assigned values
agro$matched_value <- matched_values

#The assigned values are converted to integer type and assigned to a new field
#called fc within the shapefile
agro$fc <- as.integer(agro$matched_value)

#The NA values in the fc field are replaced with 0 for rasterization 
agro$fc <- replace_na(agro$fc, 0)

#A raster is created from the shapefile to have the same extent. The resolution
#is also defined, which corresponds to that of the shapefile 
ras_agro <- rast(agro, resolution = 120)

#The fc field is reasterized
ras_fc <- rasterize(agro, ras_agro, field = "fc")

#The raster with the fc values is exported
writeRaster(ras_fc, "RESULTS/RAS_SCH/fc_ras.tif", overwrite = TRUE) 

writeVector(agro, "RESULTS/SHP_SCH/AGRO.shp", overwrite = TRUE)

#Value raster fc is converted from SpatRaster to RasterLayer
ras_kfc <- raster(ras_fc)

#A function is created to calculate the Kfc coefficient (coefficient due to
#soil texture)
fun_kfc <- function(x) {
  ifelse(x < 16, 0.0148 * x / 16,
         ifelse(x > 1568, 1,
                0.267 * log(x) - 0.000154 * x - 0.723))
}

#The function is applied to each cell of the raster
r_new <- calc(ras_kfc, fun_kfc)

#The raster with the Kfc coefficients is exported
writeRaster(r_new, "RESULTS/RAS_SCH/Kfc_ras.tif", overwrite = TRUE)

#The DEM is loaded
ras_dem <- rast("Raster/O_DEM_120.tif") %>% terra::project("epsg:9377")

#The SpatRaster is converted to RasterLayer
ras_dem <- raster(ras_dem)

#Compute the slope of the terrain and store the result in a new raster object
slope <- terrain(ras_dem, v = "slope", unit = "radians")

#Convert the slope values from radians to degrees
#slope_degrees <- slope * (180/pi)

#Convert the slope values from radians to percent
slope_percent <- tan(slope) * 100

#Define the reclassification values for the slope_percent raster
classes <- c(0.02, 0.06, 0.3, 0.3, 0.4, 0.2, 1, 2, 0.15, 2, 7, 0.1, 7, Inf, 
             0.06)

#Convert the classes vector to a matrix with 3 columns
mclases <- matrix(classes, ncol = 3, byrow = TRUE)

#Reclassify the slope_percent raster using the specified ranges and values
#The value of the coefficient Kp is obtained
slope_cla <- reclassify(slope_percent, mclases)

#The bounded raster is loaded
ras_or <- rast("Raster/DEM_LR_120_F.tif")

#The SpatRaster is converted to RasterLayer
ras_or <- raster(ras_or)

#The slope_cla raster is resampled to match the resolution and extent of ras_or
slope_resampled <- resample(slope_cla, ras_or, method = "ngb")

#The slope raster is obtained with the delimitation of the basin carried out
slope_mask <- mask(slope_resampled, ras_or)

#Write the reclassified slope raster to a new file
writeRaster(slope_mask, "RESULTS/RAS_SCH/Kp_ras.tif", overwrite = TRUE) #se obtiene el Kp

#A dataframe is created for the vegetation cover type with its respective value
cov_val <- data.frame(
  land_use = c("Tejido urbano continuo", "Tejido urbano discontinuo", 
               "Zonas industriales o comerciales", "Zonas industriales",
               "Aeropuertos", "Obras hidráulicas", 
               "Zonas de extracción minera", 
               "Explotación de materiales de construcción", 
               "Zonas verdes urbanas", "Otras zonas verdes urbanas", 
               "Rondas de cuerpos de agua de zonas urbanas",
               "Instalaciones recreativas", "Otros cultivos transitorios", 
               "Otros cultivos permanentes herbáceos", "Café",
               "Cultivos permanentes arbóreos", "Palma de aceite", 
               "Pastos limpios", "Pastos arbolados", "Pastos enmalezados",
               "Mosaico de cultivos", "Mosaico de pastos y cultivos", 
               "Mosaico de cultivos, pastos y espacios naturales",
               "Mosaico de pastos con espacios naturales", 
               "Mosaico de cultivos con espacios naturales", 
               "Bosque denso alto de tierra firme", 
               "Bosque denso bajo de tierra firme", 
               "Bosque fragmentado con pastos y cultivos", 
               "Bosque fragmentado con vegetación secundaria",
               "Bosque de galería y ripario", "Plantación forestal", 
               "Plantación de coníferas", 
               "Herbazal denso de tierra firme no arbolado",
               "Herbazal denso de tierra firme arbolado", 
               "Herbazal denso de tierra firme con arbustos", 
               "Arbustal denso", "Arbustal abierto",
               "Vegetación secundaria alta", "Vegetación secundaria baja", 
               "Afloramientos rocosos", "Tierras desnudas y degradadas",
               "Zonas quemadas", "Zonas pantanosas", "Turberas", "Ríos", 
               "Embalses"),
  value = c(NA, NA, NA, NA, NA, NA, NA, NA, 0.09, 0.09, NA, NA, 0.10, 0.10, 
            0.10, 0.10, 0.10, 0.18, 0.18, 0.18, 0.10, 0.14, 0.14, 0.14, 0.14, 
            0.20, 0.20, 0.20, 0.20, 0.20, 0.10, 0.10, 0.09, 0.09, 0.09, 0.09, 
            0.09, 0.20, 0.20, NA, 0.09, NA, NA, NA, NA, NA)
)

#The shapefile with the vegetation cover is loaded
cover <- vect("Shapes/COBERTURAS.shp") %>% 
  terra::project("epsg:9377")

#A specific text pattern is used to extract the vegetation cover from the 
#attribute table of the shapefile
cover$leyenda_sd <- trimws(str_replace_all(cover$leyenda, 
                                           "[[:digit:]\\.]+", ""))

#The match function is used to assign the vegetation cover values from the
#dataframe to the shapefile
match_cov <- cov_val[match(cover$leyenda_sd, cov_val$land_use), "value"]

#A new field is created with the attribute table of the vegetation cover
#shapefile with the assigned values
cover$Kv <- match_cov

#The NA values in the Kv field are replaced with 0 for rasterization 
cover$Kv <- replace_na(cover$Kv, 0)

#A raster is created from the shapefile to have the same extent. The resolution
#is also defined, which corresponds to that of the shapefile 
ras_cov <- rast(cover, resolution = 120)

#The Kv field is reasterized
ras_Kv <- rasterize(cover, ras_cov, field = "Kv")

ras_Kv <- raster(ras_Kv)

#The raster with the Kv values is exported
writeRaster(ras_Kv, "RESULTS/RAS_SCH/Kv_ras.tif", overwrite = TRUE)

#The final result of the coverages is exported
writeVector(cover, "RESULTS/SHP_SCH/COBER.shp", overwrite = TRUE) #obtiene el Kv 

#The vegetation cover is loaded
cover_m <- vect("RESULTS/SHP_SCH/COBER.shp") %>% terra::project("epsg:9377")

#Create a logical index of features in the vector layer with a "Bosque" value
bosque_index <- grepl("Bosque", cover_m$leyenda_sd)

#Add a new column to the attribute table of the shapefile
#The new column will have a value of 0.2 for features that have a "Bosque"
#value in the "leyenda_sd" column and 0.12 for all other features
cover_m$Cf0 <- ifelse(bosque_index, 0.2, 0.12) #obtener valor de Cfo

#A raster is created from the shapefile to have the same extent. The resolution
#is also defined, which corresponds to that of the shapefile 
ras_cov_m <- rast(cover_m, resolution = 120)

#The Cf0 field is reasterized
ras_Cf0 <- rasterize(cover_m, ras_cov_m, field = "Cf0")

#The raster with the Cf0 values is exported
writeRaster(ras_Cf0, "RESULTS/RAS_SCH/Cf0_ras.tif", overwrite = TRUE)

#The match function is used to associate the textures in the shapefile with
#their respective CC values
matched_values_CC <- val[match(agro$texturef, val$text), "CC"]

#A field is created in the shapefile with the assigned values
agro$matched_value_cc <- matched_values_CC

#The assigned values are converted to integer type and assigned to a new field
#called cc within the shapefile
agro$CC <- as.integer(agro$matched_value_cc)

#The NA values in the CC field are replaced with 0 for rasterization 
agro$CC <- replace_na(agro$CC, 0)

#The CC field is reasterized
ras_CC <- rasterize(agro, ras_agro, field = "CC")

#The raster with the CC values is exported
writeRaster(ras_CC, "RESULTS/RAS_SCH/CC_ras.tif", overwrite = TRUE)

#The match function is used to associate the textures in the shapefile with
#their respective PM values
matched_values_PP <- val[match(agro$texturef, val$text), "PM"]

#A field is created in the shapefile with the assigned values
agro$matched_value_PP <- matched_values_PP

#The assigned values are converted to integer type and assigned to a new field
#called PM within the shapefile
agro$PM <- as.integer(agro$matched_value_PP)

#The NA values in the PM field are replaced with 0 for rasterization 
agro$PM <- replace_na(agro$PM, 0)

#The PM field is reasterized
ras_PM <- rasterize(agro, ras_agro, field = "PM")

#The raster with the PM values is exported
writeRaster(ras_PM, "RESULTS/RAS_SCH/PM_ras.tif", overwrite = TRUE)

#The final result of the agrology shape is exported
writeVector(agro, "RESULTS/SHP_SCH/AGRO.shp", overwrite = TRUE)

#Calculation of the infiltration coefficient. If the coefficient s greater than
#1 this value is corrected and the value of 1 is taken
Ci <- clamp(ras_Kv + r_new + slope_mask, 0, 1)

#The raster with the infiltration coefficient is exported
writeRaster(Ci, "RESULTS/RAS_SCH/Ci.tif", overwrite = T)

#Calculation of the monthly retention of precipitation in the foliaage

#The Cf0 raster file is loaded
Cf0 <- rast("RESULTS/RAS_SCH/Cf0_ras.tif") %>% terra::project("epsg:9377")

#Convert the file type from SpatRaster to RasterLayer in order to perform
#map algebra
Cf0 <- raster(Cf0)


#----BORRAR----
#Retention threshold
# ret_u <- Cf0
# 
# ret_u[which(!is.na(ret_u[]))] <- 5
#   
# writeRaster(ret_u, "C:/Users/David Gomez/Desktop/ret_u.tif", overwrite = T)

pat_pptm <- list()
pptm <- list()
pptm_s <- list()
ret_l <- list()

ret_fun <- function(x) {
  ifelse(x <= ret_u, x,
         ifelse(x < ret_u, ret_u, x))
}

for (i in year_month) {
  #Filename pattern list is created for uploading 
  pat_pptm[[i]] <- glob2rx(paste0(i, "*.tif$"))
  
  #Monthly precipitation data is listed
  pptm[[i]] <- list.files("RESULTS/S_PPT/", 
                          pattern = pat_pptm[[i]], full.names = TRUE)
  
  #The stack function is used to store each monthly data of the precipitation
  #raster
  pptm_s[[i]] <- stack(pptm[[i]])
  
  p1 <- raster(pptm_s[[i]], 1)
  
  ret_l[[i]] <- Cf0 
  
  ret_u <- ret_l[[i]]
  
  ret_u[ret_u[] > 0] <- 5
  
  ret_u[p1[] <= ret_u] <- p1[p1[] <= ret_u]
  
  ret_cp <- Cf0 * p1
  
  r_n <- calc(stack(ret_u, ret_cp),
              fun = function (x) {
                ifelse(x[1] == 5 & x[2] > x[1], x[2], x[1])
              })
  
  
  ret_l[[i]] <- r_n
    
  writeRaster(ret_l[[i]], "RESULTS/RAS_SCH/RET/ret_p.tif", overwrite = T)
}

#-----

#-optimized code

#Create regular expressions patterns for .tif file names corresponding to each
#value in year_month
pat_pptm <- sapply(year_month, function(i) glob2rx(paste0(i, "*.tif$")))

#Find and list files in the directory that match each pattern
pptm <- lapply(pat_pptm, function(pat) list.files("RESULTS/S_PPT/", 
                                                  pattern = pat,
                                                  full.names = TRUE))

#Create raster stacks for each month
pptm_s <- lapply(pptm, stack)

#Perform operations for each month
ret_l <- lapply(seq_along(pptm_s), function (i) {
  #The layer of the created RasterStack is selected
  p1 <- raster(pptm_s[[i]], 1)
  
  #The retention threshold is created by assigning it the Cf0 raster to have 
  #the same extent
  ret_u <- Cf0
  
  #The retention threshold value is assigned
  ret_u[ret_u[] > 0] <- 5
  
  #Values where precipitation is less than or equal to the retention threshold 
  #are assigned to retention
  ret_u[p1[] <= ret_u] <- p1[p1[] <= ret_u]
  
  #The fraction of precipitation retained is calculated
  ret_cp <- Cf0 * p1
  
  #If the amount of precipitation retained is less than the retention threshold,
  #the retention threshold is assumed. If not, the retention assumes the value
  #of the precipitation retained and the first analysis performed for the
  #precipitation is retained
  r_n <- calc(stack(ret_u, ret_cp),
              fun = function (x) {
                ifelse(x[1] == 5 & x[2] > x[1], x[2], x[1])
              })
  
  #Write raster to file with unique name based on year_month
  writeRaster(r_n, paste0("RESULTS/RAS_SCH/RET/", year_month[i], "-RET", 
                          ".tif"), overwrite = TRUE)
})

#Name the list elements by year_month
names(ret_l) <- year_month


#HSi

#---a ver----
# Load required libraries
library(terra)
library(raster)

# Load the infiltration coefficient raster file and project it
Ci_r <- rast("RESULTS/RAS_SCH/Ci.tif") %>% terra::project("epsg:9377")

#Convert the file type from SpatRaster to RasterLayer
Ci_r <- raster(Ci_r)

# Function to perform operations for each month
process_month <- function(month) {
  #Pattern to load the raster files
  pattern <- glob2rx(paste0(month, "*.tif$"))
  
  # List monthly precipitation data
  precm_files <- list.files("RESULTS/S_PPT/", pattern = pattern, 
                            full.names = TRUE)
  
  #List monthly retention data
  retm_files <- list.files("RESULTS/RAS_SCH/RET/", pattern = pattern, 
                           full.names = TRUE)
  
  # precm_stack <- brick(precm_files)
  # retm_stack <- brick(retm_files)
  #The layer of the created RasterStack is selected
  precm_stack <- raster(stack(precm_files), 1)
  
  #The layer of the created RasterStack is selected
  retm_stack <- raster(stack(retm_files), 1)
  
  #The precipitation that infiltrates is obtained
  pi_r <- Ci_r * (precm_stack - retm_stack)
  
  #The address where the result is to stored is created
  outfile <- file.path("RESULTS", "RAS_SCH", "PI", paste0(month, "-PI.tif"))
  
  #The result is exported as a raster file
  writeRaster(pi_r, outfile, overwrite = TRUE)
}

# Apply the function to each month
system.time(pi_l <- lapply(year_month, process_month))

#Name the list elements by year_month
names(pi_l) <- year_month

#---HSI----
#Create regular expressions patterns for .tif file names corresponding to each
#value in year_month
pat_pin <- sapply(year_month, function(i) glob2rx(paste0(i, "*.tif$")))

#Find and list files in the directory that match each pattern
pinm <- lapply(pat_pin, function(pat) list.files("RESULTS/RAS_SCH/PI/", 
                                                  pattern = pat,
                                                  full.names = TRUE))

#Create raster stacks for each month
pinm_s <- lapply(pinm, stack)

#Perform operations for each month
hsi_l <- lapply(seq_along(pinm_s), function (i) {
  #The layer of the created RasterStack is selected
  hsi <- raster(pinm_s[[i]], 1)
  
  hsi[hsi[] > 0] <- 0

  #Write raster to file with unique name based on year_month
  writeRaster(hsi, paste0("RESULTS/RAS_SCH/HSi/", year_month[i], "-HSi", 
                          ".tif"), overwrite = TRUE)
})

#Name the list elements by year_month
names(hsi_l) <- year_month

# Load the infiltration coefficient raster file and project it
CC_r <- rast("RESULTS/RAS_SCH/CC_ras.tif") %>% terra::project("epsg:9377")

#Convert the file type from SpatRaster to RasterLayer
CC_r <- raster(CC_r)

hsi_j <- rast("RESULTS/RAS_SCH/HSi/2000-01-HSi.tif") %>% 
  terra::project("epsg:9377")

hsi_j <- raster(hsi_j)

hsi_j <- CC_r

writeRaster(hsi_j, paste0("RESULTS/RAS_SCH/HSi/", "2000-01", "-HSi", ".tif"),
            overwrite = TRUE)

pim_files <- list()
etpm_files <- list()
hsi_files <- list()
pattern <- list()
pim_stack <- list()
etpm_stack <- list()

year_month_hsi <- year_month[-1]

for (j in year_month) {
  #j <- as.character(i)
  
  #Pattern to load the raster files
  pattern[[j]] <- glob2rx(paste0(year_month, "*.tif$"))
  
  # List monthly precipitation data
  pim_files[[j]] <- list.files("RESULTS/RAS_SCH/PI/", pattern = pattern[[j]], 
                          full.names = TRUE)
  
  #List monthly retention data
  etpm_files[[j]] <- list.files("RESULTS/ETpm/", pattern = pattern[[j]], 
                           full.names = TRUE)
  
  hsi_files[[j]] <- list.files("RESULTS/RAS_SCH/HSi/", pattern = pattern[[j]],
                               full.names = TRUE)
  
  if (j == "2000-01") {
    hsi <- CC_r
    
    writeRaster(hsi, paste0("RESULTS/RAS_SCH/HSi/", j, "-HSi", ".tif"),
                overwrite = TRUE)
  }
  
  
  # precm_stack <- brick(precm_files)
  # retm_stack <- brick(retm_files)
  #The layer of the created RasterStack is selected
  pim_stack[[j]] <- raster(stack(pim_files[[j]]), 1)
  
  #The layer of the created RasterStack is selected
  etpm_stack[[j]] <- raster(stack(etpm_files[[j]]), 1)
  
  
}

for (j in year_month_hsi) {
  #Pattern to load the raster files
  pattern[[j]] <- glob2rx(paste0(year_month, "*.tif$"))
  
  hsi_files[[j]] <- list.files("RESULTS/RAS_SCH/HSi/", pattern = pattern[[j]],
                               full.names = TRUE)
}



# Function to perform operations for each month
process_month <- function(month) {
  #Pattern to load the raster files
  pattern <- glob2rx(paste0(year_month, "*.tif$"))
  
  # List monthly precipitation data
  pim_files <- list.files("RESULTS/RAS_SCH/PI", pattern = pattern, 
                            full.names = TRUE)
  
  #List monthly retention data
  etpm_files <- list.files("RESULTS/ETpm/", pattern = pattern, 
                           full.names = TRUE)
  
  # precm_stack <- brick(precm_files)
  # retm_stack <- brick(retm_files)
  #The layer of the created RasterStack is selected
  pim_stack <- raster(stack(pim_files), 1)
  
  #The layer of the created RasterStack is selected
  etpm_stack <- raster(stack(etpm_files), 1)
  
  #The precipitation that infiltrates is obtained
  pi_r <- Ci_r * (precm_stack - retm_stack)
  
  #The address where the result is to stored is created
  outfile <- file.path("RESULTS", "RAS_SCH", "PI", paste0(month, "-PI.tif"))
  
  #The result is exported as a raster file
  writeRaster(pi_r, outfile, overwrite = TRUE)
}

# Apply the function to each month
system.time(pi_l <- lapply(year_month, process_month))

#Name the list elements by year_month
names(pi_l) <- year_month

vp <- c("2015-01", "2015-02", "2015-03")

vi <- vector()

for (j in 1:(length(vp) - 1)) {
  vi <- c(vi, vp[j+1])
}
#-----

#---HSi-----

# Load the infiltration coefficient raster file and project it
CC_r <- rast("RESULTS/RAS_SCH/CC_ras.tif") %>% terra::project("epsg:9377")

#Convert the file type from SpatRaster to RasterLayer
CC_r <- raster(CC_r)

#year_month_hsi <- year_month[-1]

pim_files <- list()
etpm_files <- list()
hsi_files <- list()
pattern <- list()
pim_stack <- list()
etpm_stack <- list()
hsi <- list()

for (j in year_month) {
  #j <- as.character(i)
  
  #Pattern to load the raster files
  pattern[[j]] <- glob2rx(paste0(year_month, "*.tif$"))
  
  # List monthly precipitation data
  pim_files[[j]] <- list.files("RESULTS/RAS_SCH/PI", pattern = pattern[[j]], 
                               full.names = TRUE)
  
  #List monthly retention data
  etpm_files[[j]] <- list.files("RESULTS/ETpm/", pattern = pattern[[j]], 
                                full.names = TRUE)
  
  hsi_files[[j]] <- list.files("RESULTS/RAS_SCH/HSi/", pattern = pattern[[j]],
                               full.names = TRUE)
}

#Assign the string "1999-12" to the variable ym_99
ym_99 <- "1999-12"

#Convert the pattern from glob to regular expression. This is used to match 
#files ending with ".tif" that also start with the string "1999-12"
pat_99 <- glob2rx(paste0(ym_99, "*.tif$"))

# List all files in the specified directory "DATA_99/RESULTS/RAS_SCH/PI" 
#that match the pattern defined earlier, returning the full names 
#(i.e., including directory paths).
pim_files_99 <- list.files("DATA_99/RESULTS/RAS_SCH/PI", 
                           pattern = pat_99, 
                           full.names = TRUE)

#Repeat the process for a different directory "DATA_99/RESULTS/ETPm".
etpm_files_99 <- list.files("DATA_99/RESULTS/ETPm", 
                             pattern = pat_99, 
                             full.names = TRUE)

#Loop through the "year_month" variable
for (j in 1:length(year_month)) {
  #Check if the current value of "year_month" is "2000-01"
  if (year_month[j] == "2000-01") {
    # Create raster objects from the tif files specified earlier
    pim_raster <- raster(pim_files_99)
    
    etpm_raster <- raster(etpm_files_99)
    
    # Overlay the two rasters, applying a function to each pair of cells.
    # If the cell value of the first raster is greater than the second, 
    #assign it the value from a CC_r object, otherwise assign it zero.
    hsi_raster <- overlay(pim_raster, etpm_raster, fun = function(x,y) {
      ifelse(x > y, values(CC_r), 0)
    })
    
    # Store the raster in a list, with the key being the current 
    #"year_month" value.
    hsi[[year_month[j]]] <- hsi_raster
    
    # Write the raster to a .tif file, with a name based on the "year_month" 
    #value. If the file already exists, it is overwritten.
    writeRaster(hsi[[year_month[j]]], 
                paste0("RESULTS/RAS_SCH/HSi/", year_month[j], "-HSi", ".tif"),
                overwrite = TRUE)
  }
  else {
    # If the current "year_month" value isn't "2000-01", load the raster for 
    #the current year and month, then print a loading message.
    print(paste("Loading", pim_files[[year_month[j]]]))
    pim_raster <- raster(pim_files[[year_month[j]]])
    
    print(paste("Loading", etpm_files[[year_month[j]]]))
    etpm_raster <- raster(etpm_files[[year_month[j]]])
    
    #Overlay the rasters as before.
    hsi_raster <- overlay(pim_raster, etpm_raster, fun = function(x,y) {
      ifelse(x > y, values(CC_r), 0)
    })
    
    # If not at the end of the "year_month" list, save the new raster for the 
    #next month and write it to a .tif file.
    if (j < length(year_month)) {
      hsi[[year_month[j+1]]] <- hsi_raster
      
      writeRaster(hsi[[year_month[j+1]]], 
                  paste0("RESULTS/RAS_SCH/HSi/", year_month[j+1], "-HSi", ".tif"),
                  overwrite = TRUE)
    }
  }
}

#-----HD-----
# Load the wilting point raster file and project it
PM_r <- rast("RESULTS/RAS_SCH/PM_ras.tif") %>% terra::project("epsg:9377")

#Convert the file type from SpatRaster to RasterLayer
PM_r <- raster(PM_r)

# hsi_list <- list()
# pi_list <- list()
# hsi_s <- list()
# pi_s <- list()
# hd_list <- list()
# 
# for (i in year_month) {
#   hsi_list[[j]] <- list.files("RESULTS/RAS_SCH/HSi",
#                               pattern = glob2rx(paste0(j, "*.tif$")),
#                               full.names = TRUE)
#   
#   pi_list[[j]] <- list.files("RESULTS/RAS_SCH/PI",
#                              pattern = glob2rx(paste0(j, "*.tif$")),
#                              full.names = TRUE)
#   
#   hsi_s[[j]] <- stack(hsi_list[[j]])
#   
#   pi_s[[j]] <- stack(pi_list[[j]])
#   
#   hd_list[[j]] <- hsi_s[[j]] + pi_s[[j]] - PM_r
#   
#   writeRaster(hd_list[[j]], paste0("RESULTS/RAS_SCH/HD/", j, "-HD", ".tif"), 
#              overwrite = TRUE)
# }

hd_fun <- function(j) {
  hsi_files <- list.files("RESULTS/RAS_SCH/HSi",
                          pattern = glob2rx(paste0(j, "*.tif$")),
                          full.names = TRUE)
  
  pi_files <- list.files("RESULTS/RAS_SCH/PI",
                         pattern = glob2rx(paste0(j, "*.tif$")),
                         full.names = TRUE)
  
  hsi_stack <- stack(hsi_files)
  
  pi_stack <- stack(pi_files)
  
  hd_stack <- hsi_stack + pi_stack - PM_r
  
  # Replace negative pixel values in hd_stack with 0
  #hd_stack <- pmax(hd_stack, 0)
  
  hd_stack <- clamp(hd_stack, lower = 0)
  
  writeRaster(hd_stack, paste0("RESULTS/RAS_SCH/HD/", j, "-HD", ".tif"), 
                     overwrite = TRUE)
}

hd_l <- lapply(year_month, hd_fun)

names(hd_l) <- year_month

#-----C1-----
# Load the infiltration coefficient raster file and project it
CC_r <- rast("RESULTS/RAS_SCH/CC_ras.tif") %>% terra::project("epsg:9377")

#Convert the file type from SpatRaster to RasterLayer
CC_r <- raster(CC_r)

c1_fun <- function(j) {
  hsi_files <- list.files("RESULTS/RAS_SCH/HSi",
                          pattern = glob2rx(paste0(j, "*.tif$")),
                          full.names = TRUE)
  
  pi_files <- list.files("RESULTS/RAS_SCH/PI",
                         pattern = glob2rx(paste0(j, "*.tif$")),
                         full.names = TRUE)
  
  hsi_stack <- stack(hsi_files)
  
  pi_stack <- stack(pi_files)
  
  c1_stack <- (hsi_stack - PM_r + pi_stack) / (CC_r - PM_r)
  
  # Replace negative pixel values in hd_stack with 0
  #hd_stack <- pmax(hd_stack, 0)
  
  c1_stack <- clamp(c1_stack, lower = 0, upper = 1)
  
  writeRaster(c1_stack, paste0("RESULTS/RAS_SCH/C1/", j, "-C1", ".tif"), 
              overwrite = TRUE)
}

c1_l <- lapply(year_month, c1_fun)

names(c1_l) <- year_month


#-----ETR1----
etr1_fun <- function(j) {
  C1_files <- list.files("RESULTS/RAS_SCH/C1",
                          pattern = glob2rx(paste0(j, "*.tif$")),
                          full.names = TRUE)
  
  etpm_files <- list.files("RESULTS/ETPm",
                         pattern = glob2rx(paste0(j, "*.tif$")),
                         full.names = TRUE)
  
  C1_stack <- stack(C1_files)
  
  etpm_stack <- stack(etpm_files)
  
  etr1_s <- C1_stack * etpm_stack
  
  writeRaster(etr1_s, paste0("RESULTS/RAS_SCH/ETR1/", j, "-ETR1", ".tif"), 
              overwrite = TRUE)
}

etr1_l <- lapply(year_month, etr1_fun)

names(etr1_l) <- year_month

#-----C2-----
c2_fun <- function(j) {
  hsi_files <- list.files("RESULTS/RAS_SCH/HSi",
                          pattern = glob2rx(paste0(j, "*.tif$")),
                          full.names = TRUE)
  
  etr1_files <- list.files("RESULTS/RAS_SCH/ETR1",
                           pattern = glob2rx(paste0(j, "*.tif$")),
                           full.names = TRUE)
  
  pi_files <- list.files("RESULTS/RAS_SCH/PI",
                           pattern = glob2rx(paste0(j, "*.tif$")),
                           full.names = TRUE)
  
  hsi_stack <- stack(hsi_files)
  
  etr1_stack <- stack(etr1_files)
  
  pi_stack <- stack(pi_files)
  
  c2_s <- (hsi_stack - PM_r + pi_stack - etr1_stack) / (CC_r - PM_r)
  
  c2_s <- clamp(c2_s, lower = 0, upper = 1)
  
  writeRaster(c2_s, paste0("RESULTS/RAS_SCH/C2/", j, "-C2", ".tif"), 
              overwrite = TRUE)
}

c2_l <- lapply(year_month, c2_fun)

names(c2_l) <- year_month

#-----ETR-----
# Función para comparar los píxeles
compare_pixels <- function(x, y) {
  ifelse(x <= y, x, y)
}

etr_fun <- function(j) {
  c1_files <- list.files("RESULTS/RAS_SCH/C1",
                          pattern = glob2rx(paste0(j, "*.tif$")),
                          full.names = TRUE)
  
  c2_files <- list.files("RESULTS/RAS_SCH/C2",
                           pattern = glob2rx(paste0(j, "*.tif$")),
                           full.names = TRUE)
  
  etpm_files <- list.files("RESULTS/ETPm",
                         pattern = glob2rx(paste0(j, "*.tif$")),
                         full.names = TRUE)
  
  hd_files <- list.files("RESULTS/RAS_SCH/HD",
                         pattern = glob2rx(paste0(j, "*.tif$")),
                         full.names = TRUE)
  
  c1_stack <- stack(c1_files)
  
  c2_stack <- stack(c2_files)
  
  etpm_stack <- stack(etpm_files)
  
  hd_stack <- stack(hd_files)
  
  etr_s <- ((c1_stack + c2_stack) / 2) * etpm_stack
  
  # Aplicar la función de comparación a los rasters etr_s y hd_stack
  result_etr <- overlay(etr_s, hd_stack, fun = compare_pixels)
  
  writeRaster(result_etr, paste0("RESULTS/RAS_SCH/ETR/", j, "-ETR", ".tif"), 
              overwrite = TRUE)
}

etr_l <- lapply(year_month, etr_fun)

names(etr_l) <- year_month

#-----HSf-----
hsf_fun <- function(j) {
  hd_files <- list.files("RESULTS/RAS_SCH/HD",
                         pattern = glob2rx(paste0(j, "*.tif$")),
                         full.names = TRUE)
  
  etr_files <- list.files("RESULTS/RAS_SCH/ETR",
                         pattern = glob2rx(paste0(j, "*.tif$")),
                         full.names = TRUE)
  
  hd_stack <- stack(hd_files)
  
  etr_stack <- stack(etr_files)
  
  hsf_s <- hd_stack + PM_r - etr_stack
  
  # Aplicar la función de comparación a los rasters etr_s y hd_stack
  result_hsf <- overlay(hsf_s, CC_r, fun = compare_pixels)
  
  result_hsf <- clamp(result_hsf, lower = 0)
  
  writeRaster(result_hsf, paste0("RESULTS/RAS_SCH/HSf/", j, "-HSf", ".tif"), 
              overwrite = TRUE)
}

hsf_l <- lapply(year_month, hsf_fun)

names(hsf_l) <- year_month

#-----HSf-----
#RECHARGE - SCHOSINSKY - RESULTS
rec_sch_fun <- function(j) {
  pi_files <- list.files("RESULTS/RAS_SCH/PI",
                         pattern = glob2rx(paste0(j, "*.tif$")),
                         full.names = TRUE)
  
  hsi_files <- list.files("RESULTS/RAS_SCH/HSi",
                          pattern = glob2rx(paste0(j, "*.tif$")),
                          full.names = TRUE)
  
  hsf_files <- list.files("RESULTS/RAS_SCH/HSf",
                         pattern = glob2rx(paste0(j, "*.tif$")),
                         full.names = TRUE)
  
  etr_files <- list.files("RESULTS/RAS_SCH/ETR",
                          pattern = glob2rx(paste0(j, "*.tif$")),
                          full.names = TRUE)
  
  etpm_files <- list.files("RESULTS/ETPm",
                          pattern = glob2rx(paste0(j, "*.tif$")),
                          full.names = TRUE)
  
  pi_s <- stack(pi_files)
  
  hsi_s <- stack(hsi_files)
  
  hsf_s <- stack(hsf_files)
  
  etr_s <- stack(etr_files)
  
  etpm_s <- stack(etpm_files)
  
  compare_v <- function(pi, etpm) {
    ifelse(pi > etpm, 0, pi + hsi_s - hsf_s - etr_s)
  }
  
  rp_sch <- overlay(pi_s, etpm_s, fun = compare_pixels)
  
  #rp_sch <- pi_s + hsi_s - hsf_s - etr_s
  
  # Aplicar la función de comparación a los rasters etr_s y hd_stack
  #result_hsf <- overlay(hsf_s, CC_r, fun = compare_pixels)
  
  #result_hsf <- clamp(result_hsf, lower = 0)
  
  writeRaster(rp_sch, paste0("RESULTS/RAS_SCH/REC_M/", j, "-REC_M", ".tif"), 
              overwrite = TRUE)
}

rpsch_l <- lapply(year_month, rec_sch_fun)

names(rpsch_l) <- year_month

#RECHARGE - SCHOSINSKY - RESULTS
# Function to perform operations for each month
rechs_fun <- function(x) {
  #Pattern to load the raster files
  pattern <- glob2rx(paste0(x, "*.tif$"))
  
  # List monthly infiltrating precipitation data
  pi_files <- list.files("RESULTS/RAS_SCH/PI/", pattern = pattern, 
                            full.names = TRUE)
  
  #List monthly evapotranspiration data
  etpm_files <- list.files("RESULTS/ETPm/", pattern = pattern, 
                           full.names = TRUE)
  
  # precm_stack <- brick(precm_files)
  # retm_stack <- brick(retm_files)
  #The layer of the created RasterStack is selected
  pi_stack <- raster(stack(pi_files), 1)
  
  #The layer of the created RasterStack is selected
  etpm_stack <- raster(stack(etpm_files), 1)
  
  #The potential recharge is obtained
  rp_s <- pi_stack - (0.45 * etpm_stack)
  
  #rp_s <- clamp(rp_s, lower = 0)
  
  #The address where the result is to stored is created
  outfile <- file.path("RESULTS", "REC_A", "REC_SCH", paste0(x, "-REC_SCH.tif"))
  
  #The result is exported as a raster file
  writeRaster(rp_s, outfile, overwrite = TRUE)
}

# Apply the function to each month
system.time(rps_l <- lapply(year_month, rechs_fun))

#Name the list elements by year_month
names(rps_l) <- year_month

# Create a vector of unique years
unique_years <- unique(years)

# Define a function to sum rasters for a specific year
sum_yearly_rasters <- function(year) {
  # Get a vector of months for this year
  year_months <- paste0(year, "-", sprintf("%02d", months))
  
  # Get the list of raster files for these months
  raster_files <- paste0("RESULTS/RAS_SCH/REC_M/", year_months, "-REC_M.tif")
  
  # Read the raster files into a RasterStack
  raster_stack <- stack(raster_files)
  
  # Sum the layers in the RasterStack
  yearly_sum <- sum(raster_stack)
  
  # Write the yearly sum to a new raster file
  outfile <- paste0("RESULTS/RAS_SCH/REC_A/", year, "-REC_A.tif")
  writeRaster(yearly_sum, outfile, overwrite = TRUE)
  
  return(yearly_sum)
}

# Apply the function to each year
system.time(yearly_sums <- lapply(unique_years, sum_yearly_rasters))

# Name the list elements by unique_years
names(yearly_sums) <- unique_years

#-----
ruta <- "RESULTS/RAS_SCH/REC_M/"  
archivos_raster <- list.files(path = ruta, pattern="*.tif$", full.names=TRUE)
lista_raster <- lapply(archivos_raster, raster)


tiene_negativos <- function(r) {
  any(cellStats(r, stat = 'min') < 0)
}

resultados <- lapply(lista_raster, tiene_negativos)

hay_negativos <- any(unlist(resultados))

num_negativos <- sum(unlist(resultados))
#-----

agro$texture_values <- replace_na(agro$texture_values, 0)
#-------------------

sel_years <- c("2010", "2015", "2013")



font_add(family = "CMU Serif", regular = "cmunrm.ttf")
font_import(paths = "C:/Windows/Fonts/CMU Serif.ttf")
showtext_auto()

bound <- st_read("V1_CUENCA_30m.shp")
# plot the raster using ggplot2
ggplot(data = r_df, aes(x = x, y = y, fill = layer)) +
  geom_raster() +
  scale_fill_viridis(na.value = "transparent") + 
  coord_equal() +
  labs(title = "Methodology 1",
       x = "Longitude",
       y = "Latitude",
       fill = "Recharge (mm/y)") +
  theme_bw() +
  theme(text = element_text(family = "CMU Serif", size = 14),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_rect(colour = "black", size = 0.5),
        legend.title.align = 0.5,
        #axis.line = element_blank()) +
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
        #panel.background = element_blank(),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        #plot.margin = unit(c(1, 1, 1, 2), "cm"),
        #legend.background = element_rect(fill = "white"),
        #legend.box = "horizontal") +
  geom_text(x = 0, y = 0, label = "my label", 
            size = 6, family = "CMU Serif") 


ggplot() +
  #geom_sf(data = st_as_sf(map), fill = "grey") +
  geom_sf(data = st_as_sf(rec_t), aes(fill = value)) +
  scale_fill_viridis() +
  labs(title = "Raster Map")


raster_files <- list.files(path = "RESULTS/REC_A/prueba/", pattern = "*.tif",
                           full.names = TRUE)

# Nombres de los rasters
nombres_rasters <- c("Nombre1", "Nombre2", "Nombre3", "Nombre4", "Nombre5", "Nombre6", "Nombre7", "Nombre8", "Nombre9", "Nombre10", "Nombre11", "Nombre12")

plots <- list()

for (i in 1:length(raster_files)) {
  
  r <- raster(raster_files[i])
  
  # p <- levelplot(r,
  #                main = nombres_rasters[i],
  #                xlab = "Longitud",
  #                ylab = "Latitud",
  #                scales = list(x = list(rot = 90)),
  #                col.regions = viridis(n = 100),
  #                colorkey = list(space = "right", height = 0.5, width = 1))
  # 
  p <- levelplot(r,
                 margin = FALSE,
                 colorkey = list(
                   space = 'right',
                   labels = list(at = 0:100, font = 4),
                   axis.line = list(col = 'black'),
                   width = 0.75
                   ),
                 par.settings = list (
                   strip.border = list(col = 'transparent'),
                   strip.background = list(col = 'transparent')
                   #axis.line = list(col = 'transparent') 
                 ),
                 scales = list(draw = FALSE),
                 col.regions = viridis,
                 at = seq(0,100, len = 101),
                 names.attr = rep('', nlayers(r)),
                 main = nombres_rasters[i]) 


    
  plots[[i]] <- p
}

grid.arrange(grobs = plots, ncol = 3)

cpath <- "C:/Users/David Gomez/Desktop/TESIS/DATOS/Shapes/CUENCA_120m.shp"

border <- readOGR(dsn = cpath, layer = "CUENCA_120m")

border <- st_read(cpath) %>% as("Spatial")

border <- st_read(cpath)

pl <- levelplot(Ci_r,
               margin = FALSE,
               colorkey = list(
                 space = 'right',
                 #labels = list(at = 0:100, font = 4),
                 axis.line = list(col = 'black'),
                 width = 0.75
               ),
               #par.settings = list (
                 #strip.border = list(col = 'transparent'),
                 #strip.background = list(col = 'transparent')
                 #axis.line = list(col = 'transparent') 
               #),
               scales = list(draw = TRUE),
               col.regions = viridis,
               #at = seq(0,100, len = 101),
               #names.attr = rep('', nlayers(r)),
               main = "Recharge") +
  latticeExtra::layer(sp.polygons(border, lwd = 3))
  #+ 
  # layer(panel.abline(h = seq(from = 0, to = nrow(Ci_r), by = 100,
  #                            v = seq(from = 0, to = ncol(Ci_r), by = 100),
  #                            col = "black", lty = 3)))

library(ggplot2)
library(raster)
library(sf)

# Conversión del raster a un data.frame para ggplot
Ci_r_df <- as.data.frame(rasterToPoints(Ci_r))

# Conversión de la frontera a un objeto sf para ggplot
border_sf <- st_as_sf(border)

ggplot() +
  geom_tile(data = Ci_r_df, aes(x = x, y = y, fill = Ci)) +
  geom_sf(data = border_sf, fill = NA, color = 'black', size = 3) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Recharge", fill = "Legend Title") +
  theme(plot.title = element_text(family = "Times", face = "bold", size = 14),
        legend.text = element_text(family = "Times", size = 12))


library(sf)
library(ggplot2)
library(viridis)

cpath <- "C:/Users/David Gomez/Desktop/TESIS/DATOS/Shapes/CUENCA_120m.shp"

border <- st_read(cpath)

border_plot <- function(data, title = "Recharge") {
  ggplot() +
    geom_sf(data = data, size = 3) +
    labs(title = title) +
    theme_minimal() +
    scale_fill_viridis() 
}

border_plot(border)

Ci_r_df <- as.data.frame(Ci_r, xy=TRUE)

cpath <- "C:/Users/David Gomez/Desktop/TESIS/DATOS/Shapes/CUENCA_120m.shp"

border <- st_read(cpath)

# Load the infiltration coefficient raster file and project it
nina_rec_1 <- rast("RESULTS/REC_A/REC_1/2010-REC_1.tif") %>% 
  terra::project("epsg:9377")

#Convert the file type from SpatRaster to RasterLayer
nina_rec_1 <- raster(nina_rec_1)

nina_rec_1_spdf <- as(nina_rec_1, "SpatialPixelsDataFrame")

nina_rec_1_df <- as.data.frame(nina_rec_1_spdf)

colnames(nina_rec_1_df) <- c("value", "x", "y")

font_add(family = "CMU Serif", regular = "cmunrm.ttf")

showtext_auto()

#border_df <- 

#border_lines <- st_cast(border, "LINESTRING")

value_min_nina_1 <- min(nina_rec_1_df$value, na.rm = TRUE)
value_max_nina_1 <- max(nina_rec_1_df$value, na.rm = TRUE)
value_mid_nina_1 <- (value_min_nina_1 + value_max_nina_1) / 2
  
# value_min <- as.numeric(value_min)
# value_mid <- as.numeric(value_mid)
# value_max <- as.numeric(value_max)

nina_plot_1 <- ggplot() +
  #Define aesthetic mappings
  geom_tile(data = nina_rec_1_df, aes(x = x, y = y, fill = value)) +
  #geom_raster() +
  
  #Add geometric layers
  geom_sf(data = border, fill = NA, color = "black") +
  
  #Set coordinate system
  coord_sf() +
  #geom_polygon(data = border_sf, aes(x = long, y = lat, group = group),
  #             fill = NA, color = 'black', size = 3) +
  #geom_sf(data = border_sf, fill = NA, color = 'black', size = 3) +
  
  #Adjust color scale
  scale_fill_gradient(low = "lightblue", high = "darkblue", 
                      na.value = "transparent",
                     breaks = c(value_min_nina_1, value_mid_nina_1, 
                                value_max_nina_1),
                     labels = function(x) sprintf("%.2f", x)) + 
  #coord_equal() +
  
  #Set labels
  labs(title = "Methodology 1",
       x = "Longitude",
       y = "Latitude",
       fill = "Recharge (mm/y)") +
  
  # Set theme
  theme_bw() +
  theme(text = element_text(family = "CMU Serif", size = 14),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_rect(colour = "black", linewidth = 0.5),
        legend.title.align = 0.5) +
        #axis.line = element_blank()) +
        #axis.text = element_blank(),
        #axis.ticks = element_blank()) +
  #panel.background = element_blank(),
  #panel.grid.major = element_blank(),
  #panel.grid.minor = element_blank(),
  #plot.margin = unit(c(1, 1, 1, 2), "cm"),
  #legend.background = element_rect(fill = "white"),
  #legend.box = "horizontal") +
  
  # Add text layer
  geom_text(x = 0, y = 0, label = "my label", 
            size = 6, family = "CMU Serif")

# Load the infiltration coefficient raster file and project it
nina_rec_2 <- rast("RESULTS/REC_A/REC_2/2010-REC_2.tif") %>% 
  terra::project("epsg:9377")

#Convert the file type from SpatRaster to RasterLayer
nina_rec_2 <- raster(nina_rec_2)

nina_rec_2_spdf <- as(nina_rec_2, "SpatialPixelsDataFrame")

nina_rec_2_df <- as.data.frame(nina_rec_2_spdf)

colnames(nina_rec_2_df) <- c("value", "x", "y")

#font_add(family = "CMU Serif", regular = "cmunrm.ttf")

#showtext_auto()

#border_df <- 

#border_lines <- st_cast(border, "LINESTRING")

value_min_nina_2 <- min(nina_rec_2_df$value, na.rm = TRUE)
value_max_nina_2 <- max(nina_rec_2_df$value, na.rm = TRUE)
value_mid_nina_2 <- (value_min_nina_2 + value_max_nina_2) / 2

# value_min <- as.numeric(value_min)
# value_mid <- as.numeric(value_mid)
# value_max <- as.numeric(value_max)

nina_plot_2 <- ggplot() +
  #Define aesthetic mappings
  geom_tile(data = nina_rec_2_df, aes(x = x, y = y, fill = value)) +
  #geom_raster() +
  
  #Add geometric layers
  geom_sf(data = border, fill = NA, color = "black") +
  
  #Set coordinate system
  coord_sf() +
  #geom_polygon(data = border_sf, aes(x = long, y = lat, group = group),
  #             fill = NA, color = 'black', size = 3) +
  #geom_sf(data = border_sf, fill = NA, color = 'black', size = 3) +
  
  #Adjust color scale
  scale_fill_gradient(low = "lightblue", high = "darkblue", 
                      na.value = "transparent",
                      breaks = c(value_min_nina_2, value_mid_nina_2, 
                                 value_max_nina_2),
                      labels = function(x) sprintf("%.2f", x)) + 
  #coord_equal() +
  
  #Set labels
  labs(title = "Methodology 2",
       x = "Longitude",
       y = "Latitude",
       fill = "Recharge (mm/y)") +
  
  # Set theme
  theme_bw() +
  theme(text = element_text(family = "CMU Serif", size = 14),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_rect(colour = "black", linewidth = 0.5),
        legend.title.align = 0.5) +
  #axis.line = element_blank()) +
  #axis.text = element_blank(),
  #axis.ticks = element_blank()) +
  #panel.background = element_blank(),
  #panel.grid.major = element_blank(),
  #panel.grid.minor = element_blank(),
  #plot.margin = unit(c(1, 1, 1, 2), "cm"),
  #legend.background = element_rect(fill = "white"),
  #legend.box = "horizontal") +
  
  # Add text layer
geom_text(x = 0, y = 0, label = "my label", 
          size = 6, family = "CMU Serif")


# Load the infiltration coefficient raster file and project it
nina_rec_3 <- rast("RESULTS/REC_A/REC_3/2010-REC_3.tif") %>% 
  terra::project("epsg:9377")

#Convert the file type from SpatRaster to RasterLayer
nina_rec_3 <- raster(nina_rec_3)

nina_rec_3_spdf <- as(nina_rec_3, "SpatialPixelsDataFrame")

nina_rec_3_df <- as.data.frame(nina_rec_3_spdf)

colnames(nina_rec_3_df) <- c("value", "x", "y")

#font_add(family = "CMU Serif", regular = "cmunrm.ttf")

#showtext_auto()

#border_df <- 

#border_lines <- st_cast(border, "LINESTRING")

value_min_nina_3 <- min(nina_rec_3_df$value, na.rm = TRUE)
value_max_nina_3 <- max(nina_rec_3_df$value, na.rm = TRUE)
value_mid_nina_3 <- (value_min_nina_3 + value_max_nina_3) / 2

# value_min <- as.numeric(value_min)
# value_mid <- as.numeric(value_mid)
# value_max <- as.numeric(value_max)

nina_plot_3 <- ggplot() +
  #Define aesthetic mappings
  geom_tile(data = nina_rec_3_df, aes(x = x, y = y, fill = value)) +
  #geom_raster() +
  
  #Add geometric layers
  geom_sf(data = border, fill = NA, color = "black") +
  
  #Set coordinate system
  coord_sf() +
  #geom_polygon(data = border_sf, aes(x = long, y = lat, group = group),
  #             fill = NA, color = 'black', size = 3) +
  #geom_sf(data = border_sf, fill = NA, color = 'black', size = 3) +
  
  #Adjust color scale
  scale_fill_gradient(low = "lightblue", high = "darkblue", 
                      na.value = "transparent",
                      breaks = c(value_min_nina_3, value_mid_nina_3, 
                                 value_max_nina_3),
                      labels = function(x) sprintf("%.2f", x)) + 
  #coord_equal() +
  
  #Set labels
  labs(title = "Methodology 3",
       x = "Longitude",
       y = "Latitude",
       fill = "Recharge (mm/y)") +
  
  # Set theme
  theme_bw() +
  theme(text = element_text(family = "CMU Serif", size = 14),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_rect(colour = "black", linewidth = 0.5),
        legend.title.align = 0.5) +
  #axis.line = element_blank()) +
  #axis.text = element_blank(),
  #axis.ticks = element_blank()) +
  #panel.background = element_blank(),
  #panel.grid.major = element_blank(),
  #panel.grid.minor = element_blank(),
  #plot.margin = unit(c(1, 1, 1, 2), "cm"),
  #legend.background = element_rect(fill = "white"),
  #legend.box = "horizontal") +
  
  # Add text layer
geom_text(x = 0, y = 0, label = "my label", 
          size = 6, family = "CMU Serif")

# Load the infiltration coefficient raster file and project it
nina_rec_4 <- rast("RESULTS/RAS_SCH/REC_A/2010-REC_A.tif") %>% 
  terra::project("epsg:9377")

#Convert the file type from SpatRaster to RasterLayer
nina_rec_4 <- raster(nina_rec_4)

nina_rec_4_spdf <- as(nina_rec_4, "SpatialPixelsDataFrame")

nina_rec_4_df <- as.data.frame(nina_rec_4_spdf)

colnames(nina_rec_4_df) <- c("value", "x", "y")

#font_add(family = "CMU Serif", regular = "cmunrm.ttf")

#showtext_auto()

#border_df <- 

#border_lines <- st_cast(border, "LINESTRING")

value_min_nina_4 <- min(nina_rec_4_df$value, na.rm = TRUE)
value_max_nina_4 <- max(nina_rec_4_df$value, na.rm = TRUE)
value_mid_nina_4 <- (value_min_nina_4 + value_max_nina_4) / 2

# value_min <- as.numeric(value_min)
# value_mid <- as.numeric(value_mid)
# value_max <- as.numeric(value_max)

nina_plot_4 <- ggplot() +
  #Define aesthetic mappings
  geom_tile(data = nina_rec_4_df, aes(x = x, y = y, fill = value)) +
  #geom_raster() +
  
  #Add geometric layers
  geom_sf(data = border, fill = NA, color = "black") +
  
  #Set coordinate system
  coord_sf() +
  #geom_polygon(data = border_sf, aes(x = long, y = lat, group = group),
  #             fill = NA, color = 'black', size = 3) +
  #geom_sf(data = border_sf, fill = NA, color = 'black', size = 3) +
  
  #Adjust color scale
  scale_fill_gradient(low = "lightblue", high = "darkblue", 
                      na.value = "transparent",
                      breaks = c(value_min_nina_4, value_mid_nina_4, 
                                 value_max_nina_4),
                      labels = function(x) sprintf("%.2f", x)) + 
  #coord_equal() +
  
  #Set labels
  labs(title = "Methodology 4",
       x = "Longitude",
       y = "Latitude",
       fill = "Recharge (mm/y)") +
  
  # Set theme
  theme_bw() +
  theme(text = element_text(family = "CMU Serif", size = 14),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_rect(colour = "black", linewidth = 0.5),
        legend.title.align = 0.5) +
  #axis.line = element_blank()) +
  #axis.text = element_blank(),
  #axis.ticks = element_blank()) +
  #panel.background = element_blank(),
  #panel.grid.major = element_blank(),
  #panel.grid.minor = element_blank(),
  #plot.margin = unit(c(1, 1, 1, 2), "cm"),
  #legend.background = element_rect(fill = "white"),
  #legend.box = "horizontal") +
  
  # Add text layer
geom_text(x = 0, y = 0, label = "my label", 
          size = 6, family = "CMU Serif")

#-----prueba-----

cpath <- "C:/Users/David Gomez/Desktop/TESIS/DATOS/Shapes/CUENCA_120m.shp"
border <- st_read(cpath)
font_add(family = "CMU Serif", regular = "cmunrm.ttf")
showtext_auto()

nina_raster <- function(raster_path, title, yr) {
  raster_obj <- rast(raster_path) %>% 
    terra::project("epsg:9377") %>% 
    raster() %>% 
    as("SpatialPixelsDataFrame") %>% 
    as.data.frame()
  
  colnames(raster_obj) <- c("value", "x", "y")
  value_min <- min(raster_obj$value, na.rm = TRUE)
  value_max <- max(raster_obj$value, na.rm = TRUE)
  value_mid <- (value_min + value_max) / 2
  
  plot <- ggplot() +
    geom_tile(data = raster_obj, aes(x = x, y = y, fill = value)) +
    geom_sf(data = border, fill = NA, color = "black") +
    coord_sf() +
    scale_fill_viridis(option = "mako", 
                       na.value = "transparent",
                       breaks = c(value_min, value_mid, value_max),
                       labels = function(x) sprintf("%.2f", x),
                       direction = -1) +
    labs(title = title,
         subtitle = paste("La Niña year", yr),
         x = "Longitude",
         y = "Latitude",
         fill = "Recharge (mm/yr)") +
    theme_bw() +
    theme(text = element_text(family = "CMU Serif", size = 14),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 14),
          legend.position = "right",
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12),
          legend.key = element_rect(colour = "black", linewidth = 0.5),
          legend.title.align = 0.5,
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title = element_text(size = 14, face = "bold")) +
    geom_text(x = 0, y = 0, label = "my label", 
              size = 6, family = "CMU Serif")
  
  return(plot)
}

nina_raster <- function(raster_path, title, yr) {
  raster_obj <- rast(raster_path) %>% 
    terra::project("epsg:9377") %>% 
    raster() %>% 
    as("SpatialPixelsDataFrame") %>% 
    as.data.frame()
  
  colnames(raster_obj) <- c("value", "x", "y")
  value_min <- min(raster_obj$value, na.rm = TRUE)
  value_max <- max(raster_obj$value, na.rm = TRUE)
  value_mid <- (value_min + value_max) / 2
  
  plot <- ggplot() +
    geom_tile(data = raster_obj, aes(x = x, y = y, fill = value)) +
    geom_sf(data = border, fill = NA, color = "black") +
    coord_sf() +
    scale_fill_viridis(option = "mako", 
                       na.value = "transparent",
                       breaks = c(value_min, value_mid, value_max),
                       labels = function(x) sprintf("%.2f", x),
                       direction = -1) +
    labs(title = NULL,
         subtitle = NULL,
         x = NULL,
         y = NULL,
         fill = NULL) +
    theme_bw() +
    theme(text = element_text(family = "CMU Serif", size = 14),
          plot.title = element_blank(), #modified
          plot.subtitle = element_blank(), #modified
          legend.position = "right", #modified
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12),
          legend.key = element_rect(colour = "black", linewidth = 0.5),
          legend.title.align = 0.5,
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()) + #modified
    geom_text(x = 0, y = 0, label = "my label", 
              size = 6, family = "CMU Serif")
  
  return(plot)
}

nina_plot_1 <- nina_raster("RESULTS/REC_A/REC_1/2010-REC_1.tif", 
                              "Chaturvedi Methodology", "2010")
nina_plot_2 <- nina_raster("RESULTS/REC_A/REC_2/2010-REC_2.tif", 
                              "U.P. Irrigation Research Institute Methodology",
                           "2010")
nina_plot_3 <- nina_raster("RESULTS/REC_A/REC_3/2010-REC_3.tif", 
                              "Amritsar Methodology", "2010")
nina_plot_4 <- nina_raster("RESULTS/RAS_SCH/REC_A/2010-REC_A.tif", 
                              "Schosinsky and Losilla Methodology", "2010")

ggsave("RESULTS/GRAPHS/NINA/REC_1_NINA_2010.pdf", nina_plot_1)

ggsave("RESULTS/GRAPHS/NINA/REC_2_NINA_2010.pdf", nina_plot_2)

ggsave("RESULTS/GRAPHS/NINA/REC_3_NINA_2010.pdf", nina_plot_3)

ggsave("RESULTS/GRAPHS/NINA/REC_4_NINA_2010.pdf", nina_plot_4)


print(nina_plot_1)

nino_raster <- function(raster_path, title, yr) {
  raster_obj <- rast(raster_path) %>% 
    terra::project("epsg:9377") %>% 
    raster() %>% 
    as("SpatialPixelsDataFrame") %>% 
    as.data.frame()
  
  colnames(raster_obj) <- c("value", "x", "y")
  value_min <- min(raster_obj$value, na.rm = TRUE)
  value_max <- max(raster_obj$value, na.rm = TRUE)
  value_mid <- (value_min + value_max) / 2
  
  plot <- ggplot() +
    geom_tile(data = raster_obj, aes(x = x, y = y, fill = value)) +
    geom_sf(data = border, fill = NA, color = "black") +
    coord_sf() +
    scale_fill_viridis(option = "cividis", 
                        na.value = "transparent",
                        breaks = c(value_min, value_mid, value_max),
                        labels = function(x) sprintf("%.2f", x),
                       direction = -1) +
    labs(title = title,
         subtitle = paste("El Niño year", yr),
         x = "Longitude",
         y = "Latitude",
         fill = "Recharge (mm/yr)") +
    theme_bw() +
    theme(text = element_text(family = "CMU Serif", size = 14),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 14),
          legend.position = "right",
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12),
          legend.key = element_rect(colour = "black", linewidth = 0.5),
          legend.title.align = 0.5,
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title = element_text(size = 14, face = "bold")) +
    geom_text(x = 0, y = 0, label = "my label", 
              size = 6, family = "CMU Serif")
  
  return(plot)
}


nino_raster <- function(raster_path, title, yr) {
  raster_obj <- rast(raster_path) %>% 
    terra::project("epsg:9377") %>% 
    raster() %>% 
    as("SpatialPixelsDataFrame") %>% 
    as.data.frame()
  
  colnames(raster_obj) <- c("value", "x", "y")
  value_min <- min(raster_obj$value, na.rm = TRUE)
  value_max <- max(raster_obj$value, na.rm = TRUE)
  value_mid <- (value_min + value_max) / 2
  
  plot <- ggplot() +
    geom_tile(data = raster_obj, aes(x = x, y = y, fill = value)) +
    geom_sf(data = border, fill = NA, color = "black") +
    coord_sf() +
    scale_fill_viridis(option = "mako", 
                       na.value = "transparent",
                       breaks = c(value_min, value_mid, value_max),
                       labels = function(x) sprintf("%.2f", x),
                       direction = -1) +
                      #guide = "none") + #modified
    labs(title = NULL,
         subtitle = NULL,
         x = NULL,
         y = NULL,
         fill = NULL) +
    theme_bw() +
    theme(text = element_text(family = "CMU Serif", size = 14),
          plot.title = element_blank(), #modified
          plot.subtitle = element_blank(), #modified
          legend.position = "none", #modified
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12),
          legend.key = element_rect(colour = "black", linewidth = 0.5),
          legend.title.align = 0.5,
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()) + #modified
    geom_text(x = 0, y = 0, label = "my label", 
              size = 6, family = "CMU Serif")
  
  return(plot)
}

nino_plot_1 <- nino_raster("RESULTS/REC_A/REC_1/2015-REC_1.tif", 
                           "Chaturvedi Methodology", "2015")

nino_plot_2 <- nino_raster("RESULTS/REC_A/REC_2/2015-REC_2.tif", 
                           "U.P. Irrigation Research Institute Methodology", 
                           "2015")

nino_plot_3 <- nino_raster("RESULTS/REC_A/REC_3/2015-REC_3.tif", 
                           "Amritsar Methodology", "2015")

nino_plot_4 <- nino_raster("RESULTS/RAS_SCH/REC_A/2015-REC_A.tif", 
                           "Schosinsky and Losilla Methodology", "2015")

ggsave("RESULTS/GRAPHS/NINO/REC_1_NINO_2015.pdf", nino_plot_1)

ggsave("RESULTS/GRAPHS/NINO/REC_2_NINO_2015.pdf", nino_plot_2)

ggsave("RESULTS/GRAPHS/NINO/REC_3_NINO_2015.pdf", nino_plot_3)

ggsave("RESULTS/GRAPHS/NINO/REC_4_NINO_2015.pdf", nino_plot_4)
# Combine all the plots
grid.arrange(nina_plot_1, nina_plot_2, nina_plot_3, nina_plot_4, ncol=2)


print(nino_plot_1)

norm_raster <- function(raster_path, title, yr) {
  raster_obj <- rast(raster_path) %>% 
    terra::project("epsg:9377") %>% 
    raster() %>% 
    as("SpatialPixelsDataFrame") %>% 
    as.data.frame()
  
  colnames(raster_obj) <- c("value", "x", "y")
  value_min <- min(raster_obj$value, na.rm = TRUE)
  value_max <- max(raster_obj$value, na.rm = TRUE)
  value_mid <- (value_min + value_max) / 2
  
  plot <- ggplot() +
    geom_tile(data = raster_obj, aes(x = x, y = y, fill = value)) +
    geom_sf(data = border, fill = NA, color = "black") +
    coord_sf() +
    scale_fill_viridis(option = "viridis", 
                       na.value = "transparent",
                       breaks = c(value_min, value_mid, value_max),
                       labels = function(x) sprintf("%.2f", x),
                       direction = -1) +
    labs(title = title,
         subtitle = paste("Neutral year", yr),
         x = "Longitude",
         y = "Latitude",
         fill = "Recharge (mm/yr)") +
    theme_bw() +
    theme(text = element_text(family = "CMU Serif", size = 14),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 14),
          legend.position = "right",
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12),
          legend.key = element_rect(colour = "black", linewidth = 0.5),
          legend.title.align = 0.5,
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title = element_text(size = 14, face = "bold")) +
    geom_text(x = 0, y = 0, label = "my label", 
              size = 6, family = "CMU Serif")
  
  return(plot)
}


norm_raster <- function(raster_path, title, yr) {
  raster_obj <- rast(raster_path) %>% 
    terra::project("epsg:9377") %>% 
    raster() %>% 
    as("SpatialPixelsDataFrame") %>% 
    as.data.frame()
  
  colnames(raster_obj) <- c("value", "x", "y")
  value_min <- min(raster_obj$value, na.rm = TRUE)
  value_max <- max(raster_obj$value, na.rm = TRUE)
  value_mid <- (value_min + value_max) / 2
  
  plot <- ggplot() +
    geom_tile(data = raster_obj, aes(x = x, y = y, fill = value)) +
    geom_sf(data = border, fill = NA, color = "black") +
    coord_sf() +
    scale_fill_viridis(option = "mako", 
                       na.value = "transparent",
                       breaks = c(value_min, value_mid, value_max),
                       labels = function(x) sprintf("%.2f", x),
                       direction = -1) +
                       #guide = "none") + #modified
    labs(title = NULL,
         subtitle = NULL,
         x = NULL,
         y = NULL,
         fill = NULL) +
    theme_bw() +
    theme(text = element_text(family = "CMU Serif", size = 14),
          plot.title = element_blank(), #modified
          plot.subtitle = element_blank(), #modified
          legend.position = "none", #modified
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12),
          legend.key = element_rect(colour = "black", linewidth = 0.5),
          legend.title.align = 0.5,
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()) + #modified
    geom_text(x = 0, y = 0, label = "my label", 
              size = 6, family = "CMU Serif")
  
  return(plot)
}

norm_plot_1 <- norm_raster("RESULTS/REC_A/REC_1/2013-REC_1.tif", 
                           "Chaturvedi Methodology", "2013")

norm_plot_2 <- norm_raster("RESULTS/REC_A/REC_2/2013-REC_2.tif", 
                           "U.P. Irrigation Research Institute Methodology",
                           "2013")

norm_plot_3 <- norm_raster("RESULTS/REC_A/REC_3/2013-REC_3.tif", 
                           "Amritsar Methodology", "2013")

norm_plot_4 <- norm_raster("RESULTS/RAS_SCH/REC_A/2013-REC_A.tif", 
                           "Schosinsky and Losilla Methodology", "2013")

ggsave("RESULTS/GRAPHS/NEUTRAL/REC_1_NEUTRAL_2013.pdf", norm_plot_1)

ggsave("RESULTS/GRAPHS/NEUTRAL/REC_2_NEUTRAL_2013.pdf", norm_plot_2)

ggsave("RESULTS/GRAPHS/NEUTRAL/REC_3_NEUTRAL_2013.pdf", norm_plot_3)

ggsave("RESULTS/GRAPHS/NEUTRAL/REC_4_NEUTRAL_2013.pdf", norm_plot_4)

print(norm_plot_3)

((nina_plot_1 + nina_plot_2) / (nina_plot_3 + nina_plot_4)) + plot_layout(guides = 'collect')


((nina_plot_1 + norm_plot_1 + nino_plot_1) / 
  (nina_plot_2 + norm_plot_2 + nino_plot_2) /
  (nina_plot_3 + norm_plot_3 + nino_plot_3) /
  (nina_plot_4 + norm_plot_4 + nino_plot_4)) &
  plot_layout(guides = 'collect')

pdf("RESULTS/GRAPHS/mi_grafico2.pdf", paper = "letter")
grid.arrange(nina_plot_1, nina_plot_2, nina_plot_3, nina_plot_4,
             nino_plot_1, nino_plot_2, nino_plot_3, nino_plot_4,
             norm_plot_1, norm_plot_2, norm_plot_3, norm_plot_4,
             nrow = 3, ncol = 4)
dev.off()

#Graph - All recharge

raster_files_t <- list("RESULTS/REC_A/REC_1/2010-REC_1.tif", 
                       "RESULTS/REC_A/REC_1/2013-REC_1.tif", 
                       "RESULTS/REC_A/REC_1/2015-REC_1.tif",
                       "RESULTS/REC_A/REC_2/2010-REC_2.tif", 
                       "RESULTS/REC_A/REC_2/2013-REC_2.tif", 
                       "RESULTS/REC_A/REC_2/2015-REC_2.tif",
                       "RESULTS/REC_A/REC_3/2010-REC_3.tif", 
                       "RESULTS/REC_A/REC_3/2013-REC_3.tif", 
                       "RESULTS/REC_A/REC_3/2015-REC_3.tif", 
                       "RESULTS/RAS_SCH/REC_A/2010-REC_A.tif",
                       "RESULTS/RAS_SCH/REC_A/2013-REC_A.tif",
                       "RESULTS/RAS_SCH/REC_A/2015-REC_A.tif")

rasters_t <- lapply(raster_files_t, raster)

# Antes de crear tu stack
rasters_t <- lapply(raster_files_t, function(x) {
  r <- raster(x)
  # Proyectar al sistema de coordenadas geográficas (WGS84)
  r <- projectRaster(r, crs = "+init=epsg:4686")
  return(r)
})

p_t <- stack(rasters_t)

min_values <- sapply(1:nlayers(p_t), function(i) cellStats(p_t[[i]], stat = 'min'))
max_values <- sapply(1:nlayers(p_t), function(i) cellStats(p_t[[i]], stat = 'max'))


global_min <- min(min_values, na.rm = TRUE)
global_max <- max(max_values, na.rm = TRUE)


nlev <- 100
my.at <- seq(from = global_min, to = global_max, length.out = nlev + 1)

my.cols <- rev(viridis::viridis_pal(option = "mako")(nlev))

cpath <- "C:/Users/David Gomez/Desktop/TESIS/DATOS/Shapes/CUENCA_120m.shp"
border <- st_read(cpath)

border <- st_transform(border, 4686)

lt_O <- levelplot(p_t, layout = c(3,4),
                margin=TRUE,                       
                colorkey=list(
                  space='bottom',                   
                  #labels=list(at=100:3000, by = 300, font=2),
                  axis.line=list(col='black'),
                  width=1#,
                  #labels = list(font = 2, cex = 1)#,
                  # title="mm/yr"
                ),    
                par.settings=list(
                  strip.border=list(col='transparent'),
                  strip.background=list(col='transparent'),
                  axis.line=list(col='black')#,
                  #ontsize=list(text=14, points=12)
                ),
                scales=list(x = list(draw=TRUE), y = list(draw = TRUE)),          
                col.regions= rev(viridis::mako(100)),
                #at = my.at,
                #at=seq(0, 3000, len=500),
                names.attr=rep('', nlayers(p_t)),
                xlab = "",
                ylab = "") + 
  layer(sp.polygons(as(border,'Spatial'), lwd = 0.5))

lp_rec <- levelplot(p_t, layout = c(3,4),
                margin=TRUE,                       
                colorkey=list(
                  space='bottom',                   
                  #labels=list(at=100:3000, by = 300, font=2),
                  axis.line=list(col='black'),
                  width=1#,
                  #labels = list(at = NULL)#,
                  #labels = list(font = 2, cex = 1)#,
                  # title="mm/yr"
                ),    
                par.settings=list(
                  strip.border=list(col='transparent'),
                  strip.background=list(col='transparent'),
                  axis.line=list(col='black')#,
                  #ontsize=list(text=14, points=12)
                ),
                scales=list(x = list(draw=TRUE),
                            y = list(draw = TRUE)),          
                col.regions= rev(viridis::mako(100)),
                #at = my.at,
                #at=seq(0, 3000, len=500),
                names.attr=rep('', nlayers(p_t)),
                xlab = "",
                ylab = "") + 
  layer(sp.polygons(as(border,'Spatial'), lwd = 0.5))#,

png(filename = "RESULTS/GRAPHS/Plots/all_rec.png", 
    width = 8.27, height = 11.69, units = "in", res = 600)

print(lp_rec)

dev.off()

png(filename = "RESULTS/GRAPHS/Plots/all_rec_cus.png", 
    width = 8, height = 10, units = "in", res = 600)

print(lp_rec)

dev.off()


a <- levelplot(p, layout = c(1,4),
               margin=TRUE,                       
               colorkey=list(
                 space='bottom',                   
                 #labels=list(at=100:3000, by = 300, font=2),
                 axis.line=list(col='black'),
                 width=1
               ),    
               par.settings=list(
                 strip.border=list(col='transparent'),
                 strip.background=list(col='transparent'),
                 axis.line=list(col='black')
               ),
               scales=list(draw=TRUE),            
               col.regions= reversed_palette,                   
               #at=seq(0, 3000, len=1001),
               names.attr=rep('', nlayers(p)))#,
#main = 'Metodologías SGC') # + 
# latticeExtra::layer(sp.polygons(borde, lwd=3))
#-----

bb <- ggplot(data = nina_rec_4_df, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  geom_sf(data = border, fill = NA, color = 'black', size = 3) +
  scale_fill_viridis(na.value = "transparent") + 
  coord_equal() +
  labs(title = "Methodology 1",
       x = "Longitude",
       y = "Latitude",
       fill = "Recharge (mm/y)") +
  theme_bw() +
  theme(text = element_text(family = "CMU Serif", size = 14),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_rect(colour = "black", size = 0.5),
        legend.title.align = 0.5,
        #axis.line = element_blank()) +
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  #panel.background = element_blank(),
  #panel.grid.major = element_blank(),
  #panel.grid.minor = element_blank(),
  #plot.margin = unit(c(1, 1, 1, 2), "cm"),
  #legend.background = element_rect(fill = "white"),
  #legend.box = "horizontal") +
  geom_text(x = 0, y = 0, label = "my label", 
            size = 6, family = "CMU Serif")


#---------------------------------


#--------------------------------
ppt_s <- list()
mt_l <- list()
mint_l <- list()
maxt_l <- list()
rec <- list()
rec_2 <- list()
rec_3 <- list()
rec_4 <- list()
pattern <- list()

for (j in years) {
  #Pattern to load the raster files
  pattern[[j]] <- glob2rx(paste0(j, "*.tif$"))
  
  ppt_s[[j]] <- list.files("RESULTS/PPT/", pattern = pattern[[j]], 
                               full.names = TRUE)
  
  rec[[j]] <- list.files("RESULTS/REC_A/REC_1/", pattern = pattern[[j]], 
                                full.names = TRUE)
  
  rec_2[[j]] <- list.files("RESULTS/REC_A/REC_2/", pattern = pattern[[j]],
                               full.names = TRUE)
  
  rec_3[[j]] <- list.files("RESULTS/REC_A/REC_3/", pattern = pattern[[j]],
                           full.names = TRUE)
  
  rec_4[[j]] <- list.files("RESULTS/RAS_SCH/REC_A/", pattern = pattern[[j]],
                           full.names = TRUE)
  
  mt_l[[j]] <- list.files("RESULTS/MTEMP/", pattern = pattern[[j]], 
                          full.names = TRUE)
  # ppt_s[[j]] <- stack(ppt_s[[j]])
  # rec[[j]] <- stack(rec[[j]])
  # rec_2[[j]] <- stack(rec_2[[j]])
  # rec_3[[j]] <- stack(rec_3[[j]])
  # rec_4[[j]] <- stack(rec_4[[j]])
}

for (j in year_month) {
  #Pattern to load the raster files
  pattern[[j]] <- glob2rx(paste0(j, "*.tif$"))
  
  mt_l[[j]] <- list.files("RESULTS/MTEMP/", pattern = pattern[[j]], 
                           full.names = TRUE)
}

#Creation of the stacks for each methodology to calculate the average
stk <- stack()
stk_2 <- stack()
stk_3 <- stack()
stk_4 <- stack()

#Creation of the precipitation stack to calculate the average precipitation
stk_ppt <- stack()

#Creation of the mean temperature stack to calculate the average temperature3
stk_mt <- stack()

#Creation of the for loop to add the recharge results to each stack, by means
#of the addLayer function
#addLayer function add a layer to a Raster* object
for (i in years) {
  stk_ppt <- addLayer(stk_ppt, ppt_s[[i]]) #Precipitation stack
  stk <- addLayer(stk, rec[[i]])
  stk_2 <- addLayer(stk_2, rec_2[[i]])
  stk_3 <- addLayer(stk_3, rec_3[[i]])
  stk_4 <- addLayer(stk_4, rec_4[[i]])
  stk_mt <- addLayer(stk_mt, mt_l[[i]])
}


#The calc function is used to calculate the average precipitation and recharge.
#In addition, null values are removed by na.rm = TRUE. 
ppt_t <- calc(stk_ppt, fun = mean, na.rm = TRUE)

writeRaster(ppt_t, "RESULTS/GRAPHS/ppt_avg.tif", overwrite = TRUE)

rec_t <- calc(stk, fun = mean, na.rm =TRUE)

rec_t_2 <- calc(stk_2, fun = mean, na.rm =TRUE)

rec_t_3 <- calc(stk_3, fun = mean, na.rm = TRUE)

rec_t_4 <- calc(stk_4, fun = mean, na.rm = TRUE)

mt_t <- calc(stk_mt, fun = mean, na.rm = TRUE)

#The percentage of precipitation that is converted to recharge is calculated
#to discard methodologies. According to empirical results, the maximum 
#percentage of precipitation that can be converted to recharge is 30%. 
#Therefore, methodologies with a higher percentage than this value will be
#discarded.
#The values function is used to extract the pixel values from the raster to
#calculate the average.
r_per <- 100 - mean(values( ( (ppt_t - rec_t) / ppt_t) * 100), 
                    na.rm = TRUE) 

r_per_2 <- 100 - mean(values( ( (ppt_t - rec_t_2) / ppt_t) * 100), 
                      na.rm = TRUE)

r_per_3 <- 100 - mean(values( ( (ppt_t - (rec_t_3)) / ppt_t) * 100),
                      na.rm = TRUE)

r_per_4 <- 100 - mean(values( ( (ppt_t - (rec_t_4)) / ppt_t) * 100),
                      na.rm = TRUE)

mt_v <- mean(values(mt_t), na.rm = TRUE)

ppt_v <- mean(values(ppt_t), na.rm = TRUE)
#r_df <- as.data.frame(rec_t, xy=TRUE)

writeRaster(rec_t, filename = 
              "C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/REC/REC_T_1",
            format = "GTiff", overwrite = TRUE)

writeRaster(rec_t_2, filename = 
              "C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/REC/REC_T_2",
            format = "GTiff", overwrite = TRUE)

writeRaster(rec_t_3, filename = 
              "C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/REC/REC_T_3",
            format = "GTiff", overwrite = TRUE)

writeRaster(rec_3[["2020"]], filename = 
              "C:/Users/David Gomez/Desktop/TESIS/DATOS/RESULTS/p2020",
            format = "GTiff", overwrite = TRUE)


#-----GRAPHS------

font_add(family = "CMU Serif", regular = "cmunrm.ttf")
showtext_auto()

#-----ANNUAL TIME SERIES-----

#Function to be able to discard stations without data - precipitation
agg <- function(x) {
  if (sum(is.na(x)) >= 0.25 * length(x)) { #defined percentage
    return(NA)
  }
  else {
    return(sum(x, na.rm = TRUE))
  }
}

#Reading precipitation information
data_ppt <- read.xlsx("DatosIDW_221205.xlsx", "PTPM_CON")

#Sorting the precipitation data by dates
data_ppt <- as.xts(data_ppt[ , -1], 
                   order.by = as.Date(data_ppt[ , 1],
                                      format = "%Y-%m-%d"))

#Obtaining the annual precipitation of each station
data_y_ppt <- aggregate(data_ppt, by = year(index(data_ppt)), agg) %>% 
  data.frame(check.names = FALSE)

#Creating a variable that combines year and month
year_month_ppt <- format(index(data_ppt), "%Y-%m")

month_ppt <- format(index(data_ppt), "%m")

data_m_ppt <- aggregate(data_ppt, by = list(month_ppt), agg) %>% 
  data.frame(check.names = FALSE)

#Obtaining the monthly precipitation of each year
data_ym_ppt <- aggregate(data_ppt, by = list(year_month_ppt), agg) %>%
  data.frame(check.names = FALSE)

sta_ppt_c <- read.xlsx("sta_ppt_cuenca.xlsx")

v_ppt_c <- sta_ppt_c$CODIGO

data_y_ppt_s <- data_y_ppt[,v_ppt_c]

data_y_ppt_s <- rowMeans(data_y_ppt_s, na.rm = TRUE) %>% as.data.frame()

names(data_y_ppt_s) <- "Rainfall"

data_y_ppt_s$Year <- as.numeric(rownames(data_y_ppt_s))

ppt_pl <- ggplot(data_y_ppt_s, aes(x = Year, y = Rainfall)) +
  # geom_line(color = "#0072B2", linewidth = 1) +
  # geom_smooth(method = "loess", se = FALSE, color = "red", 
  #             linetype = "dashed") +
  geom_line(aes(color = "Precipitation"), linewidth = 1) +
  geom_smooth(aes(color = "Moving average"), method = "loess", se = FALSE, 
              linetype = "dashed") +
  scale_color_manual(values = c("Precipitation" = "#0072B2", 
                                "Moving average" = "#6A3D9A"), name = NULL) +
  theme_bw() +
  theme(text = element_text(family = "CMU Serif", size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.y = element_text(color = "black", size = 11),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = c(0.95, 0.95),
    legend.justification = c(1,1)#,
    #legend.background = element_rect(fill = "transparent", colour = "black")
  ) +
  labs(
    title = "Mean Annual Time Series",
    x = NULL,
    y = "Precipitation (mm)",
    #caption = "Fuente: Datos de precipitación"
  ) #+ 
  #geom_text(x = 0, y = 0, label = "my label", 
  #          size = 6, family = "CMU Serif") 

#Function to be able to discard stations without data - temperature
agg_mt <- function(x) {
  if (sum(is.na(x)) >= 0.25 * length(x)) { #defined percentage
    return(NA)
  }
  else {
    return(mean(x, na.rm = TRUE))
  }
}

#Reading mean temperature information
data_mt <- read.xlsx("DatosIDW_221205.xlsx", "TSSM_D")

#Sorting the temperature data by dates 
data_mt <- as.xts(data_mt[ , -1],
                  order.by = as.Date(data_mt[ , 1],
                                     format = "%Y-%m-%d"))

#Obtaining the annual temperature of each station
data_y_mt <- aggregate(data_mt, by = year(index(data_mt)), agg_mt) %>% 
  data.frame(check.names = FALSE)

month_mt <- format(index(data_mt), "%m")

data_m_mt <- aggregate(data_mt, by = list(month_mt), agg_mt) %>% 
  data.frame(check.names = FALSE)

sta_mt_c <- read.xlsx("sta_mt_cuenca.xlsx")

v_mt_c <- sta_mt_c$CODIGO

data_y_mt_s <- data_y_mt[,v_mt_c]

data_y_m_s <- rowMeans(data_y_mt_s, na.rm = TRUE) %>% as.data.frame()

names(data_y_mt_s) <- "Temperature"

data_y_mt_s$Year <- as.numeric(rownames(data_y_mt_s))

mt_pl <- ggplot(data_y_mt_s, aes(x = Year, y = Temperature)) +
  #geom_line(color = "#D55E00", size = 1) +
   geom_line(aes(color = "Temperature"), linewidth = 1) +
   geom_smooth(aes(color = "Moving average"), method = "loess", se = FALSE, 
               linetype = "dashed") +
   scale_color_manual(values = c("Temperature" = "#D55E00", 
                                 "Moving average" = "#6A3D9A"), name = NULL) +
  theme_bw() +
  theme(text = element_text(family = "CMU Serif", size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "black", size = 11),
        legend.position = c(0.95, 0.50),
        legend.justification = c(1,1)
  ) +
  labs(
    #title = "Serie de tiempo de precipitación anual",
    x = NULL,
    y = "Temperature (°C)",
    #caption = "Fuente: Datos de precipitación"
  ) 

ppt_pl + mt_pl + plot_layout(ncol = 1) 

#-----DAILY TIME SERIES-----
#Precipitation daily time series
data_ppt_df <- as.data.frame(data_ppt)

data_ppt_df <- data_ppt_df[,v_ppt_c]

data_ppt_df <- rowMeans(data_ppt_df, na.rm = TRUE) %>% as.data.frame()

colnames(data_ppt_df) <- "Precipitation"

data_ppt_df$Date <- rownames(data_ppt_df)

data_ppt_df$Date <- as.Date(data_ppt_df$Date)

data_ppt_df <- data_ppt_df[order(data_ppt_df$Date), ]

ppt_d <- ggplot(data_ppt_df, aes(x = Date, y = Precipitation)) +
  geom_line(color = "#0072B2") +
  #theme_bw() +
  theme(text = element_text(family = "CMU Serif", size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 11)) +
  labs(
    title = "Daily Time Series",
    x = NULL,
    y = "Prec (mm)",
    tag = "A"
    #caption = "Fuente: Datos de precipitación"
  ) #+ 
  #geom_text(x = 0, y = 0, label = "my label", 
  #          size = 6, family = "CMU Serif") 

#Mean temperature daily time series
data_mt_df <- as.data.frame(data_mt)

data_mt_df <- data_mt_df[,v_mt_c]

data_mt_df <- rowMeans(data_mt_df, na.rm = TRUE) %>% as.data.frame()

colnames(data_mt_df) <- "Temperature"

data_mt_df$Date <- rownames(data_mt_df)

data_mt_df$Date <- as.Date(data_mt_df$Date)

data_mt_df <- data_mt_df[order(data_mt_df$Date), ]

mt_d <- ggplot(data_mt_df, aes(x = Date, y = Temperature)) +
  geom_line(color = "#D55E00") +
  #theme_bw() +
  theme(text = element_text(family = "CMU Serif", size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 11)) +
  labs(
    #title = "Daily Time Series",
    x = NULL,
    y = "Avg Temp (°C)",
    tag = "B"
    #caption = "Fuente: Datos de precipitación"
  )

ppt_d + mt_d + plot_layout(ncol = 1)

#Maximum temperature daily time series

#Reading maximum temperature information
data_maxt <- read.xlsx("DatosIDW_221205.xlsx", "TMX_CON")

#Sorting the maximum temperature data by dates 
data_maxt <- as.xts(data_maxt[ , -1],
                    order.by = as.Date(data_maxt[ , 1],
                                       format = "%Y-%m-%d"))

month_maxt <- format(index(data_maxt), "%m")

data_m_maxt <- aggregate(data_maxt, by = list(month_maxt), agg_mt) %>% 
  data.frame(check.names = FALSE)

sta_maxt_c <- read.xlsx("sta_maxt_cuenca.xlsx")

v_maxt_c <- sta_maxt_c$CODIGO

data_maxt_df <- as.data.frame(data_maxt)

data_maxt_df <- data_maxt_df[,v_maxt_c]

data_maxt_df <- rowMeans(data_maxt_df, na.rm = TRUE) %>% as.data.frame()

colnames(data_maxt_df) <- "Maximum_Temperature"

data_maxt_df$Date <- rownames(data_maxt_df)

data_maxt_df$Date <- as.Date(data_maxt_df$Date)

data_maxt_df <- data_maxt_df[order(data_maxt_df$Date), ]

maxt_d <- ggplot(data_maxt_df, aes(x = Date, y = Maximum_Temperature)) +
  geom_line(color = "#CC79A7") +
  #theme_bw() +
  theme(text = element_text(family = "CMU Serif", size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 11)) +
  labs(
    #title = "Daily Time Series",
    x = NULL,
    y = "Max Temp (°C)",
    tag = "C"
    #caption = "Fuente: Datos de precipitación"
  )

#Minimum temperature daily time series

#Reading minimum temperature information
data_mint <- read.xlsx("DatosIDW_221205.xlsx", "TMN_CON")

#Sorting the minimum temperature data by dates
data_mint <- as.xts(data_mint[ , -1],
                    order.by = as.Date(data_mint[ , 1],
                                       format = "%Y-%m-%d"))

month_mint <- format(index(data_mint), "%m")

data_m_mint <- aggregate(data_mint, by = list(month_mint), agg_mt) %>% 
  data.frame(check.names = FALSE)


sta_mint_c <- read.xlsx("sta_mint_cuenca.xlsx")

v_mint_c <- sta_mint_c$CODIGO

data_mint_df <- as.data.frame(data_mint)

data_mint_df <- data_mint_df[,v_mint_c]

data_mint_df <- rowMeans(data_mint_df, na.rm = TRUE) %>% as.data.frame()

colnames(data_mint_df) <- "Minimum_Temperature"

data_mint_df$Date <- rownames(data_mint_df)

data_mint_df$Date <- as.Date(data_mint_df$Date)

data_mint_df <- data_mint_df[order(data_mint_df$Date), ]

mint_d <- ggplot(data_mint_df, aes(x = Date, y = Minimum_Temperature)) +
  geom_line(color = "#009E73") +
  #theme_bw() +
  theme(text = element_text(family = "CMU Serif", size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11)) +
  labs(
    #title = "Daily Time Series",
    x = NULL,
    y = "Min Temp (°C)",
    tag = "D"
    #caption = "Fuente: Datos de precipitación"
  )

#----
#Creating a variable that combines year and month
year_month_maxt <- format(index(data_maxt), "%Y-%m")

#Obtaining the maximum temperature for each year
data_ym_maxt <- aggregate(data_maxt, by = list(year_month_maxt), agg_mt) %>% 
  data.frame(check.names = FALSE)

#Reading minimum temperature information
data_mint <- read.xlsx("DatosIDW_221205.xlsx", "TMN_CON")

#Sorting the minimum temperature data by dates
data_mint <- as.xts(data_mint[ , -1],
                    order.by = as.Date(data_mint[ , 1],
                                       format = "%Y-%m-%d"))

#Creating a variable that combines year and month
year_month_mint <- format(index(data_mint), "%Y-%m")

#Obtaining the minimum temperature for each year
data_ym_mint <- aggregate(data_mint, by = list(year_month_mint), agg_mt) %>% 
  data.frame(check.names = FALSE)

#-----MEAN MONTHLY-----

data_ppt_ms <- data_m_ppt[,v_ppt_c]

data_ppt_ms <- rowMeans(data_ppt_ms, na.rm = TRUE) %>% as.data.frame()

names(data_ppt_ms) <- "Precipitation"

data_ppt_ms$Month <- as.numeric(rownames(data_ppt_ms))

ppt_m <- ggplot(data_ppt_ms, aes(x = Month, y = Precipitation)) +
  #geom_line(color = "#0072B2", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "#0072B2", fill = "#0072B2", 
              linewidth = 1) +
  scale_x_continuous(breaks = seq(1, 12, 2)) +
  scale_y_continuous(position = "right") +
  theme(text = element_text(family = "CMU Serif", size = 14),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 11),
        axis.title.y.right = element_text(hjust = 0.5, vjust = 1)) +
  labs(
    title = "Mean Monthly",
    x = NULL,
    y = "(mm/month)",
    tag = "E"
    #caption = "Fuente: Datos de precipitación"
  )


data_mt_ms <- data_m_mt[,v_mt_c]

data_mt_ms <- rowMeans(data_mt_ms, na.rm = TRUE) %>% as.data.frame()

names(data_mt_ms) <- "Temperature"

data_mt_ms$Month <- as.numeric(rownames(data_mt_ms))

mt_m <- ggplot(data_mt_ms, aes(x = Month, y = Temperature)) +
  #geom_line(color = "#0072B2", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "#D55E00", fill = "#D55E00", 
              linewidth = 1) +
  scale_x_continuous(breaks = seq(1, 12, 2)) +
  scale_y_continuous(position = "right") +
  theme(text = element_text(family = "CMU Serif", size = 14),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 11),
        axis.title.y.right = element_text(hjust = 0.5, vjust = 1)) +
  labs(
    #title = "Daily Time Series",
    x = NULL,
    y = "(°C/month)",
    tag = "F"
    #caption = "Fuente: Datos de precipitación"
  )

data_maxt_ms <- data_m_maxt[,v_maxt_c]

data_maxt_ms <- rowMeans(data_maxt_ms, na.rm = TRUE) %>% as.data.frame()

names(data_maxt_ms) <- "Maximum_Temperature"

data_maxt_ms$Month <- as.numeric(rownames(data_maxt_ms))

maxt_m <- ggplot(data_maxt_ms, aes(x = Month, y = Maximum_Temperature)) +
  #geom_line(color = "#0072B2", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "#CC79A7", fill = "#CC79A7", 
              linewidth = 1) +
  scale_x_continuous(breaks = seq(1, 12, 2)) +
  scale_y_continuous(position = "right") +
  theme(text = element_text(family = "CMU Serif", size = 14),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 11),
        axis.title.y.right = element_text(hjust = 0.5, vjust = 1)) +
  labs(
    #title = "Daily Time Series",
    x = NULL,
    y = "(°C/month)",
    tag = "G"
    #caption = "Fuente: Datos de precipitación"
  )

data_mint_ms <- data_m_mint[,v_mint_c]

data_mint_ms <- rowMeans(data_mint_ms, na.rm = TRUE) %>% as.data.frame()

names(data_mint_ms) <- "Minimum_Temperature"

data_mint_ms$Month <- as.numeric(rownames(data_mint_ms))

mint_m <- ggplot(data_mint_ms, aes(x = Month, y = Minimum_Temperature)) +
  #geom_line(color = "#0072B2", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "#009E73", fill = "#009E73", 
              linewidth = 1) +
  scale_x_continuous(breaks = seq(1, 12, 2)) +
  scale_y_continuous(position = "right") +
  theme(text = element_text(family = "CMU Serif", size = 14),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        axis.title.y.right = element_text(hjust = 0.5, vjust = 1)) +
  labs(
    #title = "Daily Time Series",
    x = NULL,
    y = "(°C/month)",
    tag = "H"
    #caption = "Fuente: Datos de precipitación"
  )

pl_ts <- ppt_d + ppt_m + mt_d + mt_m + maxt_d + maxt_m + mint_d + mint_m

pl_ts + plot_layout(widths = c(4,1))

plot <- ggplot() +
  geom_tile(data = raster_obj, aes(x = x, y = y, fill = value)) +
  geom_sf(data = border, fill = NA, color = "black") +
  coord_sf() +
  scale_fill_viridis(option = "viridis", 
                     na.value = "transparent",
                     breaks = c(value_min, value_mid, value_max),
                     labels = function(x) sprintf("%.2f", x),
                     direction = -1) +
  labs(title = title,
       subtitle = paste("Neutral year", yr),
       x = "Longitude",
       y = "Latitude",
       fill = "Recharge (mm/yr)") +
  theme_bw() +
  theme(text = element_text(family = "CMU Serif", size = 14),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.position = "right",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.key = element_rect(colour = "black", linewidth = 0.5),
        legend.title.align = 0.5,
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 14, face = "bold")) +
  geom_text(x = 0, y = 0, label = "my label", 
            size = 6, family = "CMU Serif")

#SPATIAL PLOTS

#GENERAL LOCATION

basin <- st_read("Shapes/CUENCA_120m.shp")

basin <- st_transform(basin, 4686)

#country <- st_read("Shapes/Plots/MGN_DPTO_POLITICO.shp")

#country <- st_transform(country, 4686)

countries <- ne_countries(scale = "medium", returnclass = "sf")

# sel_countries <- countries %>% 
#   filter(admin %in% c("Venezuela", "Brazil", "Ecuador", "Peru",
#                       "Panama"))

sel_countries <- countries %>% 
  filter(admin %in% "Panama")

sel_countries <- st_transform(sel_countries, 4686)

sudamerica <- ne_countries(scale = "medium", continent = "South America", 
                           returnclass = "sf")

sudamerica <- st_transform(sudamerica, 4686)

north_america <- ne_countries(scale = "medium", continent = "North America", 
                              returnclass = "sf")

north_america <- st_transform(north_america, 4686)

# oceanos <- ne_download(type = "ocean", category = "physical", returnclass = "sf")
# 
# oceanos <- st_transform(oceanos, 4686)


# 
# limits <- st_bbox(country)
# xlim <- c(limits["xmin"] - 1, limits["xmax"] + 1)
# ylim <- c(limits["ymin"] - 1, limits["ymax"] + 1)

col_c <- countries %>%
  filter(admin == "Colombia")

colc_c <- st_transform(col_c, 4686)

area_visual <- rbind(sudamerica, sel_countries)

# Calcular los límites de ese objeto
limits <- st_bbox(area_visual)
xlim <- c(limits["xmin"] + 25.2, limits["xmax"] + 2.5)
ylim <- c(limits["ymin"] - 2, limits["ymax"] + 2)

gpl <- ggplot() +
  #geom_sf(data = oceanos, fill = "lightblue") +
  geom_sf(data = sudamerica, fill = "lightgrey") +
  geom_sf(data = north_america, fill = "lightgrey") +
  #geom_sf(data = sel_countries, fill = "lightgrey") +
  #geom_sf(data = country, fill = "lightgrey") + 
  geom_sf(data = col_c, fill = "#F0E442") +
  geom_sf(data = basin, fill = "#0072B2") +
  #geom_sf_label(data = col_c, aes(label = admin), colour = "black") +
  #coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  # ggspatial::annotation_scale(location = "br",
  #                             bar_cols = c("grey60", "white"),
  #                             text_family = "CMU Serif") +
  # ggspatial::annotation_north_arrow(location = "tr", 
  #                                   which_north = "true", 
  #                                   pad_x = unit(0.1, "in"),
  #                                   pad_y = unit(0.1, "in"),
  #                                   style = ggspatial::north_arrow_nautical(
  #                                     fill = c("black", "white"),
  #                                     line_col = "grey20",
  #                                     text_family = "CMU Serif")) +
  #theme_minimal() +
  scale_x_continuous(breaks = seq(from = -80, to = -40, by = 20)) +
  #scale_y_continuous(breaks = seq(from = 10))
  theme(text = element_text(family = "CMU Serif", size = 14),
        #plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(color = "black", face = "bold"),
        panel.background = element_rect(fill = "lightblue"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_blank()) #+
  #labs(title = "Ubicación de mi cuenca hidrográfica") 

#SPECIFIC LOCATION

dd <- st_read("Shapes/Plots/Rios_VMM.shp") %>% st_transform(4686)

#ds_b <- st_read("Shapes/Drenaje_Sencillo_Basin.shp") %>% st_transform(4686)

#bbox_b <- st_bbox(dd_b)

cienaga <- st_read("Shapes/Plots/Cienagas.shp") %>% st_transform(4686)

#rmegia <- st_read("Shapes/Plots/RiosMegia_RP.shp") %>% st_transform(4686)
 
elev_z <- raster("Raster/DEM_LR_120_F.tif")

elev_z <- projectRaster(elev_z, crs = CRS("+init=epsg:4686"))

df_elev <- as.data.frame(rasterToPoints(elev_z)) # convertir a data frame
names(df_elev) <- c("lon", "lat", "value") # cambiar los nombres de las columnas


limits_b <- st_bbox(basin)
xlim_b <- c(limits_b["xmin"] - 0.5, limits_b["xmax"])
ylim_b <- c(limits_b["ymin"], limits_b["ymax"] + 0.2)

spl <- ggplot() +
  #geom_raster(data = df_dem_tot, aes(x = lon, y = lat, fill = value)) +
  geom_raster(data = df_elev, aes(x = lon, y = lat, fill = value)) +
  geom_sf(data = basin, colour = "black", fill = NA) +
  # geom_sf(data = dd, fill = "red", colour = "red") +
  # geom_sf(data = cienaga, fill = "red") +
  geom_sf(data = dd, colour = "#56B4E9", fill = "#56B4E9") +
  geom_sf(data = cienaga, colour = "#996633", fill = "#996633") +
  #geom_sf(data = rmegia, colour = "#996633", fill = "#996633") +
  coord_sf(xlim = xlim_b, ylim = ylim_b, expand = FALSE) +
  #geom_sf(data = ds_b, colour = "#0072B2") +
  #scale_x_continuous(breaks = seq(from = -73.5, to = -72.9, by = 0.2)) +
  scale_fill_viridis(option = "cividis") +
  theme(text = element_text(family = "CMU Serif", size = 14),
        #plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_blank(),
        axis.text = element_text(color = "black", face = "bold"),
        panel.background = element_rect(fill = "lightyellow"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_blank()) +
  labs(fill = "m.a.s.l.") +
  ggspatial::annotation_north_arrow(location = "tr", 
                                    which_north = "true", 
                                    pad_x = unit(0.1, "in"),
                                    pad_y = unit(0.1, "in"),
                                    style = ggspatial::north_arrow_nautical(
                                      fill = c("black", "white"),
                                      line_col = "grey20",
                                      text_family = "CMU Serif"))

gsp_pl <- gpl + spl + plot_layout(widths = c(1,4)) #+ 
#   theme(plot.margin = margin(0, 0, 0, 0), panel.spacing = unit(0, "lines"))

ggsave(filename = "RESULTS/GRAPHS/Plots/gsp_pl.png", 
       plot = gsp_pl, 
       device = "png", 
       path = NULL, 
       scale = 1, 
       width = 8, height = 5, 
       units = "in", 
       dpi = 600)

png(filename = "RESULTS/GRAPHS/Plots/gsp_pl.png", 
    width = 8, height = 5, units = "in", res = 600)

print(gsp_pl)

dev.off()

cpath <- "C:/Users/David Gomez/Desktop/TESIS/DATOS/Shapes/CUENCA_120m.shp"
border <- st_read(cpath) %>% st_transform(4686)

sch_ras <- function(raster_path, title, sc, lg) {
  raster_obj <- rast(raster_path) %>% 
    terra::project("epsg:4686") %>% 
    raster() %>% 
    as("SpatialPixelsDataFrame") %>% 
    as.data.frame()
  
   colnames(raster_obj) <- c("value", "x", "y")
  # value_min <- min(raster_obj$value, na.rm = TRUE)
  # value_max <- max(raster_obj$value, na.rm = TRUE)
  # value_mid <- (value_min + value_max) / 2
  
  plot <- ggplot() +
    geom_tile(data = raster_obj, aes(x = x, y = y, fill = value)) +
    geom_sf(data = border, fill = NA, color = "black") +
    coord_sf() +
    scale_fill_viridis(option = sc, 
                       na.value = "transparent") + #,
                       # breaks = c(value_min, value_mid, value_max),
                       # labels = function(x) sprintf("%.2f", x),
                       # direction = -1) +
    scale_x_continuous(breaks = seq(from = -73.5, to = -72.9, by = 0.2)) +
    labs(title = title,
         # subtitle = paste("Neutral year"),
         x = NULL,
         y = NULL,
         fill = lg) +
    theme_bw() +
    theme(text = element_text(family = "CMU Serif", size = 14),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          # plot.subtitle = element_text(hjust = 0.5, size = 14),
          legend.position = "right",
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12),
          legend.key = element_rect(colour = "black", linewidth = 0.5),
          legend.title.align = 0.5,
          # axis.text.x = element_text(hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.title = element_text(size = 14, face = "bold")) +
    geom_text(x = 0, y = 0, label = "my label", 
              size = 6, family = "CMU Serif")
  
  return(plot)
}

fc_plot <- sch_ras("RESULTS/RAS_SCH/fc_ras.tif", "fc Raster", 
                              "plasma", "mm/day")

kfc_plot <- sch_ras("RESULTS/RAS_SCH/Kfc_ras.tif", "Kfc Raster", 
                              "magma", "value")

kv_plot <- sch_ras("RESULTS/RAS_SCH/Kv_ras.tif", "Kv Raster", 
                               "viridis", "value")

kp_plot <- sch_ras("RESULTS/RAS_SCH/Kp_ras.tif", "Kp Raster", 
                   "turbo", "value")

ci_plot <- sch_ras("RESULTS/RAS_SCH/Ci.tif", "Ci Raster", 
                               "cividis", "value")

cc_plot <- sch_ras("RESULTS/RAS_SCH/CC_ras.tif", "CC Raster", 
                              "mako", "mm")

pm_plot <- sch_ras("RESULTS/RAS_SCH/PM_ras.tif", "PMP Raster", 
                              "rocket", "mm")

cf0_plot <- sch_ras("RESULTS/RAS_SCH/Cf0_ras.tif", "Cf0 Raster", 
                              "viridis", "value")


ppt_avg <- sch_ras("RESULTS/GRAPHS/ppt_avg.tif", "Mean Annual Precipitation",
                   "mako", "mm")
