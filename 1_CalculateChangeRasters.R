#################################
#Welcome to the R spatial world!# 
#################################

#This is the first code you should run after classifying imagery in GEE
#######################################################################
#R is better for figure creation and tabular data manipulation than GEE,
# hence the hassle of moving to a new program
#Spatial Raster (SpratRaster) data is handled via the Terra Package (Hijmans, 2025)
#######################################################################
#This code generates change rasters for all years of your classification 
# by subtracting rasters from one another
#This will give you rasters for bulk changes (start of study to end of study)
# as well as annual changes (from one year to the next)
#This also brings in your topographic data to relate changes to topography
#Then the individual change classes are extracted as well
#######################################################################
#Fair Warning: Some of these functions will take a long time (>30 minutes)
# to run, depending on the size of your catalog. Expect a warm CPU!
#######################################################################
#Things required to work in this code:
    #Classified rasters for your each year in your study period
    #rasters of topograhy (elevation, slope, aspect) for your study area
    #A decent file structure for your output data 
    #A matrix of your classified imagery's landcover classes as numbers

####Data Prep and Gathering####
#Now we can begin. We start with clearing our global environment and prepping libraries
rm(list= ls()) #<- clear out your environment for a clean start 
source("Path\\To\\Libraries.R") #<- set to the R file holding libraries

#Bring in starting data: topography, first classified image, last classified image
firstImage <- rast(
  "path\\to\\your\\first\\classified\\raster\\.tif") #<- the earliest classified image in your study

elevation <- rast(
  "path\\to\\watershed\\elevation\\raster\\.tif") #<- raster of your watershed's elevation

plot(elevation, main = 'elevation') #<- plot your elevation raster to make sure it looks right

slope <- rast("path\\to\\watershed\\slope\\raster\\.tif")   #--------------------------
                                                               ##                      |
plot(slope, main = 'slope')                                    ##                      |
                                                               ##                      |<- same as above
aspect <- rast("path\\to\\watershed\\aspect\\raster\\.tif")    ##                      |
                                                               ##                      |
plot(aspect, main = 'aspect')                               #--------------------------


#This is setting up data to be used in the coming functions 
ImgPath <- 'path\\to\\a\\folder\\of\\all\\of\\your\\classified\\rasters' #<- path to pull your imagery from, leave the path open on the end
firstYearNumber <- '1986' #<- the number of your first image's year
years <- c('1986', '1999', '2000', '2002', '2003', '2006', '2009', '2011',          #<- The years of imagery that you have   
           '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2023', '2024')  #<^
classes <- c('snow', 'rock', 'forest', 'shrub', 'forbs', 'heath')   #<- the names of your landcover classes <- these need to be in the same order 
classNums <- c(1,2,3,4,5,6)   #<- the numeric value assigned to your classes                                <- these need to be in the same order
topoList <- c('elevation', 'slope', 'aspect') #<- list of topographic variables to relate to classification

####Get landcover for each class and year####
#This function will extract each class and relate it to topography then
# converts rasters to dataframes. This is repeated for each image (year) in a folder
createData <- function(path, year, class, classNum){
  file <- file.path(path, paste0(year, '.tif')) #<- find the path to th Geotiff to put in the function
  
  raster <- rast(file) #<- load in the raster to manipulate
  
  raster <- ifel(raster == 0, NA, raster) #<- remove data classified zero* (* change to whatever value is your noData value)
  
  classImg   <- ifel(raster != classNum, NA, raster) #<- extract single landcover class to identify
  
  names(classImg) <- paste0(class, year) #<- set the column name to the year and class you are identifying

  classElevMask <-      mask(elevation, classImg) #<- get the elevation for the class you are identifying
  names(classElevMask) <- paste0(class, year, 'Elev') #<- set column name to elevation identifier
  classSlopMask <-      mask(slope,     classImg)     #
  names(classSlopMask) <- paste0(class, year, 'Slop') #
  classAspeMask <-      mask(aspect,    classImg)     # <- same as above for different topography
  names(classAspeMask) <- paste0(class, year, 'Aspe') #

  classElevDF <-      as.data.frame(classElevMask, xy = T, na.rm = T) #<- change raster to dataframe for easy plotting
  colnames(classElevDF)[colnames(classElevDF) == 'elevation'] <- paste0(class, year, 'Elev') #<- set column names
  classSlopDF <-      as.data.frame(classSlopMask, xy = T, na.rm = T)                        #
  colnames(classSlopDF)[colnames(classSlopDF) == 'elevation'] <- paste0(class, year, 'Slop') #
  classAspeDF <-      as.data.frame(classAspeMask, xy = T, na.rm = T)                        # <- same as above
  colnames(classAspeDF)[colnames(classAspeDF) == 'aspect'] <- paste0(class, year, 'Aspe')    #
  
  tifList <- list(classElevMask, classAspeMask, classSlopMask) #<- list of created tifs for export
  csvList <- list(classElevDF, classAspeDF, classSlopDF) #<- list of dataframe csvs for export
  
  ###write the rasters of class images for easy access, comment out until you're sure you want these saved
  ##writeRaster(classImg, 
  ##            paste0('path\\to\\where\\the\\raster\\will\\go\\', #<- export path to save the landcover class for a year
  ##                   names(classImg), '.tif'), 
  ##            filetype = 'GTiff', 
  ##            overwrite = TRUE)
  ##
  ###Write rasters of topo variables for a specific class
  ##for(i in tifList){
  ##  input <- i
  ##  writeRaster(input, 
  ##              paste0('path\\to\\where\\the\\raster\\will\\go\\', #<- export path for topo related landcover tif
  ##                     names(i), '.tif'), 
  ##              filetype = 'GTiff', 
  ##              overwrite = TRUE)
  ##}
  ##
  ###Write dataframes of topo variables for a specific class
  ##for(i in csvList){
  ##  write.csv(i,
  ##            paste0('path\\to\\where\\the\\csv\\will\\go\\', #<- export path for topo related landcover csv
  ##                   names(i[3]), '.csv'))
  ##}
}

#This is the loop that executes the above function for a folder that you specify, takes a while to run 
for(i in seq_along(years)){
  for(j in 1:length(classes)){
    createData(ImgPath, years[i], classes[j], classNums[j])
  }
}

##this is an optional step to calculate the topography of the entire watershed

#wholeShedElevMask <- mask(elevation, firstImage) #<- get the elevation of the whole watershed by your first image
#names(wholeShedElevMask) <- 'wholeShedElev' #<- set the column name
#wholeShedSlopMask <- mask(slope,     firstImage)
#names(wholeShedSlopMask) <- 'wholeShedSlop'
#wholeShedAspeMask <- mask(aspect,    firstImage)
#names(wholeShedAspeMask) <- 'wholeShedAspe'
#
#wholeShedElevDF <- as.data.frame(wholeShedElevMask, xy = T, na.rm = T) #<- change the raster to a dataframe
#colnames(wholeShedElevDF)[colnames(wholeShedElevDF) == 'elevation'] <- paste0('entireWatershedElev') #<- set column names
#wholeShedSlopDF <- as.data.frame(wholeShedSlopMask, xy = T, na.rm = T)
#colnames(wholeShedSlopDF)[colnames(wholeShedSlopDF) == 'slope'] <- paste0('entireWatershedSlop')
#wholeShedAspeDF <- as.data.frame(wholeShedAspeMask, xy = T, na.rm = T)
#colnames(wholeShedAspeDF)[colnames(wholeShedAspeDF) == 'aspect'] <- paste0('entireWatershedAspe')
##writeRaster(wholeShedElevMask, 
##            'path\\to\\where\\you\\are\\saving\\tifs\\wholeShedElev.tif', 
##            filetype = 'GTiff', 
##            overwrite = TRUE)     #<- these save the watershed topo stats, change the inputs and names to which variable you are saving                                                             #           class 1 | class 2 | class 3 | class 4 | class 5 | class 6 
##write.csv(wholeShedElevDF,                                                                                                                                                                          #              14        27        35        49        51        66 
##          'path\\to\\where\\you\\are\\saving\\csvs\\entireWatershedElev.csv')                                                                                                                       #
                                                                                                                                                                                                      #class 1|14    0        13         21        35        37        52
                                                                                                                                                                                                      #class 2|27   -13       0           8        22        24        39
                                                                                                                                                                                                      #class 3|35   -21       -8          0        14        16        31
####Set up data to calculate change rasters####                                                                                                                                                       #class 4|49   -35      -22         -14        0         2        17
#we need to change the classified landcover's ID number so that we don't get repeat numbers in raster calculation                                                                                     #class 5|51   -37      -24         -16       -2         0        15
reclass <- c(1,14, 2,27, 3,35, 4,49, 5,51, 6,66) #<- this reclassifies your landcover classes to numbers that can be subtracted  -> see example matrix I used for my thesis work                      #class 6|66   -52      -39         -31      -17       -15         0   
rclMatrix <- matrix(reclass, ncol = 2, byrow = TRUE) #<- put the reclassification in matrix form

class6ChangeTypeList <- list( #This monster list is all of the possible landcover conversions and their associated number from the landcover change matrix above
  noChange =        c(0,0,   13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA, 
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  snowToRock =      c(0,NA,  13,13,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  snowToForest =    c(0,NA,  13,NA,  21,21,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  snowToShrub =     c(0,NA,  13,NA,  21,NA,  35,35,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  snowToForbs =     c(0,NA,  13,NA,  21,NA,  35,NA,   37,37,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  snowToHeath =     c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,52,   39,NA, 31,NA,  17,NA,   15,NA),
  rockToSnow =      c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA, -13,-13,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  rockToForest =    c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,   8,8,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  rockToShrub =     c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,22,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  rockToForbs =     c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,24,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  rockToHeath =     c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,39, 31,NA,  17,NA,   15,NA),
  forestToSnow =    c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,-21,-2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  forestToRock =    c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,-8,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  forestToShrub =   c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,14,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  forestToForbs =   c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,16, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  forestToHeath =   c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,31,  17,NA,   15,NA),
  shrubToSnow =     c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,-35, -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  shrubToRock =     c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,-22, -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  shrubToForest =   c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,-14, 2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  shrubToForbs =    c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,   2,2, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  shrubToHeath =    c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,17,   15,NA),
  forbsToSnow =     c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA,-37,-37,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  forbsToRock =     c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA, -24,-24, -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  forbsToForest =   c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,-16, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  forbsToShrub =    c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,-2,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  forbsToHeath =    c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,15),
  heathToSnow =     c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,-52,
                    -39,NA, -31,NA, -17,NA, -15,NA,   52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  heathToRock =     c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,-39, -31,NA, -17,NA, -15,NA,  52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  heathToForest =   c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,-31, -17,NA, -15,NA,  52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  heathToShrub =    c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,-17, -15,NA,  52,NA,   39,NA, 31,NA,  17,NA,   15,NA),
  heathToForbs =    c(0,NA,  13,NA,  21,NA,  35,NA,   37,NA,  -13,NA,  8,NA,  22,NA,   24,NA,  -21,NA, -2,NA,
                     -8,NA,  14,NA,  16,NA, -35,NA,  -22,NA,  -14,NA,  2,NA, -37,NA,  -24,NA,  -16,NA, -52,NA,
                    -39,NA, -31,NA, -17,NA, -15,-15,  52,NA,   39,NA, 31,NA,  17,NA,   15,NA)
) 

#Change Classification Function: This function calculates the change between two rasters. Rasters are grabbed chronologically from a folder
changeClass <- function(path, year, nextYear){
  firstImageFile <- rast(file.path(path, paste0('.tif'))) #<- the first image in your series, used for bulk change analysis
  firstImageFile <- classify(firstImageFile, rclMatrix) #<- reclassify the first image with the matrix we created above
  firstImageFile <- ifel(firstImageFile == 0, NA, firstImageFile) #<- get rid of any NA values in your imagery, in this case a value of 0
  
  file1 <- rast(file.path(path, paste0(year, '.tif'))) #<- first image for annual change calculation
  file2 <- rast(file.path(path, paste0(nextYear, '.tif'))) #<- second image for annual change calculation
  file1 <- ifel(file1 == 0, NA, file1)
  file1 <- classify(file1, rclMatrix) #<- see above, same process but for first and second images in annual change calculation
  file2 <- ifel(file2 == 0, NA, file2) 
  file2 <- classify(file2, rclMatrix)
  
  longChange <- file1 - firstImageFile #<- calculate the bulk change between an image and the start of the study
  longChangeDF <- as.data.frame(longChange, xy = T, na.rm = T) #<- convert bulk change image to a dataframe
  change <- file2 - file1 #<- calculate the annual change between two images
  changeDF <- as.data.frame(change, xy = T, na.rm = T) #<- convert annual change to a dataframe
  colnames(longChangeDF)[colnames(longChangeDF) == 'classification'] <- paste0('changeRaster_', firstYearNumber, '_', year) #<- set column names for bulk changes
  colnames(changeDF)[colnames(changeDF) == 'classification'] <- paste0('changeRaster_', year, '_', nextYear) #<- set column names for annual changes
  
  writeRaster(change, paste0('Path\\where\\you\\want\\landcover\\change\\tifs\\to\\go\\', 
                             'changeRaster_', year, '_', nextYear,  #<- save out the annual change raster with a readable name
                             '.tif'), filetype = 'GTiff', 
              overwrite = TRUE)
  write.csv(changeDF,
            paste0('Path\\where\\you\\want\\landcover\\change\\csvs\\to\\go\\', 
                   names(changeDF[3]), '.csv')) #<- save the annual change raster as a csv 
  
  writeRaster(longChange, paste0('Path\\where\\you\\want\\landcover\\change\\tifs\\to\\go\\', 
                                 'changeRaster_', firstYearNumber, '_', year,  #<- save out the bulk change raster with a readable name
                                 '.tif'), filetype = 'GTiff', 
              overwrite = TRUE)
  write.csv(longChangeDF,
            paste0('Path\\where\\you\\want\\landcover\\change\\csvs\\to\\go\\', 
                   names(longChangeDF[3]), '.csv')) #<- save the bulk change raster as a csv 
  
  #this loop will give you class specific landcover changes based on calculated change raster (ie., how much snow converted to rocks between two years)
  for (i in 1:length(class6ChangeTypeList)) { #<- this a loop to walk the previous function through all of the possible landcover changes
    Classifier <- class6ChangeTypeList[[i]] #<- decide which change we are looking for via the huge list above
    Matrix <- matrix(Classifier, ncol = 2, byrow = TRUE) #<- get the change in matrix format
    
    name <- classify(change, Matrix) #<- get the name of the landcover change for annual changes
    longName <- classify(longChange, Matrix) #<- same as above but for bulk changes
    names(name) <- names(class6ChangeTypeList)[[i]] #<- set column names for csvs
    names(longName) <- names(class6ChangeTypeList)[[i]]
    bandName <- names(class6ChangeTypeList)[[i]] #<- set band names for rasters
    longBandName <- names(class6ChangeTypeList)[[i]]
    
    writeRaster(name, paste0('Path\\where\\you\\want\\landcover\\change\\tifs\\to\\go\\', 
                             bandName, year, '_', nextYear, #<- save the annual change tiff with a readable name  
                             '.tif'), filetype = 'GTiff', 
                overwrite = TRUE)
    classChangeDF <- as.data.frame(name, xy = T, na.rm = T) #<- change the raster to a dataframe
    write.csv(classChangeDF,
              paste0('Path\\where\\you\\want\\landcover\\change\\csvs\\to\\go\\', 
                     names(classChangeDF[3]), '_', year, '_', nextYear, '.csv')) #<- save the annual change csv with a readable name
    
    writeRaster(longName, paste0('Path\\where\\you\\want\\landcover\\change\\tifs\\to\\go\\', 
                                 longBandName, firstYearNumber, '_', year,  #<- same as above but for bulk changes
                                 '.tif'), filetype = 'GTiff', 
                overwrite = TRUE)
    longClassChangeDF <- as.data.frame(longName, xy = T, na.rm = T)
    write.csv(longClassChangeDF,
              paste0('Path\\where\\you\\want\\landcover\\change\\csvs\\to\\go\\', 
                     names(longClassChangeDF[3]), '_', firstYearNumber, '_', year, '.csv'))
  }
}

#for loop to execute the above function for a folder, this takes a long time to run
for(i in seq_along(years)){
  changeClass(l8ImgPath, years[i], years[i+1]) 
}

############################################################################
##You now have changes for every year and every class in your study.
##Next codes will plot your distributions, total area, and changes over time