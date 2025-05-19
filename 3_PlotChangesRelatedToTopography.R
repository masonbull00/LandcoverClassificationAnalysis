###########################################################
#create PDF plots of landcover distirbution and topography#
###########################################################

#this code calculates the stable areas in the watershed, and then plots them along with the changed areas related to a specific
#landcover type related to topography
#You need a change raster
# topography rasters
# csvs of various landcover types and topography
# an idea of what colors you are using for plotting

#start with clearing out environments for a clean start and load libraries
rm(list= ls()) #<- clear out your environment for a clean start 
source("Path\\To\\Libraries.R") #<- set to the R file holding libraries

####Stable Areas####
#bring in the change raster to find where the stable areas are, along with topography for analysis
changeRaster <- rast("path\\to\\your\\change\\raster\\ChangeRaster86_21.tif") #<- the bulk change change raster 
elevation <- rast("path\\to\\your\\topography\\raster\\for\\Elevation.tif")
slope <-     rast("path\\to\\your\\topography\\raster\\for\\Slope.tif")
aspect <-    rast("path\\to\\your\\topography\\raster\\for\\Aspect.tif")


#calculate the stable areas
stable <- ifel(changeRaster != 0, NA, changeRaster) #<- gives you the areas that did not change between the start and end of the study
plot(stable) #<- plot to see how the stable areas look

#write out the stable areas raster for future use
writeRaster(stable, "path\\to\\where\\the\\stable\\regions\\go\\stableRegions.tif")

#bring in each year's classified image to get stable areas in each year
#this gets used later in NDVI calculations, but it is easy to add here
l5class <- rast("path\\to\\classified\\image\\for\\first\\image\\.tif")
l8class <- rast("path\\to\\classified\\image\\for\\last\\image\\.tif")

#These lines are setup for the coming loops
classes <- c('snow', 'rock', 'forest', 'shrub', 'forbs', 'heath') #<- these are the classes we want to analyze
class_values <- 1:6 #<- the number corresponding to each landcover type

inputRast <- c(l5 = l5class, l8 = l8class) #<- rasters going into the loop

outputRast <- list()   # 
outputStable <- list() # 
outputElev <- list()   # <- lists for outputs from the coming loops
outputSlop <- list()   # 
outputAspe <- list()   # 

#create a loop to get each class from each image
for (inputName in names(inputRast)){
  input <- inputRast[[inputName]] #<- get the name of the variable we are looking at
  
  for (i in seq_along(class_values)) {
    value <- class_values[i] 
    name <- classes[i]
    
    isolatedClass <- ifel(input != value, NA, input) #<- isolate the landcover class of interest
    
    outputName <- paste0(inputName, name) #<- name the output
    outputRast[[outputName]] <- isolatedClass #<- add the isolated landcover to the output list
  }
}

#check the loop if you want to see how it performed
##l5Heath <- outputRast[['l5snow']]
##l8Heath <- outputRast[['l8snow']]
##plot(l5Heath)
##plot(l8Heath)

#loop to mask the stable raster by each class found above
for (i in names(outputRast)) {
  input <- outputRast[[i]] #<- grab an image from the outputs above
  mask <- mask(input, stable, inverse = T) #<- mask the output by the stable regions to get a stable landcover type
  outputName <- paste0(i, 'Stable') #<- name the output
  writeRaster(mask, filename = paste0('path\\where\\stable\\regions\\go\\', outputName, '.tif'))
  outputStable[[outputName]] <- mask #<- add the output to a list
}

#check your outputs to see if it makes sense
##l5Stab <- outputStable[['l5snowStable']]
##l8Stab <- outputStable[['l8snowStable']]


#output paths for saving what we just created
tifPath <- "path\\where\\stable\\landcover\\tifs\\should\\go\\"
csvPath <- "path\\where\\stable\\landcover\\csvs\\should\\go\\"

#mask the topo rasters by the stable area rasters to get stable topography
for (i in names(outputStable)){
  input <- outputStable[[i]] #<- raster from the previous outputs                      
  writeRaster(input, filename = paste0(tifPath, i, '.tif'), filetype = 'GTiff', overwrite = T) #<- save the input raster for access later
  elev <- mask(elevation, input) #<- get stable region elevation,
  aspe <- mask(aspect, input) #<- aspect,
  slop <- mask(slope, input) #<- and slope
  elevDF <- as.data.frame(elev) #<- convert to a dataframe
  aspeDF <- as.data.frame(aspe)
  slopDF <- as.data.frame(slop)
  write.csv(elevDF, file = paste0(csvPath, 'elevation/', i, 'Elev.csv')) #<- save the stable topography dataframe as a csv
  write.csv(aspeDF, file = paste0(csvPath, 'aspect/', i, 'Aspect.csv'))
  write.csv(slopDF, file = paste0(csvPath, 'slope/', i, 'Slope.csv'))
}

####plotting####
#this function will plot the two transitions and two stable areas of a given change vector
plottingMultivar <- function(tran1, aes1, title1, #<- transitional landcover, aesthetics for the change, name of the change 
                             tran2, aes2, title2,
                             stab1, aes3, title3, #<- stable landcover, aesthetics for the landcover, name of the landcover
                             stab2, aes4, title4,
                             col1, col2, col3, col4, #<- color list
                             plotTitle, xlabel, xlimit, ylimit){ #<- plot aesthetics
  ggplot() + 
    geom_density(data = tran1, aes(x = aes1, color = paste0(title1, '(', round((length(aes1)*0.0009), 2), 'km\u00b2)')), linetype = 'dashed', key_glyph = draw_key_path) +
    geom_density(data = tran2, aes(x = aes2, color = paste0(title2, '(', round((length(aes2)*0.0009), 2), 'km\u00b2)')), linetype = 'dotted', key_glyph = draw_key_path) +
    geom_density(data = stab1, aes(x = aes3, color = paste0(title3, '(', round((length(aes3)*0.0009), 2), 'km\u00b2)')), key_glyph = draw_key_path) +
    geom_density(data = stab2, aes(x = aes4, color = paste0(title4, '(', round((length(aes4)*0.0009), 2), 'km\u00b2)')), key_glyph = draw_key_path) + 
    scale_color_manual(name="", values = c(col1, col2, col3, col4)) +
    scale_fill_manual(name="",  values = c(col1, col2, col3, col4)) + 
    theme_cust() + xlab(xlabel) + ylab('Occurrence Density') +
    scale_x_continuous(limits = c(0,xlimit), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, ylimit) ,expand = c(0,0)) +
    theme(legend.position = c(.83,.8)) +
    labs(title = plotTitle)
}

#These lines bring in data for plotting the stable and change plot overlay
#we start with the stable landcover elevation of each class we want to observe
l5StabHeathElev <-    read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5HeathStableElev.csv")
l5StabRockElev <-      read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5RockStableElev.csv")
l5StabSnowElev <-      read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5SnowStableElev.csv")
l5StabForbsElev <-    read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5ForbsStableElev.csv")
l5StabShrubElev <-    read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5ShrubStableElev.csv")
l5StabForestElev <-  read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5ForestStableElev.csv")

#Then aspect
 l5StabHeathAspe <-    read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5HeathStableAspe.csv")
  l5StabRockAspe <-     read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5RockStableAspe.csv")
  l5StabSnowAspe <-     read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5SnowStableAspe.csv")
 l5StabForbsAspe <-    read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5ForbsStableAspe.csv")
 l5StabShrubAspe <-    read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5ShrubStableAspe.csv")
l5StabForestAspe <-   read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5ForestStableAspe.csv")

#and slope
 l5StabHeathSlop <-     read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5HeathStableSlop.csv")
  l5StabRockSlop <-      read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5RockStableSlop.csv")
  l5StabSnowSlop <-      read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5SnowStableSlop.csv")
 l5StabForbsSlop <-     read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5ForbsStableSlop.csv")
 l5StabShrubSlop <-     read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5ShrubStableSlop.csv")
l5StabForestSlop <-    read.csv("path\\to\\your\\stable\\landcover\\csvs\\l5ForestStableSlop.csv")

#Now we bring in the landcover changes that we want to observe, again by topography
#start with elevation
  heathToRockElev <-   read.csv("path\\to\\your\\changes\\as\\csvs\\heathToRockElevationDF.csv")
  rockToHeathElev <-   read.csv("path\\to\\your\\changes\\as\\csvs\\rockToHeathElevationDF.csv")
 forbsToShrubElev <-  read.csv("path\\to\\your\\changes\\as\\csvs\\forbsToShrubElevationDF.csv")
 shrubToForbsElev <-  read.csv("path\\to\\your\\changes\\as\\csvs\\shrubToForbsElevationDF.csv")
 heathToForbsElev <-  read.csv("path\\to\\your\\changes\\as\\csvs\\heathToForbsElevationDF.csv")
 forbsToHeathElev <-  read.csv("path\\to\\your\\changes\\as\\csvs\\forbsToHeathElevationDF.csv")
shrubToForestElev <- read.csv("path\\to\\your\\changes\\as\\csvs\\shrubToForestElevationDF.csv")
forestToShrubElev <- read.csv("path\\to\\your\\changes\\as\\csvs\\forestToShrubElevationDF.csv")
  snowToHeathElev <-   read.csv("path\\to\\your\\changes\\as\\csvs\\snowToHeathElevationDF.csv")
  heathToSnowElev <-   read.csv("path\\to\\your\\changes\\as\\csvs\\heathToSnowElevationDF.csv")
   rockToSnowElev <-    read.csv("path\\to\\your\\changes\\as\\csvs\\rockToSnowElevationDF.csv")
   snowToRockElev <-    read.csv("path\\to\\your\\changes\\as\\csvs\\snowToRockElevationDF.csv")

#then aspect
  heathToRockAspe <-   read.csv("path\\to\\your\\changes\\as\\csvs\\heathToRockAspectDF.csv")
  rockToHeathAspe <-   read.csv("path\\to\\your\\changes\\as\\csvs\\rockToHeathAspectDF.csv")
 forbsToShrubAspe <-  read.csv("path\\to\\your\\changes\\as\\csvs\\forbsToShrubAspectDF.csv")
 shrubToForbsAspe <-  read.csv("path\\to\\your\\changes\\as\\csvs\\shrubToForbsAspectDF.csv")
 heathToForbsAspe <-  read.csv("path\\to\\your\\changes\\as\\csvs\\heathToForbsAspectDF.csv")
 forbsToHeathAspe <-  read.csv("path\\to\\your\\changes\\as\\csvs\\forbsToHeathAspectDF.csv")
shrubToForestAspe <- read.csv("path\\to\\your\\changes\\as\\csvs\\shrubToForestAspectDF.csv")
forestToShrubAspe <- read.csv("path\\to\\your\\changes\\as\\csvs\\forestToShrubAspectDF.csv")
  snowToHeathAspe <-   read.csv("path\\to\\your\\changes\\as\\csvs\\snowToHeathAspectDF.csv")
  heathToSnowAspe <-   read.csv("path\\to\\your\\changes\\as\\csvs\\heathToSnowAspectDF.csv")
   rockToSnowAspe <-    read.csv("path\\to\\your\\changes\\as\\csvs\\rockToSnowAspectDF.csv")
   snowToRockAspe <-    read.csv("path\\to\\your\\changes\\as\\csvs\\snowToRockAspectDF.csv")
   
#and then slope
  heathToRockSlop <-   read.csv("path\\to\\your\\changes\\as\\csvs\\heathToRockSlopeDF.csv")
  rockToHeathSlop <-   read.csv("path\\to\\your\\changes\\as\\csvs\\rockToHeathSlopeDF.csv")
 forbsToShrubSlop <-  read.csv("path\\to\\your\\changes\\as\\csvs\\forbsToShrubSlopeDF.csv")
 shrubToForbsSlop <-  read.csv("path\\to\\your\\changes\\as\\csvs\\shrubToForbsSlopeDF.csv")
 heathToForbsSlop <-  read.csv("path\\to\\your\\changes\\as\\csvs\\heathToForbsSlopeDF.csv")
 forbsToHeathSlop <-  read.csv("path\\to\\your\\changes\\as\\csvs\\forbsToHeathSlopeDF.csv")
shrubToForestSlop <- read.csv("path\\to\\your\\changes\\as\\csvs\\shrubToForestSlopeDF.csv")
forestToShrubSlop <- read.csv("path\\to\\your\\changes\\as\\csvs\\forestToShrubSlopeDF.csv")
  snowToHeathSlop <-   read.csv("path\\to\\your\\changes\\as\\csvs\\snowToHeathSlopeDF.csv")
  heathToSnowSlop <-   read.csv("path\\to\\your\\changes\\as\\csvs\\heathToSnowSlopeDF.csv")
   rockToSnowSlop <-    read.csv("path\\to\\your\\changes\\as\\csvs\\rockToSnowSlopeDF.csv")
   snowToRockSlop <-    read.csv("path\\to\\your\\changes\\as\\csvs\\snowToRockSlopeDF.csv")
   
#This is the path where the plots will save to
plotPath <- 'C:\\Users\\mason\\OneDrive\\Desktop\\BSU\\Research\\RSpatial\\NJlandcoverClasses\\graphs'


#and now we can plot them and see how things look
rockAndHeathElev <- #<- comment out this line on each plot to view in the plot pane
  plottingMultivar(heathToRockElev, heathToRockElev$heathToRockElevation, 'Heath to Rock ',
                   rockToHeathElev, rockToHeathElev$rockToHeathElevation, 'Rock to Heath ',
                   l5StabHeathElev, l5StabHeathElev$elevation, 'Stable Heath ',
                   l5StabRockElev,  l5StabRockElev$elevation, 'Stable Rock ',
                   '#8b7575', '#a56f6f', '#e17272', '#a4a09d', 'Rock and Heath', 'Elevation (m)', 1800, 0.005)
rockAndHeathAspe <- 
  plottingMultivar(heathToRockAspe, heathToRockAspe$heathToRockAspect, 'Heath to Rock ',
                   rockToHeathAspe, rockToHeathAspe$rockToHeathAspect, 'Rock to Heath ',
                   l5StabHeathAspe, l5StabHeathAspe$aspect, 'Stable Heath ',
                   l5StabRockAspe,  l5StabRockAspe$aspect, 'Stable Rock ',
                   '#8b7575', '#a56f6f', '#e17272', '#a4a09d', 'Rock and Heath', 'Aspect (Degrees)', 360, 0.013)
rockAndHeathSlop <- 
  plottingMultivar(heathToRockSlop, heathToRockSlop$heathToRockSlope, 'Heath to Rock ',
                   rockToHeathSlop, rockToHeathSlop$rockToHeathSlope, 'Rock to Heath ',
                   l5StabHeathSlop, l5StabHeathSlop$slope, 'Stable Heath ',
                   l5StabRockSlop,  l5StabRockSlop$slope, 'Stable Rock ',
                   '#8b7575', '#a56f6f', '#e17272', '#a4a09d', 'Rock and Heath', 'Slope (Degrees)', 80, 0.08)
shrubAndForbsElev <- 
  plottingMultivar(shrubToForbsElev, shrubToForbsElev$shrubToForbsElevation, 'Shrub to Forbs ',
                   forbsToShrubElev, forbsToShrubElev$forbsToShrubElevation, 'Forbs to Shrub ',
                   l5StabShrubElev,  l5StabShrubElev$elevation, 'Stable Shrub ',
                   l5StabForbsElev,  l5StabForbsElev$elevation, 'Stable Forbs ',
                   '#a8bbac', '#42a4a5', '#ffea5f','#89e7ff', 'Shrub and Forbs', 'Elevation (m)', 1800, 0.005)
shrubAndForbsAspe <- 
  plottingMultivar(shrubToForbsAspe, shrubToForbsAspe$shrubToForbsAspect, 'Shrub to Forbs ',
                   forbsToShrubAspe, forbsToShrubAspe$forbsToShrubAspect, 'Forbs to Shrub ',
                   l5StabShrubAspe,  l5StabShrubAspe$aspect, 'Stable Shrub ',
                   l5StabForbsAspe,  l5StabForbsAspe$aspect, 'Stable Forbs ',
                   '#a8bbac', '#42a4a5', '#ffea5f','#89e7ff', 'Shrub and Forbs', 'Aspect (Degrees)', 360, 0.013)
shrubAndForbsSlop <- 
  plottingMultivar(shrubToForbsSlop, shrubToForbsSlop$shrubToForbsSlope, 'Shrub to Forbs ',
                   forbsToShrubSlop, forbsToShrubSlop$forbsToShrubSlope, 'Forbs to Shrub ',
                   l5StabShrubSlop,  l5StabShrubSlop$slope, 'Stable Shrub ',
                   l5StabForbsSlop,  l5StabForbsSlop$slope, 'Stable Forbs ',
                   '#a8bbac', '#42a4a5', '#ffea5f','#89e7ff', 'Shrub and Forbs', 'Slope (Degrees)', 80, 0.08)
heathAndForbsElev <- 
  plottingMultivar(heathToForbsElev, heathToForbsElev$heathToForbsElevation, 'Heath to Forbs ',
                   forbsToHeathElev, forbsToHeathElev$forbsToHeathElevation, 'Forbs to Heath ',
                   l5StabHeathElev,  l5StabHeathElev$elevation, 'Stable Heath ',
                   l5StabForbsElev,  l5StabForbsElev$elevation, 'Stable Forbs ',
                   '#ef832e', '#a44f10', '#ffea5f', '#e17272', 'Heath and Forbs', 'Elevation (m)', 1800, 0.005)
heathAndForbsAspe <- 
  plottingMultivar(heathToForbsAspe, heathToForbsAspe$heathToForbsAspect, 'Heath to Forbs ',
                   forbsToHeathAspe, forbsToHeathAspe$forbsToHeathAspect, 'Forbs to Heath ',
                   l5StabHeathAspe,  l5StabHeathAspe$aspect, 'Stable Heath ',
                   l5StabForbsAspe,  l5StabForbsAspe$aspect, 'Stable Forbs ',
                   '#ef832e', '#a44f10', '#ffea5f', '#e17272', 'Heath and Forbs', 'Aspect (Degrees)', 360, 0.013)
heathAndForbsSlop <- 
  plottingMultivar(heathToForbsSlop, heathToForbsSlop$heathToForbsSlope, 'Heath to Forbs ',
                   forbsToHeathSlop, forbsToHeathSlop$forbsToHeathSlope, 'Forbs to Heath ',
                   l5StabHeathSlop,  l5StabHeathSlop$slope, 'Stable Heath ',
                   l5StabForbsSlop,  l5StabForbsSlop$slope, 'Stable Forbs ',
                   '#ef832e', '#a44f10', '#ffea5f', '#e17272', 'Heath and Forbs', 'Slope (Degrees)', 80, 0.08)
shrubAndForestElev <-
  plottingMultivar(shrubToForestElev, shrubToForestElev$shrubToForestElevation, 'Shrub to Forest ',
                   forestToShrubElev, forestToShrubElev$forestToShrubElevation, 'Forest to Shrub ',
                   l5StabForestElev,  l5StabForestElev$elevation, 'Stable Forest ',
                   l5StabShrubElev,   l5StabShrubElev$elevation, 'Stable Shrub ',
                   '#326176', '#2e9f98', '#6e9e52', '#89e7ff', 'Shrub and Forest', 'Elevation (m)', 1800, 0.005)
shrubAndForestAspe <-
  plottingMultivar(shrubToForestAspe, shrubToForestAspe$shrubToForestAspect, 'Shrub to Forest ',
                   forestToShrubAspe, forestToShrubAspe$forestToShrubAspect, 'Forest to Shrub ',
                   l5StabForestAspe,  l5StabForestAspe$aspect, 'Stable Forest ',
                   l5StabShrubAspe,   l5StabShrubAspe$aspect, 'Stable Shrub ',
                   '#326176', '#2e9f98', '#6e9e52', '#89e7ff', 'Shrub and Forest', 'Aspect (Degrees)', 360, 0.013)
shrubAndForestSlop <-
  plottingMultivar(shrubToForestSlop, shrubToForestSlop$shrubToForestSlope, 'Shrub to Forest ',
                   forestToShrubSlop, forestToShrubSlop$forestToShrubSlope, 'Forest to Shrub ',
                   l5StabForestSlop,  l5StabForestSlop$slope, 'Stable Forest ',
                   l5StabShrubSlop,   l5StabShrubSlop$slope, 'Stable Shrub ',
                   '#326176', '#2e9f98', '#6e9e52', '#89e7ff', 'Shrub and Forest', 'Slope (Degrees)', 80, 0.08)
heathAndSnowElev <-
  plottingMultivar(heathToSnowElev, heathToSnowElev$heathToSnowElevation, 'Heath to Snow ',
                   snowToHeathElev, snowToHeathElev$snowToHeathElevation, 'Snow to Heath ',
                   l5StabHeathElev, l5StabHeathElev$elevation, 'Stable Heath ',
                   l5StabSnowElev,  l5StabSnowElev$elevation, 'Stable Snow ',
                   '#e1e1e1', '#e17252', '#e17272', 'gray', 'Heath and Snow', 'Elevation (m)', 1800, 0.005)
heathAndSnowAspe <-
  plottingMultivar(heathToSnowAspe, heathToSnowAspe$heathToSnowAspect, 'Heath to Snow ',
                   snowToHeathAspe, snowToHeathAspe$snowToHeathAspect, 'Snow to Heath ',
                   l5StabHeathAspe, l5StabHeathAspe$aspect, 'Stable Heath ',
                   l5StabSnowAspe,  l5StabSnowAspe$aspect, 'Stable Snow ',
                   '#e1e1e1', '#e17252', '#e17272', 'gray', 'Heath and Snow', 'Aspect (Degrees)', 360, 0.013)
heathAndSnowSlop <-
  plottingMultivar(heathToSnowSlop, heathToSnowSlop$heathToSnowSlope, 'Heath to Snow ',
                   snowToHeathSlop, snowToHeathSlop$snowToHeathSlope, 'Snow to Heath ',
                   l5StabHeathSlop, l5StabHeathSlop$slope, 'Stable Heath ',
                   l5StabSnowSlop,  l5StabSnowSlop$slope, 'Stable Snow ',
                   '#e1e1e1', '#e17252', '#e17272', 'gray', 'Heath and Snow', 'Slope (Degrees)', 80, 0.08)
rockAndSnowElev <-
  plottingMultivar(rockToSnowElev, rockToSnowElev$rockToSnowElevation, 'Rock to Snow ',
                   snowToRockElev, snowToRockElev$snowToRockElevation, 'Snow to Rock ',
                   l5StabRockElev, l5StabRockElev$elevation, 'Stable Rock ',
                   l5StabSnowElev, l5StabSnowElev$elevation, 'Stable Snow ',
                   '#a0a4ff', 'darkgrey', '#b4bddb', '#dbb4b4', 'Rock and Snow', 'Elevation (m)', 1800, 0.005)
rockAndSnowAspe <-
  plottingMultivar(rockToSnowAspe, rockToSnowAspe$rockToSnowAspect, 'Rock to Snow ',
                   snowToRockAspe, snowToRockAspe$snowToRockAspect, 'Snow to Rock ',
                   l5StabRockAspe, l5StabRockAspe$aspect, 'Stable Rock ',
                   l5StabSnowAspe, l5StabSnowAspe$aspect, 'Stable Snow ',
                   '#a0a4ff', 'darkgrey', '#b4bddb', '#dbb4b4', 'Rock and Snow', 'Aspect (Degrees)', 360, 0.013)
rockAndSnowSlop <-
  plottingMultivar(rockToSnowSlop, rockToSnowSlop$rockToSnowSlope, 'Rock to Snow ',
                   snowToRockSlop, snowToRockSlop$snowToRockSlope, 'Snow to Rock ',
                   l5StabRockSlop, l5StabRockSlop$slope, 'Stable Rock ',
                   l5StabSnowSlop, l5StabSnowSlop$slope, 'Stable Snow ',
                   '#a0a4ff', 'darkgrey', '#b4bddb', '#dbb4b4', 'Rock and Snow', 'Slope (Degrees)', 80, 0.08)

#save out the plots
ggsave(plot =   rockAndHeathElev, path = plotPath, filename =   'rockAndHeathElevOverlay.pdf', device = 'pdf')
ggsave(plot =  shrubAndForbsElev, path = plotPath, filename =  'shrubAndForbsElevOverlay.pdf', device = 'pdf')
ggsave(plot =  heathAndForbsElev, path = plotPath, filename =  'heathAndForbsElevOverlay.pdf', device = 'pdf')
ggsave(plot = shrubAndForestElev, path = plotPath, filename = 'shrubAndForestElevOverlay.pdf', device = 'pdf')
ggsave(plot =   heathAndSnowElev, path = plotPath, filename =   'heathAndSnowElevOverlay.pdf', device = 'pdf')
ggsave(plot =    rockAndSnowElev, path = plotPath, filename =    'rockAndSnowElevOverlay.pdf', device = 'pdf')
ggsave(plot =   rockAndHeathAspe, path = plotPath, filename =   'rockAndHeathAspeOverlay.pdf', device = 'pdf')
ggsave(plot =  shrubAndForbsAspe, path = plotPath, filename =  'shrubAndForbsAspeOverlay.pdf', device = 'pdf')
ggsave(plot =  heathAndForbsAspe, path = plotPath, filename =  'heathAndForbsAspeOverlay.pdf', device = 'pdf')
ggsave(plot = shrubAndForestAspe, path = plotPath, filename = 'shrubAndForestAspeOverlay.pdf', device = 'pdf')
ggsave(plot =   heathAndSnowAspe, path = plotPath, filename =   'heathAndSnowAspeOverlay.pdf', device = 'pdf')
ggsave(plot =    rockAndSnowAspe, path = plotPath, filename =    'rockAndSnowAspeOverlay.pdf', device = 'pdf')
ggsave(plot =   rockAndHeathSlop, path = plotPath, filename =   'rockAndHeathSlopOverlay.pdf', device = 'pdf')
ggsave(plot =  shrubAndForbsSlop, path = plotPath, filename =  'shrubAndForbsSlopOverlay.pdf', device = 'pdf')
ggsave(plot =  heathAndForbsSlop, path = plotPath, filename =  'heathAndForbsSlopOverlay.pdf', device = 'pdf')
ggsave(plot = shrubAndForestSlop, path = plotPath, filename = 'shrubAndForestSlopOverlay.pdf', device = 'pdf')
ggsave(plot =   heathAndSnowSlop, path = plotPath, filename =   'heathAndSnowSlopOverlay.pdf', device = 'pdf')
ggsave(plot =    rockAndSnowSlop, path = plotPath, filename =    'rockAndSnowSlopOverlay.pdf', device = 'pdf')





##This is an optional section for plotting your aspect on a rose diagram instead of as lines in a pdf##

#polar coordinates
aspectDF$cut <- cut(aspectDF$aspect, seq(0,360, by = 20), ordered_result = T, labels = F)
aspectDF$cut <- aspectDF$cut * 20
aspectDF$category <- "Entire Watershed"
#trying with the top five changes
snowToRockAspe$cut <- cut(snowToRockAspe$snowToRockAspect, seq(0,360, by = 20), ordered_result = T, labels = F)
snowToRockAspe$cut <- snowToRockAspe$cut * 20
snowToHeathAspe$cut <- cut(snowToHeathAspe$snowToHeathAspect, seq(0,360, by = 20), ordered_result = T, labels = F)
snowToHeathAspe$cut <- snowToHeathAspe$cut * 20
rockToHeathAspe$cut <- cut(rockToHeathAspe$rockToHeathAspect, seq(0,360, by = 20), ordered_result = T, labels = F)
rockToHeathAspe$cut <- rockToHeathAspe$cut * 20
heathToForbsAspe$cut <- cut(heathToForbsAspe$heathToForbsAspect, seq(0,360, by = 20), ordered_result = T, labels = F)
heathToForbsAspe$cut <- heathToForbsAspe$cut * 20
forbsToShrubAspe$cut <- cut(forbsToShrubAspe$forbsToShrubAspect, seq(0,360, by = 20), ordered_result = T, labels = F)
forbsToShrubAspe$cut <- forbsToShrubAspe$cut * 20

snowToRockAspe$category <- "Snow To Rock"
snowToHeathAspe$category <- "Snow to Heath"
rockToHeathAspe$category <- "Rock to Heath"
heathToForbsAspe$category <- "Heath to Forbs"
forbsToShrubAspe$category <- "Forbs to Shrub"

combined_data <- bind_rows(snowToRockAspe, snowToHeathAspe, rockToHeathAspe, heathToForbsAspe, forbsToShrubAspe)

# Plot using geom_bar with fill by category
ggplot(combined_data, aes(x = cut, fill = category)) + 
  geom_bar(position = "dodge") + scale_x_binned(limits = c(0,360), expand = c(0,0), n.breaks = 18) +
  coord_polar(start = 0) + 
  theme_minimal() +
  scale_fill_manual(values = c("#97b510", "#e09c79", "#d15502", "#9c9575", "#9d4c0f")) +
  labs(title = "Hillslope Aspect Distribution by Category")
ggplot(combined_data, aes(x = cut, fill = category)) + 
  geom_bar(position = "dodge") + scale_x_binned(limits = c(0,360), expand = c(0,0), n.breaks = 18) +
  scale_y_continuous(limits = c(0,2500)) +
  coord_polar(start = 0) + 
  facet_wrap(~category) + 
  theme_minimal() +
  scale_fill_manual(values = c("#a8bbac", "#a44f10", "#a56f6f", "#e17252", "darkgrey")) +
  labs(title = "Hillslope Aspect Distribution by Category")

#trying with the inverse of the top five changes
rockToSnowAspe$cut <- cut(rockToSnowAspe$rockToSnowAspect, seq(0,360, by = 20), ordered_result = T, labels = F)
rockToSnowAspe$cut <- rockToSnowAspe$cut * 20
heathToSnowAspe$cut <- cut(heathToSnowAspe$heathToSnowAspect, seq(0,360, by = 20), ordered_result = T, labels = F)
heathToSnowAspe$cut <- heathToSnowAspe$cut * 20
heathToRockAspe$cut <- cut(heathToRockAspe$heathToRockAspect, seq(0,360, by = 20), ordered_result = T, labels = F)
heathToRockAspe$cut <- heathToRockAspe$cut * 20
forbsToHeathAspe$cut <- cut(forbsToHeathAspe$forbsToHeathAspect, seq(0,360, by = 20), ordered_result = T, labels = F)
forbsToHeathAspe$cut <- forbsToHeathAspe$cut * 20
shrubToForbsAspe$cut <- cut(shrubToForbsAspe$shrubToForbsAspect, seq(0,360, by = 20), ordered_result = T, labels = F)
shrubToForbsAspe$cut <- shrubToForbsAspe$cut * 20

rockToSnowAspe$category <- "Rock to Snow"
heathToSnowAspe$category <- "Heath to Snow"
heathToRockAspe$category <- "Heath to Rock"
forbsToHeathAspe$category <- "Forbs to Heath"
shrubToForbsAspe$category <- "Shrub to Forbs"

combined_data_inverse <- bind_rows(forbsToHeathAspe, heathToRockAspe, heathToSnowAspe, rockToSnowAspe, shrubToForbsAspe)

#p <- ggplot(combined_data_inverse, aes(x = cut, fill = category)) + 
p + geom_bar(data = combined_data_inverse, aes(x = cut, fill = category), position = "dodge") +
  scale_x_binned(limits = c(0,360), expand = c(0,0), n.breaks = 18) +
  scale_y_continuous(limits = c(0,2500)) +
  coord_polar(start = 0) + 
  facet_wrap(~category) + 
  theme_minimal() +
  scale_fill_manual(values = c('black', "#a8bbac", "#a44f10", "#a56f6f", "#e17252", "darkgrey")) +
  labs(title = "Hillslope Aspect Distribution by Category (inverse)")

p <- ggplot(aspectDF, aes(x = cut, fill = category)) +
  geom_bar(position = 'dodge') + scale_x_binned(limits = c(0,360), expand = c(0,0), n.breaks = 18) +
  scale_y_continuous(labels = function(x) x/15) +#, limits = c(0,2500)) +
  coord_polar()# + theme_minimal() +
scale_fill_manual(values = 'black')

ggplot(data = aspect) + 
  geom_density(aes(x = aspect, color = paste0('aspect', '(', round((length(aspect)*0.0009), 2), 'km\u00b2)')), linetype = 'dashed', key_glyph = draw_key_path) + 
  theme_cust() + xlab('aspect') + ylab('Occurrence Density') +
  scale_x_continuous(limits = c(0,360), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 0.013) ,expand = c(0,0)) +
  theme(legend.position = c(.83,.8)) +
  labs(title = 'Entire Nellie Juan Aspect')

mask(elevation, aspect) %>% ggplot() + 
  geom_density(aes(x = elevation, color = paste0('aspect', '(', round((length(elevation)*0.0009), 2), 'km\u00b2)')), linetype = 'dashed', key_glyph = draw_key_path) + 
  theme_cust() + xlab('elevation') + ylab('Occurrence Density') +
  scale_x_continuous(limits = c(0,1800), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 0.005) ,expand = c(0,0)) +
  theme(legend.position = c(.83,.8)) +
  labs(title = 'Entire Nellie Juan Elevation')

ggplot(data = slope) + 
  geom_density(aes(x = slope, color = paste0('slope', '(', round((length(slope)*0.0009), 2), 'km\u00b2)')), linetype = 'dashed', key_glyph = draw_key_path) + 
  theme_cust() + xlab('slope') + ylab('Occurrence Density') +
  scale_x_continuous(limits = c(0,80), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 0.080) ,expand = c(0,0)) +
  theme(legend.position = c(.83,.8)) +
  labs(title = 'Entire Nellie Juan Slope')
