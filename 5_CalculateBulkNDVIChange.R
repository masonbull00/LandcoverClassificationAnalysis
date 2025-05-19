###################################################
##Bulk Change NDVI Comparisons by landcover class##
###################################################

#This code will create density plots of NDVI for the beginning and end of the study period for each class
#Then determine if your NDVI is significantly differnet via a t-test

#you will need:
## NDVI of the beginning and end of the study period for the entire watershed
## a change map for the same time period
## classified maps from the start and end of the study


#start with clearing out environments for a clean start and load libraries
rm(list= ls()) #<- clear out your environment for a clean start 
source("Path\\To\\Libraries.R") #<- set to the R file holding libraries


####DATA GATHERING####
#bring in the necessary rasters, one for each year being analyzed
NDVIstart <- rast("path\\to\\your\\image\\of\\NDVI\\for\\the\\start\\of\\study\\.tif") #<- start of study

NDVIend <- rast("path\\to\\your\\image\\of\\NDVI\\for\\the\\end\\of\\study\\.tif") #<- end of study

change <- rast("path\\to\\your\\change\\raster\\form\\start\\to\\end\\.tif") #<- the change raster to get data from later

#optionally bring in the watershed outline for plotting
shp <- vect("path\\to\\watershed\\shapefile\\.shp") #<- shapefile of the watershed outline for plotting
shp <- project(shp, NDVIend) #<- set projection if necessary
plot(NDVIend) #<- plot NDVI:
plot(shp, add = T) #<- then add the shapefile


#bring in each year's classified image to get NDVI of each class
startClass <- rast("path\\to\\first\\classified\\image\\.tif")
endClass <- rast("path\\to\\last\\classified\\image\\.tif")

#get the snow for a mask because we don't care about NDVI of snow
#You can skip this step and not apply snow masks later if your watershed does not have significant snow (or other low NDVI class) coverage
startSnow <- ifel(startClass != 1, NA, startClass) #<- get snow from first image
plot(startSnow) #<- plot to see if it worked
endSnow <- ifel(endClass != 1, NA, endClass)
plot(endSnow)

#find the stable regions in the watershed
stable <- ifel(change != 0, NA, change) #<- create a raster that is unchanged areas between start and end of study

#mask NDVI imagery to get rid of unwanted landcover
NDVIstartNoSnow<- mask(NDVIstart, startSnow, inverse = T) #<- mask out snow for the whole image
NDVIendNoSnow <- mask(NDVIend, endSnow, inverse = T)
NDVIstartNoSnowStable <- mask(NDVIstartNoSnow, stable) #<- mask out snow for only stable regions
NDVIendNoSnowStable <- mask(NDVIendNoSnow, stable)

plot(NDVIstartNoSnow, main = 'Watershed Start, No Snow') #<- plot to see if your masks worked
plot(NDVIendNoSnow, main = 'Watershed End, No Snow')
plot(NDVIstartNoSnowStable, main = 'Watershed Start, Stable Regions, No Snow')
plot(NDVIendNoSnowStable, main = 'Watershed End, Stable Regions, No Snow')

NDVIstartNoSnowDF <- as.data.frame(NDVIstartNoSnow, na.rm = T)#             |
NDVIendNoSnowDF <- as.data.frame(NDVIendNoSnow, na.rm = T)#                 |
NDVIstartNoSnowStableDF <- as.data.frame(NDVIstartNoSnowStable, na.rm = T)# |<- convert the above rasters to dataframes for later
NDVIendNoSnowStableDF <- as.data.frame(NDVIendNoSnowStable, na.rm = T)#     |

#set up data for future loops
classes <- c('Rock', 'Forest', 'Shrub', 'Forbs', 'Heath') #<- define the classes you are using
class_values <- 2:6 #<- assign their numeric value

inputRast <- c(Start = startClass, End = endClass) #<- define your inputs as rasters
inputNDVI <- c(StartNDVI = NDVIstart, EndNDVI = NDVIend)

outputRast <- list() #<- create empty lists for outputs
outputNDVI <- list()
stableNDVI <- list()
outputStableClassNDVI <- list()
outputClassNDVI <- list()

#create a loop to get each class from each image
for (inputName in names(inputRast)){
  input <- inputRast[[inputName]] #<- get the name of the input raster
  
  for (i in seq_along(class_values)) {
    value <- class_values[i] #<- get the class name and number to isolate
    name <- classes[i]
    
    isolatedClass <- ifel(input != value, NA, input) #<- define a single class from input image
    
    outputName <- paste0(inputName, name) #<- name the output file
    outputRast[[outputName]] <- isolatedClass #<- put output file in the empty list
  }
}

#get stable pixels from the change raster to compare no change areas
for (i in names(inputNDVI)) {
  input <- inputNDVI[[i]] #<- get the input NDVI 
  mask <- mask(input, stable) #<- mask the NDVI input by stable regions
  outputName <- paste0('stable', i) #<- name it
  stableNDVI[[outputName]] <- mask #<- output it
}

#create a loop to get NDVI of individual classes in stable class areas
for (i in names(outputRast)){
  input <- outputRast[[i]] #<- grab isolated class from above
  for (rast in names(stableNDVI)) {
    NDVI <- stableNDVI[[rast]] #<- get stable NDVI
    mask <- if(grepl('Start', i)) {mask(stableNDVI[["stableStartNDVI"]], input)} else #<- get start NDVI per class masked by stable regions
      if(grepl('End', i)) {mask(stableNDVI[["stableEndNDVI"]], input)} else{ #<- get end NDVI per class masked by stable regions
        stop('Inputs do not contain Start or End') #<- state an error if inputs don't have the right names
      }
    outputName <- paste0('NDVIstable', i) #<- name your output stable region NDVI
    outputStableClassNDVI[[outputName]] <- mask #<- put it in a list
  }
}

#create a loop to get NDVI of individual classes regardless of Stability
for (i in names(outputRast)){
  input <- outputRast[[i]] #<- get NDVI raster
  mask <- if(grepl('Start', i)) {mask(NDVIstart, input)} else #<- get NDVI of class at start
    if(grepl('End', i)) {mask(NDVIend, input)} else{ #<- get NDVI of class at end
      stop('Inputs do not contain Start or End') #<- same error message
    }
  outputName <- paste0('NDVI', i) #<-name it
  outputClassNDVI[[outputName]] <- mask #<- output it
}


####MAP PLOTTING####
#set colors and breakpoints for NDVI color ramp for plotting
breakpoint <- seq(-1, 1, 0.5) #<- break the color ramp apart by (starting point, end point, sequence)
pal <- colorRampPalette(c( 'red', 'grey', 'green'))(length(breakpoint)) #<- assign color palette

#plot the NDVI for every class as a map
plottingMaps <- function(dat, title){
  p <- ggplot() + geom_spatraster(data = dat, aes(fill = NDVI)) + #<- plot the raster of NDVI for whatever you are plotting
    geom_spatvector(data = shp, fill = NA) + #<- add the watershed shp
    geom_sf() + scale_fill_gradientn( 
      colors = pal, breaks = breakpoint, limits = c(-1, 1), na.value = 'white') +  #<- assign color ramp
    labs(title = title, fill = 'NDVI') + theme_cust() +
    scale_x_continuous(breaks = c(-149.1, -148.9, -148.7)) + #<- axis labels
    scale_y_continuous(breaks = c(60.4, 60.3, 60.2))
}

#these lines are all plotting based on class from the functions above
#You will need to change the names of the inputs and names of plots based on your classes (ie., rock, heather, shrub, etc.)
plottingMaps(NDVIstartNoSnow, 'NJ 1986 No Snow')
plottingMaps(NDVIendNoSnow, 'NJ 2021 No Snow')

#watershedStart <-
  plottingMaps(NDVIstart, 'NJ NDVI 1986')
#watershedEnd<-  
  plottingMaps(NDVIend, 'NJ NDVI 2021')

#stabRockStart <- 
  plottingMaps(outputStableClassNDVI[["NDVIstableStartRock"]],     'Stable Rock NDVI Start')
#stabForestStart <- 
  plottingMaps(outputStableClassNDVI[["NDVIstableStartForest"]], 'Stable Forest NDVI Start')
#stabShrubStart <- 
  plottingMaps(outputStableClassNDVI[["NDVIstableStartShrub"]],   'Stable Shrub NDVI Start')
#stabForbsStart <- 
  plottingMaps(outputStableClassNDVI[["NDVIstableStartForbs"]],   'Stable Forbs NDVI Start')
#stabHeathStart <- 
  plottingMaps(outputStableClassNDVI[["NDVIstableStartHeath"]],   'Stable Heath NDVI Start')

#RockStart <- 
  plottingMaps(outputClassNDVI[["NDVIStartRock"]],     'Unstable Rock NDVI Start')
#ForestStart <- 
  plottingMaps(outputClassNDVI[["NDVIStartForest"]], 'Unstable Forest NDVI Start')
#ShrubStart <- 
  plottingMaps(outputClassNDVI[["NDVIStartShrub"]],   'Unstable Shrub NDVI Start')
#ForbsStart <- 
  plottingMaps(outputClassNDVI[["NDVIStartForbs"]],   'Unstable Forbs NDVI Start')
#HeathStart <- 
  plottingMaps(outputClassNDVI[["NDVIStartHeath"]],   'Unstable Heath NDVI Start')

#stabRockEnd <- 
  plottingMaps(outputStableClassNDVI[["NDVIstableEndRock"]],     'Stable Rock NDVI End')
#stabForestEnd <- 
  plottingMaps(outputStableClassNDVI[["NDVIstableEndForest"]], 'Stable Forest NDVI End')
#stabShrubEnd <- 
  plottingMaps(outputStableClassNDVI[["NDVIstableEndShrub"]],   'Stable Shrub NDVI End')
#stabForbsEnd <- 
  plottingMaps(outputStableClassNDVI[["NDVIstableEndForbs"]],   'Stable Forbs NDVI End')
#stabHeathEnd <- 
  plottingMaps(outputStableClassNDVI[["NDVIstableEndHeath"]],   'Stable Heath NDVI End')

#RockEnd <- 
  plottingMaps(outputClassNDVI[["NDVIEndRock"]],     'Unstable Rock NDVI End')
#ForestEnd <- 
  plottingMaps(outputClassNDVI[["NDVIEndForest"]], 'Unstable Forest NDVI End')
#ShrubEnd <- 
  plottingMaps(outputClassNDVI[["NDVIEndShrub"]],   'Unstable Shrub NDVI End')
#ForbsEnd <- 
  plottingMaps(outputClassNDVI[["NDVIEndForbs"]],   'Unstable Forbs NDVI End')
#HeathEnd <- 
  plottingMaps(outputClassNDVI[["NDVIEndHeath"]],   'Unstable Heath NDVI End')

#save the maps from above
ggsave('path\\where\\you\\want\\to\\save\\maps\\watershedStartNDVI.png', watershedStart)
ggsave('path\\where\\you\\want\\to\\save\\maps\\watershedStartNDVI.png', watershedEnd)

  ggsave('path\\where\\you\\want\\to\\save\\maps\\stabRockStartNDVI.png',   stabRockStart)
ggsave('path\\where\\you\\want\\to\\save\\maps\\stabForestStartNDVI.png', stabForestStart)
 ggsave('path\\where\\you\\want\\to\\save\\maps\\stabShrubStartNDVI.png',  stabShrubStart)
 ggsave('path\\where\\you\\want\\to\\save\\maps\\stabForbsStartNDVI.png',  stabForbsStart)
 ggsave('path\\where\\you\\want\\to\\save\\maps\\stabHeathStartNDVI.png',  stabHeathStart)

  ggsave('path\\where\\you\\want\\to\\save\\maps\\RockStartNDVI.png',   RockStart)
ggsave('path\\where\\you\\want\\to\\save\\maps\\ForestStartNDVI.png', ForestStart)
 ggsave('path\\where\\you\\want\\to\\save\\maps\\ShrubStartNDVI.png',  ShrubStart)
 ggsave('path\\where\\you\\want\\to\\save\\maps\\ForbsStartNDVI.png',  ForbsStart)
 ggsave('path\\where\\you\\want\\to\\save\\maps\\HeathStartNDVI.png',  HeathStart)

  ggsave('path\\where\\you\\want\\to\\save\\maps\\stabRockEndNDVI.png',   stabRockEnd)
ggsave('path\\where\\you\\want\\to\\save\\maps\\stabForestEndNDVI.png', stabForestEnd)
 ggsave('path\\where\\you\\want\\to\\save\\maps\\stabShrubEndNDVI.png',  stabShrubEnd)
 ggsave('path\\where\\you\\want\\to\\save\\maps\\stabForbsEndNDVI.png',  stabForbsEnd)
 ggsave('path\\where\\you\\want\\to\\save\\maps\\stabHeathEndNDVI.png',  stabHeathEnd)

  ggsave('path\\where\\you\\want\\to\\save\\maps\\RockEndNDVI.png',   RockEnd)
ggsave('path\\where\\you\\want\\to\\save\\maps\\ForestEndNDVI.png', ForestEnd)
 ggsave('path\\where\\you\\want\\to\\save\\maps\\ShrubEndNDVI.png',  ShrubEnd)
 ggsave('path\\where\\you\\want\\to\\save\\maps\\ForbsEndNDVI.png',  ForbsEnd)
 ggsave('path\\where\\you\\want\\to\\save\\maps\\HeathEndNDVI.png',  HeathEnd)


####Plotting PDFs####
#Now convert those rasters to dataframes for easy plotting of density curves
#create an empty output list
dfUnstableOutputs <- list()
dfStableOutputs <- list()

#convert NDVI images to data frames for density plotting
for(i in names(outputStableClassNDVI)){
  df <- as.data.frame(outputStableClassNDVI[[i]], xy = TRUE)
  outputName <- paste0(i, 'DF')
  dfStableOutputs[[outputName]] <- df
}

for(i in names(outputClassNDVI)){
  df <- as.data.frame(outputClassNDVI[[i]], xy = TRUE)
  outputName <- paste0(i, 'DF')
  dfUnstableOutputs[[outputName]] <- df
}

#This is a function for plotting density curves of the NDVI values you parsed out above
NDVIDensityPlotting <- function(datl5, datl8, title, col){
  ggplot() + geom_density(data = datl5, aes(x=NDVI, linetype = 'Start'), color = col, key_glyph = draw_key_smooth) +
    geom_density(data = datl8, aes(x=NDVI, linetype = 'End'), color = col, key_glyph = draw_key_smooth) + 
    scale_x_continuous(limits = c(-1,1), expand = c(0,0)) + labs(x = 'NDVI', y = 'Occurrence Density', title = title) + theme_cust() + 
    scale_linetype_manual(values = c('Start' = 'solid', 'End' = 'dashed'))
}

#this is the same kind of figure generation and saving as the section above
#stabRockDF <- 
  NDVIDensityPlotting(dfStableOutputs[["NDVIstableStartRockDF"]], dfStableOutputs[["NDVIstableEndRockDF"]], 'NJ Stable Rock', '#45414E')
#stabForestDF <- 
  NDVIDensityPlotting(dfStableOutputs[["NDVIstableStartForestDF"]], dfStableOutputs[["NDVIstableEndForestDF"]], 'NJ Stable Forest', '#0E5B07')
#stabShrubDF <- 
  NDVIDensityPlotting(dfStableOutputs[["NDVIstableStartShrubDF"]], dfStableOutputs[["NDVIstableEndShrubDF"]], 'NJ Stable Shrub', '#CE6C47')
#stabForbsDF <- 
  NDVIDensityPlotting(dfStableOutputs[["NDVIstableStartForbsDF"]], dfStableOutputs[["NDVIstableEndForbsDF"]], 'NJ Stable Forbs', '#FFC145')
#stabHeathDF <- 
  NDVIDensityPlotting(dfStableOutputs[["NDVIstableStartHeathDF"]], dfStableOutputs[["NDVIstableEndHeathDF"]], 'NJ Stable Heath', '#E14607')
#stabWatershedDF <-
  NDVIDensityPlotting(NDVIstartNoSnowStableDF, NDVIendNoSnowStableDF, 'Stable Watershed', 'black')

#RockDF <-   
  NDVIDensityPlotting(dfUnstableOutputs[["NDVIStartRockDF"]],   dfUnstableOutputs[["NDVIEndRockDF"]],   'NJ Unstable Rock', '#45414E')
#ForestDF <- 
  NDVIDensityPlotting(dfUnstableOutputs[["NDVIStartForestDF"]], dfUnstableOutputs[["NDVIEndForestDF"]], 'NJ Unstable Forest', '#0E5B07')
#ShrubDF <-  
  NDVIDensityPlotting(dfUnstableOutputs[["NDVIStartShrubDF"]],  dfUnstableOutputs[["NDVIEndShrubDF"]],  'NJ Unstable Shrub', '#CE6C47')
#ForbsDF <-  
  NDVIDensityPlotting(dfUnstableOutputs[["NDVIStartForbsDF"]],  dfUnstableOutputs[["NDVIEndForbsDF"]],  'NJ Unstable Forbs', '#FFC145')
#HeathDF <-  
  NDVIDensityPlotting(dfUnstableOutputs[["NDVIStartHeathDF"]],  dfUnstableOutputs[["NDVIEndHeathDF"]],  'NJ Unstable Heath', '#E14607')
#WatershedDF <-
  NDVIDensityPlotting(NDVIstartNoSnowDF, NDVIendNoSnowDF, 'Unstable Watershed', 'black')

ggsave('path\\where\\you\\want\\to\\save\\density\\plots\\stabRockNDVIDensity.pdf', stabRockDF)
ggsave('path\\where\\you\\want\\to\\save\\density\\plots\\stabForestNDVIDensity.pdf', stabForestDF)
ggsave('path\\where\\you\\want\\to\\save\\density\\plots\\stabShrubNDVIDensity.pdf', stabShrubDF)
ggsave('path\\where\\you\\want\\to\\save\\density\\plots\\stabForbsNDVIDensity.pdf', stabForbsDF)
ggsave('path\\where\\you\\want\\to\\save\\density\\plots\\stabHeathNDVIDensity.pdf', stabHeathDF)
ggsave('path\\where\\you\\want\\to\\save\\density\\plots\\stabWatershedNDVIDensity.pdf', stabWatershedDF)

ggsave('path\\where\\you\\want\\to\\save\\density\\plots\\RockNDVIDensity.pdf', RockDF)
ggsave('path\\where\\you\\want\\to\\save\\density\\plots\\ForestNDVIDensity.pdf', ForestDF)
ggsave('path\\where\\you\\want\\to\\save\\density\\plots\\ShrubNDVIDensity.pdf', ShrubDF)
ggsave('path\\where\\you\\want\\to\\save\\density\\plots\\ForbsNDVIDensity.pdf', ForbsDF)
ggsave('path\\where\\you\\want\\to\\save\\density\\plots\\HeathNDVIDensity.pdf', HeathDF)
ggsave('path\\where\\you\\want\\to\\save\\density\\plots\\WatershedNDVIDensity.pdf', WatershedDF)


#####Statistics####
##get stats related to line differences for each class via a t test
#heath
heathDatStart <- dfStableOutputs[['NDVIstableStartHeathDF']] #<- grab the class dataframe for start and end of study
heathDatEnd <-   dfStableOutputs[['NDVIstableEndHeathDF']]
heathDat <- merge(heathDatStart,  #<- merge the two dfs for t tests
                  heathDatEnd, by = c("x", "y"), suffix = c("Start", "End"), all = T)
t.test(heathDat$NDVIEnd, 
       heathDat$NDVIStart, paired = TRUE, alternative = "two.sided") #<- perform the t test and check for significance in the console

#rock
rockDatStart <- dfStableOutputs[['NDVIstableStartRockDF']]
rockDatEnd <-   dfStableOutputs[['NDVIstableEndRockDF']]
rockDat <- merge(rockDatStart, 
                 rockDatEnd, by = c("x", "y"), suffix = c("Start", "End"), all = T)
t.test(rockDat$NDVIEnd, 
       rockDat$NDVIStart, paired = TRUE, alternative = "two.sided")

#forest
forestDatStart <- dfStableOutputs[['NDVIstableStartForestDF']]
forestDatEnd <-   dfStableOutputs[['NDVIstableEndForestDF']]
forestDat <- merge(forestDatStart, 
                   forestDatEnd, by = c("x", "y"), suffix = c("Start", "End"), all = T)
t.test(forestDat$NDVIEnd, 
       forestDat$NDVIStart, paired = TRUE, alternative = "two.sided")

#shrub
shrubDatStart <- dfStableOutputs[['NDVIstableStartShrubDF']]
shrubDatEnd <-   dfStableOutputs[['NDVIstableEndShrubDF']]
shrubDat <- merge(shrubDatStart, 
                  shrubDatEnd, by = c("x", "y"), suffix = c("Start", "End"), all = T)
t.test(shrubDat$NDVIEnd, 
       shrubDat$NDVIStart, paired = TRUE, alternative = "two.sided")

#Forbs
forbsDatStart <- dfStableOutputs[['NDVIstableStartForbsDF']]
forbsDatEnd <-   dfStableOutputs[['NDVIstableEndForbsDF']]
forbsDat <- merge(forbsDatStart, 
                  forbsDatEnd, by = c("x", "y"), suffix = c("Start", "End"), all = T)
t.test(forbsDat$NDVIEnd, 
       forbsDat$NDVIStart, paired = TRUE, alternative = "two.sided")
