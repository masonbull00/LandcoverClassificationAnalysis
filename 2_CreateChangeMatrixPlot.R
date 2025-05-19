#########################################
##Create a change matrix of your data####
#########################################

#This code will create a matrix of your landcover changes to see how much of each landcover changed
#This is using the change rasters you created in a previous code
#you only need one change raster calculated 

#start with clearing out environments for a clean start and load libraries
rm(list= ls()) #<- clear out your environment for a clean start 
source("Path\\To\\Libraries.R") #<- set to the R file holding libraries

#bring in your changed data as a csv for plotting and manipulation
change <- read.csv("path\\to\\your\\change\\raster\\dataframe\\changeRaster_1986_2024.csv") #<- file that is your change between the start and end of the study
colnames(change)[colnames(change) == 'changeRaster_1986_2024'] <- 'changes' #<- rename the columns to something easy to read

#prep data to get how much landcover changed from one type to another, displayed in a heatmap
heatmapDat <- change %>%
  mutate(x_class = case_when(changes %in% c(-13, -21, -35, -37, -52) ~ "Snow", #<- these numbers correspond to a type of landcover, 
                             changes %in% c(13, -8, -22, -24, -39)   ~ "Rock",  #determined by the matrix from the CalculateChangeRasters code
                             changes %in% c(21, 8, -14, -16, -31)    ~ "Forest",
                             changes %in% c(35, 22, 14, -2, -17)     ~ "Shrub",
                             changes %in% c(37, 24, 16, 2, -15)      ~ "Forbs",
                             changes %in% c(52, 39, 31, 17, 15)      ~ "Heath", #<- the X_class is change TO a landcover
                             TRUE ~ "add label"), 
         y_class = case_when(changes %in% (c(-13, -21, -35, -37, -52)*-1) ~ "Snow", #<- Y_class is change FROM a landcover
                             changes %in% (c(13, -8, -22, -24, -39)*-1)   ~ "Rock",
                             changes %in% (c(21, 8, -14, -16, -31)*-1)    ~ "Forest",
                             changes %in% (c(35, 22, 14, -2, -17)*-1)     ~ "Shrub",
                             changes %in% (c(37, 24, 16, 2, -15)*-1)      ~ "Forbs", #your numbers and class names will change depending on landcover
                             changes %in% (c(52, 39, 31, 17, 15)*-1)      ~ "Heath",
                             TRUE ~ "add label")) %>% 
  group_by(x_class, y_class) %>% summarise(n = n()) %>% ungroup() #<- group the data by class, get the count

dummyVal <- data.frame("x_class" = c("Snow",   "Snow", "Rock", "Forest", "Shrub", "Forbs", "Heath"), #<- this is a dummy dataframe for creating useable dataframes
                       "y_class" = c("Forest", "Snow", "Rock", "Forest", "Shrub", "Forbs", "Heath"), 
                       "n" = c(0, NA, NA, NA, NA, NA, NA))

heatmapDat <- rbind(heatmapDat, dummyVal) #<- bind our real data and dummy data 


#calculate area in km2
heatmapArea <- heatmapDat %>% mutate(area = (n*0.0009), area = round(area, 2)) %>% na.omit() #<- change the pixel count to an area in km2

#add in any 0 values to manually fill out the heatmap
missingConversions <- data.frame(x_class = c('Snow', 'Snow'), #<- you may find that 0 area changes are put as NA, this will manually fill those in
                                 y_class = c('Shrub', 'Forbs'), #<- input your classes that are 0 and manually fill them with 0
                                 n = c(0,0),
                                 area = c(0.00,0.00))
heatmapArea <- rbind(heatmapArea, missingConversions) #<- insert the missing conversions


colorList <- data.frame(                                         #<- get a list for each change category and assing the change a unique color
  x_class = c('Forbs',   'Forbs',   'Forbs',   'Forbs',   'Forbs',   
              'Forest',  'Forest',  'Forest',  'Forest',  'Forest',  
              'Heath',   'Heath',   'Heath',   'Heath',   'Heath',   
              'Rock',    'Rock',    'Rock',    'Rock',    'Rock',    
              'Shrub',   'Shrub',   'Shrub',   'Shrub',   'Shrub',   
              'Snow',    'Snow',    'add label', 'Snow',    'Snow',    'Snow'),
  y_class = c('Forest',  'Heath',   'Rock',    'Shrub',   'Snow',    
              'Forbs',   'Heath',   'Rock',    'Shrub',   'Snow',    
              'Forbs',   'Forest',  'Rock',    'Shrub',   'Snow',    
              'Forbs',   'Forest',  'Heath',   'Shrub',   'Snow',    
              'Forbs',   'Forest',  'Heath',   'Rock',    'Snow',    
              'Heath',   'Rock',    'add label', 'Forest',  'Shrub',   'Forbs'),
  color   = c('#9fbc2e', '#ef832e', '#bb8e10', '#a8bbac', '#ffea5f', 
              '#556200', '#766119', '#326119', '#326176', '#6e9e52', 
              '#a44f10', '#7a4800', '#a56f6f', '#94117f', '#e17272', 
              '#9c9575', '#70947b', '#9c7575', '#758e9c', '#a4a09d', 
              '#42a4a5', '#2e9f98', '#668bff', '#0b66af', '#89e7ff',
              '#e1e1e1', '#e1e1e2', 'white',   '#e1e1e3', '#e1e1e4', '#e1e1e5') #<- when this has all numbers as #e1e1e1 then the other colors don't all work, but when colors are set separately the other colors work
)

heatmapArea <- merge(heatmapArea, colorList, all =T) #<- merge the color list and change data

#plot the heatmap using ggplot 
#coloredAreaHeatmap <-
heatmapArea %>%  filter(x_class != "add label") %>%
  mutate(x_class = factor(x_class, levels = c("Snow", "Rock", "Forest", "Shrub", "Forbs", "Heath"), ordered = T),
         y_class = factor(y_class, levels = c("Snow", "Rock", "Forest", "Shrub", "Forbs", "Heath"), ordered = T)) %>%
  ggplot(aes(x = x_class, y = y_class, fill = color)) + geom_tile() + theme_cust() + 
  geom_text(aes(label = paste0(area, ' km\u00b2')), size = 4.5) + 
  scale_fill_manual(values = heatmapArea$color[order(heatmapArea$color)]) +
  theme(text = element_text(size = 12)) +
  labs(fill = bquote("Area Changed "(km^2)), x = '2021 (To raster)', y = '1986 (From raster)') +
  theme(legend.position = 'none')

#save the heatmap
ggsave(filename = "areaHeatmapColored.png", plot = coloredAreaHeatmap, path = 'path\\to\\save\\the\\plot\\to\\')

















