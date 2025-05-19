#######################################
##Calculate trends of NDVI over time ##
#######################################

#this code gets the mean NDVI for each year of your imagery and looks for significant trends
#You can use a linear model if you study area does not have major changes between one year to another (logging, fires, etc.) (Zhu, Fu, et al., 2016)
#If this is not the case you can use Mann-Kendall also in this code

#You will need:
## NDVI images for every year of your study 

#start with clearing your environment
rm(list= ls()) #<- clear out your environment for a clean start 
source("Path\\To\\Libraries.R") #<- set to the R file holding libraries
library(Kendall) #<- install the mann kendall package 

classPath <- 'path\\to\\your\\landcover\\class\\rasters\\for\\each\\year\\'
NDVIpath <- 'path\\to\\your\\ndvi\\images\\for\\each\\year\\'
years <- c('1986', '1999', '2000', '2002', '2003', '2006', '2009',  
           '2015', '2016', '2017', '2018', '2019', '2020', '2021', #<- the years that you have imagery for 
           '2024')
classes <- c('rock', 'forest', 'shrub', 'forbs', 'heath') #<- classes you want to analyze
stable <- rast("path\\to\\stableRegions.tif") #<- load in stable regions for plotting later

#initiate empty lists for outputs from loops
classRasterStack <- list() #<- entire class
stableClassRasterStack <- list() #<- stable regions for a class

#Initiate empty data frames for NDVI data
NDVIMeans <- data.frame(class = character(), #<- class you are observing
                        year = character(), #<- year you are observing
                        mean = numeric(), #<- mean NDVI
                        std = numeric(), #<- standard deviation of NDVI
                        yrCount = numeric()) #<- number of years from start of study
StableNDVIMeans <- data.frame(class = character(),
                              year = character(),
                              mean = numeric(),
                              std = numeric(),
                              yrCount = numeric())

##this function adds the mean and std of an image NDVI to the above dataframe
createNDVIbyClass <- function(year, class, NDVIMeans, classRasterStack){
  inputNDVI <- rast(paste0(NDVIpath, 'NDVI', year, '.tif')) #<- the NDVI of the year you are observing
  lc <- rast(paste0(classPath, class, year, '.tif')) #<- class you want to observe
  masked <- mask(inputNDVI, lc) #<- get the NDVI of the class for a specific year by masking
  maskedDF <- as.data.frame(masked) #<- convert to a dataframe
  
  classRasterStack[[class]] <- c(classRasterStack[[class]], masked) #<- add your raster to a stack of images
  
  NDVIMeans <- NDVIMeans %>% add_row(class = class, #<- populate the empty df with data from your new dataframe
                                     year = year,
                                     mean = mean(maskedDF[[1]], na.rm = TRUE), 
                                     std = sd(maskedDF[[1]], na.rm = TRUE),
                                     yrCount = (as.numeric(year) - 1986)) #<- change 1986 to the start of your study
  return(list(NDVIMeans = NDVIMeans, classRasterStack = classRasterStack))
}

#This is the loop to execute the function over the input years and classes
for(i in seq_along(years)){
  for(j in seq_along(classes)){
    result <- createNDVIbyClass(years[i], classes[j], NDVIMeans, classRasterStack)
    NDVIMeans <- result$NDVIMeans
    classRasterStack <- result$classRasterStack
  }
}

#quick plot to see how mean NDVI changes over time by class
NDVIMeans %>% ggplot(aes(x = year, y = mean, color = class, group = class)) + geom_point() + geom_line() + theme_minimal()



##this section is all identical to above, but uses the stable regions raster loaded at the beginning for comparison
createNDVIbyClassStableRegions <- function(year, class, StableNDVIMeans, stableClassRasterStack){
  inputNDVI <- rast(paste0(NDVIpath, 'NDVI', year, '.tif')) #<- get NDVI for a year
  masked <- mask(inputNDVI, stable) #<- mask NDVI for a year by the stable regions
  maskedDF <- as.data.frame(masked) #<- convert to df
  
  stableClassRasterStack[[class]] <- c(stableClassRasterStack[[class]], masked)
  
  StableNDVIMeans <- StableNDVIMeans %>% add_row(class = class, 
                                                 year = year,
                                                 mean = mean(maskedDF[[1]], na.rm = TRUE), 
                                                 std = sd(maskedDF[[1]], na.rm = TRUE),
                                                 yrCount = (as.numeric(year) - 1986)) #<- change 1986 to the start year for your data
  return(list(StableNDVIMeans = StableNDVIMeans, stableClassRasterStack = stableClassRasterStack))
}

#execute the function
for(i in seq_along(years)){
  for(j in seq_along(classes)){
    result <- createNDVIbyClassStableRegions(years[i], classes[j], StableNDVIMeans, stableClassRasterStack)
    StableNDVIMeans <- result$StableNDVIMeans
    stableClassRasterStack <- result$stableClassRasterStack
  }
}

StableNDVIMeans %>% ggplot(aes(x = year, y = mean, color = class, group = class)) + geom_point() + geom_line() + theme_minimal()


#Calculate mean NDVI of the entire watershed for a given year and plot it
raster_files <- paste0(NDVIpath, "NDVI", years, ".tif") #<- get the NDVI images for every year
ndvi_stack <- rast(raster_files)#<- Load NDVI rasters into a stack

# Calculate mean NDVI for each year
mean_ndvi <- global(ndvi_stack, mean, na.rm = TRUE)[, 1]  #<- Extract mean values
mean_ndvi_df <- data.frame(year = as.numeric(years), mean_ndvi) #<- convert to a dataframe

# Fit a linear trend
trend_model <- lm(mean_ndvi ~ year, data = mean_ndvi_df) #<- this only works for non-disturbed areas, otherwise use Mann-Kendall

#plot the trend of NDVI for the whole watershed and show the p value and R squared
mean_ndvi_df %>% ggplot(aes(x = year, y = mean_ndvi)) + geom_point() + 
  geom_smooth(method = 'lm', se = T, color = 'darkgrey') + theme_cust() + 
  labs(x = 'Year', y = ' Mean NDVI', title = 'Entire Watershed') +
  annotate("text", x= 2020, y=0.075, label = paste("R² =", round(summary(trend_model)$r.squared, 3)), color = "black") +
  annotate("text", x= 2020, y=0.07, label = paste("p value =", round(summary(trend_model)$coefficients[2,4], 3)), color = "black")


#Now we want to plot the NDVI trends for each landcover class
# Initialize a data frame to store results
class_trends <- data.frame(class = character(), #<- class to look at 
                           slope = numeric(), #<- the r squared value
                           p_value = numeric(), #<- p value
                           stringsAsFactors = FALSE)

#This plots NDVI trends for stable regions
for (class in classes) {
  # Load land cover mask for this class
  class_mask <- rast(paste0('path\\where\\stable\\regions\\go\\', class, 'Stable.tif'))
  
  # Mask NDVI stack by this class
  class_stack <- mask(ndvi_stack, class_mask)
  
  # Calculate mean NDVI for each year
  class_mean_ndvi <- global(class_stack, mean, na.rm = TRUE)[, 1]
  
  # Create a data frame
  class_ndvi_df <- data.frame(year = as.numeric(years), mean_ndvi = class_mean_ndvi)
  
  # Fit a linear model
  class_model <- lm(mean_ndvi ~ year, data = class_ndvi_df)
  
  # Extract the slope and p-value
  slope <- coef(class_model)["year"]
  p_value <- summary(class_model)$coefficients["year", "Pr(>|t|)"]
  r_square <- summary(class_model)$r.squared
  
  # Store the results
  class_trends <- rbind(class_trends, data.frame(class = class, slope = slope, p_value = p_value))
  
  # Plot each class trend (optional)
  p <- class_ndvi_df %>% ggplot(aes(x = year, y = mean_ndvi)) + geom_point() +
    labs(title = paste0("NDVI Trend for ", class), x = 'Year', y = 'Mean NDVI') + 
    theme_cust() + 
    geom_smooth(method = 'lm',  se = T, color = 'darkgrey') + 
    annotate("text", x= 2020, y=0.1, label = paste("P Value =", round(p_value, 3)), color = "black") +
    annotate("text", x= 2020, y=0.08, label = paste("R² =", round(r_square, 3)), color = "black")
  print(p)
}
# Print the trends by class
print(class_trends)


#This plots NDVI trends for a whole picture of a class, not just stbale or changed
for(class in classes){
  # Load land cover mask for this class (assumes one mask per class across all years)
  class_mask <- rast(paste0(classPath, class, years, '.tif'))
  
  # Mask NDVI stack by this class
  class_stack <- mask(ndvi_stack, class_mask)
  
  # Calculate mean NDVI for each year
  class_mean_ndvi <- global(class_stack, mean, na.rm = TRUE)[, 1]
  
  # Create a data frame
  class_ndvi_df <- data.frame(year = as.numeric(years), mean_ndvi = class_mean_ndvi)
  
  # Fit a linear model
  class_model <- lm(mean_ndvi ~ year, data = class_ndvi_df)
  
  # Extract the slope and p-value
  slope <- coef(class_model)["year"]
  p_value <- summary(class_model)$coefficients["year", "Pr(>|t|)"]
  r_square <- summary(class_model)$r.squared
  
  # Store the results
  class_trends <- rbind(class_trends, data.frame(class = class, 
                                                 slope = slope, 
                                                 p_value = p_value))
  
  # Plot each class trend (optional)
  p <- class_ndvi_df %>% ggplot(aes(x = year, y = mean_ndvi)) + geom_point() +
    labs(title = paste0("NDVI Trend for ", class), x = 'Year', y = 'Mean NDVI') + 
    theme_cust() + 
    geom_smooth(method = 'lm',  se = T, color = 'darkgrey') + 
    annotate("text", x= 2020, y=0.1, label = paste("P Value =", round(p_value, 3)), color = "black") +
    annotate("text", x= 2020, y=0.08, label = paste("R² =", round(r_square, 3)), color = "black")
  print(p)
}


#This section is a repeat of above but using Mann-Kendall instead of a linear model
# Example code for the watershed-wide Mann-Kendall test
mean_ndvi_df <- data.frame(year = as.numeric(years), mean_ndvi = mean_ndvi)

# Perform the Mann-Kendall test
mk_result <- MannKendall(mean_ndvi_df$mean_ndvi)

# Print the result
print(mk_result)

# Plot the NDVI time series
plot(mean_ndvi_df$year, mean_ndvi_df$mean_ndvi, pch = 19, col = "blue",
     main = "NDVI Trend for Watershed (Mann-Kendall)", xlab = "Year", ylab = "Mean NDVI")


# Initialize a data frame to store class-specific results
class_trends_mk <- data.frame(class = character(), 
                              tau = numeric(), 
                              p_value = numeric(), 
                              stringsAsFactors = FALSE)

for (class in classes) {
  # Mask NDVI stack by class
  class_mask <- rast(paste0(classPath, 'l8', class, 'Stable.tif'))
  class_stack <- mask(ndvi_stack, class_mask)
  
  # Calculate mean NDVI for each year
  class_mean_ndvi <- global(class_stack, mean, na.rm = TRUE)[, 1]
  
  # Create a data frame for the Mann-Kendall test
  class_ndvi_df <- data.frame(year = as.numeric(years), mean_ndvi = class_mean_ndvi)
  
  # Perform Mann-Kendall test for this class
  mk_class_result <- MannKendall(class_ndvi_df$mean_ndvi)
  
  # Extract Kendall's Tau and p-value
  tau <- mk_class_result$tau
  p_value <- mk_class_result$sl
  
  # Store the results
  class_trends_mk <- rbind(class_trends_mk, data.frame(class = class, 
                                                       tau = tau, 
                                                       p_value = p_value))
  
  # Plot each class trend (optional)
  p <- class_ndvi_df %>% ggplot(aes(x = year, y = mean_ndvi)) + geom_point() +
    labs(title = paste0("Mann-Kendall NDVI Trend for ", class), x = 'Year', y = 'Mean NDVI') + 
    theme_cust() + 
    geom_smooth(method = 'lm',  se = T, color = 'darkgrey') + 
    annotate("text", x= 2020, y=0.1, label = paste("P Value =", round(p_value, 3)), color = "black") +
    annotate("text", x= 2020, y=0.08, label = paste("R² =", round(r_square, 3)), color = "black") +
    annotate("text", x= 2020, y=0.06, label = paste("Tau =", round(tau, 3)), color = "black")
  print(p)
}

# Print the trends for each class
print(class_trends_mk)