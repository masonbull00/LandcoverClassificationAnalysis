##############################################
##Plot cumulative change per year of imagery##
##############################################

#This code will produce a line chart of cumulative landcover change for specific landcover
## conversions you want to analyze

#Run this after you run the heatmap change matirx code so that you have an idea of colors for changes

#you need:
##change rasters for every year
##a color library to use for changes 

#start with clearing out environments for a clean start and load libraries
rm(list= ls()) #<- clear out your environment for a clean start 
source("Path\\To\\Libraries.R") #<- set to the R file holding libraries

#Set up data paths for your inputs
path <- 'path\\to\\your\\folder\\of\\change\\rasters\\' 
filePattern <- paste('rockToHeath', 'snowToRock', 'snowToHeath', 'heathToForbs', 'forbsToShrub', sep = '|') #<- the landcover conversions you wnat to analyze
files <- list.files(path, pattern = filePattern) #<- list out the files you are analyzing

#Get the names of the landcover change metrics to plot and add them to a list
listOfMetrics <- c() #<- an empty list to be populated later

#a loop to populate the landcover change metrics list above
for(i in files){
  metric <- read.table(text = i, sep = '_')[[1]]
  #if metric is already in listOfMetrics don't add it,if the metric is not there then add the metric to the list
  ifelse((metric %in% listOfMetrics) == F, listOfMetrics <- c(listOfMetrics, metric), NA)
}

#create an empty dataframe of the metrics
df <- data.frame(metric = character(), #<- the landcover change
                 area = numeric(), #<- how large the change is (km2)
                 yearStart = numeric(), #<- the initial year of the change
                 yearEnd = numeric(), #<- year the landocver change went to
                 rateOfChange = numeric(), #<- the rate of change between two years 
                 stringsAsFactors = FALSE)

#calculate area and the year for each file and add it to the dataframe created above
for(j in 1:length(files)){
  dat <- read.csv(paste0(path, files[j]))
  metric <- read.table(text = files[j], sep = '_')[[1]] #<- this is predicated on a constant naming structure from previous codes
  area <- as.numeric(count(dat)*0.0009) #<- this is the scaling factor for 30m Landsat pixels to square kilometers
  yearStart <- read.table(text = files[j], sep = '_')[[2]] #<- this again requires constant naming structure
  yearEnd   <- read.table(text = files[j], sep = '_')[[3]]
  if (j > 1) {  # Ensure there's a previous row to reference
    prevYearEnd <- df$yearEnd[j - 1]
    rateOfChange <- area / (as.numeric(yearEnd) - as.numeric(yearStart)) #<- calculate the rate of change between two timesteps
  } else {
    rateOfChange <- NA  #<- no previous row, so rate cannot be calculated
  }
  df[j,] <- c(metric, area, yearStart, yearEnd, rateOfChange) #<- populate the dataframe
}


# format columns to appropriate form
df <- df %>% mutate_at(c("area", "yearStart", "yearEnd"), as.numeric) %>%
  mutate(metric = factor(metric))

# filter to just the areas starting in 1986 (or your starting year)
df_start <- df %>% filter(yearStart == 1986) %>% 
  filter(yearEnd != 2000) %>% filter(yearEnd != 2011) #<- these filter out years where your data is not good enough to plot

# calculate the difference in rate of change between two timesteps, in this case before and after 2009 
#these two chunks are for checking data and do not get plotted, but are very helpful to contextualize your data
df_start %>% filter(yearEnd %in% c(2009, 2023)) %>%
  pivot_wider(id_cols = metric, names_from = yearEnd, values_from = area) %>% 
  mutate(deltaArea09_23 = `2023` - `2009`,
         changeInRate09 = `2009`/(2009-1986),
         changeInRate23 = deltaArea09_23/(2023-2009), 
         percentIncrease = ((changeInRate23-changeInRate09)/changeInRate09)*100)

#same as above but for before 2009, between 2009 and 2015, and after 2015 
d <- df_1986 %>% filter(yearEnd %in% c(2009, 2015, 2023)) %>% 
  pivot_wider(id_cols = metric, names_from = yearEnd, values_from = area) %>% 
  mutate(deltaArea09_23 = `2023` - `2009`,
         deltaArea15_23 = `2023` - `2015`,
         changeInRate09 = `2009`/(2009-1986),
         changeInRate23_15 = deltaArea15_23/(2023-2015),
         changeInRate23_09 = deltaArea09_23/(2023-2009), 
         percentIncrease_09_23 = ((changeInRate23_09-changeInRate09)/changeInRate09)*100,
         percentIncrease_09_15 = ((changeInRate23_15-changeInRate09)/changeInRate23_15)*100)  


# plot the rates of change for each year you have imagery
df_1986 %>% ggplot(aes(x = yearEnd, y = area, color = metric)) + #<- set up data to plot 
  geom_line(linewidth = 1.25) + 
  theme_cust() + #<- call your theme from the source code
  scale_color_manual(labels = c('Snow to Rock', 'Snow to Heath', 'Rock to Heath', #<- labels for your legend
                                'Heath to Forbs', 'Forbs to Shrubs'), 
                     values = c(forbsToShrub = '#42a4a5', heathToForbs = '#ef992a', #<- set the color for indivual changes, colors are from change matrix code
                                rockToHeath = '#a56f6f', snowToHeath = '#e17252', snowToRock = 'darkgrey'),
                     breaks = c('snowToRock', 'snowToHeath', 'rockToHeath', 'heathToForbs', #<- how to group your color codes
                                'forbsToShrub')) +
  scale_x_continuous(expand = c(0,0), limits = c(1984, 2024), #<- how far your data spans
                     breaks = c(1986, 1990, 2000, 2010, 2020), #<- where to label breaks on the x axis
                     labels = c(1986, 1990, 2000, 2010, 2020)) + #<- label the above breaks
  labs(x = 'Year', y = 'Cumulative Area ( km\u00b2)') + #<- axis labels
  guides(color=guide_legend(title="")) + 
  new_scale_color() + #<- set a new color scale for labeling years by satellite
  geom_vline(xintercept = c(1986, 1999, 2002, 2003, 2006, 2009, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2023), linetype = 'dashed', color = 'grey75') + #<_ set vertical dashed lines for any years that you have iamgery for (comment out if you have imagery from every year in your study)
  geom_text(aes(x = yearEnd, label = as.character(yearEnd), color = as.character(yearEnd), y = 27), angle = 90, vjust = -0.2, size = 3.2) + #<- label the vertical lines from above
  scale_color_manual(values = c('1986' = 'black',    '1999' = 'blue',     '2002' = 'blue',     '2003' = 'black',    '2006' = 'black', 
                                '2009' = 'black',    '2015' = 'darkgreen', '2016' = 'darkgreen', '2017' = 'darkgreen',  #<- set a color coordination for different satellites, here: black is Landsat 5, blue is landsat 7, and green is Landsat 8. Years correspond to vertical lines
                                '2018' = 'darkgreen', '2019' = 'darkgreen', '2020' = 'darkgreen', '2021' = 'darkgreen', '2023' = 'darkgreen'),
                     guide = 'none')

