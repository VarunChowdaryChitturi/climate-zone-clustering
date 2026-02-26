#install req packages
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")
#Libraries for data manipulation and clustering
library(tidyverse)
library(cluster)
library(factoextra)
#Loading data set
df <- read.csv("GlobalWeatherRepository.csv")

head(df)
#Inspect data set
#checking structure and missing values
dim(df)
colnames(df)
str(df)
colSums(is.na(df))

#Select relevant features(only numericals)
climate_df <- df %>%
  select(
    temperature_celsius,
    humidity,
    wind_kph,
    pressure_mb,
    precip_mm,
    cloud,
    visibility_km,
    uv_index,
    air_quality_PM2.5,
    air_quality_PM10
  )

head(climate_df)

#Handle with missing vals(replace NA with col mean)
#Using Z score standardizn
for(i in 1:ncol(climate_df)){
  climate_df[is.na(climate_df[,i]), i] <- mean(climate_df[,i], na.rm = TRUE)
}

colSums(is.na(climate_df))

#Scale data(standardizing)
scaled_data <- scale(climate_df)

head(scaled_data)

#-----PART-2------
#Elbow method(got k=4)
set.seed(123)

wss <- numeric(10)

for (k in 1:10) {
  km <- kmeans(scaled_data, centers = k, nstart = 10)
  wss[k] <- km$tot.withinss
}

wss
plot(
  1:10, 
  wss, 
  type = "b",
  pch = 19,
  frame = FALSE,
  xlab = "Number of Clusters (K)",
  ylab = "Total Within-Cluster Sum of Squares",
  main = "Elbow Method"
)
#Run final clustring model
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 4, nstart = 25)
#Attatch cluster numbers for each city
df$Cluster <- as.factor(kmeans_result$cluster)

head(df[, c("location_name", "Cluster")])

kmeans_result

#Understands Cluster characteristics and Interpret with the Output
aggregate(climate_df, by = list(Cluster = df$Cluster), mean)

#------PART-3-------
#Extreme weather Outlier detection
#Using Z score
#Detect temperature Outliers(extreme heat and cold temp)
temp_z <- scale(climate_df$temperature_celsius)

heatwave_index <- which(abs(temp_z) > 3)

heatwave_cities <- df[heatwave_index, c("location_name", "temperature_celsius")]

heatwave_cities
#Detect Heavy rain fall outliers
rain_z <- scale(climate_df$precip_mm)

heavy_rain_index <- which(abs(rain_z) > 3)

heavy_rain_cities <- df[heavy_rain_index, c("location_name", "precip_mm")]

heavy_rain_cities
#Detect strong wind outliers
wind_z <- scale(climate_df$wind_kph)

strong_wind_index <- which(abs(wind_z) > 3)

strong_wind_cities <- df[strong_wind_index, c("location_name", "wind_kph")]

strong_wind_cities
#Detect dangerous pollution
pm25_z <- scale(climate_df$air_quality_PM2.5)

pollution_index <- which(abs(pm25_z) > 3)

polluted_cities <- df[pollution_index, c("location_name", "air_quality_PM2.5")]

polluted_cities
#Count total outliers to get an idea
length(heatwave_index)
length(heavy_rain_index)
length(strong_wind_index)
length(pollution_index)


#---PART-4-----
#Visualizing K means cluster(diff colors diff clusters)
#interpret form the op image
fviz_cluster(
  kmeans_result,
  data = scaled_data,
  ellipse.type = "convex",
  palette = "jco",
  ggtheme = theme_minimal(),
  main = "Climate Zone Clustering"
)

#Temperature distribution plot
boxplot(
  climate_df$temperature_celsius,
  main = "Temperature Distribution",
  col = "lightblue",
  ylab = "Temperature (°C)"
)

#Clean Outlier summary

#Temperature Extremes
temp_z <- scale(climate_df$temperature_celsius)
heatwave_index <- which(temp_z > 3)
coldwave_index <- which(temp_z < -3)
cat("Number of Heatwaves:", length(heatwave_index), "\n")
cat("Number of Coldwaves:", length(coldwave_index), "\n")

#Heavy Rainfall
rain_z <- scale(climate_df$precip_mm)
heavy_rain_index <- which(rain_z > 3)
cat("Heavy Rainfall Events:", length(heavy_rain_index), "\n")

#Strong wind events
wind_z <- scale(climate_df$wind_kph)
strong_wind_index <- which(wind_z > 3)
cat("Strong Wind Events:", length(strong_wind_index), "\n")

#High pollution
pm25_z <- scale(climate_df$air_quality_PM2.5)
pollution_index <- which(pm25_z > 3)
cat("Dangerous Pollution Events:", length(pollution_index), "\n")


#----Hierarchial Clustering---
#Take sample data instead of getting crashes
set.seed(123)
sample_index <- sample(1:nrow(scaled_data), 2000)
sample_data <- scaled_data[sample_index, ]
#compute distance on sample
dist_matrix <- dist(sample_data)
#Hierarchial clustering
hc_result <- hclust(dist_matrix, method = "ward.D2")
#Plot dendogram
plot(hc_result, labels = FALSE,
     main = "Hierarchical Clustering (Sampled Data)")