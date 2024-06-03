setwd("H:/Kenya price data")
library(RSelenium) 
library(dplyr)
library(rvest)
library(stringr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(maps)
library(geosphere)

a <- c("Baringo town", "Embu town", "Garissa town", "Isiolo town", "Kajiado town", "Kilifi town", "Kitui town", "Kwale town", "Laikipia town", "Lamu town", "Makueni town", "Mandera town", "Marsabit town", "Meru town", "Narok town", "Nyeri town", "Samburu town", "Taita Taveta town", "Tana River town", "Tharaka Nithi town", "Turkana town", "Wajir town", "West Pokot town")

# Define path to chromedriver
chromedriver <- "C:\\Users\\LMADAGA\\AppData\\Local\\binman\\binman_chromedriver\\win32\\124.0.6367.78.chromedriver.exe"

# Start RSelenium
driver <- rsDriver(browser = "chrome", chromever = "124.0.6367.78", extraCapabilities = list(chromever = chromedriver))
remDr <- driver[["client"]]

remDr$navigate("https://www.geoplaner.com/")

Latlist = list() # Creating an empty list to store all the latitudes
Longlist = list() # Creating an empty list to store all the longitudes

web_place = remDr$findElement(using = "class", value = "e80adr") # Finding the css element where the locations will be entered on the geoplaner website

for (val in 1:length(a)) {
  aa = web_place$sendKeysToElement(list(paste(a)[val], key="enter")) # Once the location is entered, enter command is executed 
  source <- remDr$getPageSource() #web page source
  
  locn =  read_html(as.character(source)) %>% html_nodes("#dt") %>% html_text() # reading the webpage as the HTML
  
  
  Latlist[val] = strsplit(strsplit(locn, " +")[[1]][8],"°")[[1]][1] #Extracting the latitudes
  Longlist[val] = strsplit(strsplit(locn, " +")[[1]][9],"°")[[1]][1] #Extracting the longitudes
  
  web_place$clearElement() #clear the input area so that the next location can be entered
  Sys.sleep(5) #Allowing system enough time to reload the page after each execution
}

source <- remDr$getPageSource() #web page source
aa = web_place$sendKeysToElement(list(paste(a)[length(a)], key="enter"))
locn =  read_html(as.character(source)) %>% html_nodes("#dt") %>% html_text() 
Latlist = append(Latlist,strsplit(strsplit(locn, " +")[[1]][8],"°")[[1]][1])
Longlist = append(Longlist,strsplit(strsplit(locn, " +")[[1]][9],"°")[[1]][1])

df = data.frame(Location = c(a[2:length(a)], a[1]), 
                Latitude = as.numeric(c(unlist(Latlist)[3:length(Latlist)],unlist(Latlist)[2])),
                Longitude = as.numeric(c(unlist(Longlist)[3:length(Longlist)],unlist(Longlist)[2])))

df

write.csv(df, "Market_coodinates_Kenya.csv", row.names = FALSE)




#Merge the market coodinates to the market prices dataset
# Load required packages
library(data.table)
library(leaflet)
library(geodata)
library(lubridate)
library(terra)

# Read the dataset containing market names and coordinates
coods <- read.csv("Market_coodinates_Kenya.csv")
head(coods)


# Create a leaflet map to check the coodinates correctness.
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(data = coods, ~Longitude, ~Latitude, popup = ~Market)

# Print the map
m


# Read the dataset containing market prices
dta <- read.csv("Kenya-Price-Data.csv")

# Create a new column in the market prices dataset
dta$Latitude <- NA
dta$Longitude <- NA

# Iterate through markets and find matching coordinates
for (i in 1:nrow(dta)) {
  market <- dta[i, "Market"]
  coord <- coods[coods$Market == market, c("Latitude", "Longitude")]
  
  if (nrow(coord) > 0) {
    dta[i, c("Latitude", "Longitude")] <- coord
  }
}

# Save the modified dataset with coordinates
write.csv(dta, "Kenya-Price-Data_With_Coordinates.csv", row.names = FALSE)


prices <- read.csv("Kenya-Price-Data_With_Coordinates.csv")
dim(prices)
head(prices)
table(prices$Market)
sapply(prices, class)

# Create a leaflet map
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(data = prices, ~Longitude, ~Latitude, popup = ~Market)

# Print the map
m

