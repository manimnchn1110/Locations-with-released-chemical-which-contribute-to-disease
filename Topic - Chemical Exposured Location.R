#Topic - Chemical Exposured Location network
#Writed by Mani - Dec 2
#######################################################################

#Step 1 - Prepare data
#1.1 Required Package
library(readr)
library(dplyr)
library(leaflet)
library(tidyr)
library(Hmisc)
library(igraph)


#1.2 Load dataset
chemical_disease_dataset <- read_csv("chemicals-and-disease-DFE.csv")
chemical_location_dataset <- read_csv("Location_chemical.csv")

#Step 2 - Create Chemical_disease location list: 
#2.1 Keep chemical only mentioned in chemical contribute to disease
chemical_disease_dataset <- filter(chemical_disease_dataset, verify_relationship == "yes_direct")
##tolower for matching
chemical_disease_dataset$chemical_name = tolower(chemical_disease_dataset$chemical_name)
chemical_disease_dataset$chemical_name = sub(pattern = '<span class="chemical">',replacement = "", chemical_disease_dataset$chemical_name)
chemical_disease_dataset$chemical_name = sub(pattern = '</span>', replacement = "", chemical_disease_dataset$chemical_name)
chemical_location_dataset$CHEMICAL <- tolower(chemical_location_dataset$CHEMICAL)

##Filter - keep row only when chemical of chemical_location in checmial of chemical_disease with yes_direct relationship
chemical_location_dataset <- filter(chemical_location_dataset, CHEMICAL %in% chemical_disease_dataset$chemical_name)
chemical_disease_dataset <- filter(chemical_disease_dataset, chemical_name %in% chemical_location_dataset$CHEMICAL)

#2.2 collcet information of location point(including Name, Earth location, Building type, Chemical)
##Information from chemical_location
Longitude = chemical_location_dataset$LONGITUDE
Latitude = chemical_location_dataset$LATITUDE
Company_name = chemical_location_dataset$FACILITY_NAME
Industry_type = chemical_location_dataset$INDUSTRY_SECTOR
chemical_from_location = chemical_location_dataset$CHEMICAL

##Information from chemical_disease
chemical_from_chemical_disease = chemical_disease_dataset$chemical_name
###dataframe cleaning
chemical_disease_dataset$disease_name = tolower(chemical_disease_dataset$disease_name)
chemical_disease_dataset$disease_name = as.vector(sub(pattern = '<span class="disease">', replacement = "", chemical_disease_dataset$disease_name))
chemical_disease_dataset$disease_name = as.vector(sub(pattern = '</span>', replacement = "", chemical_disease_dataset$disease_name))

disease_from_chemical_disease = chemical_disease_dataset$disease_name

###Create dataframe
chemical_location_df <- data.frame(Longitude = as.vector(Longitude), 
                                   Latitude = as.vector(Latitude),
                                   Chemical = as.vector(chemical_from_location),
                                   Industry = as.vector(Industry_type),
                                   Company = as.vector(Company_name),
                                   stringsAsFactors = FALSE)
chemical_disease_df <-data.frame(Chemical = as.vector(chemical_from_chemical_disease),
                                 Disease = as.vector(disease_from_chemical_disease),
                                 stringsAsFactors = FALSE)
####Dataframe cleaning
chemical_disease_df <- chemical_disease_df[!(duplicated(chemical_disease_df$Chemical)&duplicated(chemical_disease_df$Disease)),]



#2.3 Merge two df and keep chemical_location_df
chemical_location_df <- merge(chemical_location_df, chemical_disease_df, all.x = T)
chemical_location_df$Chemical <-  capitalize(chemical_location_df$Chemical)
chemical_location_df$Disease <- capitalize(chemical_location_df$Disease)

##data cleaning
chemical_location_df <- chemical_location_df[complete.cases(chemical_location_df),]

#Step 3 - Visualization
#3.1 Define Industry type color
industry_type_color
Type_count = chemical_location_df %>% group_by(Industry) %>% summarise()
color <- substr(rainbow(count(Type_count)),1,7)
chemical_color_df <- data.frame(Industry = as.vector(Type_count$Industry), Color = color, stringsAsFactors = FALSE)

#3.2 apply to main df
chemical_location_df <- merge(chemical_location_df, chemical_color_df, all.x = TRUE)


#Step 4 - Visualization
#4.1 define popup
pop_up_list <- NULL
for(i in 1:length(chemical_location_df$Chemical)){
  pop_up_item <- paste(sep = "<br/>",
                       "<b> Chemical: ",
                       chemical_location_df$Chemical[i],
                       "</b>",
                       "<b><font color='##FF0000'> Disease: ",
                       chemical_location_df$Disease[i],
                       "</font></b>",
                       "<b> Comapny: ",
                       chemical_location_df$Company[i],
                       "</b>",
                       "<b> Industry: ",
                       chemical_location_df$Industry[i],
                       "</b>")
  pop_up_list <- c(pop_up_list, pop_up_item)
}

##append main dataframe
chemical_location_df <- data.frame(chemical_location_df, pop_up = as.vector(pop_up_list), stringsAsFactors = FALSE)

#Step 4 - Draw chemical_disease location map in US
##Chemical_location_Point(latitute, longtitute)
draw_map <- leaflet(data = chemical_location_df)
draw_map <- setView(draw_map, lng = -74.032470, lat = 40.743863, zoom = 9)
draw_map <- addTiles(draw_map)%>%addProviderTiles("CartoDB.DarkMatter")
draw_map <- addCircles(draw_map, 
                       lng = ~Longitude, 
                       lat = ~Latitude, 
                       popup = ~pop_up,
                       color = ~Color)
draw_map