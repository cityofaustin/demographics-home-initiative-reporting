#This script calculates indicator values for Austin's HOME initiative annual reporting effort.
# Five demographic characteristics we will pull for tracts: 
# Indicator 1: % communities of color
# Indicator 2: % low income
# Indicator 3: % children living in poverty 
# Indicator 4: % renters
# Indicator 5: % without a BA

#Load required packages
library(tidycensus)
library(tidyverse)
library(sf)
library(mapview)

#Define the counties that overlap the COA boundary
austin_counties <- c("Travis", "Hays", "Williamson")
austin_msa_counties <- c("Travis", "Hays", "Williamson","Caldwell","Bastrop")

#set the value for HUD Median Family Income (MFI)
#https://www.huduser.gov/portal/datasets/il/il2023/2023summary.odn?year=2023&states=%24states%24&data=2023&inputname=METRO12420M12420*Austin-Round+Rock%2C+TX+MSA&stname=%24stname%24&statefp=99&selection_type=hmfa
austin_mfi <- 122300 #2023 value
austin_mfi_80pct <- (austin_mfi*0.8) 

#Set the list of census/ACS variables needed for the five vulnerability indicators listed at the top.
#Indicators 3 and 4 can be pulled directly, while indicators 1, 2, and 5 require calculations as noted below.
vulnerability_vars <- c(
  pct_white = "DP05_0082P", #nonHispanic White alone
  pct_in_poverty_under18 = "S1701_C03_002", #Indicator 3 % children living in poverty
  pct_renters = "DP04_0047P", # Indicator 4: % renters
  pop_over_25 = "S1501_C01_006",
  bachelors_or_higher = "S1501_C01_015",
  inc_total = "B19001_001", #Total income-earning households
  inc_under_10k = "B19001_002", #Income less than $10,000
  inc_10to15k = "B19001_003", #Income from $10,000 to $14,999
  inc_15t020k = "B19001_004", #Income from $15,000 to $19,999
  inc_20to25k = "B19001_005", #Income from $20,000 to $24,999
  inc_25to30k = "B19001_006", #Income from $25,000 to $29,999
  inc_30to35k = "B19001_007", #Income from $30,000 to $34,999
  inc_35to40k = "B19001_008", #Income from $35,000 to $39,999
  inc_40to45k = "B19001_009", #Income from $40,000 to $44,999
  inc_45to50k = "B19001_010", #Income from $45,000 to $49,999
  inc_50to60k = "B19001_011", #Income from $50,000 to $59,999
  inc_60to75k = "B19001_012", #Income from $60,000 to $74,999
  inc_75to100k = "B19001_013" #Income from $75,000 to $99,999
  
)

#Import data from the Census Bureau API
austin_data <- get_acs(
  geography = "tract",
  variables = vulnerability_vars,
  year = 2023,
  state = "TX",
  county = austin_msa_counties,
  geometry = TRUE,
  cb = FALSE,
  output = "wide"
)|>
  st_transform(2277) 

austin_data_clean <- austin_data |>
  #Indicator 1: Percent communities of color is calculated by subtracting the percent of the population that is non-Hispanic white alone from the population total (100%)
  mutate(pct_communities_of_color = round(100 - pct_whiteE, digits = 1),
         
  #Indicator 5: Percent without a bachelors is calculated as (population over 25 - # with bachelors)/population over 25
         pct_wo_bachelors = round(((pop_over_25E - bachelors_or_higherE)/pop_over_25E)*100, digits = 1),
  
  #Calculate the number of people in the final income bracket from $75,000 to the 80% MFI cutoff 
         inc_75toMFI = inc_75to100kE*((austin_mfi_80pct-75000)/(99999-75000)
         ), .after = NAME)|>
  
  #Indicator 2: Add up the number of people in each income bracket up to 80% MFI, divide by total earners
  mutate(pct_low_income = 
           round(
            ((inc_under_10kE +
         inc_10to15kE +
         inc_15t020kE +
         inc_20to25kE +
         inc_25to30kE +
         inc_30to35kE +
         inc_35to40kE +
         inc_40to45kE +
         inc_45to50kE +
         inc_50to60kE +
         inc_60to75kE +
         inc_75toMFI)/inc_totalE)*100,digits = 1), .after = NAME)

#Select columns for final dataset, rename with shorter column names
home_data_export <- austin_data_clean |>
  select(GEOID, 
         NAME, 
         pct_communities_of_color, # % communities of color
         pct_low_income, # % low income
         pct_in_poverty_under18E, # % children living in poverty 
         pct_wo_bachelors, # % without a BA
         pct_rentersE) |> # % renters
            rename(coms_color = pct_communities_of_color,
                 low_inc = pct_low_income,
                 child_pov = pct_in_poverty_under18E,
                 no_ba = pct_wo_bachelors,
                 renters = pct_rentersE)
              

#Save version of data frame without geometry to view in Excel  
home_data_export_csv <- st_drop_geometry(home_data_export)

view(home_data_export)

#Uncomment the lines below and run to export files as CSV, Shapefile, or GeoJSON
#write_csv(home_data_export_csv, "data_csv/home_reporting_data_2023.csv")
#st_write(home_data_export, "data_geo/home_reporting_data_2023.geojson")
#st_write(home_data_export, "data_shapefile/home_reporting_data_2023.shp")






  

