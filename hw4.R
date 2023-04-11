library(tidyverse)
library(dplyr)

data = read_csv("plan372-hw4/airport_pairs.csv")

# MARKET SATURATION ANALYSIS----------------------------------------------------

# Create a table of the existing flights to or from RDU, and the number of 
# passengers  traveling to each destination.
RDU_data = data.frame(data$origin, data$dest, data$passengers)
  
RDU_data <- RDU_data %>% 
  rename("origin" = "data.origin",
         "dest" = "data.dest",
         "passengers" = "data.passengers")

RDU_data <- RDU_data %>% 
  filter(origin == "RDU" | dest == "RDU")

# What is the most popular non-stop destination from RDU?
RDU_data <- RDU_data %>% 
  filter(origin == "RDU", passengers >= 10000)

arrange(RDU_data, desc(passengers))
# Answer: Atlanta


# BRINGING IN CENSUS DATA-------------------------------------------------------
library(tidycensus)
load_variables(year = 2020, dataset = "pl")

census_api_key("a819ba33cc6ca556ab50076f125e6f3c072e736e")

cbsa_population <- get_acs(
  geography = "cbsa",
  variables = "B01003_001",
  year = 2020)

origin_population <- cbsa_population %>% 
  rename("origin_cbsa" = "GEOID",
  "origin_pop" = "estimate")

# Create copies of census data before joining to airport data.
dest_population <- cbsa_population %>% 
  rename("dest_cbsa" = "GEOID",
         "dest_pop" = "estimate")

# Join Census data to airport data.
origin_population$origin_cbsa = as.numeric(origin_population$origin_cbsa)

data = left_join(data, origin_population, by=c("origin_cbsa"))

dest_population$dest_cbsa = as.numeric(dest_population$dest_cbsa)

data = left_join(data, dest_population, by=c("dest_cbsa"))

# Use group_by and summarize to create a new dataset that shows total CBSA to CBSA volumes.
cbsa_to_cbsa = group_by(data, origin_cbsa, dest_cbsa) %>%
  summarize(tot_passengers=sum(passengers))

# Remove airports that are not in metropolitan area.
data = data[!grepl('Micro', data$NAME.y),]

data = left_join(data, cbsa_to_cbsa, by=c("origin_cbsa", "dest_cbsa"))

# Generate scatterplots between origin population and total passengers, 
# destination population and total passengers, and 
# flight distance and total passengers.
ggplot(data, aes(x=origin_pop, y=tot_passengers, group=1)) +
  geom_point()

ggplot(data, aes(x=dest_pop, y=tot_passengers, group=1)) +
  geom_point()

ggplot(data, aes(x=distancemiles, y=tot_passengers, group=1)) +
  geom_point()


# PASSENGER VOLUME REGRESSION---------------------------------------------------

# Run a regression of total passenger volumes on origin CBSA population.
single_variable_model1 = lm(tot_passengers~origin_pop, data)
summary(single_variable_model1)

# Run a regression of total passenger volumes on destination CBSA population.
single_variable_model2 = lm(tot_passengers~dest_pop, data)
summary(single_variable_model2)

# Run a regression of total passenger volumes on distance miles.
single_variable_model3 = lm(tot_passengers~distancemiles, data)
summary(single_variable_model3)


# PASSENGER VOLUME PREDICTION---------------------------------------------------
# RDU to Portland, OR (PDX) - 2,363 mi
# RDU to El Paso, TX (ELP) - 1,606 mi
# RDU to Tallahassee, FL (TLH) - 496 mi
# RDU to San Diego (SAN) - 2,193 mi

# Predict passenger demand for the above routes.
passenger_forecast <- subset(data, select=c("origin", "dest", "distancemiles", 
"origin_cbsa", "dest_cbsa", "origin_pop", "dest_pop"))

passenger_forecast <- passenger_forecast %>%
  filter(origin == "RDU" & dest == "PDX" | 
           origin == "RDU" & dest == "ELP" | 
           origin == "RDU" & dest == "TLH" | 
           origin == "RDU" & dest == "SAN" |
           dest == "RDU" & origin == "PDX" | 
           dest == "RDU" & origin == "ELP" | 
           dest == "RDU" & origin == "TLH" | 
           dest == "RDU" & origin == "SAN")

# We are missing some routes in the new passenger_forecast dataset: 
# RDU -> TLH
# TLH -> RDU
# PDX -> RDU
# ELP -> RDU

# Use other data from "data" that use these origin or destinations to 
# complete "passenger_forecast".
filter(data, origin %in% c("TLH"))

passenger_forecast <- passenger_forecast %>% 
  add_row(origin = "RDU", dest = "TLH", distancemiles = 496, origin_cbsa = 39580, 
          dest_cbsa = 45220, origin_pop = 1362997, dest_pop = 384783)

passenger_forecast <- passenger_forecast %>% 
  add_row(origin = "TLH", dest = "RDU", distancemiles = 496, origin_cbsa = 45220, 
          dest_cbsa = 39580, origin_pop = 384783, dest_pop = 1362997)

passenger_forecast <- passenger_forecast %>% 
  add_row(origin = "PDX", dest = "RDU", distancemiles = 2363, origin_cbsa = 38900, 
          dest_cbsa = 39580, origin_pop = 2472774, dest_pop = 1362997)

passenger_forecast <- passenger_forecast %>% 
  add_row(origin = "ELP", dest = "RDU", distancemiles = 1606, origin_cbsa = 21340, 
          dest_cbsa = 39580, origin_pop = 841602, dest_pop = 1362997)

# Calculate the total passengers based on linear regression model.
passenger_forecast$origin_tot_pass <- with(passenger_forecast, 3.744e-02 * origin_pop + 9.891e-04)

passenger_forecast$dest_tot_pass <- with(passenger_forecast, 3.504e-02 * dest_pop + 9.887e-04)

passenger_forecast$distance_tot_pass <- with(passenger_forecast, -6.108 * distancemiles + 6.356)