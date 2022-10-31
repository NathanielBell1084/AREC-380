library(tidyverse)
library(lubridate)
library(readxl)
library(modelr)

########
# Nuwan Hewabethmage
########
# read in the file where the document held
data.file <- file.path("C:","Users","nhewa","Documents","AREC380",
                      "Home Work","Homework data")


# read in the value 
co2.data <-read_excel(file.path(data.file,"Table_11.1_Carbon_Dioxide_Emissions_From_Energy_Consumption_by_Source.xlsx"
                                    ),skip = 8)
             

electric.cars.data <-read_excel(file.path(data.file,"10567_pev_sales_2-28-20.xlsx"),
                                      skip = 2)


# view what we read in
#View(co2.data)
#View(electric.cars.data)

# we want to show the total CO2 emission so we narrow down to only the Total emission
co2.data %>%
  select(Month,`Total Energy CO2 Emissions`)->Only_total_emission

#######print(Only_total_emission)

# convert the month data to integers for prediction

Only_total_emission%>%
  mutate(
    month.as.int = as.integer(factor(Month))
  )->Only_total_emission

# create a model to see how it looks like
lm(`Total Energy CO2 Emissions` ~ month.as.int +I(month.as.int^5), data = Only_total_emission) ->model_total_emission

Only_total_emission%>%
  add_predictions(model_total_emission,"model total emission")->Only_total_emission


# scatter plot diagram  for CO2 Emission 
ggplot(data = Only_total_emission)+
  geom_point(
    mapping = aes(
      x = Month, # month can be represented in X and Y can be represented in Total CO2 emissions
      y = `Total Energy CO2 Emissions`
    ),
    color = "black"
  )+
  geom_point(
    mapping = aes(
      x = Month,
      y = `model total emission`
    ),
    color = "orange"
  )+
  geom_smooth(
    mapping = aes(
      x = Month,
      y = `Total Energy CO2 Emissions`
    ),
    color = "red"
  )+
  labs(
    title = "Total Energy CO2 Emissions (Million Metric Tons of CO2)",
    x = "Month",
    y = "Energy CO2 Emissions (Million Metric Tons of CO2)"
  )










# we also want to show what are the main two contributors to CO2 emission
# One of the main contributors is Petroleum 
co2.data %>%
  select(Month,`Petroleum, Excluding Biofuels, CO2 Emissions`)->
  petroleum_emission

# we have to create a prediction first we have to convert Month

petroleum_emission%>%
  mutate(
    month.as.int = as.integer(factor(Month))
  )->petroleum_emission
  
#Now we can create a prediction 
lm(`Petroleum, Excluding Biofuels, CO2 Emissions` ~ month.as.int +I(month.as.int^5), data = petroleum_emission) ->model_petroleum_emission

petroleum_emission%>%
  add_predictions(model_petroleum_emission,"model petroleum emission")->petroleum_emission

# Petroleum Emission Scatter plot of each month
ggplot(data = petroleum_emission)+
  geom_point(
    mapping = aes(
      x = Month,
      y = `Petroleum, Excluding Biofuels, CO2 Emissions`
    ),
    color = "black"
  )+
  geom_point(
    mapping = aes(
      x = Month,
      y = `model petroleum emission`
    ),
    color = "orange"
  )+
  geom_smooth(
    mapping = aes(
      x = Month,
      y = `Petroleum, Excluding Biofuels, CO2 Emissions`
    ),
    color = "red"
  )+
  labs(
    title = "Petroleum CO2 Emissions (Million Metric Tons of CO2)",
    x = "Month",
    y = "Petroleum CO2 Emissions (Million Metric Tons of CO2)"
  )




electric.cars.data %>%
  slice(1:55)%>%
  select(Vehicle,Total)-> only_electric_cars

# View(only_electric_cars)
ggplot()+
  geom_point(
    data = only_electric_cars,
    mapping = aes(
      x = Vehicle,
      y= Total
    )
  )+ 
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(
    title = "Total EV vehicles sold in the United State past ten years",
    x = "Vehicles",
    y = "Total EV vehicles sold"
  )

electric.cars.data %>%
  slice(1:55)%>%
  select(Vehicle,`2011`)->EV.sold.2011


ggplot()+
  geom_point(
    data = EV.sold.2011,
    mapping = aes(
      x = Vehicle,
      y = `2011`
    )
  )+ 
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(
    title = "Total EV vehicles sold in the United State in 2011",
    x = "Vehicles",
    y = "Total EV vehicles sold in 2011"
  )



electric.cars.data %>%
  slice(1:55)%>%
  select(Vehicle,`2019`)->EV_sold_2019

ggplot()+
  geom_point(
    data = EV_sold_2019,
    mapping = aes(
      x = Vehicle,
      y = `2019`
    )
  )+ 
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(
    title = "Total EV vehicles sold in the United State in 2019",
    x = "Vehicles",
    y = "Total EV vehicles sold in 2019"
  )

