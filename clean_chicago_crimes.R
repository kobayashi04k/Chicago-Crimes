library(tidyverse)
library(chron)

# clean Chicago_Crimes_2020-2021 to change and add variable names
chicago_crimes <- read.csv("data/Chicago_Crimes_2020-2021.csv",
                           col.names = c("case_number",
                                         "date",
                                         "block",
                                         "iucr",
                                         "primary_description", "secondary_description",
                                         "location_description",
                                         "arrest",
                                         "domestic",
                                         "beat",
                                         "ward",
                                         "fbi_cd",
                                         "x_coordinate",
                                         "y_coordinate",
                                         "latitude",
                                         "longitude",
                                         "location")) %>%
                  separate(date, c("date", "time"), 11) %>%
                  mutate(time = chron(times = format(strptime(time, "%I:%M:%S %p"), "%H:%M:%S"))) %>%
                  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
                  select(case_number, 
                         date, 
                         time, 
                         primary_description, 
                         secondary_description,
                         location_description,
                         latitude,
                         longitude,
                         location)    

# order the crimes by date and add a new column called month_year
chicago_crimes <- chicago_crimes[order(as.Date(chicago_crimes$date, format="%m/%d/%Y")),] %>%
                  mutate(month_year = (format(date, "%m/%Y")))

# write the cleaned Chicago crimes data set to new csv file
write.csv(chicago_crimes, file = "data/clean_chicago_crimes.csv")
