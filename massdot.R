library(tidyverse)
library(skimr)
library(nnet)
library(dplyr)
library(effsize)
library(ggplot2)


data2008 <- read_csv("/Users/juliustyson/Documents/Internship/FARS2020NationalAuxiliaryCSV/FARS2008NationalCSV/vehicle.csv")
data2009 <- read_csv("/Users/juliustyson/Documents/Internship/FARS2020NationalAuxiliaryCSV/FARS2009NationalCSV/vehicle.csv")
data2010 <- read_csv("/Users/juliustyson/Documents/Internship/FARS2020NationalAuxiliaryCSV/FARS2010NationalCSV/vehicle.csv")
data2011 <- read_csv("/Users/juliustyson/Documents/Internship/FARS2020NationalAuxiliaryCSV/FARS2011NationalCSV/vehicle.csv")
data2012 <- read_csv("/Users/juliustyson/Documents/Internship/FARS2020NationalAuxiliaryCSV/FARS2012NationalCSV/vehicle.csv")
data2013 <- read_csv("/Users/juliustyson/Documents/Internship/FARS2020NationalAuxiliaryCSV/FARS2013NationalCSV/vehicle.csv")
data2014 <- read_csv("/Users/juliustyson/Documents/Internship/FARS2020NationalAuxiliaryCSV/FARS2014NationalCSV/vehicle.csv")
data2015 <- read_csv("/Users/juliustyson/Documents/Internship/FARS2020NationalAuxiliaryCSV/FARS2015NationalCSV/vehicle.csv")

data2016 <- read_csv("/Users/juliustyson/Documents/Internship/FARS2020NationalAuxiliaryCSV/FARS2016NationalCSV/vehicle.csv")
data2017 <- read_csv("/Users/juliustyson/Documents/Internship/FARS2020NationalAuxiliaryCSV/FARS2017NationalCSV/vehicle.csv")
data2018 <- read_csv("/Users/juliustyson/Documents/Internship/FARS2020NationalAuxiliaryCSV/FARS2018NationalCSV/vehicle.csv")
data2019 <- read_csv("/Users/juliustyson/Documents/Internship/FARS2020NationalAuxiliaryCSV/FARS2019NationalCSV/vehicle.csv")
data2020 <- read_csv("/Users/juliustyson/Documents/Internship/FARS2020NationalAuxiliaryCSV/FARS2020NationalCSV/vehicle.csv")
data2021 <- read_csv("/Users/juliustyson/Documents/Internship/FARS2020NationalAuxiliaryCSV/FARS2021NationalCSV/vehicle.csv")

# data2008 <- mutate(data2008, TRLR2VIN = as.character(TRLR2VIN))
# data2009 <- mutate(data2009, TRLR2VIN = as.character(TRLR2VIN))
# data2010 <- mutate(data2010, TRLR2VIN = as.character(TRLR2VIN))
# data2011 <- mutate(data2011, TRLR2VIN = as.character(TRLR2VIN))
# data2012 <- mutate(data2012, TRLR2VIN = as.character(TRLR2VIN))
# data2013 <- mutate(data2013, TRLR2VIN = as.character(TRLR2VIN))
# data2014 <- mutate(data2014, TRLR2VIN = as.character(TRLR2VIN))
# data2015 <- mutate(data2015, TRLR2VIN = as.character(TRLR2VIN))

data2016 <- mutate(data2016, TRLR2VIN = as.character(TRLR2VIN))
data2017 <- mutate(data2017, TRLR2VIN = as.character(TRLR2VIN))
data2018 <- mutate(data2018, TRLR2VIN = as.character(TRLR2VIN))
data2019 <- mutate(data2019, TRLR2VIN = as.character(TRLR2VIN))
data2020 <- mutate(data2020, TRLR2VIN = as.character(TRLR2VIN))
data2021 <- mutate(data2021, TRLR2VIN = as.character(TRLR2VIN))


# Combine all datasets into one
data_raw <- bind_rows(
  data2008 %>% mutate(year = 2008),
  data2009 %>% mutate(year = 2009),
  data2010 %>% mutate(year = 2010),
  data2011 %>% mutate(year = 2011),
  data2012 %>% mutate(year = 2012),
  data2013 %>% mutate(year = 2013),
  data2014 %>% mutate(year = 2014),
  data2015 %>% mutate(year = 2015),
  data2016 %>% mutate(year = 2016),
  data2017 %>% mutate(year = 2017),
  data2018 %>% mutate(year = 2018),
  data2019 %>% mutate(year = 2019),
  data2020 %>% mutate(year = 2020),
  data2021 %>% mutate(year = 2021)
)

MA_data <- data_raw %>%
  filter(STATENAME == "Massachusetts") %>%
  relocate(SPEEDREL, SPEEDRELNAME, TRAV_SP, TRAV_SPNAME, DEATHS) %>%
  filter(TRAV_SP < 998, TRAV_SP > 0, SPEEDREL < 9) %>%
  mutate(TRAV_SP = ifelse(TRAV_SP < 5, 5, 5 * ceiling(TRAV_SP / 5)))

ggplot(MA_data, aes(x = TRAV_SP, y = DEATHS)) +
  geom_bar(stat = "identity", fill = "#333333", width = 4) +  # Adjust width as needed
  labs(title = "Deaths by Travel Speed",
       x = "Travel Speed",
       y = "Number of Deaths") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightgray"))


# REQUIRES rounded TRAV_SP
MA_data_proportions <- MA_data %>%
  group_by(TRAV_SP) %>%
  summarize(Proportion = mean(DEATHS > 0, na.rm = TRUE))

ggplot(MA_data_proportions, aes(x = TRAV_SP, y = Proportion)) +
  geom_bar(stat = "identity", fill = "blue", width = 4) +
  labs(title = "Proportion of Deaths by Travel Speed",
       x = "Travel Speed (rounded to nearest multiple of 5)",
       y = "Proportion of Deaths") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percentage
  theme_minimal()



#---------------------------------STACKED PROP---------------------------------
MA_data_proportions <- MA_data %>%
  mutate(SPEEDREL_category = ifelse(SPEEDREL == 0, "Speed 0", "Speed > 0")) %>%
  group_by(TRAV_SP) %>%
  summarize(Proportion = mean(DEATHS > 0, na.rm = TRUE),
            Proportion_speed0 = mean(DEATHS > 0 & SPEEDREL == 0, na.rm = TRUE),
            Proportion_speedgt0 = mean(DEATHS > 0 & SPEEDREL > 0, na.rm = TRUE))


#BOTH
ggplot(MA_data_proportions, aes(x = TRAV_SP)) +
  geom_bar(aes(y = Proportion_speed0, fill = "Not reported speeding"), stat = "identity", width = 4, position = position_stack(reverse = TRUE)) +
  geom_bar(aes(y = Proportion_speedgt0, fill = "Reported speeding"), stat = "identity", width = 4, position = position_stack(reverse = TRUE)) +
  labs(title = "Percentage of Pedestrian Crashes Resulting in Death",
       x = "Travel Speed",
       y = "") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Format y-axis as percentage
  scale_fill_manual(values = c("Not reported speeding" = "#333333", "Reported speeding" = "darkred"), name = "") +  # Set colors for the bars and add legend title
  theme_minimal() +
  theme(legend.position = "bottom") + # Place the legend below the graph
  theme(panel.background = element_rect(fill = "lightgray"))


#ONLY SPEED == 0
ggplot(MA_data_proportions, aes(x = TRAV_SP)) +
  geom_bar(aes(y = Proportion_speed0, fill = "Speed 0"), stat = "identity", width = 4) +
  #geom_bar(aes(y = Proportion_speedgt0, fill = "Speed > 0"), stat = "identity", width = 4) +
  labs(title = "Proportion of Deaths by Travel Speed and Speed Relation",
       x = "Travel Speed (rounded to nearest multiple of 5)",
       y = "Proportion of Deaths") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percentage
  scale_fill_manual(values = c("Speed 0" = "blue", "Speed > 0" = "red"), name = "Speed Relation") +  # Set colors for the bars
  theme_minimal()

#---------------------------------STACKED MEAN---------------------------------
MA_data_avg_deaths <- MA_data %>%
  group_by(TRAV_SP) %>%
  summarize(AvgDeaths = mean(DEATHS, na.rm = TRUE))

ggplot(MA_data_avg_deaths, aes(x = TRAV_SP, y = AvgDeaths)) +
  geom_bar(stat = "identity", fill = "#333333", width = 4) +
  labs(title = "Average Deaths by Travel Speed",
       x = "Travel Speed",
       y = "Average Deaths") +
  theme_minimal() + 
  theme(panel.background = element_rect(fill = "lightgray"))


ggplot(MA_data_proportions, aes(x = TRAV_SP, y = 1, fill = Proportion)) +
  geom_tile() +
  labs(title = "Heatmap of Deaths by Travel Speed and Speed Relation",
       x = "Travel Speed (rounded to nearest multiple of 5)",
       y = "") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())



