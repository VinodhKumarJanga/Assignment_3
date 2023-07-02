install.packages("ggplot2")
library("ggplot2")
install.packages("dplyr")
library("dplyr")
install.packages("stringr")
library("stringr")
install.packages("tidyverse")
library("tidyverse")

ds <- read.csv("/Users/vinodhjanga/Downloads/StormEvents_details-ftp_v1.0_d2005_c20220425.csv")

head(ds)

limit_ds <- ds %>% select(BEGIN_YEARMONTH, EPISODE_ID, STATE, STATE_FIPS, CZ_NAME, CZ_TYPE, CZ_FIPS, EVENT_TYPE)

#Arrange the data by the state name (STATE)
arrange_ds <- limit_ds %>% arrange(STATE)

#Change state and county names to title case (e.g., “New Jersey” instead of “NEW JERSEY”)
change_ds <- arrange_ds %>% mutate(STATE = str_to_title(STATE), CZ_NAME = str_to_title(CZ_NAME))

filtere_ds <- change_ds %>% filter(CZ_TYPE == "C") %>% select(-CZ_TYPE)

pad_ds <- filtere_ds %>% mutate(STATE_FIPS = str_pad(STATE_FIPS, width = 2, pad = "0"), 
                                CZ_FIPS = str_pad(CZ_FIPS, width = 3, pad = "0")) %>% 
  unite(FIPS, STATE_FIPS, CZ_FIPS, sep = "")

rename_ds <- pad_ds %>% rename_all(tolower)

data("state")

state_ds <- data.frame(
  state_name = state.name,
  area = state.area,
  region = state.region
)

year <- 2005 

filtered_year_ds <- pad_df %>% filter(substr(BEGIN_YEARMONTH, 1, 4) == year)

events_per_state <- filtered_year_ds %>%
  group_by(STATE) %>%
  summarise(events = n())

merged_ds <- merge(events_per_state, state_df, by.x = "STATE", by.y = "state_name", all.x = TRUE)

scatter_plot <- ggplot(merged_df, aes(x = area, y = events)) +
  geom_point(aes(color = region)) +
  labs(x = "State Area", y = "Number of Events") +
  ggtitle("Scatter Plot of State Area vs. Number of Events")

scatter_plot

