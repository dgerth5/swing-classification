library(readr)
library(tidyverse)
library(mclust)

statcast_data24_2 <- read_csv("C:/Users/david/Downloads/statcast_data24_2.csv")

swing_metrics <- statcast_data24_2 %>%
  mutate(attack_direction = if_else(stand == "R", attack_direction, attack_direction * -1),
         count = paste0(balls,"-",strikes)) %>%
  select(batter, plate_x, plate_z, count, pitch_name, attack_angle, attack_direction, swing_path_tilt, bat_speed, swing_length) %>%
  drop_na(attack_angle, attack_direction, swing_path_tilt, bat_speed, swing_length)

swing_scaled <- swing_metrics %>%
  select(attack_angle, attack_direction, swing_path_tilt, bat_speed, swing_length) %>%
  scale() %>% 
  as.data.frame()

swing_classification_model <- Mclust(swing_scaled, G = 10)
summary(swing_classification_model)

#saveRDS(swing_classification_model, "swing_classification_model.RDS")
