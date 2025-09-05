library(readr)
library(tidyverse)
library(mclust)
library(gt)

# Read Data
statcast_data24 <- read_csv("statcast_data24.csv")
fg <- read_csv("fangraphs-leaderboards (70).csv") # this is just to get savant id's for hitters later

# Normalize attack direction to be RHH facing
swing_metrics <- statcast_data24 %>%
  mutate(attack_direction = if_else(stand == "R", attack_direction, attack_direction * -1),
         count = paste0(balls,"-",strikes)) %>%
  select(batter, plate_x, plate_z, count, pitch_name, sz_top, sz_bot, attack_angle, attack_direction, swing_path_tilt, bat_speed, swing_length) %>%
  drop_na(attack_angle, attack_direction, swing_path_tilt, bat_speed, swing_length)

# Normalize swing data
swing_scaled <- swing_metrics %>%
  select(attack_angle, attack_direction, swing_path_tilt, bat_speed, swing_length) %>%
  scale() %>% 
  as.data.frame()

# Model
swing_classification_model <- Mclust(swing_scaled, G = 10)
summary(swing_classification_model)

#saveRDS(swing_classification_model, "swing_classification_model.RDS") # save model as RDS

swing_classification_model <- readRDS("swing_classification_model.RDS")

# Swing type averages
swing_metrics$pred_swing_type <- swing_classification_model[["classification"]]
swing_type_averages <- swing_metrics %>%
  group_by(pred_swing_type) %>%
  summarise(avg_aa = mean(attack_angle),
            avg_ad = mean(attack_direction),
            avg_spt = mean(swing_path_tilt),
            avg_bs = mean(bat_speed),
            avg_sl = mean(swing_length),
            n = n()) %>%
  arrange(-n)

swing_type_averages %>%
  mutate(per_swing = n / sum(n)) %>%
  select(-n) %>%
  gt() %>%
  tab_header(title = md("**Swing Classifications**"), subtitle = "Season 2024. Attack Direction normalized to be facing RHH") %>%
  fmt_number(c("avg_aa", "avg_ad", "avg_spt", "avg_bs", "avg_sl"), decimals = 1) %>%
  fmt_percent("per_swing", decimals = 0) %>%
  cols_label(pred_swing_type = "Swing Type",
             avg_aa = "Attack Angle",
             avg_ad = "Attack Direction",
             avg_spt = "VBA",
             avg_bs = "Bat Speed",
             avg_sl = "Swing Length",
             per_swing = "% Used")

# Hitter specific swing type summary
batter_swing_summary <- swing_metrics %>%
  group_by(batter, pred_swing_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = pred_swing_type,
              values_from = n,
              values_fill = 0) %>%
  select(batter, `1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`) %>%
  mutate(tot_swings = rowSums(across(`1`:`10`))) %>%
  mutate(across(`1`:`10`, ~ .x / tot_swings, .names = "pct_{col}"),
         n_types_over10 = rowSums(across(starts_with("pct_"), ~ .x > 0.1)),
         most_used_swing = across(starts_with("pct_")) %>% 
           apply(1, function(x) gsub("pct_", "", names(x)[which.max(x)]))) %>%
  select(batter, tot_swings, starts_with("pct_"), n_types_over10, most_used_swing)

batter_savant_id <- fg %>%
  select(MLBAMID, Name)

batter_swing_summary2 <- left_join(batter_savant_id, 
                                   batter_swing_summary, 
                                   by = c("MLBAMID"= "batter"))

write_csv(batter_swing_summary2, "batter_swing_summary.csv")
write_csv(swing_metrics, "swing_metrics.csv")
