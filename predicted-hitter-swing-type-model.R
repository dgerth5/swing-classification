library(catboost)
library(tidyverse)
library(readr)
library(RColorBrewer)

model_df <- read_csv("swing_metrics.csv")
fg <- read_csv("fangraphs-leaderboards (70).csv") # this is just to get savant id's for hitters later

batter_savant_id <- fg %>%
  select(MLBAMID, Name)

model_df <- left_join(batter_savant_id, 
                      model_df, 
                      by = c("MLBAMID"= "batter"))

model_df$count <- as.factor(model_df$count)
model_df$pitch_name <- as.factor(model_df$pitch_name)
model_df$pred_swing_type <- as.factor(model_df$pred_swing_type)
model_df$MLBAMID <- as.factor(model_df$MLBAMID)

# create pool
train_pool <- catboost.load_pool(data = model_df[, c("count", "plate_x", "plate_z", "MLBAMID")],
                                 label = as.numeric(model_df$pred_swing_type))

# list parameters (placeholders for now)
params <- list(loss_function = "MultiClass",
               eval_metric = "MultiClass",
               iterations = 500,
               verbose = 100)

# train model
# cat_model <- catboost.train(learn_pool = train_pool,
#                             params = params)
# 
# write_rds(cat_model, "swing_class_cat_model.RDS")

cat_model <- readRDS("swing_class_cat_model.RDS")
