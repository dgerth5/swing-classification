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

model_df <- model_df %>% drop_na()

model_df$count <- as.factor(model_df$count)
model_df$pitch_name <- as.factor(model_df$pitch_name)
model_df$pred_swing_type <- as.factor(model_df$pred_swing_type)
model_df$MLBAMID <- as.factor(model_df$MLBAMID)

# create pool
train_pool <- catboost.load_pool(data = model_df[, c("count","plate_x", "plate_z", "MLBAMID")],
                                 label = as.numeric(model_df$pred_swing_type))

# list parameters (placeholders for now)
params <- list(loss_function = "MultiClass",
               eval_metric = "MultiClass",
               iterations = 500,
               verbose = 100,
               od_type = "Iter",     # early stopping
               od_wait = 50,         # stop if no improvement for 50 iterations
               verbose = 100)         # print progress every 100 iterations


# train model
cat_model <- catboost.train(learn_pool = train_pool,
                            params = params)

# get predictions on the training set
train_preds <- catboost.predict(cat_model, train_pool, prediction_type = "Class")

# compute accuracy
true_labels <- as.numeric(model_df$pred_swing_type)-1
accuracy <- mean(train_preds == true_labels)

print(paste("In-sample accuracy:", round(accuracy, 4)))

write_rds(cat_model, "swing_class_cat_model.RDS")

cat_model <- readRDS("swing_class_cat_model.RDS")


# plot example

factor_name_df <- model_df %>%
  select(Name, MLBAMID) %>%
  distinct()

grid <- expand.grid(
  plate_x = seq(-1, 1, length.out = 50),
  plate_z = seq(0.5, 4.5, length.out = 50),
  count = unique(model_df$count)[-13]
)

# Bobby Witt
grid$MLBAMID <- factor_name_df$MLBAMID[factor_name_df$Name == "Aaron Judge"]

grid_pool <- catboost.load_pool(data = grid)

grid$pred_class <- catboost.predict(cat_model, grid_pool, prediction_type = "Class") + 1


ggplot(grid, aes(x = plate_x, y = plate_z, fill = factor(pred_swing_type))) +
  geom_tile(alpha = 0.85) +
  facet_wrap(~count, ncol = 4, labeller = labeller(count = function(x) paste("Count:", x))) +
  scale_fill_brewer(
    type = "qual", 
    palette = "Paired", 
    name = "Predicted\nSwing Type",
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      ncol = 1
    )
  ) +
  geom_rect(
    data = sz,
    aes(xmin = -0.705, xmax = 0.705, ymin = bot_sz, ymax = top_sz),
    fill = NA, 
    color = "#2c3e50", 
    linewidth = 1.5, 
    inherit.aes = FALSE,
    linetype = "solid"
  )
