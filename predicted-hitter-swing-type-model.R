library(catboost)
library(tidyverse)
library(readr)
library(RColorBrewer)

# load data, convert to factors
model_df <- read_csv("swing_metrics.csv")

model_df$count <- as.factor(model_df$count)
model_df$pitch_name <- as.factor(model_df$pitch_name)
model_df$pred_swing_type <- as.factor(model_df$pred_swing_type)
model_df$batter <- as.factor(model_df$batter)

# create pool
train_pool <- catboost.load_pool(data = model_df[, c("count", "plate_x", "plate_z", "batter")],
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

# Example plot

# strike zone parameters by data frame
sz_params <- model_df %>% 
  group_by(batter) %>%
  summarise(top_sz = mean(sz_top, na.rm=TRUE),
            bot_sz = mean(sz_bot, na.rm=TRUE))

# parameters
batter_id <- model_df$batter[367]  # Luis Arraez
sz_top <- sz_params$top_sz[216]
sz_bot <- sz_params$bot_sz[216]
sz_left <- -0.705
sz_right <- 0.705

# counts
counts_for_batter <- unique(model_df$count)[-13]

# create prediction grid
plate_x_seq <- seq(-1, 1, length.out = 100) 
plate_z_seq <- seq(0.5, 4.5, length.out = 100)

grid <- expand.grid(plate_x = plate_x_seq,
                    plate_z = plate_z_seq,
                    count = counts_for_batter)
grid$batter <- batter_id

grid_pool <- catboost.load_pool(data = grid)
grid$pred_swing_type <- catboost.predict(cat_model, grid_pool, prediction_type = "Class")

# adjustments so that all swing type colors are show up (want to find a better workaround for this)
all_levels <- as.character(0:9)
grid$pred_swing_type <- factor(as.character(grid$pred_swing_type), levels = all_levels)

swing_colors <- setNames(brewer.pal(10, "Paired"), all_levels)
missing_levels <- setdiff(all_levels, unique(as.character(grid$pred_swing_type)))
if (length(missing_levels) > 0) {
  dummy <- data.frame(plate_x = 10,     # out of frame
                      plate_z = 10,     # out of frame
                      count = counts_for_batter[1],  
                      batter = batter_id,
                      pred_swing_type = factor(missing_levels, levels = all_levels))
  grid <- rbind(grid, dummy)
}

# final plot
ggplot(grid, aes(x = plate_x, y = plate_z, fill = pred_swing_type)) +
  geom_tile(alpha = 0.8) +
  facet_wrap(~count, ncol = 3, nrow = 4) +
  scale_fill_manual(name = "Swing Type",
                    values = swing_colors,
                    breaks = all_levels,
                    drop = FALSE) +
  geom_rect(aes(xmin = sz_left, xmax = sz_right,
                ymin = sz_bot, ymax = sz_top),
            fill = NA, color = "black", linewidth = 1) +
  labs(title = "Predicted Swing Type for Luis Arraez",
       x = "Plate X", y = "Plate Z") +
  xlim(c(-1,1))+ylim(c(0,5))+
  theme_minimal()

