# 2024-11-20

libraries <- c("readr", "dplyr", "ggplot2", "ranger", "caret", "patchwork")
lapply(libraries, require, character.only = T)
remove(libraries)

df <- read_csv("source_data/2024-11-12-clay_silt_sand.csv")
#

# Tests -------------------------------------------------------------------

set.seed(1)

cv_control_object <- trainControl(
  method = "cv", number = 10,
  returnResamp = 'all', savePredictions = 'all')

covariables <- df %>% select(-c("id", "clay")) %>% names()

model <- train(
  as.formula(paste("clay", "~",
                   paste(covariables, collapse = '+'))),
  data = df,
  method = "ranger",
  keep.inbag = TRUE,
  num.trees = 100,
  replace = TRUE,
  importance = "permutation",
  trControl = cv_control_object,
  tuneGrid = expand.grid(
    mtry = 2,
    min.node.size = 5,
    splitrule = "variance")
)

# Out-of-bag predictions for each sample:
oob <- environment(model[["modelInfo"]][["oob"]])[["model"]][["pred"]] %>%
  arrange(rowIndex)

ranger_model <- model[["finalModel"]]

predict_w_all_trees <- predict(
  ranger_model, df, predict.all = TRUE)$predictions # has all trees and all samples

predict_w_all_trees <- predict_w_all_trees %>% as_tibble() %>% 
  mutate(estm_mean = apply(., 1, FUN = mean),
         estm_median = apply(., 1, FUN = median),
         .before = 1)

predic_w_all_trees <- bind_cols(predict_w_all_trees, df)

# Solução 1 ---------------------------------------------------------------

plot1 <- oob %>% 
  ggplot(aes(obs, pred))+
  geom_point(size = 1, shape = 21, alpha = 0.3)+
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 0.5)+
  labs(y = "Clay (%) OOB pred", x = "Clay (%) obs")+
  scale_x_continuous(limits = c(0, 100))+scale_y_continuous(limits = c(0, 100))+
  theme_classic()+
  guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both")) |
  predic_w_all_trees %>% 
    ggplot(aes(clay, estm_mean))+
    geom_point(size = 1, shape = 22, alpha = 0.3)+
    geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 0.5)+
    labs(y = "MEAN pred clay (%) from trees", x = "Clay (%) obs")+
    scale_x_continuous(limits = c(0, 100))+scale_y_continuous(limits = c(0, 100))+
    theme_classic()+
    guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both")) |
    predic_w_all_trees %>% 
      ggplot(aes(clay, estm_median))+
      geom_point(size = 1, shape = 24, alpha = 0.3)+
      geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 0.5)+
      labs(y = "MEDIAN pred clay (%) from trees", x = "Clay (%) obs")+
      scale_x_continuous(limits = c(0, 100))+scale_y_continuous(limits = c(0, 100))+
      theme_classic()+
      guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both"))

ggsave("./project_products/2024-11-20-pred_clay_from_trees.png",
       plot1, 
       width = 30, height = 10, units = "cm", dpi = 600)

# Solução 2 ---------------------------------------------------------------

# Initialize storage for out-of-bag predictions per tree
n_samples <- nrow(df)
n_trees <- ranger_model$num.trees
oob_predictions <- matrix(NA, nrow = n_samples, ncol = n_trees)

# Loop through trees and calculate out-of-bag predictions
for (tree_idx in seq_len(n_trees)) {
  # Extract inbag indicators for the current tree
  inbag <- ranger_model$inbag.counts[[tree_idx]]
  
  # Identify out-of-bag samples for this tree
  oob_idx <- which(inbag == 0)
  
  # Predict for the OOB samples using this tree
  tree_pred <- predict(ranger_model, data = df[oob_idx, ], predict.all = TRUE)
  oob_predictions[oob_idx, tree_idx] <- tree_pred$predictions[oob_idx]
  
}

# Aggregate tree predictions using median
final_oob_prediction <- apply(oob_predictions, 1, function(x) median(x, na.rm = TRUE))

oob_trees_prediction <- bind_cols(df, final_oob_prediction)
oob_trees_prediction <- oob_trees_prediction %>% rename(pred = 8)

plot2 <- oob_trees_prediction %>% 
  ggplot(aes(clay, pred))+
  geom_point(size = 2, shape = 24, alpha = 0.3, fill = "#004567")+
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 0.5)+
  labs(y = "OOB MEDIAN pred clay (%) from trees", x = "Clay (%) obs")+
  scale_x_continuous(limits = c(0, 100))+scale_y_continuous(limits = c(0, 100))+
  theme_classic()+
  guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both"))

ggsave("./project_products/2024-11-20-oob_median_pred_clay_from_inbag_trees.png",
       plot2, 
       width = 10, height = 10, units = "cm", dpi = 600)
