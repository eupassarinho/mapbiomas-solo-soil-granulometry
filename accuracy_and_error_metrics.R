
## functions for the validation statistics

my_summary_metrics <- function(data, lev = NULL, model = NULL) {
  
  regression_eval <- function(pred, obs){
    
    # mean error
    ME <- round(mean(pred - obs, na.rm = TRUE), digits = 4)
    
    # mean square error
    MSE <-   round(mean((pred - obs)^2, na.rm = TRUE), digits = 4)
    
    # mean absolute error
    MAE <-   round(mean(abs(pred - obs), na.rm = TRUE), digits = 4)
    
    # root mean square error
    RMSE <-   round(sqrt(mean((pred - obs)^2, na.rm = TRUE)), digits = 4)
    
    # Pearson's correlation squared
    r2 <-  round((cor(pred, obs, method = 'spearman', use = 'pairwise.complete.obs')^2), digits = 4)
    
    # Nash–Sutcliffe model efficiency coefficient
    #NSE <- round(hydroGOF::NSE(sim = pred, obs = obs), digits = 4)
    NSE <- round(
      (1-
         (sum((obs - pred)**2, na.rm = TRUE)/
            sum((obs - mean(obs, na.rm = TRUE))**2, 
                na.rm = TRUE))), 
      digits = 4)
    
    # Lin's concordance correlation coefficient
    CCC <- round(yardstick::ccc_vec(truth = obs, estimate = pred), digits = 4)
    
    # Willmott's concordance index
    #d <- round(hydroGOF::d(sim = pred, obs = obs), digits = 4)
    d <- round(
      (1 - 
         (sum((obs - pred)**2, na.rm = TRUE)/
            sum((abs(pred - mean(obs, na.rm = TRUE)) + abs(obs - mean(obs)))**2))),
      digits = 4)
    
    out <- c(ME, MAE, MSE, RMSE, NSE, r2, CCC, d)
    names(out) <- c("ME", "MAE", "MSE", "RMSE", "NSE", "Rsquared", "CCC", "d")
    
    if (any(is.nan(out))) 
      out[is.nan(out)] <- NA
    out
    
  }
  
  if (is.character(data$obs)) 
    data$obs <- factor(data$obs, levels = lev)
  regression_eval(data[, "pred"], data[, "obs"])
  
}