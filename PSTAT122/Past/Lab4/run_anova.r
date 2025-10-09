library(tidyverse)
run_anova <- function(dataframe){
  
  # rename columns 
  colnames(dataframe) <- c("y", "x")
  
  # get the pieces for SSE
  sum_stats <- dataframe %>% 
    group_by(x) %>% 
    summarize(Si = <function>(y), 
              ybar_i = <function>(y),
              n = n())
  
  SSE <- sum(sum_stats$Si * (sum_stats$n - 1))
  
  # Get df2
  a <- dim(sum_stats)[1]
  N <- dim(dataframe)[1]
  df2 <- N-a
  
  # Calculate MSE
  MSE <- <variable>/<variable>
  
  # Get SSE
  y_ddot <- <function>(dataframe$y)
  
  # using n from sum_stats allows for unbalanced datasets
  SST <- sum(sum_stats$n*(sum_stats$ybar_i - y_ddot)^2)
  df1 <- (a-1)
  MST <- SST/df1
  
  F <- <variable> / <variable>
  
  pval <- pf(F, df1, df2, lower.tail=FALSE)
  
  results <- matrix(c(df1, SST, MST, F, pval,
                      df2, SSE, MSE, NA, NA), nrow=2, byrow=TRUE)
  
  colnames(results) <- c("df", "Sum Sq", "Mean Sq", "F", "p-value")
  rownames(results) <- c("method", "Residuals")
  
  return(results)
}

# Part b - Loading the data (missing the function call and kable stuff)
google <- c(46, 49, 51, 42)
waze <- c(44, 47, 47, 43)
gut <- c(50, 51, 45, 43)
commute_df <- data.frame(
  time = c(google, waze, gut),
  method = c(rep("google", 4),
    rep("waze", 4),
    rep("gut",4))
)

# Part c - Loading the data (missing the function calls and kable stuff)
t_100 <- c(21.8, 21.9, 21.7, 21.6, 21.7)
t_125 <- c(21.7, 21.4, 21.5, 21.4)
t_150 <- c(21.9, 21.8, 21.8, 21.6, 21.5)
t_175 <- c(21.9, 21.7, 21.8, 21.4)
brick_df <- data.frame(
  density = c(t_100, t_125, t_150, t_175),
  temp = c(rep("100", 5),
    rep("125", 4),
    rep("150", 5),
    rep("175", 4))
)