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
