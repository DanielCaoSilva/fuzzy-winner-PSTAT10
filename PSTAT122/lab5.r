set.seed(10282024)
norm_data1 <- rnorm(n=100, mean=2, sd=4)

hist(norm_data1)

norm_data2 <- rnorm(n=100, mean=2.5, sd=4)
norm_data3 <- rnorm(n=100, mean=3, sd=4)

norm_data_all <- data.frame(
    y = c(norm_data1, norm_data2, norm_data3),
    x = c(rep("group1", 100),
          rep("group2", 100),
          rep("group3", 100))
)

# plotting the different realizations from the same distributions
library(ggplot2)
ggplot(data=norm_data_all, mapping = aes(x=x, y=y)) + 
    geom_boxplot()
# they look similar so we need to run an ANOVA to see if there is a difference in means

model1 <- aov(y ~ x, data=norm_data_all)
summary(model1)

# What happened after running the ANOVA to detect the difference in means?
# Is this a Type 1 error, a Type 2 error, or neither?










# if the p-value is less than 0.05, then we reject the null hypothesis leading to a Type 1 error 
# if there was not a true difference in means. 
# If the p-value is greater than 0.05, then we fail to reject the null hypothesis
# leading to a Type 2 error if there was a true difference in means.





# but since there is none then it is neither


# Estimating Power and the probability of a Type 2 error
reps <- 1000
pvals <- NA

for(i in 1:reps){
    norm_data1 <- rnorm(n=100, mean=2, sd=4)
    norm_data2 <- rnorm(n=100, mean=2.5, sd=4)
    norm_data3 <- rnorm(n=100, mean=3, sd=4)
    
    norm_data_all <- data.frame(
        y = c(norm_data1, norm_data2, norm_data3),
        x = c(rep("group1", 100),
              rep("group2", 100),
              rep("group3", 100))
    )
    
    model1 <- aov(y ~ x, data=norm_data_all)
    pvals[i] <- summary(model1)[[1]][1,5]
}
# The probability of making a Type 2 error in this setup
sum(pvals >= 0.05)/reps
# The ANOVA estimate of the statistical power
sum(pvals < 0.05)/reps
# typically we want a power of at least 80% or so. This is low because the true diff in means
# is actually very small