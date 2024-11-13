# Part 1

# Dataframe loading
control <- c()
jumpingjacks <- c()
armsup <- c()
both <- c()
breath_df <- data.frame(
    time = c(control, jumpingjacks, armsup, both),
    jumpingjacks = c(
        rep("no", length(control)),
        rep("yes", length(jumpingjacks)),
        rep("no", length(armsup)),
        rep("yes", length(both))),
    armsup = c(
        rep("no", length(control) + length(jumpingjacks)),
        rep("yes", length(armsup) + length(both)))
)

# Plots
library(ggplot2)
library(dplyr)
ggplot(
        data=breath_df, 
        mapping=aes(
            x=<df_feature>, y=<df_feature>, fill=<df_feature>)) 
    + geom_boxplot() 
    + ggtitle("<Fill_In>")
tmp <- breath_df 
    %>% group_by(<df_feature>, <df_feature>) 
    %>% summarize(mean=mean(time))

# interaction line plot
ggplot(
        data=tmp, 
        aes(
            y=mean, x=<df_feature>, 
            group=<df_feature>, color=<df_feature>)) 
    + geom_point() 
    + geom_line() 
    + ggtitle("Fill_In") 
    + ylab("mean time")

sum_stats <- breath_df 
    %>% group_by(<df_feature>, <df_feature>) 
    %>% summarize(mean=mean(time), sd=sd(time), n=n())
colnames(sum_stats) <- c("Jumping Jacks", "Arms Up", "mean", "sd", "n")
kable(sum_stats)

# Insert the formal test
# i.e. 
# H_o: /beta_1 = /beta_2 = /beta_3 = 0
# H_a: ...
# Each /beta_i represents a regression coefficient from the LM 
# y = /beta_0 + /beta_1*x_1 + /beta_2*x_2 + /beta_3*x_1*x_2
# where x_1 = 0 or 1 depending on the jumping jacks condition 
# and x_2 = 0 or 1 depending on the arms up condition


# Linear model and p-value
model1 <- lm(
    time ~ <df_feature> + <df_feature> + <df_feature_interaction_term>, 
    data=breath_df)
output <- summary(model1)$coefficients
# Optional Formatting for Table output
rownames(output) <- c(
    "(Intercept)", "<df_feature>", "<df_feature>", "<df_feature_interaction_term>")
kable(output)

f <- summary(model1)$fstatistic
p_val <- pf(f[1], f[2], f[3], lower.tail=FALSE)

# Part 2