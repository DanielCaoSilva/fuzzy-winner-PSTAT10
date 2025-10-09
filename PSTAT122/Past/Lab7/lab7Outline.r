##### Lab 7 - PSTAT 122 #####

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

# Methods
# <Insert Methods description>

# Results
# <Insert Results description>

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

#<Insert p-value analysis>

# Discussion
#<Insert Discussion Section>

# Part 2
# 1)
# df loading
approach1 <- c(1000, 1500, 1200, 1800, 1600, 1100, 1000, 1250)
approach2 <- c(1500, 1800, 2000, 1200, 2000, 1700, 1800, 1900)
approach3 <- c(900, 1000, 1200, 1500, 1200, 1550, 1000, 1100)
donation <- data.frame(
    contribution = c(approach1, approach2, approach3),
    approach = c(rep("1", 8), rep("2", 8), rep("3", 8))
)
# a) run aov on the data to check for differences in the mean contribution and save p-value
# b) run lm and check p-value consistency with ANOVA

# 2)
# df load
circuit <- data.frame(
    thickness = c(
        14.037, 16.165, 13.972, 13.907,
        13.880, 13.860, 14.032, 13.914,
        14.821, 14.757, 14.843, 14.878,
        14.888, 14.921, 14.415, 14.932),
    A = rep(c(rep("low", 4), rep("high", 4)), 2),
    B = c(rep("low", 8), rep("high", 8))
)
# a) Estimate the factor effects (hint: use `lm`)
# b) Report the p-value from `lm` and write about conclusions 
#    and evaluate the individual coefficient p-values 
#    (hint: use `summary(<model_name>)$fstatistic`)
# c) Write down a regression equation similar to what was seen in lecture 14 slide 26