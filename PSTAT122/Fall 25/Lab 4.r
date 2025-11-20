

# Record the data in a dataframe

control <- c()
jumpingjacks <- c()
armsup <- c()
both <- c()
breath_df <- data.frame(
    time = c(control, jumpingjacks, armsup, both),
    jumpingjacks = c(rep("no", length(control)),
        rep("yes", length(jumpingjacks)),
        rep("no", length(armsup)),
        rep("yes", length(both))),
    armsup = c(rep("no", length(control) + length(jumpingjacks)),
        rep("yes", length(armsup) + length(both)))
)

# Iteraction Plot
library(ggplot2)
ggplot(data=breath_df, mapping=aes(x=jumpingjacks, y=time, fill=armsup)) +
geom_boxplot() + ggtitle("Length of time holding breath")
library(dplyr)
tmp <- breath_df %>%
group_by(jumpingjacks, armsup) %>%
summarize(mean=mean(time))
# make line interaction plot
ggplot(data=tmp, aes(y=mean, x=jumpingjacks,
    group=armsup, color=armsup)) +
geom_point() + geom_line() +
    ggtitle("Length of time holding breath") +
    ylab("mean time")

# Summary Stats
sum_stats <- breath_df %>%
group_by(jumpingjacks, armsup) %>%
summarize(mean=mean(time), sd=sd(time), n=n())
colnames(sum_stats) <- c("Jumping Jacks", "Arms Up", "mean", "sd", "n")
kable(sum_stats)

# Linear Model
model1 <- lm(time ~ jumpingjacks + armsup + jumpingjacks*armsup, data=breath_df)
output <- summary(model1)$coefficients
# let's make the row names of the table look a little nicer
rownames(output) <- c("(Intercept)", "Jumping Jacks", "Arms Up", "JJ/AU Interaction")
kable(output)

# Results - P-value
f <- summary(model1)$fstatistic
p_val <- pf(f[1], f[2], f[3], lower.tail=FALSE)