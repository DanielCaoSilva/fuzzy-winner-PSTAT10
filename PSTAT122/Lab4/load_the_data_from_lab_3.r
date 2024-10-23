mt1 <- c(3129, 3000, 2865, 2890)
mt2 <- c(3200, 3300, 2975, 3150)
mt3 <- c(2800, 2900, 2985, 3050)
mt4 <- c(2600, 2700, 2600, 2765)
mt_df <- data.frame(
    strength = c(mt1, mt2, mt3, mt4),
    mt = c(rep("1", length(mt1)),
        rep("2", length(mt2)),
        rep("3", length(mt3)),
        rep("4", length(mt4)))
)