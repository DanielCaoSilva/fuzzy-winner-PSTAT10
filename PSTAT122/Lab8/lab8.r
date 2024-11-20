source(power_factorial_23.r)

for(p_i in 1: 3){
    beta_mean <-c(5, rep(0.5, 7)
    beta_se <- c(rep(1,8), rep(0.75,8), rep(1.5,8))
    power_i <- NA
    power <- NA
    replicates <- 2:10
    
    power1 <- NA
    for(i in 1:length(replicate)){
        power[i] <- power_factorial_23(beta_mean, beta_se[p_i], replicates[i])
    }
    power_i[p_i] <- power 
}


all_power <- data.frame(
    power = power_i
    beta_se = c(rep("1", length(power_i[1]),
        rep("0.75", length(power_i[2])),
        rep("1.5", length(power_i[3]))),
    replicates = rep(replicates, 3))
)
ggplot(data=all_power, mapping=aes(x=replicates, y=power,
    group=beta_se, color=beta_se))+
    geom_point()+geom_line()
    