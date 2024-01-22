setwd("/Users/sdong217/Desktop/COVID_NH/NH synthpop/raw")
data = read.csv("ltc_state_2018.csv")

mean(data$avg_obs_medianlos_sta, na.rm=T)
