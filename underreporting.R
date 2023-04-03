setwd("/Users/sdong217/Desktop/COVID_NH")
data = read.csv("estimates.csv")

sum(data$infections)/sum(data$diagnoses)

data_2023 = data[substr(data$date,1,4)=="2023",]

underreport_df = data.frame(state=unique(data_2023$state), underreport_factor = rep(0,times=50))

states = unique(data_2023$state)
for(index in 1:length(states)){
  state = states[index]
  underreport_df$underreport_factor[index] = sum(data_2023[data_2023$state==state,]$infections)/sum(data_2023[data_2023$state==state,]$diagnoses)
}
