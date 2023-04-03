setwd("/Users/sdong217/Desktop/COVID_NH/NH_CMS_data/raw")
nh_deaths_data = read.csv("faclevel_2023.csv")
us_deaths_data = read.csv("us_weekly_deaths.csv")
us_deaths23_data = us_deaths_data[which(substring(us_deaths_data["Date"][,],10,11)=="23"),]

nh_deaths = sum(nh_deaths_data$Residents.Weekly.COVID.19.Deaths, na.rm=T)
us_deaths = sum(us_deaths23_data$Weekly.Deaths, na.rm=T)

