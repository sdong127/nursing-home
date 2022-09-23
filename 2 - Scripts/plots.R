library(data.table)
library(ggplot2)

setwd("/Users/sdong217/Desktop/COVID_NH/NursingHome/nursing-home/2 - Scripts")
load("screen2xweek_lowvax_150.RData")

###################################INFECTIONS###################################

calc_infs = function(data = out){
  res = mean(data$res_tot)
  staff = mean(data$staff_tot)
  infs = res + staff
  
  return(infs)
}

com_inc_vec = c("50","100","150")

type_vec = rep(c("no screening", "weekly screening", "twice weekly screening"),times=3)

inf_vec = c(infs_50_noscreen,infs_50_screenweek,infs_50_screen2xweek,
            infs_100_noscreen,infs_100_screenweek,infs_100_screen2xweek,
            infs_150_noscreen,infs_150_screenweek,infs_150_screen2xweek)

create_df = function(com_inc = com_inc_vec, type = type_vec, infs = inf_vec){
  df = data.frame(com_inc=rep(com_inc,each=3), type=type, infs=infs)
  
  return(df)
}

# noscreen_lowvax = c(infs_0_noscreen, infs_20_noscreen, infs_40_noscreen, infs_60_noscreen,
#                     infs_80_noscreen, infs_100_noscreen, infs_120_noscreen, infs_140_noscreen,
#                     infs_160_noscreen, infs_180_noscreen, infs_200_noscreen)
# 
# screenweek_lowvax = c(infs_0_screenweek, infs_20_screenweek, infs_40_screenweek, infs_60_screenweek,
#                     infs_80_screenweek, infs_100_screenweek, infs_120_screenweek, infs_140_screenweek,
#                     infs_160_screenweek, infs_180_screenweek, infs_200_screenweek)
# 
# screen2xweek_lowvax = c(infs_0_screen2xweek, infs_20_screen2xweek, infs_40_screen2xweek, infs_60_screen2xweek,
#                       infs_80_screen2xweek, infs_100_screen2xweek, infs_120_screen2xweek, infs_140_screen2xweek,
#                       infs_160_screen2xweek, infs_180_screen2xweek, infs_200_screen2xweek)

ggplot(df, aes(x=com_inc, y=infs, col=type)) + 
  geom_point(size=2) + scale_color_manual(values=c("red","blue","green")) +
  ggtitle("Low booster coverage \n (60% residents boosted, 45% staff and visitors boosted)") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Mean no. of cumulative nursing home-acquired \n infections in residents and staff over 1 month") +
  scale_x_discrete(limits=c("50","100","150")) + xlab("Community incidence (cases per 100k per day)") + ylim(0,10.1) + 
  guides(color = guide_legend(title = "Screening frequency"))

ggplot(df, aes(x=com_inc, y=infs, fill=type)) + 
  geom_col(width=0.5, position='dodge') + scale_fill_manual(values=c("red","blue","green")) +
  ggtitle("High booster coverage \n (90% residents boosted, 70% staff and visitors boosted)") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Mean no. of cumulative nursing home-acquired \n infections in residents and staff over 1 month") +
  scale_x_discrete(limits=c("50","100","150")) + xlab("Community incidence (cases per 100k per day)") + ylim(0,10) +
  guides(fill = guide_legend(title = "Screening frequency")) 

#####################################DEATHS#####################################

calc_deaths = function(data = out){
  res = mean(data$res_tot)
  staff = mean(data$staff_tot)
  deaths = (res+staff)*0.05
  
  return(deaths)
}

com_inc_vec = c("50","100","150")

type_vec = rep(c("no screening", "weekly screening", "twice weekly screening"),times=3)

death_vec = c(deaths_50_noscreen,deaths_50_screenweek,deaths_50_screen2xweek,
            deaths_100_noscreen,deaths_100_screenweek,deaths_100_screen2xweek,
            deaths_150_noscreen,deaths_150_screenweek,deaths_150_screen2xweek)

create_df = function(com_inc = com_inc_vec, type = type_vec, deaths = death_vec){
  df = data.frame(com_inc=rep(com_inc,each=3), type=type, deaths=deaths)
  
  return(df)
}

ggplot(df, aes(x=com_inc, y=deaths, col=type)) + 
  geom_point(size=2) + scale_color_manual(values=c("red","blue","green")) +
  ggtitle("High booster coverage \n (90% residents boosted, 70% staff and visitors boosted)") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Mean no. of cumulative deaths \n in residents and staff over 1 month") +
  scale_x_discrete(limits=c("50","100","150")) + xlab("Community incidence (cases per 100k per day)") + ylim(0,.6) + 
  guides(color = guide_legend(title = "Screening frequency"))

ggplot(df, aes(x=com_inc, y=deaths, fill=type)) + 
  geom_col(width=0.5, position='dodge') + scale_fill_manual(values=c("red","blue","green")) +
  ggtitle("High booster coverage \n (90% residents boosted, 70% staff and visitors boosted)") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Mean no. of cumulative deaths \n in residents and staff over 1 month") +
  scale_x_discrete(limits=c("50","100","150")) + xlab("Community incidence (cases per 100k per day)") + ylim(0,.6) +
  guides(fill = guide_legend(title = "Screening frequency")) 

