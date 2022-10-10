library(data.table)
library(ggplot2)
library(ggpattern)
library(tidyverse)

setwd("/Users/sdong217/Desktop/COVID_NH/NursingHome/nursing-home/2 - Scripts")
load("noscreen_highvax_150.RData")

###################################INFECTIONS###################################

calc_infs = function(data = out){
  res = mean(data$res_tot)
  staff = mean(data$staff_tot)
  visit = mean(data$visit_tot)
  infs = res + staff + visit
  
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

ggplot(df, aes(x=com_inc, y=infs, col=type)) + 
  geom_point(size=2) + scale_color_manual(values=c("red","blue","green")) +
  ggtitle("High booster coverage \n (90% residents boosted, 70% staff and visitors boosted)") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Mean no. of cumulative nursing home-acquired infections \n in residents, staff, and visitors over 1 month") +
  scale_x_discrete(limits=c("50","100","150")) + xlab("Community incidence (cases per 100k per day)") + ylim(0,10.1) + 
  guides(color = guide_legend(title = "Screening frequency"))

ggplot(df, aes(x=com_inc, y=infs, fill=type)) + 
  geom_col(width=0.5, position='dodge') + scale_fill_manual(values=c("red","blue","green")) +
  ggtitle("High booster coverage \n (90% residents boosted, 70% staff and visitors boosted)") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Mean no. of cumulative nursing home-acquired infections \n in residents, staff, and visitors over 1 month") +
  scale_x_discrete(limits=c("50","100","150")) + xlab("Community incidence (cases per 100k per day)") + ylim(0,10.1) +
  guides(fill = guide_legend(title = "Screening frequency")) 

#################################DEATHS in RESIDENTS############################

calc_deaths_res = function(data = out){
  res = mean(data$res_tot)
  deaths = res*0.031
  
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
  ylab("Mean no. of cumulative deaths in residents over 1 month") +
  scale_x_discrete(limits=c("50","100","150")) + xlab("Community incidence (cases per 100k per day)") + ylim(0,.2) + 
  guides(color = guide_legend(title = "Screening frequency"))

ggplot(df, aes(x=com_inc, y=deaths, fill=type)) + 
  geom_col(width=0.5, position='dodge') + scale_fill_manual(values=c("red","blue","green")) +
  ggtitle("High booster coverage \n (90% residents boosted, 70% staff and visitors boosted)") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Mean no. of cumulative deaths in residents over 1 month") +
  scale_x_discrete(limits=c("50","100","150")) + xlab("Community incidence (cases per 100k per day)") + ylim(0,.2) +
  guides(fill = guide_legend(title = "Screening frequency")) 


###############################DEATHS by PAXLOVID UPTAKE##########################

calc_deaths_pax = function(data = out){
  res = mean(data$res_tot)
  deaths_4 = res*0.031
  deaths_15 = res*0.028
  deaths_30 = res*0.024
  
  return(c(deaths_4,deaths_15,deaths_30))
}

com_inc_vec = c("50","100","150")
pax_prop_vec = c("4%","15%","30%")
boost_vec = c("low","high")

death_vec = c(deaths_50_lowboost,deaths_100_lowboost,deaths_150_lowboost,
              deaths_50_highboost,deaths_100_highboost,deaths_150_highboost)

create_df = function(com_inc = com_inc_vec, pax_uptake = pax_prop_vec, boost_uptake = boost_vec, deaths = death_vec){
  df = data.frame(com_inc=rep(rep(com_inc,each=3),times=2), pax_uptake=rep(pax_uptake,times=6), 
                  boost_uptake = rep(boost_vec,each=9), deaths=deaths)
  
  return(df)
}


# bar graph by community incidence and Pax uptake
ggplot(data = df, aes(x = factor(com_inc, levels=c("50","100","150")), y = deaths, fill = factor(pax_uptake,levels=c("4%","15%","30%")), pattern = boost_uptake)) +
  geom_bar_pattern(stat="identity", position = position_dodge(preserve = "single"), width=0.75,
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.05,
                   pattern_spacing = 0.01,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual("Paxlovid uptake", values = c("orange","springgreen2","royalblue")) +
  scale_pattern_manual(values = c(low = "stripe", high = "none")) +
  labs(x = "Community incidence (cases per 100k per day)", 
       y = "Mean no. of cumulative deaths in residents over 1 month", pattern = "Booster uptake") + 
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) + 
  ggtitle("No screening") + theme(plot.title = element_text(hjust = 0.5)) + ylim(0,.2)


# bar graph by size
ggplot(data = df, aes(x = factor(com_inc, levels=c("50","100","150")), y = deaths, fill = factor(pax_uptake,levels=c("30%","15%","4%")), pattern = boost_uptake)) +
  geom_bar_pattern(stat="identity", position = position_dodge(preserve = "single"), width=0.75,
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.05,
                   pattern_spacing = 0.01,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual("Paxlovid uptake", values = c("orange","springgreen2","royalblue")) +
  scale_pattern_manual(values = c(low = "stripe", high = "none")) +
  labs(x = "Community incidence (cases per 100k per day)", 
       y = "Mean no. of cumulative deaths in residents over 1 month", pattern = "Booster uptake") + 
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) + 
  ggtitle("No screening") + theme(plot.title = element_text(hjust = 0.5)) + ylim(0,.2)


# line graph
df$com_inc = rep(rep(c(50,100,150),each=3),times=2)
df$pax_uptake = factor(df$pax_uptake, levels=c("4%","15%","30%"))

line_plot = ggplot(data = df[df$boost_uptake=="low",], aes(x = com_inc, y = deaths, color=pax_uptake)) +
  geom_line(aes(color=pax_uptake)) + geom_point(aes(color=pax_uptake)) +
  scale_color_manual("Paxlovid uptake", values = c("orange","springgreen2","royalblue")) +
  labs(x = "Community incidence (cases per 100k per day)", 
       y = "Mean no. of cumulative deaths in residents over 1 month", pattern = "Booster uptake") + 
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) + 
  ggtitle("No screening") + theme(plot.title = element_text(hjust = 0.5)) + ylim(0,.2) + scale_x_continuous(breaks=c(50,100,150))

line_plot + geom_line(data=df[df$boost_uptake=="high",], linetype="dashed") + geom_point(data=df[df$boost_uptake=="high",], aes(color=pax_uptake))
