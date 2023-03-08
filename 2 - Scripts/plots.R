library(data.table)
library(ggplot2)
library(ggpattern)
library(tidyverse)

setwd("/Users/sdong217/Desktop/COVID_NH/NursingHome/nursing-home/3 - Output")
load("noscreen_10_lowvax.RData")

############################INFECTIONS IN RESIDENTS#############################

calc_resinfs = function(data = out){
  infs = mean(data$res_tot)
  return(infs)
}

com_inc_vec = c("10","100","200")

type_vec = rep(c("no screening", "weekly screening", "twice weekly screening"),times=3)

inf_vec = c(resinfs_10_noscreen,resinfs_10_screenweek,resinfs_10_screen2xweek,
            resinfs_100_noscreen,resinfs_100_screenweek,resinfs_100_screen2xweek,
            resinfs_200_noscreen,resinfs_200_screenweek,resinfs_200_screen2xweek)

create_infs_df = function(com_inc = com_inc_vec, type = type_vec, infs = inf_vec){
  df = data.frame(com_inc=rep(com_inc,each=3), type=type, infs=infs)
  
  return(df)
}

df=create_infs_df()
df$type = factor(df$type, levels=c("twice weekly screening", "weekly screening", "no screening"))

png("highboostresinfs_TR.png", width = 6, height = 5, units = 'in', res = 300)
ggplot(df, aes(x=com_inc, y=infs, group=type)) + geom_line(aes(color=type)) + 
  geom_point(aes(color=type)) + scale_color_manual(values=c("orange","springgreen2","royalblue")) +
  ggtitle("Infections in residents, high booster uptake") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Mean no. of nursing home-acquired infections \n in residents over 1 month") +
  scale_x_discrete(limits=c("10","100","200")) + xlab("Community incidence") + ylim(0,10) + 
  guides(color = guide_legend(title = "Screening frequency"))
dev.off()


############################INFECTIONS IN STAFF#############################

calc_staffinfs = function(data = out){
  infs = mean(data$staff_tot)
  return(infs)
}

com_inc_vec = c("10","100","200")

type_vec = rep(c("no screening", "weekly screening", "twice weekly screening"),times=3)

inf_vec = c(staffinfs_10_noscreen,staffinfs_10_screenweek,staffinfs_10_screen2xweek,
            staffinfs_100_noscreen,staffinfs_100_screenweek,staffinfs_100_screen2xweek,
            staffinfs_200_noscreen,staffinfs_200_screenweek,staffinfs_200_screen2xweek)

create_infs_df = function(com_inc = com_inc_vec, type = type_vec, infs = inf_vec){
  df = data.frame(com_inc=rep(com_inc,each=3), type=type, infs=infs)
  
  return(df)
}

df=create_infs_df()
df$type = factor(df$type, levels=c("twice weekly screening", "weekly screening", "no screening"))

png("highbooststaffinfs_TR.png", width = 6, height = 5, units = 'in', res = 300)
ggplot(df, aes(x=com_inc, y=infs, group=type)) + geom_line(aes(color=type)) + 
  geom_point(aes(color=type)) + scale_color_manual(values=c("orange","springgreen2","royalblue")) +
  ggtitle("Infections in staff, high booster uptake") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Mean no. of nursing home-acquired infections \n in staff over 1 month") +
  scale_x_discrete(limits=c("10","100","200")) + xlab("Community incidence") + ylim(0,5) + 
  guides(color = guide_legend(title = "Screening frequency"))
dev.off()


#################################DEATHS in RESIDENTS############################

calc_deaths_res = function(data = out, cfr = 0.028){
  res = mean(data$res_tot)
  deaths = res*cfr
  
  return(deaths)
}

com_inc_vec = c("10","100","200")

type_vec = rep(c("no screening", "weekly screening", "twice weekly screening"),times=3)

death_vec = c(deaths_10_noscreen,deaths_10_screenweek,deaths_10_screen2xweek,
            deaths_100_noscreen,deaths_100_screenweek,deaths_100_screen2xweek,
            deaths_200_noscreen,deaths_200_screenweek,deaths_200_screen2xweek)

create_deaths_df = function(com_inc = com_inc_vec, type = type_vec, deaths = death_vec){
  df = data.frame(com_inc=rep(com_inc,each=3), type=type, deaths=deaths)
  
  return(df)
}

df=create_deaths_df()
df$type = factor(df$type, levels=c("twice weekly screening", "weekly screening", "no screening"))

png("highboostdeaths.png", width = 6, height = 5, units = 'in', res = 300)
ggplot(df, aes(x=com_inc, y=deaths, group=type)) + geom_line(aes(color=type)) + 
  geom_point(aes(color=type)) + scale_color_manual(values=c("orange","springgreen2","royalblue")) +
  ggtitle("High booster uptake") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Mean no. of deaths in residents over 1 month") +
  scale_x_discrete(limits=c("10","100","200")) + xlab("Community incidence") + ylim(0,.3) + 
  guides(color = guide_legend(title = "Screening frequency"))
dev.off()


###############################COST-EFFECTIVENESS PLOT##########################

df = read.csv("cost_results.csv")

df[df$screen_vec == "none",]$screen_vec = "no screening"
df[df$screen_vec == "weekly",]$screen_vec = "weekly screening"
df[df$screen_vec == "twice-weekly",]$screen_vec = "twice-weekly screening"
df$screen_vec = factor(df$screen_vec, levels = c("no screening", "weekly screening", "twice-weekly screening"))

df[df$com_inc_vec==4,]$com_inc_vec = "4 cases per 100k"
df[df$com_inc_vec==40,]$com_inc_vec = "40 cases per 100k"
df[df$com_inc_vec==80,]$com_inc_vec = "80 cases per 100k"
df$com_inc_vec = factor(df$com_inc_vec, levels=c("4 cases per 100k", "40 cases per 100k", "80 cases per 100k"))

low_boost_df = df[df$boost_vec=="low",]
options(scipen=2)
low_boost = ggplot(low_boost_df, aes(pax_vec,cost_vec)) + geom_point()
png("costeff_lowboost_plot.png", width = 9, height = 6, units = 'in', res = 300)
low_boost + facet_grid(vars(screen_vec), vars(com_inc_vec)) + geom_hline(yintercept=150000, linetype="dashed", color="red") + 
  xlab("Paxlovid uptake") + ylab("Cost (USD)") + ggtitle("Cost of testing per resident death averted (low booster uptake)")
dev.off()

high_boost_df = df[df$boost_vec=="high",]
options(scipen=2)
high_boost = ggplot(high_boost_df, aes(pax_vec,cost_vec)) + geom_point()
png("costeff_highboost_plot.png", width = 9, height = 6, units = 'in', res = 300)
high_boost + facet_grid(vars(screen_vec), vars(com_inc_vec)) + geom_hline(yintercept=150000, linetype="dashed", color="red") + 
  xlab("Paxlovid uptake") + ylab("Cost (USD)") + ggtitle("Cost of testing per resident death averted (high booster uptake)")
dev.off()


###############################COST-EFFECTIVENESS HEAT MAP##########################

df = read.csv("cost_results.csv")

df[df$screen_vec == "none",]$screen_vec = "no screening"
df[df$screen_vec == "weekly",]$screen_vec = "weekly screening"
df[df$screen_vec == "twice-weekly",]$screen_vec = "twice-weekly screening"
df$screen_vec = factor(df$screen_vec, levels = c("no screening", "weekly screening", "twice-weekly screening"))

df[df$com_inc_vec==4,]$com_inc_vec = "4 cases per 100k"
df[df$com_inc_vec==40,]$com_inc_vec = "40 cases per 100k"
df[df$com_inc_vec==80,]$com_inc_vec = "80 cases per 100k"
df$com_inc_vec = factor(df$com_inc_vec, levels=c("4 cases per 100k", "40 cases per 100k", "80 cases per 100k"))

cost_eff_line = 150000
df$cost_eff = df$cost_vec
df$cost_eff[df$cost_eff<cost_eff_line] = -1
df$cost_eff[df$cost_eff==cost_eff_line] = 0
df$cost_eff[df$cost_eff>cost_eff_line & df$cost_eff<2*cost_eff_line] = 1
df$cost_eff[df$cost_eff>2*cost_eff_line & df$cost_eff<3*cost_eff_line] = 2
df$cost_eff[df$cost_eff>3*cost_eff_line] = 3

heat_map = ggplot(df, aes(pax_vec,boost_vec,fill=cost_eff)) + geom_tile(color="black")
png("costeff_heatmap.png", width = 9, height = 6, units = 'in', res = 300)
heat_map + facet_grid(vars(screen_vec), vars(com_inc_vec)) + 
  xlab("Paxlovid uptake") + ylab("Booster uptake") + ggtitle("Cost of testing per resident death averted") +
  scale_fill_gradient(low="white", high="blue", labels=c("<$150k", "$150k", "$150k-300k", "$300k-450k", ">$450k")) + 
  guides(fill = guide_legend(title = "Cost"))
dev.off()
