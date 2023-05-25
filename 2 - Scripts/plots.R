library(data.table)
library(ggplot2)
library(ggpattern)
library(tidyverse)

setwd("/Users/sdong217/Desktop/COVID_NH/NursingHome/nursing-home/3 - Output")
load("noscreen_5_highvax_70s.RData")

############################INFECTIONS IN RESIDENTS#############################

calc_resinfs = function(data = out){
  infs = mean(data$res_tot)
  return(infs)
}

com_inc_vec = c("5","50","100")

type_vec = rep(c("no screening", "weekly screening", "twice weekly screening"),times=3)

inf_vec = c(resinfs_low_noscreen,resinfs_low_screenweek,resinfs_low_screen2xweek,
            resinfs_med_noscreen,resinfs_med_screenweek,resinfs_med_screen2xweek,
            resinfs_high_noscreen,resinfs_high_screenweek,resinfs_high_screen2xweek)

create_infs_df = function(com_inc = com_inc_vec, type = type_vec, infs = inf_vec){
  df = data.frame(com_inc=rep(com_inc,each=3), type=type, infs=infs)
  
  return(df)
}

df=create_infs_df()
df$type = factor(df$type, levels=c("twice weekly screening", "weekly screening", "no screening"))

png("highvaxresinfs_2.png", width = 6, height = 5, units = 'in', res = 300)
ggplot(df, aes(x=com_inc, y=infs, group=type)) + geom_line(aes(color=type)) + 
  geom_point(aes(color=type)) + scale_color_manual(values=c("orange","springgreen2","royalblue")) +
  ggtitle("Infections in residents, high booster uptake") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Number of nursing home-acquired infections \n in residents over 30 days") +
  scale_x_discrete(limits=c("5","50","100")) + xlab("Community incidence") + 
  scale_y_continuous(limits=c(0,20)) + guides(color = guide_legend(title = "Screening frequency"))
dev.off()


############################INFECTIONS IN STAFF#############################

calc_staffinfs = function(data = out){
  infs = mean(data$staff_tot)
  return(infs)
}

com_inc_vec = c("5","50","100")

type_vec = rep(c("no screening", "weekly screening", "twice weekly screening"),times=3)

inf_vec = c(staffinfs_low_noscreen,staffinfs_low_screenweek,staffinfs_low_screen2xweek,
            staffinfs_med_noscreen,staffinfs_med_screenweek,staffinfs_med_screen2xweek,
            staffinfs_high_noscreen,staffinfs_high_screenweek,staffinfs_high_screen2xweek)

create_infs_df = function(com_inc = com_inc_vec, type = type_vec, infs = inf_vec){
  df = data.frame(com_inc=rep(com_inc,each=3), type=type, infs=infs)
  
  return(df)
}

df=create_infs_df()
df$type = factor(df$type, levels=c("twice weekly screening", "weekly screening", "no screening"))

png("highvaxstaffinfs_2.png", width = 6, height = 5, units = 'in', res = 300)
ggplot(df, aes(x=com_inc, y=infs, group=type)) + geom_line(aes(color=type)) + 
  geom_point(aes(color=type)) + scale_color_manual(values=c("orange","springgreen2","royalblue")) +
  ggtitle("Infections in staff, high booster uptake") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Number of nursing home-acquired infections \n in staff over 30 days") +
  scale_x_discrete(limits=c("5","50","100")) + xlab("Community incidence") + ylim(0,20) + 
  guides(color = guide_legend(title = "Screening frequency"))
dev.off()


#################################DEATHS in RESIDENTS############################

calc_deaths_res = function(data = out, cfr = 0.018){
  res = mean(data$res_tot)
  deaths = res*cfr
  
  return(deaths)
}

com_inc_vec = c("5","50","100")

type_vec = rep(c("no screening", "weekly screening", "twice weekly screening"),times=3)

death_vec = c(deaths_low_noscreen,deaths_low_screenweek,deaths_low_screen2xweek,
            deaths_med_noscreen,deaths_med_screenweek,deaths_med_screen2xweek,
            deaths_high_noscreen,deaths_high_screenweek,deaths_high_screen2xweek)

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



###############################COST-EFFECTIVENESS HEAT MAP##########################

df = read.csv("results_nomask.csv")

df = subset(df, screen_vec!="none")
df[df$screen_vec == "weekly",]$screen_vec = "weekly screening"
df[df$screen_vec == "twice-weekly",]$screen_vec = "twice-weekly screening"
df$screen_vec = factor(df$screen_vec, levels = c("weekly screening", "twice-weekly screening"))

df[df$pax_vec==0,]$pax_vec = "0% antiviral uptake"
df[df$pax_vec==25,]$pax_vec = "25% antiviral uptake"
df[df$pax_vec==50,]$pax_vec = "50% antiviral uptake"
df[df$pax_vec==75,]$pax_vec = "75% antiviral uptake"
df[df$pax_vec==100,]$pax_vec = "100% antiviral uptake"
df$pax_vec = factor(df$pax_vec, levels=c("0% antiviral uptake", "25% antiviral uptake", "50% antiviral uptake", "75% antiviral uptake", "100% antiviral uptake"))

df[df$com_inc_vec==5,]$com_inc_vec = "5"
df[df$com_inc_vec==20,]$com_inc_vec = "20"
df[df$com_inc_vec==40,]$com_inc_vec = "40"
df[df$com_inc_vec==60,]$com_inc_vec = "60"
df[df$com_inc_vec==80,]$com_inc_vec = "80"
df[df$com_inc_vec==100,]$com_inc_vec = "100"
df$com_inc_vec = factor(df$com_inc_vec, levels=c("5", "20", "40", "60", "80", "100"))

cost_eff_line = 150000
df$cost_eff = NA
df$cost_eff = ifelse(df$cost_vec<=5*cost_eff_line,1,df$cost_eff)
df$cost_eff = ifelse(df$cost_vec>5*cost_eff_line & df$cost_vec<=15*cost_eff_line,2,df$cost_eff)
df$cost_eff = ifelse(df$cost_vec>15*cost_eff_line,3,df$cost_eff)

df$graph_vals = df$cost_vec / 1000

heat_map = ggplot(df, aes(com_inc_vec,boost_vec,fill=cost_eff)) + geom_tile(color="black")
png("costeff_heatmap_5yr_0.png", width = 13, height = 7, units = 'in', res = 300)
heat_map + facet_grid(vars(screen_vec), vars(pax_vec)) + 
  xlab("Community incidence (# of cases per 100k)") + ylab("Booster uptake (%)") + ggtitle("Incremental cost of screening per life-year gained: 5-year life expectancy") +
  scale_fill_gradient(low="lightcyan1",labels="<=$150k") + 
  guides(fill = guide_legend(title = "Cost per life year")) + geom_text(aes(label = graph_vals), size=3) + 
  scale_y_continuous(breaks=c(0,20,40,60,80,100)) + scale_x_discrete(breaks=c(5,20,40,60,80,100)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

