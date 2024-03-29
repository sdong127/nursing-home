library(data.table)
library(ggplot2)
library(ggpattern)
library(tidyverse)
library(ggpubr)

setwd("/Users/sdong217/Desktop/COVID_NH/NursingHome/nursing-home/4 - Output")
load("lts_noscreen_5_lowvax_70s.RData")


############################FUNCTIONS#############################

calc_resinfs = function(data = out){
  infs = mean(data$res_tot)
  return(infs)
}

create_infs_df = function(com_inc = com_inc_vec, type = type_vec, infs = inf_vec){
  df = data.frame(com_inc=rep(com_inc,each=3), type=type, infs=infs)
  return(df)
}

calc_staffinfs = function(data = out){
  infs = mean(data$staff_tot)
  return(infs)
}


############################INFECTIONS PLOT#####################################

com_inc_vec = c("5","50","100")

type_vec = rep(c("no screening", "weekly screening", "twice weekly screening"),times=3)

inf_vec = c(resinfs_low_noscreen,resinfs_low_screenweek,resinfs_low_screen2xweek,
            resinfs_med_noscreen,resinfs_med_screenweek,resinfs_med_screen2xweek,
            resinfs_high_noscreen,resinfs_high_screenweek,resinfs_high_screen2xweek)

inf_vec = c(staffinfs_low_noscreen,staffinfs_low_screenweek,staffinfs_low_screen2xweek,
            staffinfs_med_noscreen,staffinfs_med_screenweek,staffinfs_med_screen2xweek,
            staffinfs_high_noscreen,staffinfs_high_screenweek,staffinfs_high_screen2xweek)

df=create_infs_df()
df$type = factor(df$type, levels=c("twice weekly screening", "weekly screening", "no screening"))

resinfs_low <- ggplot(df, aes(x=com_inc, y=infs, group=type)) + geom_line(aes(color=type)) + 
  geom_point(aes(color=type)) + scale_color_manual(values=c("orange","springgreen2","royalblue")) +
  ggtitle("Resident infections, low booster uptake") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Number of nursing home-acquired infections \n in residents over 30 days") +
  scale_x_discrete(limits=c("5","50","100")) + xlab("Community incidence") + 
  scale_y_continuous(limits=c(0,20)) + guides(color = guide_legend(title = "Screening frequency"))

resinfs_high <- ggplot(df, aes(x=com_inc, y=infs, group=type)) + geom_line(aes(color=type)) + 
  geom_point(aes(color=type)) + scale_color_manual(values=c("orange","springgreen2","royalblue")) +
  ggtitle("Resident infections, high booster uptake") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Number of nursing home-acquired infections \n in residents over 30 days") +
  scale_x_discrete(limits=c("5","50","100")) + xlab("Community incidence") + 
  scale_y_continuous(limits=c(0,20)) + guides(color = guide_legend(title = "Screening frequency"))


staffinfs_low <- ggplot(df, aes(x=com_inc, y=infs, group=type)) + geom_line(aes(color=type)) + 
  geom_point(aes(color=type)) + scale_color_manual(values=c("orange","springgreen2","royalblue")) +
  ggtitle("Staff infections, low booster uptake") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Number of nursing home-acquired infections \n in staff over 30 days") +
  scale_x_discrete(limits=c("5","50","100")) + xlab("Community incidence") + ylim(0,20) + 
  guides(color = guide_legend(title = "Screening frequency"))

staffinfs_high <- ggplot(df, aes(x=com_inc, y=infs, group=type)) + geom_line(aes(color=type)) + 
  geom_point(aes(color=type)) + scale_color_manual(values=c("orange","springgreen2","royalblue")) +
  ggtitle("Staff infections, high booster uptake") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Number of nursing home-acquired infections \n in staff over 30 days") +
  scale_x_discrete(limits=c("5","50","100")) + xlab("Community incidence") + ylim(0,20) + 
  guides(color = guide_legend(title = "Screening frequency"))


ggarrange(resinfs_low, resinfs_high, staffinfs_low, staffinfs_high, ncol = 2, nrow = 2)
ggsave("infs.pdf", width = 12, height = 10, units = 'in')
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

df = read.csv("results_staffmask70.csv")

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
df$cost_eff = ifelse(df$cost_vec<=1*cost_eff_line,1,df$cost_eff)
df$cost_eff = ifelse(df$cost_vec>1*cost_eff_line & df$cost_vec<=3*cost_eff_line,2,df$cost_eff)
df$cost_eff = ifelse(df$cost_vec>3*cost_eff_line,3,df$cost_eff)

df$graph_vals = df$cost_vec / 1000

heat_map = ggplot(df, aes(com_inc_vec,boost_vec,fill=cost_eff)) + geom_tile(color="black")
heat_map + facet_grid(vars(screen_vec), vars(pax_vec)) + 
  xlab("Community incidence (# of cases per 100k)") + ylab("Booster uptake (%)") + ggtitle("Incremental cost of screening per life-year gained: 1-year life expectancy") +
  scale_fill_gradient(low="lightblue1", high="steelblue4",labels=c("<=$150k", "$150k-450k", ">$450k"), breaks=c(1,2,3)) + 
  guides(fill = guide_legend(title = "Cost per life year")) + geom_text(aes(label = graph_vals), size=3) + 
  scale_y_continuous(breaks=c(0,20,40,60,80,100)) + scale_x_discrete(breaks=c(5,20,40,60,80,100)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("heatmap.pdf", width = 13, height = 7, units = 'in')
dev.off()



############################SENSITIVITY ANALYSIS INFECTIONS PLOT#####################################

com_inc_vec = c("5","50","100")

type_vec = c(rep(c("no screening", "weekly screening", "twice weekly screening"),times=3),
             rep(c("no screening - decreased contact", "weekly screening - decreased contact", "twice weekly screening - decreased contact"),times=3),
             rep(c("no screening - increased contact", "weekly screening - increased contact", "twice weekly screening - increased contact"),times=3))

inf_vec = c(resinfs_low_noscreen,resinfs_low_screenweek,resinfs_low_screen2xweek,
            resinfs_med_noscreen,resinfs_med_screenweek,resinfs_med_screen2xweek,
            resinfs_high_noscreen,resinfs_high_screenweek,resinfs_high_screen2xweek,
            dc_resinfs_low_noscreen,dc_resinfs_low_screenweek,dc_resinfs_low_screen2xweek,
            dc_resinfs_med_noscreen,dc_resinfs_med_screenweek,dc_resinfs_med_screen2xweek,
            dc_resinfs_high_noscreen,dc_resinfs_high_screenweek,dc_resinfs_high_screen2xweek,
            ic_resinfs_low_noscreen,ic_resinfs_low_screenweek,ic_resinfs_low_screen2xweek,
            ic_resinfs_med_noscreen,ic_resinfs_med_screenweek,ic_resinfs_med_screen2xweek,
            ic_resinfs_high_noscreen,ic_resinfs_high_screenweek,ic_resinfs_high_screen2xweek)

inf_vec = c(staffinfs_low_noscreen,staffinfs_low_screenweek,staffinfs_low_screen2xweek,
            staffinfs_med_noscreen,staffinfs_med_screenweek,staffinfs_med_screen2xweek,
            staffinfs_high_noscreen,staffinfs_high_screenweek,staffinfs_high_screen2xweek,
            dc_staffinfs_low_noscreen,dc_staffinfs_low_screenweek,dc_staffinfs_low_screen2xweek,
            dc_staffinfs_med_noscreen,dc_staffinfs_med_screenweek,dc_staffinfs_med_screen2xweek,
            dc_staffinfs_high_noscreen,dc_staffinfs_high_screenweek,dc_staffinfs_high_screen2xweek,
            ic_staffinfs_low_noscreen,ic_staffinfs_low_screenweek,ic_staffinfs_low_screen2xweek,
            ic_staffinfs_med_noscreen,ic_staffinfs_med_screenweek,ic_staffinfs_med_screen2xweek,
            ic_staffinfs_high_noscreen,ic_staffinfs_high_screenweek,ic_staffinfs_high_screen2xweek)

df=create_infs_df()
df$type = factor(df$type, levels=c("twice weekly screening", "weekly screening", "no screening",
                                   "twice weekly screening - decreased contact", "weekly screening - decreased contact", "no screening - decreased contact",
                                   "twice weekly screening - increased contact", "weekly screening - increased contact", "no screening - increased contact"))

resinfs_low <- ggplot(df, aes(x=com_inc, y=infs, group=type)) + geom_line(aes(color=type)) + 
  geom_point(aes(color=type)) + scale_color_manual(values=c("orange","springgreen2","royalblue","lightsalmon","palegreen","steelblue1","darkorange3","springgreen4","dodgerblue4")) +
  ggtitle("Resident infections, low booster uptake") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Number of nursing home-acquired infections \n in residents over 30 days") +
  scale_x_discrete(limits=c("5","50","100")) + xlab("Community incidence") + 
  scale_y_continuous(limits=c(0,20)) + guides(color = guide_legend(title = "Screening frequency"))

resinfs_high <- ggplot(df, aes(x=com_inc, y=infs, group=type)) + geom_line(aes(color=type)) + 
  geom_point(aes(color=type)) + scale_color_manual(values=c("orange","springgreen2","royalblue")) +
  ggtitle("Resident infections, high booster uptake") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Number of nursing home-acquired infections \n in residents over 30 days") +
  scale_x_discrete(limits=c("5","50","100")) + xlab("Community incidence") + 
  scale_y_continuous(limits=c(0,20)) + guides(color = guide_legend(title = "Screening frequency"))


staffinfs_low <- ggplot(df, aes(x=com_inc, y=infs, group=type)) + geom_line(aes(color=type)) + 
  geom_point(aes(color=type)) + scale_color_manual(values=c("orange","springgreen2","royalblue")) +
  ggtitle("Staff infections, low booster uptake") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Number of nursing home-acquired infections \n in staff over 30 days") +
  scale_x_discrete(limits=c("5","50","100")) + xlab("Community incidence") + ylim(0,20) + 
  guides(color = guide_legend(title = "Screening frequency"))

staffinfs_high <- ggplot(df, aes(x=com_inc, y=infs, group=type)) + geom_line(aes(color=type)) + 
  geom_point(aes(color=type)) + scale_color_manual(values=c("orange","springgreen2","royalblue")) +
  ggtitle("Staff infections, high booster uptake") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Number of nursing home-acquired infections \n in staff over 30 days") +
  scale_x_discrete(limits=c("5","50","100")) + xlab("Community incidence") + ylim(0,20) + 
  guides(color = guide_legend(title = "Screening frequency"))


ggarrange(resinfs_low, resinfs_high, staffinfs_low, staffinfs_high, ncol = 2, nrow = 2)
ggsave("infs.pdf", width = 12, height = 10, units = 'in')
dev.off()
