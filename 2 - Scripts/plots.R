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
  ggtitle("Infections in residents, low booster uptake") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Mean no. of nursing home-acquired infections \n in residents over 1 month") +
  scale_x_discrete(limits=c("10","100","200")) + xlab("Community incidence") + ylim(0,15) + 
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

calc_deaths_res = function(data = out, ifr = 0.028){
  res = mean(data$res_tot)
  deaths = res*ifr
  
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


###############################DEATHS by PAXLOVID UPTAKE##########################

calc_deaths_pax = function(data = out, 
                           perc_5_uptake = 0.027, perc_21_uptake = 0.024, perc_49_uptake = 0.018){
  res = mean(data$res_tot)
  deaths_5 = res*perc_5_uptake
  deaths_21 = res*perc_21_uptake
  deaths_49 = res*perc_49_uptake
  
  return(c(deaths_5,deaths_21,deaths_49))
}

com_inc_vec = c(10,100,200)
pax_prop_vec = c("5%","21%","49%")
boost_vec = c("low","high")

death_vec = c(deaths_10_lowboost,deaths_100_lowboost,deaths_200_lowboost,
              deaths_10_highboost,deaths_100_highboost,deaths_200_highboost)

create_pax_df = function(com_inc = com_inc_vec, pax_uptake = pax_prop_vec, boost_uptake = boost_vec, deaths = death_vec){
  df = data.frame(com_inc=rep(rep(com_inc,each=3),times=2), pax_uptake=rep(pax_uptake,times=6), 
                  boost_uptake = rep(boost_vec,each=9), deaths=deaths)
  
  return(df)
}


# bar graph by size and booster uptake
# ggplot(data = df, aes(x = factor(com_inc, levels=c("50","100","150")), y = deaths, fill = factor(pax_uptake,levels=c("30%","15%","4%")), pattern = boost_uptake)) +
#   geom_bar_pattern(stat="identity", position = position_dodge(preserve = "single"), width=0.75,
#                    color = "black", 
#                    pattern_fill = "black",
#                    pattern_angle = 45,
#                    pattern_density = 0.05,
#                    pattern_spacing = 0.01,
#                    pattern_key_scale_factor = 0.6) +
#   scale_fill_manual("Paxlovid uptake", values = c("orange","springgreen2","royalblue")) +
#   scale_pattern_manual(values = c(low = "stripe", high = "none")) +
#   labs(x = "Community incidence", 
#        y = "Mean no. of cumulative deaths in residents over 1 month", pattern = "Booster uptake") + 
#   guides(pattern = guide_legend(override.aes = list(fill = "white")),
#          fill = guide_legend(override.aes = list(pattern = "none"))) + 
#   ggtitle("Twice-weekly screening") + theme(plot.title = element_text(hjust = 0.5)) + ylim(0,.3)


# line graph
df=create_pax_df()
df$pax_uptake = factor(df$pax_uptake, levels=c("5%","21%","49%"))

png("twiceweeklyscreenpax.png", width = 4, height = 6, units = 'in', res = 300)
line_plot = ggplot(data = df[df$boost_uptake=="low",], aes(x = com_inc, y = deaths, color=pax_uptake)) +
  geom_line(aes(color=pax_uptake)) + geom_point(aes(color=pax_uptake)) +
  scale_color_manual("Paxlovid uptake", values = c("orange","springgreen2","royalblue")) +
  labs(x = "Community incidence", 
       y = "Mean no. of deaths in residents over 1 month") + 
  ggtitle("Twice-weekly screening") + theme(plot.title = element_text(hjust = 0.5)) + ylim(0,.3) + scale_x_continuous(breaks=c(10,100,200))

line_plot + geom_line(data=df[df$boost_uptake=="high",], linetype="dashed") + geom_point(data=df[df$boost_uptake=="high",], aes(color=pax_uptake))
dev.off()

png("boostlegend.png", width = 4, height = 6, units = 'in', res = 300)
ggplot(data=df, aes(x=com_inc, y=deaths, group=boost_uptake)) + geom_line(aes(linetype=boost_uptake)) + scale_linetype_manual("Booster uptake", values=c("dashed", "solid"))
dev.off()


#########################DIFFERENCE BETWEEN SCREENING STRATS#####################

com_inc_vec = c("10","100","200")

type_vec = rep(c("weekly screening", "twice weekly screening"),times=3)

inf_vec = c(infs_10_noscreen-infs_10_screenweek,infs_10_noscreen-infs_10_screen2xweek,
            infs_100_noscreen-infs_100_screenweek,infs_100_noscreen-infs_100_screen2xweek,
            infs_200_noscreen-infs_200_screenweek,infs_200_noscreen-infs_200_screen2xweek)

create_diffinfs_df = function(com_inc = com_inc_vec, type = type_vec, infs = inf_vec){
  df = data.frame(com_inc=rep(com_inc,each=2), type=type, infs=infs)
  
  return(df)
}

df=create_diffinfs_df()
df$type = factor(df$type, levels=c("twice weekly screening", "weekly screening"))

png("reldiff_highvax.png", width = 6, height = 5, units = 'in', res = 300)
ggplot(df, aes(x=com_inc, y=infs, group=type)) + geom_line(aes(color=type)) + 
  geom_point(aes(color=type)) + scale_color_manual(values=c("orange","springgreen2")) +
  ggtitle("High booster uptake") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Relative difference in no. of infections \n between screening strategy and no screening") +
  scale_x_discrete(limits=c("10","100","200")) + xlab("Community incidence") + ylim(0,10) + 
  guides(color = guide_legend(title = "Screening frequency"))
dev.off()
