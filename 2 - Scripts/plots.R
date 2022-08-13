library(data.table)
library(ggplot2)

setwd("/Users/sdong217/Desktop/COVID_NH/NursingHome/nursing-home/2 - Scripts")
load("screenall_highCI_novax.RData")

#' Make bar plot
#' 
#' Make plot showing number of incident infections over one month
#' 
#' @param data output from batch job
#' @param visitors whether there are visitors
#' @param title addition to title of plot
make_plot = function(data = out, visitors = TRUE, title){
  com_res = c(mean(data$com_15_res, na.rm=T), mean(data$com_22_res, na.rm=T), mean(data$com_29_res, na.rm=T), mean(data$com_36_res, na.rm=T), mean(data$com_43_res, na.rm=T))
  nh_res = c(mean(data$nh_15_res, na.rm=T), mean(data$nh_22_res, na.rm=T), mean(data$nh_29_res, na.rm=T), mean(data$nh_36_res, na.rm=T), mean(data$nh_43_res, na.rm=T))
  com_staff = c(mean(data$com_15_staff, na.rm=T), mean(data$com_22_staff, na.rm=T), mean(data$com_29_staff, na.rm=T), mean(data$com_36_staff, na.rm=T), mean(data$com_43_staff, na.rm=T))
  nh_staff = c(mean(data$nh_15_staff, na.rm=T), mean(data$nh_22_staff, na.rm=T), mean(data$nh_29_staff, na.rm=T), mean(data$nh_36_staff, na.rm=T), mean(data$nh_43_staff, na.rm=T))
  
  if(visitors){
    com_visit = c(mean(data$com_15_visit, na.rm=T), mean(data$com_22_visit, na.rm=T), mean(data$com_29_visit, na.rm=T), mean(data$com_36_visit, na.rm=T), mean(data$com_43_visit, na.rm=T))
    nh_visit = c(mean(data$nh_15_visit, na.rm=T), mean(data$nh_22_visit, na.rm=T), mean(data$nh_29_visit, na.rm=T), mean(data$nh_36_visit, na.rm=T), mean(data$nh_43_visit, na.rm=T))
  }
  
  df = data.frame(day=rep(c(1,8,15,22,29),times=6), type=c(rep("Residents in community",times=5),rep("Residents in NH",times=5),rep("Staff in community",times=5),rep("Staff in NH",times=5),rep("Visitors in community",times=5),rep("Visitors in NH",times=5)), 
                            infs=c(com_res,nh_res,com_staff,nh_staff,com_visit,nh_visit))
  
  plot = ggplot(df, aes(fill=type, y=infs, x=day)) + geom_bar(position='dodge', stat='identity') + ggtitle(paste("New Infections per week",title)) + ylab("No. of infections") + 
    scale_fill_manual('Infection origin', values=c('gold', 'brown2', 'springgreen2', 'forestgreen', 'cadetblue2', 'royalblue2')) + scale_y_continuous(limit=c(0,14), breaks=seq(0,14,1))
  
  return(plot)
}


