library(data.table)
library(ggplot2)

setwd("/Users/sdong217/Desktop/COVID_NH/NursingHome/nursing-home/3 - Output")
load("noscreen_10.RData")

#' Make bar plot
#' 
#' Make plot showing number of incident infections over one month
#' 
#' @param data output from batch job
#' @param visitors whether there are visitors
#' @param title addition to title of plot
make_plot = function(data = out, visitors = TRUE, temp_staff = F, title){
  com_res = c(mean(data$com_1_res), mean(data$com_8_res), mean(data$com_15_res), mean(data$com_22_res), mean(data$com_29_res), mean(data$com_36_res), mean(data$com_43_res))
  nh_res = c(mean(data$nh_1_res), mean(data$nh_8_res), mean(data$nh_15_res), mean(data$nh_22_res), mean(data$nh_29_res), mean(data$nh_36_res), mean(data$nh_43_res))
  com_staff = c(mean(data$com_1_staff), mean(data$com_8_staff), mean(data$com_15_staff), mean(data$com_22_staff), mean(data$com_29_staff), mean(data$com_36_staff), mean(data$com_43_staff))
  nh_staff = c(mean(data$nh_1_staff), mean(data$nh_8_staff), mean(data$nh_15_staff), mean(data$nh_22_staff), mean(data$nh_29_staff), mean(data$nh_36_staff), mean(data$nh_43_staff))
  
  if(visitors & temp_staff){
    com_visit = c(mean(data$com_1_visit), mean(data$com_8_visit), mean(data$com_15_visit), mean(data$com_22_visit), mean(data$com_29_visit), mean(data$com_36_visit), mean(data$com_43_visit))
    nh_visit = c(mean(data$nh_1_visit), mean(data$nh_8_visit), mean(data$nh_15_visit), mean(data$nh_22_visit), mean(data$nh_29_visit), mean(data$nh_36_visit), mean(data$nh_43_visit))
    com_temp_staff = c(mean(data$com_1_temp_staff), mean(data$com_8_temp_staff), mean(data$com_15_temp_staff), mean(data$com_22_temp_staff), mean(data$com_29_temp_staff), mean(data$com_36_temp_staff), mean(data$com_43_temp_staff))
    nh_temp_staff = c(mean(data$nh_1_temp_staff), mean(data$nh_8_temp_staff), mean(data$nh_15_temp_staff), mean(data$nh_22_temp_staff), mean(data$nh_29_temp_staff), mean(data$nh_36_temp_staff), mean(data$nh_43_temp_staff))

    # df = data.frame(day=rep(c(1,8,15,22,29,36,43),times=8), type=c(rep("Residents in community",times=7),rep("Residents in NH",times=7),rep("Staff in community",times=7),rep("Staff in NH",times=7),rep("Visitors in community",times=7),rep("Visitors in NH",times=7),rep("Temporary Staff in community",times=7),rep("Temporary Staff in NH",times=7)),
    #                 infs=c(com_res,nh_res,com_staff,nh_staff,com_visit,nh_visit,com_temp_staff,nh_temp_staff))
    df = data.frame(day=rep(c(1,8,15,22,29,36,43),times=6), type=c(rep("Residents in community",times=7),rep("Residents in NH",times=7),rep("Staff in community",times=7),rep("Staff in NH",times=7),rep("Visitors in community",times=7),rep("Visitors in NH",times=7)),
                    infs=c(com_res,nh_res,com_staff,nh_staff,com_visit,nh_visit))
    
  }else if(visitors){
    com_visit = c(mean(data$com_1_visit), mean(data$com_8_visit), mean(data$com_15_visit), mean(data$com_22_visit), mean(data$com_29_visit), mean(data$com_36_visit), mean(data$com_43_visit))
    nh_visit = c(mean(data$nh_1_visit), mean(data$nh_8_visit), mean(data$nh_15_visit), mean(data$nh_22_visit), mean(data$nh_29_visit), mean(data$nh_36_visit), mean(data$nh_43_visit))

    df = data.frame(day=rep(c(1,8,15,22,29,36,43),times=6), type=c(rep("Residents in community",times=7),rep("Residents in NH",times=7),rep("Staff in community",times=7),rep("Staff in NH",times=7),rep("Visitors in community",times=7),rep("Visitors in NH",times=7)),
                    infs=c(com_res,nh_res,com_staff,nh_staff,com_visit,nh_visit))
    
  }else{
    df = data.frame(day=rep(c(1,8,15,22,29,36,43),times=4), type=c(rep("Residents in community",times=7),rep("Residents in NH",times=7),rep("Staff in community",times=7),rep("Staff in NH",times=7)), 
                    infs=c(com_res,nh_res,com_staff,nh_staff))
  }
  
  # plot = ggplot(df, aes(fill=type, y=infs, x=day)) + geom_bar(position='dodge', stat='identity') + ggtitle(paste("New Infections per week",title)) + ylab("No. of infections") +
  #   scale_fill_manual('Infection origin', values=c('gold', 'brown2', 'springgreen2', 'forestgreen', 'cadetblue2', 'royalblue2', 'mediumpurple1', 'slateblue2')) + scale_y_continuous(limit=c(0,1.5), breaks=seq(0,1.5,0.2))
  
  plot = ggplot(df, aes(fill=type, y=infs, x=day)) + geom_bar(position='dodge', stat='identity') + ggtitle(paste("New Infections per week",title)) + ylab("No. of infections") +
    scale_fill_manual('Infection origin', values=c('gold', 'brown2', 'springgreen2', 'forestgreen', 'cadetblue2', 'royalblue2')) + scale_y_continuous(limit=c(0,10), breaks=seq(0,10,1))
  
  return(plot)
}


make_inf_df = function(data=out, visitors = T, temp_staff = F){
  com_res = c(mean(data$com_1_res), mean(data$com_8_res), mean(data$com_15_res), mean(data$com_22_res), mean(data$com_29_res), mean(data$com_36_res), mean(data$com_43_res))
  com_res_total = sum(com_res)
  nh_res = c(mean(data$nh_1_res), mean(data$nh_8_res), mean(data$nh_15_res), mean(data$nh_22_res), mean(data$nh_29_res), mean(data$nh_36_res), mean(data$nh_43_res))
  nh_res_total = sum(nh_res)
  com_staff = c(mean(data$com_1_staff), mean(data$com_8_staff), mean(data$com_15_staff), mean(data$com_22_staff), mean(data$com_29_staff), mean(data$com_36_staff), mean(data$com_43_staff))
  com_staff_total = sum(com_staff)
  nh_staff = c(mean(data$nh_1_staff), mean(data$nh_8_staff), mean(data$nh_15_staff), mean(data$nh_22_staff), mean(data$nh_29_staff), mean(data$nh_36_staff), mean(data$nh_43_staff))
  nh_staff_total = sum(nh_staff)

  if(visitors & temp_staff){
    com_visit = c(mean(data$com_1_visit), mean(data$com_8_visit), mean(data$com_15_visit), mean(data$com_22_visit), mean(data$com_29_visit), mean(data$com_36_visit), mean(data$com_43_visit))
    com_visit_total = sum(com_visit)
    nh_visit = c(mean(data$nh_1_visit), mean(data$nh_8_visit), mean(data$nh_15_visit), mean(data$nh_22_visit), mean(data$nh_29_visit), mean(data$nh_36_visit), mean(data$nh_43_visit))
    nh_visit_total = sum(nh_visit)
    com_temp_staff = c(mean(data$com_1_temp_staff), mean(data$com_8_temp_staff), mean(data$com_15_temp_staff), mean(data$com_22_temp_staff), mean(data$com_29_temp_staff), mean(data$com_36_temp_staff), mean(data$com_43_temp_staff))
    com_temp_staff_total = sum(com_temp_staff)
    nh_temp_staff = c(mean(data$nh_1_temp_staff), mean(data$nh_8_temp_staff), mean(data$nh_15_temp_staff), mean(data$nh_22_temp_staff), mean(data$nh_29_temp_staff), mean(data$nh_36_temp_staff), mean(data$nh_43_temp_staff))
    nh_temp_staff_total = sum(nh_temp_staff)
    
    com_inc_df = data.frame(com_residents = com_res_total, com_staff = com_staff_total, com_temp_staff = com_temp_staff_total, com_visitors = com_visit_total, com_total = sum(com_res_total, com_staff_total, com_temp_staff_total, com_visit_total),
                            nh_residents = nh_res_total, nh_staff = nh_staff_total, nh_temp_staff = nh_temp_staff_total, nh_visitors = nh_visit_total, nh_total = sum(nh_res_total, nh_staff_total, nh_temp_staff, nh_visit_total))
  }else if(visitors){
    com_visit = c(mean(data$com_1_visit), mean(data$com_8_visit), mean(data$com_15_visit), mean(data$com_22_visit), mean(data$com_29_visit), mean(data$com_36_visit), mean(data$com_43_visit))
    com_visit_total = sum(com_visit)
    nh_visit = c(mean(data$nh_1_visit), mean(data$nh_8_visit), mean(data$nh_15_visit), mean(data$nh_22_visit), mean(data$nh_29_visit), mean(data$nh_36_visit), mean(data$nh_43_visit))
    nh_visit_total = sum(nh_visit)
    
    com_inc_df = data.frame(com_residents = com_res_total, com_staff = com_staff_total, com_visitors = com_visit_total, com_total = sum(com_res_total, com_staff_total, com_visit_total),
                            nh_residents = nh_res_total, nh_staff = nh_staff_total, nh_visitors = nh_visit_total, nh_total = sum(nh_res_total, nh_staff_total, nh_visit_total))
  }else{
    com_inc_df = data.frame(com_residents = com_res_total, com_staff = com_staff_total, com_total = sum(com_res_total, com_staff_total),
                            nh_residents = nh_res_total, nh_staff = nh_staff_total, nh_total = sum(nh_res_total, nh_staff_total))
  }
}
