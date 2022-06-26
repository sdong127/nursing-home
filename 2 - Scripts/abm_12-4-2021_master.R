#************************************** Nursing Home ABM ****************************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

#' @import tidyverse
#' @import igraph
#' @import tictoc
library(tidyverse)
library(igraph)
library(tictoc)
library(data.table)

synthpop = read.csv("/Users/sdong217/Desktop/COVID_NH/NursingHome/nursing-home/0 - Synthetic Population/synthpop.csv")


#' Structure nursing home and staff/visitor-resident relationships
#'
#' This function sorts nursing home residents into rooms (42 doubles, 36 singles), adds staffing shifts,
#' option for cohorting among staff and residents, and allows/assigns visitors to residents.
#'
#' @param synthpop synthetic population; defaults to synthpop_NH stored in file
#' @param cohorting assign staff to residents; defaults to T
#' @param visitors allow visitors; defaults to F
#'
#' @return out data frame of structured nursing home
#'
#' @export
make_NH = function(synthpop, cohorting = T, visitors = FALSE){
  
  # select residents who live with roommates
  doubles = subset(synthpop, private_room == FALSE) %>%
    
    # sort residents into rooms
    mutate(room.placeholder = rep(1:nrow(subset(synthpop, private_room == FALSE))/2, each = 2, 
                                  length.out = nrow(subset(synthpop, private_room == FALSE))),
           room = as.numeric(as.factor(paste(room.placeholder)))) %>%
    
    # get rid of room placeholder variable
    dplyr::select(-room.placeholder) %>%
    
    # randomly reorder residents in rooms
    mutate(room = sample(room)) %>% group_by(room)
  
  
  # make data frame for the rest of the nursing home synthpop
  others = data.frame(subset(synthpop, (private_room == T & type == 0) | type != 0))
  # assign room numbers to residents with private rooms
  shared_id = (nrow(doubles)/2)+1
  for(row in 1:nrow(subset(synthpop, private_room == T & type == 0))){
    if(others$type[row] == 0){
      others$room[row] = shared_id
    }
    shared_id = shared_id+1
  }
  others[others$type == 1,]$room = 99  # 99 indicates staff
  
  # bind doubles and others data frames
  rooms = doubles %>% bind_rows(others) %>%
    arrange(type, room)
  
  # ------------------------------------------------------------------------------------------------
  
  # assign shifts to direct care staff
  rn_cohort_morning = subset(rooms, type == 1 & role == 0)[1:5,] %>% mutate(rn_cohort_morning = 1:5)
  rn_cohort_evening = subset(rooms, type == 1 & role == 0)[6:10,] %>% mutate(rn_cohort_evening = 6:10) 
  rn_cohort_night = subset(rooms, type == 1 & role == 0)[11:13,] %>% mutate(rn_cohort_night = 11:13)
  rn = rn_cohort_morning %>% bind_rows(rn_cohort_evening) %>% bind_rows(rn_cohort_night)
  
  lpn_cohort_morning = subset(rooms, type == 1 & role == 1)[1:3,] %>% mutate(lpn_cohort_morning = 1:3) 
  lpn_cohort_evening = subset(rooms, type == 1 & role == 1)[4:6,] %>% mutate(lpn_cohort_evening = 4:6)
  lpn_cohort_night = subset(rooms, type == 1 & role == 1)[7:8,] %>% mutate(lpn_cohort_night = 7:8)
  lpn = lpn_cohort_morning %>% bind_rows(lpn_cohort_evening) %>% bind_rows(lpn_cohort_night)
  
  cna_cohort_morning = subset(rooms, type == 1 & role == 2)[1:15,] %>% mutate(cna_cohort_morning = 1:15)  
  cna_cohort_evening = subset(rooms, type == 1 & role == 2)[16:30,] %>% mutate(cna_cohort_evening = 16:30)
  cna_cohort_night = subset(rooms, type == 1 & role == 2)[31:39,] %>% mutate(cna_cohort_night = 31:39)
  cna = cna_cohort_morning %>% bind_rows(cna_cohort_evening) %>% bind_rows(cna_cohort_night)
  
  ma_cohort_morning = subset(rooms, type == 1 & role == 3)[1:2,] %>% mutate(ma_cohort_morning = 1:2) 
  ma_cohort_evening = subset(rooms, type == 1 & role == 3)[3,] %>% mutate(ma_cohort_evening = 3)
  med_aide = ma_cohort_morning %>% bind_rows(ma_cohort_evening)
  
  admin_cohort_morning = subset(rooms, type == 1 & role == 4)[1:10,] %>% mutate(admin_cohort_morning = 1:10)
  admin_cohort_evening = subset(rooms, type == 1 & role == 4)[11:20,] %>% mutate(admin_cohort_evening = 11:20)
  admin = admin_cohort_morning %>% bind_rows(admin_cohort_evening)
  
  # ------------------------------------------------------------------------------------------------
  
  residents = subset(rooms, type == 0)
  
  # assign direct care staff to residents
  if(cohorting == TRUE){
    
    res_counter = 1
    rn_morning = nrow(rn_cohort_morning)
    for(i in 1:rn_morning){
      residents[res_counter:(res_counter+(nrow(residents)/rn_morning)-1),"rn_cohort_morning"] = i
      res_counter = res_counter+nrow(residents)/rn_morning
    }
    res_counter = 1
    rn_evening = nrow(rn_cohort_evening)
    for(i in ((rn_morning+1):(rn_morning+rn_evening))){
      residents[res_counter:(res_counter+(nrow(residents)/rn_evening)-1),"rn_cohort_evening"] = i
      res_counter = res_counter+nrow(residents)/rn_evening
    }
    res_counter = 1
    rn_night = nrow(rn_cohort_night)
    for(i in ((rn_morning+rn_evening+1):(rn_morning+rn_evening+rn_night))){
      residents[res_counter:(res_counter+(nrow(residents)/rn_night)-1),"rn_cohort_night"] = i
      res_counter = res_counter+nrow(residents)/rn_night
    }
    res_counter = 1
    lpn_morning = nrow(lpn_cohort_morning)
    for(i in 1:lpn_morning){
      residents[res_counter:(res_counter+(nrow(residents)/lpn_morning)-1),"lpn_cohort_morning"] = i
      res_counter = res_counter+nrow(residents)/lpn_morning
    }
    res_counter = 1
    lpn_evening = nrow(lpn_cohort_evening)
    for(i in ((lpn_morning+1):(lpn_morning+lpn_evening))){
      residents[res_counter:(res_counter+(nrow(residents)/lpn_evening)-1),"lpn_cohort_evening"] = i
      res_counter = res_counter+nrow(residents)/lpn_evening
    }
    res_counter = 1
    lpn_night = nrow(lpn_cohort_night)
    for(i in ((lpn_morning+lpn_evening+1):(lpn_morning+lpn_evening+lpn_night))){
      residents[res_counter:(res_counter+(nrow(residents)/lpn_night)-1),"lpn_cohort_night"] = i
      res_counter = res_counter+nrow(residents)/lpn_night
    }
    res_counter = 1
    cna_morning = nrow(cna_cohort_morning)
    for(i in 1:cna_morning){
      residents[res_counter:(res_counter+(nrow(residents)/cna_morning)-1),"cna_cohort_morning"] = i
      res_counter = res_counter+nrow(residents)/cna_morning
    }
    res_counter = 1
    cna_evening = nrow(cna_cohort_evening)
    for(i in ((cna_morning+1):(cna_morning+cna_evening))){
      residents[res_counter:(res_counter+(nrow(residents)/cna_evening)-1),"cna_cohort_evening"] = i
      res_counter = res_counter+nrow(residents)/cna_evening
    }
    res_counter = 1
    cna_night = nrow(cna_cohort_night)
    for(i in ((cna_morning+cna_evening+1):(cna_morning+cna_evening+cna_night))){
      residents[res_counter:(res_counter+(nrow(residents)/cna_night)-1),"cna_cohort_night"] = i
      res_counter = res_counter+nrow(residents)/cna_night
    }
    res_counter = 1
    med_aide_morning = nrow(ma_cohort_morning)
    for(i in 1:med_aide_morning){
      residents[res_counter:(res_counter+(nrow(residents)/med_aide_morning)-1),"ma_cohort_morning"] = i
      res_counter = res_counter+nrow(residents)/med_aide_morning
    }
    res_counter = 1
    med_aide_evening = nrow(ma_cohort_evening)
    for(i in ((med_aide_morning+1):(med_aide_morning+med_aide_evening))){
      residents[res_counter:(res_counter+(nrow(residents)/med_aide_evening)-1),"ma_cohort_evening"] = i
      res_counter = res_counter+nrow(residents)/med_aide_evening
    }
    
  }else{
    
  }
  
  # bind residents, staff, and visitors into dataframe
  out = residents %>% bind_rows(rn) %>% bind_rows(lpn) %>% bind_rows(cna) %>% 
    bind_rows(med_aide) %>% bind_rows(admin)
  
  # ------------------------------------------------------------------------------------------------
  
  # add visitors and assign to residents with family column
  if(visitors){
    # visitors represented by type = 2
    visitors = data.frame(id = (nrow(synthpop)+1):(nrow(synthpop)+nrow(residents)), type = 2, 
                          family = 1:nrow(residents), comorbid = 0)
    out['family'] = ifelse(out$type == 0, 1:nrow(residents), NA)
    
    out = out %>% bind_rows(visitors)
  }
  
  return(out)
  
}


#' Initialize nursing home
#'
#' This function takes in a data frame exported by make_NH().
#' It adds epidemiological attributes of the full nursing home community.

#' @param rel_trans_room_symp_res Additional relative attack rate of a symptomatic infected resident in shared room; 
#' defaults to 1 (used to be rel_trans_HH_symp_child)
#' @param p_asymp_nonres Fraction of non-residents with asymptomatic disease; defaults to 0.5 (used to be p_asymp_adult)
#' @param p_asymp_res Fraction of residents with asymptomatic disease; defaults to 0.4 (used to be p_asymp_child)
#' @param p_subclin_nonres Fraction of non-residents with subclinical but not techincally asymptomatic disease; defaults to 0
#' @param p_subclin_res Fraction of residents with subclinical but not techincally asymptomatic disease; defaults to 0
#' @param daily_attack Average daily attack rate for mild infection; defaults to 0.18
#' @param res_vax Vaccination rate of residents; defaults to some amount (used to be child_vax)
#' @param staff_vax_req Whether staff are required to get vaccine; defaults to F
#' @param staff_vax Vaccination rate of visitors; defaults to some amount based on community vax rate
#' @param visit_vax Vaccination rate of visitors; defaults to some amount based on community vax rate
#' @param staff_trans_red Factor by which staff transmissibility is reduced due to intervention; defaults to 1
#' (used to be teacher_trans)
#' @param visit_trans_red Factor by which visitor transmissibility is reduced due to intervention; defaults to 1
#' (used to be family_susp)
#' @param res_trans_red Factor by which resident transmissibility is reduced due to intervention; defaults to 1
#' @param staff_susp_red Factor by which staff susceptibility is reduced due to intervention; defaults to 1
#' (used to be teacher_trans)
#' @param visit_susp_red Factor by which visitor susceptibility is reduced due to intervention; defaults to 1
#' (used to be family_susp)
#' @param res_susp_red Factor by which resident susceptibility is reduced due to intervention; defaults to 1
#' @param disperse_transmission Whether transmission is overdispersed (vs. all have equal attack rate); default to T
#' @param isolate Whether symptomatic individuals isolate when symptoms emerge; defaults to T
#' @param vax_eff Vaccine efficacy, defaults to 0
#' @param start Data frame from make_NH()
#'
#' @return df data frame of resident and staff attributes.
#'
#' @export
initialize_NH = function(rel_trans_room_symp_res = 1, 
                         p_asymp_nonres = .5, p_asymp_res = .4, p_subclin_nonres = 0, p_subclin_res = 0,
                         daily_attack = .18, staff_vax_req = F, res_vax = 0, staff_vax = 0, visit_vax = 0, 
                         staff_trans_red = 1, visit_trans_red = 1, res_trans_red = 1, 
                         staff_susp_red = 1, visit_susp_red = 1, res_susp_red = 1, 
                         disperse_transmission = T, isolate = T, vax_eff = 0, start){
  
  n = nrow(start)
  
  # vax values for staff
  if(staff_vax_req == F){
    staff_vax = staff_vax 
  } else{
    staff_vax = 1
  }
  
  # initialize data frame
  df = start %>%
    mutate(cohorting = ifelse(is.na(rn_cohort_morning) & type==0, F, ifelse(type==0, T, F)),
           start = F,
           start.init = F,
           t_exposed = -99,
           t_inf = -1,
           symp = NA,
           sub_clin = NA,
           t_symp = -1,
           t_end_inf = -1,
           t_end_inf_home = -1,
           t_notify = -17, # time at which infected finds out they are positive
           t_quarantine = -13,
           t_end_quarantine = -13,
           tot_inf = 0,
           detected = 0,
           quarantined = F,
           test = 0,
           test_ct = 0,
           relative_trans_room_symp_res = ifelse(type != 0, 0, rel_trans_room_symp_res),
           daily_attack_rate = daily_attack,
           source = 0,
           source_symp = NA,
           tot_inf = 0,
           super_spread = disperse_transmission,
           location = "",
           
           # trackers for unit testing
           person.days.at.risk.room.res = 0,
           person.days.at.risk.room.staff = 0,
           person.days.at.risk.room.visit = 0,
           person.days.at.risk.common.res = 0,
           person.days.at.risk.common.staff = 0,
           person.days.at.risk.staff.staff = 0,
           # inf_days = 0,
           # symp_days = 0,
           # symp_and_inf_days = 0,
           # last = 0,
           
    ) %>%
    mutate(p_asymp = ifelse(type != 0, p_asymp_nonres, p_asymp_res),
           p_subclin = ifelse(type!=0, p_subclin_nonres, p_subclin_res),
           
           # isolation
           isolate = rbinom(n(), size = 1, prob = isolate),
           
           # transmission probability by 8-hr time period per day
           room_trans_prob = 1-(1-daily_attack)^(1/3), # used to be class_trans_prob
           # room_trans_prob = ifelse(type == 0 & comorbid == 1, room_trans_prob*res_trans_red*1/2, room_trans_prob),
           # room_trans_prob = ifelse(type == 0 & comorbid == 2, room_trans_prob*res_trans_red*1/4, room_trans_prob),
           room_trans_prob = ifelse(type == 1, room_trans_prob*staff_trans_red, room_trans_prob),
           room_trans_prob = ifelse(type == 2, room_trans_prob*visit_trans_red, room_trans_prob),
           
           # susceptibility
           res_vax_val = res_vax, # used to be child_vax_val
           staff_vax_val = staff_vax,
           visit_vax_val = visit_vax,
           vax_eff_val = vax_eff,
           
           vacc = ifelse(type != 0, 1, rbinom(n, size = 1, prob = res_vax_val)),
           vacc = ifelse(type == 1, rbinom(n, size = 1, prob = staff_vax_val), vacc),
           vacc = ifelse(type == 2, rbinom(n, size = 1, prob = visit_vax_val), vacc),
           
           susp = ifelse(vacc==0, 1, 1-vax_eff_val),
           susp = ifelse(type==0, susp*res_susp_red, susp),
           susp = ifelse(type==1, susp*staff_susp_red, susp),
           susp = ifelse(type==2, susp*visit_susp_red, susp))
  
  return(df)
}


#' Make schedule
#'
#' Make a schedule of when staff and visitors are present/absent
#'
#' @param time number of days; defaults to 45 (to capture 30-day picture)
#' @param nh data frame from initialize_NH()
#'
#' @return d Returns an n x time data frame that indicates whether an individual is in the 
#' nursing home at a particular time
#'
#' @export
make_schedule = function(time = 45, nh){
  
  # basic time vector
  vec = data.frame(
    
    # time since start in 8-hour shifts
    t = rep(1:time)
    
  )
  
  # replicate for each person
  vec_exp = vec %>% slice(rep(1:n(), times = nrow(nh))) 
  vec_exp$id = rep(1:nrow(nh), each = time)
  
  
  # time matrix for residents, staff, visitors
  if("family" %in% colnames(nh)){
    d = select(nh, id, type, role, room, family, rn_cohort_morning, rn_cohort_evening, rn_cohort_night, 
               lpn_cohort_morning, lpn_cohort_evening, lpn_cohort_night, cna_cohort_morning, 
               cna_cohort_evening, cna_cohort_night, ma_cohort_morning, ma_cohort_evening,
               admin_cohort_morning, admin_cohort_evening) %>% left_join(vec_exp, "id")
    
    # mark staff and residents present based on time of day
    d$shift = ifelse(d$type==0, "all", "absent")
    d$shift = ifelse(d$type==1 & (!is.na(d$rn_cohort_morning) | !is.na(d$lpn_cohort_morning) | 
                                    !is.na(d$cna_cohort_morning) | !is.na(d$ma_cohort_morning) | 
                                    !is.na(d$admin_cohort_morning)), "morning", d$shift)
    d$shift = ifelse(d$type==1 & (!is.na(d$rn_cohort_evening) | !is.na(d$lpn_cohort_evening) | 
                                    !is.na(d$cna_cohort_evening) | !is.na(d$ma_cohort_evening) | 
                                    !is.na(d$admin_cohort_evening)), "evening", d$shift)
    d$shift = ifelse(d$type==1 & (!is.na(d$rn_cohort_night) | !is.na(d$lpn_cohort_night) | 
                                    !is.na(d$cna_cohort_night)), "night", d$shift)
    
    # mark visitors present in the mornings, rotating throughout the week
    # each visitor comes 4-5 times a month
    visitor_sched = d$shift[d$type == 2]
    i = 1
    while(i < (nrow(subset(d, type == 2)) - time)){
      for(j in seq(from=1, to=time, by=7)){
        visitor_sched[i+j-1] = "morning"
      }
      i = i + time + 1
    }
    d$shift[d$type == 2] = visitor_sched
    
  }else{ # time matrix for just residents and staff
    d = select(nh, id, type, role, room, rn_cohort_morning, rn_cohort_evening, rn_cohort_night,
               lpn_cohort_morning, lpn_cohort_evening, lpn_cohort_night, cna_cohort_morning, 
               cna_cohort_evening, cna_cohort_night, ma_cohort_morning, ma_cohort_evening,
               admin_cohort_morning, admin_cohort_evening) %>% left_join(vec_exp, "id")
    
    # mark staff and residents present based on time of day
    d$shift = ifelse(d$type==0, "all", "absent")
    d$shift = ifelse(d$type==1 & (!is.na(d$rn_cohort_morning) | !is.na(d$lpn_cohort_morning) | 
                                    !is.na(d$cna_cohort_morning) | !is.na(d$ma_cohort_morning) | 
                                    !is.na(d$admin_cohort_morning)), "morning", d$shift)
    d$shift = ifelse(d$type==1 & (!is.na(d$rn_cohort_evening) | !is.na(d$lpn_cohort_evening) | 
                                    !is.na(d$cna_cohort_evening) | !is.na(d$ma_cohort_evening) | 
                                    !is.na(d$admin_cohort_evening)), "evening", d$shift)
    d$shift = ifelse(d$type==1 & (!is.na(d$rn_cohort_night) | !is.na(d$lpn_cohort_night) | 
                                    !is.na(d$cna_cohort_night)), "night", d$shift)
  }
  
  # assign staff to residents and randomize each day if no cohorting
  if(is.na(nh$rn_cohort_morning[nh$id==1])){
    rn_morning = nh[!is.na(nh$rn_cohort_morning),]$rn_cohort_morning
    rn_evening = nh[!is.na(nh$rn_cohort_evening),]$rn_cohort_evening
    rn_night = nh[!is.na(nh$rn_cohort_night),]$rn_cohort_night
    lpn_morning = nh[!is.na(nh$lpn_cohort_morning),]$lpn_cohort_morning
    lpn_evening = nh[!is.na(nh$lpn_cohort_evening),]$lpn_cohort_evening
    lpn_night = nh[!is.na(nh$lpn_cohort_night),]$lpn_cohort_night
    cna_morning = nh[!is.na(nh$cna_cohort_morning),]$cna_cohort_morning
    cna_evening = nh[!is.na(nh$cna_cohort_evening),]$cna_cohort_evening
    cna_night = nh[!is.na(nh$cna_cohort_night),]$cna_cohort_night
    ma_morning = nh[!is.na(nh$ma_cohort_morning),]$ma_cohort_morning
    ma_evening = nh[!is.na(nh$ma_cohort_evening),]$ma_cohort_evening
    
    for(i in 1:time){
      
      # assign morning staff
      res = 1
      for(j in rn_morning){
        d$rn_cohort_morning[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(rn_morning))-1) 
                            & d$type==0 & d$t==i] = sample(rn_morning,1)
        res = res + (nrow(subset(nh,type==0))/length(rn_morning))
      }
      res = 1
      for(j in lpn_morning){
        d$lpn_cohort_morning[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(lpn_morning))-1) 
                             & d$type==0 & d$t==i] = sample(lpn_morning,1)
        res = res + (nrow(subset(nh,type==0))/length(lpn_morning))
      }
      res = 1
      for(j in cna_morning){
        d$cna_cohort_morning[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(cna_morning))-1) 
                             & d$type==0 & d$t==i] = sample(cna_morning,1)
        res = res + (nrow(subset(nh,type==0))/length(cna_morning))
      }
      res = 1
      for(j in ma_morning){
        d$ma_cohort_morning[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(ma_morning))-1) 
                            & d$type==0 & d$t==i] = sample(ma_morning,1)
        res = res + (nrow(subset(nh,type==0))/length(ma_morning))
      }
      
      # assign evening staff
      res = 1
      for(j in rn_evening){
        d$rn_cohort_evening[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(rn_evening))-1) 
                            & d$type==0 & d$t==i] = sample(rn_evening,1)
        res = res + (nrow(subset(nh,type==0))/length(rn_evening))
      }
      res = 1
      for(j in lpn_evening){
        d$lpn_cohort_evening[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(lpn_evening))-1) 
                             & d$type==0 & d$t==i] = sample(lpn_evening,1)
        res = res + (nrow(subset(nh,type==0))/length(lpn_evening))
      }
      res = 1
      for(j in cna_evening){
        d$cna_cohort_evening[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(cna_evening))-1) 
                             & d$type==0 & d$t==i] = sample(cna_evening,1)
        res = res + (nrow(subset(nh,type==0))/length(cna_evening))
      }
      d$ma_cohort_evening[d$type==0 & d$t==i] = 3
      
      # assign night staff
      res = 1
      for(j in rn_night){
        d$rn_cohort_night[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(rn_night))-1) 
                          & d$type==0 & d$t==i] = sample(rn_night,1)
        res = res + (nrow(subset(nh,type==0))/length(rn_night))
      }
      res = 1
      for(j in lpn_night & i%%3==0){
        d$lpn_cohort_night[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(lpn_night))-1) 
                           & d$type==0 & d$t==i] = sample(lpn_night,1)
        res = res + (nrow(subset(nh,type==0))/length(lpn_night))
      }
      res = 1
      for(j in cna_night & i%%3==0){
        d$cna_cohort_night[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(cna_night))-1) 
                           & d$type==0 & d$t==i] = sample(cna_night,1)
        res = res + (nrow(subset(nh,type==0))/length(cna_night))
      }
    }
    
  }
  
  return(d)
  
}


#' Create cohorted dataframe 
#'
#' Store information about which staff are assigned to which residents
#'
#' @param nh data frame from initialize_NH()
#'
#' @return cohorts list of dataframes of each resident's staff
#'
#' @export
make_cohort = function(nh){
  
  cohorts = vector(mode="list", length=nrow(nh[nh$type==0,]))
  
  for(res in 1:nrow(nh[nh$type==0,])){
    
    staff_vec = list(as.data.table(nh[nh$rn_cohort_morning==nh$rn_cohort_morning[nh$id==res] & nh$type==1,][rowSums(is.na(nh[nh$rn_cohort_morning==nh$rn_cohort_morning[nh$id==res] & nh$type==1,])) 
                                                                                                                                != ncol(nh[nh$rn_cohort_morning==nh$rn_cohort_morning[nh$id==res] & nh$type==1,]),]),
                     as.data.table(nh[nh$rn_cohort_evening==nh$rn_cohort_evening[nh$id==res] & nh$type==1,][rowSums(is.na(nh[nh$rn_cohort_evening==nh$rn_cohort_evening[nh$id==res] & nh$type==1,])) 
                                                                                                                                != ncol(nh[nh$rn_cohort_evening==nh$rn_cohort_evening[nh$id==res] & nh$type==1,]),]),
                     as.data.table(nh[nh$rn_cohort_night==nh$rn_cohort_night[nh$id==res] & nh$type==1,][rowSums(is.na(nh[nh$rn_cohort_night==nh$rn_cohort_night[nh$id==res] & nh$type==1,])) 
                                                                                                                          != ncol(nh[nh$rn_cohort_night==nh$rn_cohort_night[nh$id==res] & nh$type==1,]),]),
                     as.data.table(nh[nh$lpn_cohort_morning==nh$lpn_cohort_morning[nh$id==res] & nh$type==1,][rowSums(is.na(nh[nh$lpn_cohort_morning==nh$lpn_cohort_morning[nh$id==res] & nh$type==1,])) 
                                                                                                                                  != ncol(nh[nh$lpn_cohort_morning==nh$lpn_cohort_morning[nh$id==res] & nh$type==1,]),]),
                     as.data.table(nh[nh$lpn_cohort_evening==nh$lpn_cohort_evening[nh$id==res] & nh$type==1,][rowSums(is.na(nh[nh$lpn_cohort_evening==nh$lpn_cohort_evening[nh$id==res] & nh$type==1,])) 
                                                                                                                                  != ncol(nh[nh$lpn_cohort_evening==nh$lpn_cohort_evening[nh$id==res] & nh$type==1,]),]),
                     as.data.table(nh[nh$lpn_cohort_night==nh$lpn_cohort_night[nh$id==res] & nh$type==1,][rowSums(is.na(nh[nh$lpn_cohort_night==nh$lpn_cohort_night[nh$id==res] & nh$type==1,])) 
                                                                                                                            != ncol(nh[nh$lpn_cohort_night==nh$lpn_cohort_night[nh$id==res] & nh$type==1,]),]),
                     as.data.table(nh[nh$cna_cohort_morning==nh$cna_cohort_morning[nh$id==res] & nh$type==1,][rowSums(is.na(nh[nh$cna_cohort_morning==nh$cna_cohort_morning[nh$id==res] & nh$type==1,])) 
                                                                                                                                  != ncol(nh[nh$cna_cohort_morning==nh$cna_cohort_morning[nh$id==res] & nh$type==1,]),]),
                     as.data.table(nh[nh$cna_cohort_evening==nh$cna_cohort_evening[nh$id==res] & nh$type==1,][rowSums(is.na(nh[nh$cna_cohort_evening==nh$cna_cohort_evening[nh$id==res] & nh$type==1,])) 
                                                                                                                                  != ncol(nh[nh$cna_cohort_evening==nh$cna_cohort_evening[nh$id==res] & nh$type==1,]),]),
                     as.data.table(nh[nh$cna_cohort_night==nh$cna_cohort_night[nh$id==res] & nh$type==1,][rowSums(is.na(nh[nh$cna_cohort_night==nh$cna_cohort_night[nh$id==res] & nh$type==1,])) 
                                                                                                                            != ncol(nh[nh$cna_cohort_night==nh$cna_cohort_night[nh$id==res] & nh$type==1,]),]),
                     as.data.table(nh[nh$ma_cohort_morning==nh$ma_cohort_morning[nh$id==res] & nh$type==1,][rowSums(is.na(nh[nh$ma_cohort_morning==nh$ma_cohort_morning[nh$id==res] & nh$type==1,])) 
                                                                                                                                != ncol(nh[nh$ma_cohort_morning==nh$ma_cohort_morning[nh$id==res] & nh$type==1,]),]),
                     as.data.table(nh[nh$ma_cohort_evening==nh$ma_cohort_evening[nh$id==res] & nh$type==1,][rowSums(is.na(nh[nh$ma_cohort_evening==nh$ma_cohort_evening[nh$id==res] & nh$type==1,])) 
                                                                                                                                != ncol(nh[nh$ma_cohort_evening==nh$ma_cohort_evening[nh$id==res] & nh$type==1,]),]))
    staff_vec = rbindlist(staff_vec)
    
    cohorts[[res]] <- as.data.frame(staff_vec)
  }
  
  return(cohorts)

}


#' Set room transmission (used to be run_household)
#'
#' Determine who is infected at a timestep
#' in the same room as an infected individual
#'
#' @param a id of infected individual
#' @param df data frame in run_model()
#' @param t current day
#' @param quarantine whether quarantine occurs
#' @param cohorts list of dataframes with staff vectors for each resident
#'
#' @return infs id of infected individuals
#'
#' @export
run_room = function(a, df, t, quarantine, cohorts){
  
  ## if infected is resident
  if(df$type[df$id==a]==0){
    
    # if resident has roommate
    if(df$room[df$id==a]<34 & sum(df$room[df$type==0]==df$room[df$id==a])>1){
      # roommate
      roommate_vec = df[df$room==df$room[df$id==a] & df$id!=a,]
      roommate_vec = roommate_vec[rowSums(is.na(roommate_vec)) != ncol(roommate_vec),]
      
      # determine whether roommate becomes infected
      prob_roommate = rbinom(nrow(roommate_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*roommate_vec$susp*roommate_vec$present_susp < 1,
                                                                         df$room_trans_prob[df$id==a]*roommate_vec$susp*roommate_vec$present_susp,
                                                                         1))
      roommate = roommate_vec$id
      
      # list infected roommate
      roommate_infs = roommate*prob_roommate
      
    }else{roommate_infs = 0}
    
    # make vector of staff in infected resident's room at current time
    staff_vec = cohorts[[a]] %>% filter(id%in%df$id)
    staff_vec$susp = (df %>% filter(id%in%staff_vec$id))$susp
    staff_vec$present_susp = (df %>% filter(id%in%staff_vec$id))$present_susp
    
    # determine whether staff becomes infected
    if(quarantine & ((df$t_notify[df$id==a]<=t & df$t_notify[df$id==a]!=-17) | (df$symp[df$id==a]==1 & df$t_symp[df$id==a]<=t & df$t_symp[df$id==a]!=-1))){
      prob_staff = rbinom(nrow(staff_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*staff_vec$susp*staff_vec$present_susp*.5 < 1,
                                                                   df$room_trans_prob[df$id==a]*staff_vec$susp*staff_vec$present_susp*.5,
                                                                   1))
    }else{
      prob_staff = rbinom(nrow(staff_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*staff_vec$susp*staff_vec$present_susp < 1,
                                                                   df$room_trans_prob[df$id==a]*staff_vec$susp*staff_vec$present_susp,
                                                                   1))
    }
    
    staff = staff_vec$id
    
    # list infected staff
    staff_infs = staff*prob_staff
    
    # make vector of resident's visitor present at NH
    if('family' %in% colnames(df) & df$flag_fam[df$id==a]!=1 & any(df$shift=="morning" & df$type==2)){
      visitor_id = df$id[df$family==df$family[df$id==a] & df$id!=a]
      visitor_id = visitor_id[!is.na(visitor_id)]
      
      if(length(visitor_id)>0){
        visitor = df[df$id==visitor_id,]
        if(visitor$shift!="absent") {visit_vec = visitor}
      }
      
      # determine whether visitor becomes infected
      if(length(visit_vec)>0){
        prob_visit = rbinom(nrow(visit_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*visit_vec$susp*visit_vec$present_susp < 1,
                                                                     df$room_trans_prob[df$id==a]*visit_vec$susp*visit_vec$present_susp,
                                                                     1))
        visit = visit_vec$id
        
        # list infected visitors
        visit_infs = visit*prob_visit
        
      }else{visit_infs = 0}
    }else{visit_infs = 0}
    
    infs = c(roommate_infs, staff_infs, visit_infs)
    
    
    ## if infected is direct-care staff
  } else if(df$type[df$id==a]==1 & df$role[df$id==a]!=4 & df$shift[df$id==a]!="absent"){
    
    # find out what role they are
    staff_row = df[df$id==a,][, c("rn_cohort_morning", "rn_cohort_evening", "rn_cohort_night", 
                                  "lpn_cohort_morning", "lpn_cohort_evening", "lpn_cohort_night",
                                  "cna_cohort_morning", "cna_cohort_evening", "cna_cohort_night",
                                  "ma_cohort_morning", "ma_cohort_evening", "admin_cohort_morning",
                                  "admin_cohort_evening")]
    for(role in 1:length(staff_row)){
      if(!is.na(staff_row[role])){
        staff_role = names(staff_row)[role]
      }
    }
    staff_role_id = staff_row[!is.na(staff_row)][[1]]
    
    # make vector of residents that staff treats
    res_vec = df[df[staff_role]==staff_role_id & df$type==0,]
    res_vec = res_vec[rowSums(is.na(res_vec)) != ncol(res_vec),]
    
    # determine whether residents become infected
    # prob_res = ifelse(res_vec$quarantined, rbinom(nrow(res_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp*.5 < 1,
    #                                                                                      df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp*.5,
    #                                                                                      1)), 
    #                   rbinom(nrow(res_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp < 1, 
    #                                                                 df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp, 1)))
    prob_res = rbinom(nrow(res_vec), size = 1, prob = ifelse(res_vec$quarantined, ifelse(df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp*.5 < 1,
                                                                                         df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp*.5,
                                                                                         1),
                                                             ifelse(df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp < 1,
                                                                    df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp, 1)))
    
    
    res = res_vec$id
    
    # list infected residents
    res_infs = res*prob_res
    
    # make vector of residents' visitor present at NH
    if('family' %in% colnames(df) & any(df$shift=="morning" & df$type==2) & length(res)>0){
      visit_vec <- c()
      for(b in res){
        if(df$flag_fam[df$id==b]!=1){
          visitor_id = df$id[df$family==df$family[df$id==b] & df$id!=b]
          visitor_id = visitor_id[!is.na(visitor_id)]
          if(length(visitor_id)>0){
            visitor = df[df$id==visitor_id,]
            if(visitor$shift!="absent") {visit_vec = as.data.frame(append(visit_vec, visitor))}
          }
        }
      }
      # determine whether visitor becomes infected
      if(length(visit_vec)>0){
        prob_visit = rbinom(nrow(visit_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*visit_vec$susp*visit_vec$present_susp < 1,
                                                                     df$room_trans_prob[df$id==a]*visit_vec$susp*visit_vec$present_susp,
                                                                     1))
        visit = visit_vec$id
        
        # list infected visitors
        visit_infs = visit*prob_visit
        
      }else{visit_infs = 0}
    }else{visit_infs = 0}
    
    infs = c(res_infs, visit_infs)
    
    
    ## if infected is visitor  
  } else if(df$type[df$id==a]==2 & df$shift[df$id==a]!="absent"){
    
    # make vector of residents that visitor sees, including roommates
    res_vec = df[df$family==df$family[df$id==a] & df$id!=a,]
    res_vec = res_vec[rowSums(is.na(res_vec)) != ncol(res_vec),]
    res_id = res_vec$id
    if(df$room[df$id==res_id]<34){
      roommate = df[df$room==res_vec$room & df$id!=res_id,]
      res_vec[nrow(res_vec) + 1,] = roommate[rowSums(is.na(roommate)) != ncol(roommate),]
    }
    
    # determine whether residents become infected
    prob_res = rbinom(nrow(res_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp < 1,
                                                             df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp,
                                                             1))
    res = res_vec$id
    
    # list infected residents
    res_infs = res*prob_res
    
    # make vector of staff in visitor's resident's room at current time
    staff_vec = cohorts[[res_id]] %>% filter(id%in%df$id)
    staff_vec$susp = (df %>% filter(id%in%staff_vec$id))$susp
    staff_vec$present_susp = (df %>% filter(id%in%staff_vec$id))$present_susp
    
    # determine whether staff becomes infected
    if(quarantine & ((df$t_notify[df$id==a]<=t & df$t_notify[df$id==a]!=-17) | (df$symp[df$id==a]==1 & df$t_symp[df$id==a]<=t & df$t_symp[df$id==a]!=-1))){
      prob_staff = rbinom(nrow(staff_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*staff_vec$susp*staff_vec$present_susp*.5 < 1,
                                                                   df$room_trans_prob[df$id==a]*staff_vec$susp*staff_vec$present_susp*.5,
                                                                   1))
    }else{
      prob_staff = rbinom(nrow(staff_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*staff_vec$susp*staff_vec$present_susp < 1,
                                                                   df$room_trans_prob[df$id==a]*staff_vec$susp*staff_vec$present_susp,
                                                                   1))
    }
    
    staff = staff_vec$id
    
    # list infected staff
    staff_infs = staff*prob_staff
    
    infs = c(res_infs, staff_infs)
    
  }else{
    infs = 0
  }
  
  
  #print(df$class_trans_prob[df$id==a]*df$relative_trans_HH[df$id==a]*HH_vec$susp*HH_vec$not_inf)
  return(infs)
}


#' Set common area transmission (used to be run_rand)
#'
#' Determine who is infected at a timestep
#' from common area contact with an infected individual
#'
#' @param a id of infected individual
#' @param df data frame in run_model()
#' @param n_contact_common number of contacts staff/visitors encounter in nursing home common area
#' @param rel_trans_staff relative transmission in common area interactions (vs. resident room);
#' defaults to 1/4
#'
#' @return infs id of infected individuals
#'
#' @export
run_common = function(a, df, n_contact_common, rel_trans_common = 1/4){
  
  if(n_contact_common>0){
    # pull contacts from random graph of residents/staff present in nursing home common area
    tot = length(df$id[!df$isolated & !df$quarantined & df$type!=2 & df$id!=a])
    contact_take = ifelse(n_contact_common<=tot, n_contact_common, tot)
    contact_id = sample(df$id[!df$isolated & !df$quarantined & df$type!=2 & df$id!=a], contact_take, replace=F)
    contacts = df[df$id %in% contact_id,]
    id.susp = contacts[contacts$present_susp & contacts$susp!=0,]$id
    #print(dim(contacts))
    
    # determine whether a contact becomes infected
    prob_common = rbinom(nrow(contacts), size = 1,
                        prob = ifelse(df$room_trans_prob[df$id==a]*contacts$susp*contacts$present_susp*rel_trans_common < 1,
                                      df$room_trans_prob[df$id==a]*contacts$susp*contacts$present_susp*rel_trans_common,
                                      1))
    # infected individuals
    infs = contacts$id*prob_common
    
    return(list(infs, id.susp))
  }
  else{
    return(0)
  }
}



#' Set staff transmission (used to be run_staff_rand)
#'
#' Determine who is infected at a timestep
#' from contact between in-nursing-home staff
#'
#' @param a id of infected staff member
#' @param df school data frame in run_model()
#' @param n_contact_staff number of contacts staff encounters in nursing home during shift
#' @param rel_trans_staff relative transmission in staff-staff interactions (vs. resident room);
#' defaults to 1/4, used to be rel_trans_adult
#'
#' @return infs id of infected individuals
#'
#' @export
run_staff = function(a, df, n_contact_staff, rel_trans_staff = 1/4){
  
  if(n_contact_staff>0){
    # pull contacts from random graph of staff present in nursing home
    tot = length(df$id[!df$isolated & df$type == 1 & df$id!=a])
    contact_take = ifelse(n_contact_staff<=tot, n_contact_staff, tot)
    contact_id = sample(df$id[!df$isolated & df$type == 1 & df$id!=a], contact_take, replace=F)
    contacts = df[df$id %in% contact_id,]
    id.susp = contacts[contacts$present_susp & contacts$susp != 0,]$id
    #print(dim(contacts))
    
    # determine whether a contact becomes infected
    prob_staff = rbinom(nrow(contacts), size = 1,
                        prob = ifelse(df$room_trans_prob[df$id==a]*contacts$susp*contacts$present_susp*rel_trans_staff < 1,
                                      df$room_trans_prob[df$id==a]*contacts$susp*contacts$present_susp*rel_trans_staff,
                                      1))
    # infected individuals
    infs = contacts$id*prob_staff
    
    return(list(infs, id.susp))
  }
  else{
    return(0)
  } 
}



#' Set infection parameters
#'
#' Set infection parameters for individuals infected at a particular timestep
#'
#' @param df.u data frame in run_model() of infected individuals
#' @param days_inf_mild length of infectious period for mild COVID, defaults to 5 (used to be days_inf)
#' @param days_inf_mod length of infectious period for moderate COVID, defaults to 10
#' @param days_inf_severe length of infectious period for severe COVID, defaults to 20
#' @param set indication of seeding model vs. creating infections
#' @param mult_asymp_res multiplier on asymptomatic infection for residents; default is 1 (used to be mult_asymp)
#' @param mult_asymp_nonres multiplier on asymptomatic infection for staff and visitors; default is 1 (used to be mult_asymp_child)
#' @param seed_asymp when making a seed, force to be asymptomatic; default is false
#' @param overdisp_off all overdispersion off; defaults to F
#'
#' @return df.u with updated parameters
#'
#' @export
# note to self -- add additional parameters to change around here
make_infected = function(df.u, days_inf = 5, set = NA, mult_asymp_res = 1, mult_asymp_nonres = 1, seed_asymp = F, overdisp_off = F){
  
  if(is.na(set)[1]){
    #  set infectivity  parameters
    df.u$symp = rbinom(nrow(df.u), size = 1, prob = 1-df.u$p_asymp)
    df.u$sub_clin = ifelse(df.u$symp, rbinom(nrow(df.u), size = 1, prob =  df.u$p_subclin/(1-df.u$p_asymp)), 1)
    df.u$t_symp = df.u$t_exposed + rgamma(nrow(df.u), shape = 5.8, scale=0.95)
    val = rnorm(nrow(df.u), mean = 2, sd = 0.4)
    df.u$t_inf = ifelse(df.u$t_symp - val > df.u$t_exposed + 1, df.u$t_symp - val, df.u$t_exposed + 1)
    
  } else{
    #  set infectivity  parameters
    if(seed_asymp) {
      df.u$symp = 0
      df.u$sub_clin = 0
    }else{
      df.u$symp = rbinom(nrow(df.u), size = 1, prob = 1-df.u$p_asymp)
      df.u$sub_clin = ifelse(df.u$symp, rbinom(nrow(df.u), size = 1, prob =  df.u$p_subclin/(1-df.u$p_asymp)), 1)
    }
    df.u$t_inf = set + runif(nrow(df.u), min = -0.5, max = 0.5)
    df.u$t_symp = df.u$t_inf + rnorm(nrow(df.u), mean = 2, sd = 0.4)
    symp_gap <- rgamma(nrow(df.u), shape = 5.8, scale=.95)
    df.u$t_exposed = ifelse(df.u$t_symp - symp_gap < df.u$t_inf - 1, df.u$t_symp - symp_gap, df.u$t_inf - 1)
  }
  
  # add overdispersion
  attack_mult = rlnorm(nrow(df.u), meanlog = log(.84)-log((.84^2+.3)/.84^2)/2, sdlog = sqrt(log((.84^2+.3)/.84^2)))/.84
  chk = (df.u$super_spread | (df.u$type==1 | df.u$type==2))*as.numeric(!overdisp_off)
  df.u$room_trans_prob = ifelse(chk, df.u$room_trans_prob*attack_mult, df.u$room_trans_prob)
  
  # adjust for symptomatic residents
  df.u$room_trans_prob = ifelse(df.u$symp, ifelse(df.u$type==0, df.u$room_trans_prob*df.u$relative_trans_room_symp_res, df.u$room_trans_prob), df.u$room_trans_prob)
  
  # adjust for asymptomatic infection
  df.u$room_trans_prob = ifelse(!df.u$symp, ifelse(df.u$type==0, df.u$room_trans_prob*mult_asymp_res, df.u$room_trans_prob*mult_asymp_nonres), df.u$room_trans_prob)
  # df.u$relative_trans_common = ifelse(df.u$symp & df.u$isolate==0, df.u$relative_trans_common*df.u$relative_trans_room_symp_res, df.u$relative_trans_common)
  
  # add end time
  # df.u$t_end_inf_home = ifelse(df.u$comorbid==0, df.u$t_symp + 
  #                                rlnorm(nrow(df.u), meanlog = log(days_inf_mild)-log((days_inf_mild^2 + 2)/days_inf_mild^2)/2, sdlog = sqrt(log((days_inf_mild^2 + 2)/days_inf_mild^2))), df.u$t_end_inf_home)
  # df.u$t_end_inf_home = ifelse(df.u$comorbid==1, df.u$t_symp +
  #                                rlnorm(nrow(df.u), meanlog = log(days_inf_mod)-log((days_inf_mod^2 + 2)/days_inf_mod^2)/2, sdlog = sqrt(log((days_inf_mod^2 + 2)/days_inf_mod^2))), df.u$t_end_inf_home)
  # df.u$t_end_inf_home = ifelse(df.u$comorbid==2, df.u$t_symp +
  #                                rlnorm(nrow(df.u), meanlog = log(days_inf_severe)-log((days_inf_severe^2 + 2)/days_inf_severe^2)/2, sdlog = sqrt(log((days_inf_severe^2 + 2)/days_inf_severe^2))), df.u$t_end_inf_home)
  df.u$t_end_inf_home = df.u$t_inf +
    rlnorm(nrow(df.u), meanlog = log(days_inf)-log((days_inf^2 + 2)/days_inf^2)/2, sdlog = sqrt(log((days_inf^2 + 2)/days_inf^2)))
  
  df.u$t_end_inf = ifelse(df.u$symp==1 & !df.u$sub_clin & df.u$isolate & df.u$t_symp<df.u$t_end_inf_home, df.u$t_symp, df.u$t_end_inf_home)
  
  df.u$t_notify = ifelse(df.u$symp==1 & !df.u$sub_clin & df.u$isolate, df.u$t_symp, -17)
  
  # make unsusceptible
  df.u$susp = 0
  
  return(df.u)
}


# skip these:
# inf_days = 0,
# inf_home_days = 0,
# symp_days = 0,
# symp_and_inf_days = 0,
# last = 0,

#' Run model
#'
#' Perform a single model run
#'
#' @param time length of time to run model; defaults to 30
#' @param test whether there is weekly testing; defaults to F
#' @param test_sens test sensitivity; defaults to 0.7
#' @param test_frac fraction of nursing home tested; defaults to 0.9
#' @param test_days test frequency; "day", "week", "2x_week"; defaults to "week"
#' @param test_type group tested; defaults to "all", also allows "residents" and "staff"
#' @param test_start_day day tests are implemented for weekly testing; defaults to 1 = Monday
#' @param n_contact_common number of contacts a staff/visitor has with other staff/visitors in the common area; defaults to 6
#' @param n_contact_staff number of contacts a staff member has with other staff members during a shift; defaults to 10
#' @param n_start number of infections to seed model; defaults to 1
#' @param mult_asymp_res multiplier on asymptomatic infection for residents; default is 1 (used to be mult_asymp)
#' @param mult_asymp_nonres multiplier on asymptomatic infection for staff and visitors; default is 1 (used to be mult_asymp_child)
#' @param days_inf_mild length of infectious period for mild COVID, defaults to 5 days (used to be days_inf)
#' @param days_inf_mod length of infectious period for moderate COVID, defaults to 10 days
#' @param days_inf_severe length of infectious period for severe COVID, defaults to 20 days
#' @param seed_asymp whether to seed with an asymptomatic case
#' @param time_seed_inf time(s) at which to introduce new infectious individuals; defaults to NA and randomly selects one time
#' @param start_type type of seed; default is "mix" (also "resident", "staff", "visitor", "cont")
#' @param start_mult value to indicate relative frequency of resident/staff infections; defaults to 1 
#' (staff 2x as likely as residents since residents don't leave) (are staff or residents more likely to get infected?)
#' @param nonres_prob if start_type = "cont", set daily probability of infectious entry for staff and visitors, defaults to .05 (used to be child_prob)
#' @param rel_trans_common relative transmission in common area interactions vs. resident's room; defaults to 1/4
#' @param rel_trans_staff relative transmission in staff-staff interactions vs. resident's room; defaults to 1/4 (used to be rel_trans_adult)
#' @param quarantine whether or not people quarantine upon exposure; defaults to F
#' @param quarantine.length length of quarantine when someone is infectious; defaults to 5
#' @param overdisp_off all overdispersion off; defaults to F
#' @param df data frame from initialize_NH()
#' @param sched schedule data frame from make_schedule()
#'
#' @return df updated df with transmission results
#' @return time_seed_inf when the first individual was dropped in
#'
#' @export
#### NOTE: I found this to be slower when coded w/tidyverse.
#### Therefore for the most part, this is coded in base.
run_model = function(time = 30,
                     test = T,
                     test_days = "week",
                     test_sens =  .7,
                     test_frac = .9,
                     test_start_day = 1,
                     n_contact_common = 6,
                     n_contact_staff = 10,
                     n_start = 1,
                     days_inf = 5,
                     # days_inf_mild = 5,
                     # days_inf_mod = 10, 
                     # days_inf_severe = 20,
                     mult_asymp_res = 1,
                     mult_asymp_nonres = 1,
                     seed_asymp = F,
                     time_seed_inf = NA,
                     start_type = "cont",
                     start_mult = 1,
                     nonres_prob = 0.001,
                     quarantine = F,
                     quarantine.length = 5,
                     rel_trans_common = 1/4,
                     rel_trans_staff = 1/4,
                     test_type = "all",
                     overdisp_off = F,
                     df = nh, sched, cohorts){
  
  #### SEED MODEL ####
  # seed with an infectious case
  if(is.na(time_seed_inf)) time_seed_inf = sample(1:14, 1)     # any time in the cycle
  
  # any individual not visitor
  # note staff 2x as likely as residents to be infected
  if(start_type=="mix") id.samp = sample(df$id[!df$type==2], n_start, prob = (df$type==1*start_mult+1)/(sum(df$type==1*start_mult+1) + sum(!df$type==1)))              
  
  # specific types
  if(start_type=="resident") id.samp = sample(df$id[df$type==0], n_start)      
  if(start_type=="staff") id.samp = sample(df$id[df$type==1], n_start)              
  if(start_type=="visitor") id.samp = sample(df$id[df$type==2], n_start)
  
  # vary over time
  if(start_type == "cont"){
    
    # pull out IDs of staff/visitors
    nonres_IDs = df$id[!df$type==0]
    
    # pick times
    vec = 1:(time+15)
    nonres_pulls = rbinom(time+15, size = length(nonres_IDs), prob = nonres_prob)
    nonres_times = rep(vec, nonres_pulls)
    
    # pick people
    nonres = sample(nonres_IDs, length(nonres_times))
    
    # set up vector
    time_seed_inf = nonres_times
    
    id.samp = nonres
    df.temp = data.frame(id.samp, time_seed_inf) %>% arrange(id.samp) %>%
      left_join(df, c("id.samp" = "id")) %>% filter(susp!=0)
    time_seed_inf = 15 # start on Monday morning with testing
    
    
  }else{
    # compress if time_seed_inf is a vector
    df.temp = data.frame(time_seed_inf, id.samp) # backward compatibility
  }
  
  df$start.time = time_seed_inf
  
  # setup
  if(nrow(df.temp)>0){
    df[df$id%in%df.temp$id.samp,] = make_infected(df.u = df[df$id%in%df.temp$id.samp,], days_inf = days_inf,
                                                  set = df.temp$time_seed_inf, seed_asymp = seed_asymp,
                                                  mult_asymp_res = mult_asymp_res, mult_asymp_nonres = mult_asymp_nonres,
                                                  overdisp_off = overdisp_off)
    df$start = df$id %in% df.temp$id.samp
    df$start.init = df$id %in% df.temp$id.samp
    
  }
  
  # test days
  # if null, make this Monday
  if(test_days == "week") {testing_days = seq(test_start_day, (time+15), by = 7)}
  if(test_days == "day") {testing_days = 1:(time+15)}
  if(test_days == "2x_week"){testing_days = c(seq(5, (time+15), by = 7), seq(1, (time+15), by = 7))}
  # df$switch = 0
  # df$temp_switch = 0
  
  #print(testing_days)
  
  # testing (visitors don't get tested)
  if(test_type=="all"){df$test_type = df$type!=2 & (!df$vacc | test_frac>=0.7)
  } else if(test_type=="residents"){df$test_type = df$type==0
  } else if(test_type=="staff"){df$test_type = df$type==1}
  # room_test_ind = 0
  # room_test_ind_q = 0
  # test_frac_orig = test_frac
  # df$uh.oh = 0
  
  # infectious days
  df$days_inf = days_inf
  # df$days_inf = ifelse(df$comorbid==0, days_inf_mild, df$days_inf)
  # df$days_inf = ifelse(df$comorbid==1, days_inf_mod, df$days_inf)
  # df$days_inf = ifelse(df$comorbid==2, days_inf_severe, df$days_inf)
  
  
  #print(paste("start notification:", df$t_notify[df$start]))
  # run over time steps
  for(t in time_seed_inf:(time_seed_inf+time-1)){
    #print(paste("Time:", t, sched$day[sched$t==t][1], sched$group_two[sched$t==t][1]))
    
    # assign direct-care staff accordingly if not cohorted
    if(df$cohorting[df$id==1][1]==F){
      for(res in 1:nrow(df[df$type==0,])){
        df$rn_cohort_morning[df$id==res] <- sched$rn_cohort_morning[sched$id==res & sched$t==t]
        df$rn_cohort_evening[df$id==res] <- sched$rn_cohort_evening[sched$id==res & sched$t==t]
        df$rn_cohort_night[df$id==res] <- sched$rn_cohort_night[sched$id==res & sched$t==t]
        df$lpn_cohort_morning[df$id==res] <- sched$lpn_cohort_morning[sched$id==res & sched$t==t]
        df$lpn_cohort_evening[df$id==res] <- sched$lpn_cohort_evening[sched$id==res & sched$t==t]
        df$lpn_cohort_night[df$id==res] <- sched$lpn_cohort_night[sched$id==res & sched$t==t]
        df$cna_cohort_morning[df$id==res] <- sched$cna_cohort_morning[sched$id==res & sched$t==t]
        df$cna_cohort_evening[df$id==res] <- sched$cna_cohort_evening[sched$id==res & sched$t==t]
        df$cna_cohort_night[df$id==res] <- sched$cna_cohort_night[sched$id==res & sched$t==t]
        df$ma_cohort_morning[df$id==res] <- sched$ma_cohort_morning[sched$id==res & sched$t==t]
        df$ma_cohort_evening[df$id==res] <- sched$ma_cohort_evening[sched$id==res & sched$t==t]
        staff_vec = list(as.data.table(df[df$rn_cohort_morning==df$rn_cohort_morning[df$id==res] & df$type==1,][rowSums(is.na(df[df$rn_cohort_morning==df$rn_cohort_morning[df$id==res] & df$type==1,])) 
                                                                                                                != ncol(df[df$rn_cohort_morning==df$rn_cohort_morning[df$id==res] & df$type==1,]),]),
                         as.data.table(df[df$rn_cohort_evening==df$rn_cohort_evening[df$id==res] & df$type==1,][rowSums(is.na(df[df$rn_cohort_evening==df$rn_cohort_evening[df$id==res] & df$type==1,])) 
                                                                                                                != ncol(df[df$rn_cohort_evening==df$rn_cohort_evening[df$id==res] & df$type==1,]),]),
                         as.data.table(df[df$rn_cohort_night==df$rn_cohort_night[df$id==res] & df$type==1,][rowSums(is.na(df[df$rn_cohort_night==df$rn_cohort_night[df$id==res] & df$type==1,])) 
                                                                                                            != ncol(df[df$rn_cohort_night==df$rn_cohort_night[df$id==res] & df$type==1,]),]),
                         as.data.table(df[df$lpn_cohort_morning==df$lpn_cohort_morning[df$id==res] & df$type==1,][rowSums(is.na(df[df$lpn_cohort_morning==df$lpn_cohort_morning[df$id==res] & df$type==1,])) 
                                                                                                                  != ncol(df[df$lpn_cohort_morning==df$lpn_cohort_morning[df$id==res] & df$type==1,]),]),
                         as.data.table(df[df$lpn_cohort_evening==df$lpn_cohort_evening[df$id==res] & df$type==1,][rowSums(is.na(df[df$lpn_cohort_evening==df$lpn_cohort_evening[df$id==res] & df$type==1,])) 
                                                                                                                  != ncol(df[df$lpn_cohort_evening==df$lpn_cohort_evening[df$id==res] & df$type==1,]),]),
                         as.data.table(df[df$lpn_cohort_night==df$lpn_cohort_night[df$id==res] & df$type==1,][rowSums(is.na(df[df$lpn_cohort_night==df$lpn_cohort_night[df$id==res] & df$type==1,])) 
                                                                                                              != ncol(df[df$lpn_cohort_night==df$lpn_cohort_night[df$id==res] & df$type==1,]),]),
                         as.data.table(df[df$cna_cohort_morning==df$cna_cohort_morning[df$id==res] & df$type==1,][rowSums(is.na(df[df$cna_cohort_morning==df$cna_cohort_morning[df$id==res] & df$type==1,])) 
                                                                                                                  != ncol(df[df$cna_cohort_morning==df$cna_cohort_morning[df$id==res] & df$type==1,]),]),
                         as.data.table(df[df$cna_cohort_evening==df$cna_cohort_evening[df$id==res] & df$type==1,][rowSums(is.na(df[df$cna_cohort_evening==df$cna_cohort_evening[df$id==res] & df$type==1,])) 
                                                                                                                  != ncol(df[df$cna_cohort_evening==df$cna_cohort_evening[df$id==res] & df$type==1,]),]),
                         as.data.table(df[df$cna_cohort_night==df$cna_cohort_night[df$id==res] & df$type==1,][rowSums(is.na(df[df$cna_cohort_night==df$cna_cohort_night[df$id==res] & df$type==1,])) 
                                                                                                              != ncol(df[df$cna_cohort_night==df$cna_cohort_night[df$id==res] & df$type==1,]),]),
                         as.data.table(df[df$ma_cohort_morning==df$ma_cohort_morning[df$id==res] & df$type==1,][rowSums(is.na(df[df$ma_cohort_morning==df$ma_cohort_morning[df$id==res] & df$type==1,])) 
                                                                                                                != ncol(df[df$ma_cohort_morning==df$ma_cohort_morning[df$id==res] & df$type==1,]),]),
                         as.data.table(df[df$ma_cohort_evening==df$ma_cohort_evening[df$id==res] & df$type==1,][rowSums(is.na(df[df$ma_cohort_evening==df$ma_cohort_evening[df$id==res] & df$type==1,])) 
                                                                                                                != ncol(df[df$ma_cohort_evening==df$ma_cohort_evening[df$id==res] & df$type==1,]),]))
        staff_vec = rbindlist(staff_vec)
        
        cohorts[[res]] <- as.data.frame(staff_vec)
      }
    }
    
    df$shift = sched$shift[sched$t==t]
    
    # mark who is infectious (either at home or in NH)
    df$inf = df$t_inf > -1 & df$t_inf <= t & df$t_end_inf_home >= t
    
    # mark who is isolated where
    df$isolate_home = df$isolate & df$inf & df$t_notify <= t & df$t_notify!=-17 & df$t_end_inf_home>=t & df$type!=0
    df$isolate_room = df$isolate & df$inf & df$t_notify <= t & df$t_notify!=-17 & df$t_end_inf_home>=t & df$type==0
    
    # checks
    # df$inf_days = df$inf_days + df$inf
    # df$symp_days = df$symp_days + ifelse(df$symp_now==1 & !is.na(df$symp_now), 1, 0)
    # df$symp_and_inf_days = df$symp_and_inf_days + df$symp_now*df$inf
    # df$last = ifelse(df$inf, t, df$last)
    
    # infectious and meant to be at nursing home
    df$trans_now = df$shift!="absent" & df$inf
    
    # flag infected residents and their roommates
    df$flag_room = 0
    df$flag_room[df$trans_now & df$type==0] <- 1
    if(sum(df$flag_room)>0){
      flagged_rooms = unique(df$room[df$flag_room==1])
    } else{flagged_rooms = 0}
    df$flag_room[df$type==0 & df$room%in%flagged_rooms] <- 1
    
    # flag infected residents' visitors
    df$flag_fam = 0
    if("family"%in% colnames(df)){
      df$flag_fam[df$trans_now & df$type==0] <- 1
      if(sum(df$flag_fam)>0){
        flagged_families = unique(df$family[df$flag_fam==1])
      } else{flagged_families = 0}
      df$flag_fam[df$type==2 & df$family%in%flagged_families] <- 1
    }
    
    # quarantine
    if(quarantine){
      df$quarantined = df$flag_room==1 & df$type==0 & !df$trans_now
      df$quarantined = ifelse(df$t_quarantine<=t & df$t_quarantine!=-13 & df$t_end_quarantine>=t, T, df$quarantined)
      # df$t_end_quarantine = ifelse(df$t_quarantine<=t & df$t_quarantine!=-13 & df$t_notify!=-17, df$t_notify, df$t_end_quarantine)
    }
    
    # re-estimate who is present (among staff and visitors) DOUBLE CHECK
    df$shift[df$type!=0] = ifelse(sched$shift[sched$t==t & sched$type!=0]!="absent" & !df$isolate_home[df$type!=0] & !df$quarantined[df$type!=0], df$shift[df$type!=0], "absent")
    if("family"%in%colnames(df)){
      df$shift[df$type==2] = ifelse(sched$shift[sched$t==t & sched$type==2]!="absent" & df$flag_fam[df$type==2]!=1 & !df$isolate_home[df$type==2] & !df$quarantined[df$type==2], df$shift[df$type==2], "absent")
    }
    df$inf = df$t_inf > -1 & df$t_inf <= t & df$t_end_inf_home >= t
    
    df$not_inf = df$t_exposed==-99 | df$t_exposed>t # if exposed from community, can be exposed earlier
    if(t==15) df$not_inf_keep = df$not_inf
    df$present_susp = df$shift!="absent" & df$not_inf
    df$isolated = df$isolate_home | df$isolate_room
    #print(sum(as.numeric(df$q_out)))
    #print(sum(as.numeric(df$q_out & sched$present[sched$t==t])))
    
    # update infectious and at nursing home
    df$trans_now = df$shift!="absent" & df$inf
    
    # check who is transmissible right now
    # mat[,(t-time_seed_inf+1)] = df$trans_now
    
    # set infections to 0 for this timestep
    df$now = F
    
    ## group testing
    if(test & t%in%testing_days){
      #print(test); print(t); print(testing_days)
      #print(t)
      #print("got to testing"); print(dim(df)); print(df %>% group_by(vacc) %>% summarize(sum(test_type)))
      
      df$test_ct = df$test_ct + rbinom(nrow(df), size = 1, prob = (df$shift!="absent")*test_frac*as.numeric(df$test_type)) # count how many tests individual takes
      df$test = rbinom(nrow(df), size = 1, prob = test_sens*test_frac*as.numeric(df$test_type)) # record only accurate test results?
      df$t_end_inf = ifelse(df$test & df$trans_now, t, df$t_end_inf)
      df$t_notify = ifelse(df$test & df$trans_now, t, df$t_notify)
      df$detected = ifelse(df$test & df$trans_now, 1, df$detected)
      df$t_end_quarantine = ifelse(df$t_quarantine<=t & df$t_quarantine!=-13 & df$t_notify!=-17, df$t_notify, df$t_end_quarantine)
      # room_test_ind = room_test_ind + length(unique(df$room[df$test_type & df$present & !df$isolated & !df$q_room]))
      
      #print(paste("Time:", t))
      #print(sum(df$test))
      #print(sum(df$inf & df$test & df$present))
      #print(df$id[df$inf & df$test & df$present])
      #print(df$class[df$inf & df$test & df$present])
      #print(df$family[df$inf & df$test & df$present])
    }
    #print(testing_days)
    #print(test_frac)
    
    #### SELECT NEXT GENERATION INFECTIONS ####
    # run model for infectious individuals in resident room
    if(sum(df$trans_now)>0) {
      
      room_infs = df$id[df$trans_now & (df$type==0 | (df$type==1 & df$role!=4) | df$type==2)]
      if(length(room_infs)>1) room_infs = sample(room_infs)
      
      room_inf_vec_total <- c()
      for(a in room_infs){
        room_inf_vec <- c()
        
        # ROOM CONTACTS
        for(shift in 1:3){
          if(shift==1 & (df$shift[df$id==a]=="morning" | df$shift[df$id==a]=="all")){
            infs = run_room(a, df[df$shift=="morning" | df$shift=="all",], t, quarantine, cohorts)
            room_inf_vec[(length(room_inf_vec)+1):(length(room_inf_vec)+length(infs))] = infs
          }
          if(shift==2 & (df$shift[df$id==a]=="evening" | df$shift[df$id==a]=="all")){
            infs = run_room(a, df[df$shift=="evening" | df$shift=="all",], t, quarantine, cohorts)
            room_inf_vec[(length(room_inf_vec)+1):(length(room_inf_vec)+length(infs))] = infs
          }
          if(shift==3 & (df$shift[df$id==a]=="night" | df$shift[df$id==a]=="all")){
            infs = run_room(a, df[df$shift=="night" | df$shift=="all",], t, quarantine, cohorts)
            room_inf_vec[(length(room_inf_vec)+1):(length(room_inf_vec)+length(infs))] = infs
          }
          room_inf_vec = room_inf_vec[!room_inf_vec==0]
          df$present_susp[df$id%in%room_inf_vec] = 0
          if(length(room_inf_vec)>0){room_inf_vec_total[(length(room_inf_vec_total)+1):(length(room_inf_vec_total)+length(room_inf_vec))] = room_inf_vec}
        } 
        
        df$location[df$id%in%room_inf_vec] = "Room"
        
        # if infected is resident
        if(df$type[df$id==a]==0){
          
          # check staff in resident's room
          staff_vec = cohorts[[a]] %>% filter(id%in%df$id)
          staff_vec$susp = (df %>% filter(id%in%staff_vec$id))$susp
          staff_vec$present_susp = (df %>% filter(id%in%staff_vec$id))$present_susp
          
          #Track risk set for unit testing
          df$person.days.at.risk.room.res[df$id == a] <- df$person.days.at.risk.room.res[df$id == a] + 
            (df$t_inf[df$id == a] <= t & df$t_end_inf_home[df$id == a] >= t)*sum(df$present_susp[df$room==df$room[df$id==a] &
                                                                                                   df$type==0 & df$susp!=0])
          df$person.days.at.risk.room.staff[df$id == a] <- df$person.days.at.risk.room.staff[df$id == a] +
            (df$t_inf[df$id == a] <= t & df$t_end_inf_home[df$id == a] >= t)*sum(df$present_susp[df$id%in%staff_vec$id & df$type==1 & df$susp!=0])
          if("family" %in% colnames(df)){
            df$person.days.at.risk.room.visit[df$id == a] <- df$person.days.at.risk.room.visit[df$id == a] +
              (df$t_inf[df$id == a] <= t & df$t_end_inf_home[df$id == a] >= t)*sum(df$present_susp[df$family==df$family[df$id==a] &
                                                                                                     df$type==2 & df$susp!=0])
          }
        }
        
        # if infected is direct-care staff
        if(df$type[df$id==a]==1 & df$role[df$id==a]!=4){
          
          # find out what role they are
          staff_row = df[df$id==a,][, c("rn_cohort_morning", "rn_cohort_evening", "rn_cohort_night", 
                                        "lpn_cohort_morning", "lpn_cohort_evening", "lpn_cohort_night",
                                        "cna_cohort_morning", "cna_cohort_evening", "cna_cohort_night",
                                        "ma_cohort_morning", "ma_cohort_evening", "admin_cohort_morning",
                                        "admin_cohort_evening")]
          for(role in 1:length(staff_row)){
            if(!is.na(staff_row[role])){
              staff_role = names(staff_row)[role]
            }
          }
          staff_role_id = staff_row[!is.na(staff_row)][[1]]
          
          # make vector of residents that staff treats
          res_vec = df[df[staff_role]==staff_role_id & df$type==0,]
          res_vec = res_vec[rowSums(is.na(res_vec)) != ncol(res_vec),]
          res = res_vec$id
          
          # quarantine
          # if pre/asymptomatic, find when infected tests positive/symptoms show
          if(test & quarantine){
            # get next day of testing
            future_days<-c()
            for(day in testing_days){
              future_days[length(future_days)+1] = ifelse(day-t>0, day, 0)
            }
            next_day = t+min(abs(future_days-t))
            
            res_vec_id = res_vec$id
            
            df$t_quarantine[df$id%in%res_vec_id & !df$id%in%room_inf_vec] = ifelse(df$symp[df$id==a]==1 & df$t_symp[df$id==a]!=-1, 
                                                          df$t_symp[df$id==a], 
                                                          ifelse(test, ifelse(next_day-df$t_inf[df$id==a] < df$days_inf[df$id==a], next_day, df$t_quarantine[df$id%in%res_vec_id & !df$id%in%room_inf_vec]), df$t_quarantine[df$id%in%res_vec_id & !df$id%in%room_inf_vec]))
            
            df$t_end_quarantine[df$id%in%res_vec_id & !df$id%in%room_inf_vec] = ifelse(df$t_quarantine[df$id%in%res_vec_id & !df$id%in%room_inf_vec]!=-13, 
                                                              df$t_quarantine[df$id%in%res_vec_id & !df$id%in%room_inf_vec]+quarantine.length, df$t_end_quarantine[df$id%in%res_vec_id & !df$id%in%room_inf_vec])
          }
          
          #Track risk set for unit testing
          df$person.days.at.risk.room.res[df$id == a] <- df$person.days.at.risk.room.res[df$id == a] +
            (df$shift[df$id==a]==sched$shift[sched$id==a & sched$t==t] & df$t_inf[df$id == a]<=t & df$t_end_inf_home[df$id == a]>=t)*sum(df$present_susp[df$id%in%res_vec$id &
                                                                                                                                                           df$type==0 & df$susp!=0])
          # check visitors in room
          if(df$shift[df$id==a]=="morning" & 'family' %in% colnames(df) & any(df$shift=="morning" & df$type==2) & length(res)>0){
            visit_vec <- c()
            for(b in res){
              if(df$flag_fam[df$id==b]!=1){
                visitor_id = df$id[df$family==df$family[df$id==b] & df$id!=b]
                visitor_id = visitor_id[!is.na(visitor_id)]
                if(length(visitor_id)>0){
                  visitor = df[df$id==visitor_id,]
                  if(visitor$shift!="absent") {visit_vec = as.data.frame(append(visit_vec, visitor))}
                }
              }
            }
            df$person.days.at.risk.room.visit[df$id == a] <- df$person.days.at.risk.room.visit[df$id == a] +
              (df$shift[df$id==a]==sched$shift[sched$id==a & sched$t==t] & df$t_inf[df$id == a] <= t & df$t_end_inf_home[df$id == a] >= t)*sum(df$present_susp[df$id%in%visit_vec &
                                                                                                                                                                 df$type==2 & df$susp != 0])
          }
        }
        
        # if infected is visitor
        if(df$type[df$id==a]==2){
          
          # make vector of residents that visitor sees, including roommates
          res_vec = df[df$family==df$family[df$id==a] & df$id!=a,]
          res_vec = res_vec[rowSums(is.na(res_vec)) != ncol(res_vec),]
          res_id = res_vec$id
          if(df$room[df$id==res_id]<34){
            roommate = df[df$room==res_vec$room & df$id!=res_id,]
            res_vec[nrow(res_vec) + 1,] = roommate[rowSums(is.na(roommate)) != ncol(roommate),]
          }
          
          # quarantine
          # if pre/asymptomatic, find when symptoms show
          if(quarantine){
            res_vec_id = res_vec$id
            
            df$t_quarantine[df$id%in%res_vec_id & !df$id%in%room_inf_vec] = ifelse(df$symp[df$id==a]==1 & df$t_symp[df$id==a]!=-1, df$t_symp[df$id==a], df$t_quarantine[df$id%in%res_vec_id & !df$id%in%room_inf_vec])
            
            df$t_end_quarantine[df$id%in%res_vec_id & !df$id%in%room_inf_vec] = 
              ifelse(df$t_quarantine[df$id%in%res_vec_id & !df$id%in%room_inf_vec]!=-13, df$t_quarantine[df$id%in%res_vec_id & !df$id%in%room_inf_vec]+quarantine.length, df$t_end_quarantine[df$id%in%res_vec_id & !df$id%in%room_inf_vec])
          }
          
          #Track risk set for unit testing
          df$person.days.at.risk.room.res[df$id == a] <- df$person.days.at.risk.room.res[df$id == a] +
            (df$shift[df$id==a]==sched$shift[sched$id==a & sched$t==t] & df$t_inf[df$id == a] <= t & df$t_end_inf_home[df$id == a] >= t)*sum(df$present_susp[df$id%in%res_vec$id &
                                                                                                                                                               df$type==0 & df$susp != 0])
          # make vector of staff in visitor's resident's room at current time
          staff_vec = cohorts[[res_id]] %>% filter(id%in%df$id)
          staff_vec$susp = (df %>% filter(id%in%staff_vec$id))$susp
          staff_vec$present_susp = (df %>% filter(id%in%staff_vec$id))$present_susp
          
          df$person.days.at.risk.room.staff[df$id == a] <- df$person.days.at.risk.room.staff[df$id == a] +
            (df$shift[df$id==a]==sched$shift[sched$id==a & sched$t==t] & df$t_inf[df$id == a] <= t & df$t_end_inf_home[df$id == a] >= t)*sum(df$present_susp[df$id%in%staff_vec$id & 
                                                                                                                                                               df$type==1 & df$susp != 0])
        }
        
        # add to total # of infections from this person
        df$tot_inf[df$id==a] = df$tot_inf[df$id==a] + sum(unique(room_inf_vec[!room_inf_vec==0])>0, na.rm=T)
        
        # flag people infected at this time step
        df$now = ifelse(df$id%in%room_inf_vec, T, df$now)
        df$source = ifelse(df$id%in%room_inf_vec, a, df$source)
        df$source_symp = ifelse(df$id%in%room_inf_vec, df$symp[df$id==a], df$source_symp)
        df$not_inf = ifelse(df$id%in%room_inf_vec, F, df$not_inf)
      }
      
      # run model for infectious individuals in staff interactions
      staff_infs = df$id[df$trans_now & df$type==1]
      if(length(staff_infs) > 1) staff_infs = sample(staff_infs)
      
      staff_inf_vec_total <- c()
      for(a in staff_infs){
        staff_inf_vec.out <- c()
        
        # STAFF INTERACTIONS
        for(shift in 1:3){
          if(shift==1 & (df$shift[df$id==a]=="morning")){
            staff_inf_vec.out = run_staff(a, df[df$shift=="morning" & df$type==1,], n_contact_staff, rel_trans_staff)
          }
          if(shift==2 & (df$shift[df$id==a]=="evening")){
            staff_inf_vec.out = run_staff(a, df[df$shift=="evening" & df$type==1,], n_contact_staff, rel_trans_staff)
          }
          if(shift==3 & (df$shift[df$id==a]=="night")){
            staff_inf_vec.out = run_staff(a, df[df$shift=="night" & df$type==1,], n_contact_staff, rel_trans_staff)
          }
          staff_inf_vec.out[[1]] = staff_inf_vec.out[[1]][!staff_inf_vec.out[[1]]==0]
        }
        df$present_susp[df$id%in%staff_inf_vec.out[[1]]] = 0
        staff_inf_vec <- staff_inf_vec.out[[1]]
        if(length(staff_inf_vec)>0){staff_inf_vec_total[(length(staff_inf_vec_total)+1):(length(staff_inf_vec_total)+length(staff_inf_vec))] = staff_inf_vec}
        #rand_trans = 0
        df$location[df$id%in%staff_inf_vec] = "Staff interactions"
        
        #Track risk set for unit testing
        df$person.days.at.risk.staff.staff[df$id == a] <- df$person.days.at.risk.staff.staff[df$id == a] + (df$shift[df$id==a]==sched$shift[sched$id==a & sched$t==t] & 
                                                                                                              df$t_inf[df$id == a] <= t & 
                                                                                                              df$t_end_inf[df$id == a] >= t)*sum(df$present_susp[df$id %in% staff_inf_vec.out[[2]] & df$susp!=0 & !(df$id%in%unique(room_inf_vec_total))])
        
        # add to total # of infections from this person
        df$tot_inf[df$id==a] = df$tot_inf[df$id==a] + sum(unique(staff_inf_vec[!staff_inf_vec==0])>0, na.rm=T)
        
        # flag people infected at this time step
        df$now = ifelse(df$id%in%staff_inf_vec, T, df$now)
        df$source = ifelse(df$id%in%staff_inf_vec, a, df$source)
        df$source_symp = ifelse(df$id%in%staff_inf_vec, df$symp[df$id==a], df$source_symp)
        df$not_inf = ifelse(df$id%in%staff_inf_vec, F, df$not_inf)
      }
      
      # run model for infectious individuals in common area of nursing home
      common_infs = df$id[df$trans_now & !df$isolated & !df$quarantined & df$type!=2]
      if(length(common_infs)>1) common_infs = sample(common_infs)
      
      for(a in common_infs){
        common_inf_vec.out <- c()
        
        # COMMON AREA INTERACTIONS
        for(shift in 1:3){
          if(shift==1 & (df$shift[df$id==a]=="morning" | df$shift[df$id==a]=="all")){
            infs = run_common(a, df[(df$shift=="morning" | df$shift=="all") & !df$isolated & !df$quarantined & df$type!=2,], n_contact_common, rel_trans_common)
            common_inf_vec.out = infs
            common_inf_vec.out[[1]] = common_inf_vec.out[[1]][!common_inf_vec.out[[1]]==0]
            df$present_susp[df$id%in%common_inf_vec.out[[1]]] = 0
          }
          if(shift==2 & (df$shift[df$id==a]=="evening" | df$shift[df$id==a]=="all")){
            infs = run_common(a, df[(df$shift=="evening" | df$shift=="all") & !df$isolated & !df$quarantined & df$type!=2,], n_contact_common, rel_trans_common)
            if(df$type[df$id==a]!=0){
              common_inf_vec.out = infs
              common_inf_vec.out[[1]] = common_inf_vec.out[[1]][!common_inf_vec.out[[1]]==0]
              df$present_susp[df$id%in%common_inf_vec.out[[1]]] = 0
            }else{
              if(length(infs[[1]])>0){common_inf_vec.out[[1]][(length(common_inf_vec.out[[1]])+1):(length(common_inf_vec.out[[1]])+length(infs[[1]]))] = infs[[1]]}
              if(length(infs[[2]])>0){common_inf_vec.out[[2]][(length(common_inf_vec.out[[2]])+1):(length(common_inf_vec.out[[2]])+length(infs[[2]]))] = infs[[2]]}
            }
          }
          if(shift==3 & (df$shift[df$id==a]=="night" | df$shift[df$id==a]=="all")){
            infs = run_common(a, df[(df$shift=="night" | df$shift=="all") & !df$isolated & !df$quarantined & df$type!=2,], n_contact_common, rel_trans_common)
            if(df$type[df$id==a]!=0){
              common_inf_vec.out = infs
              common_inf_vec.out[[1]] = common_inf_vec.out[[1]][!common_inf_vec.out[[1]]==0]
              df$present_susp[df$id%in%common_inf_vec.out[[1]]] = 0
            }else{
              if(length(infs[[1]])>0){common_inf_vec.out[[1]][(length(common_inf_vec.out[[1]])+1):(length(common_inf_vec.out[[1]])+length(infs[[1]]))] = infs[[1]]}
              if(length(infs[[2]])>0){common_inf_vec.out[[2]][(length(common_inf_vec.out[[2]])+1):(length(common_inf_vec.out[[2]])+length(infs[[2]]))] = infs[[2]]}
            }
          }
        }
        common_inf_vec <- common_inf_vec.out[[1]]
        df$location[df$id%in%common_inf_vec] = "Common area"
        
        # quarantine
        # if pre/asymptomatic, find when infected tests positive/symptoms show
        if(test & quarantine & length(df[df$id%in%common_inf_vec.out[[2]] & !df$id%in%common_inf_vec & (df$type!=1 | df$role==4),])>0){
          future_days <- c()
          for(day in testing_days){
            future_days[length(future_days)+1] = ifelse(day-t>0, day, 0)
          }
          next_day = t+min(abs(future_days-t))
          df$t_quarantine[df$id%in%common_inf_vec.out[[2]] & !df$id%in%common_inf_vec & (df$type!=1 | df$role==4)] = ifelse(df$symp[df$id==a]==1 & df$t_symp[df$id==a]!=-1, 
                                                                                                 df$t_symp[df$id==a], ifelse(test, 
                                                                                                                             ifelse(next_day-df$t_inf[df$id==a] < df$days_inf[df$id==a], next_day, 
                                                                                                                                    df$t_quarantine[df$id%in%common_inf_vec.out[[2]] & !df$id%in%common_inf_vec & (df$type!=1 | df$role==4)]), df$t_quarantine[df$id%in%common_inf_vec.out[[2]] & !df$id%in%common_inf_vec & (df$type!=1 | df$role==4)]))
          df$t_end_quarantine[df$id%in%common_inf_vec.out[[2]] & !df$id%in%common_inf_vec & (df$type!=1 | df$role==4)] = ifelse(df$t_quarantine[df$id%in%common_inf_vec.out[[2]] & !df$id%in%common_inf_vec & (df$type!=1 | df$role==4)]!=-13, 
                                                                                                     df$t_quarantine[df$id%in%common_inf_vec.out[[2]] & !df$id%in%common_inf_vec & (df$type!=1 | df$role==4)]+quarantine.length, 
                                                                                                     df$t_end_quarantine[df$id%in%common_inf_vec.out[[2]] & !df$id%in%common_inf_vec & (df$type!=1 | df$role==4)])
        }
        
        #Track risk set for unit testing
        df$person.days.at.risk.common.res[df$id == a] <- df$person.days.at.risk.common.res[df$id == a] + (df$shift[df$id==a]==sched$shift[sched$id==a & sched$t==t] & 
                                                                                                              df$t_inf[df$id == a] <= t & 
                                                                                                              df$t_end_inf[df$id == a] >= t)*sum(df$present_susp[df$id %in% unique(common_inf_vec.out[[2]]) & df$type==0 & df$susp!=0 & !(df$id %in% c(unique(room_inf_vec_total), unique(staff_inf_vec_total)))])
        df$person.days.at.risk.common.staff[df$id == a] <- df$person.days.at.risk.common.staff[df$id == a] + (df$shift[df$id==a]==sched$shift[sched$id==a & sched$t==t] & 
                                                                                                                  df$t_inf[df$id == a] <= t & 
                                                                                                                  df$t_end_inf[df$id == a] >= t)*sum(df$present_susp[df$id %in% unique(common_inf_vec.out[[2]]) & df$type==1 & df$susp!=0 & !(df$id %in% c(unique(room_inf_vec_total), unique(staff_inf_vec_total)))])
        
        # add to total # of infections from this person
        df$tot_inf[df$id==a] = df$tot_inf[df$id==a] + sum(unique(common_inf_vec[!common_inf_vec==0])>0, na.rm=T)
        
        # flag people infected at this time step
        df$now = ifelse(df$id%in%common_inf_vec, T, df$now)
        df$source = ifelse(df$id%in%common_inf_vec, a, df$source)
        df$source_symp = ifelse(df$id%in%common_inf_vec, df$symp[df$id==a], df$source_symp)
        df$not_inf = ifelse(df$id%in%common_inf_vec, F, df$not_inf)
      }
    }
    
    #### SET UP NEXT GENERATION INFECTEDS ####
    # update values updated at this stage
    # need to put in probability distributions
    if(sum(df$now)>0){
      
      df$start[df$now] = 0      # remove seed if infected earlier
      df$t_exposed[df$now] = t
      df[df$now,] = make_infected(df[df$now,], days_inf = days_inf, mult_asymp_res = mult_asymp_res, 
                                  mult_asymp_nonres = mult_asymp_nonres, overdisp_off = overdisp_off)
      #print("New exposures:")
      #print(df %>% filter(now) %>% arrange(source) %>% select(id, HH_id, class, group, adult, family, source, location, symp))
    }
    
    # round values
    df$t_notify = ceiling(df$t_notify)
    #print(t); print(class_quarantine)
    #print(df %>% #filter(!adult) %>%
    #        group_by(class) %>% summarize(mean(quarantined), sum(quarantined)))
    #if(sum(class_quarantine$t_notify!=-1)>0) print(class_quarantine)
  }
  return(df)
}


start = make_NH(synthpop = synthpop, cohorting = F, visitors = F)
nh = initialize_NH(rel_trans_room_symp_res = 1, 
                   p_asymp_nonres = 0, p_asymp_res = 0, p_subclin_nonres = 0, p_subclin_res = 0,
                   daily_attack = .18, staff_vax_req = F, res_vax = 0, staff_vax = 0, visit_vax = 0, 
                   staff_trans_red = 1, visit_trans_red = 1, res_trans_red = 1, 
                   staff_susp_red = 1, visit_susp_red = 1, res_susp_red = 1, 
                   disperse_transmission = T, isolate = T, vax_eff = 0, start = start)
cohorts = make_cohort(nh = nh)
sched = make_schedule(time = 45, nh = nh)



#' Run model multiple times and summarize results
#'
#' @param N number of runs
#' @param cohorting whether certain staff are assigned to certain residents; defaults to F
#' @param visitors whether visitors are allowed; defaults to F
#' @param rel_trans_common Relative attack rate of common area contact (vs. room); defaults to 1/4
#' @param rel_trans_staff Relative attack rate of staff-staff interactions (vs. room); defaults to 1/4
#' @param rel_trans_room_symp_res Additional relative attack rate of a symptomatic infected resident in shared room; defaults to 1
#' @param p_asymp_nonres Fraction of staff with asymptomatic (unsuspected) disease; defaults to 0.8
#' @param p_asymp_res Fraction of residents with asymptomatic (unsuspected) disease; defaults to 0.4
#' @param p_subclin_nonres Fraction of non-residents with subclinical but not techincally asymptomatic disease; defaults to 0
#' @param p_subclin_res Fraction of residents with subclinical but not techincally asymptomatic disease; defaults to 0
#' @param daily_attack Average daily attack rate in residents; defaults to 0.01
#' @param res_vax Vaccination rate of residents; defaults to 0
#' @param staff_vax_req Whether staff are required to get vaccine; defaults to F
#' @param staff_vax Vaccination rate of staff; defaults to some amount
#' @param visit_vax Vaccination rate of visitors; defaults to some amount
#' @param staff_trans_red Factor by which staff transmissibility is reduced due to intervention; defaults to 1
#' @param visit_trans_red Factor by which visitor transmissibility is reduced due to intervention; defaults to 1
#' @param res_trans_red Factor by which resident transmissibility is reduced due to intervention; defaults to 1
#' @param staff_susp_red Factor by which staff susceptibility is reduced due to intervention; defaults to 1
#' @param visit_susp_red Factor by which visitor susceptibility is reduced due to intervention; defaults to 1
#' @param res_susp_red Factor by which resident susceptibility is reduced due to intervention; defaults to 1
#' @param disperse_transmission Whether transmission is overdispersed (vs. all have equal attack rate); default to T
#' @param n_contact_common number of contacts a staff/resident has with other staff/residents in common area; defaults to 6
#' @param n_contact_staff number of contacts a staff member has with other staff members; defaults to 10
#' @param n_start number of infections to seed model; defaults to 1
#' @param time_seed_inf time(s) at which to introduce new infectious individuals; defaults to NA and randomly selects one time
#' @param days_inf_mild length of infectious period for mild COVID, defaults to 5 (used to be days_inf)
#' @param days_inf_mod length of infectious period for moderate COVID, defaults to 10
#' @param days_inf_severe length of infectious period for severe COVID, defaults to20
#' @param mult_asymp_res multiplier on asymptomatic infection for residents; default is 1
#' @param mult_asymp_nonres multiplier on asymptomatic infection for staff and visitors; default is 1
#' @param seed_asymp whether to seed with an asymptomatic case
#' @param isolate Whether symptomatic individuals isolate when symptoms emerge; defaults to T
#' @param time length of time to run model; defaults to 30
#' @param test whether there is weekly testing; defaults to F
#' @param test_sens test sensitivity; defaults to 0.7
#' @param test_frac fraction of school tested; defaults to 0.9
#' @param test_days test frequency; "day", "week", "2x_week"; defaults to "week"
#' @param test_type group tested; defaults to "all", also allows "staff" and "students"
#' @param test_start_day day tests are implemented for weekly testing; defaults to 1 = Monday morning
#' @param start_mult value to indicate relative frequency of adult/child infections; defaults to 1 (adults 2x as likely as kids)
#' @param start_type type of seed; default is "mix" (also "residents", "staff", "visitor", "cont")
#' @param nonres_prob if start_type = "cont", set daily probability of infectious entry for staff and visitors, defaults to .05
#' @param quarantine whether people quarantine upon exposure; defaults to T
#' @param quarantine.length length of quarantine when someone is infectious; defaults to 5
#' @param vax_eff Vaccine efficacy, defaults to 0.9
#' @param overdisp_off all overdispersion off; defaults to F
#' @param nh nursing home population; from initialize_NH()
#' @param sched nursing home schedule; from make_schedule()
#'
#' @export
mult_runs = function(N, cohorting = T, visitors = F, n_contacts = 4, rel_trans_common = 1/4, rel_trans_staff = 1/4, 
                     rel_trans_room_symp_res = 1, p_asymp_nonres = 0, p_asymp_res = 0, 
                     p_subclin_nonres = 0, p_subclin_res = 0, daily_attack = 0.18, staff_vax_req = F, 
                     res_vax = 0, staff_vax = 0, visit_vax = 0, staff_trans_red = 1, visit_trans_red = 1, res_trans_red = 1, 
                     staff_susp_red = 1, visit_susp_red = 1, res_susp_red = 1, disperse_transmission = T, n_contact_common = 6,
                     n_contact_staff = 10, n_start = 1, time_seed_inf = NA, days_inf = 5, mult_asymp_res = 1, mult_asymp_nonres = 1, seed_asymp = F, 
                     isolate = T, time = 30, test = T, test_sens = 0.7, test_frac = 0.9, test_days = 'week', 
                     test_type = 'all', test_start_day = 1, start_mult = 1, start_type = 'cont', nonres_prob = 0.001, 
                     quarantine = F, quarantine.length = 5, vax_eff = 0, overdisp_off = F, nh, sched, cohorts){
  
  keep = data.frame(all = numeric(N), tot = numeric(N), R0 = numeric(N), Rt = numeric(N), start = numeric(N), start_staff = numeric(N),
                    start_visit = numeric(N), start_res = numeric(N), start_symp = numeric(N), source_asymp = numeric(N), source_asymp_visit = numeric(N),
                    res_all = numeric(N), staff_all = numeric(N), visit_all = numeric(N), staff_tot = numeric(N), visit_tot = numeric(N),
                    res_tot = numeric(N), daily_attack = numeric(N), test = numeric(N), detected = numeric(N), detected_staff = numeric(N), 
                    detected_res = numeric(N), detected_staff_subclin = numeric(N), detected_res_subclin = numeric(N), 
                    symp = numeric(N), symp_res = numeric(N), asymp_res = numeric(N), symp_staff = numeric(N),
                    asymp_staff = numeric(N), room = numeric(N), common = numeric(N), staff_interactions = numeric(N), avg_infs = numeric(N),
                    num_room = numeric(N), quarantine_check = numeric(N), quarantined = numeric(N), quarantined_res = numeric(N),
                    from_staff = numeric(N), isolated = numeric(N),
                    avg_room = numeric(N), clin_res = numeric(N), clin_staff = numeric(N), clin_visit = numeric(N), notify_staff = numeric(N),
                    notify_res = numeric(N), notify_visit = numeric(N), clin_res2 = numeric(N), clin_staff2 = numeric(N), clin_visit2 = numeric(N), 
                    notify_staff2 = numeric(N), notify_res2 = numeric(N), notify_visit2 = numeric(N))
  
  #tic()
  # run over time
  for(i in 1:N){
    
    ## run model
    df = run_model(time = time, test = test, test_days = test_days, test_sens = test_sens, 
                   test_frac = test_frac, test_start_day = test_start_day, n_contact_common = n_contact_common, 
                   n_contact_staff = n_contact_staff,
                   n_start = n_start, days_inf = days_inf, mult_asymp_res = mult_asymp_res, mult_asymp_nonres = mult_asymp_nonres, 
                   seed_asymp = seed_asymp, time_seed_inf = time_seed_inf, start_type = start_type, start_mult = start_mult, 
                   nonres_prob = nonres_prob, quarantine = quarantine, quarantine.length = quarantine.length,
                   rel_trans_common = rel_trans_common, rel_trans_staff = rel_trans_staff, test_type = test_type, 
                   overdisp_off = overdisp_off, df = nh, sched = sched, cohorts = cohorts)
    
    time_keep = df$start.time[1]
    #print(time_keep)
    #print(length(time_keep:(time_keep+time-1)))
    
    # store output
    keep$all[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1)
    keep$tot[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & !df$id%in%c(df$id[df$start==T]))
    
    # keep$all_15[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & df$t_end_inf_home >= 15)
    # keep$tot_15[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & !df$id%in%c(df$id[df$start==T]) & df$t_end_inf_home >= 15)
    # keep$detected_15[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==15)
    # 
    # keep$all_15_staff[i] = sum(df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & df$t_end_inf_home >= 15)
    # keep$tot_15_staff[i] = sum(df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & !df$id%in%c(df$id[df$start==T]) & df$t_end_inf_home >= 15)
    # keep$detected_15_staff[i] = sum(df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==15)
    # 
    # keep$all_15_res[i] = sum(df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & df$t_end_inf_home >= 15)
    # keep$tot_15_res[i] = sum(df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & !df$id%in%c(df$id[df$start==T]) & df$t_end_inf_home >= 15)
    # keep$detected_15_res[i] = sum(df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==15)
    # 
    # keep$all_22[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & df$t_end_inf_home >= 22)
    # keep$tot_22[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & !df$id%in%c(df$id[df$start==T]) & df$t_end_inf_home >= 22)
    # keep$detected_22[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==22)
    # 
    # keep$all_22_staff[i] = sum(df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & df$t_end_inf_home >= 22)
    # keep$tot_22_staff[i] = sum(df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & !df$id%in%c(df$id[df$start==T]) & df$t_end_inf_home >= 22)
    # keep$detected_22_staff[i] = sum(df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==22)
    # 
    # keep$all_22_res[i] = sum(df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & df$t_end_inf_home >= 22)
    # keep$tot_22_res[i] = sum(df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & !df$id%in%c(df$id[df$start==T]) & df$t_end_inf_home >= 22)
    # keep$detected_22_res[i] = sum(df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==22)
    # 
    # keep$all_29[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & df$t_end_inf_home >= 29)
    # keep$tot_29[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & !df$id%in%c(df$id[df$start==T]) & df$t_end_inf_home >= 29)
    # keep$detected_29[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==29)
    # 
    # keep$all_29_staff[i] = sum(df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & df$t_end_inf_home >= 29)
    # keep$tot_29_staff[i] = sum(df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & !df$id%in%c(df$id[df$start==T]) & df$t_end_inf_home >= 29)
    # keep$detected_29_staff[i] = sum(df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==29)
    # 
    # keep$all_29_res[i] = sum(df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & df$t_end_inf_home >= 29)
    # keep$tot_29_res[i] = sum(df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & !df$id%in%c(df$id[df$start==T]) & df$t_end_inf_home >= 29)
    # keep$detected_29_res[i] = sum(df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==29)
    # 
    # keep$all_36[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & df$t_end_inf_home >= 36)
    # keep$tot_36[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & !df$id%in%c(df$id[df$start==T]) & df$t_end_inf_home >= 36)
    # keep$detected_36[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==36)
    # 
    # keep$all_36_staff[i] = sum(df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & df$t_end_inf_home >= 36)
    # keep$tot_36_staff[i] = sum(df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & !df$id%in%c(df$id[df$start==T]) & df$t_end_inf_home >= 36)
    # keep$detected_36_staff[i] = sum(df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==36)
    # 
    # keep$all_36_res[i] = sum(df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & df$t_end_inf_home >= 36)
    # keep$tot_36_res[i] = sum(df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & !df$id%in%c(df$id[df$start==T]) & df$t_end_inf_home >= 36)
    # keep$detected_36_res[i] = sum(df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==36)
    # 
    # keep$all_43[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & df$t_end_inf_home >= 43)
    # keep$tot_43[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & !df$id%in%c(df$id[df$start==T]) & df$t_end_inf_home >= 43)
    # keep$detected_43[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==43)
    # 
    # keep$all_43_staff[i] = sum(df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & df$t_end_inf_home >= 43)
    # keep$tot_43_staff[i] = sum(df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & !df$id%in%c(df$id[df$start==T]) & df$t_end_inf_home >= 43)
    # keep$detected_43_staff[i] = sum(df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==43)
    # 
    # keep$all_43_res[i] = sum(df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & df$t_end_inf_home >= 43)
    # keep$tot_43_res[i] = sum(df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & !df$id%in%c(df$id[df$start==T]) & df$t_end_inf_home >= 43)
    # keep$detected_43_res[i] = sum(df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==43)
    
    # keep$from_staff[i] = 0 #sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & !df$HH_id%in%c(df$HH_id[df$start]) & !df$adult[df$source])
    keep$R0[i] = sum(df$tot_inf[df$start==T])
    keep$Rt[i] =  mean(df$tot_inf[df$t_inf!=0 & df$t_end_inf_home>=time_keep], na.rm = T)
    keep$avg_infs[i] = mean(df$tot_inf[df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$start==T])
    keep$start[i] = sum(df$start==T & df$t_end_inf_home>=time_keep & df$t_inf < time_keep+time - 1)
    # keep$room_test_ind[i] = df$room_test_ind[1]
    # keep$room_test_ind_q[i] = df$room_test_ind_q[1]
    # keep$test_qs[i] = sum(df$test_ct_q)
    keep$test[i] = sum(df$test_ct)
    keep$detected[i] = sum(df$detected)
    keep$detected_staff[i] = sum(df$detected[df$type==1])
    keep$detected_res[i] = sum(df$detected[df$type==0])
    keep$detected_staff_subclin[i] = sum(df$detected[df$type==1 & df$sub_clin], na.rm = T)
    keep$detected_res_subclin[i] = sum(df$detected[df$type==0 & df$sub_clin], na.rm = T)
    keep$quarantine_check[i] = max(df$t_end_inf-df$t_end_inf_home, na.rm = T)#(df$uh.oh[1])
    # keep$avg_class[i] = unlist(df %>% filter(t_exposed!=-99 & t_exposed <= time_keep + time - 1 & class!=99) %>% group_by(class) %>%
    # summarize(num = length(class)) %>% ungroup() %>% summarize(mean(num, na.rm = T)))
    keep$isolated[i] = sum(df$isolated)
    keep$quarantined[i] = sum(df$quarantined)
    keep$quarantined_res[i] = sum(df$quarantined[df$type==0])
    keep$start_staff[i] = sum(df$type==1 & df$start==T & df$t_end_inf_home>=time_keep & df$t_inf < time_keep+time - 1)
    keep$start_visit[i] = sum(df$type==2 & df$start==T & df$t_end_inf_home>=time_keep & df$t_inf < time_keep+time - 1)
    keep$start_res[i] = sum(df$type==0 & df$start==T & df$t_end_inf_home>=time_keep & df$t_inf < time_keep+time - 1)
    keep$start_symp[i] = sum(df$symp[df$start==T], na.rm = T)
    keep$source_asymp[i] = sum(!df$source_symp & df$t_inf <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$id%in%c(df$id[df$start==T]), na.rm = T)
    keep$source_asymp_visit[i] = sum(df$type==2 & !df$source_symp & df$t_inf <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$id%in%c(df$id[df$start==T]), na.rm = T)
    keep$staff_all[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$type==1 & df$t_inf <= time_keep + time - 1)
    keep$res_all[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$type==0 & df$t_inf <= time_keep + time - 1)
    keep$visit_all[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$type==2 & df$t_inf <= time_keep + time - 1)
    keep$staff_tot[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$type==1 & df$t_inf <= time_keep + time - 1 & !df$id%in%c(df$id[df$start==T]))
    keep$res_tot[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$type==0 & df$t_inf <= time_keep + time - 1 & !df$id%in%c(df$id[df$start==T]))
    keep$visit_tot[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$type==2 & df$t_inf <= time_keep + time - 1 & !df$id%in%c(df$id[df$start==T]))
    keep$symp[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$symp==1 & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$symp_res[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$symp==1 & df$t_inf <= time_keep + time - 1 & df$type==0, na.rm = T)
    keep$asymp_res[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$symp==0 & df$t_inf <= time_keep + time - 1 & df$type==0, na.rm = T)
    keep$symp_staff[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$symp==1 & df$t_inf <= time_keep + time - 1 & df$type==1, na.rm = T)
    keep$asymp_staff[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$symp==0 & df$t_inf <= time_keep + time - 1 & df$type==1, na.rm = T)
    keep$sick_at_end[i] = sum(df$t_inf<=time_keep + time - 1 & df$t_end_inf > time_keep + time - 1)
    keep$room[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Room")
    keep$common[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Common area")
    keep$staff_interactions[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Staff interactions")
    keep$num_room[i] = length(unique(df$room[df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1]))
    keep$clin_staff[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$clin_res[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$clin_visit[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$type==2 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$clin_staff2[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$type==1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$clin_res2[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$clin_visit2[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$type==2 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$notify_staff[i] = sum(df$t_notify>=15 & df$type==1 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$notify_res[i] = sum(df$t_notify>=15 & df$type==0 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$notify_visit[i] = sum(df$t_notify>=15 & df$type==2 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$notify_staff2[i] = sum(df$t_notify>=15 & df$type==1 & df$t_notify <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$notify_res2[i] = sum(df$t_notify>=15 & df$type==0 & df$t_notify <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$notify_visit2[i] = sum(df$t_notify>=15 & df$type==2 & df$t_notify <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    # keep$switch[i] = df$switch[1]
    # keep$temp_switch[i] = df$temp_switch[1]
    
    
    #John's new checks
    keep$inf_ct_sympR_R_room[i] = sum(df$location == "Room" & df$type==0 & df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_asympR_R_room[i] = sum(df$location == "Room" & df$type==0 & !df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_sympR_S_room[i] = sum(df$location == "Room" & df$type==1 & df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_asympR_S_room[i] = sum(df$location == "Room" & df$type==1 & !df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_S_S_room[i] = sum(df$location == "Room" & df$type==1 & df$source %in% df$id[df$type==1], na.rm = TRUE)
    keep$inf_ct_sympS_R_room[i] = sum(df$location == "Room" & df$type==0 & df$source_symp & df$source %in% df$id[df$type==1], na.rm = TRUE)
    keep$inf_ct_asympS_R_room[i] = sum(df$location == "Room" & df$type==0 & !df$source_symp & df$source %in% df$id[df$type==1], na.rm = TRUE)
    
    keep$inf_ct_sympR_V_room[i] = sum(df$location == "Room" & df$type==2 & df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_asympR_V_room[i] = sum(df$location == "Room" & df$type==2 & !df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_sympS_V_room[i] = sum(df$location == "Room" & df$type==2 & df$source_symp & df$source %in% df$id[df$type==1], na.rm = TRUE)
    keep$inf_ct_asympS_V_room[i] = sum(df$location == "Room" & df$type==2 & !df$source_symp & df$source %in% df$id[df$type==1], na.rm = TRUE)
    keep$inf_ct_V_V_room[i] = sum(df$location == "Room" & df$type==2 & df$source %in% df$id[df$type==2], na.rm = TRUE)
    keep$inf_ct_sympV_R_room[i] = sum(df$location == "Room" & df$type==0 & df$source_symp & df$source %in% df$id[df$type==2], na.rm = TRUE)
    keep$inf_ct_asympV_R_room[i] = sum(df$location == "Room" & df$type==0 & !df$source_symp & df$source %in% df$id[df$type==2], na.rm = TRUE)
    keep$inf_ct_sympV_S_room[i] = sum(df$location == "Room" & df$type==1 & df$source_symp & df$source %in% df$id[df$type==2], na.rm = TRUE)
    keep$inf_ct_asympV_S_room[i] = sum(df$location == "Room" & df$type==1 & !df$source_symp & df$source %in% df$id[df$type==2], na.rm = TRUE)
    
    keep$inf_ct_sympR_R_common[i] = sum(df$location == "Common area" & df$type==0 & df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_asympR_R_common[i] = sum(df$location == "Common area" & df$type==0 & !df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_sympR_S_common[i] = sum(df$location == "Common area" & df$type==1 & df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_asympR_S_common[i] = sum(df$location == "Common area" & df$type==1 & !df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_sympS_S_common[i] = sum(df$location == "Common area" & df$type==1 & df$source_symp & df$source %in% df$id[df$type==1], na.rm = TRUE)
    keep$inf_ct_asympS_S_common[i] = sum(df$location == "Common area" & df$type==1 & !df$source_symp & df$source %in% df$id[df$type==1], na.rm = TRUE)
    keep$inf_ct_sympS_R_common[i] = sum(df$location == "Common area" & df$type==0 & df$source_symp & df$source %in% df$id[df$type==1], na.rm = TRUE)
    keep$inf_ct_asympS_R_common[i] = sum(df$location == "Common area" & df$type==0 & !df$source_symp & df$source %in% df$id[df$type==1], na.rm = TRUE)
    
    keep$inf_ct_R_V_common[i] = sum(df$location == "Common area" & df$type==2 & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_S_V_common[i] = sum(df$location == "Common area" & df$type==2 & df$source %in% df$id[df$type==1], na.rm = TRUE)
    keep$inf_ct_V_V_common[i] = sum(df$location == "Common area" & df$type==2 & df$source %in% df$id[df$type==2], na.rm = TRUE)
    keep$inf_ct_V_R_common[i] = sum(df$location == "Common area" & df$type==0 & df$source %in% df$id[df$type==2], na.rm = TRUE)
    keep$inf_ct_V_S_common[i] = sum(df$location == "Common area" & df$type==1 & df$source %in% df$id[df$type==2], na.rm = TRUE)
    
    keep$inf_ct_R_R_staff[i] = sum(df$location == "Staff interactions" & df$type==0 & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_R_S_staff[i] = sum(df$location == "Staff interactions" & df$type==1 & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_sympS_S_staff[i] = sum(df$location == "Staff interactions" & df$type==1 & df$source_symp & df$source %in% df$id[df$type==1], na.rm = TRUE)
    keep$inf_ct_asympS_S_staff[i] = sum(df$location == "Staff interactions" & df$type==1 & !df$source_symp & df$source %in% df$id[df$type==1], na.rm = TRUE)
    keep$inf_ct_S_R_staff[i] = sum(df$location == "Staff interactions" & df$type==0 & df$source %in% df$id[df$type==1], na.rm = TRUE)
    
    keep$inf_ct_R_V_staff[i] = sum(df$location == "Staff interactions" & df$type==2 & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_S_V_staff[i] = sum(df$location == "Staff interactions" & df$type==2 & df$source %in% df$id[df$type==1], na.rm = TRUE)
    keep$inf_ct_V_V_staff[i] = sum(df$location == "Staff interactions" & df$type==2 & df$source %in% df$id[df$type==2], na.rm = TRUE)
    keep$inf_ct_V_R_staff[i] = sum(df$location == "Staff interactions" & df$type==0 & df$source %in% df$id[df$type==2], na.rm = TRUE)
    keep$inf_ct_V_S_staff[i] = sum(df$location == "Staff interactions" & df$type==1 & df$source %in% df$id[df$type==2], na.rm = TRUE)
    
    keep$risk_ct_sympR_R_room[i] = sum(df$person.days.at.risk.room.res[df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_asympR_R_room[i] = sum(df$person.days.at.risk.room.res[!df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_sympR_S_room[i] = sum(df$person.days.at.risk.room.staff[df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_asympR_S_room[i] = sum(df$person.days.at.risk.room.staff[!df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_sympS_R_room[i] = sum(df$person.days.at.risk.room.res[df$symp & df$type==1], na.rm = TRUE)
    keep$risk_ct_asympS_R_room[i] = sum(df$person.days.at.risk.room.res[!df$symp & df$type==1], na.rm = TRUE)
    
    keep$risk_ct_sympR_V_room[i] = sum(df$person.days.at.risk.room.visit[df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_asympR_V_room[i] = sum(df$person.days.at.risk.room.visit[!df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_sympS_V_room[i] = sum(df$person.days.at.risk.room.visit[df$symp & df$type==1], na.rm = TRUE)
    keep$risk_ct_asympS_V_room[i] = sum(df$person.days.at.risk.room.visit[!df$symp & df$type==1], na.rm = TRUE)
    keep$risk_ct_sympV_R_room[i] = sum(df$person.days.at.risk.room.res[df$symp & df$type==2], na.rm = TRUE)
    keep$risk_ct_asympV_R_room[i] = sum(df$person.days.at.risk.room.res[!df$symp & df$type==2], na.rm = TRUE)
    keep$risk_ct_sympV_S_room[i] = sum(df$person.days.at.risk.room.staff[df$symp & df$type==2], na.rm = TRUE)
    keep$risk_ct_asympV_S_room[i] = sum(df$person.days.at.risk.room.staff[!df$symp & df$type==2], na.rm = TRUE)
    
    keep$risk_ct_sympR_R_common[i] = sum(df$person.days.at.risk.common.res[df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_asympR_R_common[i] = sum(df$person.days.at.risk.common.res[!df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_sympR_S_common[i] = sum(df$person.days.at.risk.common.staff[df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_asympR_S_common[i] = sum(df$person.days.at.risk.common.staff[!df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_sympS_S_common[i] = sum(df$person.days.at.risk.common.staff[df$symp & df$type==1], na.rm = TRUE)
    keep$risk_ct_asympS_S_common[i] = sum(df$person.days.at.risk.common.staff[!df$symp & df$type==1], na.rm = TRUE)
    keep$risk_ct_sympS_R_common[i] = sum(df$person.days.at.risk.common.res[df$symp & df$type==1], na.rm = TRUE)
    keep$risk_ct_asympS_R_common[i] = sum(df$person.days.at.risk.common.res[!df$symp & df$type==1], na.rm = TRUE)
    
    keep$risk_ct_sympS_S_staff[i] = sum(df$person.days.at.risk.staff.staff[df$symp & df$type==1], na.rm = TRUE)
    keep$risk_ct_asympS_S_staff[i] = sum(df$person.days.at.risk.staff.staff[!df$symp & df$type==1], na.rm = TRUE)
    
    keep$length.infectious.low_risk_obs[i] = mean(floor(df$t_end_inf_home[df$t_inf > 0 & df$comorbid==0]) - ceiling(df$t_inf[df$t_inf > 0 & df$comorbid==0]) + 1)
    keep$length.infectious.low_risk_sd[i] = sd(floor(df$t_end_inf_home[df$t_inf > 0 & df$comorbid==0]) - ceiling(df$t_inf[df$t_inf > 0 & df$comorbid==0]) + 1)
    keep$length.infectious.mod_risk_obs[i] = mean(floor(df$t_end_inf_home[df$t_inf > 0 & df$comorbid==1]) - ceiling(df$t_inf[df$t_inf > 0 & df$comorbid==1]) + 1)
    keep$length.infectious.mod_risk_sd[i] = sd(floor(df$t_end_inf_home[df$t_inf > 0 & df$comorbid==1]) - ceiling(df$t_inf[df$t_inf > 0 & df$comorbid==1]) + 1)
    keep$length.infectious.high_risk_obs[i] = mean(floor(df$t_end_inf_home[df$t_inf > 0 & df$comorbid==2]) - ceiling(df$t_inf[df$t_inf > 0 & df$comorbid==2]) + 1)
    keep$length.infectious.high_risk_sd[i] = sd(floor(df$t_end_inf_home[df$t_inf > 0 & df$comorbid==2]) - ceiling(df$t_inf[df$t_inf > 0 & df$comorbid==2]) + 1)
    
    keep$length.infectious.symp.low_risk_obs[i] = mean(floor(df$t_end_inf[df$symp & !df$sub_clin & df$t_inf > 0 & df$comorbid==0]) - ceiling(df$t_inf[df$symp & !df$sub_clin & df$t_inf > 0 & df$comorbid==0]) + 1)
    keep$length.infectious.asymp.low_risk_obs[i] = mean(floor(df$t_end_inf[(!df$symp | df$sub_clin) & df$t_inf > 0 & df$comorbid==0]) - ceiling(df$t_inf[(!df$symp | df$sub_clin) & df$t_inf > 0 & df$comorbid==0]) + 1)
    keep$length.infectious.symp.mod_risk_obs[i] = mean(floor(df$t_end_inf[df$symp & !df$sub_clin & df$t_inf > 0 & df$comorbid==1]) - ceiling(df$t_inf[df$symp & !df$sub_clin & df$t_inf > 0 & df$comorbid==1]) + 1)
    keep$length.infectious.asymp.mod_risk_obs[i] = mean(floor(df$t_end_inf[(!df$symp | df$sub_clin) & df$t_inf > 0 & df$comorbid==1]) - ceiling(df$t_inf[(!df$symp | df$sub_clin) & df$t_inf > 0 & df$comorbid==1]) + 1)
    keep$length.infectious.symp.high_risk_obs[i] = mean(floor(df$t_end_inf[df$symp & !df$sub_clin & df$t_inf > 0 & df$comorbid==2]) - ceiling(df$t_inf[df$symp & !df$sub_clin & df$t_inf > 0 & df$comorbid==2]) + 1)
    keep$length.infectious.asymp.high_risk_obs[i] = mean(floor(df$t_end_inf[(!df$symp | df$sub_clin) & df$t_inf > 0 & df$comorbid==2]) - ceiling(df$t_inf[(!df$symp | df$sub_clin) & df$t_inf > 0 & df$comorbid==2]) + 1)
    
    keep$n_res_obs[i] = sum(df$type==0)
    keep$n_dc_staff_obs[i] = sum(df$type==1 & df$role!=4 & !is.na(df$role))
    keep$n_admin_staff_obs[i] = sum(df$type==1 & df$role==4 & !is.na(df$role))
    keep$n_visit_obs[i] = sum(df$type==2)
    
    keep$p_asymp_res_obs[i] = sum(!df$symp & df$t_exposed > 0 & df$type==0)/sum(df$t_exposed > 0 & df$type==0)
    keep$p_asymp_nonres_obs[i] = sum(!df$symp & df$t_exposed > 0 & df$type!=0)/sum(df$t_exposed > 0 & df$type!=0)
    keep$p_subclin_res_obs[i] = sum(df$sub_clin & df$t_exposed > 0 & df$type==0)/sum(df$t_exposed > 0 & df$type==0)
    keep$p_subclin_nonres_obs[i] = sum(df$sub_clin & df$t_exposed > 0 & df$type!=0)/sum(df$t_exposed > 0 & df$type!=0)
    
    keep$length.incubation_obs[i] = mean(floor(df$t_inf[df$t_inf > 0 & !df$start]) - ceiling(df$t_exposed[df$t_inf > 0 & !df$start] + runif(length(df$t_exposed[df$t_inf > 0 & !df$start]), min = -0.5, max = 0.5)) + 1)
    keep$length.symp.gap_obs[i] = mean(floor(df$t_symp[df$t_inf > 0 & !df$start]) - ceiling(df$t_exposed[df$t_inf > 0 & !df$start] + runif(length(df$t_exposed[df$t_inf > 0 & !df$start]), min = -0.5, max = 0.5)) + 1)
    
    keep$res.vax.rate_obs[i] = mean(df$vacc[df$type==0])
    keep$staff.vax.rate_obs[i] = mean(df$vacc[df$type==1])
    keep$visit.vax.rate_obs[i] = mean(df$vacc[df$type==2])
    
    if("family"%in%colnames(df)){
      keep$vax.eff_obs[i] = (ifelse(is.na(mean(df$susp[df$type==0 & df$vacc] == 0)),
                                    sum(df$type==0 & df$vacc),
                                    mean(df$susp[df$type==0 & df$vacc] == 0))*sum(df$type==0 & df$vacc) +
                               ifelse(is.na(mean(df$susp[df$type==1 & df$vacc] == 0)),
                                      sum((df$type==1 & df$vacc)),
                                      mean(df$susp[df$type==1 & df$vacc] == 0))*sum((df$type==1 & df$vacc)) +
                               ifelse(is.na(mean(df$susp[df$type==2 & df$vacc] == 0)),
                                      sum((df$type==2 & df$vacc)),
                                      mean(df$susp[df$type==2 & df$vacc] == 0))*sum((df$type==2 & df$vacc)))/
        (sum(df$type==0 & df$vacc) + sum(df$type==1 & df$vacc) + sum(df$type==2 & df$vacc))
    } else{
      keep$vax.eff_obs[i] = (ifelse(is.na(mean(df$susp[df$type==0 & df$vacc] == 0)),
                                    sum(df$type==0 & df$vacc),
                                    mean(df$susp[df$type==0 & df$vacc] == 0))*sum(df$type==0 & df$vacc) +
                               ifelse(is.na(mean(df$susp[df$type==1 & df$vacc] == 0)),
                                      sum((df$type==1 & df$vacc)),
                                      mean(df$susp[df$type==1 & df$vacc] == 0))*sum((df$type==1 & df$vacc)))/
        (sum(df$type==0 & df$vacc) + sum(df$type==1 & df$vacc))
    }
    
    
    keep$res.prob_obs[i] = ifelse(start_type == "cont", mean(sapply(df$start.time[1]:(time + df$start.time[1] - 1), function(t){sum(ceiling(df$t_inf[df$start & df$type==0]) == t)/sum(df$type==0)})), NA)
    keep$nonres.prob_obs[i] = ifelse(start_type == "cont", mean(sapply(df$start.time[1]:(time + df$start.time[1] - 1), function(t){sum(ceiling(df$t_inf[df$start & df$type!=0]) == t)/sum(df$type!=0)})), NA)
    
    
    # Alyssa's new checks
    keep$seed_res[i] = sum(df$start.init[df$type==0])
    keep$seed_nonres[i] = sum(df$start.init[df$type!=0])
    keep$start_res_time[i] = mean(df$t_inf[df$start.init & df$type==0])
    keep$start_nonres_time[i] = mean(df$t_inf[df$start.init & df$type!=0])
    keep$not_inf_start[i] = sum(df$not_inf_keep)
    keep$test_type.check[i] = sum(df$test_type)
    keep$vaxxed[i] = sum(df$vacc)
    #print(i)
    
  }
  
  #toc()
  
  return(keep) #keep) #list(keep, mod, class, sched))
}

