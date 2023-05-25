#************************************** Nursing Home ABM ****************************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

#' @import tidyverse
#' @import igraph
#' @import tictoc
#' @import data.table
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
#' @param visitors allow visitors; defaults to T
#' @param temp_staff add temporary staff; defaults to F
#'
#' @return out data frame of structured nursing home
#'
#' @export
make_NH = function(synthpop, cohorting = T, visitors = T, temp_staff = F){
  
  # select residents who live with roommates
  doubles = subset(synthpop, private_room == F) %>%
    
    # sort residents into rooms
    mutate(room.placeholder = rep(1:nrow(subset(synthpop, private_room == F))/2, each = 2, 
                                  length.out = nrow(subset(synthpop, private_room == F))),
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
  if(cohorting){
    
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
  }
  
  # bind residents and staff into dataframe
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
  
  # ------------------------------------------------------------------------------------------------
  
  # add temporary staff 
  if(temp_staff){
    
    temp_staff = data.frame(id = (nrow(out)+1):(nrow(out)+nrow(rn)+nrow(lpn)+nrow(cna)+nrow(med_aide)+nrow(admin)), type = 3, role = subset(synthpop$role, between(synthpop$id, 91, 173)), comorbid = 0)
    
    # assign shifts to direct care staff
    temp_rn_cohort_morning = subset(temp_staff, type == 3 & role == 0)[1:5,] %>% mutate(temp_rn_cohort_morning = 1:5)
    temp_rn_cohort_evening = subset(temp_staff, type == 3 & role == 0)[6:10,] %>% mutate(temp_rn_cohort_evening = 6:10) 
    temp_rn_cohort_night = subset(temp_staff, type == 3 & role == 0)[11:13,] %>% mutate(temp_rn_cohort_night = 11:13)
    temp_rn = temp_rn_cohort_morning %>% bind_rows(temp_rn_cohort_evening) %>% bind_rows(temp_rn_cohort_night)
    
    temp_lpn_cohort_morning = subset(temp_staff, type == 3 & role == 1)[1:3,] %>% mutate(temp_lpn_cohort_morning = 1:3) 
    temp_lpn_cohort_evening = subset(temp_staff, type == 3 & role == 1)[4:6,] %>% mutate(temp_lpn_cohort_evening = 4:6)
    temp_lpn_cohort_night = subset(temp_staff, type == 3 & role == 1)[7:8,] %>% mutate(temp_lpn_cohort_night = 7:8)
    temp_lpn = temp_lpn_cohort_morning %>% bind_rows(temp_lpn_cohort_evening) %>% bind_rows(temp_lpn_cohort_night)
    
    temp_cna_cohort_morning = subset(temp_staff, type == 3 & role == 2)[1:15,] %>% mutate(temp_cna_cohort_morning = 1:15)  
    temp_cna_cohort_evening = subset(temp_staff, type == 3 & role == 2)[16:30,] %>% mutate(temp_cna_cohort_evening = 16:30)
    temp_cna_cohort_night = subset(temp_staff, type == 3 & role == 2)[31:39,] %>% mutate(temp_cna_cohort_night = 31:39)
    temp_cna = temp_cna_cohort_morning %>% bind_rows(temp_cna_cohort_evening) %>% bind_rows(temp_cna_cohort_night)
    
    temp_ma_cohort_morning = subset(temp_staff, type == 3 & role == 3)[1:2,] %>% mutate(temp_ma_cohort_morning = 1:2) 
    temp_ma_cohort_evening = subset(temp_staff, type == 3 & role == 3)[3,] %>% mutate(temp_ma_cohort_evening = 3)
    temp_med_aide = temp_ma_cohort_morning %>% bind_rows(temp_ma_cohort_evening)
    
    temp_admin_cohort_morning = subset(temp_staff, type == 3 & role == 4)[1:10,] %>% mutate(temp_admin_cohort_morning = 1:10)
    temp_admin_cohort_evening = subset(temp_staff, type == 3 & role == 4)[11:20,] %>% mutate(temp_admin_cohort_evening = 11:20)
    temp_admin = temp_admin_cohort_morning %>% bind_rows(temp_admin_cohort_evening)
    
    
    # cohort temporary staff
    if(cohorting){
      
      res_counter = 1
      temp_rn_morning = nrow(temp_rn_cohort_morning)
      for(i in 1:temp_rn_morning){
        out[res_counter:(res_counter+(nrow(residents)/temp_rn_morning)-1),"temp_rn_cohort_morning"] = i
        res_counter = res_counter+nrow(residents)/temp_rn_morning
      }
      res_counter = 1
      temp_rn_evening = nrow(temp_rn_cohort_evening)
      for(i in ((temp_rn_morning+1):(temp_rn_morning+temp_rn_evening))){
        out[res_counter:(res_counter+(nrow(residents)/temp_rn_evening)-1),"temp_rn_cohort_evening"] = i
        res_counter = res_counter+nrow(residents)/temp_rn_evening
      }
      res_counter = 1
      temp_rn_night = nrow(temp_rn_cohort_night)
      for(i in ((temp_rn_morning+temp_rn_evening+1):(temp_rn_morning+temp_rn_evening+temp_rn_night))){
        out[res_counter:(res_counter+(nrow(residents)/temp_rn_night)-1),"temp_rn_cohort_night"] = i
        res_counter = res_counter+nrow(residents)/temp_rn_night
      }
      res_counter = 1
      temp_lpn_morning = nrow(temp_lpn_cohort_morning)
      for(i in 1:temp_lpn_morning){
        out[res_counter:(res_counter+(nrow(residents)/temp_lpn_morning)-1),"temp_lpn_cohort_morning"] = i
        res_counter = res_counter+nrow(residents)/temp_lpn_morning
      }
      res_counter = 1
      temp_lpn_evening = nrow(temp_lpn_cohort_evening)
      for(i in ((temp_lpn_morning+1):(temp_lpn_morning+temp_lpn_evening))){
        out[res_counter:(res_counter+(nrow(residents)/temp_lpn_evening)-1),"temp_lpn_cohort_evening"] = i
        res_counter = res_counter+nrow(residents)/temp_lpn_evening
      }
      res_counter = 1
      temp_lpn_night = nrow(temp_lpn_cohort_night)
      for(i in ((temp_lpn_morning+temp_lpn_evening+1):(temp_lpn_morning+temp_lpn_evening+temp_lpn_night))){
        out[res_counter:(res_counter+(nrow(residents)/temp_lpn_night)-1),"temp_lpn_cohort_night"] = i
        res_counter = res_counter+nrow(residents)/temp_lpn_night
      }
      res_counter = 1
      temp_cna_morning = nrow(temp_cna_cohort_morning)
      for(i in 1:temp_cna_morning){
        out[res_counter:(res_counter+(nrow(residents)/temp_cna_morning)-1),"temp_cna_cohort_morning"] = i
        res_counter = res_counter+nrow(residents)/temp_cna_morning
      }
      res_counter = 1
      temp_cna_evening = nrow(temp_cna_cohort_evening)
      for(i in ((temp_cna_morning+1):(temp_cna_morning+temp_cna_evening))){
        out[res_counter:(res_counter+(nrow(residents)/temp_cna_evening)-1),"temp_cna_cohort_evening"] = i
        res_counter = res_counter+nrow(residents)/temp_cna_evening
      }
      res_counter = 1
      temp_cna_night = nrow(temp_cna_cohort_night)
      for(i in ((temp_cna_morning+temp_cna_evening+1):(temp_cna_morning+temp_cna_evening+temp_cna_night))){
        out[res_counter:(res_counter+(nrow(residents)/temp_cna_night)-1),"temp_cna_cohort_night"] = i
        res_counter = res_counter+nrow(residents)/temp_cna_night
      }
      res_counter = 1
      temp_med_aide_morning = nrow(temp_ma_cohort_morning)
      for(i in 1:temp_med_aide_morning){
        out[res_counter:(res_counter+(nrow(residents)/temp_med_aide_morning)-1),"temp_ma_cohort_morning"] = i
        res_counter = res_counter+nrow(residents)/temp_med_aide_morning
      }
      res_counter = 1
      temp_med_aide_evening = nrow(temp_ma_cohort_evening)
      for(i in ((temp_med_aide_morning+1):(temp_med_aide_morning+temp_med_aide_evening))){
        out[res_counter:(res_counter+(nrow(residents)/temp_med_aide_evening)-1),"temp_ma_cohort_evening"] = i
        res_counter = res_counter+nrow(residents)/temp_med_aide_evening
      }
    }
    
    # bind residents and temporary staff into dataframe
    out = out %>% bind_rows(temp_rn) %>% bind_rows(temp_lpn) %>% bind_rows(temp_cna) %>% 
      bind_rows(temp_med_aide) %>% bind_rows(temp_admin)
  }
  
  return(out)
  
}



#' Initialize nursing home
#'
#' This function takes in a data frame exported by make_NH().
#' It adds epidemiological attributes of the full nursing home community.

#' @param p_asymp_nonres Fraction of non-residents with asymptomatic disease; defaults to 0.5 (used to be p_asymp_adult)
#' @param p_asymp_res Fraction of residents with asymptomatic disease; defaults to 0.4 (used to be p_asymp_child)
#' @param p_subclin_nonres Fraction of non-residents with subclinical but not techincally asymptomatic disease; defaults to 0
#' @param p_subclin_res Fraction of residents with subclinical but not techincally asymptomatic disease; defaults to 0
#' @param daily_attack Average daily attack rate; defaults to 0.18
#' @param res_boost Proportion of residents with booster dose; defaults to some amount
#' @param staff_vax_req Whether staff are required to get most updated recommended vaccine; defaults to F
#' @param staff_boost Proportion of staff with booster dose; defaults to some amount
#' @param visit_boost Proportion of visitors with booster dose; defaults to some amount
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
#' @param isolate Whether symptomatic individuals isolate when symptoms emerge; defaults to T
#' @param prim_previnf_eff efficacy of 2 doses or previous infection
#' @param boost_eff booster efficacy
#' @param start Data frame from make_NH()
#'
#' @return df data frame of resident and staff attributes.
#'
#' @export
initialize_NH = function(p_asymp_nonres = 1, p_asymp_res = 1, p_subclin_nonres = 0, p_subclin_res = 0,
                         daily_attack = .18, staff_vax_req = F, 
                         res_boost = 0.74, staff_boost = 0.51, visit_boost = 0.34, 
                         staff_trans_red = 0.7, visit_trans_red = 0.7, res_trans_red = 0.7, 
                         staff_susp_red = 0, visit_susp_red = 0, res_susp_red = 0, 
                         isolate = T, prim_previnf_eff = 0.4, boost_eff = 0.7, days_inf = 5,
                         days_isolate_res = 5, days_isolate_staff = 5, days_isolate_visit = 5, start){
  
  n = nrow(start)
  
  # vax values for staff
  if(staff_vax_req == T){
    staff_boost = 1
  }
  
  # initialize data frame
  df = start %>%
    mutate(cohorting = ifelse(is.na(rn_cohort_morning) & type==0, F, ifelse(type==0, T, F)),
           start = F,
           start.init = F,
           start.time = NA, 
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
           t_end_isolate = -1,
           inf_quarantine = F, # whether someone got infected while quarantining in room
           tot_inf = 0,
           detected = 0,
           quarantined = F,
           test = 0,
           test_ct = 0,
           test_ct_res = 0,
           test_type = F,
           test_tp_count = 0,
           test_fn_count = 0,
           test_eligible = 0,
           daily_attack_rate = daily_attack,
           source = 0,
           source_symp = NA,
           tot_inf = 0,
           location = "",
           
           # trackers for unit testing
           person.days.at.risk.room.res = 0,
           person.days.at.risk.room.res.quarantine = 0,
           person.days.at.risk.room.staff = 0,
           person.days.at.risk.room.staff.quarantine = 0,
           person.days.at.risk.room.visit = 0,
           person.days.at.risk.common.res = 0,
           person.days.at.risk.common.staff = 0,
           person.days.at.risk.staff.staff = 0,
           
           days_inf = days_inf,
           days_isolate_res = days_isolate_res,
           days_isolate_staff = days_isolate_staff,
           days_isolate_visit = days_isolate_visit,
           inf = F,
           isolate_home = F,
           isolate_room = F,
           trans_now = F,
           flag_fam = 0,
           quarantined = F,
           not_inf = T,
           not_inf_keep = NA,
           present_susp = T,
           isolated = F,
           now = F) %>%
    
    mutate(p_asymp = ifelse(type != 0, p_asymp_nonres, p_asymp_res),
           p_subclin = ifelse(type!=0, p_subclin_nonres, p_subclin_res),
           
           # isolation
           isolate = rbinom(n(), size = 1, prob = isolate),
           
           # susceptibility
           res_boost_val = res_boost, 
           staff_boost_val = staff_boost,
           visit_boost_val = visit_boost,
           prim_previnf_eff_val = prim_previnf_eff,
           boost_eff_val = boost_eff,
           
           vacc = 0,
           vacc = ifelse(type == 0, rbinom(n(), size = 1, prob = res_boost_val), vacc),
           vacc = ifelse(type == 1 | type==3, rbinom(n(), size = 1, prob = staff_boost_val), vacc),
           vacc = ifelse(type == 2, rbinom(n(), size = 1, prob = visit_boost_val), vacc),
           
           # transmission probability by 8-hr time period per day
           room_trans_prob = 1-(1-daily_attack)^(1/3),
           room_trans_prob = ifelse(type == 0, room_trans_prob*(1-res_trans_red), room_trans_prob),
           room_trans_prob = ifelse(type == 1 | type == 3, room_trans_prob*(1-staff_trans_red), room_trans_prob),
           room_trans_prob = ifelse(type == 2, room_trans_prob*(1-visit_trans_red), room_trans_prob),
           
           susp = ifelse(vacc==0, 1-prim_previnf_eff_val, 1-boost_eff_val),
           susp = ifelse(type==0, susp*(1-res_susp_red), susp),
           susp = ifelse(type==1 | type==3, susp*(1-staff_susp_red), susp),
           susp = ifelse(type==2, susp*(1-visit_susp_red), susp))
  
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
    d = nh %>% left_join(vec_exp, "id")
    
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
    d = nh %>% left_join(vec_exp, "id")
    
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
  
  # assign temporary staff to residents and randomize each day if no cohorting
  if('temp_rn_cohort_morning' %in% colnames(nh)){
    if(is.na(nh$temp_rn_cohort_morning[nh$id==1])){
      temp_rn_morning = nh[!is.na(nh$temp_rn_cohort_morning),]$temp_rn_cohort_morning
      temp_rn_evening = nh[!is.na(nh$temp_rn_cohort_evening),]$temp_rn_cohort_evening
      temp_rn_night = nh[!is.na(nh$temp_rn_cohort_night),]$temp_rn_cohort_night
      temp_lpn_morning = nh[!is.na(nh$temp_lpn_cohort_morning),]$temp_lpn_cohort_morning
      temp_lpn_evening = nh[!is.na(nh$temp_lpn_cohort_evening),]$temp_lpn_cohort_evening
      temp_lpn_night = nh[!is.na(nh$temp_lpn_cohort_night),]$temp_lpn_cohort_night
      temp_cna_morning = nh[!is.na(nh$temp_cna_cohort_morning),]$temp_cna_cohort_morning
      temp_cna_evening = nh[!is.na(nh$temp_cna_cohort_evening),]$temp_cna_cohort_evening
      temp_cna_night = nh[!is.na(nh$temp_cna_cohort_night),]$temp_cna_cohort_night
      temp_ma_morning = nh[!is.na(nh$temp_ma_cohort_morning),]$temp_ma_cohort_morning
      temp_ma_evening = nh[!is.na(nh$temp_ma_cohort_evening),]$temp_ma_cohort_evening
      
      for(i in 1:time){
        
        # assign morning staff
        res = 1
        for(j in temp_rn_morning){
          d$temp_rn_cohort_morning[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(temp_rn_morning))-1) 
                                   & d$type==0 & d$t==i] = sample(temp_rn_morning,1)
          res = res + (nrow(subset(nh,type==0))/length(temp_rn_morning))
        }
        res = 1
        for(j in temp_lpn_morning){
          d$temp_lpn_cohort_morning[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(temp_lpn_morning))-1) 
                                    & d$type==0 & d$t==i] = sample(temp_lpn_morning,1)
          res = res + (nrow(subset(nh,type==0))/length(temp_lpn_morning))
        }
        res = 1
        for(j in temp_cna_morning){
          d$temp_cna_cohort_morning[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(temp_cna_morning))-1) 
                                    & d$type==0 & d$t==i] = sample(temp_cna_morning,1)
          res = res + (nrow(subset(nh,type==0))/length(temp_cna_morning))
        }
        res = 1
        for(j in temp_ma_morning){
          d$temp_ma_cohort_morning[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(temp_ma_morning))-1) 
                                   & d$type==0 & d$t==i] = sample(temp_ma_morning,1)
          res = res + (nrow(subset(nh,type==0))/length(temp_ma_morning))
        }
        
        # assign evening staff
        res = 1
        for(j in temp_rn_evening){
          d$temp_rn_cohort_evening[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(temp_rn_evening))-1) 
                                   & d$type==0 & d$t==i] = sample(temp_rn_evening,1)
          res = res + (nrow(subset(nh,type==0))/length(temp_rn_evening))
        }
        res = 1
        for(j in temp_lpn_evening){
          d$temp_lpn_cohort_evening[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(temp_lpn_evening))-1) 
                                    & d$type==0 & d$t==i] = sample(temp_lpn_evening,1)
          res = res + (nrow(subset(nh,type==0))/length(temp_lpn_evening))
        }
        res = 1
        for(j in temp_cna_evening){
          d$temp_cna_cohort_evening[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(temp_cna_evening))-1) 
                                    & d$type==0 & d$t==i] = sample(temp_cna_evening,1)
          res = res + (nrow(subset(nh,type==0))/length(temp_cna_evening))
        }
        d$temp_ma_cohort_evening[d$type==0 & d$t==i] = 3
        
        # assign night staff
        res = 1
        for(j in temp_rn_night){
          d$temp_rn_cohort_night[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(temp_rn_night))-1) 
                                 & d$type==0 & d$t==i] = sample(temp_rn_night,1)
          res = res + (nrow(subset(nh,type==0))/length(temp_rn_night))
        }
        res = 1
        for(j in temp_lpn_night & i%%3==0){
          d$temp_lpn_cohort_night[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(temp_lpn_night))-1) 
                                  & d$type==0 & d$t==i] = sample(temp_lpn_night,1)
          res = res + (nrow(subset(nh,type==0))/length(temp_lpn_night))
        }
        res = 1
        for(j in temp_cna_night & i%%3==0){
          d$temp_cna_cohort_night[d$id %in% res:(res+(nrow(subset(nh,type==0))/length(temp_cna_night))-1) 
                                  & d$type==0 & d$t==i] = sample(temp_cna_night,1)
          res = res + (nrow(subset(nh,type==0))/length(temp_cna_night))
        }
      }
    }
  }
  
  return(d)
  
}



#' Create list of people in each room
#'
#' Store info about people in each room per shift per day
#'
#' @param df data frame from make_schedule()
#'
#' @return cohorts list of people in each staff/resident/visitor room
#'
#' @export
make_room = function(df){
  
  cohorts = vector(mode="list", length=length(unique(df$id)))
  
  # resident rooms
  for(i in 1:length(unique(df$id[df$type==0]))){
    
    staff_vec_morning = list(as.data.table(df[df$rn_cohort_morning==df$rn_cohort_morning[df$id==i] & df$type==1,][rowSums(is.na(df[df$rn_cohort_morning==df$rn_cohort_morning[df$id==i] & df$type==1,])) 
                                                                                                                  != ncol(df[df$rn_cohort_morning==df$rn_cohort_morning[df$id==i] & df$type==1,]),]),
                             as.data.table(df[df$lpn_cohort_morning==df$lpn_cohort_morning[df$id==i] & df$type==1,][rowSums(is.na(df[df$lpn_cohort_morning==df$lpn_cohort_morning[df$id==i] & df$type==1,])) 
                                                                                                                    != ncol(df[df$lpn_cohort_morning==df$lpn_cohort_morning[df$id==i] & df$type==1,]),]),
                             as.data.table(df[df$cna_cohort_morning==df$cna_cohort_morning[df$id==i] & df$type==1,][rowSums(is.na(df[df$cna_cohort_morning==df$cna_cohort_morning[df$id==i] & df$type==1,])) 
                                                                                                                    != ncol(df[df$cna_cohort_morning==df$cna_cohort_morning[df$id==i] & df$type==1,]),]),
                             as.data.table(df[df$ma_cohort_morning==df$ma_cohort_morning[df$id==i] & df$type==1,][rowSums(is.na(df[df$ma_cohort_morning==df$ma_cohort_morning[df$id==i] & df$type==1,])) 
                                                                                                                  != ncol(df[df$ma_cohort_morning==df$ma_cohort_morning[df$id==i] & df$type==1,]),]))
    
    staff_vec_evening = list(as.data.table(df[df$rn_cohort_evening==df$rn_cohort_evening[df$id==i] & df$type==1,][rowSums(is.na(df[df$rn_cohort_evening==df$rn_cohort_evening[df$id==i] & df$type==1,])) 
                                                                                                                  != ncol(df[df$rn_cohort_evening==df$rn_cohort_evening[df$id==i] & df$type==1,]),]),
                             as.data.table(df[df$lpn_cohort_evening==df$lpn_cohort_evening[df$id==i] & df$type==1,][rowSums(is.na(df[df$lpn_cohort_evening==df$lpn_cohort_evening[df$id==i] & df$type==1,])) 
                                                                                                                    != ncol(df[df$lpn_cohort_evening==df$lpn_cohort_evening[df$id==i] & df$type==1,]),]),
                             as.data.table(df[df$cna_cohort_evening==df$cna_cohort_evening[df$id==i] & df$type==1,][rowSums(is.na(df[df$cna_cohort_evening==df$cna_cohort_evening[df$id==i] & df$type==1,])) 
                                                                                                                    != ncol(df[df$cna_cohort_evening==df$cna_cohort_evening[df$id==i] & df$type==1,]),]),
                             as.data.table(df[df$ma_cohort_evening==df$ma_cohort_evening[df$id==i] & df$type==1,][rowSums(is.na(df[df$ma_cohort_evening==df$ma_cohort_evening[df$id==i] & df$type==1,])) 
                                                                                                                  != ncol(df[df$ma_cohort_evening==df$ma_cohort_evening[df$id==i] & df$type==1,]),]))
    
    staff_vec_night = list(as.data.table(df[df$rn_cohort_night==df$rn_cohort_night[df$id==i] & df$type==1,][rowSums(is.na(df[df$rn_cohort_night==df$rn_cohort_night[df$id==i] & df$type==1,])) 
                                                                                                            != ncol(df[df$rn_cohort_night==df$rn_cohort_night[df$id==i] & df$type==1,]),]),
                           as.data.table(df[df$lpn_cohort_night==df$lpn_cohort_night[df$id==i] & df$type==1,][rowSums(is.na(df[df$lpn_cohort_night==df$lpn_cohort_night[df$id==i] & df$type==1,])) 
                                                                                                              != ncol(df[df$lpn_cohort_night==df$lpn_cohort_night[df$id==i] & df$type==1,]),]),
                           as.data.table(df[df$cna_cohort_night==df$cna_cohort_night[df$id==i] & df$type==1,][rowSums(is.na(df[df$cna_cohort_night==df$cna_cohort_night[df$id==i] & df$type==1,])) 
                                                                                                              != ncol(df[df$cna_cohort_night==df$cna_cohort_night[df$id==i] & df$type==1,]),]))
    
    staff_vec_morning = rbindlist(staff_vec_morning)
    staff_vec_evening = rbindlist(staff_vec_evening)
    staff_vec_night = rbindlist(staff_vec_night)
    
    
    if(df$room[df$id==i][1]<34 & sum(df$room[df$type==0]==df$room[df$id==i])>max(df$t)){
      roommate_vec = df[df$room==df$room[df$id==i] & df$id!=i,][rowSums(is.na(df[df$room==df$room[df$id==i] & df$id!=i,])) != ncol(df[df$room==df$room[df$id==i] & df$id!=i,]),]
    }else{roommate_vec = c()}
    
    
    if('family' %in% colnames(df)){
      visitor_id = df$id[df$family==df$family[df$id==i] & df$id!=i][!is.na(df$id[df$family==df$family[df$id==i] & df$id!=i])]
      if(length(visitor_id)>0){
        visit_vec = df[df$id==visitor_id,]
      }
    }else{visit_vec = c()}
    
    
    if('temp_rn_cohort_morning' %in% colnames(df)){
      temp_staff_vec_morning = list(as.data.table(df[df$temp_rn_cohort_morning==df$temp_rn_cohort_morning[df$id==i] & df$type==3,][rowSums(is.na(df[df$temp_rn_cohort_morning==df$temp_rn_cohort_morning[df$id==i] & df$type==3,])) 
                                                                                                                                   != ncol(df[df$temp_rn_cohort_morning==df$temp_rn_cohort_morning[df$id==i] & df$type==3,]),]),
                                    as.data.table(df[df$temp_lpn_cohort_morning==df$temp_lpn_cohort_morning[df$id==i] & df$type==3,][rowSums(is.na(df[df$temp_lpn_cohort_morning==df$temp_lpn_cohort_morning[df$id==i] & df$type==3,])) 
                                                                                                                                     != ncol(df[df$temp_lpn_cohort_morning==df$temp_lpn_cohort_morning[df$id==i] & df$type==3,]),]),
                                    as.data.table(df[df$temp_cna_cohort_morning==df$temp_cna_cohort_morning[df$id==i] & df$type==3,][rowSums(is.na(df[df$temp_cna_cohort_morning==df$temp_cna_cohort_morning[df$id==i] & df$type==3,])) 
                                                                                                                                     != ncol(df[df$temp_cna_cohort_morning==df$temp_cna_cohort_morning[df$id==i] & df$type==3,]),]),
                                    as.data.table(df[df$temp_ma_cohort_morning==df$temp_ma_cohort_morning[df$id==i] & df$type==3,][rowSums(is.na(df[df$temp_ma_cohort_morning==df$temp_ma_cohort_morning[df$id==i] & df$type==3,])) 
                                                                                                                                   != ncol(df[df$temp_ma_cohort_morning==df$temp_ma_cohort_morning[df$id==i] & df$type==3,]),]))
      
      temp_staff_vec_evening = list(as.data.table(df[df$temp_rn_cohort_evening==df$temp_rn_cohort_evening[df$id==i] & df$type==3,][rowSums(is.na(df[df$temp_rn_cohort_evening==df$temp_rn_cohort_evening[df$id==i] & df$type==3,])) 
                                                                                                                                   != ncol(df[df$temp_rn_cohort_evening==df$temp_rn_cohort_evening[df$id==i] & df$type==3,]),]),
                                    as.data.table(df[df$temp_lpn_cohort_evening==df$temp_lpn_cohort_evening[df$id==i] & df$type==3,][rowSums(is.na(df[df$temp_lpn_cohort_evening==df$temp_lpn_cohort_evening[df$id==i] & df$type==3,])) 
                                                                                                                                     != ncol(df[df$temp_lpn_cohort_evening==df$temp_lpn_cohort_evening[df$id==i] & df$type==3,]),]),
                                    as.data.table(df[df$temp_cna_cohort_evening==df$temp_cna_cohort_evening[df$id==i] & df$type==3,][rowSums(is.na(df[df$temp_cna_cohort_evening==df$temp_cna_cohort_evening[df$id==i] & df$type==3,])) 
                                                                                                                                     != ncol(df[df$temp_cna_cohort_evening==df$temp_cna_cohort_evening[df$id==i] & df$type==3,]),]),
                                    as.data.table(df[df$temp_ma_cohort_evening==df$temp_ma_cohort_evening[df$id==i] & df$type==3,][rowSums(is.na(df[df$temp_ma_cohort_evening==df$temp_ma_cohort_evening[df$id==i] & df$type==3,])) 
                                                                                                                                   != ncol(df[df$temp_ma_cohort_evening==df$temp_ma_cohort_evening[df$id==i] & df$type==3,]),]))
      
      temp_staff_vec_night = list(as.data.table(df[df$temp_rn_cohort_night==df$temp_rn_cohort_night[df$id==i] & df$type==3,][rowSums(is.na(df[df$temp_rn_cohort_night==df$temp_rn_cohort_night[df$id==i] & df$type==3,])) 
                                                                                                                             != ncol(df[df$temp_rn_cohort_night==df$temp_rn_cohort_night[df$id==i] & df$type==3,]),]),
                                  as.data.table(df[df$temp_lpn_cohort_night==df$temp_lpn_cohort_night[df$id==i] & df$type==3,][rowSums(is.na(df[df$temp_lpn_cohort_night==df$temp_lpn_cohort_night[df$id==i] & df$type==3,])) 
                                                                                                                               != ncol(df[df$temp_lpn_cohort_night==df$temp_lpn_cohort_night[df$id==i] & df$type==3,]),]),
                                  as.data.table(df[df$temp_cna_cohort_night==df$temp_cna_cohort_night[df$id==i] & df$type==3,][rowSums(is.na(df[df$temp_cna_cohort_night==df$temp_cna_cohort_night[df$id==i] & df$type==3,])) 
                                                                                                                               != ncol(df[df$temp_cna_cohort_night==df$temp_cna_cohort_night[df$id==i] & df$type==3,]),]))
      
      temp_staff_vec_morning = rbindlist(temp_staff_vec_morning)
      temp_staff_vec_evening = rbindlist(temp_staff_vec_evening)
      temp_staff_vec_night = rbindlist(temp_staff_vec_night)
    }
    else{
      temp_staff_vec_morning = c()
      temp_staff_vec_evening = c()
      temp_staff_vec_night = c()
    }
    
    cohorts[[i]] = list(as.data.frame(staff_vec_morning), as.data.frame(staff_vec_evening), as.data.frame(staff_vec_night), as.data.frame(roommate_vec), as.data.frame(visit_vec), 
                        as.data.frame(temp_staff_vec_morning), as.data.frame(temp_staff_vec_evening), as.data.frame(temp_staff_vec_night))
  }
  
  # permanent staff rooms
  for(i in (length(unique(df$id[df$type==0]))+1):(length(unique(df$id[df$type==0]))+length(unique(df$id[df$type==1 & df$role!=4])))){
    staff_row = df[df$id==i,][, c("rn_cohort_morning", "rn_cohort_evening", "rn_cohort_night", 
                                  "lpn_cohort_morning", "lpn_cohort_evening", "lpn_cohort_night",
                                  "cna_cohort_morning", "cna_cohort_evening", "cna_cohort_night",
                                  "ma_cohort_morning", "ma_cohort_evening", "admin_cohort_morning",
                                  "admin_cohort_evening")]
    for(role in 1:length(staff_row)){
      if(!is.na(staff_row[role][1,])){
        staff_role = colnames(staff_row)[role]
      }
    }
    staff_role_id = staff_row[!is.na(staff_row)][[1]]
    
    # make vector of residents that staff treats
    res_vec = df[df[staff_role]==staff_role_id & df$type==0,][rowSums(is.na(df[df[staff_role]==staff_role_id & df$type==0,])) != ncol(df[df[staff_role]==staff_role_id & df$type==0,]),]
    
    cohorts[[i]] = as.data.frame(res_vec)
  }
  
  # visitor rooms
  if('family' %in% colnames(df)){
    for(i in (length(unique(df$id[df$type<2]))+1):(length(unique(df$id[df$type<2]))+(length(unique(df$id[df$type==2]))))){
      res_vec = df[df$family==df$family[df$id==i] & df$id!=i,][rowSums(is.na(df[df$family==df$family[df$id==i] & df$id!=i,])) != ncol(df[df$family==df$family[df$id==i] & df$id!=i,]),]
      res_id = res_vec$id
      if(df$room[df$id==res_id][1]<34){
        res_vec = rbindlist(list(as.data.table(res_vec), as.data.table(df[df$room==res_vec$room & df$id!=res_id,][rowSums(is.na(df[df$room==res_vec$room & df$id!=res_id,])) != ncol(df[df$room==res_vec$room & df$id!=res_id,]),])))
      }
      cohorts[[i]] = as.data.frame(res_vec)
    }
  }
  
  # temporary staff rooms
  if('temp_rn_cohort_morning' %in% colnames(df)){
    for(i in (length(unique(df$id[df$type<3]))+1):(length(unique(df$id[df$type<3]))+(length(unique(df$id[df$type==3 & df$role!=4]))))){
      temp_staff_row = df[df$id==i,][, c("temp_rn_cohort_morning", "temp_rn_cohort_evening", "temp_rn_cohort_night", 
                                         "temp_lpn_cohort_morning", "temp_lpn_cohort_evening", "temp_lpn_cohort_night",
                                         "temp_cna_cohort_morning", "temp_cna_cohort_evening", "temp_cna_cohort_night",
                                         "temp_ma_cohort_morning", "temp_ma_cohort_evening", "temp_admin_cohort_morning",
                                         "temp_admin_cohort_evening")]
      for(role in 1:length(temp_staff_row)){
        if(!is.na(temp_staff_row[role][1,])){
          temp_staff_role = colnames(temp_staff_row)[role]
        }
      }
      temp_staff_role_id = temp_staff_row[!is.na(temp_staff_row)][[1]]
      
      # make vector of residents that staff treats
      res_vec = df[df[temp_staff_role]==temp_staff_role_id & df$type==0,][rowSums(is.na(df[df[temp_staff_role]==temp_staff_role_id & df$type==0,])) != ncol(df[df[temp_staff_role]==temp_staff_role_id & df$type==0,]),]
      
      cohorts[[i]] = as.data.frame(res_vec)
    }
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
#' @param cohorts list of dataframes of people in the same room as each person
#' @param shift current shift
#'
#' @return infs id of infected individuals
#'
#' @export
run_room = function(a, df, t, quarantine, cohorts, shift){
  
  ## if infected is resident
  if(df$type[df$id==a]==0){
    
    # if resident has roommate
    if(df$room[df$id==a]<34){
      # roommate
      roommate_vec = cohorts[[a]][[4]][cohorts[[a]][[4]]$t==t & cohorts[[a]][[4]]$id%in%df$id,]
      roommate_vec$susp = df[df$id%in%roommate_vec$id,]$susp
      roommate_vec$present_susp = df[df$id%in%roommate_vec$id,]$present_susp
      
    }else{roommate_vec = c()}
    
    # make vector of staff in infected resident's room at current time
    staff_vec = cohorts[[a]][[shift]][cohorts[[a]][[shift]]$t==t & cohorts[[a]][[shift]]$id%in%df$id,]
    if(nrow(staff_vec)>0){
      if(quarantine & df$isolated[df$id==a]){
        staff_vec$susp = df[df$id%in%staff_vec$id,]$susp*0.5
      }else{
        staff_vec$susp = df[df$id%in%staff_vec$id,]$susp
      }
      staff_vec$present_susp = df[df$id%in%staff_vec$id,]$present_susp
    }else{
      if(!"temp_rn_cohort_morning"%in%colnames(df)){
        staff_vec = c()
      }else if(nrow(cohorts[[a]][[shift+5]][cohorts[[a]][[shift+5]]$t==t & cohorts[[a]][[shift+5]]$id%in%df$id,])>0){
        # bring in temporary staff
        staff_vec = cohorts[[a]][[shift+5]][cohorts[[a]][[shift+5]]$t==t & cohorts[[a]][[shift+5]]$id%in%df$id,]
        if(quarantine & df$isolated[df$id==a]){
          staff_vec$susp = df[df$id%in%staff_vec$id,]$susp*0.5
        }else{
          staff_vec$susp = df[df$id%in%staff_vec$id,]$susp
        }
        staff_vec$present_susp = df[df$id%in%staff_vec$id,]$present_susp
      }else{
        staff_vec = c()
      }
    }
    
    # make vector of resident's visitor present at NH
    if('family' %in% colnames(df) & df$flag_fam[df$id==a]!=1 & any(df$shift=="morning" & df$type==2)){
      visit_vec = cohorts[[a]][[5]][cohorts[[a]][[5]]$t==t & cohorts[[a]][[5]]$id%in%df$id,]
      if(nrow(visit_vec)>0){
        visit_vec$susp = df[df$id%in%visit_vec$id,]$susp
        visit_vec$present_susp = df[df$id%in%visit_vec$id,]$present_susp
      }else{visit_vec = c()}
    }else{visit_vec = c()}
    
    room_vec = rbindlist(list(as.data.table(roommate_vec), as.data.table(staff_vec), as.data.table(visit_vec)), fill=T)
    
    # determine who in room gets infected
    prob_room = rbinom(nrow(room_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*room_vec$susp*room_vec$present_susp < 1,
                                                               df$room_trans_prob[df$id==a]*room_vec$susp*room_vec$present_susp,
                                                               1))
    room = room_vec$id
    
    # list infected 
    infs = room*prob_room
    
    return(list(infs, staff_vec, visit_vec, roommate_vec))
    
    
    ## if infected is direct-care staff
  } else if((df$type[df$id==a]==1 | df$type[df$id==a]==3) & df$role[df$id==a]!=4 & df$shift[df$id==a]!="absent"){
    
    # make vector of residents that staff treats
    res_vec = cohorts[[a]][cohorts[[a]]$t==t & cohorts[[a]]$id%in%df$id,]
    res_vec$quarantined = df[df$id%in%res_vec$id,]$quarantined
    res_vec$susp = df[df$id%in%res_vec$id,]$susp
    res_vec$present_susp = df[df$id%in%res_vec$id,]$present_susp
    if(quarantine){
      res_vec$susp = ifelse(res_vec$quarantined, res_vec$susp*0.5, res_vec$susp)
    }
    
    # determine who in room gets infected
    prob_room = rbinom(nrow(res_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp < 1,
                                                              df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp,
                                                              1))
    room = res_vec$id
    
    # list infected 
    infs = room*prob_room
    
    return(list(infs, res_vec))
    
    
    ## if infected is visitor  
  } else if(df$type[df$id==a]==2 & df$shift[df$id==a]!="absent"){
    
    # make vector of residents that visitor sees, including roommates
    res_vec = cohorts[[a]][cohorts[[a]]$t==t & cohorts[[a]]$id%in%df$id,]
    res_vec$susp = df[df$id%in%res_vec$id,]$susp
    res_vec$present_susp = df[df$id%in%res_vec$id,]$present_susp
    
    # determine who in room gets infected
    prob_room = rbinom(nrow(res_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp < 1,
                                                              df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp,
                                                              1))
    room = res_vec$id
    
    # list infected 
    infs = room*prob_room
    
    return(list(infs, res_vec))
    
  }else{
    infs = 0
  }
  
  return(infs)
}


#' Set common area transmission (used to be run_rand)
#'
#' Determine who is infected at a timestep
#' from common area contact with an infected individual
#'
#' @param a id of infected individual
#' @param df data frame in run_model()
#' @param n_contact_common_res number of contacts a staff/residents has with other residents in the common area; defaults to 3
#' @param n_contact_common_staff number of contacts a staff/residents has with other staff in the common area; defaults to 6
#' @param rel_trans_staff relative transmission in common area interactions (vs. resident room);
#' defaults to 1/4
#'
#' @return infs id of infected individuals
#'
#' @export
run_common = function(a, df, n_contact_common_res, n_contact_common_staff, rel_trans_common = 1/4){
  
  if(n_contact_common_res==0 & n_contact_common_staff==0){
    return(0)
  }
  
  if(n_contact_common_res>0){
    
    # pull random contacts from residents and staff present in nursing home common area
    tot_res = length(df$id[!df$isolated & !df$quarantined & df$id!=a & df$type==0])
    contact_take_res = ifelse(n_contact_common_res<=tot_res, n_contact_common_res, tot_res)
    contact_id_res = sample(df$id[!df$isolated & !df$quarantined & df$id!=a & df$type==0], contact_take_res, replace=F)
    contacts_res = df[df$id %in% contact_id_res,]
    id.susp_res = contacts_res[contacts_res$present_susp & contacts_res$susp!=0,]$id
    
    # determine whether a contact becomes infected
    prob_common_res = rbinom(nrow(contacts_res), size = 1,
                             prob = ifelse(df$room_trans_prob[df$id==a]*contacts_res$susp*contacts_res$present_susp*rel_trans_common < 1,
                                           df$room_trans_prob[df$id==a]*contacts_res$susp*contacts_res$present_susp*rel_trans_common,
                                           1))
    # infected individuals
    infs_res = contacts_res$id*prob_common_res
    
  }else{
    infs_res = 0
    id.susp_res = 0
  }
  
  if(n_contact_common_staff>0){
    
    # pull random contacts from residents and staff present in nursing home common area
    tot_staff = length(df$id[!df$isolated & !df$quarantined & df$id!=a & (df$type==1 | df$type==3)])
    contact_take_staff = ifelse(n_contact_common_staff<=tot_staff, n_contact_common_staff, tot_staff)
    contact_id_staff = sample(df$id[!df$isolated & !df$quarantined & df$id!=a & (df$type==1 | df$type==3)], contact_take_staff, replace=F)
    contacts_staff = df[df$id %in% contact_id_staff,]
    id.susp_staff = contacts_staff[contacts_staff$present_susp & contacts_staff$susp!=0,]$id
    
    # determine whether a contact becomes infected
    prob_common_staff = rbinom(nrow(contacts_staff), size = 1,
                               prob = ifelse(df$room_trans_prob[df$id==a]*contacts_staff$susp*contacts_staff$present_susp*rel_trans_common < 1,
                                             df$room_trans_prob[df$id==a]*contacts_staff$susp*contacts_staff$present_susp*rel_trans_common,
                                             1))
    # infected individuals
    infs_staff = contacts_staff$id*prob_common_staff
    
  }else{
    infs_staff = 0
    id.susp_staff = 0
  }
  
  return(list(c(infs_res, infs_staff), c(id.susp_res, id.susp_staff)))
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
    tot = length(df$id[!df$isolated & !df$quarantined & (df$type == 1 | df$type==3) & df$id!=a])
    contact_take = ifelse(n_contact_staff<=tot, n_contact_staff, tot)
    contact_id = sample(df$id[!df$isolated & !df$quarantined & (df$type == 1 | df$type==3) & df$id!=a], contact_take, replace=F)
    contacts = df[df$id %in% contact_id,]
    id.susp = contacts[contacts$present_susp & contacts$susp != 0,]$id
    
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



#' Set infection parameters for seeded infections
#'
#' Set infection parameters for individuals infected before looping through timesteps
#'
#' @param df.u data frame in run_model() of infected individuals
#' @param days_inf length of infectious period, defaults to 3 
#' @param days_isolate_res length of isolation for residents, defaults to 10
#' @param days_isolate_staff length of isolation for staff, defaults to 7
#' @param days_isolate_visit length of isolation for visitors, defaults to 5
#' @param set indication of seeding model vs. creating infections
#' @param seed_asymp when making a seed, force to be asymptomatic; default is false
#'
#' @return df.u with updated parameters
#'
#' @export
# note to self -- add additional parameters to change around here
make_infected_start = function(df.u, time = 30, days_inf = 3, days_isolate_res = 10, days_isolate_staff = 7,
                               days_isolate_visit = 5, set = NA, seed_asymp = F){
  
  if(is.na(set)[1]){
    # set infectivity  parameters
    df.u$symp = rep(rbinom(length(unique(df.u$id)), size = 1, prob = 1-df.u$p_asymp), each=time+15)
    df.u$sub_clin = ifelse(df.u$symp, rep(rbinom(length(unique(df.u$id)), size = 1, prob =  df.u$p_subclin/(1-df.u$p_asymp)), each=time+15), 1)
    df.u$t_symp = df.u$t_exposed + rep(rgamma(length(unique(df.u$id)), shape = 8.38, rate=2.20), each=time+15)
    df.u$t_inf = df.u$t_exposed + rep(rgamma(length(unique(df.u$id)), shape = 4.45, rate=1.42), each=time+15)
    df.u$t_inf = ifelse(df.u$t_inf >= df.u$t_symp, df.u$t_symp-1, df.u$t_inf)
    
  }else{
    
    # set infectivity  parameters
    if(seed_asymp) {
      df.u$symp = 0
      df.u$sub_clin = 0
    }else{
      df.u$symp = rep(rbinom(length(unique(df.u$id)), size = 1, prob = 1-df.u$p_asymp), each=time+15)
      df.u$sub_clin = ifelse(df.u$symp, rep(rbinom(length(unique(df.u$id)), size = 1, prob =  df.u$p_subclin/(1-df.u$p_asymp)), each=time+15), 1)
    }
    df.u$t_inf = set + rep(runif(length(unique(df.u$id)), min = -0.5, max = 0.5), each=time+15)
    df.u$t_exposed = df.u$t_inf - rep(rgamma(length(unique(df.u$id)), shape = 4.45, rate = 1.42), each=time+15)
    df.u$t_symp = df.u$t_exposed + rep(rgamma(length(unique(df.u$id)), shape = 8.38, rate = 2.20), each=time+15)
    df.u$t_symp = ifelse(df.u$t_symp <= df.u$t_inf, df.u$t_inf + 1, df.u$t_symp)
  }
  
  df.u$t_end_inf_home = df.u$t_inf +
    rep(rlnorm(length(unique(df.u$id)), meanlog = log(days_inf)-log((days_inf^2 + 2)/days_inf^2)/2, sdlog = sqrt(log((days_inf^2 + 2)/days_inf^2))), each=time+15)
  
  df.u$t_end_inf = ifelse(df.u$symp==1 & !df.u$sub_clin & df.u$isolate & df.u$t_symp<df.u$t_end_inf_home, df.u$t_symp, df.u$t_end_inf_home)
  
  df.u$t_notify = ifelse(df.u$symp==1 & !df.u$sub_clin & df.u$isolate, df.u$t_symp, -17)
  
  df.u$t_end_isolate = ifelse(df.u$isolate & df.u$type==0 & df.u$t_notify!=-17 & df.u$t_notify>0, df.u$t_notify + days_isolate_res, df.u$t_end_isolate)
  df.u$t_end_isolate = ifelse(df.u$isolate & (df.u$type==1 | df.u$type==3) & df.u$t_notify!=-17 & df.u$t_notify>0, df.u$t_notify + days_isolate_staff, df.u$t_end_isolate)
  df.u$t_end_isolate = ifelse(df.u$isolate & df.u$type==2 & df.u$t_notify!=-17 & df.u$t_notify>0, df.u$t_notify + days_isolate_visit, df.u$t_end_isolate)
  
  return(df.u)
}



#' Set infection parameters for seeded infections
#'
#' Set infection parameters for individuals infected at a particular timestep
#'
#' @param df.u data frame in run_model() of infected individuals
#' @param days_inf length of infectious period, defaults to 3
#' @param days_isolate_res length of isolation for residents, defaults to 10
#' @param days_isolate_staff length of isolation for staff, defaults to 7
#' @param days_isolate_visit length of isolation for visitors, defaults to 5
#' @param set indication of seeding model vs. creating infections
#' @param seed_asymp when making a seed, force to be asymptomatic; default is false
#'
#' @return df.u with updated parameters
#'
#' @export
make_infected = function(df.u, days_inf = 3, days_isolate_res = 10, days_isolate_staff = 7,
                         days_isolate_visit = 5, set = NA, seed_asymp = F){
  
  if(is.na(set)[1]){
    
    # set infectivity  parameters
    df.u$symp = rbinom(nrow(df.u), size = 1, prob = 1-df.u$p_asymp)
    df.u$sub_clin = ifelse(df.u$symp, rbinom(nrow(df.u), size = 1, prob =  df.u$p_subclin/(1-df.u$p_asymp)), 1)
    df.u$t_symp = df.u$t_exposed + rgamma(nrow(df.u), shape=8.38, rate=2.20)
    df.u$t_inf = df.u$t_exposed + rgamma(nrow(df.u), shape=4.45, rate=1.42)
    df.u$t_inf = ifelse(df.u$t_inf >= df.u$t_symp, df.u$t_symp-1, df.u$t_inf)
    
  } else{
    # set infectivity  parameters
    if(seed_asymp) {
      df.u$symp = 0
      df.u$sub_clin = 0
    }else{
      df.u$symp = rbinom(nrow(df.u), size = 1, prob = 1-df.u$p_asymp)
      df.u$sub_clin = ifelse(df.u$symp, rbinom(nrow(df.u), size = 1, prob =  df.u$p_subclin/(1-df.u$p_asymp)), 1)
    }
    df.u$t_inf = set + runif(nrow(df.u), min = -0.5, max = 0.5)
    df.u$t_exposed = df.u$t_inf - rgamma(nrow(df.u), shape=4.45, rate=1.42)
    df.u$t_symp = df.u$t_exposed + rgamma(nrow(df.u), shape=8.38, rate=2.20)
    df.u$t_symp = ifelse(df.u$t_symp <= df.u$t_inf, df.u$t_inf + 1, df.u$t_symp)
  }
  
  df.u$t_end_inf_home = df.u$t_inf +
    rlnorm(nrow(df.u), meanlog = log(days_inf)-log((days_inf^2 + 2)/days_inf^2)/2, sdlog = sqrt(log((days_inf^2 + 2)/days_inf^2)))
  
  df.u$t_end_inf = ifelse(df.u$symp==1 & !df.u$sub_clin & df.u$isolate & df.u$t_symp<df.u$t_end_inf_home, df.u$t_symp, df.u$t_end_inf_home)
  
  df.u$t_notify = ifelse(df.u$symp==1 & !df.u$sub_clin & df.u$isolate, df.u$t_symp, -17)
  
  # make unsusceptible
  df.u$susp = 0
  
  df.u$t_end_isolate = ifelse(df.u$isolate & df.u$type==0 & df.u$t_notify!=-17 & df.u$t_notify>0, df.u$t_notify + days_isolate_res, df.u$t_end_isolate)
  df.u$t_end_isolate = ifelse(df.u$isolate & (df.u$type==1 | df.u$type==3) & df.u$t_notify!=-17 & df.u$t_notify>0, df.u$t_notify + days_isolate_staff, df.u$t_end_isolate)
  df.u$t_end_isolate = ifelse(df.u$isolate & df.u$type==2 & df.u$t_notify!=-17 & df.u$t_notify>0, df.u$t_notify + days_isolate_visit, df.u$t_end_isolate)
  
  return(df.u)
  
}



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
#' @param visit_test whether visitors test before entering NH
#' @param n_contact_common_res number of contacts a staff/resident has with other residents in the common area; defaults to 3
#' @param n_contact_common_staff number of contacts a staff/resident has with other staff in the common area; defaults to 3
#' @param n_contact_staff number of contacts a staff member has with other staff members during a shift; defaults to 10
#' @param n_start number of infections to seed model; defaults to 1
#' @param days_inf length of infectious period, defaults to 3 days
#' @param days_isolate_res length of isolation for residents, defaults to 10
#' @param days_isolate_staff length of isolation for staff, defaults to 7
#' @param days_isolate_visit length of isolation for visitors, defaults to 5
#' @param seed_asymp whether to seed with an asymptomatic case
#' @param start_mult value to indicate relative frequency of resident/staff infections; defaults to 1 
#' (staff 2x as likely as residents since residents don't leave) (are staff or residents more likely to get infected?)
#' @param staff_prob set daily probability of infectious entry for staff, defaults to .001 (used to be child_prob)
#' @param visit_prob set daily probability of infectious entry for visitors, defaults to .001
#' @param rel_trans_common relative transmission in common area interactions vs. resident's room; defaults to 1/4
#' @param rel_trans_staff relative transmission in staff-staff interactions vs. resident's room; defaults to 1/4 (used to be rel_trans_adult)
#' @param quarantine whether or not people quarantine upon exposure; defaults to F
#' @param quarantine.length length of quarantine when someone is exposed; defaults to 7
#' @param test_recovered whether or not to test recovered individuals
#' @param test_isolated whether or not to test isolated individuals (for testing purposes)
#' @param df data frame from make_schedule()
#' @param cohorts list from make_room()
#'
#' @return df updated df with transmission results
#' @return time_seed_inf when the first individual was dropped in
#'
#' @export
#### NOTE: I found this to be slower when coded w/tidyverse.
#### Therefore for the most part, this is coded in base.
run_model = function(time = 30,
                     test = F,
                     test_days = "week",
                     test_sens =  1,
                     test_frac = 1,
                     test_start_day = 7,
                     visit_test = F,
                     n_contact_common_res = 3,
                     n_contact_common_staff = 3,
                     n_contact_staff = 6,
                     n_start = 1,
                     days_inf = 5,
                     days_isolate_res = 10,
                     days_isolate_staff = 7,
                     days_isolate_visit = 5,
                     seed_asymp = F,
                     start_mult = 1,
                     staff_prob = 0.005,
                     visit_prob = 0.005,
                     quarantine = F,
                     quarantine.length = 7,
                     rel_trans_common = 1/4,
                     rel_trans_staff = 1/4,
                     test_type = "all",
                     test_recovered = T,
                     df, cohorts){
  
  #### SEED MODEL ####
  
  # pull out IDs of staff/visitors
  staff_IDs = unique(df$id[df$type==1])
  visit_IDs = unique(df$id[df$type==2])
  temp_staff_IDs = unique(df$id[df$type==3])
  
  # pick times
  vec = 1:(time+15)
  staff_pulls = rbinom(time+15, size = length(staff_IDs), prob = staff_prob)
  staff_times = rep(vec, staff_pulls)
  
  visit_pulls = rbinom(time+15, size = length(visit_IDs), prob = visit_prob)
  visit_times = rep(vec, visit_pulls)
  
  temp_staff_pulls = rbinom(time+15, size = length(temp_staff_IDs), prob = staff_prob)
  temp_staff_times = rep(vec, temp_staff_pulls)
  
  # pick people
  staff = sample(staff_IDs, length(staff_times))
  visit = sample(visit_IDs, length(visit_times))
  temp_staff = sample(temp_staff_IDs, length(temp_staff_times))
  
  # set up vector
  if("family" %in% colnames(df) & "temp_rn_cohort_morning" %in% colnames(df)){
    time_seed_inf = c(staff_times, visit_times, temp_staff_times)
    id.samp = c(staff, visit, temp_staff)
  }else if("family"%in%colnames(df)){
    time_seed_inf = c(staff_times, visit_times)
    id.samp = c(staff, visit)
  }else{
    time_seed_inf = staff_times
    id.samp = staff
  }
  
  df.temp = data.frame(id.samp, time_seed_inf) %>% arrange(id.samp) %>%
    left_join(df, c("id.samp" = "id")) %>% filter(susp!=0)
  time_seed_inf = 15 # start on Monday morning with testing
  
  df$start.time = time_seed_inf
  
  # setup
  if(nrow(df.temp)>0){
    df[df$id%in%df.temp$id.samp,] = make_infected_start(df.u = df[df$id%in%df.temp$id.samp,], time = time, days_inf = days_inf,
                                                        days_isolate_res = days_isolate_res, days_isolate_staff = days_isolate_staff,
                                                        days_isolate_visit = days_isolate_visit,
                                                        set = df.temp$time_seed_inf, seed_asymp = seed_asymp)
    df$start = df$id %in% df.temp$id.samp
    df$start.init = df$id %in% df.temp$id.samp
    
  }
  
  # test days
  # if null, make this Monday
  if(test_days == "week") {testing_days = seq(test_start_day, (time+15), by = 7)}
  if(test_days == "day") {testing_days = 1:(time+15)}
  if(test_days == "2x_week"){testing_days = c(seq(4, (time+15), by = 7), seq(test_start_day, (time+15), by = 7))}
  
  #print(testing_days)
  
  # testing
  if(test_type=="all"){df$test_type = df$type!=2 & test_frac>=0.7
  } else if(test_type=="residents"){df$test_type = df$type==0
  } else if(test_type=="staff"){df$test_type = (df$type==1 | df$type==3)}
  
  # run over time steps
  # BIG CHANGE 
  for(t in time_seed_inf:(time_seed_inf+time-1)){
    
    df_time = df[df$t==t,]
    
    # mark who is infectious (either at home or in NH)
    df_time$inf = df_time$t_inf > -1 & df_time$t_inf <= t & df_time$t_end_inf_home >= t
    
    # mark who is isolated where
    df_time$isolate_home = df_time$isolate & df_time$inf & df_time$t_notify <= t & df_time$t_notify!=-17 & df_time$t_end_isolate>=t & df_time$type!=0
    df_time$isolate_room = df_time$isolate & df_time$inf & df_time$t_notify <= t & df_time$t_notify!=-17 & df_time$t_end_isolate>=t & df_time$type==0
    
    # update end of infectious period in NH
    df_time$t_end_inf = ifelse(df_time$t_end_inf!=-1 & df_time$t_quarantine!=-13 & df_time$t_end_quarantine!=-13 & df_time$t_notify!=-17 & df_time$t_end_inf>=df_time$t_notify, df_time$t_notify, df_time$t_end_inf)
    
    # quarantine
    if(quarantine){
      df_time$quarantined = ifelse(df_time$t_quarantine<=t & df_time$t_quarantine!=-13 & df_time$t_end_quarantine>=t & df_time$t_end_quarantine!=-13 & df_time$vacc==0, T, F)
      df_time$t_end_quarantine = ifelse(df_time$t_quarantine<=t & df_time$t_quarantine!=-13 & df_time$t_end_quarantine>t & df_time$t_notify!=-17 & df_time$t_notify>=t & df_time$t_notify-df_time$t_quarantine<df_time$t_end_quarantine-df_time$t_quarantine & (df_time$t_end_inf>t | df_time$t_end_inf==-1) & df_time$vacc==0, 
                                        df_time$t_notify, df_time$t_end_quarantine)
    }
    
    # update infectious and at nursing home
    df_time$trans_now = df_time$shift!="absent" & df_time$inf
    
    # flag known infected/quarantined residents' visitors
    if("family"%in% colnames(df_time)){
      df_time$flag_fam[df_time$trans_now & df_time$type==0] = ifelse(df_time$t_notify[df_time$trans_now & df_time$type==0]<=t, 1, 0)
      if(quarantine){
        df_time$flag_fam[df_time$type==0 & df_time$vacc==0] = ifelse(df_time$quarantined[df_time$type==0 & df_time$vacc==0], 1, 0)
      }
      if(sum(df_time$flag_fam)>0){
        flagged_families = unique(df_time$family[df_time$flag_fam==1])
      } else{flagged_families = 0}
      df_time$flag_fam[df_time$type==2] = ifelse(df_time$family[df_time$type==2]%in%flagged_families, 1, 0)
    }
    
    df_time$not_inf = df_time$t_exposed==-99 | df_time$t_exposed>t # if exposed from community, can be exposed earlier
    if(t==15) df_time$not_inf_keep = df_time$not_inf
    df_time$isolated = df_time$isolate_home | df_time$isolate_room
    
    # re-estimate who is present (among staff and visitors) 
    df_time$shift[df_time$type!=0] = ifelse(df_time$shift[df_time$type!=0]!="absent" & !df_time$isolate_home[df_time$type!=0] & !df_time$quarantined[df_time$type!=0], df_time$shift[df_time$type!=0], "absent")
    if("family"%in%colnames(df_time)){
      df_time$shift[df_time$type==2] = ifelse(df_time$shift[df_time$type==2]!="absent" & !df_time$isolate_home[df_time$type==2] & !df_time$quarantined[df_time$type==2], df_time$shift[df_time$type==2], "absent")
      df_time$shift[df_time$type==2] = ifelse(df_time$flag_fam[df_time$type==2]==1, "absent", df_time$shift[df_time$type==2])
    }
    
    # track residents with no direct-care staff at this time & make temporary staff present
    if("temp_rn_cohort_morning"%in%colnames(df_time)){
      for(res in 1:length(df_time$id[df_time$type==0])){
        if(length(cohorts[[res]][[1]]$id[cohorts[[res]][[1]]$t==t & cohorts[[res]][[1]]$id%in%df_time[df_time$shift=="morning" | df_time$shift=="all",]$id])==0){
          df_time$shift[df_time$id%in%cohorts[[res]][[6]]$id[cohorts[[res]][[6]]$t==t & cohorts[[res]][[6]]$id%in%df_time$id]] = "morning"
        }
        if(length(cohorts[[res]][[2]]$id[cohorts[[res]][[2]]$t==t & cohorts[[res]][[2]]$id%in%df_time[df_time$shift=="evening" | df_time$shift=="all",]$id])==0){
          df_time$shift[df_time$id%in%cohorts[[res]][[7]]$id[cohorts[[res]][[7]]$t==t & cohorts[[res]][[7]]$id%in%df_time$id]] = "evening"
        }
        if(length(cohorts[[res]][[3]]$id[cohorts[[res]][[3]]$t==t & cohorts[[res]][[3]]$id%in%df_time[df_time$shift=="night" | df_time$shift=="all",]$id])==0){
          df_time$shift[df_time$id%in%cohorts[[res]][[8]]$id[cohorts[[res]][[8]]$t==t & cohorts[[res]][[8]]$id%in%df_time$id]] = "night"
        }
      }
    }
    
    df_time$present_susp = df_time$shift!="absent" & df_time$not_inf
    
    # update infectious and at nursing home
    df_time$trans_now = df_time$shift!="absent" & df_time$inf
    
    # set infections to 0 for this timestep
    df_time$now = F
    
    ## group testing
    if(test){
      
      if(!test_recovered){
          
        df_time_test = df_time[(df_time$t_inf==-1 | df_time$t_end_inf_home>=t),]
        
        if(t%in%testing_days){
          
          # testing residents and staff
          df_time_novisit = df_time_test[df_time_test$type!=2,]
          
          df_time_novisit$test_ct = df_time_novisit$test_ct + rbinom(nrow(df_time_novisit), size = 1, prob = (df_time_novisit$shift!="absent")*test_frac*as.numeric(df_time_novisit$test_type)) # count how many tests individual takes
          df_time_novisit$test_ct_res = ifelse(df_time_novisit$type==0, df_time_novisit$test_ct, df_time_novisit$test_ct_res)
          df_time_novisit$test = rbinom(nrow(df_time_novisit), size = 1, prob = test_sens*test_frac*as.numeric(df_time_novisit$test_type)*(df_time_novisit$shift!="absent")) # record only accurate test results?
          df_time_novisit$t_end_inf = ifelse(df_time_novisit$test & df_time_novisit$trans_now, t, df_time_novisit$t_end_inf)
          df_time_novisit$t_notify = ifelse(df_time_novisit$test & df_time_novisit$trans_now & (df_time_novisit$t_symp>=t | df_time_novisit$symp==0), t, df_time_novisit$t_notify)
          df_time_novisit$detected = ifelse(df_time_novisit$test & df_time_novisit$trans_now, 1, df_time_novisit$detected)
          df_time_novisit$t_end_quarantine = ifelse(df_time_novisit$test & df_time_novisit$t_quarantine<=t & df_time_novisit$t_quarantine!=-13 & df_time_novisit$t_end_quarantine>t & df_time_novisit$t_notify!=-17 & df_time_novisit$t_notify>=t & df_time_novisit$t_notify-df_time_novisit$t_quarantine<df_time_novisit$t_end_quarantine-df_time_novisit$t_quarantine & (df_time_novisit$t_end_inf>t | df_time_novisit$t_end_inf==-1) & df_time_novisit$vacc==0, 
                                                 df_time_novisit$t_notify, df_time_novisit$t_end_quarantine)
          df_time_novisit$t_end_isolate = ifelse(df_time_novisit$test & df_time_novisit$isolate & df_time_novisit$type==0 & df_time_novisit$t_notify!=-17 & df_time_novisit$t_notify>0, df_time_novisit$t_notify + days_isolate_res, df_time_novisit$t_end_isolate)
          df_time_novisit$t_end_isolate = ifelse(df_time_novisit$test & df_time_novisit$isolate & (df_time_novisit$type==1 | df_time_novisit$type==3) & df_time_novisit$t_notify!=-17 & df_time_novisit$t_notify>0, df_time_novisit$t_notify + days_isolate_staff, df_time_novisit$t_end_isolate)
          df_time_novisit$t_end_isolate = ifelse(df_time_novisit$test & df_time_novisit$isolate & df_time_novisit$type==2 & df_time_novisit$t_notify!=-17 & df_time_novisit$t_notify>0, df_time_novisit$t_notify + days_isolate_visit, df_time_novisit$t_end_isolate)
          df_time_novisit$isolate_home = df_time_novisit$test & df_time_novisit$isolate & df_time_novisit$inf & df_time_novisit$t_notify <= t & df_time_novisit$t_notify!=-17 & df_time_novisit$t_end_isolate>=t & df_time_novisit$type!=0
          df_time_novisit$isolate_room = df_time_novisit$test & df_time_novisit$isolate & df_time_novisit$inf & df_time_novisit$t_notify <= t & df_time_novisit$t_notify!=-17 & df_time_novisit$t_end_isolate>=t & df_time_novisit$type==0
          df_time_novisit$isolated = df_time_novisit$test & (df_time_novisit$isolate_home | df_time_novisit$isolate_room)
          # room_test_ind = room_test_ind + length(unique(df$room[df$test_type & df$present & !df$isolated & !df$q_room]))
          
          df_time_novisit$test_tp_count = df_time_novisit$test_tp_count + ifelse(df_time_novisit$test_type & df_time_novisit$shift!="absent", df_time_novisit$inf*df_time_novisit$test, 0)
          df_time_novisit$test_fn_count = df_time_novisit$test_fn_count + ifelse(df_time_novisit$test_type & df_time_novisit$shift!="absent", df_time_novisit$inf*(1-df_time_novisit$test), 0)
          df_time_novisit$test_eligible = df_time_novisit$test_eligible + (df_time_novisit$shift!="absent")*df_time_novisit$test_type
          
          df_time_test[df_time_test$type!=2,] = df_time_novisit
        }
        
        # visitor testing
        if("family"%in% colnames(df_time) & visit_test){
          df_time_test$test_type[df_time_test$type==2] = T
          df_time_test$test_ct[df_time_test$type==2] = df_time_test$test_ct[df_time_test$type==2] + rbinom(nrow(df_time_test[df_time_test$type==2,]), size = 1, prob = (df_time_test$shift[df_time_test$type==2]!="absent")*as.numeric(df_time_test$test_type[df_time_test$type==2]))
          df_time_test$test[df_time_test$type==2] = rbinom(nrow(df_time_test[df_time_test$type==2,]), size = 1, prob = test_sens*as.numeric(df_time_test$test_type[df_time_test$type==2])*(df_time_test$shift[df_time_test$type==2]!="absent"))
          
          df_time_test$t_end_inf[df_time_test$type==2] = ifelse(df_time_test$test[df_time_test$type==2] & df_time_test$trans_now[df_time_test$type==2], t, df_time_test$t_end_inf[df_time_test$type==2])
          df_time_test$t_notify[df_time_test$type==2] = ifelse(df_time_test$test[df_time_test$type==2] & df_time_test$trans_now[df_time_test$type==2] & (df_time_test$t_symp[df_time_test$type==2]>=t | df_time_test$symp[df_time_test$type==2]==0), t, df_time_test$t_notify[df_time_test$type==2])
          df_time_test$detected[df_time_test$type==2] = ifelse(df_time_test$test[df_time_test$type==2] & df_time_test$trans_now[df_time_test$type==2], 1, df_time_test$detected[df_time_test$type==2])
          df_time_test$t_end_quarantine[df_time_test$type==2] = ifelse(df_time_test$test[df_time_test$type==2] & df_time_test$t_quarantine[df_time_test$type==2]<=t & df_time_test$t_quarantine[df_time_test$type==2]!=-13 & df_time_test$t_end_quarantine[df_time_test$type==2]>t & df_time_test$t_notify[df_time_test$type==2]!=-17 & df_time_test$t_notify[df_time_test$type==2]>=t & df_time_test$t_notify[df_time_test$type==2]-df_time_test$t_quarantine[df_time_test$type==2]<df_time_test$t_end_quarantine[df_time_test$type==2]-df_time_test$t_quarantine[df_time_test$type==2] & (df_time_test$t_end_inf[df_time_test$type==2]>t | df_time_test$t_end_inf[df_time_test$type==2]==-1) & df_time_test$vacc[df_time_test$type==2]==0, 
                                                                       df_time_test$t_notify[df_time_test$type==2], df_time_test$t_end_quarantine[df_time_test$type==2])
          df_time_test$t_end_isolate[df_time_test$type==2] = ifelse(df_time_test$test[df_time_test$type==2] & df_time_test$isolate[df_time_test$type==2] & df_time_test$t_notify[df_time_test$type==2]!=-17 & df_time_test$t_notify[df_time_test$type==2]>0, df_time_test$t_notify[df_time_test$type==2] + days_isolate_visit, df_time_test$t_end_isolate[df_time_test$type==2])
          df_time_test$isolate_home[df_time_test$type==2] = df_time_test$test[df_time_test$type==2] & df_time_test$isolate[df_time_test$type==2] & df_time_test$inf[df_time_test$type==2] & df_time_test$t_notify[df_time_test$type==2] <= t & df_time_test$t_notify[df_time_test$type==2]!=-17 & df_time_test$t_end_isolate[df_time_test$type==2]>=t
          df_time_test$isolated[df_time_test$type==2] = df_time_test$test[df_time_test$type==2] & df_time_test$isolate_home[df_time_test$type==2]
          
          df_time_test$test_tp_count[df_time_test$type==2] = df_time_test$test_tp_count[df_time_test$type==2] + ifelse(df_time_test$test_type[df_time_test$type==2] & df_time_test$shift[df_time_test$type==2]!="absent", df_time_test$inf[df_time_test$type==2]*df_time_test$test[df_time_test$type==2], 0)
          df_time_test$test_fn_count[df_time_test$type==2] = df_time_test$test_fn_count[df_time_test$type==2] + ifelse(df_time_test$test_type[df_time_test$type==2] & df_time_test$shift[df_time_test$type==2]!="absent", df_time_test$inf[df_time_test$type==2]*(1-df_time_test$test[df_time_test$type==2]), 0)
          df_time_test$test_eligible[df_time_test$type==2] = df_time_test$test_eligible[df_time_test$type==2] + (df_time_test$shift[df_time_test$type==2]!="absent")*df_time_test$test_type[df_time_test$type==2]
        }
        
        df_time[(df_time$t_inf==-1 | df_time$t_end_inf_home>=t),] = df_time_test
        
        # re-estimate who is present (among staff and visitors) 
        df_time$shift[df_time$type!=0] = ifelse(df_time$shift[df_time$type!=0]!="absent" & !df_time$isolate_home[df_time$type!=0] & !df_time$quarantined[df_time$type!=0], df_time$shift[df_time$type!=0], "absent")
        if("family"%in%colnames(df_time)){
          df_time$shift[df_time$type==2] = ifelse(df_time$shift[df_time$type==2]!="absent" & !df_time$isolate_home[df_time$type==2] & !df_time$quarantined[df_time$type==2], df_time$shift[df_time$type==2], "absent")
          df_time$shift[df_time$type==2] = ifelse(df_time$flag_fam[df_time$type==2]==1, "absent", df_time$shift[df_time$type==2])
        }
        
        df_time$present_susp = df_time$shift!="absent" & df_time$not_inf
        
        # update infectious and at nursing home
        df_time$trans_now = df_time$shift!="absent" & df_time$inf
      }else{
          
        if(t%in%testing_days){
          
          # testing residents and staff
          df_time_novisit = df_time[df_time$type!=2,]
          
          df_time_novisit$test_ct = df_time_novisit$test_ct + rbinom(nrow(df_time_novisit), size = 1, prob = (df_time_novisit$shift!="absent")*test_frac*as.numeric(df_time_novisit$test_type)) # count how many tests individual takes
          df_time_novisit$test_ct_res = ifelse(df_time_novisit$type==0, df_time_novisit$test_ct, df_time_novisit$test_ct_res)
          df_time_novisit$test = rbinom(nrow(df_time_novisit), size = 1, prob = test_sens*test_frac*as.numeric(df_time_novisit$test_type)*(df_time_novisit$shift!="absent")) # record only accurate test results?
          df_time_novisit$t_end_inf = ifelse(df_time_novisit$test & df_time_novisit$trans_now, t, df_time_novisit$t_end_inf)
          df_time_novisit$t_notify = ifelse(df_time_novisit$test & df_time_novisit$trans_now & (df_time_novisit$t_symp>=t | df_time_novisit$symp==0), t, df_time_novisit$t_notify)
          df_time_novisit$detected = ifelse(df_time_novisit$test & df_time_novisit$trans_now, 1, df_time_novisit$detected)
          df_time_novisit$t_end_quarantine = ifelse(df_time_novisit$test & df_time_novisit$t_quarantine<=t & df_time_novisit$t_quarantine!=-13 & df_time_novisit$t_end_quarantine>t & df_time_novisit$t_notify!=-17 & df_time_novisit$t_notify>=t & df_time_novisit$t_notify-df_time_novisit$t_quarantine<df_time_novisit$t_end_quarantine-df_time_novisit$t_quarantine & (df_time_novisit$t_end_inf>t | df_time_novisit$t_end_inf==-1) & df_time_novisit$vacc==0, 
                                            df_time_novisit$t_notify, df_time_novisit$t_end_quarantine)
          df_time_novisit$t_end_isolate = ifelse(df_time_novisit$test & df_time_novisit$isolate & df_time_novisit$type==0 & df_time_novisit$t_notify!=-17 & df_time_novisit$t_notify>0, df_time_novisit$t_notify + days_isolate_res, df_time_novisit$t_end_isolate)
          df_time_novisit$t_end_isolate = ifelse(df_time_novisit$test & df_time_novisit$isolate & (df_time_novisit$type==1 | df_time_novisit$type==3) & df_time_novisit$t_notify!=-17 & df_time_novisit$t_notify>0, df_time_novisit$t_notify + days_isolate_staff, df_time_novisit$t_end_isolate)
          df_time_novisit$t_end_isolate = ifelse(df_time_novisit$test & df_time_novisit$isolate & df_time_novisit$type==2 & df_time_novisit$t_notify!=-17 & df_time_novisit$t_notify>0, df_time_novisit$t_notify + days_isolate_visit, df_time_novisit$t_end_isolate)
          df_time_novisit$isolate_home = df_time_novisit$test & df_time_novisit$isolate & df_time_novisit$inf & df_time_novisit$t_notify <= t & df_time_novisit$t_notify!=-17 & df_time_novisit$t_end_isolate>=t & df_time_novisit$type!=0
          df_time_novisit$isolate_room = df_time_novisit$test & df_time_novisit$isolate & df_time_novisit$inf & df_time_novisit$t_notify <= t & df_time_novisit$t_notify!=-17 & df_time_novisit$t_end_isolate>=t & df_time_novisit$type==0
          df_time_novisit$isolated = df_time_novisit$test & (df_time_novisit$isolate_home | df_time_novisit$isolate_room)
          
          df_time_novisit$test_tp_count = df_time_novisit$test_tp_count + ifelse(df_time_novisit$test_type & df_time_novisit$shift!="absent", df_time_novisit$inf*df_time_novisit$test, 0)
          df_time_novisit$test_fn_count = df_time_novisit$test_fn_count + ifelse(df_time_novisit$test_type & df_time_novisit$shift!="absent", df_time_novisit$inf*(1-df_time_novisit$test), 0)
          df_time_novisit$test_eligible = df_time_novisit$test_eligible + (df_time_novisit$shift!="absent")*df_time_novisit$test_type
          
          df_time[df_time$type!=2,] = df_time_novisit
        }
        
        # visitor testing
        if("family"%in% colnames(df_time) & visit_test){
          df_time$test_type[df_time$type==2] = T
          df_time$test_ct[df_time$type==2] = df_time$test_ct[df_time$type==2] + rbinom(nrow(df_time[df_time$type==2,]), size = 1, prob = (df_time$shift[df_time$type==2]!="absent")*as.numeric(df_time$test_type[df_time$type==2]))
          df_time$test[df_time$type==2] = rbinom(nrow(df_time[df_time$type==2,]), size = 1, prob = test_sens*as.numeric(df_time$test_type[df_time$type==2])*(df_time$shift[df_time$type==2]!="absent"))
          
          df_time$t_end_inf[df_time$type==2] = ifelse(df_time$test[df_time$type==2] & df_time$trans_now[df_time$type==2], t, df_time$t_end_inf[df_time$type==2])
          df_time$t_notify[df_time$type==2] = ifelse(df_time$test[df_time$type==2] & df_time$trans_now[df_time$type==2] & (df_time$t_symp[df_time$type==2]>=t | df_time$symp[df_time$type==2]==0), t, df_time$t_notify[df_time$type==2])
          df_time$detected[df_time$type==2] = ifelse(df_time$test[df_time$type==2] & df_time$trans_now[df_time$type==2], 1, df_time$detected[df_time$type==2])
          df_time$t_end_quarantine[df_time$type==2] = ifelse(df_time$test[df_time$type==2] & df_time$t_quarantine[df_time$type==2]<=t & df_time$t_quarantine[df_time$type==2]!=-13 & df_time$t_end_quarantine[df_time$type==2]>t & df_time$t_notify[df_time$type==2]!=-17 & df_time$t_notify[df_time$type==2]>=t & df_time$t_notify[df_time$type==2]-df_time$t_quarantine[df_time$type==2]<df_time$t_end_quarantine[df_time$type==2]-df_time$t_quarantine[df_time$type==2] & (df_time$t_end_inf[df_time$type==2]>t | df_time$t_end_inf[df_time$type==2]==-1) & df_time$vacc[df_time$type==2]==0, 
                                                             df_time$t_notify[df_time$type==2], df_time$t_end_quarantine[df_time$type==2])
          df_time$t_end_isolate[df_time$type==2] = ifelse(df_time$test[df_time$type==2] & df_time$isolate[df_time$type==2] & df_time$t_notify[df_time$type==2]!=-17 & df_time$t_notify[df_time$type==2]>0, df_time$t_notify[df_time$type==2] + days_isolate_visit, df_time$t_end_isolate[df_time$type==2])
          df_time$isolate_home[df_time$type==2] = df_time$test[df_time$type==2] & df_time$isolate[df_time$type==2] & df_time$inf[df_time$type==2] & df_time$t_notify[df_time$type==2] <= t & df_time$t_notify[df_time$type==2]!=-17 & df_time$t_end_isolate[df_time$type==2]>=t
          df_time$isolated[df_time$type==2] = df_time$test[df_time$type==2] & df_time$isolate_home[df_time$type==2]
          
          df_time$test_tp_count[df_time$type==2] = df_time$test_tp_count[df_time$type==2] + ifelse(df_time$test_type[df_time$type==2] & df_time$shift[df_time$type==2]!="absent", df_time$inf[df_time$type==2]*df_time$test[df_time$type==2], 0)
          df_time$test_fn_count[df_time$type==2] = df_time$test_fn_count[df_time$type==2] + ifelse(df_time$test_type[df_time$type==2] & df_time$shift[df_time$type==2]!="absent", df_time$inf[df_time$type==2]*(1-df_time$test[df_time$type==2]), 0)
          df_time$test_eligible[df_time$type==2] = df_time$test_eligible[df_time$type==2] + (df_time$shift[df_time$type==2]!="absent")*df_time$test_type[df_time$type==2]
        }
          
        # re-estimate who is present (among staff and visitors) 
        df_time$shift[df_time$type!=0] = ifelse(df_time$shift[df_time$type!=0]!="absent" & !df_time$isolate_home[df_time$type!=0] & !df_time$quarantined[df_time$type!=0], df_time$shift[df_time$type!=0], "absent")
        if("family"%in%colnames(df_time)){
          df_time$shift[df_time$type==2] = ifelse(df_time$shift[df_time$type==2]!="absent" & !df_time$isolate_home[df_time$type==2] & !df_time$quarantined[df_time$type==2], df_time$shift[df_time$type==2], "absent")
          df_time$shift[df_time$type==2] = ifelse(df_time$flag_fam[df_time$type==2]==1, "absent", df_time$shift[df_time$type==2])
        }
        
        df_time$present_susp = df_time$shift!="absent" & df_time$not_inf
        
        # update infectious and at nursing home
        df_time$trans_now = df_time$shift!="absent" & df_time$inf
      }
    }

    
    #### SELECT NEXT GENERATION INFECTIONS ####
    # run model for infectious individuals in resident room
    if(sum(df_time$trans_now)>0) {
      
      room_infs = df_time$id[df_time$trans_now & (df_time$type==0 | ((df_time$type==1 | df_time$type==3) & df_time$role!=4) | df_time$type==2)]
      if(length(room_infs)>1) room_infs = sample(room_infs)
      
      room_inf_vec_total = c()
      for(a in room_infs){
        room_inf_vec = c()
        staff_vec_id = c()
        visit_present = F
        visit_vec_id = 0
        has_roommate = F
        roommate_vec_id = 0
        
        # ROOM CONTACTS
        for(shift in 1:3){
          if(shift==1 & (df_time$shift[df_time$id==a]=="morning" | df_time$shift[df_time$id==a]=="all")){
            infs = run_room(a, df_time[df_time$shift=="morning" | df_time$shift=="all",], t, quarantine, cohorts, shift)
            if(length(infs[[1]])>0){room_inf_vec[(length(room_inf_vec)+1):(length(room_inf_vec)+length(infs[[1]]))] = infs[[1]]}
            if(df_time$type[df_time$id==a]==0){
              if(!is.null(infs[[2]])){
                # store staff id for residents
                staff_vec_id[(length(staff_vec_id)+1):(length(staff_vec_id)+length(infs[[2]]$id))] = infs[[2]]$id
              }
              if(!is.null(infs[[3]])){
                # check for visitors
                visit_vec_id = infs[[3]]$id
                visit_present = T
              }
              if(!is.null(infs[[4]])){
                # check for roommate
                roommate_vec_id = infs[[4]]$id
                has_roommate = T
              }
            }
          }
          if(shift==2 & (df_time$shift[df_time$id==a]=="evening" | df_time$shift[df_time$id==a]=="all")){
            infs = run_room(a, df_time[df_time$shift=="evening" | df_time$shift=="all",], t, quarantine, cohorts, shift)
            if(length(infs[[1]])>0){room_inf_vec[(length(room_inf_vec)+1):(length(room_inf_vec)+length(infs[[1]]))] = infs[[1]]}
            # store staff id for residents
            if(df_time$type[df_time$id==a]==0 & !is.null(infs[[2]])){staff_vec_id[(length(staff_vec_id)+1):(length(staff_vec_id)+length(infs[[2]]$id))] = infs[[2]]$id}
          }
          if(shift==3 & (df_time$shift[df_time$id==a]=="night" | df_time$shift[df_time$id==a]=="all")){
            infs = run_room(a, df_time[df_time$shift=="night" | df_time$shift=="all",], t, quarantine, cohorts, shift)
            if(length(infs[[1]])>0){room_inf_vec[(length(room_inf_vec)+1):(length(room_inf_vec)+length(infs[[1]]))] = infs[[1]]}
            # store staff id for residents
            if(df_time$type[df_time$id==a]==0 & !is.null(infs[[2]])){staff_vec_id[(length(staff_vec_id)+1):(length(staff_vec_id)+length(infs[[2]]$id))] = infs[[2]]$id}
          }
          room_inf_vec = room_inf_vec[!room_inf_vec==0]
          df_time$susp[df_time$id%in%room_inf_vec] = 0
          if(length(room_inf_vec)>0){room_inf_vec_total[(length(room_inf_vec_total)+1):(length(room_inf_vec_total)+length(room_inf_vec))] = room_inf_vec}
        } 
        
        df_time$location[df_time$id%in%room_inf_vec] = "Room"
        
        # if infected is resident
        if(df_time$type[df_time$id==a]==0){
          
          if(test & quarantine){
            # get next day of testing
            future_days = c()
            for(day in testing_days){
              future_days[length(future_days)+1] = ifelse(day-t>0, day, 0)
            }
            next_day = t+min(abs(future_days-t))
            
            if(t%in%testing_days){
              if(visit_present){
                df_time$t_quarantine[df_time$id==visit_vec_id & (df_time$t_quarantine==-13 | df_time$t_end_quarantine<t) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = t
                df_time$t_end_quarantine[df_time$id==visit_vec_id & df_time$t_quarantine==t & (df_time$t_end_quarantine==-13 | df_time$t_end_quarantine<t) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = t+quarantine.length
              }
              if(has_roommate){
                df_time$t_quarantine[df_time$id==roommate_vec_id & (df_time$t_quarantine==-13 | df_time$t_end_quarantine<t) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = t
                df_time$t_end_quarantine[df_time$id==roommate_vec_id & df_time$t_quarantine==t & (df_time$t_end_quarantine==-13 | df_time$t_end_quarantine<t) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = t+quarantine.length
              }
            }else{
              if(visit_present){
                df_time$t_quarantine[df_time$id==visit_vec_id & (df_time$t_quarantine==-13 | df_time$t_end_quarantine<t) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = ifelse(df_time$symp[df_time$id==a]==1 & df_time$t_symp[df_time$id==a]>=t & df_time$t_symp[df_time$id==a]!=-1 & df_time$t_symp[df_time$id==a]<next_day, 
                                                                                                                                                                                                df_time$t_symp[df_time$id==a], 
                                                                                                                                                                                                ifelse(next_day-df_time$t_inf[df_time$id==a] < df_time$t_end_inf[df_time$id==a]-df_time$t_inf[df_time$id==a], next_day, df_time$t_quarantine[df_time$id==visit_vec_id & (df_time$t_quarantine==-13 | df_time$t_end_quarantine<t) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]))
                df_time$t_end_quarantine[df_time$id==visit_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | df_time$t_end_quarantine<t) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = ifelse(df_time$t_quarantine[df_time$id==visit_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | df_time$t_end_quarantine<t) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]>=t, 
                                                                                                                                                                                                                                  df_time$t_quarantine[df_time$id==visit_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | df_time$t_end_quarantine<t) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]+quarantine.length, 
                                                                                                                                                                                                                                  df_time$t_end_quarantine[df_time$id==visit_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | df_time$t_end_quarantine<t) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0])
              }
              if(has_roommate){
                df_time$t_quarantine[df_time$id==roommate_vec_id & (df_time$t_quarantine==-13 | df_time$t_end_quarantine<t) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = ifelse(df_time$symp[df_time$id==a]==1 & df_time$t_symp[df_time$id==a]>=t & df_time$t_symp[df_time$id==a]!=-1 & df_time$t_symp[df_time$id==a]<next_day, 
                                                                                                                                                                                                   df_time$t_symp[df_time$id==a], 
                                                                                                                                                                                                   ifelse(next_day-df_time$t_inf[df_time$id==a] < df_time$t_end_inf[df_time$id==a]-df_time$t_inf[df_time$id==a], next_day, df_time$t_quarantine[df_time$id==roommate_vec_id & (df_time$t_quarantine==-13 | df_time$t_end_quarantine<t) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]))
                df_time$t_end_quarantine[df_time$id==roommate_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | df_time$t_end_quarantine<t) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = ifelse(df_time$t_quarantine[df_time$id==roommate_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | df_time$t_end_quarantine<t) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]>=t, 
                                                                                                                                                                                                                                     df_time$t_quarantine[df_time$id==roommate_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | df_time$t_end_quarantine<t) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]+quarantine.length, 
                                                                                                                                                                                                                                     df_time$t_end_quarantine[df_time$id==roommate_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | df_time$t_end_quarantine<t) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0])
              }
            }
          }
          
          #Track risk set for unit testing
          if(quarantine & df_time$isolated[df_time$id==a]){
            # indicate whether staff are infected while treating infected residents
            df_time$inf_quarantine[df_time$id%in%room_inf_vec & (df_time$type==1 | df_time$type==3)] = T
            df_time$person.days.at.risk.room.staff.quarantine[df_time$id == a] = df_time$person.days.at.risk.room.staff.quarantine[df_time$id == a] +
              (df_time$t_inf[df_time$id == a] <= t & df_time$t_end_inf_home[df_time$id == a] >= t)*sum(df_time$present_susp[df_time$id%in%staff_vec_id & (df_time$type==1 | df_time$type==3) & (df_time$susp!=0 | df_time$id %in% room_inf_vec)])
          }else{
            df_time$person.days.at.risk.room.staff[df_time$id == a] = df_time$person.days.at.risk.room.staff[df_time$id == a] +
              (df_time$t_inf[df_time$id == a] <= t & df_time$t_end_inf_home[df_time$id == a] >= t)*sum(df_time$present_susp[df_time$id%in%staff_vec_id & (df_time$type==1 | df_time$type==3) & (df_time$susp!=0 | df_time$id %in% room_inf_vec)])
            
            if(visit_present){
              df_time$person.days.at.risk.room.visit[df_time$id == a] = df_time$person.days.at.risk.room.visit[df_time$id == a] +
                (df_time$t_inf[df_time$id == a] <= t & df_time$t_end_inf_home[df_time$id == a] >= t)*sum(df_time$present_susp[df_time$id==visit_vec_id &
                                                                                                                                df_time$type==2 & (df_time$susp!=0 | df_time$id %in% room_inf_vec)])
            }
          }
          df_time$person.days.at.risk.room.res[df_time$id == a] = df_time$person.days.at.risk.room.res[df_time$id == a] + 
            (df_time$t_inf[df_time$id == a] <= t & df_time$t_end_inf_home[df_time$id == a] >= t)*sum(df_time$present_susp[df_time$id==roommate_vec_id &
                                                                                                                            df_time$type==0 & (df_time$susp!=0 | df_time$id %in% room_inf_vec)])
        }
        
        # if infected is direct-care staff
        if((df_time$type[df_time$id==a]==1 | df_time$type[df_time$id==a]==3) & df_time$role[df_time$id==a]!=4){
          
          # quarantine
          # if pre/asymptomatic, find when infected tests positive/symptoms show
          if(test & quarantine){
            # get next day of testing
            future_days = c()
            for(day in testing_days){
              future_days[length(future_days)+1] = ifelse(day-t>0, day, 0)
            }
            next_day = t+min(abs(future_days-t))
            
            res_vec_id = infs[[2]]$id
            
            if(t%in%testing_days){
              df_time$t_quarantine[df_time$id%in%res_vec_id & (df_time$t_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = t
              
              df_time$t_end_quarantine[df_time$id%in%res_vec_id & df_time$t_quarantine==t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = t+quarantine.length
            }else{
              df_time$t_quarantine[df_time$id%in%res_vec_id & (df_time$t_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = ifelse(df_time$symp[df_time$id==a]==1 & df_time$t_symp[df_time$id==a]>=t & df_time$t_symp[df_time$id==a]!=-1 & df_time$t_symp[df_time$id==a]<next_day, 
                                                                                                                                                                                                                                df_time$t_symp[df_time$id==a], 
                                                                                                                                                                                                                                ifelse(next_day-df_time$t_inf[df_time$id==a] < df_time$t_end_inf[df_time$id==a]-df_time$t_inf[df_time$id==a], next_day, df_time$t_quarantine[df_time$id%in%res_vec_id & (df_time$t_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]))
              
              df_time$t_end_quarantine[df_time$id%in%res_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = ifelse(df_time$t_quarantine[df_time$id%in%res_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]>=t, 
                                                                                                                                                                                                                                                                  df_time$t_quarantine[df_time$id%in%res_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]+quarantine.length, 
                                                                                                                                                                                                                                                                  df_time$t_end_quarantine[df_time$id%in%res_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0])
            }
          }
          
          #Track risk set for unit testing
          if(quarantine){
            # check if residents got infected by staff while quarantined
            df_time$inf_quarantine[df_time$id%in%room_inf_vec & df_time$quarantined & df_time$type==0] = T
            df_time$person.days.at.risk.room.res.quarantine[df_time$id == a] = df_time$person.days.at.risk.room.res.quarantine[df_time$id == a] +
              (df_time$t_inf[df_time$id == a]<=t & df_time$t_end_inf_home[df_time$id == a]>=t)*sum(df_time$present_susp[df_time$id%in%infs[[2]]$id & df_time$quarantined & df_time$type==0 & (df_time$susp!=0 | df_time$id %in% room_inf_vec)])
          }
          df_time$person.days.at.risk.room.res[df_time$id == a] = df_time$person.days.at.risk.room.res[df_time$id == a] +
            (df_time$t_inf[df_time$id == a]<=t & df_time$t_end_inf_home[df_time$id == a]>=t)*sum(df_time$present_susp[df_time$id%in%infs[[2]]$id & !df_time$quarantined & df_time$type==0 & (df_time$susp!=0 | df_time$id %in% room_inf_vec)])
        }
        
        # if infected is visitor
        if(df_time$type[df_time$id==a]==2){
          
          # quarantine
          # if pre/asymptomatic, find when symptoms show
          if(quarantine){
            res_vec_id = infs[[2]]$id
            
            df_time$t_quarantine[df_time$id%in%res_vec_id & (df_time$t_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = ifelse(df_time$symp[df_time$id==a]==1 & df_time$t_symp[df_time$id==a]>=t & df_time$t_symp[df_time$id==a]!=-1, 
                                                                                                                                                                                                                              df_time$t_symp[df_time$id==a], 
                                                                                                                                                                                                                              df_time$t_quarantine[df_time$id%in%res_vec_id & (df_time$t_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0])
            
            df_time$t_end_quarantine[df_time$id%in%res_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = ifelse(df_time$t_quarantine[df_time$id%in%res_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]>=t, 
                                                                                                                                                                                                                                                                df_time$t_quarantine[df_time$id%in%res_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]+quarantine.length, 
                                                                                                                                                                                                                                                                df_time$t_end_quarantine[df_time$id%in%res_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0])
          }
          
          #Track risk set for unit testing
          df_time$person.days.at.risk.room.res[df_time$id == a] = df_time$person.days.at.risk.room.res[df_time$id == a] +
            (df_time$t_inf[df_time$id == a] <= t & df_time$t_end_inf_home[df_time$id == a] >= t)*sum(df_time$present_susp[df_time$id%in%infs[[2]]$id & df_time$type==0 & (df_time$susp!=0 | df_time$id %in% room_inf_vec)])
        }
        
        # add to total # of infections from this person
        df_time$tot_inf[df_time$id==a] = df_time$tot_inf[df_time$id==a] + sum(unique(room_inf_vec[!room_inf_vec==0])>0, na.rm=T)
        
        # flag people infected at this time step
        df_time$now = ifelse(df_time$id%in%room_inf_vec, T, df_time$now)
        df_time$source = ifelse(df_time$id%in%room_inf_vec, a, df_time$source)
        df_time$source_symp = ifelse(df_time$id%in%room_inf_vec, df_time$symp[df_time$id==a], df_time$source_symp)
        df_time$not_inf = ifelse(df_time$id%in%room_inf_vec, F, df_time$not_inf)
      }
      
      # run model for infectious individuals in staff interactions
      staff_infs = df_time$id[df_time$trans_now & (df_time$type==1 | df_time$type==3)]
      if(length(staff_infs) > 1) staff_infs = sample(staff_infs)
      
      staff_inf_vec_total = c()
      for(a in staff_infs){
        staff_inf_vec.out = c()
        
        # STAFF INTERACTIONS
        for(shift in 1:3){
          if(shift==1 & (df_time$shift[df_time$id==a]=="morning")){
            staff_inf_vec.out = run_staff(a, df_time[df_time$shift=="morning" & (df_time$type==1 | df_time$type==3),], n_contact_staff, rel_trans_staff)
          }
          if(shift==2 & (df_time$shift[df_time$id==a]=="evening")){
            staff_inf_vec.out = run_staff(a, df_time[df_time$shift=="evening" & (df_time$type==1 | df_time$type==3),], n_contact_staff, rel_trans_staff)
          }
          if(shift==3 & (df_time$shift[df_time$id==a]=="night")){
            staff_inf_vec.out = run_staff(a, df_time[df_time$shift=="night" & (df_time$type==1 | df_time$type==3),], n_contact_staff, rel_trans_staff)
          }
          staff_inf_vec.out[[1]] = staff_inf_vec.out[[1]][!staff_inf_vec.out[[1]]==0]
        }
        df_time$susp[df_time$id%in%staff_inf_vec.out[[1]]] = 0
        staff_inf_vec = staff_inf_vec.out[[1]]
        if(length(staff_inf_vec)>0){staff_inf_vec_total[(length(staff_inf_vec_total)+1):(length(staff_inf_vec_total)+length(staff_inf_vec))] = staff_inf_vec}
        df_time$location[df_time$id%in%staff_inf_vec] = "Staff interactions"
        
        staff_vec_id = staff_inf_vec.out[[2]]
        
        if(test & quarantine & length(staff_vec_id)>0){
          # get next day of testing
          future_days = c()
          for(day in testing_days){
            future_days[length(future_days)+1] = ifelse(day-t>0, day, 0)
          }
          next_day = t+min(abs(future_days-t))
          
          if(t%in%testing_days){
            df_time$t_quarantine[df_time$id%in%staff_vec_id & (df_time$t_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = t
            
            df_time$t_end_quarantine[df_time$id%in%staff_vec_id & df_time$t_quarantine==t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = t+quarantine.length
          }else{
            df_time$t_quarantine[df_time$id%in%staff_vec_id & (df_time$t_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = ifelse(df_time$symp[df_time$id==a]==1 & df_time$t_symp[df_time$id==a]>=t & df_time$t_symp[df_time$id==a]!=-1 & df_time$t_symp[df_time$id==a]<next_day, 
                                                                                                                                                                                                                                df_time$t_symp[df_time$id==a], 
                                                                                                                                                                                                                                ifelse(next_day-df_time$t_inf[df_time$id==a] < df_time$t_end_inf[df_time$id==a]-df_time$t_inf[df_time$id==a], next_day, df_time$t_quarantine[df_time$id%in%staff_vec_id & (df_time$t_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]))
            
            df_time$t_end_quarantine[df_time$id%in%staff_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = ifelse(df_time$t_quarantine[df_time$id%in%staff_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]>=t, 
                                                                                                                                                                                                                                                                  df_time$t_quarantine[df_time$id%in%staff_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]+quarantine.length, 
                                                                                                                                                                                                                                                                  df_time$t_end_quarantine[df_time$id%in%staff_vec_id & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0])
          }
        }
        
        #Track risk set for unit testing
        df_time$person.days.at.risk.staff.staff[df_time$id == a] = df_time$person.days.at.risk.staff.staff[df_time$id == a] + (df_time$t_inf[df_time$id == a] <= t & 
                                                                                                                                 df_time$t_end_inf[df_time$id == a] >= t)*sum(df_time$present_susp[df_time$id %in% staff_inf_vec.out[[2]] & (df_time$susp!=0 | df_time$id %in% staff_inf_vec) & !(df_time$id%in%unique(room_inf_vec_total))])
        
        # add to total # of infections from this person
        df_time$tot_inf[df_time$id==a] = df_time$tot_inf[df_time$id==a] + sum(unique(staff_inf_vec[!staff_inf_vec==0])>0, na.rm=T)
        
        # flag people infected at this time step
        df_time$now = ifelse(df_time$id%in%staff_inf_vec, T, df_time$now)
        df_time$source = ifelse(df_time$id%in%staff_inf_vec, a, df_time$source)
        df_time$source_symp = ifelse(df_time$id%in%staff_inf_vec, df_time$symp[df_time$id==a], df_time$source_symp)
        df_time$not_inf = ifelse(df_time$id%in%staff_inf_vec, F, df_time$not_inf)
      }
      
      # run model for infectious individuals in common area of nursing home
      common_infs = df_time$id[df_time$trans_now & !df_time$isolated & !df_time$quarantined & df_time$type!=2]
      if(length(common_infs)>1) common_infs = sample(common_infs)
      
      for(a in common_infs){
        common_inf_vec.out = c()
        
        # COMMON AREA INTERACTIONS
        for(shift in 1:3){
          if(shift==1 & (df_time$shift[df_time$id==a]=="morning" | df_time$shift[df_time$id==a]=="all")){
            infs = run_common(a, df_time[(df_time$shift=="morning" | df_time$shift=="all") & !df_time$isolated & !df_time$quarantined & df_time$type!=2,], n_contact_common_res, n_contact_common_staff, rel_trans_common)
            common_inf_vec.out = infs
            common_inf_vec.out[[1]] = common_inf_vec.out[[1]][!common_inf_vec.out[[1]]==0]
            df_time$susp[df_time$id%in%common_inf_vec.out[[1]]] = 0
          }
          if(shift==2 & (df_time$shift[df_time$id==a]=="evening" | df_time$shift[df_time$id==a]=="all")){
            if(df_time$type[df_time$id==a]!=0){
              infs = run_common(a, df_time[(df_time$shift=="evening" | df_time$shift=="all") & !df_time$isolated & !df_time$quarantined & df_time$type!=2,], n_contact_common_res, n_contact_common_staff, rel_trans_common)
              common_inf_vec.out = infs
              common_inf_vec.out[[1]] = common_inf_vec.out[[1]][!common_inf_vec.out[[1]]==0]
              df_time$susp[df_time$id%in%common_inf_vec.out[[1]]] = 0
            }else{
              infs = run_common(a, df_time[(df_time$shift=="evening" | df_time$shift=="all") & !df_time$isolated & !df_time$quarantined & df_time$type!=2 & !df_time$id%in%common_inf_vec.out[[2]],], n_contact_common_res, n_contact_common_staff, rel_trans_common)
              infs[[1]] = infs[[1]][!infs[[1]]==0]
              if(length(infs[[1]])>0){
                common_inf_vec.out[[1]][(length(common_inf_vec.out[[1]])+1):(length(common_inf_vec.out[[1]])+length(infs[[1]]))] = infs[[1]]
                df_time$susp[df_time$id%in%common_inf_vec.out[[1]]] = 0
              }
              if(length(infs[[2]])>0){common_inf_vec.out[[2]][(length(common_inf_vec.out[[2]])+1):(length(common_inf_vec.out[[2]])+length(infs[[2]]))] = infs[[2]]}
            }
          }
          if(shift==3 & (df_time$shift[df_time$id==a]=="night" | df_time$shift[df_time$id==a]=="all")){
            if(df_time$type[df_time$id==a]!=0){
              infs = run_common(a, df_time[(df_time$shift=="night" | df_time$shift=="all") & !df_time$isolated & !df_time$quarantined & df_time$type!=2,], n_contact_common_res, n_contact_common_staff, rel_trans_common)
              common_inf_vec.out = infs
              common_inf_vec.out[[1]] = common_inf_vec.out[[1]][!common_inf_vec.out[[1]]==0]
              df_time$susp[df_time$id%in%common_inf_vec.out[[1]]] = 0
            }else{
              infs = run_common(a, df_time[(df_time$shift=="night" | df_time$shift=="all") & !df_time$isolated & !df_time$quarantined & df_time$type!=2 & !df_time$id%in%common_inf_vec.out[[2]],], n_contact_common_res, n_contact_common_staff, rel_trans_common)
              infs[[1]] = infs[[1]][!infs[[1]]==0]
              if(length(infs[[1]])>0){
                common_inf_vec.out[[1]][(length(common_inf_vec.out[[1]])+1):(length(common_inf_vec.out[[1]])+length(infs[[1]]))] = infs[[1]]
                df_time$susp[df_time$id%in%common_inf_vec.out[[1]]] = 0
              }
              if(length(infs[[2]])>0){common_inf_vec.out[[2]][(length(common_inf_vec.out[[2]])+1):(length(common_inf_vec.out[[2]])+length(infs[[2]]))] = infs[[2]]}
            }
          }
        }
        common_inf_vec = common_inf_vec.out[[1]]
        df_time$location[df_time$id%in%common_inf_vec] = "Common area"
        
        # quarantine
        # if pre/asymptomatic, find when infected tests positive/symptoms show
        if(test & quarantine & length(df_time[df_time$id%in%common_inf_vec.out[[2]],])>0){
          future_days = c()
          for(day in testing_days){
            future_days[length(future_days)+1] = ifelse(day-t>0, day, 0)
          }
          next_day = t+min(abs(future_days-t))
          
          if(t%in%testing_days){
            df_time$t_quarantine[df_time$id%in%common_inf_vec.out[[2]] & (df_time$t_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = t
            
            df_time$t_end_quarantine[df_time$id%in%common_inf_vec.out[[2]] & df_time$t_quarantine==t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = t+quarantine.length
          }else{
            df_time$t_quarantine[df_time$id%in%common_inf_vec.out[[2]] & (df_time$t_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = ifelse(df_time$symp[df_time$id==a]==1 & df_time$t_symp[df_time$id==a]>=t & df_time$t_symp[df_time$id==a]!=-1 & df_time$t_symp[df_time$id==a]<next_day, 
                                                                                                                                                                                                                                           df_time$t_symp[df_time$id==a], ifelse(next_day-df_time$t_inf[df_time$id==a] < df_time$t_end_inf[df_time$id==a]-df_time$t_inf[df_time$id==a], next_day, 
                                                                                                                                                                                                                                                                                 df_time$t_quarantine[df_time$id%in%common_inf_vec.out[[2]] & (df_time$t_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]))
            df_time$t_end_quarantine[df_time$id%in%common_inf_vec.out[[2]] & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0] = ifelse(df_time$t_quarantine[df_time$id%in%common_inf_vec.out[[2]] & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]>=t, 
                                                                                                                                                                                                                                                                             df_time$t_quarantine[df_time$id%in%common_inf_vec.out[[2]] & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0]+quarantine.length, 
                                                                                                                                                                                                                                                                             df_time$t_end_quarantine[df_time$id%in%common_inf_vec.out[[2]] & df_time$t_quarantine>=t & (df_time$t_end_quarantine==-13 | (df_time$t_end_quarantine<t & df_time$t_end_quarantine!=-13)) & (df_time$t_notify>t | df_time$t_inf==-1) & df_time$vacc==0])
          }
        }
        
        #Track risk set for unit testing
        df_time$person.days.at.risk.common.res[df_time$id == a] = df_time$person.days.at.risk.common.res[df_time$id == a] + (df_time$t_inf[df_time$id == a] <= t & 
                                                                                                                               df_time$t_end_inf[df_time$id == a] >= t)*sum(df_time$present_susp[df_time$id %in% unique(common_inf_vec.out[[2]]) & df_time$type==0 & (df_time$susp!=0 | df_time$id %in% common_inf_vec) & !(df_time$id %in% unique(c(room_inf_vec_total, staff_inf_vec_total)))])
        df_time$person.days.at.risk.common.staff[df_time$id == a] = df_time$person.days.at.risk.common.staff[df_time$id == a] + (df_time$t_inf[df_time$id == a] <= t & 
                                                                                                                                   df_time$t_end_inf[df_time$id == a] >= t)*sum(df_time$present_susp[df_time$id %in% unique(common_inf_vec.out[[2]]) & (df_time$type==1 | df_time$type==3) & (df_time$susp!=0 | df_time$id %in% common_inf_vec) & !(df_time$id %in% unique(c(room_inf_vec_total, staff_inf_vec_total)))])
        
        # add to total # of infections from this person
        df_time$tot_inf[df_time$id==a] = df_time$tot_inf[df_time$id==a] + sum(unique(common_inf_vec[!common_inf_vec==0])>0, na.rm=T)
        
        # flag people infected at this time step
        df_time$now = ifelse(df_time$id%in%common_inf_vec, T, df_time$now)
        df_time$source = ifelse(df_time$id%in%common_inf_vec, a, df_time$source)
        df_time$source_symp = ifelse(df_time$id%in%common_inf_vec, df_time$symp[df_time$id==a], df_time$source_symp)
        df_time$not_inf = ifelse(df_time$id%in%common_inf_vec, F, df_time$not_inf)
      }
    }
    
    
    #### SET UP NEXT GENERATION INFECTEDS ####
    # update values updated at this stage
    if(sum(df_time$now)>0){
      
      df_time$start[df_time$now] = F     # remove seed if infected earlier
      df_time$t_exposed[df_time$now] = t
      df_time[df_time$now,] = make_infected(df_time[df_time$now,], days_inf = days_inf, days_isolate_res = days_isolate_res,
                                            days_isolate_staff = days_isolate_staff, days_isolate_visit = days_isolate_visit)
    }
    
    # round values
    df_time$t_notify = ceiling(df_time$t_notify)
    df_time$t_end_isolate = ceiling(df_time$t_end_isolate)
    
    # check quarantine
    if(quarantine){
      df_time$t_end_quarantine = ifelse(df_time$t_notify==df_time$t_quarantine | (df_time$t_notify<df_time$t_end_quarantine & df_time$t_notify!=-17 & df_time$t_end_quarantine!=-13) & df_time$vacc==0, df_time$t_notify, df_time$t_end_quarantine)
    }
    
    # update next day
    if("temp_rn_cohort_morning" %in% colnames(df)){
      df[df$t==t+1, !(colnames(df) %in% c("rn_cohort_morning", "rn_cohort_evening", "rn_cohort_night",
                                          "lpn_cohort_morning", "lpn_cohort_evening", "lpn_cohort_night",
                                          "cna_cohort_morning", "cna_cohort_evening", "cna_cohort_night",
                                          "ma_cohort_morning", "ma_cohort_evening", "admin_cohort_morning",
                                          "admin_cohort_evening", 
                                          "temp_rn_cohort_morning", "temp_rn_cohort_evening", "temp_rn_cohort_night",
                                          "temp_lpn_cohort_morning", "temp_lpn_cohort_evening", "temp_lpn_cohort_night",
                                          "temp_cna_cohort_morning", "temp_cna_cohort_evening", "temp_cna_cohort_night",
                                          "temp_ma_cohort_morning", "temp_ma_cohort_evening", "temp_admin_cohort_morning",
                                          "temp_admin_cohort_evening", "t", "shift"))] = 
        df_time[,!(colnames(df_time) %in% c("rn_cohort_morning", "rn_cohort_evening", "rn_cohort_night",
                                            "lpn_cohort_morning", "lpn_cohort_evening", "lpn_cohort_night",
                                            "cna_cohort_morning", "cna_cohort_evening", "cna_cohort_night",
                                            "ma_cohort_morning", "ma_cohort_evening", "admin_cohort_morning",
                                            "admin_cohort_evening", 
                                            "temp_rn_cohort_morning", "temp_rn_cohort_evening", "temp_rn_cohort_night",
                                            "temp_lpn_cohort_morning", "temp_lpn_cohort_evening", "temp_lpn_cohort_night",
                                            "temp_cna_cohort_morning", "temp_cna_cohort_evening", "temp_cna_cohort_night",
                                            "temp_ma_cohort_morning", "temp_ma_cohort_evening", "temp_admin_cohort_morning",
                                            "temp_admin_cohort_evening", "t", "shift"))]
    }else{
      df[df$t==t+1, !(colnames(df) %in% c("rn_cohort_morning", "rn_cohort_evening", "rn_cohort_night",
                                          "lpn_cohort_morning", "lpn_cohort_evening", "lpn_cohort_night",
                                          "cna_cohort_morning", "cna_cohort_evening", "cna_cohort_night",
                                          "ma_cohort_morning", "ma_cohort_evening", "admin_cohort_morning",
                                          "admin_cohort_evening", "t", "shift"))] = 
        df_time[,!(colnames(df_time) %in% c("rn_cohort_morning", "rn_cohort_evening", "rn_cohort_night",
                                            "lpn_cohort_morning", "lpn_cohort_evening", "lpn_cohort_night",
                                            "cna_cohort_morning", "cna_cohort_evening", "cna_cohort_night",
                                            "ma_cohort_morning", "ma_cohort_evening", "admin_cohort_morning",
                                            "admin_cohort_evening", "t", "shift"))]
    }
    
    # clear up memory space
    rm(df_time)
    gc(reset=TRUE)
    
  }
  return(df[df$t==(time+14),])
}


start = make_NH(synthpop = synthpop, cohorting = T, visitors = T, temp_staff = F)
nh = initialize_NH(p_asymp_nonres = 0.5, p_asymp_res = 0.5, p_subclin_nonres = 0, p_subclin_res = 0,
                   daily_attack = 0.18, staff_vax_req = F, 
                   res_boost = 0, staff_boost = 0, visit_boost = 0,
                   staff_trans_red = 0, visit_trans_red = 0, res_trans_red = 0, 
                   staff_susp_red = 0, visit_susp_red = 0, res_susp_red = 0, 
                   isolate = T, 
                   prim_previnf_eff = 0.4, boost_eff = 0.7, days_inf = 5,
                   days_isolate_res = 10, days_isolate_staff = 7, days_isolate_visit = 5, start)
sched = make_schedule(time = 45, nh = nh)
cohorts = make_room(df = sched)



#' Run model multiple times and summarize results
#'
#' @param N number of runs
#' @param rel_trans_common Relative attack rate of common area contact (vs. room); defaults to 1/4
#' @param rel_trans_staff Relative attack rate of staff-staff interactions (vs. room); defaults to 1/4
#' @param p_asymp_nonres Fraction of staff with asymptomatic (unsuspected) disease; defaults to 0.8
#' @param p_asymp_res Fraction of residents with asymptomatic (unsuspected) disease; defaults to 0.4
#' @param p_subclin_nonres Fraction of non-residents with subclinical but not techincally asymptomatic disease; defaults to 0
#' @param p_subclin_res Fraction of residents with subclinical but not techincally asymptomatic disease; defaults to 0
#' @param daily_attack Average daily attack rate in residents; defaults to 0.18
#' @param res_boost Proportion of residents with booster dose; defaults to some amount
#' @param staff_vax_req Whether staff are required to get most updated vaccine; defaults to F
#' @param staff_boost Proportion of staff with booster dose; defaults to some amount
#' @param visit_boost Proportion of visitors with booster dose; defaults to some amount
#' @param staff_trans_red Factor by which staff transmissibility is reduced due to intervention; defaults to 1
#' @param visit_trans_red Factor by which visitor transmissibility is reduced due to intervention; defaults to 1
#' @param res_trans_red Factor by which resident transmissibility is reduced due to intervention; defaults to 1
#' @param staff_susp_red Factor by which staff susceptibility is reduced due to intervention; defaults to 1
#' @param visit_susp_red Factor by which visitor susceptibility is reduced due to intervention; defaults to 1
#' @param res_susp_red Factor by which resident susceptibility is reduced due to intervention; defaults to 1
#' @param n_contact_common_res number of contacts a staff/resident has with other residents in common area; defaults to 3
#' @param n_contact_common_staff number of contacts a staff/resident has with other staff in common area; defaults to 3
#' @param n_contact_staff number of contacts a staff member has with other staff members; defaults to 10
#' @param n_start number of infections to seed model; defaults to 1
#' @param days_inf length of infectious period, defaults to 3
#' @param days_isolate_res length of isolation for residents, defaults to 10
#' @param days_isolate_staff length of isolation for staff, defaults to 7
#' @param days_isolate_visit length of isolation for visitors, defaults to 5
#' @param seed_asymp whether to seed with an asymptomatic case
#' @param isolate Whether symptomatic individuals isolate when symptoms emerge; defaults to T
#' @param time length of time to run model; defaults to 30
#' @param test whether there is weekly testing; defaults to F
#' @param test_sens test sensitivity; defaults to 0.7
#' @param test_frac fraction of school tested; defaults to 0.9
#' @param test_days test frequency; "day", "week", "2x_week"; defaults to "week"
#' @param test_type group tested; defaults to "all", also allows "staff" and "residents"
#' @param test_start_day day tests are implemented for weekly testing; defaults to 1 = Monday morning
#' @param visit_test where visitors test before entering NH
#' @param start_mult value to indicate relative frequency of adult/child infections; defaults to 1 (adults 2x as likely as kids)
#' @param staff_prob set daily probability of infectious entry for staff, defaults to .001
#' @param visit_prob set daily probability of infectious entry for visitors, defaults to .001
#' @param quarantine whether people quarantine upon exposure; defaults to T
#' @param quarantine.length length of quarantine when someone is infectious; defaults to 5
#' @param prim_previnf_eff efficacy of 2 doses or previous infection
#' @param boost_eff booster efficacy
#' @param test_recovered whether or not to test recovered individuals
#' @param sched nursing home population schedule; from make_schedule()
#' @param cohorts list from make_room()
#'
#' @export
mult_runs = function(N, rel_trans_common = 1/4, rel_trans_staff = 1/4, 
                     p_asymp_nonres = 0.5, p_asymp_res = 0.5, 
                     p_subclin_nonres = 0, p_subclin_res = 0, daily_attack = 0.18, staff_vax_req = F, 
                     res_boost = 0, staff_boost = 0, visit_boost = 0,
                     staff_trans_red = 0, visit_trans_red = 0, res_trans_red = 0, 
                     staff_susp_red = 0, visit_susp_red = 0, res_susp_red = 0, n_contact_common_res = 3, n_contact_common_staff = 3,
                     n_contact_staff = 6, n_start = 1, days_inf = 5, days_isolate_res = 10,
                     days_isolate_staff = 7, days_isolate_visit = 5, seed_asymp = F, 
                     isolate = T, time = 30, test = F, test_sens = 0.84, test_frac = 0.9, test_days = 'week', 
                     test_type = 'all', test_start_day = 7, visit_test = F, start_mult = 1, staff_prob = 0.005, visit_prob = 0.005,
                     quarantine = F, quarantine.length = 7, prim_previnf_eff = 0.4, boost_eff = 0.7, test_recovered = T, sched, cohorts){
  
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
                   test_frac = test_frac, test_start_day = test_start_day, visit_test = visit_test, n_contact_common_res = n_contact_common_res, 
                   n_contact_common_staff = n_contact_common_staff, n_contact_staff = n_contact_staff,
                   n_start = n_start, days_inf = days_inf, days_isolate_res = days_isolate_res,
                   days_isolate_staff = days_isolate_staff, days_isolate_visit = days_isolate_visit,
                   seed_asymp = seed_asymp, start_mult = start_mult, 
                   staff_prob = staff_prob, visit_prob = visit_prob, quarantine = quarantine, quarantine.length = quarantine.length,
                   rel_trans_common = rel_trans_common, rel_trans_staff = rel_trans_staff, test_type = test_type, test_recovered = test_recovered,
                   df = sched, cohorts = cohorts)
    
    time_keep = df$start.time[1]
    #print(time_keep)
    #print(length(time_keep:(time_keep+time-1)))
    
    # store output
    keep$all[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1)
    keep$tot[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & !df$id%in%c(df$id[df$start==T]))
    
    keep$all_15[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & df$t_end_inf_home >= 15)
    keep$tot_15[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & !df$id%in%c(df$id[df$start==T]) & df$t_end_inf_home >= 15)
    keep$detected_15[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==15)
    
    keep$com_1_res[i] = sum(df$type==0 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 1 & df$t_inf>=-6 & df$id%in%c(df$id[df$start.init==T]))
    keep$nh_1_res[i] = sum(df$type==0 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 1 & df$t_inf>=-6 & !df$id%in%c(df$id[df$start.init==T]))
    
    keep$com_1_staff[i] = sum(df$type==1 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 1 & df$t_inf>=-6 & df$id%in%c(df$id[df$start.init==T]))
    keep$nh_1_staff[i] = sum(df$type==1 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 1 & df$t_inf>=-6 & !df$id%in%c(df$id[df$start.init==T]))
    
    keep$com_8_res[i] = sum(df$type==0 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 8 & df$t_inf>=1 & df$id%in%c(df$id[df$start.init==T]))
    keep$nh_8_res[i] = sum(df$type==0 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 8 & df$t_inf>=1 & !df$id%in%c(df$id[df$start.init==T]))
    
    keep$com_8_staff[i] = sum(df$type==1 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 8 & df$t_inf>=1 & df$id%in%c(df$id[df$start.init==T]))
    keep$nh_8_staff[i] = sum(df$type==1 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 8 & df$t_inf>=1 & !df$id%in%c(df$id[df$start.init==T]))
    
    keep$com_15_res[i] = sum(df$type==0 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 15 & df$t_inf>=8 & df$id%in%c(df$id[df$start.init==T]))
    keep$nh_15_res[i] = sum(df$type==0 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 15 & df$t_inf>=8 & !df$id%in%c(df$id[df$start.init==T]))
    
    keep$com_15_staff[i] = sum(df$type==1 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 15 & df$t_inf>=8 & df$id%in%c(df$id[df$start.init==T]))
    keep$nh_15_staff[i] = sum(df$type==1 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 15 & df$t_inf>=8 & !df$id%in%c(df$id[df$start.init==T]))
    
    keep$all_22[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & df$t_end_inf_home >= 22)
    keep$tot_22[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & !df$id%in%c(df$id[df$start.init==T]) & df$t_end_inf_home >= 22)
    keep$detected_22[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==22)
    
    keep$com_22_res[i] = sum(df$type==0 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 22 & df$t_inf >= 15 & df$id%in%c(df$id[df$start.init==T]))
    keep$nh_22_res[i] = sum(df$type==0 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 22 & df$t_inf >= 15 & !df$id%in%c(df$id[df$start.init==T]))
    
    keep$com_22_staff[i] = sum(df$type==1 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 22 & df$t_inf >= 15 & df$id%in%c(df$id[df$start.init==T]))
    keep$nh_22_staff[i] = sum(df$type==1 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 22 & df$t_inf >= 15 & !df$id%in%c(df$id[df$start.init==T]))
    
    keep$all_29[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & df$t_end_inf_home >= 29)
    keep$tot_29[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & !df$id%in%c(df$id[df$start.init==T]) & df$t_end_inf_home >= 29)
    keep$detected_29[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==29)
    
    keep$com_29_res[i] = sum(df$type==0 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 29 & df$t_inf >= 22 & df$id%in%c(df$id[df$start.init==T]))
    keep$nh_29_res[i] = sum(df$type==0 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 29 & df$t_inf >= 22 & !df$id%in%c(df$id[df$start.init==T]))
    
    keep$com_29_staff[i] = sum(df$type==1 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 29 & df$t_inf >= 22 & df$id%in%c(df$id[df$start.init==T]))
    keep$nh_29_staff[i] = sum(df$type==1 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 29 & df$t_inf >= 22 & !df$id%in%c(df$id[df$start.init==T]))
    
    keep$all_36[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & df$t_end_inf_home >= 36)
    keep$tot_36[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & !df$id%in%c(df$id[df$start.init==T]) & df$t_end_inf_home >= 36)
    keep$detected_36[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==36)
    
    keep$com_36_res[i] = sum(df$type==0 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 36 & df$t_inf >= 29 & df$id%in%c(df$id[df$start.init==T]))
    keep$nh_36_res[i] = sum(df$type==0 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 36 & df$t_inf >= 29 & !df$id%in%c(df$id[df$start.init==T]))
    
    keep$com_36_staff[i] = sum(df$type==1 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 36 & df$t_inf >= 29 & df$id%in%c(df$id[df$start.init==T]))
    keep$nh_36_staff[i] = sum(df$type==1 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 36 & df$t_inf >= 29 & !df$id%in%c(df$id[df$start.init==T]))
    
    keep$all_43[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & df$t_end_inf_home >= 43)
    keep$tot_43[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & !df$id%in%c(df$id[df$start.init==T]) & df$t_end_inf_home >= 43)
    keep$detected_43[i] = sum(df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==43)
    
    keep$com_43_res[i] = sum(df$type==0 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 43 & df$t_inf >= 36 & df$id%in%c(df$id[df$start.init==T]))
    keep$nh_43_res[i] = sum(df$type==0 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 43 & df$t_inf >= 36 & !df$id%in%c(df$id[df$start.init==T]))
    
    keep$com_43_staff[i] = sum(df$type==1 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 43 & df$t_inf >= 36 & df$id%in%c(df$id[df$start.init==T]))
    keep$nh_43_staff[i] = sum(df$type==1 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 43 & df$t_inf >= 36 & !df$id%in%c(df$id[df$start.init==T]))
    
    if("family"%in%colnames(df)){
      keep$com_1_visit[i] = sum(df$type==2 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 1 & df$t_inf>=-6 & df$id%in%c(df$id[df$start.init==T]))
      keep$nh_1_visit[i] = sum(df$type==2 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 1 & df$t_inf>=-6 & !df$id%in%c(df$id[df$start.init==T]))
      
      keep$com_8_visit[i] = sum(df$type==2 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 8 & df$t_inf>=1 & df$id%in%c(df$id[df$start.init==T]))
      keep$nh_8_visit[i] = sum(df$type==2 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 8 & df$t_inf>=1 & !df$id%in%c(df$id[df$start.init==T]))
      
      keep$com_15_visit[i] = sum(df$type==2 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 15 & df$t_inf>=8 & df$id%in%c(df$id[df$start.init==T]))
      keep$nh_15_visit[i] = sum(df$type==2 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 15 & df$t_inf>=8 & !df$id%in%c(df$id[df$start.init==T]))
      
      keep$com_22_visit[i] = sum(df$type==2 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 22 & df$t_inf >= 15 & df$id%in%c(df$id[df$start.init==T]))
      keep$nh_22_visit[i] = sum(df$type==2 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 22 & df$t_inf >= 15 & !df$id%in%c(df$id[df$start.init==T]))
      
      keep$com_29_visit[i] = sum(df$type==2 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 29 & df$t_inf >= 22 & df$id%in%c(df$id[df$start.init==T]))
      keep$nh_29_visit[i] = sum(df$type==2 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 29 & df$t_inf >= 22 & !df$id%in%c(df$id[df$start.init==T]))
      
      keep$com_36_visit[i] = sum(df$type==2 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 36 & df$t_inf >= 29 & df$id%in%c(df$id[df$start.init==T]))
      keep$nh_36_visit[i] = sum(df$type==2 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 36 & df$t_inf >= 29 & !df$id%in%c(df$id[df$start.init==T]))
      
      keep$com_43_visit[i] = sum(df$type==2 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 43 & df$t_inf >= 36 & df$id%in%c(df$id[df$start.init==T]))
      keep$nh_43_visit[i] = sum(df$type==2 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 43 & df$t_inf >= 36 & !df$id%in%c(df$id[df$start.init==T]))
    }
    
    if("temp_rn_cohort_morning"%in%colnames(df)){
      keep$com_1_temp_staff[i] = sum(df$type==3 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 1 & df$t_inf>=-6 & df$id%in%c(df$id[df$start.init==T]))
      keep$nh_1_temp_staff[i] = sum(df$type==3 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 1 & df$t_inf>=-6 & !df$id%in%c(df$id[df$start.init==T]))
      
      keep$com_8_temp_staff[i] = sum(df$type==3 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 8 & df$t_inf>=1 & df$id%in%c(df$id[df$start.init==T]))
      keep$nh_8_temp_staff[i] = sum(df$type==3 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 8 & df$t_inf>=1 & !df$id%in%c(df$id[df$start.init==T]))
      
      keep$com_15_temp_staff[i] = sum(df$type==3 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 15 & df$t_inf>=8 & df$id%in%c(df$id[df$start.init==T]))
      keep$nh_15_temp_staff[i] = sum(df$type==3 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 15 & df$t_inf>=8 & !df$id%in%c(df$id[df$start.init==T]))
      
      keep$com_22_temp_staff[i] = sum(df$type==3 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 22 & df$t_inf >= 15 & df$id%in%c(df$id[df$start.init==T]))
      keep$nh_22_temp_staff[i] = sum(df$type==3 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 22 & df$t_inf >= 15 & !df$id%in%c(df$id[df$start.init==T]))
      
      keep$com_29_temp_staff[i] = sum(df$type==3 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 29 & df$t_inf >= 22 & df$id%in%c(df$id[df$start.init==T]))
      keep$nh_29_temp_staff[i] = sum(df$type==3 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 29 & df$t_inf >= 22 & !df$id%in%c(df$id[df$start.init==T]))
      
      keep$com_36_temp_staff[i] = sum(df$type==3 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 36 & df$t_inf >= 29 & df$id%in%c(df$id[df$start.init==T]))
      keep$nh_36_temp_staff[i] = sum(df$type==3 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 36 & df$t_inf >= 29 & !df$id%in%c(df$id[df$start.init==T]))
      
      keep$com_43_temp_staff[i] = sum(df$type==3 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 43 & df$t_inf >= 36 & df$id%in%c(df$id[df$start.init==T]))
      keep$nh_43_temp_staff[i] = sum(df$type==3 & df$t_inf!=-1 & df$t_end_inf_home>=time_keep & df$t_inf < 43 & df$t_inf >= 36 & !df$id%in%c(df$id[df$start.init==T]))
    }
    
    keep$R0[i] = sum(df$tot_inf[df$start==T])
    keep$Rt[i] =  mean(df$tot_inf[df$t_inf!=0 & df$t_end_inf_home>=time_keep], na.rm = T)
    keep$avg_infs[i] = mean(df$tot_inf[df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$start==T])
    keep$start[i] = sum(df$start==T & df$t_end_inf_home>=time_keep & df$t_inf < time_keep+time - 1)
    keep$test_count[i] = sum(df$test_ct)
    keep$test_count_res[i] = sum(df$test_ct_res)
    keep$test_tp_count[i] <- sum(df$test_tp_count)
    keep$test_fn_count[i] <- sum(df$test_fn_count)
    keep$test_eligible[i] <- sum(df$test_eligible)
    keep$detected[i] = sum(df$detected)
    keep$detected_staff[i] = sum(df$detected[(df$type==1 | df$type==3)])
    keep$detected_res[i] = sum(df$detected[df$type==0])
    keep$detected_visit[i] = sum(df$detected[df$type==2])
    keep$detected_staff_subclin[i] = sum(df$detected[(df$type==1 | df$type==3) & df$sub_clin], na.rm = T)
    keep$detected_res_subclin[i] = sum(df$detected[df$type==0 & df$sub_clin], na.rm = T)
    keep$detected_visit_subclin[i] = sum(df$detected[df$type==2 & df$sub_clin], na.rm = T)
    keep$quarantine_check[i] = max(df$t_end_inf-df$t_end_inf_home, na.rm = T)#(df$uh.oh[1])
    # keep$avg_class[i] = unlist(df %>% filter(t_exposed!=-99 & t_exposed <= time_keep + time - 1 & class!=99) %>% group_by(class) %>%
    # summarize(num = length(class)) %>% ungroup() %>% summarize(mean(num, na.rm = T)))
    keep$isolated[i] = sum(df$isolated)
    keep$quarantined[i] = sum(df$quarantined)
    keep$quarantined_res[i] = sum(df$quarantined[df$type==0])
    keep$quarantined_staff[i] = sum(df$quarantined[(df$type==1 | df$type==3)])
    keep$quarantined_visit[i] = sum(df$quarantined[df$type==2])
    keep$start_staff[i] = sum((df$type==1 | df$type==3) & df$start==T & df$t_end_inf_home>=time_keep & df$t_inf < time_keep+time - 1)
    keep$start_visit[i] = sum(df$type==2 & df$start==T & df$t_end_inf_home>=time_keep & df$t_inf < time_keep+time - 1)
    keep$start_res[i] = sum(df$type==0 & df$start==T & df$t_end_inf_home>=time_keep & df$t_inf < time_keep+time - 1)
    keep$start_symp[i] = sum(df$symp==1 & df$start==T & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$source_asymp[i] = sum(!df$source_symp & df$t_inf <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$id%in%c(df$id[df$start==T]), na.rm = T)
    keep$source_asymp_visit[i] = sum(df$type==2 & !df$source_symp & df$t_inf <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$id%in%c(df$id[df$start==T]), na.rm = T)
    keep$staff_all[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & (df$type==1 | df$type==3) & df$t_inf <= time_keep + time - 1)
    keep$res_all[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$type==0 & df$t_inf <= time_keep + time - 1)
    keep$visit_all[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$type==2 & df$t_inf <= time_keep + time - 1)
    keep$staff_tot[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & (df$type==1 | df$type==3) & df$t_inf <= time_keep + time - 1 & !df$id%in%c(df$id[df$start==T]))
    keep$res_tot[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$type==0 & df$t_inf <= time_keep + time - 1 & !df$id%in%c(df$id[df$start==T]))
    keep$visit_tot[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$type==2 & df$t_inf <= time_keep + time - 1 & !df$id%in%c(df$id[df$start==T]))
    keep$symp[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$symp==1 & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$symp_res[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$symp==1 & df$t_inf <= time_keep + time - 1 & df$type==0, na.rm = T)
    keep$asymp_res[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$symp==0 & df$t_inf <= time_keep + time - 1 & df$type==0, na.rm = T)
    keep$symp_staff[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$symp==1 & df$t_inf <= time_keep + time - 1 & (df$type==1 | df$type==3), na.rm = T)
    keep$asymp_staff[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$symp==0 & df$t_inf <= time_keep + time - 1 & (df$type==1 | df$type==3), na.rm = T)
    keep$sick_at_end[i] = sum(df$t_inf<=time_keep + time - 1 & df$t_end_inf > time_keep + time - 1)
    keep$room[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Room")
    keep$common[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Common area")
    keep$staff_interactions[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Staff interactions")
    keep$num_room[i] = length(unique(df$room[df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1]))
    keep$clin_staff[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & (df$type==1 | df$type==3) & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$clin_res[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$clin_visit[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$type==2 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$clin_staff2[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & (df$type==1 | df$type==3) & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$clin_res2[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$type==0 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$clin_visit2[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$type==2 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$notify_staff[i] = sum(df$t_notify>=15 & (df$type==1 | df$type==3) & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$notify_res[i] = sum(df$t_notify>=15 & df$type==0 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$notify_visit[i] = sum(df$t_notify>=15 & df$type==2 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$notify_staff2[i] = sum(df$t_notify>=15 & (df$type==1 | df$type==3) & df$t_notify <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$notify_res2[i] = sum(df$t_notify>=15 & df$type==0 & df$t_notify <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$notify_visit2[i] = sum(df$t_notify>=15 & df$type==2 & df$t_notify <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    
    
    #John's new checks
    keep$inf_ct_sympR_R_room[i] = sum(df$location == "Room" & df$type==0 & df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_asympR_R_room[i] = sum(df$location == "Room" & df$type==0 & !df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    if(quarantine){
      keep$inf_ct_sympR_S_room_quarantine[i] = sum(df$location == "Room" & (df$type==1 | df$type==3) & df$source_symp & df$inf_quarantine & df$source %in% df$id[df$type==0], na.rm = TRUE)
      keep$inf_ct_asympR_S_room_quarantine[i] = sum(df$location == "Room" & (df$type==1 | df$type==3) & !df$source_symp & df$inf_quarantine & df$source %in% df$id[df$type==0], na.rm = TRUE)
      keep$inf_ct_sympS_R_room_quarantine[i] = sum(df$location == "Room" & df$type==0 & df$source_symp & df$inf_quarantine & df$source %in% df$id[(df$type==1 | df$type==3)], na.rm = TRUE)
      keep$inf_ct_asympS_R_room_quarantine[i] = sum(df$location == "Room" & df$type==0 & !df$source_symp & df$inf_quarantine & df$source %in% df$id[(df$type==1 | df$type==3)], na.rm = TRUE)
    }
    keep$inf_ct_sympR_S_room[i] = sum(df$location == "Room" & (df$type==1 | df$type==3) & df$source_symp & !df$inf_quarantine & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_asympR_S_room[i] = sum(df$location == "Room" & (df$type==1 | df$type==3) & !df$source_symp & !df$inf_quarantine & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_sympS_R_room[i] = sum(df$location == "Room" & df$type==0 & df$source_symp & !df$inf_quarantine & df$source %in% df$id[(df$type==1 | df$type==3)], na.rm = TRUE)
    keep$inf_ct_asympS_R_room[i] = sum(df$location == "Room" & df$type==0 & !df$source_symp & !df$inf_quarantine & df$source %in% df$id[(df$type==1 | df$type==3)], na.rm = TRUE)
    
    keep$inf_ct_sympR_V_room[i] = sum(df$location == "Room" & df$type==2 & df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_asympR_V_room[i] = sum(df$location == "Room" & df$type==2 & !df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_sympV_R_room[i] = sum(df$location == "Room" & df$type==0 & df$source_symp & df$source %in% df$id[df$type==2], na.rm = TRUE)
    keep$inf_ct_asympV_R_room[i] = sum(df$location == "Room" & df$type==0 & !df$source_symp & df$source %in% df$id[df$type==2], na.rm = TRUE)
    
    keep$inf_ct_sympR_R_common[i] = sum(df$location == "Common area" & df$type==0 & df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_asympR_R_common[i] = sum(df$location == "Common area" & df$type==0 & !df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_sympR_S_common[i] = sum(df$location == "Common area" & (df$type==1 | df$type==3) & df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_asympR_S_common[i] = sum(df$location == "Common area" & (df$type==1 | df$type==3) & !df$source_symp & df$source %in% df$id[df$type==0], na.rm = TRUE)
    keep$inf_ct_sympS_S_common[i] = sum(df$location == "Common area" & (df$type==1 | df$type==3) & df$source_symp & df$source %in% df$id[(df$type==1 | df$type==3)], na.rm = TRUE)
    keep$inf_ct_asympS_S_common[i] = sum(df$location == "Common area" & (df$type==1 | df$type==3) & !df$source_symp & df$source %in% df$id[(df$type==1 | df$type==3)], na.rm = TRUE)
    keep$inf_ct_sympS_R_common[i] = sum(df$location == "Common area" & df$type==0 & df$source_symp & df$source %in% df$id[(df$type==1 | df$type==3)], na.rm = TRUE)
    keep$inf_ct_asympS_R_common[i] = sum(df$location == "Common area" & df$type==0 & !df$source_symp & df$source %in% df$id[(df$type==1 | df$type==3)], na.rm = TRUE)
    
    keep$inf_ct_sympS_S_staff[i] = sum(df$location == "Staff interactions" & (df$type==1 | df$type==3) & df$source_symp & df$source %in% df$id[(df$type==1 | df$type==3)], na.rm = TRUE)
    keep$inf_ct_asympS_S_staff[i] = sum(df$location == "Staff interactions" & (df$type==1 | df$type==3) & !df$source_symp & df$source %in% df$id[(df$type==1 | df$type==3)], na.rm = TRUE)
    
    keep$risk_ct_sympR_R_room[i] = sum(df$person.days.at.risk.room.res[df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_asympR_R_room[i] = sum(df$person.days.at.risk.room.res[!df$symp & df$type==0], na.rm = TRUE)
    
    if(quarantine){
      keep$risk_ct_sympR_S_room_quarantine[i] = sum(df$person.days.at.risk.room.staff.quarantine[df$symp & df$type==0], na.rm = TRUE)
      keep$risk_ct_asympR_S_room_quarantine[i] = sum(df$person.days.at.risk.room.staff.quarantine[!df$symp & df$type==0], na.rm = TRUE)
      keep$risk_ct_sympS_R_room_quarantine[i] = sum(df$person.days.at.risk.room.res.quarantine[df$symp & (df$type==1 | df$type==3)], na.rm = TRUE)
      keep$risk_ct_asympS_R_room_quarantine[i] = sum(df$person.days.at.risk.room.res.quarantine[!df$symp & (df$type==1 | df$type==3)], na.rm = TRUE)
    }
    keep$risk_ct_sympR_S_room[i] = sum(df$person.days.at.risk.room.staff[df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_asympR_S_room[i] = sum(df$person.days.at.risk.room.staff[!df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_sympS_R_room[i] = sum(df$person.days.at.risk.room.res[df$symp & (df$type==1 | df$type==3)], na.rm = TRUE)
    keep$risk_ct_asympS_R_room[i] = sum(df$person.days.at.risk.room.res[!df$symp & (df$type==1 | df$type==3)], na.rm = TRUE)
    
    keep$risk_ct_sympR_V_room[i] = sum(df$person.days.at.risk.room.visit[df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_asympR_V_room[i] = sum(df$person.days.at.risk.room.visit[!df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_sympV_R_room[i] = sum(df$person.days.at.risk.room.res[df$symp & df$type==2], na.rm = TRUE)
    keep$risk_ct_asympV_R_room[i] = sum(df$person.days.at.risk.room.res[!df$symp & df$type==2], na.rm = TRUE)
    
    keep$risk_ct_sympR_R_common[i] = sum(df$person.days.at.risk.common.res[df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_asympR_R_common[i] = sum(df$person.days.at.risk.common.res[!df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_sympR_S_common[i] = sum(df$person.days.at.risk.common.staff[df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_asympR_S_common[i] = sum(df$person.days.at.risk.common.staff[!df$symp & df$type==0], na.rm = TRUE)
    keep$risk_ct_sympS_S_common[i] = sum(df$person.days.at.risk.common.staff[df$symp & (df$type==1 | df$type==3)], na.rm = TRUE)
    keep$risk_ct_asympS_S_common[i] = sum(df$person.days.at.risk.common.staff[!df$symp & (df$type==1 | df$type==3)], na.rm = TRUE)
    keep$risk_ct_sympS_R_common[i] = sum(df$person.days.at.risk.common.res[df$symp & (df$type==1 | df$type==3)], na.rm = TRUE)
    keep$risk_ct_asympS_R_common[i] = sum(df$person.days.at.risk.common.res[!df$symp & (df$type==1 | df$type==3)], na.rm = TRUE)
    
    keep$risk_ct_sympS_S_staff[i] = sum(df$person.days.at.risk.staff.staff[df$symp & (df$type==1 | df$type==3)], na.rm = TRUE)
    keep$risk_ct_asympS_S_staff[i] = sum(df$person.days.at.risk.staff.staff[!df$symp & (df$type==1 | df$type==3)], na.rm = TRUE)
    
    keep$length.infectious.low_risk_obs[i] = mean(floor(df$t_end_inf_home[df$t_inf > 0 & df$comorbid==0]) - ceiling(df$t_inf[df$t_inf > 0 & df$comorbid==0]) + 1)
    keep$length.infectious.low_risk_sd[i] = sd(floor(df$t_end_inf_home[df$t_inf > 0 & df$comorbid==0]) - ceiling(df$t_inf[df$t_inf > 0 & df$comorbid==0]) + 1)
    keep$length.infectious.mod_risk_obs[i] = mean(floor(df$t_end_inf_home[df$t_inf > 0 & df$comorbid==1]) - ceiling(df$t_inf[df$t_inf > 0 & df$comorbid==1]) + 1)
    keep$length.infectious.mod_risk_sd[i] = sd(floor(df$t_end_inf_home[df$t_inf > 0 & df$comorbid==1]) - ceiling(df$t_inf[df$t_inf > 0 & df$comorbid==1]) + 1)
    keep$length.infectious.high_risk_obs[i] = mean(floor(df$t_end_inf_home[df$t_inf > 0 & df$comorbid==2]) - ceiling(df$t_inf[df$t_inf > 0 & df$comorbid==2]) + 1)
    keep$length.infectious.high_risk_sd[i] = sd(floor(df$t_end_inf_home[df$t_inf > 0 & df$comorbid==2]) - ceiling(df$t_inf[df$t_inf > 0 & df$comorbid==2]) + 1)
    
    keep$length.infectious.symp.low_risk_obs[i] = mean(floor(df$t_end_inf_home[df$symp & !df$sub_clin & df$t_inf > 0 & df$comorbid==0]) - ceiling(df$t_inf[df$symp & !df$sub_clin & df$t_inf > 0 & df$comorbid==0]) + 1)
    keep$length.infectious.asymp.low_risk_obs[i] = mean(floor(df$t_end_inf_home[(!df$symp | df$sub_clin) & df$t_inf > 0 & df$comorbid==0]) - ceiling(df$t_inf[(!df$symp | df$sub_clin) & df$t_inf > 0 & df$comorbid==0]) + 1)
    keep$length.infectious.symp.mod_risk_obs[i] = mean(floor(df$t_end_inf_home[df$symp & !df$sub_clin & df$t_inf > 0 & df$comorbid==1]) - ceiling(df$t_inf[df$symp & !df$sub_clin & df$t_inf > 0 & df$comorbid==1]) + 1)
    keep$length.infectious.asymp.mod_risk_obs[i] = mean(floor(df$t_end_inf_home[(!df$symp | df$sub_clin) & df$t_inf > 0 & df$comorbid==1]) - ceiling(df$t_inf[(!df$symp | df$sub_clin) & df$t_inf > 0 & df$comorbid==1]) + 1)
    keep$length.infectious.symp.high_risk_obs[i] = mean(floor(df$t_end_inf_home[df$symp & !df$sub_clin & df$t_inf > 0 & df$comorbid==2]) - ceiling(df$t_inf[df$symp & !df$sub_clin & df$t_inf > 0 & df$comorbid==2]) + 1)
    keep$length.infectious.asymp.high_risk_obs[i] = mean(floor(df$t_end_inf_home[(!df$symp | df$sub_clin) & df$t_inf > 0 & df$comorbid==2]) - ceiling(df$t_inf[(!df$symp | df$sub_clin) & df$t_inf > 0 & df$comorbid==2]) + 1)
    
    keep$n_res_obs[i] = sum(df$type==0)
    keep$n_dc_staff_obs[i] = sum((df$type==1 | df$type==3) & df$role!=4 & !is.na(df$role))
    keep$n_admin_staff_obs[i] = sum((df$type==1 | df$type==3) & df$role==4 & !is.na(df$role))
    keep$n_visit_obs[i] = sum(df$type==2)
    
    keep$p_asymp_res_obs[i] = sum(!df$symp & df$t_exposed > 0 & df$type==0)/sum(df$t_exposed > 0 & df$type==0)
    keep$p_asymp_nonres_obs[i] = sum(!df$symp & df$t_exposed > 0 & df$type!=0)/sum(df$t_exposed > 0 & df$type!=0)
    keep$p_subclin_res_obs[i] = sum(df$sub_clin & df$t_exposed > 0 & df$type==0)/sum(df$t_exposed > 0 & df$type==0)
    keep$p_subclin_nonres_obs[i] = sum(df$sub_clin & df$t_exposed > 0 & df$type!=0)/sum(df$t_exposed > 0 & df$type!=0)
    
    keep$length.incubation_obs[i] = mean(floor(df$t_inf[df$t_inf > 0 & !df$start]) - ceiling(df$t_exposed[df$t_inf > 0 & !df$start] + runif(length(df$t_exposed[df$t_inf > 0 & !df$start]), min = -0.5, max = 0.5)) + 1)
    keep$length.symp.gap_obs[i] = mean(floor(df$t_symp[df$t_inf > 0 & !df$start]) - ceiling(df$t_exposed[df$t_inf > 0 & !df$start] + runif(length(df$t_exposed[df$t_inf > 0 & !df$start]), min = -0.5, max = 0.5)) + 1)
    keep$length.quarantine_obs[i] = mean(floor(df$t_end_quarantine[df$t_quarantine > 0 & df$t_quarantine!=-13 & df$t_end_quarantine > 0 & df$t_end_quarantine!=-13]) - ceiling(df$t_quarantine[df$t_quarantine > 0 & df$t_quarantine!=-13 & df$t_end_quarantine > 0 & df$t_end_quarantine!=-13]))
    
    keep$length.isolation.res_obs[i] = mean(floor(df$t_end_isolate[df$type==0 & df$t_inf > 0 & df$t_end_isolate > 0]) - ceiling(df$t_notify[df$type==0 & df$t_inf > 0 & df$t_end_isolate > 0]))
    keep$length.isolation.staff_obs[i] = mean(floor(df$t_end_isolate[(df$type==1 | df$type==3) & df$t_inf > 0 & df$t_end_isolate > 0]) - ceiling(df$t_notify[(df$type==1 | df$type==3) & df$t_inf > 0 & df$t_end_isolate > 0]))
    keep$length.isolation.visit_obs[i] = mean(floor(df$t_end_isolate[df$type==2 & df$t_inf > 0 & df$t_end_isolate > 0]) - ceiling(df$t_notify[df$type==2 & df$t_inf > 0 & df$t_end_isolate > 0]))
    
    keep$days.isolation.res_obs[i] = sum(df$t_end_isolate[df$type==0 & df$t_inf > 0 & df$t_end_isolate > 0] - df$t_notify[df$type==0 & df$t_inf > 0 & df$t_end_isolate > 0])
    keep$days.isolation.staff_obs[i] = sum(df$t_end_isolate[df$type==1 & df$t_inf > 0 & df$t_end_isolate > 0] - df$t_notify[df$type==1 & df$t_inf > 0 & df$t_end_isolate > 0])
    keep$days.isolation.visit_obs[i] = sum(df$t_end_isolate[df$type==2 & df$t_inf > 0 & df$t_end_isolate > 0] - df$t_notify[df$type==2 & df$t_inf > 0 & df$t_end_isolate > 0])
    
    keep$res.prim.previnf.rate_obs[i] = mean(df$vacc[df$type==0]==0)
    keep$staff.prim.previnf.rate_obs[i] = mean(df$vacc[(df$type==1 | df$type==3)]==0)
    keep$visit.prim.previnf.rate_obs[i] = mean(df$vacc[df$type==2]==0)
    keep$res.boost.rate_obs[i] = mean(df$vacc[df$type==0]==1)
    keep$staff.boost.rate_obs[i] = mean(df$vacc[(df$type==1 | df$type==3)]==1)
    keep$visit.boost.rate_obs[i] = mean(df$vacc[df$type==2]==1)
    
    if("family"%in%colnames(df)){
      keep$prim.previnf.eff_obs[i] = (ifelse(is.na(mean(df$susp[df$type==0 & df$vacc==0] == 0)),
                                             sum(df$type==0 & df$vacc==0),
                                             mean(df$susp[df$type==0 & df$vacc==0] == 0))*sum(df$type==0 & df$vacc==0) +
                                        ifelse(is.na(mean(df$susp[(df$type==1 | df$type==3) & df$vacc==0] == 0)),
                                               sum((df$type==1 | df$type==3) & df$vacc==0),
                                               mean(df$susp[(df$type==1 | df$type==3) & df$vacc==0] == 0))*sum((df$type==1 | df$type==3) & df$vacc==0) +
                                        ifelse(is.na(mean(df$susp[df$type==2 & df$vacc==0] == 0)),
                                               sum(df$type==2 & df$vacc==0),
                                               mean(df$susp[df$type==2 & df$vacc==0] == 0))*sum((df$type==2 & df$vacc==0)))/
        (sum(df$type==0 & df$vacc==0) + sum((df$type==1 | df$type==3) & df$vacc==0) + sum(df$type==2 & df$vacc==0))
      keep$boost.eff_obs[i] = (ifelse(is.na(mean(df$susp[df$type==0 & df$vacc==1] == 0)),
                                      sum(df$type==0 & df$vacc==1),
                                      mean(df$susp[df$type==0 & df$vacc==1] == 0))*sum(df$type==0 & df$vacc==1) +
                                 ifelse(is.na(mean(df$susp[(df$type==1 | df$type==3) & df$vacc==1] == 0)),
                                        sum((df$type==1 | df$type==3) & df$vacc==1),
                                        mean(df$susp[(df$type==1 | df$type==3) & df$vacc==1] == 0))*sum((df$type==1 | df$type==3) & df$vacc==1) +
                                 ifelse(is.na(mean(df$susp[df$type==2 & df$vacc==1] == 0)),
                                        sum(df$type==2 & df$vacc==1),
                                        mean(df$susp[df$type==2 & df$vacc==1] == 0))*sum((df$type==2 & df$vacc==1)))/
        (sum(df$type==0 & df$vacc==1) + sum((df$type==1 | df$type==3) & df$vacc==1) + sum(df$type==2 & df$vacc==1))
    } else{
      keep$prim.previnf.eff_obs[i] = (ifelse(is.na(mean(df$susp[df$type==0 & df$vacc==0] == 0)),
                                             sum(df$type==0 & df$vacc==0),
                                             mean(df$susp[df$type==0 & df$vacc==0] == 0))*sum(df$type==0 & df$vacc==0) +
                                        ifelse(is.na(mean(df$susp[(df$type==1 | df$type==3) & df$vacc==0] == 0)),
                                               sum((df$type==1 | df$type==3) & df$vacc==0),
                                               mean(df$susp[(df$type==1 | df$type==3) & df$vacc==0] == 0))*sum((df$type==1 | df$type==3) & df$vacc==0))/
        (sum(df$type==0 & df$vacc==0) + sum((df$type==1 | df$type==3) & df$vacc==0))
      keep$boost.eff_obs[i] = (ifelse(is.na(mean(df$susp[df$type==0 & df$vacc==1] == 0)),
                                      sum(df$type==0 & df$vacc==1),
                                      mean(df$susp[df$type==0 & df$vacc==1] == 0))*sum(df$type==0 & df$vacc==1) +
                                 ifelse(is.na(mean(df$susp[(df$type==1 | df$type==3) & df$vacc==1] == 0)),
                                        sum((df$type==1 | df$type==3) & df$vacc==1),
                                        mean(df$susp[(df$type==1 | df$type==3) & df$vacc==1] == 0))*sum((df$type==1 | df$type==3) & df$vacc==1))/
        (sum(df$type==0 & df$vacc==1) + sum((df$type==1 | df$type==3) & df$vacc==1))
    }
    
    
    keep$res.prob_obs[i] = mean(sapply(df$start.time[1]:(time + df$start.time[1] - 1), function(t){sum(ceiling(df$t_inf[df$start & df$type==0]) == t)/sum(df$type==0)}))
    keep$staff.prob_obs[i] = mean(sapply(df$start.time[1]:(time + df$start.time[1] - 1), function(t){sum(ceiling(df$t_inf[df$start & (df$type==1 | df$type==3)]) == t)/sum(df$type==1 | df$type==3)}))
    keep$visit.prob_obs[i] = mean(sapply(df$start.time[1]:(time + df$start.time[1] - 1), function(t){sum(ceiling(df$t_inf[df$start & df$type==2]) == t)/sum(df$type==2)}))
    
    
    # Alyssa's new checks
    keep$seed_res[i] = sum(df$start.init[df$type==0])
    keep$seed_nonres[i] = sum(df$start.init[df$type!=0])
    keep$start_res_time[i] = mean(df$t_inf[df$start.init & df$type==0])
    keep$start_nonres_time[i] = mean(df$t_inf[df$start.init & df$type!=0])
    keep$not_inf_start[i] = sum(df$not_inf_keep)
    keep$test_type.check[i] = sum(df$test_type)
    keep$prim_previnf[i] = sum(df$vacc==0)
    keep$boosted[i] = sum(df$vacc==1)
    #print(i)
    
  }
  
  #toc()
  
  return(keep)
}
