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


#' Synthetic Rhode Island nursing home population
#'
#' A data frame containing a synthetic population of nursing home residents
#' representative of nursing homes in Rhode Island.
#' This is used by make_NH().
#'
#' @docType data
#' @format A data frame with
#' \describe{
#'  \item{id}{individual id #}
#'  \item{type}{0: resident, 1: staff}
#'  \item{age}{age}
#'  \item{role}{if staff; 0: RN, 1: LPN, 2: CNA, 3: medication aide/technician, 4: admin/support (non-direct care) staff}
#'  \item{sex}{0: male, 1: female}
#'  \item{private_room}{true if resident has private room}
#' }
#'
#' @usage data(synthpop_NH)
#'
#' @keywords datasets
#'
#' @source
#'
"synthpop"


#' Structure nursing home and staff/visitor-patient relationships
#'
#' This function sorts nursing home residents into rooms (42 doubles, 36 singles), adds staffing shifts,
#' option for cohorting among staff and residents, and allows/assigns visitors to residents.
#'
#' @param synthpop synthetic population; defaults to synthpop_NH stored in file
#' @param cohorting assign staff to residents; defaults to F
#' @param visitors allow visitors; defaults to F
#'
#' @return out data frame of structured nursing home
#'
#' @export
make_NH = function(synthpop, cohorting = FALSE, visitors = FALSE){
  
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
  others = data.frame(subset(synthpop, (private_room == TRUE & type == 0) | type != 0))
  others$room = 0   # 0 indicates a resident with no roommate
  others[others$type == 1,]$room = 99   # 99 indicates staff
  
  # bind doubles and others data frames
  rooms = doubles %>% bind_rows(others) %>%
    arrange(type, room)
  
  # ------------------------------------------------------------------------------------------------
  
  # assign shifts to direct care staff
  rn_cohort_morning = subset(rooms, type == 1 & role == 0)[1:5,] %>% mutate(rn_cohort_morning = 1:5)
  rn_cohort_evening = subset(rooms, type == 1 & role == 0)[6:9,] %>% mutate(rn_cohort_evening = 6:9) 
  rn_cohort_night = subset(rooms, type == 1 & role == 0)[10:12,] %>% mutate(rn_cohort_night = 10:12)
  rn = rn_cohort_morning %>% bind_rows(rn_cohort_evening) %>% bind_rows(rn_cohort_night)
  
  lpn_cohort_morning = subset(rooms, type == 1 & role == 1)[1:4,] %>% mutate(lpn_cohort_morning = 1:4) 
  lpn_cohort_evening = subset(rooms, type == 1 & role == 1)[5:7,] %>% mutate(lpn_cohort_evening = 5:7)
  lpn_cohort_night = subset(rooms, type == 1 & role == 1)[8:9,] %>% mutate(lpn_cohort_night = 8:9)
  lpn = lpn_cohort_morning %>% bind_rows(lpn_cohort_evening) %>% bind_rows(lpn_cohort_night)
  
  cna_cohort_morning = subset(rooms, type == 1 & role == 2)[1:15,] %>% mutate(cna_cohort_morning = 1:15)  
  cna_cohort_evening = subset(rooms, type == 1 & role == 2)[16:27,] %>% mutate(cna_cohort_evening = 16:27)
  cna_cohort_night = subset(rooms, type == 1 & role == 2)[28:37,] %>% mutate(cna_cohort_night = 28:37)
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
    
  }
  
  # bind residents, staff, and visitors into dataframe
  out = residents %>% bind_rows(rn) %>% bind_rows(lpn) %>% bind_rows(cna) %>% 
    bind_rows(med_aide) %>% bind_rows(admin)
  
  # ------------------------------------------------------------------------------------------------
  
  # add visitors and assign to residents with family column
  if(visitors){
    # visitors represented by type = 2
    visitors = data.frame(id = (nrow(synthpop)+1):(nrow(synthpop)+nrow(residents)), type = 2, 
                          family = 1:nrow(residents))
    out['family'] = ifelse(out$type == 0, 1:nrow(residents), NA)
    
    out = out %>% bind_rows(visitors)
  }
  
  return(out)
  
}



#' Initialize nursing home
#'
#' This function takes in a data frame exported by make_NH().
#' It adds epidemiological attributes of the full nursing home community.

#' @param n_contacts Number of sustained contacts outside of the nursing home 
#' (applies only to staff and visitors); defaults to 10
#' @param n_contacts_brief Number of brief contacts outside of the nursing home; defaults to 0
#' @param rel_trans_common Relative attack rate of common area contact (vs. room); defaults to 1 (used to be rel_trans_HH)
#' @param rel_trans_room_symp_res Additional relative attack rate of a symptomatic infected resident in shared room; 
#' defaults to 1 (used to be rel_trans_HH_symp_child)
#' @param rel_trans Relative attack rate of sustained contact (vs. resident room); defaults to 1/8
#' @param rel_trans_brief Relative attack rate of brief contact (vs. resident room); defaults to 1/50
#' @param p_asymp_staff Fraction of staff with asymptomatic disease; defaults to 0.4 (used to be p_asymp_adult)
#' @param p_asymp_res Fraction of residents with asymptomatic disease; defaults to 0.8 (used to be p_asymp_child)
#' @param p_subclin_staff Fraction of staff with subclinical but not techincally asymptomatic disease; 
#' defaults to 0 (used to be p_subclin_adult)
#' @param p_subclin_res Fraction of residents with subclinical but not techincally asymptomatic disease; 
#' defaults to 0 (used to be p_subclin_child)
#' @param attack Average daily attack rate in residents; defaults to 0.01
#' @param rel_nonres_trans Relative transmissibility of staff and visitors (vs. residents); defaults to 1 (used to be child_trans)
#' @param rel_nonres_susp Relative susceptibility of staff and visitors (vs. residents); defaults to .5 (used to be child_susp)
#' @param res_vax Vaccination rate of residents; defaults to some amount (used to be child_vax)
#' @param staff_vax_req Whether staff are required to get vaccine; defaults to F
#' @param visit_vax Vaccination rate of visitors; defaults to some amount (new addition)
#' @param res_trans_red Factor by which resident transmissibility is reduced due to intervention; defaults to 1
#' (new addition)
#' @param staff_trans_red Factor by which staff transmissibility is reduced due to intervention; defaults to 1
#' (used to be teacher_trans)
#' @param visit_trans_red Factor by which visitor transmissibility is reduced due to intervention; defaults to 1
#' (used to be family_susp)
#' @param disperse_transmission Whether transmission is overdispersed (vs. all have equal attack rate); default to T
#' @param isolate Whether symptomatic individuals isolate when symptoms emerge; defaults to T
#' @param notify Whether nursing homes are notified following a positive test; defaults to T
#' @param dedens Whether dedensification measures reduce attack rate; defaults to F
#' @param vax_eff Vaccine efficacy, defaults to 0.9
#' @param start Data frame from make_NH()
#'
#' @return out data frame of resident and staff attributes.
#'
#' @export
initialize_NH = function(n_contacts = 10, n_contacts_brief = 0, rel_trans_common = 1, rel_trans_room_symp_res = 1,
                             rel_trans = 1/8, rel_trans_brief = 1/50, p_asymp_staff = .35, p_asymp_res = .7, 
                              p_subclin_staff = 0, p_subclin_res = 0, attack = .01, rel_nonres_trans = 1, 
                            rel_nonres_susp = .5, res_vax = 0, staff_vax_req = F, visit_vax = 0, res_trans_red = 1, 
                            res_susp_red = 1, staff_trans_red = 1, staff_susp_red = 1, visit_trans_red = 1, 
                            visit_susp_red = 1, disperse_transmission = T, isolate = T, notify = T, dedens = T,
                            vax_eff = .9, start){
  
  n = nrow(start)
  
  # vax values for staff
  if(staff_vax_req == F){
    staff_vax = 0.5 # hypothetical
  } else{
    staff_vax = 1
  }
  
  # initialize data frame
  df = start %>%
    mutate(start = F,
           start.init = F,
           t_exposed = -99,
           t_inf = -1,
           symp = NA,
           sub_clin = NA,
           t_symp = -1,
           t_end_inf = -1,
           t_end_inf_home = -1,
           t_notify = -17,
           c_trace = -1,
           c_trace_start = -1,
           tot_inf = 0,
           detected = 0,
           detected_q = 0,
           detected_q_start = 0,
           quarantined = 0,
           quarantined2 = 0,
           test_ct = 0,
           test_ct_q = 0,
           n_contact = n_contacts,
           n_contact_brief = n_contacts_brief,
           relative_trans = rel_trans,
           relative_trans_common = rel_trans_common,
           relative_trans_room_symp_res = ifelse(type != 0, 1, rel_trans_room_symp_res),
           relative_trans_brief = rel_trans_brief,
           attack_rate = attack,
           dedens = dedens,
           source = 0,
           source_symp = NA,
           tot_inf = 0,
           super_spread = disperse_transmission,
           out = 0,
           location = "",
           
           # trackers for unit testing
           person.days.at.risk.home.parents = 0,
           person.days.at.risk.home.students = 0,
           person.days.at.risk.class.students = 0,
           person.days.at.risk.class.teachers = 0,
           person.days.at.risk.random.students = 0,
           person.days.at.risk.random.teachers = 0,
           person.days.at.risk.random.staff = 0,
           person.days.at.risk.specials.kids = 0,
           person.days.at.risk.specials.teachers = 0,
           person.days.at.risk.care.students = 0,
           person.days.at.risk.care.parents = 0,
           inf_days = 0,
           inf_home_days = 0,
           symp_days = 0,
           symp_and_inf_days = 0,
           last = 0,
           rapid_tp_count  = 0,
           rapid_fn_count = 0,
           pcr_tp_count = 0,
           pcr_fn_count = 0,
           test_q_eligible = 0,
           test_regular_eligible = 0
    ) %>%
    mutate(p_asymp = ifelse(type != 0, p_asymp_staff, p_asymp_res),
           p_subclin = ifelse(type != 0, p_subclin_staff, p_subclin_res),
           
           # isolation
           isolate = rbinom(n(), size = 1, prob = isolate),
           notify = notify,
           
           # transmission probability
           room_trans_prob = attack, # used to be class_trans_prob
           room_trans_prob = ifelse(type == 0, room_trans_prob*res_trans_red, rel_nonres_trans*room_trans_prob),
           room_trans_prob = dedens*room_trans_prob,
           room_trans_prob = ifelse(type == 1, room_trans_prob*staff_trans_red, room_trans_prob),
           room_trans_prob = ifelse(type == 2, room_trans_prob*visit_trans_red, room_trans_prob),
           
           # susceptibility
           rel_nonres_susp_val = rel_nonres_susp, # used to be child_susp_val
           res_vax_val = res_vax, # used to be child_vax_val
           staff_vax_val = staff_vax,
           visit_vax_val = visit_vax,
           vax_eff_val = vax_eff,
           
           vacc = ifelse(type != 0, 1, rbinom(n, size = 1, prob = res_vax_val)),
           vacc = ifelse(type == 1, rbinom(n, size = 1, prob = staff_vax_val), vacc),
           vacc = ifelse(type == 2, rbinom(n, size = 1, prob = visit_vax_val), vacc),
           
           susp = ifelse(vacc==0, 1, rbinom(n, size = 1, prob = 1-vax_eff_val)),
           susp = ifelse(type != 0, rel_nonres_susp_val*susp, susp))
  
  return(df)
}


#' Make schedule
#'
#' Make a schedule of when staff and visitors are present/absent
#'
#' @param time number of days; defaults to 30
#' @param df data frame from make_NH()
#'
#' @return d Returns an n x time*3 data frame that indicates whether an individual is in the 
#' nursing home at a particular time
#'
#' @export
make_schedule = function(time = 30, df){
  
  # basic time vector
  vec = data.frame(
    
    # time since start in 8-hour shifts
    t = rep(1:time, each = 3),
    
    # day of the week
    day = rep(c("M_morn", "M_evening", "M_night", 
                "T_morn", "T_evening", "T_night", 
                "W_morn", "W_evening", "W_night", 
                "Th_morn", "Th_evening", "Th_night", 
                "F_morn", "F_evening", "F_night", 
                "Sa_morn", "Sa_evening", "Sa_night", 
                "Su_morn", "Su_evening", "Su_night"), length.out = time*3)
    
  )
  
  # replicate for each person
  vec_exp = vec %>% slice(rep(1:n(), times = nrow(df))) %>% mutate(id = rep(1:nrow(df), each = time*3))
  
  # ------------------------------------------------------------------------------------------------
  
  # time matrix for residents, staff, visitors
  if("family" %in% colnames(df)){
    d = df %>% select(id, type, role, room, family, rn_cohort_morning, rn_cohort_evening, rn_cohort_night,
                      lpn_cohort_morning, lpn_cohort_evening, lpn_cohort_night, cna_cohort_morning, 
                      cna_cohort_evening, cna_cohort_night, ma_cohort_morning, ma_cohort_evening,
                      admin_cohort_morning, admin_cohort_evening) %>% left_join(vec_exp, "id") %>%
      
      # mark staff and residents present based on time of day
      mutate(present = ifelse(grepl("morn", day) & (!is.na(rn_cohort_morning) | !is.na(lpn_cohort_morning) | 
                                                      !is.na(cna_cohort_morning) | !is.na(ma_cohort_morning) | 
                                                      !is.na(admin_cohort_morning)), TRUE, FALSE),
             present = ifelse(grepl("evening", day) & (!is.na(rn_cohort_evening) | !is.na(lpn_cohort_evening) | 
                                                         !is.na(cna_cohort_evening) | !is.na(ma_cohort_evening) | 
                                                         !is.na(admin_cohort_evening)), TRUE, present),
             present = ifelse(grepl("night", day) & (!is.na(rn_cohort_night) | !is.na(lpn_cohort_night) | 
                                                       !is.na(cna_cohort_night)), TRUE, present),
             present = ifelse(type == 0, TRUE, present),
             
      )
    
    # mark visitors present in the mornings, rotating throughout the week
    # each visitor comes 4-5 times a month
    visitor_sched = d$present[d$type == 2]
    i = 1
    while(i < (nrow(subset(d, type == 2)) - time*3)){
      for(j in seq(from=1, to=time*3, by=21)){
        visitor_sched[i+j-1] = TRUE
      }
      i = i + time*3 + 3
    }
    d$present[d$type == 2] = visitor_sched
  }
  
  # ------------------------------------------------------------------------------------------------
  
  # time matrix for just residents and staff
  else{
    d = df %>% select(id, type, role, room, rn_cohort_morning, rn_cohort_evening, rn_cohort_night,
                      lpn_cohort_morning, lpn_cohort_evening, lpn_cohort_night, cna_cohort_morning, 
                      cna_cohort_evening, cna_cohort_night, ma_cohort_morning, ma_cohort_evening,
                      admin_cohort_morning, admin_cohort_evening) %>% left_join(vec_exp, "id") %>%
      
      # mark staff and residents present based on time of day
      mutate(present = ifelse(grepl("morn", day) & (!is.na(rn_cohort_morning) | !is.na(lpn_cohort_morning) | 
                                                      !is.na(cna_cohort_morning) | !is.na(ma_cohort_morning) | 
                                                      !is.na(admin_cohort_morning)), TRUE, FALSE),
             present = ifelse(grepl("evening", day) & (!is.na(rn_cohort_evening) | !is.na(lpn_cohort_evening) | 
                                                      !is.na(cna_cohort_evening) | !is.na(ma_cohort_evening) | 
                                                      !is.na(admin_cohort_evening)), TRUE, present),
             present = ifelse(grepl("night", day) & (!is.na(rn_cohort_night) | !is.na(lpn_cohort_night) | 
                                                    !is.na(cna_cohort_night)), TRUE, present),
             present = ifelse(type == 0, TRUE, present)
             
      )
  }
  
  # assign staff to residents and randomize each day if no cohorting
  if(is.na(df$rn_cohort_morning[df$id==1])){
    rn_morning = df[!is.na(df$rn_cohort_morning),]$rn_cohort_morning
    rn_evening = df[!is.na(df$rn_cohort_evening),]$rn_cohort_evening
    rn_night = df[!is.na(df$rn_cohort_night),]$rn_cohort_night
    lpn_morning = df[!is.na(df$lpn_cohort_morning),]$lpn_cohort_morning
    lpn_evening = df[!is.na(df$lpn_cohort_evening),]$lpn_cohort_evening
    lpn_night = df[!is.na(df$lpn_cohort_night),]$lpn_cohort_night
    cna_morning = df[!is.na(df$cna_cohort_morning),]$cna_cohort_morning
    cna_evening = df[!is.na(df$cna_cohort_evening),]$cna_cohort_evening
    cna_night = df[!is.na(df$cna_cohort_night),]$cna_cohort_night
    ma_morning = df[!is.na(df$ma_cohort_morning),]$ma_cohort_morning
    ma_evening = df[!is.na(df$ma_cohort_evening),]$ma_cohort_evening

    for(i in 1:time){
      res = 1
      for(j in rn_morning){
        d$rn_cohort_morning[d$id %in% res:(res+(nrow(subset(df,type==0))/length(rn_morning))-1) 
                            & d$type==0 & d$t==i] = sample(rn_morning,1)
        res = res + (nrow(subset(df,type==0))/length(rn_morning))
      }
      res = 1
      for(j in rn_evening){
        d$rn_cohort_evening[d$id %in% res:(res+(nrow(subset(df,type==0))/length(rn_evening))-1) 
                            & d$type==0 & d$t==i] = sample(rn_evening,1)
        res = res + (nrow(subset(df,type==0))/length(rn_evening))
      }
      res = 1
      for(j in rn_night){
        d$rn_cohort_night[d$id %in% res:(res+(nrow(subset(df,type==0))/length(rn_night))-1) 
                            & d$type==0 & d$t==i] = sample(rn_night,1)
        res = res + (nrow(subset(df,type==0))/length(rn_night))
      }
      res = 1
      for(j in lpn_morning){
        d$lpn_cohort_morning[d$id %in% res:(res+(nrow(subset(df,type==0))/length(lpn_morning))-1) 
                          & d$type==0 & d$t==i] = sample(lpn_morning,1)
        res = res + (nrow(subset(df,type==0))/length(lpn_morning))
      }
      res = 1
      for(j in lpn_evening){
        d$lpn_cohort_evening[d$id %in% res:(res+(nrow(subset(df,type==0))/length(lpn_evening))-1) 
                             & d$type==0 & d$t==i] = sample(lpn_evening,1)
        res = res + (nrow(subset(df,type==0))/length(lpn_evening))
      }
      res = 1
      for(j in lpn_night){
        d$lpn_cohort_night[d$id %in% res:(res+(nrow(subset(df,type==0))/length(lpn_night))-1) 
                             & d$type==0 & d$t==i] = sample(lpn_night,1)
        res = res + (nrow(subset(df,type==0))/length(lpn_night))
      }
      res = 1
      for(j in cna_morning){
        d$cna_cohort_morning[d$id %in% res:(res+(nrow(subset(df,type==0))/length(cna_morning))-1) 
                             & d$type==0 & d$t==i] = sample(cna_morning,1)
        res = res + (nrow(subset(df,type==0))/length(cna_morning))
      }
      res = 1
      for(j in cna_evening){
        d$cna_cohort_evening[d$id %in% res:(res+(nrow(subset(df,type==0))/length(cna_evening))-1) 
                             & d$type==0 & d$t==i] = sample(cna_evening,1)
        res = res + (nrow(subset(df,type==0))/length(cna_evening))
      }
      res = 1
      for(j in cna_night){
        d$cna_cohort_night[d$id %in% res:(res+(nrow(subset(df,type==0))/length(cna_night))-1) 
                             & d$type==0 & d$t==i] = sample(cna_night,1)
        res = res + (nrow(subset(df,type==0))/length(cna_night))
      }
      res = 1
      for(j in ma_morning){
        d$ma_cohort_morning[d$id %in% res:(res+(nrow(subset(df,type==0))/length(ma_morning))-1) 
                           & d$type==0 & d$t==i] = sample(ma_morning,1)
        res = res + (nrow(subset(df,type==0))/length(ma_morning))
      }
    }
    d$ma_cohort_evening[d$type==0] = 3

  }
  
  return(d)
  
}


#' Set room transmission (used to be rn_household)
#'
#' Determine who is infected at a timestep
#' in the same room as an infected individual
#'
#' @param a id of infected individual
#' @param df data frame from run_model()
#'
#' @return infs id of infected individuals
#'
#' @export
run_room = function(a, df){
  
  # if infected is resident
  if(df$type[df$id==a]==0){
    
    # if resident has roommate
    if(df$room[df$id==a]>0 & sum(df$room==df$room[df$id==a])>1){
      # roommate
      roommate_vec = df[df$room==df$room[df$id==a] & df$id!=a,]
      roommate_vec = roommate_vec[rowSums(is.na(roommate_vec)) != ncol(roommate_vec),]
      
      # determine whether roommate becomes infected
      prob_roommate = rbinom(nrow(roommate_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*roommate_vec$susp*roommate_vec$present_susp*roommate_vec$not_inf < 1,
                                                                 df$room_trans_prob[df$id==a]*roommate_vec$susp*roommate_vec$present_susp*roommate_vec$not_inf,
                                                                 1))
      roommate = roommate_vec$id
      
      # list infected roommate
      roommate_inf = roommate*prob_roommate
    }
    
    else{roommate_inf = 0}
    
    # make vector of staff in infected resident's room
    staff_vec = df[df$rn_cohort_morning==df$rn_cohort_morning[df$id==a] & df$type==1,] %>% 
      bind_rows(df[df$rn_cohort_evening==df$rn_cohort_evening[df$id==a] & df$type==1,]) %>% 
      bind_rows(df[df$rn_cohort_night==df$rn_cohort_night[df$id==a] & df$type==1,]) %>% 
      bind_rows(df[df$lpn_cohort_morning==df$lpn_cohort_morning[df$id==a] & df$type==1,]) %>% 
      bind_rows(df[df$lpn_cohort_evening==df$lpn_cohort_evening[df$id==a] & df$type==1,]) %>%
      bind_rows(df[df$lpn_cohort_night==df$lpn_cohort_night[df$id==a] & df$type==1,]) %>% 
      bind_rows(df[df$cna_cohort_morning==df$cna_cohort_morning[df$id==a] & df$type==1,]) %>% 
      bind_rows(df[df$cna_cohort_evening==df$cna_cohort_evening[df$id==a] & df$type==1,]) %>% 
      bind_rows(df[df$cna_cohort_night==df$cna_cohort_night[df$id==a] & df$type==1,]) %>% 
      bind_rows(df[df$ma_cohort_morning==df$ma_cohort_morning[df$id==a] & df$type==1,]) %>% 
      bind_rows(df[df$ma_cohort_evening==df$ma_cohort_evening[df$id==a] & df$type==1,])
    staff_vec = staff_vec[rowSums(is.na(staff_vec)) != ncol(staff_vec),]
    
    # determine whether staff becomes infected
    prob_staff = rbinom(nrow(staff_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*staff_vec$susp*staff_vec$present_susp*staff_vec$not_inf < 1,
                                                                df$room_trans_prob[df$id==a]*staff_vec$susp*staff_vec$present_susp*staff_vec$not_inf,
                                                                1))
    staff = staff_vec$id
    
    # list infected staff
    staff_infs = staff*prob_staff
    
    
    # make vector of resident's visitor(s)
    visit_vec = df[df$family==df$family[df$id==a] & df$id!=a,]
    visit_vec = visit_vec[rowSums(is.na(visit_vec)) != ncol(visit_vec),]
    
    # determine whether visitors become infected
    prob_visit = rbinom(nrow(visit_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*visit_vec$susp*visit_vec$present_susp*visit_vec$not_inf < 1,
                                                                df$room_trans_prob[df$id==a]*visit_vec$susp*visit_vec$present_susp*visit_vec$not_inf,
                                                                1))
    visit = visit_vec$id
    
    # list infected visitors
    visit_infs = visit*prob_visit
    
  }
  else{
    roommate_inf = 0
    staff_inf = 0
    visit_inf = 0
  }
  
  infs = c(roommate_inf, staff_inf, visit_inf)
  
  # if infected is direct-care staff
  if(df$type[df$id==a]==1 & df$role[df$id==a]!=4 & df$present[df$id==a]){
    
    # find out what role they are
    staff_row = c(df[df$id==a,8:18])
    staff_role = names(staff_row[!is.na(staff_row)])
    staff_role_id = staff_row[!is.na(staff_row)][[1]]
    
    # make vector of residents that staff treats
    res_vec = df[df[staff_role]==staff_role_id & df$type==0,]
    res_vec = res_vec[rowSums(is.na(res_vec)) != ncol(res_vec),]
    
    # determine whether residents become infected
    prob_res = rbinom(nrow(res_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp*res_vec$not_inf < 1,
                                                                df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp*res_vec$not_inf,
                                                                1))
    res = res_vec$id
    
    # list infected residents
    res_infs = res*prob_res
    
  }
  else{
    res_infs = 0
  }
  
  infs = c(infs, res_infs)
  
  # if infected is visitor
  if(df$type[df$id==a]==2 & df$present[df$id==a]){
    
    # make vector of resident that visitor sees, including roommates
    res_vec = df[df$family==df$family[df$id==a] & df$id!=a,]
    res_vec = res_vec[rowSums(is.na(res_vec)) != ncol(res_vec),]
    res_id = res_vec$id
    roommate = df[df$room==res_vec$room & df$id!=res_id,]
    res_vec[nrow(res_vec) + 1,] = roommate[rowSums(is.na(roommate)) != ncol(roommate),]
    
    # determine whether residents become infected
    prob_res = rbinom(nrow(res_vec), size = 1, prob = ifelse(df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp*res_vec$not_inf < 1,
                                                             df$room_trans_prob[df$id==a]*res_vec$susp*res_vec$present_susp*res_vec$not_inf,
                                                             1))
    res = res_vec$id
    
    # list infected residents
    res_infs = res*prob_res
  }
  else{
    res_infs = 0
  }
  
  infs = c(infs, res_infs)
 
  
  #print(df$class_trans_prob[df$id==a]*df$relative_trans_HH[df$id==a]*HH_vec$susp*HH_vec$not_inf)
  return(infs)
}


#' Set common area transmission (used to be run_rand)
#'
#' Determine who is infected at a timestep
#' from common area contact with an infected individual
#'
#' @param a id of infected individual
#' @param df data frame from make_schedule()?
#' @param area_contacts graph of common area contacts at time t
#'
#' @return infs id of infected individuals
#'
#' @export
run_common = function(a, df, area_contacts){
  
  # pull contacts from random graph (of residents, staff and visitors present at NH)
  id = which(df$id[df$present]==a)
  contact_id = df$id[df$present][area_contacts[[id]][[1]]]
  #print(length(contact_id))
  contacts = df[df$id %in% contact_id,]
  
  # determine whether a contact becomes infected
  prob_common = rbinom(nrow(contacts), size = 1,
                     prob = ifelse(df$relative_trans_common[df$id==a]*df$relative_trans[df$id==a]*contacts$susp*contacts$present_susp*contacts$not_inf < 1,
                                   df$relative_trans_common[df$id==a]*df$relative_trans[df$id==a]*contacts$susp*contacts$present_susp*contacts$not_inf,
                                   1))
  
  # infected individuals
  infs = contacts$id*prob_common
  #print(a); print(infs)
  return(infs)
}



#' Set staff transmission (used to be run_staff_rand)
#'
#' Determine who is infected at a timestep
#' from contact between in-nursing-home staff
#'
#' @param a id of infected staff member
#' @param df school data frame from initialize_school()
#' @param n_contact number of contacts staff encounters in nursing home
#' @param rel_trans_staff relative transmission in staff-staff interactions (vs. resident room);
#' defaults to 2 (look into this), used to be rel_trans_adult
#'
#' @return infs id of infected individuals
#'
#' @export
run_staff = function(a, df, n_contact, rel_trans_staff = 2){
  
  if(n_contact>0){
    # pull contacts from random graph of staff present in nursing home
    tot = length(df$id[df$present & df[df$type == 1,]])
    contact_take = ifelse(n_contact<=tot, n_contact, tot)
    contact_id = sample(df$id[df$present & df[df$type == 1,]], contact_take)
    contacts = df[df$id %in% contact_id & df$id!=a,]
    id.susp = contacts[contacts$present_susp & contacts$susp != 0,]$id
    #print(dim(contacts))
    
    # determine whether a contact becomes infected
    prob_staff = rbinom(nrow(contacts), size = 1,
                       prob = ifelse(df$room_trans_prob[df$id==a]*df$relative_trans[df$id==a]*contacts$susp*contacts$present_susp*rel_trans_staff*contacts$not_inf < 1,
                                     df$room_trans_prob[df$id==a]*df$relative_trans[df$id==a]*contacts$susp*contacts$present_susp*rel_trans_staff*contacts$not_inf,
                                     1))
    # infected individuals
    infs = contacts$id*prob_staff
    
    return(list(infs, id.susp))
  }
  else{
    return(0)
  } 
}



#' Update quarantine
#'
#' Mark classes for quarantine based on current symptomatic infections
#'
#' @param class_quarantine data frame of quarantine times
#' @param df.u data frame of infections whose classes should be quarantined
#'
#' @return class_quarantine updated
#'
#' @export
make_quarantine = function(class_quarantine, df.u, quarantine.length = 10, quarantine.grace = 3, hs = F, hs.classes = NA){
  
  if(!hs){
    # note classes to potentially be quarantined
    for(k in 1:nrow(class_quarantine)){
      hold = class_quarantine$hold[k]
      class_quarantine$hold[k] = ifelse(class_quarantine$class_group[k]%in%paste(df.u$class, df.u$group) | paste(class_quarantine$class[k], "99")%in%paste(df.u$class, df.u$group),  
                                        max(df.u$t_notify[df.u$class==class_quarantine$class[k]]), class_quarantine$hold[k])
    }
    
    # quarantine if class has been back for more than grace period of days after previous quarantine
    class_quarantine$t_notify = ifelse(class_quarantine$hold > class_quarantine$t_notify + quarantine.length + quarantine.grace,
                                       class_quarantine$hold, class_quarantine$t_notify)
    class_quarantine$num = ifelse(class_quarantine$hold > class_quarantine$t_notify, class_quarantine$num + 1, class_quarantine$num)
  }else{
    
    hs.classes2 = hs.classes %>% inner_join(df.u %>% select(id, t_notify), c("id" = "id"))
    for(k in 1:nrow(class_quarantine)){
      hold = class_quarantine$hold[k]
      #print(hs.classes$t_notify[hs.classes$class==class_quarantine$class[k]])
      
      class_quarantine$hold[k] = ifelse(class_quarantine$class[k]%in%hs.classes$class[hs.classes$id%in%df.u$id],
                                        max(hs.classes2$t_notify[hs.classes2$class==class_quarantine$class[k]]), class_quarantine$hold[k])
    }
    
    # quarantine if class has been back for more than grace period of days after previous quarantine
    class_quarantine$t_notify = ifelse(class_quarantine$hold > class_quarantine$t_notify + quarantine.length + quarantine.grace,
                                       class_quarantine$hold, class_quarantine$t_notify)
  }
  return(class_quarantine)
}



#' Set infection parameters
#'
#' Set infection parameters for individuals infected at a particular timestep
#'
#' @param df.u data frame from initialize_NH()?
#' @param days_inf length of infectious period, defaults to 5
#' @param set indication of seeding model vs. creating infections
#' @param mult_asymp_res multiplier on asymptomatic infection for residents; default is 1 (used to be mult_asymp)
#' @param mult_asymp_nonres multiplier on asymptomatic infection for staff and visitors; default is 1 (used to be mult_asymp_child)
#' @param seed_asymp when making a seed, force to be asymptomatic; default is false
#' @param turnaround.time test turnaround time, default = 1 day
#' @param overdisp_off all overdispersion off; defaults to F
#'
#' @return df.u with updated parameters
#'
#' @export
# note to self -- add additional parameters to change around here
make_infected = function(df.u, days_inf, set = NA, mult_asymp_res = 1, mult_asymp_nonres = 1, seed_asymp = F, turnaround.time = 1, overdisp_off = F){
  
  if(is.na(set)[1]){
    #  set infectivity  parameters
    df.u$symp = rbinom(nrow(df.u), size = 1, prob = 1-df.u$p_asymp)
    df.u$sub_clin = ifelse(df.u$symp, rbinom(nrow(df.u), size = 1, prob =  df.u$p_subclin/(1-df.u$p_asymp)), 1)
    df.u$t_symp = df.u$t_exposed + rgamma(nrow(df.u), shape = 5.8, scale=.95)
    val = rnorm(nrow(df.u), mean = 2, sd = .4)
    df.u$t_inf = ifelse(df.u$t_symp - val > df.u$t_exposed + 1, df.u$t_symp - val,
                        df.u$t_exposed + 1)
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
    df.u$t_symp = df.u$t_inf + rnorm(nrow(df.u), mean = 2, sd = .4)
    symp_gap <- rgamma(nrow(df.u), shape = 5.8, scale=.95)
    df.u$t_exposed = ifelse(df.u$t_symp - symp_gap < df.u$t_inf - 1, df.u$t_symp - symp_gap, df.u$t_inf - 1)
  }
  
  # add overdispersion
  attack_mult = rlnorm(nrow(df.u), meanlog = log(.84)-log((.84^2+.3)/.84^2)/2, sdlog = sqrt(log((.84^2+.3)/.84^2)))/.84
  chk = (df.u$super_spread | df.u$type==0)*as.numeric(!overdisp_off)
  df.u$room_trans_prob = ifelse(chk, df.u$room_trans_prob*attack_mult, df.u$room_trans_prob)
  
  # adjust for asymptomatic infection if applicable
  df.u$room_trans_prob = ifelse(!df.u$symp, ifelse(df.u$type==0, df.u$room_trans_prob*mult_asymp_res, df.u$room_trans_prob*mult_asymp_nonres), df.u$room_trans_prob)
  df.u$relative_trans_common = ifelse(df.u$symp & df.u$isolate==0, df.u$relative_trans_common*df.u$relative_trans_room_symp_res, df.u$relative_trans_common)
  
  # add end time
  df.u$t_end_inf_home = df.u$t_inf +
    rlnorm(nrow(df.u), meanlog = log(days_inf)-log((days_inf^2 + 2)/days_inf^2)/2, sdlog = sqrt(log((days_inf^2 + 2)/days_inf^2)))
  df.u$t_end_inf = ifelse(df.u$symp==1 & !df.u$sub_clin & df.u$isolate & df.u$t_symp<df.u$t_end_inf_home, df.u$t_symp, df.u$t_end_inf_home)
  df.u$t_notify = ifelse(df.u$symp==1 & !df.u$sub_clin & df.u$isolate & df.u$notify, df.u$t_symp + turnaround.time, -17)
  
  return(df.u)
}



#' Run model
#'
#' Perform a single model run
#'
#' @param time length of time to run model; defaults to 30
#' @param notify whether classrooms (residents?) are notified and quarantined; defaults to F
#' @param test whether there is weekly testing; defaults to F
#' @param test_sens test sensitivity; defaults to 0.7
#' @param test_frac fraction of nursing home tested; defaults to 0.9
#' @param test_days test frequency; "day", "week", "2x_week"; defaults to "week"
#' @param test_type group tested; defaults to "all", also allows "residents" and "staff"
#' @param test_start_day day tests are implemented for weekly testing; defaults to 1 = Monday
#' @param n_staff_contact number of contacts a staff member has with other staff members; defaults to 1
#' @param n_HH number of households a household interacts with when not attending school; defaults to 0 
#' (change to number of people staff members interact with when not working?)
#' @param bubble whether out-of-school interactions occur with a 'bubble'; defaults to F (out-of-NH interactions of staff?)
#' @param n_start number of infections to seed model; defaults to 1
#' @param mult_asymp_res multiplier on asymptomatic infection for residents; default is 1 (used to be mult_asymp)
#' @param mult_asymp_nonres multiplier on asymptomatic infection for staff and visitors; default is 1 (used to be mult_asymp_child)
#' @param days_inf length of infectious period (assuming mild case or quarantined on symptoms)
#' @param seed_asymp whether to seed with an asymptomatic case
#' @param time_seed_inf time(s) at which to introduce new infectious individuals; defaults to NA and randomly selects one time
#' @param start_type type of seed; default is "mix" (also "resident", "staff", "visitor", "cont)
#' @param quarantine.length length of quarantine when someone is infectious; defaults to 10
#' @param quarantine.grace length of grace period after which a quarantined class (residnet/staff?) returns not to be "re-quarantined"
#' @param start_mult value to indicate relative frequency of resident/staff infections; defaults to 0.5 - used to default to 1 
#' (staff 2x as likely as residents since residents don't leave) (are staff or residents more likely to get infected?)
#' @param num_staff number of staff interacting with residents, defaults to 4 (used to be num_adults)
#' @param include_weekends if TRUE includes weekends in additional out-of-school mixing, defaults to F (remove)
#' @param turnaround.time test turnaround time, default = 1 day
#' @param nonres_prob if start_type = "cont", set daily probability of infectious entry for staff and visitors, defaults to .05 (used to be child_prob)
#' @param res_prob if start_type = "cont", set daily probability of infectious entry for residents, defaults to .01 (used to be adult_prob)
#' @param type "base", "On/off", "A/B", "Remote"; defaults to "base" (remove)
#' @param rel_trans_CC relative transmission in childcare vs. classroom; defaults to 2 (remove bc no care contacts)
#' @param rel_trans_staff relative transmission in staff-staff interactions vs. resident's room; defaults to 2 (used to be rel_trans_adult)
#' @param test_quarantine whether quarantined individuals attend school but are tested daily; defaults to FALSE (remove?)
#' @param surveillance whether surveillance is underway; defaults to F
#' @param rapid_test_sens sensitivity of rapid tests, defaults to 80%
#' @param overdisp_off all overdispersion off; defaults to F
#' @param version v1 quarantines full cohort in A/B; v2 only sub-cohort; defaults to 2 (remove)
#' @param df data frame from make_NH() (initialize_school()?)
#' @param sched schedule data frame from make_schedule()
#'
#' @return df updated df with transmission results
#' @return time_seed_inf when the first individual was dropped in
#' @return class_quarantine a matrix of class quarantine times (change to resident quarantine times?)
#' @return mat a check on if the people who you think are present are actually the ones present
#'
#' @export
#### NOTE: I found this to be slower when coded w/tidyverse.
#### Therefore for the most part, this is coded in base.
run_model = function(time = 30,
                     notify = F,
                     test = F,
                     test_days = "week",
                     test_sens =  .7,
                     test_frac = .9,
                     test_start_day = 1,
                     n_staff_contact = 0,
                     n_HH = 0, # change
                     bubble = F, # change
                     n_start = 1,
                     days_inf = 6,
                     mult_asymp_res = 1,
                     mult_asymp_nonres = 1,
                     seed_asymp = F,
                     time_seed_inf = NA,
                     start_type = "mix",
                     quarantine.length = 10,
                     quarantine.grace = 3,
                     start_mult = 1,
                     num_staff = 2,
                     include_weekends = T, # remove
                     turnaround.time = 1,
                     nonres_prob = 0.056,
                     res_prob = 0.013,
                     type = "base", # remove
                     rel_trans_CC = 2, # remove
                     rel_trans_staff = 2,
                     test_type = "all",
                     test_quarantine = F, # remove
                     surveillance = F,
                     rapid_test_sens = 0.8,
                     overdisp_off = F,
                     version = 2, # remove
                     df, sched){
  
  #### SEED MODEL ####
  # seed with an infectious case
  if(is.na(time_seed_inf)) time_seed_inf = sample(1:14, 1)     # any day in the cycle (?)
  
  # any individual not visitor
  # note staff 2x as likely as residents to be infected
  if(start_type=="mix") id.samp = sample(df$id[!df$type==2], n_start, prob = (df$type[!df$type==2]*start_mult+1)/(sum(df$type[!df$type==2]*(start_mult+1)) + sum(!df$type==2)))                
  
  # specific types
  if(start_type=="resident") id.samp = sample(df$id[df$type==0], n_start)      
  if(start_type=="staff") id.samp = sample(df$id[df$type==1], n_start)              
  if(start_type=="visitor") id.samp = sample(df$id[df$type==2], n_start)
  
  # set up scheduling if high school
  # hs.classes = NA
  # if(high_school){
  #   hs.classes = make_hs_classes(df = df, nper = nper)    
  #   classes.ind = sapply(df$id, function(a) hs.classes$class[hs.classes$id == a])
  # }
  # 
  # # quarantine
  # if(version == 1 | type=="base") df$group[df$group!=99] = 0  # make sure quarantine doesn't go by group
  # if(!high_school){class_quarantine = expand_grid(class = unique(df$class[df$class!=99]), group = unique(df$group[df$group
  #                                                                                                                 !=99])) %>%
  #   mutate(class_group = paste(class, group), t_notify = -quarantine.grace-quarantine.length, hold = -quarantine.grace-quarantine.length, num = 0)
  # }else{class_quarantine = data.frame(class = unique(hs.classes$class), t_notify = -quarantine.grace-quarantine.length, hold = -quarantine.grace-quarantine.length)}
  # mat = matrix(NA, nrow = max(df$id), ncol = time)
  
  # vary over time
  if(start_type == "cont"){
    
    # pull out_IDs
    res_IDs = df$id[df$type==0]
    nonres_IDs = df$id[!df$type==0]
    
    # pick times
    vec = 1:(time+15) # change so that nonres_times < nonres_IDs
    res_pulls = rbinom(time+15, size = length(res_IDs), prob = res_prob)
    res_times = rep(vec, res_pulls)
    
    nonres_pulls = rbinom(time+15, size = length(nonres_IDs), prob = nonres_prob)
    nonres_times = rep(vec, nonres_pulls)
    
    # pick people
    res = sample(res_IDs, length(res_times))
    nonres = sample(nonres_IDs, length(nonres_times))
    
    # set up vectors
    time_seed_inf = c(res_times, nonres_times)
    
    id.samp = c(res, nonres)
    df.temp = data.frame(id.samp, time_seed_inf) %>% arrange(id.samp) %>%
      left_join(df, c("id.samp" = "id")) %>% filter(susp!=0)
    time_seed_inf = 15 # start on Monday with testing
    
    
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
                                                  turnaround.time = turnaround.time, overdisp_off = overdisp_off)
    df$start = df$id %in% df.temp$id.samp
    df$start.init = df$id %in% df.temp$id.samp
    
  }
  
  # test days
  # if null, make this Monday
  if(test_days == "week") {testing_days = seq(test_start_day, (time+15), by = 7)}
  if(test_days == "day") {testing_days = 1:(time+15)}
  if(test_days == "2x_week"){
    if(turnaround.time>1){
      testing_days = c(seq(5, (time+15), by = 7), seq(1, (time+15), by = 7))
    } else{
      testing_days = c(seq(4, (time+15), by = 7), seq(1, (time+15), by = 7))
    }}
  df$switch = 0
  df$temp_switch = 0
  
  #print(testing_days)
  
  # testing
  if(test_type=="all"){ df$test_type = df$type!=2 & (!df$vacc | test_frac>=0.7)
  } else if(test_type=="residents"){df$test_type = df$type==0
  } else if(test_type=="staff"){df$test_type = df$type==1}
  class_test_ind = 0
  class_test_ind_q = 0
  test_frac_orig = test_frac
  df$uh.oh = 0
  
  #print(paste("start notification:", df$t_notify[df$start]))
  # run over time steps
  for(t in time_seed_inf:(time_seed_inf+time-1)){
    #print(paste("Time:", t, sched$day[sched$t==t][1], sched$group_two[sched$t==t][1]))
    
    # class quarantines
    classes_out = class_quarantine[class_quarantine$t_notify > -1 & class_quarantine$t_notify <= t & t <= (class_quarantine$t_notify + quarantine.length-1),]
    df$present = sched$present[sched$t==t]
    df$inf = df$t_inf > -1 & df$t_inf <= t & df$t_end_inf >= t
    df$symp_now = !df$family & !is.na(df$symp) & df$symp==1 & !df$sub_clin & df$t_inf <= t & df$t_end_inf >= t
    #print(paste("Time:", t, sched$day[sched$t==t][1]))
    #print(classes_out)
    df$flag = (paste(df$class, df$group))%in%classes_out$class_group | (df$class%in%classes_out$class & df$group==99)
    
    # present
    if(test_quarantine==T) {
      df$test_type_q = !df$family & df$inc_test & (df$flag | df$symp_now)
      if(t == 1) df$test_q_keep = df$test_type_q
      #print("got to test_q"); print(dim(df)); print(df %>% group_by(vacc) %>% summarize(sum(test_type_q)))
      df$test_ct_q = df$test_ct_q + df$present*as.numeric(df$test_type_q)
      df$test_q = rbinom(nrow(df), size = 1, prob = rapid_test_sens*df$present*df$test_type_q)
      df$t_end_inf = ifelse(df$inf & df$test_q & df$present, t, df$t_end_inf)
      df$t_notify = ifelse(df$inf & df$test_q & df$present, t+1, df$t_notify)
      df$detected = ifelse(df$inf & df$test_q & df$present, 1, df$detected)
      df$detected_q = ifelse(df$inf & df$test_q & df$present, 1, df$detected_q)
      df$detected_q_start = ifelse(df$inf & df$test_q & df$present & df$start, 1, df$detected_q_start)
      class_test_ind_q = class_test_ind_q + length(unique(df$class[df$test_type_q & df$present & !df$quarantined]))
      df$q_out = df$detected & df$inf
      
      df$rapid_tp_count = df$rapid_tp_count + ifelse(df$test_type_q & df$present, df$inf*df$test_q, 0)
      df$rapid_fn_count = df$rapid_fn_count + ifelse(df$test_type_q & df$present, df$inf*(1-df$test_q), 0)
      df$test_q_eligible = df$test_q_eligible + df$present*df$test_type_q
    }else{df$q_out = !df$vacc & df$flag}
    
    # re-estimated who is present
    df$present = sched$present[sched$t==t] & !df$q_out & !df$HH_id%in%df$HH_id[df$q_out]
    df$inf_home = df$t_inf > -1 & df$t_inf <= t & df$t_end_inf_home >= t & !df$start
    df$inf = df$t_inf > -1 & df$t_inf <= t & df$t_end_inf >= t
    
    # checks
    df$inf_days = df$inf_days + df$inf
    df$inf_home_days = df$inf_home_days + df$inf_home
    df$symp_days = df$symp_days + ifelse(df$symp_now==1 & !is.na(df$symp_now), 1, 0)
    df$symp_and_inf_days = df$symp_and_inf_days + df$symp_now*df$inf
    df$last = ifelse(df$inf, t, df$last)
    
    if(high_school & nrow(classes_out)>0){
      df$nq = !unlist(lapply(classes.ind, function(a) sum(a %in% classes_out$class)>0))
      df$present =  sched$present[sched$t==t] & df$nq
    }
    df$not_inf = df$t_exposed==-99 | df$t_exposed>t # if exposed from community, can be exposed earlier
    if(t==15) df$not_inf_keep = df$not_inf
    df$present_susp = df$present & df$not_inf
    df$quarantined = df$quarantined + as.numeric(df$q_out & sched$present[sched$t==t])
    #print(sum(as.numeric(df$q_out)))
    #print(sum(as.numeric(df$q_out & sched$present[sched$t==t])))
    
    df$quarantined2 = df$quarantined2 + as.numeric(df$q_out)
    df$quarantined_now = df$q_out & sched$present[sched$t==t]
    
    # check who is present
    mat[,(t-time_seed_inf+1)] = df$present
    
    # infectious and at school
    df$trans_now = df$present & df$inf & !df$family
    df$trans_outside = !df$present & df$inf & !df$class%in%classes_out$class & !df$HH_id%in%df$HH_id[df$class%in%classes_out$class] & (!df$adult | df$family)#& !df$family
    df$mix_outside = !df$present & !df$class%in%classes_out$class & !df$HH_id%in%df$HH_id[df$class%in%classes_out$class] & (!df$adult | df$family)#& !df$family
    if(high_school){
      df$trans_outside = !df$present & df$inf & (!df$adult | df$family)
      if(nrow(classes_out)>0) df$trans_outside = df$trans_outside & df$nq
    }
    #if(nrow(classes_out)>0){
    #print("Quarantined classes:")
    #print(classes_out)
    #}
    
    #if(sum(df$trans_now>0)){
    #print("Currently infectious, in school:")
    #print(df %>% filter(trans_now) %>% select(id, HH_id, class, group, adult, family, symp))
    #print(paste("In attendance: ", sum(df$quarantined_now & df$trans_now)))
    #}
    
    #if(sum(df$trans_outside>0)){
    #print("Currently infectious, outside of school:")
    #print(df %>% filter(trans_outside) %>% select(id, HH_id, class, group, adult, family, symp))}
    
    # set infections to 0 for this timestep
    df$now = F
    
    ## group testing
    if(test & t%in%testing_days){
      #print(test); print(t); print(testing_days)
      #print(t)
      #print("got to testing"); print(dim(df)); print(df %>% group_by(vacc) %>% summarize(sum(test_type)))
      df$test_ct = df$test_ct + rbinom(nrow(df), size = 1, prob = df$present*test_frac*as.numeric(df$test_type))
      df$test = rbinom(nrow(df), size = 1, prob = test_sens*test_frac*as.numeric(df$test_type))
      df$t_end_inf = ifelse(df$inf & df$test & df$present, t, df$t_end_inf)
      df$t_notify = ifelse(df$inf & df$test & df$present, t+1, df$t_notify)
      df$detected = ifelse(df$inf & df$test & df$present, 1, df$detected)
      class_test_ind = class_test_ind + length(unique(df$class[df$test_type & df$present & !df$quarantined]))
      
      df$pcr_tp_count = df$pcr_tp_count + ifelse(df$test_type & df$present, df$inf*df$test, 0)
      df$pcr_fn_count = df$pcr_fn_count + ifelse(df$test_type & df$present, df$inf*(1-df$test), 0)
      df$test_regular_eligible = df$test_regular_eligible + df$present*df$test_type
      
      #print(paste("Time:", t))
      #print(sum(df$test))
      #print(sum(df$inf & df$test & df$present))
      #print(df$id[df$inf & df$test & df$present])
      #print(df$class[df$inf & df$test & df$present])
      #print(df$family[df$inf & df$test & df$present])
      
      if(surveillance==T & sum(df$inf & df$test & df$present)>=1){
        #print("switch to reg")
        test_frac = 0.9
        testing_days = c(testing_days, t+1)
        surveillance = F
        df$switch = t
      }
      
      if(surveillance==F & sum(df$inf & df$test & df$present)==0 & (t-1)%in%testing_days){
        #print("switch to surv")
        test_frac = test_frac_orig
        surveillance = T
        df$switch = 0
        df$temp_switch = df$temp_switch + 1
      }
      
      #print(surveillance)
      #print(df$switch[1])
      #print(testing_days)
      #print(test_frac)
      # set up notification -- now below
      #df.u = df %>% filter(inf & test)
      #if(notify){class_quarantine = make_quarantine(class_quarantine, df.u, quarantine.length = quarantine.length, quarantine.grace = quarantine.grace, hs = high_school, hs.classes = hs.classes)}
      
    }
    
    #### SELECT NEXT GENERATION INFECTIONS ####
    # run model for infectious individuals at home
    if(sum(df$inf_home)>0) {
      
      home_infs = df$id[df$inf_home]
      if(sum(df$inf_home > 1)) home_infs = sample(home_infs)
      
      for(a in home_infs){
        
        # HOUSEHOLD CONTACTS
        inf_vec = run_household(a, df)
        df$location[df$id%in%inf_vec] = "Household"
        
        #Track risk set for unit testing
        df$person.days.at.risk.home.students[df$id == a] <- df$person.days.at.risk.home.students[df$id == a] +
          ifelse(df$start[df$id == a], 0,
                 (df$t_inf[df$id == a] <= t & df$t_end_inf_home[df$id == a] >= t)*sum(df$not_inf[df$HH_id == df$HH_id[df$id == a] &
                                                                                                   !df$adult & df$susp != 0]))
        df$person.days.at.risk.home.parents[df$id == a] <- df$person.days.at.risk.home.parents[df$id == a] +
          ifelse(df$start[df$id == a], 0,
                 (df$t_inf[df$id == a] <= t & df$t_end_inf_home[df$id == a] >= t)*sum(df$not_inf[df$HH_id == df$HH_id[df$id == a] & df$adult & df$susp != 0]))
        
        # add to total # of infections from this person
        df$tot_inf[df$id==a] = df$tot_inf[df$id==a] #+ sum(inf_vec>0)
        
        # flag people infected at this time step
        df$now = ifelse(df$id%in%inf_vec, T, df$now)
        df$source = ifelse(df$id%in%inf_vec, a, df$source)
        df$source_symp = ifelse(df$id%in%inf_vec, df$symp[df$id==a], df$source_symp)
        df$not_inf = ifelse(df$id%in%inf_vec, F, df$not_inf)
        df$present_susp = ifelse(df$id%in%inf_vec, F, df$present_susp)
      }
    }
    
    # run model for infectious individuals at school
    if(sum(df$trans_now)>0) {
      
      # RANDOM CONTACT STRUCTURE
      # sample from a random regular graph
      # this approach ensures reciprocity
      # you may want to split out to ensure reciprocity in contact type
      random_contacts = sample_k_regular(sum(df$present), df$n_contact[1] + df$n_contact_brief[1])
      #if(n_staff_contact>0) random_staff_contacts = sample_k_regular(sum(df$present & df$adult & !df$family), n_staff_contact)
      
      # SPECIAL SUBJECTS
      specials = data.frame(teacher = rep(df$id[df$specials], each = 4), class = sample(1:max(df$class[!is.na(df$class) & df$class<99]), 4*sum(df$specials), replace = T))
      #specials2 = specials %>% group_by(teacher) %>% summarize(class = unique(class)) %>% ungroup()
      #df$specials_count = df$specials_count + sapply(df$class, function(a) sum(specials2$class==a))
      
      # run transmission in schools
      school_infs = df$id[df$trans_now]
      if(sum(df$trans_now > 1)) school_infs = sample(school_infs)
      
      # choose contacts that become infected
      for(a in school_infs){
        
        # CLASS CONTACTS
        class_trans = run_class(a, df, high_school = high_school, hs.classes = hs.classes)
        #class_trans = 0
        df$location[df$id%in%class_trans] = "Class"
        
        #Track risk set for unit testing
        df$person.days.at.risk.class.students[df$id == a] <- df$person.days.at.risk.class.students[df$id == a] +
          ifelse(df$class[df$id == a] != 99,
                 (sched$present[sched$id == a & sched$t == t] & df$t_inf[df$id == a] <= t & df$t_end_inf[df$id == a] >= t)*sum(df$present_susp[df$class == df$class[df$id == a] & !df$adult & df$susp != 0]), 0)
        df$person.days.at.risk.class.teachers[df$id == a] <- df$person.days.at.risk.class.teachers[df$id == a] +
          ifelse(df$class[df$id == a] != 99,
                 (sched$present[sched$id == a & sched$t == t] & df$t_inf[df$id == a] <= t & df$t_end_inf[df$id == a] >= t)*sum(df$present_susp[df$class == df$class[df$id == a] & df$adult & df$susp != 0]), 0)
        
        # RANDOM CONTACTS
        rand_trans = tryCatch({run_rand(a, df, random_contacts)}, error = function(err) {0})
        #rand_trans = 0
        df$location[df$id%in%rand_trans] = "Random contacts"
        
        #Track risk set for unit testing
        df$person.days.at.risk.random.students[df$id == a] <- df$person.days.at.risk.random.students[df$id == a] + (sched$present[sched$id == a & sched$t == t] & df$t_inf[df$id == a] <= t & df$t_end_inf[df$id == a] >= t)*sum(df$present_susp[df$id %in% df$id[df$present][random_contacts[[which(df$id[df$present] == a)]][[1]]] & !df$adult & df$susp != 0 & !(df$id %in% class_trans)])
        df$person.days.at.risk.random.teachers[df$id == a] <- df$person.days.at.risk.random.teachers[df$id == a] + (sched$present[sched$id == a & sched$t == t] & df$t_inf[df$id == a] <= t & df$t_end_inf[df$id == a] >= t)*sum(df$present_susp[df$id %in% df$id[df$present][random_contacts[[which(df$id[df$present] == a)]][[1]]] & df$adult & df$susp != 0 & !(df$id %in% class_trans)])
        
        # RANDOM ADULT CONTACTS
        # note this doesn't strictly keep reciprocity
        # but k-regular graphs can be finnicky at low ##s
        # and this captures the general pattern
        if(df$adult[df$id==a] & !df$family[df$id==a]){
          rand_staff_trans.out = run_staff_rand(a, df, n_staff_contact, rel_trans_adult)
          rand_staff_trans <- rand_staff_trans.out[[1]]
          
          #Track risk set for unit testing
          df$person.days.at.risk.random.staff[df$id == a] <- df$person.days.at.risk.random.staff[df$id == a] + (sched$present[sched$id == a & sched$t == t] & df$t_inf[df$id == a] <= t & df$t_end_inf[df$id == a] >= t)*sum(df$present_susp[df$id %in% rand_staff_trans.out[[2]] & !(df$id %in% c(class_trans, rand_trans))])
          
        }else{rand_staff_trans = 0}
        df$location[df$id%in%rand_staff_trans] = "Staff contacts"
        
        # SPECIALS CONTACTS
        specials_trans = run_specials(a, df, specials)
        #specials_trans = 0
        df$location[df$id%in%specials_trans] = "Related arts"
        
        #Track risk set for unit testing
        df$person.days.at.risk.specials.kids[df$id == a] <- df$person.days.at.risk.specials.kids[df$id == a] +
          ifelse(df$specials[df$id == a],
                 (sched$present[sched$id == a & sched$t == t] & df$t_inf[df$id == a] <= t & df$t_end_inf[df$id == a] >= t)*sum(df$present_susp[df$class %in% specials$class[specials$teacher == a] & df$susp != 0 & !(df$id %in% c(class_trans, rand_trans, rand_staff_trans))]), 0)
        df$person.days.at.risk.specials.teachers[df$id == a] <- df$person.days.at.risk.specials.teachers[df$id == a] +
          ifelse(!df$adult[df$id == a],
                 (sched$present[sched$id == a & sched$t == t] & df$t_inf[df$id == a] <= t & df$t_end_inf[df$id == a] >= t)*sum(df$present_susp[df$id %in% specials$teacher[specials$class == df$class[df$id == a]] & df$susp != 0 & !(df$id %in% c(class_trans, rand_trans, rand_staff_trans))]),
                 ifelse(df$specials[df$id == a],
                        (sched$present[sched$id == a & sched$t == t] & df$t_inf[df$id == a] <= t & df$t_end_inf[df$id == a] >= t)*sum(df$present_susp[df$class %in% specials$class[specials$teacher == a] & df$susp != 0 & df$adult & !(df$id %in% c(class_trans, rand_trans, rand_staff_trans))]) , 0))
        
        # return id if person is infected
        # and 0 otherwise
        inf_vec = c(class_trans, rand_trans, rand_staff_trans, specials_trans)
        
        # add to total # of infections from this person
        df$tot_inf[df$id==a] = df$tot_inf[df$id==a] + sum(unique(inf_vec)>0)
        
        # flag people infected at this time step
        df$now = ifelse(df$id%in%inf_vec, T, df$now)
        df$source = ifelse(df$id%in%inf_vec, a, df$source)
        df$source_symp = ifelse(df$id%in%inf_vec, df$symp[df$id==a], df$source_symp)
        df$not_inf = ifelse(df$id%in%inf_vec, F, df$not_inf)
        df$present_susp = ifelse(df$id%in%inf_vec, F, df$present_susp)
        
      }
    }
    
    # run model for infectious individuals OUTSIDE school
    HHs = unique(df$HH_id[!df$adult & df$mix_outside])
    len = length(HHs)
    if(sum(df$trans_outside)>0 & n_HH>0 & len > 1 &
       (include_weekends | !sched$day[sched$t==t][1]%in%c("Sa", "Su"))) {
      
      #print(sched$day[sched$t==t][1])
      if(!bubble){
        
        # how many households are around
        tot = ceiling(len/n_HH)
        
        # care contacts
        care_contacts = data.frame(HH_id = HHs,
                                   cat = sample(rep(1:tot, each = n_HH)[1:len]))
      }
      
      # run transmission in care groups
      non_school_infs = df$id[df$trans_outside]
      if(sum(df$trans_outside > 1)) non_school_infs = sample(non_school_infs)
      
      # choose contacts that become infected
      for(a in non_school_infs){
        
        # CARE CONTACTS
        care_trans.out = run_care(a, df, care_contacts, rel_trans_CC, num_adults = num_adults)
        care_trans = care_trans.out[[1]]
        df$location[df$id%in%care_trans] = "Child care"
        
        #Track risk set for unit testing
        df$person.days.at.risk.care.students[df$id == a] <- df$person.days.at.risk.care.students[df$id == a] +
          ifelse(a %in% care_trans.out[[2]] | !df$adult[df$id == a],
                 (df$trans_outside[df$id == a] & df$t_inf[df$id == a] <= t & df$t_end_inf[df$id == a] >= t)*sum(df$not_inf[df$HH_id %in% care_contacts$HH_id[care_contacts$cat == care_contacts$cat[care_contacts$HH_id == df$HH_id[df$id == a]]] & !df$adult & df$susp != 0 & df$HH_id != df$HH_id[df$id == a]]), 0)
        df$person.days.at.risk.care.parents[df$id == a] <- df$person.days.at.risk.care.parents[df$id == a] +
          ifelse(a %in% care_trans.out[[2]] | !df$adult[df$id == a],
                 (df$trans_outside[df$id == a] & df$t_inf[df$id == a] <= t & df$t_end_inf[df$id == a] >= t)*sum(df$not_inf[df$id %in% care_trans.out[[2]] & df$susp != 0 & df$HH_id != df$HH_id[df$id == a]]), 0)
        
        # return id if person is infected
        # and 0 otherwise
        inf_vec = c(care_trans)
        
        # add to total # of infections from this person
        df$tot_inf[df$id==a] = df$tot_inf[df$id==a] #+ sum(inf_vec>0)
        
        # flag people infected at this time step
        df$now = ifelse(df$id%in%inf_vec, T, df$now)
        df$source = ifelse(df$id%in%inf_vec, a, df$source)
        df$source_symp = ifelse(df$id%in%inf_vec, df$symp[df$id==a], df$source_symp)
        df$not_inf = ifelse(df$id%in%inf_vec, F, df$not_inf)
        df$present_susp = ifelse(df$id%in%inf_vec, F, df$present_susp)
        
      }
    }
    
    #### SET UP NEXT GENERATION INFECTEDS ####
    # update values updated at this stage
    # need to put in probability distributions
    if(sum(df$now>0)){
      
      df$start[df$now] = 0      # remove seed if infected earlier
      df$t_exposed[df$now] = t
      df[df$now,] = make_infected(df[df$now,], days_inf = days_inf, mult_asymp = mult_asymp, mult_asymp_child = mult_asymp_child, turnaround.time = turnaround.time, overdisp_off = overdisp_off)
      #print("New exposures:")
      #print(df %>% filter(now) %>% arrange(source) %>% select(id, HH_id, class, group, adult, family, source, location, symp))
    }
    
    # round values
    df$t_notify = ceiling(df$t_notify)
    
    if(notify & sum(df$t_notify==(t+1) & !df$family)>0){
      df.u = df %>% filter(t_notify==(t+1) & !family)
      #print("Quarantined: "); print(df.u %>% dplyr::select(adult, family, class, symp, sub_clin, t_notify, start))
      
      # set up notification
      class_quarantine = make_quarantine(class_quarantine, df.u, quarantine.length = quarantine.length, quarantine.grace = quarantine.grace, hs = high_school, hs.classes = hs.classes)
      df$uh.oh = df$uh.oh + sum(df$source[df$now]%in%(df$id[df$class%in%classes_out$class]) & df$location[df$now]!="Household")>0
      
    }
    #print(t); print(class_quarantine)
    #print(df %>% #filter(!adult) %>%
    #        group_by(class) %>% summarize(mean(quarantined), sum(quarantined)))
    #if(sum(class_quarantine$t_notify!=-1)>0) print(class_quarantine)
    
  }
  # remember to add mat back in
  #print(df$id[df$t_exposed!=-99 & df$class==df$class[df$start]])
  #print(sum(df$t_exposed!=-99))
  df$class_test_ind = class_test_ind
  df$class_test_ind_q = class_test_ind_q
  df$surveillance = surveillance
  #print(sum(class_quarantine$t_notify>-1))#; print(tail(class_quarantine))
  return(df) #, time_seed_inf, class_quarantine, mat))
}

#' Run model multiple times and summarize results
#'
#' @param N number of runs
#' @param n_other_adults Number of adults in the school other than primary teachers; defaults to 30
#' @param n_contacts Number of sustained contacts outside of the classroom; defaults to 10
#' @param n_contacts_brief Number of brief contacts outside of the classroom; defaults to 20
#' @param rel_trans_HH Relative attack rate of household contact (vs. classrom); defaults to 1
#' @param rel_trans_HH_symp_child Additional relative attack rate of a symptomatic infected child in the household; defaults to 1
#' @param rel_trans Relative attack rate of sustained contact (vs. classroom); defaults to 1/8
#' @param rel_trans_brief Relative attack rate of brief contact (vs. classroom); defaults to 1/50
#' @param rel_trans_CC relative transmission in childcare vs. classroom; defaults to 2
#' @param rel_trans_adult relative transmission in staff-staff interactions vs. classroom; defaults to 2
#' @param p_asymp_adult Fraction of adults with asymptomatic (unsuspected) disease; defaults to 0.2
#' @param p_asymp_child Fraction of children with asymptomatic (unsuspected) disease; defaults to 0.8
#' @param p_subclin_adult Fraction of adults with subclinical but not techincally asymptomatic disease; defaults to 0
#' @param p_subclin_child Fraction of children with subclinical but not techincally asymptomatic disease; defaults to 0
#' @param attack Average daily attack rate in adults; defaults to 0.01
#' @param child_trans Relative transmissibility of children (vs. adults); defaults to 1
#' @param child_susp Relative transmissibility of children (vs. adults); defaults to .5
#' @param child_vax Vaccination rate of children; defaults to 0
#' @param teacher_trans Factor by which teacher transmissibility is reduced due to intervention; defaults to 1
#' @param teacher_susp Teacher vaccination rate; defaults to 0.8
#' @param family_susp Household member vaccination rate; defaults to 0.7
#' @param disperse_transmission Whether transmission is overdispersed (vs. all have equal attack rate); default to T
#' @param n_staff_contact number of contacts a teacher/staff member has with other teachers/staff members; defaults to 1
#' @param n_HH number of households a household interacts with when not attending school; defaults to 0
#' @param bubble whether out-of-school interactions occur with a 'bubble'; defaults to F
#' @param n_start number of infections to seed model; defaults to 1
#' @param time_seed_inf time(s) at which to introduce new infectious individuals; defaults to NA and randomly selects one time
#' @param days_inf length of infectious period (assuming mild case or quarantined on symptoms)
#' @param mult_asymp multiplier on asymptomatic infection for adults; default is 1
#' @param mult_asymp_child multiplier on asymptomatic infection for children; default is 1
#' @param seed_asymp whether to seed with an asymptomatic case
#' @param isolate Whether symptomatic individuals isolate when symptoms emerge; defaults to T
#' @param dedens Whether dedensification measures reduce attack rate; defaults to F
#' @param run_specials_now Whether special subjects are run; defaults to F
#' @param time length of time to run model; defaults to 30
#' @param notify whether classrooms are notified and quarantined; defaults to F
#' @param test whether there is weekly testing; defaults to F
#' @param test_sens test sensitivity; defaults to 0.7
#' @param test_frac fraction of school tested; defaults to 0.9
#' @param test_days test frequency; "day", "week", "2x_week"; defaults to "week"
#' @param test_type group tested; defaults to "all", also allows "staff" and "students"
#' @param test_start_day day tests are implemented for weekly testing; defaults to 1 = Monday
#' @param n_class number of classes per grade
#' @param high_school whether to use a high school schedule of random period mixing; defaults to F
#' @param nper number of school periods; defaults to 8
#' @param start_mult value to indicate relative frequency of adult/child infections; defaults to 1 (adults 2x as likely as kids)
#' @param start_type type of seed; default is "mix" (also "adult", "child")
#' @param child_prob if start_type = "cont", set daily probability of infectious entry for children, defaults to .05
#' @param adult_prob if start_type = "cont", set daily probability of infectious entry for adults, defaults to .01
#' @param quarantine.length length of quarantine when someone is infectious; defaults to 10
#' @param quarantine.grace length of grace period after which a quarantined class returns not to be "re-quarantined"
#' @param turnaround.time test turnaround time, default = 3 days
#' @param type schedule; "base", "On/off", "A/B", "Remote"; defaults to "base"
#' @param version v1 quarantines full cohort in A/B; v2 only sub-cohort; defaults to 2
#' @param total_days number of days in school; defaults to 5
#' @param num_adults number of adults interacting with children, defaults to 2
#' @param includeFamily whether to include family, default = FALSE
#' @param include_weekends if TRUE excludes weekends from additional out-of-school mixing, defaults to F
#' @param vax_eff Vaccine efficacy, defaults to 0.9
#' @param test_quarantine whether quarantined individuals attend school but are tested daily; defaults to FALSE
#' @param surveillance whether surveillance is underway; defaults to F
#' @param rapid_test_sens sensitivity of rapid tests, defaults to 80%
#' @param overdisp_off all overdispersion off; defaults to F
#' @param no_test_vacc Indicates whether vaccinated individuals are excluded from TTS & screening; defaults to F
#' @param synthpop synthetic population; defaults to synthpop based on Maryland elementary school
#' @param class make_school object; defaults to NA and will call for each simulation
#'
#' @export
mult_runs = function(N, n_other_adults, n_contacts, rel_trans_HH,
                     rel_trans, rel_trans_CC, rel_trans_adult, p_asymp_adult, child_prob, adult_prob,
                     p_asymp_child, attack, child_trans, child_susp, child_vax, p_subclin_adult, p_subclin_child,
                     teacher_susp, disperse_transmission, n_staff_contact, n_HH, num_adults, family_susp,
                     n_start, time_seed_inf, days_inf, mult_asymp, mult_asymp_child, seed_asymp, isolate, dedens, run_specials_now,
                     time, notify, test, test_sens, test_frac, test_days, test_type, quarantine.length, quarantine.grace,
                     type, total_days, includeFamily, synthpop, class, n_class, high_school, nper, start_mult, start_type,
                     include_weekends, turnaround.time, test_start_day, test_quarantine, vax_eff, surveillance,
                     rapid_test_sens, overdisp_off, no_test_vacc, rel_trans_HH_symp_child, teacher_trans,
                     n_contacts_brief = 0, rel_trans_brief = 1/50, bubble = F, version = 2){
  
  keep = data.frame(all = numeric(N), tot = numeric(N), R0 = numeric(N), Rt = numeric(N), start = numeric(N), start_adult = numeric(N), asymp_kids = numeric(N),
                    source_asymp = numeric(N), source_asymp_family_kids = numeric(N), source_asymp_family_staff = numeric(N), start_family = numeric(N),
                    adult = numeric(N), teacher = numeric(N), family = numeric(N), staff_family = numeric(N), children = numeric(N), children_tot = numeric(N), family_tot = numeric(N),
                    adult_tot = numeric(N), attack = numeric(N), class = numeric(N), household = numeric(N), detected = numeric(N),
                    detected_staff = numeric(N), detected_students = numeric(N), detected_staff_subclin = numeric(N), detected_students_subclin = numeric(N),
                    symp = numeric(N), symp_kids = numeric(N), avg_infs = numeric(N), class_test_ind = numeric(N), test_qs = numeric(N),
                    quarantine_check = numeric(N), quarantined = numeric(N), quarantined_tot = numeric(N), quarantined_kids = numeric(N), from_kids = numeric(N), related_arts = numeric(N),
                    child_care = numeric(N), random = numeric(N), random_staff = numeric(N), num_classroom = numeric(N), avg_class = numeric(N), clin_staff = numeric(N), clin_students = numeric(N), clin_family = numeric(N))
  
  #tic()
  # run over time
  for(i in 1:N){
    
    ## make class
    if(is.na(unlist(class))[1]){
      
      class = make_school(synthpop = synthpop, n_other_adults = n_other_adults, includeFamily = includeFamily, n_class = n_class)
      
    }
    ## make school
    school = initialize_school(n_contacts = n_contacts, n_contacts_brief = n_contacts_brief, rel_trans_HH = rel_trans_HH,
                               rel_trans = rel_trans, rel_trans_brief = rel_trans_brief, p_asymp_adult = p_asymp_adult,
                               p_asymp_child = p_asymp_child, p_subclin_adult = p_subclin_adult, p_subclin_child = p_subclin_child,
                               attack = attack, child_trans = child_trans, child_susp = child_susp, child_vax = child_vax, family_susp = family_susp,
                               teacher_trans = teacher_trans, teacher_susp = teacher_susp, disperse_transmission = disperse_transmission,
                               isolate = isolate, dedens = dedens, run_specials = run_specials_now, start = class, vax_eff = vax_eff, notify = notify,
                               no_test_vacc = no_test_vacc, rel_trans_HH_symp_child = rel_trans_HH_symp_child)
    
    ## make schedule
    sched = make_schedule(time = time + 15, df = school, type = type, total_days = total_days)
    
    ## run model
    df = run_model(time = time, notify = notify, test = test, df = school, sched = sched,
                   test_sens = test_sens, test_frac = test_frac, test_days = test_days, days_inf = days_inf,
                   mult_asymp = mult_asymp, mult_asymp_child = mult_asymp_child, seed_asymp = seed_asymp, n_HH = n_HH, n_staff_contact = n_staff_contact,
                   n_start = n_start, time_seed_inf = time_seed_inf, high_school = high_school, nper = nper,
                   start_mult = start_mult, start_type = start_type, child_prob = child_prob, adult_prob = adult_prob, test_type = test_type,
                   rel_trans_CC = rel_trans_CC, rel_trans_adult = rel_trans_adult, quarantine.length = quarantine.length, quarantine.grace = quarantine.grace,
                   num_adults = num_adults, bubble = bubble, include_weekends = include_weekends, turnaround.time = turnaround.time,
                   test_start_day = test_start_day, type = type, test_quarantine = test_quarantine, version = version, surveillance = surveillance,
                   rapid_test_sens = rapid_test_sens, overdisp_off = overdisp_off)
    
    time_keep = df$start.time[1]
    #print(time_keep)
    #print(length(time_keep:(time_keep+time-1)))
    
    # store output
    keep$all[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1)
    keep$tot[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & !df$HH_id%in%c(df$HH_id[df$start]))
    
    keep$all_15[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & df$t_end_inf_home >= 15)
    keep$tot_15[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 15)
    keep$detected_15[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==15)
    
    keep$all_15_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & df$t_end_inf_home >= 15)
    keep$tot_15_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 15)
    keep$detected_15_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==15)
    
    keep$all_15_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & df$t_end_inf_home >= 15)
    keep$tot_15_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 15)
    keep$detected_15_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==15)
    
    keep$all_22[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & df$t_end_inf_home >= 22)
    keep$tot_22[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 22)
    keep$detected_22[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==22)
    
    keep$all_22_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & df$t_end_inf_home >= 22)
    keep$tot_22_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 22)
    keep$detected_22_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==22)
    
    keep$all_22_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & df$t_end_inf_home >= 22)
    keep$tot_22_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 22)
    keep$detected_22_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==22)
    
    keep$all_29[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & df$t_end_inf_home >= 29)
    keep$tot_29[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 29)
    keep$detected_29[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==29)
    
    keep$all_29_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & df$t_end_inf_home >= 29)
    keep$tot_29_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 29)
    keep$detected_29_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==29)
    
    keep$all_29_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & df$t_end_inf_home >= 29)
    keep$tot_29_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 29)
    keep$detected_29_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==29)
    
    keep$all_36[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & df$t_end_inf_home >= 36)
    keep$tot_36[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 36)
    keep$detected_36[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==36)
    
    keep$all_36_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & df$t_end_inf_home >= 36)
    keep$tot_36_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 36)
    keep$detected_36_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==36)
    
    keep$all_36_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & df$t_end_inf_home >= 36)
    keep$tot_36_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 36)
    keep$detected_36_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==36)
    
    keep$all_43[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & df$t_end_inf_home >= 43)
    keep$tot_43[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 43)
    keep$detected_43[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==43)
    
    keep$all_43_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & df$t_end_inf_home >= 43)
    keep$tot_43_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 43)
    keep$detected_43_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==43)
    
    keep$all_43_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43  & df$t_end_inf_home >= 43)
    keep$tot_43_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 43)
    keep$detected_43_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==43)
    
    keep$from_kids[i] = 0 #sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & !df$HH_id%in%c(df$HH_id[df$start]) & !df$adult[df$source])
    keep$R0[i] = sum(df$tot_inf[df$start])
    keep$Rt[i] =  mean(df$tot_inf[df$t_inf!=0 & df$t_end_inf_home>=time_keep], na.rm = T)
    keep$avg_infs[i] = mean(df$tot_inf[df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$start])
    keep$start[i] = sum(df$start & df$t_end_inf_home>=time_keep & df$t_inf < time_keep+time - 1)
    keep$class_test_ind[i] = df$class_test_ind[1]
    keep$class_test_ind_q[i] = df$class_test_ind_q[1]
    keep$test_qs[i] = sum(df$test_ct_q)
    keep$test_regular[i] = sum(df$test_ct)
    keep$detected[i] = sum(df$detected)
    keep$detected_q[i] = sum(df$detected_q)
    keep$detected_q_start[i] = sum(df$detected_q_start)
    keep$detected_staff[i] = sum(df$detected[df$adult])
    keep$detected_students[i] = sum(df$detected[!df$adult])
    keep$detected_staff_subclin[i] = sum(df$detected[df$adult & df$sub_clin], na.rm = T)
    keep$detected_students_subclin[i] = sum(df$detected[!df$adult & df$sub_clin], na.rm = T)
    keep$quarantine_check[i] = max(df$t_end_inf-df$t_end_inf_home, na.rm = T)#(df$uh.oh[1])
    keep$avg_class[i] = unlist(df %>% filter(t_exposed!=-99 & t_exposed <= time_keep + time - 1 & class!=99) %>% group_by(class) %>%
                                 summarize(num = length(class)) %>% ungroup() %>% summarize(mean(num, na.rm = T)))
    keep$quarantined[i] = sum(df$quarantined)
    keep$quarantined2[i] = sum(df$quarantined2)
    keep$quarantined_kids[i] = sum(df$quarantined[!df$adult])#length(unique(df$id[df$quarantined>0])) #sum(df$quarantined[!df$adult])
    keep$start_adult[i] = sum(df$adult & df$start & df$t_end_inf_home>=time_keep)
    keep$start_family[i] = sum(df$family & df$start & df$t_end_inf_home>=time_keep)
    keep$start_kids[i] = sum(!df$adult & df$start & df$t_end_inf_home>=time_keep)
    keep$start_symp[i] = sum(df$symp[df$start], na.rm = T)
    keep$source_asymp[i] = sum(!df$source_symp & df$t_inf <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$HH_id%in%c(df$HH_id[df$start]), na.rm = T)
    keep$source_asymp_family_kids[i] = sum(df$family & !df$source_symp & df$t_inf <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$HH_id%in%c(df$HH_id[df$start]), na.rm = T)
    keep$source_asymp_family_staff[i] =sum(df$family_staff & !df$source_symp & df$t_inf <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$HH_id%in%c(df$HH_id[df$start]), na.rm = T)
    keep$adult[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$adult & !df$family & df$t_inf <= time_keep + time - 1)
    keep$teacher[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$adult & !df$family & df$t_inf <= time_keep + time - 1 & df$class < 99)
    keep$family[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$family & df$t_inf <= time_keep + time - 1 )
    keep$staff_family[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$family_staff & df$t_inf <= time_keep + time - 1)
    keep$children[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$adult & df$t_inf <= time_keep + time - 1)
    keep$children_tot[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$adult & df$t_inf <= time_keep + time - 1 & !df$HH_id%in%c(df$HH_id[df$start]))
    keep$school_adult_tot[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$adult & !df$family & df$t_inf <= time_keep + time - 1 & !df$HH_id%in%c(df$HH_id[df$start]))
    keep$family_tot[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$adult & df$family & df$t_inf <= time_keep + time - 1 & !df$HH_id%in%c(df$HH_id[df$start]))
    keep$symp[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$symp==1 & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$symp_kids[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$symp==1 & df$t_inf <= time_keep + time - 1 & !df$adult, na.rm = T)
    keep$asymp_kids[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$symp==0 & df$t_inf <= time_keep + time - 1 & !df$adult, na.rm = T)
    keep$sick_at_end[i] = sum(df$t_inf<=time_keep + time - 1 & df$t_end_inf > time_keep + time - 1)
    keep$class[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Class")
    keep$household[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Household")
    keep$related_arts[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Related arts")
    keep$child_care[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Child care")
    keep$random[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Random contacts")
    keep$random_staff[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Staff contacts")
    keep$num_classroom[i] = length(unique(df$class[df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$class < 99]))
    keep$clin_staff[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$clin_students[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & !df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$clin_family[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$clin_staff2[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$clin_students2[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & !df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$clin_family2[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$notify_staff[i] = sum(df$t_notify>=15 & df$adult & !df$family & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$notify_students[i] = sum(df$t_notify>=15 & !df$adult & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$notify_family[i] = sum(df$t_notify>=15 & df$family & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$notify_staff2[i] = sum(df$t_notify>=15 & df$adult & !df$family & df$t_notify <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$notify_students2[i] = sum(df$t_notify>=15 & !df$adult & df$t_notify <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$notify_family2[i] = sum(df$t_notify>=15 & df$family & df$t_notify <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$surveillance[i] = df$surveillance[1]
    keep$switch[i] = df$switch[1]
    keep$temp_switch[i] = df$temp_switch[1]
    #keep$specials_count[i] = sum(df$specials_count[df$start])
    
    
    #John's new checks
    keep$inf_ct_sympK_A_home[i] = sum(df$location == "Household" & df$adult & df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_asympK_A_home[i] = sum(df$location == "Household" & df$adult & !df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_sympK_K_home[i] = sum(df$location == "Household" & !df$adult & df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_asympK_K_home[i] = sum(df$location == "Household" & !df$adult & !df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_sympA_K_home[i] = sum(df$location == "Household" & !df$adult & df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    keep$inf_ct_asympA_K_home[i] = sum(df$location == "Household" & !df$adult & !df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    keep$inf_ct_sympA_A_home[i] = sum(df$location == "Household" & df$adult & df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    keep$inf_ct_asympA_A_home[i] = sum(df$location == "Household" & df$adult & !df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    
    keep$inf_ct_sympK_A_class[i] = sum(df$location == "Class" & df$adult & df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_asympK_A_class[i] = sum(df$location == "Class" & df$adult & !df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_sympK_K_class[i] = sum(df$location == "Class" & !df$adult & df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_asympK_K_class[i] = sum(df$location == "Class" & !df$adult & !df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_sympA_K_class[i] = sum(df$location == "Class" & !df$adult & df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    keep$inf_ct_asympA_K_class[i] = sum(df$location == "Class" & !df$adult & !df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    keep$inf_ct_sympA_A_class[i] = sum(df$location == "Class" & df$adult & df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    keep$inf_ct_asympA_A_class[i] = sum(df$location == "Class" & df$adult & !df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    
    keep$inf_ct_sympK_A_specials[i] = sum(df$location == "Related arts" & df$adult & df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_asympK_A_specials[i] = sum(df$location == "Related arts" & df$adult & !df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_sympK_K_specials[i] = sum(df$location == "Related arts" & !df$adult & df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_asympK_K_specials[i] = sum(df$location == "Related arts" & !df$adult & !df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_sympA_K_specials[i] = sum(df$location == "Related arts" & !df$adult & df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    keep$inf_ct_asympA_K_specials[i] = sum(df$location == "Related arts" & !df$adult & !df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    keep$inf_ct_sympA_A_specials[i] = sum(df$location == "Related arts" & df$adult & df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    keep$inf_ct_asympA_A_specials[i] = sum(df$location == "Related arts" & df$adult & !df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    
    keep$inf_ct_sympK_A_care[i] = sum(df$location == "Child care" & df$adult & df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_asympK_A_care[i] = sum(df$location == "Child care" & df$adult & !df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_sympK_K_care[i] = sum(df$location == "Child care" & !df$adult & df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_asympK_K_care[i] = sum(df$location == "Child care" & !df$adult & !df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_sympA_K_care[i] = sum(df$location == "Child care" & !df$adult & df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    keep$inf_ct_asympA_K_care[i] = sum(df$location == "Child care" & !df$adult & !df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    keep$inf_ct_sympA_A_care[i] = sum(df$location == "Child care" & df$adult & df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    keep$inf_ct_asympA_A_care[i] = sum(df$location == "Child care" & df$adult & !df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    
    keep$inf_ct_sympK_A_rand[i] = sum(df$location == "Random contacts" & df$adult & df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_asympK_A_rand[i] = sum(df$location == "Random contacts" & df$adult & !df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_sympK_K_rand[i] = sum(df$location == "Random contacts" & !df$adult & df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_asympK_K_rand[i] = sum(df$location == "Random contacts" & !df$adult & !df$source_symp & df$source %in% df$id[!df$adult], na.rm = TRUE)
    keep$inf_ct_sympA_K_rand[i] = sum(df$location == "Random contacts" & !df$adult & df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    keep$inf_ct_asympA_K_rand[i] = sum(df$location == "Random contacts" & !df$adult & !df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    keep$inf_ct_sympA_A_rand[i] = sum(df$location == "Random contacts" & df$adult & df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    keep$inf_ct_asympA_A_rand[i] = sum(df$location == "Random contacts" & df$adult & !df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    
    keep$inf_ct_sympA_A_staff[i] = sum(df$location == "Staff contacts" & df$adult & df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    keep$inf_ct_asympA_A_staff[i] = sum(df$location == "Staff contacts" & df$adult & !df$source_symp & df$source %in% df$id[df$adult], na.rm = TRUE)
    
    keep$risk_ct_sympK_A_home[i] = sum(df$person.days.at.risk.home.parents[df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_asympK_A_home[i] = sum(df$person.days.at.risk.home.parents[!df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_sympK_K_home[i] = sum(df$person.days.at.risk.home.students[df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_asympK_K_home[i] = sum(df$person.days.at.risk.home.students[!df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_sympA_A_home[i] = sum(df$person.days.at.risk.home.parents[df$symp & df$adult], na.rm = TRUE)
    keep$risk_ct_asympA_A_home[i] = sum(df$person.days.at.risk.home.parents[!df$symp & df$adult], na.rm = TRUE)
    keep$risk_ct_sympA_K_home[i] = sum(df$person.days.at.risk.home.students[df$symp & df$adult], na.rm = TRUE)
    keep$risk_ct_asympA_K_home[i] = sum(df$person.days.at.risk.home.students[!df$symp & df$adult], na.rm = TRUE)
    
    keep$risk_ct_sympK_A_class[i] = sum(df$person.days.at.risk.class.teachers[df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_asympK_A_class[i] = sum(df$person.days.at.risk.class.teachers[!df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_sympK_K_class[i] = sum(df$person.days.at.risk.class.students[df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_asympK_K_class[i] = sum(df$person.days.at.risk.class.students[!df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_sympA_A_class[i] = sum(df$person.days.at.risk.class.teachers[df$symp & df$adult], na.rm = TRUE)
    keep$risk_ct_asympA_A_class[i] = sum(df$person.days.at.risk.class.teachers[!df$symp & df$adult], na.rm = TRUE)
    keep$risk_ct_sympA_K_class[i] = sum(df$person.days.at.risk.class.students[df$symp & df$adult], na.rm = TRUE)
    keep$risk_ct_asympA_K_class[i] = sum(df$person.days.at.risk.class.students[!df$symp & df$adult], na.rm = TRUE)
    
    keep$risk_ct_sympK_A_specials[i] = sum(df$person.days.at.risk.specials.teachers[df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_asympK_A_specials[i] = sum(df$person.days.at.risk.specials.teachers[!df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_sympK_K_specials[i] = sum(df$person.days.at.risk.specials.kids[df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_asympK_K_specials[i] = sum(df$person.days.at.risk.specials.kids[!df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_sympA_A_specials[i] = sum(df$person.days.at.risk.specials.teachers[df$symp & df$adult], na.rm = TRUE)
    keep$risk_ct_asympA_A_specials[i] = sum(df$person.days.at.risk.specials.teachers[!df$symp & df$adult], na.rm = TRUE)
    keep$risk_ct_sympA_K_specials[i] = sum(df$person.days.at.risk.specials.kids[df$symp & df$adult], na.rm = TRUE)
    keep$risk_ct_asympA_K_specials[i] = sum(df$person.days.at.risk.specials.kids[!df$symp & df$adult], na.rm = TRUE)
    
    keep$risk_ct_sympK_A_care[i] = sum(df$person.days.at.risk.care.parents[df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_asympK_A_care[i] = sum(df$person.days.at.risk.care.parents[!df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_sympK_K_care[i] = sum(df$person.days.at.risk.care.students[df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_asympK_K_care[i] = sum(df$person.days.at.risk.care.students[!df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_sympA_A_care[i] = sum(df$person.days.at.risk.care.parents[df$symp & df$adult], na.rm = TRUE)
    keep$risk_ct_asympA_A_care[i] = sum(df$person.days.at.risk.care.parents[!df$symp & df$adult], na.rm = TRUE)
    keep$risk_ct_sympA_K_care[i] = sum(df$person.days.at.risk.care.students[df$symp & df$adult], na.rm = TRUE)
    keep$risk_ct_asympA_K_care[i] = sum(df$person.days.at.risk.care.students[!df$symp & df$adult], na.rm = TRUE)
    
    keep$risk_ct_sympK_A_rand[i] = sum(df$person.days.at.risk.random.teachers[df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_asympK_A_rand[i] = sum(df$person.days.at.risk.random.teachers[!df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_sympK_K_rand[i] = sum(df$person.days.at.risk.random.students[df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_asympK_K_rand[i] = sum(df$person.days.at.risk.random.students[!df$symp & !df$adult], na.rm = TRUE)
    keep$risk_ct_sympA_A_rand[i] = sum(df$person.days.at.risk.random.teachers[df$symp & df$adult], na.rm = TRUE)
    keep$risk_ct_asympA_A_rand[i] = sum(df$person.days.at.risk.random.teachers[!df$symp & df$adult], na.rm = TRUE)
    keep$risk_ct_sympA_K_rand[i] = sum(df$person.days.at.risk.random.students[df$symp & df$adult], na.rm = TRUE)
    keep$risk_ct_asympA_K_rand[i] = sum(df$person.days.at.risk.random.students[!df$symp & df$adult], na.rm = TRUE)
    
    keep$risk_ct_sympA_A_staff[i] = sum(df$person.days.at.risk.random.staff[df$symp & df$adult], na.rm = TRUE)
    keep$risk_ct_asympA_A_staff[i] = sum(df$person.days.at.risk.random.staff[!df$symp & df$adult], na.rm = TRUE)
    
    keep$length.infectious_obs[i] = mean(floor(df$t_end_inf_home[df$t_inf > 0]) - ceiling(df$t_inf[df$t_inf > 0]) + 1)
    keep$length.infectious_sd[i] = sd(floor(df$t_end_inf_home[df$t_inf > 0]) - ceiling(df$t_inf[df$t_inf > 0]) + 1)
    
    keep$length.infectious.school.symp.present_obs[i] = mean(floor(df$t_end_inf[df$symp & !df$sub_clin & df$t_inf > 0 & (!df$adult | !df$family)]) - ceiling(df$t_inf[df$symp & !df$sub_clin & df$t_inf > 0 & (!df$adult | !df$family)]) + 1)
    keep$length.infectious.school.asymp.present_obs[i] = mean(floor(df$t_end_inf[(!df$symp | df$sub_clin) & df$t_inf > 0 & (!df$adult | !df$family)]) - ceiling(df$t_inf[(!df$symp | df$sub_clin) & df$t_inf > 0 & (!df$adult | !df$family)]) + 1)
    keep$length.infectious.school.symp.family_obs[i] = mean(floor(df$t_end_inf[df$symp & !df$sub_clin & df$t_inf > 0 & df$family]) - ceiling(df$t_inf[df$symp & !df$sub_clin & df$t_inf > 0 & df$family]) + 1)
    keep$length.infectious.school.asymp.family_obs[i] = mean(floor(df$t_end_inf[(!df$symp | df$sub_clin) & df$t_inf > 0 & df$family]) - ceiling(df$t_inf[(!df$symp | df$sub_clin) & df$t_inf > 0 & df$family]) + 1)
    
    keep$n_students_obs[i] = sum(!(df$adult))
    keep$n_teachers_obs[i] = sum((df$adult & df$class != 99))
    keep$n_staff_obs[i] = sum((df$adult & !df$family & df$class == 99))
    
    keep$p_asymp_adult_obs[i] = sum(!df$symp & df$t_exposed > 0 & df$adult)/sum(df$t_exposed > 0 & df$adult)
    keep$p_asymp_child_obs[i] = sum(!df$symp & df$t_exposed > 0 & !df$adult)/sum(df$t_exposed > 0 & !df$adult)
    keep$p_subclin_adult_obs[i] = sum(df$sub_clin & df$t_exposed > 0 & df$adult)/sum(df$t_exposed > 0 & df$adult)
    keep$p_subclin_child_obs[i] = sum(df$sub_clin & df$t_exposed > 0 & !df$adult)/sum(df$t_exposed > 0 & !df$adult)
    
    keep$length.incubation_obs[i] = mean(floor(df$t_inf[df$t_inf > 0 & !df$start]) - ceiling(df$t_exposed[df$t_inf > 0 & !df$start] + runif(length(df$t_exposed[df$t_inf > 0 & !df$start]), min = -0.5, max = 0.5)) + 1)
    keep$length.symp.gap_obs[i] = mean(floor(df$t_symp[df$t_inf > 0 & !df$start]) - ceiling(df$t_exposed[df$t_inf > 0 & !df$start] + runif(length(df$t_exposed[df$t_inf > 0 & !df$start]), min = -0.5, max = 0.5)) + 1)
    
    keep$child.vax.rate_obs[i] = mean(df$vacc[!df$adult])
    keep$teacher.vax.rate_obs[i] = mean(df$vacc[df$adult & !df$family])
    keep$family.vax.rate_obs[i] = mean(df$vacc[df$family])
    keep$vax.eff_obs[i] = (ifelse(is.na(mean(df$susp[!df$adult & df$vacc] == 0)),
                                  sum(!(df$adult) & df$vacc),
                                  mean(df$susp[!df$adult & df$vacc] == 0))*sum(!(df$adult) & df$vacc) +
                             ifelse(is.na(mean(df$susp[df$adult & !df$family & df$vacc] == 0)),
                                    sum((df$adult & !df$family & df$vacc)),
                                    mean(df$susp[df$adult & !df$family & df$vacc] == 0))*sum((df$adult & !df$family & df$vacc)) +
                             ifelse(is.na(mean(df$susp[df$family & df$vacc] == 0)),
                                    sum((df$family & df$vacc)),
                                    mean(df$susp[df$family & df$vacc] == 0))*sum((df$family & df$vacc)))/
      (sum(!(df$adult) & df$vacc) +sum((df$adult & !df$family & df$vacc)) + sum((df$family & df$vacc))) 
    
    keep$child.prob_obs[i] = ifelse(start_type == "cont", mean(sapply(df$start.time[1]:(time + df$start.time[1] - 1), function(t){sum(ceiling(df$t_inf[df$start & !df$adult]) == t)/sum(!df$adult)})), NA)
    keep$adult.prob_obs[i] = ifelse(start_type == "cont", mean(sapply(df$start.time[1]:(time + df$start.time[1] - 1), function(t){sum(ceiling(df$t_inf[df$start & df$adult]) == t)/sum(df$adult)})), NA)
    
    keep$rapid_tp_count[i] <- sum(df$rapid_tp_count)
    keep$rapid_fn_count[i] <- sum(df$rapid_fn_count)
    keep$pcr_tp_count[i] <- sum(df$pcr_tp_count)
    keep$pcr_fn_count[i] <- sum(df$pcr_fn_count)
    
    keep$test_q_eligible[i] <- sum(df$test_q_eligible)
    keep$test_regular_eligible[i] <- sum(df$test_regular_eligible)
    
    # Alyssa's new checks
    keep$seed_kids[i] = sum(df$start.init[!df$adult])
    keep$seed_adults[i] = sum(df$start.init[df$adult])
    keep$start_kids_time[i] = mean(df$t_inf[df$start.init & !df$adult])
    keep$start_adult_time[i] = mean(df$t_inf[df$start.init & df$adult])
    keep$not_inf_start[i] = sum(df$not_inf_keep)
    keep$test_type.check[i] = sum(df$test_type)
    keep$inc_test[i] = sum(df$inc_test & df$vacc)
    keep$vaxxed[i] = sum(df$vacc)
    #keep$test_q_start[i] = sum(df$test_q_keep)
    #print(i)
    
  }
  
  #toc()
  
  return(keep) #keep) #list(keep, mod, class, sched))
}

