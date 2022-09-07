library(data.table)

#Set working directory where model output is stored
##Use the model code in "/2 - Tests/abm_12-4-2021_master.R" to generate the model output -- this code collects additional results used to unit test the model output
setwd("/Users/sdong217/Desktop/COVID_NH/NursingHome/nursing-home/3 - Output")

#Bind model output into single data table
filelist <- list.files()
output <- rbindlist(lapply(1:length(filelist), function(a){load(filelist[a]); output = out; return(output)}), fill=T)

output <- data.table(output)
output$daily_attack_vax = 0.18
output$test = T
visitors = T
symptomatic = T

#Create summary measures of model output, grouped by model input parameters
output <- output[,.(inf_ct_sympR_R_room.sum = sum(inf_ct_sympR_R_room, na.rm = TRUE), inf_ct_asympR_R_room.sum = sum(inf_ct_asympR_R_room, na.rm = TRUE), inf_ct_sympR_S_room.sum = sum(inf_ct_sympR_S_room, na.rm = TRUE), inf_ct_asympR_S_room.sum = sum(inf_ct_asympR_S_room, na.rm = TRUE), inf_ct_sympS_R_room.sum = sum(inf_ct_sympS_R_room, na.rm = TRUE), inf_ct_asympS_R_room.sum = sum(inf_ct_asympS_R_room, na.rm = TRUE),
                    # inf_ct_sympR_S_room_quarantine.sum = sum(inf_ct_sympR_S_room_quarantine, na.rm = TRUE), inf_ct_asympR_S_room_quarantine.sum = sum(inf_ct_asympR_S_room_quarantine, na.rm = TRUE), inf_ct_sympS_R_room_quarantine.sum = sum(inf_ct_sympS_R_room_quarantine, na.rm = TRUE), inf_ct_asympS_R_room_quarantine.sum = sum(inf_ct_asympS_R_room_quarantine, na.rm = TRUE),
                    inf_ct_sympR_V_room.sum = sum(inf_ct_sympR_V_room, na.rm = TRUE), inf_ct_asympR_V_room.sum = sum(inf_ct_asympR_V_room, na.rm = TRUE), inf_ct_sympV_R_room.sum = sum(inf_ct_sympV_R_room, na.rm = TRUE), inf_ct_asympV_R_room.sum = sum(inf_ct_asympV_R_room, na.rm = TRUE), 
                    
                    inf_ct_sympR_R_common.sum = sum(inf_ct_sympR_R_common, na.rm = TRUE), inf_ct_asympR_R_common.sum = sum(inf_ct_asympR_R_common, na.rm = TRUE), inf_ct_sympR_S_common.sum = sum(inf_ct_sympR_S_common, na.rm = TRUE), inf_ct_asympR_S_common.sum = sum(inf_ct_asympR_S_common, na.rm = TRUE), inf_ct_sympS_S_common.sum = sum(inf_ct_sympS_S_common, na.rm = TRUE), inf_ct_asympS_S_common.sum = sum(inf_ct_asympS_S_common, na.rm = TRUE), inf_ct_sympS_R_common.sum = sum(inf_ct_sympS_R_common, na.rm = TRUE), inf_ct_asympS_R_common.sum = sum(inf_ct_asympS_R_common, na.rm = TRUE),
                    
                    inf_ct_sympS_S_staff.sum = sum(inf_ct_sympS_S_staff, na.rm = TRUE), inf_ct_asympS_S_staff.sum = sum(inf_ct_asympS_S_staff, na.rm = TRUE), 
                    
                    risk_ct_sympR_R_room.sum = sum(risk_ct_sympR_R_room, na.rm = TRUE), risk_ct_asympR_R_room.sum = sum(risk_ct_asympR_R_room, na.rm = TRUE), risk_ct_sympR_S_room.sum = sum(risk_ct_sympR_S_room, na.rm = TRUE), risk_ct_asympR_S_room.sum = sum(risk_ct_asympR_S_room, na.rm = TRUE), risk_ct_sympS_R_room.sum = sum(risk_ct_sympS_R_room, na.rm = TRUE), risk_ct_asympS_R_room.sum = sum(risk_ct_asympS_R_room, na.rm = TRUE), 
                    # risk_ct_sympR_S_room_quarantine.sum = sum(risk_ct_sympR_S_room_quarantine, na.rm = TRUE), risk_ct_asympR_S_room_quarantine.sum = sum(risk_ct_asympR_S_room_quarantine, na.rm = TRUE), risk_ct_sympS_R_room_quarantine.sum = sum(risk_ct_sympS_R_room_quarantine, na.rm = TRUE), risk_ct_asympS_R_room_quarantine.sum = sum(risk_ct_asympS_R_room_quarantine, na.rm = TRUE),
                    risk_ct_sympR_V_room.sum = sum(risk_ct_sympR_V_room, na.rm = TRUE), risk_ct_asympR_V_room.sum = sum(risk_ct_asympR_V_room, na.rm = TRUE), risk_ct_sympV_R_room.sum = sum(risk_ct_sympV_R_room, na.rm = TRUE), risk_ct_asympV_R_room.sum = sum(risk_ct_asympV_R_room, na.rm = TRUE),
                    
                    risk_ct_sympR_R_common.sum = sum(risk_ct_sympR_R_common, na.rm = TRUE), risk_ct_asympR_R_common.sum = sum(risk_ct_asympR_R_common, na.rm = TRUE), risk_ct_sympR_S_common.sum = sum(risk_ct_sympR_S_common, na.rm = TRUE), risk_ct_asympR_S_common.sum = sum(risk_ct_asympR_S_common, na.rm = TRUE), risk_ct_sympS_S_common.sum = sum(risk_ct_sympS_S_common, na.rm = TRUE), risk_ct_asympS_S_common.sum = sum(risk_ct_asympS_S_common, na.rm = TRUE), risk_ct_sympS_R_common.sum = sum(risk_ct_sympS_R_common, na.rm = TRUE), risk_ct_asympS_R_common.sum = sum(risk_ct_asympS_R_common, na.rm = TRUE),
                    
                    risk_ct_sympS_S_staff.sum = sum(risk_ct_sympS_S_staff, na.rm = TRUE), risk_ct_asympS_S_staff.sum = sum(risk_ct_asympS_S_staff, na.rm = TRUE), 
                    
                    length.infectious.low_risk_obs.mean = mean(length.infectious.low_risk_obs, na.rm = TRUE), length.infectious.low_risk_sd.mean = mean(length.infectious.low_risk_sd, na.rm = TRUE), length.infectious.mod_risk_obs.mean = mean(length.infectious.mod_risk_obs, na.rm = TRUE), length.infectious.mod_risk_sd.mean = mean(length.infectious.mod_risk_sd, na.rm = TRUE), length.infectious.high_risk_obs.mean = mean(length.infectious.high_risk_obs, na.rm = TRUE), length.infectious.high_risk_sd.mean = mean(length.infectious.high_risk_sd, na.rm = TRUE), 
                    length.infectious.symp.low_risk_obs.mean = mean(length.infectious.symp.low_risk_obs, na.rm = TRUE), length.infectious.asymp.low_risk_obs.mean = mean(length.infectious.asymp.low_risk_obs, na.rm = TRUE), length.infectious.symp.mod_risk_obs.mean = mean(length.infectious.symp.mod_risk_obs, na.rm = TRUE), length.infectious.asymp.mod_risk_obs.mean = mean(length.infectious.asymp.mod_risk_obs, na.rm = TRUE), length.infectious.symp.high_risk_obs.mean = mean(length.infectious.symp.high_risk_obs, na.rm = TRUE), length.infectious.asymp.high_risk_obs.mean = mean(length.infectious.asymp.high_risk_obs, na.rm = TRUE), 
                    n_res_obs.mean = mean(n_res_obs, na.rm = TRUE), n_dc_staff_obs.mean = mean(n_dc_staff_obs, na.rm = TRUE), n_admin_staff_obs.mean  = mean(n_admin_staff_obs, na.rm = TRUE),
                    n_visit_obs.mean = mean(n_visit_obs, na.rm = TRUE),
                    p_asymp_res_obs.mean = mean(p_asymp_res_obs, na.rm = TRUE), p_asymp_nonres_obs.mean = mean(p_asymp_nonres_obs, na.rm = TRUE), 
                    p_subclin_res_obs.mean = mean(p_subclin_res_obs, na.rm = TRUE), p_subclin_nonres_obs.mean = mean(p_subclin_nonres_obs, na.rm = TRUE),
                    length.incubation_obs.mean = mean(length.incubation_obs, na.rm = TRUE), length.symp.gap_obs.mean = mean(length.symp.gap_obs, na.rm = TRUE), 
                    length.quarantine_obs.mean = mean(length.quarantine_obs, na.rm = TRUE),
                    res.vax.rate_obs.mean = mean(res.vax.rate_obs, na.rm = TRUE), staff.vax.rate_obs.mean = mean(staff.vax.rate_obs, na.rm = TRUE), visit.vax.rate_obs.mean = mean(visit.vax.rate_obs, na.rm = TRUE), vax.eff_obs.mean = mean(vax.eff_obs, na.rm = TRUE),
                    res.prob_obs.mean = mean(res.prob_obs, na.rm = TRUE), staff.prob_obs.mean = mean(staff.prob_obs, na.rm = TRUE), visit.prob_obs.mean = mean(visit.prob_obs, na.rm = TRUE),
                    
                    test_sens.obs = sum(test_tp_count)/(sum(test_tp_count)+sum(test_fn_count)), test_frac.obs = sum(test_count)/sum(test_eligible),
                    
                    seed_res.mean = mean(seed_res, na.rm = TRUE), seed_nonres.mean = mean(seed_nonres, na.rm = TRUE), 
                    start_res_time.mean = mean(start_res_time, na.rm = TRUE), start_nonres_time.mean = mean(start_nonres_time, na.rm = TRUE),
                    not_inf_start.mean = mean(not_inf_start, na.rm = TRUE), test_type.check.mean = mean(test_type.check, na.rm = TRUE), vaxxed.mean = mean(vaxxed, na.rm = TRUE),
                    
                    group = paste(p_asymp_nonres, p_asymp_res, p_subclin_nonres, p_subclin_res, mult_asymp_res, mult_asymp_nonres, days_inf, rel_trans_common, vax_eff, daily_attack_unvax, daily_attack_vax, res_vax, staff_vax_req, staff_vax, visit_vax, res_susp_red, res_trans_red, staff_susp_red, staff_trans_red, visit_susp_red, visit_trans_red, staff_prob, visit_prob, test, isolate, rel_trans_room_symp_res, rel_trans_staff, test_sens, test_frac, quarantine)),
                 
                 by = .(p_asymp_nonres, p_asymp_res, p_subclin_nonres, p_subclin_res, mult_asymp_res, mult_asymp_nonres, days_inf, rel_trans_common, vax_eff, daily_attack_unvax, daily_attack_vax, res_vax, staff_vax_req, staff_vax, visit_vax, res_susp_red, res_trans_red, staff_susp_red, staff_trans_red, visit_susp_red, visit_trans_red, staff_prob, visit_prob, test, isolate, rel_trans_room_symp_res, rel_trans_staff, test_sens, test_frac, quarantine)]

#Create table to compare observed SAR with predicted SAR
##NB: This does not really work with testing yet, since it is difficult to determine a closed-form formula for the predicted SAR
sar.compare <- expand.grid(location = c("Room", "Common area", "Staff interactions"), source = c("res", "staff", "visit"), susp = c("res", "staff", "visit"), symptomatic = T, quarantine = F, isolate = T, group = output$group) %>% left_join(output, by = "group") %>%
  mutate(expected.sar = ifelse(source=="staff" | source=="visit" | susp=="staff" | susp=="visit" | location=="Common area", 1-(1-daily_attack_vax)^(1/3), daily_attack_vax)*
           ifelse(source=="res", res_trans_red, 1)*
           ifelse(source=="res" & !symptomatic, mult_asymp_res, 1)*
           ifelse(source!="res" & !symptomatic, mult_asymp_nonres, 1)*
           ifelse(source=="staff", staff_trans_red, 1)*
           ifelse(source=="visit", visit_trans_red, 1)*
           ifelse(susp=="res", res_susp_red, 1)*
           ifelse(susp=="staff", staff_susp_red, 1)*
           ifelse(susp=="visit", visit_susp_red, 1)*
           ifelse(location == "Room" & source=="res" & symptomatic, rel_trans_room_symp_res, 1)*
           ifelse(location=="Room" & ((source=="res" & susp=="staff") | (source=="staff" & susp=="res")) & quarantine.x, 0.5, 1)*
           ifelse(location == "Common area", rel_trans_common, 1)*
           ifelse(location=="Common area" & source=="res" & symptomatic & !isolate.x, rel_trans_room_symp_res, 1)*
           ifelse(location == "Staff interactions", rel_trans_staff, 1)
           # ifelse(source=="res" & comorbid==0, NA, 1)*
           # ifelse(source=="res" & comorbid==1, res_trans_red*1/2, 1)*
           # ifelse(source=="res" & comorbid==2, res_trans_red*1/4, 1)*
         ) %>%
  relocate(expected.sar) %>%
  mutate(observed.sar = NA)
if(visitors==T){
  sar.compare = sar.compare %>% mutate(observed.sar = ifelse(location=="Room",
                                                             ifelse(source=="res",
                                                                    ifelse(susp=="res", 
                                                                           ifelse(symptomatic, 
                                                                                  inf_ct_sympR_R_room.sum/risk_ct_sympR_R_room.sum, 
                                                                                  inf_ct_asympR_R_room.sum/risk_ct_asympR_R_room.sum),
                                                                           ifelse(susp=="staff", 
                                                                                  ifelse(symptomatic, 
                                                                                         ifelse(quarantine.x,
                                                                                                inf_ct_sympR_S_room_quarantine.sum/risk_ct_sympR_S_room_quarantine.sum,
                                                                                                inf_ct_sympR_S_room.sum/risk_ct_sympR_S_room.sum),
                                                                                         ifelse(quarantine.x,
                                                                                                inf_ct_asympR_S_room_quarantine.sum/risk_ct_asympR_S_room_quarantine.sum,
                                                                                                inf_ct_asympR_S_room.sum/risk_ct_asympR_S_room.sum)),
                                                                                  ifelse(susp=="visit",
                                                                                         ifelse(symptomatic,
                                                                                                inf_ct_sympR_V_room.sum/risk_ct_sympR_V_room.sum,
                                                                                                inf_ct_asympR_V_room.sum/risk_ct_asympR_V_room.sum),
                                                                                         observed.sar))),
                                                                    ifelse(source=="staff",
                                                                           ifelse(susp=="res", 
                                                                                  ifelse(symptomatic, 
                                                                                         ifelse(quarantine.x,
                                                                                                inf_ct_sympS_R_room_quarantine.sum/risk_ct_sympS_R_room_quarantine.sum,
                                                                                                inf_ct_sympS_R_room.sum/risk_ct_sympS_R_room.sum),
                                                                                         ifelse(quarantine.x,
                                                                                                inf_ct_asympS_R_room_quarantine.sum/risk_ct_asympS_R_room_quarantine.sum,
                                                                                                inf_ct_asympS_R_room.sum/risk_ct_asympS_R_room.sum)),
                                                                                  observed.sar),
                                                                           ifelse(source=="visit",
                                                                                  ifelse(susp=="res",
                                                                                         ifelse(symptomatic,
                                                                                                inf_ct_sympV_R_room.sum/risk_ct_sympV_R_room.sum,
                                                                                                inf_ct_asympV_R_room.sum/risk_ct_asympV_R_room.sum),
                                                                                         observed.sar),
                                                                                  observed.sar))),
                                                             observed.sar))
}else{
  sar.compare = sar.compare %>% mutate(observed.sar = ifelse(location == "Room",
                                                             ifelse(source=="res",
                                                                    ifelse(susp=="res",
                                                                           ifelse(symptomatic,
                                                                                  inf_ct_sympR_R_room.sum/risk_ct_sympR_R_room.sum,
                                                                                  inf_ct_asympR_R_room.sum/risk_ct_asympR_R_room.sum),
                                                                           ifelse(susp=="staff",
                                                                                  ifelse(symptomatic,
                                                                                         ifelse(quarantine.x,
                                                                                                inf_ct_sympR_S_room_quarantine.sum/risk_ct_sympR_S_room_quarantine.sum,
                                                                                                inf_ct_sympR_S_room.sum/risk_ct_sympR_S_room.sum),
                                                                                         ifelse(quarantine.x,
                                                                                                inf_ct_asympR_S_room_quarantine.sum/risk_ct_asympR_S_room_quarantine.sum,
                                                                                                inf_ct_asympR_S_room.sum/risk_ct_asympR_S_room.sum)),
                                                                                  observed.sar)),
                                                                    ifelse(source=="staff",
                                                                           ifelse(susp=="res",
                                                                                  ifelse(symptomatic,
                                                                                         ifelse(quarantine.x,
                                                                                                inf_ct_sympS_R_room_quarantine.sum/risk_ct_sympS_R_room_quarantine.sum,
                                                                                                inf_ct_sympS_R_room.sum/risk_ct_sympS_R_room.sum),
                                                                                         ifelse(quarantine.x,
                                                                                                inf_ct_asympS_R_room_quarantine.sum/risk_ct_asympS_R_room_quarantine.sum,
                                                                                                inf_ct_asympS_R_room.sum/risk_ct_asympS_R_room.sum)),
                                                                                  observed.sar),
                                                                           observed.sar)),
                                                             observed.sar))
}
                                                                
sar.compare = sar.compare %>% mutate(observed.sar = ifelse(location == "Common area",
                                                           ifelse(source=="res",
                                                                  ifelse(susp=="res",
                                                                         ifelse(symptomatic,
                                                                                inf_ct_sympR_R_common.sum/risk_ct_sympR_R_common.sum,
                                                                                inf_ct_asympR_R_common.sum/risk_ct_asympR_R_common.sum),
                                                                         ifelse(susp=="staff",
                                                                                ifelse(symptomatic,
                                                                                       inf_ct_sympR_S_common.sum/risk_ct_sympR_S_common.sum,
                                                                                       inf_ct_asympR_S_common.sum/risk_ct_asympR_S_common.sum),
                                                                                observed.sar)),
                                                                  ifelse(source=="staff",
                                                                         ifelse(susp=="res",
                                                                                ifelse(symptomatic,
                                                                                       inf_ct_sympS_R_common.sum/risk_ct_sympS_R_common.sum,
                                                                                       inf_ct_asympS_R_common.sum/risk_ct_asympS_R_common.sum),
                                                                                ifelse(susp=="staff",
                                                                                       ifelse(symptomatic,
                                                                                              inf_ct_sympS_S_common.sum/risk_ct_sympS_S_common.sum,
                                                                                              inf_ct_asympS_S_common.sum/risk_ct_asympS_S_common.sum),
                                                                                       observed.sar)),
                                                                         observed.sar)),
                                                           ifelse(location == "Staff interactions",
                                                                  ifelse(source=="staff",
                                                                         ifelse(susp=="staff",
                                                                                ifelse(symptomatic,
                                                                                       inf_ct_sympS_S_staff.sum/risk_ct_sympS_S_staff.sum,
                                                                                       inf_ct_asympS_S_staff.sum/risk_ct_asympS_S_staff.sum),
                                                                                observed.sar),
                                                                         observed.sar),
                                                                  observed.sar)),
                                     rel.diff = observed.sar/expected.sar) %>%
  relocate(observed.sar) %>%
  relocate(rel.diff)

# test later
#Create table to compare observed parameters with input parameters SAR
other.param.compare <- expand.grid(group = output$group, parameter = c("Length of Infectiousness (low risk)",
                                                                       "Length of Infectiousness (moderate risk)",
                                                                       "Length of Infectiousness (high risk)",
                                                                       "Length of Infectiousness (symptomatic, low risk)",
                                                                       "Length of Infectiousness (symptomatic, moderate risk)",
                                                                       "Length of Infectiousness (symptomatic, high risk)",
                                                                       "Length of Infectiousness (asymptomatic, low risk)",
                                                                       "Length of Infectiousness (asymptomatic, moderate risk)",
                                                                       "Length of Infectiousness (asymptomatic, high risk)",
                                                                       "Time from exposure to infectiousness",
                                                                       "Time from exposure to symptoms",
                                                                       "Number of Residents",
                                                                       "Number of Direct-Care Staff",
                                                                       "Number of Administrative Staff",
                                                                       "Number of Visitors",
                                                                       "Probability of Asymptomatic (resident)",
                                                                       "Probability of Asymptomatic (non-resident)",
                                                                       "Probability of Subclinical (resident)",
                                                                       "Probability of Subclinical (non-resident)",
                                                                       "Resident Vaccination Rate",
                                                                       "Staff Vaccination Rate",
                                                                       "Visitor Vaccination Rate",
                                                                       "Community Incidence Rate (staff)",
                                                                       "Community Incidence Rate (visitor)",
                                                                       "Rapid Test Sensitivity",
                                                                       "Test Coverage")) %>%
  relocate(parameter) %>%
  left_join(output, by = "group")
if(visitors==T){
  other.param.compare = other.param.compare %>% mutate(expected.param = ifelse(parameter == "Length of Infectiousness (low risk)",
                                                                               days_inf,
                                                                               ifelse(parameter == "Length of Infectiousness (moderate risk)",
                                                                                      days_inf,
                                                                                      ifelse(parameter == "Length of Infectiousness (high risk)",
                                                                                             days_inf,
                                                                                             ifelse(parameter == "Time from exposure to infectiousness",
                                                                                                    mean(sapply(rgamma(263, shape = 5.8, scale=.95) - rnorm(263, mean = 2, sd = 0.4), function(a){max(a,1)})),
                                                                                                    ifelse(parameter == "Time from exposure to symptoms",
                                                                                                           mean(rgamma(263, shape = 5.8, scale=.95)),
                                                                                                           ifelse(parameter == "Number of Residents",
                                                                                                                  90,
                                                                                                                  ifelse(parameter == "Number of Direct-Care Staff",
                                                                                                                         63,
                                                                                                                         ifelse(parameter == "Number of Administrative Staff",
                                                                                                                                20,
                                                                                                                                ifelse(parameter == "Number of Visitors",
                                                                                                                                       90,
                                                                                                                                       ifelse(parameter == "Probability of Asymptomatic (resident)",
                                                                                                                                              p_asymp_res,
                                                                                                                                              ifelse(parameter == "Probability of Asymptomatic (non-resident)",
                                                                                                                                                     p_asymp_nonres,
                                                                                                                                                     ifelse(parameter == "Probability of Subclinical (resident)",
                                                                                                                                                            p_subclin_res + p_asymp_res,
                                                                                                                                                            ifelse(parameter == "Probability of Subclinical (non-resident)",
                                                                                                                                                                   p_subclin_nonres + p_asymp_nonres,
                                                                                                                                                                   ifelse(parameter == "Resident Vaccination Rate",
                                                                                                                                                                          res_vax,
                                                                                                                                                                          ifelse(parameter == "Staff Vaccination Rate",
                                                                                                                                                                                 staff_vax,
                                                                                                                                                                                 ifelse(parameter == "Visitor Vaccination Rate",
                                                                                                                                                                                        visit_vax,
                                                                                                                                                                                        ifelse(parameter == "Community Incidence Rate (staff)",
                                                                                                                                                                                               staff_prob*((1 - staff_vax)+staff_vax*(1-vax_eff)),
                                                                                                                                                                                               ifelse(parameter == "Community Incidence Rate (visitor)",
                                                                                                                                                                                                      visit_prob*((1-visit_vax)+visit_vax*(1-vax_eff)),
                                                                                                                                                                                                      -99)))))))))))))))))))
                                                                                                                                          # ifelse(parameter == "Length of Infectiousness in Staff Interactions (symptomatic, staff)",
                                                                                                                                          #        mean(rnorm(83, mean = 2, sd = 0.4)),
                                                                                                                                          #        ifelse(parameter == "Length of Infectiousness in Staff Interactions (asymptomatic, staff)",
                                                                                                                                          #               days_inf, -99))
}else{
    other.param.compare = other.param.compare %>% mutate(expected.param = ifelse(parameter == "Length of Infectiousness (low risk)",
                                                                                 days_inf,
                                                                                 ifelse(parameter == "Length of Infectiousness (moderate risk)",
                                                                                        days_inf,
                                                                                        ifelse(parameter == "Length of Infectiousness (high risk)",
                                                                                               days_inf,
                                                                                               ifelse(parameter == "Time from exposure to infectiousness",
                                                                                                      mean(sapply(rgamma(173, shape = 5.8, scale=.95) - rnorm(173, mean = 2, sd = 0.4), function(a){max(a,1)})),
                                                                                                      ifelse(parameter == "Time from exposure to symptoms",
                                                                                                             mean(rgamma(173, shape = 5.8, scale=.95)),
                                                                                                             ifelse(parameter == "Number of Residents",
                                                                                                                    90,
                                                                                                                    ifelse(parameter == "Number of Direct-Care Staff",
                                                                                                                           63,
                                                                                                                           ifelse(parameter == "Number of Administrative Staff",
                                                                                                                                  20,
                                                                                                                                  ifelse(parameter == "Number of Visitors",
                                                                                                                                         0,
                                                                                                                                         ifelse(parameter == "Probability of Asymptomatic (resident)",
                                                                                                                                                p_asymp_res,
                                                                                                                                                ifelse(parameter == "Probability of Asymptomatic (non-resident)",
                                                                                                                                                       p_asymp_nonres,
                                                                                                                                                       ifelse(parameter == "Probability of Subclinical (resident)",
                                                                                                                                                              p_subclin_res + p_asymp_res,
                                                                                                                                                              ifelse(parameter == "Probability of Subclinical (non-resident)",
                                                                                                                                                                     p_subclin_nonres + p_asymp_nonres,
                                                                                                                                                                     ifelse(parameter == "Resident Vaccination Rate",
                                                                                                                                                                            res_vax,
                                                                                                                                                                            ifelse(parameter == "Staff Vaccination Rate",
                                                                                                                                                                                   staff_vax,
                                                                                                                                                                                   ifelse(parameter == "Visitor Vaccination Rate",
                                                                                                                                                                                          0,
                                                                                                                                                                                          ifelse(parameter == "Community Incidence Rate (staff)",
                                                                                                                                                                                                 staff_prob*((1 - staff_vax)+staff_vax*(1-vax_eff)),
                                                                                                                                                                                                 ifelse(parameter == "Community Incidence Rate (visitor)",
                                                                                                                                                                                                        NA, -99)))))))))))))))))))
                                                                                                                                            # ifelse(parameter == "Length of Infectiousness in Staff Interactions (symptomatic, staff)",
                                                                                                                                            #        mean(rnorm(83, mean = 2, sd = 0.4)),
                                                                                                                                            #        ifelse(parameter == "Length of Infectiousness in Staff Interactions (asymptomatic, staff)",
                                                                                                                                            #               days_inf, -99))
}
other.param.compare = other.param.compare %>% mutate(expected.param = ifelse(parameter == "Rapid Test Sensitivity",
                                                                             test_sens,
                                                                             ifelse(parameter == "Test Coverage",
                                                                                    test_frac,
                                                                                    expected.param))) %>%
  relocate(expected.param) %>%
  mutate(obs.param = ifelse(parameter == "Length of Infectiousness (low risk)",
                            length.infectious.low_risk_obs.mean,
                            ifelse(parameter == "Length of Infectiousness (moderate risk)",
                                   length.infectious.mod_risk_obs.mean,
                                   ifelse(parameter == "Length of Infectiousness (high risk)",
                                          length.infectious.high_risk_obs.mean,
                                          ifelse(parameter == "Time from exposure to infectiousness",
                                                 length.incubation_obs.mean,
                                                 ifelse(parameter == "Time from exposure to symptoms",
                                                        length.symp.gap_obs.mean,
                                                        ifelse(parameter == "Number of Residents",
                                                               n_res_obs.mean,
                                                               ifelse(parameter == "Number of Direct-Care Staff",
                                                                      n_dc_staff_obs.mean,
                                                                      ifelse(parameter == "Number of Administrative Staff",
                                                                             n_admin_staff_obs.mean,
                                                                             ifelse(parameter == "Number of Visitors",
                                                                                    n_visit_obs.mean,
                                                                                    ifelse(parameter == "Probability of Asymptomatic (resident)",
                                                                                           p_asymp_res_obs.mean,
                                                                                           ifelse(parameter == "Probability of Asymptomatic (non-resident)",
                                                                                                  p_asymp_nonres_obs.mean,
                                                                                                  ifelse(parameter == "Probability of Subclinical (resident)",
                                                                                                         p_subclin_res_obs.mean,
                                                                                                         ifelse(parameter == "Probability of Subclinical (non-resident)",
                                                                                                                p_subclin_nonres_obs.mean,
                                                                                                                ifelse(parameter == "Resident Vaccination Rate",
                                                                                                                       res.vax.rate_obs.mean,
                                                                                                                       ifelse(parameter == "Staff Vaccination Rate",
                                                                                                                              staff.vax.rate_obs.mean,
                                                                                                                              ifelse(parameter == "Visitor Vaccination Rate",
                                                                                                                                     visit.vax.rate_obs.mean,
                                                                                                                                     ifelse(parameter == "Community Incidence Rate (staff)",
                                                                                                                                            staff.prob_obs.mean,
                                                                                                                                            ifelse(parameter == "Community Incidence Rate (visitor)",
                                                                                                                                                   visit.prob_obs.mean,
                                                                                                                                                   -99))))))))))))))))))) %>%
                                                                                                                                            # ifelse(parameter == "Length of Infectiousness at Care (symptomatic, family)",
                                                                                                                                            #        length.infectious.school.symp.present_obs.mean,
                                                                                                                                            #        ifelse(parameter == "Length of Infectiousness at Care (asymptomatic, family)",
                                                                                                                                            #               length.infectious.school.asymp.present_obs.mean,-99)) 
  mutate(obs.param = ifelse(parameter == "Rapid Test Sensitivity",
                            test_sens.obs/test_frac.obs,
                                   ifelse(parameter == "Test Coverage",
                                          test_frac.obs,
                                          obs.param))) %>%
  relocate(obs.param) %>%
  mutate(rel.diff = obs.param/expected.param) %>%
  relocate(rel.diff)

