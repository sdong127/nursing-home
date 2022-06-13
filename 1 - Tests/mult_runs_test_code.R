library(data.table)

#Set working directory where model output is stored
##Use the model code in "/2 - Tests/abm_12-4-2021_master.R" to generate the model output -- this code collects additional results used to unit test the model output
setwd("/Users/sdong217/Desktop/COVID_NH/NursingHome/nursing-home/3 - Output/Test Output")

#Bind model output into single data table
filelist <- list.files()
output <- rbindlist(lapply(1:length(filelist), function(a){load(filelist[a]); output = out; return(output)}))

output <- data.table(output)
output$attack = 0.11
output$test = T
visitors = F

#Create summary measures of model output, grouped by model input parameters
output <- output[,.(inf_ct_sympR_R_room.sum = sum(inf_ct_sympR_R_room, na.rm = TRUE), inf_ct_asympR_R_room.sum = sum(inf_ct_asympR_R_room, na.rm = TRUE), inf_ct_sympR_S_room.sum = sum(inf_ct_sympR_S_room, na.rm = TRUE), inf_ct_asympR_S_room.sum = sum(inf_ct_asympR_S_room, na.rm = TRUE), inf_ct_S_S_room.sum = sum(inf_ct_S_S_room, na.rm = TRUE), inf_ct_sympS_R_room.sum = sum(inf_ct_sympS_R_room, na.rm = TRUE), inf_ct_asympS_R_room.sum = sum(inf_ct_asympS_R_room, na.rm = TRUE),
                    inf_ct_sympR_V_room.sum = sum(inf_ct_sympR_V_room, na.rm = TRUE), inf_ct_asympR_V_room.sum = sum(inf_ct_asympR_V_room, na.rm = TRUE), inf_ct_sympS_V_room.sum = sum(inf_ct_sympS_V_room, na.rm = TRUE), inf_ct_asympS_V_room.sum = sum(inf_ct_asympS_V_room, na.rm = TRUE), inf_ct_V_V_room.sum = sum(inf_ct_V_V_room, na.rm = TRUE), inf_ct_sympV_R_room.sum = sum(inf_ct_sympV_R_room, na.rm = TRUE), inf_ct_asympV_R_room.sum = sum(inf_ct_asympV_R_room, na.rm = TRUE), inf_ct_sympV_S_room.sum = sum(inf_ct_sympV_S_room, na.rm = TRUE), inf_ct_asympV_S_room.sum = sum(inf_ct_asympV_S_room, na.rm = TRUE),
                    
                    inf_ct_sympR_R_common.sum = sum(inf_ct_sympR_R_common, na.rm = TRUE), inf_ct_asympR_R_common.sum = sum(inf_ct_asympR_R_common, na.rm = TRUE), inf_ct_sympR_S_common.sum = sum(inf_ct_sympR_S_common, na.rm = TRUE), inf_ct_asympR_S_common.sum = sum(inf_ct_asympR_S_common, na.rm = TRUE), inf_ct_sympS_S_common.sum = sum(inf_ct_sympS_S_common, na.rm = TRUE), inf_ct_asympS_S_common.sum = sum(inf_ct_asympS_S_common, na.rm = TRUE), inf_ct_sympS_R_common.sum = sum(inf_ct_sympS_R_common, na.rm = TRUE), inf_ct_asympS_R_common.sum = sum(inf_ct_asympS_R_common, na.rm = TRUE),
                    inf_ct_R_V_common.sum = sum(inf_ct_R_V_common, na.rm = TRUE), inf_ct_S_V_common.sum = sum(inf_ct_S_V_common, na.rm = TRUE), inf_ct_V_V_common.sum = sum(inf_ct_V_V_common, na.rm = TRUE), inf_ct_V_R_common.sum = sum(inf_ct_V_R_common, na.rm = TRUE), inf_ct_V_S_common.sum = sum(inf_ct_V_S_common, na.rm = TRUE), 
                    
                    inf_ct_R_R_staff.sum = sum(inf_ct_R_R_staff, na.rm = TRUE), inf_ct_R_S_staff.sum = sum(inf_ct_R_S_staff, na.rm = TRUE), inf_ct_sympS_S_staff.sum = sum(inf_ct_sympS_S_staff, na.rm = TRUE), inf_ct_asympS_S_staff.sum = sum(inf_ct_asympS_S_staff, na.rm = TRUE), inf_ct_S_R_staff.sum = sum(inf_ct_S_R_staff, na.rm = TRUE), 
                    inf_ct_R_V_staff.sum = sum(inf_ct_R_V_staff, na.rm = TRUE), inf_ct_S_V_staff.sum = sum(inf_ct_S_V_staff, na.rm = TRUE), inf_ct_V_V_staff.sum = sum(inf_ct_V_V_staff, na.rm = TRUE), inf_ct_V_R_staff.sum = sum(inf_ct_V_R_staff, na.rm = TRUE), inf_ct_V_S_staff.sum = sum(inf_ct_V_S_staff, na.rm = TRUE),
                    
                    risk_ct_sympR_R_room.sum = sum(risk_ct_sympR_R_room, na.rm = TRUE), risk_ct_asympR_R_room.sum = sum(risk_ct_asympR_R_room, na.rm = TRUE), risk_ct_sympR_S_room.sum = sum(risk_ct_sympR_S_room, na.rm = TRUE), risk_ct_asympR_S_room.sum = sum(risk_ct_asympR_S_room, na.rm = TRUE), risk_ct_sympS_R_room.sum = sum(risk_ct_sympS_R_room, na.rm = TRUE), risk_ct_asympS_R_room.sum = sum(risk_ct_asympS_R_room, na.rm = TRUE), 
                    risk_ct_sympR_V_room.sum = sum(risk_ct_sympR_V_room, na.rm = TRUE), risk_ct_asympR_V_room.sum = sum(risk_ct_asympR_V_room, na.rm = TRUE), risk_ct_sympS_V_room.sum = sum(risk_ct_sympS_V_room, na.rm = TRUE), risk_ct_asympS_V_room.sum = sum(risk_ct_asympS_V_room, na.rm = TRUE), risk_ct_sympV_R_room.sum = sum(risk_ct_sympV_R_room, na.rm = TRUE), risk_ct_asympV_R_room.sum = sum(risk_ct_asympV_R_room, na.rm = TRUE), risk_ct_sympV_S_room.sum = sum(risk_ct_sympV_S_room, na.rm = TRUE), risk_ct_asympV_S_room.sum = sum(risk_ct_asympV_S_room, na.rm = TRUE),
                    
                    risk_ct_sympR_R_common.sum = sum(risk_ct_sympR_R_common, na.rm = TRUE), risk_ct_asympR_R_common.sum = sum(risk_ct_asympR_R_common, na.rm = TRUE), risk_ct_sympR_S_common.sum = sum(risk_ct_sympR_S_common, na.rm = TRUE), risk_ct_asympR_S_common.sum = sum(risk_ct_asympR_S_common, na.rm = TRUE), risk_ct_sympS_S_common.sum = sum(risk_ct_sympS_S_common, na.rm = TRUE), risk_ct_asympS_S_common.sum = sum(risk_ct_asympS_S_common, na.rm = TRUE), risk_ct_sympS_R_common.sum = sum(risk_ct_sympS_R_common, na.rm = TRUE), risk_ct_asympS_R_common.sum = sum(risk_ct_asympS_R_common, na.rm = TRUE),
                    
                    risk_ct_sympS_S_staff.sum = sum(risk_ct_sympS_S_staff, na.rm = TRUE), risk_ct_asympS_S_staff.sum = sum(risk_ct_asympS_S_staff, na.rm = TRUE), 
                    
                    length.infectious.low_risk_obs.mean = mean(length.infectious.low_risk_obs, na.rm = TRUE), length.infectious.low_risk_sd.mean = mean(length.infectious.low_risk_sd, na.rm = TRUE), length.infectious.mod_risk_obs.mean = mean(length.infectious.mod_risk_obs, na.rm = TRUE), length.infectious.mod_risk_sd.mean = mean(length.infectious.mod_risk_sd, na.rm = TRUE), length.infectious.high_risk_obs.mean = mean(length.infectious.high_risk_obs, na.rm = TRUE), length.infectious.high_risk_sd.mean = mean(length.infectious.high_risk_sd, na.rm = TRUE), 
                    length.infectious.symp.low_risk_obs.mean = mean(length.infectious.symp.low_risk_obs, na.rm = TRUE), length.infectious.asymp.low_risk_obs.mean = mean(length.infectious.asymp.low_risk_obs, na.rm = TRUE), length.infectious.symp.mod_risk_obs.mean = mean(length.infectious.symp.mod_risk_obs, na.rm = TRUE), length.infectious.asymp.mod_risk_obs.mean = mean(length.infectious.asymp.mod_risk_obs, na.rm = TRUE), length.infectious.symp.high_risk_obs.mean = mean(length.infectious.symp.high_risk_obs, na.rm = TRUE), length.infectious.asymp.high_risk_obs.mean = mean(length.infectious.asymp.high_risk_obs, na.rm = TRUE), 
                    n_res_obs.mean = mean(n_res_obs, na.rm = TRUE), n_dc_staff_obs.mean = mean(n_dc_staff_obs, na.rm = TRUE), n_admin_staff_obs.mean  = mean(n_admin_staff_obs, na.rm = TRUE),
                    n_visit_obs.mean = mean(n_visit_obs, na.rm = TRUE),
                    p_asymp_res_obs.mean = mean(p_asymp_res_obs, na.rm = TRUE), p_asymp_nonres_obs.mean = mean(p_asymp_nonres_obs, na.rm = TRUE), 
                    p_subclin_res_obs.mean = mean(p_subclin_res_obs, na.rm = TRUE), p_subclin_nonres_obs.mean = mean(p_subclin_nonres_obs, na.rm = TRUE),
                    length.incubation_obs.mean = mean(length.incubation_obs, na.rm = TRUE), length.symp.gap_obs.mean = mean(length.symp.gap_obs, na.rm = TRUE),
                    res.vax.rate_obs.mean = mean(res.vax.rate_obs, na.rm = TRUE), staff.vax.rate_obs.mean = mean(staff.vax.rate_obs, na.rm = TRUE), visit.vax.rate_obs.mean = mean(visit.vax.rate_obs, na.rm = TRUE), vax.eff_obs.mean = mean(vax.eff_obs, na.rm = TRUE),
                    res.prob_obs.mean = mean(res.prob_obs, na.rm = TRUE), nonres.prob_obs.mean = mean(nonres.prob_obs, na.rm = TRUE),
                    
                    seed_res.mean = mean(seed_res, na.rm = TRUE), seed_nonres.mean = mean(seed_nonres, na.rm = TRUE), 
                    start_res_time.mean = mean(start_res_time, na.rm = TRUE), start_nonres_time.mean = mean(start_nonres_time, na.rm = TRUE),
                    not_inf_start.mean = mean(not_inf_start, na.rm = TRUE), test_type.check.mean = mean(test_type.check, na.rm = TRUE), vaxxed.mean = mean(vaxxed, na.rm = TRUE),
                    
                    group = paste(p_asymp_nonres, p_asymp_res, p_subclin_nonres, p_subclin_res, mult_asymp_res, mult_asymp_nonres, days_inf, rel_trans_common, vax_eff, attack, res_vax, staff_vax_req, staff_vax, visit_vax, res_susp_red, res_trans_red, staff_susp_red, staff_trans_red, visit_susp_red, visit_trans_red, nonres_prob, test, test_frac, isolate, rel_trans_room_symp_res, rel_trans_staff, test_sens, test_frac, quarantine)),
                 
                 by = .(p_asymp_nonres, p_asymp_res, p_subclin_nonres, p_subclin_res, mult_asymp_res, mult_asymp_nonres, days_inf, rel_trans_common, vax_eff, attack, res_vax, staff_vax_req, staff_vax, visit_vax, res_susp_red, res_trans_red, staff_susp_red, staff_trans_red, visit_susp_red, visit_trans_red, nonres_prob, test, test_frac, isolate, rel_trans_room_symp_res, rel_trans_staff, test_sens, test_frac, quarantine)]

#Create table to compare observed SAR with predicted SAR
##NB: This does not really work with testing yet, since it is difficult to determine a closed-form formula for the predicted SAR
sar.compare <- expand.grid(location = c("Room", "Common area", "Staff interactions"), source = c("res", "staff", "visit"), susp = c("res", "staff", "visit"), isolate = T, symptomatic = c(T,F), quarantine = F, group = output$group) %>% left_join(output, by = "group") %>%
  mutate(expected.sar = attack*
           ifelse(source=="res", res_trans_red, 1)*
           # ifelse(source=="res" & comorbid==0, NA, 1)*
           # ifelse(source=="res" & comorbid==1, res_trans_red*1/2, 1)*
           # ifelse(source=="res" & comorbid==2, res_trans_red*1/4, 1)*
           ifelse(source=="staff", staff_trans_red, 1)*
           ifelse(source=="visit", visit_trans_red, 1)*
           ifelse(source=="res" & !symptomatic, mult_asymp_res, 1)*
           ifelse(source!="res" & !symptomatic, mult_asymp_nonres, 1)*
           ifelse(location == "Room", ifelse(source=="res" & symptomatic, rel_trans_room_symp_res, 1), 1)*
           ifelse(location == "Common area", rel_trans_common*ifelse(source=="res" & symptomatic & !isolate.x, rel_trans_room_symp_res, 1), 1)*
           ifelse(location == "Staff interactions", rel_trans_staff, 1)*
           ifelse(susp=="res", res_susp_red, 1)*
           ifelse(susp=="staff", staff_susp_red, 1)*
           ifelse(susp=="visit", visit_susp_red, 1)*
           ifelse(location=="Room" & quarantine, 0.5, 1)) %>%
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
                                                                           inf_ct_sympR_S_room.sum/risk_ct_sympR_S_room.sum,
                                                                           inf_ct_asympR_S_room.sum/risk_ct_asympR_S_room.sum),
                                                                    ifelse(susp=="visit",
                                                                           ifelse(symptomatic,
                                                                                  inf_ct_sympR_V_room.sum/risk_ct_sympR_V_room.sum,
                                                                                  inf_ct_asympR_V_room.sum/risk_ct_asympR_V_room.sum),
                                                                           observed.sar))),
                                                      ifelse(source=="staff",
                                                             ifelse(susp=="res", 
                                                                    ifelse(symptomatic, 
                                                                           inf_ct_sympS_R_room.sum/risk_ct_sympS_R_room.sum, 
                                                                           inf_ct_asympS_R_room.sum/risk_ct_asympS_R_room.sum),
                                                                    ifelse(susp=="visit",
                                                                           ifelse(symptomatic,
                                                                                  inf_ct_sympS_V_room.sum/risk_ct_sympS_V_room.sum,
                                                                                  inf_ct_asympS_V_room.sum/risk_ct_asympS_V_room.sum),
                                                                           observed.sar)),
                                                             ifelse(source=="visit",
                                                                    ifelse(susp=="res",
                                                                           ifelse(symptomatic,
                                                                                  inf_ct_sympV_R_room.sum/risk_ct_sympV_R_room.sum,
                                                                                  inf_ct_asympV_R_room.sum/risk_ct_asympV_R_room.sum),
                                                                           ifelse(susp=="staff",
                                                                                  ifelse(symptomatic,
                                                                                         inf_ct_sympV_S_room.sum/risk_ct_sympV_S_room.sum,
                                                                                         inf_ct_asympV_S_room.sum/risk_ct_asympV_S_room.sum),
                                                                                  observed.sar)),
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
                                                                           inf_ct_sympR_S_room.sum/risk_ct_sympR_S_room.sum,
                                                                           inf_ct_asympR_S_room.sum/risk_ct_asympR_S_room.sum),
                                                                    observed.sar)),
                                                      ifelse(source=="staff",
                                                             ifelse(susp=="res",
                                                                    ifelse(symptomatic,
                                                                           inf_ct_sympS_R_room.sum/risk_ct_sympS_R_room.sum,
                                                                           inf_ct_asympS_R_room.sum/risk_ct_asympS_R_room.sum),
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
# other.param.compare <- expand.grid(parameter = c("Length of Mild Infectiousness", 
#                                                  "Length of Moderate Infectiousness",
#                                                  "Length of Severe Infectiousness", 
#                                                  "Length of Mild Infectiousness in Room (symptomatic, res/staff)", 
#                                                  "Length of Mild Infectiousness in Room (symptomatic, res/staff)",
#                                                                        "Length of Infectiousness at School (asymptomatic, kid/teacher)",
#                                                                        "Length of Infectiousness at Care (symptomatic, family)", 
#                                                                        "Length of Infectiousness at Care (asymptomatic, family)", 
#                                                                        "Time from exposure to infectiousness", 
#                                                                        "Time from exposure to symptoms", 
#                                                                        "Number of students", 
#                                                                        "Number of teachers", 
#                                                                        "Number of non-classroom staff", 
#                                                                        "Probability of Asymptomatic (adult)", 
#                                                                        "Probability of Asymptomatic (kid)", 
#                                                                        "Probability of Subclinical (adult)", 
#                                                                        "Probability of Subclinical (kid)", 
#                                                                        "Child Vaccination Rate", 
#                                                                        "Teacher Vaccination Rate", 
#                                                                        "Family Vaccination Rate", 
#                                                                        "Community Incidence Rate (kid)", 
#                                                                        "Community Incidence Rate (adult)",
#                                                                        "PCR Sensitivity",
#                                                                        "Rapid Test Sensitivity",
#                                                                        "Regular Test Coverage",
#                                                                        "Test Quarantine Coverage")) %>%
#   relocate(parameter) %>%
#   left_join(output, by = "group") %>%
#   mutate(expected.param = ifelse(parameter == "Length of Infectiousness",
#                                  days_inf,
#                                  ifelse(parameter == "Length of Infectiousness at School (symptomatic, kid/teacher)",
#                                         2*ifelse(test, test_sens*test_frac*(2/7)*0.5 + (1-test_frac*test_sens*(2/7)), 1),
#                                         ifelse(parameter == "Length of Infectiousness at School (asymptomatic, kid/teacher)",
#                                                days_inf*ifelse(test, test_sens*test_frac*(days_inf/7)*0.5 + (1-test_frac*test_sens*(days_inf/7)), 1),
#                                                ifelse(parameter == "Time from exposure to infectiousness",
#                                                       mean(sapply(rgamma(1000, shape = 5.8, scale=.95) - rnorm(1000, mean = 2, sd = 0.4), function(a){max(a,1)})),
#                                                       ifelse(parameter == "Time from exposure to symptoms",
#                                                              mean(rgamma(1000, shape = 5.8, scale=.95)),
#                                                              ifelse(parameter == "Number of students",
#                                                                     ifelse(high_school, 1451, 638),
#                                                                     ifelse(parameter == "Number of teachers",
#                                                                            ifelse(high_school, 4, 6)*n_class,
#                                                                            ifelse(parameter == "Number of non-classroom staff",
#                                                                                   n_staff_obs.mean,
#                                                                                   ifelse(parameter == "Probability of Asymptomatic (adult)",
#                                                                                          p_asymp_adult,
#                                                                                          ifelse(parameter == "Probability of Asymptomatic (kid)",
#                                                                                                 p_asymp_child,
#                                                                                                 ifelse(parameter == "Probability of Subclinical (adult)",
#                                                                                                        p_subclin_adult + p_asymp_adult,
#                                                                                                        ifelse(parameter == "Probability of Subclinical (kid)",
#                                                                                                               p_subclin_child + p_asymp_child,
#                                                                                                               ifelse(parameter == "Child Vaccination Rate",
#                                                                                                                      child.vax,
#                                                                                                                      ifelse(parameter == "Teacher Vaccination Rate",
#                                                                                                                             teacher_susp,
#                                                                                                                             ifelse(parameter == "Family Vaccination Rate",
#                                                                                                                                    family_susp,
#                                                                                                                                    ifelse(parameter == "Community Incidence Rate (kid)",
#                                                                                                                                           child_prob*((1 - child.vax)+child.vax*(1-vax_eff)),
#                                                                                                                                           ifelse(parameter == "Community Incidence Rate (adult)",
#                                                                                                                                                  adult_prob*((1-family_susp)+family_susp*(1-vax_eff)),
#                                                                                                                                                  ifelse(parameter == "Length of Infectiousness at Care (symptomatic, family)",
#                                                                                                                                                         mean(rnorm(1000, mean = 2, sd = 0.4)),
#                                                                                                                                                         ifelse(parameter == "Length of Infectiousness at Care (asymptomatic, family)",
#                                                                                                                                                                days_inf, -99)))))))))))))))))))) %>%
#   mutate(expected.param = ifelse(parameter == "PCR Sensitivity",
#                                  test_sens,
#                                  ifelse(parameter == "Rapid Test Sensitivity",
#                                         rapid_test_sens,
#                                         ifelse(parameter == "Regular Test Coverage",
#                                                test_frac,
#                                                ifelse(parameter == "Test Quarantine Coverage",
#                                                       1,
#                                                       expected.param))))) %>%
#   relocate(expected.param) %>%
#   mutate(obs.param = ifelse(parameter == "Length of Infectiousness",
#                             length.infectious_obs.mean,
#                             ifelse(parameter == "Length of Infectiousness at School (symptomatic, kid/teacher)",
#                                    length.infectious.school.symp.present_obs.mean,
#                                    ifelse(parameter == "Length of Infectiousness at School (asymptomatic, kid/teacher)",
#                                           length.infectious.school.asymp.present_obs.mean,
#                                           ifelse(parameter == "Time from exposure to infectiousness",
#                                                  length.incubation_obs.mean,
#                                                  ifelse(parameter == "Time from exposure to symptoms",
#                                                         length.symp.gap_obs.mean,
#                                                         ifelse(parameter == "Number of students",
#                                                                n_students_obs.mean,
#                                                                ifelse(parameter == "Number of teachers",
#                                                                       n_teachers_obs.mean,
#                                                                       ifelse(parameter == "Number of non-classroom staff",
#                                                                              n_staff_obs.mean,
#                                                                              ifelse(parameter == "Probability of Asymptomatic (adult)",
#                                                                                     p_asymp_adult_obs.mean,
#                                                                                     ifelse(parameter == "Probability of Asymptomatic (kid)",
#                                                                                            p_asymp_child_obs.mean,
#                                                                                            ifelse(parameter == "Probability of Subclinical (adult)",
#                                                                                                   p_subclin_adult_obs.mean,
#                                                                                                   ifelse(parameter == "Probability of Subclinical (kid)",
#                                                                                                          p_subclin_child_obs.mean,
#                                                                                                          ifelse(parameter == "Child Vaccination Rate",
#                                                                                                                 child.vax.rate_obs.mean,
#                                                                                                                 ifelse(parameter == "Teacher Vaccination Rate",
#                                                                                                                        teacher.vax.rate_obs.mean,
#                                                                                                                        ifelse(parameter == "Family Vaccination Rate",
#                                                                                                                               family.vax.rate_obs.mean,
#                                                                                                                               ifelse(parameter == "Community Incidence Rate (kid)",
#                                                                                                                                      child.prob_obs.mean,
#                                                                                                                                      ifelse(parameter == "Community Incidence Rate (adult)",
#                                                                                                                                             adult.prob_obs.mean,
#                                                                                                                                             ifelse(parameter == "Length of Infectiousness at Care (symptomatic, family)",
#                                                                                                                                                    length.infectious.school.symp.present_obs.mean,
#                                                                                                                                                    ifelse(parameter == "Length of Infectiousness at Care (asymptomatic, family)",
#                                                                                                                                                           length.infectious.school.asymp.present_obs.mean,-99)))))))))))))))))))) %>%
#   mutate(obs.param = ifelse(parameter == "PCR Sensitivity",
#                             pcr_test_sens.obs/test_regular_frac.obs,
#                             ifelse(parameter == "Rapid Test Sensitivity",
#                                    rapid_test_sens.obs/test_quarantine_frac.obs,
#                                    ifelse(parameter == "Regular Test Coverage",
#                                           test_regular_frac.obs,
#                                           ifelse(parameter == "Test Quarantine Coverage",
#                                                  test_quarantine_frac.obs,
#                                                  obs.param))))) %>%
#   relocate(obs.param) %>%
#   mutate(rel.diff = obs.param/expected.param) %>%
#   relocate(rel.diff)