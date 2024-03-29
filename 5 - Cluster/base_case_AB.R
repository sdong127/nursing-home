####************************** SIMULATIONS **************************#### 

# source files
setwd("/Users/sdong217/Desktop/COVID_NH/NursingHome/nursing-home/5 - Cluster")
source("functions.R")

# local
#wd = "/gpfs/home/sdong12/nursing.home/nursing-home/5 - Cluster/Output"
# this assumes that I open as an R project
library(here)
wd = here("3 - Output")
print(wd)
setwd(wd)

# 5-day
df_ELEM = make_df()
# 
# # Surveillance
# df_ELEM_SURV = make_df(scenario = c("Base case"), 
#                        test_frac = ifelse(level=="Elementary", c(0.1, 0.2, .9), c(0.2, 0.4, .9)),
#                        attack = c(0.02, 0.04),
#                        child_vax = c(s.child_vax, s.child_vax.vary),
#                        surveillance = T,
# 		       		                       test_quarantine = test_q) %>%
#   filter(!(test_frac==0.9 & attack!=0.04))
# df_ELEM_SURV2 = df_ELEM_SURV %>% mutate(test = F) %>% filter(attack==0.04 & test_frac==0.9)
# 
# # Extra base case to smooth noise
# df_ELEM_ADD = make_df(scenario = c("Base case"), 
#                       child_prob = c(1)*3/100000, adult_prob = c(1)*3/100000,
#                       n_tot = s.n_tot*2, 
#                       test_days = c("week", "2x_week"),
#                       test_frac = c(.7, .9),
#                       child_vax = c(s.child_vax, s.child_vax.vary),
#                       test_quarantine = test_q)
# # A/B
# df_ELEM1 = make_df(scenario = c("A/B (2)"), 
#                    attack = c(.02, .04), 
#                    test = F,
#                    child_vax = c(s.child_vax, s.child_vax.vary),
#                    test_quarantine = test_q)
# 
# # Remote
# df_ELEM2 = make_df(scenario = c("Remote"), 
#                    attack = c(.02, .04),
#                    test = F,
#                    child_vax = c(s.child_vax, s.child_vax.vary),
#                    quarantine.length = 10, 
#                    test_quarantine = test_q, notify = F) 
# 
# # combine data frames
# 
# # only include extras for the elementary level
# if(level=="Elementary"){
#   df_ELEM_temp = rbind(df_ELEM, df_ELEM_SURV, df_ELEM_SURV2, df_ELEM_ADD, df_ELEM1, df_ELEM2)
# } else {df_ELEM_temp = rbind(df_ELEM, df_ELEM_SURV, df_ELEM_SURV2, df_ELEM1, df_ELEM2)}
# 
# # modify variables
# df_ELEM = df_ELEM_temp %>% 
#   mutate(i = row_number()) %>% 
#   
#   # 10 day quarantine without TTS; 7 days for TTS
#   filter((test_quarantine & quarantine.length==7) | (!test_quarantine & quarantine.length==10)) %>%
#   
#   # set additional parameters
#   mutate(isolate = ifelse(test_quarantine, test_q_isolate, 1), 
#          
#          # drop quarantine grace period with test quarantine
#          quarantine.grace = ifelse(test_quarantine, 0, 3),
#          
#          # set vaccine efficacy from main arguments
#          vax_eff = vax_eff_val, 
#          
#          # set rapid test sensitivity based on version
#          rapid_test_sens = ifelse(version==4, .6, .8))
# 
# # check dimension while on cluster
# print(dim(df_ELEM))

# make class
set.seed(3232)
start = make_NH(synthpop = synthpop, cohorting = T, visitors = T, temp_staff = F)
nh = initialize_NH(p_asymp_nonres = 1, p_asymp_res = 1, p_subclin_nonres = 0, p_subclin_res = 0,
                   daily_attack = .18, staff_vax_req = F, 
                   res_boost = 0, staff_boost = 0, visit_boost = 0,
                   staff_trans_red = 0, visit_trans_red = 0, res_trans_red = 0, 
                   staff_susp_red = 0, visit_susp_red = 0, res_susp_red = 0, 
                   isolate = T, 
                   prim_previnf_eff = 0, boost_eff = 0, days_inf = 5,
                   days_isolate_res = 5, days_isolate_staff = 5, days_isolate_visit = 5, start = start)
sched = make_schedule(time=45, nh = nh)
cohorts = make_room(df = sched)


print(detectCores())

df_ELEM$n_tot = 1

# run code
# profiling library
library(profvis)

# wrapper function
tic()
profvis({
g = run_parallel(df_ELEM[1,], sched, cohorts)
})
toc()

# more general function w/simplification
profvis({
  g = sims(df_ELEM, 1, nh = nh, sched = sched)
})


# more general function
profvis({
  sims(df_ELEM, 1, nh = nh, sched = sched)
}, simplify = FALSE)
