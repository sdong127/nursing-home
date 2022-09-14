####************************** SIMULATIONS **************************#### 

# source files
source("functions.R")

# local
wd = "/gpfs/home/sdong12/nursing.home/nursing-home/5 - Cluster/Output"
setwd(wd)

# screening sensitivity
# df_SENS1 = make_df(test_sens = c(.7, .9),
#                    attack = sens.attack, 
#                    child_prob = sens.prob, 
#                    adult_prob = sens.prob) 

# community notification rate
df_SENS = make_df(staff_prob = c(0,20,40,60,80,100,120,140,160,180,200)/100000, 
                  visit_prob = c(0,20,40,60,80,100,120,140,160,180,200)/100000) 

# length of infectious period
# df_SENS3 = make_df(attack = c(.01, .02), days_inf = 10,
#                    child_prob = sens.prob, 
#                    adult_prob = sens.prob)

# frequency of screening
# df_SENS4 = make_df(attack = sens.attack, test = T,
#                    child_prob = sens.prob, 
#                    adult_prob = sens.prob,
#                    test_days = c("2x_week"))

# day of screening
# df_SENS5 = make_df(attack = sens.attack, test = T,
#                    child_prob = sens.prob, 
#                    adult_prob = sens.prob,
#                    test_start_day = c(1:7)) 

# vaccine efficacy
# df_SENS6 = make_df(attack = sens.attack, 
#                    child_prob = sens.prob, 
#                    adult_prob = sens.prob,
#                    vax_eff = c(.5, .7, .9), child_vax = s.vax.sens)

# vax coverage
# df_SENS7 = make_df(attack = sens.attack, 
#                    child_prob = sens.prob, 
#                    adult_prob = sens.prob,
#                    child_vax = c(0, .3, .6, .9))

# don't test unvaccinated
# df_SENS8 = make_df(attack = sens.attack, 
#                    child_prob = sens.prob, 
#                    adult_prob = sens.prob,
#                    no_test_vacc = c(T, F), child_vax = s.vax.sens) 

# rapid test sensitivity
# df_SENS9 = make_df(attack = sens.attack, 
#                    child_prob = sens.prob, 
#                    adult_prob = sens.prob,
#                    rapid_test_sens = c(.6, .8))

# bind them
# df_ELEM = rbind(df_SENS1, df_SENS2, df_SENS3, df_SENS4, df_SENS5, df_SENS6, df_SENS7, df_SENS8, df_SENS9) %>%
df_ELEM = df_SENS %>%
  mutate(i = row_number())

# check dimension
print(dim(df_ELEM))

# make class
set.seed(3232)
start = make_NH(synthpop = synthpop, cohorting = T, visitors = T, temp_staff = T)
nh = initialize_NH(rel_trans_room_symp_res = 1,
                   p_asymp_nonres = 0, p_asymp_res = 0, p_subclin_nonres = 0, p_subclin_res = 0,
                   daily_attack_unvax = .18, daily_attack_vax = .11, staff_vax_req = F, res_vax = 0.65, staff_vax = 0.4, visit_vax = 0.4, 
                   staff_trans_red = 1, visit_trans_red = 1, res_trans_red = 1, 
                   staff_susp_red = 1, visit_susp_red = 1, res_susp_red = 1, 
                   disperse_transmission = T, isolate = T, vax_eff = 0.5, start = start)
sched = make_schedule(time=45, nh = nh)
cohorts = make_room(df = sched)

df_ELEM$n_tot = 1

# run code
tic()
run_parallel(df_ELEM, sched, cohorts)
toc()
