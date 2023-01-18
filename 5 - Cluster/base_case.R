####************************** SIMULATIONS **************************#### 

# source files
source("functions.R")

# local
wd = "/gpfs/home/sdong12/nursing.home/nursing-home/5 - Cluster/Output"
setwd(wd)

# 5-day
df_ELEM = make_df()


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
tic()
g = run_parallel(df_ELEM, sched, cohorts)
toc()
