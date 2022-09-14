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
start = make_NH(synthpop = synthpop, cohorting = T, visitors = T, temp_staff = T)
nh = initialize_NH(rel_trans_room_symp_res = 1,
                   p_asymp_nonres = 0, p_asymp_res = 0, p_subclin_nonres = 0, p_subclin_res = 0,
                   daily_attack_unvax = .18, daily_attack_vax = .11, staff_vax_req = F, res_vax = 0.65, staff_vax = 0.4, visit_vax = 0.4, 
                   staff_trans_red = 1, visit_trans_red = 1, res_trans_red = 1, 
                   staff_susp_red = 1, visit_susp_red = 1, res_susp_red = 1, 
                   disperse_transmission = T, isolate = T, vax_eff = 0.5, start = start)
sched = make_schedule(time=45, nh = nh)
cohorts = make_room(df = sched)


print(detectCores())

df_ELEM$n_tot = 1

# run code
tic()
g = run_parallel(df_ELEM, sched, cohorts)
toc()
