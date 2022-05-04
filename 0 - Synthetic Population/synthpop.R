nh_q1 = read.csv("/Users/sdong217/Desktop/COVID_NH/NH synthpop/raw/PBJ_2019_Q1.csv")
nh_q2 = read.csv("/Users/sdong217/Desktop/COVID_NH/NH synthpop/raw/PBJ_2019_Q2.csv")
nh_q3 = read.csv("/Users/sdong217/Desktop/COVID_NH/NH synthpop/raw/PBJ_2019_Q3.csv")
nh_q4 = read.csv("/Users/sdong217/Desktop/COVID_NH/NH synthpop/raw/PBJ_2019_Q4.csv")

nh_1_2 = rbind(nh_q1, nh_q2)
nh_1_2_3 = rbind(nh_1_2, nh_q3)
nh = rbind(nh_1_2_3, nh_q4)

avg_census = mean(nh$mdscensus)
avg_hrs_rn = mean(nh$hrs_rn)
avg_hrs_lpn = mean(nh$hrs_lpn)
avg_hrs_cna = mean(nh$hrs_cna)
avg_hrs_medaide = mean(nh$hrs_medaide)

hprd_rn = avg_hrs_rn/avg_census
hprd_lpn = avg_hrs_lpn/avg_census
hprd_cna = avg_hrs_cna/avg_census
hprd_medaide = avg_hrs_medaide/avg_census