setwd("/Users/sdong217/Desktop/COVID_NH/NursingHome/nursing-home/3 - Output")
load('rev_noscreen_5_highvax_70s.RData')

# average cost of all tests + staff time on testing residents
total = mean(out$test_count)*5 + mean(out$test_count_res)*12/60*15.43
total

# average test count
mean(out$test_count)
mean(out$detected)

# average cases detected per 1000 tests
mean(out$detected)/mean(out$test_count)*1000


