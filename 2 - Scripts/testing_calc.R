setwd("/Users/sdong217/Desktop/COVID_NH/NursingHome/nursing-home/3 - Output")
load('screen2xweek_200_highvax.RData')

# average cases detected per 1000 tests
mean(out$detected)/mean(out$test_count)*1000


# average cost of tests on detected cases + staff time on testing residents
detected = mean(out$detected)*5 + mean(out$detected_res)*12/60*15.43
detected

# average cost of all tests + staff time on testing residents
total = mean(out$test_count)*5 + mean(out$test_count_res)*12/60*15.43
total

# ratio of total testing cost to cost spent on detected cases
(total)/(detected)


# average test count
mean(out$test_count)
