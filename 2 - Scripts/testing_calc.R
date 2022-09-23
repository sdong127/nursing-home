load('screen2xweek_highvax_150.RData')

staff_time_week = 1080
staff_time_2xweek = 2160

# average cases detected per 1000 tests
mean(out$detected)/mean(out$test_count)*1000

# average cost of tests on detected cases + staff time on testing residents
detected = mean(out$detected)*5 + mean(out$detected_res)*3
detected
# average cost of all tests + staff time on testing residents
total = mean(out$test_count)*5 + staff_time_2xweek
total
# ratio of total testing cost to cost spent on detected cases
(total)/(detected)

# average test count
mean(out$test_count)
