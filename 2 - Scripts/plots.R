library(ggplot2)

load("no_screen.RData")
no_screen = data.table(out)
no_screen_vec = c(mean(no_screen$start, na.rm=T), mean(no_screen$all, na.rm=T))
no_screen_df = data.frame(x=c(1,30), y=no_screen_vec)

load("screen_nonres.RData")
screen_nonres = data.table(out)
screen_nonres_vec = c(mean(screen_nonres$start, na.rm=T), mean(screen_nonres$all, na.rm=T))
screen_nonres_df = data.frame(x=c(1,30), y=screen_nonres_vec)

load("screen_res.RData")
screen_res = data.table(out)
screen_res_vec = c(mean(screen_res$start, na.rm=T), mean(screen_res$all, na.rm=T))
screen_res_df = data.frame(x=c(1,30), y=screen_res_vec)

load("screen_all.RData")
screen_all = data.table(out)
screen_all_vec = c(mean(screen_all$start, na.rm=T), mean(screen_all$all, na.rm=T))
screen_all_df = data.frame(x=c(1,30), y=screen_all_vec)


plot(no_screen_df$x, no_screen_df$y, type = "o",col = "red", xlab = "day", ylab = "no. of infections", 
     main = "Cumulative Infections per month")

lines(screen_nonres_df$x, screen_nonres_df$y, type = "o", col = "blue")

lines(screen_res_df$x, screen_res_df$y, type = "o", col = "black")

lines(screen_all_df$x, screen_all_df$y, type = "o", col = "green")

legend(x="bottomright", legend=c("no screening", "screen residents", "screen staff & visitors", "screen residents, staff, & visitors"),
       col=c("red", "black", "blue", "green"), lty = c(1,1,1,1), cex=0.7)


