

# draw from exponential distribution and create the cumulative sum for the bus times
buses <- cumsum(rexp(1000, rate=1/10))
# draw from uniform distribution and sort for the person arrival times
arrivals <- sort(runif(1000)*1000*10)

# find out which bus is the next bus for each arrival
nextbus <- sapply(arrivals, function(x) which((buses-x)>=0)[1])
# he will catch all the buses which time is more than x but he will catch the first bus
waiting <- (buses[nextbus]-arrivals)
summary(waiting)


cbind(buses, arrivals, nextbus, waiting)

n = 1500
rate = 1/10
set.seed(25)
ebt<- cumsum(rexp(n, rate = rate))
#bt<-cumsum(rpois(n, lambda =1/rate)) # Bus time
set.seed(25)
at<-sort(runif(n)*max(ebt)) # arrival time
enb<-sapply(at,function(x) which((ebt-x) >= 0)[1]) # next bus
ewt <- ebt[enb]- at # waiting time
summary(ewt)

cbind(ebt,bt,at,ewt,wt)


set.seed(25)
bt<-cumsum(rpois(n, lambda =1/rate)) # Bus time
set.seed(25)
at<-sort(runif(n)*max(bt)) # arrival time
nb<-sapply(at,function(x) which((bt-x) >= 0)[1]) # next bus
wt <- bt[nb]- at # waiting time
summary(wt)


