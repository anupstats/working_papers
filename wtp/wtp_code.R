

# draw from Poisson distribution and create the cumulative sum for the bus times

buses <- cumsum(rexp(1000, rate=1/10))
# draw from uniform distribution and sort for the person arrival times
arrivals <- sort(runif(1000)*1000*10)

# find out which bus is the next bus for each arrival
nextbus <- sapply(arrivals, function(x) which((buses-x)>=0)[1]) # he will catch all the buses which time is more than x but he will catch the first bus

waiting <- (buses[nextbus]-arrivals)

summary(waiting)


cbind(buses, arrivals, nextbus, waiting)

