## delta well 
n.days <- 24
temps <- 15 + 7 * sin(2 * pi * (1:n.days - 6.50) / n.days)
plot(temps)
temps[1] == temps[24]
temps[1]
temps[24]

# cbind issue
cbind(as.character(raw.data$date[cut.off.series[1:n.time.steps.1 %% 2 != 0]]), 
      wt.data)
