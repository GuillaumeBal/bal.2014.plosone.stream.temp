jags.outputs.2$BUGSoutput$summary

scorff.old <- read.table('3.stream.temp.modeling/scorff.results.2014.txt', h = T)

plot(scorff.old[which(substring(rownames(scorff.old), 1, 2) == 'br')[1:24], 1],
     jags.outputs.2$BUGSoutput$summary[1:24, 1])
curve( 1 * x, col = 'red', add = T)


summary(lm(scorff.old[which(substring(rownames(scorff.old), 1, 2) == 'br')[1:24], 1] ~
           jags.outputs.2$BUGSoutput$summary[1:24, 1]))


by(at.data.s, INDICES = ind.6m, mean, na.rm = T)


pb.conv <- which(jags.outputs.2$BUGSoutput$summary[ , 'Rhat'] > 1.1)
jags.outputs.2$BUGSoutput$summary[pb.conv, ]
