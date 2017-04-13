# old scorff results
scorff.old <- read.table('3.stream.temp.modeling/scorff.results.2014.txt', h = T)

# plot then vs new fit
plot(jags.outputs.2$BUGSoutput$summary[1:24, 1], type = 'l')
#plot(lowess(jags.outputs.2$BUGSoutput$summary[1:24, 1], f = 0.6), type = 'l')
plot(scorff.old[which(substring(rownames(scorff.old), 1, 2) == 'br')[1:24], 1],
     jags.outputs.2$BUGSoutput$summary[1:24, 1])
curve( 1 * x, col = 'red', add = T)

# check parameters corresponfing linear model
lm.comp <- lm(scorff.old[which(substring(rownames(scorff.old), 1, 2) == 'br')[1:24], 1] ~
                jags.outputs.2$BUGSoutput$summary[1:24, 1])
summary(lm.comp)

curve(lm.comp$coefficients[1] + lm.comp$coefficients[2] * x , add = TRUE, col = 'blue')

# calculate some means
by(at.data.s, INDICES = ind.6m, mean, na.rm = T)

# check prameters with convergence issues
pb.conv <- which(jags.outputs.2$BUGSoutput$summary[ , 'Rhat'] > 1.1)
jags.outputs.2$BUGSoutput$summary[pb.conv, ]

matplot(t0.lfl, type = 'l')