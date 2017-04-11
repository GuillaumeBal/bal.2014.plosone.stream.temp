jags.data.1 <- list('wt.data',
                    'at.data',
                    'lfl.data',
                    'n.time.steps.1',
                    'n.time.steps.year.1',
                    'pi.value')

jags.params.1 <- c('alpha.wt', 'beta.wt', 't0.wt', 'sd.wt',
                   'alpha.at', 'beta.at', 't0.at', 'sd.at',
                   'alpha.lfl', 'beta.lfl', 't0.lfl', 'sd.lfl')


model.loc.1 <- "2.ts.shift.estimates/model.t0.estimates.txt"

### inits gen
jags.inits.1 <- function(){
  list(
    # water temp
    "alpha.wt" = quantile(wt.data, probs = runif(1, 0.25, 0.75), na.rm = TRUE),
    "beta.wt" = (quantile(wt.data, probs = 0.975, na.rm = TRUE) - quantile(wt.data, probs = 0.025, na.rm = TRUE)) / 2 + rnorm(1, 1, 1),
    "tau.wt" = runif(1, 1 / (2 ^ 2), 1 / (0.5 ^ 2)),
    # air temp
    "alpha.at" = quantile(at.data, probs = runif(1, 0.25, 0.75), na.rm = TRUE),
    "beta.at" = (quantile(at.data, probs = 0.975, na.rm = TRUE) - quantile(at.data, probs = 0.025, na.rm = TRUE)) / 2 + rnorm(1, 1, 1),
    "tau.at" = runif(1, 1 / (2 ^ 2), 1 / (0.5 ^ 2)),
    # flow
    "alpha.lfl" = quantile(lfl.data, probs = runif(1, 0.25, 0.75), na.rm = TRUE),
    "beta.lfl" = (quantile(lfl.data, probs = 0.975, na.rm = TRUE) - quantile(lfl.data, probs = 0.025, na.rm = TRUE)) / 2 + rnorm(1, .2, 1),
    "tau.lfl" = runif(1, 1 / (1 ^ 2), 1 / (.1 ^ 2))
  )
}

# run model
jags.outputs.1 <- jags(jags.data.1, parameters.to.save = jags.params.1, model.file = model.loc.1, 
                     n.chains = mcmc.chains.1, n.burnin = mcmc.burn.1, n.thin = mcmc.thin.1, n.iter = mcmc.length.1,
                     refresh = mcmc.burn.1 / 20,
                     inits = jags.inits.1,
                     DIC = TRUE)
# print outputs
print(jags.outputs.1)

# put mcmc chains into r memory 
attach.jags(jags.outputs.1)

# visual check of Rhat values 
hist(jags.outputs.1$BUGSoutput$summary[ ,'Rhat'], breaks = length(jags.params.1),
     main = 'Rhat values, issue if values above 1.10')
abline(v = 1.10, col = 'red')

# save summary outputs
write.table(jags.outputs.1$BUGSoutput$summary, 'ts.shift.outputs.txt')

# add rough curves on time series plots
par(mfrow = c(3, 1),
    mar = c(2, 4, 2, 2),
    oma = c(1, .5, 0.5, .5))
plot(wt.data, type = 'l', main = 'Used data')
lines(mean(alpha.wt) + mean(beta.wt) * sin(2 * pi * (1:length(wt.data) + mean(t0.wt)) / n.time.steps.year.1), 
      col = 'red')
plot(at.data, type = 'l')
lines(mean(alpha.at) + mean(beta.at) * sin(2 * pi * (1:length(at.data) + mean(t0.at)) / n.time.steps.year.1), 
      col = 'red')
plot(lfl.data, type = 'l')
lines(mean(alpha.lfl) - mean(beta.lfl) * sin(2 * pi * (1:length(lfl.data) + mean(t0.lfl)) / n.time.steps.year.1), 
      col = 'red')