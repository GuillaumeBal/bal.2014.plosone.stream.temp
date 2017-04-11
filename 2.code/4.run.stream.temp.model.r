jags.data.2 <- list(#'wt.data.s',
                    'at.data.s',
                    #'lfl.data.s',
                    'n.t.step.year', 
                    'ind.6m', 'n.6m.windows', 
                    'ac.period.cutoff', 'n.ac.periods',
                    'pi.value',
                    'rho.max',
                    'n.sets.parameters', 'set'
)

jags.params.2 <- c('alpha.at', 'beta.at', 't0.at', 'sigma.at', 'rho.at')


model.loc.2 <- "3.stream.temp.modeling/wt.ts.model.2.txt"

### inits gen
jags.inits.2 <- function(){
  list(
    "alpha.at" = quantile(at.data, probs = runif(n.6m.windows, 0.25, 0.75), na.rm = TRUE),
    "beta.at" = (quantile(at.data, probs = 0.975, na.rm = TRUE) - quantile(at.data, probs = 0.025, na.rm = TRUE)) / 2 + 
      rnorm(n.6m.windows, 1, 1),
    "tau.at" = rep(runif(1, 1 / (2 ^ 2), 1 / (0.5 ^ 2)), n.sets.parameters),
    'rho.at' = runif(n.sets.parameters, 0.2, 0.6) * autocor.incl 
  )
}

# run model
jags.outputs.2 <- jags(jags.data.2, parameters.to.save = jags.params.2, model.file = model.loc.2, 
                       n.chains = mcmc.chains.2, n.burnin = mcmc.burn.2, n.thin = mcmc.thin.2, n.iter = mcmc.length.2,
                       refresh = mcmc.burn.2 / 20,
                       inits = jags.inits.2,
                       DIC = TRUE)

# print outputs
print(jags.outputs.2)

# put mcmc chains into r memory 
attach.jags(jags.outputs.2)

# visual check of Rhat values 
hist(jags.outputs.2$BUGSoutput$summary[ ,'Rhat'], breaks = length(jags.params.2),
     main = 'Rhat values, issue if values above 1.20')
abline(v = 1.20, col = 'red')