context("Testing the regression cofficients estimation")


library(survivalMPLdc)
library(copula)
library(survival)
library(splines2)
library(matrixcalc)
library(testthat)

test_that("Testing the MPL estimated regression coefficients versus the true values", {

##-- Copula types
copula3 <- 'frank'

##-- Marginal distribution for T, C, and A
a <- 2
lambda <- 2
cons7 <- 0.2
cons9 <- 10
tau <- 0.8
betas <- c(-0.5, 0.1)
phis <- c(0.3, 0.2)
distr.ev <- 'weibull'
distr.ce <- 'exponential'

##-- Real marginal baseline hazard for T
ht0b <- a * (seq(0, 5.4, 0.01) ^ (a - 1)) / (lambda ^ a)

##-- Sample size 200
n <- 5000
set.seed(0)
##-- One sample Monte Carlo dataset
cova <- cbind(rbinom(n, 1, 0.5), runif(n, min=-10, max=10))
set.seed(0)
surv <- surv_data_dc(n, a, cova, lambda, betas, phis, cons7, cons9, tau, copula3,
                      distr.ev, distr.ce)
n <- nrow(cova)
p <- ncol(cova)

##-- event and dependent censoring proportions
colSums(surv)[c(2,3)]/n
X <- surv[,1] # Observed time
del<-surv[,2] # failure status
eta<-surv[,3] # dependent censoring status

##-- Inputs
control=coxph_mpl_dc.control(ordSp=4, binCount=2500, tie='No', tau=0.8, copula=copula3,
                               pent='penalty_mspl', smpart='REML', penc='penalty_mspl', smparc='REML',
                               maxit2=100, maxit=10000,
                               cat.smpar='No')

##-- Perform MPL estimation
coxMPLests <- coxph_mpl_dc(surv, cova, control = control, )


##-- Obtain the MPL coefficient estimates
mpl_beta_phi_zp <- rbind( coef(object = coxMPLests, parameter = "beta",),
                          coef(object = coxMPLests, parameter = "phi",)
)


##-- Plot the true and estimated baseline hazards (95% confidence interval) for T
plot(x = coxMPLests, parameter = "theta", funtype="hazard",
     xout = seq(0, 5.4, 0.01), se = TRUE,
     cols=c("blue", "red"), ltys=c(4, 2), type="l", lwd=1, cex=1, cex.axis=1, cex.lab=1,
     xlab="Time", ylab="Hazard",
     xlim=c(0, 5.4), ylim=c(0, 4)
)
par(new=TRUE)
plot(seq(0, 5.4, 0.01), ht0b,
     type="l", col="green",
     lty=1, lwd=1, cex.axis=1, cex.lab=1, xlim=c(0, 5.4), ylim=c(0, 4),
     xlab='Time', ylab='Hazard')
title("MPL Hazard", cex.main=1)
legend( 'topleft',legend = c( "MPL", "95% Confidence Interval", "True"),
        col = c("blue", "red", "green"),
        lty = c(4, 2, 1),
        cex = 1)

expect_equal(
  round( mpl_beta_phi_zp[,1], 8 ),
  c(-0.49690106, 0.09671272, 0.33270738, 0.19981402)
  )

expect_equal(
  round( mpl_beta_phi_zp[,2], 8 ),
  c(0.033302473, 0.003093199, 0.037263795, 0.004401592)
)

})


