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



##-- Selecting bin sample or number of knots using AIC vs binCounts plot
binCounts <- c(500, 1000, 1500, 2000, 2500)
bn <- length(binCounts)
aics<-rep(0, bn)
for (j in 1:bn)
{ control=coxph_mpl_dc.control(ordSp = 4,
                               binCount = binCounts[j], tie = 'No',
                               tau = 0.8, copula = copula3,
                               pent = 'penalty_mspl', smpart = 0, penc = 'penalty_mspl', smparc = 0,
                               maxit2 = 100, maxit = 5000,
                               mid = 1, asy = 0, ac = 1, cv = 0,
                               ac.theta = 1e-5, ac.gamma = 1e-5, ac.Utheta = -1e-2, ac.Ugamma = -1e-2,
                               min.theta = 1e-7, min.gamma = 1e-7, min.ht = 1e-7, min.hc = 1e-7,
                               min.St = 1e-7, min.Sc = 1e-7, min.C = 1e-7, min.dC = 1e-7,
                               eps = 1e-5, tol.thga = 1e-5, tol.bph = 1e-5, tol.smpar = 1e-2,
                               cat.smpar = 'No')
aics[j]<-coxph_mpl_dc(surv, cova, control)$mpl_aic
#print(j)
}
binCount <- binCounts[ which.min( aics ) ]
plot(binCounts, aics)

coxMPLests <- coxph_mpl_dc(surv, cova, ordSp=4, binCount=binCount, tie='No', tau=0.8, copula=copula3,
                            pent='penalty_mspl', smpart='REML', penc='penalty_mspl', smparc='REML',
                            maxit2=100, maxit=10000, mid=1, asy=1, ac=1, cv=1,
                            ac.theta=1e-5, ac.gamma=1e-5, ac.Utheta=-1e-2, ac.Ugamma=-1e-2,
                            min.theta=1e-7, min.gamma=1e-7, min.ht=1e-7, min.hc=1e-7,
                            min.St=1e-7, min.Sc=1e-7,
                            min.C=1e-7, min.dC=1e-7, eps=1e-5,
                            tol.thga=1e-5, tol.bph=1e-5, tol.smpar=1e-2,
                            cat.smpar='No'
)
mpl_beta_phi_zp <- coxMPLests$mpl_beta_phi_zp
mpl_h0t <- coxMPLests$mpl_h0t
mpl_h0Ti <- approx( X, mpl_h0t, xout = seq(0, 5.4, 0.01),
                     method="constant", rule = 2, ties = mean)$y


##-- Plot the true and estimated baseline hazards for T
plot(seq(0, 5.4, 0.01), mpl_h0Ti,
     type="l", col="grey", lty=4, lwd=1, cex.axis=1, cex.lab=1, ylim=c(0, 3),
     xlab='Time', ylab='Hazard')
par(new=TRUE)
plot(seq(0, 5.4, 0.01), ht0b,
     type="l", col="green",
     lty=1, lwd=1, cex.axis=1, cex.lab=1, ylim=c(0, 3),
     xlab='Time', ylab='Hazard')
legend(x = 0, y =3,
       col = c("green","grey"), lty = c(1,4), legend = c( "True", "MPL" ),
       cex = 0.5
       )

expect_equal(
  round(aics,2),
  c(12772.98, 12766.93, 12761.98, 12759.81, 12755.97)
)

expect_equal(
  round( mpl_beta_phi_zp[,1], 8 ),
  c(-0.49690106, 0.09671272, 0.33270738, 0.19981402)
  )

expect_equal(
  round( mpl_beta_phi_zp[,2], 8 ),
  c(0.033302473, 0.003093199, 0.037263795, 0.004401592)
)

})


