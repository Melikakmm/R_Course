#EX5 
#Melika keshavarzmirzamohammdi
#2054633

#------------------------------------------------------------------------------
#Q1

#--------a---------- 
#uniform prior
#the posterior for multiple measurements is a gamma disribution with:
data <- c(4, 1, 3, 1, 3)
alpha = 13
lambda = 5
mu = seq(0, 5, by = 1/10)
post_U = dgamma(mu, shape = alpha , rate = lambda);post_U
#plot1
plot(mu, post_U, main= 'The Gamma Post With Uniform Prior',
     xlab = 'Poisson Rate(Mu)', ylab = 'Post For Mu',
     col = 'salmon1', type = 'l', xlim = c(0, 10))

#The mean, median, and variance value:

#numerically

E_N_U = weighted.mean(mu, post_U); E_N_U #the mean
E_2_N_U = weighted.mean(mu^2, post_U); E_2_N_U #second moment
var_N_U = E_2_N_U - (E_N_U)^2; var_N_U#the variance
mediam_N_U<-median(mu)#the median


#analytically

E_A_U<-integrate(function(x){x*dgamma(x,shape=alpha, rate = lambda)}, -Inf, Inf);E_A_U#the mean
E_2_A_U<-integrate(function(x){x^2*dgamma(x,shape=alpha, rate = lambda)}, -Inf, Inf);E_2_A_U#second moment
var_A_U <- E_2_A_U$value - (E_A_U$value)^2;var_A_U#the variance
mediam_N_U = qgamma(0.5, shape = alpha, rate = lambda); mediam_N_U #the median

#----------b--------------
#again the posterior is gamma since the Jef prior is a member of Gamma conjugate family.
alpha = 12.5
lambda = 5

mu = seq(0, 5, by = 1/10)
post = dgamma(mu, shape = alpha , rate = lambda)
#plot2
plot(mu, post, main= 'The Gamma Post With Jeff Prior',
     xlab = 'Poisson Rate(Mu)', ylab = 'Post For Mu',
     col = 'cyan3', type = 'l', xlim = c(0, 10))

#second part of question:

#numerically

E_N_J = weighted.mean(mu, post); E_N_J #the mean
E_2_N_J = weighted.mean(mu^2, post); E_2_N_J #second moment
var_N_J = E_2_N_J - (E_N_J)^2; var_N_J#the variance
mediam_N_J<-median(mu)#the median


#analytically

E_A_J<-integrate(function(x){x*dgamma(x,shape=alpha, rate = lambda)}, -Inf, Inf);E_A_J#the mean
E_2_A_J<-integrate(function(x){x^2*dgamma(x,shape=alpha, rate = lambda)}, -Inf, Inf);E_2_A_J#second moment
var_A_J <- E_2_A_J$value - (E_A_J$value)^2;var_A_J#the variance
mediam_N_J <- qgamma(0.5, shape = alpha, rate = lambda); mediam_N_J #the median

#---------c-------------

#95% credibility 

#for the posterior with uniform prior:

alpha = 13
lambda = 5
low_U = qgamma(0.025, shape =alpha, rate = lambda );low_U
high_U = qgamma(0.975, shape = alpha, rate = lambda); high_U
#similar normal dis:
post_N_U <- dnorm(mu, mean= E_A_U$value, sd = sqrt(var_A_U));post_N_U
low_U_N = qnorm(0.025, mean = E_A_U$value, sd = sqrt(var_A_U));low_U_N
high_U_N = qnorm(0.975, mean = E_A_U$value, sd = sqrt(var_A_U));high_U_N

#for the posterior with Jeff prior:


alpha = 12.5
lambda = 5
low_J = qgamma(0.025, shape =alpha, rate = lambda );low_J
high_J = qgamma(0.975, shape = alpha, rate = lambda); high_J
#similar normal dis:
post_N_J <- dnorm(mu, mean = E_A_J$value,sd = sqrt(var_A_J));post_N_J
low_J_N = qnorm(0.025, mean = E_A_J$value, sd = sqrt(var_A_J));low_J_N
high_J_N = qnorm(0.975, mean = E_A_J$value, sd = sqrt(var_A_J));high_J_N



#plot3
par(mfrow = c(2, 1))
plot(mu, post_U, col = 'red', main = 'The comparison of credibility intervals',
     type = 'l', lwd = 2, xlab = 'MU', ylab = 'pdf', ylim = c(0, 1.5))
abline(v = low_U, col = 'red', lwd = 2, lty = 2)
abline(v = high_U, col = 'red', lwd = 2, lty = 2)
lines(mu,post_N_U, col = 'pink', lwd = 2)
abline(v = low_U_N, col ='pink', lty = 2, lwd = 2 )
abline(v = high_U_N, col = 'pink', lty = 2, lwd = 2)
mtext('Posterior with unif prior and a normal dis with similar properties',
      side = 3, line = 3, font = 1)
plot(mu, post, col = 'purple', lwd = 2, type = 'l', xlab = 'MU',
     ylab = 'pdf', ylim = c(0, 1.5))
abline(v = low_J, col = 'purple', lwd = 2, lty = 2)
abline(v = high_J, col = 'purple', lwd = 2, lty = 2)
lines(mu, post_N_J, col = 'blue', lwd = 2)
abline(v = low_J_N, col = 'blue', lwd = 2, lty = 2)
abline(v = high_J_N, col = 'blue', lwd = 2, lty = 2)
mtext('Posterior with Jef prior and an normal dis with similar properties',
      line = 1, side = 3, font = 1)


#------------------------------------------------------------------------------
#EX2
#p(alpha . beta | D) = p(alpha| D) p(beta | D)
#prior dis for both parameters are considered to be uniform.

p.log.like <- function(data, a, b){
  logL <- 0.0
  for(x in data){
    logL <- logL -log(1 + ((x -a)/b)^2)
  }
  return(logL)
}

#Alpha
n.sample <- 200
x.min <- -6; x.max <- 6
h <- (x.max - x.min)/n.sample
alpha <- seq(from = x.min, by = h, length.out = n.sample + 1)

#Beta
b.min = 1; b.max = 6
hb <- (b.max -b.min )/ n.sample
beta <- seq(from = b.min, by = hb, length.out = n.sample + 1)

#the data and its dimenstion:
#I sampled from the inverse for of data distribution for individual measurement.
x <- runif(200, min = 1.5, max = 1.7)
inverse <-  function(x){
  y <- (( (exp(x)/pi) -1)^0.5) + 1
  return(y)
}

data <- inverse(x);data
str.dim <- readline('Enter data set dimension: ')
dim <- as.numeric(unlist(strsplit(str.dim, ',')))
dt <- data[1:dim]

#calculating max for alpha:
#*we put the beta = 1 and make it constant to get the max of alpha. 
log.star_alpha <- p.log.like(dt, alpha, 1)#this function is for the posterior of alpha
index.max.alpha <- which.max(log.star_alpha)
alpha.max <- alpha[index.max.alpha]
cat('The max of alpha is :', alpha.max)

#calculating max for beta:
log.star_beta <- p.log.like(dt, 1, beta)#this function is for the posterior of beta
index.max.beta <- which.max(log.star_beta)
beta.max <- beta[index.max.beta]
cat('The max of beta is :', beta.max)





#exp and normalization both posts:
post.star_alpha <- exp(log.star_alpha)
post.star_beta <- exp(log.star_beta)
y.post_alpha <- post.star_alpha /(h*sum(post.star_alpha))
y.post_beta <- post.star_beta /(hb*sum(post.star_beta))


#we want p(alpha .beta | D):
P = y.post_alpha * y.post_beta;P

#plot4
par(mfrow = c(3, 1))
plot(alpha, y.post_alpha, type = 'l', lwd = 2, col = 'plum1', xlab ='alpha')
mtext('Posterior for alpha, n = 100:', line = 2, side = 3)
plot(beta , y.post_beta, lwd = 2, col = 'salmon3', type = 'l')
mtext('posterior for beta, n= 100:', line = 2, side = 3 )
plot(alpha * beta, P, lwd = 2, col = 'gold', type = 'l')
mtext('posterior for alpha and beta, n = 100:', line = 2, side = 3)

#every time I calculated everything by different numbers of collected data, and 
#I stuck the plots all together by par:

#plot5
par(mfrow = c(3, 4))
plot(alpha * beta, P, lwd = 2, col = 'tomato1', type = 'l', main = 'n = 100')
mean.dt = mean(dt)
abline(v = mean.dt, lty = 2, lwd = 2, col = 'forestgreen')
#-------------------------------------------------------------------------------

#EX3

#----------------a-----------------
#Generative model
signal <- function(x, a, b, x0, w, t) {
  t* (a* exp(-(x-x0)^2/(2*w^2)) + b)
}

#parameters

x0 <- 0
A.true <- 2
B.true <- 1
Delta.t <- 5


#The data
#I increased the resolution of data.
#I should keep the sample range intact too.
w <- c(0.1, 0.25, 1, 2, 3)

xdat_1 <- seq(from = -70*w[1], to = 70*w[1], by = 0.5*w[1])
xdat_2 <- seq(from = -28*w[2], to = 70*w[2], by = 0.5*w[2])
xdat_3 <- seq(from = - 7*w[3], to =  7*w[3], by = 0.5*w[3])
xdat_4 <- seq(from =-3.5*w[4], to =3.5*w[4], by = 0.5*w[4])
xdat_5 <- seq(from =-7/3*w[5], to =7/3*w[5], by = 0.5*w[5])






s.true_1 <- signal(xdat_1, A.true, B.true, x0, w[1], Delta.t)
s.true_2 <- signal(xdat_2, A.true, B.true, x0, w[2], Delta.t)
s.true_3 <- signal(xdat_3, A.true, B.true, x0, w[3], Delta.t)
s.true_4 <- signal(xdat_4, A.true, B.true, x0, w[4], Delta.t)
s.true_5 <- signal(xdat_5, A.true, B.true, x0, w[5], Delta.t)


ddat_1 <- rpois(length(s.true_1), s.true_1)
ddat_2 <- rpois(length(s.true_2), s.true_2)
ddat_3 <- rpois(length(s.true_3), s.true_3)
ddat_4 <- rpois(length(s.true_4), s.true_4)
ddat_5 <- rpois(length(s.true_5), s.true_5)



xplot_1 <- seq(from = min(xdat_1), to = max(xdat_1), by = 0.05*w[1])
xplot_2 <- seq(from = min(xdat_2), to = max(xdat_2), by = 0.05*w[2])
xplot_3 <- seq(from = min(xdat_3), to = max(xdat_3), by = 0.05*w[3])
xplot_4 <- seq(from = min(xdat_4), to = max(xdat_4), by = 0.05*w[4])
xplot_5 <- seq(from = min(xdat_5), to = max(xdat_5), by = 0.05*w[5])



splot_1 <- signal(xplot_1, A.true, B.true, x0, w[1], Delta.t)
splot_2 <- signal(xplot_2, A.true, B.true, x0, w[2], Delta.t)
splot_3 <- signal(xplot_3, A.true, B.true, x0, w[3], Delta.t)
splot_4 <- signal(xplot_4, A.true, B.true, x0, w[4], Delta.t)
splot_5 <- signal(xplot_5, A.true, B.true, x0, w[5], Delta.t)




#plotting signals with different resolutions:
#plot6
par(mfrow = c(3, 2))
xdat.off_1 <- xdat_1 -0.25
plot(xdat.off_1 , ddat_1, type = 's', col = 'red', lwd = 1, xlim = range(xplot_1),
     ylim = range(c(splot_1, ddat_1)), main = 'w = 0.1',
     xlab = 'x', ylab = 'signal background counts')
lines(xplot_1 , splot_1, col = 'forestgreen')
mtext('Here we changed the resolution of the data for 5 different w:', side= 3, line = 2.5,
      font = 1)

xdat.off_2 <- xdat_2 -0.25
plot(xdat.off_2 , ddat_2, type = 's', col = 'red', lwd = 1, xlim = range(xplot_2),
     ylim = range(c(splot_2, ddat_2)), main = 'w = 0.25',
     xlab = 'x', ylab = 'signal background counts')
lines(xplot_2 , splot_2, col = 'forestgreen')


xdat.off_3 <- xdat_3 - 0.25
plot(xdat.off_3 , ddat_3, type = 's', col = 'red', lwd = 1, xlim = range(xplot_3),
     ylim = range(c(splot_3, ddat_3)), main = 'w = 1',
     xlab = 'x', ylab = 'signal background counts')
lines(xplot_3 , splot_3, col = 'forestgreen')

xdat.off_4 <- xdat_4 - 0.25
plot(xdat.off_4 , ddat_4, type = 's', col = 'red', lwd = 1, xlim = range(xplot_4),
     ylim = range(c(splot_4, ddat_4)), main = 'w = 2',
     xlab = 'x', ylab = 'signal background counts')
lines(xplot_4 , splot_4, col = 'forestgreen')


xdat.off_5 <- xdat_5 - 0.25 
plot(xdat.off_5 , ddat_5, type = 's', col = 'red', lwd = 1, xlim = range(xplot_5),
     ylim = range(c(splot_5, ddat_5)), main = 'w = 3',
     xlab = 'x', ylab = 'signal background counts')
lines(xplot_5 , splot_5, col = 'forestgreen')

#---------------b-------------

#I changed the ratio A/B to 1, 2, 10, 25.
A <- c(1, 4, 30, 100)
B <- c(1, 2, 3, 4)
x0 <- 0
w<- 1
Delta.t <- 5
set.seed(205)




#A/B = 1
xdat <- seq(from = - 7*w, to =  7*w, by = 0.5*w)
s.true_AB_1 <- signal(xdat, A[1], B[1], x0, w, Delta.t)
ddat <- rpois(length(s.true_AB_1), s.true_AB_1)

xplot <- seq(from = min(xdat), to = max(xdat), by = 0.05*w)
splot <- signal(xplot, A[1], B[1], x0, w, Delta.t)




#plot7:


#A/B = 1
par(mfrow = c(2, 2))
xdat.off <- xdat - 0.25
#plot
plot(xdat.off , ddat, type = 's', col = 'cyan3', lwd = 1, xlim = range(xplot),
     ylim = range(c(splot, ddat)), xlab = 'x', ylab = 'Signal background counts',
     main = 'A/B = 1')
lines(xplot, splot, col = 'purple')



#A/B= 2
xdat <- seq(from = - 7*w, to =  7*w, by = 0.5*w)
s.true_AB_2 <- signal(xdat, A[2], B[2], x0, w, Delta.t)
ddat <- rpois(length(s.true_AB_2), s.true_AB_2)
xplot <- seq(from = min(xdat), to = max(xdat), by = 0.05*w)
splot <- signal(xplot, A[2], B[2], x0, w, Delta.t)
xdat.off <- xdat - 0.25
#plot
plot(xdat.off , ddat, type = 's', col = 'cyan2', lwd = 1, xlim = range(xplot),
     ylim = range(c(splot, ddat)), xlab = 'x', ylab = 'Signal background counts',
     main = 'A/B = 2')
lines(xplot, splot, col = 'purple')



#A/B = 10
xdat <- seq(from = - 7*w, to =  7*w, by = 0.5*w)
s.true_AB_3 <- signal(xdat, A[3], B[3], x0, w, Delta.t)
ddat <- rpois(length(s.true_AB_3), s.true_AB_3)
xplot <- seq(from = min(xdat), to = max(xdat), by = 0.05*w)
splot <- signal(xplot, A[3], B[3], x0, w, Delta.t)
xdat.off <- xdat - 0.25
#plot
plot(xdat.off , ddat, type = 's', col = 'cyan2', lwd = 1, xlim = range(xplot),
     ylim = range(c(splot, ddat)), xlab = 'x', ylab = 'Signal background counts',
     main = 'A/B = 10')
lines(xplot, splot, col = 'purple')




#A/B = 25
xdat <- seq(from = - 7*w, to =  7*w, by = 0.5*w)
s.true_AB_4 <- signal(xdat, A[4], B[4], x0, w, Delta.t)
ddat <- rpois(length(s.true_AB_4), s.true_AB_4)
xplot <- seq(from = min(xdat), to = max(xdat), by = 0.05*w)
splot <- signal(xplot, A[4], B[4], x0, w, Delta.t)
xdat.off <- xdat - 0.25
#plot
plot(xdat.off , ddat, type = 's', col = 'cyan2', lwd = 1, xlim = range(xplot),
     ylim = range(c(splot, ddat)), xlab = 'x', ylab = 'Signal background counts',
     main = 'A/B = 25')
lines(xplot, splot, col = 'purple')




#----------------------------Finish----------------------  :)



