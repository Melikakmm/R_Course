#Exercise series *2*




#EX1


p1 <- c(0.15, 0.21, 0.35, 0.15, 0.14)
p2 <- c(0.14, 0.05, 0.64, 0.08, 0.09)
values <- c(15.58, 15.9, 16, 16.1, 16.2)


EX1 <- sum(values * p1)
EX2 <- sum(values * p2)

variance1 <- sum(values * values * p1) - EX1**2
variance2 <- sum(values * values * p2) - EX2**2

cat('Variance of method one:', variance1)
cat('Expected value of method one:',EX1 )


cat('Variance of method two:', variance2)
cat('Expected value of method two:',EX2)

#---------------------------------------------------------------------------
#EX2

# ----------A-------------
#buiding the pdf
set.seed(1)
ex <- rexp(50, rate = 1/30)

#plotting
#It might sound extra work, but I plotted the distribution with and without replications.


#replicated for 1000 times
library(ggplot2)
# with 1000 replications.
n = 50 #this the number of people
ex_frame <- data.frame(value = c(t(rexp(1000 * n, rate = 1/30)))) ; ex_frame
#plot1
ggplot(ex_frame, aes(x=value)) + 
  geom_histogram(aes(y=..density..),binwidth=.25, col="purple", fill="yellow")+
  labs(title= "Exponential distribution with mean = 30mins", caption = 'with 1000 replications ') +
  xlab("x") +
  ylab("y")



#The plot without replication:
ex_frame_no_rep <- data.frame(value = c(t(rexp(n, rate = 1/30)))) ;ex_frame_no_rep
#plot2
ggplot(ex_frame_no_rep, aes(x=value)) + 
  geom_histogram(aes(y=..density..),binwidth=.25, col="darkred", fill="yellow")+
  labs(title= "Exponential distribution with mean = 30mins", caption = 'without replications ') +
  xlab("x") +
  ylab("y")

#----------B----------------
#less than 10 minutes 
# CDF

cat('The probabilty that a person will wait for less than 10 minuets:',pexp(10, 1/30))



#-----------C---------------
#mean of simulated data for 50 people(Empirical)
# I used the data frame both with and without replications.

sim_mean_no_rep<- mean(ex_frame_no_rep$value); sim_mean_no_rep#no replication
sim_mean <- mean(ex_frame$value); sim_mean#with replication

######obviousely the mean with replication is more close to the actual mean

#mean calculated from theory(Theoritical)
lambda = 1/30
Theo_mean <- 1/lambda ; Theo_mean

#plot3
#sorry that it takes too much time.
plot_3<- ggplot(ex_frame, aes(x=value)) + 
  stat_function(fun=dnorm, color="black", args=list(mean=mean(ex_frame$value), sd=sd(ex_frame$value)))+
  geom_vline(xintercept = Theo_mean, colour="red") +
  geom_text(aes(x=0, label="\nTheoretical mean", y=0.005), colour="red", angle=90) +
  geom_vline(xintercept = sim_mean_no_rep, colour="green")+
  geom_text(aes(x=sim_mean_no_rep+0.5, label="\n mean", y=0.009), colour="green", angle=90) +
  geom_vline(xintercept = sim_mean, colour="blue")+
  geom_text(aes(x=sim_mean-0.2, label="\n mean with replication", y=0.0025), colour="blue", angle=90)+
  ggtitle( "Theoretical mean vs sample mean") +
  xlab("x") +
  ylab("y")+theme_gray()

plot_3
#-----D--------
cat('probabilitty of waiting more than one hour',1-pexp(60, 1/30))
  
#--------------------------------------------------------------------
#EX3
#There is one typo every 3 pages, so lambda = 1/3
success <- 0:20

#plot4
plot(success, dpois(success, lambda=1/3),
     type='h',
     main='Poisson pdf (lambda = 1/3)',
     ylab='Probability density',
     xlab =' Successes',
     lwd=3,
     col = 'brown')
     abline(h=seq(0, 1, 0.1), col="grey")
     abline(v=seq(0, 20 ,1), col="grey")

#plot5
plot(success, ppois(success, lambda=1/3),
     type='h',
     main='Poisson cdf (lambda = 1/3)',
     ylab='Probability',
     xlab =' Successes',
     lwd=3,
     col = 'deeppink')
     abline(h=seq(0, 1, 0.1), col="grey")
     abline(v=seq(0, 20, 1), col="grey")


# probability of that there is at least one error on a specific page.
cat('probability of that there is at least one error on a specific page',1-dpois(0, 1/3))

#---------------------------------------------------------------------------

#EX4
#it is with replacement, so it is bionomial.

cat(' The probability that at least 10 draws are needed:',1-pgeom(9, 4/52))


#----------------------------------------------------------------------------

#EX5
#------A-----------
#first we assume that c is one to get the integral for the function without considering c.

custom_pdf <- function(t, c = 1) {
  c*(t-1)*(2-t) -> f
  return(f)
}

integral <- integrate(custom_pdf, lower = 1, upper =2 )
integral
#because c is our normalization parameter in this pdf:
cat('The constant c is :',1/integral$value)
# The pdf and cdf function will be like :
#pdf = 6(t-1)(2-t)
#cdf = 2t^3 + 9t^2+12t+5

custom_cdf <- function(t) {
  5- 2*(t**3) + 9*(t**2)-12*t -> m
  return(m)
  
}
#
#-------B-----------


#pdf
df.1 <- function(t) {
  ifelse(t>1 & t<2, 6*(t-1)*(2-t) , 0)
}


#cdf
pf.1<- function(t) {
  ifelse(1<t & t<2,5- 2*(t**3) + 9*(t**2)-12*t,ifelse(t<=1, 0,ifelse(t>=2, 1, 0)))
}
#checking
pf.1(1.5)
pf.1(2)

#Quantile 
set.seed(12345)
#defining a reverse function:
inverse <- function(fn, interval = NULL, lower = min(interval), upper = max(interval), ...){
  Vectorize(function(y){
    uniroot(f = function(x){fn(x) -y}, lower = lower, upper = upper , ...)$root
  })
}
  
  

qf.1 <- function(x) {
  output<-inverse(pf.1,interval = NULL, lower = 1, upper = 2 )
  return(output(x))
}

rf.1 <- function(n) {
  us <- runif(n)
  out <- ifelse(us>0 & us<1, qf.1(us), ifelse(us>1, 2,0))
  return(out)
}


#checking the functions :) 
pf.1(1.75)
qf.1(0.84375)
qf.1(1)
rf.1(100)

#plot6
#plotting the cdf and pdf
input <- seq(0, 3, 0.01)
probabilities <- seq(0, 1, 0.005)
plot(input, df.1(input),
     main = 'PDF, CDF, and reverse in a single plot for question 5',
     ylim = c(0, 3),
     ylab = 'output',
     type = 'l',
     col = 'blue')
     lines(input, pf.1(input),col = 'purple')
     lines(probabilities,qf.1(probabilities), col = 'orange' )
     abline(h=seq(0, 3, 0.5), col="grey")
     abline(v=seq(0, 3, 0.5), col="grey")
     abline(h=1:2,lty="dashed", col="deeppink")
     legend('topright',
            c('PDF', 'CDF', 'Inverse CDF'),
            fill = c('blue', 'purple', 'orange'))
#-------c--------
cat('more than 75 minutes:', 1-pf.1(1.25))
cat('between 90 an 120 minutes:', pf.1(2)- pf.1(1.5))
#-------------------------------------------------------------------------------
#EX6

#-------A---------
#pdf2
df.2 <- function(x) {
  ifelse(x>1 & x<2,2/(x**2) , 0)
}
#cdf2
pf.2<- function(x) {
  ifelse(1<x & x<2,(-2/(x)) + 2,ifelse(x<=1, 0,1))
}

#the inverse function is very easy to derive with hand; therefore:
qf.2 <- function(x) {
  op<- ifelse(0<x & x<1,(2/(2-x)),ifelse(x<=0, 0,ifelse(x>=1, 2, 0)))
  return(op)
}
rf.2 <- function(n) {
  k <- runif(n)
  ot <- ifelse(0.5<k & k<2, qf.2(k), ifelse(k>1, 2,0))
  return(ot)
}
#just cheking
qf.2(pf.2(1.5))



#plot7:
input <- seq(0, 3, 0.01)
probabilities <- seq(0, 1, 0.005)

plot(input, df.2(input),
     main = 'PDF, CDF, and reverse in a single plot for question 6',
     ylim = c(0, 3),
     ylab = 'output',
     type = 'l',
     col = 'blue')
lines(input, pf.2(input),col = 'green')
lines(probabilities,qf.2(probabilities), col = 'brown' )
abline(h=seq(0, 3, 0.5), col="grey")
abline(v=seq(0, 3, 0.5), col="grey")
legend('topright',
       c('PDF', 'CDF', 'Inverse CDF'),
       fill = c('blue', 'green', 'brown'))



#-----B------

#15000 = 1.5 * 10^4 .
cat('The probabity that tires last less than 15000km is :',pf.2(1.5))

#----c---------
#random sampling
sample <- rf.2(3000); sample
#making a data frame from samples and the outputs of 
data_3000 <- data.frame(sample,df.2(sample));data_3000


#mean or E[X]:
mean <- weighted.mean(data_3000$sample, data_3000$df.2.sample.);mean


#second moment or E[X^2] :
data_3000$square <- sapply(data_3000$sample, function(x) x^2)
data_3000
sec <- weighted.mean(data_3000$square, data_3000$df.2.sample.); sec

#variance = E[X^2] - E[X]
Variance <- sec - mean ; Variance 




#finish 
#Melika Keshavarzmirzamohammadi :)
