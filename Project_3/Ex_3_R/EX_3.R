#Exercsie 3 due for 1st of May
#Melika Keshavarzmirzamohammdi
#2054633

#------------------------------------------------------------------------------
#EX1
 tri <- function(x, a, b, c){
   ifelse(x>= a & x<c, 2*(x-a)/(b-a)*(c-a), ifelse(x>=c & x<= b, 2*(b-x)/(b-a)*(b-c), 0))
 }

#Now we choose the intervals as:

A <- 1; B<-3; C<- 2


d.tri <- function(x){
  return(tri(x, A, B, C))
}

#just checking
d.tri(2)
d.tri(1.5)


#-----------A------------
library("RColorBrewer")
data = runif(100, 1, 3)
#plot1
plot1<-plot(data, d.tri(data),
     ylab = 'f(x)',
     type = 'h',
     main = 'The Triangular Distribution',
     col=brewer.pal(n = 3, name = "RdBu"))
     grid(nx = NULL, ny = NULL,
          lty = 2,
          col = 'gray',
          lwd = 2)



#----------B-----------

#reverse sampling   
#cdf
cdf <- function(x, a, b, c){
  if (x<=a){
    return(0)
  }
  else if(x> a && x<=c){
    return(((x-a)^2-a^2)/(b-a)*(c-a))
    
  }
  else if(x>c && x<b){
    return(((b-x)^2-b^2)/(b-a)*(b-c))
  }
  else{
    return(1)
  }
}

     
#in order to use inverse sampling we should create the inverse cdf.

revcdf <- function(y, a, b, c) {
  if (0 < y && y < cdf(c, a, b, c)){
    return(a + (y*(b-a)*(c-a)) ** 0.5)
  }
  else if(cdf(c, a, b, c) <= y && y < 1){
    return(b - ((1-y)*(b-a)*(b-c)) ** 0.5)
  } 
}

#just checking:
revcdf(0.98, 1, 3, 2)






#-------------C---------------
#my laptop did not work with a large data set.
randomsamples <- mapply(revcdf, runif(n = 0.1e+07, min = 0, max = 1), A, B, C);randomsamples
#I want to impelement the samples on pdf too, in order to see how  well they are sampled.
#That is why I made answers.
answers <- mapply(tri , randomsamples, A, B , C);answers






library("RColorBrewer")
# Install
#install.packages("wesanderson")
# Load
library(wesanderson)
#Here I implemented the random samples drawn from the reverse function to 
#The pdf itself to see whether they are compatible or not.


#plot2
#The superimpose; the histogram of samples drawn from the reverse function
#the green showing the pdf implemented on the samples
hist(randomsamples,
     density = 20,
     col =brewer.pal(n = 3, name = "OrRd"),
     main = 'The Superimpose Plot', xlim = c(0, 3))

hist(answers, density = 15,
     col =brewer.pal(n = 3, name = "Greens"),
     add = T)
title(sub = 'A comparison of cdf and pdf which are plotted with the reverse sampling',
      font.sub = 3, col.sub = 'darkred')

    

     


#-------------------------------------------------------------------------------
#EX2


#--------A------------
#Exponential
#lambda = 1, E[x] = 1, p(x>=k) = exp(-x), upperbound : exp(-x) <= 1/x
#plot3
data <- seq(0, 10, 0.01)
plot3 <- plot(data, 1-pexp(data, 1),
              type = 'h',
              xlab = 'data',
              ylab = 'p(x>=k)',
              main = 'The Markov Inequality For Exp',
              col=brewer.pal(n = 3, name = "Blues"))
              grid(nx = NULL, ny = NULL,
                   lty = 2,
                   lwd = 2)
#The bound
yfit <- (1+data)*exp(-data)
lines(data, yfit, col = 'black', lwd = 2)
legend(x = "topright", 
       legend = c("Exp Cdf", "bound"), 
       lty = c(1, 1),  
       col = c('blue', 'black'),  
       lwd = 3)    

#--------B-------------  
#Uniform
#U(3, 5)
#Here we have p(x>=k) = 1-p(x<=k)= 1-(x-3)/2= (5-x)/2
#plot4

plot4 <- plot(data, 1-punif(data, min = 3, max = 5),
              type ='h',
              xlab = 'data',
              ylab = 'p(x>=k)',
              main = 'The Markov Inequality For Unif',
              col='purple', xlim = c(-1, 10),
              ylim = c(0, 2))
              grid(nx = NULL, ny = NULL,
                   lty = 2,
                   lwd = 2)
yfit2 <- 25/4 - data**2/4
lines(data, yfit2, col = 'darkred', lwd = 2)
legend(x = "topright",          
       legend = c("Unif Cdf", "bound"),  
       lty = c(1, 1),        
       col = c('purple', 'darkred'),
       lwd = 3)    

#-------------C-----------

#The bionomial distribution
#mean here is np which is 1*(1/2) = 1/2; therefore, our bound will be 1/2k.


#plot 5

plot5 <- plot(data, 1-pbinom(data, 1, 0.5),
              type = 'h',
              xlab = 'data',
              ylab = 'p(x>= k)',
              main = 'The Markov Inequality For Binom',
              col = 'coral1')
#bound
yfit3 <- 1/(2*data)    
lines(data, yfit3, col = 'green', lwd = 2)
legend(x = "topright",          
       legend = c("Binom", "bound"),  
       lty = c(1, 1),        
       col = c('coral1', 'green'),
       lwd = 3)    
#--------d-------------

#poisson
#again mean is 0.5 here, since the mean for poisson is lambda.

#plot6

plot6 <- plot(data, 1-ppois(data, 0.5),
              type = 'h',
              xlab = 'data',
              ylab = 'p(x>= k)',
              main = 'The Markov Inequality For Pois',
              col = 'cyan4')
#bound
yfit4 <- 1/(2*data)    
lines(data, yfit4, col = 'red', lwd = 2)
legend(x = "topright",          
       legend = c("Pois", "bound"),  
       lty = c(1, 1),        
       col = c('cyan4', 'red'),
       lwd = 3)  
#-------------------------------------------------------------------------------
#EX3
#thre is an explanantion to this bound function below.

upperbound <- function(k){
  return(1/(k^2))
}



#-----------A------------
#Normal distribution

mu <- 3; sigma <- 5;

#I divided the condition infront of probability to two branches.
p_1_1 <- function(k){
  prob_r <- 1 - pnorm(mu + k*sigma, mu, sigma)
  return(prob_r)
}
p_1_2 <- function(k){
  prob_r_r <- pnorm(mu - k*sigma, mu, sigma)
  return(prob_r_r)
}

#plot7
data <- seq(1,10, by = 0.05)
plot(data, p_1_1(data),
     col = 'red',
     main = 'The Chebyshev Inequality For Norm',
     xlab = 'data',
     type = 'h',
     ylab = 'probability')
#the blue one which is for other condition overlaps the red one,
#so there is no need to add it.
#plot(data, p_1_2(data),
#      col = 'blue',
#      type = 'h',
#      main = 'The Chebyshev Inequality For Norm',
#      xlab = 'data',
#      ylab = 'probability', add = T)
fy5 <- upperbound(data)
lines(data,fy5, col = 'orange', lwd = 2 )
legend(x = "topright",          
       legend = c("Norm", "Cheby bound"),  
       lty = c(1, 1),        
       col = c('red', 'orange'),
       lwd = 3) 


    
#------------B-------------
#Exponential distribution

lambda <- 1

p_2_1 <- function(k){
  prob_r_ex <- 1 - pexp(lambda + k*(1/(lambda)), lambda)
  return(prob_r_ex)
}

#This branch below gives us zero prob.

#p_2_2 <- function(k){
#  prob_r_r_ex <- pexp(lambda - k*(1/(lambda)), lambda)
#  return(prob_r_r_ex)
#}

#plot8
data <- seq(1,10, by = 0.05)
plot(data, p_2_1(data),
     col = 'blueviolet',
     main = 'The Chebyshev Inequality For Exp',
     xlab = 'data',
     type = 'h',
     ylab = 'probability')

######well this branch is not usefull bc it keeps giving us zero.
#####Therefore, I put it in a comment.

#plot(data, p_2_2(data),
#     col = 'red',
#     type = 'h',
#     main = 'The Chebyshev Inequality For Exp',
#     xlab = 'data',
#     ylab = 'probability', add = T)

fy5 <- upperbound(data)
lines(data,fy5, col = 'orange', lwd = 2 )
legend(x = "topright",          
       legend = c("Exp", "Cheby bound"),  
       lty = c(1, 1),        
       col = c('blueviolet', 'orange'),
       lwd = 3) 



#----------C-------------

#UNiform
a <- 1 - sqrt(2)
b <- 1 + sqrt(2)

mean <- (a+b)/2
sigunif <- sqrt((b-a)^2/12)


p_3_1 <- function(k){
  prob_r_un <- 1 - punif(mean + k*sigunif, min = 1- (2)^0.5, max = 1 + (2)^0.5)
  return(prob_r_un)
}
p_3_2 <- function(k){
  prob_r_r_un <- punif(mean - k*sigunif, min = 1- (2)^0.5, max = 1 + (2)^0.5)
  return(prob_r_r_un)
}

#plot9
data <- seq(1,10, by = 0.05)
plot(data, p_3_1(data),
     col = 'aquamarine4',
     main = 'The Chebyshev Inequality For Unif',
     xlab = 'data',
     type = 'h',
     ylab = 'probability')
#again not that useful

#plot(data, p_3_2(data),
#     col = 'blue',
#     type = 'h',
#     main = 'The Chebyshev Inequality For Unif',
#     xlab = 'data',
#     ylab = 'probability', add = T)
fy5 <- upperbound(data)
lines(data,fy5, col = 'orange', lwd = 2 )
legend(x = "topright",          
       legend = c("Unif", "Cheby bound"),  
       lty = c(1, 1),        
       col = c('aquamarine4', 'orange'),
       lwd = 3) 




#---------d------------
#poisson distribution
lambda = 1/3
pmean <- 1/3
psigma <- sqrt(1/3)


p_4_1 <- function(k){
  prob_r_p <- 1 - ppois(pmean + k*psigma, lambda)
  return(prob_r_p)
}
p_4_2 <- function(k){
  prob_r_r_p <- pnorm(pmean - k*psigma, lambda)
  return(prob_r_r_p)
}

#plot10
data <- seq(1,10, by = 0.05)
plot(data, p_4_1(data),
     col = 'darkred',
     main = 'The Chebyshev Inequality For Pois',
     xlab = 'data',
     type = 'h',
     ylab = 'probability')

#there is no need to add this branch.
#plot(data, p_4_2(data),
#      col = 'green',
#      type = 'h',
#      main = 'The Chebyshev Inequality For Pois',
#      xlab = 'data',
#      ylab = 'probability', add = T)
fy5 <- upperbound(data)
lines(data,fy5, col = 'orange', lwd = 2 )
legend(x = "topright",          
       legend = c("Pois", "Cheby bound"),  
       lty = c(1, 1),        
       col = c('darkred', 'orange'),
       lwd = 3) 


#-------------------------------------------------------------------------------
#EX4
#Model


#probability of white and black:
white <- function(x){
  return((x-1)/5)
}
black <- function(x){
  return(1-white(x))
}




#here we define a function that extracts balls from bags:

output <- function(p){
  norm_sum <- 0
  for (j in 1:6){
    norm_sum <- norm_sum + p(j) * ph[j]
  }
  for (j in 1:6){
    ph[j] <- p(j) * ph[j] / norm_sum
  }
  return(ph)
}



ph <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)



# I introduced a matrix to save the probability for each event.
Matrixp <- matrix(array(ph), nrow=1, ncol=6)


cat('first probabilities : ', ph)
print('Insert w for white and b for black to choose a color. To cancel the program write stop.')
input<- readline('The color of first ball extracted:') 


#Instructions : w instead of white and b instead of black.
#And s for stopping the algorithm be 



while(input != 's'){
  
  
  
  if(input=='w'){
    ph <- output(white)
    print('A white ball was extracted. The new probabilities are:')
    print(ph)
    input <- readline('Choose another ball:')
  }
  
  
  
  else if(input=='b'){
    ph <- output(black)
    print('A black ball was extracted. The new probabilities are:')
    print(ph)
    input <- readline('please choose another ball:')
  }
  
  
  
  
  else if(input=='s'){
    print('Looking forward playing again! see you soon!')
  }
  
  
  
  
  else{
    input <- readline(' :((   Not Valid. Try another input! ')
  }
  
  
  
  
  #This is when we write the Matrix:
  Matrixp <- rbind(Matrixp, array(ph))
}


#-------------
#plotting the bags' probabilities


output_n <- dim(Matrixp)[1]
Matrixp


#plot11
#bag 1
plot(seq(1, output_n, by=1),
     Matrixp[,1],
     col='darkgoldenrod',
     main= 'probability of bag one',
     xlab="extraction",
     ylab="Probability",
     type="l",
     lwd=3)


#plot12
#bag 2
plot(seq(1, output_n, by=1),
     Matrixp[,2],
     col='coral2',
     main= 'probability of bag two',
     xlab="extraction",
     ylab="Probability",
     type="l",
     lwd=3)


#plot13
#bag 3
plot(seq(1, output_n, by=1),
     Matrixp[,3],
     col='cyan4',
     main= 'probability of bag three',
     xlab="extraction",
     ylab="Probability",
     type="l",
     lwd=3)

#plot14
#bag 4
plot(seq(1, output_n, by=1),
     Matrixp[,4],
     col='darkred',
     main= 'probability of bag four',
     xlab="extraction",
     ylab="Probability",
     type="l",
     lwd=3)


#plot15
#bag 5
plot(seq(1, output_n, by=1),
     Matrixp[,5],
     col='cadetblue',
     main= 'probability of bag five',
     xlab="extraction",
     ylab="Probability",
     type="l",
     lwd=3)

#plot16
#bag 6
plot(seq(1, output_n, by=1),
     Matrixp[,6],
     col='orange',
     main= 'probability of bag six',
     xlab="extraction",
     ylab="Probability",
     type="l",
     lwd=3)


#------------------------------------------------------------------------------
#EX5


#Now we want to sample boxes:
#In order to put input to the strings I used glue.
library(glue)


number_of_bag <- sample(0:5, 1);number_of_bag



sampling <- function(number_of_bag, ex_number){
  x <- runif(1, 0, 5)
  if(x>number_of_bag){
    print(glue('Extraction number {ex_number}.
               ball is black.'))
    return(black)
  }
  
  
  
  else{
    print(glue('Extraction number {ex_number}.
               ball is white.'))
    return(white)
  }
}

#-----------------------------------
#probability Initializer.
ph_2 <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)



#again we use a matrix to save the probabilities:

Matrixp_2 <- matrix(array(ph_2), nrow=1, ncol=6)

steps <- as.integer(readline('please insert the number of steps to start: '))




#iteration for saving:
n <- 0
while(n < steps){
  
  
  ph_2 <- output(sampling(number_of_bag, n))

  Matrixp_2 <- rbind(Matrixp_2, array(ph_2))
  
  n <- n + 1
}

#---------------------
#plotting all bags in the matrix.

output_n_2 <- dim(Matrixp_2)[1]
Matrixp_2#Matrix of probs for the last question



#plot17
#bag1
plot(seq(1, output_n_2, by=1),
     Matrixp_2[,1],
     col='darkorange',
     main="Probabilites box one",
     xlab="Extractions",
     ylab="Probability",
     type="l", lwd=3)

#plot18
#bag2
plot(seq(1, output_n_2, by=1),
     Matrixp_2[,2],
     col='yellow',
     main="Probabilites box two",
     xlab="Extractions",
     ylab="Probability",
     type="l", lwd=3)

#plot19
#bag3

plot(seq(1, output_n_2, by=1),
     Matrixp_2[,3],
     col='blueviolet',
     main="Probabilites box three",
     xlab="Extractions",
     ylab="Probability",
     type="l", lwd=3)
#plot20
#bag4
plot(seq(1, output_n_2, by=1),
     Matrixp_2[,4],
     col='green',
     main="Probabilites box four",
     xlab="Extractions",
     ylab="Probability",
     type="l", lwd=3)
#plot21
#bag5
plot(seq(1, output_n_2, by=1),
     Matrixp_2[,5],
     col='coral3',
     main="Probabilites box five",
     xlab="Extractions",
     ylab="Probability",
     type="l", lwd=3)
#plot22
#bag6
plot(seq(1, output_n_2, by=1),
     Matrixp_2[,6],
     col='pink',
     main="Probabilites box six",
     xlab="Extractions",
     ylab="Probability",
     type="l", lwd=3)




#finished


