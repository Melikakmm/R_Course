#Exercise series 4 
#Melika Keshavavrzmirzamohammadi
#
#some parts might be overdone, just for the purpose of learning and experimenting.


#EX1
#I chose Italy and Poland trends to analyse

Italy_20 <- read.csv('/Users/melikakeshavarz/Downloads/Region_Mobility_Report_CSVs/2020_IT_Region_Mobility_Report.csv',
                     sep = ','); Italy_20
Italy_21 <- read.csv('/Users/melikakeshavarz/Downloads/Region_Mobility_Report_CSVs/2021_IT_Region_Mobility_Report.csv',
                     sep = ','); Italy_21
Italy_22 <- read.csv('/Users/melikakeshavarz/Downloads/Region_Mobility_Report_CSVs/2022_IT_Region_Mobility_Report.csv',
                  sep = ','); Italy_22



Poland_20 <- read.csv('/Users/melikakeshavarz/Downloads/Region_Mobility_Report_CSVs/2020_PL_Region_Mobility_Report.csv',
                      sep = ','); Poland_20
Poland_21 <- read.csv('/Users/melikakeshavarz/Downloads/Region_Mobility_Report_CSVs/2021_PL_Region_Mobility_Report.csv',
         sep = ','); Poland_21
Poland_22 <- read.csv('/Users/melikakeshavarz/Downloads/Region_Mobility_Report_CSVs/2022_PL_Region_Mobility_Report.csv',
                   sep = ','); Poland_22


Italy_DF <- rbind(Italy_20, Italy_21, Italy_22);Italy_DF
Poland_DF <- rbind(Poland_20, Poland_21, Poland_22); Poland_DF

#cleaning total dataframes:
Italy_DF[is.na(Italy_DF)] = 0;Italy_DF
Italy_DF$date <- as.Date(Italy_DF$date)#inverting the dates to date format




Poland_DF[is.na(Poland_DF)] = 0; Poland_DF
Poland_DF$date <- as.Date(Poland_DF$date)#inverting the dates to date format


#working with tibbles is easier.
Italy_DF <- tibble(Italy_DF)
Poland_DF <- tibble(Poland_DF)

#cleaning per year:
library(ggplot2)
Italy_20[is.na(Italy_20)] = 0; tibble(Italy_20) -> Italy_20
Italy_21[is.na(Italy_21)] = 0; tibble(Italy_21) -> Italy_21
Italy_22[is.na(Italy_22)] = 0; tibble(Italy_22) -> Italy_22

Poland_20[is.na(Poland_20)] = 0; tibble(Poland_20) ->Poland_20
Poland_21[is.na(Poland_21)] = 0; tibble(Poland_21) ->Poland_21
Poland_22[is.na(Poland_22)] = 0; tibble(Poland_22) ->Poland_22


#In order to understand the data better, I decided to produce two plots for Q1 too:
#I plotted 6 categories' trajectories for both Poland and Italy: 
library(ggplot2)
library(tidyverse)
library(scales)
library(grid)
library(gridExtra)
library(cowplot) 




#ITALY------******
#plot1
r <- ggplot()+ geom_line(data = head(Italy_DF, n = 50), aes(x = date, y = retail_and_recreation_percent_change_from_baseline),
                         col ='darkred')+theme(axis.title.y =element_blank())
g <- ggplot()+geom_line(data = head(Italy_DF, n = 50), aes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline),
                        col = 'purple')+theme(axis.title.y =element_blank())
p <- ggplot()+ geom_line(data = head(Italy_DF, n = 50), aes(x = date, y = parks_percent_change_from_baseline),
                         col = 'green')+theme(axis.title.y =element_blank())
t <- ggplot()+geom_line(data = head(Italy_DF, n = 50), aes(x = date, y = transit_stations_percent_change_from_baseline),
                        col = 'blue')+theme(axis.title.y =element_blank())
w <- ggplot()+ geom_line(data = head(Italy_DF, n = 50), aes(x = date, y = workplaces_percent_change_from_baseline),
                         col = 'orange')+theme(axis.title.y =element_blank())
resi <- ggplot()+ geom_line(data = head(Italy_DF, n = 50), aes(x = date, y = residential_percent_change_from_baseline),
                            col = 'pink')+theme(axis.title.y =element_blank())
plot1<-plot_grid(r, g, p, t, w, resi, labels=c("retail&recreation", "grocery&farmacy", 'parks',
                                               'transit&station', 'workplaces', 'residential'),
                 ncol = 2, nrow = 3, label_size = 8, label_colour = 'darkgrey')
y.grob <- textGrob("ITALY", 
                   gp=gpar(fontface="bold", col="blue", fontsize=15), rot=90)
grid.arrange(arrangeGrob(plot1, left = y.grob))






#POLAND-----******
#plot2
r2 <- ggplot()+ geom_line(data = head(Poland_DF, n = 50), aes(x = date, y = retail_and_recreation_percent_change_from_baseline),
                         col ='darkred')+theme(axis.title.y =element_blank())
g2 <- ggplot()+geom_line(data = head(Poland_DF, n = 50), aes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline),
                        col = 'purple')+theme(axis.title.y =element_blank())
p2 <- ggplot()+ geom_line(data = head(Poland_DF, n = 50), aes(x = date, y = parks_percent_change_from_baseline),
                         col = 'green')+theme(axis.title.y =element_blank())
t2 <- ggplot()+geom_line(data = head(Poland_DF, n = 50), aes(x = date, y = transit_stations_percent_change_from_baseline),
                        col = 'blue')+theme(axis.title.y =element_blank())
w2 <- ggplot()+ geom_line(data = head(Poland_DF, n = 50), aes(x = date, y = workplaces_percent_change_from_baseline),
                         col = 'orange')+theme(axis.title.y =element_blank())
resi2 <- ggplot()+ geom_line(data = head(Poland_DF, n = 50), aes(x = date, y = residential_percent_change_from_baseline),
                            col = 'pink')+theme(axis.title.y =element_blank())
plot2<-plot_grid(r2, g2, p2, t2, w2, resi2, labels=c("retail&recreation", "grocery&farmacy", 'parks',
                                               'transit&station', 'workplaces', 'residential'),
                 ncol = 2, nrow = 3, label_size = 8, label_colour = 'darkgrey')  
y.grob <- textGrob("POLAND", 
                   gp=gpar(fontface="bold", col="red", fontsize=15), rot=90)
grid.arrange(arrangeGrob(plot1, left = y.grob))




#average changes in visitors for mentioned places for both Italy and Poland:

vector_M_IT <- c()
for(i in Italy_DF[,10:15]){
  vector_M_IT <- append(vector_M_IT, mean(i))
};vector_M_IT

vector_M_PL <- c()
for(j in Poland_DF[,10:15]){
  vector_M_PL <- append(vector_M_PL, mean(j))
};vector_M_PL



#I decided to create a beautiful table for mean of visitors for each place:
#install.packages("data.table")
#install.packages("dplyr")
#install.packages("formattable")
#install.packages("tidyr")

library(data.table)
library(dplyr)
library(formattable)
library(tidyr)

mean.df <- data.frame(c('Retail And Recreation', 'Grocery And Pharmacy',
                        'Parks', 'Transit And Station', 'Workplaces', 'Residential'),vector_M_IT, vector_M_PL)
colnames(mean.df) <- c( 'places','mean_IT', 'mean_PL')
mean.df




#install.packages('gt')
#install.packages('palmerpenguins')
library(tidyverse)
library(gt)
library(palmerpenguins)
theme_set(theme_bw(16))


#plot3
#The table is saved as: name :plot3 ,filetype : png.
mytable<-gt(mean.df)
mytable

mytable %>%
  tab_header(title = md('**Mean of Changes In Visitors**'),
             subtitle = 'from 2020 to 2022')%>%
  data_color(columns = c(mean_IT, mean_PL),
             colors =scales::col_numeric(palette = c('red','yellow','yellowgreen', 'green'),
             domain = c(-22, 34)))

#changes in Italy were dramatic!!!




#-------EX1 part 2 ------------------------


#install.packages('lubridate')
library(readr)
library(lubridate)

#I did this exercise just for Italy and Poland in the year 2021,
#since I understood there would be an overlapping of weeks and 
#months if I use lubridate for the three years.



#Here I added a column counting **weeks** to both datasets of Italy and Poland:
#For Italy
Italy_21$weeks<-lubridate::week(ymd(Italy_21$date))
head(Italy_21$weeks)
colnames(Italy_21)

#For Poland
Poland_21$weeks <- lubridate::week(ymd(Poland_21$date))
head(Poland_21$weeks)



#creating average over one week dataset:
#For Italy
Italy_21 %>%
  group_by(weeks) %>%
  summarise_at(vars('retail_and_recreation_percent_change_from_baseline',
                    'grocery_and_pharmacy_percent_change_from_baseline',
                    'parks_percent_change_from_baseline',
                    'transit_stations_percent_change_from_baseline',
                    'workplaces_percent_change_from_baseline',
                    'residential_percent_change_from_baseline'), mean) ->week_IT
week_IT



#For Poland
Poland_21 %>%
  group_by(weeks) %>%
  summarise_at(vars('retail_and_recreation_percent_change_from_baseline',
                    'grocery_and_pharmacy_percent_change_from_baseline',
                    'parks_percent_change_from_baseline',
                    'transit_stations_percent_change_from_baseline',
                    'workplaces_percent_change_from_baseline',
                    'residential_percent_change_from_baseline'), mean) ->week_PL
week_PL




#Here I added a column counting **months** to both datasets of Italy and Poland:
#For Italy:
Italy_21$months<-lubridate::month(ymd(Italy_21$date))
head(Italy_21$months)

#For Poland:
Poland_21$months<-lubridate::month(ymd(Poland_21$date))
head(Poland_21$months)
colnames(Poland_21)

#creating average over one month:

#For Italy
Italy_21 %>%
  group_by(months) %>%
  summarise_at(vars('retail_and_recreation_percent_change_from_baseline',
                    'grocery_and_pharmacy_percent_change_from_baseline',
                    'parks_percent_change_from_baseline',
                    'transit_stations_percent_change_from_baseline',
                    'workplaces_percent_change_from_baseline',
                    'residential_percent_change_from_baseline'), mean) ->month_IT
month_IT



#For Poland
Poland_21 %>%
  group_by(months) %>%
  summarise_at(vars('retail_and_recreation_percent_change_from_baseline',
                    'grocery_and_pharmacy_percent_change_from_baseline',
                    'parks_percent_change_from_baseline',
                    'transit_stations_percent_change_from_baseline',
                    'workplaces_percent_change_from_baseline',
                    'residential_percent_change_from_baseline'), mean) ->month_PL
month_PL



#plots
#install.packages('gridExtra')
#install.packages('scales')
library(ggplot2)
library(tidyverse)
library(scales)
library(grid)
library(gridExtra)
library(cowplot) 
#plotting for weeks:


#IT
ggplot(data = week_IT)+
  geom_line(mapping = aes(x =weeks,
                          y =retail_and_recreation_percent_change_from_baseline, col='Retail&Recreation'))+
  geom_line(mapping = aes(x=weeks,
                          y =grocery_and_pharmacy_percent_change_from_baseline, col = 'Grocery&Pharmacy'))+
  geom_line(mapping = aes(x = weeks,
                          y = parks_percent_change_from_baseline, col = 'Parks'))+
  geom_line(mapping=aes(x = weeks,
                        y = transit_stations_percent_change_from_baseline, col ='Transit&stations'))+
  geom_line(mapping = aes(x = weeks,
                          y = workplaces_percent_change_from_baseline, col = 'Workplaces'))+
  geom_line(mapping = aes(x = weeks,
                          y =residential_percent_change_from_baseline, col = 'residential'))+
  theme(legend.position = 'none')+
  theme(axis.title.y =element_blank()) -> plotIT_w



#PL
ggplot(data = week_PL)+
  geom_line(mapping = aes(x =weeks,
                          y =retail_and_recreation_percent_change_from_baseline, col='Retail&Recreation'))+
  geom_line(mapping = aes(x=weeks,
                          y =grocery_and_pharmacy_percent_change_from_baseline, col = 'Grocery&Pharmacy'))+
  geom_line(mapping = aes(x = weeks,
                          y = parks_percent_change_from_baseline, col = 'Parks'))+
  geom_line(mapping=aes(x = weeks,
                        y = transit_stations_percent_change_from_baseline, col ='Transit&stations'))+
  geom_line(mapping = aes(x = weeks,
                          y = workplaces_percent_change_from_baseline, col = 'Workplaces'))+
  geom_line(mapping = aes(x = weeks,
                          y =residential_percent_change_from_baseline, col = 'residential'))+
  theme(legend.position = 'bottom')+theme(legend.text = element_text(colour="blue", size=7))+
  theme(legend.title=element_blank())+
  theme(axis.title.y =element_blank())-> plotPL_w




#plot4
#putting the plots together
plot4<-plot_grid(plotIT_w, plotPL_w, labels=c('ITALY', 'POLAND'),
                 ncol = 1, nrow = 2, label_size = 8, label_colour = 'purple')
y.grob <- textGrob("Changes Over Weeks For Both Italy and Poland", 
                   gp=gpar(fontface="bold", col="blue", fontsize=8))
grid.arrange(arrangeGrob(plot4, top = y.grob))


#plotting for months:

#IT
ggplot(data = month_IT)+
  geom_line(mapping = aes(x =months,
                          y =retail_and_recreation_percent_change_from_baseline, col='Retail&Recreation'))+
  geom_line(mapping = aes(x=months,
                          y =grocery_and_pharmacy_percent_change_from_baseline, col = 'Grocery&Pharmacy'))+
  geom_line(mapping = aes(x = months,
                          y = parks_percent_change_from_baseline, col = 'Parks'))+
  geom_line(mapping=aes(x = months,
                        y = transit_stations_percent_change_from_baseline, col ='Transit&stations'))+
  geom_line(mapping = aes(x = months,
                          y = workplaces_percent_change_from_baseline, col = 'Workplaces'))+
  geom_line(mapping = aes(x = months,
                          y =residential_percent_change_from_baseline, col = 'residential'))+
  theme_dark()+
  theme(legend.position = 'none')+
  theme(axis.title.y =element_blank()) -> plotIT_m



#PL
ggplot(data = month_PL)+
  geom_line(mapping = aes(x =months,
                          y =retail_and_recreation_percent_change_from_baseline, col='Retail&Recreation'))+
  geom_line(mapping = aes(x=months,
                          y =grocery_and_pharmacy_percent_change_from_baseline, col = 'Grocery&Pharmacy'))+
  geom_line(mapping = aes(x = months,
                          y = parks_percent_change_from_baseline, col = 'Parks'))+
  geom_line(mapping=aes(x = months,
                        y = transit_stations_percent_change_from_baseline, col ='Transit&stations'))+
  geom_line(mapping = aes(x = months,
                          y = workplaces_percent_change_from_baseline, col = 'Workplaces'))+
  geom_line(mapping = aes(x = months,
                          y =residential_percent_change_from_baseline, col = 'residential'))+theme_dark()+
  theme(legend.position = 'bottom')+theme(legend.position = 'bottom')+theme(legend.text = element_text(colour="blue", size=7))+
  theme(legend.title=element_blank())+
  theme(axis.title.y =element_blank())-> plotPL_m


#plot5
#putting the plots together
plot5<-plot_grid(plotIT_m, plotPL_m, labels=c('ITALY', 'POLAND'),
                 ncol = 1, nrow = 2, label_size = 8, label_colour = 'red')
y.grob <- textGrob("Changes Over Months For Both Italy and Poland", 
                   gp=gpar(fontface="bold", col="orange", fontsize=8))
grid.arrange(arrangeGrob(plot5, top = y.grob))



#------------------------------------------------------------------------------
#EX2


#*****seeds with even digits should be chosen*******
# s = 1 gives string of numbers.
#s = 0 we have numbers.
#n is the number of iterations. in other words, number of randnumbers we want to generate.


Middlesquared <- function(seed, n, s) {
  options("scipen" = 2*nchar(seed))
  
  if(missing(n)) {
    n <- 10 #default mood of n.
  }
  
  if(missing(s)) {
    s <- 1#default mood of 
  }
  #seed with odd digits does not work!
  if(nchar(seed)%%2 != 0){
    
    cat("Does  not  work  :(  , try  an  even-digit  seed!")
    
    cat("\n")
    
    stop
  } else  
    while_char <- seed
  library(stringi)
  Rand_N <- c()
  for(i in 1:n){
    seed_sqr <- seed**2
    
    #When the seed_sqr digits becomes odd, we add 0 to the left:
    while(nchar(seed_sqr) < 2*nchar(while_char)) {
      seed_sqr<- stri_pad_left(seed_sqr, 2*nchar(while_char), 0)
    }
    
    
    
    #Finding the middle number
    left_digit <- nchar(seed_sqr)/4 + 1
    right_digit <- nchar(seed_sqr) - (nchar(seed_sqr)/4)
    #now we drop both left and right digits to remain with the middle:
    randn<- as.numeric(substr(seed_sqr, left_digit, right_digit))
    
    while(nchar(randn) < nchar(while_char)){
      randn <- stri_pad_left(randn, nchar(while_char), 0)
    }
    
    Rand_N[i] <- randn 
    randn <- as.numeric(randn)
    seed <- randn
    while_char <- substr(seed_sqr, left_digit, right_digit) 
  }
  #do we want rand numbers as string or just numbers?
  ifelse(s==1, Rand_N,
         ifelse(s==0, Rand_N <- as.numeric(Rand_N))
  )
  Rand_N
}



#checking:
Middlesquared(11, 10, 0)

#The weak point of this method is it terminates after producing specific 
#number of random numbers.For instance, for seed = 11, the algorithm
#generates 0 after 8 iterations.



#------------------------------------------------------------------------------

#EX3


#Bionomial 

#P(pi | n, r, M) = P(r| pi, n, M) P(pi| M) / P(r|n, M)

#P(pi| M) = UNIFORM

n <- 150
r <- 29

pi <- seq(0, 1, length.out = 2000)
post <- dbinom(x = r, size = n, prob = pi)
#Normalization
samples <- 2000
delta.pi <- 1/samples
pi <- seq(from = 1/(2*samples), by = delta.pi, length.out = samples)
p.star <- dbinom(x = r, size = n, prob = pi)
post_norm <- p.star/ (delta.pi*sum(p.star))

#plot6
plot(pi, post_norm, type = 'l', lwd = 4, col = 'pink',
     main ='Normalized Post',
     xlab = 'Probability', ylab = 'P(pi|n, r, M)')


#plot7
#likelihood
#P(r|pi, n, M)


like<- dbinom(x = 1:150, size = n, prob = 29/150)
plot(1:150, like, col = 'darkred', main = 'Likelihood', xlab = 'number of people',
    ylab = 'P(r|pi, n, M)', lwd = 4 , type = 'l')

#-------------------------------------------------------------------------------
#EX4


#------------a------------------

#number of heads


#Beta prior properties :
alpha <- 30
beta <- 30


#Normalization:
samples <- 2000 ; delta.p <- 1/samples
p<- seq(from = 1/(2*samples), by = delta.p, length.out = samples)


#plot8
par(mfrow = c(3, 3))
for(r in seq(from = 0, to = 30, by = 5)){
  #plotting post with Uniform prior:
  p.star <- dbinom(x = r, size = n, prob = p)
  post_Unif <- p.star/(delta.p * sum(p.star))
  plot(p, post_Unif, col = 'palegreen1', type = 'l', xlab ='p',
       ylab = 'P(P|r.n.M)', main = sprintf('r=%d', r))
  mean_Unif <- delta.p* sum(p*post_Unif)
  abline(v = mean_Unif, col = 'palegreen1', lty = 2)
  #plotting post with Beta prior:
  post_Beta <- dbeta(x = p, alpha+r, beta+n-r)
  lines(p, post_Beta, lwd = 2, col = 'olivedrab')
  mean_beta <- delta.p *sum(p*post_Beta)
  abline(v = mean_beta, col = 'olivedrab', lty = 2)
  #The Unif prior itself:
  lines(p, dunif(x = p, min = 0, max = 1), col = 'tomato1')
  #The Beta prior itself:
  lines(p, dbeta(x = p, alpha, beta), col = 'salmon2')

  
}

#-------------b------------------
#install.packages('bayestestR')
library(bayestestR)
library(dplyr)
library(ggplot2)




#most probable value functions:

#1_for conjugate binomial likelihood and Uniform prior:
p_0_u <- function(r, n){
  return(r/n)
}
#2_for conjugate binomial likelihood and Beta prior:
p_0_B <- function(r, n, alpha, beta){
  return((r+alpha-1)/(n+alpha+beta-2))
}

#I defined separate vectors for Most probable value and two interval line.
#for each post.
most_probable_unif <- c();most_probable_B <- c()

cred_unif_low <- c();cred_unif_high <- c()

cred_B_low <- c();cred_B_high <- c()


#In this loop, I calculated ***the line intervals*** for credibility as well
#as ***the most probable value*** for both posts:




p<- seq(from = 1/(2*samples), by = delta.p, length.out = samples)
for(n in 1:30){
  #for a fixed r:
  r = n-1 #I fixed r to avoid too many iterations.
  #for the post with uniform prior:
  p.star <- dbinom(x = r, size = n, prob = p)
  post_Unif <- p.star/(delta.p * sum(p.star))
  most_probable_unif[n]<-p_0_u(r, n)
  #here I took HDI as a version of credibility interval:
  a <- t.test(post_Unif)
  cred_unif_low[n] <- a$conf.int[1]
  cred_unif_high[n] <- a$conf.int[2]

    
    
    
  #for the post with beta prior:
  post_Beta <- dbeta(x = p, alpha+r, beta+n-r)
  most_probable_B[n]<-p_0_B(r, n, alpha, beta)
  b <- t.test(post_Beta)
  cred_B_low[n] <- b$conf.int[1]
  cred_B_high[n] <- b$conf.int[2]

    
}




#Then I put the information produced in these two data frame:

unif_frame <- data.frame(1:30, most_probable_unif, cred_unif_high, cred_unif_low)
colnames(unif_frame) <- c('n', 'Most probabe value',
                          'credibility line (high)',
                          'credibility line (low)'); unif_frame

B_frame <- data.frame(1:30, most_probable_B, cred_B_high, cred_B_low)
colnames(B_frame) <- c('n', 'Most probabe value',
                          'credibility line (high)',
                          'credibility line (low)'); B_frame


#for every change in n, we get slightly different intervals.
#But for most probable quantities, for small n changes it changes dramatically,
#And then almost flattens.




#Finish :)



