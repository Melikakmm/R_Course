#EX1
volume <- setNames(c(7.45, 2.6, 2.3, 1.6, 1.2, 1.09, 1.08, 1.07, 0.97, 0.79, 0.77,
                     0.75, 0.35), c('Ness', 'Lomond', 'Morar', 'Tay', 'Awe', 'Maree',
                                    'Ericht', 'Lochy', 'Rannoch', 'Shiel', 'Katrine',
                                    'Arkaig', 'Shin'))
Area <- setNames(c(56, 71, 27, 26.4, 39, 28.6,
                   18.6, 16, 19, 19.5, 12.4, 16,
                   22.5 ), c('Ness', 'Lomond', 'Morar', 'Tay', 'Awe', 'Maree',
                             'Ericht', 'Lochy', 'Rannoch', 'Shiel', 'Katrine',
                             'Arkaig', 'Shin'))
len <- setNames(c(39, 36, 18.8, 23, 41, 20, 23, 16, 15.7, 28,12.9, 19.3, 27.8  )
                , c('Ness', 'Lomond', 'Morar', 'Tay', 'Awe', 'Maree',
                    'Ericht', 'Lochy', 'Rannoch', 'Shiel', 'Katrine',
                    'Arkaig', 'Shin'))
Maxdepth <- setNames(c(230, 190, 390, 150, 94, 114, 156, 162, 134, 128, 151,
                       109, 49), c('Ness', 'Lomond', 'Morar', 'Tay', 'Awe', 'Maree',
                                   'Ericht', 'Lochy', 'Rannoch', 'Shiel', 'Katrine',
                                   'Arkaig', 'Shin'))
Meandepth <- setNames(c(132, 37, 87, 60.6, 32, 38, 57.6, 70, 51, 40, 43.4, 46.5, 15.5), c('Ness', 'Lomond', 'Morar', 'Tay', 'Awe', 'Maree',
                                                                                          'Ericht', 'Lochy', 'Rannoch', 'Shiel', 'Katrine',
                                                                                          'Arkaig', 'Shin'))






scottish.lakes<-data.frame(volume, Area, len, Maxdepth, Meandepth)
scottish.lakes


#Q1
min(scottish.lakes[,1])
max(scottish.lakes[,1])
min(scottish.lakes[,2])
max(scottish.lakes[,2])
#Q2
scottish.lakes <- scottish.lakes[order(Area), ]
#two largest are last two areas, when the data is ordered.
scottish.lakes[length(Area), 2]
scottish.lakes[(length(Area)-1), 2]
#Q3
A<-sum(scottish.lakes[,2])
cat('Area occupied by water in scotland:', A, 'KM^2')
#----------------------------------------------------------------------------
#EX2

#Q1
file <- read.csv('/Users/melikakeshavarz/downloads/crude-oil-prices.csv',
                  sep = ',', fill = TRUE)
#checking to see whether it is a dataframe or not.
class(file)

file


#Q2
library(ggplot2)
fig<-ggplot(data = file, aes(x=Year, y=Oil...Crude.prices.since.1861..current..., color = 'Oil Price'))+geom_line()+
  ylab('Oil Price')+ggtitle('Oil Price As A Function Of The Year')
fig


#Q3
which.max(file$Oil...Crude.prices.since.1861..current...)
cat('The highest price:',
    file[152, 4],'$', '_ which happened in year:', file[152, 3])



#Q4
#the price column is put in the list below.
price <- file[,4]
price[1]

#the finite difference formula is introduced as a function.

finite.differences <- function(x) {
  dif <- vector(length = 160)
  for(i in 1:159) {
    dif[i] <- x[i+1]-x[i]

  }
  dif
  return(dif)
}
#input : the list of price
#der stands for derivative

der<-finite.differences(price)
der.plot <- ggplot()+geom_line(aes(x =file[ 1:length(der),3],y= der), colour="#E69F00")+
     ggtitle('Finite Difference')+ xlab( 'year')+ ylab ( 'speed of price changes')
der.plot
#------------------------------------------------------------------------------
#EX3

#Q1

#put the data in a tibble.
library(tibble)
coal.frame<-read.csv('/Users/melikakeshavarz/Downloads/coal-production-by-country.csv',
         sep = ',')
coal.frame
coal.t<-as_tibble(coal.frame)
coal.t



#Q2
#showing the name of countries.
#we should get rid of continents and oceans and general parts.
remove <-c( "Asia Pacific", "Central and South America","North America" ,
            "OECD", "Other Africa", "Other CIS", "Persian Gulf", "Reunion",
            "South Africa", "USSR", "World", "Western Sahara", "United States Pacific Islands",
            "South & Central America" , "Other S. & Cent. America" ,"Other Asia & Pacific",
            "OPEC" ,"Europe", "EU-28","CIS","Central African Republic",
            "Asia and Oceania","Africa" )

#In this tibble below we just have countries' names.
country.tibble <- coal.t[!grepl(paste(remove, collapse='|'), coal.t$Entity),]
country.tibble


#to avoid repetition:
world<-unique(country.tibble[,1])
world


#number of countries participated in this table.

library(dplyr) 
n<-count(world)
n



#showing the number of entries for each country.
countries <- table(country.tibble[, 1])
countries


#plotting the countries entity
#plotly is a tremendous way to observe bar plots with a lot of data
#install.packages("plotly")
library(plotly)
library(ggplot2)


Entity.barplot <- ggplot(as.data.frame(countries), aes(x=names(countries),
                                                       y = Freq)) + 
  geom_bar(stat="identity", fill = rainbow(length(names(countries))))+
  coord_flip()


Entity.barplot %>% ggplotly
Entity.barplot




#Q3
#I created a tibble of aggregated data starting from 1970.
coal_after_1970<-country.tibble$Coal.production..TWh.[country.tibble$Year>=1970]
Entity_after_1970 <- country.tibble$Entity[country.tibble$Year>=1970]
year <- country.tibble$Year[country.tibble$Year>= 1970]


AGG.frame<-aggregate.data.frame(country.tibble$Coal.production..TWh.[country.tibble$Year>=1970],
          by = list(Entity_after_1970), FUN = sum)
AGG.frame
#in order to get top 5 countries in producing coal, we should sort
#our data in a descending way and get the 5 countries at the top.
Top_5<-AGG.frame[rev(order(AGG.frame$x)),] %>% head(5)
#The top 5 countries with the highest production:
Top_5



#Q4
#For the sake of comfort, I made tibbles out of Top 5 countries and their productions after 1970 .
#coal_after_1970 = coal production after 1970.
tibble_1970 <- tibble(Entity_after_1970,year, coal_after_1970)
tibble_China <- tibble_1970[tibble_1970$Entity_after_1970 == 'China',]
tibble_United_States <- tibble_1970[tibble_1970$Entity_after_1970 == 'United States',]
tibble_Eurasia <- tibble_1970[tibble_1970$Entity_after_1970 == 'Eurasia',]
tibble_Russia <- tibble_1970[tibble_1970$Entity_after_1970 == 'Russia',]
tibble_Australia <- tibble_1970[tibble_1970$Entity_after_1970 == 'Australia',]

library(plotly)
Top_plot <- ggplot() + geom_line(data = tibble_China, aes(x = year, y = coal_after_1970,  color = 'China'))+
  geom_line( data = tibble_United_States, aes(x = year, y = coal_after_1970, color = 'United States'))+
  geom_line(data = tibble_Eurasia, aes(x = year, y= coal_after_1970, color = 'Eurasia'))+
  geom_line(data = tibble_Russia, aes(x = year, y= coal_after_1970, color = 'Russia'))+
  geom_line(data = tibble_Australia, aes(x= year, y= coal_after_1970, color = 'Australia'))+xlab('Year')+ylab('Coal Produnction')+
  ggtitle('Production Of Top Five Countries Over Years After 1970')
  xlim(1970, 2019)
Top_plot




#Q5
#I created a tibble of cumulative sum of world's data

world_cumsum.tibble <-coal.t[coal.t$Entity == 'World',]
world_cumsum.tibble$cumsum<- cumsum(world_cumsum.tibble$Coal.production..TWh.)
world_cumsum.tibble

library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 
world_cumsum.plot<-barplot(height=world_cumsum.tibble$cumsum, names=world_cumsum.tibble$Year, col=coul,
                           xlab = 'Years', ylab = 'Cumsum', main = "Cumulative Sum Of World's Production")

#--------------------------------------------------------------------------------------------------
#EX4

#Q1
#I read the file with read_csv and it automatically convert it to a tibble.
#then I separated the data form Italy
library(tidyverse)
vac_data<- read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations-by-manufacturer.csv')


vac_data
vac_data %>%
  filter(location == 'Italy') -> vac_ITA
vac_ITA


#Q2
#plotting the number of vaccines used over time for each company.
#I used filter in each plot to plot all vaccines brands in a single plot.

ITA_plot <- ggplot()+geom_line(data = filter(vac_ITA, vaccine == 'Moderna'), aes(x = date, y=total_vaccinations, color = 'Moderna'))+
  geom_line(data = filter(vac_ITA, vaccine == 'Pfizer/BioNTech'), aes(x = date, y = total_vaccinations, color = 'Pfizer/BioNTech'))+
  geom_line(data = filter(vac_ITA, vaccine == 'Johnson&Johnson'), aes(x = date, y = total_vaccinations, color = 'Johnson&Johnson' ))+
  geom_line(data = filter(vac_ITA, vaccine == 'Oxford/AstraZeneca'), aes(x = date, y = total_vaccinations, color = 'Oxford/AstraZeneca' ))+
  geom_line(data = filter(vac_ITA, vaccine == 'Novavax'), aes(x = date, y = total_vaccinations, color = 'Novavax' ))+theme_grey()+
  ggtitle('(Italy)The total number of vaccines used by different companies over time')
ITA_plot


#Q3
#plot vaccine shots per day(independent on the brand)
#I grouped the data with same dates.(get the sum of total_vaccinations with the same data ) 
vac_ITA %>% group_by(date) %>% summarise_at(vars(total_vaccinations),funs(sum)) -> shots_ITA
shots_plot_ITA <- ggplot(shots_ITA, aes(x = date, y = total_vaccinations )) +geom_line(colour = "#56B4E9")+
  ylab('Total Shots Per Day')+ggtitle("Italy's total shot")
shots_plot_ITA


#Q4
#here we do the same for GERMANY and UNITED STATES.

######Germany
library(ggplot2)
vac_data %>%
  filter(location == 'Germany') -> vac_Ger
vac_Ger

#plotting the total vaccines with respect to time for different brands.
Ger_plot <- ggplot()+geom_line(data = filter(vac_Ger, vaccine == 'Moderna'), aes(x = date, y=total_vaccinations, color = 'Moderna'))+
  geom_line(data = filter(vac_Ger, vaccine == 'Pfizer/BioNTech'), aes(x = date, y = total_vaccinations, color = 'Pfizer/BioNTech'))+
  geom_line(data = filter(vac_Ger, vaccine == 'Johnson&Johnson'), aes(x = date, y = total_vaccinations, color = 'Johnson&Johnson' ))+
  geom_line(data = filter(vac_Ger, vaccine == 'Oxford/AstraZeneca'), aes(x = date, y = total_vaccinations, color = 'Oxford/AstraZeneca' ))+
  geom_line(data = filter(vac_Ger, vaccine == 'Novavax'), aes(x = date, y = total_vaccinations, color = 'Novavax' ))+theme_gray()+
  ggtitle('(Germany)The total number of vaccines used by different companies over time')+
  scale_color_manual(values=c("#CC79A7","#D55E00","#56B4E9","#000000","#F0E442"))
Ger_plot

#group the data by the same date.
vac_Ger %>% group_by(date) %>% summarise_at(vars(total_vaccinations),funs(sum)) -> shots_Ger

shots_plot_Ger <- ggplot(shots_Ger, aes(x = date, y = total_vaccinations)) +geom_line(colour = 'green')+
  ylab('Total Shots Per Day')+ggtitle("Germany's total shot")
shots_plot_Ger

#####United states of America
vac_data %>%
  filter(location == 'United States') -> vac_Am
vac_Am
#By using Unique function for the vaccines, I understand America just used Moderna, Pfizer/Biotech
#and Johnson&Johnson.

unique(vac_Am$vaccine)

#plotting the total vaccines with respect to time for different brands.
Am_plot <- ggplot()+geom_line(data = filter(vac_Am, vaccine == 'Moderna'), aes(x = date, y=total_vaccinations, color = 'Moderna'))+
  geom_line(data = filter(vac_Am, vaccine == 'Pfizer/BioNTech'), aes(x = date, y = total_vaccinations, color = 'Pfizer/BioNTech'))+
  geom_line(data = filter(vac_Am, vaccine == 'Johnson&Johnson'), aes(x = date, y = total_vaccinations, color = 'Johnson&Johnson' ))+
  theme_grey()+ggtitle('(Germany)The total number of vaccines used by different companies over time')
Am_plot

vac_Am %>% group_by(date) %>% summarise_at(vars(total_vaccinations),funs(sum)) -> shots_Am
shots_plot_Am <- ggplot(shots_Am, aes(x = date, y = total_vaccinations)) +geom_line(colour = 'purple')+
  ylab('Total Shots Per Day')+ggtitle("American's total shot")
shots_plot_Am

#...............................................................................

#Question 4 continued

library(tidyverse)


#the type of column with dates was not clear, so here not only did I read the data with csv reader,
#but I also specified type of columns with date. 
new_vac_data <- read_csv("/Users/melikakeshavarz/Downloads/covid-19-data-master/public/data/vaccinations/vaccinations.csv")
s <- spec(new_vac_data)
s
cols_condense(s)
date = col_character()

new_vac_data

#plotting for European countries.
#first we filter out the OWID_EUR.

new_vac_data %>%
  filter(iso_code == 'OWID_EUR' ) -> Eur_vac
Eur_vac

#install.packages("scales")
library(scales)
library(ggplot2)


#I used scale to change the format of the days, I could have scaled the x axis in to come up with
#daily scaling of x coordinate, but the plot would have been really unclear. 
Eur_vac_plot <- ggplot()+geom_line(data = Eur_vac, aes(x =date, y= daily_vaccinations_per_million),
                                   colour='darkred')+
  scale_x_date(labels = date_format("%y/%m/%d"))+theme_linedraw() 
Eur_vac_plot
#................................................................
#Last Question
#studying

#sad plot....

income_plot <- ggplot() + geom_line(data = filter(new_vac_data, iso_code == 'OWID_LIC'),
                                    aes(x = date, y= total_vaccinations, color = 'LOW INCOME'))+
                          geom_line(data= filter(new_vac_data, iso_code == 'OWID_HIC' ),
                                           aes(x = date, y= total_vaccinations, color = 'HIGH INCOME'))+
                          geom_line(data = filter(new_vac_data, iso_code == 'OWID_UMC'),
                                           aes(x = date, y= total_vaccinations, color = 'UPPER MIDDLE INCOME'))+
                          geom_line(data = filter(new_vac_data, iso_code == 'OWID_LMC'),
                                          aes(x = date, y= total_vaccinations, color = 'LOWER MIDDLE INCOME'))+
                          theme_grey()
income_plot












