###########################
### Rutgers Future Scholars
### Brief Sample R code
### Ryan Womack
### Summer 2019
############################

## SETUP

# for graphing, we need extra packages
install.packages("lattice", dependencies=TRUE)
install.packages("ggplot2", dependencies=TRUE)

# after installing packages, use the library command to load packages into active memory
library(lattice)
library(ggplot2)


## DATA WRANGLING - data import and cleaning

# grab your data
# adjust working directory as necessary
setwd("/home/ryan/Downloads/")

# can import data directly into R with these steps
download.file("https://ed-public-download.app.cloud.gov/downloads/Most-Recent-Cohorts-All-Data-Elements.csv", "mydata.csv")

# a couple of options are necessary to get the data to read in smoothly
mydata <- read.csv("mydata.csv",  as.is=TRUE, na.strings = c("PrivacySuppressed","NULL"))

# we explore the data
head(mydata)
names(mydata)

# attaching the data lets us easily use variable names in their short form
attach(mydata)

# for example
plot(MD_EARN_WNE_P10~GRAD_DEBT_MDN_SUPP)

# understand the data structure
nrow(mydata)

class(MD_EARN_WNE_P10)
class(GRAD_DEBT_MDN_SUPP)
MD_EARN_WNE_P10
as.numeric(MD_EARN_WNE_P10)

table(CONTROL)
table(HIGHDEG)

# limit to 4-yr institutions and eliminate for profits
mydata <- subset(mydata, CONTROL==1 | CONTROL==2)
mydata <- subset(mydata, HIGHDEG==3 | HIGHDEG==4)
attach(mydata)

table(CONTROL)
table(HIGHDEG)

plot(MD_EARN_WNE_P10~GRAD_DEBT_MDN_SUPP)

# we can start to explore the data with commands like histogram to generate a histogram
library(lattice)

# the lattice package uses the | symbol (found above the Return key on the keyboard)
# to indicate which categorical variable to use to break down the data

histogram(MD_EARN_WNE_P10)
histogram(~MD_EARN_WNE_P10 | STABBR)
histogram(~MD_EARN_WNE_P10 | CONTROL, strip=strip.custom(strip.levels=c(TRUE,TRUE)))
histogram(~MD_EARN_WNE_P10 | HIGHDEG, strip=strip.custom(strip.levels=c(TRUE,TRUE)))
densityplot(~MD_EARN_WNE_P10 | CONTROL)
densityplot(~MD_EARN_WNE_P10 | CONTROL, layout=c(1,2), strip=strip.custom(strip.levels=c(TRUE,TRUE)))
densityplot(~MD_EARN_WNE_P10 | HIGHDEG, layout=c(1,2), strip=strip.custom(strip.levels=c(TRUE,TRUE)))

# other commands like max and sort can be used to undestand the data
max(MD_EARN_WNE_P10)
sort(MD_EARN_WNE_P10)

#### DATA ANALYSIS

# first plots, some barcharts


barchart(~PCTFLOAN|CONTROL)

# output is not very useful - just a count
# we probably want to know the average values of the variables
# so we have to compute that separately
# we use the aggregate function
# here PCTFLOAN is broken down by public/private status (categorical variable comes second)
# use FUN=mean to compute the mean of the data, or substitute another function
# store the result as a new variable, here called res

res <- aggregate(PCTFLOAN ~ CONTROL, data = mydata, FUN = mean)

# res is just text, so we need to plot it as follows

ggplot(res, aes(x = CONTROL, y = PCTFLOAN)) + geom_bar(stat = "identity")

# ggplot2 is a powerful and extensive graphics package with many functions
# there are always at least two parts to a ggplot command
# first, specify the data and variables to be used
# second, after the + symbol, specify a method ("geom" for geometry) to use
# to plot the data

# experiment with your own combinations of variables here
# remember you must run both the aggregate and ggplot commands to generate
# useful output beyond counts of the data


## REGRESSION

# remember this plot?
plot(MD_EARN_WNE_P10~GRAD_DEBT_MDN_SUPP)

cor(MD_EARN_WNE_P10,GRAD_DEBT_MDN_SUPP, use="complete.obs")
summary(lm(MD_EARN_WNE_P10~GRAD_DEBT_MDN_SUPP))

# ggplot can show us this relationship graphically

ggplot(mydata,aes(y=MD_EARN_WNE_P10,x=GRAD_DEBT_MDN_SUPP))+geom_point()+geom_smooth(method='lm')


# EASY ANALYSIS FUNCTION

# let's try to find what affects earnings

myearn<-function(z)
{
  print(densityplot(z))
  plot(MD_EARN_WNE_P10~z)
  ggplot(mydata,aes(y=MD_EARN_WNE_P10,x=z))+geom_point()+geom_smooth(method='lm')
}

myearn(GRAD_DEBT_MDN_SUPP)

plot(MD_EARN_WNE_P10~GRAD_DEBT_MDN_SUPP)
ggplot(mydata,aes(y=MD_EARN_WNE_P10,x=GRAD_DEBT_MDN_SUPP))+geom_point()+geom_smooth(method='lm')

# we can sort, filter, and export our data

deadlycolleges <- subset(mydata, DEATH_YR4_RT>0)
deadlycolleges <- deadlycolleges[order(deadlycolleges$DEATH_YR4_RT),]
deadlycolleges[,"INSTNM"]
write.csv(deadlycolleges, file="deadlycolleges.csv")

# something more usual, like SAT

brainycolleges <- subset(mydata, SAT_AVG>1400)
brainycolleges <- brainycolleges[order(brainycolleges$SAT_AVG),]
brainycolleges[,"INSTNM"]
write.csv(brainycolleges, file="brainycolleges.csv")

###############
# Evaluating Population
# Functions using the idbr package
# developed for Rutgers Future Scholars
# by Ryan Womack
# 2017-07-12
###############

# see 
# https://www.census.gov/data/developers/data-sets/international-database.html
# for information of variables used by idb
#
# look up FIPS country codes here
# https://en.wikipedia.org/wiki/List_of_FIPS_country_codes
#
# FIPS are standardized two-letter country codes and are 
# the easiest way to input country information for idbr


#####
# install required packages
#####

install.packages("idbr", dependencies=TRUE)
install.packages("ggthemes", dependencies=TRUE)
install.packages("animation", dependencies=TRUE)
install.packages("dplyr", dependencies=TRUE)

#####
# load required packages
#####

library(idbr) # devtools::install_github('walkerke/idbr')
library(ggplot2)
library(animation)
library(dplyr)
library(ggthemes)

######
# setup
# you must request an Census API key for this package to work
# get your key at
# http://api.census.gov/data/key_signup.html

# idb_api_key('you must put your Census API key in here for this to work')
idb_api_key('72e24b879a932f21b4ec758546dd8a5193282bec')

######

# fertility

# function to compare fertility rates
# input two country codes, start year, end year
# put quotes around country codes

fertility <- function(a,b,c,d)
{
  country_one<-a
  country_two<-b
  start_year<-c
  end_year<-d
  
  ssr_df <- idb5(c(country_one,country_two), year=c:d, variable='TFR' ,country_name = TRUE)
  
  ggplot(ssr_df, aes(x = time, y=TFR, color=NAME)) + 
    geom_line(size = 1) + 
    ylab('Total Fertility Rate') + 
    xlab('Year') + 
    theme(legend.title = element_blank(), 
          legend.position = "bottom") 
}

######

# death rate

# function to compare death rates
# input two country codes, start year, end year
# put quotes around country codes

death <- function(a,b,c,d)
{
  country_one<-a
  country_two<-b
  start_year<-c
  end_year<-d
  
  ssr_df <- idb5(c(country_one,country_two), year=c:d, variable='CDR' ,country_name = TRUE)
  
  ggplot(ssr_df, aes(x = time, y=CDR, color=NAME)) + 
    geom_line(size = 1) + 
    ylab('Crude Death Rate') + 
    xlab('Year') + 
    theme(legend.title = element_blank(), 
          legend.position = "bottom") 
}

######

# growth rate

# function to compare growth rates
# input two country codes, start year, end year
# put quotes around country codes

growth <- function(a,b,c,d)
{
  country_one<-a
  country_two<-b
  start_year<-c
  end_year<-d
  
  ssr_df <- idb5(c(country_one,country_two), year=c:d, variable='GR' ,country_name = TRUE)
  
  ggplot(ssr_df, aes(x = time, y=GR, color=NAME)) + 
    geom_line(size = 1) + 
    ylab('Growth Rate') + 
    xlab('Year') + 
    theme(legend.title = element_blank(), 
          legend.position = "bottom") 
}


######

# population

# function to compare total population
# input two country codes, start year, end year
# put quotes around country codes

population <- function(a,b,c,d)
{
  country_one<-a
  country_two<-b
  start_year<-c
  end_year<-d
  
  ssr_df <- idb5(c(country_one,country_two), year=c:d, variable='POP' ,country_name = TRUE)
  
  ggplot(ssr_df, aes(x = time, y=POP, color=NAME)) + 
    geom_line(size = 1) + 
    ylab('Total Population') + 
    xlab('Year') + 
    theme(legend.title = element_blank(), 
          legend.position = "bottom") 
}


#####
# Note we can also retrieve sets of concepts like this
ssr_df <- idb5(c('RS','US'), year=1990:2015, concept='Components of population growth' ,country_name = TRUE)
ssr_df <- idb5(c('RS'), year=1990:2015, concept='Fertility rates' ,country_name = TRUE)



##############################
# Population Pyramid
# 
# Create a population pyramid!
##############################

pyramid2<-function(countrycode,countryname,startyear,endyear)
{
  
  # countrycode<-c('UK')
  # countryname<-"UK"
  # startyear=2010
  # endyear=2050
  
  
  
  male <- idb1(countrycode, startyear:endyear, sex = 'male') %>%
    mutate(POP = POP * -1,
           SEX = 'Male')
  
  female <- idb1(countrycode, startyear:endyear, sex = 'female') %>%
    mutate(SEX = 'Female')
  
  countrydata <- rbind(male, female) %>%
    mutate(abs_pop = abs(POP))
  
  # Animate it with a for loop
  
  saveHTML({
    
    for (i in startyear:endyear) {
      
      title <- as.character(i)
      
      year_data <- filter(countrydata, time == i)
      
      g1 <- ggplot(year_data, aes(x = AGE, y = POP, fill = SEX, width = 1)) +
        coord_fixed() +
        coord_flip() +
        annotate('text', x = 98, y = -800000,
                 label = 'Data: US Census Bureau IDB; idbr R package', size = 3) +
        geom_bar(data = subset(year_data, SEX == "Female"), stat = "identity") +
        geom_bar(data = subset(year_data, SEX == "Male"), stat = "identity") +
        scale_y_continuous(breaks = seq(-1000000, 1000000, 500000),
                           labels = paste0(as.character(c(seq(1, 0, -0.5), c(0.5, 1))), "m"),
                           limits = c(min(countrydata$POP), max(countrydata$POP))) +
        theme_economist(base_size = 14) +
        scale_fill_manual(values = c('#ff9896', '#d62728')) +
        ggtitle(paste0('Population structure of ',countryname, title)) +
        ylab('Population') +
        xlab('Age') +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        guides(fill = guide_legend(reverse = TRUE))
      
      print(g1)
      
    }
    
  }, htmlfile="index.html")
  
  
}

# for example
death("UK","US", 2010, 2050)
fertility("UK","US", 2010, 2050)
growth("UK","US", 2010, 2050)
population("UK","US", 2010, 2050)
pyramid2("UK","UK", 2010, 2050)
