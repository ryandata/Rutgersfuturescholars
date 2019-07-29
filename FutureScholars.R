###########################
### Rutgers Future Scholars
### Brief Sample R code
### Ryan Womack
### Summer 2019
############################

setwd("/home/ryan/Downloads/")
download.file("https://ed-public-download.app.cloud.gov/downloads/Most-Recent-Cohorts-All-Data-Elements.csv", "mydata.csv")
mydata<-read.csv("mydata.csv")

# we explore the data
head(mydata)
names(mydata)
attach(mydata)
plot(MD_EARN_WNE_P10~GRAD_DEBT_MDN_SUPP)
nrows()
nrow()
nrow(mydata)
class(MD_EARN_WNE_P10)
class(GRAD_DEBT_MDN_SUPP)
as.numeric(MD_EARN_WNE_P10)

# data is read in in text format, primarily due to presence of the word "NULL" and "PrivacySuppressed"
# we can find and replace all of the 
# we can convert variables one at a time using as.numeric
mydata$MD_EARN_WNE_P10<-as.numeric(mydata$MD_EARN_WNE_P10)
mydata$GRAD_DEBT_MDN_SUPP<-as.numeric(mydata$GRAD_DEBT_MDN_SUPP)
attach(mydata)

# ultimately it is easier to reimport the entire data frame using 
# as.is and na.strings options to et us bring the data in in the correct numeric format

mydata <- read.csv ("Most-Recent-Cohorts-Scorecard-Elements.csv", as.is=TRUE, na.strings = c("PrivacySuppressed","NULL"))
summary(mydata)
attach(mydata)

death deht adj earj9jgs


# now working better
plot(MD_EARN_WNE_P10~GRAD_DEBT_MDN_SUPP)

# we can start to explore the data with commands like histogram to generate a histogram
library(lattice)

histogram(MD_EARN_WNE_P10)
histogram(~MD_EARN_WNE_P10 | STABBR)
histogram(~MD_EARN_WNE_P10 | CONTROL)
histogram(~MD_EARN_WNE_P10 | HIGHDEG)

densityplot(~MD_EARN_WNE_P10 | CONTROL)
densityplot(~MD_EARN_WNE_P10 | CONTROL, layout=c(1,3))
densityplot(~MD_EARN_WNE_P10 | HIGHDEG, layout=c(1,5))



# other commands like max and sort can be used to undestand the data
max(MD_EARN_WNE_P10)
sort(MD_EARN_WNE_P10)

#### DATA ANALYSIS
# getting started
# check and set your working directory

getwd()
setwd("C:/Users/Staff/Desktop")

summary(mydata)

# the summary command provides a useful overview of the data and
# its values. We can also browse data in Excel if it is not
# too large.

# now create a subset of 4 year colleges only
mydata2<-subset(mydata, HIGHDEG=="3" | HIGHDEG=="4")

# we attach the data so that we can use short variable names
attach(mydata2)

# for graphing, we need extra packages
install.packages("lattice", dependencies=TRUE)
install.packages("ggplot2", dependencies=TRUE)

# after installing packages, use the library command to load packages into active memory
library(lattice)
library(ggplot2)

# first plots, some barcharts
# the lattice package uses the | symbol (found above the Return key on the keyboard)
# to indicate which categorical variable to use to break down the data

barchart(~PCTFLOAN|CONTROL)

# output is not very useful - just a count
# we probably want to know the average values of the variables
# so we have to compute that separately
# we use the aggregate function
# here PCTFLOAN is broken down by HBCU status (categorical variable comes second)
# use FUN=mean to compute the mean of the data, or substitute another function
# store the result as a new variable, here called res

res <- aggregate(PCTFLOAN ~ HBCU, data = mydata, FUN = mean)

# res is just text, so we need to plot it as follows

ggplot(res, aes(x = HBCU, y = PCTFLOAN)) + geom_bar(stat = "identity")

# ggplot2 is a powerful and extensive graphics package with many functions
# there are always at least two parts to a ggplot command
# first, specify the data and variables to be used
# second, after the + symbol, specify a method ("geom" for geometry) to use
# to plot the data

# experiment with your own combinations of variables here
# remember you must run both the aggregate and ggplot commands to generate
# useful output beyond counts of the data


#### DATA VISUALIZATION AND WEB APPS


##### Editing Data for use with GoogleVis
##### Not done in class
##### This merges all years of Scorecard Data into one file

# this should be edited to match your own directory
setwd("/home/ryan/Downloads/college-scorecard-raw-data-030216/")
library(reshape)

# this step is necessary to process the files and add a year marker to them
mynames<-paste("merged_",seq.int(1996,2013),"_PP.csv",sep="")
myChanges<-c("1996"="YEAR", "1997"="YEAR", "1998"="YEAR", "1999"="YEAR", "2000"="YEAR",
             "2001"="YEAR", "2002"="YEAR", "2003"="YEAR", "2004"="YEAR", "2005"="YEAR",
             "2006"="YEAR", "2007"="YEAR", "2008"="YEAR", "2009"="YEAR", "2010"="YEAR",
             "2011"="YEAR", "2012"="YEAR", "2013"="YEAR")

# creates new files for each year, with a year column added
for (i in seq(1:18))
{
  myname<-mynames[i]
  mydata<-read.csv(myname, as.is=TRUE, na.strings="NULL")
  j<<-i+1995
  mydata<-merge(j,mydata)
  
  write.csv(mydata, file=myname)
}

# merging all files happens outside of R
# In linux, go into terminal environment and cat files together with
# cat *.csv > mergedfile.csv

# Now we read in the big data file and select only the 4-year colleges
# using the subset command to look for matches

mydata<-read.csv("mergedfile.csv")
mydata2<-subset(mydata,HIGHDEG==3|HIGHDEG==4)

# we also want to filter the columns to only a few of the most
# relevant ones
# this is done with the subset+select combination below

mydata3<-subset(mydata2,select=c(x,INSTNM,CITY,STABBR,SATMTMID,SATVRMID,SATWRMID,
                                 ADM_RATE,UGDS,UGDS_WHITE,UGDS_BLACK,UGDS_ASIAN,UGDS_HISP,
                                 GRAD_DEBT_MDN_SUPP,pct25_earn_wne_p6,PCTFLOAN,md_earn_wne_p10))

# just cleaning up here, and making sure YEAR is numeric
mydata3<-rename(mydata3, c(x="YEAR"))
mydata3$YEAR<-as.numeric(mydata3$YEAR)
for (i in seq.int(5,17))
{
  mydata3[,i]<-as.numeric(mydata3[,i])
}


# removing duplicates (a problem for googleVis)
mydata3$NAME<-paste(mydata3$INSTNM,mydata3$CITY,mydata3$STABBR,sep="-") 

names(mydata3)

# and exporting the file for later use
write.csv(mydata3, file="select_college_data_1996_2013.csv")

# create New Jersey only dataset
mydata4<-subset(mydata3, STABBR=="NJ")
write.csv(mydata4, file="NJ_only_select_college_data_1996_2013.csv")

#read it in again
mydata4<-read.csv("NJ_only_select_college_data_1996_2013.csv")
##### Run GoogleVis

library(googleVis)
G <- gvisMotionChart(mydata4, idvar="NAME", timevar="YEAR")
plot(G)

#### Working with Shiny

#### Shiny files (app.R, ui.R, server.R) are stored separately
#### load app.R for a project that browses individual points
#### load ui.R and server.R for a scatterplot that lets you select variables