###Load packages###
library(tidyverse)
library(ggplot2)
library(rio)

###Load Nigeria COVID data baseline wave###
setwd('/Users/adamporton/Documents/Academics/Winter 2021/PUBPOL 543 (Data Viz)/Group Project')
DF_baseline <- import('https://github.com/adam-porton/PubPol543/raw/main/r1_sect_a_3_4_5_6_8_9_12.csv')

###Subset by variables of interest###
DF <- DF_baseline %>%
  select(zone,state, lga, sector, ea, hhid, interviewer_id, wt_baseline, s9q1, s9q2)
DF <- rename(DF, covid_illness = s9q1,
             covid_finance = s9q2)
head(DF)
str(DF)

###Create table of variables of interest (minus N/As)###
##covid_illness##
#Create table
covid_illness_tbl <- table(DF$covid_illness,
                           exclude = "")
covid_illness_tbl <- prop.table(covid_illness_tbl)*100

#Create data frame
covid_illness_frame=as.data.frame(covid_illness_tbl)
#Rename vars
names(covid_illness_frame)=c("covid_illness","pct")

covid_illness_frame

##covid_finance##
#Create table
covid_finance_tbl <- table(DF$covid_finance,
                           exclude = "")
covid_finance_tbl <- prop.table(covid_finance_tbl)*100

#Create data frame
covid_finance_frame=as.data.frame(covid_finance_tbl)
#Rename vars
names(covid_finance_frame)=c("covid_finance","pct")

covid_finance_frame

tableFreq=tableFreq[order(tableFreq$pct),]

###Create covid_finance bar chart on full sample (minus N/As)###
base = ggplot(data = covid_finance_frame, 
              aes(x = covid_finance, 
                  y = pct)) 
plot1 = base + geom_bar(stat = 'identity',
                        fill = "#0073C2FF",
                        width = .6)

###Add additional graph elements###
#Add in titles
#titleText='How much of a threat coronavirus outbreak to household finances?'
#sourceText='Nigeria COVID-19 National Longitudinal Phone Survey (COVID-19 NLPS) 2020'

#plot2 = plot1 + labs(title=titleText,
#                     x =NULL, 
#                     y = NULL,
#                     caption = sourceText)

#plot3 = plot2 + scale_y_continuous(labels=scales::unit_format(suffix = '%'))
#plot3

#Save out
#ggsave("covid_finance.png")

###Create covid_illness bar chart on full sample (minus N/As)###
base = ggplot(data = covid_illness_frame, 
              aes(x = covid_illness, 
                  y = pct)) 
plot1 = base + geom_bar(fil ="gray",
                        stat = 'identity',
                        fill = "#0073C2FF",
                        width = .5) 

#Add in titles
titleText='How do you feel about possibility you/family might become ill from COVID-19 ?'
sourceText='Nigeria COVID-19 National Longitudinal Phone Survey (COVID-19 NLPS) 2020'

plot2 = plot1 + labs(title=titleText,
                     x =NULL, 
                     y = NULL,
                     caption = sourceText)

plot3 = plot2 + scale_y_continuous(labels=scales::unit_format(suffix = '%'))
plot3

#Save out
ggsave("covid_illness.png")
