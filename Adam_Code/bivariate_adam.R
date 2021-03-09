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

###Bivariate plots###
library(magrittr)
(IllnessvsFinance=table(DF$covid_illness,DF$covid_finance,exclude = '')) #Cross tab
(IllnessvsFinance_mgCol=prop.table(IllnessvsFinance,
                                   margin = 2)%>%round(.,3)) #Pct

#making a data frame from contingency table

IllnessvsFinanceDF=as.data.frame(IllnessvsFinance)
names(IllnessvsFinanceDF)=c("illness","finance","counts")
IllnessvsFinanceDF <- IllnessvsFinanceDF[complete.cases(IllnessvsFinanceDF),]
#adding marginal percents:
IllnessvsFinanceDF$pctCol=as.data.frame(IllnessvsFinance_mgCol)[,3]
head(IllnessvsFinanceDF)

IllnessvsFinanceDF <- IllnessvsFinanceDF %>% mutate(illness = factor(illness, levels = rev(levels(illness))))

###Additional plot elements###
library(ggplot2)
base1=ggplot(data=IllnessvsFinanceDF, 
             aes(x=finance, y=pctCol,
                 fill=illness, width=.85)) # this 'aes' in legend

barDodge= base1 +  geom_bar(stat="identity",
                            position ='dodge') 
#barDodge
plot2 <- barDodge + scale_fill_brewer(palette="Red")
plot2

plot3 <- plot2 + theme(axis.text.x = element_text(size=7))
plot3
#TODO: reverse order of numbers in illness groups and find better 1st green color?; title; annotation; new axis labels; spread out finance columns?

#Add in titles
titleText='Concern about COVID Illness by Concer about Threat to Finances'
sourceText='Nigeria COVID-19 National Longitudinal Phone Survey (COVID-19 NLPS) 2020'

plot4 = plot3 + labs(title=titleText,
                     x =NULL, 
                     y = NULL,
                     caption = sourceText)

plot5 = plot4 + scale_y_continuous(labels=scales::unit_format(suffix = '%'))
plot5

#Save out
ggsave("illness_by_finance.png")
