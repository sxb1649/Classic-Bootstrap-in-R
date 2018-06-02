# Classic-Bootstrap-in-R

BUAN 6357 – Fall 2017 (Johnston) HW 2 Due 8 Oct 2017 (Sunday) 
 
Problem Description: 
The file “benefits.csv” contains data about the compensation and benefits provided to individual teachers in multiple schools within multiple school districts.  The file “benefits_labels2.txt” provides a short description of each of the included variables.  Make sure your code pulls the data from directory “C:/data/BUAN6357/hw_2”. 
We need a classical bootstrap of the “benefits” data explaining average teacher salary.  We are only interested in global relationships so do not include District Identifier, School Identifier or non-salary benefits in the model(s).  We want 750 iterations of the classical bootstrap.  For initial validation purposes use 498541152 as your RNG seed. 
 
Deliverables: 
1. Original data frame   (Name: benefits_raw ) 2. Regression result    (Name: benefits_ols  ) 3. Classical bootstrap result   (Name: benefits_boot ) 
 
  
Notes: 
1. Keep all variable references in the original form and order to assist in validating your deliverables. 2. Part 1 of the homework is to compute the named deliverables according to the specifications.  Your role in this part is that of “programmer”. 3. Part 2 of the homework is to answer questions about the data and the analysis.  Not all the answers will be immediately available from the deliverables but can be obtained (calculated) from the deliverables.  Your role in this part of the homework is that of “analyst” but you are expected to also be able to extract additional information from the deliverables. 


---------------------------------------------------------------------------------------------------
      name:  <unnamed>
       log:  C:\Users\Jason\Desktop\data2\benefits_labels.txt
  log type:  text
 opened on:  17 Aug 2017, 13:13:20

. use C:\Users\Jason\Desktop\data\benefits.dta, clear

. describe

Contains data from C:\Users\Jason\Desktop\data\benefits.dta
  obs:         1,848                          
 vars:            18                          8 Feb 2011 16:42
 size:       147,840                          
---------------------------------------------------------------------------------------------------
              storage   display    value
variable name   type    format     label      variable label
---------------------------------------------------------------------------------------------------
distid          float   %9.0g                 district identifier
schid           int     %9.0g                 school identifier
lunch           float   %9.0g                 percent eligible, free lunch
enroll          int     %9.0g                 school enrollment
staff           float   %9.0g                 staff per 1000 students
exppp           int     %9.0g                 expenditures per pupil
avgsal          float   %9.0g                 average teacher salary, $
avgben          int     %9.0g                 average teacher non-salary benefits, $
math4           float   %9.0g                 percent passing 4th grade math test
story4          float   %9.0g                 percent passing 4th grade reading test
bs              float   %9.0g                 avgben/avgsal
lavgsal         float   %9.0g                 log(avgsal)
lenroll         float   %9.0g                 log(enroll)
lstaff          float   %9.0g                 log(staff)
bsbar           double  %10.0g                within-district avg of bs
lunchbar        double  %10.0g                within-district avg of lunch
lenrollbar      double  %10.0g                within-district avg of lenroll
lstaffbar       double  %10.0g                within-district avg of lstaff
---------------------------------------------------------------------------------------------------
Sorted by: distid  schid

. log close
      name:  <unnamed>
       log:  C:\Users\Jason\Desktop\data2\benefits_labels.txt
  log type:  text
 closed on:  17 Aug 2017, 13:13:20
---------------------------------------------------------------------------------------------------


#library(ggplot2)
library(dplyr)
library(broom)
library(sqldf)
setwd("C:/data/BUAN6357/hw_2")
getwd()

#Reading the dataframe
benefits_raw=read.csv('benefits.csv')
#Setting the seed
set.seed(498541152)

#Running the regression models
benefits_ols=lm(avgsal ~.-distid-schid-avgben, data=benefits_raw)

#Alternate way of doing it
#lm(avgsal ~., data=benefits_raw[,c(-1,-2,-8)])
#Running the bootstrap
benefits_boot		<-	benefits_raw	%>% 
  bootstrap(750)	%>%
  do(
    tidy(
           lm(avgsal ~.-distid-schid-avgben, data=.)
    )
  )

#Output
benefits_ols
benefits_boot


#Part2

#library(ggplot2)
library(dplyr)
library(broom)
library(sqldf)
setwd("C:/data/BUAN6357/hw_2")
getwd()

#Reading the dataframe
benefits_raw=read.csv('benefits.csv')
#Setting the seed
set.seed(498541152)

#Running the regression models
benefits_ols=lm(avgsal ~.-distid-schid-avgben, data=benefits_raw)

#Alternate way of doing it
#lm(avgsal ~., data=benefits_raw[,c(-1,-2,-8)])
#Running the bootstrap
benefits_boot		<-	benefits_raw	%>% 
  bootstrap(750)	%>%
  do(
    tidy(
      lm(avgsal ~.-distid-schid-avgben, data=.)
    )
  )

#Output
benefits_ols
benefits_boot

#To make the numbers readbale for elearning
options(scipen=6)
#after running HW1 code...run this code and get the CI values 
#CI code for non bootstrap ols
confint(benefits_ols)  


#term is the column name that can be visible by tidy(benefits_ols)
#output interpretationfor ols model  
#for lunch ->yes, remove lunch since 0 lies within CI
#for staff ->no, since 0 lies outside CI
#CI code for bootstrap ols
alpha	<-	0.05
bootci	<-	benefits_boot		%>% 
  group_by(term)	%>% 
  summarize(	low	=	quantile(estimate,   alpha/2),
             high	=	quantile(estimate, 1-alpha/2)
  )
bootci

#output interpretation for bootstrap model
#for lunch ->yes, remove lunch since 0 lies within CI
#for staff ->no, since 0 lies outside CI
