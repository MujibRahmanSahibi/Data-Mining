#++++++++++++++++Data Mining SP23++++++++++++#
#setwd("G:/My Drive/Spring23/ITC360/Datasets")
###Upload the dataset to R 
x=read.csv("datapreprocessing.csv")

View(x)
dim(x)

##name of attributes
names(x)


#Chapter 2: Data pre processing
#Inconsistent values

#Gender

class(x$Gender)
table(x$Gender)

#child is an inconsistent value for Gender
x$Gender=gsub("Child", NA, x$Gender)
table(x$Gender)
x$Gender

#Height  ##nature numerical 
summary(x$Height)
class(x$Height)
x$Height

x$Height=gsub("164 cm ", 164, x$Height)
x$Height=gsub("186cm", 186, x$Height)
x$Height
class(x$Height)
x$Height=as.numeric(x$Height)
summary(x$Height)

#Weight
x$Weight
x$Weight=gsub("Male", NA, x$Weight)
x$Weight=gsub("54kg", 54, x$Weight)
x$Weight=as.numeric(x$Weight)
class(x$Weight)
summary(x$Weight)

## replace function
x=read.csv("datapreprocessing.csv")
View(x)
x_height=x$Height
class(x_height)
x_height=as.character(x_height)
x_height
x_height=replace(x_height, x_height %in% c("186cm", "164 cm "), c(186, 164))
x_height
x_height=as.numeric(x_height)
summary(x_height)

#Chapter 2: Data pre processing
#Duplicated rows 
x=read.csv("datapreprocessing.csv")
View(x)
##package tidyverse
#install.packages("tidyverse")
library(tidyverse)
dp_value=duplicated(x)
table(dp_value)

##Lets introduce some duplicated objects (rows)
dp_rows=x[c(1,2), ]
View(dp_rows)

y=rbind(x, dp_rows)
View(y)

dp_object=duplicated(y)
table(dp_object)

y[duplicated(y), ]   #this gives the duplicated rows with all details

###how to remove duplicated Rows/Objects
y_unique=unique(y)
y_dup=duplicated(y_unique)
table(y_dup)
y_dup
