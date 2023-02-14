#++++++++++++++++Data Mining SP23++++++++++++#
setwd("D:/AUAF Assignments/Spring 2023/ITC 360/Data-Mining/R")
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

#Chapter 2: Data pre processing
#Missing Values

#install.packages("mice")

#spotting the missing values

g=read.csv("navalues.csv")
is.na(g)
colSums(is.na(g))     #it gives all na values in each column
sum(is.na(g))         #gives the total number of na values
sum(is.na(g$Weight))  #gives na values for individual column

library(mice)
md.pattern(g)

##to take care of the missing values mitigation
h=g
View(h)

##omit the rows with missing values>>> the easiest way to deal NAs
h_new=na.omit(h)
View(h_new)
colSums(is.na(h_new))
md.pattern(h_new)
dim(h_new)

##for our analysis  we work only with Gender and Height
h_gen_hei=data.frame(h$Gender, h$Height)
names(h_gen_hei)=c("Gender","Height")
View(h_gen_hei)

colSums(is.na(h_gen_hei))
h_gen_hei=na.omit(h_gen_hei)
dim(h_gen_hei)
colSums(is.na(h_gen_hei))

###calculations in the presence of NAs
sum(is.na(h$Weight))
mean(h$Weight, na.rm = T)

####Very very very very important discussion here
##systematic missing values vs Random missing values 
View(g)

##test for random/syst missing values 
##Weight 
sum(is.na(g$Weight))
dum_na=is.na(g$Weight)  #binary variable recall from ITC 255
dum_na 

#Variable Gender: 

#Gender is QL  and dum_na is QL(binary)
#which test of association we use here>>>Chi-square test 
chisq.test(g$Gender, dum_na)  #No association, i.e. random missing values
##Height (ANT) and dum_na (Binary) >>>t-test 
t.test(g$Height~dum_na)   ##Hence no association, i.e. random missing values

#Chapter 2: Data pre processing
#out-liers
x=read.csv("no_na_values.csv")
View(x)
names(x)
str(x)
###suppose we wanna work with the variable Age
summary(x$Age)
plot(density(x$Age), col="blue")
##check for the outliers
boxplot(x$Age, horizontal = T)
boxplot.stats(x$Age)
###We remove the outliers only from Age
age_new=x$Age[x$Age<24]
boxplot(age_new, horizontal = T)
##you want to have a new dataset 
x_new=x[x$Age<24, ]
View(x_new)
###height
boxplot(x$Height, horizontal = T)
boxplot.stats(x$Height)

x_new_height=x[x$Height>58, ]
boxplot(x_new_height$Height, horizontal = T)

#Chapter 2: Data pre processing
#generating new attributes

names(x)
##Height in cm>>>generate a new var height in foot/feet
#1 cm=0.0328084 feet

height_in_feet=x$Height*0.0328084
height_in_feet
x=cbind(x, height_in_feet)
View(x)

##Decoding of the non-numerical var
##Gender
factor(x$Gender)
levels(x$Gender)
##0: Female, 1: Male
Gender_new=c()    ##empty vector 

for (i in 1:length(x$Gender)){
  if (x$Gender[i]=="Female"){
    Gender_new[i]=0
  } else {
    Gender_new[i]=1
  }
}


x = cbind(x, Gender_new)
View(x)
