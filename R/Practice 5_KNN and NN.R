#supervised Learning KNN/ITC360
getwd()
setwd("D:/AUAF Assignments/Spring 2023/ITC 360/Data-Mining/R")
g=read.csv("grades1.csv")
head(g)
#Problem: predict whether a new student belongs to A or Not A group
#using their midterm and Quizzes scores
###create predictor matrix and classification matrix
g.x=g[,c(3,4)]
head(g.x)
g.y=g[,8]
head(g.y)
####Split the dataset
s=sample(nrow(g), floor(0.8*nrow(g)))
s
dim(g)
g.train=g.x[s, ]
g.test=g.x[-s, ]
train.y=g.y[s]
test.y=g.y[-s]
NROW(train.y)
###Specify k
sqrt(nrow(g))  ###k=11 better to be odd
#install.packages("class")
library(class)
g.knn=knn(train = g.train, test = g.test, cl = train.y, k=11)
g.knn
g.pr=data.frame(g.test$MT, g.test$Q, test.y, g.knn)
View(g.pr)
##Confusion matrix
table(test.y, g.knn)
err.knn= 5/NROW(test.y)
err.knn  #.2
######################New data
MT=c(98, 70, 60, 93, 60)
Q=c(92, 89, 78, 91, 80)
###where Knn would classify them
pre.new=data.frame(cbind(MT, Q))
pre.new
g.knn1=knn(train = g.train, test = pre.new, cl = train.y, k=11)
View(g.knn1)
table(g.knn1)

##---Artificial neural network classification--##
##a new data set on students scores and grades (real dataset)
g=read.csv("grades1.csv")
nrow(g)
#install.packages("neuralnet")
library(neuralnet)
head(g)
#problem: classify the students in A or NotA using thier
#MT score and Q Score
x=data.frame(g$MT, g$Q, g$Grade)
colnames(x)=c("MT", "Q", "Grade")
head(x)
#Split the dataset x into training and test
s=sample(nrow(x), floor(0.8*nrow(x)))
x.train=x[s, ]
x.test=x[-s, -3]
head(x.test)
x.test.y=x[-s, 3]
head(x.test)
head(x.test.y)
##Run the ANN classification Model
nn=neuralnet(Grade~MT+Q, data =x.train, hidden = 3,
             act.fct = "logistic", linear.output = FALSE)
plot(nn)
##Compute the error rate using test dataset
pr.test=compute(nn, x.test)
p1=pr.test$net.result
head(p1)
pred=ifelse(p1>0.5, 1, 0)  ###dummy variable
table(x.test.y, pred)   ##confussion matrix
Err.rate=7/NROW(x.test)
Err.rate #.29
###Use the ANN model to classify new students
MT=c(94, 90, 60)
Q=c(97, 50, 70)
new.st=data.frame(cbind(MT, Q))
head(new.st)
pr.test=compute(nn, new.st)
p1=pr.test$net.result
pred0=ifelse(p1>0.5, 1, 0)  ##dummay variable
pred0

