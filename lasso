### data preparation

library(readr)
data2 <- read_delim("C:/Users/dr.tapak/Desktop/Gharebaghi/data2.txt", 
                    "\t", escape_double = FALSE, col_types = cols(ID = col_number()), 
                    trim_ws = TRUE)
data <- data2
nn <- data[,1]

#data[,117]
data3 <-data.frame(data[,-c(1,117)]) 
dim(data3)
colnames(data3)

row.names(data3) <- as.vector(as.matrix(nn[1:2566,]))

rownames(data3)


my.data <- data.frame(t(data3))
colnames(my.data)
rownames(my.data)
is.data.frame(my.data)
dim(my.data)
yy<- my.data[,ncol(my.data)]
aaa <- data.frame(cbind(my.data$MIMAT0005582,my.data$MIMAT0019776,my.data$MIMAT0027430,
                        my.data$MIMAT0027436,my.data$MIMAT0027474,my.data$MIMAT0015079,
                        my.data$MIMAT0003320,my.data$MIMAT0004970,my.data$MIMAT0005922,
                        my.data$MIMAT0015075,my.data$MIMAT0018949,my.data$MIMAT0022259,
                        my.data$MIMAT0019776,my.data$MIMAT0027392,yy))



dim(aaa)
colnames(aaa) <-c("MIMAT0005582","my.data$MIMAT0019776","MIMAT0027430",
                  "MIMAT0027436","MIMAT0027474","MIMAT0015079",
                  "MIMAT0003320","MIMAT0004970","MIMAT0005922",
                  "MIMAT0015075","MIMAT0018949","MIMAT0022259",
                  "MIMAT0019776","MIMAT0027392","y") 
xx <- aaa


aaa$y <- factor(aaa$y)

"MIMAT0005582" "MIMAT0019776" "MIMAT0027430" "MIMAT0027436" "MIMAT0027474"

###########Logistic regression
aaa$y <- factor(aaa$y)
mylogit <- glm(y ~ MIMAT0005582, data = aaa, family = "binomial")
summary(mylogit)

mylogit <- glm(y ~ MIMAT0019776, data = aaa, family = "binomial")
summary(mylogit)

mylogit <- glm(y ~ MIMAT0027430, data = aaa, family = "binomial")
summary(mylogit)
mylogit <- glm(y ~ MIMAT0027436, data = aaa, family = "binomial")
summary(mylogit)


mylogit <- glm(y ~ MIMAT0027474, data = aaa, family = "binomial")
summary(mylogit)


mylogit <- glm(y ~  MIMAT0015079, data = aaa, family = "binomial")  ##MCP
summary(mylogit) 
MIMAT0003320


mylogit <- glm(y ~  my.data$MIMAT0003320, data = aaa, family = "binomial")  ##MCP
summary(mylogit) 



mylogit <- glm(y ~   my.data$MIMAT0004970, data = aaa, family = "binomial")  ##MCP
summary(mylogit) 



mylogit <- glm(y ~  my.data$MIMAT0004970, data = aaa, family = "binomial")  ##MCP
summary(mylogit) 


mylogit <- glm(y ~  my.data$MIMAT0005922, data = aaa, family = "binomial")  ##MCP
summary(mylogit) 




mylogit <- glm(y ~  my.data$MIMAT0015075, data = aaa, family = "binomial")  ##MCP
summary(mylogit) 


mylogit <- glm(y ~  my.data$MIMAT0018949, data = aaa, family = "binomial")  ##MCP
summary(mylogit) 



mylogit <- glm(y ~  my.data$MIMAT0022259, data = aaa, family = "binomial")  ##MCP
summary(mylogit) 




mylogit <- glm(y ~  my.data$MIMAT0019776, data = aaa, family = "binomial")  ##MCP
summary(mylogit) 




mylogit <- glm(y ~  my.data$MIMAT0027392, data = aaa, family = "binomial")  ##MCP
summary(mylogit) 

library(psych)
t.test(aaa$MIMAT0005582,yy)
describeBy(aaa$MIMAT0005582,aaa$y)
#################################RF



iris.obj <- rfsrc(y ~ ., data =aaa,mtry = 3,ntree = 1000)
## wisconsin prognostic breast cancer data

plot(iris.obj)

plot.variable(iris.obj, partial = TRUE,plots.per.page = 2)

#################Final data is my.data which is a data frame
install.packages("devtools")
install.packages("grpreg")
#install.packages("fastDummies")
library(grpreg)
library(knitr)
library("devtools")
#library("fastDummies")

library(caret) ## for creting dummy variables


library(grpreg)






