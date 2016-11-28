
#11/22
#Jing

#Read the datasets
D1<-extra.activity <- read.csv("~/Desktop/HUDK 4050/Data_jam_Jing/extra-activity.csv")
D2<-student.data <- read.csv("~/Desktop/HUDK 4050/Data_jam_Jing/student-data.csv", header=TRUE)

#do the correlation plot
library(corrplot)

#Generate X,Y Plot
plot(D2$G1,D2$av.seconds.per.task) #normal distribution of the first period grade 
plot(D2$G2,D2$av.seconds.per.task) #students with 0 grade might lack coinfidence in answering the questions
plot(D2$G3,D2$av.seconds.per.task) #the major plots are more dense than the previous plot, students score 0 might give up after attempts

#compute correlation plot between variables
cor.test(D2$forum.posts,D2$G1)
#data:  D2$forum.posts and D2$G1
#t = 3.6507, df = 998, p-value = 0.0002751
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
  0.05318203 0.17554047
#sample estimates:
  cor p
0.1147966 
#low p-value,but also low correlation value, so do not need to consider

#pick some categotical and numeric varibles and create a new dataframe
D3<-dplyr::select(D2, G1, G2, G3,levels.complete,customize.character, avatar.requests, teacher.requests,av.seconds.per.task)
#try many times to choose the variables that would entail correlations to each other

#make the categorical variables numeric
D3$schoolsup<-ifelse(D2$schoolsup=="no",0,1)
D3$famsup<-ifelse(D2$famsup=="no",0,1)
D3$higher<-ifelse(D2$higher=="no",0,1)

plot(D3)

#cluster?
#prediction? decision tree?
#pca
#visualization



