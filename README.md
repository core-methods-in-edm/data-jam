# Data-Jam
## Code Books

### student-data.csv
====================

id - Student id number  
1 school - student's school (binary: "GP" - Gabriel Pereira or "MS" - Mousinho da Silveira)  
2 sex - student's sex (binary: "F" - female or "M" - male)  
3 age - student's age (numeric: from 15 to 22)  
4 address - student's home address type (binary: "U" - urban or "R" - rural)  
5 famsize - family size (binary: "LE3" - less or equal to 3 or "GT3" - greater than 3)  
6 Pstatus - parent's cohabitation status (binary: "T" - living together or "A" - apart)  
7 Medu - mother's education (numeric: 0 - none,  1 - primary education (4th grade), 2 – 5th to 9th grade, 3 – secondary education or 4 – higher education)  
8 Fedu - father's education (numeric: 0 - none,  1 - primary education (4th grade), 2 – 5th to 9th grade, 3 – secondary education or 4 – higher education)  
9 Mjob - mother's job (nominal: "teacher", "health" care related, civil "services" (e.g. administrative or police), "at_home" or "other")  
10 Fjob - father's job (nominal: "teacher", "health" care related, civil "services" (e.g. administrative or police), "at_home" or "other")  
11 reason - reason to choose this school (nominal: close to "home", school "reputation", "course" preference or "other")  
12 guardian - student's guardian (nominal: "mother", "father" or "other")  
13 traveltime - home to school travel time (numeric: 1 - <15 min., 2 - 15 to 30 min., 3 - 30 min. to 1 hour, or 4 - >1 hour)  
14 studytime - weekly study time (numeric: 1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours)  
15 failures - number of past class failures (numeric: n if 1<=n<3, else 4)  
16 schoolsup - extra educational support (binary: yes or no)  
17 famsup - family educational support (binary: yes or no)  
18 paid - extra paid classes within the course subject (Math or Portuguese) (binary: yes or no)  
19 activities - extra-curricular activities (binary: yes or no)  
20 nursery - attended nursery school (binary: yes or no)  
21 higher - wants to take higher education (binary: yes or no)  
22 internet - Internet access at home (binary: yes or no)  
23 romantic - with a romantic relationship (binary: yes or no)  
24 famrel - quality of family relationships (numeric: from 1 - very bad to 5 - excellent)  
25 freetime - free time after school (numeric: from 1 - very low to 5 - very high)  
26 goout - going out with friends (numeric: from 1 - very low to 5 - very high)  
27 Dalc - workday alcohol consumption (numeric: from 1 - very low to 5 - very high)  
28 Walc - weekend alcohol consumption (numeric: from 1 - very low to 5 - very high)  
29 health - current health status (numeric: from 1 - very bad to 5 - very good)  
30 absences - number of school absences (numeric: from 0 to 93)  
31 G1 - first period grade (numeric: from 0 to 20)  
31 G2 - second period grade (numeric: from 0 to 20)  
32 G3 - final grade (numeric: from 0 to 20, output target)  
33 forum.posts - how many forum posts student made on LMS over semester  
34 levels.complete - how many levels of an online tutor a student completed over the semester  
35 avatar.requests - how many times a student asked for hints and help from the AI avatar in the online tutor  
36 teacher.requests - how many times a student sent a request for help to a live teacher  
37 customize.character - whether or not a student paid extra to customize their interface  
38 facebook - whether or not a student linked their Facebook account to the online system  
39 time.in.session - how much time a student spent in online sessions  
40 av.seconds.per.task - how many seconds a student spent on average in a task  

### student-activity.csv
====================

id - student id number  
hockey - student plays hockey  
soccer - student plays soccer  
dance - student competes in dance competition  
a.football - student plays American football  
comm.service - student is involved in community service  
choir - student sings in choir  
tennis - student plays tennis  
table.tennis - student plays table tennis  
running - student runs comptetitively  



```{r}
A1<-read.csv(“student-data.csv”)
View(A1)
```

#cleaning data 
```{r}
A1$isFemale <- ifelse(A1$sex=="F", 1, 0)
A1$ISURBAN <- ifelse(A1$address=="U", 1, 0)
A1$PTOGETHER <- ifelse(A1$Pstatus=="T", 1, 0)
A1$Mjobhome<- ifelse(A1$Mjob=="at_home", 1, 0)
A1$Fjobhome<- ifelse(A1$Fjob=="at_home", 1, 0)
A1$Mjobhealth<- ifelse(A1$Mjob=="health", 1, 0)
A1$Fjobhealth<- ifelse(A1$Fjob=="health", 1, 0)
A1$Mjobservice<- ifelse(A1$Mjob=="services", 1, 0)
A1$Fjobservice<- ifelse(A1$Fjob=="services", 1, 0)
A1$Mjobteacher<- ifelse(A1$Mjob=="teacher", 1, 0)
A1$Fjobteacher<- ifelse(A1$Fjob=="teacher", 1, 0)
A1$reasoncourse<- ifelse(A1$reason=="course", 1, 0)
A1$reasonreputation<- ifelse(A1$reason=="reputation", 1, 0)
A1$reasonhome<- ifelse(A1$reason=="home", 1, 0)
A1$guardiananother←ifelse(A1$guardian==”other”,1,0)
A1$guardianm0←ifelse(A1$guardian==”mother”,1,0)
A1$famsup←ifelse(A1$famsup==”Yes”,1,0)
A1$schoolsup←ifelse(A1$schoolsup==”Yes”,1,0)
A1$paid←ifelse(A1$paid==”Yes”,1,0)
A1$activities←ifelse(A1$activities==”Yes”,1,0)
A1$nursery←ifelse(A1$nursery==”Yes”,1,0)
A1$higher←ifelse(A1$higher==”Yes”,1,0)
A1$internet←ifelse(A1$internet==”Yes”,1,0)
A1$romantic←ifelse(A1$romantic==”Yes”,1,0)
A1$facebook←ifelse(A1$facebook==”Yes”,1,0)
```
#taking out the qualtative and unnecessary variables

```{r}
A1$grades<-A1$G1+A1$G2+A1$G3
A1<- subset(newstudent,select=-c(G1,G2,G3,sex,address,Pstatus,Mjob,Fjob,reason))
 A1<- subset(newstudent,select=-c(school,famsize))
 extra←read.csv('extra-activity.csv')
```


```{r}
 A1$grades<-A1$G1+A1$G2+A1$G3
A1<- subset(newstudent,select=-c(G1,G2,G3,sex,address,Pstatus,Mjob,Fjob,reason))
 A1<- subset(newstudent,select=-c(school,famsize))
 extra←read.csv('extra-activity.csv')
```
# cleaning up the extra-activity data aka binaries 
```{r}
extra$hockey←ifelse(A1$hockey==”Yes”,1,0)
extra$soccer←ifelse(A1$soccer==”Yes”,1,0)
extra$dance←ifelse(A1$dance==”Yes”,1,0)
extra$a.football←ifelse(A1$a.football==”Yes”,1,0)
extra$comm.service←ifelse(A1$comm.service==”Yes”,1,0)
extra$choir←ifelse(A1$choir==”Yes”,1,0)
extra$tennis←ifelse(A1$tennis==”Yes”,1,0)
extra$table.tennis←ifelse(A1$table.tennis==”Yes”,1,0)
extra$running←ifelse(A1$running==”Yes”,1,0)
```
#merging the clean student data with clean extra activities data by student id 
```{r}
 total<-merge(A1,extra, by='id')
 View(total)
 total<- subset(total,select=-c(id))
 library(corrplot)
```

#correlation matrix. Found that several variables correlated
# Fedu vs Medu (.644) PTOGETHER vs customize character (-.726) 
# G1 vs G2 (.854) G1 vs G3 (.806) G2 vs G3 (.909) 
#avatar requests vs G1 (-.513) time in session vs levels complete (.799) 
# average sec per task vs levels complete (.709) 
#average sec per task vs time in session (.587) 
#football vs hockey (-.575) football vs soccer (-.601) 
# table tennis vs choir (.554) table tennis vs tennis (-.576) 
# table tennis vs running (.599) 

```{r}
COR <- cor(total)
 View(COR)
 COR<- subset(COR,select=-c(activities))
 corrplot(COR, order="AOE", method="circle", tl.pos="lt", type="upper",        
                  tl.col="black", tl.cex=0.6, tl.srt=45, 
                  addCoef.col="black", addCoefasPercent = TRUE,
                  sig.level=0.50, insig = "blank")
```

#Did not cluster- from vsualizing data did not seem like there are any clear #clusters in the data
#conducting stepwise to determine 'important' variables

```{r}
library(MASS)
fit <- lm(y~.,data=total)
step <- stepAIC(fit, direction="both")
step$anova # display results 
```

#final result of regression 

```{r}
fit<-lm(total$grades ~ age + Medu + Mjobhealth + Mjobservice + Fjobteacher + 
             studytime + failures + schoolsup + famsup + paid + higher + 
             internet + romantic + famrel + goout + health + levels.complete + 
             avatar.requests + teacher.requests + time.in.session + choir + 
             table.tennis,data=total)
 summary(fit)
 plot(fit)
```

#regression of all variables predicting grades
#findings that positive significant predictors of grades are mjobhealth
#mjobservice fjobteacher studytime higher internet famrel teacherreq tabletennis
# findings that negative significant predictors of grades are failures schoolsup
#paid famsup romantic goout health avatarreq timeinsession

 
#creating decision tree out of stepwise result
#tree predicts 60 percent accuracy of predicting score within 5 points of true #value 

```{r}
 fit1<-rpart(A1$grades~.,data=A1)
 printcp(fit1)
 post(fit1, file = "tree2")
 A1$prediction <- predict(fit1, A1)
 A1$difference<-A1$grades-A1$prediction
 summary(A1$difference)
 A1$accurate <- ifelse(abs(A1$difference) <=5,1, 0)
 summary(A1$accurate)
 summary(A1$grades) 

```
