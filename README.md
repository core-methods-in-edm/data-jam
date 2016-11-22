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
#read extra activity data as table
activity <- read.csv("extra-activity.csv",row.names=1)

#not sure this was needed, maybe I forgot to take it out at some point...
activity <- as.numeric(activity)

#change yeses and nos to 2,1
activity2 <- as.data.frame(sapply(activity, as.numeric))

#change 1,2 to 0,1
activity2 <- ifelse(activity2 == 1, 0, 1)

#read as matrix so that can transpose?
activity2 <- as.matrix(activity2)


#FINALLY people by people (with activities in common?)
people <- activity2 %*% t(activity2)

#take out the self connections?
diag(people) <- NA

# too too big, computer sad. 
p <- graph.adjacency(people,mode="undirected")
plot(p,layout=layout.fruchterman.reingold, vertex.size=6)
degree(p)
betweenness(p)

#take subset of random 20 students so computer doesn't try to die
activity1 <- dplyr::sample_n(activity, 20, replace = TRUE)

#redo steps from above with random 20 students:

#change yeses and nos to 2,1
activity2 <- as.data.frame(sapply(activity1, as.numeric))

#change 1,2 to 0,1
activity2 <- ifelse(activity2 == 1, 0, 1)

#read as matrix so that can transpose?
activity2 <- as.matrix(activity2)


#FINALLY people by people (with activities in common?)
people2 <- activity2 %*% t(activity2)

#take out self attachments
diag(people2) <- NA

#doable now?
p <- graph.adjacency(people2,mode="undirected")
plot(p,layout=layout.fruchterman.reingold, vertex.size=6)

#need to look into how to read the printouts from these?

degree(p)
# [1] 37 36 31 35 38 26 38 22  5  5 22 34  8 24 28 24 42 36 39 38

betweenness(p)
# [1] 2.94042416 0.67859086 0.45984021 1.87601509 2.74531450 0.71596320 2.36959022 0.05353466
 [9] 0.00000000 0.00000000 0.05353466 9.52803863 0.00000000 4.18571429 8.38298447 7.09749695
[17] 2.75655469 0.67859086 2.73470558 0.74310698

#try with activities instead of people
sport <- t(activity2) %*% activity2
diag(sport) <- NA

a <- graph.adjacency(sport,mode="undirected")
plot(a,layout=layout.fruchterman.reingold, vertex.size=6)

port <- read.csv("student-data.csv", header = TRUE, sep = ",")
portsmall <- dplyr::select(port, sex, failures, romantic, G3)

#plotted with the vertices the color of the final grade range (but what's the range to color basis??)
plot(a,layout=layout.fruchterman.reingold, vertex.size=6, vertex.color=portsmall$G3)

#gives a box top left. but empty.
legend("topleft", c("1","2","3","4","5"), cex=0.8, fill=colors)

#how do you print a legend? impossible, clearly. 


```

