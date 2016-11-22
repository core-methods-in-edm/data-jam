library("rpart")
library(corrplot)

ex_activity = read.csv("extra-activity.csv")
student = read.csv("student-data.csv")

ex_activity_num = as.data.frame(ifelse(ex_activity[-1] == "yes", 1,0)) 
ex_activity_num = lapply(ex_activity_num, as.numeric)
ex_activity_mat = do.call(cbind, ex_activity_num)

ex_activity_sum = rowSums(ex_activity_mat)
ex_activity_new = cbind(ex_activity_mat, ex_activity_sum)

analyst_data = cbind(student[c("levels.complete","avatar.requests","teacher.requests","Medu","Fedu","traveltime","studytime")], ex_activity_sum)
#View(analyst_data)
reg_level = lm(levels.complete~ex_activity_sum+Medu+Fedu+traveltime+studytime+avatar.requests+teacher.requests, data = analyst_data)

summary(reg_level)

pairs(levels.complete~ex_activity_sum+Medu+Fedu+traveltime+studytime+avatar.requests+teacher.requests, main="scatter plot matrices", data = analyst_data)

analyst_cor = cor(analyst_data)
#View(analyst_cor)
corrplot(analyst_cor, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

#From the plot, since Medu and Fedu have a high relationship, including both of them may cause multicollinearity, I would drop Medu. 
#Also teacher request and avatar request have high relationship and low relationship with levels.complete. I would drop avatar.requests
#traveltime also didnot see a large effect on levels complete

reg_new_level = lm(levels.complete~ex_activity_sum+Fedu+studytime, data = analyst_data)
summary(reg_new_level)
error = analyst_data$levels.complete-(-2.990-1.272*analyst_data$ex_activity_sum+3.672*analyst_data$Fedu+37.036*analyst_data$studytime)
final_reg = cbind(analyst_data$levels.complete, analyst_data$ex_activity_sum, analyst_data$Fedu, analyst_data$studytime, error)
colnames(final_reg) = c("levels.complete","ex_activity_sum","Fedu","studytime","error")
pairs(levels.complete~ex_activity_sum+Fedu+studytime+error, data = final_reg)

#as studytime increase, the range of error becomes larger and larger.

