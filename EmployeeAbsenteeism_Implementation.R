rm(list=ls())
setwd("D:/Data Science/First_Project")
library("xlsx")
employee_data=read.xlsx("Absenteeism_at_work_Project.xls",sheetIndex = 1)
#Detecting the missing values
employee_sample=as.data.frame(employee_data)
#9th record transporation expense=155
#mean method=221.0355,18.00406
#median method=225,18
#KNN Imputation=155

library("DMwR")
employee_sample=knnImputation(employee_sample,k=3)
write.csv(employee_sample,file = "KNNImputed.csv")

#Detecting Outlier Analysis
numeric_data=sapply(employee_sample, is.numeric)
numeric_values=employee_sample[,numeric_data]
cnames=colnames(numeric_values)
library("usdm")
lm_model=lm(Absenteeism.time.in.hours~.,data = employee_sample)
cooksd=cooks.distance(lm_model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")
library("ggplot2")
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Absenteeism.time.in.hours"), data = subset(employee_sample))+ 
           stat_boxplot(geom = "boxplot", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "yellow" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot of responded for",cnames[i])))
}
#Plotting Plots together
gridExtra::grid.arrange(gn2,gn3,gn4,ncol=3)
gridExtra::grid.arrange(gn6,gn7,gn8,gn9,ncol=4)
gridExtra::grid.arrange(gn18,gn19,gn20,ncol=3)

#Fetching Values of Outliers and replacing it with NA
#val=employee_sample$Height[employee_sample$Height %in% boxplot.stats(employee_sample$Height)$out]
categorical_val=c("Disciplinary.failure","Education","Social.smoker","Pet")
cnames=cnames[which(!cnames %in% categorical_val)]
for (i in cnames) {
  outliers_value=employee_sample[,i][employee_sample[,i] %in% boxplot.stats(employee_sample[,i])$out]
  employee_sample[,i][employee_sample[,i] %in% outliers_value]=NA
}

#Finding the NA Outliers Values using KNN
employee_sample=knnImputation(employee_sample,k=3)
write.csv(employee_sample,"OutliersRemovedData.csv")

#correlation plot
library("corrgram")
corrgram(employee_sample[,numeric_data],order = F,upper.panel = panel.pie,text.panel = panel.txt,
         main="Correlation plot")

#Feature Scaling
qqnorm(employee_sample$Hit.target)
hist(employee_sample$Work.load.Average.day.)
hist(employee_sample$Distance.from.Residence.to.Work)
hist(employee_sample$Age)
#Removing all the categorical Variables and target variables
cnames_cat=c("Absenteeism.time.in.hours","Social.drinker",
             "Day.of.the.week","Seasons","Education","Reason.for.absence")
cnames=cnames[which(!cnames %in% cnames_cat)]
cnames
#cnames=c("Work.load.Average.day.","Hit.target","Weight","Height","Body.mass.index")
#Apply Normalization
for (i in cnames) {
  employee_sample[,i]=(employee_sample[,i]-min(employee_sample[,i]))/
                      max(employee_sample[,i]-min(employee_sample[,i]))
}
#employee_sample=employee_sample[,-21]
#Random Forest
library("randomForest")
RF_MOdel=randomForest(Absenteeism.time.in.hours ~.,data = employee_sample,importance=TRUE,ntree=500)
importance(RF_MOdel,type = 1)

#Building Linear Regression Model
#Check for multi-collinearity
library("usdm")
vifcor(employee_sample[,-21],th=0.9)
train_index=sample(1:nrow(employee_sample),0.8*nrow(employee_sample))
train_data=employee_sample[train_index,]
test_data=employee_sample[-train_index,]

lm_model=lm(Absenteeism.time.in.hours ~.,data = train_data)
summary(lm_model)

predictions_model=predict(lm_model,test_data[,-21])

#Building Decision Tree
library("rpart")
library("MASS")
train_index=sample(1:nrow(employee_sample),0.8*nrow(employee_sample))
train_data=employee_sample[train_index,]
test_data=employee_sample[-train_index,]
reg_model=rpart(Absenteeism.time.in.hours ~.,data = train_data,method = "anova")
summary(reg_model)
test_model=predict(reg_model,test_data[,-21])
library("rpart.plot")
rpart.plot(reg_model)
#Calculating the error metrics
regr.eval(test_data[,21],test_model,stats = c('mae','rmse','mape','mse'))
regr.eval(test_data[,21],predictions_model,stats = c('mae','rmse','mape','mse'))

#Forecasting the Losses
library("forecast")
forecast_mdel=ts(data = test_model,start = c(2011,1),end = c(2011,12),frequency = 12)
forecast_mdel
Predicted_value=as.data.frame(table(forecast_mdel),index=c(1:nrow(forecast_mdel)))
Predicted_value
forecast_LRM=ts(data=predictions_model,start=c(2011,1),end=c(2011,12),frequency = 12)
forecast_LRM
