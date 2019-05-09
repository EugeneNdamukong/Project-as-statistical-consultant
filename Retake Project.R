library(readr)
#Importing data
happy <- read_delim("C:/Users/Eugene/Desktop/Courses of the Master of Statistics/Second Semester/Statistical consulting/Project/Happy.txt", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)

pairs(happy[,c("Happiness_Score","Health", "Freedom","Trust",
               "Generosity" , "Family")])
   
hist(happy$Happiness_Score)
max(happy$Happiness_Score)
min(happy$Happiness_Score)
happy.fit=lm(Happiness_Score~ Health+Freedom+Trust+Generosity+Family, data = happy)
summary(happy.fit)
plot(happy.fit)

#To build a predictive model, data will be split to 60% train, 20% validation
#20% test

happy_train1=happy[1:126,]
happy_test=happy[127:157,]

#Building model with 10-fold CV
lm_list=list()
mse_vec=vector(mode="numeric",length=10)
for(i in 1:10){
  happy_train1=happy_train1[sample(nrow(happy_train1)),]
  happy_train2=happy_train1[1:76,]
  happy_val=happy_train1[77:126,]
  happy_fit=lm(Happiness_Score~ Health+Freedom+Trust+Generosity+Family+Economy, data = happy_train2)
  lm_list[[i]]=happy_fit
  pred=predict(happy_fit,happy_val)
  mse_vec[i]=mean((pred-happy_val$Happiness_Score)^2)
}
mse_vec
plot(mse_vec,type = 'l',xlab = 'Model',ylab = 'Mean Square Error',col='blue')
min(mse_vec)

lm_list[[8]]
library(car)
vif(lm_list[[8]])
summary(lm_list[[8]])
par(mfrow=c(2,2))
plot(lm_list[[8]])
dev.off()

#Testing model
attach(happy_test)
pred_test = predict(lm_list[[8]],happy_test)
plot(pred_test,happy_test$Happiness_Score,ylab = "Predicted Happiness score",xlab = "Measured Happiness score")
mean((pred_test-happy_test$Happiness_Score)^2)
pairs(data.frame(pred_test,happy_test[,c("Health","Freedom","Trust","Generosity","Family","Economy")]))

#Preding happiness score using predictors independently 
detach(happy_test)
health_grid=seq(min(happy_test$Health),max(happy_test$Health),length.out = 31)
economy_grid=seq(min(happy_test$Economy),max(happy_test$Economy),length.out = 31)
trust_grid=seq(min(happy_test$Trust),max(happy_test$Trust),length.out = 31)
family_grid=seq(min(happy_test$Family),max(happy_test$Family),length.out = 31)
freedom_grid=seq(min(happy_test$Freedom),max(happy_test$Freedom),length.out = 31)
generosity_grid=seq(min(happy_test$Generosity),max(happy_test$Generosity),length.out = 31)

new_test_health=data.frame(Health=health_grid,Economy=mean(happy_test$Economy),Trust=mean(happy_test$Trust),
                    Family=mean(happy_test$Family),Freedom=mean(happy_test$Freedom),Generosity=mean(happy_test$Generosity))
new_test_economy=data.frame(Health=mean(happy_test$Health),Economy=economy_grid,Trust=mean(happy_test$Trust),
                           Family=mean(happy_test$Family),Freedom=mean(happy_test$Freedom),Generosity=mean(happy_test$Generosity))
new_test_trust=data.frame(Health=mean(happy_test$Health),Economy=mean(happy_test$Economy),Trust=trust_grid,
                            Family=mean(happy_test$Family),Freedom=mean(happy_test$Freedom),Generosity=mean(happy_test$Generosity))
new_test_family=data.frame(Health=mean(happy_test$Health),Economy=mean(happy_test$Economy),Trust=mean(happy_test$Trust),
                          Family=family_grid,Freedom=mean(happy_test$Freedom),Generosity=mean(happy_test$Generosity))
new_test_freedom=data.frame(Health=mean(happy_test$Health),Economy=mean(happy_test$Economy),Trust=mean(happy_test$Trust),
                           Family=mean(happy_test$Family),Freedom=freedom_grid,Generosity=mean(happy_test$Generosity))
new_test_generosity=data.frame(Health=mean(happy_test$Health),Economy=mean(happy_test$Economy),Trust=mean(happy_test$Trust),
                            Family=mean(happy_test$Family),Freedom=mean(happy_test$Freedom),Generosity=generosity_grid)

pred_health=predict(lm_list[[8]],new_test_health,se=T,interval ='prediction')
#pred_health=as.data.frame(pred_health)
conf_health=predict(lm_list[[8]],new_test_health,se=T,interval ='confidence')
#conf_health=as.data.frame(conf_health)

pred_economy=predict(lm_list[[8]],new_test_economy,se=T,interval ='prediction')
#pred_economy=as.data.frame(pred_economy)
conf_economy=predict(lm_list[[8]],new_test_economy,se=T,interval ='confidence')
#conf_economy=as.data.frame(conf_economy)

pred_trust=predict(lm_list[[8]],new_test_trust,se=T,interval ='prediction')
#pred_trust=as.data.frame(pred_trust)
conf_trust=predict(lm_list[[8]],new_test_trust,se=T,interval ='confidence')
#conf_trust=as.data.frame(conf_trust)

pred_family=predict(lm_list[[8]],new_test_family,se=T,interval ='prediction')
#pred_family=as.data.frame(pred_family)
conf_family=predict(lm_list[[8]],new_test_family,se=T,interval ='confidence')
#conf_family=as.data.frame(conf_family)

pred_freedom=predict(lm_list[[8]],new_test_freedom,se=T,interval ='prediction')
#pred_freedom=as.data.frame(pred_freedom)
conf_freedom=predict(lm_list[[8]],new_test_freedom,se=T,interval ='confidence')
#conf_freedom=as.data.frame(conf_freedom)

pred_generosity=predict(lm_list[[8]],new_test_generosity,se=T,interval ='prediction')
#pred_generosity=as.data.frame(pred_generosity)
conf_generosity=predict(lm_list[[8]],new_test_generosity,se=T,interval ='confidence')
#conf_generosity=as.data.frame(conf_generosity)

list_pred=list(pred_economy,pred_family,pred_freedom,pred_generosity,pred_health,pred_trust)
list_conf=list(conf_economy,conf_family,conf_freedom,conf_generosity,conf_health,conf_trust)
exp_var=c("Economy","Family","Freedom","Generosity","Health","Trust")
par(mfrow=c(2,2))
par(pty="m")
par(mar = c(5,5,4,2)+0.1)
for(j in 1:6){
  for(i in 1:length(exp_var)) {
    
    matplot(new_test[,c(exp_var[6])],list_pred[[6]]$fit,lty=c(1,2,2),type="l",lwd=3,
           xlab=exp_var[6],ylab="Predicted score",
          cex.lab=1.5,cex.axis=1.3,col="red")
    matplot(new_test[,c(exp_var[6])],list_conf[[6]]$fit,lty=c(1,2,2),type="l",
            lwd=2,col="blue",add=TRUE)
    #plot(new_test[,c(exp_var[i])],list_pred[[j]][,c("fit.fit")],lty=c(1),type="l",lwd=3,
     #    xlab=exp_var[i],ylab="Predicted score")
    #matlines(new_test[,c(exp_var[i])],list_pred[[j]][,c("fit.lwr","fit.upr")],
    #         lty=c(2,2),type="l",lwd=3,col="red")
   # matlines(new_test[,c(exp_var[i])],list_conf[[j]][,c("fit.lwr","fit.upr")],
      #       lty=c(2,2),type="l",lwd=3,col="blue")
  }
  
}

