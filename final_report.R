library(readxl)
my_data <- read_excel("data-tableB3.xlsx")
data <- as.matrix(my_data)
head(data)
dim(data)

#remove NA
df <- na.omit(data) 
dim(df)
head(df)
y <- df[,1]
x <- df[,2:12]
pairs(y~ ., data = df)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(y ~ ., data = data.frame(df),
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
                    )
step.model$results

# cross
df2=0
for(i in 2:12){
 for(j in 2:12){
  df22=(df[,i]*df[,j])/max(df[,i]*df[,j])
  df2=cbind(df2,df22)
}
}
df3=df2[,-1]
best_Ra=0
for(i in 0:9){
  idx1=12*i+1
  idx2=12*(i+1)
  df2=df3[,idx1:idx2]
  init_model <- lm(y ~ ., data = data.frame(df2),na.action=na.omit)
  best_model <- step(init_model, direction = "both")
  

  Ra=summary(best_model)$adj.r.squared
 if (Ra>best_Ra){
   best_Ra  = Ra
   getModel = best_model}}
best_Ra
summary(getModel)
par(mfrow = c(2, 2))
plot(getModel)

#sigmoid + cross
df2=0
for(i in 2:12){
 for(j in 2:12){
  df22=1/1+exp(-df[,i]*df[,j])
  df2=cbind(df2,df22)
}
}
df3=df2[,-1]
best_Ra=0
for(i in 0:9){
  idx1=12*i+1
  idx2=12*(i+1)
  df2=df3[,idx1:idx2]
  init_model <- lm(y ~ ., data = data.frame(df2),na.action=na.omit)
  best_model <- step(init_model, direction = "both")
  

  Ra=summary(best_model)$adj.r.squared
 if (Ra>best_Ra){
   best_Ra  = Ra
   getModel = best_model}}
best_Ra
summary(getModel)
par(mfrow = c(2, 2))
plot(getModel)

# log + cross
df2=0
for(i in 2:11){
 for(j in 2:11){
  df22=log(df[,i]*df[,j])
  df2=cbind(df2,df22)
}
}
df3=df2[,-1]
best_Ra=0
for(i in 0:9){
  idx1=10*i+1
  idx2=10*(i+1)
  df2=df3[,idx1:idx2]
  init_model <- lm(y ~ ., data = data.frame(df2),na.action=na.omit)
  best_model <- step(init_model, direction = "both")
  

  Ra=summary(best_model)$adj.r.squared
 if (Ra>best_Ra){
   best_Ra  = Ra
   getModel = best_model}}
best_Ra
summary(getModel)
par(mfrow = c(2, 2))
plot(getModel)

# AIC=-130.39, log(y) ~ x1 + x5 + x8 + x10,Ra=0.8661 

df2=log(df[,2:11])
init_model <- lm(log(y) ~ ., data = data.frame(df2))
best_model <- step(init_model, direction = "both")
summary(best_model)
par(mfrow = c(2, 2))
plot(best_model)

#sigmoid 
#AIC=-133.37 log(y) ~ x1 + x5 + x8 + x10 + x11_s Ra=0.8819  
x11_s=1/1+exp(-df[,12])

df2=cbind(log(df[,2:11]),x11_s)
init_model <- lm(log(y) ~ ., data = data.frame(df2))
best_model <- step(init_model, direction = "both")
summary(best_model)
par(mfrow = c(2, 2))
plot(best_model)

#sigmoid 
#AIC=45.25 y ~ x4 + x6 + x8 + x10 + x11 + x10_s Ra=0.9059 
x6_s=1/exp(-df[,7])
x8_s=1/exp(-df[,9])
x10_s=1/exp(-df[,11]/sum(df[,11]))
df2=cbind(df,x6_s,x8_s,x10_s)
init_model <- lm(y ~ ., data = data.frame(df2))
best_model <- step(init_model, direction = "both")
summary(best_model)
par(mfrow = c(2, 2))
plot(best_model)

#sigmoid
#AIC=44.25 y ~  x4 + x8 + x10 + x11 + x6_s + x10_s Ra=0.9059 
x6_s=1/1+exp(-df[,7])
x8_s=1/1+exp(-df[,9])
x10_s=1/1+exp(-df[,11]/sum(df[,11]))
df3=cbind(df,x6_s,x8_s,x10_s)
init_model <- lm(y ~ ., data = data.frame(df3))
best_model <- step(init_model, direction = "both")
summary(best_model)
par(mfrow = c(2, 2))
plot(best_model)

#AIC = 68.29 x5,x8,x10  Ra=0.7808
init_model <- lm(y ~ ., data = data.frame(df))
best_model <- step(init_model, direction = "both")
summary(best_model)
par(mfrow = c(2, 2))
plot(best_model)

#add weight
df3=cbind(df[,9],df[,6]^3,df[,9]^3,df[,11]^3)
init_model <- lm(y ~ ., data = data.frame(df3))
best_model <- step(init_model, direction = "both")
summary(best_model)
par(mfrow = c(2, 2))
plot(best_model)

#add weight
df3=cbind(df,3*df[,6],7*df[,9],10*df[,11])
init_model <- lm(y ~ ., data = data.frame(df3))
best_model <- step(init_model, direction = "both")
summary(best_model)
par(mfrow = c(2, 2))
plot(best_model)

