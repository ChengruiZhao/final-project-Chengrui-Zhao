# read in packages needed
library(corrplot)
library(MASS)
library(car)
library(ordinal)

#set options so that all digits are read and plots can be shown
options(scipen = 999)
options(width = 10000)

#read in female data
lines <- readLines("C:\\Users\\DELL\\Downloads\\2017_2019_FemRespData.DAT")

#split every line of dataset into one character one column and turn it into dataset
split_data <- lapply(lines, function(line) {
  unlist(strsplit(line, split = ""))
})
female <- do.call(rbind, lapply(split_data, function(row) {
  as.character(row) 
}))
female<- as.data.frame(female, stringsAsFactors = FALSE)

#read in male data
lines <- readLines("C:\\Users\\DELL\\Downloads\\2017_2019_MaleData.DAT")

#split every line of dataset into one character one column and turn it into dataset
split_data <- lapply(lines, function(line) {
  unlist(strsplit(line, split = ""))
})
male <- do.call(rbind, lapply(split_data, function(row) {
  as.character(row) 
}))
male<- as.data.frame(male, stringsAsFactors = FALSE)

#get columns needed
female1<-female[,c("V17","V18","V25","V26","V27","V34","V36","V37","V38","V39","V53","V62","V63","V65","V66","V67","V68","V70","V71","V72","V73","V75","V76","V79","V89","V98","V99","V100","V101","V102","V218","V685","V686","V687","V688","V689","V693","V816","V2410","V2411","V2421","V2429")]

# some questions take two column to answer, combine them into one column
female1$V36<-paste(female1$V36,female1$V37,sep="")
female1$V72<-paste(female1$V72,female1$V73,sep="")
female1<-female1[,c(-8,-21)]

# get the degree of willing according to columns
willing<-rep(1,each=nrow(female1))
for(i in 1:nrow(female1)){
  if(((female1[i,"V25"]!=0)|(female1[i,"V26"]!=0)|(female1[i,"V89"]==1)|(female1[i,"V2410"]==1)|(female1[i,"V2411"]==1)|(female1[i,"V2429"]==1))&((female1[i,"V218"]==1)| (female1[i,"V102"]==1)|(female1[i,"V27"]!=0))){
    willing[i]=4
    }
  else if((female1[i,"V25"]!=0)|(female1[i,"V26"]!=0)|(female1[i,"V27"]!=0)|(female1[i,"V89"]==1)| (female1[i,"V102"]==1)|(female1[i,"V218"]==1)|(female1[i,"V2410"]==1)|(female1[i,"V2411"]==1)|(female1[i,"V2429"]==1)){
     willing[i]=3
    }
  else if(((female1[i,"V685"]==1)|(female1[i,"V686"]==1)|(female1[i,"V687"]==1)|(female1[i,"V688"]==1)|(female1[i,"V689"]==1))&((female1[i,"V816"]==3)|(female1[i,"V816"]==4))){
    willing[i]=0
    }
  else if(((female1[i,"V685"]==1)|(female1[i,"V686"]==1)|(female1[i,"V687"]==1)|(female1[i,"V688"]==1)|(female1[i,"V689"]==1))&((female1[i,"V816"]==1)|(female1[i,"V816"]==2))){
    willing[i]=1
    }
  else {willing[i]=2}
  }
willing<-data.frame(willing)
female1<-cbind(female1,willing)

# select only those graduated according to the question
female1<-female1[which(female1$V34!=1),]

#eliminate those unrelated to factors and response
female1<-female1[,c("V17","V18","V36","V38","V39","V53","V62","V63","V65","V66","V67","V68","V70","V71","V72","V75","V76","V79","willing")]

# turn all columns into factor according to the survey
female1 <- as.data.frame(lapply(female1, function(x) {
  as.factor(x)
}))

#get columns needed
male1<-male[,c("V17","V18","V29","V31","V32","V33","V34","V47","V48","V57","V58","V60","V61","V62","V63","V65","V66","V67","V68","V70","V71","V74","V158","V163","V331","V334","V578","V627","V628","V2444","V2445","V2739","V2740","V3192","V3276","V3277","V3278","V3386","V3396")]

# some questions take two column to answer, combine them into one column
male1$V31<-paste(male1$V31,male1$V32,sep="")
male1$V67<-paste(male1$V67,male1$V68,sep="")
male1<-male1[,c(-5,-19)]

# get the degree of willing according to columns
willing<-rep(1,each=nrow(male1))
for(i in 1:nrow(male1)){
  if(((male1[i,"V331"]==1)|(male1[i,"V334"]==1)|(male1[i,"V2444"]==1)|(male1[i,"V2445"]==1)|(male1[i,"V3276"]==1)|(male1[i,"V3277"]==1)|(male1[i,"V3278"]==1)|(male1[i,"V3386"]==1)|(male1[i,"V3396"]==1))&((male1[i,"V578"]==1)|(male1[i,"V627"]==1)|(male1[i,"V627"]==2)|(male1[i,"V627"]==3)|(male1[i,"V628"]==1)|(male1[i,"V2740"]==1))){
    willing[i]=4
  }
  else if(((male1[i,"V331"]==1)|(male1[i,"V334"]==1)|(male1[i,"V2444"]==1)|(male1[i,"V2445"]==1)|(male1[i,"V3276"]==1)|(male1[i,"V3277"]==1)|(male1[i,"V3278"]==1)|(male1[i,"V3386"]==1)|(male1[i,"V3396"]==1)|(male1[i,"V578"]==1)|(male1[i,"V627"]==1)|(male1[i,"V627"]==2)|(male1[i,"V627"]==3)|(male1[i,"V628"]==1)|(male1[i,"V2740"]==1))){
    willing[i]=3
  }
  else if((male1[i,"V158"]==1)&((male1[i,"V163"]==5))){
    willing[i]=0
  }
  else if((male1[i,"V158"]==1)&((male1[i,"V163"]==1))){
    willing[i]=1
  }
  else {willing[i]=2}
}
willing<-data.frame(willing)
male1<-cbind(male1,willing)

# select only those graduated according to the question
male1<-male1[which(male1$V29!=1),]

#eliminate those unrelated to factors and response
male1<-male1[,c("V17","V18","V31","V33","V34","V48","V57","V58","V60","V61","V62","V63","V65","V66","V67","V70","V71","V74","willing")]

# turn all columns into factor according to the survey
male1 <- as.data.frame(lapply(male1, function(x) {
  as.factor(x)
}))

#conduct exploratory analysis
par(mfrow = c(4, 4), mar = c(2, 2, 2, 2), oma = c(2, 2, 2, 2))
boxplot(female1$willing,main="willing for female")
boxplot(male1$willing,main="willing for male")
hist(as.numeric(female1[,1]),main="f race distribution")
hist(as.numeric(male1[,1]),main="m race distribution")
hist(as.numeric(female1[,3]),main="f degree distribution")
hist(as.numeric(male1[,3]),main="m degree distribution")
hist(as.numeric(female1[,6]),main="f college distribution")
hist(as.numeric(male1[,6]),main="m college distribution")
hist(as.numeric(female1[,8]),main="f onown distribution")
hist(as.numeric(male1[,8]),main="m onown distribution")
hist(as.numeric(female1[,9]),main="f parent distribution")
hist(as.numeric(male1[,9]),main="m parent distribution")
hist(as.numeric(female1[,10]),main="f intact distribution")
hist(as.numeric(male1[,10]),main="m intact distribution")
hist(as.numeric(female1[,17]),main="f foster distribution")
hist(as.numeric(male1[,17]),main="m foster distribution")

# draw correlation plot to eliminate collinearity
par(mfrow=c(1,2))
cor1<-cor(data.frame(lapply(female1,as.numeric)),method="spearman")
corrplot(cor1,type="full",is.corr=T,add=F,diag=T)
cor2<-cor(data.frame(lapply(male1,as.numeric)),method="spearman")
corrplot(cor2,type="full",is.corr=T,add=F,diag=T)
female1<-female1[,c("V17","V36","V38","V39","V63","V65","V66","V70","V71","V72","V76","willing")]
male1<-male1[,c("V17","V31","V33","V34","V58","V60","V61","V65","V66","V67","V71","willing")]

#some variables are ordinal, turn them into it
female1$willing <- factor(female1$willing, ordered = TRUE)
female1$V36 <- factor(female1$V36, ordered = TRUE)
female1$V70<- factor(female1$V70, ordered = TRUE)

#eliminate survey choices with too little samples to avoid errors
female1<-female1[which(female1$V17!=9),]
female1<-female1[which(female1$V38!=9),]
female1<-female1[which(female1$V38!=" "),]
female1<-female1[which(female1$V39!=8),]
female1<-female1[which(female1$V65!=8),]
female1<-female1[which(female1$V66!=9),]
female1<-female1[which(female1$V66!=8),]
female1<-female1[which(female1$V70!=8),]
female1<-female1[which(female1$V70!=" "),]
female1<-female1[which(female1$V72!=98),]
female1<-female1[which(female1$V76!=8),]
female1<-female1[which(female1$V76!=9),]

#conduct Cumulative Link Model
fit1 <- clm(female1$willing ~ female1$V17 + female1$V36 + female1$V38 + female1$V39 + female1$V63 + female1$V65 + female1$V66 + female1$V70 + female1$V71 + female1$V72 + female1$V76)

#here's one way to choose valid predictors by using smallest AIC
step(fit1)
fit1 <- clm(female1$willing ~female1$V36 +female1$V38 + female1$V63 +female1$V66 +female1$V72 )

#here's one way to choose valid predictors by using p-value>0.05
fit1 <- clm(female1$willing ~ female1$V38+ female1$V63+ female1$V66)
summary(fit1)

#some variables are ordinal, turn them into it
male1$willing <- factor(male1$willing, ordered = TRUE)
male1$V31 <- factor(male1$V31, ordered = TRUE)
male1$V65<- factor(male1$V65, ordered = TRUE)

#eliminate survey choices with too little samples to avoid errors
male1<-male1[which(male1$V31!=98),]
male1<-male1[which(male1$V31!=99),]
male1<-male1[which(male1$V33!=" "),]
male1<-male1[which(male1$V33!=9),]
male1<-male1[which(male1$V34!=" "),]
male1<-male1[which(male1$V60!=8),]
male1<-male1[which(male1$V61!=9),]
male1<-male1[which(male1$V65!=8),]
male1<-male1[which(male1$V65!=" "),]
male1<-male1[which(male1$V66!=" "),]
male1<-male1[which(male1$V66!=8),]
male1<-male1[which(male1$V66!=9),]
male1<-male1[which(male1$V67!=" "),]
male1<-male1[which(male1$V67!=98),]
male1<-male1[which(male1$V71!=8),]
male1<-male1[which(male1$V71!=9),]

#conduct Cumulative Link Model
fit2 <- clm(male1$willing ~ male1$V17 + male1$V31 + male1$V33 + male1$V34 + male1$V58 + male1$V60 + male1$V61 + male1$V65 + male1$V66 + male1$V67 + male1$V71)

#here's one way to choose valid predictors by using smallest AIC
step(fit2)
fit2 <- clm(male1$willing ~ male1$V17 +male1$V58 + male1$V60 + male1$V61 + male1$V65)

#here's one way to choose valid predictors by using p-value>0.05
fit2 <- clm(male1$willing ~ male1$V17+ male1$V58)
summary(fit2)
