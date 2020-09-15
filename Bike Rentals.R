setwd("C:/Users/Sakshi/Desktop/Great Learning/Sample R Projects/Bike Rentals Prediction")
day=read.csv("day.csv", header = TRUE)
hour=read.csv("hour.csv", header = TRUE)
nrow(day)
ncol(day)
dim(day)
names(day)
attach(day)

####Converting Temp Data ####
day$raw.temp=temp*41
head(day)

#####Calculating different seasons Mean, Median & Std Deviation ####
spring=subset(day, season==1)$raw.temp
sp.mean=mean(spring)
sp.median=median(spring)
sp.sd=sd(spring)

summer=subset(day, season==2)$raw.temp
su.mean=mean(summer)
su.median=median(summer)
su.sd=sd(summer)


fall=subset(day, season==3)$raw.temp
fa.mean=mean(fall)
fa.median=median(fall)
fa.sd=sd(fall)

winter=subset(day, season==4)$raw.temp
wi.mean=mean(winter)
wi.median=median(winter)
wi.sd=sd(winter)

#####Graphical Representation ######
par(mfrow = c(2, 2))

par(mar = rep(2, 4)) 
hist(x=spring, main="Temperatures in spring season",
     xlab="Temperature is Celcius", ylab="No of days")
abline(v=sp.mean,col="red")
text(x=17, y=35, labels = paste("Mean=", round(sp.mean, 2)), col="red")
abline(v=sp.median, col="blue")
text(x=6, y=35, labels = paste("Median=", round(sp.median,2)),col="blue")


par(mar = rep(2, 4)) 
hist(x=summer, main="Temperatures in summer season", 
     xlab="Temperature is Celcius", ylab="No of days", xlim=c(10,35), ylim=c(0,40))
abline(v=su.mean, col="red")
text(x=15, y=40, labels = paste("Mean=", round(su.mean,2)),col="red")
abline(v=su.median, col="blue")
text(x=30, y=40, labels = paste("Median=", round(su.median,2)), col="blue")

par(mar = rep(2, 4)) 
hist(x=fall, main="Temperatures in fall season", 
     xlab="Temperature is Celcius", ylab="No of days", xlim=c(15,40), ylim=c(0,70))
abline(v=fa.mean, col="red")
text(x=20, y=40, labels = paste("Mean=", round(fa.mean,2)),col="red")
abline(v=fa.median, col="blue")
text(x=35, y=40, labels = paste("Median=", round(fa.median,2)), col="blue")

par(mar = rep(2, 4)) 
hist(x=winter, main="Temperatures in winter season", 
     xlab="Temperature is Celcius", ylab="No of days", xlim=c(0,35), ylim=c(0,45))
abline(v=wi.mean, col="red")
text(x=25, y=40, labels = paste("Mean=", round(wi.mean,2)),col="red")
abline(v=wi.median, col="blue")
text(x=10, y=40, labels = paste("Median=", round(wi.median,2)), col="blue")


####Correlation between Real & Feeling Temp & no of bikes######

day$raw.atemp=atemp*50
corr_atemp=cor.test(x=day$raw.atemp, y=day$cnt)
corr_atemp

corr_temp=cor.test(x=day$raw.temp, y=day$cnt)
corr_temp

plot(x=day$raw.temp, y=day$cnt, Main="Correlation between Temp & Bike Rentals",
     xlab = "Temp", ylab = "Bike Rentals", xlim = c(0, 45), ylim=c(0, 9000),
     col="deepskyblue", pch=20)

points(x=day$raw.atemp, y=day$cnt, Main="Correlation between Temp & Bike Rentals",
       xlab = "Temp", ylab = "Bike Rentals", xlim = c(0, 45), ylim=c(0, 9000),
       col="darkolivegreen", pch=20)

legend("bottomright",
       legend = c("Temperature", "Feeling Temperature"),
       col = c("deepskyblue", "darkolivegreen"),
       pch = c(20, 20),
       bg = "white")


####Is the feeling temperature significantly different to the real temperature?##### 
######If yes, is there a difference in each season?#######



test.result=t.test(x=day$raw.atemp, y=day$raw.temp, alternative = "two.sided")
test.result

temp.spring=subset(day,subset = season=="1")$raw.temp
atemp.spring=subset(day, subset = season=="1")$raw.atemp
test.spring=t.test(x=temp.spring, y=atemp.spring, alternative = "two.sided")
test.spring


temp.summer=subset(day,subset = season=="2")$raw.temp
atemp.summer=subset(day, subset = season=="2")$raw.atemp
test.summer=t.test(x=temp.summer, y=atemp.summer, alternative = "two.sided")
test.summer


temp.fall=subset(day,subset = season=="3")$raw.temp
atemp.fall=subset(day, subset = season=="3")$raw.atemp
test.fall=t.test(x=temp.fall, y=atemp.fall, alternative = "two.sided")
test.fall


temp.winter=subset(day,subset = season=="4")$raw.temp
atemp.winter=subset(day, subset = season=="4")$raw.atemp
test.winter=t.test(x=temp.winter, y=atemp.winter, alternative = "two.sided")
test.winter



####Are holidays and nice weather good predictors for the number of total bike rentals####

lookup <- data.frame("numbers"=c("1","2","3","4"),
                     "weather"=c("nice","cloudy", "wet", "lousy")
)

day <- merge(x= day,
             y= lookup,
             by.x="weathersit",
             by.y="numbers",
)


total.rentals=lm(cnt~holiday+weather, data=day)
summary(total.rentals)

anv.weather <- anova (total.rentals)
summary(anv.weather)
anv.weather
weather.aov=aov(cnt~weather, data=day)
summary(weather.aov)
TukeyHSD(weather.aov)



#What are the mean temperature, humidity, windspeed and total rentals per month?#

day$raw.hum <- (day$hum*100)
day$raw.windspeed <- (day$windspeed*67)
day$raw.temp <- (day$temp*41)



library(dplyr)

month.agg <- day %>% 
  group_by(mnth) %>% 
  summarise(
    mean.temp = mean(raw.temp),
    mean.hum = mean(raw.hum),
    mean.windspeed = mean(raw.windspeed),
    mean.rentals = mean(cnt)
  )


#What percentage of days are appropriate for biking concerning the weather with conditions A) Temperature > 5°, weather situation 1-3, windspeed < 40 km/h and B) Temperature > 10°, weather situation 1-2, windspeed < 20 km/h?
biking.day <- function (temp.thresh, windspeed.thresh, weathersit.thresh){
  
  result <- with (day, raw.temp > temp.thresh & 
                    raw.windspeed < windspeed.thresh & 
                    weathersit < weathersit.thresh)
  
  return(result)
}

mean(biking.day(5, 40, 3))










