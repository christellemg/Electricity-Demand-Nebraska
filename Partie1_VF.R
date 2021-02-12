#######################################################################
#######################################################################
#######################################################################
#######################################################################


#INITIALISATION

Sys.setlocale("LC_TIME", "C")

library(timeSeries)
library(astsa)
library(forecast)

setwd("~/HEC - MASTER/H2021/PREVISION/PROJET")

load("sppdata.RData")
reg=sppdata[,4]

# ADAPTER AU BON FUSEAU HORAIRE

datechar=row.names(reg)
dateter = as.POSIXct(datechar, tz="GMT")
datecst=format(dateter, tz="America/Chicago",usetz=TRUE)
les=timeSeries(reg,datecst,format= "%Y-%m-%d %H:%M:%S", tz = 'America/Chicago')
date=as.Date(datecst, format = "%Y-%m-%d %H:%M:%S", tz = 'America/Chicago')

# IMPUTATION

sum(is.na(les))
les[is.na(les),]

les[is.na(les),] = les[which(is.na(les)) - 24,]

#######################################################################
#######################################################################
#######################################################################
#######################################################################

# ANALYSE EXPLORATOIRE


#ALLURE GÉNÉRALE HORAIRE DE 2011-01-01 7h À  2021-01-01 6h



plot(les, ylab="Demande Horaire",col="blue", at="pretty",
     main="Graphique de la demande d'électricité à Lincoln entre 2011 et 2021")
abline(v=as.POSIXct("2011-01-01"), lty=10, col="red")
abline(v=as.POSIXct("2012-01-01"), lty=10, col="red")
abline(v=as.POSIXct("2013-01-01"), lty=10, col="red")
abline(v=as.POSIXct("2014-01-01"), lty=10, col="red")
abline(v=as.POSIXct("2015-01-01"), lty=10, col="red")
abline(v=as.POSIXct("2016-01-01"), lty=10, col="red")
abline(v=as.POSIXct("2017-01-01"), lty=10, col="red")
abline(v=as.POSIXct("2018-01-01"), lty=10, col="red")
abline(v=as.POSIXct("2019-01-01"), lty=10, col="red")
abline(v=as.POSIXct("2020-01-01"), lty=10, col="red")
abline(v=as.POSIXct("2021-01-01"), lty=10, col="red")

# CONSTITUTION DATAFRAME


df=data.frame(reg,datecst,date)
rownames(df) = NULL

df$day = format(df$date, "%d")
df$month = format(df$date, "%m")
df$year = format(df$date, "%Y")
df$weekday = weekdays(date)
df$weekdaybin = ifelse(df$weekday == "Saturday" | df$weekday == "Sunday" , 1, 0)
dfconge = df[1:26303,]
dfconge$jourferie = ifelse(dfconge$date == as.Date("2011-01-01") , 1, ifelse(dfconge$date == as.Date("2011-01-17") , 1, 
                    ifelse(dfconge$date == as.Date("2011-02-21") , 1, ifelse(dfconge$date == as.Date("2011-05-30") , 1, 
                    ifelse(dfconge$date == as.Date("2011-06-19") , 1, ifelse(dfconge$date == as.Date("2011-07-04") , 1, 
                    ifelse(dfconge$date == as.Date("2011-09-05") , 1, ifelse(dfconge$date == as.Date("2011-10-10") , 1, 
                    ifelse(dfconge$date == as.Date("2011-11-11") , 1, ifelse(dfconge$date == as.Date("2011-11-24") , 1, 
                    ifelse(dfconge$date == as.Date("2011-11-25") , 1, ifelse(dfconge$date == as.Date("2011-12-26") , 1, 
                    ifelse(dfconge$date == as.Date("2011-12-25") , 1, ifelse(dfconge$date == as.Date("2012-01-01") , 1, 
                    ifelse(dfconge$date == as.Date("2012-01-02") , 1, ifelse(dfconge$date == as.Date("2012-01-16") , 1, 
                    ifelse(dfconge$date == as.Date("2012-02-20") , 1, ifelse(dfconge$date == as.Date("2012-05-28") , 1, 
                    ifelse(dfconge$date == as.Date("2012-06-19") , 1, ifelse(dfconge$date == as.Date("2012-07-04") , 1,  
                    ifelse(dfconge$date == as.Date("2012-09-03") , 1, ifelse(dfconge$date == as.Date("2012-10-08") , 1, 
                    ifelse(dfconge$date == as.Date("2012-11-11") , 1, ifelse(dfconge$date == as.Date("2012-11-12") , 1, 
                    ifelse(dfconge$date == as.Date("2012-11-22") , 1, ifelse(dfconge$date == as.Date("2012-11-23") , 1, 
                    ifelse(dfconge$date == as.Date("2012-12-25") , 1, ifelse(dfconge$date == as.Date("2013-01-01") , 1, 
                    ifelse(dfconge$date == as.Date("2013-01-21") , 1, ifelse(dfconge$date == as.Date("2013-02-18") , 1, 
                    ifelse(dfconge$date == as.Date("2013-05-27") , 1, ifelse(dfconge$date == as.Date("2013-06-19") , 1, 
                    ifelse(dfconge$date == as.Date("2013-07-04") , 1, ifelse(dfconge$date == as.Date("2013-09-02") , 1, 
                    ifelse(dfconge$date == as.Date("2013-10-14") , 1, ifelse(dfconge$date == as.Date("2013-11-11") , 1, 
                    ifelse(dfconge$date == as.Date("2013-11-28") , 1, ifelse(dfconge$date == as.Date("2013-11-29") , 1, 
                    ifelse(dfconge$date == as.Date("2013-12-25") , 1, ifelse(dfconge$date == as.Date("2013-12-31") , 1, 0))))))))))))))))))))))))))))))))))))))))


df$hour = substr(df$datecst,12,13)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
head(df)
str(df)


# DEMANDE JOURNALIÈRE MAXX

dfjourmax = aggregate(df, by=list(df$date), FUN = max)

picdemandemoymensuel = aggregate(dfjourmax$LES, by=list(dfjourmax$month), FUN=mean)
picdemandemoyannuel = aggregate(dfjourmax$LES, by=list(dfjourmax$year), FUN=mean)
picdemandeHEBDO = aggregate(dfjourmax$LES, by=list(dfjourmax$weekday), FUN=mean)

plot(dfjourmax$date, dfjourmax$LES,type="l", col="green",xlab="Années", 
     ylab="Demande journalière max",
     main="Graphique de la demande d'électricité maximale journalière à Lincoln entre 2011 et 2021")

summary(dfjourmax$LES)



# BOXPLOT 

boxplot(LES~hour, data=df, main="Demande horaire", xlab="Mois de l'année", ylab="Demande")
boxplot(LES~month, data=dfjourmax, main="Demande mensuelle", xlab="Mois de l'année", ylab="Demande")
boxplot(LES~year, data=dfjourmax, main="Demande annuelle", xlab="Années", ylab="Demande")
#dfjourmax$weekday = factor(dfjourmax$weekday, levels=c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"))
dfjourmax$weekday = factor(dfjourmax$weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
boxplot(LES~weekday, data=dfjourmax, main="Demande hebdomadaire", xlab="Jour de la semaine", ylab="Demande")



#######################################################################
#######################################################################
#######################################################################
#######################################################################


# MÉTHODES NAIVES



#FORMATAGE DES DONNÉES (PIC JOURNALIER)
df1 = df[,c(1,3)]
dem.jour <- aggregate(LES ~ date, df1,FUN = max)

# ENLEVER LE 29 FÉVRIER
dem.jour= dem.jour[-c(425, 1886, 3347), ] 


# CRÉATION DE L'OBJET TIMESERIES
sdate1  <- c(2011,1)
dem.jour.ts <- ts(dem.jour$LES, start=sdate1, frequency=365)
plot(dem.jour.ts,ylab="Daily demand in LES",type="l",main="dem.jour.ts")

# DIVISION ENTRE TRAIN VALID ET TEST POUR LES RAPPORTS SUIVANTS
dem.jour.ts.train = window(dem.jour.ts, start=c(2011,1), end=c(2016,365))
plot(dem.jour.ts.train,ylab="Daily demand in LES",type="l",main="dem.jour.ts.train")

dem.jour.ts.valid = window(dem.jour.ts, start=c(2017,1), end=c(2018,365))
plot(dem.jour.ts.valid,ylab="Daily demand in LES",type="l",main="dem.jour.ts.valid")
  
dem.jour.ts.test = window(dem.jour.ts, start=c(2019,1))
plot(dem.jour.ts.test,ylab="Daily demand in LES",type="l",main="dem.jour.ts.test")

ffcaststart <- c(2017,1)
ffcastend <- c(2019,1)

# A- NAIVE NO CHANGE

naive1 <- naive(dem.jour.ts, h=1)

forecast <- window(naive1$fitted, start=ffcaststart, end=ffcastend) 
observed <- window(naive1$x, start=ffcaststart, end=ffcastend)
bias1  <- mean(forecast-observed)
pbias1 <- mean((forecast-observed)/observed)*100
mape1  <- mean(abs((forecast-observed)/observed)*100)

# B- NAIVE SAISONNIER

naiveS <- snaive(dem.jour.ts, h=1)

forecastS <- window(naiveS$fitted, start=ffcaststart, end=ffcastend) 
observedS <- window(naiveS$x, start=ffcaststart, end=ffcastend) 
biasS  <- mean(forecastS-observedS) 
pbiasS <- mean((forecastS-observedS)/observedS)*100
mapeS  <- mean(abs((forecastS-observedS)/observedS)*100)


# C- MOYENNE MOBILE 3 JOURS

naive3t <- zoo::rollmean(dem.jour.ts, 3, align="right")
naive3 <- naive(naive3t, h=1)

forecast3 <- window(naive3$fitted, start=ffcaststart, end=ffcastend) 
observed3 <- window(dem.jour.ts, start=ffcaststart, end=ffcastend) 
bias3  <- mean(forecast3-observed3) 
pbias3 <- mean((forecast3-observed3)/observed3)*100
mape3  <- mean(abs((forecast3-observed3)/observed3)*100)

# D- MOYENNE MOBILE 7 JOURS

naive7t <- zoo::rollmean(dem.jour.ts, 7, align="right")
naive7 <- naive(naive7t, h=1)

forecast7 <- window(naive7$fitted, start=ffcaststart, end=ffcastend) 
observed7 <- window(dem.jour.ts, start=ffcaststart, end=ffcastend) 
bias7  <- mean(forecast7-observed7) 
pbias7 <- mean((forecast7-observed7)/observed7)*100
mape7  <- mean(abs((forecast7-observed7)/observed7)*100)


cat("Using function <accuracy>:","\n")
print(accuracy(forecast, observed)[,1:5])
print(accuracy(forecastS, observedS)[,1:5])
print(accuracy(forecast3, observed3)[,1:5])
print(accuracy(forecast7, observed7)[,1:5])


# GRAPHIQUES DES MÉTHODES NAIVES
plot(observedS, ylab="Demande et prévisions journalières d'électricité à Lincoln")
lines(forecastS, col="red")
lines(window(forecast, start=ffcaststart), col="blue")
lines(window(forecast3, start=ffcaststart), col="cyan")
lines(window(forecast7, start=ffcaststart), col="green")
legend("bottomleft", 
       legend=c("observé","naive no change", "naive saisonnier","moyenne mobile 3 jours","naive hebdomadaire"),
       col=c("black","blue","red","cyan","green"), lty=1)

plot(window(observedS, start=c(2018,334)), lwd=2, ylab="Demande d'électricité à Lincoln", 
     main="Demande et prévisions naives à Lincoln sur décembre 2018")
lines(window(forecastS, start=c(2018,334)), col="red")
lines(window(forecast, start=c(2018,334)), col="blue")
lines(window(forecast3, start=c(2018,334)), col="cyan")
lines(window(forecast7, start=c(2018,334)), col="green")
legend("bottomleft", 
       legend=c("observé","naive no change", "naive sais 365","moy mob 3 jours","moy mob 7 jours"),
       col=c("black","blue","red","cyan","green"), lty=1)


##############################################################################
##############################################################################
##############################################################################

# VARIABLE EXPLICATIVES  

##############################################################################

# MÉTÉO / TEMPÉRATURE

meteo=read.csv("weather.csv",header=TRUE)
meteo= meteo[-c(424, 1882, 3340), ] 

summary(meteo)

# IMPUTATION 


meteo[is.na(meteo$TMAX),] = meteo[which(is.na(meteo$TMAX)) - 1,]
meteo[is.na(meteo$TMIN),] = meteo[which(is.na(meteo$TMIN)) - 1,]
meteo[is.na(meteo$TMIN),] = meteo[which(is.na(meteo$TMIN)) - 1,]

imp1 <- c("USW00094995", "2011-03-18", 0.00,70,35)    
imp2 <- c("USW00094995", "2012-04-06", 0.01,67,40)  
imp3 <- c("USW00094995", "2013-05-08", 0.01,74,39)
imp4 <- c("USW00094995", "2015-06-04", 0.54,79,62)  
imp5 <- c("USW00094995", "2016-06-13", 0.00,95,65)  
imp6 <- c("USW00094995", "2017-06-20", 0.00,85,59)  
imp7 <- c("USW00094995", "2018-08-26", 0.00,92,58)  


meteo_new <- rbind(meteo[1:76, ], imp1, meteo[77:459, ], imp2, meteo[460:855,],
                   imp3, meteo[856:1611,] , imp4, meteo[1612:1988,] , 
                   imp5, meteo[1989:2355,], imp6, meteo[2356:2786,] , 
                   imp7, meteo[2787:3644,]) 

meteo_new$PRCP = as.numeric(meteo_new$PRCP)
meteo_new$TMAX = as.integer(meteo_new$TMAX)
meteo_new$TMIN = as.integer(meteo_new$TMIN)
str(meteo_new)

summary(meteo_new)

#Conversion des températures en celsius
meteo_new$t.max.c =  5/9*(meteo_new$TMAX-32)
meteo_new$t.min.c =  5/9*(meteo_new$TMIN-32)


#Calcul de la température moyenne
meteo_new$mean.temp = (meteo_new$t.max.c+meteo_new$t.min.c)/2



#Suppression de la variable station, TMAX, TMIN, t.max.c et t.min.c
meteo_new <- subset(meteo_new, select = -c(STATION,TMAX,TMIN))

#Création HDD et CDD
meteo_new$CDD = ifelse(meteo_new$mean.temp >=14 ,meteo_new$mean.temp-14 , 0)
meteo_new$HDD = ifelse(meteo_new$mean.temp < 14 ,14-meteo_new$mean.temp , 0)


# Création objets time series 
w.prcp   <- timeSeries(meteo_new$PRCP, meteo_new$DATE, format="%Y-%m-%d")
w.temp <- timeSeries(meteo_new$mean.temp, meteo_new$DATE, format="%Y-%m-%d")
w.tempmax <- timeSeries(meteo_new$t.max.c, meteo_new$DATE, format="%Y-%m-%d")
w.tempmin <- timeSeries(meteo_new$t.min.c, meteo_new$DATE, format="%Y-%m-%d")
w.cdd <- timeSeries(meteo_new$CDD, meteo_new$DATE, format="%Y-%m-%d")
w.hdd <- timeSeries(meteo_new$HDD, meteo_new$DATE, format="%Y-%m-%d")

demande<- timeSeries(dem.jour$LES, dem.jour$date, format="%Y-%m-%d")



plot(series(w.temp), series(demande),
         ylab="Demande journalière à Lincoln (MW/h)",
         xlab="Température moyenne (Celsius)",
     main="Relation entre température et demande d'électricité", pch=23)

plot(series(w.cdd), series(demande),
     ylab="Demande journalière à Lincoln (MW/h)",
     xlab="CDD/HDD", pch=23,
     main="Relation entre CDD/HDD et demande d'électricité",
     col="blue",xlim=c(0,30))
points(series(w.hdd), series(demande),pch=23,col="red")
legend(1,802, legend=c("CDD","HDD"),pch=23 ,col=c("blue", "red"))


plot(series(w.hdd), series(demande),
     ylab="Demande journalière à Lincoln (MW/h)",
     xlab="HDD", pch=23,main="Relation entre HDD et demande d'électricité")

plot(lag(w.hdd,1), series(demande), 
     xlab="lag-1 HDDt",ylab="daily demand in Toronto (MW/h)")

plot(lag(w.hdd,2), series(demande), 
     xlab="lag-2 HDDt",ylab="Demande journalière à Lincoln (MW/h)",
     main="Relation entre le HDD deux fois différencié et demande d'électricité")


# Ajouter la variable saison

dfjourmax$date_m_d=format(dfjourmax$date, format = "%m-%d")

WS <- format(as.Date("12-21", format = "%m-%d"), format = "%m-%d") # Winter Solstice
SE <- format(as.Date("03-21", format = "%m-%d"), format = "%m-%d") # Spring Equinox
SS <- format(as.Date("06-21", format = "%m-%d"), format = "%m-%d") # Summer Solstice
FE <- format(as.Date("09-21", format = "%m-%d"), format = "%m-%d") # Fall Equinox

dfjourmax$season =  ifelse (dfjourmax$date_m_d >= WS | dfjourmax$date_m_d < SE, "Winter",
                            ifelse (dfjourmax$date_m_d >= SE & dfjourmax$date_m_d < SS, "Spring",
                                    ifelse (dfjourmax$date_m_d >= SS & dfjourmax$date_m_d < FE, "Summer", "Fall")))

boxplot(LES~season, data=dfjourmax, main="Demande par saison", xlab="Saisons", ylab="Demande")

picdemandesaison = aggregate(dfjourmax$LES, by=list(dfjourmax$season), FUN=mean)


totohiver <- subset(dfjourmax, season == "Winter")
totoete <- subset(dfjourmax, season == "Summer")
par(mfrow=c(2,1))
boxplot(LES~weekday, data=totohiver, main="Demande hebdomadaire hivernale", 
        xlab="Jour de la semaine", ylab="Demande")
boxplot(LES~weekday, data=totoete, main="Demande hebdomadaire estivale", 
        xlab="Jour de la semaine", ylab="Demande")

# COMPARAISON ÉTÉ VS HIVER (1 SEMAINE)

week1 <- series(window(les,
                       start=timeDate("2017-01-01-00", format="%Y-%m-%d-%H"),
                       end=timeDate("2017-01-07-23", format="%Y-%m-%d-%H")))
week3 <- series(window(les,
                       start=timeDate("2017-07-16-00", format="%Y-%m-%d-%H"),
                       end=timeDate("2017-07-22-23", format="%Y-%m-%d-%H")))
plot(week1, axes=F,
     lty=1, col="blue", type="l", ylim=c(min(week1,week3),max(week1,week3)),
     ylab="Les hourly demand (in MW)", xlab="",
     main="Comparaison de la demande horaire hivernale et estivale")
lines(week3, col="red", lty=1, lwd=1.8)
axis(1, at=seq(1+12,24*7+12, by=24),
     labels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
legend("bottomright", legend=c("Jan 1-7, 2017","Jul 16-22, 2017"), 
       lty=1, col=c("blue","red"))

##############################################################################


# Variables chronologiques (dans la partie analyse exploratoire)

year2011 <- series(window(demande,
                          start=timeDate("2011-01-01", format="%Y-%m-%d"),
                          end=timeDate("2011-12-31", format="%Y-%m-%d")))
year2015 <- series(window(demande,
                          start=timeDate("2015-01-01", format="%Y-%m-%d"),
                          end=timeDate("2015-12-31", format="%Y-%m-%d")))
year2019 <- series(window(demande,
                          start=timeDate("2019-01-01", format="%Y-%m-%d"),
                          end=timeDate("2019-12-31", format="%Y-%m-%d")))


plot(year2011,
     lty=1, col="red", type="l", ylim=c(min(year2011,year2015,year2019),max(year2011,year2015,year2019)),
     xlab="Mois de l'année", ylab="Demande journalière à Lincoln en MW",
     main="Comparaison de la demande d'électricité entre différentes années selon le mois")
lines(year2015, lty=1,col="blue", lwd=1.8)
lines(year2019, lty=1,col="green", lwd=1.8)
legend("topleft", legend=c("2011","2015","2020"),lty=1 ,col=c("red", "blue","green"))


##############################################################################


# JOUR CONGÉ + FIN DE SEMAINE

dfcongemax = aggregate(dfconge, by=list(dfconge$date), FUN = max)
boxplot(LES~jourferie, data=dfcongemax, main="Effet des jours fériés sur la demande", 
        xlab="Férié (1) ou pas (0)", ylab="Demande")


dfcongemax$fdscong = ifelse(dfcongemax$weekdaybin == 1 | dfcongemax$jourferie == 1 , 1, 0)
boxplot(LES~fdscong, data=dfcongemax, main="Effet des jours de congé sur la demande", 
        xlab="Off (1) ou pas (0)", ylab="Demande")



#######################################################################
#######################################################################
#######################################################################
#######################################################################













