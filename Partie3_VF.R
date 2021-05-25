############################################################
############################################################
############################################################
############################################################
############################################################
############################################################

#################### PARTIE 3 PROJET #######################

############################################################
############################################################

# ARIMA / SARIMA 

# CONSTITUTION DATAFRAME

#INITIALISATION

Sys.setlocale("LC_TIME", "C")
library(forecast)
setwd("~/HEC - MASTER/H2021/PREVISION/PROJET/PARTIE3")
load("sppdata.RData")
reg=sppdata[,4]

# ADAPTER AU BON FUSEAU HORAIRE
datechar=row.names(reg)
dateter = as.POSIXct(datechar, tz="GMT")
datecst=format(dateter, tz="America/Chicago",usetz=TRUE)
les=timeSeries(reg,datecst,format= "%Y-%m-%d %H:%M:%S", 
               tz = 'America/Chicago')
date=as.Date(datecst, format = "%Y-%m-%d %H:%M:%S", 
             tz = 'America/Chicago')

# IMPUTATION
les[is.na(les),] = les[which(is.na(les)) - 24,]
df=data.frame(reg,date)
rownames(df) = NULL
head(df)
str(df)

library(astsa)
library(timeSeries)

# CREATION DES VARIABLES EXPLICATIVES

df$day = as.numeric(format(df$date, "%d"))
df$month = as.numeric(format(df$date, "%m"))
df$year = as.numeric(format(df$date, "%Y"))
df$date_m_d=format(df$date, format = "%m-%d")
df$weekday = weekdays(date)
df$Lundi = 
  ifelse(df$weekday == 'Monday', 1, 0 )
df$Mardi = 
  ifelse(df$weekday == 'Tuesday', 1, 0 )
df$Mercredi = 
  ifelse(df$weekday == 'Wednesday', 1, 0 )
df$Jeudi = 
  ifelse(df$weekday == 'Thursday', 1, 0 )
df$Vendredi = 
  ifelse(df$weekday == 'Friday', 1, 0 )
df$Samedi =
  ifelse(df$weekday == 'Saturday', 1, 0 )
df$Dimanche =
  ifelse(df$weekday == 'Sunday', 1, 0 )
df$weekdaybin = 
  ifelse(df$weekday == "Saturday" 
        | df$weekday == "Sunday" , 1, 0)
df$Janvier = ifelse(df$month == 1, 1, 0 )
df$Fevrier = ifelse(df$month == 2, 1, 0 )
df$Mars = ifelse(df$month == 3, 1, 0 )
df$Avril = ifelse(df$month == 4, 1, 0 )
df$May = ifelse(df$month == 5, 1, 0 )
df$Juin = ifelse(df$month == 6, 1, 0 )
df$Juillet = ifelse(df$month == 7, 1, 0 )
df$Aout = ifelse(df$month == 8, 1, 0 )
df$Septembre = ifelse(df$month == 9, 1, 0 )
df$Octobre = ifelse(df$month == 10, 1, 0 )
df$Novembre = ifelse(df$month == 11, 1, 0 )
df$Decembre = ifelse(df$month == 12, 1, 0 )
df$Nouvel_an = 
  ifelse(df$date_m_d == '01-01', 1, 
              ifelse(df$Lundi==1 & 
               df$date_m_d == '01-02',1,0))
df$Fete_National = 
  ifelse(df$date_m_d == '07-04', 1, 0)
df$Veteran_Day = 
  ifelse(df$date_m_d == '11-11', 1, 0)
df$Noel = 
  ifelse(df$date_m_d == '12-25', 1, 0)

#Martin Luther King Day
df$Martin_Luther_day = 0 
for (i in 2011:2020) {
  df$Martin_Luther_day[which(df$month==1 & 
             df$Lundi==1 &
             df$year == i)[c(49:72)]] = 1
}

#President day
df$President_day = 0 
for (i in 2011:2020) {
  df$President_day[which(df$month==1 & 
            df$Lundi==1 &
            df$year == i)[c(49:72)]] = 1
}

#Arbor day

df$Arbor_day = 0 
Longueur = NULL

for (i in 2011:2020) {
  Longueur = 
    length(which(df$month==4& df$Vendredi==1 & 
                df$year == i))  
  df$Arbor_day[which(df$month==4& df$Vendredi==1 
   &df$year == i)[c((Longueur-23):Longueur)]]  = 1
}

#Memorial day

df$Memorial_day = 0 
Longueur = NULL

for (i in 2011:2020) {
  Longueur = 
    length(which(df$month==1 & df$Lundi==1 & 
        df$year == i))  
  df$Memorial_day[which(df$month==1 & df$Lundi==1 & 
   df$year == i)[c((Longueur-23):Longueur)]] = 1
}

#Labor day 

df$Labor_day = 0 
for (i in 2011:2020) {
  df$Labor_day[which(df$month==9 &
    df$Lundi==1 & df$year == i)[c(1:24)]] = 1
}

#Columbus day 

df$Columbus_day = 0 
for (i in 2011:2020) {
  df$Columbus_day[which(df$month==10 & df$Lundi==1 & 
        df$year == i)[c(25:48)]] = 1
}


#Thanks Giving

df$Thanks_Giving = 0 
for (i in 2011:2020) {
  df$Thanks_Giving[which(df$month==11 & df$Jeudi==1 & 
          df$year == i)[c(73:96)]] = 1
}


df$Jour_Ferie = 
  ifelse(df$Nouvel_an == 1 | df$Fete_National == 1 |
         df$Noel == 1 | df$Labor_day == 1 |
         df$Thanks_Giving == 1,1,0)



WS <- format(as.Date("12-21", format = "%m-%d"), 
             format = "%m-%d") # Winter Solstice
SE <- format(as.Date("03-21", format = "%m-%d"), 
             format = "%m-%d") # Spring Equinox
SS <- format(as.Date("06-21", format = "%m-%d"), 
             format = "%m-%d") # Summer Solstice
FE <- format(as.Date("09-21", format = "%m-%d"), 
             format = "%m-%d") # Fall Equinox

df$season =  
  ifelse (df$date_m_d >= WS | df$date_m_d < SE, 1,
  ifelse (df$date_m_d >= SE & 
     df$date_m_d < SS, 2,
  ifelse (df$date_m_d >= SS & 
     df$date_m_d < FE, 3, 4)))
df$Winter = ifelse(df$season == 1, 1, 0)
df$Spring = ifelse(df$season == 2, 1, 0)
df$Summer = ifelse(df$season == 3, 1, 0)
df$Fall = ifelse(df$season == 4, 1, 0)



#FORMATAGE DES DONNEES (PIC JOURNALIER)

df_29 = 
  aggregate(.~date, df[,-c(6,7)], max)

df_29$Jour_Ferie_Veille = NULL

for (i in 1:nrow(df_29)) {
  df_29$Jour_Ferie_Veille[i] = 
    ifelse(df_29$Jour_Ferie[i+1] == 1, 1, 0)
}

df_29$Jour_Ferie_Lendemain = NULL
df_29$Jour_Ferie_Lendemain[1] = 0
for (i in 2:nrow(df_29)) {
  df_29$Jour_Ferie_Lendemain[i] = 
    ifelse(df_29$Jour_Ferie[i-1] == 1, 1, 0)
}

# Enlever les 29 fevrier 
df_28= df_29[-which(df_29$day==29 & 
              df_29$month==2),] 

#ENLEVER LE 1 JANVIER 2021
df_29 = df_29[-c(3654),]



##Diviser train, valid et test 
df_29_Train = df_29[1:2192,]
df_29_Train2 = df_29[1:2557,]
df_29_Valid = df_29[2193:2922,]
df_29_Valid2 = df_29[2558:2922,]
df_29_Test = df_29[2923:nrow(df_29),]

# METEO / TEMPERATURE

ventmoy = read.csv("ventmoy.csv", 
                   header=TRUE)
tempete = read.csv("tempete.csv",
                   header=TRUE)

meteo=read.csv("weather.csv",
               header=TRUE)


meteo$DATE = as.character(meteo$DATE)

#meteo= meteo[-c(424, 1882, 3340), ] 

summary(meteo)

# IMPUTATION 

meteo[is.na(meteo$TMAX),4] = 
  meteo[which(is.na(meteo$TMAX)) - 1,][,4]
meteo[is.na(meteo$TMAX),4] = 
  meteo[which(is.na(meteo$TMAX)) - 1,][,4]
meteo[is.na(meteo$TMIN),5] = 
  meteo[which(is.na(meteo$TMIN)) - 1,][,5]
meteo[is.na(meteo$TMIN),5] = 
  meteo[which(is.na(meteo$TMIN)) - 1,][,5]


imp1 <- c("USW00094995", 
          "2011-03-18", 0.00,70,35)    
imp2 <- c("USW00094995", 
          "2012-04-06", 0.01,67,40)  
imp3 <- c("USW00094995", 
          "2013-05-08", 0.01,74,39)
imp4 <- c("USW00094995", 
          "2015-06-04", 0.54,79,62)  
imp5 <- c("USW00094995", 
          "2016-06-17", 0.00,95,65)  
imp6 <- c("USW00094995", 
          "2017-06-20", 0.00,85,59)  
imp7 <- c("USW00094995", 
          "2018-08-26", 0.00,92,58)  


meteo_29 <- rbind(meteo[1:76, ], 
                  imp1, meteo[77:459, ], 
                  imp2, meteo[460:855,] , 
                  imp3, meteo[856:1611,] , 
                  imp4, meteo[1612:1988,] , 
                  imp5, meteo[1989:2355,],
                  imp6, meteo[2356:2786,] , 
                  imp7, meteo[2787:3647,]) 
meteo_29$PRCP = as.numeric(meteo_29$PRCP)
meteo_29$TMAX = as.integer(meteo_29$TMAX)
meteo_29$TMIN = as.integer(meteo_29$TMIN)
meteo_29$Wind = ventmoy[,7]

str(meteo_29)

summary(meteo_29)


#Imputation des vents 

meteo_29$Wind[which(is.na(meteo_29$Wind))] = 
  meteo_29$Wind[which(is.na(meteo_29$Wind)) - 1]
meteo_29$Wind[which(is.na(meteo_29$Wind))] = 
  meteo_29$Wind[which(is.na(meteo_29$Wind)) - 1]
meteo_29$Wind[which(is.na(meteo_29$Wind))] = 
  meteo_29$Wind[which(is.na(meteo_29$Wind)) - 1]
meteo_29$Wind[which(is.na(meteo_29$Wind))] = 
  meteo_29$Wind[which(is.na(meteo_29$Wind)) - 1]
meteo_29$Wind[which(is.na(meteo_29$Wind))] = 
  meteo_29$Wind[which(is.na(meteo_29$Wind)) - 1]
meteo_29$Wind[which(is.na(meteo_29$Wind))] = 
  meteo_29$Wind[which(is.na(meteo_29$Wind)) - 1]
meteo_29$Wind[which(is.na(meteo_29$Wind))] = 
  meteo_29$Wind[which(is.na(meteo_29$Wind)) - 1]
meteo_29$Wind[which(is.na(meteo_29$Wind))] = 
  meteo_29$Wind[which(is.na(meteo_29$Wind)) - 1]
meteo_29$Wind[which(is.na(meteo_29$Wind))] = 
  meteo_29$Wind[which(is.na(meteo_29$Wind)) - 1]


#Conversion des temperatures en celsius
meteo_29$t.max.c =  5/9*(meteo_29$TMAX-32)
meteo_29$t.min.c =  5/9*(meteo_29$TMIN-32)


#Calcul de la temperature moyenne
meteo_29$mean.temp = 
  (meteo_29$t.max.c+meteo_29$t.min.c)/2

#Bruit pour temp????rature et vent

set.seed(10)

bruit.temp = rnorm(nrow(meteo_29),0,0.5)
meteo_29$mean.temp.bruit = 
  meteo_29$mean.temp + bruit.temp

bruit.wind = rnorm(nrow(meteo_29),0,1.5)
meteo_29$Wind.bruit = 
  meteo_29$Wind + bruit.wind

meteo_29 <- subset(meteo_29, select = 
                     -c(STATION,TMAX,TMIN))

#Creation HDD et CDD
meteo_29$CDD = 
  ifelse(meteo_29$mean.temp >=14 ,
        meteo_29$mean.temp-14 , 0)
meteo_29$CDD.bruit = 
  ifelse(meteo_29$mean.temp.bruit >=14 ,
         meteo_29$mean.temp.bruit-14 , 0)

meteo_29$HDD =
  ifelse(meteo_29$mean.temp < 10 ,
  10-meteo_29$mean.temp , 0)
meteo_29$HDD.bruit = 
  ifelse(meteo_29$mean.temp.bruit < 10 ,
   10-meteo_29$mean.temp.bruit , 0)

#Creation de Cp 
meteo_29$CP =
  ifelse(meteo_29$mean.temp < 12 ,
         (meteo_29$Wind)^0.5*
           (12 - meteo_29$mean.temp) ,0)
meteo_29$CP.bruit = 
  ifelse(meteo_29$mean.temp.bruit < 12 ,
  (meteo_29$Wind.bruit)^0.5*
    (12 - meteo_29$mean.temp.bruit) , 0)

#ENLEVER LE 1 JANVIER 2021
meteo_29 = meteo_29[-c(3654),]
meteo_28 = meteo_29[-which(df_29$day==29 
            & df_29$month==2),] 




#Tornado

Tornado = 
  tempete[which(tempete$EVENT_TYPE == 'Tornado'),]

Date_tornado = 
  unique(Tornado$BEGIN_DATE)

df_29$Tornado = 
  ifelse(df_29$date %in% as.Date(Date_tornado, 
         format = "%m/%d/%Y"),1,0)

##Diviser train, valid et test 
df_29_Train = df_29[1:2192,]
df_29_Valid = df_29[2193:2922,]
df_29_Test = df_29[2923:nrow(df_29),]

# Creation objets time series 

w.prcp   = 
  timeSeries(meteo_29$PRCP, meteo_29$DATE, 
       format="%Y-%m-%d")
w.temp = 
  timeSeries(meteo_29$mean.temp, meteo_29$DATE, 
       format="%Y-%m-%d")
w.tempmax = 
  timeSeries(meteo_29$t.max.c, meteo_29$DATE,
       format="%Y-%m-%d")
w.tempmin = 
  timeSeries(meteo_29$t.min.c, meteo_29$DATE,
       format="%Y-%m-%d")
w.cdd = 
  timeSeries(meteo_29$CDD, meteo_29$DATE, 
       format="%Y-%m-%d")
w.cdd_train = 
  window(w.cdd, start=time(w.cdd)[1], 
        end=time(w.cdd)[2192])
w.cdd_train2 = 
  window(w.cdd, start=time(w.cdd)[1], 
         end=time(w.cdd)[2557])
w.cdd_valid = 
  window(w.cdd, start=time(w.cdd)[2193], 
         end=time(w.cdd)[2922])
w.cdd_valid2 = 
  window(w.cdd, start=time(w.cdd)[2558], 
        end=time(w.cdd)[2922])
w.cdd_test  = 
  window(w.cdd, start=time(w.cdd)[2923], 
        end=time(w.cdd)[nrow(w.cdd)])
w.cdd.bruit = 
  timeSeries(meteo_29$CDD.bruit, meteo_29$DATE,
           format="%Y-%m-%d")
w.cdd.bruit_valid = 
  window(w.cdd.bruit, 
          start=time(w.cdd.bruit)[2193], 
          end=time(w.cdd.bruit)[2922])
w.cdd.bruit_test = 
  window(w.cdd.bruit, 
           start=time(w.cdd.bruit)[2923], 
           end=time(w.cdd.bruit)[nrow(w.cdd.bruit)])


w.hdd = 
  timeSeries(meteo_29$HDD, meteo_29$DATE, 
         format="%Y-%m-%d")
w.hdd_train =
  window(w.hdd, start=time(w.hdd)[1],
      end=time(w.hdd)[2192])
w.hdd_train2 = 
  window(w.hdd, start=time(w.hdd)[1], 
          end=time(w.hdd)[2557])
w.hdd_valid = 
  window(w.hdd, start=time(w.hdd)[2193], 
           end=time(w.hdd)[2922])
w.hdd_test  = 
  window(w.hdd, start=time(w.hdd)[2923], 
        end=time(w.hdd)[nrow(w.hdd)])
w.hdd.bruit = 
  timeSeries(meteo_29$HDD.bruit, meteo_29$DATE,
            format="%Y-%m-%d")
w.hdd.bruit_valid =
  window(w.hdd.bruit, 
        start=time(w.hdd.bruit)[2193], 
        end=time(w.hdd.bruit)[2922])
w.hdd.bruit_test = 
  window(w.hdd.bruit, 
        start=time(w.hdd.bruit)[2923],
        end=time(w.hdd.bruit)[nrow(w.hdd.bruit)])

w.wind = 
  timeSeries(meteo_29$Wind, meteo_29$DATE, 
            format="%Y-%m-%d")

w.cp = 
  timeSeries(meteo_29$CP, meteo_29$DATE, 
            format="%Y-%m-%d")
w.cp_train = 
  window(w.cp, start=time(w.cp)[1], 
        end=time(w.cp)[2192])
w.cp_train2 = 
  window(w.cp, start=time(w.cp)[1], 
         end=time(w.cp)[2557])
w.cp_valid = 
  window(w.cp, start=time(w.cp)[2193], 
        end=time(w.cp)[2922])
w.cp_test = 
  window(w.cp, start=time(w.cp)[2923], 
          end=time(w.cp)[nrow(w.cp)])
w.cp.bruit = 
  timeSeries(meteo_29$CP.bruit, 
           meteo_29$DATE, 
            format="%Y-%m-%d")
w.cp.bruit_valid = window(w.cp.bruit, 
            start=time(w.cp.bruit)[2193], 
            end=time(w.cp.bruit)[2922])
w.cp.bruit_test = window(w.cp.bruit, 
            start=time(w.cp.bruit)[2923],
            end=time(w.cp)[nrow(w.cp.bruit)])

w.cddlag1 = lag(w.cdd,1)
w.cddlag1_train = lag(w.cdd_train,1)
w.cddlag1_train2 = lag(w.cdd_train2,1)
w.cddlag1_valid = lag(w.cdd_valid,1)
w.cddlag1_test = lag(w.cdd_test,1)

w.cddlag2 = lag(w.cdd,2)
w.cddlag2_train = lag(w.cdd_train,2)
w.cddlag2_train2 = lag(w.cdd_train2,2)
w.cddlag2_valid = lag(w.cdd_valid,2)
w.cddlag2_test = lag(w.cdd_test,2)

w.hddlag1 = lag(w.hdd,1)
w.hddlag1_train = lag(w.hdd_train,1)
w.hddlag1_train2 = lag(w.hdd_train2,1)
w.hddlag1_valid = lag(w.hdd_valid,1)
w.hddlag1_test = lag(w.hdd_test,1)

w.hddlag2 = lag(w.hdd,2)
w.hddlag2_train = lag(w.hdd_train,2)
w.hddlag2_train2 = lag(w.hdd_train2,2)
w.hddlag2_valid = lag(w.hdd_valid,2)
w.hddlag2_test = lag(w.hdd_test,2)

demande = timeSeries(df_29$LES, df_29$date,
                     format="%Y-%m-%d")
demande_train = timeSeries(df_29_Train$LES, 
                           df_29_Train$date,
                           format="%Y-%m-%d")
demande_train2 = timeSeries(df_29_Train2$LES, 
                            df_29_Train2$date,
                            format="%Y-%m-%d")
demande_valid = timeSeries(df_29_Valid$LES, 
                            df_29_Valid$date, 
                            format="%Y-%m-%d")
demande_test = timeSeries(df_29_Test$LES,
                           df_29_Test$date, 
                           format="%Y-%m-%d")

###################################################

# ANALYSE SERIE CHRONO

#graphe de ma timeseries

toto = lm(LES~date, 
          data=df_29_Train)
plot(df_29_Train$date, df_29_Train$LES,
     type="l", col="blue",xlab="Annees", 
     ylab="Pic de la demande [MWh]",
     main="Graphique du pic de la demande 
     journaliere a
     Lincoln entre 2011 et 2017")
abline(toto, col="red", lwd=2)
acf2(demande, max.lag = 50)


toto_train = lm(LES~date, 
                data=df_29_Train)
plot(df_29_Train$date, df_29_Train$LES,
     type="l", col="blue",xlab="Annees", 
     ylab="Pic de la demande",
     main="Graphique du pic de la 
     demande journaliere a
     Lincoln entre 2011 et 2021")
abline(toto_train, col="red", lwd=2)
acf2(demande_train, max.lag = 50)


# BOXCOX

lambda =
  BoxCox.lambda(demande)
lambda_train =
  BoxCox.lambda(demande_train)

YMt.mean = 
  applySeries(demande,FUN=colMeans)
YMt.sd   = 
  applySeries(demande,FUN=colSds)
YMt.mean.bc = 
  applySeries(BoxCox(demande,lambda),
           FUN=colMeans)
YMt.sd.bc = 
  applySeries(BoxCox(demande,lambda),
          FUN=colSds)
par(mfrow=c(2,2))

plot(series(YMt.mean),series(YMt.sd), 
     xlab="Moyenne mensuelle du 
     pic journalier de la demande",
     ylab="??cart type",
     main="Pic de la demande 
     avant Box-Cox")
tata = lm(YMt.sd~YMt.mean)
abline(tata, col="red", lwd=2)
plot(series(YMt.mean.bc),series(YMt.sd.bc),
     xlab="Moyenne mensuelle 
     du pic journalier de la demande",
     ylab="??cart type",
     main=paste("Pic de la 
                demande apr??s Box-Cox"))
titi = lm(YMt.sd.bc~YMt.mean.bc)
abline(titi, col="red", lwd=2)

hist(demande,main="Avant Box-Cox",
     xlab="Pic de la demande")
hist(BoxCox(demande,lambda),
     main="Apr??s Box-Cox",
     xlab="BoxCox du pic de
     la demande")


# diff lag 7 de la partie training

difflag7 = diff(demande_train, lag=7)
plot(difflag7,
     main="differanciation 
     training lag =7",
     ylab="Difference MW",
     xlab="temps", type="l")
abline(h=0, col="red", lwd=2)

difflag7b = 
  diff(BoxCox(demande_train,
              lambda_train), lag=7)
plot(difflag7b, col="blue",
     main="Diff??renciation au lag 7 de
     la s??rie du pic de la demande",
     ylab="Pic de la demande diff??renci??"
     ,xlab="Ann??es", type="l")
abline(h=0, col="red", lwd=2)

acf2(difflag7, 
     main="ACF et PACF de la 
     s??rie diff??renci??e lag 7",
     max.lag = 30)
acf2(difflag7b, 
     main="ACF et PACF avec 
     differentiation de decalage 7 
     - BoxCox")
acf2(diff(difflag7b,1), 
     main="ACF et PACF avec 
     differentiation de decalage 1 et 7 
     - BoxCox")


# proc??dure it??rative pour ajuster modele

o3 = sarima(as.numeric(
  BoxCox(demande_train,lambda_train)),
  2,0,3,0,1,1,7)
o3
cat("ARIMA(2,0,3)(0,1,1)[7] - AIC:",
    o3$AIC," BIC:",o3$BIC,"\n")




###############################################

# predictions SARIMA

# entrainement unique du modele

# EXPANDING ET ROLLING WINDOW

demande_train_valid = 
  rbind(demande_train, demande_valid)


Prediction1 = NULL
prediction_final1 = NULL
prediction_se1 = NULL

Prediction2 = NULL
prediction_final2 = NULL
prediction_se2 = NULL

for (i in (nrow(demande_train)+1):
     nrow(demande_train_valid))
{
  Prediction1 = 
    sarima.for(BoxCox
               (demande_train_valid
                 [1:i-1],
                 lambda_train), 
               n.ahead = 1 ,
               p=2,
               d=0,
               q=3,
               P=0,
               D=1,
               Q=1,
               S=7,
               fixed=c( 1.3214,-0.3329,
                        -0.4649 ,-0.2055,
                        -0.0870,-1.0000),
               no.constant = TRUE,
               plot = FALSE)
  
  prediction_final1[i-2192] =  
    InvBoxCox(Prediction1$pred[1],lambda_train) 
  prediction_se1[i-2192] = 
    InvBoxCox(Prediction1$se[1], lambda_train)
  
  
  Prediction2 = 
    sarima.for(BoxCox
               (demande_train_valid
                 [(i-nrow(demande_train)):i-1],
                 lambda_train), 
               n.ahead = 1 ,
               p=2,
               d=0,
               q=3,
               P=0,
               D=1,
               Q=1,
               S=7,
               fixed=c( 1.3214,-0.3329,
                        -0.4649 ,-0.2055,
                        -0.0870,-1.0000),
               no.constant = TRUE,
               plot = FALSE)
  
  prediction_final2[i-2192] =  
    InvBoxCox(Prediction2$pred[1],lambda_train) 
  prediction_se2[i-2192] = 
    InvBoxCox(Prediction2$se[1], lambda_train)
  
  
  print(paste("    Step:", 
              i-2192,
              "    Pred_exp:", 
              prediction_final1[i-2192],
              "    Se_exp:", 
              prediction_se1[i-2192],
              "    Pred_rol:", 
              prediction_final2[i-2192],
              "    Se_rol:", 
              prediction_se2[i-2192]))

}

erreur1 = NULL
upper_pred1=NULL
lower_pred1=NULL

erreur2 = NULL
upper_pred2=NULL
lower_pred2=NULL

for (i in 1:(nrow(demande_valid)))
{
  erreur1[i] = 
    prediction_final1[i]-demande_valid[i]
  upper_pred1[i] = 
    prediction_final1[i]+1.96*prediction_se1[i]
  lower_pred1[i] = 
    prediction_final1[i]-1.96*prediction_se1[i]
  
  erreur2[i] = 
    prediction_final2[i]-demande_valid[i]
  upper_pred2[i] =
    prediction_final2[i]+1.96*prediction_se2[i]
  lower_pred2[i] =
    prediction_final2[i]-1.96*prediction_se2[i]
}


RMSE1 = 
  (mean(erreur1^2, na.rm = T))^0.5
MAPE1 =
  (mean(abs(erreur1/demande_valid), 
        na.rm = T))*100
ME1 = mean(erreur1, na.rm = T)

TABLEAU1 = cbind(RMSE1, MAPE1, ME1)
colnames(TABLEAU1) =
  c('RMSE', 'MAPE', 'ME')
TABLEAU1

RMSE2 =
  (mean(erreur2^2, na.rm = T))^0.5
MAPE2 = 
  (mean(abs(erreur2/demande_valid), 
        na.rm = T))*100
ME2 = 
  mean(erreur2, na.rm = T)

TABLEAU2 = cbind(RMSE2, MAPE2, ME2)
colnames(TABLEAU2) = 
  c('RMSE', 'MAPE', 'ME')
TABLEAU2


#Intervalle de prevision
erreur_95p=NULL

for(i in 1:730)
{
  erreur_95p[i] = 
    ifelse(as.numeric(demande_valid[i])<=
      as.numeric(lower_pred1[i]) || 
      as.numeric(demande_valid[i])>=
     as.numeric(upper_pred1[i]),0,1)
}

sum(erreur_95p, na.rm = T)/730
sum(erreur_95p, na.rm = T)

### 92.74% 

# GRAPHIQUE

dem.jour = aggregate(LES ~ date, 
                      df,FUN = max)
dem.jour = dem.jour[-c(3654),]

sdate1  = c(2011,1)
yt = ts(dem.jour$LES, start=sdate1)
yt.train = window(yt, start=time(yt)[1], 
                  end=time(yt)[2192])
yt.valid = window(yt, start=time(yt)[2193], 
                  end=time(yt)[2922])
yt.test = window(yt, start=time(yt)[2923])


prediction_final1 = ts(prediction_final1, 
             start=time(yt)[2193])
lower_pred1 = ts(lower_pred1, 
              start=time(yt)[2193])
upper_pred1 = ts(upper_pred1, 
              start=time(yt)[2193])

plot(window(prediction_final1, 
            start=time(yt)[2800]),
     col="red", 
     ylim=c(250,740), lwd = 2, 
     xlab="Septembre-D??cembre 2018",
     main="Pr??vision du pic de 
     la demande avec SARIMA",
     ylab="Pic de la demande [MWh]")
lines(window(yt.valid, start=time(yt)[2800],
             end=time(yt)[3200]),lwd=2)
lines(window(lower_pred1, start=time(yt)[2800]),
      col="blue", type="l", lty=2)
lines(window(upper_pred1, start=time(yt)[2800]),
      col="blue", type="l", lty=2)
legend("topright", 
       legend=c("predictions","observations",
                "binf","bsup"),
       col=c("red","black","blue","blue"),
       lty=c(1,1,2,2))



# entrainement journalier du modele

# EXPANDING ET ROLLING WINDOW

for (i in (nrow(demande_train)+1):
     nrow(demande_train_valid))
{
  Prediction1 = 
    sarima.for(BoxCox
               (demande_train_valid
                 [1:i-1],
                 lambda_train), 
               n.ahead = 1 ,
               p=2,
               d=0,
               q=3,
               P=0,
               D=1,
               Q=1,
               S=7,
               plot = FALSE)
  
  prediction_final1[i-2192] =  
    InvBoxCox(Prediction1$pred[1],
              lambda_train) 
  prediction_se1[i-2192] =
    Prediction1$se[1]
  
  Prediction2 = 
    sarima.for(BoxCox
               (demande_train_valid
                 [(i-nrow(demande_train)):i-1],
                 lambda_train), 
               n.ahead = 1 ,
               p=2,
               d=0,
               q=3,
               P=0,
               D=1,
               Q=1,
               S=7,
               plot = FALSE)
  
  prediction_final2[i-2192] =  
    InvBoxCox(Prediction2$pred[1],
              lambda_train) 
  prediction_se2[i-2192] = 
    Prediction2$se[1]
  
  
  print(paste("    Step:", 
              i-2192,
              "    Pred_exp:", 
              prediction_final1[i-2192],
              "    Se_exp:", 
              prediction_se1[i-2192],
              "    Pred_rol:", 
              prediction_final2[i-2192],
              "    Se_rol:", 
              prediction_se2[i-2192]))
}

erreur1 = NULL
upper_pred1=NULL
lower_pred1=NULL

erreur2 = NULL
upper_pred2=NULL
lower_pred2=NULL

for (i in 1:(nrow(demande_valid)))
{
  erreur1[i] = 
    prediction_final1[i]-demande_valid[i]
  upper_pred1[i] = 
    prediction_final1[i]+1.96*prediction_se1[i]
  lower_pred1[i] = 
    prediction_final1[i]-1.96*prediction_se1[i]
  
  erreur2[i] = 
    prediction_final2[i]-demande_valid[i]
  upper_pred2[i] =
    prediction_final2[i]+1.96*prediction_se2[i]
  lower_pred2[i] =
    prediction_final2[i]-1.96*prediction_se2[i]
}


RMSE1 = 
  (mean(erreur1^2, na.rm = T))^0.5
MAPE1 =
  (mean(abs(erreur1/demande_valid), 
        na.rm = T))*100
ME1 = mean(erreur1, na.rm = T)

TABLEAU1 = cbind(RMSE1, MAPE1, ME1)
colnames(TABLEAU1) =
  c('RMSE', 'MAPE', 'ME')
TABLEAU1

RMSE2 =
  (mean(erreur2^2, na.rm = T))^0.5
MAPE2 = 
  (mean(abs(erreur2/demande_valid), 
        na.rm = T))*100
ME2 = 
  mean(erreur2, na.rm = T)

TABLEAU2 = cbind(RMSE2, MAPE2, ME2)
colnames(TABLEAU2) = 
  c('RMSE', 'MAPE', 'ME')
TABLEAU2


#diebold mariano pour departager les methodes

dm.test((prediction_final1-demande_valid), 
        (prediction_final2-demande_valid)) 

########################################################
########################################################
########################################################


# MODELE ARX

# CONSTITUTION DATAFRAME

df_29$Jour = ifelse(df_29$Lundi==1,1,
             ifelse(df_29$Mardi==1,2,
             ifelse(df_29$Mercredi==1,3,
             ifelse(df_29$Jeudi==1,4,
             ifelse(df_29$Vendredi==1,5,
             ifelse(df_29$Samedi==1,6,7))))))

toto1 = df_29[,c(1,2,4,37,43,44,46)]
toto1$DATE = toto1$date
toto2 = meteo_29[,c(1,9,11,13)]
toto2$DATE = as.Date(toto2$DATE)

# enlever les mois!

toto11 = toto1[,-c(3)]

######################################################

library(dynlm)

#######################################################

# ARX MODELE 1 -  ENTRAINEMENT UNIQUE


df_arx = merge(x= toto11, y= toto2,
               by= "DATE", 
               all.x= T)
#df_arx$month = as.factor(df_arx$month)
df_arx$Jour = as.factor(df_arx$Jour)
df_arx$Jour_Ferie = 
  as.factor(df_arx$Jour_Ferie)
df_arx$Jour_Ferie_Veille = 
  as.factor(df_arx$Jour_Ferie_Veille)
df_arx$Jour_Ferie_Lendemain = 
  as.factor(df_arx$Jour_Ferie_Lendemain)
df_arx = df_arx[-c(2)]
df_arx$lag1HDD = as.numeric(w.hddlag1)
df_arx$lag1CDD = as.numeric(w.cddlag1)
df_arx$lag2HDD = as.numeric(w.hddlag2)
df_arx$lag2CDD = as.numeric(w.cddlag2)

df_arx$CDD[2193:3653] = 
  as.numeric(w.cdd.bruit[2193:3653])
df_arx$HDD[2193:3653] = 
  as.numeric(w.hdd.bruit[2193:3653])
df_arx$CP[2193:3653] = 
  as.numeric(w.cp.bruit[2193:3653])
df_arx = df_arx[-c(1)]


df_arx_Train = df_arx[1:2192,]
df_arx_Valid = df_arx[2193:2922,]
df_arx_Test = df_arx[2923:nrow(df_arx),]

df_init = 
  as.zoo(ts(rbind(df_arx_Train,
                  df_arx_Valid)))


model_arx = 
  dynlm(formula = LES ~ . + 
   L(LES,1),
   data=df_init[1:(nrow(df_arx_Train)),])
print(model_arx)

acf2(model_arx$residuals)
plot(model_arx$residuals)
qqnorm(model_arx$residuals, 
       ylab="Residuals", 
       xlab="Normal Scores", 
       main="QQ Plot") 
qqline(model_arx$residuals)


Y = NULL
Y = 
  as.numeric(lag(
    timeSeries(df_arx_Valid$LES),1))
Y[1] = 
  df_arx_Train$LES[nrow(df_arx_Train)]
Y


df_arx_Valid$month = 
  as.numeric(df_arx_Valid$month)
df_arx_Valid$Jour_Ferie = 
  as.numeric(df_arx_Valid$Jour_Ferie)
df_arx_Valid$Jour_Ferie_Veille = 
  as.numeric(df_arx_Valid$Jour_Ferie_Veille)
df_arx_Valid$Jour_Ferie_Lendemain = 
  as.numeric(df_arx_Valid$Jour_Ferie_Lendemain)
df_arx_Valid$Jour =
  as.numeric(df_arx_Valid$Jour)

x0 = cbind(rep(1,nrow(df_arx_Valid)), 
           df_arx_Valid[,-1], Y)

pred0 = as.matrix(x0)%*%coef(model_arx)

erreur = pred0 - df_arx_Valid$LES


RMSE = (mean(erreur^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur/demande_valid), 
             na.rm = T))*100
ME = mean(erreur, na.rm = T)

TABLEAU = cbind(RMSE, MAPE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE', 'ME')
TABLEAU

pred11 = pred0



##############################################

# ENTRAINEMENT A CHAQUE PAS DE TEMPS 

##############################################


pred_exp = list()
erreur_exp = list()
pred_roll = list()
erreur_roll = list()
model_exp = vector("list",
    length = nrow(df_arx_Valid))
model_roll = vector("list",
     length = nrow(df_arx_Valid))


  

for (i in seq(1,nrow(df_arx_Valid))){
  
  df_arx_re = merge(x= toto11, y= toto2, 
                    by= "DATE", all.x= T)
  #df_arx_re$month = 
  #  as.factor(df_arx_re$month)
  df_arx_re$Jour = 
    as.factor(df_arx_re$Jour)
  df_arx_re$Jour_Ferie = 
    as.factor(df_arx_re$Jour_Ferie)
  df_arx_re$Jour_Ferie_Veille = 
    as.factor(df_arx_re$Jour_Ferie_Veille)
  df_arx_re$Jour_Ferie_Lendemain = 
    as.factor(df_arx_re$Jour_Ferie_Lendemain)
  df_arx_re = df_arx_re[-c(2)]
  df_arx_re$lag1HDD = as.numeric(w.hddlag1)
  df_arx_re$lag1CDD = as.numeric(w.cddlag1)
  df_arx_re$lag2HDD = as.numeric(w.hddlag2)
  df_arx_re$lag2CDD = as.numeric(w.cddlag2)
  
  df_arx_re$CDD[(2192+i):3653] = 
    as.numeric(w.cdd.bruit[(2192+i):3653])
  df_arx_re$HDD[(2192+i):3653] = 
    as.numeric(w.hdd.bruit[(2192+i):3653])
  df_arx_re$CP[(2192+i):3653] = 
    as.numeric(w.cp.bruit[(2192+i):3653])
  df_arx_re = df_arx_re[-c(1)]
  
  df_arx_Train_re = 
    df_arx_re[1:(2192+(i-1)),]
  df_arx_Valid_re = 
    df_arx_re[(2192+i):2922,]
  df_arx_Test_re = 
    df_arx_re[2923:nrow(df_arx_re),]
  
  df_re = 
    as.zoo(ts(rbind(df_arx_Train_re,
                    df_arx_Valid_re)))
  
  Y = NULL
  Y = 
    as.numeric(lag(
      timeSeries(df_arx_Valid_re$LES),1))
  Y[1] =
    df_arx_Train_re$LES[nrow(df_arx_Train_re)]
  Y
  
  #df_arx_Valid_re$month = 
  #  as.numeric(df_arx_Valid_re$month)
  df_arx_Valid_re$Jour_Ferie =
    as.numeric(df_arx_Valid_re$Jour_Ferie)
  df_arx_Valid_re$Jour_Ferie_Veille = 
    as.numeric(df_arx_Valid_re$Jour_Ferie_Veille)
  df_arx_Valid_re$Jour_Ferie_Lendemain = 
    as.numeric(df_arx_Valid_re$Jour_Ferie_Lendemain)
  df_arx_Valid_re$Jour = 
    as.numeric(df_arx_Valid_re$Jour)
  
  x00 = 
    cbind(rep(1,nrow(df_arx_Valid_re)),
          df_arx_Valid_re[,-1], Y)
  
  
  model_exp[[i]] =
    dynlm(formula = LES ~ . + L(LES,1),
        data=df_re[1:(nrow(df_arx_Train)+i-1),])
  
  pred_exp[[i]] = 
    as.matrix(x00[1,])%*%coef(model_exp[[i]])
  
  erreur_exp[[i]] = 
    pred_exp[[i]] - df_arx_Valid$LES[i]
  
  model_roll[[i]] = 
    dynlm(formula = LES ~ . + L(LES,1),
         data=df_re[(1+(i-1)):
                      (nrow(df_arx_Train)+(i-1)),])
  
  pred_roll[[i]] = 
    as.matrix(x00[1,])%*%coef(model_roll[[i]])
  
  erreur_roll[[i]] = 
    pred_roll[[i]] - df_arx_Valid$LES[i]
  
  print(paste("   Step:", i,
              "   Expanding model pred:",
              pred_exp[[i]],
              "   Rolling model pred:",
              pred_roll[[i]],
              "   Real observation:",
              df_arx_Valid$LES[i]))
  
  
}

pred_exp = unlist(pred_exp)
erreur_exp = unlist(erreur_exp)

RMSE_exp = 
  (mean(erreur_exp^2, na.rm = T))^0.5
MAPE_exp = 
  (mean(abs(erreur_exp/df_arx_Valid_re$LES),
        na.rm = T))*100
ME_exp = 
  mean(erreur_exp, na.rm = T)

TABLEAU_exp = 
  cbind(RMSE_exp, MAPE_exp, ME_exp)
colnames(TABLEAU_exp) = 
  c('RMSE', 'MAPE', 'ME')

TABLEAU_exp

pred_roll = unlist(pred_roll)
erreur_roll = unlist(erreur_roll)

RMSE_roll = 
  (mean(erreur_roll^2, na.rm = T))^0.5
MAPE_roll = 
  (mean(abs(erreur_roll/df_arx_Valid$LES), 
        na.rm = T))*100
ME_roll =
  mean(erreur_roll, na.rm = T)

TABLEAU_roll = 
  cbind(RMSE_roll, MAPE_roll, ME_roll)
colnames(TABLEAU_roll) = 
  c('RMSE', 'MAPE', 'ME')

TABLEAU_roll

pred12 = pred_exp
pred13 = pred_roll


############################################

# ARX MODELE 2 - ENTRAINEMENT UNIQUE

############################################
toto11 = toto1[,-c(3)]

df_arx = merge(x= toto11, y= toto2,
               by= "DATE", all.x= T)
#df_arx$month = as.factor(df_arx$month)
df_arx$Jour = as.factor(df_arx$Jour)
df_arx$Jour_Ferie = 
  as.factor(df_arx$Jour_Ferie)
df_arx$Jour_Ferie_Veille = 
  as.factor(df_arx$Jour_Ferie_Veille)
df_arx$Jour_Ferie_Lendemain = 
  as.factor(df_arx$Jour_Ferie_Lendemain)
df_arx = df_arx[-c(2)]
df_arx$lag1HDD = as.numeric(w.hddlag1)
df_arx$lag1CDD = as.numeric(w.cddlag1)
df_arx$lag2HDD = as.numeric(w.hddlag2)
df_arx$lag2CDD = as.numeric(w.cddlag2)

df_arx$CDD[2193:3653] = 
  as.numeric(w.cdd.bruit[2193:3653])
df_arx$HDD[2193:3653] = 
  as.numeric(w.hdd.bruit[2193:3653])
df_arx$CP[2193:3653] = 
  as.numeric(w.cp.bruit[2193:3653])
df_arx = df_arx[-c(1)]


df_arx_Train = df_arx[1:2192,]
df_arx_Valid = df_arx[2193:2922,]
df_arx_Test = df_arx[2923:nrow(df_arx),]

df_init = 
  as.zoo(ts(
    rbind(df_arx_Train,df_arx_Valid)))


model_arx_7 = 
  dynlm(formula = LES ~ . +
          L(LES,1) + L(LES,2)
     + L(LES,3) + L(LES,4) +
       L(LES,5)
     + L(LES,6) + L(LES,7),
    data=df_init[1:(nrow(df_arx_Train)),])

print(model_arx_7)
acf2(model_arx_7$residuals)
plot(model_arx_7$residuals, 
     ylab="R??sidus du mod??le", 
     xlab="Temps")
qqnorm(model_arx_7$residuals, 
       ylab="Residuals", 
       xlab="Normal Scores", 
       main="QQ Plot") 
qqline(model_arx_7$residuals)



Y = NULL
Y = 
 as.numeric(lag(
   timeSeries(df_arx_Valid$LES),1))
Y[1] = 
  df_arx_Train$LES[nrow(df_arx_Train)]


Y2 = NULL
Y2 = 
  as.numeric(lag(timeSeries(
    df_arx_Valid$LES),2))
Y2[1] =
  df_arx_Train$LES[nrow(
    df_arx_Train)-1]
Y2[2] = 
  df_arx_Train$LES[nrow(
    df_arx_Train)]


Y3 = NULL
Y3 = as.numeric(lag(
  timeSeries(df_arx_Valid$LES),3))
Y3[1] = 
  df_arx_Train$LES[nrow(df_arx_Train)-2]
Y3[2] =
  df_arx_Train$LES[nrow(df_arx_Train)-1]
Y3[3] = 
  df_arx_Train$LES[nrow(df_arx_Train)]

Y4 = NULL
Y4 = 
  as.numeric(lag(
    timeSeries(df_arx_Valid$LES),4))
Y4[1] = 
  df_arx_Train$LES[nrow(df_arx_Train)-3]
Y4[2] = 
  df_arx_Train$LES[nrow(df_arx_Train)-2]
Y4[3] = 
  df_arx_Train$LES[nrow(df_arx_Train)-1]
Y4[4] = 
  df_arx_Train$LES[nrow(df_arx_Train)]

Y5 = NULL
Y5 = 
  as.numeric(lag(
    timeSeries(df_arx_Valid$LES),5))
Y5[1] = 
  df_arx_Train$LES[nrow(df_arx_Train)-4]
Y5[2] = 
  df_arx_Train$LES[nrow(df_arx_Train)-3]
Y5[3] =
  df_arx_Train$LES[nrow(df_arx_Train)-2]
Y5[4] = 
  df_arx_Train$LES[nrow(df_arx_Train)-1]
Y5[5] = 
  df_arx_Train$LES[nrow(df_arx_Train)]

Y6 = NULL
Y6 = 
  as.numeric(lag(
    timeSeries(df_arx_Valid$LES),6))
Y6[1] = 
  df_arx_Train$LES[nrow(df_arx_Train)-5]
Y6[2] = 
  df_arx_Train$LES[nrow(df_arx_Train)-4]
Y6[3] = 
  df_arx_Train$LES[nrow(df_arx_Train)-3]
Y6[4] = 
  df_arx_Train$LES[nrow(df_arx_Train)-2]
Y6[5] = 
  df_arx_Train$LES[nrow(df_arx_Train)-1]
Y6[6] =
  df_arx_Train$LES[nrow(df_arx_Train)]

Y7 = NULL
Y7 = 
  as.numeric(lag(
    timeSeries(df_arx_Valid$LES),7))
Y7[1] = 
  df_arx_Train$LES[nrow(df_arx_Train)-6]
Y7[2] = 
  df_arx_Train$LES[nrow(df_arx_Train)-5]
Y7[3] = 
  df_arx_Train$LES[nrow(df_arx_Train)-4]
Y7[4] = 
  df_arx_Train$LES[nrow(df_arx_Train)-3]
Y7[5] = 
  df_arx_Train$LES[nrow(df_arx_Train)-2]
Y7[6] =
  df_arx_Train$LES[nrow(df_arx_Train)-1]
Y7[7] =
  df_arx_Train$LES[nrow(df_arx_Train)]


#df_arx_Valid$month =
#  as.numeric(df_arx_Valid$month)
df_arx_Valid$Jour_Ferie = 
  as.numeric(df_arx_Valid$Jour_Ferie)
df_arx_Valid$Jour_Ferie_Veille = 
  as.numeric(
    df_arx_Valid$Jour_Ferie_Veille)
df_arx_Valid$Jour_Ferie_Lendemain = 
  as.numeric(
    df_arx_Valid$Jour_Ferie_Lendemain)
df_arx_Valid$Jour = 
  as.numeric(df_arx_Valid$Jour)

x0 = 
  cbind(rep(1,nrow(df_arx_Valid)), 
        df_arx_Valid[,-1], Y, Y2,
        Y3, Y4, Y5, Y6, Y7)

pred0 = 
  as.matrix(x0)%*%coef(model_arx_7)

se0 = 
  summary(model_arx_7)$sigma*sqrt(
    as.matrix(x0)%*%summary(
      model_arx_7)$cov.unscaled%*%t(x0)+1)
se0 = se0[,1]
alpha = 0.05
pred0.lb = 
  pred0 - qt(1-alpha/2, 
     summary(model_arx_7)$df[2])*se0
pred0.ub = 
  pred0 + qt(1-alpha/2, 
     summary(model_arx_7)$df[2])*se0

erreur = pred0 - df_arx_Valid$LES


RMSE = 
  (mean(erreur^2, na.rm = T))^0.5
MAPE = 
  (mean(abs(erreur/demande_valid), 
        na.rm = T))*100
ME = mean(erreur, na.rm = T)

TABLEAU = cbind(RMSE, MAPE, ME)
colnames(TABLEAU) = 
  c('RMSE', 'MAPE', 'ME')
TABLEAU

pred31 = pred0

#Intervalle de prevision
erreur_95=NULL
for(i in 1:730)
{
  erreur_95[i] = 
    ifelse(demande_valid[i]<=pred0.lb[i] 
           ||  
      demande_valid[i]>=pred0.ub[i],0,1)
}

sum(erreur_95, na.rm = T)/730
sum(erreur_95, na.rm = T)

#graphique

pred0 = ts(pred0, 
                       start=time(yt)[2193])
pred0.lb = ts(pred0.lb, 
                 start=time(yt)[2193])
pred0.ub = ts(pred0.ub, 
                 start=time(yt)[2193])

plot(window(pred0, 
            start=time(yt)[2800]),
     col="red", 
     ylim=c(250,740), lwd = 2, 
     xlab="Septembre-D??cembre 2018",
     main="Pr??vision du pic de 
     la demande avec ARX7",
     ylab="Pic de la demande [MWh]")
lines(window(yt.valid, start=time(yt)[2800],
             end=time(yt)[3200]),lwd=2)
lines(window(pred0.lb, start=time(yt)[2800]),
      col="blue", type="l", lty=2)
lines(window(pred0.ub, start=time(yt)[2800]),
      col="blue", type="l", lty=2)
legend("topright", 
       legend=c("predictions","observations",
                "binf","bsup"),
       col=c("red","black","blue","blue"),
       lty=c(1,1,2,2))

################################################

# ENTRAINEMENT A CHAQUE PAS DE TEMPS 

################################################



pred_exp = list()
erreur_exp = list()
pred_roll = list()
erreur_roll = list()
model_exp = vector("list",
      length = nrow(df_arx_Valid))
model_roll = vector("list",
      length = nrow(df_arx_Valid))




for (i in seq(1,nrow(df_arx_Valid))){
  
  df_arx_re = merge(x= toto11, y= toto2, 
                    by= "DATE", all.x= T)
  #df_arx_re$month = 
  #  as.factor(df_arx_re$month)
  df_arx_re$Jour = 
    as.factor(df_arx_re$Jour)
  df_arx_re$Jour_Ferie = 
    as.factor(df_arx_re$Jour_Ferie)
  df_arx_re$Jour_Ferie_Veille = 
    as.factor(df_arx_re$Jour_Ferie_Veille)
  df_arx_re$Jour_Ferie_Lendemain = 
    as.factor(df_arx_re$Jour_Ferie_Lendemain)
  df_arx_re = df_arx_re[-c(2)]
  df_arx_re$lag1HDD = as.numeric(w.hddlag1)
  df_arx_re$lag1CDD = as.numeric(w.cddlag1)
  df_arx_re$lag2HDD = as.numeric(w.hddlag2)
  df_arx_re$lag2CDD = as.numeric(w.cddlag2)
  
  df_arx_re$CDD[(2192+i):3653] = 
    as.numeric(w.cdd.bruit[(2192+i):3653])
  df_arx_re$HDD[(2192+i):3653] = 
    as.numeric(w.hdd.bruit[(2192+i):
                             3653])
  df_arx_re$CP[(2192+i):3653] = 
    as.numeric(w.cp.bruit[(2192+i):3653])
  df_arx_re = df_arx_re[-c(1)]
  
  df_arx_Train_re = 
    df_arx_re[1:(2192+(i-1)),]
  df_arx_Valid_re = 
    df_arx_re[(2192+i):2922,]
  df_arx_Test_re = 
    df_arx_re[2923:nrow(df_arx_re),]
  
  df_re = 
    as.zoo(ts(rbind(
      df_arx_Train_re,
      df_arx_Valid_re)))
  
  Y = NULL
  Y = as.numeric(lag(
    timeSeries(df_arx_Valid_re$LES),1))
  Y[1] = 
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)]
  
  
  Y2 = NULL
  Y2 = 
    as.numeric(lag(
      timeSeries(df_arx_Valid_re$LES),2))
  Y2[1] = 
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)-1]
  Y2[2] =
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)]
  
  
  Y3 = NULL
  Y3 = 
    as.numeric(lag(
      timeSeries(df_arx_Valid_re$LES),3))
  Y3[1] = 
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)-2]
  Y3[2] =
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)-1]
  Y3[3] = 
    df_arx_Train_re$LES[nrow(
    df_arx_Train_re)]
  
  Y4 = NULL
  Y4 = 
    as.numeric(lag(
      timeSeries(df_arx_Valid_re$LES),4))
  Y4[1] = 
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)-3]
  Y4[2] =
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)-2]
  Y4[3] =
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)-1]
  Y4[4] = 
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)]
  
  Y5 = NULL
  Y5 = 
    as.numeric(lag(
      timeSeries(df_arx_Valid_re$LES),5))
  Y5[1] = 
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)-4]
  Y5[2] = 
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)-3]
  Y5[3] = 
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)-2]
  Y5[4] = 
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)-1]
  Y5[5] = 
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)]
  
  Y6 = NULL
  Y6 = 
    as.numeric(lag(timeSeries(df_arx_Valid_re$LES),6))
  Y6[1] = 
    df_arx_Train_re$LES[nrow(df_arx_Train_re)-5]
  Y6[2] = 
    df_arx_Train_re$LES[nrow(df_arx_Train_re)-4]
  Y6[3] = 
    df_arx_Train_re$LES[nrow(df_arx_Train_re)-3]
  Y6[4] = 
    df_arx_Train_re$LES[nrow(df_arx_Train_re)-2]
  Y6[5] =
    df_arx_Train_re$LES[nrow(df_arx_Train_re)-1]
  Y6[6] =
    df_arx_Train_re$LES[nrow(df_arx_Train_re)]
  
  Y7 = NULL
  Y7 = 
    as.numeric(lag(
      timeSeries(df_arx_Valid_re$LES),7))
  Y7[1] =
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)-6]
  Y7[2] = 
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)-5]
  Y7[3] =
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)-4]
  Y7[4] = 
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)-3]
  Y7[5] = 
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)-2]
  Y7[6] =
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)-1]
  Y7[7] =
    df_arx_Train_re$LES[nrow(
      df_arx_Train_re)]
  
  #df_arx_Valid_re$month = 
  #  as.numeric(df_arx_Valid_re$month)
  df_arx_Valid_re$Jour_Ferie = 
    as.numeric(df_arx_Valid_re$Jour_Ferie)
  df_arx_Valid_re$Jour_Ferie_Veille = 
    as.numeric(df_arx_Valid_re$Jour_Ferie_Veille)
  df_arx_Valid_re$Jour_Ferie_Lendemain = 
    as.numeric(df_arx_Valid_re$Jour_Ferie_Lendemain)
  df_arx_Valid_re$Jour =
    as.numeric(df_arx_Valid_re$Jour)
  
  x00 = 
    cbind(rep(1,nrow(df_arx_Valid_re)),
          df_arx_Valid_re[,-1], Y, Y2,
          Y3, Y4, Y5, Y6, Y7)
  
  
  model_exp[[i]] = 
    dynlm(formula = LES ~ . + 
            L(LES,1) + L(LES,2) + L(LES,3)
         + L(LES,4) + L(LES,5) + L(LES,6) + L(LES,7),
       data=df_re[1:(nrow(df_arx_Train)+i-1),])
  
  pred_exp[[i]] = 
    as.matrix(x00[1,])%*%coef(model_exp[[i]])
  
  erreur_exp[[i]] = 
    pred_exp[[i]] - df_arx_Valid$LES[i]
  
  model_roll[[i]] = 
    dynlm(formula = LES ~ . + L(LES,1) + 
            L(LES,2) +
            L(LES,3)
           + L(LES,4) + L(LES,5) + L(LES,6) +
            L(LES,7),
   data=df_re[(1+(i-1)):(nrow(
     df_arx_Train)+(i-1)),])
  
  pred_roll[[i]] = 
    as.matrix(x00[1,])%*%coef(
      model_roll[[i]])
  
  erreur_roll[[i]] = 
    pred_roll[[i]] - df_arx_Valid$LES[i]
  
  print(paste("   Step:", i,
              "   Expanding model pred:",
              pred_exp[[i]],
              "   Rolling model pred:",
              pred_roll[[i]],
              "   Real observation:",
              df_arx_Valid$LES[i]))
  
  
}

pred_exp = unlist(pred_exp)
erreur_exp = unlist(erreur_exp)

RMSE_exp = 
  (mean(erreur_exp^2, na.rm = T))^0.5
MAPE_exp = 
  (mean(abs(erreur_exp/df_arx_Valid_re$LES),
        na.rm = T))*100
ME_exp =
  mean(erreur_exp, na.rm = T)

TABLEAU_exp =
  cbind(RMSE_exp, MAPE_exp, ME_exp)
colnames(TABLEAU_exp) = 
  c('RMSE', 'MAPE', 'ME')

TABLEAU_exp

pred_roll = unlist(pred_roll)
erreur_roll = unlist(erreur_roll)

RMSE_roll = 
  (mean(erreur_roll^2, na.rm = T))^0.5
MAPE_roll = 
  (mean(abs(erreur_roll/df_arx_Valid$LES), 
        na.rm = T))*100
ME_roll = 
  mean(erreur_roll, na.rm = T)

TABLEAU_roll =
  cbind(RMSE_roll, MAPE_roll, ME_roll)
colnames(TABLEAU_roll) =
  c('RMSE', 'MAPE', 'ME')

TABLEAU_roll

pred32 = pred_exp
pred33 = pred_roll

##############################################

# GRAPHIQUE COMPARAISON
# SARIMA VS ARX7

plot(window(pred0, 
            start=time(yt)[2800]),
     col="red", 
     ylim=c(330,690), lwd = 2, 
     xlab="Septembre-D??cembre 2018",
     main="Pr??vision du pic de 
     la demande avec ARX7",
     ylab="Pic de la demande [MWh]")
lines(window(yt.valid, start=time(yt)[2800],
             end=time(yt)[3200]),lwd=2)
lines(window(prediction_final1, start=time(yt)[2800]),
      col="blue", lwd = 2)

legend("topright", 
       legend=c("SARIMA","ARX7",
                "Observations"),
       col=c("blue","red","black"),
       lty=c(1,1,1))

plot(pred0,
     col="red", 
     ylim=c(330,690), 
     xlab="Septembre-D??cembre 2018",
     main="Pr??vision du pic de 
     la demande avec ARX7",
     ylab="Pic de la demande [MWh]")
lines(yt.valid)
lines(prediction_final1,
      col="blue")

legend("topright", 
       legend=c("SARIMA","ARX7",
                "Observations"),
       col=c("blue","red","black"),
       lty=c(1,1,1))


############################################### 

# MEILLEUR MODELE DE REGRESSION
# AUX ERREURS AR(1)
# SUR ECHANTILLON TEST

fit6 = auto.arima(demande_train, xreg=cbind(
  w.hdd_train, w.cdd_train, lag(w.hdd_train, 1),
  lag(w.cdd_train, 1), lag(w.hdd_train,2), 
  lag(w.cdd_train,2),
  w.cp_train, df_29_Train$Lundi, 
  df_29_Train$Mardi, df_29_Train$Mercredi,
  df_29_Train$Jeudi,  
  df_29_Train$Samedi, df_29_Train$Dimanche, 
  df_29_Train$Jour_Ferie, 
  df_29_Train$Jour_Ferie_Veille, 
  df_29_Train$Jour_Ferie_Lendemain, 
  df_29_Train$Janvier, df_29_Train$Fevrier,
  df_29_Train$Mars, df_29_Train$Avril,
  df_29_Train$May, df_29_Train$Juin, 
  df_29_Train$Juillet, df_29_Train$Aout, 
  df_29_Train$Septembre, df_29_Train$Octobre, 
  df_29_Train$Novembre))

print(fit6)

erreur = NULL
pred = NULL
binf = NULL
bsup = NULL

for(i in 1:nrow(w.hdd_test))
{
  pred[i] <- predict(fit6, n.ahead=1, 
       newxreg = cbind(w.hdd.bruit_test[i],
         w.cdd.bruit_test[i], 
         lag(w.hdd_test, 1)[i],
         lag(w.cdd_test, 1)[i],
         lag(w.hdd_test,2)[i],
         lag(w.cdd_test,2)[i], 
         w.cp.bruit_test[i],
         df_29_Test$Lundi[i], 
         df_29_Test$Mardi[i], 
         df_29_Test$Mercredi[i],
         df_29_Test$Jeudi[i],
        df_29_Test$Samedi[i], 
        df_29_Test$Dimanche[i], 
        df_29_Test$Jour_Ferie[i], 
        df_29_Test$Jour_Ferie_Veille[i],
          df_29_Test$Jour_Ferie_Lendemain[i],
        df_29_Test$Janvier[i], 
        df_29_Test$Fevrier[i], 
        df_29_Test$Mars[i],
           df_29_Test$Avril[i],
        df_29_Test$May[i],
        df_29_Test$Juin[i],
        df_29_Test$Juillet[i],
        df_29_Test$Aout[i],
            df_29_Test$Septembre[i], 
        df_29_Test$Octobre[i],
        df_29_Test$Novembre[i]))
  
  erreur[i] = 
    pred[[i]][1]-demande_test[i]
  
  binf [i] = 
    pred[[i]][1]-1.96*23.904
  bsup[i] = 
    pred[[i]][1]+1.96*23.904
  
}




MAPE_2019_1 = 
  (mean(abs(
    erreur[1:60]/demande_test[1:60]),
        na.rm = T))*100
MAPE_2019_2 =
  (mean(abs(
    erreur[61:120]/demande_test[61:120]), 
        na.rm = T))*100
MAPE_2019_3 =
  (mean(abs(
    erreur[121:180]/demande_test[121:180]),
        na.rm = T))*100
MAPE_2019_4 =
  (mean(abs(
    erreur[181:240]/demande_test[181:240]),
        na.rm = T))*100
MAPE_2019_5 = 
  (mean(abs(
    erreur[241:300]/demande_test[241:300]),
        na.rm = T))*100
MAPE_2019_6 = 
  (mean(abs(
    erreur[301:365]/demande_test[301:365]),
        na.rm = T))*100

MAPE_2020_1 = 
  (mean(abs(
    erreur[366:425]/demande_test[366:425]),
        na.rm = T))*100
MAPE_2020_2 = 
  (mean(abs(
    erreur[426:485]/demande_test[426:485]),
        na.rm = T))*100
MAPE_2020_3 =
  (mean(abs(
    erreur[486:545]/demande_test[486:545]),
        na.rm = T))*100
MAPE_2020_4 = 
  (mean(abs(
    erreur[546:605]/demande_test[546:605]),
        na.rm = T))*100
MAPE_2020_5 = 
  (mean(abs(
    erreur[606:665]/demande_test[606:665]),
        na.rm = T))*100
MAPE_2020_6 = 
  (mean(abs(
    erreur[666:731]/demande_test[666:731]), 
        na.rm = T))*100

# MAPE PAR MOIS
# COMPARAISON 2019 2020

TABLEAU = matrix(c(data = MAPE_2019_1,
                   MAPE_2020_1, 
                   MAPE_2019_2,
                   MAPE_2020_2, 
                   MAPE_2019_3,
                   MAPE_2020_3,
                   MAPE_2019_4, 
                   MAPE_2020_4, 
                   MAPE_2019_5,
                   MAPE_2020_5, 
                   MAPE_2019_6,
                   MAPE_2020_6),2,6)
colnames(TABLEAU) = c('Jan-Fev', 
                      'Mars-Avril',
                      'Mai-Juin', 
                      'Juil-Aout', 
                      'Sept-Oct', 
                      'Nov-Dec')
rownames(TABLEAU) = c('2019', '2020')

TABLEAU

# MAPE 2019

RMSE = (mean(erreur[1:365]^2,
             na.rm = T))^0.5
MAPE = 
  (mean(abs(erreur[1:365]/
              demande_test[1:365]),
             na.rm = T))*100
MAE = mean(abs(erreur[1:365]), 
           na.rm = T)
ME = mean(erreur[1:365],
          na.rm = T)


TABLEAU = 
  cbind(RMSE, MAPE,MAE, ME)
rownames(TABLEAU) = c('2019')
colnames(TABLEAU) = 
  c('RMSE', 'MAPE','MAE', 'ME')
TABLEAU

#Intervalle de prevision 2019

erreur_19=NULL

for(i in 1:365)
{
  erreur_19[i] = 
    ifelse(demande_test[i]<=binf[i] 
           ||  
             demande_test[i]>=bsup[i],0,1)
}

sum(erreur_19, na.rm = T)/365
sum(erreur_19, na.rm = T)

# graphe

LOL = which(erreur_19[1:365]==0)
LOL_erreur = demande_test[LOL]
LOL_1 = NULL 
for (i in 1:365) {
  LOL_1[i] = 0
}

j=1
for (i in LOL) {
  LOL_1[i] = LOL_erreur[j]
  j= j+1
}

LOL_1 = ts(unlist(LOL_1), 
           start=time(yt)[2923])


prev_reg = 
  ts(unlist(pred),
     start=time(yt)[2923])
binf = 
  ts(unlist(binf),
     start=time(yt)[2923])
bsup = 
  ts(unlist(bsup), 
     start=time(yt)[2923])


plot(window(prev_reg,
            start=time(yt)[2923],
            end=time(yt)[3288]),
     col="red", 
     ylim=c(280,800), lwd = 2,
     xlab="2019",
     main="Pr??vision du pic de la 
     demande avec le mod??le de r??gression
     aux erreurs AR(1) sur le test",
     ylab="Pic de la demande [MWh]")
lines(window(yt.test, 
             start=time(yt)[2923],
             end=time(yt)[3288]),
      lwd=2)
lines(window(binf, start=time(yt)[2923], 
             end=time(yt)[3288]),
      col="blue", type="l", lty=2)
lines(window(bsup, 
             start=time(yt)[2923],
      end=time(yt)[3288]),
      col="blue", type="l", lty=2)
points(window(as.ts(LOL_1), 
              start=time(yt)[2923],
              end=time(yt)[3288]),
       pch=21, col = 'green',lwd=5)
legend("topright", 
       legend=c("predictions",
                "observations",
                "binf","bsup",
                "observations hors IP"),
       col=c("red","black",
             "blue","blue","green"), 
       lty=c(1,1,2,2,NA),pch=c(NA,NA,NA,NA,20))



# MAPE 2020

RMSE = (mean(erreur[366:731]^2,
             na.rm = T))^0.5
MAPE = 
  (mean(abs(erreur[366:731]/
              demande_test[366:731]),
        na.rm = T))*100
MAE = mean(abs(erreur[366:731]),
           na.rm = T)
ME = mean(erreur[366:731],
          na.rm = T)



TABLEAU = cbind(RMSE, MAPE,MAE, ME)
rownames(TABLEAU) = c('2020')
colnames(TABLEAU) = 
  c('RMSE', 'MAPE','MAE', 'ME')
TABLEAU



RMSE = (mean(erreur[426:516]^2, 
             na.rm = T))^0.5
MAPE = 
  (mean(abs(erreur[426:516]/
              demande_test[426:516]), 
             na.rm = T))*100
MAE = mean(abs(erreur[426:516]), 
           na.rm = T)
ME = mean(erreur[426:516], 
          na.rm = T)


TABLEAU = cbind(RMSE, MAPE,MAE, ME)
colnames(TABLEAU) =
  c('RMSE', 'MAPE','MAE', 'ME')
TABLEAU

# MAPE 2019-2020

RMSE = (mean(erreur^2,
             na.rm = T))^0.5
MAPE = 
  (mean(abs(erreur/
              demande_test),
        na.rm = T))*100
MAE = mean(abs(erreur),
           na.rm = T)
ME = mean(erreur,
          na.rm = T)



TABLEAU = cbind(RMSE, MAPE,MAE, ME)
rownames(TABLEAU) = c('2020')
colnames(TABLEAU) = 
  c('RMSE', 'MAPE','MAE', 'ME')
TABLEAU


