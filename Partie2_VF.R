################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

#################### PARTIE 2 PROJET ###########################################

################################################################################
################################################################################




#INITIALISATION

Sys.setlocale("LC_TIME", "C")
library(forecast)
setwd("~/HEC - MASTER/H2021/PREVISION/PROJET/PARTIE2")
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


#FORMATAGE DES DONNEES (PIC JOURNALIER DE LA DEMANDE)
dem.jour <- aggregate(LES ~ date, df,FUN = max)

#ENLEVER LE 1 JANVIER 2021
dem.jour = dem.jour[-c(3654),]

#DEFINIR MA TIMESERIES
sdate1  <- c(2011,1)


######################################################################
######################################################################

# BENCHMARK - NAIVE NO CHANGE

yt <- ts(dem.jour$LES, start=sdate1, frequency=365.25)
#yt <- ts(dem.jour$LES, start=sdate1, frequency=7)

naive1 <- naive(yt, h=1)

forecast <- window(naive1$fitted, start=time(yt)[2193], 
                   end=time(yt)[2922]) 
observed <- window(naive1$x, start=time(yt)[2193], 
                   end=time(yt)[2922])
bias1  <- mean(forecast-observed)
pbias1 <- mean((forecast-observed)/observed)*100
mape1  <- mean(abs((forecast-observed)/observed)*100)

cat("Using function <accuracy>:","\n")
print(accuracy(forecast, observed)[,1:5])

# B- NAIVE SAISONNIER

naiveS <- snaive(yt, h=1)

forecastS <- window(naiveS$fitted, start=time(yt)[2193], end=time(yt)[2922]) 
observedS <- window(naiveS$x, start=time(yt)[2193], end=time(yt)[2922]) 
biasS  <- mean(forecastS-observedS) 
pbiasS <- mean((forecastS-observedS)/observedS)*100
mapeS  <- mean(abs((forecastS-observedS)/observedS)*100)


# C- MOYENNE MOBILE 3 JOURS

naive3t <- zoo::rollmean(yt, 3, align="right")
naive3 <- naive(naive3t, h=1)

forecast3 <- window(naive3$fitted, start=time(yt)[2193], end=time(yt)[2922]) 
observed3 <- window(yt, start=time(yt)[2193], end=time(yt)[2922]) 
bias3  <- mean(forecast3-observed3) 
pbias3 <- mean((forecast3-observed3)/observed3)*100
mape3  <- mean(abs((forecast3-observed3)/observed3)*100)

# D- MOYENNE MOBILE 7 JOURS

naive7t <- zoo::rollmean(dem.jour.ts, 7, align="right")
naive7 <- naive(naive7t, h=1)

forecast7 <- window(naive7$fitted, start=time(yt)[2193], end=time(yt)[2922]) 
observed7 <- window(yt, start=time(yt)[2193], end=time(yt)[2922]) 
bias7  <- mean(forecast7-observed7) 
pbias7 <- mean((forecast7-observed7)/observed7)*100
mape7  <- mean(abs((forecast7-observed7)/observed7)*100)


cat("Using function <accuracy>:","\n")
print(accuracy(forecast, observed)[,1:5])
print(accuracy(forecastS, observedS)[,1:5])
print(accuracy(forecast3, observed3)[,1:5])
print(accuracy(forecast7, observed7)[,1:5])

######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################

#################### METHODES LISSAGE ################################

######################################################################
######################################################################

###################### LISSAGE SIMPLE ################################

yt <- ts(dem.jour$LES, start=sdate1)

# DIVISION ENTRE TRAIN VALID ET TEST POUR LES RAPPORTS SUIVANTS
yt.train = window(yt, start=time(yt)[1], end=time(yt)[2192])
plot(yt.train)
yt.valid = window(yt, start=time(yt)[2193], end=time(yt)[2922])
yt.test = window(yt, start=time(yt)[2923])

# 1 MODELE - EXPANDING WINDOW

sesopt1 = ses(yt.train, initial = "optimal")
cat("Optimal value of alpha:\n")
print(sesopt1$model$par)


prevses1 = NULL

for(i in 1:length(yt.valid)){
  trainses1 = window(yt, start= time(yt)[1] ,end= time(yt)[2192+(i-1)])
  prevses1[[i]] = forecast(trainses1, h=1, 
                           model = sesopt1, use.initial.values=T)$mean
}

#prevses1=ts(unlist(prevses1), start=time(yt)[2193],frequency=365.25)
prevses1=ts(unlist(prevses1), start=time(yt)[2193])

sesacc1 = accuracy(prevses1, yt.valid)
print(sesacc1)


# 2 MODELES (RE-TRAINED A 1 AN) - EXPANDING WINDOW


prevses1a = NULL
for(i in 1:365){
  trainses1a = window(yt, start= time(yt)[1] ,end= time(yt)[2192+(i-1)] )
  prevses1a[[i]] = forecast(trainses1a, h=1, 
                            model = sesopt1, use.initial.values=TRUE)$mean
}

modelses1b = ses(trainses1a)
print(modelses1b$model$par)

prevses1b = NULL
for(i in 1:365){
  trainses1b = window(yt, start= time(yt)[1] ,end= time(yt)[2557+(i-1)] )
  prevses1b[[i]] = forecast(trainses1b, h=1, 
                            model = modelses1b, use.initial.values=TRUE)$mean
}

toto=c(prevses1a, prevses1b)
prevses1bis=ts(unlist(toto), start=time(yt)[2193])

sesacc1bis = accuracy(prevses1bis, yt.valid)
print(sesacc1bis)


# 1 MODELE - ROLLING WINDOW


prevses2 = NULL
for(i in 1:length(yt.valid)){
  trainses2 = window(yt, start= time(yt)[i] ,end= time(yt)[2192+(i-1)] )
  prevses2[[i]] = forecast(trainses2, h=1, 
                           model = sesopt1, use.initial.values=TRUE)$mean
}

prevses2=ts(unlist(prevses2), start=time(yt)[2193])

sesacc2 = accuracy(prevses2, yt.valid)
print(sesacc2)

# 2 MODELES (RE-TRAINED A 1 AN) - ROLLING WINDOW


prevses3a = NULL
for(i in 1:365){
  trainses3a = window(yt, start= time(yt)[i] ,end= time(yt)[2192+(i-1)] )
  prevses3a[[i]] = forecast(trainses3a, h=1, 
                            model = sesopt1, use.initial.values=TRUE)$mean
}

modelses3b = ses(trainses3a)
print(modelses3b$model$par)

prevses3b = NULL
for(i in 1:365){
  trainses3b = window(yt, start= time(yt)[365+i] ,end= time(yt)[2557+(i-1)] )
  prevses3b[[i]] = forecast(trainses3b, h=1, 
                            model = modelses3b, use.initial.values=TRUE)$mean
}

toto3=c(prevses3a, prevses3b)
prevses3bis=ts(unlist(toto3), start=time(yt)[2193])


hwacc3bis = accuracy(prevses3bis, yt.valid)
print(hwacc3bis)


dm.test((prevses3bis-yt.valid), (prevses1-yt.valid)) 



######################################################################
######################################################################

###################### LISSAGE HOLT ##################################

# 1 MODELE - EXPANDING WINDOW

holtopt1 = holt(yt.train, initial = "optimal")
cat("Optimal value of alpha/beta:\n")
print(holtopt1$model$par)


prevholt1 = NULL

for(i in 1:length(yt.valid)){
  trainholt1 = window(yt, start= time(yt)[1] ,end= time(yt)[2192+(i-1)] )
  prevholt1[[i]] = forecast(trainholt1, h=1, 
                            model = holtopt1, use.initial.values=T)$mean
}

prevholt1=ts(unlist(prevholt1), start=time(yt)[2193])

holtacc1 = accuracy(prevholt1, yt.valid)
print(holtacc1)

# 2 MODELES (RE-TRAINED A 1 AN) - EXPANDING WINDOW


prevholt1a = NULL
for(i in 1:365){
  trainholt1a = window(yt, start= time(yt)[1] ,end= time(yt)[2192+(i-1)] )
  prevholt1a[[i]] = forecast(trainholt1a, h=1, 
                             model = holtopt1, use.initial.values=TRUE)$mean
}

modelholt1b = holt(trainholt1a)
print(modelholt1b$model$par)

prevholt1b = NULL
for(i in 1:365){
  trainholt1b = window(yt, start= time(yt)[1] ,end= time(yt)[2557+(i-1)] )
  prevholt1b[[i]] = forecast(trainholt1b, h=1,
                             model = modelholt1b, use.initial.values=TRUE)$mean
}

toto=c(prevholt1a, prevholt1b)
prevholt1bis=ts(unlist(toto), start=time(yt)[2193])

holtacc1bis = accuracy(prevholt1bis, yt.valid)
print(holtacc1bis)

dm.test((prevholt1-yt.valid), (prevholt1bis-yt.valid)) 

# 1 MODELE - ROLLING WINDOW

prevholt2 = NULL
for(i in 1:length(yt.valid)){
  trainholt2 = window(yt, start= time(yt)[i] ,end= time(yt)[2192+(i-1)] )
  prevholt2[[i]] = forecast(trainholt2, h=1, 
                            model = holtopt1, use.initial.values=TRUE)$mean
}

prevholt2=ts(unlist(prevholt2), start=time(yt)[2193])

holtacc2 = accuracy(prevholt2, yt.valid)
print(holtacc2)

dm.test((prevholt1-yt.valid), (prevholt2-yt.valid)) 

# 2 MODELES (RE-TRAINED A 1 AN) - ROLLING WINDOW


prevholt3a = NULL
for(i in 1:365){
  trainholt3a = window(yt, start= time(yt)[i] ,end= time(yt)[2192+(i-1)] )
  prevholt3a[[i]] = forecast(trainholt3a, h=1, 
                             model = holtopt1, use.initial.values=TRUE)$mean
}

modelholt3b = holt(trainholt3a)
print(modelholt3b$model$par)

prevholt3b = NULL
for(i in 1:365){
  trainholt3b = window(yt, start= time(yt)[365+i] ,end= time(yt)[2557+(i-1)] )
  prevholt3b[[i]] = forecast(trainholt3b, h=1, 
                             model = modelholt3b, use.initial.values=TRUE)$mean
}

toto3=c(prevholt3a, prevholt3b)
prevholt3bis=ts(unlist(toto3), start=time(yt)[2193])


holtacc3bis = accuracy(prevholt3bis, yt.valid)
print(holtacc3bis)

dm.test((prevholt2-yt.valid), (prevholt3bis-yt.valid)) 
dm.test((prevholt1bis-yt.valid), (prevholt3bis-yt.valid)) 

dm.test((prevses3bis-yt.valid), (prevholt3bis-yt.valid)) 



######################################################################
######################################################################

###################### LISSAGE HOLT-WINTER ###########################

# CREATION DE L'OBJET TIMESERIES DE FREQUENCE 7 POUR HW
sdate1  <- c(2011,1)
yt <- ts(dem.jour$LES, start=sdate1, frequency=7)

# DIVISION ENTRE TRAIN VALID ET TEST POUR LES RAPPORTS SUIVANTS
yt.train = window(yt, start=time(yt)[1], end=time(yt)[2192], frequency=7)
yt.valid = window(yt, start=time(yt)[2193], end=time(yt)[2922], frequency=7)
yt.test = window(yt, start=time(yt)[2923], frequency=7)

# 1 MODELE ADDITIF - EXPANDING WINDOW

model1 = hw(yt.train)
#model1 = hw(yt.train, damped = T)
print(model1$model$par)

prev1 = NULL
binf1 = NULL
bsup1 = NULL

for(i in 1:length(yt.valid)){
  trainhw1 = window(yt, start= time(yt)[1] ,end= time(yt)[2192+(i-1)], 
                    frequency=7 )
  prev1[[i]] = forecast(trainhw1, h=1, 
                        model = model1, use.initial.values=TRUE)$mean
  binf1[[i]] = forecast(trainhw1, h=1, 
                        model=model1, use.initial.values=TRUE)$lower[2]
  bsup1[[i]] = forecast(trainhw1, h=1, 
                        model=model1, use.initial.values=TRUE)$upper[2]
}


prev1=ts(unlist(prev1), start=time(yt)[2193], frequency=7)
binf1=ts(unlist(binf1), start=time(yt)[2193], frequency=7)
bsup1=ts(unlist(bsup1), start=time(yt)[2193], frequency=7)

hwacc1 = accuracy(prev1, yt.valid)
print(hwacc1)

# INTERVALLE DE PREVISION

erreur95hw = NULL
for ( i in 1:length(yt.valid)) {
  erreur95hw[i] = ifelse((yt.valid[i] <= bsup1[i] && 
                            yt.valid[i] >= binf1[i]),0,1)
}

pcterreurhw = sum(erreur95hw)/730
pcterreurhw

# 2 MODELES ADDITIFS (RE-TRAINED A 1 AN) - EXPANDING WINDOW

prev1a = NULL
for(i in 1:365){
  trainhw1a = window(yt, start= time(yt)[1] ,end= time(yt)[2192+(i-1)] )
  prev1a[[i]] = forecast(trainhw1a, h=1, 
                         model = model1, use.initial.values=TRUE)$mean
}

model1b = hw(trainhw1a)
print(model1b$model$par)

prev1b = NULL
for(i in 1:365){
  trainhw1b = window(yt, start= time(yt)[1] ,end= time(yt)[2557+(i-1)] )
  prev1b[[i]] = forecast(trainhw1b, h=1, 
                         model = model1b, use.initial.values=TRUE)$mean
}

toto=c(prev1a, prev1b)
prev1bis=ts(unlist(toto), start=time(yt)[2193], frequency=7)

hwacc1bis = accuracy(prev1bis, yt.valid)
print(hwacc1bis)

dm.test((prev1-yt.valid), (prev1bis-yt.valid)) 

# 1 MODELE ADDITIF - ROLLING WINDOW


prev3 = NULL
for(i in 1:length(yt.valid)){
  trainhw3 = window(yt, start= time(yt)[i] ,end= time(yt)[2192+(i-1)] )
  prev3[[i]] = forecast(trainhw3, h=1, 
                        model = model1, use.initial.values=TRUE)$mean
}

prev3=ts(unlist(prev3), start=time(yt)[2193], frequency=7)

hwacc3 = accuracy(prev3, yt.valid)
print(hwacc3)


# 2 MODELES ADDITIFS (RE-TRAINED A 1 AN) - ROLLING WINDOW


prev3a = NULL
for(i in 1:365){
  trainhw3a = window(yt, start= time(yt)[i] ,end= time(yt)[2192+(i-1)] )
  prev3a[[i]] = forecast(trainhw3a, h=1,
                         model = model1, use.initial.values=TRUE)$mean
}

model3b = hw(trainhw3a)
print(model3b$model$par)

prev3b = NULL
for(i in 1:365){
  trainhw3b = window(yt, start= time(yt)[365+i] ,end= time(yt)[2557+(i-1)] )
  prev3b[[i]] = forecast(trainhw3b, h=1, 
                         model = model3b, use.initial.values=TRUE)$mean
}

toto3=c(prev3a, prev3b)
prev3bis=ts(unlist(toto3), start=time(yt)[2193], frequency=7)


hwacc3bis = accuracy(prev3bis, yt.valid)
print(hwacc3bis)

dm.test((prev3-yt.valid), (prev3bis-yt.valid)) 
dm.test((prev3-yt.valid), (prev1-yt.valid)) 


######################################################################
######################################################################

###################### LISSAGE ETS ###################################

sdate1  <- c(2011,1)
yt <- ts(dem.jour$LES, start=sdate1,frequency=7)

# DIVISION ENTRE TRAIN VALID ET TEST POUR LES RAPPORTS SUIVANTS
yt.train = window(yt, start=time(yt)[1], end=time(yt)[2192],frequency=7)
yt.valid = window(yt, start=time(yt)[2193], end=time(yt)[2922],frequency=7)
yt.test = window(yt, start=time(yt)[2923],frequency=7)

# 1 MODELE - EXPANDING WINDOW

modelets1 <- ets(yt.train)
#modelets1 <- ets(yt.train, allow.multiplicative.trend = T)
# les deux modeles obtenus sont les memes avec/sans multiplicative trend

prev_ets1 = NULL
binf2 = NULL
bsup2 = NULL

for(i in 1:length(yt.valid)){
  train_ets1 = window(yt, start= time(yt)[1] ,end= time(yt)[2192+(i-1)], 
                      frequency=7 )
  prev_ets1[[i]]= forecast(train_ets1, h=1,
                           model = modelets1, use.initial.values = T )$mean
  binf2[[i]] = forecast(train_ets1, h=1, 
                        model=modelets1, use.initial.values=TRUE)$lower[2]
  bsup2[[i]] = forecast(train_ets1, h=1, 
                        model=modelets1, use.initial.values=TRUE)$upper[2]
}

prev_ets1=ts(unlist(prev_ets1), start=time(yt)[2193], frequency=7)
binf2=ts(unlist(binf2), start=time(yt)[2193], frequency=7)
bsup2=ts(unlist(bsup2), start=time(yt)[2193], frequency=7)

etsacc1 = accuracy(prev_ets1, yt.valid)
print(etsacc1)

# INTERVALLE DE PREVISION

erreur95ets = NULL
for ( i in 1:length(yt.valid)) {
  erreur95ets[i] = ifelse((yt.valid[i] <= bsup2[i] && 
                             yt.valid[i] >= binf2[i]),0,1)
}

pcterreurets = sum(erreur95ets)/730
pcterreurets

# 2 MODELES (RE-TRAINED A 1 AN) - EXPANDING WINDOW

#modelets1 <- ets(yt.train)

prev_ets1a = NULL

for(i in 1:length(365)){
  train_ets1a = window(yt, start= time(yt)[1] ,end= time(yt)[2192+(i-1)], 
                       frequency=7 )
  prev_ets1a[[i]]= forecast(train_ets1a, h=1, 
                            model = modelets1, use.initial.values = T)$mean
}

modelets1b = ets(train_ets1a)
prev_ets1b = NULL

for(i in 1:365){
  train_ets1b = window(yt, start= time(yt)[1] ,end= time(yt)[2557+(i-1)], 
                       frequency=7)
  prev_ets1b[[i]]= forecast(train_ets1b, h=1,
                            model = modelets1b, use.initial.values = T)$mean
}  

totoets=c(prev_ets1a, prev_ets1b)
prev_ets1bis=ts(unlist(totoets), start=time(yt)[2193], frequency=7)

etsacc1bis = accuracy(prev_ets1bis, yt.valid)
print(etsacc1bis)

# 1 MODELE - ROLLING WINDOW

prev_ets2 = NULL

for(i in 1:length(yt.valid)){
  train_ets2 = window(yt, start= time(yt)[i] ,end= time(yt)[2192+(i-1)],
                      frequency=7 )
  prev_ets2[[i]]= forecast(train_ets2, h=1, 
                           model = modelets1, use.initial.values = T)$mean
}


prev_ets2=ts(unlist(prev_ets2), start=time(yt)[2193], frequency=7)

etsacc2 = accuracy(prev_ets2, yt.valid)
print(etsacc2)

# 2 MODELES (RE-TRAINED A 1 AN) - ROLLING WINDOW

prev_ets2a = NULL

for(i in 1:365){
  train_ets2a = window(yt, start= time(yt)[i] ,end= time(yt)[2192+(i-1)], 
                       frequency=7 )
  prev_ets2a[[i]]= forecast(train_ets2a, h=1, 
                            model = modelets1, use.initial.values = T)$mean
}

modelets2b = ets(train_ets2a)
prev_ets2b = NULL

for(i in 1:365){
  train_ets2b = window(yt, start= time(yt)[365+i] ,end= time(yt)[2557+(i-1)], 
                       frequency=7 )
  prev_ets2b[[i]]= forecast(train_ets2b, h=1, 
                            model = modelets2b, use.initial.values = T)$mean
}  

totoets2=c(prev_ets2a, prev_ets2b)
prev_ets2bis=ts(unlist(totoets2), start=time(yt)[2193], frequency=7)

etsacc2bis = accuracy(prev_ets2bis, yt.valid)
print(etsacc2bis)

dm.test((prev_ets2-yt.valid), (prev_ets2bis-yt.valid)) 
dm.test((prev1-yt.valid), (prev_ets1-yt.valid)) 

######################################################################
######################################################################

###################### LISSAGE DSHW ##################################

sdate1  <- c(2011,1)
yt <- ts(dem.jour$LES, start=sdate1)

# DIVISION ENTRE TRAIN VALID ET TEST POUR LES RAPPORTS SUIVANTS
yt.train = window(yt, start=time(yt)[1], end=time(yt)[2192])
yt.valid = window(yt, start=time(yt)[2193], end=time(yt)[2922])
yt.test = window(yt, start=time(yt)[2923])

###################### msts(7,364) ##################################

# 1 MODELE - EXPANDING WINDOW

train_dshw = msts(yt.train, seasonal.periods = c(7, 364))
dshw_model = dshw(train_dshw)
#dshw_model = dshw(train_dshw, lambda = "auto")

prev_dswh = NULL

for(i in 1:length(yt.valid)){
  train_dshwbis = window(yt, start= time(yt)[1] ,
                         end= time(yt)[2192+(i-1)] )
  train_dshwbis = msts(train_dshwbis,seasonal.periods = c(7, 364))
  prev_dswh[[i]] = dshw(train_dshwbis, h=1, model = dshw_model)$mean
}

prev_dswh=ts(unlist(prev_dswh), start=time(yt)[2193])
dshwacc = accuracy(prev_dswh, yt.valid)
print(dshwacc)

# 2 MODELES (RE-TRAINED A 1 AN) - EXPANDING WINDOW


prevdshwa = NULL
for(i in 1:365){
  traindshwa = window(yt, start= time(yt)[1] ,end= time(yt)[2192+(i-1)] )
  traindshwa = msts(traindshwa ,seasonal.periods = c(7, 364))
  prevdshwa[[i]] = dshw(traindshwa, h=1, model = dshw_model)$mean
}

train_dshwb = msts(traindshwa, seasonal.periods = c(7, 364))
dshw_modelb = dshw(train_dshwb)
#dshw_modelb = dshw(train_dshwb, lambda = "auto")

prevdshwb = NULL
for(i in 1:365){
  traindshwb = window(yt, start= time(yt)[1] ,end= time(yt)[2557+(i-1)] )
  traindshwb = msts(traindshwb ,seasonal.periods = c(7, 364))
  prevdshwb[[i]] = dshw(traindshwb, h=1, model = dshw_modelb)$mean
}

totodshw=c(prevdshwa, prevdshwb)
prevdshwbis=ts(unlist(totodshw), start=time(yt)[2193])


dshwaccbis = accuracy(prevdshwbis, yt.valid)
print(dshwaccbis)

dm.test((prev_dswh-yt.valid), (prevdshwbis-yt.valid)) 

# 1 MODELE - ROLLING WINDOW

prev_dswh1 = NULL

for(i in 1:length(yt.valid)){
  train_dshwbis1 = window(yt, start= time(yt)[i] ,end= time(yt)[2192+(i-1)] )
  train_dshwbis1 = msts(train_dshwbis1,seasonal.periods = c(7, 364))
  prev_dswh1[[i]] = dshw(train_dshwbis1, h=1, model = dshw_model)$mean
}

prev_dswh1=ts(unlist(prev_dswh1), start=time(yt)[2193])
dshwacc1 = accuracy(prev_dswh1, yt.valid)
print(dshwacc1)

dm.test((prev_dswh-yt.valid), (prev_dswh1-yt.valid)) 


# 2 MODELES (RE-TRAINED A 1 AN) - ROLLING WINDOW


prevdshwaa = NULL
for(i in 1:365){
  traindshwaa = window(yt, start= time(yt)[i] ,end= time(yt)[2192+(i-1)] )
  traindshwaa = msts(traindshwaa ,seasonal.periods = c(7, 364))
  prevdshwaa[[i]] = dshw(traindshwaa, h=1, model = dshw_model)$mean
}

train_dshwbb = msts(traindshwaa, seasonal.periods = c(7, 364))
dshw_modelbb = dshw(train_dshwbb)
#dshw_modelbb = dshw(train_dshwbb, lambda = "auto")

prevdshwbb = NULL
for(i in 1:365){
  traindshwbb = window(yt, start= time(yt)[365+i] ,end= time(yt)[2557+(i-1)] )
  traindshwbb = msts(traindshwbb ,seasonal.periods = c(7, 364))
  prevdshwbb[[i]] = dshw(traindshwbb, h=1, model = dshw_modelbb)$mean
}

totoo=c(prevdshwaa, prevdshwbb)
prevvdshw=ts(unlist(totoo), start=time(yt)[2193])


dshwaccbis1 = accuracy(prevvdshw, yt.valid)
print(dshwaccbis1)

dm.test((prev_dswh1-yt.valid), (prevvdshw-yt.valid)) 



######################################################################
######################################################################

###################### LISSAGE TBATS #################################

###################### msts(7,365.25) ################################


# 1 MODELE - EXPANDING WINDOW

train_tbats = msts(yt.train, seasonal.periods = c(7, 365.25))
tbats_mod = tbats(train_tbats)

prev_tbats = NULL
binf = NULL
bsup = NULL

for(i in 1:length(yt.valid)){
  
  train_tbatsbis = window(yt, start= time(yt)[1] ,end= time(yt)[2192+(i-1)] )
  train_tbatsbis = msts(train_tbatsbis, seasonal.periods = c(7, 365.25))
  prev_tbats[[i]] = forecast(train_tbatsbis, h=1, 
                             model=tbats_mod, use.initial.value=T)$mean
  binf[[i]] = forecast(train_tbatsbis, h=1, model=tbats_mod)$lower[2]
  bsup[[i]] = forecast(train_tbatsbis, h=1, model=tbats_mod)$upper[2]
}


prev_tbats=ts(unlist(prev_tbats), start=time(yt)[2193])
binf=ts(unlist(binf), start=time(yt)[2193])
bsup=ts(unlist(bsup), start=time(yt)[2193])

tbatsacc = accuracy(prev_tbats, yt.valid)
print(tbatsacc)

as.character(tbats_mod)

# INTERVALLE DE PREVISION

erreur95tbats = NULL
for ( i in 1:length(yt.valid)) {
  erreur95tbats[i] = ifelse((yt.valid[i] <= bsup[i] &&
                               yt.valid[i] >= binf[i]),0,1)
}

pcterreurtbats = sum(erreur95tbats)/730
pcterreurtbats


# 1 MODELE - ROLLING WINDOW

prev_tbats1 = NULL

for(i in 1:length(yt.valid)){
  
  train_tbatsbis1 = window(yt, start= time(yt)[i] ,end= time(yt)[2192+(i-1)])
  train_tbatsbis1 = msts(train_tbatsbis1, seasonal.periods = c(7, 365.25))
  prev_tbats1[[i]] = forecast(train_tbatsbis1, h=1, model=tbats_mod)$mean
}


prev_tbats1=ts(unlist(prev_tbats1), start=time(yt)[2193])

tbatsacc1 = accuracy(prev_tbats1, yt.valid)
print(tbatsacc1)



#plot(window(yt.valid,start=time(yt)[2871]),col="black", ylim=c(310,700), ylab="Pic de la demande [MWh]", xlab="Septembre-D??cembre 2018",
#     main="Comparaison des pr??visions du pic journalier de la demande sur l'??chantillon de validation")
#lines(window(forecast, start=time(yt)[2871]), col="pink")
#lines(window(prev_tbats, start=time(yt)[2871]), col="cyan")
#lines(window(prev1, start=time(yt)[2871]), col="red")
#legend("topright", 
#       legend=c("Observations","Benchmark Naive","TBATS","Holt-Winter"),
#       col=c("black","pink", "cyan","red"), lty=1)

plot(window(prev_tbats,start=time(yt)[2800],end=time(yt)[3200]),col="red", 
     ylim=c(300,720), lwd = 2, xlab="Septembre-D??cembre 2018",
     main="Pr??vision du pic de la demande avec la m??thode TBATS avec intervalle de pr??vision ?? 95%",ylab="Pic de la demande [MWh]")
lines(window(yt.valid, start=time(yt)[2800],end=time(yt)[3200]),lwd=2)
lines(window(binf, start=time(yt)[2800],end=time(yt)[3200]),col="blue", type="l", lty=2)
lines(window(bsup, start=time(yt)[2800],end=time(yt)[3200]),col="blue", type="l", lty=2)
legend("topright", 
       legend=c("predictions","observations","binf","bsup"),
       col=c("red","black","blue","blue"), lty=c(1,1,2,2))



######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################

#################### METHODES REGRESSION #############################

######################################################################
######################################################################

library(astsa)
library(timeSeries)

# CREATION DES VARIABLES EXPLICATIVES

df$day = as.numeric(format(df$date, "%d"))
df$month = as.numeric(format(df$date, "%m"))
df$year = as.numeric(format(df$date, "%Y"))
df$date_m_d=format(df$date, format = "%m-%d")
df$weekday = weekdays(date)
df$Lundi = ifelse(df$weekday == 'Monday', 1, 0 )
df$Mardi = ifelse(df$weekday == 'Tuesday', 1, 0 )
df$Mercredi = ifelse(df$weekday == 'Wednesday', 1, 0 )
df$Jeudi = ifelse(df$weekday == 'Thursday', 1, 0 )
df$Vendredi = ifelse(df$weekday == 'Friday', 1, 0 )
df$Samedi = ifelse(df$weekday == 'Saturday', 1, 0 )
df$Dimanche = ifelse(df$weekday == 'Sunday', 1, 0 )
df$weekdaybin = ifelse(df$weekday == "Saturday" | df$weekday == "Sunday" , 1, 0)
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
df$Nouvel_an = ifelse(df$date_m_d == '01-01', 1, 
                      ifelse(df$Lundi==1 & df$date_m_d == '01-02',1,0))
df$Fete_National = ifelse(df$date_m_d == '07-04', 1, 0)
df$Veteran_Day = ifelse(df$date_m_d == '11-11', 1, 0)
df$Noel = ifelse(df$date_m_d == '12-25', 1, 0)

#Martin Luther King Day
df$Martin_Luther_day = 0 
for (i in 2011:2020) {
  df$Martin_Luther_day[which(df$month==1 & 
                               df$Lundi==1 & df$year == i)[c(49:72)]] = 1
}

#President day
df$President_day = 0 
for (i in 2011:2020) {
  df$President_day[which(df$month==1 & 
                           df$Lundi==1 & df$year == i)[c(49:72)]] = 1
}

#Arbor day

df$Arbor_day = 0 
Longueur = NULL

for (i in 2011:2020) {
  Longueur = length(which(df$month==4& df$Vendredi==1 & df$year == i))  
  df$Arbor_day[which(df$month==4& df$Vendredi==1 & 
                       df$year == i)[c((Longueur-23):Longueur)]] = 1
}

#Memorial day

df$Memorial_day = 0 
Longueur = NULL

for (i in 2011:2020) {
  Longueur = length(which(df$month==1 & df$Lundi==1 & df$year == i))  
  df$Memorial_day[which(df$month==1 & df$Lundi==1 & 
                          df$year == i)[c((Longueur-23):Longueur)]] = 1
}

#Labor day 

df$Labor_day = 0 
for (i in 2011:2020) {
  df$Labor_day[which(df$month==9& df$Lundi==1 & df$year == i)[c(1:24)]] = 1
}

#Columbus day 

df$Columbus_day = 0 
for (i in 2011:2020) {
  df$Columbus_day[which(df$month==10& df$Lundi==1 & 
                          df$year == i)[c(25:48)]] = 1
}


#Thanks Giving

df$Thanks_Giving = 0 
for (i in 2011:2020) {
  df$Thanks_Giving[which(df$month==11 & df$Jeudi==1 & 
                           df$year == i)[c(73:96)]] = 1
}


df$Jour_Ferie = ifelse(df$Nouvel_an == 1 | df$Fete_National == 1 |
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

df$season =  ifelse (df$date_m_d >= WS | df$date_m_d < SE, 1,
                     ifelse (df$date_m_d >= SE & df$date_m_d < SS, 2,
                             ifelse (df$date_m_d >= SS & df$date_m_d < FE, 3, 4)))
df$Winter = ifelse(df$season == 1, 1, 0)
df$Spring = ifelse(df$season == 2, 1, 0)
df$Summer = ifelse(df$season == 3, 1, 0)
df$Fall = ifelse(df$season == 4, 1, 0)

#df$hour = substr(df$datecst,12,13)

#FORMATAGE DES DONNEES (PIC JOURNALIER)

df_29 <-aggregate(.~date, df[,-c(6,7)], max)

df_29$Jour_Ferie_Veille = NULL

for (i in 1:nrow(df_29)) {
  df_29$Jour_Ferie_Veille[i] = ifelse(df_29$Jour_Ferie[i+1] == 1, 1, 0)
}

df_29$Jour_Ferie_Lendemain = NULL
df_29$Jour_Ferie_Lendemain[1] = 0
for (i in 2:nrow(df_29)) {
  df_29$Jour_Ferie_Lendemain[i] = ifelse(df_29$Jour_Ferie[i-1] == 1, 1, 0)
}

# Enlever les 29 fevrier 
df_28= df_29[-which(df_29$day==29 & df_29$month==2),] 

#ENLEVER LE 1 JANVIER 2021
df_29 = df_29[-c(3654),]



##Diviser train, valid et test 
df_29_Train = df_29[1:2192,]
df_29_Train2 = df_29[1:2557,]
df_29_Valid = df_29[2193:2922,]
df_29_Valid2 = df_29[2558:2922,]
df_29_Test = df_29[2923:nrow(df_29),]

# METEO / TEMPERATURE

ventmoy = read.csv("ventmoy.csv", header=TRUE)
tempete = read.csv("tempete.csv", header=TRUE)

meteo=read.csv("weather.csv",header=TRUE)


meteo$DATE = as.character(meteo$DATE)

#meteo= meteo[-c(424, 1882, 3340), ] 

summary(meteo)

# IMPUTATION 

meteo[is.na(meteo$TMAX),4] = meteo[which(is.na(meteo$TMAX)) - 1,][,4]
meteo[is.na(meteo$TMAX),4] = meteo[which(is.na(meteo$TMAX)) - 1,][,4]
meteo[is.na(meteo$TMIN),5] = meteo[which(is.na(meteo$TMIN)) - 1,][,5]
meteo[is.na(meteo$TMIN),5] = meteo[which(is.na(meteo$TMIN)) - 1,][,5]


imp1 <- c("USW00094995", "2011-03-18", 0.00,70,35)    
imp2 <- c("USW00094995", "2012-04-06", 0.01,67,40)  
imp3 <- c("USW00094995", "2013-05-08", 0.01,74,39)
imp4 <- c("USW00094995", "2015-06-04", 0.54,79,62)  
imp5 <- c("USW00094995", "2016-06-17", 0.00,95,65)  
imp6 <- c("USW00094995", "2017-06-20", 0.00,85,59)  
imp7 <- c("USW00094995", "2018-08-26", 0.00,92,58)  


meteo_29 <- rbind(meteo[1:76, ], imp1, meteo[77:459, ], 
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
meteo_29$mean.temp = (meteo_29$t.max.c+meteo_29$t.min.c)/2

#Bruit pour temp????rature et vent

bruit.temp = rnorm(nrow(meteo_29),0,0.5)
meteo_29$mean.temp.bruit = meteo_29$mean.temp + bruit.temp

bruit.wind = rnorm(nrow(meteo_29),0,1.5)
meteo_29$Wind.bruit = meteo_29$Wind + bruit.wind

#Suppression de la variable station, TMAX, TMIN, t.max.c et t.min.c
meteo_29 <- subset(meteo_29, select = -c(STATION,TMAX,TMIN))

#Creation HDD et CDD
meteo_29$CDD = ifelse(meteo_29$mean.temp >=14 ,meteo_29$mean.temp-14 , 0)
meteo_29$CDD.bruit = ifelse(meteo_29$mean.temp.bruit >=14 ,
                            meteo_29$mean.temp.bruit-14 , 0)

meteo_29$HDD = ifelse(meteo_29$mean.temp < 10 ,10-meteo_29$mean.temp , 0)
meteo_29$HDD.bruit = ifelse(meteo_29$mean.temp.bruit < 10 ,
                            10-meteo_29$mean.temp.bruit , 0)

#Creation de Cp 
meteo_29$CP = ifelse(meteo_29$mean.temp < 12 ,
                     (meteo_29$Wind)^0.5*(12 - meteo_29$mean.temp) , 0)
meteo_29$CP.bruit = ifelse(meteo_29$mean.temp.bruit < 12 ,
                           (meteo_29$Wind.bruit)^0.5*(12 - meteo_29$mean.temp.bruit) , 0)

#ENLEVER LE 1 JANVIER 2021
meteo_29 = meteo_29[-c(3654),]
meteo_28 = meteo_29[-which(df_29$day==29 & df_29$month==2),] 




#Tornado

Tornado = tempete[which(tempete$EVENT_TYPE == 'Tornado'),]

Date_tornado = unique(Tornado$BEGIN_DATE)

df_29$Tornado = ifelse(df_29$date %in% as.Date(Date_tornado, 
                                               format = "%m/%d/%Y"),1,0)

##Diviser train, valid et test 
df_29_Train = df_29[1:2192,]
df_29_Valid = df_29[2193:2922,]
df_29_Test = df_29[2923:nrow(df_29),]

# Creation objets time series 

w.prcp   <- timeSeries(meteo_29$PRCP, meteo_29$DATE, 
                       format="%Y-%m-%d")
w.temp <- timeSeries(meteo_29$mean.temp, meteo_29$DATE, 
                     format="%Y-%m-%d")
w.tempmax <- timeSeries(meteo_29$t.max.c, meteo_29$DATE,
                        format="%Y-%m-%d")
w.tempmin <- timeSeries(meteo_29$t.min.c, meteo_29$DATE,
                        format="%Y-%m-%d")
w.cdd <- timeSeries(meteo_29$CDD, meteo_29$DATE, 
                    format="%Y-%m-%d")
w.cdd_train <- window(w.cdd, start=time(w.cdd)[1], 
                      end=time(w.cdd)[2192])
w.cdd_train2 <- window(w.cdd, start=time(w.cdd)[1], 
                       end=time(w.cdd)[2557])
w.cdd_valid <- window(w.cdd, start=time(w.cdd)[2193], 
                      end=time(w.cdd)[2922])
w.cdd_valid2 <- window(w.cdd, start=time(w.cdd)[2558], 
                       end=time(w.cdd)[2922])
w.cdd_test  <- window(w.cdd, start=time(w.cdd)[2923], 
                      end=time(w.cdd)[nrow(w.cdd)])
w.cdd.bruit <- timeSeries(meteo_29$CDD.bruit, meteo_29$DATE,
                          format="%Y-%m-%d")
w.cdd.bruit_valid <- window(w.cdd.bruit, start=time(w.cdd.bruit)[2193], 
                            end=time(w.cdd.bruit)[2922])
w.cdd.bruit_test  <- window(w.cdd.bruit, start=time(w.cdd.bruit)[2923], 
                            end=time(w.cdd.bruit)[nrow(w.cdd.bruit)])


w.hdd <- timeSeries(meteo_29$HDD, meteo_29$DATE, 
                    format="%Y-%m-%d")
w.hdd_train <- window(w.hdd, start=time(w.hdd)[1],
                      end=time(w.hdd)[2192])
w.hdd_train2 <- window(w.hdd, start=time(w.hdd)[1], 
                       end=time(w.hdd)[2557])
w.hdd_valid <- window(w.hdd, start=time(w.hdd)[2193], 
                      end=time(w.hdd)[2922])
w.hdd_test  <- window(w.hdd, start=time(w.hdd)[2923], 
                      end=time(w.hdd)[nrow(w.hdd)])
w.hdd.bruit <- timeSeries(meteo_29$HDD.bruit, meteo_29$DATE,
                          format="%Y-%m-%d")
w.hdd.bruit_valid <- window(w.hdd.bruit, start=time(w.hdd.bruit)[2193], 
                            end=time(w.hdd.bruit)[2922])
w.hdd.bruit_test  <- window(w.hdd.bruit, start=time(w.hdd.bruit)[2923],
                            end=time(w.hdd.bruit)[nrow(w.hdd.bruit)])

w.wind <- timeSeries(meteo_29$Wind, meteo_29$DATE, format="%Y-%m-%d")

w.cp <- timeSeries(meteo_29$CP, meteo_29$DATE, 
                   format="%Y-%m-%d")
w.cp_train <- window(w.cp, start=time(w.cp)[1], 
                     end=time(w.cp)[2192])
w.cp_train2 <- window(w.cp, start=time(w.cp)[1], 
                      end=time(w.cp)[2557])
w.cp_valid <- window(w.cp, start=time(w.cp)[2193], 
                     end=time(w.cp)[2922])
w.cp_test <- window(w.cp, start=time(w.cp)[2923], 
                    end=time(w.cp)[nrow(w.cp)])
w.cp.bruit <- timeSeries(meteo_29$CP.bruit, meteo_29$DATE, 
                         format="%Y-%m-%d")
w.cp.bruit_valid <- window(w.cp.bruit, start=time(w.cp.bruit)[2193], 
                           end=time(w.cp.bruit)[2922])
w.cp.bruit_test <- window(w.cp.bruit, start=time(w.cp.bruit)[2923],
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

demande<- timeSeries(df_29$LES, df_29$date,
                     format="%Y-%m-%d")
demande_train<- timeSeries(df_29_Train$LES, df_29_Train$date,
                           format="%Y-%m-%d")
demande_train2<- timeSeries(df_29_Train2$LES, df_29_Train2$date,
                            format="%Y-%m-%d")
demande_valid <- timeSeries(df_29_Valid$LES, df_29_Valid$date, 
                            format="%Y-%m-%d")
demande_test <- timeSeries(df_29_Test$LES, df_29_Test$date, 
                           format="%Y-%m-%d")


plot(series(w.prcp), series(demande),
     ylab="Pic journalier de la demande a Lincoln (MW/h)",
     xlab="Precipitations moyennes journalieres (in)",
     main="Relation entre temperature et demande delectricite", 
     pch=23)


plot(series(w.temp), series(demande),
     ylab="Pic journalier de la demande a Lincoln (MW/h)",
     xlab="Temperature moyenne journaliere (Celsius)",
     main="Relation entre temperature et demande delectricite", 
     pch=23)

plot(series(w.cdd), series(demande),
     ylab="Pic journalier de la demande a Lincoln (MW/h)",
     xlab="CDD/HDD", pch=23,
     main="Relation entre CDD/HDD et demande delectricite",
     col="blue",xlim=c(0,30))
points(series(w.hdd), series(demande),pch=23,col="red")
legend(1,802, legend=c("CDD","HDD"),pch=23 ,col=c("blue", "red"))


plot(series(lag(w.hdd,1)), series(demande), 
     xlab="lag-1 HDDt",ylab="Pic journalier de la demande a Lincoln (MW/h)")

plot(series(lag(w.hdd,2)), series(demande), 
     xlab="lag-2 HDDt",ylab="Pic journalier de la demande a Lincoln (MW/h)",
     main="Relation entre le HDD deux fois differencie et demande d electricite")

plot(series(lag(w.cdd,1)), series(demande), 
     xlab="lag-1 CDDt",ylab="Pic journalier de la demande a Lincoln (MW/h)")

plot(series(lag(w.cdd,2)), series(demande), 
     xlab="lag-2 CDDt",ylab="Pic journalier de la demande a Lincoln (MW/h)",
     main="Relation entre le HDD deux fois differencie et demande d electricite")


plot(series(w.wind), series(demande), 
     xlab="Wind",ylab="Pic journalier de la demande a Lincoln (MW/h)",
     main="Relation entre le HDD deux fois differencie et demande d electricite")

plot(series(w.cp), series(demande), col="green", 
     xlab="Refroidissement ??olien CP",
     ylab="Pic journalier de la demande a Lincoln (MW/h)",
     main="Relation entre le CP et demande d electricite")

plot(demande_train)

lambda <- BoxCox.lambda(demande_train)

acf2(BoxCox(demande_train,lambda))

acf2(demande_train)


##############################################################################

#MODELE #1


fit <- auto.arima(demande_train, xreg=cbind(
  w.hdd_train, w.cdd_train,
  df_29_Train$Lundi, df_29_Train$Mardi, df_29_Train$Mercredi,
  df_29_Train$Jeudi, df_29_Train$Samedi, df_29_Train$Dimanche))
print(fit)
acf(residuals(fit),
    main="With proper error structure (using auto.arima)")


adjreg <- sarima(demande_train, 2,0,1, 
                 xreg=cbind(
                   w.hdd_train, w.cdd_train, 
                   df_29_Train$Lundi, df_29_Train$Mardi, 
                   df_29_Train$Mercredi, df_29_Train$Jeudi,
                   df_29_Train$Samedi, df_29_Train$Dimanche))

adjreg

erreur = NULL
pred = NULL
for(i in 1:nrow(w.hdd_valid))
{
  pred[i] <- predict(fit, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i], df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i]))
  
  erreur[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur^2))^0.5
MAPE = (mean(abs(erreur/demande_valid)))*100
ME = mean(erreur)

TABLEAU = cbind(RMSE, MAPE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE', 'ME')
TABLEAU


#MODELE #1.1

fit1 <- auto.arima(demande_train, xreg=cbind(
  w.hdd_train, w.cdd_train, lag(w.hdd_train, 1), lag(w.cdd_train, 1),
  lag(w.hdd_train,2), lag(w.cdd_train,2), 
  df_29_Train$Lundi, df_29_Train$Mardi, df_29_Train$Mercredi,
  df_29_Train$Jeudi, df_29_Train$Samedi, df_29_Train$Dimanche))
print(fit1)
acf(residuals(fit1)[-(1:2)],
    main="With proper error structure (using auto.arima)")


adjreg1 <- sarima(demande_train, 5,0,0, 
                  xreg=cbind(
                    w.hdd_train, w.cdd_train, lag(w.hdd_train, 1), 
                    lag(w.cdd_train, 1),
                    lag(w.hdd_train,2), lag(w.cdd_train,2), 
                    df_29_Train$Lundi, df_29_Train$Mardi, 
                    df_29_Train$Mercredi, 
                    df_29_Train$Jeudi, df_29_Train$Samedi, 
                    df_29_Train$Dimanche))

adjreg1

erreur = NULL
for(i in 1:nrow(w.hdd_valid))
{
  pred[i] <- predict(fit1, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i],
    lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i], df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i]))
  
  erreur[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur/demande_valid), na.rm = T))*100
ME = mean(erreur, na.rm = T)

TABLEAU = cbind(RMSE, MAPE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE', 'ME')
TABLEAU


#MODELE #2

fit2 <- auto.arima(demande_train, xreg=cbind(
  w.hdd_train, w.cdd_train, lag(w.hdd_train, 1), lag(w.cdd_train, 1), 
  lag(w.hdd_train,2), lag(w.cdd_train,2), 
  df_29_Train$Lundi, df_29_Train$Mardi, df_29_Train$Mercredi,
  df_29_Train$Jeudi,
  df_29_Train$Samedi, df_29_Train$Dimanche,
  df_29_Train$Janvier, df_29_Train$Fevrier, df_29_Train$Mars,
  df_29_Train$Avril,
  df_29_Train$May, df_29_Train$Juin, df_29_Train$Juillet, 
  df_29_Train$Aout, df_29_Train$Septembre, df_29_Train$Octobre,
  df_29_Train$Novembre))
fit2
acf(residuals(fit2)[-(1:2)],
    main="With proper error structure (using auto.arima)")


adjreg2 <- sarima(demande_train, 1,0,0, 
                  xreg=cbind(
                    w.hdd_train, w.cdd_train, lag(w.hdd_train, 1), 
                    lag(w.cdd_train, 1),
                    lag(w.hdd_train,2), lag(w.cdd_train,2), 
                    df_29_Train$Lundi, df_29_Train$Mardi, 
                    df_29_Train$Mercredi, df_29_Train$Jeudi,
                    df_29_Train$Samedi, df_29_Train$Dimanche,
                    df_29_Train$Janvier, df_29_Train$Fevrier, 
                    df_29_Train$Mars, df_29_Train$Avril,
                    df_29_Train$May, df_29_Train$Juin, df_29_Train$Juillet, 
                    df_29_Train$Aout, df_29_Train$Septembre, 
                    df_29_Train$Octobre, df_29_Train$Novembre))

adjreg2

erreur = NULL
for(i in 1:nrow(w.hdd_valid))
{
  pred[i] <- predict(fit2, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i], lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i], df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], 
    df_29_Valid$Mars[i], df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i], 
    df_29_Valid$Juillet[i], df_29_Valid$Aout[i], df_29_Valid$Septembre[i],
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i]))
  
  erreur[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur/demande_valid), na.rm = T))*100
ME = mean(erreur, na.rm = T)

TABLEAU = cbind(RMSE, MAPE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE', 'ME')
TABLEAU

#MODELE #3

fit3 <- auto.arima(demande_train, xreg=cbind(
  w.hdd_train, w.cdd_train, lag(w.hdd_train, 1), 
  lag(w.cdd_train, 1), lag(w.hdd_train,2),
  lag(w.cdd_train,2), 
  df_29_Train$Lundi, df_29_Train$Mardi, df_29_Train$Mercredi,
  df_29_Train$Jeudi, df_29_Train$Samedi, df_29_Train$Dimanche, 
  df_29_Train$Jour_Ferie))

print(fit3)

acf(residuals(fit3)[-(1:2)],
    main="With proper error structure (using auto.arima)")

adjreg3 <- sarima(demande_train, 5,0,0, 
                  xreg=cbind(
                    w.hdd_train, w.cdd_train, lag(w.hdd_train, 1), 
                    lag(w.cdd_train, 1),
                    lag(w.hdd_train,2), lag(w.cdd_train,2), 
                    df_29_Train$Lundi, df_29_Train$Mardi, 
                    df_29_Train$Mercredi, df_29_Train$Jeudi,
                    df_29_Train$Samedi, df_29_Train$Dimanche, 
                    df_29_Train$Jour_Ferie))

adjreg3

erreur = NULL
for(i in 1:nrow(w.hdd_valid))
{
  pred[i] <- predict(fit3, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i],
    lag(w.cdd_valid, 1)[i], lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i], df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i]))
  
  erreur[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur/demande_valid), na.rm = T))*100
MAE = mean(abs(erreur), na.rm = T)
ME = mean(erreur, na.rm = T)

TABLEAU = cbind(RMSE, MAPE,MAE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE','MAE', 'ME')
TABLEAU

#MODELE #4

fit4 <- auto.arima(demande_train, xreg=cbind(
  w.hdd_train, w.cdd_train, lag(w.hdd_train, 1), lag(w.cdd_train, 1), 
  lag(w.hdd_train,2), lag(w.cdd_train,2), 
  df_29_Train$Lundi, df_29_Train$Mardi, df_29_Train$Mercredi, 
  df_29_Train$Jeudi,
  df_29_Train$Samedi, df_29_Train$Dimanche, 
  df_29_Train$Noel, df_29_Train$Fete_National, df_29_Train$Labor_day, 
  df_29_Train$Thanks_Giving, df_29_Train$Nouvel_an))

print(fit4)
acf(residuals(fit4)[-(1:2)],
    main="With proper error structure (using auto.arima)")

adjreg4 <- sarima(demande_train, 5,0,0, 
                  xreg=cbind(
                    w.hdd_train, w.cdd_train, lag(w.hdd_train, 1), 
                    lag(w.cdd_train, 1),
                    lag(w.hdd_train,2), lag(w.cdd_train,2), 
                    df_29_Train$Lundi, df_29_Train$Mardi, 
                    df_29_Train$Mercredi, df_29_Train$Jeudi,
                    df_29_Train$Samedi, df_29_Train$Dimanche, 
                    df_29_Train$Noel, df_29_Train$Fete_National, 
                    df_29_Train$Labor_day, 
                    df_29_Train$Thanks_Giving, df_29_Train$Nouvel_an))


adjreg4

erreur = NULL
for(i in 1:nrow(w.hdd_valid))
{
  pred[i] <- predict(fit4, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i],
    lag(w.cdd_valid, 1)[i], lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i], 
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Noel[i], df_29_Valid$Fete_National[i], 
    df_29_Valid$Labor_day[i], 
    df_29_Valid$Thanks_Giving[i], df_29_Valid$Nouvel_an[i]))
  
  erreur[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur/demande_valid), na.rm = T))*100
ME = mean(erreur, na.rm = T)

TABLEAU = cbind(RMSE, MAPE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE', 'ME')
TABLEAU

#MODELE #4.1

fit4.1 <- auto.arima(demande_train, xreg=cbind(
  w.hdd_train, w.cdd_train, lag(w.hdd_train, 1), lag(w.cdd_train, 1), 
  lag(w.hdd_train,2), lag(w.cdd_train,2), 
  df_29_Train$Lundi, df_29_Train$Mardi, df_29_Train$Mercredi, 
  df_29_Train$Jeudi, df_29_Train$Samedi, df_29_Train$Dimanche, 
  df_29_Train$Jour_Ferie, df_29_Train$Janvier, df_29_Train$Fevrier,
  df_29_Train$Mars, df_29_Train$Avril, df_29_Train$May,
  df_29_Train$Juin, df_29_Train$Juillet, df_29_Train$Aout, 
  df_29_Train$Septembre, df_29_Train$Octobre, df_29_Train$Novembre))

print(fit4.1)
acf(residuals(fit4.1)[-(1:2)],
    main="With proper error structure (using auto.arima)")

adjreg4 <- sarima(demande_train, 1,0,0, 
                  xreg=cbind(
                    w.hdd_train, w.cdd_train, lag(w.hdd_train, 1),
                    lag(w.cdd_train, 1), lag(w.hdd_train,2),
                    lag(w.cdd_train,2), 
                    df_29_Train$Lundi, df_29_Train$Mardi, 
                    df_29_Train$Mercredi, df_29_Train$Jeudi,
                    df_29_Train$Samedi, df_29_Train$Dimanche, 
                    df_29_Train$Jour_Ferie, df_29_Train$Janvier,
                    df_29_Train$Fevrier, df_29_Train$Mars,
                    df_29_Train$Avril, df_29_Train$May,
                    df_29_Train$Juin, df_29_Train$Juillet, 
                    df_29_Train$Aout, df_29_Train$Septembre, 
                    df_29_Train$Octobre, df_29_Train$Novembre))


adjreg4

erreur = NULL
for(i in 1:nrow(w.hdd_valid))
{
  pred[i] <- predict(fit4.1, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i], lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i], 
    df_29_Valid$Jeudi[i], df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Janvier[i], 
    df_29_Valid$Fevrier[i], df_29_Valid$Mars[i], df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i],
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], df_29_Valid$Octobre[i],
    df_29_Valid$Novembre[i]))
  
  erreur[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur/demande_valid), na.rm = T))*100
ME = mean(erreur, na.rm = T)

TABLEAU = cbind(RMSE, MAPE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE', 'ME')
TABLEAU


#Modele 5
fit5 <- auto.arima(demande_train, xreg=cbind(
  w.hdd_train, w.cdd_train, lag(w.hdd_train, 1), lag(w.cdd_train, 1), 
  lag(w.hdd_train,2), lag(w.cdd_train,2), 
  df_29_Train$Lundi, df_29_Train$Mardi, df_29_Train$Mercredi, 
  df_29_Train$Jeudi, df_29_Train$Samedi, df_29_Train$Dimanche, 
  df_29_Train$Jour_Ferie, df_29_Train$Jour_Ferie_Veille, 
  df_29_Train$Jour_Ferie_Lendemain, df_29_Train$Janvier, 
  df_29_Train$Fevrier,
  df_29_Train$Mars, df_29_Train$Avril, df_29_Train$May, df_29_Train$Juin,
  df_29_Train$Juillet, df_29_Train$Aout, 
  df_29_Train$Septembre, df_29_Train$Octobre, df_29_Train$Novembre))

print(fit5)

acf(residuals(fit5)[-(1:2)],
    main="With proper error structure (using auto.arima)")

adjreg5 <- sarima(demande_train, 1,0,0, 
                  xreg=cbind(
                    w.hdd_train, w.cdd_train, lag(w.hdd_train, 1), 
                    lag(w.cdd_train, 1), lag(w.hdd_train,2), 
                    lag(w.cdd_train,2), 
                    df_29_Train$Lundi, df_29_Train$Mardi, 
                    df_29_Train$Mercredi, df_29_Train$Jeudi,
                    df_29_Train$Samedi, df_29_Train$Dimanche, 
                    df_29_Train$Jour_Ferie, df_29_Train$Jour_Ferie_Veille,
                    df_29_Train$Jour_Ferie_Lendemain,
                    df_29_Train$Janvier, df_29_Train$Fevrier, 
                    df_29_Train$Mars, df_29_Train$Avril, df_29_Train$May,
                    df_29_Train$Juin, df_29_Train$Juillet, 
                    df_29_Train$Aout, df_29_Train$Septembre, 
                    df_29_Train$Octobre,
                    df_29_Train$Novembre))


adjreg5

erreur = NULL
for(i in 1:nrow(w.hdd_valid))
{
  pred[i] <- predict(fit5, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i], lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i], df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i], 
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i],
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i], 
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i]))
  
  erreur[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur/demande_valid), na.rm = T))*100
ME = mean(erreur, na.rm = T)

TABLEAU = cbind(RMSE, MAPE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE', 'ME')
TABLEAU

#MODELE #6
fit6 <- auto.arima(demande_train, xreg=cbind(
  w.hdd_train, w.cdd_train, lag(w.hdd_train, 1), lag(w.cdd_train, 1),
  lag(w.hdd_train,2), lag(w.cdd_train,2),
  w.cp_train, df_29_Train$Lundi, df_29_Train$Mardi, df_29_Train$Mercredi, 
  df_29_Train$Jeudi,  
  df_29_Train$Samedi, df_29_Train$Dimanche, df_29_Train$Jour_Ferie, 
  df_29_Train$Jour_Ferie_Veille, 
  df_29_Train$Jour_Ferie_Lendemain, df_29_Train$Janvier, df_29_Train$Fevrier,
  df_29_Train$Mars, df_29_Train$Avril, df_29_Train$May, df_29_Train$Juin,
  df_29_Train$Juillet, df_29_Train$Aout, 
  df_29_Train$Septembre, df_29_Train$Octobre, df_29_Train$Novembre))

print(fit6)

acf(residuals(fit6)[-(1:2)],
    main="With proper error structure (1,0,0)", ylim = c(-0.1,0.4))
pacf(residuals(fit6)[-(1:2)],
     main="With proper error structure (1,0,0)")

adjreg6 <- sarima(demande_train, 1,0,0, 
                  xreg=cbind(
                    w.hdd_train, w.cdd_train, lag(w.hdd_train, 1), 
                    lag(w.cdd_train, 1), lag(w.hdd_train,2), 
                    lag(w.cdd_train,2),
                    w.cp_train,df_29_Train$Lundi, df_29_Train$Mardi, 
                    df_29_Train$Mercredi, df_29_Train$Jeudi,
                    df_29_Train$Samedi, df_29_Train$Dimanche, 
                    df_29_Train$Jour_Ferie, df_29_Train$Jour_Ferie_Veille,
                    df_29_Train$Jour_Ferie_Lendemain, df_29_Train$Janvier, 
                    df_29_Train$Fevrier,
                    df_29_Train$Mars, df_29_Train$Avril, df_29_Train$May, 
                    df_29_Train$Juin, df_29_Train$Juillet, df_29_Train$Aout, 
                    df_29_Train$Septembre, df_29_Train$Octobre, 
                    df_29_Train$Novembre))
adjreg6

erreur = NULL
binf = NULL
bsup = NULL

for(i in 1:nrow(w.hdd_valid))
{
  pred[i] <- predict(fit6, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i],
    lag(w.cdd_valid, 1)[i], lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i], 
    w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i], df_29_Valid$Jour_Ferie[i],
    df_29_Valid$Jour_Ferie_Veille[i],
    df_29_Valid$Jour_Ferie_Lendemain[i], df_29_Valid$Janvier[i], 
    df_29_Valid$Fevrier[i], df_29_Valid$Mars[i],
    df_29_Valid$Avril[i],df_29_Valid$May[i], df_29_Valid$Juin[i],
    df_29_Valid$Juillet[i], df_29_Valid$Aout[i],
    df_29_Valid$Septembre[i], df_29_Valid$Octobre[i], 
    df_29_Valid$Novembre[i]))
  
  erreur[i] = pred[[i]][1]-demande_valid[i]
  binf [i] = pred[[i]][1]-1.96*23.904
  bsup[i] = pred[[i]][1]+1.96*23.904
  
}

RMSE = (mean(erreur^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur/demande_valid), na.rm = T))*100
MAE = mean(abs(erreur), na.rm = T)
ME = mean(erreur, na.rm = T)

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################




## figure pred vs obs

prev_reg=ts(unlist(pred), start=time(yt)[2193])
binf=ts(unlist(binf), start=time(yt)[2193])
bsup=ts(unlist(bsup), start=time(yt)[2193])

plot(window(prev_reg,start=time(yt)[2800]),col="red", 
     ylim=c(300,720), lwd = 2, xlab="Septembre-D??cembre 2018",
     main="Pr??vision du pic de la demande avec le mod??le de r??gression aux erreurs AR(1)",ylab="Pic de la demande [MWh]")
lines(window(yt.valid, start=time(yt)[2800],end=time(yt)[3200]),lwd=2)
lines(window(binf, start=time(yt)[2800]),col="blue", type="l", lty=2)
lines(window(bsup, start=time(yt)[2800]),col="blue", type="l", lty=2)
legend("topright", 
       legend=c("predictions","observations","binf","bsup"),
       col=c("red","black","blue","blue"), lty=c(1,1,2,2))


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

TABLEAU = cbind(RMSE, MAPE,MAE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE','MAE', 'ME')
TABLEAU

#Analyse erreur

DATE_valid = as.character(seq(as.Date("2017-01-03"), 
                              as.Date("2018-12-31"), by="days"))

erreur_matrix = as.data.frame(cbind(DATE_valid, as.numeric(erreur[-(1:2)])))

erreur_matrix$Residus = as.numeric(as.character(erreur_matrix$V2))
erreur_matrix$Mois = as.factor(substr(DATE_valid,6,7))
erreur_matrix$Year = as.factor(substr(DATE_valid,1,4))
erreur_matrix$Jour_semaine = weekdays(as.Date(DATE_valid))

library(ggplot2)

ggplot(data=erreur_matrix, aes(x=Mois, y=Residus, fill=Mois)) + geom_boxplot() + ggtitle("r??sidus") + xlab("Mois de l'ann??e") + ylab("R??sidus") + scale_fill_discrete (name = "Mois", labels = c("Janvier","F??vrier","Mars","Avril","Mai","Juin","Juillet","Ao??t","Septembre","Octobre","Novembre","D??cembre"))


boxplot(Residus ~ Mois, data = erreur_matrix,
        main="Residus sur le pic journalier d'electricitr 
        a Lincoln NE selon le mois de l'annee")

boxplot(Residus ~ Year, data = erreur_matrix,
        main="Residus sur le pic journalier d'rlectricitr
        a Lincoln NE selon le mois de l'annee")

boxplot(Residus ~ Jour_semaine, data = erreur_matrix,
        main="Residus sur le pic journalier d'rlectricitr 
        a Lincoln NE selon le jour de la semaine")


#Intervalle de prevision
erreur_95=NULL
for(i in 1:nrow(w.hdd_valid))
{
  erreur_95[i] = ifelse(demande_valid[i]<=pred[[i]][1]+1.96*23.904 && 
                          demande_valid[i]>=pred[[i]][1]-1.96*23.904,0,1)
}

sum(erreur_95, na.rm = T)/nrow(w.hdd_valid)


# Prevision Sans bruit 
erreur = NULL
for(i in 1:nrow(w.hdd_valid))
{
  pred[i] <- predict(fit6, n.ahead=1, newxreg = cbind(
    w.hdd_valid[i], w.cdd_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i],
    lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i], w.cp_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i], 
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i],
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i],
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i]))
  
  erreur[i] = pred[[i]][1]-demande_valid[i]
  
}

RMSE = (mean(erreur^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur/demande_valid), na.rm = T))*100
ME = mean(erreur, na.rm = T)

TABLEAU = cbind(RMSE, MAPE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE', 'ME')
TABLEAU

#Methode #2 

erreur1 = NULL
for(i in 1:365)
{
  pred[i] <- predict(fit6, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i],
    lag(w.cdd_valid, 1)[i], lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i],
    w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i], 
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i], 
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i], 
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i],
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i]))
  
  erreur1[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur1^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur1/demande_valid), na.rm = T))*100
ME = mean(erreur1, na.rm = T)

TABLEAU2 = cbind(RMSE, MAPE, ME)
colnames(TABLEAU2) = c('RMSE', 'MAPE', 'ME')
TABLEAU2


fit6.1 <- arima(demande[1:2557], xreg=cbind(
  w.hdd[1:2557], w.cdd[1:2557], lag(w.hdd, 1)[1:2557], lag(w.cdd, 1)[1:2557],
  lag(w.hdd,2)[1:2557], lag(w.cdd,2)[1:2557], w.cp[1:2557],
  df_29$Lundi[1:2557], df_29$Mardi[1:2557], df_29$Mercredi[1:2557], 
  df_29$Jeudi[1:2557],
  df_29$Samedi[1:2557], df_29$Dimanche[1:2557], 
  df_29$Jour_Ferie[1:2557], df_29$Jour_Ferie_Veille[1:2557], 
  df_29$Jour_Ferie_Lendemain[1:2557],
  df_29$Janvier[1:2557], df_29$Fevrier[1:2557],
  df_29$Mars[1:2557], df_29$Avril[1:2557], df_29$May[1:2557], 
  df_29$Juin[1:2557],
  df_29$Juillet[1:2557], df_29$Aout[1:2557], 
  df_29$Septembre[1:2557], df_29$Octobre[1:2557], 
  df_29$Novembre[1:2557]), order = c(1,0,0))

print(fit6.1)

acf(residuals(fit6.1)[-(1:2)],
    main="With proper error structure (using auto.arima)")

adjreg6.1 <- sarima(demande[1:2557], 1,0,0, 
                    xreg=cbind(
                      w.hdd[1:2557], w.cdd[1:2557], lag(w.hdd, 1)[1:2557], 
                      lag(w.cdd, 1)[1:2557],
                      lag(w.hdd,2)[1:2557], lag(w.cdd,2)[1:2557], w.cp[1:2557],
                      df_29$Lundi[1:2557], df_29$Mardi[1:2557], 
                      df_29$Mercredi[1:2557], df_29$Jeudi[1:2557],
                      df_29$Samedi[1:2557], df_29$Dimanche[1:2557], 
                      df_29$Jour_Ferie[1:2557], 
                      df_29$Jour_Ferie_Veille[1:2557],
                      df_29$Jour_Ferie_Lendemain[1:2557],
                      df_29$Janvier[1:2557], df_29$Fevrier[1:2557],
                      df_29$Mars[1:2557], df_29$Avril[1:2557], 
                      df_29$May[1:2557], df_29$Juin[1:2557],
                      df_29$Juillet[1:2557], df_29$Aout[1:2557], 
                      df_29$Septembre[1:2557], df_29$Octobre[1:2557], 
                      df_29$Novembre[1:2557]))

adjreg6.1


for(i in 366:730)
{
  pred[i] <- predict(fit6.1, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i],
    lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i], w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i], 
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i],
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i], 
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i],
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i]))
  
  erreur1[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur1^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur1/demande_valid), na.rm = T))*100
MAE = mean(abs(erreur1), na.rm = T)
ME = mean(erreur1, na.rm = T)

TABLEAU = cbind(RMSE, MAPE,MAE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE','MAE', 'ME')
TABLEAU

#Methode #3

erreur2 = NULL
for(i in 1:183)
{
  pred[i] <- predict(fit6, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i],
    lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i], w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i], 
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i], 
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i], 
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i], 
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i]))
  
  erreur2[i] = pred[[i]][1]-demande_valid[i]
}

fit6.1 <- auto.arima(demande[1:2375], xreg=cbind(
  w.hdd[1:2375], w.cdd[1:2375], lag(w.hdd, 1)[1:2375], lag(w.cdd, 1)[1:2375], 
  lag(w.hdd,2)[1:2375],
  lag(w.cdd,2)[1:2375], w.cp[1:2375],
  df_29$Lundi[1:2375], df_29$Mardi[1:2375], df_29$Mercredi[1:2375], 
  df_29$Jeudi[1:2375],
  df_29$Samedi[1:2375], df_29$Dimanche[1:2375], 
  df_29$Jour_Ferie[1:2375], df_29$Jour_Ferie_Veille[1:2375], 
  df_29$Jour_Ferie_Lendemain[1:2375],
  df_29$Janvier[1:2375], df_29$Fevrier[1:2375],
  df_29$Mars[1:2375], df_29$Avril[1:2375], df_29$May[1:2375], 
  df_29$Juin[1:2375],
  df_29$Juillet[1:2375], df_29$Aout[1:2375], 
  df_29$Septembre[1:2375], df_29$Octobre[1:2375], df_29$Novembre[1:2375]))

print(fit6.1)

acf(residuals(fit6.1)[-(1:2)],
    main="With proper error structure (using auto.arima)")

adjreg6.1 <- sarima(demande[1:2375], 1,0,0, 
                    xreg=cbind(
                      w.hdd[1:2375], w.cdd[1:2375], lag(w.hdd, 1)[1:2375],
                      lag(w.cdd, 1)[1:2375], lag(w.hdd,2)[1:2375],
                      lag(w.cdd,2)[1:2375], w.cp[1:2375],
                      df_29$Lundi[1:2375], df_29$Mardi[1:2375], 
                      df_29$Mercredi[1:2375], df_29$Jeudi[1:2375],
                      df_29$Samedi[1:2375], df_29$Dimanche[1:2375], 
                      df_29$Jour_Ferie[1:2375], 
                      df_29$Jour_Ferie_Veille[1:2375], 
                      df_29$Jour_Ferie_Lendemain[1:2375],
                      df_29$Janvier[1:2375], df_29$Fevrier[1:2375],
                      df_29$Mars[1:2375], df_29$Avril[1:2375], 
                      df_29$May[1:2375], df_29$Juin[1:2375],
                      df_29$Juillet[1:2375], df_29$Aout[1:2375], 
                      df_29$Septembre[1:2375], df_29$Octobre[1:2375], 
                      df_29$Novembre[1:2375]))
adjreg6.1


for(i in 184:365)
{
  pred[i] <- predict(fit6.1, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i],
    lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i], w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i], 
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i], 
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i], 
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i], 
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i]))
  
  erreur2[i] = pred[[i]][1]-demande_valid[i]
}

fit6.2 <- auto.arima(demande[1:2557], xreg=cbind(
  w.hdd[1:2557], w.cdd[1:2557], lag(w.hdd, 1)[1:2557], 
  lag(w.cdd, 1)[1:2557], lag(w.hdd,2)[1:2557],
  lag(w.cdd,2)[1:2557], w.cp[1:2557],
  df_29$Lundi[1:2557], df_29$Mardi[1:2557], df_29$Mercredi[1:2557],
  df_29$Jeudi[1:2557], df_29$Samedi[1:2557], df_29$Dimanche[1:2557], 
  df_29$Jour_Ferie[1:2557], df_29$Jour_Ferie_Veille[1:2557],
  df_29$Jour_Ferie_Lendemain[1:2557],
  df_29$Janvier[1:2557], df_29$Fevrier[1:2557],
  df_29$Mars[1:2557], df_29$Avril[1:2557], df_29$May[1:2557],
  df_29$Juin[1:2557], df_29$Juillet[1:2557], df_29$Aout[1:2557], 
  df_29$Septembre[1:2557], df_29$Octobre[1:2557], df_29$Novembre[1:2557]))

print(fit6.2)

acf(residuals(fit6.2)[-(1:2)],
    main="With proper error structure (using auto.arima)")

adjreg6.2 <- sarima(demande[1:2557], 1,0,0, 
                    xreg=cbind(
                      w.hdd[1:2557], w.cdd[1:2557], lag(w.hdd, 1)[1:2557],
                      lag(w.cdd, 1)[1:2557], lag(w.hdd,2)[1:2557],
                      lag(w.cdd,2)[1:2557], w.cp[1:2557],
                      df_29$Lundi[1:2557], df_29$Mardi[1:2557], 
                      df_29$Mercredi[1:2557], df_29$Jeudi[1:2557],
                      df_29$Samedi[1:2557], df_29$Dimanche[1:2557], 
                      df_29$Jour_Ferie[1:2557], 
                      df_29$Jour_Ferie_Veille[1:2557], 
                      df_29$Jour_Ferie_Lendemain[1:2557],
                      df_29$Janvier[1:2557], df_29$Fevrier[1:2557],
                      df_29$Mars[1:2557], df_29$Avril[1:2557], 
                      df_29$May[1:2557], df_29$Juin[1:2557], 
                      df_29$Juillet[1:2557], df_29$Aout[1:2557], 
                      df_29$Septembre[1:2557], df_29$Octobre[1:2557], 
                      df_29$Novembre[1:2557]))
adjreg6.2

for(i in 366:548)
{
  pred[i] <- predict(fit6.2, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i], lag(w.hdd_valid,2)[i],
    lag(w.cdd_valid,2)[i], w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i],
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i],
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i], 
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i]))
  
  erreur2[i] = pred[[i]][1]-demande_valid[i]
}

fit6.3 <- auto.arima(demande[1:2740], xreg=cbind(
  w.hdd[1:2740], w.cdd[1:2740], lag(w.hdd, 1)[1:2740], lag(w.cdd, 1)[1:2740], 
  lag(w.hdd,2)[1:2740],
  lag(w.cdd,2)[1:2740], w.cp[1:2740],
  df_29$Lundi[1:2740], df_29$Mardi[1:2740], df_29$Mercredi[1:2740], 
  df_29$Jeudi[1:2740],
  df_29$Samedi[1:2740], df_29$Dimanche[1:2740], 
  df_29$Jour_Ferie[1:2740], df_29$Jour_Ferie_Veille[1:2740], 
  df_29$Jour_Ferie_Lendemain[1:2740],
  df_29$Janvier[1:2740], df_29$Fevrier[1:2740],
  df_29$Mars[1:2740], df_29$Avril[1:2740], df_29$May[1:2740],
  df_29$Juin[1:2740],
  df_29$Juillet[1:2740], df_29$Aout[1:2740], 
  df_29$Septembre[1:2740], df_29$Octobre[1:2740],
  df_29$Novembre[1:2740]))

print(fit6.3)

acf(residuals(fit6.3)[-(1:2)],
    main="With proper error structure (using auto.arima)")

adjreg6.3 <- sarima(demande[1:2740], 1,0,0, 
                    xreg=cbind(
                      w.hdd[1:2740], w.cdd[1:2740], lag(w.hdd, 1)[1:2740], 
                      lag(w.cdd, 1)[1:2740],
                      lag(w.hdd,2)[1:2740], lag(w.cdd,2)[1:2740], w.cp[1:2740],
                      df_29$Lundi[1:2740], df_29$Mardi[1:2740],
                      df_29$Mercredi[1:2740], df_29$Jeudi[1:2740],
                      df_29$Samedi[1:2740], df_29$Dimanche[1:2740], 
                      df_29$Jour_Ferie[1:2740], 
                      df_29$Jour_Ferie_Veille[1:2740], 
                      df_29$Jour_Ferie_Lendemain[1:2740],
                      df_29$Janvier[1:2740], df_29$Fevrier[1:2740],
                      df_29$Mars[1:2740], df_29$Avril[1:2740], 
                      df_29$May[1:2740], df_29$Juin[1:2740],
                      df_29$Juillet[1:2740], df_29$Aout[1:2740], 
                      df_29$Septembre[1:2740], df_29$Octobre[1:2740], 
                      df_29$Novembre[1:2740]))
adjreg6.3

for(i in 549:730)
{
  pred[i] <- predict(fit6.3, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i],
    lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i], w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i], df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i], 
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i],
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i],
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i]))
  
  erreur2[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur2^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur2/demande_valid), na.rm = T))*100
MAE = mean(abs(erreur2), na.rm = T)
ME = mean(erreur2, na.rm = T)

TABLEAU = cbind(RMSE, MAPE,MAE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE','MAE', 'ME')
TABLEAU


#Rolling window

erreur3 = NULL
for(i in 1:183)
{
  pred[i] <- predict(fit6, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i], lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i],
    w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i], 
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i], 
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i], 
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i]))
  
  erreur3[i] = pred[[i]][1]-demande_valid[i]
}

fit6.1 <- auto.arima(demande[184:2375], xreg=cbind(
  w.hdd[184:2375], w.cdd[184:2375], lag(w.hdd, 1)[184:2375], 
  lag(w.cdd, 1)[184:2375], lag(w.hdd,2)[184:2375],
  lag(w.cdd,2)[184:2375], w.cp[184:2375],
  df_29$Lundi[184:2375], df_29$Mardi[184:2375], df_29$Mercredi[184:2375], 
  df_29$Jeudi[184:2375],
  df_29$Samedi[184:2375], df_29$Dimanche[184:2375], 
  df_29$Jour_Ferie[184:2375], df_29$Jour_Ferie_Veille[184:2375], 
  df_29$Jour_Ferie_Lendemain[184:2375],
  df_29$Janvier[184:2375], df_29$Fevrier[184:2375],
  df_29$Mars[184:2375], df_29$Avril[184:2375], df_29$May[184:2375],
  df_29$Juin[184:2375],
  df_29$Juillet[184:2375], df_29$Aout[184:2375], 
  df_29$Septembre[184:2375], df_29$Octobre[184:2375], 
  df_29$Novembre[184:2375]))

print(fit6.1)

acf(residuals(fit6.1)[-(1:2)],
    main="With proper error structure (using auto.arima)")

adjreg6.1 <- sarima(demande[184:2375], 1,0,0, 
                    xreg=cbind(
                      w.hdd[184:2375], w.cdd[184:2375], 
                      lag(w.hdd, 1)[184:2375], lag(w.cdd, 1)[184:2375],
                      lag(w.hdd,2)[184:2375], lag(w.cdd,2)[184:2375], 
                      w.cp[184:2375],
                      df_29$Lundi[184:2375], df_29$Mardi[184:2375], 
                      df_29$Mercredi[184:2375], df_29$Jeudi[184:2375],
                      df_29$Samedi[184:2375], df_29$Dimanche[184:2375], 
                      df_29$Jour_Ferie[184:2375], 
                      df_29$Jour_Ferie_Veille[184:2375], 
                      df_29$Jour_Ferie_Lendemain[184:2375],
                      df_29$Janvier[184:2375], df_29$Fevrier[184:2375],
                      df_29$Mars[184:2375], df_29$Avril[184:2375],
                      df_29$May[184:2375], df_29$Juin[184:2375], 
                      df_29$Juillet[184:2375], df_29$Aout[184:2375], 
                      df_29$Septembre[184:2375], df_29$Octobre[184:2375], 
                      df_29$Novembre[184:2375]))
adjreg6.1

# try avec box cox? parce que diagramme qq des r??sidus bof

adjreg6.1 <- sarima(BoxCox(demande[184:2375],lambda), 1,0,0, 
                    xreg=cbind(
                      w.hdd[184:2375], w.cdd[184:2375], 
                      lag(w.hdd, 1)[184:2375], lag(w.cdd, 1)[184:2375],
                      lag(w.hdd,2)[184:2375], lag(w.cdd,2)[184:2375], 
                      w.cp[184:2375],
                      df_29$Lundi[184:2375], df_29$Mardi[184:2375], 
                      df_29$Mercredi[184:2375], df_29$Jeudi[184:2375],
                      df_29$Samedi[184:2375], df_29$Dimanche[184:2375], 
                      df_29$Jour_Ferie[184:2375], 
                      df_29$Jour_Ferie_Veille[184:2375], 
                      df_29$Jour_Ferie_Lendemain[184:2375],
                      df_29$Janvier[184:2375], df_29$Fevrier[184:2375],
                      df_29$Mars[184:2375], df_29$Avril[184:2375],
                      df_29$May[184:2375], df_29$Juin[184:2375], 
                      df_29$Juillet[184:2375], df_29$Aout[184:2375], 
                      df_29$Septembre[184:2375], df_29$Octobre[184:2375], 
                      df_29$Novembre[184:2375]))

# on voit que ca ne r??soud pas le probl??me de non normalit?? des r??sidus..




for(i in 184:365)
{
  pred[i] <- predict(fit6.1, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i],
    lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i], w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i], 
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i],
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i], 
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i],
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i]))
  
  erreur3[i] = pred[[i]][1]-demande_valid[i]
}

fit6.2 <- auto.arima(demande[366:2557], xreg=cbind(
  w.hdd[366:2557], w.cdd[366:2557], lag(w.hdd, 1)[366:2557], 
  lag(w.cdd, 1)[366:2557],
  lag(w.hdd,2)[366:2557], lag(w.cdd,2)[366:2557], w.cp[366:2557],
  df_29$Lundi[366:2557], df_29$Mardi[366:2557], df_29$Mercredi[366:2557], 
  df_29$Jeudi[366:2557], 
  df_29$Samedi[366:2557], df_29$Dimanche[366:2557], 
  df_29$Jour_Ferie[366:2557], df_29$Jour_Ferie_Veille[366:2557], 
  df_29$Jour_Ferie_Lendemain[366:2557],
  df_29$Janvier[366:2557], df_29$Fevrier[366:2557],
  df_29$Mars[366:2557], df_29$Avril[366:2557], df_29$May[366:2557],
  df_29$Juin[366:2557],
  df_29$Juillet[366:2557], df_29$Aout[366:2557], 
  df_29$Septembre[366:2557], df_29$Octobre[366:2557], 
  df_29$Novembre[366:2557]))

print(fit6.2)

acf(residuals(fit6.2)[-(1:2)],
    main="With proper error structure (using auto.arima)")

adjreg6.2 <- sarima(demande[366:2557], 1,0,0, 
                    xreg=cbind(
                      w.hdd[366:2557], w.cdd[366:2557], 
                      lag(w.hdd, 1)[366:2557],
                      lag(w.cdd, 1)[366:2557], lag(w.hdd,2)[366:2557], 
                      lag(w.cdd,2)[366:2557],
                      w.cp[366:2557],
                      df_29$Lundi[366:2557], df_29$Mardi[366:2557], 
                      df_29$Mercredi[366:2557],
                      df_29$Jeudi[366:2557], df_29$Samedi[366:2557],
                      df_29$Dimanche[366:2557], 
                      df_29$Jour_Ferie[366:2557], 
                      df_29$Jour_Ferie_Veille[366:2557], 
                      df_29$Jour_Ferie_Lendemain[366:2557],
                      df_29$Janvier[366:2557], df_29$Fevrier[366:2557],
                      df_29$Mars[366:2557], df_29$Avril[366:2557],
                      df_29$May[366:2557],
                      df_29$Juin[366:2557], df_29$Juillet[366:2557],
                      df_29$Aout[366:2557], 
                      df_29$Septembre[366:2557], df_29$Octobre[366:2557],
                      df_29$Novembre[366:2557]))
adjreg6.2

for(i in 366:548)
{
  pred[i] <- predict(fit6.2, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i],
    lag(w.cdd_valid, 1)[i],
    lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i], w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i], 
    df_29_Valid$Jeudi[i], 
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i],
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i],
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i], 
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i]))
  
  erreur3[i] = pred[[i]][1]-demande_valid[i]
}

fit6.3 <- auto.arima(demande[548:2740], xreg=cbind(
  w.hdd[548:2740], w.cdd[548:2740], lag(w.hdd, 1)[548:2740], 
  lag(w.cdd, 1)[548:2740], lag(w.hdd,2)[548:2740],
  lag(w.cdd,2)[548:2740], w.cp[548:2740],
  df_29$Lundi[548:2740], df_29$Mardi[548:2740], df_29$Mercredi[548:2740], 
  df_29$Jeudi[548:2740], df_29$Samedi[548:2740], df_29$Dimanche[548:2740], 
  df_29$Jour_Ferie[548:2740], df_29$Jour_Ferie_Veille[548:2740], 
  df_29$Jour_Ferie_Lendemain[548:2740],
  df_29$Janvier[548:2740], df_29$Fevrier[548:2740],
  df_29$Mars[548:2740], df_29$Avril[548:2740], df_29$May[548:2740],
  df_29$Juin[548:2740], 
  df_29$Juillet[548:2740], df_29$Aout[548:2740], 
  df_29$Septembre[548:2740], df_29$Octobre[548:2740], 
  df_29$Novembre[548:2740]))

print(fit6.3)

acf(residuals(fit6.3)[-(1:2)],
    main="With proper error structure (using auto.arima)")

adjreg6.3 <- sarima(demande[548:2740], 1,0,0, 
                    xreg=cbind(
                      w.hdd[548:2740], w.cdd[548:2740], 
                      lag(w.hdd, 1)[548:2740], lag(w.cdd, 1)[548:2740],
                      lag(w.hdd,2)[548:2740], lag(w.cdd,2)[548:2740],
                      w.cp[548:2740],
                      df_29$Lundi[548:2740], df_29$Mardi[548:2740],
                      df_29$Mercredi[548:2740],
                      df_29$Jeudi[548:2740], df_29$Samedi[548:2740], 
                      df_29$Dimanche[548:2740], 
                      df_29$Jour_Ferie[548:2740], 
                      df_29$Jour_Ferie_Veille[548:2740], 
                      df_29$Jour_Ferie_Lendemain[548:2740],
                      df_29$Janvier[548:2740], df_29$Fevrier[548:2740],
                      df_29$Mars[548:2740], df_29$Avril[548:2740],
                      df_29$May[548:2740], 
                      df_29$Juin[548:2740], df_29$Juillet[548:2740], 
                      df_29$Aout[548:2740], 
                      df_29$Septembre[548:2740], df_29$Octobre[548:2740],
                      df_29$Novembre[548:2740]))
adjreg6.3

for(i in 549:730)
{
  pred[i] <- predict(fit6.2, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i],
    lag(w.cdd_valid, 1)[i],
    lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i], w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i], df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i],
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], 
    df_29_Valid$Mars[i], df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i],
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i]))
  
  erreur3[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur3^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur3/demande_valid), na.rm = T))*100
ME = mean(erreur3, na.rm = T)

TABLEAU = cbind(RMSE, MAPE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE', 'ME')
TABLEAU


#Modele 8

fit8 <- auto.arima(demande_train, xreg=cbind(
  w.hdd_train, w.cdd_train, lag(w.hdd_train, 1), lag(w.cdd_train, 1),
  lag(w.hdd_train,2),
  lag(w.cdd_train,2), w.cp_train,
  df_29_Train$Lundi, df_29_Train$Mardi, df_29_Train$Mercredi, 
  df_29_Train$Jeudi,
  df_29_Train$Samedi, df_29_Train$Dimanche, 
  df_29_Train$Jour_Ferie, df_29_Train$Jour_Ferie_Veille,
  df_29_Train$Jour_Ferie_Lendemain,
  df_29_Train$Janvier, df_29_Train$Fevrier,
  df_29_Train$Mars, df_29_Train$Avril, df_29_Train$May, 
  df_29_Train$Juin, 
  df_29_Train$Juillet, df_29_Train$Aout, 
  df_29_Train$Septembre, df_29_Train$Octobre, df_29_Train$Novembre,
  df_29_Train$Tornado))

print(fit8)

acf(residuals(fit8)[-(1:2)],
    main="With proper error structure (using auto.arima)")

adjreg8 <- sarima(demande_train, 1,0,0, 
                  xreg=cbind(
                    w.hdd_train, w.cdd_train, lag(w.hdd_train, 1), 
                    lag(w.cdd_train, 1), lag(w.hdd_train,2),
                    lag(w.cdd_train,2), w.cp_train,
                    df_29_Train$Lundi, df_29_Train$Mardi,
                    df_29_Train$Mercredi, df_29_Train$Jeudi,
                    df_29_Train$Samedi, df_29_Train$Dimanche, 
                    df_29_Train$Jour_Ferie, df_29_Train$Jour_Ferie_Veille,
                    df_29_Train$Jour_Ferie_Lendemain,
                    df_29_Train$Janvier, df_29_Train$Fevrier,
                    df_29_Train$Mars, df_29_Train$Avril, df_29_Train$May, 
                    df_29_Train$Juin,
                    df_29_Train$Juillet, df_29_Train$Aout, 
                    df_29_Train$Septembre, df_29_Train$Octobre, 
                    df_29_Train$Novembre, df_29_Train$Tornado))


adjreg8

erreur = NULL
for(i in 1:nrow(w.hdd_valid))
{
  pred[i] <- predict(fit8, n.ahead=1, newxreg = cbind(
    w.hdd_valid[i], w.cdd_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i], lag(w.hdd_valid,2)[i],
    lag(w.cdd_valid,2)[i], w.cp_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i], 
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i], 
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i], 
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i], 
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i], 
    df_29_Valid$Tornado[i]))
  
  erreur[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur/demande_valid), na.rm = T))*100
MAE = mean(abs(erreur), na.rm = T)
ME = mean(erreur, na.rm = T)

TABLEAU = cbind(RMSE, MAPE,MAE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE','MAE', 'ME')
TABLEAU


#Analyse Residus
DATE_valid = as.character(seq(as.Date("2017-01-03"), 
                              as.Date("2018-12-31"), by="days"))

erreur_matrix = as.data.frame(cbind(DATE_valid, as.numeric(erreur[-(1:2)])))

erreur_matrix$Residus = as.numeric(as.character(erreur_matrix$V2))
erreur_matrix$Mois = as.factor(substr(DATE_valid,6,7))
erreur_matrix$Year = as.factor(substr(DATE_valid,1,4))
erreur_matrix$Jour_semaine = weekdays(as.Date(DATE_valid))


boxplot(Residus ~ Mois, data = erreur_matrix,
        main="Residus sur le pic journalier d'electricite 
        a Lincoln NE selon le mois de l'annee")

boxplot(Residus ~ Year, data = erreur_matrix,
        main="Residus sur le pic journalier d'electricite
        a Lincoln NE selon le mois de l'annee")

boxplot(Residus ~ Jour_semaine, data = erreur_matrix,
        main="Residus sur le pic journalier d'electricite 
        a Lincoln NE selon le jour de la semaine")

#Intervalle de prevision
erreur_95=NULL
for(i in 1:nrow(w.hdd_valid))
{
  erreur_95[i] = ifelse(demande_valid[i]<=pred[[i]][1]+1.96*23.904 && 
                          demande_valid[i]>=pred[[i]][1]-1.96*23.904,0,1)
}

sum(erreur_95, na.rm = T)/nrow(w.hdd_valid)


#Methode 2
erreur1 = NULL
for(i in 1:365)
{
  pred[i] <- predict(fit8, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i],
    lag(w.cdd_valid, 1)[i], lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i],
    w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i], 
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i], 
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i],
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i], 
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i], 
    df_29_Valid$Tornado[i]))
  
  erreur1[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur1^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur1/demande_valid), na.rm = T))*100
ME = mean(erreur1, na.rm = T)

TABLEAU2 = cbind(RMSE, MAPE, ME)
colnames(TABLEAU2) = c('RMSE', 'MAPE', 'ME')
TABLEAU2


fit8.1 <- arima(demande[1:2557], xreg=cbind(
  w.hdd[1:2557], w.cdd[1:2557], lag(w.hdd, 1)[1:2557], 
  lag(w.cdd, 1)[1:2557],
  lag(w.hdd,2)[1:2557], lag(w.cdd,2)[1:2557], w.cp[1:2557],
  df_29$Lundi[1:2557], df_29$Mardi[1:2557], df_29$Mercredi[1:2557], 
  df_29$Jeudi[1:2557],
  df_29$Samedi[1:2557], df_29$Dimanche[1:2557], 
  df_29$Jour_Ferie[1:2557], df_29$Jour_Ferie_Veille[1:2557], 
  df_29$Jour_Ferie_Lendemain[1:2557],
  df_29$Janvier[1:2557], df_29$Fevrier[1:2557],
  df_29$Mars[1:2557], df_29$Avril[1:2557], df_29$May[1:2557], 
  df_29$Juin[1:2557],
  df_29$Juillet[1:2557], df_29$Aout[1:2557], 
  df_29$Septembre[1:2557], df_29$Octobre[1:2557],
  df_29$Novembre[1:2557], df_29$Tornado[1:2557]), order = c(1,0,0))

print(fit8.1)

acf(residuals(fit8.1)[-(1:2)],
    main="With proper error structure (using auto.arima)")



for(i in 366:730)
{
  pred[i] <- predict(fit8.1, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i],
    lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i], w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i], 
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i],
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i],
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i], df_29_Valid$Tornado[i]))
  
  erreur1[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur1^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur1/demande_valid), na.rm = T))*100
MAE = mean(abs(erreur1), na.rm = T)
ME = mean(erreur1, na.rm = T)

TABLEAU = cbind(RMSE, MAPE,MAE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE','MAE', 'ME')
TABLEAU

#Methode #3

erreur2 = NULL
for(i in 1:183)
{
  pred[i] <- predict(fit8, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i],
    lag(w.cdd_valid, 1)[i],
    lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i], w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i], 
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i], 
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i], 
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i], 
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i], 
    df_29_Valid$Tornado[i]))
  
  erreur2[i] = pred[[i]][1]-demande_valid[i]
}

fit8.1 <- auto.arima(demande[1:2375], xreg=cbind(
  w.hdd[1:2375], w.cdd[1:2375], lag(w.hdd, 1)[1:2375], 
  lag(w.cdd, 1)[1:2375], lag(w.hdd,2)[1:2375],
  lag(w.cdd,2)[1:2375], w.cp[1:2375],
  df_29$Lundi[1:2375], df_29$Mardi[1:2375], df_29$Mercredi[1:2375],
  df_29$Jeudi[1:2375],
  df_29$Samedi[1:2375], df_29$Dimanche[1:2375], 
  df_29$Jour_Ferie[1:2375], df_29$Jour_Ferie_Veille[1:2375],
  df_29$Jour_Ferie_Lendemain[1:2375],
  df_29$Janvier[1:2375], df_29$Fevrier[1:2375],
  df_29$Mars[1:2375], df_29$Avril[1:2375], df_29$May[1:2375], 
  df_29$Juin[1:2375],
  df_29$Juillet[1:2375], df_29$Aout[1:2375], 
  df_29$Septembre[1:2375], df_29$Octobre[1:2375], df_29$Novembre[1:2375], 
  df_29$Tornado[1:2375]))

print(fit8.1)


for(i in 184:365)
{
  pred[i] <- predict(fit8.1, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i],
    lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i], w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i], 
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i], 
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i], 
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i],
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i], df_29_Valid$Tornado[i]))
  
  erreur2[i] = pred[[i]][1]-demande_valid[i]
}

fit8.2 <- auto.arima(demande[1:2557], xreg=cbind(
  w.hdd[1:2557], w.cdd[1:2557], lag(w.hdd, 1)[1:2557], lag(w.cdd, 1)[1:2557], 
  lag(w.hdd,2)[1:2557],
  lag(w.cdd,2)[1:2557], w.cp[1:2557],
  df_29$Lundi[1:2557], df_29$Mardi[1:2557], df_29$Mercredi[1:2557],
  df_29$Jeudi[1:2557], df_29$Samedi[1:2557], df_29$Dimanche[1:2557], 
  df_29$Jour_Ferie[1:2557], df_29$Jour_Ferie_Veille[1:2557], 
  df_29$Jour_Ferie_Lendemain[1:2557],
  df_29$Janvier[1:2557], df_29$Fevrier[1:2557],
  df_29$Mars[1:2557], df_29$Avril[1:2557], df_29$May[1:2557],
  df_29$Juin[1:2557], df_29$Juillet[1:2557], df_29$Aout[1:2557], 
  df_29$Septembre[1:2557], df_29$Octobre[1:2557], df_29$Novembre[1:2557], 
  df_29$Tornado[1:2557]))

print(fit8.2)

for(i in 366:548)
{
  pred[i] <- predict(fit8.2, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i], lag(w.hdd_valid,2)[i],
    lag(w.cdd_valid,2)[i], w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i], 
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i],
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i], 
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i],df_29_Valid$Tornado[i] ))
  
  erreur2[i] = pred[[i]][1]-demande_valid[i]
}

fit8.3 <- auto.arima(demande[1:2740], xreg=cbind(
  w.hdd[1:2740], w.cdd[1:2740], lag(w.hdd, 1)[1:2740], lag(w.cdd, 1)[1:2740], 
  lag(w.hdd,2)[1:2740],
  lag(w.cdd,2)[1:2740], w.cp[1:2740],
  df_29$Lundi[1:2740], df_29$Mardi[1:2740], df_29$Mercredi[1:2740], 
  df_29$Jeudi[1:2740],
  df_29$Samedi[1:2740], df_29$Dimanche[1:2740], 
  df_29$Jour_Ferie[1:2740], df_29$Jour_Ferie_Veille[1:2740], 
  df_29$Jour_Ferie_Lendemain[1:2740],
  df_29$Janvier[1:2740], df_29$Fevrier[1:2740],
  df_29$Mars[1:2740], df_29$Avril[1:2740], df_29$May[1:2740],
  df_29$Juin[1:2740],
  df_29$Juillet[1:2740], df_29$Aout[1:2740], 
  df_29$Septembre[1:2740], df_29$Octobre[1:2740], df_29$Novembre[1:2740],
  df_29$Tornado[1:2740]))

print(fit8.3)


for(i in 549:730)
{
  pred[i] <- predict(fit8.3, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i],
    lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i], w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i], df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i], 
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Janvier[i], df_29_Valid$Fevrier[i], df_29_Valid$Mars[i], 
    df_29_Valid$Avril[i],
    df_29_Valid$May[i], df_29_Valid$Juin[i],df_29_Valid$Juillet[i],
    df_29_Valid$Aout[i], df_29_Valid$Septembre[i], 
    df_29_Valid$Octobre[i], df_29_Valid$Novembre[i], df_29_Valid$Tornado[i] ))
  
  erreur2[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur2^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur2/demande_valid), na.rm = T))*100
MAE = mean(abs(erreur2), na.rm = T)
ME = mean(erreur2, na.rm = T)

TABLEAU = cbind(RMSE, MAPE,MAE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE','MAE', 'ME')
TABLEAU



#Modele 9 

fit9 <- auto.arima(demande_train, xreg=cbind(
  w.hdd_train, w.cdd_train, lag(w.hdd_train, 1),
  lag(w.cdd_train, 1), 
  lag(w.hdd_train,2), lag(w.cdd_train,2), w.cp_train,
  df_29_Train$Lundi, df_29_Train$Mardi, df_29_Train$Mercredi, 
  df_29_Train$Jeudi,
  df_29_Train$Samedi, df_29_Train$Dimanche, 
  df_29_Train$Jour_Ferie, df_29_Train$Jour_Ferie_Veille, 
  df_29_Train$Jour_Ferie_Lendemain,
  df_29_Train$Summer, df_29_Train$Winter, df_29_Train$Fall,
  df_29_Train$Tornado ))

print(fit9)

acf(residuals(fit9)[-(1:2)],
    main="With proper error structure (using auto.arima)")

adjreg9 <- sarima(demande_train, 1,0,0, 
                  xreg=cbind(
                    w.hdd_train, w.cdd_train, lag(w.hdd_train, 1), 
                    lag(w.cdd_train, 1), 
                    lag(w.hdd_train,2), lag(w.cdd_train,2), w.cp_train,
                    df_29_Train$Lundi, df_29_Train$Mardi, df_29_Train$Mercredi,
                    df_29_Train$Jeudi,
                    df_29_Train$Samedi, df_29_Train$Dimanche, 
                    df_29_Train$Jour_Ferie, df_29_Train$Jour_Ferie_Veille, 
                    df_29_Train$Jour_Ferie_Lendemain, 
                    df_29_Train$Summer, df_29_Train$Winter, df_29_Train$Fall))


adjreg9
erreur = NULL
for(i in 1:nrow(w.hdd_valid))
{
  pred[i] <- predict(fit9,  n.ahead=1, newxreg = cbind(
    w.hdd_valid[i], w.cdd_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i], lag(w.hdd_valid,2)[i],
    lag(w.cdd_valid,2)[i], w.cp_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i], 
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Summer[i], df_29_Valid$Winter[i], df_29_Valid$Fall[i]))
  
  erreur[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur/demande_valid), na.rm = T))*100
MAE = mean(abs(erreur), na.rm = T)
ME = mean(erreur, na.rm = T)

TABLEAU = cbind(RMSE, MAPE,MAE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE','MAE', 'ME')
TABLEAU

#Analyse Residus
DATE_valid = as.character(seq(as.Date("2017-01-03"), 
                              as.Date("2018-12-31"), by="days"))

erreur_matrix = as.data.frame(cbind(DATE_valid, as.numeric(erreur[-(1:2)])))

erreur_matrix$Residus = as.numeric(as.character(erreur_matrix$V2))
erreur_matrix$Mois = as.factor(substr(DATE_valid,6,7))
erreur_matrix$Year = as.factor(substr(DATE_valid,1,4))
erreur_matrix$Jour_semaine = weekdays(as.Date(DATE_valid))


boxplot(Residus ~ Mois, data = erreur_matrix,
        main="Residuss sur le pic journalier d'electricite a 
        Lincoln NE selon le mois de l'annee")

#Methode 2 

erreur1 = NULL
for(i in 1:365)
{
  pred[i] <- predict(fit9, n.ahead=1, newxreg = cbind(
    w.hdd_valid[i], w.cdd_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i],
    lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i], w.cp_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i],
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i],
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Summer[i], df_29_Valid$Winter[i], df_29_Valid$Fall[i]))
  erreur1[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur1^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur1/demande_valid), na.rm = T))*100
ME = mean(erreur1, na.rm = T)

TABLEAU2 = cbind(RMSE, MAPE, ME)
colnames(TABLEAU2) = c('RMSE', 'MAPE', 'ME')
TABLEAU2


fit9.1 <- arima(demande[1:2557], xreg=cbind(
  w.hdd_train[1:2557], w.cdd_train[1:2557], lag(w.hdd_train, 1)[1:2557],
  lag(w.cdd_train, 1)[1:2557], lag(w.hdd_train,2)[1:2557], 
  lag(w.cdd_train,2)[1:2557], w.cp_train[1:2557],
  df_29_Train$Lundi[1:2557], df_29_Train2$Mardi[1:2557], 
  df_29_Train$Mercredi[1:2557], 
  df_29_Train$Jeudi[1:2557], df_29_Train$Samedi[1:2557], 
  df_29_Train$Dimanche[1:2557], 
  df_29_Train$Jour_Ferie[1:2557], df_29_Train$Jour_Ferie_Veille[1:2557], 
  df_29_Train$Jour_Ferie_Lendemain[1:2557], 
  df_29_Train$Summer[1:2557], df_29_Train$Winter[1:2557], 
  df_29_Train$Fall[1:2557]), order = c(1,0,0))

print(fit6.1)


adjreg9.1 <- sarima(demande[1:2557], 1,0,0, 
                    xreg=cbind(
                      w.hdd_train[1:2557], w.cdd_train[1:2557], 
                      lag(w.hdd_train, 1)[1:2557], 
                      lag(w.cdd_train, 1)[1:2557], lag(w.hdd_train,2)[1:2557], 
                      lag(w.cdd_train,2)[1:2557], w.cp_train[1:2557],
                      df_29_Train$Lundi[1:2557], df_29_Train2$Mardi[1:2557], 
                      df_29_Train$Mercredi[1:2557], df_29_Train$Jeudi[1:2557],
                      df_29_Train$Samedi[1:2557], df_29_Train$Dimanche[1:2557], 
                      df_29_Train$Jour_Ferie[1:2557],
                      df_29_Train$Jour_Ferie_Veille[1:2557],
                      df_29_Train$Jour_Ferie_Lendemain[1:2557],
                      df_29_Train$Janvier[1:2557], df_29_Train$Fevrier[1:2557],
                      df_29_Train$Mars[1:2557], df_29_Train$Avril[1:2557],
                      df_29_Train$May[1:2557], df_29_Train$Juin[1:2557],
                      df_29_Train$Juillet[1:2557], df_29_Train$Aout[1:2557], 
                      df_29_Train$Septembre[1:2557], df_29_Train$Octobre[1:2557], 
                      df_29_Train$Novembre[1:2557]))

adjreg9.1


for(i in 366:730)
{
  pred[i] <- predict(fit9.1, n.ahead=1, newxreg = cbind(
    w.hdd.bruit_valid[i], w.cdd.bruit_valid[i], lag(w.hdd_valid, 1)[i], 
    lag(w.cdd_valid, 1)[i],
    lag(w.hdd_valid,2)[i], lag(w.cdd_valid,2)[i], w.cp.bruit_valid[i],
    df_29_Valid$Lundi[i], df_29_Valid$Mardi[i], df_29_Valid$Mercredi[i], 
    df_29_Valid$Jeudi[i],
    df_29_Valid$Samedi[i], df_29_Valid$Dimanche[i],
    df_29_Valid$Jour_Ferie[i], df_29_Valid$Jour_Ferie_Veille[i],
    df_29_Valid$Jour_Ferie_Lendemain[i], 
    df_29_Valid$Summer[i], df_29_Valid$Winter[i], df_29_Valid$Fall[i]))
  
  erreur1[i] = pred[[i]][1]-demande_valid[i]
}

RMSE = (mean(erreur1^2, na.rm = T))^0.5
MAPE = (mean(abs(erreur1/demande_valid), na.rm = T))*100
ME = mean(erreur1, na.rm = T)

TABLEAU = cbind(RMSE, MAPE, ME)
colnames(TABLEAU) = c('RMSE', 'MAPE', 'ME')
TABLEAU






