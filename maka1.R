###sateliit
setwd("C:/Users/Mats/Documents/Kool/MAKATÖÖ/kagu.andmenaidis")

sat = read.csv("Kagu-Eesti_SMI_prt_pix.csv.sort")

#täielikud read, test, kas heleduse jaotused on samad

sat1 = subset(sat, select = -c(label,prtk))
sat1[sat1 == 0] = NA
sat1 = na.omit(sat1)

satnames = names(sat1)
satnames

#blue
b2 = unlist(sat1[,grep("B2", satnames)])
hist(b2)
b02 = unlist(sat1[,grep("B02", satnames)])*9
#9-kordne erinevus, kuna spatial resolution on vastavalt 10 ja 30
hist(b02) #kuskil 0

mean(b2); mean(b02)
var(b2); var(b02)

#green
par(mfrow = c(1,2))
b3 = unlist(sat1[,grep("B2", satnames)])
hist(b3)
b03 = unlist(sat1[,grep("B03", satnames)])*9
hist(b03)

mean(b3); mean(b03)
var(b3); var(b03)

#red
par(mfrow = c(1,2))
b4 = unlist(sat1[,grep("B4", satnames)])
hist(b4)
b04 = unlist(sat1[,grep("B04", satnames)])*9
hist(b04)

mean(b4); mean(b04)
var(b4); var(b04)

#SWIR1
par(mfrow = c(1,2))
b6 = unlist(sat1[,grep("B6", satnames)])
hist(b6)
b11 = unlist(sat1[,grep("B11", satnames)])*9 #peaks nagu 9/4 olema, mitte 9...
hist(b11)

mean(b6); mean(b11)
var(b6); var(b11)

#SWIR2
par(mfrow = c(1,2))
b7 = unlist(sat1[,grep("B7", satnames)])
hist(b7)
b12 = unlist(sat1[,grep("B12", satnames)])*9 #peaks nagu 9/4 olema, mitte 9...
hist(b12)

mean(b7); mean(b12)
var(b7); var(b12)


#mis aja kohta on andmed?
LC = satnames[grep("LC08",satnames)]
S2 = satnames[grep("S2",satnames)]

LCdate = as.Date(unique(substr(LC, 13,20)), format="%Y%m%d")
S2date = as.Date(unique(substr(S2, 13,20)), format="%Y%m%d")
LCdate;S2date #"20180512" "20180528" "20170617" "20180823" "20180510" "20180526"; "20170505" "20180510" "20180530" "20180609" "20170502" "20170830" "20180527" "20180823" "20170924" "20180919"

dates = data.frame(date = c(S2date,LCdate))
length(S2date) #10
length(LCdate) #6
dates$sat = c(rep("Sentinel",10),rep("Landsat",6))
dates$y = c(rep(1,10),rep(1,6))

require(ggplot2)
dateplot = ggplot(data = dates) + geom_point(aes(date,y,col = sat, size = 10, shape = sat))
dateplot

sort(S2date)
#2. ja 5. mai keskmistada?


#####lidar
#prooviks dimensionaalsust vähendada?

SID108_2017 = read.csv("ALS/pilved36m/108_2017.xyz", header = F, sep = " ")
SID122_2017 = read.csv("ALS/pilved36m/122_2017.xyz", header = F, sep = " ")

sid108 = SID108_2017[SID108_2017$V2 == 1,]
m = ggplot(data = sid108, aes(V3,V4))
m + geom_point(aes(col = V5, size = 4, alpha = 0.4)) + scale_colour_gradient2(high = "darkgreen")

sid122 = SID122_2017[SID122_2017$V2 == 1,]
m = ggplot(data = sid122, aes(V3,V4))
m + geom_point(aes(col = V5, size = 4, alpha = 0.4)) + scale_colour_gradient2(high = "darkgreen")

#proovime kõik sisse lugeda:
setwd("C:/Users/Mats/Documents/Kool/MAKATÖÖ/kagu.andmenaidis/ALS/2017")
temp = list.files(pattern="*.xyz")

readx = function(arg){
  read.csv(arg, header = F, sep = " ")
}

XYZ = lapply(temp, readx)


sizes = lapply(XYZ, nrow)
cc = data.frame(sizes)
sizes = cc[1,]
max(sizes) #7497
min(sizes) #2579

# head(XYZ[[2]][,5])
# 
# fun1 = function(lst){
#   asd1 = lst[[1]]
#   V5 = asd1[1,1]
#   V5
# }
# 
# asd = XYZ[1]
# asd1 = asd[1]
# asd11 = asd[[1]]
# asd111 = XYZ[2][[1]]
# asd11[,5]
# 
# V5 = lapply(XYZ, fun1)

XYZ = sapply(temp, readx) #sapply vist lihtsam
V5 = XYZ[5,]
#V5[3]

# fun1 = function(arg){
#   n = length(arg)
#   add = 7497 - n
#   sam = sample(arg, size = add, replace = T)
#   c(arg,sam)
# }
# #probleem: hajuvus on vähenenud
# asd1 = sapply(V5, fun1)

fun2 = function(arg){
  sam = sample(arg, size = 2579)
  sam
}

#probleem: osa infost on raisku läinud
asd2 = sapply(V5, fun2)
asdn = names(as.data.frame(asd2))

#vähendame dimensionaalsust:
asdx = as.data.frame(t(asd2))

lidar.pca <- prcomp(asdx, center = TRUE,scale. = TRUE)
summary(lidar.pca)

cumvar = cumsum(lidar.pca$sdev**2)/sum(lidar.pca$sdev**2)
cumvar #68 esimest kirjeldab 90% hajuvusest

PCA90 = as.data.frame(lidar.pca$x[,1:68])
pcanames = gsub("_","",substr(asdn,1,4))
PCA90

#muld juurde ja SENtinel kevad ja sügis 2017

#sateliit sample by SID
require(dplyr)
satsamp = sat1 %>% group_by(SID) %>% sample_n(1)


###proovime aint lidari peal klassifitseerida?

km = kmeans(PCA90, centers = 5, nstart = 10) #palju klasse üldse võtta?
km$cluster

PCA90$clus = km$cluster
PCA90$SID = pcanames
table(PCA90$clus)

#x = PCA90[c("SID", "clus")]

cl4 = PCA90$SID[PCA90$clus == 4]

###
setwd("C:/Users/Mats/Documents/Kool/MAKATÖÖ/kagu.andmenaidis")
takseer = read.csv("SMI_100PRT12_16_TAKSEER.csv", header = T, sep = ";")
names = names(takseer)
names[1] = "SID"
names(takseer) = names
names
attach(takseer)
takseer$ARV_KOKKU = as.numeric(ARV_VMA) +  as.numeric(ARV_VKU) + 
  as.numeric(ARV_VKS) + as.numeric(ARV_VHB) + as.numeric(ARV_VLM) +
  as.numeric(ARV_VLV) + as.numeric(ARV_VXX)


#kõige suurema osakaaluga:

mx = apply(takseer[,15:21],1,max)
enamus = colnames(takseer[,15:21])[apply(takseer[,15:21],1,which.max)]
takseer$OSAKAAL = mx / takseer$ARV_KOKKU
takseer$ENAMUS = substr(enamus, 6,7)

table(takseer$muld)

#Prooviks esmalt hinnata koguarvukust?
#Kasutame ainult Sentineli; Ainult sügis ja kevad? Et oleks rohkem ruute, kus andmed on olemas.
S2date

grep("S2", satnames)
Sentinel = sat[,grep("S2", names(sat))]
Sentinel[Sentinel == 0] = NA


S2date_nonunique = as.Date(substr(S2, 13,20), format="%Y%m%d")
names(Sentinel)
S2date_nonunique
months(S2date_nonunique) #9 erinevat kanalit

m1 = Sentinel[,1:9];m2 = Sentinel[,10:18];m3 = Sentinel[,19:27]; m4 = Sentinel[,37:45]; m5 = Sentinel[,55:63]
j1 = Sentinel[,28:36]
a1 = Sentinel[,46:54]; a2 = Sentinel[,64:72]
s1 = Sentinel[,73:81]; s2 = Sentinel[,82:90]
mj = (m1 + m2 + m3 + m4 + m5 + j1)/6
as = (a1 + a2 + s1 + s2)/4
#AGA! see ei arvesta NA

# Xmj <- list(m1,m2,m3,m4,m5,j1)
# Ymj <- do.call(cbind, Xmj)
# Ymj <- array(Ymj, dim=c(dim(Xmj[[1]]), length(Xmj)))
# Ymj = numeric(Ymj)
# mj = colMeans(aperm(Ymj, c(3, 1, 2)), na.rm = TRUE)

#tuleb vist eraldi funktsioon kirjutada ja seda sapplyida

matmean = function(arg){
}

#või kui muud üle ei jää, siis ikkagi for loop :S

tmp1 = matrix(NA, nrow = 5978, ncol = 1)
dim(tmp1)
for(i in 1:9){
  #i = 1
  tmp = matrix(rowMeans(cbind(m1[,i],m2[,i],m3[,i],m4[,i],m5[,i],j1[,i]),na.rm = T))
  print(dim(tmp1))
  print(dim(tmp))
  tmp1 = cbind(tmp1, tmp)
  print(dim(tmp1))
}

mj = tmp1[,-1]


tmp1 = matrix(NA, nrow = 5978, ncol = 1)
dim(tmp1)
for(i in 1:9){
  #i = 1
  tmp = matrix(rowMeans(cbind(a1[,i],a2[,i],s1[,i],s2[,i]),na.rm = T))
  print(dim(tmp1))
  print(dim(tmp))
  tmp1 = cbind(tmp1, tmp)
  print(dim(tmp1))
}

as = tmp1[,-1]

#nonii, saimegi iga lapi kohta midagi (ehk puuduvaid andmeid pole)
mjas = data.frame(cbind(mj,as))
mjas1 = na.omit(mjas)

Bs = c("B2","B3","B4","B5","B6","B7","B8","B11","B12")
kuud = c("MJ","AS")
kuud = rep(kuud, each = 9)
Bs = rep(Bs, 2)
PST = paste(kuud,Bs,sep ="")
names(mjas) = PST
#Mai-Juuni ja August-September, 2017-2018 aasta keskmine bandide kaupa

mjas$muld = sat$muld; mjas$SID = sat$SID
#muld		Eesti 1:10000 digitaalse mullakaardi mulda iseloomustav kood. Mullad on rühmitatud mullamaatriksi järgi 
#(Construction of tree species composition map of Estonia using multispectral satellite images, soil map and a random forest algoritm¡ by Mait Lang, Mihkel Kaha, Diana Laarmann and Allan Sims. 2018. Accepted for publishing in the journal Forestry Studies | Metsanduslikud Uurimused volume 68, 2018(DOI: 10.2478/fsmu-2018-0001)).
#faktortunnus

satsamp = mjas %>% group_by(SID) %>% sample_n(1)

sat_proov = satsamp[satsamp$SID %in% takseer$SID,]


korgus15 = read.csv("ALS/Kagu-Eesti_15m_korgus.csv")
katvus15 = read.csv("ALS/Kagu-Eesti_15m_katvus.csv")

korgus15_17 = korgus15[grep("2017",korgus15$FileTitle),]
katvus15_17 = katvus15[grep("2017",katvus15$FileTitle),]
katvus15_17$SID = gsub("_", "", substr(katvus15_17$FileTitle,1,4))
korgus15_17$SID = gsub("_", "", substr(korgus15_17$FileTitle,1,4))

katvus15_17 = katvus15_17[,-c(1,2)]; korgus15_17 = korgus15_17[,-c(1,2)]
names(katvus15_17)
#intensiivsused välja: (seal nagunii palju "-1.#IND00")
katvus_int = katvus15_17[,-(50:82)]
korgus_int = korgus15_17[,-(50:83)]


######################################################################
DATA = merge(takseer,sat_proov, by = "SID")
DATA = merge(DATA, korgus_int, by = "SID")
#kõik andmed (va lidari toorandmed)
DATA = merge(DATA, katvus_int, by = "SID")
######################################################################

#kuidas endale lihtsam ülesanne võtta?
#vaatamae vaid neid lappe, kus üks puuliik on suures enamuses 
#(ja kus üldse puid kasvab)

DATA100 = DATA[DATA$ARV_KOKKU > 100,]

#vaataks vaid neid, kus ühe puuliigi osakaal on piisavalt suur
DATA100_60 = DATA100[DATA100$OSAKAAL > 0.6,]
DATA100_60$PEAPUULIIK == DATA100_60$ENAMUS
#võtame "XX" välja
DATAXX = DATA100_60[DATA100_60$ENAMUS != "XX",]
#huvi pärast, kas:
table(DATAXX$PEAPUULIIK == DATAXX$ENAMUS)

#sellega võiks töötada?

#nonlinear mixed model:
require(tidyr)
require(nlme)
#andmed pikka formaati!

#esmalt andmestik pikka formaati
require(tidyr)
#sat1 = head(sat,50); 
sat1 = sat
sat1 = sat1[,-c(3,4,131)]
require(stringr)
satnames = names(sat1);
band = str_sub(satnames[-(1:3)],-3,-1)
sat = str_sub(satnames[-(1:3)],1,2)
dates = as.Date(gsub("_","",str_sub(satnames[-(1:3)],-12,-4)), format="%Y%m%d")
gsub("_","",str_sub(satnames[-(1:3)],-12,-4))

sat_long = sat1  %>% gather(drug, value, -SID,-cat) #töötab
#nüüd loome mudeli ja siis  arvutame prognoosi igale punktile
#aga enne separate

sat_sep = separate(sat_long, drug, c("sat","ylelend","kp","band"), sep = "_")

#viskame hetkel need välja, mida mõlemas pole:
sats = sat_sep[sat_sep$band %in% c("B2","B3","B4","B02","B03","B04", "B6","B7","B11","B12"),]
sats$band[sats$band == "B02"] = "B2"
sats$band[sats$band == "B03"] = "B3"
sats$band[sats$band == "B04"] = "B4"
sats$band[sats$band == "B11"] = "B6"
sats$band[sats$band == "B12"] = "B7"
#kevad ja sügis?

sats$aaeg[substr(sats$kp,6,6) < 7] = "kevad";sats$aaeg[substr(sats$kp,6,6) > 7] = "sygis"

head(sats)
#ctr = lmeControl(maxIter = 1000)
# sid = lme(value~band+sat+SID+aaeg+ band * sat, 
#           random =~ sat | aaeg1,
#           data = sats, na.action = na.exclude)

m1 = lme(value~band+sat+SID+aaeg+ band * sat, 
         random =~ 1 | ylelend,
         data = sats, na.action = na.exclude)
#iga bandi jaoks eraldi mudel? ka sateliidid eraldi
#või lmer, mis lubab random effecti ka bandi jaoks
summary(m1)
ranef(m1)
fixef(m1)

# na.action = na.omit
#aga see sama, kui kohe ära kustutada puudvad väljad
#mis na.action siin olema peab!?!?!?

#kuupäev on allutatud aastaajale
#ülelennu trajektoor ka juhuslikuks mõjuks!


pred = predict(m1)
sats$pred = pred

pred1 = predict(m1,sats)
sats$pred1 = pred1

require(dplyr)
sats1 = sats %>% group_by(SID, band, aaeg) %>% sample_n(1)

sats2 = merge(sats1, takseer, by = "SID", all.y = T)
test = sats2 %>%  group_by(SID) %>%  sample_n(1)
hist(test$ARV_KOKKU) #võtta alumine piir näiteks 50 m3 hektari kohta
dim(sats2)
sats3 = sats2[sats2$ARV_KOKKU > 25,]
dim(sats3)
sats3 = sats3[sats3$OSAKAAL > 0.5,]
dim(sats3)
DATAX = sats3[sats3$ENAMUS != "XX",]
dim(DATAX)

train = DATAX[DATAX$SID < 1109,]
test = DATAX[DATAX$SID >= 1109,]
table(DATAX$PEAPUULIIK)
#saaks ainult kask, kuusk ja mänd välja võtta...
#kas saaks BOOTSRAPPIDA KMEANS)


#22.10.2018
data1 = DATAX[DATAX$PEAPUULIIK %in% c("KS", "KU","MA"),]

#21.10.2018

#multinomial regression
require(nnet)
m1 = multinom(data = data1, PEAPUULIIK ~ band:pred:aaeg)
summary(m1)
predict(m1, data1, "probs")
table(predict(m1, data1))

##andmed uuesti lühikesse formaati?
#data_wide = spread(data1, c(band,aaeg), pred)
install.packages("reshape2")
require(reshape2)

data_wide <- dcast(data1, SID ~ band + aaeg, value.var="pred1")
dataw = merge(data_wide, takseer, by = "SID", all.x = T)

form = ENAMUS ~ B2_kevad+B2_sygis+B3_kevad+B3_sygis+B4_kevad+B4_sygis+B6_kevad+B6_sygis+B7_kevad+B7_sygis
m_enamus = multinom(data = dataw, formula = form)
#m_pea = multinom(data = dataw, PEAPUULIIK ~ B2_kevad+B2_sygis+B3_kevad+B3_sygis+B4_kevad+B4_sygis+B6_kevad+B6_sygis+B7_kevad+B7_sygis)
head(predict(m1, data1, "probs"))
head(predict(m_enamus, dataw)) #makes sense!

#dataw$ENAMUS_PRED = predict(m_enamus, dataw)
#dataw$PPL_PRED = predict(m_pea, dataw)
#table(dataw$ENAMUS == dataw$ENAMUS_PRED)
#colMax <- apply(predict(m2, dataw, "probs"), 1, function(x) max(x))
tst1 = cbind(dataw[,c("SID", "OSAKAAL", "ENAMUS")], predict(m_enamus, dataw), predict(m_enamus, dataw, "probs"))
nms1 = names(tst1); nms1[4] = "ENAMUS_PRED"; names(tst1) = nms1
confusion = table(tst1$ENAMUS, tst1$ENAMUS_PRED)
confusion 
#5 kaske ja 5 kuuske valesti klassifitseeritud männiks
sum(diag(confusion))/47 #0.596

step(m_enamus)
#leave one out cross valid.


#võttes ka lidari info:
data2 = merge(dataw, korgus_int, by = "SID", all.x = T)  %>% merge(korgus_int, by = "SID", all.x = T) %>% merge(katvus_int, by = "SID", all.x = T)
data2 = subset(data2, select = -Elev.CURT.mean.CUBE) #sellega on probleeme
n1 = names(data2)
n2 = n1[c(2:11,35:199)]
n2
form_lidar = as.formula(paste("ENAMUS", paste(n2, collapse=" + "), sep=" ~ "))
m_lidar = multinom(data = data2, formula = form_lidar)
# Error in nnet.default(X, Y, w, mask = mask, size = 0, skip = TRUE, softmax = TRUE,  : 
#                         too many (1497) weights
#SEE vist puuduva andme probleem?
confusion = table(data2$ENAMUS, predict(m_lidar, data2))
confusion 
sum(diag(confusion))/47

#train and test
trainlidar = data2 %>% group_by(ENAMUS) %>% sample_frac(0.6) #28
testlidar = data2[!(data2$SID %in% trainlidar$SID),] #19
mlidtrain = multinom(data = trainlidar, formula = form_lidar)
confusion = table(testlidar$ENAMUS, predict(mlidtrain, testlidar))
confusion 
sum(diag(confusion))/19 #0.42



#PCA?

LIDARtoPCA = cbind(katvus_int[,-67], korgus_int[,-c(50,51)])
sapply(LIDARtoPCA, class)[sapply(LIDARtoPCA, class) == "factor"]
LIDARtoPCA$Elev.CURT.mean.CUBE #siin väljad puudu
LIDARtoPCA$Elev.CURT.mean.CUBE == "-1.#IND00"

LIDARtoPCA = subset(LIDARtoPCA, select = -Elev.CURT.mean.CUBE)
lpca = prcomp(LIDARtoPCA)

cumvar = cumsum(lpca$sdev**2)/sum(lpca$sdev**2)
cumvar > 0.999 #5 esimest
lpca5 = data.frame(lpca$x[,1:5])
lpca5$SID = katvus_int$SID

lpca11 = data.frame(lpca$x[,1:11]) #99.9% hajuvusest kirjeldatud
lpca11$SID = katvus_int$SID

#merge
data3 = merge(dataw, lpca5, by = "SID", all.x = T)
names(data3)
data11 = merge(dataw, lpca11, by = "SID", all.x = T)
names(data11)

n3 = names(data3)[c(2:11,35:39)]
n11 = names(data11)[c(2:11,35:45)]
formula_lidar = as.formula(paste("ENAMUS", paste(n3, collapse=" + "), sep=" ~ "))
formula11 = as.formula(paste("ENAMUS", paste(n11, collapse=" + "), sep=" ~ "))

m_lidar = multinom(data = data3, formula = formula_lidar)
tst1$ENAMUS_PRED_LIDAR = predict(m_lidar, data3)
confusion = table(tst1$ENAMUS, tst1$ENAMUS_PRED_LIDAR)
confusion 
sum(diag(confusion))/47 #0.83

#sama asi, kui 11 PC-d
m11 = multinom(data = data11, formula = formula11)
tst1$ENAMUS11 = predict(m11, data11)
confusion = table(tst1$ENAMUS, tst1$ENAMUS11)
confusion 
sum(diag(confusion))/47 #1


options(scipen = 100)
round(predict(m_lidar, data3, "probs"), digits = 3)
tst1 = cbind(tst1, round(predict(m_lidar, data3, "probs"), digits = 3))

round(predict(m_lidar, data11, "probs"), digits = 3)



#faktortunnus multinomis?
muld = data.frame(muld = sat$muld)
muld$SID = sat$SID

#muld1 = muld %>% group_by(SID) %>% sample_n(1)
#muld1 = muld %>% group_by(SID) %>% table() %>% which.max()
x = table(muld[muld$SID==122,])
#which.max(table(muld[muld$SID==122,]))
muld[muld$SID==122,][,table(muld[muld$SID==122,])]


library(dplyr)
# muld1 = muld %>% 
#   group_by(SID) %>% 
#   mutate(cmn = rownames(table())[1]) %>%
#   ungroup() #%>% 
#   #%>%
#   #group_by(SID) %>%
#   #sample_n(1) %>%
#   #subset(select = -N)

#muld1 = muld1 %>% group_by(SID) %>% table() %>% rownames()[1]
# fun_cmn <- function(x){
#   tbl <- table(x$???)
#   x$freq <- rep(names(tbl)[which.max(tbl)],nrow(x))
#   x
# }
# require(plyr)
# ddply(muld,fun=fun_cmn)
muld1 = muld %>% group_by(SID) %>% sample_n(1)

data4 = merge(data11, muld1, all.x = T)
formula4 = as.formula(ENAMUS ~ B2_kevad + B2_sygis + B3_kevad + B3_sygis + B4_kevad + 
                        B4_sygis + B6_kevad + B6_sygis + B7_kevad + B7_sygis + PC1 + 
                        PC2 + PC3 + PC4 + PC5 + factor(muld))
m4 = multinom(data = data4, formula = formula4)

tst1$ENAMUS_LID_MULD = predict(m4, data4)
confusion = table(tst1$ENAMUS, tst1$ENAMUS_LID_MULD)
confusion 
sum(diag(confusion))/47 #1

tst1 = cbind(tst1, round(predict(m4, data4, "probs"), digits = 3))
#tõenäosused 1 :D,
table(data4$muld)
#kui võtta 48, 61 ja "teised"?
data4$muldx = data4$muld; data4$muldx[data4$muld != 48 & data4$muld != 61] = 999

formula5 = as.formula(ENAMUS ~ B2_kevad + B2_sygis + B3_kevad + B3_sygis + B4_kevad + 
                        B4_sygis + B6_kevad + B6_sygis + B7_kevad + B7_sygis + PC1 + 
                        PC2 + PC3 + PC4 + PC5 + factor(muldx))
m5 = multinom(data = data4, formula = formula5)

tst1$ENAMUS_LID_MULDx = predict(m5, data4)
confusion = table(tst1$ENAMUS, tst1$ENAMUS_LID_MULDx)
confusion 
sum(diag(confusion))/47 #1
tst1 = cbind(tst1, round(predict(m5, data4, "probs"), digits = 3))

train = data4 %>% group_by(ENAMUS) %>% sample_frac(0.6) #28
test = data4[!(data4$SID %in% train$SID),] #19

m_train = multinom(data = train, formula = formula5)
test$ENAMUS_pred = predict(m_train, test)
confusion = table(test$ENAMUS, test$ENAMUS_pred)
confusion

round(predict(m_train, train, "probs"), 5)
round(predict(m_train, test, "probs"), 5)#mulla infot olen hetkel valesti kasutanud


#### mudel 3 train ja test peal? ####
m_lidar_train = multinom(data = train, formula = formula_lidar)
test$ENAMUS_PRED_LIDAR = predict(m_lidar_train, test)
round(predict(m_lidar_train, test, "probs"),3)
confusion = table(test$ENAMUS, test$ENAMUS_PRED_LIDAR)
confusion 
sum(diag(confusion))/19 #0.211

##11 PCA-ga mudel train ja test peal:
m11_train = multinom(data = train, formula = formula11)
test$ENAMUS_PRED11 = predict(m11_train, test)
round(predict(m11_train, test, "probs"),3)
confusion = table(test$ENAMUS, test$ENAMUS_PRED11)
confusion 
sum(diag(confusion))/19 #0.368

#knn ilma mullata
require(class)
knn1 = knn(train = train[,n3], test = test[,n3], cl = train$ENAMUS, k = 3)
class = data.frame(true = test$ENAMUS, knnclass = knn1)
table(class) #???

#11PCA
knn11 = knn(train = train[,n11], test = test[,n11], cl = train$ENAMUS, k = 3)
class11 = data.frame(true = test$ENAMUS, knnclass = knn11)
table(class11) #???

####################  23.10.2018  ##################
#mudelid loopis

bands = unique(sats$band)
satikad = unique(sats$sat)
#aga sateliitne ka vaja
# for(band in bands){
#   for(satel in satikad){
#     data_band = sats[sats$band == band & sats$sat == satel,]
#     model = lme(value ~ SID + aaeg, random =~ 1 | ylelend, data = data_band, na.action = na.exclude)
#     sats$pred_indvd_model[sats$band == band & sats$sat == satel] = predict(model, data_band)
#     }
#   }

for(band in bands){

    data_band = sats[sats$band == band & sats$sat %in% c("S2AL1C", "S2BL1C"),]
    model = lme(value ~ SID + aaeg, random =~ 1 | ylelend, data = data_band, na.action = na.exclude)
    sats$pred_indvd_model[sats$band == band & sats$sat %in% c("S2AL1C", "S2BL1C")] = predict(model, data_band)
    
    data_band = sats[sats$band == band & sats$sat == "LC08",]
    model = lme(value ~ SID + aaeg, random =~ 1 | ylelend, data = data_band, na.action = na.exclude)
    sats$pred_indvd_model[sats$band == band & sats$sat == "LC08"] = predict(model, data_band)
  
}



#24.11

#LOOCV

require(class)
knn5cv = knn.cv(train = data4[,n3], cl = data4$ENAMUS, k = 3)
class5 = data.frame(true = data4$ENAMUS, knnclass = knn5cv)
table(class5) 

knn11cv = knn.cv(train = data4[,n11], cl = data4$ENAMUS, k = 3)
class11 = data.frame(true = data4$ENAMUS, knnclass = knn5cv)
table(class11) #pole mingit vahet



