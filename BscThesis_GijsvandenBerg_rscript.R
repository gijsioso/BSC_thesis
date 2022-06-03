#install.packages("lidR")
#install.packages("writexl")
library(lidR)
library(ggplot2)
library(writexl)
library(raster)
library(sp)
library(rgdal)

######AHN4data

###AHN4_oude_exclosures
lasAHN4ex <- readLAS("AHN4_oud2.las")
metricsAHN4ex<- cloud_metrics(lasAHN4ex, func = .stdmetrics)
#normalize height
nlasAHN4ex <- normalize_height(lasAHN4ex, knnidw())
hist(filter_ground(nlasAHN4ex)$Z, breaks = seq(-0.6, 0.6, 0.01), main = "", xlab = "Elevation")
metricsAHN4exnormalized<- cloud_metrics(nlasAHN4ex, func = .stdmetrics)

#aboveground
abovegroundAHN4ex <- filter_poi(nlasAHN4ex, Classification == 1L)
abovegroundAHN4ex <- filter_poi(nlasAHN4ex, Classification == 1L & Z<1.50)
vegmetricsAHN4ex<-cloud_metrics(abovegroundAHN4ex, func = .stdmetrics)

##add metrics
min(abovegroundAHN4ex$Z)
varAHN4ex<-var(nlasAHN4ex$Z)  ##calculate variance
covAHN4ex<- vegmetricsAHN4ex$zsd/vegmetricsAHN4ex$zmean  ##calculate cov
vegmetricsAHN4ex<- append(vegmetricsAHN4ex,c(varAHN4ex,covAHN4ex,metricsAHN4exnormalized$pground))  #add variance and cov to metrics list
names(vegmetricsAHN4ex)[37:39]<- c("var","cov","pground")   #add the right names


#create data frame
AHN4exvegdata<- data.frame(c(vegmetricsAHN4ex[1],vegmetricsAHN4ex[2],vegmetricsAHN4ex[3],vegmetricsAHN4ex[13],vegmetricsAHN4ex[18],vegmetricsAHN4ex[23],vegmetricsAHN4ex[24],vegmetricsAHN4ex[26],vegmetricsAHN4ex[27],vegmetricsAHN4ex[37],vegmetricsAHN4ex[38],vegmetricsAHN4ex[39])) ##data frame

write_xlsx(AHN4exvegdata,"C:\\Users\\Gijs\\Documents\\bachelorproject\\AHN4ex_metric.xlsx") #write excelfile
#write.csv(HVR1vegdata,"C:\\Users\\Gijs\\Documents\\bachelorproject\\HVR1_metrics.csv") #write excelfile

#################################################################AHN4 Reference
lasAHN4ref <- readLAS("AHN4_ref.las")
metricsAHN4ref<- cloud_metrics(lasAHN4ref, func = .stdmetrics)
#normalize height
nlasAHN4ref <- normalize_height(lasAHN4ref, knnidw())
hist(filter_ground(nlasAHN4ref)$Z, breaks = seq(-0.6, 0.6, 0.01), main = "", xlab = "Elevation")
metricsAHN4refnormalized<- cloud_metrics(nlasAHN4ref, func = .stdmetrics)

#aboveground
abovegroundAHN4ref <- filter_poi(nlasAHN4ref, Classification == 1L)
vegmetricsAHN4ref<-cloud_metrics(abovegroundAHN4ref, func = .stdmetrics)

##add metrics

varAHN4ref<-var(nlasAHN4ref$Z)  ##calculate variance
covAHN4ref<- vegmetricsAHN4ref$zsd/vegmetricsAHN4ref$zmean  ##calculate cov
vegmetricsAHN4ref<- append(vegmetricsAHN4ref,c(varAHN4ref,covAHN4ref,metricsAHN4refnormalized$pground))  #add variance and cov to metrics list
names(vegmetricsAHN4ref)[37:39]<- c("var","cov","pground")   #add the right names


#create data frame
AHN4refvegdata<- data.frame(c(vegmetricsAHN4ref[1],vegmetricsAHN4ref[2],vegmetricsAHN4ref[3],vegmetricsAHN4ref[13],vegmetricsAHN4ref[18],vegmetricsAHN4ref[23],vegmetricsAHN4ref[24],vegmetricsAHN4ref[26],vegmetricsAHN4ref[27],vegmetricsAHN4ref[37],vegmetricsAHN4ref[38],vegmetricsAHN4ref[39])) ##data frame

write_xlsx(AHN4refvegdata,"C:\\Users\\Gijs\\Documents\\bachelorproject\\AHN4ref_metrics.xlsx") #write excelfile




#####AHN3data

###AHN3_oude_exclosures
lasAHN3ex <- readLAS("AHN3_oud2.las")
metricsAHN3ex<- cloud_metrics(lasAHN3ex, func = .stdmetrics)

#normalize height
nlasAHN3ex <- normalize_height(lasAHN3ex, knnidw())
hist(filter_ground(nlasAHN3ex)$Z, breaks = seq(-0.6, 0.6, 0.01), main = "", xlab = "Elevation")
metricsAHN3exnormalized<- cloud_metrics(nlasAHN3ex, func = .stdmetrics)

#aboveground
abovegroundAHN3ex <- filter_poi(nlasAHN3ex, Classification == 1L)
vegmetricsAHN3ex<-cloud_metrics(abovegroundAHN3ex, func = .stdmetrics)

##add metrics
#min(abovegroundAHN3ex$Z)
varAHN3ex<-var(nlasAHN3ex$Z)  ##calculate variance
covAHN3ex<- vegmetricsAHN3ex$zsd/vegmetricsAHN3ex$zmean  ##calculate cov
vegmetricsAHN3ex<- append(vegmetricsAHN3ex,c(varAHN3ex,covAHN3ex,metricsAHN3exnormalized$pground))  #add variance and cov to metrics list
names(vegmetricsAHN3ex)[37:39]<- c("var","cov","pground")   #add the right names


#create data frame
AHN3exvegdata<- data.frame(c(vegmetricsAHN3ex[1],vegmetricsAHN3ex[2],vegmetricsAHN3ex[3],vegmetricsAHN3ex[13],vegmetricsAHN3ex[18],vegmetricsAHN3ex[23],vegmetricsAHN3ex[24],vegmetricsAHN3ex[26],vegmetricsAHN3ex[27],vegmetricsAHN3ex[37],vegmetricsAHN3ex[38],vegmetricsAHN3ex[39])) ##data frame

write_xlsx(AHN3exvegdata,"C:\\Users\\Gijs\\Documents\\bachelorproject\\AHN3ex_metric.xlsx") #write excelfile


#################################################################AHN3 Reference
lasAHN3ref <- readLAS("AHN3_ref.las")
metricsAHN3ref<- cloud_metrics(lasAHN3ref, func = .stdmetrics)
#normalize height
nlasAHN3ref <- normalize_height(lasAHN3ref, knnidw())
hist(filter_ground(nlasAHN3ref)$Z, breaks = seq(-0.6, 0.6, 0.01), main = "", xlab = "Elevation")
metricsAHN3refnormalized<- cloud_metrics(nlasAHN3ref, func = .stdmetrics)


#aboveground
abovegroundAHN3ref <- filter_poi(nlasAHN3ref, Classification == 1L)
vegmetricsAHN3ref<-cloud_metrics(abovegroundAHN3ref, func = .stdmetrics)

##add metrics
varAHN3ref<-var(nlasAHN3ref$Z)  ##calculate variance
covAHN3ref<- vegmetricsAHN3ref$zsd/vegmetricsAHN3ref$zmean  ##calculate cov
vegmetricsAHN3ref<- append(vegmetricsAHN3ref,c(varAHN3ref,covAHN3ref,metricsAHN3refnormalized$pground))  #add variance and cov to metrics list
names(vegmetricsAHN3ref)[37:39]<- c("var","cov","pground")   #add the right names


#create data frame
AHN3refvegdata<- data.frame(c(vegmetricsAHN3ref[1],vegmetricsAHN3ref[2],vegmetricsAHN3ref[3],vegmetricsAHN3ref[13],vegmetricsAHN3ref[18],vegmetricsAHN3ref[23],vegmetricsAHN3ref[24],vegmetricsAHN3ref[26],vegmetricsAHN3ref[27],vegmetricsAHN3ref[37],vegmetricsAHN3ref[38],vegmetricsAHN3ref[39])) ##data frame

write_xlsx(AHN3refvegdata,"C:\\Users\\Gijs\\Documents\\bachelorproject\\AHN3ref_metrics.xlsx") #write excelfile


###### HV1data
```{r}
lasHV1 <- readLAS("HV1_class.las")
cloud_metrics(lasHV1, func = .stdmetrics)

######Vegetatieonly from cloudcompare
lasHV1veg <- readLAS("HV1_classified_onlyvegetation.las")
metricsHV1veg<- cloud_metrics(lasHV1veg, func = .stdmetrics)
######
#normalize height
nlasHV1 <- normalize_height(lasHV1, knnidw())
hist(filter_ground(nlasHV1)$Z, breaks = seq(-0.6, 0.6, 0.01), main = "", xlab = "Elevation")
metricsHV1normalized<- cloud_metrics(nlasHV1, func = .stdmetrics)


#aboveground
abovegroundHV1 <- filter_poi(nlasHV1, Classification == 1L) ##vegetation points
abovegroundHV1_130cm <- filter_poi(nlasHV1, Classification == 1L& Z<1.3) #points below 130cm
vegmetricsHV1<-cloud_metrics(abovegroundHV1, func = .stdmetrics)
vegmetricsHV1_130cm<-cloud_metrics(abovegroundHV1_130cm, func = .stdmetrics)

abovegroundHV1_130up <- filter_poi(nlasHV1, Classification == 1L& Z>1.3) #points above 130cm
vegmetricsHV1_130above<-cloud_metrics(abovegroundHV1_130up, func = .stdmetrics)
##add metrics
varHV1<-var(nlasHV1$Z)  ##calculate variance
covHV1<- vegmetricsHV1$zsd/vegmetricsHV1$zmean  ##calculate cov
vegmetricsHV1<- append(vegmetricsHV1,c(varHV1,covHV1,metricsHV1normalized$pground))  #add variance and cov to metrics list
names(vegmetricsHV1)[37:39]<- c("var","cov","pground")   #add the right names


#create data frame
HV1vegdata<- data.frame(c(vegmetricsHV1[1],vegmetricsHV1[2],vegmetricsHV1[3],vegmetricsHV1[13],vegmetricsHV1[18],vegmetricsHV1[23],vegmetricsHV1[24],vegmetricsHV1[26],vegmetricsHV1[27],vegmetricsHV1[37],vegmetricsHV1[38],vegmetricsHV1[39])) ##data frame

write_xlsx(HV1vegdata,"C:\\Users\\Gijs\\Documents\\bachelorproject\\HV1_metrics.xlsx") #write excelfile




#####HVR1 data
```{r}
lasHVR1 <- readLAS("HVR2_classified.las")
cloud_metrics(lasHVR1, func = .stdmetrics)

######Vegetatieonly from cloudcompare
lasHVR1veg <- readLAS("HVR2_classified_onlyveg.las")
metricsHVR1veg<- cloud_metrics(lasHVR1veg, func = .stdmetrics)
######
#normalize height
nlasHVR1 <- normalize_height(lasHVR1, knnidw())
hist(filter_ground(nlasHVR1)$Z, breaks = seq(-0.6, 0.6, 0.01), main = "", xlab = "Elevation")
metricsHVR1normalized<- cloud_metrics(nlasHVR1, func = .stdmetrics)

#aboveground
abovegroundHVR1 <- filter_poi(nlasHVR1, Classification == 1L)
vegmetricsHVR1<-cloud_metrics(abovegroundHVR1, func = .stdmetrics_z)

abovegroundHVR1 <- filter_poi(nlasHVR1, Classification == 1L) ##vegetation points
abovegroundHVR1_130cm <- filter_poi(nlasHVR1, Classification == 1L& Z<1.3) #points below 130cm
abovegroundHVR1_130up <- filter_poi(nlasHVR1, Classification == 1L& Z>1.3) #points above 130cm
vegmetricsHVR1<-cloud_metrics(abovegroundHVR1, func = .stdmetrics)
vegmetricsHVR1_130cm<-cloud_metrics(abovegroundHVR1_130cm, func = .stdmetrics)
vegmetricsHVR1_130above<-cloud_metrics(abovegroundHVR1_130up, func = .stdmetrics)
##add metrics
varHVR1<-var(nlasHVR1$Z)  ##calculate variance
covHVR1<- vegmetricsHVR1$zsd/vegmetricsHVR1$zmean  ##calculate cov
vegmetricsHVR1<- append(vegmetricsHVR1,c(varHVR1,covHVR1,metricsHVR1normalized$pground))  #add variance and cov to metrics list
names(vegmetricsHVR1)[37:39]<- c("var","cov","pground")   #add the right names


#create data frame
HVR1vegdata<- data.frame(c(vegmetricsHVR1[1],vegmetricsHVR1[2],vegmetricsHVR1[3],vegmetricsHVR1[13],vegmetricsHVR1[18],vegmetricsHVR1[23],vegmetricsHVR1[24],vegmetricsHVR1[26],vegmetricsHVR1[27],vegmetricsHVR1[37],vegmetricsHVR1[38],vegmetricsHVR1[39])) ##data frame

write_xlsx(HVR1vegdata,"C:\\Users\\Gijs\\Documents\\bachelorproject\\HVR1_metrics.xlsx") #write excelfile
write.csv(HVR1vegdata,"C:\\Users\\Gijs\\Documents\\bachelorproject\\HVR1_metrics.csv") #write excelfile


#####OUD 2 Data

```{r}
lasoud2 <- readLAS("OUD_2_classified.las")
metricsoud2<- cloud_metrics(lasoud2, func = .stdmetrics)

#normalize height
nlasoud2 <- normalize_height(lasoud2, knnidw())
hist(filter_ground(nlasoud2)$Z, breaks = seq(-0.6, 0.6, 0.01), main = "", xlab = "Elevation")
metricsoud2normalized<- cloud_metrics(nlasoud2, func = .stdmetrics)

#aboveground
abovegroundoud2 <- filter_poi(nlasoud2, Classification == 1L)
vegmetricsoud2<-cloud_metrics(abovegroundoud2, func = .stdmetrics)

##add metrics
varoud2<-var(nlasoud2$Z)  ##calculate variance
covoud2<- vegmetricsoud2$zsd/vegmetricsoud2$zmean  ##calculate cov
vegmetricsoud2<- append(vegmetricsoud2,c(varoud2,covoud2,metricsoud2normalized$pground))  #add variance and cov to metrics list
names(vegmetricsoud2)[37:39]<- c("var","cov","pground")   #add the right names


#create data frame
oud2vegdata<- data.frame(c(vegmetricsoud2[1],vegmetricsoud2[2],vegmetricsoud2[3],vegmetricsoud2[13],vegmetricsoud2[18],vegmetricsoud2[23],vegmetricsoud2[24],vegmetricsoud2[26],vegmetricsoud2[27],vegmetricsoud2[37],vegmetricsoud2[38],vegmetricsoud2[39])) ##data frame

write_xlsx(oud2vegdata,"C:\\Users\\Gijs\\Documents\\bachelorproject\\Oud2_metrics.xlsx") #write excelfile
write.csv(HVR1vegdata,"C:\\Users\\Gijs\\Documents\\bachelorproject\\HVR1_metrics.csv") #write excelfile



#####OUD reference Data
```{r}
################################################################OUD_reference
lasoudR <- readLAS("OUD_r_class.las")
metricsoudR<- cloud_metrics(lasoudR, func = .stdmetrics)
#normalize height
nlasoudR <- normalize_height(lasoudR, knnidw())
hist(filter_ground(nlasoudR)$Z, breaks = seq(-0.6, 0.6, 0.01), main = "", xlab = "Elevation")
metricsoudRnormalized<- cloud_metrics(nlasoudR, func = .stdmetrics)

#aboveground
abovegroundoudR <- filter_poi(nlasoudR, Classification == 1L)
vegmetricsoudR<-cloud_metrics(abovegroundoudR, func = .stdmetrics)

##add metrics
varoudR<-var(nlasoudR$Z)  ##calculate variance
covoudR<- vegmetricsoudR$zsd/vegmetricsoudR$zmean  ##calculate cov
vegmetricsoudR<- append(vegmetricsoudR,c(varoudR,covoudR,metricsoudRnormalized$pground))  #add variance and cov to metrics list
names(vegmetricsoudR)[37:39]<- c("var","cov","pground")   #add the right names


#create data frame
oudRvegdata<- data.frame(c(vegmetricsoudR[1],vegmetricsoudR[2],vegmetricsoudR[3],vegmetricsoudR[13],vegmetricsoudR[18],vegmetricsoudR[23],vegmetricsoudR[24],vegmetricsoudR[26],vegmetricsoudR[27],vegmetricsoudR[37],vegmetricsoudR[38],vegmetricsoudR[39])) ##data frame

write_xlsx(oudRvegdata,"C:\\Users\\Gijs\\Documents\\bachelorproject\\OudR_metrics.xlsx") #write excelfile
#write.csv(HVR1vegdata,"C:\\Users\\Gijs\\Documents\\bachelorproject\\HVR1_metrics.csv") #write excelfile



##change analysis
```{r}

#install.packages("data.table")
library( data.table )


###HV1
HV1classnosoil<- raster("HV1_Classnosoil.tif")
HV1classnosoil<- as.data.frame(HV1classnosoil)
percHVnosoil1<-setDT( HV1classnosoil )[ , 100 * .N / nrow(HV1classnosoil), by = HV1classnosoil ]
write_xlsx(percHVnosoil1,"C:\\Users\\Gijs\\Documents\\bachelorproject\\HV1_percnosoil.xlsx") #write excelfile

##HVR1
HVR1class<- raster("HVR1_class.tif")
HVR1class<- as.data.frame(HVR1class)
percHVR1<-setDT( HVR1class )[ , 100 * .N / nrow(HVR1class), by = HVR1class ]
write_xlsx(percHVR1,"C:\\Users\\Gijs\\Documents\\bachelorproject\\HVR1_perc.xlsx") #write excelfile

######################oud
###oud2
oud2class<- raster("oud_2_class.tif")
oud2class<- as.data.frame(oud2class)
percoud2<-setDT( oud2class )[ , 100 * .N / nrow(oud2class), by = oud2class ]
write_xlsx(percHV1,"C:\\Users\\Gijs\\Documents\\bachelorproject\\HV1_perc.xlsx") #write excelfile
###oudr
oudrclass<- raster("oud_ref_class.tif")
oudrclass<- as.data.frame(oudrclass)
percoudr<-setDT( oudrclass )[ , 100 * .N / nrow(oudrclass), by = oudrclass ]
write_xlsx(percoudr,"C:\\Users\\Gijs\\Documents\\bachelorproject\\oudr_perc.xlsx") #write excelfile
