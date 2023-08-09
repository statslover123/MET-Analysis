library(dplyr)
library(tidyverse)
library(stringr)
library(lme4)
library(emmeans)

## psuedo lsd calc
lsd_cal<-function(t_value,mse,df){t_value*sqrt(mse*(2/df))}
  
##file you are reading in to analyze
raw<-read.csv('C:/Users/schmu/Downloads/LND ALL DATA.csv')

raw<-raw %>% 
  filter(!(str_detect(Experiment.Name, 'XX|xx')))
##what you are naming output
csv_nameyield<-paste('Lind_fouryear','yield.csv')

raw2<-raw %>%
  filter(str_detect(Year, '2019|2020|2021|2023'))
rm(raw)
rawHARD<-raw2 %>%
  filter(str_detect(Experiment.Name, 'Hard'))
rawSOFT<-raw2 %>%
  filter(str_detect(Experiment.Name, 'SW'))
rawHERB<-raw2 %>%
  filter(str_detect(Name1, 'CL+|CLP|AX|LCS Shine (Check)|LCS Shine'))
rawHERB<-rawHERB[!grepl("2023 SWW", rawHERB$Experiment.Name),]
rawHERB[rawHERB=="LCS Shine (Check)"] <- "LCS Shine"


hardlist<- rawHARD %>% group_by(Name1) %>% summarise(n=n_distinct(Year)) %>% 
  filter(n>=length(unique(rawHARD$Year))) %>% pull(Name1)
rawHARD<-rawHARD[rawHARD$Name1 %in% hardlist,]
softlist<- rawSOFT %>% group_by(Name1) %>% summarise(n=n_distinct(Year)) %>% 
  filter(n>=length(unique(rawSOFT$Year))) %>% pull(Name1)
rawSOFT<-rawSOFT[rawSOFT$Name1 %in% softlist,]
herblist<- rawHERB %>% group_by(Name1) %>% summarise(n=n_distinct(Year)) %>% 
  filter(n>=length(unique(rawHERB$Year))) %>% pull(Name1)
rawHERB<-rawHERB[rawHERB$Name1 %in% herblist,]

#df2<-df2[!(df2$yld_bua=="-9" ),]
rawHARD$Year<-as.factor(rawHARD$Year)
rawSOFT$Year<-as.factor(rawSOFT$Year)
rawHERB$Year<-as.factor(rawHERB$Year)

rawHARD$BLOC<-as.factor(rawHARD$BLOC)
rawSOFT$BLOC<-as.factor(rawSOFT$BLOC)
rawHERB$BLOC<-as.factor(rawHERB$BLOC)

hardyield<-rawHARD[rawHARD$YLD_BUA> 0,]
softyield<-rawSOFT[rawSOFT$YLD_BUA> 0,]
herbyield<-rawHERB[rawHERB$YLD_BUA> 0,]



## random effect work
yieldherbLMer<-lmer(YLD_BUA~Name1+(1|Experiment.Name)+(1|BLOC:Experiment.Name),data=herbyield)
summary(yieldherbLMer)
ranef(yieldherbLMer)
library(lattice)
r<-ranef(yieldherbLMer, conVar=TRUE)
lattice::dotplot(r, scales = list(x =list(relation = 'free')))
yieldherbLMer2<-lmer(YLD_BUA~Name1+(1|Experiment.Name)+(1|Experiment.Name:BLOC),data=herbyield)
summary(yieldherbLMer2)
ranef(yieldherbLMer2)

re<-ranef(yieldherbLMer2, conVar=TRUE)
lattice::dotplot(re, scales = list(x =list(relation = 'free')))
yieldherbresult<-summary(emmeans(yieldherbLMer,specs='Name1'))
yieldherbresult$cv<-sqrt(mean(abs(residuals(yieldherbLMer)^2)))/mean(yieldherbresult$emmean)*100


countherb<-herbyield %>% count(Name1, sort = TRUE)
countherbadd<-na.omit(countherb)
countherb<-round(mean(countherb$n),digits=1)
errorherb_df<-df.residual(yieldherbLMer)
therb<-abs(qt(p = .05, df = errorherb_df))
mse_herb<-(mean(residuals(yieldherbLMer)^2))


yieldherbresult$lsd<-lsd_cal(therb,mse_herb,countherb)
yieldherbresult$trial<-'herb'
yieldherbresult<-merge(yieldherbresult,countherbadd, by="Name1")
##soft
yieldsoftLMer<-lmer(YLD_BUA~Name1+(1|Year)+(1|BLOC:Year),data=softyield)
yieldsoftresult<-summary(emmeans(yieldsoftLMer,specs='Name1'))
yieldsoftresult$cv<-sqrt(mean(abs(residuals(yieldsoftLMer)^2)))/mean(yieldsoftresult$emmean)*100
countsoft<-softyield %>% count(Name1, sort = TRUE)
countsoftadd<-na.omit(countsoft)
countsoft<-round(mean(countsoft$n),digits=1)
errorsoft_df<-df.residual(yieldsoftLMer)
tsoft<-abs(qt(p = .05, df = errorsoft_df))
mse_soft<-(mean(residuals(yieldsoftLMer)^2))

yieldsoftresult$lsd<-lsd_cal(tsoft,mse_soft,countsoft)
yieldsoftresult$trial<-'soft'
yieldsoftresult<-merge(yieldsoftresult,countsoftadd, by="Name1")

##hard
yieldhardLMer<-lmer(YLD_BUA~Name1+(1|Year)+(1|BLOC:Year),data=hardyield)
yieldhardresult<-summary(emmeans(yieldhardLMer,specs='Name1'))
yieldhardresult$cv<-sqrt(mean(abs(residuals(yieldhardLMer)^2)))/mean(yieldhardresult$emmean)*100
counthard<-hardyield %>% count(Name1, sort = TRUE)
counthardadd<-na.omit(counthard)
counthard<-round(mean(counthard$n),digits=1)
errorhard_df<-df.residual(yieldhardLMer)
thard<-abs(qt(p = .05, df = errorhard_df))
mse_hard<-(mean(residuals(yieldhardLMer)^2))
yieldhardresult$lsd<-lsd_cal(thard,mse_hard,counthard)
yieldhardresult$trial<-'hard'
yieldhardresult<-merge(yieldhardresult,counthardadd, by="Name1")

yield<-rbind(yieldhardresult,yieldsoftresult,yieldherbresult)

yield = subset(yield, select = c(Name1, emmean, cv,lsd,trial,n))
write.csv(yield,csv_nameyield)
rm(list=setdiff(ls(), c("rawHARD",'rawSOFT','rawHERB','lsd_cal')))

##tw
hardtw<-rawHARD[rawHARD$TW> 0,]
softtw<-rawSOFT[rawSOFT$TW> 0,]
herbtw<-rawHERB[rawHERB$TW> 0,]



## random effect work
library(merTools)
twherbLMer<-lmer(TW~Name1+(1|Year)+(1|BLOC:Year),data=herbtw)
twherbresult<-summary(emmeans(twherbLMer,specs='Name1'))
twherbresult$cv<-sqrt(mean(abs(residuals(twherbLMer)^2)))/mean(twherbresult$emmean)*100


countherb<-herbtw %>% count(Name1, sort = TRUE)
countherb<-round(mean(countherb$n),digits=1)
errorherb_df<-df.residual(twherbLMer)
therb<-abs(qt(p = .05, df = errorherb_df))
mse_herb<-(mean(residuals(twherbLMer)^2))
twherbresult$lsd<-lsd_cal(therb,mse_herb,countherb)
twherbresult$trial<-'herb'

##soft
twsoftLMer<-lmer(TW~Name1+(1|Year)+(1|BLOC:Year),data=softtw)
twsoftresult<-summary(emmeans(twsoftLMer,specs='Name1'))
twsoftresult$cv<-sqrt(mean(abs(residuals(twsoftLMer)^2)))/mean(twsoftresult$emmean)*100
countsoft<-softtw %>% count(Name1, sort = TRUE)
countsoft<-round(mean(countsoft$n),digits=1)
errorsoft_df<-df.residual(twsoftLMer)
tsoft<-abs(qt(p = .05, df = errorsoft_df))
mse_soft<-(mean(residuals(twsoftLMer)^2))
twsoftresult$lsd<-lsd_cal(tsoft,mse_soft,countsoft)
twsoftresult$trial<-'soft'

##hard
twhardLMer<-lmer(TW~Name1+(1|Year)+(1|BLOC:Year),data=hardtw)
twhardresult<-summary(emmeans(twhardLMer,specs='Name1'))
twhardresult$cv<-sqrt(mean(abs(residuals(twhardLMer)^2)))/mean(twhardresult$emmean)*100
counthard<-hardtw %>% count(Name1, sort = TRUE)
counthard<-round(mean(counthard$n),digits=1)
errorhard_df<-df.residual(twhardLMer)
thard<-abs(qt(p = .05, df = errorhard_df))
mse_hard<-(mean(residuals(twhardLMer)^2))
twhardresult$lsd<-lsd_cal(thard,mse_hard,counthard)
twhardresult$trial<-'hard'

tw<-rbind(twhardresult,twsoftresult,twherbresult)
tw = subset(tw, select = c(Name1, emmean, cv,lsd,trial))


##PROTEIN

hardprotein<-rawHARD[rawHARD$PROTEIN> 0,]
softprotein<-rawSOFT[rawSOFT$PROTEIN> 0,]
herbprotein<-rawHERB[rawHERB$PROTEIN> 0,]



## random effect work
proteinherbLMer<-lmer(PROTEIN~Name1+(1|Year)+(1|BLOC:Year),data=herbprotein)
proteinherbresult<-summary(emmeans(proteinherbLMer,specs='Name1'))
proteinherbresult$cv<-sqrt(mean(abs(residuals(proteinherbLMer)^2)))/mean(proteinherbresult$emmean)*100


countherb<-herbprotein %>% count(Name1, sort = TRUE)
countherb<-round(mean(countherb$n),digits=1)
errorherb_df<-df.residual(proteinherbLMer)
therb<-abs(qt(p = .05, df = errorherb_df))
mse_herb<-(mean(residuals(proteinherbLMer)^2))
proteinherbresult$lsd<-lsd_cal(therb,mse_herb,countherb)
proteinherbresult$trial<-'herb'

##soft
proteinsoftLMer<-lmer(PROTEIN~Name1+(1|Year)+(1|BLOC:Year),data=softprotein)
proteinsoftresult<-summary(emmeans(proteinsoftLMer,specs='Name1'))
proteinsoftresult$cv<-sqrt(mean(abs(residuals(proteinsoftLMer)^2)))/mean(proteinsoftresult$emmean)*100
countsoft<-softprotein %>% count(Name1, sort = TRUE)
countsoft<-round(mean(countsoft$n),digits=1)
errorsoft_df<-df.residual(proteinsoftLMer)
tsoft<-abs(qt(p = .05, df = errorsoft_df))
mse_soft<-(mean(residuals(proteinsoftLMer)^2))
proteinsoftresult$lsd<-lsd_cal(tsoft,mse_soft,countsoft)
proteinsoftresult$trial<-'soft'

##hard
proteinhardLMer<-lmer(PROTEIN~Name1+(1|Year)+(1|BLOC:Year),data=hardprotein)
proteinhardresult<-summary(emmeans(proteinhardLMer,specs='Name1'))
proteinhardresult$cv<-sqrt(mean(abs(residuals(proteinhardLMer)^2)))/mean(proteinhardresult$emmean)*100
counthard<-hardprotein %>% count(Name1, sort = TRUE)
counthard<-round(mean(counthard$n),digits=1)
errorhard_df<-df.residual(proteinhardLMer)
thard<-abs(qt(p = .05, df = errorhard_df))
mse_hard<-(mean(residuals(proteinhardLMer)^2))
proteinhardresult$lsd<-lsd_cal(thard,mse_hard,counthard)
proteinhardresult$trial<-'hard'

protein<-rbind(proteinhardresult,proteinsoftresult,proteinherbresult)
protein = subset(protein, select = c(Name1, emmean, cv,lsd,trial))
