

##################################
###DemoCrime Project R Code#######
##################################

#Accessing vdem data############################################################
load('vdem.Rdata')

#Restricting the dataset to the 2011-2016 period
table(vdem$year)
vdem = vdem[vdem$year %in% 2011:2016,]

#Keeping only a handful of variables
vars = c("country_name", "year", "e_pop", "v2x_libdem", "v2x_polyarchy", 
         "v2x_partipdem", "v2x_delibdem", "v2x_egaldem", "e_gdp",
         "e_ti_cpi")
vdem = vdem[,vars]

#Accessing the unemployment data
unemployment = read.csv('unemployment_rates 2011-21.csv')
#Dropping cases beyond 2017
unemployment = unemployment[,1:7]

#Accessing the European sourcebook data#########################################
assault = read.csv('Assault.csv')
total = read.csv('Total.csv')
drug = read.csv('Drug.csv')
fraud = read.csv('Fraud.csv')
homicide = read.csv('Homicide.csv')
robbery = read.csv('Robbery.csv')
sex = read.csv('Sex.csv')
theft = read.csv('Theft.csv')

#Restricting vdem to the same countries seen in the sourcebook
table(vdem$country_name)
assault$COUNTRY

#Using the same name for Macedonia
assault$country_name = ifelse(assault$COUNTRY=="TFYR of Macedonia", "Macedonia", assault$COUNTRY)
assault$COUNTRY=NULL
total$country_name = ifelse(total$COUNTRY=="TFYR of Macedonia", "Macedonia", total$COUNTRY)
total$COUNTRY=NULL
drug$country_name = ifelse(drug$COUNTRY=="TFYR of Macedonia", "Macedonia", drug$COUNTRY)
drug$COUNTRY=NULL
fraud$country_name = ifelse(fraud$COUNTRY=="TFYR of Macedonia", "Macedonia", fraud$COUNTRY)
fraud$COUNTRY=NULL
homicide$country_name = ifelse(homicide$COUNTRY=="TFYR of Macedonia", "Macedonia", homicide$COUNTRY)
homicide$COUNTRY=NULL
robbery$country_name = ifelse(robbery$COUNTRY=="TFYR of Macedonia", "Macedonia", robbery$COUNTRY)
robbery$COUNTRY=NULL
sex$country_name = ifelse(sex$COUNTRY=="TFYR of Macedonia", "Macedonia", sex$COUNTRY)
sex$COUNTRY=NULL
theft$country_name = ifelse(theft$COUNTRY=="TFYR of Macedonia", "Macedonia", theft$COUNTRY)
theft$COUNTRY=NULL

#Listwise deletion
assault[assault == "#VALUE!"] = NA
total[total == "#VALUE!"] = NA
drug[drug == "#VALUE!"] = NA
fraud[fraud == "#VALUE!"] = NA
homicide[homicide == "#VALUE!"] = NA
robbery[robbery == "#VALUE!"] = NA
sex[sex == "#VALUE!"] = NA
theft[theft == "#VALUE!"] = NA

#Setting crime rates as numeric
assault[, 1:6] = apply(assault[, 1:6], 2, as.numeric)
total[, 1:6] = apply(total[, 1:6], 2, as.numeric)
drug[, 1:6] = apply(drug[, 1:6], 2, as.numeric)
fraud[, 1:6] = apply(fraud[, 1:6], 2, as.numeric)
homicide[, 1:6] = apply(homicide[, 1:6], 2, as.numeric)
robbery[, 1:6] = apply(robbery[, 1:6], 2, as.numeric)
sex[, 1:6] = apply(sex[, 1:6], 2, as.numeric)
theft[, 1:6] = apply(theft[, 1:6], 2, as.numeric)


#Combining the three UK jurisdictions
assault[47,] = c(sum(assault$X2011[44:46]), sum(assault$X2012[44:46]), sum(assault$X2013[44:46]), 
                 sum(assault$X2014[44:46]), sum(assault$X2015[44:46]), sum(assault$X2016[44:46]), "United Kingdom")
assault = assault[-c(44:46),]
total[47,] = c(sum(total$X2011[44:46]), sum(total$X2012[44:46]), sum(total$X2013[44:46]), 
                 sum(total$X2014[44:46]), sum(total$X2015[44:46]), sum(total$X2016[44:46]), "United Kingdom")
total = total[-c(44:46),]
drug[47,] = c(sum(drug$X2011[44:46]), sum(drug$X2012[44:46]), sum(drug$X2013[44:46]), 
                 sum(drug$X2014[44:46]), sum(drug$X2015[44:46]), sum(drug$X2016[44:46]), "United Kingdom")
drug = drug[-c(44:46),]
fraud[47,] = c(sum(fraud$X2011[44:46]), sum(fraud$X2012[44:46]), sum(fraud$X2013[44:46]), 
                 sum(fraud$X2014[44:46]), sum(fraud$X2015[44:46]), sum(fraud$X2016[44:46]), "United Kingdom")
fraud = fraud[-c(44:46),]
homicide[47,] = c(sum(homicide$X2011[44:46]), sum(homicide$X2012[44:46]), sum(homicide$X2013[44:46]), 
                 sum(homicide$X2014[44:46]), sum(homicide$X2015[44:46]), sum(homicide$X2016[44:46]), "United Kingdom")
homicide = homicide[-c(44:46),]
robbery[47,] = c(sum(robbery$X2011[44:46]), sum(robbery$X2012[44:46]), sum(robbery$X2013[44:46]), 
                 sum(robbery$X2014[44:46]), sum(robbery$X2015[44:46]), sum(robbery$X2016[44:46]), "United Kingdom")
robbery = robbery[-c(44:46),]
sex[47,] = c(sum(sex$X2011[44:46]), sum(sex$X2012[44:46]), sum(sex$X2013[44:46]), 
                 sum(sex$X2014[44:46]), sum(sex$X2015[44:46]), sum(sex$X2016[44:46]), "United Kingdom")
sex = sex[-c(44:46),]
theft[47,] = c(sum(theft$X2011[44:46]), sum(theft$X2012[44:46]), sum(theft$X2013[44:46]), 
                 sum(theft$X2014[44:46]), sum(theft$X2015[44:46]), sum(theft$X2016[44:46]), "United Kingdom")
theft = theft[-c(44:46),]

#Fixing variable labels for years
names(assault) = c("2011", "2012", "2013", "2014", "2015", "2016", "country_name")
names(total) = c("2011", "2012", "2013", "2014", "2015", "2016", "country_name")
names(drug) = c("2011", "2012", "2013", "2014", "2015", "2016", "country_name")
names(fraud) = c("2011", "2012", "2013", "2014", "2015", "2016", "country_name")
names(homicide) = c("2011", "2012", "2013", "2014", "2015", "2016", "country_name")
names(robbery) = c("2011", "2012", "2013", "2014", "2015", "2016", "country_name")
names(sex) = c("2011", "2012", "2013", "2014", "2015", "2016", "country_name")
names(theft) = c("2011", "2012", "2013", "2014", "2015", "2016", "country_name")
names(unemployment) = c("country_name", "2011", "2012", "2013", "2014", "2015", "2016")

#Turning unemployment and crime data into a person period format
library(tidyr)
assault_long = pivot_longer(assault, cols = -country_name, names_to = "year", values_to = "assault")
total_long = pivot_longer(total, cols = -country_name, names_to = "year", values_to = "total")
drug_long = pivot_longer(drug, cols = -country_name, names_to = "year", values_to = "drug")
fraud_long = pivot_longer(fraud, cols = -country_name, names_to = "year", values_to = "fraud")
homicide_long = pivot_longer(homicide, cols = -country_name, names_to = "year", values_to = "homicide")
robbery_long = pivot_longer(robbery, cols = -country_name, names_to = "year", values_to = "robbery")
sex_long = pivot_longer(sex, cols = -country_name, names_to = "year", values_to = "sex")
theft_long = pivot_longer(theft, cols = -country_name, names_to = "year", values_to = "theft")
unemployment_long = pivot_longer(unemployment, cols = -country_name, names_to = "year", values_to = "unemployment")

#setting the dependent variables as numeric
assault_long$assault = as.numeric(assault_long$assault) 
total_long$total = as.numeric(total_long$total) 
drug_long$drug = as.numeric(drug_long$drug) 
fraud_long$fraud = as.numeric(fraud_long$fraud) 
homicide_long$homicide = as.numeric(homicide_long$homicide) 
robbery_long$robbery = as.numeric(robbery_long$robbery) 
sex_long$sex = as.numeric(sex_long$sex) 
theft_long$theft = as.numeric(theft_long$theft) 
unemployment_long$unemployment = as.numeric(unemployment_long$unemployment) 

#Merging the three datasets#######################################################
#Finding countries that are not called the same in the sourcebook and vdem
sort(intersect(unique(vdem$country_name), unique(assault_long$country_name)))
sort(unique(assault_long$country_name))
sort(unique(vdem$country_name))
#Bosnia and the Czech Republic are called differently in vdem
vdem$country_name[823:828] = "Bosnia-Herzegovina"
vdem$country_name[859:864] = "Czech Republic"

#The merging
data = merge(merge(merge(merge(merge(merge(merge(merge(
             assault_long, total_long, by=c("country_name", "year")), 
             drug_long, by=c("country_name", "year")), 
             fraud_long, by=c("country_name", "year")), 
             homicide_long, by=c("country_name", "year")), 
             robbery_long, by=c("country_name", "year")), 
             sex_long, by=c("country_name", "year")), 
             theft_long, by=c("country_name", "year")),
             vdem, by=c("country_name", "year"))

#Distribution of dependent variables
hist(data$assault)
hist(log(data$assault))
hist(data$total)
hist(log(data$total))
hist(data$drug)
hist(log(data$drug))
hist(data$fraud)
hist(log(data$fraud))
hist(data$homicide)
hist(log(data$homicide))
hist(data$robbery)
hist(log(data$robbery))
data[35,]
data$robbery[35] = 0.0001  #I change the 0 in robbery for Bosnia so I can run the log-linear model later.
hist(data$sex)
hist(log(data$sex))
data[data$sex == 0,]
data[54,]
data$sex[54] = 0.0001  #I change the 0 in sex for Cyprus so I can run the log-linear model later.
hist(data$theft)
hist(log(data$theft))

#Other descriptives
#The dataset is composed of 44 countries measured across 6 years, for a total of 264 observations.
options(scipen = 999)
summary(data)  #I do not include e_ti_cpi because it has lots of missing cases.
mean = sapply(data, function(x) mean(x, na.rm = TRUE))
#write.csv(mean, " means.csv")
min = sapply(data, function(x) min(x, na.rm = TRUE))
#write.csv(min, " minima.csv")
max = sapply(data, function(x) max(x, na.rm = TRUE))
#write.csv(max, " maxima.csv")
sd = sapply(data, function(x) sd(x, na.rm = TRUE))
#write.csv(sd, " standard_deviations.csv")
N = sapply(data, function(x) length(na.omit(x)))
#write.csv(N, " sample_size.csv")


#Fixed effects model
library(plm)
#Convert your_data to a panel data frame
pdata = pdata.frame(data, index = "country_name")

#Finding out the share of between country variability
library(lme4)
library(performance)
summary(lmer(log(assault) ~ (1|country_name), data = data))
icc(lmer(log(total) ~ (1|country_name), data = data))
icc(lmer(log(drug) ~ (1|country_name), data = data))
icc(lmer(log(fraud) ~ (1|country_name), data = data))
icc(lmer(log(homicide) ~ (1|country_name), data = data))
icc(lmer(log(robbery) ~ (1|country_name), data = data))
icc(lmer(log(sex) ~ (1|country_name), data = data))
icc(lmer(log(theft) ~ (1|country_name), data = data))

#Models###################################################

#Model-1 models
#Assault
assaultpolyarchy1 = lm(log(assault) ~ v2x_polyarchy , data = data)
summary(assaultpolyarchy1)
assaultlibdem1 = lm(log(assault) ~ v2x_libdem , data = data)
summary(assaultlibdem1)
assaultpartipdem1 = lm(log(assault) ~ v2x_partipdem , data = data)
summary(assaultpartipdem1)
assaultdelibdem1 = lm(log(assault)~ v2x_delibdem , data = data)
summary(assaultdelibdem1)
assaultegaldem1 = lm(log(assault) ~ v2x_egaldem , data = data)
summary(assaultegaldem1)
#Total
totalpolyarchy1 = lm(log(total) ~ v2x_polyarchy , data = data)
summary(totalpolyarchy1)
totallibdem1 = lm(log(total) ~ v2x_libdem , data = data)
summary(totallibdem1)
totalpartipdem1 = lm(log(total) ~ v2x_partipdem , data = data)
summary(totalpartipdem1)
totaldelibdem1 = lm(log(total)~ v2x_delibdem , data = data)
summary(totaldelibdem1)
totalegaldem1 = lm(log(total) ~ v2x_egaldem , data = data)
summary(totalegaldem1)
#Drug
drugpolyarchy1 = lm(log(drug) ~ v2x_polyarchy , data = data)
summary(drugpolyarchy1)
druglibdem1 = lm(log(drug) ~ v2x_libdem , data = data)
summary(druglibdem1)
drugpartipdem1 = lm(log(drug) ~ v2x_partipdem , data = data)
summary(drugpartipdem1)
drugdelibdem1 = lm(log(drug)~ v2x_delibdem , data = data)
summary(drugdelibdem1)
drugegaldem1 = lm(log(drug) ~ v2x_egaldem , data = data)
summary(drugegaldem1)
#Fraud
fraudpolyarchy1 = lm(log(fraud) ~ v2x_polyarchy , data = data)
summary(fraudpolyarchy1)
fraudlibdem1 = lm(log(fraud) ~ v2x_libdem , data = data)
summary(fraudlibdem1)
fraudpartipdem1 = lm(log(fraud) ~ v2x_partipdem , data = data)
summary(fraudpartipdem1)
frauddelibdem1 = lm(log(fraud)~ v2x_delibdem , data = data)
summary(frauddelibdem1)
fraudegaldem1 = lm(log(fraud) ~ v2x_egaldem , data = data)
summary(fraudegaldem1)
#Homicide
homicidepolyarchy1 = lm(log(homicide) ~ v2x_polyarchy , data = data)
summary(homicidepolyarchy1)
homicidelibdem1 = lm(log(homicide) ~ v2x_libdem , data = data)
summary(homicidelibdem1)
homicidepartipdem1 = lm(log(homicide) ~ v2x_partipdem , data = data)
summary(homicidepartipdem1)
homicidedelibdem1 = lm(log(homicide)~ v2x_delibdem , data = data)
summary(homicidedelibdem1)
homicideegaldem1 = lm(log(homicide) ~ v2x_egaldem , data = data)
summary(homicideegaldem1)
#Robbery
robberypolyarchy1 = lm(log(robbery) ~ v2x_polyarchy , data = data)
summary(robberypolyarchy1)
robberylibdem1 = lm(log(robbery) ~ v2x_libdem , data = data)
summary(robberylibdem1)
robberypartipdem1 = lm(log(robbery) ~ v2x_partipdem , data = data)
summary(robberypartipdem1)
robberydelibdem1 = lm(log(robbery)~ v2x_delibdem , data = data)
summary(robberydelibdem1)
robberyegaldem1 = lm(log(robbery) ~ v2x_egaldem , data = data)
summary(robberyegaldem1)
#Sex
sexpolyarchy1 = lm(log(sex) ~ v2x_polyarchy , data = data)
summary(sexpolyarchy1)
sexlibdem1 = lm(log(sex) ~ v2x_libdem , data = data)
summary(sexlibdem1)
sexpartipdem1 = lm(log(sex) ~ v2x_partipdem , data = data)
summary(sexpartipdem1)
sexdelibdem1 = lm(log(sex)~ v2x_delibdem , data = data)
summary(sexdelibdem1)
sexegaldem1 = lm(log(sex) ~ v2x_egaldem , data = data)
summary(sexegaldem1)
#Theft
theftpolyarchy1 = lm(log(theft) ~ v2x_polyarchy , data = data)
summary(theftpolyarchy1)
theftlibdem1 = lm(log(theft) ~ v2x_libdem , data = data)
summary(theftlibdem1)
theftpartipdem1 = lm(log(theft) ~ v2x_partipdem , data = data)
summary(theftpartipdem1)
theftdelibdem1 = lm(log(theft)~ v2x_delibdem , data = data)
summary(theftdelibdem1)
theftegaldem1 = lm(log(theft) ~ v2x_egaldem , data = data)
summary(theftegaldem1)

#Model-2 models
#Assault
assaultpolyarchy2 = plm(log(assault) ~ v2x_polyarchy , data = pdata, effect = "individual")
summary(assaultpolyarchy2)
assaultlibdem2 = plm(log(assault) ~ v2x_libdem , data = pdata, effect = "individual")
summary(assaultlibdem2)
assaultpartipdem2 = plm(log(assault) ~ v2x_partipdem , data = pdata, effect = "individual")
summary(assaultpartipdem2)
assaultdelibdem2 = plm(log(assault) ~ v2x_delibdem , data = pdata, effect = "individual")
summary(assaultdelibdem2)
assaultegaldem2 = plm(log(assault) ~ v2x_egaldem , data = pdata, effect = "individual")
summary(assaultegaldem2)
#Total
totalpolyarchy2 = plm(log(total) ~ v2x_polyarchy , data = pdata, effect = "individual")
summary(totalpolyarchy2)
totallibdem2 = plm(log(total) ~ v2x_libdem , data = pdata, effect = "individual")
summary(totallibdem2)
totalpartipdem2 = plm(log(total) ~ v2x_partipdem , data = pdata, effect = "individual")
summary(totalpartipdem2)
totaldelibdem2 = plm(log(total) ~ v2x_delibdem , data = pdata, effect = "individual")
summary(totaldelibdem2)
totalegaldem2 = plm(log(total) ~ v2x_egaldem , data = pdata, effect = "individual")
summary(totalegaldem2)
#Drug
drugpolyarchy2 = plm(log(drug) ~ v2x_polyarchy , data = pdata, effect = "individual")
summary(drugpolyarchy2)
druglibdem2 = plm(log(drug) ~ v2x_libdem , data = pdata, effect = "individual")
summary(druglibdem2)
drugpartipdem2 = plm(log(drug) ~ v2x_partipdem , data = pdata, effect = "individual")
summary(drugpartipdem2)
drugdelibdem2 = plm(log(drug) ~ v2x_delibdem , data = pdata, effect = "individual")
summary(drugdelibdem2)
drugegaldem2 = plm(log(drug) ~ v2x_egaldem , data = pdata, effect = "individual")
summary(drugegaldem2)
#Fraud
fraudpolyarchy2 = plm(log(fraud) ~ v2x_polyarchy , data = pdata, effect = "individual")
summary(fraudpolyarchy2)
fraudlibdem2 = plm(log(fraud) ~ v2x_libdem , data = pdata, effect = "individual")
summary(fraudlibdem2)
fraudpartipdem2 = plm(log(fraud) ~ v2x_partipdem , data = pdata, effect = "individual")
summary(fraudpartipdem2)
frauddelibdem2 = plm(log(fraud) ~ v2x_delibdem , data = pdata, effect = "individual")
summary(frauddelibdem2)
fraudegaldem2 = plm(log(fraud) ~ v2x_egaldem , data = pdata, effect = "individual")
summary(fraudegaldem2)
#Homicide
homicidepolyarchy2 = plm(log(homicide) ~ v2x_polyarchy , data = pdata, effect = "individual")
summary(homicidepolyarchy2)
homicidelibdem2 = plm(log(homicide) ~ v2x_libdem , data = pdata, effect = "individual")
summary(homicidelibdem2)
homicidepartipdem2 = plm(log(homicide) ~ v2x_partipdem , data = pdata, effect = "individual")
summary(homicidepartipdem2)
homicidedelibdem2 = plm(log(homicide) ~ v2x_delibdem , data = pdata, effect = "individual")
summary(homicidedelibdem2)
homicideegaldem2 = plm(log(homicide) ~ v2x_egaldem , data = pdata, effect = "individual")
summary(homicideegaldem2)
#Robbery
robberypolyarchy2 = plm(log(robbery) ~ v2x_polyarchy , data = pdata, effect = "individual")
summary(robberypolyarchy2)
robberylibdem2 = plm(log(robbery) ~ v2x_libdem , data = pdata, effect = "individual")
summary(robberylibdem2)
robberypartipdem2 = plm(log(robbery) ~ v2x_partipdem , data = pdata, effect = "individual")
summary(robberypartipdem2)
robberydelibdem2 = plm(log(robbery) ~ v2x_delibdem , data = pdata, effect = "individual")
summary(robberydelibdem2)
robberyegaldem2 = plm(log(robbery) ~ v2x_egaldem , data = pdata, effect = "individual")
summary(robberyegaldem2)
#Sex
sexpolyarchy2 = plm(log(sex) ~ v2x_polyarchy , data = pdata, effect = "individual")
summary(sexpolyarchy2)
sexlibdem2 = plm(log(sex) ~ v2x_libdem , data = pdata, effect = "individual")
summary(sexlibdem2)
sexpartipdem2 = plm(log(sex) ~ v2x_partipdem , data = pdata, effect = "individual")
summary(sexpartipdem2)
sexdelibdem2 = plm(log(sex) ~ v2x_delibdem , data = pdata, effect = "individual")
summary(sexdelibdem2)
sexegaldem2 = plm(log(sex) ~ v2x_egaldem , data = pdata, effect = "individual")
summary(sexegaldem2)
#Theft
theftpolyarchy2 = plm(log(theft) ~ v2x_polyarchy , data = pdata, effect = "individual")
summary(theftpolyarchy2)
theftlibdem2 = plm(log(theft) ~ v2x_libdem , data = pdata, effect = "individual")
summary(theftlibdem2)
theftpartipdem2 = plm(log(theft) ~ v2x_partipdem , data = pdata, effect = "individual")
summary(theftpartipdem2)
theftdelibdem2 = plm(log(theft) ~ v2x_delibdem , data = pdata, effect = "individual")
summary(theftdelibdem2)
theftegaldem2 = plm(log(theft) ~ v2x_egaldem , data = pdata, effect = "individual")
summary(theftegaldem2)

#Summarising models
library(sjPlot)

#Assault
tab_model(assaultpolyarchy1, assaultpolyarchy2,  
          file = " polyarchy_assault.html")
tab_model(assaultlibdem1, assaultlibdem2,  
          file = " libdem_assault.html")
tab_model(assaultpartipdem1, assaultpartipdem2, 
          file = " partipdem_assault.html")
tab_model(assaultdelibdem1, assaultdelibdem2,  
          file = " delibdem_assault.html")
tab_model(assaultegaldem1, assaultegaldem2, 
          file = " egaldem_assault.html")

#Total
tab_model(totalpolyarchy1, totalpolyarchy2,  
          file = " polyarchy_total.html")
tab_model(totallibdem1, totallibdem2,  
          file = " libdem_total.html")
tab_model(totalpartipdem1, totalpartipdem2, 
          file = " partipdem_total.html")
tab_model(totaldelibdem1, totaldelibdem2,  
          file = " delibdem_total.html")
tab_model(totalegaldem1, totalegaldem2, 
          file = " egaldem_total.html")

#Drug
tab_model(drugpolyarchy1, drugpolyarchy2,  
          file = " polyarchy_drug.html")
tab_model(druglibdem1, druglibdem2,  
          file = " libdem_drug.html")
tab_model(drugpartipdem1, drugpartipdem2, 
          file = " partipdem_drug.html")
tab_model(drugdelibdem1, drugdelibdem2,  
          file = " delibdem_drug.html")
tab_model(drugegaldem1, drugegaldem2, 
          file = " egaldem_drug.html")

#Fraud
tab_model(fraudpolyarchy1, fraudpolyarchy2,  
          file = " polyarchy_fraud.html")
tab_model(fraudlibdem1, fraudlibdem2,  
          file = " libdem_fraud.html")
tab_model(fraudpartipdem1, fraudpartipdem2, 
          file = " partipdem_fraud.html")
tab_model(frauddelibdem1, frauddelibdem2,  
          file = " delibdem_fraud.html")
tab_model(fraudegaldem1, fraudegaldem2, 
          file = " egaldem_fraud.html")

#Homicide
tab_model(homicidepolyarchy1, homicidepolyarchy2,  
          file = " polyarchy_homicide.html")
tab_model(homicidelibdem1, homicidelibdem2,  
          file = " libdem_homicide.html")
tab_model(homicidepartipdem1, homicidepartipdem2, 
          file = " partipdem_homicide.html")
tab_model(homicidedelibdem1, homicidedelibdem2,  
          file = " delibdem_homicide.html")
tab_model(homicideegaldem1, homicideegaldem2, 
          file = " egaldem_homicide.html")

#Robbery
tab_model(robberypolyarchy1, robberypolyarchy2,  
          file = " polyarchy_robbery.html")
tab_model(robberylibdem1, robberylibdem2,  
          file = " libdem_robbery.html")
tab_model(robberypartipdem1, robberypartipdem2, 
          file = " partipdem_robbery.html")
tab_model(robberydelibdem1, robberydelibdem2,  
          file = " delibdem_robbery.html")
tab_model(robberyegaldem1, robberyegaldem2, 
          file = " egaldem_robbery.html")

#Sex
tab_model(sexpolyarchy1, sexpolyarchy2,  
          file = " polyarchy_sex.html")
tab_model(sexlibdem1, sexlibdem2,  
          file = " libdem_sex.html")
tab_model(sexpartipdem1, sexpartipdem2, 
          file = " partipdem_sex.html")
tab_model(sexdelibdem1, sexdelibdem2,  
          file = " delibdem_sex.html")
tab_model(sexegaldem1, sexegaldem2, 
          file = " egaldem_sex.html")

#Theft
tab_model(theftpolyarchy1, theftpolyarchy2,  
          file = " polyarchy_theft.html")
tab_model(theftlibdem1, theftlibdem2,  
          file = " libdem_theft.html")
tab_model(theftpartipdem1, theftpartipdem2, 
          file = " partipdem_theft.html")
tab_model(theftdelibdem1, theftdelibdem2,  
          file = " delibdem_theft.html")
tab_model(theftegaldem1, theftegaldem2, 
          file = " egaldem_theft.html")


#Summarising results as a table#################################################

#Assault
coefs_1 = c(summary(assaultpolyarchy1)$coefficients[2], summary(assaultlibdem1)$coefficients[2], 
            summary(assaultpartipdem1)$coefficients[2], summary(assaultdelibdem1)$coefficients[2],
            summary(assaultegaldem1)$coefficients[2])
coefs_2 = c(summary(assaultpolyarchy2)$coefficients[1], summary(assaultlibdem2)$coefficients[1], 
            summary(assaultpartipdem2)$coefficients[1], summary(assaultdelibdem2)$coefficients[1],
            summary(assaultegaldem2)$coefficients[1])
loCI_1 = c(summary(assaultpolyarchy1)$coefficients[2] - 1.96*summary(assaultpolyarchy1)$coefficients[2,2], 
           summary(assaultlibdem1)$coefficients[2] - 1.96*summary(assaultlibdem1)$coefficients[2,2], 
            summary(assaultpartipdem1)$coefficients[2] - 1.96*summary(assaultpartipdem1)$coefficients[2,2],
            summary(assaultdelibdem1)$coefficients[2] - 1.96*summary(assaultdelibdem1)$coefficients[2,2],
            summary(assaultegaldem1)$coefficients[2] - 1.96*summary(assaultegaldem1)$coefficients[2,2])
loCI_2 = c(summary(assaultpolyarchy2)$coefficients[1] - 1.96*summary(assaultpolyarchy2)$coefficients[2], 
           summary(assaultlibdem2)$coefficients[1] - 1.96*summary(assaultlibdem2)$coefficients[2], 
           summary(assaultpartipdem2)$coefficients[1] - 1.96*summary(assaultpartipdem2)$coefficients[2],
           summary(assaultdelibdem2)$coefficients[1] - 1.96*summary(assaultdelibdem2)$coefficients[2],
           summary(assaultegaldem2)$coefficients[1] - 1.96*summary(assaultegaldem2)$coefficients[2])
hiCI_1 = c(summary(assaultpolyarchy1)$coefficients[2] + 1.96*summary(assaultpolyarchy1)$coefficients[2,2], 
           summary(assaultlibdem1)$coefficients[2] + 1.96*summary(assaultlibdem1)$coefficients[2,2], 
           summary(assaultpartipdem1)$coefficients[2] + 1.96*summary(assaultpartipdem1)$coefficients[2,2],
           summary(assaultdelibdem1)$coefficients[2] + 1.96*summary(assaultdelibdem1)$coefficients[2,2],
           summary(assaultegaldem1)$coefficients[2] + 1.96*summary(assaultegaldem1)$coefficients[2,2])
hiCI_2 = c(summary(assaultpolyarchy2)$coefficients[1] + 1.96*summary(assaultpolyarchy2)$coefficients[2], 
           summary(assaultlibdem2)$coefficients[1] + 1.96*summary(assaultlibdem2)$coefficients[2], 
           summary(assaultpartipdem2)$coefficients[1] + 1.96*summary(assaultpartipdem2)$coefficients[2],
           summary(assaultdelibdem2)$coefficients[1] + 1.96*summary(assaultdelibdem2)$coefficients[2],
           summary(assaultegaldem2)$coefficients[1] + 1.96*summary(assaultegaldem2)$coefficients[2])
names = c("polyarchy","libdem","partipdem","delibdem","egaldem")
assault = as.data.frame(cbind(coefs_1, coefs_2))
loCI = as.data.frame(cbind(loCI_1, loCI_2))
hiCI = as.data.frame(cbind(hiCI_1, hiCI_2))

#Assault plot
assault = as.data.frame(cbind(c(assault[1,], assault[2,], assault[3,], assault[4,], assault[5,]),
                              cbind(c(loCI[1,], loCI[2,], loCI[3,], loCI[4,], loCI[5,]),  
                                    cbind(c(hiCI[1,], hiCI[2,], hiCI[3,], hiCI[4,], hiCI[5,]),           
                                          rep(names, each = 2), rep(1:2, 5)))))
names(assault) = c("coefficient", "loCI", "hiCI", "index", "model")
library(ggplot2)
library(viridis)
assault$coefficient = as.numeric(assault$coefficient)
assault$loCI = as.numeric(assault$loCI)
assault$hiCI = as.numeric(assault$hiCI)
assault$index = as.character(assault$index)
assault$model = as.character(assault$model)

assault = ggplot(assault, aes(fill=model, y=coefficient, x=index)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=loCI, ymax=hiCI), 
                position=position_dodge(width=0.9),
                width=0.3, colour="gray", alpha=0.5, size=1) + 
  scale_fill_manual(values = viridis_pal(option = "D")(2), # 2 colors from the viridis palette
                    labels = c("1" = "simple regression", "2" = "fixed effects")) + 
  labs(fill = "Model", 
       title = "Assault",
       x = "quality of democracy index",
       y = "estimated association with the log of crime rates") +
  scale_x_discrete(labels = c("Deliberative", "Egalitarian", "Liberal", "Participatory", "Electoral")) +
  theme_minimal()
#Exporting the graph in high resolution
library(ggpubr)
ggexport(assault, width=2000, height=1600, res=320,
         filename=" assault.jpeg")

#Total
coefs_1 = c(summary(totalpolyarchy1)$coefficients[2], summary(totallibdem1)$coefficients[2], 
            summary(totalpartipdem1)$coefficients[2], summary(totaldelibdem1)$coefficients[2],
            summary(totalegaldem1)$coefficients[2])
coefs_2 = c(summary(totalpolyarchy2)$coefficients[1], summary(totallibdem2)$coefficients[1], 
            summary(totalpartipdem2)$coefficients[1], summary(totaldelibdem2)$coefficients[1],
            summary(totalegaldem2)$coefficients[1])
loCI_1 = c(summary(totalpolyarchy1)$coefficients[2] - 1.96*summary(totalpolyarchy1)$coefficients[2,2], 
           summary(totallibdem1)$coefficients[2] - 1.96*summary(totallibdem1)$coefficients[2,2], 
           summary(totalpartipdem1)$coefficients[2] - 1.96*summary(totalpartipdem1)$coefficients[2,2],
           summary(totaldelibdem1)$coefficients[2] - 1.96*summary(totaldelibdem1)$coefficients[2,2],
           summary(totalegaldem1)$coefficients[2] - 1.96*summary(totalegaldem1)$coefficients[2,2])
loCI_2 = c(summary(totalpolyarchy2)$coefficients[1] - 1.96*summary(totalpolyarchy2)$coefficients[2], 
           summary(totallibdem2)$coefficients[1] - 1.96*summary(totallibdem2)$coefficients[2], 
           summary(totalpartipdem2)$coefficients[1] - 1.96*summary(totalpartipdem2)$coefficients[2],
           summary(totaldelibdem2)$coefficients[1] - 1.96*summary(totaldelibdem2)$coefficients[2],
           summary(totalegaldem2)$coefficients[1] - 1.96*summary(totalegaldem2)$coefficients[2])
hiCI_1 = c(summary(totalpolyarchy1)$coefficients[2] + 1.96*summary(totalpolyarchy1)$coefficients[2,2], 
           summary(totallibdem1)$coefficients[2] + 1.96*summary(totallibdem1)$coefficients[2,2], 
           summary(totalpartipdem1)$coefficients[2] + 1.96*summary(totalpartipdem1)$coefficients[2,2],
           summary(totaldelibdem1)$coefficients[2] + 1.96*summary(totaldelibdem1)$coefficients[2,2],
           summary(totalegaldem1)$coefficients[2] + 1.96*summary(totalegaldem1)$coefficients[2,2])
hiCI_2 = c(summary(totalpolyarchy2)$coefficients[1] + 1.96*summary(totalpolyarchy2)$coefficients[2], 
           summary(totallibdem2)$coefficients[1] + 1.96*summary(totallibdem2)$coefficients[2], 
           summary(totalpartipdem2)$coefficients[1] + 1.96*summary(totalpartipdem2)$coefficients[2],
           summary(totaldelibdem2)$coefficients[1] + 1.96*summary(totaldelibdem2)$coefficients[2],
           summary(totalegaldem2)$coefficients[1] + 1.96*summary(totalegaldem2)$coefficients[2])
names = c("polyarchy","libdem","partipdem","delibdem","egaldem")
total = as.data.frame(cbind(coefs_1, coefs_2))
loCI = as.data.frame(cbind(loCI_1, loCI_2))
hiCI = as.data.frame(cbind(hiCI_1, hiCI_2))


#Total plot
total = as.data.frame(cbind(c(total[1,], total[2,], total[3,], total[4,], total[5,]),
                            cbind(c(loCI[1,], loCI[2,], loCI[3,], loCI[4,], loCI[5,]),  
                                  cbind(c(hiCI[1,], hiCI[2,], hiCI[3,], hiCI[4,], hiCI[5,]),           
                                        rep(names, each = 2), rep(1:2, 5)))))
names(total) = c("coefficient", "loCI", "hiCI", "index", "model")
library(ggplot2)
library(viridis)
total$coefficient = as.numeric(total$coefficient)
total$loCI = as.numeric(total$loCI)
total$hiCI = as.numeric(total$hiCI)
total$index = as.character(total$index)
total$model = as.character(total$model)

total = ggplot(total, aes(fill=model, y=coefficient, x=index)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=loCI, ymax=hiCI), 
                position=position_dodge(width=0.9),
                width=0.3, colour="gray", alpha=0.5, size=1) + 
  scale_fill_manual(values = viridis_pal(option = "D")(2), # 2 colors from the viridis palette
                    labels = c("1" = "simple regression", "2" = "fixed effects")) + 
  labs(fill = "Model", 
       title = "All crime",
       x = "quality of democracy index",
       y = "estimated association with the log of crime rates") +
  scale_x_discrete(labels = c("Deliberative", "Egalitarian", "Liberal", "Participatory", "Electoral")) +
  theme_minimal()
#Exporting the graph in high resolution
library(ggpubr)
ggexport(total, width=2000, height=1600, res=320,
         filename=" total.jpeg")


#Drug
coefs_1 = c(summary(drugpolyarchy1)$coefficients[2], summary(druglibdem1)$coefficients[2], 
            summary(drugpartipdem1)$coefficients[2], summary(drugdelibdem1)$coefficients[2],
            summary(drugegaldem1)$coefficients[2])
coefs_2 = c(summary(drugpolyarchy2)$coefficients[1], summary(druglibdem2)$coefficients[1], 
            summary(drugpartipdem2)$coefficients[1], summary(drugdelibdem2)$coefficients[1],
            summary(drugegaldem2)$coefficients[1])
loCI_1 = c(summary(drugpolyarchy1)$coefficients[2] - 1.96*summary(drugpolyarchy1)$coefficients[2,2], 
           summary(druglibdem1)$coefficients[2] - 1.96*summary(druglibdem1)$coefficients[2,2], 
           summary(drugpartipdem1)$coefficients[2] - 1.96*summary(drugpartipdem1)$coefficients[2,2],
           summary(drugdelibdem1)$coefficients[2] - 1.96*summary(drugdelibdem1)$coefficients[2,2],
           summary(drugegaldem1)$coefficients[2] - 1.96*summary(drugegaldem1)$coefficients[2,2])
loCI_2 = c(summary(drugpolyarchy2)$coefficients[1] - 1.96*summary(drugpolyarchy2)$coefficients[2], 
           summary(druglibdem2)$coefficients[1] - 1.96*summary(druglibdem2)$coefficients[2], 
           summary(drugpartipdem2)$coefficients[1] - 1.96*summary(drugpartipdem2)$coefficients[2],
           summary(drugdelibdem2)$coefficients[1] - 1.96*summary(drugdelibdem2)$coefficients[2],
           summary(drugegaldem2)$coefficients[1] - 1.96*summary(drugegaldem2)$coefficients[2])
hiCI_1 = c(summary(drugpolyarchy1)$coefficients[2] + 1.96*summary(drugpolyarchy1)$coefficients[2,2], 
           summary(druglibdem1)$coefficients[2] + 1.96*summary(druglibdem1)$coefficients[2,2], 
           summary(drugpartipdem1)$coefficients[2] + 1.96*summary(drugpartipdem1)$coefficients[2,2],
           summary(drugdelibdem1)$coefficients[2] + 1.96*summary(drugdelibdem1)$coefficients[2,2],
           summary(drugegaldem1)$coefficients[2] + 1.96*summary(drugegaldem1)$coefficients[2,2])
hiCI_2 = c(summary(drugpolyarchy2)$coefficients[1] + 1.96*summary(drugpolyarchy2)$coefficients[2], 
           summary(druglibdem2)$coefficients[1] + 1.96*summary(druglibdem2)$coefficients[2], 
           summary(drugpartipdem2)$coefficients[1] + 1.96*summary(drugpartipdem2)$coefficients[2],
           summary(drugdelibdem2)$coefficients[1] + 1.96*summary(drugdelibdem2)$coefficients[2],
           summary(drugegaldem2)$coefficients[1] + 1.96*summary(drugegaldem2)$coefficients[2])
names = c("polyarchy","libdem","partipdem","delibdem","egaldem")
drug = as.data.frame(cbind(coefs_1, coefs_2))
loCI = as.data.frame(cbind(loCI_1, loCI_2))
hiCI = as.data.frame(cbind(hiCI_1, hiCI_2))

#Drug plot
drug = as.data.frame(cbind(c(drug[1,], drug[2,], drug[3,], drug[4,], drug[5,]),
                           cbind(c(loCI[1,], loCI[2,], loCI[3,], loCI[4,], loCI[5,]),  
                                 cbind(c(hiCI[1,], hiCI[2,], hiCI[3,], hiCI[4,], hiCI[5,]),           
                                       rep(names, each = 2), rep(1:2, 5)))))
names(drug) = c("coefficient", "loCI", "hiCI", "index", "model")
library(ggplot2)
library(viridis)
drug$coefficient = as.numeric(drug$coefficient)
drug$loCI = as.numeric(drug$loCI)
drug$hiCI = as.numeric(drug$hiCI)
drug$index = as.character(drug$index)
drug$model = as.character(drug$model)

drug = ggplot(drug, aes(fill=model, y=coefficient, x=index)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=loCI, ymax=hiCI), 
                position=position_dodge(width=0.9),
                width=0.3, colour="gray", alpha=0.5, size=1) + 
  scale_fill_manual(values = viridis_pal(option = "D")(2), # 2 colors from the viridis palette
                    labels = c("1" = "simple regression", "2" = "fixed effects")) + 
  labs(fill = "Model", 
       title = "Drugs",
       x = "quality of democracy index",
       y = "estimated association with the log of crime rates") +
  scale_x_discrete(labels = c("Deliberative", "Egalitarian", "Liberal", "Participatory", "Electoral")) +
  theme_minimal()
#Exporting the graph in high resolution
library(ggpubr)
ggexport(drug, width=2000, height=1600, res=320,
         filename=" drug.jpeg")


#Fraud
coefs_1 = c(summary(fraudpolyarchy1)$coefficients[2], summary(fraudlibdem1)$coefficients[2], 
            summary(fraudpartipdem1)$coefficients[2], summary(frauddelibdem1)$coefficients[2],
            summary(fraudegaldem1)$coefficients[2])
coefs_2 = c(summary(fraudpolyarchy2)$coefficients[1], summary(fraudlibdem2)$coefficients[1], 
            summary(fraudpartipdem2)$coefficients[1], summary(frauddelibdem2)$coefficients[1],
            summary(fraudegaldem2)$coefficients[1])
loCI_1 = c(summary(fraudpolyarchy1)$coefficients[2] - 1.96*summary(fraudpolyarchy1)$coefficients[2,2], 
           summary(fraudlibdem1)$coefficients[2] - 1.96*summary(fraudlibdem1)$coefficients[2,2], 
           summary(fraudpartipdem1)$coefficients[2] - 1.96*summary(fraudpartipdem1)$coefficients[2,2],
           summary(frauddelibdem1)$coefficients[2] - 1.96*summary(frauddelibdem1)$coefficients[2,2],
           summary(fraudegaldem1)$coefficients[2] - 1.96*summary(fraudegaldem1)$coefficients[2,2])
loCI_2 = c(summary(fraudpolyarchy2)$coefficients[1] - 1.96*summary(fraudpolyarchy2)$coefficients[2], 
           summary(fraudlibdem2)$coefficients[1] - 1.96*summary(fraudlibdem2)$coefficients[2], 
           summary(fraudpartipdem2)$coefficients[1] - 1.96*summary(fraudpartipdem2)$coefficients[2],
           summary(frauddelibdem2)$coefficients[1] - 1.96*summary(frauddelibdem2)$coefficients[2],
           summary(fraudegaldem2)$coefficients[1] - 1.96*summary(fraudegaldem2)$coefficients[2])
hiCI_1 = c(summary(fraudpolyarchy1)$coefficients[2] + 1.96*summary(fraudpolyarchy1)$coefficients[2,2], 
           summary(fraudlibdem1)$coefficients[2] + 1.96*summary(fraudlibdem1)$coefficients[2,2], 
           summary(fraudpartipdem1)$coefficients[2] + 1.96*summary(fraudpartipdem1)$coefficients[2,2],
           summary(frauddelibdem1)$coefficients[2] + 1.96*summary(frauddelibdem1)$coefficients[2,2],
           summary(fraudegaldem1)$coefficients[2] + 1.96*summary(fraudegaldem1)$coefficients[2,2])
hiCI_2 = c(summary(fraudpolyarchy2)$coefficients[1] + 1.96*summary(fraudpolyarchy2)$coefficients[2], 
           summary(fraudlibdem2)$coefficients[1] + 1.96*summary(fraudlibdem2)$coefficients[2], 
           summary(fraudpartipdem2)$coefficients[1] + 1.96*summary(fraudpartipdem2)$coefficients[2],
           summary(frauddelibdem2)$coefficients[1] + 1.96*summary(frauddelibdem2)$coefficients[2],
           summary(fraudegaldem2)$coefficients[1] + 1.96*summary(fraudegaldem2)$coefficients[2])
names = c("polyarchy","libdem","partipdem","delibdem","egaldem")
fraud = as.data.frame(cbind(coefs_1, coefs_2))
loCI = as.data.frame(cbind(loCI_1, loCI_2))
hiCI = as.data.frame(cbind(hiCI_1, hiCI_2))

#Fraud plot
fraud = as.data.frame(cbind(c(fraud[1,], fraud[2,], fraud[3,], fraud[4,], fraud[5,]),
                            cbind(c(loCI[1,], loCI[2,], loCI[3,], loCI[4,], loCI[5,]),  
                                  cbind(c(hiCI[1,], hiCI[2,], hiCI[3,], hiCI[4,], hiCI[5,]),           
                                        rep(names, each = 2), rep(1:2, 5)))))
names(fraud) = c("coefficient", "loCI", "hiCI", "index", "model")
library(ggplot2)
library(viridis)
fraud$coefficient = as.numeric(fraud$coefficient)
fraud$loCI = as.numeric(fraud$loCI)
fraud$hiCI = as.numeric(fraud$hiCI)
fraud$index = as.character(fraud$index)
fraud$model = as.character(fraud$model)

fraud = ggplot(fraud, aes(fill=model, y=coefficient, x=index)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=loCI, ymax=hiCI), 
                position=position_dodge(width=0.9),
                width=0.3, colour="gray", alpha=0.5, size=1) + 
  scale_fill_manual(values = viridis_pal(option = "D")(2), # 2 colors from the viridis palette
                    labels = c("1" = "simple regression", "2" = "fixed effects")) + 
  labs(fill = "Model", 
       title = "Fraud",
       x = "quality of democracy index",
       y = "estimated association with the log of crime rates") +
  scale_x_discrete(labels = c("Deliberative", "Egalitarian", "Liberal", "Participatory", "Electoral")) +
  theme_minimal()
#Exporting the graph in high resolution
library(ggpubr)
ggexport(fraud, width=2000, height=1600, res=320,
         filename=" fraud.jpeg")


#Homicide
coefs_1 = c(summary(homicidepolyarchy1)$coefficients[2], summary(homicidelibdem1)$coefficients[2], 
            summary(homicidepartipdem1)$coefficients[2], summary(homicidedelibdem1)$coefficients[2],
            summary(homicideegaldem1)$coefficients[2])
coefs_2 = c(summary(homicidepolyarchy2)$coefficients[1], summary(homicidelibdem2)$coefficients[1], 
            summary(homicidepartipdem2)$coefficients[1], summary(homicidedelibdem2)$coefficients[1],
            summary(homicideegaldem2)$coefficients[1])
loCI_1 = c(summary(homicidepolyarchy1)$coefficients[2] - 1.96*summary(homicidepolyarchy1)$coefficients[2,2], 
           summary(homicidelibdem1)$coefficients[2] - 1.96*summary(homicidelibdem1)$coefficients[2,2], 
           summary(homicidepartipdem1)$coefficients[2] - 1.96*summary(homicidepartipdem1)$coefficients[2,2],
           summary(homicidedelibdem1)$coefficients[2] - 1.96*summary(homicidedelibdem1)$coefficients[2,2],
           summary(homicideegaldem1)$coefficients[2] - 1.96*summary(homicideegaldem1)$coefficients[2,2])
loCI_2 = c(summary(homicidepolyarchy2)$coefficients[1] - 1.96*summary(homicidepolyarchy2)$coefficients[2], 
           summary(homicidelibdem2)$coefficients[1] - 1.96*summary(homicidelibdem2)$coefficients[2], 
           summary(homicidepartipdem2)$coefficients[1] - 1.96*summary(homicidepartipdem2)$coefficients[2],
           summary(homicidedelibdem2)$coefficients[1] - 1.96*summary(homicidedelibdem2)$coefficients[2],
           summary(homicideegaldem2)$coefficients[1] - 1.96*summary(homicideegaldem2)$coefficients[2])
hiCI_1 = c(summary(homicidepolyarchy1)$coefficients[2] + 1.96*summary(homicidepolyarchy1)$coefficients[2,2], 
           summary(homicidelibdem1)$coefficients[2] + 1.96*summary(homicidelibdem1)$coefficients[2,2], 
           summary(homicidepartipdem1)$coefficients[2] + 1.96*summary(homicidepartipdem1)$coefficients[2,2],
           summary(homicidedelibdem1)$coefficients[2] + 1.96*summary(homicidedelibdem1)$coefficients[2,2],
           summary(homicideegaldem1)$coefficients[2] + 1.96*summary(homicideegaldem1)$coefficients[2,2])
hiCI_2 = c(summary(homicidepolyarchy2)$coefficients[1] + 1.96*summary(homicidepolyarchy2)$coefficients[2], 
           summary(homicidelibdem2)$coefficients[1] + 1.96*summary(homicidelibdem2)$coefficients[2], 
           summary(homicidepartipdem2)$coefficients[1] + 1.96*summary(homicidepartipdem2)$coefficients[2],
           summary(homicidedelibdem2)$coefficients[1] + 1.96*summary(homicidedelibdem2)$coefficients[2],
           summary(homicideegaldem2)$coefficients[1] + 1.96*summary(homicideegaldem2)$coefficients[2])
names = c("polyarchy","libdem","partipdem","delibdem","egaldem")
homicide = as.data.frame(cbind(coefs_1, coefs_2))
loCI = as.data.frame(cbind(loCI_1, loCI_2))
hiCI = as.data.frame(cbind(hiCI_1, hiCI_2))

#Homicide plot
homicide = as.data.frame(cbind(c(homicide[1,], homicide[2,], homicide[3,], homicide[4,], homicide[5,]),
                               cbind(c(loCI[1,], loCI[2,], loCI[3,], loCI[4,], loCI[5,]),  
                                     cbind(c(hiCI[1,], hiCI[2,], hiCI[3,], hiCI[4,], hiCI[5,]),           
                                           rep(names, each = 2), rep(1:2, 5)))))
names(homicide) = c("coefficient", "loCI", "hiCI", "index", "model")
library(ggplot2)
library(viridis)
homicide$coefficient = as.numeric(homicide$coefficient)
homicide$loCI = as.numeric(homicide$loCI)
homicide$hiCI = as.numeric(homicide$hiCI)
homicide$index = as.character(homicide$index)
homicide$model = as.character(homicide$model)

homicide = ggplot(homicide, aes(fill=model, y=coefficient, x=index)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=loCI, ymax=hiCI), 
                position=position_dodge(width=0.9),
                width=0.3, colour="gray", alpha=0.5, size=1) + 
  scale_fill_manual(values = viridis_pal(option = "D")(2), # 2 colors from the viridis palette
                    labels = c("1" = "simple regression", "2" = "fixed effects")) + 
  labs(fill = "Model", 
       title = "Homicide",
       x = "quality of democracy index",
       y = "estimated association with the log of crime rates") +
  scale_x_discrete(labels = c("Deliberative", "Egalitarian", "Liberal", "Participatory", "Electoral")) +  
  theme_minimal()
#Exporting the graph in high resolution
library(ggpubr)
ggexport(homicide, width=2000, height=1600, res=320,
         filename=" homicide.jpeg")


#Robbery
coefs_1 = c(summary(robberypolyarchy1)$coefficients[2], summary(robberylibdem1)$coefficients[2], 
            summary(robberypartipdem1)$coefficients[2], summary(robberydelibdem1)$coefficients[2],
            summary(robberyegaldem1)$coefficients[2])
coefs_2 = c(summary(robberypolyarchy2)$coefficients[1], summary(robberylibdem2)$coefficients[1], 
            summary(robberypartipdem2)$coefficients[1], summary(robberydelibdem2)$coefficients[1],
            summary(robberyegaldem2)$coefficients[1])
loCI_1 = c(summary(robberypolyarchy1)$coefficients[2] - 1.96*summary(robberypolyarchy1)$coefficients[2,2], 
           summary(robberylibdem1)$coefficients[2] - 1.96*summary(robberylibdem1)$coefficients[2,2], 
           summary(robberypartipdem1)$coefficients[2] - 1.96*summary(robberypartipdem1)$coefficients[2,2],
           summary(robberydelibdem1)$coefficients[2] - 1.96*summary(robberydelibdem1)$coefficients[2,2],
           summary(robberyegaldem1)$coefficients[2] - 1.96*summary(robberyegaldem1)$coefficients[2,2])
loCI_2 = c(summary(robberypolyarchy2)$coefficients[1] - 1.96*summary(robberypolyarchy2)$coefficients[2], 
           summary(robberylibdem2)$coefficients[1] - 1.96*summary(robberylibdem2)$coefficients[2], 
           summary(robberypartipdem2)$coefficients[1] - 1.96*summary(robberypartipdem2)$coefficients[2],
           summary(robberydelibdem2)$coefficients[1] - 1.96*summary(robberydelibdem2)$coefficients[2],
           summary(robberyegaldem2)$coefficients[1] - 1.96*summary(robberyegaldem2)$coefficients[2])
hiCI_1 = c(summary(robberypolyarchy1)$coefficients[2] + 1.96*summary(robberypolyarchy1)$coefficients[2,2], 
           summary(robberylibdem1)$coefficients[2] + 1.96*summary(robberylibdem1)$coefficients[2,2], 
           summary(robberypartipdem1)$coefficients[2] + 1.96*summary(robberypartipdem1)$coefficients[2,2],
           summary(robberydelibdem1)$coefficients[2] + 1.96*summary(robberydelibdem1)$coefficients[2,2],
           summary(robberyegaldem1)$coefficients[2] + 1.96*summary(robberyegaldem1)$coefficients[2,2])
hiCI_2 = c(summary(robberypolyarchy2)$coefficients[1] + 1.96*summary(robberypolyarchy2)$coefficients[2], 
           summary(robberylibdem2)$coefficients[1] + 1.96*summary(robberylibdem2)$coefficients[2], 
           summary(robberypartipdem2)$coefficients[1] + 1.96*summary(robberypartipdem2)$coefficients[2],
           summary(robberydelibdem2)$coefficients[1] + 1.96*summary(robberydelibdem2)$coefficients[2],
           summary(robberyegaldem2)$coefficients[1] + 1.96*summary(robberyegaldem2)$coefficients[2])
names = c("polyarchy","libdem","partipdem","delibdem","egaldem")
robbery = as.data.frame(cbind(coefs_1, coefs_2))
loCI = as.data.frame(cbind(loCI_1, loCI_2))
hiCI = as.data.frame(cbind(hiCI_1, hiCI_2))

#Robbery plot
robbery = as.data.frame(cbind(c(robbery[1,], robbery[2,], robbery[3,], robbery[4,], robbery[5,]),
                              cbind(c(loCI[1,], loCI[2,], loCI[3,], loCI[4,], loCI[5,]),  
                                    cbind(c(hiCI[1,], hiCI[2,], hiCI[3,], hiCI[4,], hiCI[5,]),           
                                          rep(names, each = 2), rep(1:2, 5)))))
names(robbery) = c("coefficient", "loCI", "hiCI", "index", "model")
library(ggplot2)
library(viridis)
robbery$coefficient = as.numeric(robbery$coefficient)
robbery$loCI = as.numeric(robbery$loCI)
robbery$hiCI = as.numeric(robbery$hiCI)
robbery$index = as.character(robbery$index)
robbery$model = as.character(robbery$model)

robbery = ggplot(robbery, aes(fill=model, y=coefficient, x=index)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=loCI, ymax=hiCI), 
                position=position_dodge(width=0.9),
                width=0.3, colour="gray", alpha=0.5, size=1) + 
  scale_fill_manual(values = viridis_pal(option = "D")(2), # 2 colors from the viridis palette
                    labels = c("1" = "simple regression", "2" = "fixed effects")) + 
  labs(fill = "Model", 
       title = "Robbery",
       x = "quality of democracy index",
       y = "estimated association with the log of crime rates") +
  scale_x_discrete(labels = c("Deliberative", "Egalitarian", "Liberal", "Participatory", "Electoral")) +  
  theme_minimal()
#Exporting the graph in high resolution
library(ggpubr)
ggexport(robbery, width=2000, height=1600, res=320,
         filename=" robbery.jpeg")


#Sex
coefs_1 = c(summary(sexpolyarchy1)$coefficients[2], summary(sexlibdem1)$coefficients[2], 
            summary(sexpartipdem1)$coefficients[2], summary(sexdelibdem1)$coefficients[2],
            summary(sexegaldem1)$coefficients[2])
coefs_2 = c(summary(sexpolyarchy2)$coefficients[1], summary(sexlibdem2)$coefficients[1], 
            summary(sexpartipdem2)$coefficients[1], summary(sexdelibdem2)$coefficients[1],
            summary(sexegaldem2)$coefficients[1])
loCI_1 = c(summary(sexpolyarchy1)$coefficients[2] - 1.96*summary(sexpolyarchy1)$coefficients[2,2], 
           summary(sexlibdem1)$coefficients[2] - 1.96*summary(sexlibdem1)$coefficients[2,2], 
           summary(sexpartipdem1)$coefficients[2] - 1.96*summary(sexpartipdem1)$coefficients[2,2],
           summary(sexdelibdem1)$coefficients[2] - 1.96*summary(sexdelibdem1)$coefficients[2,2],
           summary(sexegaldem1)$coefficients[2] - 1.96*summary(sexegaldem1)$coefficients[2,2])
loCI_2 = c(summary(sexpolyarchy2)$coefficients[1] - 1.96*summary(sexpolyarchy2)$coefficients[2], 
           summary(sexlibdem2)$coefficients[1] - 1.96*summary(sexlibdem2)$coefficients[2], 
           summary(sexpartipdem2)$coefficients[1] - 1.96*summary(sexpartipdem2)$coefficients[2],
           summary(sexdelibdem2)$coefficients[1] - 1.96*summary(sexdelibdem2)$coefficients[2],
           summary(sexegaldem2)$coefficients[1] - 1.96*summary(sexegaldem2)$coefficients[2])
hiCI_1 = c(summary(sexpolyarchy1)$coefficients[2] + 1.96*summary(sexpolyarchy1)$coefficients[2,2], 
           summary(sexlibdem1)$coefficients[2] + 1.96*summary(sexlibdem1)$coefficients[2,2], 
           summary(sexpartipdem1)$coefficients[2] + 1.96*summary(sexpartipdem1)$coefficients[2,2],
           summary(sexdelibdem1)$coefficients[2] + 1.96*summary(sexdelibdem1)$coefficients[2,2],
           summary(sexegaldem1)$coefficients[2] + 1.96*summary(sexegaldem1)$coefficients[2,2])
hiCI_2 = c(summary(sexpolyarchy2)$coefficients[1] + 1.96*summary(sexpolyarchy2)$coefficients[2], 
           summary(sexlibdem2)$coefficients[1] + 1.96*summary(sexlibdem2)$coefficients[2], 
           summary(sexpartipdem2)$coefficients[1] + 1.96*summary(sexpartipdem2)$coefficients[2],
           summary(sexdelibdem2)$coefficients[1] + 1.96*summary(sexdelibdem2)$coefficients[2],
           summary(sexegaldem2)$coefficients[1] + 1.96*summary(sexegaldem2)$coefficients[2])
names = c("polyarchy","libdem","partipdem","delibdem","egaldem")
sex = as.data.frame(cbind(coefs_1, coefs_2))
loCI = as.data.frame(cbind(loCI_1, loCI_2))
hiCI = as.data.frame(cbind(hiCI_1, hiCI_2))


#Sex plot
sex = as.data.frame(cbind(c(sex[1,], sex[2,], sex[3,], sex[4,], sex[5,]),
                          cbind(c(loCI[1,], loCI[2,], loCI[3,], loCI[4,], loCI[5,]),  
                                cbind(c(hiCI[1,], hiCI[2,], hiCI[3,], hiCI[4,], hiCI[5,]),           
                                      rep(names, each = 2), rep(1:2, 5)))))
names(sex) = c("coefficient", "loCI", "hiCI", "index", "model")
library(ggplot2)
library(viridis)
sex$coefficient = as.numeric(sex$coefficient)
sex$loCI = as.numeric(sex$loCI)
sex$hiCI = as.numeric(sex$hiCI)
sex$index = as.character(sex$index)
sex$model = as.character(sex$model)

sex = ggplot(sex, aes(fill=model, y=coefficient, x=index)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=loCI, ymax=hiCI), 
                position=position_dodge(width=0.9),
                width=0.3, colour="gray", alpha=0.5, size=1) + 
  scale_fill_manual(values = viridis_pal(option = "D")(2), # 2 colors from the viridis palette
                    labels = c("1" = "simple regression", "2" = "fixed effects")) + 
  labs(fill = "Model", 
       title = "Sex",
       x = "quality of democracy index",
       y = "estimated association with the log of crime rates") +
  scale_x_discrete(labels = c("Deliberative", "Egalitarian", "Liberal", "Participatory", "Electoral")) +  
  theme_minimal()
#Exporting the graph in high resolution
library(ggpubr)
ggexport(sex, width=2000, height=1600, res=320,
         filename=" sex.jpeg")


#Theft
coefs_1 = c(summary(theftpolyarchy1)$coefficients[2], summary(theftlibdem1)$coefficients[2], 
            summary(theftpartipdem1)$coefficients[2], summary(theftdelibdem1)$coefficients[2],
            summary(theftegaldem1)$coefficients[2])
coefs_2 = c(summary(theftpolyarchy2)$coefficients[1], summary(theftlibdem2)$coefficients[1], 
            summary(theftpartipdem2)$coefficients[1], summary(theftdelibdem2)$coefficients[1],
            summary(theftegaldem2)$coefficients[1])
loCI_1 = c(summary(theftpolyarchy1)$coefficients[2] - 1.96*summary(theftpolyarchy1)$coefficients[2,2], 
           summary(theftlibdem1)$coefficients[2] - 1.96*summary(theftlibdem1)$coefficients[2,2], 
           summary(theftpartipdem1)$coefficients[2] - 1.96*summary(theftpartipdem1)$coefficients[2,2],
           summary(theftdelibdem1)$coefficients[2] - 1.96*summary(theftdelibdem1)$coefficients[2,2],
           summary(theftegaldem1)$coefficients[2] - 1.96*summary(theftegaldem1)$coefficients[2,2])
loCI_2 = c(summary(theftpolyarchy2)$coefficients[1] - 1.96*summary(theftpolyarchy2)$coefficients[2], 
           summary(theftlibdem2)$coefficients[1] - 1.96*summary(theftlibdem2)$coefficients[2], 
           summary(theftpartipdem2)$coefficients[1] - 1.96*summary(theftpartipdem2)$coefficients[2],
           summary(theftdelibdem2)$coefficients[1] - 1.96*summary(theftdelibdem2)$coefficients[2],
           summary(theftegaldem2)$coefficients[1] - 1.96*summary(theftegaldem2)$coefficients[2])
hiCI_1 = c(summary(theftpolyarchy1)$coefficients[2] + 1.96*summary(theftpolyarchy1)$coefficients[2,2], 
           summary(theftlibdem1)$coefficients[2] + 1.96*summary(theftlibdem1)$coefficients[2,2], 
           summary(theftpartipdem1)$coefficients[2] + 1.96*summary(theftpartipdem1)$coefficients[2,2],
           summary(theftdelibdem1)$coefficients[2] + 1.96*summary(theftdelibdem1)$coefficients[2,2],
           summary(theftegaldem1)$coefficients[2] + 1.96*summary(theftegaldem1)$coefficients[2,2])
hiCI_2 = c(summary(theftpolyarchy2)$coefficients[1] + 1.96*summary(theftpolyarchy2)$coefficients[2], 
           summary(theftlibdem2)$coefficients[1] + 1.96*summary(theftlibdem2)$coefficients[2], 
           summary(theftpartipdem2)$coefficients[1] + 1.96*summary(theftpartipdem2)$coefficients[2],
           summary(theftdelibdem2)$coefficients[1] + 1.96*summary(theftdelibdem2)$coefficients[2],
           summary(theftegaldem2)$coefficients[1] + 1.96*summary(theftegaldem2)$coefficients[2])
names = c("polyarchy","libdem","partipdem","delibdem","egaldem")
theft = as.data.frame(cbind(coefs_1, coefs_2))
loCI = as.data.frame(cbind(loCI_1, loCI_2))
hiCI = as.data.frame(cbind(hiCI_1, hiCI_2))

#Theft plot
theft = as.data.frame(cbind(c(theft[1,], theft[2,], theft[3,], theft[4,], theft[5,]),
                              cbind(c(loCI[1,], loCI[2,], loCI[3,], loCI[4,], loCI[5,]),  
                                    cbind(c(hiCI[1,], hiCI[2,], hiCI[3,], hiCI[4,], hiCI[5,]),           
                                          rep(names, each = 2), rep(1:2, 5)))))
names(theft) = c("coefficient", "loCI", "hiCI", "index", "model")
library(ggplot2)
library(viridis)
theft$coefficient = as.numeric(theft$coefficient)
theft$loCI = as.numeric(theft$loCI)
theft$hiCI = as.numeric(theft$hiCI)
theft$index = as.character(theft$index)
theft$model = as.character(theft$model)

theft = ggplot(theft, aes(fill=model, y=coefficient, x=index)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=loCI, ymax=hiCI), 
                position=position_dodge(width=0.9),
                width=0.3, colour="gray", alpha=0.5, size=1) + 
  scale_fill_manual(values = viridis_pal(option = "D")(2), # 2 colors from the viridis palette
                    labels = c("1" = "simple regression", "2" = "fixed effects")) + 
  labs(fill = "Model", 
       title = "Theft",
       x = "quality of democracy index",
       y = "estimated association with the log of crime rates") +
  scale_x_discrete(labels = c("Deliberative", "Egalitarian", "Liberal", "Participatory", "Electoral")) +  
  theme_minimal()
#Exporting the graph in high resolution
library(ggpubr)
ggexport(theft, width=2000, height=1600, res=320,
         filename=" theft.jpeg")

#All 8 graphs into 1
all = ggarrange(assault, drug, fraud, homicide, robbery, sex, theft, total, ncol=2, nrow=4,
                common.legend = TRUE, legend="bottom")
ggexport(all, width=3000, height=6000, res=320,
         filename=" all.jpeg")

#2 graphs of 4
graph1 = ggarrange(assault, drug, fraud, homicide, ncol=2, nrow=2,
                common.legend = TRUE)
ggexport(graph1, width=3000, height=3000, res=320,
         filename=" graph1.jpeg")
graph2 = ggarrange(robbery, sex, theft, total, ncol=2, nrow=2,
                   common.legend = TRUE)
ggexport(graph2, width=3000, height=3000, res=320,
         filename=" graph2.jpeg")
