library(tidyverse)
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(summarytools)
library(jtools)
library(ggstance)
library(broom)
library(car)

#import data files
sgcarmart_1 <- read.csv("sgcarmart.csv")
sgcarmart_2 <- read.csv("sgcarmartm2.csv")
sgcarmart_3 <- read.csv("sgcarmart3.csv")

#join files together
sgcarmart_12 <- merge (sgcarmart_1,sgcarmart_2,all.x = TRUE,all.y = TRUE)
sgcarmart <- merge (sgcarmart_12, sgcarmart_3, all.x = TRUE, all.y = TRUE)

#remove rows that are null and select necessary columns
avail_cars <- sgcarmart %>%
    select(-1, -2, - contains('href'), - contains('link'), - contains('electric')) %>%
    filter(!grepl('null', Current_Price))

#clean up data and remove artefacts
clean_cars <- avail_cars %>%
  #remove dollar signs, commas, and units
  mutate_all(str_replace_all, "[$]|km/h|km/L|/yr|/mth|,|cc", "") %>%
  #weird character so had to mutate separately
  mutate(Road_Tax = str_replace_all(Road_Tax, "[Â /yr]", "")) %>%
  mutate(ARF = str_replace_all(ARF, "[(after VES rebate)]", "")) %>%
  #Needed the '.%' since the numbers were not the same
  mutate(Down_Payment = str_replace_all(Down_Payment, "(Maximum .*% loan)", "")) %>%
  mutate(Down_Payment = stri_replace_all_fixed(Down_Payment, "()", "")) %>%
  mutate(VES = str_replace_all(VES, "\\(.*\\)", "")) %>%
  mutate(tank_cap = str_replace_all(tank_cap, "L", "")) %>%
  mutate(Power_hp = str_extract(Power, "\\d+(?=[^(]*\\))"))

#encode dummy variables
cleandum_cars <- clean_cars %>%
  mutate(KeylessEntry = ifelse(KeylessEntry == "Yes", 1, 0)) %>%
  mutate(AutoHeadlights = ifelse(AutoHeadlights == "Yes", 1, 0)) %>%
  mutate(AutoWipers = ifelse(AutoWipers == "Yes", 1, 0)) %>%
  mutate(PaddleShift = ifelse(PaddleShift == "Yes", 1, 0)) %>%
  mutate(SmartKey = ifelse(SmartKey == "Yes", 1, 0)) %>%
  mutate(RemoteBoot = ifelse(RemoteBoot == "Yes", 1, 0)) %>%
  mutate(ReverseCam = ifelse(ReverseCam == "Yes", 1, 0))

#convert numbers from strings to numeric
intcleandum_cars <- cleandum_cars %>%
  mutate_at(vars(-one_of(c('categories','openindiv','Vehicle.Name','veh_type','engine_type'))), as.numeric)

# Add "features" together
final_cars <- intcleandum_cars %>% 
  mutate(totalFeatures = select(., 18:24) %>% rowSums())

#convert engine type to number of cylinders
final_cars <- final_cars %>% 
  mutate(cyl = if_else(str_detect(engine_type, "(4-cylinder)"),as.numeric(4),
    if_else(str_detect(engine_type, "(V6)"), as.numeric(6),
      if_else(str_detect(engine_type, "(V8)"), as.numeric(8),
        if_else(str_detect(engine_type, "(3-cylinder)"), as.numeric(3), 
          if_else(str_detect(engine_type, "(V10)"), as.numeric(10),
                  if_else(str_detect(engine_type, "(V12)"), as.numeric(12), 
                          if_else(str_detect(engine_type, "(6-cylinder)"), as.numeric(6), as.numeric(0)))))))))

#factoring in turbocharged
final_cars <- final_cars %>%
  mutate(turbo = if_else(str_detect(engine_type, "(turbo)|(turbocharged)|(supercharged)|(Turbocharged)|(Turbo)"), as.numeric(1), as.numeric(0)))

library(zoo)
#interpolate NA values
tfinal <- final_cars %>% 
  mutate(inter_fuelC = na.approx(Fuel_consumption)) %>% 
  mutate(inter_tankC = na.approx(tank_cap)) %>%
  mutate(inter_topS = na.approx(top_speed)) %>% 
  mutate(inter_hp = na.approx(Power_hp)) %>%
  mutate(inter_engc = na.approx(eng_cap)) %>% 
  filter("inter_topS">150 & "totalFeatures">=2)

#split into different categories of prestige

tfinal <- tfinal %>%
  mutate(make_status = ifelse(categories %in% c("Ford","Citroen","Honda","Hyundai","Jeep","Kia","Mazda","Mitsubishi","Subaru","Suzuki","Toyota","Volkswagen","Nissan"), as.numeric(1),
                               ifelse(categories %in% c("Audi","Ferrari","Jaguar","Lamborghini","Lexus","Maserati","McLaren","Mercedes-Benz","Porsche","BMW","BMW ALPINA","BMW M Series","Volvo"), as.numeric(2), as.numeric(0))))

#quick summary of data
view(dfSummary(final_cars))

#prepare correlation plot
ttfinal <- tfinal %>% select(c("Current_Price","inter_engc","inter_topS","inter_hp","inter_fuelC","inter_tankC","cyl","turbo","totalFeatures","make_status"))
ttfinal_cor <- cor(ttfinal)

#rename columns and rows for plot
colnames(ttfinal_cor)[1:10] <- c("Current Price","Engine CC","Top Speed","Horsepower","Fuel Consumption","Tank Capacity","No. of Cylinders","Turbocharged?","Feature Count","Make Premium")
rownames(ttfinal_cor) <- c("Current Price","Engine CC","Top Speed","Horsepower","Fuel Consumption","Tank Capacity","No. of Cylinders","Turbocharged?","Feature Count","Make Premium")

library(corrplot)
corrplot(ttfinal_cor, type = "upper",addCoef.col=TRUE)

#filter out the "Others" category
tttfinal <- ttfinal %>% filter(make_status!=0)

#plot the SPM
scatterplotMatrix(~Current_Price+inter_engc+inter_topS+inter_hp+inter_fuelC+inter_tankC+cyl|make_status,data=tttfinal,var.label=c("Current Price","Engine CC","Top Speed","Horsepower","Fuel Consumption","Tank Capacity","No. of Cylinders"), plot.points=FALSE, main="Correlation of Price and Features, with interaction of Make Premium",diagonal=FALSE,cex.labels=1.0,legend=FALSE,smooth=FALSE)

library(caret)
#build the training and testing set
set.seed(2809)
training.samples <- tttfinal$Current_Price %>% createDataPartition(p = 0.8, list = FALSE)
train.data <- tttfinal[training.samples, ]
test.data <- tttfinal[-training.samples, ]

#build the model
model <- train.data %>% lm(formula = Current_Price~(inter_engc+inter_topS+inter_hp+inter_fuelC+inter_tankC+cyl)*make_status, data = .)
#summary of model
summary(model)

#make predictions on model
predictions <- model %>% predict(test.data)
#see model performance
R2(predictions,test.data$Current_Price)

#finding predicted R2
pr <- residuals(model)/(1 - lm.influence(model)$hat)
PRESS <- sum(pr^2)
cat("Predicted Residual Error Sum of Squares: ", PRESS)
# anova to calculate residual sum of squares
my.anova <- anova(model)
tss <- sum(my.anova$"Sum Sq")
# predictive R^2
pred.r.squared <- 1 - PRESS/(tss)
cat("Predicted R2 value: ", pred.r.squared)

#to fit residuals in 
resi_fit <- tfinal %>% lm(formula = Current_Price~(inter_engc+inter_topS+inter_hp+inter_fuelC+inter_tankC+cyl)*make_status, data = .)
resii <- residuals(resi_fit)
tfinal$residuals <- abs(resii)
head_tfinal <- tfinal %>% arrange(desc(residuals)) %>% select(categories,openindiv,Current_Price) %>% rename(Make = categories, Model = openindiv, Price = Current_Price) %>% head()
head_tfinal <- head_tfinal %>% mutate(Price = as.character(Price))

#cooksd test from r-statistics.co by Selva Prabhakaran
cooksd <- cooks.distance(model)
plot(cooksd, pch="*",cex=2,main = "Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels




