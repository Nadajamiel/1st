library(readxl)
Housing_Price_price_data <- read_excel("Housing Price dataset.xlsx")
View(Housing_Price_price_data)
set.seed(123)
sample<-sample(c(TRUE,FALSE),nrow(Housing_Price_price_data),replace=TRUE  , prob=c(0.7,0.3))
price_data<-Housing_Price_price_data[sample,]
test<- Housing_Price_price_data[!sample,]
View(price_data)
summary(price_data)

#data prep#########
price_data[] <- lapply(price_data, function(x) {
  if (is.character(x)) {
    as.factor(x)  
  } else {
    x  
  }
})

#missing values
p_na<-function(x){sum(is.na(x))/length(x)*100}
apply(price_data,2,p_na)
na.omit(price_data)

#remove non-used var
library(dplyr)
price_data<- price_data %>% select(-Region)

#change number of rooms into categorical
table(price_data$`Number of rooms`)
breaks<-c(min(price_data$`Number of rooms`)-1,0,1,2,3,max(price_data$`Number of rooms`))
price_data$`Number of rooms`<- cut(price_data$`Number of rooms`,breaks,labels = c("no_rooms","single","two","three","more"))
summary(price_data$`Number of rooms`)

#change price structure to be in thousands (easier to read)
price_data$Price<- price_data$Price/1000
price_data <- price_data %>% rename(Price_in_thousands = Price)
summary(price_data$Price_in_thousands) 

# group metro stations
table(price_data$`Metro station`)
north_stations <- c("Алтуфьево","Санино","Долгопрудная","Улица Академика Королёва","Аникеевка ","Бутырская","ВДНХ","Верхние Котлы","Бескудниково" ,"Бабушкинская", "Белокаменная", "Беломорская","Бибирево","Верхние Лихоборы","Пятницкое шоссе", "Аникеевка", "Динамо", "Водный стадион", "Владыкино"," Водный стадион", "Войковская", "Волоколамская", "Дегунино", "Деловой центр", "Депо, Динамо", "Дмитровская","Зеленоград — Крюково", "Зорге", "Карамышевская","Коптево", "Красногвардейская","Красносельская", "Красные ворота", "Курская","Лианозово", "Лихоборы", "Марьина Роща", "Марьина Роща (Шереметьевская)","Медведково", "Митино", "Народное Ополчение","Новоподрезково", "Окружная", "Отрадное", "Петровско-Разумовская", "Покровское", "Планерная", "Полежаевская", "Речной вокзал", "Рижская", "Ростокино","Савеловская", "Савёловская", "Селигерская", "Сокол", "Стахановская", "Стрешнево", "Сходненская", "Суликатная", "Тимирязевская", "Трикотажная","Тушинская", "Физтех", "Хлебниково", "Ховрино", "Хорошево", "Хорошёво", "Хорошёвская", "Щукинская", "Яхромская")
south_stations <- c("Аннино","Академическая" ,"Аэропорт Внуково","Варшавская (Коломенское)","Бульвар Адмирала Ушакова","Братиславская","Битца", "Битцевский Парк","Борисово","Верхние котлы","Пенягино", "Пражская", "Беляево", "Раменки", "Выхино", "Красный Строитель", "Улица Дмитриевского","Верхние Лихоборы","Вешняки","Боровское шоссе", "Беляево ","Бунинская аллея","Бунинская Аллея","Бутово","Бульвар Дмитрия Донского", "Варшавская","Верхние котлы", "Вешняки", "Волгоградский проспект", "Воробьёвы горы", "Воронцовская", "Выставочная", "Говорово", "Гражданская", "Давыдково", "Добрынинская", "Домодедовская", "Дубровка", "Жулебино", "ЗИЛ", "Зюзино", "Зябликово", "Калитники", "Калужская", "Кантемировская", "Каховская", "Каширская", "Китай-город","Достоевская","Кожуховская", "Коломенская", "Коммунарка", "Коньково", "Косино", "Котельники","Крестьянская застава", "Крымская", "Кузьминки","Курьяново", "Ленинский проспект", "Лермонтовский проспект", "Лесопарковая", "Лухмановская", "Люблино", "Марксистская", "Марьино", "Матвеевская","Мичуринский проспект", "Москворечье", "Нагатинская", "Нагатинский Затон", "Нагорная", "Нахимовский проспект", "Новопеределкино", "Новоясеневская", "Новые Черемушки", "Новые Черёмушки", "Озёрная", "Ольховая", "Орехово", "Остафьево", "Парк Победы", "Перерва", "Печатники", "Подольск", "Прокшино", "Пролетарская", "Проспект Вернадского", "Профсоюзная", "Пыхтино", "Рабочий посёлок", "Рабочий Посёлок", "Рассказовка","Румянцево", "Саларьево", "Севастопольская", "Семеновская", "Семёновская", "Серпуховская", "Силикатная", "Сетунь", "Студенческая", "Текстильщики", "Теплый Стан", "Тёплый Стан", "Терехово", "Тропарево", "Тропарёво","Тульская", "Угрешская", "Улица Академика Янгеля", "Улица Горчакова", "Улица Скобелевская", "Улица Старокачаловская", "Университет", "Филатов Луг", "Царицыно", "Чертановская", "Шаболовская", "Шипиловская", "Щербинка", "Юго-Восточная", "Юго-Западная", "Южная", "Ясенево")
east_stations <- c("Авиамоторная", "Автозаводская","Соколиная гора","Библиотека им. Ленина", "Алексеевская","Проспект Мира", "Шоссе Энтузиастов","Лубянка", "Алма-Атинская", "Андроновка","Бауманская","Ботанический сад", "Бульвар Рокоссовского", "ВДНХ","Волжская", "Измайлово", "Измайловская","Косино", "Котельники","Кузнецкий мост","Лубянка ","Депо","Комсомольская","Лефортово", "Локомотив","Маяковская", "Менделеевская", "Москва-Товарная","Некрасовка","Нижегородская", "Новаторская", "Новогиреево", "Новодачная", "Новокосино", "Новохохловская", "Окская","Партизанская", "Первомайская", "Перово", "Преображенская площадь","Площадь Гагарина", "Площадь Ильича", "Площадь Революции", "Полянка", "Пушкинская", "Римская","Рязанский проспект", "Свиблово", "Соколиная Гора", "Сокольники", "Таганская","Смоленская", "Сретенский бульвар", "Сухаревская", "Тверская", "Театральная", "Третьяковская","Фонвизинская", "Черкизовская", "Шоссе энтузиастов", "Щелковская", "Щёлковская", "Электрозаводская")
west_stations <- c("Арбатская", "Аэропорт","Воробьевы горы","Терехово (Мнёвники)","Александровский сад","Аминьевская","Библиотека и Ленина","Выставочный центр", "Петровский Парк", "Багратионовская", "Беговая", "Белорусская", "Боровицкая","Балтийская", "Баррикадная","Внуково", "Волоколамская","Пионерская", "Раменки", "Петровский парк", "Выставочная", "Киевская","Кленовый бульвар", "Красногорская", "Краснопресненская","Красный Балтиец", "Кропоткинская", "Крылатское", "Кунцевская","Кутузовская", "Ломоносовский проспект", "Лужники", "Марк","Международная", "Минская", "Мнёвники", "Молодежная", "Молодёжная", "Мякинино", "Нахабино","Немчиновка", "Озёрная", "Октябрьское поле", "Опалиха", "Очаково", "Павшино", "Панфиловская","Новокузнецкая", "Новослободская", "Октябрьская", "Охотный ряд", "Павелецкая", "Парк культуры", "Парк Культуры","Площадь Гагарина", "Площадь Ильича", "Площадь Революции", "Полянка", "Пушкинская", "Римская","Сколково", "Славянский бульвар", "Солнцево", "Спартак", "Спортивная", "Строгино", "Сухаревская", "Тестовская", "Технопарк", "Трубная","Улица 1905 года", "Филевский парк", "Филёвский парк", "Фили", "ЦСКА", "Шелепиха","Тургеневская", "Фрунзенская", "Цветной бульвар", "Чеховская", "Чистые пруды", "Чкаловская")
price_data$`Metro station` <- ifelse(price_data$`Metro station` %in% north_stations, "North",
                            ifelse(price_data$`Metro station` %in% south_stations, "South",
                                   ifelse(price_data$`Metro station` %in% east_stations, "East",
                                          ifelse(price_data$`Metro station` %in% west_stations, "West", NA))))
price_data$`Metro station` <- as.factor(price_data$`Metro station`)
summary(price_data$`Metro station`)

#check for outliers
library(psych)
library(DescTools)
quan<- price_data %>%select(Price_in_thousands,`Minutes to metro`,Area,`Living area`,`Kitchen area`,Floor,`Number of floors`)
boxplot(price_data$Price_in_thousands)
boxplot(price_data$`Minutes to metro`)
boxplot(price_data$Area)
boxplot(price_data$`Living area`)
boxplot(price_data$`Kitchen area`)
boxplot(price_data$Floor)
boxplot(price_data$`Number of floors`)

#num of outliers
count_outliers <- function(x) {
  return(length(boxplot.stats(x)$out))
}
outlier_counts <- sapply(quan, count_outliers)
outlier_counts

#remove outliers
replace_outliers <- function(k) {
  Q1 <- quantile(k, .25)
  Q3 <- quantile(k, .75)
  IQR_value <- IQR(k)
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  k[k < lower_bound] <- Q1  
  k[k > upper_bound] <- Q3 
  return(k)
}
price_data[] <- lapply(price_data[], function(col) {
 if (is.numeric(col)) replace_outliers(col) else col
})
summary(price_data)
write.csv(summary(price_data), "summary.csv")

#data visualization #####
library(ggplot2)
library(gridExtra)
b1<-ggplot(price_data,aes(`Apartment type`))+geom_bar(fill= c("turquoise","lightpink"))
b2<-ggplot(price_data,aes(`Metro station`))+geom_bar(fill= c("lightgreen","indianred1","yellow1","lightblue"))
b3<-ggplot(price_data,aes(`Number of rooms`))+geom_bar(fill= c("lightblue","lightblue1","lightblue2","lightblue3","lightblue4"))
b4<-ggplot(price_data,aes(Renovation))+geom_bar(fill= c("deepskyblue","violet","cyan","pink"))
f1<-Freq(price_data$`Apartment type`)
p1<-ggplot(f1, aes(x="",y=freq, fill=level)) + geom_bar(stat="identity", width=1)+ 
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(perc*100), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL)+
  scale_fill_manual(values = c("turquoise", "lightpink"))
f2<-Freq(price_data$`Metro station`)
p2<-ggplot(f2, aes(x="",y=freq, fill=level)) + geom_bar(stat="identity", width=1)+ 
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(perc*100), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL)+
  scale_fill_manual(values =c("lightgreen","indianred1","yellow1","lightblue"))
f3<-Freq(price_data$`Number of rooms`)
p3<-ggplot(f3, aes(x="",y=freq, fill=level)) + geom_bar(stat="identity", width=1)+ 
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(perc*100), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL)+
  scale_fill_manual(values = c("lightblue","lightblue1","lightblue2","lightblue3","lightblue4"))
f4<-Freq(price_data$Renovation)
p4<-ggplot(f4, aes(x="",y=freq, fill=level)) + geom_bar(stat="identity", width=1)+ 
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(perc*100), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL)+
  scale_fill_manual(values = c("deepskyblue","violet","cyan","pink"))
grid.arrange(b1,p1,nrow=1 , ncol= 2 , top= "Apartment type" )
grid.arrange(b2,p2,nrow=1 , ncol= 2 , top= "Metro Station" )
grid.arrange(b3,p3,nrow=1 , ncol= 2 , top= "# of Rooms" )
grid.arrange(b4,p4,nrow=1 , ncol= 2 , top= "Renovation" )

#histograms
library(scales)
ggplot(price_data, aes(`Price_in_thousands`)) +
  geom_histogram(aes(y=after_stat(density), fill="Histogram"), bins=12, position="identity", alpha=0.5) +
  geom_density(aes(color="Density"), size = 1, alpha = 0) +scale_fill_manual(values=c("Histogram"="violet")) +  
  scale_color_manual(values=c("Density"="violetred")) +labs(fill="Legend",title="Distribution of Price")+scale_y_continuous(labels = comma)
ggplot(price_data, aes(`Minutes to metro`)) +
  geom_histogram(aes(y=after_stat(density), fill="Histogram"), bins=12, position="identity", alpha=0.5) +
  geom_density(aes(color="Density"), size = 1, alpha = 0) +scale_fill_manual(values=c("Histogram"="lightseagreen")) +  
  scale_color_manual(values=c("Density"="steelblue")) +labs(fill="Legend",title="Distribution of Minutes to metro")
ggplot(price_data, aes(`Area`)) +
  geom_histogram(aes(y=after_stat(density), fill="Histogram"), bins=12, position="identity", alpha=0.5) +
  geom_density(aes(color="Density"), size = 1, alpha = 0) +scale_fill_manual(values=c("Histogram"="plum")) +  
  scale_color_manual(values=c("Density"="mediumorchid")) +labs(fill="Legend",title="Distribution of Area")
ggplot(price_data, aes(`Living area`)) +
  geom_histogram(aes(y=after_stat(density), fill="Histogram"), bins=12, position="identity", alpha=0.5) +
  geom_density(aes(color="Density"), size = 1, alpha = 0) +scale_fill_manual(values=c("Histogram"="lightblue")) +  
  scale_color_manual(values=c("Density"="navy")) +labs(fill="Legend",title="Distribution of Living Area")
ggplot(price_data, aes(`Kitchen area`)) +
  geom_histogram(aes(y=after_stat(density), fill="Histogram"), bins=12, position="identity", alpha=0.5) +
  geom_density(aes(color="Density"), size = 1, alpha = 0) +scale_fill_manual(values=c("Histogram"="lightgreen")) +  
  scale_color_manual(values=c("Density"="darkgreen")) +labs(fill="Legend",title="Distribution of Kitchen Area")
ggplot(price_data, aes(`Floor`)) +
  geom_histogram(aes(y=after_stat(density), fill="Histogram"), bins=12, position="identity", alpha=0.5) +
  geom_density(aes(color="Density"), size = 1, alpha = 0) +scale_fill_manual(values=c("Histogram"="firebrick1")) +  
  scale_color_manual(values=c("Density"="darkred")) +labs(fill="Legend",title="Distribution of Floor")
ggplot(price_data, aes(`Number of floors`)) +
 geom_histogram(aes(y=after_stat(density), fill="Histogram"), bins=12, position="identity", alpha=0.5) +
 geom_density(aes(color="Density"),size = 1, alpha = 0) +scale_fill_manual(values=c("Histogram"="lightsalmon")) +  
 scale_color_manual(values=c("Density"="coral4")) +labs(fill="Legend",title="Distribution of Number of floors")            

#descriptives####
library(vcdExtra)
qual_rel <- function(p, q) {
  tbl <- table(p, q)
  print("Contingency Table:")
  print(tbl)
  print("Row Proportions:")
  print(proportions(tbl, 1))
  print("Table with Margins:")
  print(addmargins(tbl))
  print("Chi-square Test:")
  print(chisq.test(tbl, correct = FALSE))
  print("Gamma:")
  print(GKgamma(tbl,level =0.95 ))
}
qual_rel(price_data$`Apartment type`,price_data$Renovation)
qual_rel(price_data$`Apartment type`,price_data$`Metro station`)
qual_rel(price_data$`Apartment type`,price_data$`Number of rooms`)
qual_rel(price_data$Renovation,price_data$`Metro station`)
qual_rel(price_data$Renovation,price_data$`Number of rooms`)
qual_rel(price_data$`Metro station`,price_data$`Number of rooms`)

#for quantitative
quan_data<- price_data %>%select(Price_in_thousands,`Minutes to metro`,Area,`Living area`,`Kitchen area`,Floor,`Number of floors`)
View(quan_data)
describe(quan_data)
Desc(quan_data)           
library("Hmisc")
r <- rcorr(as.matrix(quan_data)) 
r
write.csv(describe(quan_data), "describe.csv")
library("GGally")
ggpairs(quan_data)
ggplot(price_data, aes(x=`Minutes to metro`, fill=`Metro station`)) +
  geom_bar(position="dodge") +
  labs(title="distance for metro station")

#logistic analysis#############
#creating binary var
price_data$living_bin <- ifelse( price_data$`Living area`>= median(price_data$`Living area`), 1, 0)

#full model 
attach(price_data)
logistic_model1 <- glm(living_bin ~ Area + `Kitchen area`+ `Number of rooms`+ Price_in_thousands, 
                       family = binomial(link = "logit"),data = price_data)
logistic_model1
summary(logistic_model1)
selection<-step(logistic_model1,direction = "both")
summary(selection)  

#reduced model(remove price as it is insig)
logistic_model <- glm(living_bin ~ Area + `Kitchen area`+ `Number of rooms`, 
                      family = binomial(link = "logit"),data = price_data)

logistic_model
summary(logistic_model)
exp(coef(logistic_model))

#VIF
car::vif(logistic_model)

#diviance
anova(logistic_model)

#McFadden's R2
library(pscl)
mcfadden_r2 <-pR2(logistic_model)["McFadden"]
mcfadden_r2

#prediction
predictions <- predict(logistic_model, type = "response")
summary(predictions)

#best cutoff point
library(ROCR)
pred<-prediction(predictions,price_data$living_bin)
roc<-performance(pred,"acc")
max<-which.max(slot(roc,'y.values')[[1]])
acc<-slot(roc,'y.values')[[1]][max]
cut<-slot(roc,'x.values')[[1]][max]
print(c(Accuracy = acc , Cutoff= cut))
predicted_class <- ifelse(predictions > 0.5483376 , 1, 0)

#accuracy
CM <- table(predicted_class, price_data$living_bin)
CM

# error metric
err_metric <- function(cm) {
  accuracy <- sum(diag(cm)) / sum(cm)
  print(paste("Accuracy:", accuracy))
}
err_metric(CM)

# Classification table with useful measures
library(caret)
library(knitr)
confusionMatrix(factor(price_data$living_bin), factor(predicted_class))
CM_totals<-addmargins(CM, margin = 1:2)
kable(CM_totals)

# ROC curve
library(pROC)
roc_score <- roc(price_data$living_bin, predicted_class)
plot(roc_score, main = "ROC Curve – Logistic Regression")
#or
roc_curve<-performance(pred,'tpr','fpr')
plot(roc_curve,colorize=T,xlab='1-specificity',ylab="sensitivity",main='ROC curve')
abline(0,1)

#area under the curve 
auc<-performance(pred,'auc')
auc<-unlist(slot(auc,'y.values'))
legend(0.4,0.2,auc,title= 'AUC',cex = 0.6)

#prepare and apply  test data #####
View(test)
summary(test)

#data prep
test[] <- lapply(test, function(x) {
  if (is.character(x)) {
    as.factor(x)  
  } else {
    x  
  }
})

#check missing values
p_na<-function(x){sum(is.na(x))/length(x)*100}
apply(test,2,p_na)
na.omit(test)

#remove non-used var 
test<- test %>%select(-Region)

#change number of rooms into factor 
table(test$`Number of rooms`)
breaks<-c(min(test$`Number of rooms`)-1,0,1,2,3,max(test$`Number of rooms`))
test$`Number of rooms`<- cut(test$`Number of rooms`,breaks,labels = c("no_rooms","single","two","three","more"))
summary(test$`Number of rooms`)

#change price structure to be in thousands (easier to read)
test$Price<- test$Price/1000
test <- test %>% rename(Price_in_thousands = Price)
summary(test$Price_in_thousands) 

# group metro stations
table(test$`Metro station`)
north_stations <- c("Алтуфьево","Санино","Долгопрудная","Улица Академика Королёва","Аникеевка ","Бутырская","ВДНХ","Верхние Котлы","Бескудниково" ,"Бабушкинская", "Белокаменная", "Беломорская","Бибирево","Верхние Лихоборы","Пятницкое шоссе", "Аникеевка", "Динамо", "Водный стадион", "Владыкино"," Водный стадион", "Войковская", "Волоколамская", "Дегунино", "Деловой центр", "Депо, Динамо", "Дмитровская","Зеленоград — Крюково", "Зорге", "Карамышевская","Коптево", "Красногвардейская","Красносельская", "Красные ворота", "Курская","Лианозово", "Лихоборы", "Марьина Роща", "Марьина Роща (Шереметьевская)","Медведково", "Митино", "Народное Ополчение","Новоподрезково", "Окружная", "Отрадное", "Петровско-Разумовская", "Покровское", "Планерная", "Полежаевская", "Речной вокзал", "Рижская", "Ростокино","Савеловская", "Савёловская", "Селигерская", "Сокол", "Стахановская", "Стрешнево", "Сходненская", "Суликатная", "Тимирязевская", "Трикотажная","Тушинская", "Физтех", "Хлебниково", "Ховрино", "Хорошево", "Хорошёво", "Хорошёвская", "Щукинская", "Яхромская")
south_stations <- c("Аннино","Академическая" ,"Аэропорт Внуково","Варшавская (Коломенское)","Бульвар Адмирала Ушакова","Братиславская","Битца", "Битцевский Парк","Борисово","Верхние котлы","Пенягино", "Пражская", "Беляево", "Раменки", "Выхино", "Красный Строитель", "Улица Дмитриевского","Верхние Лихоборы","Вешняки","Боровское шоссе", "Беляево ","Бунинская аллея","Бунинская Аллея","Бутово","Бульвар Дмитрия Донского", "Варшавская","Верхние котлы", "Вешняки", "Волгоградский проспект", "Воробьёвы горы", "Воронцовская", "Выставочная", "Говорово", "Гражданская", "Давыдково", "Добрынинская", "Домодедовская", "Дубровка", "Жулебино", "ЗИЛ", "Зюзино", "Зябликово", "Калитники", "Калужская", "Кантемировская", "Каховская", "Каширская", "Китай-город","Достоевская","Кожуховская", "Коломенская", "Коммунарка", "Коньково", "Косино", "Котельники","Крестьянская застава", "Крымская", "Кузьминки","Курьяново", "Ленинский проспект", "Лермонтовский проспект", "Лесопарковая", "Лухмановская", "Люблино", "Марксистская", "Марьино", "Матвеевская","Мичуринский проспект", "Москворечье", "Нагатинская", "Нагатинский Затон", "Нагорная", "Нахимовский проспект", "Новопеределкино", "Новоясеневская", "Новые Черемушки", "Новые Черёмушки", "Озёрная", "Ольховая", "Орехово", "Остафьево", "Парк Победы", "Перерва", "Печатники", "Подольск", "Прокшино", "Пролетарская", "Проспект Вернадского", "Профсоюзная", "Пыхтино", "Рабочий посёлок", "Рабочий Посёлок", "Рассказовка","Румянцево", "Саларьево", "Севастопольская", "Семеновская", "Семёновская", "Серпуховская", "Силикатная", "Сетунь", "Студенческая", "Текстильщики", "Теплый Стан", "Тёплый Стан", "Терехово", "Тропарево", "Тропарёво","Тульская", "Угрешская", "Улица Академика Янгеля", "Улица Горчакова", "Улица Скобелевская", "Улица Старокачаловская", "Университет", "Филатов Луг", "Царицыно", "Чертановская", "Шаболовская", "Шипиловская", "Щербинка", "Юго-Восточная", "Юго-Западная", "Южная", "Ясенево")
east_stations <- c("Авиамоторная", "Автозаводская","Соколиная гора","Библиотека им. Ленина", "Алексеевская","Проспект Мира", "Шоссе Энтузиастов","Лубянка", "Алма-Атинская", "Андроновка","Бауманская","Ботанический сад", "Бульвар Рокоссовского", "ВДНХ","Волжская", "Измайлово", "Измайловская","Косино", "Котельники","Кузнецкий мост","Лубянка ","Депо","Комсомольская","Лефортово", "Локомотив","Маяковская", "Менделеевская", "Москва-Товарная","Некрасовка","Нижегородская", "Новаторская", "Новогиреево", "Новодачная", "Новокосино", "Новохохловская", "Окская","Партизанская", "Первомайская", "Перово", "Преображенская площадь","Площадь Гагарина", "Площадь Ильича", "Площадь Революции", "Полянка", "Пушкинская", "Римская","Рязанский проспект", "Свиблово", "Соколиная Гора", "Сокольники", "Таганская","Смоленская", "Сретенский бульвар", "Сухаревская", "Тверская", "Театральная", "Третьяковская","Фонвизинская", "Черкизовская", "Шоссе энтузиастов", "Щелковская", "Щёлковская", "Электрозаводская")
west_stations <- c("Арбатская", "Аэропорт","Воробьевы горы","Терехово (Мнёвники)","Александровский сад","Аминьевская","Библиотека и Ленина","Выставочный центр", "Петровский Парк", "Багратионовская", "Беговая", "Белорусская", "Боровицкая","Балтийская", "Баррикадная","Внуково", "Волоколамская","Пионерская", "Раменки", "Петровский парк", "Выставочная", "Киевская","Кленовый бульвар", "Красногорская", "Краснопресненская","Красный Балтиец", "Кропоткинская", "Крылатское", "Кунцевская","Кутузовская", "Ломоносовский проспект", "Лужники", "Марк","Международная", "Минская", "Мнёвники", "Молодежная", "Молодёжная", "Мякинино", "Нахабино","Немчиновка", "Озёрная", "Октябрьское поле", "Опалиха", "Очаково", "Павшино", "Панфиловская","Новокузнецкая", "Новослободская", "Октябрьская", "Охотный ряд", "Павелецкая", "Парк культуры", "Парк Культуры","Площадь Гагарина", "Площадь Ильича", "Площадь Революции", "Полянка", "Пушкинская", "Римская","Сколково", "Славянский бульвар", "Солнцево", "Спартак", "Спортивная", "Строгино", "Сухаревская", "Тестовская", "Технопарк", "Трубная","Улица 1905 года", "Филевский парк", "Филёвский парк", "Фили", "ЦСКА", "Шелепиха","Тургеневская", "Фрунзенская", "Цветной бульвар", "Чеховская", "Чистые пруды", "Чкаловская")
test$`Metro station` <- ifelse(test$`Metro station` %in% north_stations, "North",
                               ifelse(test$`Metro station` %in% south_stations, "South",
                                      ifelse(test$`Metro station` %in% east_stations, "East",
                                             ifelse(test$`Metro station` %in% west_stations, "West", NA))))
test$`Metro station` <- as.factor(test$`Metro station`)
summary(test$`Metro station`)

#check for outliers
boxplot(test$Price_in_thousands)
boxplot(test$`Minutes to metro`)
boxplot(test$Area)
boxplot(test$`Living area`)
boxplot(test$`Kitchen area`)
boxplot(test$Floor)
boxplot(test$`Number of floors`)
#remove outliers
test[] <- lapply(test[], function(col) {
  if (is.numeric(col)) replace_outliers(col) else col
})

#descriptives
qual_rel(test$`Apartment type`,test$Renovation)

#for quantitative
quan_data<- test %>%select(Price_in_thousands,`Minutes to metro`,Area,`Living area`,`Kitchen area`,Floor,`Number of floors`)
View(quan_data)
describe(quan_data)
Desc(quan_data)
r <- rcorr(as.matrix(quan_data))
r
ggpairs(quan_data)

#applying on test data 
test$LivingAreaBinarytest <- ifelse( test$`Living area`>= median(test$`Living area`), 1, 0)
predicted_probs <- predict(logistic_model, test, type = "response")

# Convert probabilities to binary  
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Create a confusion matrix
actual_classes <- test$LivingAreaBinarytest
confusion_matrix_test <- table(Predicted = predicted_classes, Actual = actual_classes)
print(confusion_matrix_test)

# Accuracy
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(paste("Accuracy:", accuracy_test))

#regression analysis#####
#model 1
fit1 <- lm(Price_in_thousands ~ `Apartment type` + `Metro station` + `Minutes to metro` + 
             `Number of rooms` + Area + `Living area` + `Kitchen area`+ 
             Floor + `Number of floors` + Renovation, data = price_data)
summary(fit1)
library(ggplot2)
ggplot(data=price_data, aes(fit1$residuals)) +geom_histogram(binwidth=6, color="purple4", fill="black") +ggtitle("Histogram for Model Residuals")+labs(x="Residuals")

#normality of residuals
qqnorm(rstandard(fit1))
qqline(rstandard(fit1))

#homoscedasticity
plot(fit1$residuals ~ fit1$fitted.values,xlab="fitted values",ylab = "residuals",main="Fitted values vs Residuals")
abline(h=0, col="red")

#multicollinearity
library(mctest)
mctest(fit1, type="i")

#outliers
plot(cooks.distance(fit1), pch=16, col="blue",main="Cooks Distance")
boxplot(fit1$residuals,main="residuals boxplot")

#stepwise
selection<-step(fit1 ,direction = "both")
summary(selection)

#model 2
fit2 <- lm(log(Price_in_thousands) ~ `Apartment type` + `Metro station`+ `Minutes to metro` + 
             `Number of rooms` + Area + Floor +`Number of floors` + Renovation, data = price_data)
summary(fit2)

#checking normality of residuals
qqnorm(rstandard(fit2))
qqline(rstandard(fit2))

#homoscedasticity
plot(fit2$residuals ~ fit2$fitted.values,xlab="fitted values",ylab = "residuals",main="Fitted values vs Residuals")
abline(h=0, col="red")

#multicollinearity
mctest(fit2, type="i")

#outliers
plot(cooks.distance(fit2), pch=16, col="darkgreen",main="Cooks Distance")
boxplot(fit2$residuals,main="residuals boxplot")

#ml########
# Decision tree#
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
price_data$prcat <- as.factor(ifelse(price_data$Price_in_thousands >mean(price_data$Price_in_thousands) ,"high","low"))
test$prcat <- as.factor(ifelse(test$Price_in_thousands >mean(test$Price_in_thousands) ,"high","low"))
model1 <- rpart(prcat ~ ., data = price_data[,-c(1,12)], method = "class")
model1
rpart.plot(model1)

# feature importance
importance <- varImp(model1)
importance %>%arrange(importance,desc(Overall))

## Making predictions
pred <- predict(model1, newdata = test[,-c(1,12)], type = "class")
pred

## confusion matrix
confusionMatrix(test$prcat, pred)

#my_func##########
my_func <- function(current_price, interest_rate) {
  final_prices <-{}
  number_of_months <- c(24, 60, 120)  
  for (i in 1:length(number_of_months)) {
    final_price <- current_price + (current_price * (number_of_months[i] / 12) * interest_rate)
    final_prices[i] <- final_price
  }
  return(final_prices)
}

#example
final_prices<-my_func(2000000, 0.06)
final_prices

#another function idea (predict the price)
predicted_price <- function(apartment_type = c("New_bulding", "Secondary"),metro_station = c("East","North", "South", "West"),
                            minutes_to_metro,number_of_rooms = c("no_rooms","single", "two", "three", "more"),
                            area,floor,number_of_floors,renovation = c("Renovation","Designer", "European-style renovation", "Without renovation")) 
{
  log_price <- 7.99 +(ifelse(apartment_type == "Secondary", 0.5, 0)) +(ifelse(metro_station == "North", -0.18, 0)) +
    (ifelse(metro_station == "South", -0.06, 0)) +(ifelse(metro_station == "West", -0.03, 0)) +(minutes_to_metro * -0.003) +
    (ifelse(number_of_rooms == "single", 0.33, 0)) + (ifelse(number_of_rooms == "two", 0.36, 0)) +(ifelse(number_of_rooms == "three", 0.36, 0)) +
    (ifelse(number_of_rooms == "more", 0.26, 0)) +(area * 0.013) +(floor * 0.005) +(number_of_floors * 0.002) +
    (ifelse(renovation == "Designer", 0.29, 0)) + (ifelse(renovation == "European-style renovation", 0.25, 0)) +(ifelse(renovation == "Without renovation", 0.28, 0))
  price <- exp(log_price)
  price_category <- ifelse(price > mean(price_data$Price_in_thousands), "high", "low")
  print(c(price_in_thousands= round(price) , apartment_cat= price_category))
}







