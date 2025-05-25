library(readxl)
categorical_variablesxlsx <- read_excel("~/Downloads/categorical variablesxlsx.xlsx")
View(categorical_variablesxlsx)
summary(categorical_variablesxlsx)
categorical_variablesxlsx<- lapply(categorical_variablesxlsx, as.factor)
categorical_variablesxlsx<-as.data.frame(categorical_variablesxlsx)
colnames(categorical_variablesxlsx) <- c("y", paste("x", 1:7,sep=""))
#removing missing/don't know/no answer responses
categorical_variablesxlsx$x1[categorical_variablesxlsx$x1 == -5] <- NA
categorical_variablesxlsx$x1[categorical_variablesxlsx$x1 == -2] <- NA
categorical_variablesxlsx$x2[categorical_variablesxlsx$x2 == -5] <- NA
categorical_variablesxlsx$x2[categorical_variablesxlsx$x2 == -1] <- NA
categorical_variablesxlsx$x3[categorical_variablesxlsx$x3 == -2] <- NA
categorical_variablesxlsx$x4[categorical_variablesxlsx$x4 == -2] <- NA
categorical_variablesxlsx$x5[categorical_variablesxlsx$x5 == -5] <- NA
categorical_variablesxlsx$x5[categorical_variablesxlsx$x5 == -2] <- NA
categorical_variablesxlsx$x5[categorical_variablesxlsx$x5 == -1] <- NA
categorical_variablesxlsx$y[categorical_variablesxlsx$y == -2] <- NA
sum(is.na(categorical_variablesxlsx))/1039
clean_data<-na.omit(categorical_variablesxlsx)
summary(clean_data)
summary(clean_data$y)
summary(clean_data$x1)
summary(clean_data$x2)
summary(clean_data$x3)
summary(clean_data$x4)
summary(clean_data$x5)
summary(clean_data$x6)
summary(clean_data$x7)
#clean_data[c(which(clean_data$y==1),which(clean_data$y==2),which(clean_data$y==3)),1]<-"low"
#spliting each variable into categories 
library(dplyr)
clean_data <-  clean_data%>%
  mutate(
    y = case_when(
      y %in% 1:3 ~ "Completely Disagree",
      y %in% 4:7 ~ "Neutral",
      y %in% 8:10 ~ "Completely Agree",
      TRUE ~ as.character(y)))
clean_data$y <- factor(clean_data$y, levels = c("Completely Disagree", "Neutral", "Completely Agree"))

clean_data <-  clean_data%>%
  mutate(
    x1 = case_when(
      x1 %in% 1 ~ "Lower",
      x1 %in% 2 ~ "Middle",
      x1 %in% 3 ~ "Higher",
      TRUE ~ as.character(x1)))
clean_data$x1 <- factor(clean_data$x1, levels = c("Lower", "Middle", "Higher"))

clean_data <-  clean_data%>%
  mutate(
    x3 = case_when(
      x3 %in% 1 ~ "16-29",
      x3 %in% 2 ~ "30-49",
      x3 %in% 3 ~ "50+",
      TRUE ~ as.character(x3)))
clean_data$x3 <- factor(clean_data$x3, levels = c("16-29", "30-49", "50+"))

clean_data <-  clean_data%>%
  mutate(
    x2 = case_when(
      x2 %in% 1 ~ "Male",
      x2 %in% 2 ~ "Female",
      TRUE ~ as.character(x2)))
clean_data$x2 <- factor(clean_data$x2, levels = c("Male", "Female"))

clean_data <-  clean_data%>%
  mutate(
    x4 = case_when(
      x4 %in% 1:3 ~ "Completely Disagree",
      x4 %in% 4:7 ~ "Neutral",
      x4 %in% 8:10 ~ "Completely Agree",
      TRUE ~ as.character(x4)))
clean_data$x4 <- factor(clean_data$x4, levels = c("Completely Disagree", "Neutral", "Completely Agree"))

clean_data <-  clean_data%>%
  mutate(
    x5 = case_when(
      x5 %in% 1:3 ~ "Completely Disagree",
      x5 %in% 4:7 ~ "Neutral",
      x5 %in% 8:10 ~ "Completely Agree",
      TRUE ~ as.character(x5)))
clean_data$x5 <- factor(clean_data$x5, levels = c("Completely Disagree", "Neutral", "Completely Agree"))


clean_data <-  clean_data%>%
  mutate(
    x6 = case_when(
      x6 %in% 1:3 ~ "Alot Worse Off",
      x6 %in% 4:7 ~ "Neutral",
      x6 %in% 8:10 ~ "Alot Better Off",
      TRUE ~ as.character(x6)))
clean_data$x6 <- factor(clean_data$x6, levels = c("Alot Worse Off", "Neutral", "Alot Better Off"))

clean_data <-  clean_data%>%
  mutate(
    x7 = case_when(
      x7 %in% 1:3 ~ "Not At All Important",
      x7 %in% 4:7 ~ "Neutral",
      x7 %in% 8:10 ~ "Very Important",
      TRUE ~ as.character(x7)))
clean_data$x7 <- factor(clean_data$x7, levels = c("Not At All Important", "Neutral", "Very Important"))
#which(is.na(clean_data))


# 2 way contingency table


# Load the knitr package for formatting
install.packages("knitr")
library(knitr)

#x111111111111111111

# Create the contingency table
table1 <- table(clean_data$x1, clean_data$y)

# Add margins (row and column totals)
table_with_margins1 <- addmargins(table1)

table1
prop.table(table1, 1)

# Print the table with lines between rows using kable
kable(table_with_margins1, format = "markdown", align = "c", 
      caption = "Contingency Table between Educational level and the Response Variable",
      table.attr = "class='table-hover'")

#measure of association
DescTools::GoodmanKruskalGamma(table1, conf.level=0.95)

#chi square test for independence
chi=chisq.test(table1, correct = FALSE)
chi                                         #correct means correction factor
chi$expected

#x222222222222222222
# Create the contingency table
table2 <- table(clean_data$x2, clean_data$y)

# Add margins (row and column totals)
table_with_margins2 <- addmargins(table2)

table2
prop.table(table2, 1)

# Print the table with lines between rows using kable
kable(table_with_margins2, format = "markdown", align = "c", 
      caption = "Contingency Table between Sex and the Response Variable",
      table.attr = "class='table-hover'")

#measure of association
#cramers v
# Calculate Cramér's V
library(DescTools)
CramerV(table2)



#chi square test for independence
chi=chisq.test(table2, correct = FALSE)
chi                                         #correct means correction factor
chi$expected

#x33333333333333333
# Create the contingency table
table3 <- table(clean_data$x3, clean_data$y)

# Add margins (row and column totals)
table_with_margins3 <- addmargins(table3)

table3
prop.table(table3, 1)

# Print the table with lines between rows using kable
kable(table_with_margins3, format = "markdown", align = "c", 
      caption = "Contingency Table between Age and the Response Variable",
      table.attr = "class='table-hover'")

#measure of association
DescTools::GoodmanKruskalGamma(table3, conf.level=0.95)


#chi square test for independence
chi=chisq.test(table3, correct = FALSE)
chi                                         #correct means correction factor
chi$expected

#x444444444444444444
# Create the contingency table
table4 <- table(clean_data$x4, clean_data$y)

# Add margins (row and column totals)
table_with_margins4 <- addmargins(table4)

table4
prop.table(table4, 1)

# Print the table with lines between rows using kable
kable(table_with_margins4, format = "markdown", align = "c", 
      caption = "Contingency Table between 'Dependence on Science not faith' and the Response Variable",
      table.attr = "class='table-hover'")

#measure of association
DescTools::GoodmanKruskalGamma(table4, conf.level=0.95)


#chi square test for independence
chi=chisq.test(table4, correct = FALSE)
chi                                         #correct means correction factor
chi$expected

#x55555555555555
# Create the contingency table
table5 <- table(clean_data$x5, clean_data$y)

# Add margins (row and column totals)
table_with_margins5 <- addmargins(table5)

table5
prop.table(table5, 1)

# Print the table with lines between rows using kable
kable(table_with_margins5, format = "markdown", align = "c", 
      caption = "Contingency Table between 'Science breaks ideas of R & W'  and the Response Variable",
      table.attr = "class='table-hover'")

#measure of association
DescTools::GoodmanKruskalGamma(table5, conf.level=0.95)



#chi square test for independence
chi=chisq.test(table5, correct = FALSE)
chi                                         #correct means correction factor
chi$expected


#x66666666666666
# Create the contingency table
table6 <- table(clean_data$x6, clean_data$y)

# Add margins (row and column totals)
table_with_margins6 <- addmargins(table6)

table6
prop.table(table6, 1)

# Print the table with lines between rows using kable
kable(table_with_margins6, format = "markdown", align = "c", 
      caption = "Contingency Table between 'World is better or worse bec. of sci. and tech.' and the Response Variable",
      table.attr = "class='table-hover'")

#measure of association
DescTools::GoodmanKruskalGamma(table6, conf.level=0.95)


#chi square test for independence
chi=chisq.test(table6, correct = FALSE)
chi                                         #correct means correction factor
chi$expected

#x777777777777
# Create the contingency table
table7 <- table(clean_data$x7, clean_data$y)

# Add margins (row and column totals)
table_with_margins7 <- addmargins(table1)

table7
prop.table(table7, 1)

# Print the table with lines between rows using kable
kable(table_with_margins7, format = "markdown", align = "c", 
      caption = "Contingency Table between 'Importance of God' and the Response Variable",
      table.attr = "class='table-hover'")

#measure of association
DescTools::GoodmanKruskalGamma(table7, conf.level=0.95)

#chi square test for independence
chi=chisq.test(table7, correct = FALSE)
chi                                         #correct means correction factor
chi$expected


#3 wayyyy
table <- table(clean_data$x6,clean_data$x4,clean_data$y)
tt <- ftable(table)
tt
kable(tt, format = "markdown", align = "c", 
      caption = "3x3 Contingency Table",
      table.attr = "class='table-hover'")

table_1 <-  xtabs(frequency ~ y + x2 + x4, data = clean_data) 

table_1.m <-  xtabs(Frequency ~ y + x2, data = subset(Book1, Gender == "Male")) 
table_1.m

table_1.f <-  xtabs(Frequency ~ Affiliation + `Age group`, data = subset(Book1, Gender == "Female")) 
table_1.f

chisq.test(table_1.m)
chisq.test(table_1.f)

mantelhaen.test(tt)


