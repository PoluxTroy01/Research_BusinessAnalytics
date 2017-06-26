# ||| Data Exploration |||
set.seed(123)

library(ggplot2)
library(car) # For the Levene's Test
library(e1071)
library(nortest)
library(remotes) # The substitute of devtools if it can not be installed to install remote repositories
library(lda)
library(dplyr)
library(caret)
library(mice)
library(VIM)
library(lattice)
library(missForest)

# Research Business Analytics

startup<- read.csv(file="CAX_Startup_Data.csv", header=TRUE,as.is=T)

# ||| Getting sense of inconsistencies |||
# Replacing 'No Info' and ''
startup[startup=='No Info'] <- NA
startup[startup==""] <- NA

# Converting columns as Date
startup$Est..Founding.Date <- as.Date(startup$Est..Founding.Date, "%m/%d/%Y")
startup$Last.Funding.Date <- as.Date(startup$Last.Funding.Date, "%m/%d/%Y")

# noting columns that needs to be converted to numeric
col<- c(3:5,10,11,18:23,25,61,66,68:70,72,74,88,92,94:96,98,99,102:116)
for(i in col){
  startup[,i] <- as.numeric(startup[,i])
}
str(startup)

# ||| Understand the amount of missing values in the data |||
# Percent missing value for each variable
mis_val<-sapply(startup, function(x) sum(is.na(x)))
percent_mis<-as.data.frame(round((mis_val/nrow(startup))*100,1))

# Making a dataframe with variable and missing value percent for filtering
name <- row.names(percent_mis)
pcnt_mis_var <- cbind(name, percent_mis)
row.names(pcnt_mis_var) <- NULL
colnames(pcnt_mis_var) <- c("variable", "Percent.Missing")



# Check which variables are numeric
# Here is with its integer value
numeric_var <- sapply(startup, is.numeric)
numeric_cols <- which(numeric_var == 1)
cnt_data <- startup[, c(numeric_cols, 13:14)]

# Separate data frame from character variables
var <- colnames(startup) %in% colnames(cnt_data)
char_data <- startup[!var]


# Now we only have 2 data sets (continuous and character) with all the missing points


# Adding dependent variable to numeric data frame
cnt_data$Dependent.Company.Status <- char_data$Dependent.Company.Status



# Similarly, categorical variable can be explored individually to check for inconsistency, 
# missing value and appropriate treatment.
table(char_data$Local.or.global.player,useNA="always")

# Convert a variable to uppercase
char_data$Local.or.global.player <- toupper(char_data$Local.or.global.player)

# Still one level seems to be different which is due to whitespaces.
char_data$Local.or.global.player<-trimws(char_data$Local.or.global.player)

# Create additional features like counting number of investors for company
# This line will separate the Investors and count them length(strsplit(char_df$Investors[1], "|", fixed=T))
char_data$Investor.count <- length(strsplit(char_data$Investors, "|", fixed=T))
for(i in (1:length(char_data$Investors))){
  if(is.na(char_data$Investors[i])){
    char_data$Investor.count[i] <- NA 
  }else{
    lst <- strsplit(char_data$Investors[i], "|", fixed=T)
    char_data$Investor.count[i] <- length(lst[[1]])
  }
}





######################## -------------------------------------------------------------------

laClase <- sapply(cnt_data, class)
nombres <- colnames(cnt_data)


sapply(cnt_data, function(x){sd(x, na.rm=T)})

sapply(cnt_data[-c(45,44,43)], function(x){skewness(x, na.rm=T)})


correlations <- as.data.frame(cor(cnt_data[,-c(45,44,43)], use = "na.or.complete", method = "pearson"))


leveneTest(cnt_data$Employee.Count, cnt_data$Age.of.company.in.years, center=mean)

skewness(cnt_data$year.of.founding, na.rm=T)






ggplot(cnt_data, aes(Team.size.all.employees)) + 
  geom_histogram()
boxplot(cnt_data$Team.size.all.employees, horizontal = T)
skewness(cnt_data$Team.size.all.employees, na.rm=T)
summary(cnt_data$Team.size.all.employees)
quantile(cnt_data$Team.size.all.employees, probs = seq(0, 1, by= 0.05),na.rm=T)
quantile(cnt_data$Team.size.all.employees, probs = seq(.9, 1, by= 0.01),na.rm=T)


ggplot(cnt_data, aes(year.of.founding)) + 
  geom_histogram()
boxplot(cnt_data$year.of.founding, horizontal = T)
ggplot(cnt_data, aes(year.of.founding)) + 
  geom_density()
skewness(cnt_data$year.of.founding, na.rm=T)
summary(cnt_data$year.of.founding)
quantile(cnt_data$year.of.founding, probs = seq(0, 1, by= 0.05),na.rm=T)


# To make the Levene's Test on Team.size.all.employees
t.test(Team.size.all.employees~Dependent.Company.Status, data=cnt_data)
ggplot(cnt_data, aes(Team.size.all.employees)) + geom_density(alpha = 0.3)
skewness(cnt_data$Team.size.all.employees, na.rm = T)

cnt_data$cnt_response <- sapply(cnt_data$Dependent.Company.Status, function(x){
  if(x == "Success"){
    cnt_data$cnt_response = 1
  }else{
    cnt_data$cnt_response = 0
  }
})

leveneTest(cnt_data$cnt_response, cnt_data$Team.size.all.employees, center = mean)  
leveneTest(y = cnt_data$Team.size.all.employees, group = cnt_data$Dependent.Company.Status, center = mean)
var.test(cnt_data$cnt_response, cnt_data$Team.size.all.employees)


# To see if the missing values are just at random
cnt_data$auxiliary_mis <- sapply(cnt_data$Team.size.all.employees, function(x){
  if(is.na(x)){
    cnt_data$auxiliary_mis = 1
  }else{
    cnt_data$auxiliary_mis = 0
  }
})

t.test(cnt_response ~ auxiliary_mis, data = cnt_data)
t.test(auxiliary_mis ~ cnt_response, data = cnt_data)
t.test(Team.size.all.employees ~ Dependent.Company.Status, data = cnt_data)

wilcox.test(Team.size.all.employees ~ Dependent.Company.Status, data = cnt_data)

sd(cnt_data$Team.size.all.employees, na.rm = T)
median(cnt_data$Team.size.all.employees, na.rm = T)
var(cnt_data$Team.size.all.employees, na.rm = T)


skewness(cnt_data$cnt_response)
ggplot(cnt_data, aes(cnt_response)) + 
  geom_density()

pairs(cnt_data)
pairs(char_data)

data("iris")
pairs(iris)


library(caret)
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(iris[,1:4], method=c("range"))
# transform the dataset using the pre-processing parameters
transformed <- predict(preprocessParams, iris[,1:4])
# summarize the transformed dataset
summary(transformed)



"Para poder dividir la pantalla y poner dos gráficas de ANOVA"
par(mfrow = c(1,2))
plot(aov_year, 1)
plot(aov_year, 2)

###################### ------------------------------------------------------------------------

quantile(cnt_data$Team.size.all.employees, probs = seq(0, 1, by = 0.05), na.rm = T)
ggplot(cnt_data, aes(x = Team.size.all.employees)) + geom_density()
ggplot(cnt_data, aes(x = Dependent.Company.Status, y = Team.size.all.employees)) + geom_boxplot()
# Test for normality
shapiro.test(cnt_data$Team.size.all.employees)
ks.test(cnt_data$Team.size.all.employees, "pnorm", alternative="two.sided")

# Test for the response variable: Dependent.Company.Status
ad.test(cnt_data$cnt_response)
shapiro.test(cnt_data$cnt_response)
ggplot(cnt_data, aes(x = cnt_response)) + geom_density()

# Tests for "year.of.founding"
ad.test(cnt_data$year.of.founding)
shapiro.test(cnt_data$year.of.founding)
ks.test(cnt_data$year.of.founding, "pnorm", alternative="two.sided")
ggplot(cnt_data, aes(x = year.of.founding)) + geom_density()

# Tests for Internet.Activity.Score
summary(cnt_data$Internet.Activity.Score)
quantile(cnt_data$Internet.Activity.Score, probs = seq(0, 1, by = 0.05), na.rm = T)
ggplot(cnt_data, aes(x = Internet.Activity.Score)) + geom_density()
ggplot(cnt_data, aes(x = Dependent.Company.Status, y = Internet.Activity.Score)) + geom_boxplot()
ad.test(cnt_data$Internet.Activity.Score)
shapiro.test(cnt_data$Internet.Activity.Score)










# Lo mejor será ver qué porcentaje de missing values tiene cada una de las variables
percent_mis

# después probar si es que están ahí por azar o si tienen algún significado
# luego buscar la relación de las variables con un bivariate analysis

# PRIMERO LAS VARIABLES CONTINUAS: cnt_data

# ||| Probar primero la asosiación o disociación entre las variables |||
# 1 "year.of.founding" percent_mis = 12.5%
ad.test(cnt_data$year.of.founding) # For normality
qqnorm(cnt_data$year.of.founding)
qqline(cnt_data$year.of.founding)
shapiro.test(cnt_data$year.of.founding) # For normality
ks.test(cnt_data$year.of.founding, "pnorm", alternative="two.sided")
ggplot(cnt_data, aes(x = year.of.founding)) + geom_density()
    # Now to see the association
leveneTest(y = cnt_data$year.of.founding, group = cnt_data$Dependent.Company.Status, center = median) # Homogeneity of variance
ggplot(cnt_data, aes(x = Dependent.Company.Status, y = year.of.founding)) + geom_boxplot()
t.test(year.of.founding ~ Dependent.Company.Status, data = cnt_data) # Importante
wilcox.test(year.of.founding ~ Dependent.Company.Status, data = cnt_data) # Importante
aov_year <- aov(year.of.founding ~ Dependent.Company.Status, data = cnt_data)
summary(aov_year)
TukeyHSD(aov_year)

cnt_data$auxiliary_mis <- sapply(cnt_data$year.of.founding, function(x){
  if(is.na(x)){
    cnt_data$auxiliary_mis = 1
  }else{
    cnt_data$auxiliary_mis = 0
  }
})

t.test(cnt_response ~ auxiliary_mis, data = cnt_data) # Importante 
cnt_results <- data.frame("Variable" = "year.of.founding", 
                          "t_test_p-value" = 2.471e-09, 
                          "wilcox_p-value" = 6.501e-12,
                          "significant_missing" = 1)

# Las pruebas como el t.test o wilcox.test son para ver que tan representativo es el conjunto de 
# datos para describir, en este caso, qué empresas son exitosas de las que no.
# Hipótesis nula: NO HAY RELACION ENTRE A Y B
    # Nota: Solo como nota, se puede prescindir de la distribución:
        # A normal distribution of difference scores is strongly encouraged, unless the sample is 
        # large enough that you can hide behind the central limit theorem and claim a normal sampling 
        # distribution of means of the differences, in which case the t-test is robust to violations
        # of the normality assumption.




# De ahora en adelante solo será repetir los comandos para las otras variables
# 2 "Age.of.company.in.years" percent_miss = 12.5
leveneTest(y = cnt_data$Age.of.company.in.years, group = cnt_data$Dependent.Company.Status, center = median) # p-value 0.2952
ad.test(cnt_data$Age.of.company.in.years) # p-value < 2.2e-16
shapiro.test(cnt_data$Age.of.company.in.years) # p-value = 9.514e-16
qqnorm(cnt_data$Age.of.company.in.years)  
qqline(cnt_data$Age.of.company.in.years)
ggplot(cnt_data, aes(x = Dependent.Company.Status, y = Age.of.company.in.years)) + geom_boxplot()
ggplot(cnt_data, aes(x = Age.of.company.in.years)) + geom_density()
  # Now see the association
t.test(Age.of.company.in.years ~ Dependent.Company.Status, data = cnt_data) # Importante
wilcox.test(Age.of.company.in.years ~ Dependent.Company.Status, data = cnt_data) # Importante
cnt_data$auxiliary_mis <- sapply(cnt_data$Age.of.company.in.years, function(x){
  if(is.na(x)){
    cnt_data$auxiliary_mis = 1
  }else{
    cnt_data$auxiliary_mis = 0
  }
})

t.test(cnt_response ~ auxiliary_mis, data = cnt_data) # Importante 
n_var <- data.frame("Variable" = "Age.of.company.in.years", 
                          "t_test_p-value" = 2.471e-9, 
                          "wilcox_p-value" = 6.501e-12,
                          "significant_missing" = 2.2e-16,
                          "percent_miss" =  12.5)


cnt_results <- rbind(cnt_results, n_var)

# 3 "Internet.Activity.Score" percent_mis = 13.8
ad.test(cnt_data$Internet.Activity.Score) # p-value < 2.2e-16
shapiro.test(cnt_data$Internet.Activity.Score) # p-value < 2.2e-16
qqnorm(cnt_data$Internet.Activity.Score)
qqline(cnt_data$Internet.Activity.Score)
ggplot(cnt_data, aes(x = Dependent.Company.Status, y = Internet.Activity.Score)) + geom_boxplot()
ggplot(cnt_data, aes(x = Internet.Activity.Score)) + geom_density()
  # Now see the association
t.test(Internet.Activity.Score ~ Dependent.Company.Status, data = cnt_data) # p-value < 2.2e-16
wilcox.test(Internet.Activity.Score ~ Dependent.Company.Status, data = cnt_data) # p-value < 2.2e-16
cnt_data$auxiliary_mis <- sapply(cnt_data$Internet.Activity.Score, function(x){
  if(is.na(x)){
    cnt_data$auxiliary_mis = 1
  }else{
    cnt_data$auxiliary_mis = 0
  }
})
t.test(cnt_response ~ auxiliary_mis, data = cnt_data) # Importante
n_var <- data.frame("Variable" = "Internet.Activity.Score", 
                    "t_test_p-value" = 2.2e-16, 
                    "wilcox_p-value" = 2.2e-16,
                    "significant_missing" = 1)

cnt_results <- rbind(cnt_results, n_var)

# 4 "Employee.Count" percent_mis = 35.2
ad.test(cnt_data$Employee.Count) # p-value < 2.2e-16
shapiro.test(cnt_data$Employee.Count) # p-value < 2.2e-16
qqnorm(cnt_data$Employee.Count)
qqline(cnt_data$Employee.Count)
ggplot(cnt_data, aes(x = Dependent.Company.Status, y = Employee.Count)) + geom_boxplot()
ggplot(cnt_data, aes(x = Employee.Count)) + geom_density()
# Now see the association
t.test(Employee.Count ~ Dependent.Company.Status, data = cnt_data) # p-value < 2.2e-16
wilcox.test(Employee.Count ~ Dependent.Company.Status, data = cnt_data) # p-value < 2.2e-16
cnt_data$auxiliary_mis <- sapply(cnt_data$Employee.Count, function(x){
  if(is.na(x)){
    cnt_data$auxiliary_mis = 1
  }else{
    cnt_data$auxiliary_mis = 0
  }
})
t.test(cnt_response ~ auxiliary_mis, data = cnt_data) # Importante
n_var <- data.frame("Variable" = "Employee.Count", 
                    "t_test_p-value" = 0.0157, 
                    "wilcox_p-value" = 3.538e-06,
                    "significant_missing" = 1)

cnt_results <- rbind(cnt_results, n_var)

# 5 "Employees.count.MoM.change" percent_mis = 43.4
ad.test(cnt_data$Employees.count.MoM.change) # p-value < 2.2e-16
shapiro.test(cnt_data$Employees.count.MoM.change) # p-value < 2.2e-16
qqnorm(cnt_data$Employees.count.MoM.change)
qqline(cnt_data$Employees.count.MoM.change)
ggplot(cnt_data, aes(x = Dependent.Company.Status, y = Employees.count.MoM.change)) + geom_boxplot()
ggplot(cnt_data, aes(x = Employees.count.MoM.change)) + geom_density()
# Now see the association
t.test(Employees.count.MoM.change ~ Dependent.Company.Status, data = cnt_data) # p-value = 0.002484
wilcox.test(Employees.count.MoM.change ~ Dependent.Company.Status, data = cnt_data) # p-value = 0.0008696
cnt_data$auxiliary_mis <- sapply(cnt_data$Employees.count.MoM.change, function(x){
  if(is.na(x)){
    cnt_data$auxiliary_mis = 1
  }else{
    cnt_data$auxiliary_mis = 0
  }
})
t.test(cnt_response ~ auxiliary_mis, data = cnt_data) # Importante
n_var <- data.frame("Variable" = "Employees.count.MoM.change", 
                    "t_test_p-value" = 0.002484, 
                    "wilcox_p-value" = 0.0008696,
                    "significant_missing" = 1)

cnt_results <- rbind(cnt_results, n_var)

# 6 "Last.Funding.Amount" percent_mis = 33.9
ad.test(cnt_data$Last.Funding.Amount) # p-value < 2.2e-16
shapiro.test(cnt_data$Last.Funding.Amount) # p-value < 2.2e-16
qqnorm(cnt_data$Last.Funding.Amount)
qqline(cnt_data$Last.Funding.Amount)
ggplot(cnt_data, aes(x = Dependent.Company.Status, y = Last.Funding.Amount)) + geom_boxplot()
ggplot(cnt_data, aes(x = Last.Funding.Amount)) + geom_density()
# Now see the association
t.test(Last.Funding.Amount ~ Dependent.Company.Status, data = cnt_data) # p-value = 0.6208
wilcox.test(Last.Funding.Amount ~ Dependent.Company.Status, data = cnt_data) # p-value = 0.636
cnt_data$auxiliary_mis <- sapply(cnt_data$Last.Funding.Amount, function(x){
  if(is.na(x)){
    cnt_data$auxiliary_mis = 1
  }else{
    cnt_data$auxiliary_mis = 0
  }
})
t.test(cnt_response ~ auxiliary_mis, data = cnt_data) # Importante
n_var <- data.frame("Variable" = "Last.Funding.Amount", 
                    "t_test_p-value" = 0.6208, 
                    "wilcox_p-value" = 0.636,
                    "significant_missing" = 0.001176)

cnt_results <- rbind(cnt_results, n_var)

# 7 "Number.of.Investors.in.Seed" percent_mis = 10.4
ad.test(cnt_data$Number.of.Investors.in.Seed) # p-value < 2.2e-16
shapiro.test(cnt_data$Number.of.Investors.in.Seed) # p-value < 2.2e-16
qqnorm(cnt_data$Number.of.Investors.in.Seed)
qqline(cnt_data$Number.of.Investors.in.Seed)
ggplot(cnt_data, aes(x = Dependent.Company.Status, y = Number.of.Investors.in.Seed)) + geom_boxplot()
ggplot(cnt_data, aes(x = Number.of.Investors.in.Seed)) + geom_density()
# Now see the association
t.test(Number.of.Investors.in.Seed ~ Dependent.Company.Status, data = cnt_data) # p-value = 0.0002137
wilcox.test(Number.of.Investors.in.Seed ~ Dependent.Company.Status, data = cnt_data) # p-value = 0.0002548
cnt_data$auxiliary_mis <- sapply(cnt_data$Number.of.Investors.in.Seed, function(x){
  if(is.na(x)){
    cnt_data$auxiliary_mis = 1
  }else{
    cnt_data$auxiliary_mis = 0
  }
})
t.test(cnt_response ~ auxiliary_mis, data = cnt_data) # Importante
n_var <- data.frame("Variable" = "Number.of.Investors.in.Seed", 
                    "t_test_p-value" = 0.0002137, 
                    "wilcox_p-value" = 0.0002548,
                    "significant_missing" = 1)

cnt_results <- rbind(cnt_results, n_var)

# 8 "Number.of.Investors.in.Angel.and.or.VC" percent_mis = 10.4
ad.test(cnt_data$Number.of.Investors.in.Angel.and.or.VC) # p-value < 2.2e-16
shapiro.test(cnt_data$Number.of.Investors.in.Angel.and.or.VC) # p-value < 2.2e-16
qqnorm(cnt_data$Number.of.Investors.in.Angel.and.or.VC)
qqline(cnt_data$Number.of.Investors.in.Angel.and.or.VC)
ggplot(cnt_data, aes(x = Dependent.Company.Status, y = Number.of.Investors.in.Angel.and.or.VC)) + geom_boxplot()
ggplot(cnt_data, aes(x = Number.of.Investors.in.Angel.and.or.VC)) + geom_density()
# Now see the association
t.test(Number.of.Investors.in.Angel.and.or.VC ~ Dependent.Company.Status, data = cnt_data) # p-value = 0.2005
wilcox.test(Number.of.Investors.in.Angel.and.or.VC ~ Dependent.Company.Status, data = cnt_data) # p-value = 0.5221
cnt_data$auxiliary_mis <- sapply(cnt_data$Number.of.Investors.in.Angel.and.or.VC, function(x){
  if(is.na(x)){
    cnt_data$auxiliary_mis = 1
  }else{
    cnt_data$auxiliary_mis = 0
  }
})
t.test(cnt_response ~ auxiliary_mis, data = cnt_data) # Importante
n_var <- data.frame("Variable" = "Number.of.Investors.in.Angel.and.or.VC", 
                    "t_test_p-value" = 0.2005, 
                    "wilcox_p-value" = 0.5221,
                    "significant_missing" = 1)

cnt_results <- rbind(cnt_results, n_var)

# 9 "Number.of.Co.founders" percent_mis = 0 
ad.test(cnt_data$Number.of.Co.founders) # p-value < 2.2e-16
shapiro.test(cnt_data$Number.of.Co.founders) # p-value = 4.85e-16
qqnorm(cnt_data$Number.of.Co.founders)
qqline(cnt_data$Number.of.Co.founders)
ggplot(cnt_data, aes(x = Dependent.Company.Status, y = Number.of.Co.founders)) + geom_boxplot()
ggplot(cnt_data, aes(x = Number.of.Co.founders)) + geom_density()
# Now see the association
t.test(Number.of.Co.founders ~ Dependent.Company.Status, data = cnt_data) # p-value = 9.582e-06
wilcox.test(Number.of.Co.founders ~ Dependent.Company.Status, data = cnt_data) # p-value = 1.897e-06
cnt_data$auxiliary_mis <- sapply(cnt_data$Number.of.Co.founders, function(x){
  if(is.na(x)){
    cnt_data$auxiliary_mis = 1
  }else{
    cnt_data$auxiliary_mis = 0
  }
})
t.test(cnt_response ~ auxiliary_mis, data = cnt_data) # Importante
n_var <- data.frame("Variable" = "Number.of.Co.founders", 
                    "t_test_p-value" = 9.582e-06, 
                    "wilcox_p-value" = 1.897e-06,
                    "significant_missing" = 1)

cnt_results <- rbind(cnt_results, n_var)

# 10 "Number.of.of.advisors" percent_mis = 0
ad.test(cnt_data$Number.of.of.advisors) # p-value < 2.2e-16
shapiro.test(cnt_data$Number.of.of.advisors) # p-value = 2.2e-16
qqnorm(cnt_data$Number.of.of.advisors)
qqline(cnt_data$Number.of.of.advisors)
ggplot(cnt_data, aes(x = Dependent.Company.Status, y = Number.of.of.advisors)) + geom_boxplot()
ggplot(cnt_data, aes(x = Number.of.of.advisors)) + geom_density()
# Now see the association
t.test(Number.of.of.advisors ~ Dependent.Company.Status, data = cnt_data) # p-value = 9.553e-07
wilcox.test(Number.of.of.advisors ~ Dependent.Company.Status, data = cnt_data) # p-value = 0.0008003
cnt_data$auxiliary_mis <- sapply(cnt_data$Number.of.of.advisors, function(x){
  if(is.na(x)){
    cnt_data$auxiliary_mis = 1
  }else{
    cnt_data$auxiliary_mis = 0
  }
})
t.test(cnt_response ~ auxiliary_mis, data = cnt_data) # Importante
n_var <- data.frame("Variable" = "Number.of.of.advisors", 
                    "t_test_p-value" = 9.553e-07, 
                    "wilcox_p-value" = 0.0008003,
                    "significant_missing" = 1)

cnt_results <- rbind(cnt_results, n_var)

# 11 "Team.size.Senior.leadership"
ad.test(cnt_data$Team.size.Senior.leadership) # p-value < 2.2e-16
shapiro.test(cnt_data$Team.size.Senior.leadership) # p-value = 2.2e-16
qqnorm(cnt_data$Team.size.Senior.leadership)
qqline(cnt_data$Team.size.Senior.leadership)
ggplot(cnt_data, aes(x = Dependent.Company.Status, y = Team.size.Senior.leadership)) + geom_boxplot()
ggplot(cnt_data, aes(x = Team.size.Senior.leadership)) + geom_density()
# Now see the association
t.test(Team.size.Senior.leadership ~ Dependent.Company.Status, data = cnt_data) # p-value = 2.352e-08
wilcox.test(Team.size.Senior.leadership ~ Dependent.Company.Status, data = cnt_data) # p-value = 1.474e-12
cnt_data$auxiliary_mis <- sapply(cnt_data$Team.size.Senior.leadership, function(x){
  if(is.na(x)){
    cnt_data$auxiliary_mis = 1
  }else{
    cnt_data$auxiliary_mis = 0
  }
})
t.test(cnt_response ~ auxiliary_mis, data = cnt_data) # Importante
n_var <- data.frame("Variable" = "Team.size.Senior.leadership", 
                    "t_test_p-value" = 2.352e-08, 
                    "wilcox_p-value" = 1.474e-12,
                    "significant_missing" = 1,
                    "percent_miss" = 0)

cnt_results <- rbind(cnt_results, n_var)

# 12 "Team.size.all.employees"
ad.test(cnt_data$Team.size.all.employees) # p-value < 2.2e-16
shapiro.test(cnt_data$Team.size.all.employees) # p-value < 2.2e-16
qqnorm(cnt_data$Team.size.all.employees)
qqline(cnt_data$Team.size.all.employees)
ggplot(cnt_data, aes(x = Dependent.Company.Status, y = Team.size.all.employees)) + geom_boxplot()
ggplot(cnt_data, aes(x = Team.size.all.employees)) + geom_density()
# Now see the association
t.test(Team.size.all.employees ~ Dependent.Company.Status, data = cnt_data) # p-value = 0.2085
wilcox.test(Team.size.all.employees ~ Dependent.Company.Status, data = cnt_data) # p-value = 0.001339
cnt_data$auxiliary_mis <- sapply(cnt_data$Team.size.all.employees, function(x){
  if(is.na(x)){
    cnt_data$auxiliary_mis = 1
  }else{
    cnt_data$auxiliary_mis = 0
  }
})
t.test(cnt_response ~ auxiliary_mis, data = cnt_data) # Importante
n_var <- data.frame("Variable" = "Team.size.all.employees", 
                    "t_test_p-value" = 0.2085, 
                    "wilcox_p-value" = 0.001339,
                    "significant_missing" = 1,
                    "percent_miss" = 14.4)

cnt_results <- rbind(cnt_results, n_var)

# 13 "Number.of.of.repeat.investors"
ad.test(cnt_data$Number.of.of.repeat.investors) # p-value < 2.2e-16
shapiro.test(cnt_data$Number.of.of.repeat.investors) # p-value < 2.2e-16
qqnorm(cnt_data$Number.of.of.repeat.investors)
qqline(cnt_data$Number.of.of.repeat.investors)
ggplot(cnt_data, aes(x = Dependent.Company.Status, y = Number.of.of.repeat.investors)) + geom_boxplot()
ggplot(cnt_data, aes(x = Number.of.of.repeat.investors)) + geom_density()
# Now see the association
t.test(Number.of.of.repeat.investors ~ Dependent.Company.Status, data = cnt_data) # p-value = 0.003661
wilcox.test(Number.of.of.repeat.investors ~ Dependent.Company.Status, data = cnt_data) # p-value = 0.002029
cnt_data$auxiliary_mis <- sapply(cnt_data$Number.of.of.repeat.investors, function(x){
  if(is.na(x)){
    cnt_data$auxiliary_mis = 1
  }else{
    cnt_data$auxiliary_mis = 0
  }
})
t.test(cnt_response ~ auxiliary_mis, data = cnt_data) # Importante
n_var <- data.frame("Variable" = "Number.of.of.repeat.investors", 
                    "t_test_p-value" = 0.003661, 
                    "wilcox_p-value" = 0.002029,
                    "significant_missing" = 1,
                    "percent_miss" = 8.5)

cnt_results <- rbind(cnt_results, n_var)

# 14 "Years.of.education" percent_mis = 21.8
ad.test(cnt_data$Years.of.education) # p-value < 2.2e-16
shapiro.test(cnt_data$Years.of.education) # p-value < 2.2e-16
qqnorm(cnt_data$Years.of.education)
qqline(cnt_data$Years.of.education)
ggplot(cnt_data, aes(x = Dependent.Company.Status, y = Years.of.education)) + geom_boxplot()
ggplot(cnt_data, aes(x = Years.of.education)) + geom_density()
# Now see the association
t_object <- t.test(Years.of.education ~ Dependent.Company.Status, data = cnt_data) 
w_object <- wilcox.test(Years.of.education ~ Dependent.Company.Status, data = cnt_data) 
cnt_data$auxiliary_mis <- sapply(cnt_data$Years.of.education, function(x){
  if(is.na(x)){
    cnt_data$auxiliary_mis = 1
  }else{
    cnt_data$auxiliary_mis = 0
  }
})
t_missing <- t.test(cnt_response ~ auxiliary_mis, data = cnt_data) # Importante
n_var <- data.frame("Variable" = "Years.of.education", 
                    "t_test_p-value" = t_object$p.value, 
                    "wilcox_p-value" = w_object$p.value,
                    "significant_missing" = ifelse(t_missing$p.value > 0.05, 0, t_missing$p.value),
                    "percent_miss" = percent_mis["Years.of.education", ])

cnt_results <- rbind(cnt_results, n_var)




"Function that makes the first analysis"
first_analysis <- function(df, column){
  df$column <- df[, column]
  print(ad.test(df[, column]))
  print(shapiro.test(df[, column]))
  qqnorm(df[, column])
  qqline(df[, column])
  print(ggplot(df, aes(x = column)) + geom_density())
  print(ggplot(df, aes(x = Dependent.Company.Status, y = column)) + geom_boxplot())
  # Now see the association 
  t_object <- t.test(column ~ Dependent.Company.Status, data = df)
  w_object <- wilcox.test(column ~ Dependent.Company.Status, data = df)
  df$auxiliary_mis <- sapply(df$column, function(x){
    if(is.na(x)){
      df$auxiliary_mis = 1
    }else{
      df$auxiliary_mis = 0
    }
  })
  t_missing <- t.test(cnt_response ~ auxiliary_mis, data = df)
  n_var <- data.frame("Variable" = column,
                      "t_test_p-value" = t_object$p.value,
                      "wilcox_p-value" = w_object$p.value,
                      "significant_missing" = ifelse(t_missing$p.value > 0.05, 0, t_missing$p.value),
                      "percent_miss" = percent_mis[column, ])

  cnt_results <<- rbind(cnt_results, n_var)
}




first_analysis(cnt_data, "Renowned.in.professional.circle")
first_analysis(cnt_data, "Experience.in.Fortune.100.organizations")
first_analysis(cnt_data, "Experience.in.Fortune.500.organizations")
first_analysis(cnt_data, "Experience.in.Fortune.1000.organizations")
first_analysis(cnt_data, "Number.of.Recognitions.for.Founders.and.Co.founders")
first_analysis(cnt_data, "Skills.score")
first_analysis(cnt_data, "google.page.rank.of.company.website")
first_analysis(cnt_data, "Industry.trend.in.investing")
first_analysis(cnt_data, "Number.of.Direct.competitors")
first_analysis(cnt_data, "Employees.per.year.of.company.existence")
first_analysis(cnt_data, "Last.round.of.funding.received..in.milionUSD.")
first_analysis(cnt_data, "Time.to.1st.investment..in.months.")
first_analysis(cnt_data, "Avg.time.to.investment...average.across.all.rounds..measured.from.previous.investment")
first_analysis(cnt_data, "Percent_skill_Entrepreneurship")
first_analysis(cnt_data, "Percent_skill_Operations")
first_analysis(cnt_data, "Percent_skill_Engineering")
first_analysis(cnt_data, "Percent_skill_Marketing")
first_analysis(cnt_data, "Percent_skill_Leadership")
first_analysis(cnt_data, "Percent_skill_Data.Science")
first_analysis(cnt_data, "Percent_skill_Business.Strategy")
first_analysis(cnt_data, "Percent_skill_Product.Management")
first_analysis(cnt_data, "Percent_skill_Sales")
first_analysis(cnt_data, "Percent_skill_Domain")
first_analysis(cnt_data, "Percent_skill_Law")
first_analysis(cnt_data, "Percent_skill_Consulting")
first_analysis(cnt_data, "Percent_skill_Finance")
first_analysis(cnt_data, "Percent_skill_Investment")
first_analysis(cnt_data, "Renown.score")

# Queda pendiente la variable Investors.count



# ||| Now with the character dataframe |||

# To see which variables are suitable for a table 
head(sapply(char_data, function(x){length(unique(x))}), 10)
# Backup data
char_data_respaldo <- char_data

# First change
char_data$Has.the.team.size.grown <- toupper(char_data$Has.the.team.size.grown)

# Convert to lower case
char_data$Number.of..Sales.Support.material <- tolower(char_data$Number.of..Sales.Support.material)

# Convert to lower case
char_data$Cloud.or.platform.based.serive.product. <- tolower(char_data$Cloud.or.platform.based.serive.product.)



# Execute the chi-squared
tab <- table(char_data$Dependent.Company.Status, char_data$Has.the.team.size.grown)
ggplot(char_data, aes(x = Dependent.Company.Status, fill = Has.the.team.size.grown)) + geom_bar(position="dodge")
chi <- chisq.test(tab, simulate.p.value = TRUE, B = 5000)

char_results <- data.frame("Variable" = "Has.the.team.size.grown",
                           "chi_p.value" = chi$p.value,
                           "percent_miss" = percent_mis["Has.the.team.size.grown",])

usar_el_chi <- function(df, column){
  df$column <- df[,column]
  tab <- table(df$Dependent.Company.Status, df$column)
  chi <- chisq.test(tab, simulate.p.value = TRUE, B = 5000)
  #print(ggplot(df, aes(x = Dependent.Company.Status, fill = column)) + geom_bar(position="dodge"))
  n_var <- data.frame("Variable" = column,
                      "chi_p.value" = chi$p.value,
                      "percent_miss" = percent_mis[column, ])
  char_results <<- rbind(char_results, n_var) 
}

usar_el_chi(char_data, "Continent.of.company")

# Hay que investigar los detalles de la prueba de chi-squared como que nivel de alpha viene
# predeterminado, es bueno dejarle la corrección de Yates, investigar de nuevo el método
# Monte Carlo

# Independent: If the probability distribution of one variable is not affected by the presence of another 

# Chi-squared: The null hypothesis of independence assumption is to be rejected if the p-value

usar_el_chi(char_data, "Continent.of.company")
usar_el_chi(char_data, "Presence.of.a.top.angel.or.venture.fund.in.previous.round.of.investment")
usar_el_chi(char_data, "Number.of..Sales.Support.material")
usar_el_chi(char_data, "Worked.in.top.companies")
usar_el_chi(char_data, "Average.size.of.companies.worked.for.in.the.past")                                                  
usar_el_chi(char_data, "Have.been.part.of.startups.in.the.past.")
usar_el_chi(char_data, "Have.been.part.of.successful.startups.in.the.past.")
usar_el_chi(char_data, "Was.he.or.she.partner.in.Big.5.consulting.")
usar_el_chi(char_data, "Consulting.experience.")
usar_el_chi(char_data, "Product.or.service.company.")
usar_el_chi(char_data, "Catering.to.product.service.across.verticals")
usar_el_chi(char_data, "Focus.on.private.or.public.data.")
usar_el_chi(char_data, "Focus.on.consumer.data.")
usar_el_chi(char_data, "Focus.on.structured.or.unstructured.data")
usar_el_chi(char_data, "Subscription.based.business")
usar_el_chi(char_data, "Cloud.or.platform.based.serive.product.")
usar_el_chi(char_data, "Local.or.global.player")
usar_el_chi(char_data, "Linear.or.Non.linear.business.model")
usar_el_chi(char_data, "Capital.intensive.business.e.g..e.commerce..Engineering.products.and.operations.can.also.cause.a.business.to.be.capital.intensive")
usar_el_chi(char_data, "Number.of..of.Partners.of.company")
usar_el_chi(char_data, "Crowdfunding.based.business")
usar_el_chi(char_data, "Crowdsourcing.based.business")
usar_el_chi(char_data, "Machine.Learning.based.business")
usar_el_chi(char_data, "Predictive.Analytics.business")
usar_el_chi(char_data, "Speech.analytics.business")
usar_el_chi(char_data, "Prescriptive.analytics.business")
usar_el_chi(char_data, "Big.Data.Business")
usar_el_chi(char_data, "Cross.Channel.Analytics..marketing.channels")
usar_el_chi(char_data, "Owns.data.or.not...monetization.of.data..e.g..Factual")
usar_el_chi(char_data, "Is.the.company.an.aggregator.market.place..e.g..Bluekai")
usar_el_chi(char_data, "Online.or.offline.venture...physical.location.based.business.or.online.venture.")
usar_el_chi(char_data, "B2C.or.B2B.venture.")
usar_el_chi(char_data, "Top.forums.like..Tech.crunch..or..Venture.beat..talking.about.the.company.model...How.much.is.it.being.talked.about.")
usar_el_chi(char_data, "Average.Years.of.experience.for.founder.and.co.founder")
usar_el_chi(char_data, "Exposure.across.the.globe")
usar_el_chi(char_data, "Breadth.of.experience.across.verticals")
usar_el_chi(char_data, "Highest.education")
usar_el_chi(char_data, "Relevance.of.education.to.venture")
usar_el_chi(char_data, "Relevance.of.experience.to.venture")
usar_el_chi(char_data, "Degree.from.a.Tier.1.or.Tier.2.university.")
usar_el_chi(char_data, "Experience.in.selling.and.building.products")
usar_el_chi(char_data, "Top.management.similarity")
usar_el_chi(char_data, "Number.of..of.Research.publications")
usar_el_chi(char_data, "Team.Composition.score")
usar_el_chi(char_data, "Dificulty.of.Obtaining.Work.force")
usar_el_chi(char_data, "Pricing.Strategy")
usar_el_chi(char_data, "Hyper.localisation")
usar_el_chi(char_data, "Time.to.market.service.or.product")
usar_el_chi(char_data, "Employee.benefits.and.salary.structures")
usar_el_chi(char_data, "Long.term.relationship.with.other.founders")
usar_el_chi(char_data, "Proprietary.or.patent.position..competitive.position.")
usar_el_chi(char_data, "Barriers.of.entry.for.the.competitors")
usar_el_chi(char_data, "Company.awards")
usar_el_chi(char_data, "Controversial.history.of.founder.or.co.founder")
usar_el_chi(char_data, "Legal.risk.and.intellectual.property")
usar_el_chi(char_data, "Client.Reputation")
usar_el_chi(char_data, "Technical.proficiencies.to.analyse.and.interpret.unstructured.data")
usar_el_chi(char_data, "Solutions.offered")
usar_el_chi(char_data, "Invested.through.global.incubation.competitions.")
usar_el_chi(char_data, "Disruptiveness.of.technology")
usar_el_chi(char_data, "Survival.through.recession..based.on.existence.of.the.company.through.recession.times")
usar_el_chi(char_data, "Gartner.hype.cycle.stage")
usar_el_chi(char_data, "Time.to.maturity.of.technology..in.years.")


unique_count

min(cnt_results$percent_miss)
arrange(cnt_results, percent_miss)

char_results %>%
  arrange(chi_p.value)


md.pattern(startup2)  # De preferencia con datasets pequeños pero muestra la cantidad de valares
                      # que tienen valores faltantes si es que tienen un 0

# This plot shows the missig values and the different combinations with the missing values 
# and their respective percentage
aggr_plot <- aggr(startup2, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), 
                        cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# Before applying mice check if you are including categorical variables with more than 30 levels
# So we have to create a dataframe that will contain continuous and categorical variables but
# with no more than 20 unique values for the categorical

vector_unico <- sapply(char_data, function(x){length(unique(x))})
# First we add the categorical variables
startup2 <- char_data[, -c(1,3,4,5,6,8,46)]
# Second we add the continous variables 
startup2 <- cbind(startup2, cnt_data[ , -c(47,46,45,44,43)])

tempData <- mice(startup2[, -1], m=10, maxit = 10, seed = 500)

sapply(startup2, function(x){length(unique(x))})

# Buscar bien como es que se aplica la funcion mice en el conjunto de datos startup2
# Concentrarse solo en mice, missForest.


# solo como prueba con la parte categorica de startup2 2:65
tempData <- mice(startup2[, 2:65], m=10, maxit = 15, seed = 500)

#||||| Primeras pruebas con mice |||||||
# Primero es hacer un backup the startup2
original_startup2 <- startup2

# Convertir los caracteres en factores 
col<- c(1:65)
for(i in col){
  startup2[,i] <- as.factor(startup2[,i])
}
str(startup2[, 1:65])

tempData <- mice(startup2[,-1], method = "rf")

# Un ejemplo con BostonHousing para saber como funciona en realidad mice
original <- BostonHousing
BostonHousing[sample(1:nrow(BostonHousing), 40), "rad"] <- NA
BostonHousing[sample(1:nrow(BostonHousing), 40), "ptratio"] <- NA
miceMod <- mice(BostonHousing[, !names(BostonHousing) %in% "medv"], method = "rf")
miceOutput <- complete(miceMod)

# Now we have a complete dataset names completetempData
sapply(completetempData, function(x){sum(is.na(x))})
sapply(startup2, function(x){sum(is.na(x))})

# The first complete dataset:
completetempData <- complete(tempData)
# The second complete Data can be obtained with:
complete(tempData, 2)

# Casi el último paso es saber como es que se pueden ver las distribuciones de las diferentes 
# variables imputadas con respecto a las originales
# para poder ver las variables imputadas es necesario este comando:  tempData$imp$variable


# Entonces el proceso en realidad es el de hacer el análisis estadístico primero y después 
# seleccionar las mejores variables para despus hacer la imputacion de los valores faltantes

# Es hora de saber cuales son las variables mas significativas
names(cnt_results)
cnt_results %>%
  arrange(desc(t_test_p.value < 0.05)) %>%
  head(25)

cnt_seleccion <- cnt_results %>%
  arrange(t_test_p.value) %>%
  head(20)
# Solo hasta employee.count

names(char_results)
char_seleccion <- char_results %>%
  arrange(chi_p.value) %>%
  head(25)

startup3 <- cnt_data[, c(cnt_seleccion$Variable)]
inds <- which(names(char_data) %in% char_seleccion$Variable)
startup3 <- cbind(startup3, char_data[, inds])





# Se hará la imputacion con el primer set de datos: startup2
imp <- mice(startup2, method = "rf")
# Primero: obtener el dataset original y las 5 imputaciones
com <- complete(imp, "long", include = T)
# Segundo: Graficar con lattice
col <- rep(c("blue", "red")[1+as.numeric(is.na(imp$data$Internet.Activity.Score))],6)
stripplot(Age.of.company.in.years~.imp, data = com, jit = TRUE, fac=0.8, col=col, 
          pch=20, cex=1.4, xlab="Imputation Number")

# Now we see the imputations with the startup2 dataset
long <- complete(imp, "long")
levels(long$.imp) <- paste("Imputation", 1:5)
long <- cbind(long, Number.of..Sales.Support.material.na=is.na(imp$data$Number.of..Sales.Support.material))
densityplot(~Number.of..Sales.Support.material|.imp, data=long, group=Number.of..Sales.Support.material.na,
            plot.points=FALSE, ref=TRUE, xlab="Variable1", scales=list(y=list(draw=F)),
            par.settings=simpleTheme(col.line=rep(c("blue", "red"))),
            auto.key = list(columns=2, text=c("Observed", "Imputed")))



# Ya tenemos el dataset que tiene a las variables mas importantes ahora hay que hacer 
# la imputacion con startup3
# Pero primero hay que convertir las variables char a factor
col<- c(21:46)
for(i in col){
  startup3[,i] <- as.factor(startup3[,i])
}
imp2 <- mice(startup3, method = "rf")
com1 <- complete(imp2, "long", include = T)
col2 <- rep(c("blue", "red")[1+as.numeric(is.na(imp2$data$Age.of.company.in.years))],6)
stripplot(Age.of.company.in.years~.imp, data = com1, jit = TRUE, fac=0.8, col=col2, 
          pch=20, cex=1.4, xlab="Imputation Number")


# See plot densities of both observed and imputed data
# We are going to do the visualization 
long2 <- complete(imp2, "long")
levels(long2$.imp) <- paste("Imputation", 1:5)
long2 <- cbind(long2, Number.of..Sales.Support.material.na=is.na(imp2$data$Number.of..Sales.Support.material))
densityplot(~Number.of..Sales.Support.material|.imp, data=long2, group=Number.of..Sales.Support.material.na, 
            plot.points=FALSE, ref=TRUE, xlab="Variable", scales=list(y=list(draw=F)),
            par.settings=simpleTheme(col.line = rep(c("blue", "red"))),
            auto.key = list(columns=2, text=c("Observed", "Imputed")))


  

