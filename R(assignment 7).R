#Assignment 7
# R PRACTICAL : STATISTICAL ANALYSIS


# Install packages 
install.packages("readxl")

# Load library
library(readxl)

# CREATE SAMPLE DATASET

analysis_data <- data.frame(
  CustomerID = 1:10,
  Age = c(22,25,30,35,40,28,32,45,50,38),
  Income = c(
    30000,45000,50000,55000,70000,
    48000,62000,75000,80000,65000
  ),
  SpendingScore = c(
    40,55,60,65,80,
    58,72,85,90,75
  ),
  Region = c(
    "Pune","Mumbai","Delhi","Pune","Nagpur",
    "Mumbai","Delhi","Pune","Nagpur","Mumbai"
  ),
  Purchase = c(
    "Yes","No","Yes","Yes","No",
    "Yes","Yes","No","Yes","Yes"
  )
)


write.csv(analysis_data,
          "analysis_data.csv",
          row.names = FALSE)


# IMPORT DATA
data <- read.csv("analysis_data.csv")


# Q1) RETAIL INCOME VS SPENDING STRATEGY

# 1. Pearson correlation
cor(data$Income,
    data$SpendingScore,
    method = "pearson")

# 2. Significance test
cor.test(data$Income,
         data$SpendingScore,
         method = "pearson")

# 3. Generate normal distribution
x <- seq(min(data$Income),
         max(data$Income),
         length = 100)

y <- dnorm(x,
           mean(data$Income),
           sd(data$Income))

plot(x, y, type = "l",
     main = "Normal Distribution of Income")

# 4. One-sample t-test
t.test(data$Income,
       mu = 50000)

# 5. Binomial test
purchase_binary <- ifelse(data$Purchase == "Yes", 1, 0)

binom.test(sum(purchase_binary),
           length(purchase_binary),
           p = 0.5)


# Q2) CUSTOMER SEGMENTATION STRATEGY

# 1. Correlation matrix
numeric_data <- data[, c("Age",
                         "Income",
                         "SpendingScore")]

cor(numeric_data)

# 2. Strongest relationship
cor(numeric_data)

# 3. Normal distribution for SpendingScore
x <- seq(min(data$SpendingScore),
         max(data$SpendingScore),
         length = 100)

y <- dnorm(x,
           mean(data$SpendingScore),
           sd(data$SpendingScore))

plot(x, y, type = "l",
     main = "Normal Distribution of SpendingScore")

# 4. T-test for SpendingScore = 60
t.test(data$SpendingScore,
       mu = 60)

# 5. Kruskal-Wallis test
kruskal.test(SpendingScore ~ Region,
             data = data)


# Q3) FINANCIAL RISK ASSESSMENT

# 1. Correlation between Income and SpendingScore
cor(data$Income,
    data$SpendingScore)

# 2. Statistical significance
cor.test(data$Income,
         data$SpendingScore)

# 3. Poisson distribution
x <- 0:10

y <- dpois(x, lambda = 5)

plot(x, y,
     type = "h",
     main = "Poisson Distribution")

# 4. T-test on Income = 40000
t.test(data$Income,
       mu = 40000)

# 5. Binomial test for 60% safe customers
binom.test(sum(purchase_binary),
           length(purchase_binary),
           p = 0.6)


# Q4) PRODUCT DEMAND FORECASTING

# 1. Spearman correlation
cor(data$Age,
    data$Income,
    method = "spearman")

# 2. Monotonic relationship
cor.test(data$Age,
         data$Income,
         method = "spearman")

# 3. Binomial distribution
x <- 0:10

y <- dbinom(x,
            size = 10,
            prob = 0.5)

plot(x, y,
     type = "h",
     main = "Binomial Distribution")

# 4. T-test on Age = 35
t.test(data$Age,
       mu = 35)

# 5. Chi-square goodness-of-fit
region_table <- table(data$Region)

chisq.test(region_table)


# Q5) CUSTOMER SATISFACTION MODELING

# 1. Pearson correlation
cor(data$Income,
    data$SpendingScore,
    method = "pearson")

# 2. Spearman correlation
cor(data$Income,
    data$SpendingScore,
    method = "spearman")

# 3. Normal distribution for SpendingScore
x <- seq(min(data$SpendingScore),
         max(data$SpendingScore),
         length = 100)

y <- dnorm(x,
           mean(data$SpendingScore),
           sd(data$SpendingScore))

plot(x, y,
     type = "l",
     main = "Normal Distribution")

# 4. T-test for mean satisfaction = 55
t.test(data$SpendingScore,
       mu = 55)

# 5. Wilcoxon test
wilcox.test(data$Age,
            data$SpendingScore)


# Q6) SALES PROBABILITY ANALYSIS

# 1. Convert Purchase into binary and correlate with Income
purchase_binary <- ifelse(data$Purchase == "Yes", 1, 0)

cor(data$Income,
    purchase_binary)

# 2. Binomial distribution
x <- 0:10

y <- dbinom(x,
            size = 10,
            prob = 0.5)

plot(x, y,
     type = "h",
     main = "Purchase Binomial Distribution")

# 3. Binomial test
binom.test(sum(purchase_binary),
           length(purchase_binary),
           p = 0.5)

# 4. T-test on Income = 55000
t.test(data$Income,
       mu = 55000)

# 5. Chi-square test for purchase distribution
purchase_table <- table(data$Purchase)

chisq.test(purchase_table)


# Q7) STRATEGIC BUSINESS DECISION MODEL

# 1. Correlation matrix among numeric variables
all_numeric <- data[, c("Age",
                        "Income",
                        "SpendingScore")]

cor(all_numeric)

# 2. Strongest predictor of SpendingScore
cor(all_numeric)

# 3. Poisson distribution for simulated demand
x <- 0:10

y <- dpois(x,
           lambda = 4)

plot(x, y,
     type = "h",
     main = "Simulated Demand")

# 4. T-test on Income benchmark 60000
t.test(data$Income,
       mu = 60000)

# 5. Kruskal-Wallis test for Age across regions
kruskal.test(Age ~ Region,
             data = data)
