#Assignment No. 6
# R PRACTICAL : CUSTOMER DATA ANALYSIS

# CREATE SAMPLE DATASET

customer_data <- data.frame(
  CustomerID = c(1,2,3,4,5,6,7,8,9,10),
  Gender = c(
    "Male","Female","Male","Female","Male",
    "Female","Male","Female","Male","Female"
  ),
   Region = c(
    "Pune","Mumbai","Delhi","Pune","Nagpur",
    "Mumbai","Delhi","Pune","Nagpur","Mumbai"
  ),
  Purchase = c(
    "Yes","No","Yes","Yes","No",
    "Yes","No","Yes","No","Yes"
  ),
  Amount = c(
    6000,2500,7000,4500,3000,
    8000,2000,5500,3500,9000
  )
)


# SAVE CSV FILE

write.csv(customer_data,
          "customer_data.csv",
          row.names = FALSE)


# IMPORT CSV FILE
data <- read.csv("customer_data.csv")

# Q1) SALES PERFORMANCE ANALYSIS

# a) Total sales by region
aggregate(Amount ~ Region,
          data = data,
          sum)

# b) Average sales per gender
aggregate(Amount ~ Gender,
          data = data,
          mean)

# c) Frequency distribution of purchase
table(data$Purchase)

# d) Cross-tab: Region vs Purchase
cross_tab1 <- table(data$Region,
                    data$Purchase)

cross_tab1

# e) Chi-square test
chisq.test(cross_tab1,
           simulate.p.value = TRUE)


# Q2) CUSTOMER SEGMENTATION

# a) Count customers by region
table(data$Region)

# b) Average purchase by region using tapply
tapply(data$Amount,
       data$Region,
       mean)

# c) Frequency of gender
table(data$Gender)

# d) Cross-tab: Gender vs Region
cross_tab2 <- table(data$Gender,
                    data$Region)

cross_tab2

# e) Chi-square test
chisq.test(cross_tab2,
           simulate.p.value = TRUE)


# Q3) PURCHASE PATTERN ANALYSIS

# a) Total purchase amount
sum(data$Amount)

# b) Row-wise sum using apply
numeric_data <- data.frame(data$Amount)

apply(numeric_data,
      1,
      sum)

# c) Frequency of purchase
table(data$Purchase)

# d) Cross-tab: Purchase vs Region
cross_tab3 <- table(data$Purchase,
                    data$Region)

cross_tab3

# e) Chi-square test
chisq.test(cross_tab3,
           simulate.p.value = TRUE)


# Q4) HIGH-VALUE CUSTOMER ANALYSIS

# a) Filter customers with Amount > 5000
high_value <- subset(data,
                     Amount > 5000)

high_value

# b) Mean purchase of filtered group
mean(high_value$Amount)

# c) Frequency of region (filtered)
table(high_value$Region)

# d) Cross-tab: Gender vs Purchase (filtered)
cross_tab4 <- table(high_value$Gender,
                    high_value$Purchase)

cross_tab4

# e) Chi-square test
chisq.test(cross_tab4,
           simulate.p.value = TRUE)

# Q5) BUSINESS DECISION ANALYSIS

# a) Total revenue
sum(data$Amount)

# b) Average revenue per region
aggregate(Amount ~ Region,
          data = data,
          mean)

# c) Frequency distribution of regions
table(data$Region)

# d) Cross-tab: Region vs Purchase
cross_tab5 <- table(data$Region,
                    data$Purchase)

cross_tab5

# e) Statistical inference
chisq.test(cross_tab5,
           simulate.p.value = TRUE)

