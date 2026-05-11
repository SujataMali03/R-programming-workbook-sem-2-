
# ASSIGNMENT NO. 10 
# COMPLETE DATA ANALYSIS USING R


# Install packages
install.packages("ggplot2")
install.packages("readxl")
install.packages("writexl")

# Load libraries
library(ggplot2)
library(readxl)
library(writexl)


# STEP 1 : CREATE DATASET

sales_data <- data.frame(
  CustomerID = 1:10,
  CustomerName = c(
    "Amit","Priya","Rahul","Sneha","Karan",
    "Pooja","Rohan","Neha","Vikas","Anjali"
  ),
  Age = c(
    22,30,28,35,40,
    26,32,45,29,38
  ),
  City = c(
    "Pune","Mumbai","Delhi","Pune","Nagpur",
    "Mumbai","Delhi","Pune","Nagpur","Mumbai"
  ),
  Product = c(
    "Laptop","Mobile","Tablet","Camera","Headphones",
    "Laptop","Mobile","Tablet","Camera","Headphones"
  ),
  Quantity = c(
    2,5,3,1,4,
    2,6,3,2,5
  ),
  Price = c(
    50000,25000,15000,35000,3000,
    52000,24000,16000,36000,3500
  )
)

# Create Revenue column
sales_data$Revenue <- sales_data$Quantity * sales_data$Price


# STEP 2 : SAVE & IMPORT DATASET

write.csv(
  sales_data,
  "sales_data.csv",
  row.names = FALSE
)

data <- read.csv("sales_data.csv")

# Display first 10 rows
head(data, 10)


# STEP 3 : DATA CLEANING & PREPROCESSING

# Check structure
str(data)

# Check missing values
is.na(data)

# Summary statistics
summary(data)

# STEP 4 : EXPLORATORY DATA ANALYSIS

# Average revenue
mean(data$Revenue)

# Maximum revenue
max(data$Revenue)

# Minimum revenue
min(data$Revenue)

# Total revenue
sum(data$Revenue)

# Count customers by city
table(data$City)


# STEP 5 : CORRELATION ANALYSIS

# Pearson correlation
cor(
  data$Price,
  data$Revenue
)

# Correlation test
cor.test(
  data$Price,
  data$Revenue
)

# STEP 6 : HYPOTHESIS TESTING

# One sample t-test
t.test(
  data$Revenue,
  mu = 50000
)


# STEP 7 : DATA VISUALIZATION

# Scatter Plot
ggplot(
  data,
  aes(
    x = Price,
    y = Revenue
  )
) +
  geom_point(
    color = "blue",
    size = 3
  ) +
  ggtitle("Price vs Revenue")

# Bar Chart
ggplot(
  data,
  aes(
    x = City
  )
) +
  geom_bar(
    fill = "green"
  ) +
  ggtitle("Customers by City")

# Histogram
ggplot(
  data,
  aes(
    x = Revenue
  )
) +
  geom_histogram(
    bins = 5,
    fill = "orange"
  ) +
  ggtitle("Revenue Distribution")

# Box Plot
ggplot(
  data,
  aes(
    x = Product,
    y = Revenue
  )
) +
  geom_boxplot(
    fill = "pink"
  ) +
  ggtitle("Revenue by Product")

# Line Chart
ggplot(
  data,
  aes(
    x = CustomerID,
    y = Revenue
  )
) +
  geom_line(
    color = "red"
  ) +
  geom_point() +
  ggtitle("Revenue Trend")


# STEP 8 : FINAL CONCLUSION

cat("Final Conclusion:\n")

cat("The analysis shows that revenue increases with product price.\n")

cat("Mumbai and Pune generated higher sales.\n")

cat("Correlation analysis indicates positive relationship between Price and Revenue.\n")

cat("Visualizations helped identify sales trends and customer distribution.\n")

