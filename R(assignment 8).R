#Assignment 8
# R PRACTICAL : DATA ANALYSIS USING EXCEL DATASETS

# Install packages (Run only first time)
install.packages("readxl")
install.packages("writexl")

# Load libraries
library(readxl)
library(writexl)


# Q1) RETAIL SALES ANALYSIS

sales <- data.frame(
  Product = c(
    "Laptop","Mobile","Tablet","Camera","Headphones",
    "Laptop","Mobile","Tablet","Camera","Headphones"
  ),
   Category = c(
    "Electronics","Electronics","Electronics","Electronics","Accessories",
    "Electronics","Electronics","Electronics","Electronics","Accessories"
  ),
  Quantity = c(10,20,15,8,30,12,25,18,10,40),
  Price = c(
    50000,25000,15000,35000,3000,
    52000,24000,16000,36000,3500
  )
)

# Revenue column
sales$Revenue <- sales$Quantity * sales$Price

# Save Excel file
write_xlsx(list(sales = sales),
           "data.xlsx")

# Load dataset
data <- read_excel("data.xlsx",
                   sheet = "sales")

# a) Display first 10 records
head(data, 10)

# b) Total revenue for each product
aggregate(Revenue ~ Product,
          data = data,
          sum)

# c) Top 5 products with highest revenue
top_products <- aggregate(
  Revenue ~ Product,
  data = data,
  sum
)

top_products[order(-top_products$Revenue), ][1:5, ]

# d) Sales where quantity > 50
subset(data, Quantity > 50)

# e) Average product price by category
aggregate(Price ~ Category,
          data = data,
          mean)


# Q2) E-COMMERCE CUSTOMER ANALYSIS

customers <- data.frame(
  CustomerName = c(
    "Amit","Priya","Rahul","Sneha","Karan",
    "Pooja","Rohan","Neha","Vikas","Anjali"
  ),
  Gender = c(
    "Male","Female","Male","Female","Male",
    "Female","Male","Female","Male","Female"
  ),
  Age = c(22,30,45,26,55,35,40,60,28,24),
  PurchaseAmount = c(
    5000,7000,12000,4500,15000,
    9000,11000,20000,6500,4800
  )
)

# Save Excel sheet
write_xlsx(list(customers = customers),
           "customers.xlsx")

# Load dataset
customers <- read_xlsx("customers.xlsx")

# a) First 10 records
head(customers, 10)

# b) Average purchase amount
mean(customers$PurchaseAmount)

# c) Customers above average purchase
avg_purchase <- mean(customers$PurchaseAmount)

subset(customers,
       PurchaseAmount > avg_purchase)

# d) Count male and female customers
table(customers$Gender)

# e) Create age groups
customers$AgeGroup <- ifelse(
  customers$Age < 25,
  "Youth",
  ifelse(customers$Age <= 50,
         "Adult",
         "Senior")
)

customers

# f) Top 10 highest spending customers
customers[order(-customers$PurchaseAmount), ][1:10, ]


# Q3) BANKING TRANSACTION ANALYSIS

transactions <- data.frame(
  AccountID = 1:10,
  Type = c(
    "Deposit","Withdrawal","Deposit","Deposit",
    "Withdrawal","Deposit","Withdrawal","Deposit",
    "Withdrawal","Deposit"
  ),
  Amount = c(
    5000,3000,15000,20000,7000,
    25000,12000,18000,4000,30000
  )
)

# Save Excel sheet
write_xlsx(list(transactions = transactions),
           "transactions.xlsx")

# Load dataset
transactions <- read_xlsx("transactions.xlsx")

# a) First 10 records
head(transactions, 10)

# b) Total deposits
sum(subset(transactions,
           Type == "Deposit")$Amount)

# c) Total withdrawals
sum(subset(transactions,
           Type == "Withdrawal")$Amount)

# d) Transactions above 10000
subset(transactions,
       Amount > 10000)

# e) Average transaction amount
mean(transactions$Amount)

# f) Count transactions by type
table(transactions$Type)


# Q4) HEALTHCARE PATIENT DATASET

patients <- data.frame(
   PatientName = c(
    "A","B","C","D","E",
    "F","G","H","I","J"
  ),
   Age = c(25,45,65,30,70,55,40,80,22,60),
   BloodPressure = c(
    120,150,160,130,145,
    155,125,170,118,142
  ),
   Temperature = c(
    36.5,37.5,38.0,36.8,37.2,
    39.0,36.7,38.5,36.4,37.8
  )
)

# Save Excel sheet
write_xlsx(list(patients = patients),
           "patients.xlsx")

# Load dataset
patients <- read_xlsx("patients.xlsx")

# a) First 10 records
head(patients, 10)

# b) High blood pressure >140
subset(patients,
       BloodPressure > 140)

# c) Fever >37
subset(patients,
       Temperature > 37)

# d) Average patient age
mean(patients$Age)

# e) Maximum and minimum blood pressure
max(patients$BloodPressure)

min(patients$BloodPressure)

# f) Patients above 60 years
sum(patients$Age > 60)


# Q5) STUDENT PERFORMANCE DATASET

students <- data.frame(
  StudentName = c(
    "Amit","Priya","Rahul","Sneha","Karan",
    "Pooja","Rohan","Neha","Vikas","Anjali"
  ),
  Subject = c(
    "Math","Science","Math","English","Science",
    "Math","English","Science","Math","English"
  ),
  Marks = c(
    85,78,92,35,88,
    76,95,40,60,25
  )
)

# Save Excel sheet
write_xlsx(list(students = students),
           "students.xlsx")

# Load dataset
students <- read_xlsx("students.xlsx")

# a) First 10 records
head(students, 10)

# b) Students scored above 80
subset(students,
       Marks > 80)

# c) Average marks by subject
aggregate(Marks ~ Subject,
          data = students,
          mean)

# d) Top scoring student
students[which.max(students$Marks), ]

# e) Failed students (Marks < 40)
subset(students,
       Marks < 40)

# f) Count students in each subject
table(students$Subject)


# Q6) SOCIAL MEDIA ANALYTICS DATASET

posts <- data.frame(
  PostID = 1:10,
  Likes = c(
    100,250,400,50,600,
    700,80,90,500,300
  ),
  Comments = c(
    20,30,50,10,60,
    80,15,12,40,25
  ),
  Shares = c(
    10,20,40,5,50,
    60,8,7,35,18
  )
)

# Engagement column
posts$Engagement <- posts$Likes +
  posts$Comments +
  posts$Shares

# Save Excel sheet
write_xlsx(list(posts = posts),
           "posts.xlsx")

# Load dataset
posts <- read_xlsx("posts.xlsx")

# a) First 10 records
head(posts, 10)

# b) Total engagement per post
posts[, c("PostID", "Engagement")]

# c) Posts with engagement >500
subset(posts,
       Engagement > 500)

# d) Most liked post
posts[which.max(posts$Likes), ]

# e) Average engagement
mean(posts$Engagement)

# f) Posts with low engagement <100
subset(posts,
       Engagement < 100)
