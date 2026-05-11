

# Install packages (Run only first time)
install.packages("readxl")
install.packages("writexl")

# Load libraries
library(readxl)
library(writexl)


# CREATE SIMPLE DATASET

sales_data <- data.frame(
  OrderID = c(1,2,3,4,5,6,7,8,9,10),
  CustomerName = c("Amit","Priya","Rahul","Sneha","Karan",
                   "Pooja","Rohan","Neha","Vikas","Anjali"),
  
  City = c("Pune","Mumbai","Delhi","Pune","Nagpur",
           "Mumbai","Pune","Delhi","Nashik","Pune"),
  
  Category = c("Electronics","Clothing","Electronics","Furniture",
               "Electronics","Clothing","Furniture","Electronics",
               "Clothing","Electronics"),
  
  Product = c("Laptop","Shirt","Mobile","Table","Headphones",
              "Jeans","Chair","Tablet","Tshirt","Camera"),
  
  Quantity = c(2,5,3,1,4,6,2,5,7,3),
  
  Price = c(50000,1200,25000,7000,3000,
            2000,2500,15000,800,35000),
  
  Discount = c(5000,100,2000,500,300,
               200,150,1000,50,2500)
)

# Create Revenue column
sales_data$Revenue <- (sales_data$Price * sales_data$Quantity) - sales_data$Discount

# Create DiscountAmount column
sales_data$DiscountAmount <- sales_data$Discount

# SAVE DATASET AS CSV AND EXCEL


write.csv(sales_data, "sales_data.csv", row.names = FALSE)

write_xlsx(sales_data, "sales_data.xlsx")


sales_data <- read.csv("sales_data.csv")

sales_data_excel <- read_excel("sales_data.xlsx")


# Q1) IMPORTING AND EXPLORING DATA

# a) Import dataset from CSV and Excel
sales_data
sales_data_excel

# b) Display first 10 rows
head(sales_data, 10)

# c) Display last 10 rows
tail(sales_data, 10)

# d) Check structure
str(sales_data)

# e) Summary statistics
summary(sales_data)


# Q2) EXPORTING DATA

# a) Export dataset to CSV
write.csv(sales_data, "Export_Sales.csv", row.names = FALSE)

# b) Export dataset to Excel
write_xlsx(sales_data, "Export_Sales.xlsx")

# c) Export only Electronics category
electronics_data <- subset(sales_data, Category == "Electronics")

write.csv(electronics_data, "Electronics_Data.csv", row.names = FALSE)

# d) Export customers from Pune
pune_data <- subset(sales_data, City == "Pune")

write.csv(pune_data, "Pune_Customers.csv", row.names = FALSE)

# e) Export top 20 rows
top20 <- head(sales_data, 20)

write.csv(top20, "Top20.csv", row.names = FALSE)


# Q3) MODIFYING DATA AND SUBSET CREATION

# 1. Create Profit column
sales_data$Profit <- sales_data$Revenue * 0.20

# 2. Rename Revenue column to FinalAmount
names(sales_data)[names(sales_data) == "Revenue"] <- "FinalAmount"

# 3. Remove DiscountAmount column
sales_data$DiscountAmount <- NULL

# 4. Add Tax column
sales_data$Tax <- sales_data$FinalAmount * 0.18

# 5. Select orders with Quantity > 5
high_quantity <- subset(sales_data, Quantity > 5)

high_quantity


# Q4) COLUMN SELECTION


# 1. Select CustomerName and Product columns
sales_data[, c("CustomerName","Product")]

# 2. Select first 5 columns
sales_data[, 1:5]

# 3. Select numeric columns
sales_data[sapply(sales_data, is.numeric)]

# 4. Remove Discount column
sales_without_discount <- sales_data[, !(names(sales_data) %in% "Discount")]

sales_without_discount

# 5. Select specific columns using index
sales_data[, c(2,4,6)]


# Q5) SORTING DATA


# 1. Sort by City alphabetically
sales_data[order(sales_data$City), ]

# 2. Sort by City and FinalAmount
sales_data[order(sales_data$City, sales_data$FinalAmount), ]

# 3. Average price by city
aggregate(Price ~ City, data = sales_data, mean)

# 4. Maximum order value by city
aggregate(FinalAmount ~ City, data = sales_data, max)

# 5. Total quantity sold by city
aggregate(Quantity ~ City, data = sales_data, sum)
