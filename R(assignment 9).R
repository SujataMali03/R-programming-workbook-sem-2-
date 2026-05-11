#Assignment No. 9
# R PRACTICAL : OBJECT ORIENTED PROGRAMMING IN R

# Install packages (Run only first time)
install.packages("readxl")
install.packages("writexl")

# Load libraries
library(readxl)
library(writexl)


# CREATE SAMPLE DATASET

oop_customer_data <- data.frame(
  CustomerID = 1:5,
  Name = c(
    "Amit","Priya","Rahul","Sneha","Karan"
  ),
  Age = c(
    22,30,28,35,40
  ),
  Income = c(
    30000,50000,45000,60000,70000
  ),
  Membership = c(
    "Silver","Gold","Silver","Gold","Platinum"
  ),
  SpendingScore = c(
    60,80,70,90,95
  )
)

# Save Excel file
write_xlsx(oop_customer_data,
           "oop_customer_data.xlsx")

# Import Excel file
data <- read_excel("oop_customer_data.xlsx")


# Q1) CUSTOMER OBJECT MODELING (S3 SYSTEM)

# 1. Create S3 class
customer1 <- list(
  Name = data$Name[1],
  Age = data$Age[1],
  Income = data$Income[1],
  Membership = data$Membership[1]
)

class(customer1) <- "Customer"

# 2. Display attributes
attributes(customer1)

# 3. Custom print method
print.Customer <- function(x)
{
  cat("Customer Details\n")
  cat("Name:", x$Name, "\n")
  cat("Age:", x$Age, "\n")
  cat("Income:", x$Income, "\n")
  cat("Membership:", x$Membership, "\n")
}

# Print object
print(customer1)

# 4. Extract details
customer1$Name
customer1$Income

# 5. Modify attribute
customer1$Income <- 55000

print(customer1)


# Q2) ADVANCED CUSTOMER CLASS (S4 SYSTEM)

# 1. Define S4 class
setClass(
  "Customer",
  slots = c(
    Age = "numeric",
    Income = "numeric",
    Membership = "character"
  )
)

# 2. Create object
cust_obj <- new(
  "Customer",
  Age = data$Age[1],
  Income = data$Income[1],
  Membership = data$Membership[1]
)

# 3. Show method
setMethod(
  "show",
  "Customer",
  function(object)
  {
    cat("S4 Customer Object\n")
    cat("Age:", object@Age, "\n")
    cat("Income:", object@Income, "\n")
    cat("Membership:", object@Membership, "\n")
  }
)

# Display object
cust_obj


# Q3) OBJECT REFERENCING AND COPY BEHAVIOR

# 1. Assign dataset to new variable
data_copy <- data

# 2. Modify copied dataset
data_copy$Income[1] <- 99999

# 3. Check original dataset
data

# 4. Compare objects
identical(data, data_copy)


# Q4) MEMORY OPTIMIZATION STUDY

# 1. Create large object
large_data <- rbind(data, data, data, data, data)

# 2. Check memory usage
object.size(large_data)

# 3. Remove unnecessary object
rm(large_data)

# 4. Garbage collection
gc()


# Q5) LIST-BASED CUSTOMER OBJECT SYSTEM


# 1. Convert dataset into list
customer_list <- split(data, seq(nrow(data)))

# 2. Assign S3 class
for(i in 1:length(customer_list))
{
  class(customer_list[[i]]) <- "Customer"
}

# 3. Access nested values
customer_list[[1]]$Name

# 4. Modify customer data
customer_list[[1]]$Income <- 88888

# 5. Print structure
str(customer_list)


# Q6) CLASS INSPECTION AND DOCUMENTATION

# 1. Check class
class(data)

# 2. Inspect structure
str(data)

# 3. Extract attributes
attributes(data)

# 4. Summary overview
summary(data)


# Q7) S4 SLOT MANIPULATION

# 1. Create S4 class
setClass(
  "SpendingProfile",
  slots = c(
    SpendingScore = "numeric",
    Income = "numeric"
  )
)

# 2. Create object
profile1 <- new(
  "SpendingProfile",
  SpendingScore = data$SpendingScore[1],
  Income = data$Income[1]
)

# 3. Access slots
profile1@SpendingScore
profile1@Income

# 4. Modify slot values
profile1@Income <- 75000

# 5. Display object
profile1


# Q8) ADVANCED DATA STRUCTURE INTEGRATION

# 1. Create nested list
nested_object <- list(
  Dataset = data,
  Summary = summary(data)
)

# 2. Access inner elements
nested_object$Dataset

nested_object$Summary

# 3. Modify nested structure
nested_object$Dataset$Income[1] <- 65000

# 4. Apply class
class(nested_object) <- "CompanyData"

# 5. Display structure
str(nested_object)


# Q9) OBJECT-ORIENTED DATA ANALYSIS

# 1. Convert dataset into S3 object
analysis_object <- data

class(analysis_object) <- "CustomerAnalysis"

# 2. Function for average income
average_income <- function(obj)
{
  mean(obj$Income)
}

# 3. Apply function
average_income(analysis_object)

# 4. Extend object
attr(analysis_object,
     "Company") <- "Retail Pvt Ltd"

# 5. Validate object
class(analysis_object)

attributes(analysis_object)


# Q10) ENTERPRISE-LEVEL OBJECT SYSTEM DESIGN

# 1. S3 version
s3_object <- data

class(s3_object) <- "CustomerS3"

# S4 version
setClass(
  "CustomerS4",
  slots = c(
    dataset = "data.frame"
  )
)

s4_object <- new(
  "CustomerS4",
  dataset = as.data.frame(data)
)

# 2. Compare flexibility vs strictness
class(s3_object)

class(s4_object)

# 3. Summary statistics method
summary(s3_object)

summary(s4_object@dataset)

# 4. Documentation
cat("S3 is flexible and simple\n")

cat("S4 is strict and more formal\n")

