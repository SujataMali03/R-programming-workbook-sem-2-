#data cleaning and Transformation

# Create dataset
student_data <- data.frame(
  name = c("Raj", "Amit", "neha", "Raj", "Amit"),
  marks = c("80", "90", NA, "85", "90"),
  city = c("Pune", "pune", "mumbai", "PUNE", "Mumbai"),
  stringsAsFactors = FALSE
)

# 1. Remove extra spaces from name column
student_data$name <- trimws(student_data$name)
print(student_data)

# 2. Convert all names to lowercase
student_data$name <- tolower(student_data$name)
print(student_data)
# 3. Convert marks column to numeric
student_data$marks <- as.numeric(student_data$marks)

# Handle missing values (replace NA with average marks)
student_data$marks[is.na(student_data$marks)] <- mean(student_data$marks, na.rm = TRUE)

# 4. Standardize city names (all lowercase)
student_data$city <- tolower(student_data$city)

# 5. Filter students with marks greater than 80
filtered_students <- subset(student_data, marks > 80)

# 6. Create new column grade
student_data$grade <- ifelse(student_data$marks >= 85, "A", "B")

# Display cleaned dataset
print(student_data)

# Display filtered students
print(filtered_students)







# Practical Question 3: Statistical Analysis & Tables

# 1. Create the dataset
data <- data.frame(
  Gender = c("Male","Female","Male","Female","Male","Female"),
  Purchase = c("Yes","No","Yes","Yes","No","Yes"),
  Amount = c(5000,7000,8000,6000,4000,9000)
)

# Display dataset
print(data)

# 2. Calculate Mean of Amount
mean_amount <- mean(data$Amount)
print(paste("Mean Amount =", mean_amount))

# 3. Calculate Median of Amount
median_amount <- median(data$Amount)
print(paste("Median Amount =", median_amount))

# 4. Create Frequency Table for Gender
gender_freq <- table(data$Gender)
print("Frequency Table for Gender:")
print(gender_freq)

# 5. Cross-tabulation of Gender and Purchase
cross_tab <- table(data$Gender, data$Purchase)
print("Cross Tabulation:")
print(cross_tab)

# 6. Total Purchase Amount by Gender
total_purchase <- aggregate(Amount ~ Gender, data = data, sum)
print("Total Purchase Amount by Gender:")
print(total_purchase)

# 7. Count Number of 'Yes' Purchases
yes_count <- sum(data$Purchase == "Yes")
print(paste("Number of Yes Purchases =", yes_count))


