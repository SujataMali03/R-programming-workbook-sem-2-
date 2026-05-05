#Q1) Write an R program that:
#  1. Takes two numeric values from user.
#2. Performs all arithmetic operations (+, −, *, /, power, modulus).
#3. Stores the results in a vector.
#4. Displays the maximum, minimum, and average value of the results. 

a<-as.numeric(readline("Enter first number:"))
b<-as.numeric(readline("Enter second number:"))

sum_val<-a+b
diff_val<-a-b
prod_val<-a*b
div_val<-a/b
power_val<-a^b
mod_val<-a%%b

results<-c(sum_val,prod_val,div_val,power_val,mod_val)

print(results)

cat("Maximum:",max(results),"\n")
cat("Minimum:",min(results),"\n")
cat("Average:",mean(results),"\n")



#Q2) Create an R program that:
#  1. Generates 10 random numbers between 1 and 100.
#2. Calculates square root and then rounded value, ceiling, and floor for each square root value.
#3. Stores results in a data frame

n <-as.numeric(readline("Enter how many numbers:"))

#validation check
if(is.na(n)||n<=0){
  stop("Invalid input.Please enter valid positive number.")
}else{nums<-sample(1:100,n)}

# 2. Calculate square root and transformations
sqrt_vals <- sqrt(nums)
rounded <- round(sqrt_vals)
ceil_vals <- ceiling(sqrt_vals)
floor_vals <- floor(sqrt_vals)

# 3. Store in data frame
df <- data.frame(
  Numbers = nums,
  SquareRoot = sqrt_vals,
  Rounded = rounded,
  Ceiling = ceil_vals,
  Floor = floor_vals
)
print(df)



#Q3) Write an R program that:
#  1. Takes a list of names.
#2. Converts all names to uppercase.
#3. Calculates the number of characters in each name.
#4. Combines the name and its length into a formatted sentence.


n <- as.numeric(readline("Enter number of names: "))
names <- character(n)
for(i in 1:n){
  names[i] <- readline(paste("Enter name",i,":"))
}
upper_names <- toupper(names)
lengths <- nchar(upper_names)
result <- paste("Name:", upper_names, "- Length:", lengths)
print(result)



#Q4) Write an R program that:
#  1. Creates a vector of numbers from 1 to 200.
#2. Finds numbers that are divisible by 3 and 5.
#3. Calculates square root and square of those numbers.
#4. Displays results in a table format. 

# Q4: Divisible numbers and calculations

# 1. Create vector
nums <- 1:200
# 2. Find numbers divisible by 3 and 5
filtered <- nums[nums %% 3 == 0 & nums %% 5 == 0]
# 3. Calculate square root and square
sqrt_vals <- sqrt(filtered)
square_vals <- filtered^2
# 4. Store in table (data frame)
result_table <- data.frame(
  Number = filtered,
  SquareRoot = sqrt_vals,
  Square = square_vals
)
# Display
print(result_table)




#Q5) Write an R program to evaluate a quadratic equation for given a, b, c values
#and return real or complex roots.
#Formula:
 # For ax² + bx + c = 0
#Discriminant: D = b² − 4ac
#Roots: x =

# Q5: Quadratic equation roots

# Input values
a <- as.numeric(readline("Enter value of a: "))
b <- as.numeric(readline("Enter value of b: "))
c <- as.numeric(readline("Enter value of c: "))

# Calculate discriminant
D <- b^2 - 4*a*c

# Check nature of roots
if(D > 0) {
  root1 <- (-b + sqrt(D)) / (2*a)
  root2 <- (-b - sqrt(D)) / (2*a)
  cat("Real and distinct roots:\n")
  cat("Root1 =", root1, "\nRoot2 =", root2)
  
} else if(D == 0) {
  root <- -b / (2*a)
  cat("Real and equal roots:\n")
  cat("Root =", root)
  
} else {
  real_part <- -b / (2*a)
  imag_part <- sqrt(-D) / (2*a)
  cat("Complex roots:\n")
  cat("Root1 =", real_part, "+", imag_part, "i\n")
  cat("Root2 =", real_part, "-", imag_part, "i")
}
