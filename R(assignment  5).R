

# Install packages 
install.packages("ggplot2")

# Load library
library(ggplot2)


# SAMPLE DATASET

sales_data <- data.frame(
  OrderID = c(1,2,3,4,5,6,7,8,9,10),
  CustomerName = c(
    "Amit","Priya","Rahul","Sneha","Karan",
    "Pooja","Rohan","Neha","Vikas","Anjali"
  ),
   City = c(
    "Pune","Mumbai","Delhi","Pune","Nagpur",
    "Mumbai","Pune","Delhi","Nashik","Pune"
  ),
  Category = c(
    "Electronics","Clothing","Electronics","Furniture",
    "Electronics","Clothing","Furniture","Electronics",
    "Clothing","Electronics"
  ),
  Product = c(
    "Laptop","Shirt","Mobile","Table","Headphones",
    "Jeans","Chair","Tablet","Tshirt","Camera"
  ),
  Quantity = c(2,5,3,1,4,6,2,5,7,3),
  Price = c(
    50000,1200,25000,7000,3000,
    2000,2500,15000,800,35000
  ),
   Discount = c(
    5000,100,2000,500,300,
    200,150,1000,50,2500
  )
)

# Create Revenue column
sales_data$Revenue <- (sales_data$Price * sales_data$Quantity) - sales_data$Discount

# Save CSV file
write.csv(sales_data, "sales_data.csv", row.names = FALSE)

# Import CSV file
sales_data <- read.csv("sales_data.csv")


# Q1) BASIC GGPLOT CHARTS


# a) Scatter plot of Price vs Revenue
ggplot(sales_data, aes(x = Price, y = Revenue)) +
  geom_point()

# b) Bar chart for City
ggplot(sales_data, aes(x = City)) +
  geom_bar()

# c) Histogram of Price
ggplot(sales_data, aes(x = Price)) +
  geom_histogram(bins = 5)

# d) Boxplot of Revenue by Category
ggplot(sales_data, aes(x = Category, y = Revenue)) +
  geom_boxplot()

# e) Line chart for Revenue by OrderID
ggplot(sales_data, aes(x = OrderID, y = Revenue)) +
  geom_line()


# Q2) CUSTOMIZED VISUALIZATION


# a) Add title to chart
ggplot(sales_data, aes(x = City)) +
  geom_bar() +
  ggtitle("Orders by City")

# b) Change axis labels
ggplot(sales_data, aes(x = Price, y = Revenue)) +
  geom_point() +
  xlab("Product Price") +
  ylab("Total Revenue")

# c) Change color of bars
ggplot(sales_data, aes(x = City)) +
  geom_bar(fill = "blue")

# d) Change theme style
ggplot(sales_data, aes(x = City)) +
  geom_bar(fill = "green") +
  theme_minimal()

# e) Rotate x-axis labels
ggplot(sales_data, aes(x = Product, y = Revenue)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45))

# Q3) MULTI-VARIABLE VISUALIZATION


# a) Price vs Revenue colored by Category
ggplot(sales_data,
       aes(x = Price, y = Revenue, color = Category)) +
  geom_point(size = 3)

# b) Price vs Quantity colored by Category
ggplot(sales_data,
       aes(x = Price, y = Quantity, color = Category)) +
  geom_point(size = 3)

# c) Revenue vs Discount
ggplot(sales_data,
       aes(x = Discount, y = Revenue)) +
  geom_point(color = "red", size = 3)

# d) Top cities by revenue
ggplot(sales_data,
       aes(x = City, y = Revenue, fill = City)) +
  geom_bar(stat = "identity")

# e) Top products by sales
ggplot(sales_data,
       aes(x = Product, y = Revenue, fill = Product)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45))


# Q4) BAR CHART, SCATTER PLOT, HISTOGRAM ANALYSIS


# a) Number of orders per city
ggplot(sales_data, aes(x = City)) +
  geom_bar(fill = "orange")

# b) Price vs Revenue
ggplot(sales_data,
       aes(x = Price, y = Revenue)) +
  geom_point(color = "blue", size = 3)

# c) Quantity vs Revenue
ggplot(sales_data,
       aes(x = Quantity, y = Revenue)) +
  geom_point(color = "darkgreen", size = 3)

# d) Distribution of Quantity
ggplot(sales_data,
       aes(x = Quantity)) +
  geom_histogram(bins = 5, fill = "purple")

# e) Histogram by Category
ggplot(sales_data,
       aes(x = Revenue, fill = Category)) +
  geom_histogram(bins = 5)


# Q5) BOX PLOT, LINE CHART, FACETED CHART ANALYSIS


# a) Revenue by Category
ggplot(sales_data,
       aes(x = Category, y = Revenue)) +
  geom_boxplot(fill = "lightblue")

# b) Price by Product
ggplot(sales_data,
       aes(x = Product, y = Price)) +
  geom_boxplot(fill = "pink") +
  theme(axis.text.x = element_text(angle = 45))

# c) Revenue trend by OrderID
ggplot(sales_data,
       aes(x = OrderID, y = Revenue)) +
  geom_line(color = "blue") +
  geom_point()

# d) Discount trend
ggplot(sales_data,
       aes(x = OrderID, y = Discount)) +
  geom_line(color = "red") +
  geom_point()

# e) City orders by category (Faceted Chart)
ggplot(sales_data,
       aes(x = City, fill = Category)) +
  geom_bar() +
  facet_wrap(~Category)

