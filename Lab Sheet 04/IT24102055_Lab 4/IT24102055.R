# 1.
# set the working dir
setwd("C:\\Users\\it24102055\\Desktop\\IT24102055_Lab 4")

# read the text file with header (column names)
branch_data <- read.table("Exercise.txt", header = TRUE, sep=",")

# print the first rows
head(branch_data)

# 2.
#structure of the data
str(branch_data)

# get a summary of the data
summary(branch_data)

#3
boxplot(branch_data$Sales_X1, 
        outline = TRUE,
        outpch=8,
        horizontal=TRUE,
        main = "sales distribution")

# 4.
# Five number summary, min, max, q1, q2 ,q3
summary(branch_data$Advertising)

# Calculate IQR
IQR_advertising <- IQR(branch_data$Advertising)
IQR_advertising

# 5.

find_outliers <- function(years) {
  Q1 <- quantile(years)[2]
  Q3 <- quantile(years)[4]
  iqr <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * iqr
  upper_bound <- Q3 + 1.5 * iqr
  
  outliers <- years[years < lower_bound | years > upper_bound]
  
  outliers = sort(outliers)
  
  print(paste("Upper Bound : ", upper_bound))
  print(paste("Lower Bound : ", lower_bound))
  print(paste("IQR : ", iqr))
  print(paste("outliers", paste(outliers, collapse= ",")))
}


find_outliers(branch_data$Years)

