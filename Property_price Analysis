setwd("C:/Users/hp/Documents/Pproperty Price")
data <- read.csv('C:/Users/hp/Documents/Pproperty Price/Property_price.csv')
# Check data types
str(data)

# Convert columns to factors
data$REAL.ESTATE.ID <- as.factor(data$REAL.ESTATE.ID)
data$YEAR <- as.factor(data$YEAR)
data$MONTH <- as.factor(data$MONTH)
data$SALE.DATE <- as.factor(data$SALE.DATE)
data$DATE <- as.factor(data$DATE)
data$AREA.ID <- as.factor(data$AREA.ID)
data$BUILDING.NUMBER.ON.LAND <- as.factor(data$BUILDING.NUMBER.ON.LAND)
data$TYPE.OF.PROPERTY <- as.factor(data$TYPE.OF.PROPERTY)
data$NO.OF.ROOMS <- as.factor(data$NO.OF.ROOMS)
# Check the structure of the data
str(data)

# Check for duplicate REAL ESTATE IDs
library(dplyr)

# Group the data by REAL.ESTATE.ID and count the number of occurrences
duplicate_counts <- data %>%
  group_by(REAL.ESTATE.ID) %>%
  summarise(count = n())

# Filter for IDs with counts greater than 1 (i.e., duplicates)
duplicates <- duplicate_counts %>%
  filter(count > 1)

# Join the original data to get the duplicate rows
duplicates_data <- data %>%
  inner_join(duplicates, by = "REAL.ESTATE.ID")

print(duplicates_data)


# Load the ggplot2 library for plotting
library(ggplot2)

# Plot average sale price by month
ggplot(data, aes(x = MONTH, y = AMOUNT.PAID)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue") +
  labs(title = "Average Sale Price by Month", x = "Month", y = "Average Sale Price")

# Convert SALE.DATE to Date type
data$SALE.DATE <- as.Date(data$SALE.DATE, format = "%m/%d/%Y")

# Plot average sale price against the complete date of sale
ggplot(data, aes(x = SALE.DATE, y = AMOUNT.PAID)) +
  geom_line(stat = "summary", fun = "mean", color = "blue") +
  labs(title = "Average Sale Price Over Time",
       x = "Sale Date",
       y = "Average Sale Price") +
  theme_minimal()

# Descriptive statistics for SQUARE FOOTAGE column
summary(data[,"SQUARE.FOOTAGE"])

# Descriptive statistics for AMOUNT.PAID column
summary(data$AMOUNT.PAID)

# Histogram for square footage
ggplot(data, aes(x = SQUARE.FOOTAGE)) +
  geom_histogram(bins = 105, fill = "blue", color = "black") +
  labs(title = "Histogram of Square Footage", x = "Square Footage", y = "Frequency")

# Histogram for amount paid
ggplot(data, aes(x = AMOUNT.PAID)) +
  geom_histogram(bins = 105, fill = "red", color = "black") +
  labs(title = "Histogram of Amount Paid", x = "Amount Paid", y = "Frequency")

# Calculate average price by area
avg_price_by_area <- aggregate(AMOUNT.PAID ~ AREA.ID, data = data, mean)
avg_price_by_area <- avg_price_by_area[order(-avg_price_by_area$AMOUNT.PAID),]
print(head(avg_price_by_area))

# Create a bar plot for average price by area
ggplot(avg_price_by_area, aes(x = reorder(AREA.ID, -AMOUNT.PAID), y = AMOUNT.PAID)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Sale Price by Area", x = "Area ID", y = "Average Sale Price") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  # Rotate x-axis labels for better readability

# Calculate average price by property type
avg_price_by_type <- aggregate(AMOUNT.PAID ~ TYPE.OF.PROPERTY, data = data, mean)
avg_price_by_type <- avg_price_by_type[order(-avg_price_by_type$AMOUNT.PAID),]
print(head(avg_price_by_type))

# Create a bar plot for average price by property type
ggplot(avg_price_by_type, aes(x = reorder(TYPE.OF.PROPERTY, -AMOUNT.PAID), y = AMOUNT.PAID)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Sale Price by Property Type", x = "Property Type", y = "Average Sale Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Create a pivot table-like summary for average amount paid by number of rooms
pivot_summary <- data %>%
  group_by(NO.OF.ROOMS) %>%
  summarize(Avg_Amount_Paid = mean(AMOUNT.PAID, na.rm = TRUE),
            Count = n())
# Print the pivot summary
print(pivot_summary)

# Create a bar chart
ggplot(pivot_summary, aes(x = NO.OF.ROOMS, y = Avg_Amount_Paid)) +
  geom_col(fill = "skyblue", width = 0.6) +  # Use geom_col() for bar chart
  labs(
    title = "Average Amount Paid by Number of Rooms",
    x = "Number of Rooms",
    y = "Average Amount Paid"
  ) +
  theme_minimal()


# Linear regression
lm_model <- lm(AMOUNT.PAID ~ SQUARE.FOOTAGE, data = data)

# Summary of the model to check R²
summary(lm_model)
#R_Squre
cat("R^2 Value:", summary(lm_model)$r.squared, "\n")
# MAPE calculation (assuming you have the actual values)
predictions <- predict(lm_model, data)
mape <- mean(abs((data$AMOUNT.PAID - predictions) / data$AMOUNT.PAID)) * 100
print(paste("MAPE:", mape))

library(openxlsx)

# Create a copy of the data
data_copy <- data

# Sort the copied data by amount paid in descending order
data_copy <- data_copy %>%
  arrange(desc(AMOUNT.PAID))

# Delete the top 10 rows
data_copy <- data_copy[-c(1:10), ]

# Create a new Excel workbook
wb <- createWorkbook()

# Add worksheets
addWorksheet(wb, "Original_Data")
addWorksheet(wb, "Modified_Data")

# Write the original data to the "Original_Data" worksheet
writeData(wb, "Original_Data", data, rowNames = FALSE)

# Write the modified data to the "Modified_Data" worksheet
writeData(wb, "Modified_Data", data_copy, rowNames = FALSE)

# Save the Excel file
saveWorkbook(wb, "data_copy.xlsx", overwrite = TRUE)

# Run the regression again on the modified data
modified_model <- lm(AMOUNT.PAID ~ SQUARE.FOOTAGE, data = data_copy)
summary(modified_model)
# Print R^2 value
cat("New R^2 Value:", summary(modified_model)$r.squared, "\n")

# Calculate MAPE (Mean Absolute Percentage Error) for the modified data
predicted_values <- predict(modified_model, data_copy)
mape_modified <- mean(abs((data_copy$AMOUNT.PAID - predicted_values) / data_copy$AMOUNT.PAID)) * 100
cat("New_MAPE:", mape_modified, "%\n")

# Create a new column: Amount Paid Per Square Foot
data <- data %>%
  mutate(Amount_Paid_Per_Square_Foot = AMOUNT.PAID / SQUARE.FOOTAGE)

# Generate descriptive statistics for the new column
summary(data$Amount_Paid_Per_Square_Foot)

# Calculate the IQR for the new column
iqr_value <- IQR(data$Amount_Paid_Per_Square_Foot)

# Calculate the lower and upper bounds for outliers
lower_bound <- quantile(data$Amount_Paid_Per_Square_Foot, 0.25) - 1.5 * iqr_value
upper_bound <- quantile(data$Amount_Paid_Per_Square_Foot, 0.75) + 1.5 * iqr_value

# Identify outliers
outliers <- data$Amount_Paid_Per_Square_Foot[data$Amount_Paid_Per_Square_Foot < lower_bound | data$Amount_Paid_Per_Square_Foot > upper_bound]
cat("Outliers in Amount Paid Per Square Foot:", length(outliers), "\n")


# Plot a histogram for the new column
ggplot(data, aes(x = Amount_Paid_Per_Square_Foot)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Amount Paid Per Square Foot",
       x = "Amount Paid Per Square Foot",
       y = "Frequency")

# Calculate the 95th percentile of amount paid per square foot
percentile_95 <- quantile(data$Amount_Paid_Per_Square_Foot, 0.95)

# Identify rows with the top 5% of amount paid per square foot
top_5_percent_rows <- data %>%
  filter(Amount_Paid_Per_Square_Foot >= percentile_95)

# Calculate the average amount paid per square foot for these rows
average_amount_paid_per_sqft_top_5_percent <- mean(top_5_percent_rows$Amount_Paid_Per_Square_Foot)
average_amount_paid_per_sqft_top_5_percent


# Calculate the 5th percentile of amount paid per square foot
percentile_5 <- quantile(data$Amount_Paid_Per_Square_Foot, 0.05)

# Identify rows with the bottom 5% of amount paid per square foot
bottom_5_percent_rows <- data %>%
  filter(Amount_Paid_Per_Square_Foot <= percentile_5)

# Calculate the average amount paid per square foot for these rows
average_amount_paid_per_sqft_bottom_5_percent <- mean(bottom_5_percent_rows$Amount_Paid_Per_Square_Foot)
average_amount_paid_per_sqft_bottom_5_percent


# Create a frequency table of number of properties sold by property type
property_type_freq_table <- table(data$TYPE.OF.PROPERTY)

# Print the frequency table
print(property_type_freq_table)

# Identify property types with less than 30 properties sold
property_types_to_remove <- names(property_type_freq_table[property_type_freq_table < 30])
property_types_to_remove
# Filter and delete rows for property types with less than 30 properties sold
data <- data %>%
  filter(!(TYPE.OF.PROPERTY %in% property_types_to_remove))

# Create a frequency table of number of properties sold by property type after cleaning
property_type_freq_table <- table(data$TYPE.OF.PROPERTY)

# Print the frequency table
print(property_type_freq_table)

# Verify that rows have been deleted
print(paste("Number of rows after cleaning:", nrow(data)))

# Update room numbers with less than 100 properties sold to 'OTHER'
# Replace the data name from property_data to data
rooms_freq <- table(data$NO.OF.ROOMS)
print(rooms_freq)
low_frequency_rooms <- names(rooms_freq[rooms_freq < 100])
print(low_frequency_rooms)

# Update room numbers with less than 100 properties sold to 'OTHER'
data$NO.OF.ROOMS <- as.character(data$NO.OF.ROOMS)
data$NO.OF.ROOMS[data$NO.OF.ROOMS %in% low_frequency_rooms] <- "OTHER"
data$NO.OF.ROOMS <- factor(data$NO.OF.ROOMS)
rooms_freq <- table(data$NO.OF.ROOMS)
print(rooms_freq)

# Create a new column "Common Area Fixed"
data <- data %>%
  mutate(Common_Area_Fixed = ifelse(is.na(COMMON.AREA),
                                    ave(COMMON.AREA, BUILDING.NUMBER.ON.LAND, AREA.ID, TYPE.OF.PROPERTY, FUN = function(x) mean(x, na.rm = TRUE)),
                                    COMMON.AREA))
# Filter rows to exclude those with #DIV/0! error for the variable "Common_Area_Fixed"
data <- data %>%
  filter(!is.infinite(Common_Area_Fixed) & !is.nan(Common_Area_Fixed))
print(paste("Number of rows after cleaning:", nrow(data)))


library(zoo)

data <- data %>%
  arrange(AREA.ID, BUILDING.NUMBER.ON.LAND, SALE.DATE, REAL.ESTATE.ID) %>%
  group_by(AREA.ID, BUILDING.NUMBER.ON.LAND) %>%
  mutate(
    Amount_Paid_Per_Square_Foot_Lag = lag(Amount_Paid_Per_Square_Foot, n = 1, default = NA),
    Moving_Avg_Amount_Paid_Per_Sqft = ifelse(row_number() > 5,
                                             zoo::rollapply(
                                               Amount_Paid_Per_Square_Foot_Lag,
                                               width = 5,
                                               FUN = function(x) mean(x, na.rm = TRUE),
                                               align = "right",
                                               fill = NA
                                             ),
                                             NA)
  )



# Step 3: Copy and paste all rows as values
data <- data.frame(lapply(data, function(x) if (is.factor(x)) as.character(x) else x),
                   stringsAsFactors = FALSE)

data <- data %>%
  filter(!is.na(Moving_Avg_Amount_Paid_Per_Sqft))

print(paste("Number of rows after cleaning:", nrow(data)))

data$Rooms_NA <- ifelse(is.na(data$NO.OF.ROOMS) | data$NO.OF.ROOMS == "NA", 1, 0)

data$`2B` <- ifelse(is.na(data$NO.OF.ROOMS) | data$NO.OF.ROOMS != "2 B/R", 0, 1)

data$`3B` <- ifelse(is.na(data$NO.OF.ROOMS) | data$NO.OF.ROOMS != "3 B/R", 0, 1)

data$Studio <- ifelse(is.na(data$NO.OF.ROOMS) | data$NO.OF.ROOMS != "Studio", 0, 1)

data$Rooms_OTHER <- ifelse(is.na(data$NO.OF.ROOMS) | data$NO.OF.ROOMS != "OTHER", 0, 1)

data$Area.ID.102 <- ifelse(data$AREA.ID == 102, 1, 0)

data$Area.ID.213 <- ifelse(data$AREA.ID == 213, 1, 0)

data$TYPE.FLAT <- ifelse(is.na(data$TYPE.OF.PROPERTY) | data$TYPE.OF.PROPERTY != "Flat", 0, 1)


data$Type.Hotel <-  ifelse(data$TYPE.OF.PROPERTY %in% c("Hotel Rooms", "Hotel Apartment"),1,0)


data$Type.Office <- ifelse(data$TYPE.OF.PROPERTY == "Office",1,0)

lm_model_final <- lm(Amount_Paid_Per_Square_Foot ~ SQUARE.FOOTAGE + `Area.ID.102` + `Area.ID.213` + 
                       Rooms_NA + `2B` + `3B` + Studio + Rooms_OTHER + 
                       TYPE.FLAT + Type.Hotel + Type.Office + 
                       `Common_Area_Fixed` + `Moving_Avg_Amount_Paid_Per_Sqft`, 
                     data = data)

model_summary <- summary(lm_model_final)
print(model_summary)

# Print R^2 value
cat("Final R^2 Value:", summary(lm_model_final)$r.squared, "\n")

# Calculate MAPE (Mean Absolute Percentage Error) for the modified data
data$predicted_values_initial_model <- predict(lm_model_final, data)
mape_initial_model <- mean(abs((data$Amount_Paid_Per_Square_Foot - data$predicted_values_initial_model) / data$Amount_Paid_Per_Square_Foot)) * 100
cat("MAPE_initial_model:", mape_initial_model, "%\n")

Stepwise_Model <- step(lm_model_final)
summary(Stepwise_Model)

# Evaluate the stepwise model
cat("R-squared_stepwise:", summary(Stepwise_Model)$r.squared, "\n")
data$predicted_values_stepwise <- predict(Stepwise_Model, data)
mape_stepwise <- mean(abs((data$Amount_Paid_Per_Square_Foot - data$predicted_values_stepwise) / data$Amount_Paid_Per_Square_Foot)) * 100
cat("MAPE_Stepwise:", mape_stepwise, "%\n")

# Compare the models and choose the preferred one based on your criteria
if (summary(lm_model_final)$r.squared > summary(Stepwise_Model)$r.squared) {
  cat("The initial model is preferred because it has a higher R-squared.\n")
} else {
  cat("The stepwise model is preferred because it has a higher R-squared.\n")
}
write.csv(data, file = "final_file.csv")

