#Importing the data and inspecting it
data <- tryCatch({
  
  read.csv("PRSA_Data_Aotizhongxin_20130301-20170228.csv")
  
}, error=function(e){
  
  cat("Error:", e$message,"\n")
  NULL
  
})

if(!is.null(data)){
  
  head(data)
  
  str(data)
  
  dim(data)
  
  anyNA(data)
  
  sum(is.na(data))
  
}


# TASK 2 : finding na solutions

cat("NA values in dataset:", sum(is.na(data)), "\n")

cat("NA values in PM2.5:", sum(is.na(data$PM2.5)), "\n")

# NULL example
missing_column <- data$XYZ
cat("Is missing column NULL?:", is.null(missing_column), "\n")

# NaN example
data$pollution_ratio <- data$PM2.5 / data$PM10
data$pollution_ratio[1] <- 0/0

cat("NaN values in pollution_ratio:", sum(is.nan(data$pollution_ratio)), "\n")



# TASK #: Summarising the null and NA values
missing_summary <- function(df) {
  
  variables <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM", "wd")
  
  summary_table <- data.frame(
    Variable = character(),
    Total_Records = integer(),
    Missing_Values = integer(),
    Missing_Percentage = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (var in variables) {
    
    # Check if variable exists
    if (var %in% names(df)) {
      
      total_records <- nrow(df)
      missing_values <- sum(is.na(df[[var]]))
      missing_percentage <- (missing_values / total_records) * 100
      
      summary_table <- rbind(
        summary_table,
        data.frame(
          Variable = var,
          Total_Records = total_records,
          Missing_Values = missing_values,
          Missing_Percentage = round(missing_percentage, 2)
        )
      )
      
      if (missing_percentage > 20) {
        warning(paste(var, "contains more than 20% missing values."))
      }
      
    } else {
      
      warning(paste(var, "does not exist in the dataset."))
      
    }
  }
  
  return(summary_table)
}

# Call the function
summary_result <- missing_summary(data)

# Display the summary
print(summary_result)

#TASK 4: Identifying invalid variables
# Create pollution_ratio
data$pollution_ratio <- data$PM2.5 / data$PM10

# Count different invalid values
na_count <- sum(is.na(data$pollution_ratio))
nan_count <- sum(is.nan(data$pollution_ratio))
inf_count <- sum(is.infinite(data$pollution_ratio))

cat("NA values:", na_count, "\n")
cat("NaN values:", nan_count, "\n")
cat("Infinite values:", inf_count, "\n")

cat("\nAfter Replacement\n")

cat("NA values:", sum(is.na(data$pollution_ratio)), "\n")
cat("NaN values:", sum(is.nan(data$pollution_ratio)), "\n")
cat("Infinite values:", sum(is.infinite(data$pollution_ratio)), "\n")



#TASK 5: filling in the missing values
numeric_variables <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM")

# Store missing values for Task 8
missing_before <- c()
missing_after <- c()

for (var in numeric_variables) {
  
  # Check if the column exists
  if (var %in% names(data)) {
    
    # Count missing values before treatment
    before <- sum(is.na(data[[var]]))
    
    # Calculate median
    med <- median(data[[var]], na.rm = TRUE)
    
    # Replace missing values with median
    data[[var]][is.na(data[[var]])] <- med
    
    # Count missing values after treatment
    after <- sum(is.na(data[[var]]))
    
    # Store values for Task 8
    missing_before[var] <- before
    missing_after[var] <- after
    
    # Display results
    cat("\n-----------------------------------\n")
    cat("Variable :", var, "\n")
    cat("Missing Before :", before, "\n")
    cat("Median Used :", med, "\n")
    cat("Missing After :", after, "\n")
    
  } else {
    
    cat("\nColumn", var, "does not exist.\n")
    
  }
}


# Task 6: Handle Missing Categorical Values 
calculate_mode <- function(x) {
  
  # Remove NA values
  x <- x[!is.na(x)]
  
  # Find unique values
  unique_values <- unique(x)
  
  # Return the most frequent value
  mode_value <- unique_values[which.max(tabulate(match(x, unique_values)))]
  
  return(mode_value)
}

# Count missing values before replacement
before_wd <- sum(is.na(data$wd))

# Calculate mode of wd
mode_wd <- calculate_mode(data$wd)

# Replace NA values with mode
data$wd[is.na(data$wd)] <- mode_wd

# Count missing values after replacement
after_wd <- sum(is.na(data$wd))

# Display results
cat("Mode of wd:", mode_wd, "\n")
cat("Missing values before:", before_wd, "\n")
cat("Missing values after:", after_wd, "\n")


#Task 7: Implement Error Handling
calculate_mode <- function(x) {
  
  # Remove NA values
  x <- x[!is.na(x)]
  
  # Find unique values
  unique_values <- unique(x)
  
  # Return the most frequent value
  mode_value <- unique_values[which.max(tabulate(match(x, unique_values)))]
  
  return(mode_value)
}

# Count missing values before replacement
before_wd <- sum(is.na(data$wd))

# Calculate mode of wd
mode_wd <- calculate_mode(data$wd)

# Replace NA values with mode
data$wd[is.na(data$wd)] <- mode_wd

# Count missing values after replacement
after_wd <- sum(is.na(data$wd))

# Display results
cat("Mode of wd:", mode_wd, "\n")
cat("Missing values before:", before_wd, "\n")
cat("Missing values after:", after_wd, "\n")


#Task 8: Compare Missing Values Before and After Cleaning
comparison_table <- data.frame(
  
  Variable = c(numeric_variables, "wd"),
  
  Missing_Before = c(
    missing_before,
    before_wd
  ),
  
  Missing_After = c(
    missing_after,
    after_wd
  )
  
)

comparison_table$Values_Replaced <-
  comparison_table$Missing_Before -
  comparison_table$Missing_After

print(comparison_table)

#Task 9: Generate One Visualization 
before <- c(missing_before, before_wd)
after <- c(missing_after, after_wd)

barplot(
  rbind(before, after),
  
  beside = TRUE,
  
  names.arg = c(numeric_variables, "wd"),
  
  col = c("red", "green"),
  
  legend.text = c("Before Cleaning", "After Cleaning"),
  
  main = "Missing Values Before and After Cleaning",
  
  xlab = "Variables",
  
  ylab = "Number of Missing Values"
)



#Task 10: Export the Cleaned Dataset 
write.csv(
  data,
  "cleaned_air_quality_data.csv",
  row.names = FALSE
)

cat("Cleaned dataset exported successfully!\n")


