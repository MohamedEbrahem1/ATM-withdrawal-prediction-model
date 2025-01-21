# This R script preprocesses the data before it enters the cycles of iterations in our model improvement

required_packages <- c("olsrr", "MASS", "ggplot2", "dplyr", "fastDummies", "car", "lmtest")

# Install packages if they are not already installed
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Load dataset
data <- read.csv("transformed_atm_data.csv")
head(data)
data <- data[data$atm_id == "1", ] # The analysis was focused on one ATM 
data <- data[, !grepl("^atm_", names(data))]
columns_to_remove <- names(data) %in% c("currency", "weather_id", "card_type","weather_description","weather_city_name","weather_city_id")
data<- data[,!columns_to_remove]

# Identify and process categorical variables
categorical_cols <- names(data)[sapply(data, is.character)]
data <- dummy_cols(data, select_columns = categorical_cols, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
#data <- data[, !grepl("^month", names(data))]

head(data)

write.csv(data, "preprocessed_file.csv", row.names = FALSE)
