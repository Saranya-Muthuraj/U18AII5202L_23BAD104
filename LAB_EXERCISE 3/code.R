# EXPERIMENT: Multivariate Data Visualization
# STUDENT ROLL NO: 23BAD104

head(x3_healthcare_data)
colnames(x3_healthcare_data)

x3_healthcare_data$AgeGroup <- cut(
  x3_healthcare_data$Age,
  breaks = c(19, 35, 50, 70),
  labels = c("Young", "Middle", "Senior")
)

pairs(
  x3_healthcare_data[, c("Age", "BMI", "Glucose_Level", "Blood_Pressure")],
  col = x3_healthcare_data$AgeGroup,
  main = "Scatter Plot Matrix of Health Indicators"
)

cor(
  x3_healthcare_data[, c("Age", "BMI", "Glucose_Level", "Blood_Pressure")]
)

