# ==========================================================
# Roll no: 23BAD104
# ==========================================================
# TRAFFIC ACCIDENT VISUAL ENCODING (R)
# Dataset Name: Traffic_accidents
# Severity       -> Color Gradient
# Frequency      -> Size
# Accident Type  -> Shape
# ==========================================================

library(ggplot2)
library(dplyr)

colnames(Traffic_accidents)

Traffic_accidents$Severity <- as.numeric(Traffic_accidents$Severity)

acc_summary <- Traffic_accidents %>%
  group_by(Location, Accident_Type) %>%
  summarise(
    Accident_Frequency = n(),                     # Size Encoding
    Avg_Severity = mean(Severity, na.rm = TRUE),  # Color Encoding
    .groups = "drop"
  )

ggplot(
  acc_summary,
  aes(
    x = Location,
    y = Accident_Frequency
  )
) +
  geom_point(
    aes(
      color = Avg_Severity,
      size  = Accident_Frequency,
      shape = Accident_Type
    ),
    alpha = 0.85
  ) +
  scale_color_gradient(
    low  = "lightgreen",
    high = "red"
  ) +
  scale_size(
    range = c(4, 18)
  ) +
  labs(
    title    = "City Traffic Accident High Risk Zones",
    subtitle = "Color = Severity | Size = Accident Frequency | Shape = Accident Type",
    x        = "Location",
    y        = "Accident Frequency",
    color    = "Average Severity",
    size     = "Frequency",
    shape    = "Accident Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
