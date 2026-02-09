# ==========================================================
# EXPERIMENT 6 - IMPLEMENTATION OF MULTIVARIATE DISPLAYS
# Student Roll No : 23BAD104
# ==========================================================

library(ggplot2)
library(dplyr)
library(GGally)

data <- read.csv("C:/Users/mugim/Downloads/6.retail_business.xlsx")

data$Region <- as.factor(data$Region)
data$Product_Category <- as.factor(data$Product_Category)
data$Customer_Segment <- as.factor(data$Customer_Segment)

if (!dir.exists("figures")) {
  dir.create("figures")
}

# ----------------------------------------------------------
# Parallel Coordinate Plot
# ----------------------------------------------------------
png("figures/parallel_plot.png", width = 800, height = 600)

ggparcoord(
  data,
  columns = c(4, 5, 6),
  groupColumn = "Customer_Segment",
  scale = "std"
) +
  labs(
    title = "Parallel Coordinate Plot of Sales, Profit and Discount"
  ) +
  theme_minimal()

dev.off()

# ----------------------------------------------------------
# Bubble Chart
# ----------------------------------------------------------
png("figures/bubble_chart.png", width = 800, height = 600)

ggplot(
  data,
  aes(
    x = Sales,
    y = Profit,
    size = Discount,
    color = Region
  )
) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Bubble Chart: Sales vs Profit",
    x = "Sales",
    y = "Profit",
    size = "Discount"
  ) +
  theme_minimal()

dev.off()

# ----------------------------------------------------------
# Trellis Display
# ----------------------------------------------------------
png("figures/trellis_display.png", width = 900, height = 600)

ggplot(
  data,
  aes(
    x = Product_Category,
    y = Profit,
    fill = Customer_Segment
  )
) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Region) +
  labs(
    title = "Trellis Display: Profit by Category and Region",
    x = "Product Category",
    y = "Profit"
  ) +
  theme_minimal()

dev.off()

print("All plots saved in figures folder")
