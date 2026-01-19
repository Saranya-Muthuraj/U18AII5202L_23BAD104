# Roll no : 23BAD104
# -------------------------

library(ggplot2)
library(dplyr)
library(tidyr)

# Loading the dataset
df <- X1_student_performance

# Converting columns to factors
df$Subject <- as.factor(df$Subject)
df$Final_Grade <- as.factor(df$Final_Grade)

# Calculating Average Internal Marks
df <- df %>%
  mutate(
    Average_Internal = (Internal_Test1 + Internal_Test2 + Assignment_Marks) / 3
  )

# Bar Chart: Subject-wise Average Marks
ggplot(
  df %>%
    group_by(Subject) %>%
    summarise(Avg = mean(Average_Internal, na.rm = TRUE)),
  aes(x = Subject, y = Avg, fill = Subject)
) +
  geom_bar(stat = "identity") +
  ggtitle("Subject-wise Average Marks")

# Line Chart: Performance Trend Across Tests
ggplot(
  df %>%
    summarise(
      Test1 = mean(Internal_Test1, na.rm = TRUE),
      Test2 = mean(Internal_Test2, na.rm = TRUE)
    ) %>%
    pivot_longer(
      everything(),
      names_to = "Test",
      values_to = "Marks"
    ),
  aes(x = Test, y = Marks)
) +
  geom_line(group = 1) +
  geom_point() +
  ggtitle("Performance Trend Across Tests")

# Pie Chart: Grade Distribution
ggplot(
  df %>% count(Final_Grade),
  aes(x = "", y = n, fill = Final_Grade)
) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  ggtitle("Grade Distribution") +
  theme_void()
