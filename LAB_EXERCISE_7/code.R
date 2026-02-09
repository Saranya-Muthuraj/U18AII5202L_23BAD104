# ==========================================
# Roll No: 23BAD104
# Experiment: Reducing Visual Clutter
# ==========================================
library(readxl)
library(ggplot2)
df <- read_excel("C:/Users/student/Downloads/7.social_media_interactions.xlsx")

df <- na.omit(df)
df <- unique(df)

x <- df$Likes
y <- df$Comments

plot(
  df$Likes,
  df$Comments,
  pch  = 16,
  cex  = 0.8,
  col  = rgb(0, 0, 0.6, alpha = 0.3),   # darker blue
  xlab = "Likes",
  ylab = "Comments",
  main = "Alpha Blending (Enhanced Color)"
)

plot(
  jitter(df$Likes, amount = 10),
  jitter(df$Comments, amount = 10),
  pch  = 16,
  cex  = 0.8,
  col  = rgb(0.6, 0, 0, alpha = 0.3),   # darker red
  xlab = "Likes",
  ylab = "Comments",
  main = "Jittering (Enhanced Color)"
)

ggplot(df, aes(Likes, Comments)) +
  stat_bin2d(bins = 40) +
  scale_fill_gradient(
    low  = "lightblue",
    high = "darkblue"
  ) +
  labs(
    title = "Aggregation and Binning of Engagement Data",
    x     = "Likes",
    y     = "Comments"
  )
