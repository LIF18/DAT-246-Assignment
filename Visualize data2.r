library(corrplot)
library(ggplot2)
library(dplyr)


df <- read.csv("data2.csv")
numeric_vars <- df %>% select(complexity, result, extent, quality, age, skill, ai_use)
cor_matrix <- cor(numeric_vars) 

png("correlation_matrix.png", width = 800, height = 800)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = TRUE)
dev.off()


df$ai_use_factor <- factor(df$ai_use, labels = c("No AI", "AI Used"))


plot_quality <- ggplot(df, aes(x = ai_use_factor, y = quality, fill = ai_use_factor)) +
  geom_boxplot() +
  labs(title = "Code Review Quality by AI Support",
       x = "AI Usage",
       y = "Review Quality (1-5)") +
  theme_minimal() +
  theme(legend.position = "none")


plot_extent <- ggplot(df, aes(x = ai_use_factor, y = extent, fill = ai_use_factor)) +
  geom_boxplot() +
  labs(title = "Code Review Extent by AI Support",
       x = "AI Usage",
       y = "Review Extent (Number of Characters)") +
  theme_minimal() +
  theme(legend.position = "none") 

# ggsave("ai_vs_extent.png", plot = plot_extent, width = 6, height = 5)

print(plot_quality)
print(plot_extent)
# print("1. correlation_matrix.png")
# print("2. ai_vs_quality.png")
# print("3. ai_vs_extent.png")