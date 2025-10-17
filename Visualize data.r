library(dplyr)
library(ggplot2)



# reviewers <- read.csv("reviewers.csv")
# reviews <- read.csv("reviews.csv")
# df_full <- left_join(reviews, reviewers, by = "reviewer.id")
  
# df_full$AI_Usage_Label <- factor(df_full$used.cr.technology, 
#                                    levels = c(0, 1), 
#                                    labels = c("Non-AI User", "AI User"))
# write.csv(df_full,"data.csv",row.names = FALSE)

df_full = read.csv("data.csv")

age_ai = ggplot(df_full, aes(x = AI_Usage_Label, y = age, fill = AI_Usage_Label)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Engineer Age by AI Usage",
    x = "AI Usage",
    y = "Age(years)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") +
  guides(fill = "none")

age_skill = ggplot(distinct(df_full, reviewer.id, .keep_all = TRUE), aes(x = age, y = skill)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, color = "firebrick") +
  labs(
    title = "Relationship between Engineer Skill and Age",
    x = "Age (years)",
    y = "Skill Level"
  ) +
  theme_minimal()

print(age_ai)
print(age_skill)
#print(ai_quality)
