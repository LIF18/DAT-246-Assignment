library(rethinking)
library(dplyr)
library(ggplot2)

df_full <- read.csv("data2.csv")

data_to_model <- df_full %>%
  mutate(
    AI_Use = ai_use,
    Result = result,
    Extent = extent,
    Quality = factor(quality, ordered = TRUE), 
    s_Skill = scale(skill)[,1],
    s_Age = scale(age)[,1],
    s_Complexity = scale(complexity)[,1],
    s_Extent = scale(extent)[,1],
    s_Quality = as.numeric(scale(as.numeric(quality))[,1])
  )

model_extent_priors <- ulam(
  alist(
    Extent ~ dgampois(lambda, scale),
    log(lambda) <- b0 + b_ai*AI_Use + b_skill*s_Skill + b_age*s_Age + b_c*s_Complexity,

    b0 ~ dnorm(log(mean(Extent)), 0.5), 
    b_ai ~ dnorm(0, 0.2),
    b_skill ~ dnorm(0, 0.2),
    b_age ~ dnorm(0, 0.2),
    b_c ~ dnorm(0, 0.2),
    scale ~ dexp(1)
  ),
  data = data_to_model,
  chains = 4, cores = 4, iter = 2000,
  sample_prior = "only" 
)

prior_pred_extent <- sim(model_extent_priors, n = 10000)

prior_df_extent <- data.frame(Extent = as.vector(prior_pred_extent))

prior_extent <- ggplot(prior_df_extent, aes(x = Extent)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(
    title = "Prior Predictive Check for Extent",
    subtitle = "Distribution of simulated CR lengths from priors",
    x = "Simulated CR Extent (Number of Characters)",
    y = "Frequency"
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, quantile(prior_df_extent$Extent, 0.99))) 


print(prior_extent)


model_quality_priors <- ulam(
  alist(
    Quality ~ dordlogit(phi, cutpoints),
    phi <- a_ai*AI_Use + a_skill*s_Skill + a_age*s_Age + a_c*s_Complexity,

    a_ai ~ dnorm(0, 0.5),
    a_skill ~ dnorm(0, 0.5),
    a_age ~ dnorm(0, 0.5),
    a_c ~ dnorm(0, 0.5),
    cutpoints ~ dnorm(0, 1.5)
  ),
  data = data_to_model,
  chains = 4, cores = 4, iter = 2000,
  sample_prior = "only" # key point for prior check
)

prior_pred_quality <- sim(model_quality_priors, n = 10000)

prior_df_quality <- data.frame(Quality = factor(as.vector(prior_pred_quality), levels = 1:5, ordered = TRUE))

prior_quality <- ggplot(prior_df_quality, aes(x = Quality)) +
  geom_bar(fill = "lightgreen", color = "black", alpha = 0.8) +
  labs(
    title = "Prior Predictive Check for Quality",
    subtitle = "Distribution of simulated quality from priors",
    x = "Simulated Quality(1-5)",
    y = "Frequency"
  ) +
  theme_minimal()

print(prior_quality)


model_result_priors <- ulam(
  alist(
    Result ~ dbern(p),
    logit(p) <- g0 + g_ai*AI_Use + g_e*s_Extent + g_q*s_Quality,
    
    g0 ~ dnorm(0, 1.5),
    g_ai ~ dnorm(0, 1),
    g_e ~ dnorm(0, 1),
    g_q ~ dnorm(0, 1)
  ),
  data = data_to_model,
  chains = 4, cores = 4, iter = 2000,
  sample_prior = "only" 
)

prior_p_result <- link(model_result_priors)
prior_df_result_p <- data.frame(probability = as.vector(prior_p_result))

prior_result <- ggplot(prior_df_result_p, aes(x = probability)) +
  geom_density(fill = "salmon", alpha = 0.8) +
  labs(
    title = "Prior Predictive Check for Result",
    subtitle = "Distribution of P(Result=1) from priors",
    x = "Simulated Probability of CR being relevant",
    y = "Density"
  ) +
  theme_minimal()

print(prior_result)