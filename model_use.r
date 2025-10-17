library(rethinking)
library(dplyr)

df_full = read.csv("data2.csv")

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
    s_Quality = scale(quality)[,1]
  )


model_extent_ulam <- ulam(
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
  chains = 4, cores = 4, iter = 2000
)

model_quality_ulam <- ulam(
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
  chains = 4, cores = 4, iter = 2000
)

model_result_ulam <- ulam(
  alist(
    Result ~ dbern(p),
    logit(p) <- g0 + g_ai*AI_Use + g_e*s_Extent + g_q*s_Quality,
    
    g0 ~ dnorm(0, 1.5),
    g_ai ~ dnorm(0, 0.5),
    g_e ~ dnorm(0, 0.5),
    g_q ~ dnorm(0, 0.5)
  ),
  data = data_to_model,
  chains = 4, cores = 4, iter = 2000, warmup = 1000
)

E = precis(model_extent_ulam, pars = c("b_ai","b_skill","b_age","b_c"))
Q = precis(model_quality_ulam)
R = precis(model_result_ulam)


print(precis(model_extent_ulam))
print(precis(model_quality_ulam))
print(precis(model_result_ulam))
print(plot(E))
print(plot(Q))
print(plot(R))