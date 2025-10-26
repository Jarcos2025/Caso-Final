library(tidyverse)
library(janitor)
library(stargazer)
library(pROC)
library(readxl)
library(ggplot2)

DATA <- read_excel("DATA.xlsx", sheet = "Case Data")
View(DATA)

datos_limpios <-DATA%>%
  clean_names()
datos_limpios <- datos_limpios %>%
  rename(
    id = id,
    age_months = customer_age_in_months,
    churn = churn_1_yes_0_no,
    chi_score_m0 = chi_score_month_0,
    chi_score_change = chi_score_0_1,
    support_cases_m0 = support_cases_month_0,
    support_cases_change = support_cases_0_1,
    sp_m0 = sp_month_0,
    sp_change = sp_0_1,
    logins_change = logins_0_1,
    blogs_change = blog_articles_0_1,
    views_change = views_0_1,
    days_last_login_change = days_since_last_login_0_1
  )
datos_limpios <- datos_limpios %>%
  mutate(
    age_group = case_when(
      age_months < 6 ~ "01. Menos de 6m",
      age_months >= 6 & age_months <= 14 ~ "02. 6-14 meses (Riesgo)",
      age_months > 14 ~ "03. Más de 14m"
    )
  )

vars_descriptivas <- datos_limpios %>%
  select(churn, age_months, chi_score_m0, chi_score_change, 
         support_cases_m0, logins_change, views_change)

print("--- Tabla 1: Estadísticas Descriptivas ---")
stargazer(
  as.data.frame(vars_descriptivas), 
  type = "text",
  title = "Tabla 1: Estadísticas Descriptivas de Clientes QWE",
  digits = 2,
  summary.stat = c("n", "mean", "sd", "min", "max")
)

modelo_logit <- glm(
  churn ~ age_group + chi_score_m0 + chi_score_change + 
    support_cases_m0 + sp_m0 + logins_change + views_change,
  data = datos_limpios,
  family = binomial(link = "logit") 
)

print("--- Tabla 2: Modelo Logit (Consola) ---")
stargazer(
  modelo_logit,
  type = "text",
  title = "Tabla 2: Modelo Logit para Predecir Churn",
  header = FALSE,
  dep.var.labels = "Probabilidad de Churn (1=Sí)",
  covariate.labels = c(
    "Antigüedad (6-14 meses)",
    "Antigüedad (> 14 meses)",
    "CHI Score (Mes 0)",
    "Cambio CHI (0-1)",
    "Casos de Soporte (Mes 0)",
    "Prioridad Soporte (SP Mes 0)",
    "Cambio Logins (0-1)",
    "Cambio Vistas (0-1)",
    "Intercepto"
  ),
  digits = 3
)

stargazer(
  modelo_logit,
  type = "rtf", 
  out = "tabla_regresion_logit.rtf", 
  title = "Tabla 2: Modelo Logit para Predecir Churn",
  header = FALSE,
  dep.var.labels = "Probabilidad de Churn (1=Sí)",
  covariate.labels = c(
    "Antigüedad (6-14 meses)",
    "Antigüedad (> 14 meses)",
    "CHI Score (Mes 0)",
    "Cambio CHI (0-1)",
    "Casos de Soporte (Mes 0)",
    "Prioridad Soporte (SP Mes 0)",
    "Cambio Logins (0-1)",
    "Cambio Vistas (0-1)",
    "Intercepto"
  ),
  digits = 3,
  notes = "Errores estándar en paréntesis" 
)

odds_ratios <- exp(coef(modelo_logit))
print("--- Odds Ratios (OR) del Modelo ---")
print(odds_ratios)

pR2_mcfadden <- 1 - (modelo_logit$deviance / modelo_logit$null.deviance)
print(paste("Pseudo R-cuadrado (McFadden):", round(pR2_mcfadden, 4)))

probabilidades <- predict(modelo_logit, type = "response")
predicciones_clase <- ifelse(probabilidades > 0.20, 1, 0)
matriz_confusion <- table(
  Real = datos_limpios$churn,
  Predicho = predicciones_clase
)

print("Matriz de Confusión (Predichos vs. Reales)")
print(matriz_confusion)
precision <- sum(diag(matriz_confusion)) / sum(matriz_confusion)
print(paste("Precisión (Accuracy) del modelo:", round(precision, 4)))

probabilidades <- predict(modelo_logit, type = "response")
predicciones_clase <- ifelse(probabilidades > 0.15, 1, 0)
matriz_confusion <- table(
  Real = datos_limpios$churn,
  Predicho = predicciones_clase
)

print("Matriz de Confusión (Predichos vs. Reales)")
print(matriz_confusion)
precision <- sum(diag(matriz_confusion)) / sum(matriz_confusion)
print(paste("Precisión (Accuracy) del modelo:", round(precision, 4)))

probabilidades <- predict(modelo_logit, type = "response")
predicciones_clase <- ifelse(probabilidades > 0.10, 1, 0)
matriz_confusion <- table(
  Real = datos_limpios$churn,
  Predicho = predicciones_clase
)

print("Matriz de Confusión (Predichos vs. Reales)")
print(matriz_confusion)
precision <- sum(diag(matriz_confusion)) / sum(matriz_confusion)
print(paste("Precisión (Accuracy) del modelo:", round(precision, 4)))

print("Métricas ROC")
roc_curva <- roc(datos_limpios$churn, probabilidades)
print(paste("Área bajo la Curva (AUC):", round(auc(roc_curva), 4)))

png("grafico_curva_roc.png")
plot(roc_curva, 
     main = "Gráfico de Evaluación: Curva ROC",
     col = "#1c61b6",
     lwd = 3)
abline(a = 0, b = 1, lty = 2, col = "gray")
text(0.4, 0.3, paste("AUC =", round(auc(roc_curva), 3)), cex = 1.5)
dev.off()

datos_limpios$prob_churn = probabilidades

grafico_distribucion <- ggplot(datos_limpios, aes(x = prob_churn, fill = as.factor(churn))) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Gráfico de Evaluación: Distribución de Probabilidades Predichas",
    x = "Probabilidad de Churn Predicha",
    y = "Densidad",
    fill = "Churn Real"
  ) +
  theme_minimal()

ggsave("grafico_distribucion_prob.png", plot = grafico_distribucion) # Guarda el gráfico
print(grafico_distribucion) 