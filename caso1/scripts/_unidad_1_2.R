# =============================================================================
# Caso 1 · Unidad 1.2 — 2 · Formulación de los GLM: estimación, inferencia, interpretación y evaluación
# -----------------------------------------------------------------------------
# Todos los chunks de código de la unidad, extraídos de _unidad_1_2.qmd.
# Cada bloque va precedido de su LABEL y de la sección/subsección donde aparece.
#
# EJECUCIÓN: funciona desde CUALQUIER carpeta del proyecto GLM (localiza la raíz
# por _quarto.yml). Cada unidad carga además en sus chunks sus librerías propias.
# =============================================================================

# --- Librerías base (setup del documento del caso) ---------------------------
library(broom); library(aplore3); library(patchwork); library(see)
library(DHARMa); library(arm); library(performance); library(tidyverse)
library(MuMIn); library(readr); library(GGally)

SEMILLA_CURSO <- 20252026L
set.seed(SEMILLA_CURSO)
theme_set(theme_minimal(base_size = 12))

# --- Raíz del proyecto (robusto al directorio de trabajo) --------------------
.raiz <- getwd()
while (!file.exists(file.path(.raiz, "_quarto.yml")) && dirname(.raiz) != .raiz) .raiz <- dirname(.raiz)
if (!file.exists(file.path(.raiz, "_quarto.yml"))) stop("Abre el proyecto GLM: no encuentro _quarto.yml.")

# --- Dato real GLOW ----------------------------------------------------------
data(glow500)
glow <- glow500 |>
  as_tibble() |>
  mutate(fractura01 = as.integer(fracture) - 1L)   # No -> 0, Yes -> 1

# --- Cohorte simulada con CACHÉ (misma que el documento del caso) ------------
source(file.path(.raiz, "caso1", "R", "dgp_cohorte.R"))  # simular_cohorte(), cargar_cohorte(), expandir_persona_periodo()
cohorte <- cargar_cohorte()          # lee datos/ si existe; si no (o si cambió el DGP), simula y cachea


# --- Dependencia: reutiliza objetos de la Unidad 1.1 (rango).
#     Si no están en el entorno, se construyen ejecutando su script.
if (!all(sapply(c("rango"), exists))) source(file.path(.raiz, "caso1", "scripts", "_unidad_1_1.R"))

# -----------------------------------------------------------------------------
# [u12-setup]  ·  (introducción)
# -----------------------------------------------------------------------------
# La madre ya cargó tidyverse, broom, performance, DHARMa, arm, see, patchwork.
# Esta unidad añade lo suyo:
library(marginaleffects)   # efectos marginales / predicciones (Arel-Bundock et al., 2024)
library(car)               # Anova tipo II/III (Fox & Weisberg, 2019)
library(pROC)              # curva ROC y AUC (Robin et al., 2011)
library(emmeans)           # medias marginales estimadas
library(ggeffects)         # predicciones para graficar
library(MASS)              # confint por perfil, stepAIC (recommended package)

# Recuperamos el ajuste de 1.1 por si esta unidad se compila aislada:
if (!exists("fit_glm")) {
  fit_glm <- glm(fracture ~ age + priorfrac, family = binomial, data = glow)
}

# -----------------------------------------------------------------------------
# [u12-familia]  ·  2.1 Familia exponencial y relación media–varianza
# -----------------------------------------------------------------------------
family(fit_glm)              # familia y enlace del ajuste de 1.1
binomial()$variance          # función de varianza V(mu) = mu(1-mu)
binomial()$linkfun(0.25)     # logit(0.25): de la media al predictor
binomial()$linkinv(-1.0986)  # sigmoide: del predictor a la media

# -----------------------------------------------------------------------------
# [fig-u12-varianza-mu]  ·  2.1 Familia exponencial y relación media–varianza
# -----------------------------------------------------------------------------
glow |>
  mutate(p_hat = fitted(fit_glm),
         tramo = ntile(p_hat, 10)) |>
  group_by(tramo) |>
  summarise(mu = mean(p_hat),
            var_emp = var(fractura01), .groups = "drop") |>
  ggplot(aes(mu, var_emp)) +
  geom_function(fun = \(m) m * (1 - m), linewidth = 1, color = "firebrick") +
  geom_point(size = 2) +
  labs(x = expression(hat(mu)), y = "Varianza empírica de Y por tramo",
       caption = expression("Curva: V(" * mu * ") = " * mu * "(1 - " * mu * ")"))

# -----------------------------------------------------------------------------
# [fig-u12-enlaces]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
tibble(eta = seq(-5, 5, by = 0.02)) |>
  mutate(logit   = plogis(eta),
         probit  = pnorm(eta),
         cloglog = 1 - exp(-exp(eta))) |>
  pivot_longer(c(logit, probit, cloglog), names_to = "enlace", values_to = "p") |>
  ggplot(aes(eta, p, color = enlace)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = c(0, 1), linetype = "dotted") +
  labs(x = expression(eta), y = expression(p == g^{-1}(eta)), color = "Enlace")

# -----------------------------------------------------------------------------
# [u12-enlaces]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
ajustes <- list(
  logit   = glm(fracture ~ age + priorfrac, binomial("logit"),   glow),
  probit  = glm(fracture ~ age + priorfrac, binomial("probit"),  glow),
  cloglog = glm(fracture ~ age + priorfrac, binomial("cloglog"), glow))

map_dfr(ajustes, \(m) glance(m) |> select(deviance, AIC), .id = "enlace")
map_dfr(ajustes, tidy, .id = "enlace") |>
  select(enlace, term, estimate) |>
  pivot_wider(names_from = enlace, values_from = estimate)

# -----------------------------------------------------------------------------
# [fig-u12-enlaces-prob]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
rango_obs <- range(glow$age)
grid <- expand_grid(
  age       = seq(40, 105, length.out = 300),
  priorfrac = factor(c("No", "Yes"), levels = levels(glow$priorfrac)))

# Predicción en probabilidad de cada uno de los tres modelos de `ajustes`
pred_enlaces <- map_dfr(ajustes, \(m) {
  grid |> mutate(p_hat = predict(m, newdata = grid, type = "response"))
}, .id = "enlace") |>
  mutate(enlace = factor(enlace, levels = c("logit", "probit", "cloglog")))
pred_obs <- filter(pred_enlaces, age >= rango_obs[1], age <= rango_obs[2])

etiq_pf <- c(No = "Sin fractura previa", Yes = "Con fractura previa")

ggplot(pred_enlaces, aes(age, p_hat, color = enlace)) +
  annotate("rect", xmin = rango_obs[1], xmax = rango_obs[2],
           ymin = -Inf, ymax = Inf, alpha = 0.05) +
  geom_line(linewidth = 0.7, linetype = "22") +          # extrapolación
  geom_line(data = pred_obs, linewidth = 1.1) +          # rango observado
  facet_wrap(~ priorfrac, labeller = labeller(priorfrac = etiq_pf)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent()) +
  labs(x = "Edad (años)", y = "P(fractura)", color = "Enlace") +
  theme(legend.position = "bottom")

# -----------------------------------------------------------------------------
# [u12-coef]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
tidy(fit_glm)   # coeficientes en escala log-odds: lo que da summary() por defecto

# -----------------------------------------------------------------------------
# [fig-u12-tres-escalas]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
betas <- c("β = +1 (positivo)" = 1, "β = −1 (negativo)" = -1)
demo <- expand_grid(x = seq(-3, 3, length.out = 300), signo = names(betas)) |>
  mutate(beta = betas[signo],
         `log-odds` = beta * x,          # β0 = 0
         odds       = exp(beta * x),
         prob       = plogis(beta * x))

col <- c("β = +1 (positivo)" = "#2c7fb8", "β = −1 (negativo)" = "#d7301f")
escala <- function(y, ylab, titulo, y0)
  ggplot(demo, aes(x, .data[[y]], color = signo)) +
    geom_hline(yintercept = y0, linetype = "dotted", color = "grey60") +
    geom_line(linewidth = 1.1) +
    scale_color_manual(values = col) +
    labs(x = "predictor x", y = ylab, color = NULL, title = titulo)

p1 <- escala("log-odds", "log-odds (η)", "η = β₀ + βx  ·  recta (pendiente β)", 0)
p2 <- escala("odds",     "odds = e^η",   "odds  ·  multiplica e^β por unidad",            1)
p3 <- escala("prob",     "P(evento)",    "probabilidad  ·  forma en S",        0.5) +
        scale_y_continuous(limits = c(0, 1), labels = scales::label_percent())

(p1 + p2 + p3) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        plot.title = element_text(size = 9.5, face = "bold"))

# -----------------------------------------------------------------------------
# [u12-or]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
exp(cbind(OR = coef(fit_glm), confint(fit_glm)))   # OR e IC: exponencial del IC de beta

# -----------------------------------------------------------------------------
# [u12-coef-enlaces]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
# Mismos datos, distinta escala del coeficiente según el enlace
map_dfr(ajustes, \(m) tidy(m) |> select(term, estimate), .id = "enlace") |>
  pivot_wider(names_from = enlace, values_from = estimate) |>
  mutate(razon_logit_probit = logit / probit)   # ~1,6-1,8 (salvo intercepto)

# -----------------------------------------------------------------------------
# [u12-ame]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
# AME de cada covariable, en puntos de probabilidad
avg_slopes(fit_glm)                       

# Reproducción "a mano" del cálculo del AME para 'age':
p <- fitted(fit_glm)                      # pi_hat de cada individuo
mean(p * (1 - p)) * coef(fit_glm)["age"]  # = AME de age

# -----------------------------------------------------------------------------
# [u12-ame-factor]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
# Reproducción "a mano" del cálculo de AME para 'priorfrac'
g_yes <- transform(glow, priorfrac = factor("Yes", levels = levels(glow$priorfrac)))
g_no  <- transform(glow, priorfrac = factor("No",  levels = levels(glow$priorfrac)))

mean(predict(fit_glm, g_yes, type = "response") -
     predict(fit_glm, g_no,  type = "response"))   # = AME de priorfrac

# -----------------------------------------------------------------------------
# [fig-u12-dos-escalas]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
rango_obs <- range(glow$age)        # ~55–90: edad observada en GLOW
grid <- expand_grid(
  age       = seq(40, 105, length.out = 300),
  priorfrac = factor(c("No", "Yes"), levels = levels(glow$priorfrac)))
pr   <- predict(fit_glm, newdata = grid, type = "link", se.fit = TRUE)
grid <- grid |>
  mutate(eta = pr$fit, se = pr$se.fit,
         lo_eta = eta - 1.96 * se, hi_eta = eta + 1.96 * se,   # IC 95 % en log-odds
         p_hat  = plogis(eta),
         lo_p   = plogis(lo_eta), hi_p = plogis(hi_eta))       # IC 95 % en probabilidad
grid_obs <- filter(grid, age >= rango_obs[1], age <= rango_obs[2])

pA <- ggplot(grid, aes(age, eta, color = priorfrac, fill = priorfrac)) +
  annotate("rect", xmin = rango_obs[1], xmax = rango_obs[2],
           ymin = -Inf, ymax = Inf, alpha = 0.06) +
  geom_ribbon(aes(ymin = lo_eta, ymax = hi_eta), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.7, linetype = "22") +          # extrapolación
  geom_line(data = grid_obs, linewidth = 1.2) +          # rango observado
  labs(x = "Edad (años)", y = "log-odds (η)",
       color = "Fractura previa", fill = "Fractura previa",
       title = "Escala log-odds", subtitle = "rectas paralelas (separación ≈ 0,84)")

pB <- ggplot(grid, aes(age, p_hat, color = priorfrac, fill = priorfrac)) +
  annotate("rect", xmin = rango_obs[1], xmax = rango_obs[2],
           ymin = -Inf, ymax = Inf, alpha = 0.06) +
  geom_ribbon(aes(ymin = lo_p, ymax = hi_p), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.7, linetype = "22") +
  geom_line(data = grid_obs, linewidth = 1.2) +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent()) +
  labs(x = "Edad (años)", y = "P(fractura)",
       color = "Fractura previa", fill = "Fractura previa",
       title = "Escala probabilidad", subtitle = "curvas en S (separación variable)")

(pA + pB) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        plot.title    = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 8.5))

# -----------------------------------------------------------------------------
# [u12-iwls]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
X   <- model.matrix(fit_glm)          # matriz de diseño: columnas (Intercept), age, priorfracYes
y   <- glow$fractura01                # respuesta binaria 0/1
fam <- binomial()                     # la familia expone las piezas: linkinv, mu.eta, variance

beta  <- rep(0, ncol(X))              # arranque: todos los coeficientes a 0
traza <- list()                       # aquí guardaremos beta de cada iteración (para la figura)

for (it in 1:25) {
  eta <- as.vector(X %*% beta)        # predictor lineal      eta = X beta
  mu  <- fam$linkinv(eta)             # media ajustada        mu  = g^{-1}(eta) = plogis(eta)
  mep <- fam$mu.eta(eta)              # derivada del enlace    dmu/deta = mu(1-mu)
  z   <- eta + (y - mu) / mep         # respuesta de trabajo  (linealización de un paso)
  w   <- mep^2 / fam$variance(mu)     # pesos IWLS             w = (dmu/deta)^2 / V(mu)
  WX  <- X * w                        # cada fila de X escalada por su peso
  # paso de mínimos cuadrados ponderados: beta = (X'WX)^{-1} X'W z
  beta_new <- as.vector(solve(crossprod(WX, X), crossprod(WX, z)))
  traza[[it]] <- beta_new             # registramos el beta de esta vuelta
  if (max(abs(beta_new - beta)) < 1e-10) { beta <- beta_new; break }  # ¿convergió?
  beta <- beta_new                    # si no, otra iteración partiendo del nuevo beta
}
cat("Convergió en", it, "iteraciones\n")
cbind(IWLS_a_mano = beta, glm = coef(fit_glm))   # deben coincidir con glm()

# -----------------------------------------------------------------------------
# [fig-u12-iwls-convergencia]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
do.call(rbind, traza) |>          # apila la lista en matriz: filas = iteraciones, columnas = coeficientes
  as.data.frame() |>
  setNames(colnames(X)) |>        # nombra las columnas como los coeficientes
  mutate(iter = row_number()) |>  # índice de iteración
  pivot_longer(-iter, names_to = "coef", values_to = "valor") |>   # formato largo para ggplot
  ggplot(aes(iter, valor, color = coef)) +
  geom_line() + geom_point(size = 1.5) +
  geom_hline(data = enframe(coef(fit_glm), "coef", "obj"),         # valores de glm() como referencia
             aes(yintercept = obj, color = coef), linetype = "dashed") +
  labs(x = "Iteración IWLS", y = "Coeficiente", color = NULL)

# -----------------------------------------------------------------------------
# [u12-deviance]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
glance(fit_glm) |>
  select(null.deviance, df.null, deviance, df.residual, logLik, AIC, BIC)

# -----------------------------------------------------------------------------
# [u12-confint]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
list(perfil = confint(fit_glm),
     wald   = confint.default(fit_glm)) |>
  map(\(m) round(m, 4))

# -----------------------------------------------------------------------------
# [u12-inferencia]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
# Wald en escala log-odds: aporte de cada término independientemente
tidy(fit_glm)  # proporcionado por summary(fit_glm)

# Anova tipo II: aporte de cada término ajustando por el resto
car::Anova(fit_glm, type = "II")

# LRT ómnibus: aporte del bloque completo frente al modelo nulo
anova(glm(fracture ~ 1, binomial, glow), fit_glm, test = "LRT")

# -----------------------------------------------------------------------------
# [u12-escalera]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
mod_0 <- glm(fracture ~ age + priorfrac,                     family = binomial, data = glow)
mod_1 <- glm(fracture ~ age + priorfrac + momfrac + bmi,     family = binomial, data = glow)
mod_2 <- glm(fracture ~ age * priorfrac + bmi + momfrac, family = binomial, data = glow)
mod_3 <- glm(fracture ~ (age + bmi) * priorfrac + momfrac, family = binomial, data = glow)
mod_4 <- glm(fracture ~ (age + bmi) * (priorfrac + momfrac), family = binomial, data = glow)
tibble(modelo = paste0("mod_", 1:4)) |>
  bind_cols(map_dfr(list(mod_1, mod_2, mod_3, mod_4),
                    \(m) glance(m) |> dplyr::select(df = df.residual, deviance, AIC, BIC)))

# -----------------------------------------------------------------------------
# [u12-lrt-escalera]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
anova(mod_4, mod_3,  test = "LRT")   
anova(mod_3, mod_2, test = "LRT")   
anova(mod_2, mod_1, test = "LRT") 
anova(mod_1, mod_0, test = "LRT") 

# -----------------------------------------------------------------------------
# [u12-interaccion-coef]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
tidy(mod_2) 

# -----------------------------------------------------------------------------
# [u12-auto]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
#library(MuMIn)
global <- glm(fracture ~ (age + bmi) * (priorfrac + momfrac),
              family = binomial, data = glow, na.action = na.fail)  # na.fail: requisito de dredge

sel <- dredge(global, rank = "AIC")       # todos los submodelos, ordenados por AIC (respeta marginalidad)
head(sel, 3)                              # los cinco mejores
dredge(global, rank = "BIC") |> head(3)   # el BIC, más parsimonioso, suele elegir otro

# Mejor por AIC (exhaustivo) frente a la búsqueda paso a paso:
formula(get.models(sel, subset = 1)[[1]])                          # ganador de dredge
formula(MASS::stepAIC(global, direction = "both", trace = FALSE))  # ganador de stepAIC

# -----------------------------------------------------------------------------
# [fig-u12-dharma]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
sim <- DHARMa::simulateResiduals(fit_glm, plot = FALSE)
plot(sim)

# -----------------------------------------------------------------------------
# [u12-hosmer]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
performance::performance_hosmer(fit_glm, n_bins = 10)

# -----------------------------------------------------------------------------
# [fig-u12-calibracion]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
diag <- tibble(p_hat = fitted(fit_glm), y = glow$fractura01)

# --- Panel A: residuos por tramos (estilo binnedplot, en ggplot) ---
binned <- diag |>
  mutate(tramo = ntile(p_hat, 20)) |>
  group_by(tramo) |>
  summarise(p_med = mean(p_hat),
            r_med = mean(y - p_hat),
            se    = sqrt(mean(p_hat * (1 - p_hat)) / n()), .groups = "drop")

pA <- ggplot(binned, aes(p_med, r_med)) +
  geom_ribbon(aes(ymin = -2 * se, ymax = 2 * se), alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 1.8) +
  labs(x = "Probabilidad ajustada", y = "Residuo medio por tramo",
       title = "Residuos por tramos")

# --- Panel B: calibración por deciles ---
calib <- diag |>
  mutate(decil = ntile(p_hat, 10)) |>
  group_by(decil) |>
  summarise(pred = mean(p_hat), obs = mean(y), .groups = "drop")

pB <- ggplot(calib, aes(pred, obs)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +   # calibración perfecta
  geom_line() + geom_point(size = 1.8) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Riesgo predicho (media por decil)", y = "Frecuencia observada",
       title = "Calibración")

pA + pB        # patchwork: lado a lado

# -----------------------------------------------------------------------------
# [u12-separacion]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
glow |> count(smoke, raterisk, fracture) |>
  filter(smoke == "Yes", raterisk == "Less")        # celda con 0 fracturas

fit_sep <- glm(fracture ~ smoke * raterisk, binomial, glow)
tidy(fit_sep) |> filter(str_detect(term, "smoke"))  # coef enorme, EE descomunal

# -----------------------------------------------------------------------------
# [u12-firth]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
# install.packages("logistf")  # o brglm2::brglm_fit
# La verosimilitud penalizada de Firth devuelve estimaciones FINITAS e inferencia con sentido:
logistf::logistf(fracture ~ smoke * raterisk, data = glow)

# -----------------------------------------------------------------------------
# [u12-confusion]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
# --- A mano (para ver qué es cada cosa) ---
umbral <- 0.5
pred   <- factor(ifelse(fitted(fit_glm) >= umbral, "Yes", "No"),
                 levels = c("No", "Yes"))
cm <- table(Predicho = pred, Observado = glow$fracture)
cm

TP <- cm["Yes", "Yes"]; FP <- cm["Yes", "No"]
FN <- cm["No",  "Yes"]; TN <- cm["No",  "No"]
c(exactitud     = (TP + TN) / sum(cm),   # accuracy
  sensibilidad  = TP / (TP + FN),        # sensitivity / recall
  especificidad = TN / (TN + FP),        # specificity
  precision     = TP / (TP + FP)) |> round(3)   # precision / PPV

# -----------------------------------------------------------------------------
# [u12-confusion-yardstick]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
library(yardstick)
eval_df <- tibble(obs = glow$fracture, pred = pred)

conf_mat(eval_df, truth = obs, estimate = pred)        # misma matriz de confusión

metric_set(accuracy, sensitivity, specificity, precision, recall, f_meas)(
  eval_df, truth = obs, estimate = pred, event_level = "second")

# -----------------------------------------------------------------------------
# [fig-u12-roc]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
roc_obj <- pROC::roc(glow$fracture, fitted(fit_glm), quiet = TRUE)
pROC::auc(roc_obj)
pROC::ggroc(roc_obj) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed") +
  labs(x = "Especificidad", y = "Sensibilidad")

# -----------------------------------------------------------------------------
# [u12-umbral]  ·  2.2 El enlace: canónico y alternativas
# -----------------------------------------------------------------------------
pROC::coords(roc_obj, "best", best.method = "youden",
             ret = c("threshold", "sensitivity", "specificity"))
