# Paquetes de R que necesitan los cuadernos del curso (casos 1, 2 y 3).
# Ejecutar una vez, antes del primer script o render. Sin descargar nada:
#   source("https://raw.githubusercontent.com/asunmayoral/GLM/master/_scripts/instalar_paquetes.R")
# o, desde la raíz del proyecto:
#   source("_scripts/instalar_paquetes.R")
# Instala solo los que faltan; no actualiza los que ya están.

paquetes <- c(
  # manipulación, gráficos y documento
  "tidyverse", "patchwork", "GGally", "scales", "see", "knitr", "rmarkdown", "sessioninfo",
  # datos
  "aplore3",
  # GLM: ajuste, interpretación e inferencia
  "broom", "broom.mixed", "marginaleffects", "emmeans", "car", "lmtest", "MASS", "MuMIn", "arm",
  # diagnóstico, bondad de ajuste y evaluación de clasificadores
  "DHARMa", "performance", "pROC", "generalhoslem", "sure", "Hmisc", "rsample", "yardstick",
  # separación y respuestas binarias, ordinales y nominales
  "logistf", "ordinal", "brant", "nnet",
  # efectos mixtos
  "lme4", "glmmTMB",
  # conteos
  "pscl", "AER", "vcd", "vcdExtra",
  # regularización
  "glmnet",
  # supervivencia
  "survival", "survminer", "flexsurv"
)

faltan <- setdiff(paquetes, rownames(installed.packages()))
message(length(paquetes) - length(faltan), " de ", length(paquetes), " paquetes ya instalados.")

if (length(faltan) > 0) {
  message("Instalando: ", paste(faltan, collapse = ", "))
  install.packages(faltan)
}

# Un paquete puede fallar (sin conexión, retirado de CRAN, falta de compiladores):
# se comprueba de nuevo para que el fallo no pase inadvertido.
siguen <- setdiff(paquetes, rownames(installed.packages()))
if (length(siguen) > 0) {
  warning("No se han podido instalar: ", paste(siguen, collapse = ", "), call. = FALSE)
} else {
  message("Todo listo: los ", length(paquetes), " paquetes están instalados.")
}
