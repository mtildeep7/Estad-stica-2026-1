# =========================================================
# ANALISIS ESTADISTICO - ETOLOGIA CANINA
# Tema: Factores asociados a conductas relacionadas con separacion en cachorros
# Base: base_etologia_conductas_separacion_canina.xlsx
# Objetivo: ejecutar descriptivos, graficos, pruebas bivariadas, OR y regresion logistica
# =========================================================

# ---------------------------------------------------------
# 1. INSTALAR Y CARGAR PAQUETES
# ---------------------------------------------------------
paquetes <- c(
  "tidyverse", "readxl", "janitor", "gtsummary", "broom",
  "writexl", "flextable", "officer"
)

instalar_si_falta <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(instalar_si_falta) > 0){
  install.packages(instalar_si_falta)
}

library(tidyverse)
library(readxl)
library(janitor)
library(gtsummary)
library(broom)
library(writexl)
library(flextable)
library(officer)

# ---------------------------------------------------------
# 2. IMPORTAR LA BASE DE DATOS
# ---------------------------------------------------------
# Recomendacion: colocar este script y el Excel en una misma carpeta.
# Luego abrir RStudio desde esa carpeta o usar setwd("ruta/de/tu/carpeta").

archivo <- "base_etologia_conductas_separacion_canina.xlsx"

base <- read_excel(archivo, sheet = "Base_datos") %>%
  clean_names()

# Revisar estructura inicial
str(base)
glimpse(base)

# Crear carpetas para guardar resultados
if(!dir.exists("resultados")) dir.create("resultados")
if(!dir.exists("resultados/graficos")) dir.create("resultados/graficos", recursive = TRUE)
if(!dir.exists("resultados/tablas")) dir.create("resultados/tablas", recursive = TRUE)

# ---------------------------------------------------------
# 3. PREPARACION DE VARIABLES
# ---------------------------------------------------------
# Variable desenlace principal:
# conducta_relacionada_separacion = presencia o ausencia de conductas relacionadas con separacion.

base <- base %>%
  mutate(
    conducta_relacionada_separacion = factor(conducta_relacionada_separacion, levels = c("No", "Sí")),
    srb_bin = if_else(conducta_relacionada_separacion == "Sí", 1, 0)
  ) %>%
  mutate(across(where(is.character), as.factor))

# Verificar si hay datos perdidos
colSums(is.na(base))

# ---------------------------------------------------------
# 4. DESCRIPCION GENERAL DE LA BASE
# ---------------------------------------------------------
# Esta tabla resume las caracteristicas generales de los cachorros.

vars_explicativas <- c(
  "sexo", "tipo_raza", "origen", "edad_adquisicion", "edad_propietario",
  "experiencia_propietario", "sueno_nocturno_16sem", "espacio_cerrado_16sem",
  "progreso_entrenamiento_higienico_16sem", "premio_entrenamiento_16sem",
  "respuesta_mala_conducta_sin_atencion_6m", "respuesta_mala_conducta_relajado_6m",
  "respuesta_al_retorno_6m", "max_horas_solo_6m"
)

vars_conductas <- c(
  "camina_de_un_lado_a_otro", "lloriquea_gime", "gira_en_circulos",
  "destruye_objetos", "orina_inadecuada", "conducta_relacionada_separacion"
)

tabla_descriptiva <- base %>%
  select(all_of(vars_explicativas), all_of(vars_conductas)) %>%
  tbl_summary(
    missing = "no",
    statistic = all_categorical() ~ "{n} ({p}%)"
  ) %>%
  bold_labels()

tabla_descriptiva

# Guardar tabla descriptiva en Word
as_flex_table(tabla_descriptiva) %>%
  save_as_docx(path = "resultados/tablas/tabla_1_descriptiva_general.docx")

# ---------------------------------------------------------
# 5. PREVALENCIA DEL DESENLACE PRINCIPAL
# ---------------------------------------------------------
# Calcula el porcentaje de cachorros con conducta relacionada con separacion.

prevalencia_srb <- base %>%
  tabyl(conducta_relacionada_separacion) %>%
  adorn_pct_formatting(digits = 1)

prevalencia_srb
write_xlsx(prevalencia_srb, "resultados/tablas/prevalencia_srb.xlsx")

# Grafico 1: prevalencia de conducta relacionada con separacion

grafico_prevalencia <- ggplot(base, aes(x = conducta_relacionada_separacion)) +
  geom_bar() +
  labs(
    title = "Frecuencia de conductas relacionadas con separación",
    x = "Conducta relacionada con separación",
    y = "Número de cachorros"
  ) +
  theme_minimal()

grafico_prevalencia
ggsave("resultados/graficos/grafico_1_prevalencia_srb.png", grafico_prevalencia, width = 7, height = 5, dpi = 300)

# ---------------------------------------------------------
# 6. FRECUENCIA DE CONDUCTAS ESPECIFICAS
# ---------------------------------------------------------
# Estas variables representan conductas observadas cuando el cachorro queda solo.

conductas_largas <- base %>%
  select(all_of(vars_conductas[-length(vars_conductas)])) %>%
  pivot_longer(cols = everything(), names_to = "conducta", values_to = "respuesta") %>%
  filter(respuesta == "Sí") %>%
  count(conducta, sort = TRUE) %>%
  mutate(porcentaje = round(n / nrow(base) * 100, 1))

conductas_largas
write_xlsx(conductas_largas, "resultados/tablas/frecuencia_conductas_especificas.xlsx")

# Grafico 2: conductas especificas mas frecuentes

grafico_conductas <- ggplot(conductas_largas, aes(x = reorder(conducta, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Conductas específicas reportadas cuando el cachorro queda solo",
    x = "Conducta",
    y = "Número de cachorros"
  ) +
  theme_minimal()

grafico_conductas
ggsave("resultados/graficos/grafico_2_conductas_especificas.png", grafico_conductas, width = 8, height = 5, dpi = 300)

# ---------------------------------------------------------
# 7. ANALISIS BIVARIADO: TABLAS CRUZADAS Y CHI-CUADRADO
# ---------------------------------------------------------
# Se evalua si cada factor se asocia con la presencia de conducta relacionada con separacion.
# Para interpretar:
# p < 0.05: hay evidencia estadistica de asociacion.
# p >= 0.05: no hay evidencia suficiente de asociacion.

analisis_chi <- function(variable){
  tabla <- table(base[[variable]], base$conducta_relacionada_separacion)
  prueba <- suppressWarnings(chisq.test(tabla))

  tibble(
    variable = variable,
    p_valor = prueba$p.value,
    interpretacion = if_else(prueba$p.value < 0.05, "Asociacion estadisticamente significativa", "No significativa")
  )
}

resultados_chi <- map_dfr(vars_explicativas, analisis_chi) %>%
  arrange(p_valor)

resultados_chi
write_xlsx(resultados_chi, "resultados/tablas/resultados_chi_cuadrado.xlsx")

# Tabla cruzada principal con porcentajes por fila
# Ejemplo: sueño nocturno vs conducta relacionada con separacion

tabla_sueno <- base %>%
  tabyl(sueno_nocturno_16sem, conducta_relacionada_separacion) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front")

tabla_sueno
write_xlsx(tabla_sueno, "resultados/tablas/tabla_sueno_vs_srb.xlsx")

# ---------------------------------------------------------
# 8. TABLA COMPARATIVA SEGUN EL DESENLACE
# ---------------------------------------------------------
# Esta tabla compara perros con y sin conducta relacionada con separacion.
# Es una de las tablas mas importantes para el informe.

tabla_comparativa <- base %>%
  select(conducta_relacionada_separacion, all_of(vars_explicativas)) %>%
  tbl_summary(
    by = conducta_relacionada_separacion,
    missing = "no",
    statistic = all_categorical() ~ "{n} ({p}%)"
  ) %>%
  add_p(test = all_categorical() ~ "chisq.test") %>%
  bold_labels()

tabla_comparativa

as_flex_table(tabla_comparativa) %>%
  save_as_docx(path = "resultados/tablas/tabla_2_comparativa_según_srb.docx")

# ---------------------------------------------------------
# 9. GRAFICOS BIVARIADOS
# ---------------------------------------------------------
# Se crean graficos por factores importantes del articulo.

crear_grafico_bivariado <- function(variable, titulo){
  g <- ggplot(base, aes(x = .data[[variable]], fill = conducta_relacionada_separacion)) +
    geom_bar(position = "fill") +
    labs(
      title = titulo,
      x = variable,
      y = "Proporción",
      fill = "Conducta relacionada\ncon separación"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  return(g)
}

g1 <- crear_grafico_bivariado("sueno_nocturno_16sem", "Sueño nocturno a las 16 semanas y SRB")
g2 <- crear_grafico_bivariado("espacio_cerrado_16sem", "Uso de espacio cerrado a las 16 semanas y SRB")
g3 <- crear_grafico_bivariado("progreso_entrenamiento_higienico_16sem", "Entrenamiento higiénico y SRB")
g4 <- crear_grafico_bivariado("respuesta_al_retorno_6m", "Respuesta del propietario al retorno y SRB")
g5 <- crear_grafico_bivariado("respuesta_mala_conducta_relajado_6m", "Respuesta ante mala conducta al relajarse y SRB")

g1; g2; g3; g4; g5

ggsave("resultados/graficos/grafico_3_sueno_vs_srb.png", g1, width = 8, height = 5, dpi = 300)
ggsave("resultados/graficos/grafico_4_espacio_cerrado_vs_srb.png", g2, width = 8, height = 5, dpi = 300)
ggsave("resultados/graficos/grafico_5_entrenamiento_higienico_vs_srb.png", g3, width = 8, height = 5, dpi = 300)
ggsave("resultados/graficos/grafico_6_retorno_vs_srb.png", g4, width = 8, height = 5, dpi = 300)
ggsave("resultados/graficos/grafico_7_respuesta_relajado_vs_srb.png", g5, width = 8, height = 5, dpi = 300)

# ---------------------------------------------------------
# 10. ODDS RATIO CRUDO MEDIANTE REGRESION LOGISTICA SIMPLE
# ---------------------------------------------------------
# Cada variable se analiza por separado.
# OR > 1: mayor odds/probabilidad relativa de presentar SRB.
# OR < 1: menor odds/probabilidad relativa de presentar SRB, posible factor protector.
# El intervalo de confianza no debe incluir 1 para considerar asociacion clara.

or_crudo <- function(variable){
  formula_modelo <- as.formula(paste("srb_bin ~", variable))
  modelo <- glm(formula_modelo, data = base, family = binomial)

  tidy(modelo, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(variable = variable) %>%
    select(variable, term, estimate, conf.low, conf.high, p.value) %>%
    rename(
      OR = estimate,
      IC95_inf = conf.low,
      IC95_sup = conf.high,
      p_valor = p.value
    )
}

resultados_or_crudo <- map_dfr(vars_explicativas, or_crudo) %>%
  arrange(p_valor)

resultados_or_crudo
write_xlsx(resultados_or_crudo, "resultados/tablas/odds_ratio_crudo.xlsx")

# ---------------------------------------------------------
# 11. REGRESION LOGISTICA MULTIVARIABLE
# ---------------------------------------------------------
# Se incluyen variables relevantes segun el articulo y el analisis bivariado.
# No se recomienda meter demasiadas variables porque la base tiene 145 registros.

modelo_final <- glm(
  srb_bin ~ sueno_nocturno_16sem +
    espacio_cerrado_16sem +
    progreso_entrenamiento_higienico_16sem +
    premio_entrenamiento_16sem +
    respuesta_mala_conducta_sin_atencion_6m +
    respuesta_mala_conducta_relajado_6m +
    respuesta_al_retorno_6m +
    edad_propietario,
  data = base,
  family = binomial
)

summary(modelo_final)

or_ajustado <- tidy(modelo_final, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  rename(
    OR_ajustado = estimate,
    IC95_inf = conf.low,
    IC95_sup = conf.high,
    p_valor = p.value
  ) %>%
  arrange(p_valor)

or_ajustado
write_xlsx(or_ajustado, "resultados/tablas/odds_ratio_ajustado_modelo_final.xlsx")

# Tabla bonita del modelo final

tabla_modelo_final <- tbl_regression(
  modelo_final,
  exponentiate = TRUE,
  label = list(
    sueno_nocturno_16sem ~ "Sueño nocturno a ≤16 semanas",
    espacio_cerrado_16sem ~ "Uso de espacio cerrado a ≤16 semanas",
    progreso_entrenamiento_higienico_16sem ~ "Progreso de entrenamiento higiénico a ≤16 semanas",
    premio_entrenamiento_16sem ~ "Premio usado en entrenamiento a ≤16 semanas",
    respuesta_mala_conducta_sin_atencion_6m ~ "Respuesta ante mala conducta sin atención a 6 meses",
    respuesta_mala_conducta_relajado_6m ~ "Respuesta ante mala conducta al relajarse a 6 meses",
    respuesta_al_retorno_6m ~ "Respuesta del propietario al retorno a 6 meses",
    edad_propietario ~ "Edad del propietario"
  )
) %>%
  bold_labels()

tabla_modelo_final

as_flex_table(tabla_modelo_final) %>%
  save_as_docx(path = "resultados/tablas/tabla_3_modelo_logistico_final.docx")

# ---------------------------------------------------------
# 12. GRAFICO DE ODDS RATIO AJUSTADO
# ---------------------------------------------------------
# Se grafica el modelo final.
# La linea vertical en OR = 1 indica ausencia de asociacion.

or_grafico <- or_ajustado %>%
  mutate(term = fct_reorder(term, OR_ajustado))

grafico_or <- ggplot(or_grafico, aes(x = OR_ajustado, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = IC95_inf, xmax = IC95_sup), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_log10() +
  labs(
    title = "Odds Ratio ajustado para conductas relacionadas con separación",
    x = "OR ajustado, escala logarítmica",
    y = "Variable"
  ) +
  theme_minimal()

grafico_or
ggsave("resultados/graficos/grafico_8_or_ajustado.png", grafico_or, width = 9, height = 6, dpi = 300)

# ---------------------------------------------------------
# 13. EXPORTAR UNA BASE RESUMIDA CON RESULTADOS PRINCIPALES
# ---------------------------------------------------------
# Este archivo junta los principales resultados para que el estudiante pueda usarlos en su informe.

write_xlsx(
  list(
    prevalencia_srb = prevalencia_srb,
    frecuencia_conductas = conductas_largas,
    chi_cuadrado = resultados_chi,
    or_crudo = resultados_or_crudo,
    or_ajustado = or_ajustado
  ),
  "resultados/resultados_completos_etologia_srb.xlsx"
)

# ---------------------------------------------------------
# 14. GUIA BREVE PARA REDACTAR LOS RESULTADOS
# ---------------------------------------------------------
# En el informe, los estudiantes deberian responder:
#
# 1. ¿Cuantos cachorros fueron evaluados?
# 2. ¿Que porcentaje presento conducta relacionada con separacion?
# 3. ¿Cuales fueron las conductas especificas mas frecuentes?
# 4. ¿Que variables se asociaron significativamente en el analisis bivariado?
# 5. ¿Que factores aumentaron la odds de SRB en el modelo logistico?
# 6. ¿Que factores tuvieron OR menor de 1, es decir, posible efecto protector?
# 7. ¿Los resultados coinciden con el articulo cientifico revisado?
# 8. ¿Que recomendaciones practicas se podrian plantear para propietarios de cachorros?

# Mensaje final
cat("\nAnalisis finalizado. Revisa la carpeta 'resultados' para tablas y graficos.\n")
