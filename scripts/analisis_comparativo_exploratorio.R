# Analisis comparativo sobre violencia basada en genero
# Equipo: Nueva Generacion
# DataJam 2026
#
# Este script organiza una version EXPLORATORIA del analisis.
# Importante:
# - La estructura del codigo esta pensada para visualizacion y tabulacion.
# - Algunos valores incluidos en este script fueron usados solo como ejemplo
#   para explorar la logica del analisis.
# - Para la entrega final, este archivo debe alimentarse con fuentes reales
#   y verificables provenientes del Portal de Datos Abiertos de Bogota,
#   OMEG, SaluData u otras fuentes oficiales reportadas por el equipo.

# -----------------------------
# 1. Librerias
# -----------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# -----------------------------
# 2. Base exploratoria
# -----------------------------
datos <- data.frame(
  localidad = c("Ciudad Bolívar", "Teusaquillo"),
  pobreza_multidimensional = c(10.9, 3.8),
  informalidad_femenina = c(53.6, 30.0),
  tasa_VIF = c(952.8, 541.9),
  feminicidios_pct = c(20, 5),
  sin_proteccion = c(58, 30)
)

# Vista inicial
print(datos)

# -----------------------------
# 3. Tabla resumen
# -----------------------------
tabla_resumen <- datos %>%
  mutate(
    brecha_pobreza = pobreza_multidimensional - min(pobreza_multidimensional),
    brecha_VIF = tasa_VIF - min(tasa_VIF)
  )

print(tabla_resumen)

# Guardar tabla resumen
write.csv(tabla_resumen, "outputs/tabla_resumen_exploratoria.csv", row.names = FALSE)

# -----------------------------
# 4. Grafico 1: brechas estructurales
# -----------------------------
datos_largos <- datos %>%
  select(localidad, pobreza_multidimensional, informalidad_femenina) %>%
  pivot_longer(-localidad, names_to = "variable", values_to = "valor")

g1 <- ggplot(datos_largos, aes(x = localidad, y = valor, fill = variable)) +
  geom_col(position = "dodge") +
  labs(
    title = "Brechas estructurales por localidad",
    x = "Localidad",
    y = "Porcentaje",
    fill = "Variable"
  ) +
  theme_minimal()

ggsave("outputs/grafico_brechas_estructurales.png", g1, width = 8, height = 5, dpi = 300)

# -----------------------------
# 5. Grafico 2: VIF vs feminicidio
# -----------------------------
g2 <- ggplot(datos, aes(x = tasa_VIF, y = feminicidios_pct, label = localidad)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.8) +
  labs(
    title = "Relacion entre violencia intrafamiliar y feminicidio",
    x = "Tasa de violencia intrafamiliar (por 100.000)",
    y = "Porcentaje de feminicidios"
  ) +
  theme_minimal()

ggsave("outputs/grafico_vif_feminicidio.png", g2, width = 8, height = 5, dpi = 300)

# -----------------------------
# 6. Grafico 3: tipologia de violencia
# -----------------------------
tipos_violencia <- data.frame(
  tipo = c("Física", "Psicológica", "Económica"),
  porcentaje = c(70, 65, 35)
)

g3 <- ggplot(tipos_violencia, aes(x = tipo, y = porcentaje)) +
  geom_col() +
  labs(
    title = "Tipología de violencia intrafamiliar",
    x = "Tipo de violencia",
    y = "Porcentaje"
  ) +
  theme_minimal()

ggsave("outputs/grafico_tipologia_violencia.png", g3, width = 8, height = 5, dpi = 300)

# -----------------------------
# 7. Grafico 4: falta de proteccion
# -----------------------------
g4 <- ggplot(datos, aes(x = localidad, y = sin_proteccion)) +
  geom_col() +
  labs(
    title = "Victimas sin medidas de proteccion",
    x = "Localidad",
    y = "Porcentaje"
  ) +
  theme_minimal()

ggsave("outputs/grafico_sin_proteccion.png", g4, width = 8, height = 5, dpi = 300)

# -----------------------------
# 8. Indice exploratorio de vulnerabilidad
# -----------------------------
datos <- datos %>%
  mutate(
    indice_vulnerabilidad = (informalidad_femenina + pobreza_multidimensional + sin_proteccion) / 3
  )

print(datos)

write.csv(datos, "outputs/base_con_indice_exploratorio.csv", row.names = FALSE)

g5 <- ggplot(datos, aes(x = localidad, y = indice_vulnerabilidad)) +
  geom_col() +
  labs(
    title = "Indice exploratorio de vulnerabilidad",
    x = "Localidad",
    y = "Indice"
  ) +
  theme_minimal()

ggsave("outputs/grafico_indice_vulnerabilidad.png", g5, width = 8, height = 5, dpi = 300)

# -----------------------------
# 9. Mensaje final
# -----------------------------
cat("Script ejecutado correctamente.\n")
cat("Se generaron tablas y graficos exploratorios en la carpeta outputs/.\n")
cat("Recuerde reemplazar los valores exploratorios por datos oficiales antes de la entrega final.\n")
