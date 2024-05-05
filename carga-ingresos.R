library(tidyverse)
library(xlsx)

# PREPROCESAMIENTO DE DATOS
# Carga clasificación ingresos
capitulos <- read.xlsx("datos/Ingresos.xlsx", sheetName = "Capítulos", colClasses=rep("character",2))
articulos <- read.xlsx("datos/Ingresos.xlsx", sheetName = "Artículos", colClasses=rep("character",2))
conceptos <- read.xlsx("datos/Ingresos.xlsx", sheetName = "Conceptos", colClasses=rep("character",2))

# Carga de ingresos formato Emiliano
ingresos <- read_csv("datos/ingresos.csv") |>
  mutate(Economica = as.character(Economica),
         cod.capitulo = as.factor(substr(Economica, 1, 1)),
         cod.articulo = as.factor(substr(Economica, 1, 2)),
         cod.concepto = as.factor(substr(Economica, 1, 3)),
         cod.subconcepto = as.factor(substr(Economica, 1, 5))) |>
  # Convertir columnas de años en filas
  pivot_longer(
    cols = starts_with('2'),
    names_to = "Año",
    values_to = "Cantidad") |>
  mutate(Año = as.factor(Año)) |>
  # Eliminar totales por capítulo
  filter(!is.na(Economica)) |>
  # Reemplazar NAs por ceros
  replace_na(list(Cantidad=0)) |>
  # Fusionar tabla de gastos con tabla de programas
  left_join(capitulos, by=c("cod.capitulo"="Código")) |>
  left_join(articulos, by=c("cod.articulo"="Código")) |>
  left_join(conceptos, by=c("cod.concepto"="Código"))
