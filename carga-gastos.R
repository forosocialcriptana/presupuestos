library(tidyverse)
library(xlsx)

# PREPROCESAMIENTO DE DATOS
# Carga clasificación gastos
programas <- read.xlsx("datos/Gastos.xlsx", sheetName = "Programas")
programas$Código <- as.factor(programas$Código)
capitulos <- read.xlsx("datos/Gastos.xlsx", sheetName = "Capítulos")
capitulos$Código <- as.factor(capitulos$Código)
articulos <- read.xlsx("datos/Gastos.xlsx", sheetName = "Artículos")
articulos$Código <- as.factor(articulos$Código)
censo <- read.xlsx("datos/Gastos.xlsx", sheetName = "Censo")
censo <- censo |>  mutate(Año = as.factor(Año))

# Carga de gastos formato Emiliano
# Limpieza previa:
# - Fundir programa 16101 con 16100 Abastecimiento agua
# - Fundir programa 16200 con 16210 Recogida de residuos (consermancha)
# - Fundir programa 17100 con 17000 Medio ambiente
gastos <- read_csv("datos/gastos.csv") |>
  # Eliminar filas con sumas
  filter(!is.na(Economica)) |>
  mutate(
    Economica = as.character(Economica), 
    Programa = as.factor(Programa),
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
  # Eliminar los totales por capítulos
  filter(!is.na(Programa)) |>
  # Reemplazar NAs por ceros
  replace_na(list(Cantidad=0))

# Filtrar totales por programas y renombrar variables
gastos.programas <- gastos |>
  select(Año, Programa, Cantidad) |>
  rename(cod.programa = Programa) |>
  left_join(programas, by=c("cod.programa"="Código"))

# Eliminar totales por programas
gastos <- gastos |> filter(!is.na(Economica)) |>
  # Fusionar tabla de gastos con tabla de capítulos y artículos
  left_join(capitulos, by=c("cod.capitulo"="Código")) |>
  left_join(articulos, by=c("cod.articulo"="Código"))

# Carga de inversiones
inversiones <- read_csv("datos/inversiones.csv")

# Carga de cargos desde Excel 
cargos <- read_csv("datos/cargos.csv")
