# Funciones para los gráficos de los presupuestos
library(plotly)
library(sunburstR)

# Configuración gráficos
xaxis <- list(showgrid = F, zeroline = F, showticklabels = F)
yaxis <- list(showgrid = F, zeroline = F, showticklabels = F)
legend <- list(font = list(color = "steelblue"))

gasto.total <- function(gastos, año){
  total <- gastos |> filter(Año==año) |> summarise(Total = sum(Cantidad))
  return(paste(format(total$Total, big.mark="."), '€'))
}

gasto.habitante <- function(censo, gastos, año){
  total <- gastos |> filter(Año==año) |> summarise(Total = sum(Cantidad))
  poblacion <- censo |> filter(Año==año)
  return(paste(format(round(total$Total / poblacion$Población, 2), decimal.mark = ","),  '€'))
}

plot.gastos.programas <- function(gastos.programas, año){
  df <- gastos.programas |>
    filter(Año == año) |>
    group_by(Programa) |>
    summarise(Cantidad=sum(Cantidad)) |> 
    arrange(desc(Cantidad))
  p <- plot_ly(df, labels = ~Programa, values = ~Cantidad, type = 'pie', textposition = 'none', hoverinfo="text+percent", 
               text = ~paste(Programa, '</br>Cantidad: ', format(Cantidad,decimal.mark=",", big.mark="."), "€"), sort=T, direction="clockwise") |>
  layout(xaxis = xaxis, yaxis = yaxis, legend = legend, plot_bgcolor  = "transparent",
           paper_bgcolor = "transparent")
  return(p)
}

plot.variacion.gastos.programas <- function(gastos.programas, año, limite = 10000){
  gastos.programas |> 
  filter(Año %in% c(año, año-1)) |>
  group_by(Programa, Año) |>
  summarise(Cantidad = sum(Cantidad)) |>
  pivot_wider(names_from = Año, values_from = Cantidad) |>
  rename(actual = as.character(año), anterior = as.character(año-1)) |>
  mutate(Variación = actual-anterior) |>
  mutate(Porcentaje = Variación / anterior * 100, positive = Variación >= 0) |>
  # Programas con una mariación mayor 10000€
  filter(abs(Variación) >= limite) |>
  plot_ly(y = ~Programa, x = ~Variación, color = ~positive, type = "bar", orientation = 'h', hoverinfo = 'text', 
             hovertext = ~paste('Programa: ', Programa, '<br>Variación: ', format(Variación, decimal.mark=",", big.mark="."), "€  (", format(round(Porcentaje,2), decimal.mark=",", nsmall = 2), "%)")) |>
  layout(yaxis = list(categoryorder = "total descending", tickfont = list(color = "steelblue"), titlefont = list(color = "steelblue")), showlegend = FALSE, xaxis = list(showgrid=T, zeroline=T, nticks=20, showline=F, title="Variación con respecto al año anterior", titlefont = list(color = "steelblue"), tickprefix = '€', tickformat=",.2r", hoverformat = '.2f', tickfont = list(color = "steelblue")), plot_bgcolor  = "transparent", paper_bgcolor = "transparent")
}

plot.gastos.capitulos <- function(gastos, año){
  gastos.capitulos <- gastos |> filter(Año==año) |> group_by(Capítulo) |> summarise(Cantidad=sum(Cantidad))
  p <- plot_ly(gastos.capitulos, labels = ~Capítulo, values = ~Cantidad, type = 'pie', textposition = 'none', hoverinfo="text+percent", 
               text = ~paste(Capítulo, '</br>Cantidad: ', format(Cantidad,decimal.mark=",", big.mark="."), "€"), sort=T, direction="clockwise") |>
  layout(xaxis = xaxis, yaxis = yaxis, legend = legend, plot_bgcolor  = "transparent",
           paper_bgcolor = "transparent")
  return(p)
}

plot.inversiones <- function(inversiones, año){
  inversiones |> 
  filter(Año == año) |>
  mutate(Porcentaje = Cantidad / sum(Cantidad) * 100) |>
  plot_ly(y = ~Descripción, x = ~Cantidad, type = "bar", orientation = 'h', hoverinfo = 'text', 
             hovertext = ~paste('Inversión: ', Descripción, '<br>Cantidad: ', format(Cantidad, decimal.mark=",", big.mark="."), "€  (", format(round(Porcentaje,2), decimal.mark=",", nsmall = 2), "%)")) |>
  layout(yaxis = list(categoryorder = "total descending", tickfont = list(color = "steelblue"), titlefont = list(color = "steelblue")), showlegend = FALSE, xaxis = list(showgrid=T, zeroline=T, nticks=20, showline=F, tickprefix = '€', tickformat=",.2r", hoverformat = '.2f', tickfont = list(color = "steelblue")), plot_bgcolor  = "transparent", paper_bgcolor = "transparent")
}

plot.ingresos.capitulos <- function(ingresos, año){
  ingresos.capitulos <- ingresos |> filter(Año==año) |> group_by(Capítulo) |> summarise(Cantidad=sum(Cantidad))
  p <- plot_ly(ingresos.capitulos, labels = ~Capítulo, values = ~Cantidad, type = 'pie', textposition = 'none', hoverinfo="text+percent", 
               text = ~paste(Capítulo, '</br>Cantidad: ', format(Cantidad,decimal.mark=",", big.mark="."), "€"), sort=T, direction="clockwise") |>
    layout(xaxis = xaxis, yaxis = yaxis, legend = legend, plot_bgcolor  = "transparent",
           paper_bgcolor = "transparent")
  return(p)
}

plot.ingresos.articulos <- function(ingresos, año){
  ingresos.articulos <- ingresos |> filter(Año==año) |> group_by(Artículo) |> summarise(Cantidad=sum(Cantidad))
  p <- plot_ly(ingresos.articulos, labels = ~Artículo, values = ~Cantidad, type = 'pie', textposition = 'none', hoverinfo="text+percent", 
               text = ~paste(Artículo, '</br>Cantidad: ', format(Cantidad,decimal.mark=",", big.mark="."), "€"), sort=T, direction="clockwise") |>
    layout(xaxis = xaxis, yaxis = yaxis, legend = legend, plot_bgcolor  = "transparent",
           paper_bgcolor = "transparent")
  return(p)
}

plot.ingresos.conceptos <- function(ingresos, año){
  ingresos.conceptos <- ingresos |> filter(Año==año) |> group_by(Concepto) |> summarise(Cantidad=sum(Cantidad))
  p <- plot_ly(ingresos.conceptos, labels = ~Concepto, values = ~Cantidad, type = 'pie', textposition = 'none', hoverinfo="text+percent", 
               text = ~paste(Concepto, '</br>Cantidad: ', format(Cantidad,decimal.mark=",", big.mark="."), "€"), sort=T, direction="clockwise") |>
    layout(xaxis = xaxis, yaxis = yaxis, legend = legend, plot_bgcolor  = "transparent",
           paper_bgcolor = "transparent")
  return(p)
}

plot.variacion.ingresos.conceptos <- function(ingresos, año, limite = 10000){
  ingresos |> 
  filter(Año %in% c(año, año-1)) |>
  pivot_wider(names_from = Año, values_from = Cantidad) |>
  rename(actual = as.character(año), anterior = as.character(año-1)) |>
  group_by(Concepto) |>
  summarise(actual = sum(actual), anterior = sum(anterior)) |>
  mutate(Variación = actual-anterior) |>
  mutate(Porcentaje = Variación / anterior * 100, positive = Variación >= 0) |>
  # Programas con una mariación mayor 10000€
  filter(abs(Variación) >= limite) |>
  plot_ly(y = ~Concepto, x = ~Variación, color = ~positive, type = "bar", orientation = 'h', hoverinfo = 'text', 
             hovertext = ~paste('Concepto: ', Concepto, '<br>Variación: ', format(Variación, decimal.mark=",", big.mark="."), "€  (", format(round(Porcentaje,2), decimal.mark=",", nsmall = 2), "%)")) |>
  layout(yaxis = list(categoryorder = "total descending", tickfont = list(color = "steelblue"), titlefont = list(color = "steelblue")), showlegend = FALSE, xaxis = list(showgrid=T, zeroline=T, nticks=20, showline=F, title="Variación con respecto al año anterior", titlefont = list(color = "steelblue"), tickprefix = '€', tickformat=",.2r", hoverformat = '.2f', tickfont = list(color = "steelblue")), plot_bgcolor  = "transparent", paper_bgcolor = "transparent")
}
