library(googlesheets)
library(data.table)
library(DT)
library(flexdashboard)
library(shiny)
library(ggplot2)
library(plotly)
library(viridis)


ImportarDatosILE <- function(titulo.archivo, titulo.hoja, trae.nombres.originales = F) {
  gs.object <- gs_title(titulo.archivo)
  ile.data <- suppressWarnings(suppressMessages(gs_read(ss = gs.object, ws = titulo.hoja)))
  setDT(ile.data)
  ile.data[,c("fecha", "hora") := tstrsplit(`Marca temporal`, " ")]
  ile.data[, `:=`(`Marca temporal` = NULL, fecha = as.IDate(fecha), hora = as.ITime(hora))]
  nombres.originales <- names(ile.data)
  nombres.cortos <- c("id.participante", paste0("P", 1:12))
  setnames(ile.data, nombres.originales[-14:-15], nombres.cortos)
  ile.data <- melt(ile.data, id.vars = c("id.participante", "fecha", "hora"), value.name = "valor", variable.name = "pregunta")
  if(trae.nombres.originales) {
    return(list(ile.data = ile.data[], nombres.originales = nombres.originales))
  }
  return(ile.data[])
}

CalculaIndiceLateralidad <- function(ile.data) {
  ile.data[, valor.ajustado := valor - 3]
  ile.data[valor.ajustado == 0, valor.ajustado := 1]
  ile.data <- ile.data[, .(
    izquierda = .SD[(valor == 3 & valor.ajustado == 1) | (valor != 3 & valor.ajustado < 0), sum(abs(valor.ajustado))],
    derecha = .SD[(valor == 3 & valor.ajustado == 1) | (valor != 3 & valor.ajustado > 0), sum(abs(valor.ajustado))]
  ), .(id.participante, fecha, hora)]
  ile.data[, indice.lateralidad := round(((derecha - izquierda) / (derecha + izquierda)) * 100, 2)]
  ile.data[indice.lateralidad == 0, predominancia := "ninguna" ]
  ile.data[, predominancia := ifelse(izquierda > derecha, "izquierdo", "derecho")]
  ile.data[, id.participante := paste0(substr(id.participante, 1, 1), sprintf("%04d", as.numeric(substring(id.participante, 2))))]
  setorder(ile.data, id.participante)
  return(ile.data[])
}

