#=========================================================
# PIRAMIDE BIRD
# Sistema Inteligente de Gestión del Riesgo Operacional
# Green Móvil
#=========================================================

library(shiny)
library(bs4Dash)
library(plotly)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(ggrepel)
library(readr)
library(stringr)
library(purrr)
library(DBI)
library(dotenv)
library(RPostgres)
library(bcrypt)

if (file.exists(".env")) {
  load_dot_env()
}
#==========================================================
# CONEXIÓN DWH
#==========================================================
conexion_dwh <- function() {
  
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
}

con <- conexion_dwh()

# Construir la consulta SQL con solo las columnas necesarias
query <- paste0("SELECT \"IdVehiculo\", \"Fecha\", \"FechaHoraLecturaDato\", \"IdRuta\", \"Hora\", \"Variable\", \"Tabla\", \"Conductor\"
                 FROM \"it\".\"FactTelemetriaEventos\";")

# Ejecutar la consulta en la base de datos
telemetria_op <- dbGetQuery(con, query)

# Construir la consulta SQL con solo las columnas necesarias
query2 <- paste0("SELECT \"Fecha\", \"Hora\", \"Ruta\", \"Codigo\", \"CodigoOperador\", \"TipoEvento\", \"Clasificacion\", \"Responsabilidad\"
                 FROM \"op\".\"FactAccidentalidad\";")

# Ejecutar la consulta en la base de datos
accidentes <- dbGetQuery(con, query2)

# Construir la consulta SQL con solo las columnas necesarias
query3 <- paste0("SELECT \"IdIco\", \"EstadoDp\", \"IdEmpresa\", \"TipoNovedad\", \"FechaNovedad\", \"Area\", \"NoSaeConductor\", \"Puntos\",\"Detalle\"
                 FROM \"op\".\"FactDetalleIco\";")

# Ejecutar la consulta en la base de datos
infracciones <- dbGetQuery(con, query3)

accidentes <- accidentes %>%
  mutate(
    HoraAccidente = hour(hms(Hora))
  )

telemetria_op <- telemetria_op %>%
  rename(
    CodigoOperador = Conductor
  )

telemetria_op <- telemetria_op %>%
  mutate(
    CodigoOperador = as.character(CodigoOperador)
  )

accidentes <- accidentes %>%
  mutate(
    CodigoOperador = as.character(CodigoOperador)
  )

fecha_min <- min(telemetria_op$Fecha, na.rm = TRUE)
fecha_max <- max(telemetria_op$Fecha, na.rm = TRUE)

accidentes <- accidentes %>%
  filter(
    Fecha >= fecha_min,
    Fecha <= fecha_max
  )

telemetria_op <- telemetria_op %>%
  mutate(
    Fecha = as.Date(Fecha)
  )

telemetria_op <- telemetria_op %>%
  
  filter(
    
    Tabla %in% c("ALA1", "ALA2", "ALA3") |
      
      (
        Tabla == "EV19" &
          Variable != "2"
      )
  )

accidentes <- accidentes %>%
  mutate(
    Fecha = as.Date(Fecha)
  )

fecha_min <- min(
  c(telemetria_op$Fecha, accidentes$Fecha),
  na.rm = TRUE
)

fecha_max <- max(
  c(telemetria_op$Fecha, accidentes$Fecha),
  na.rm = TRUE
)

telemetria_op <- telemetria_op %>%
  filter(
    !is.na(CodigoOperador),
    CodigoOperador != 0,
    CodigoOperador != "0",
    CodigoOperador != ""
  )

accidentes <- accidentes %>%
  filter(
    !is.na(CodigoOperador),
    CodigoOperador != 0,
    CodigoOperador != "0",
    CodigoOperador != ""
  )

infracciones <- infracciones %>%
  mutate(
    NoSaeConductor = as.character(NoSaeConductor)
  )

infracciones <- infracciones %>%
  mutate(
    FechaNovedad = as.Date(FechaNovedad)
  )

infracciones <- infracciones %>%
  filter(
    FechaNovedad >= fecha_min,
    FechaNovedad <= fecha_max
  )

infracciones <- infracciones %>%
  filter(
    EstadoDp != "Contestado Contundente",
    Area == "SEGURIDAD"
  ) %>%
  rename(
    Fecha = FechaNovedad,
    CodigoOperador = NoSaeConductor
  ) %>%
  mutate(
    Fecha = as.Date(Fecha),
    Empresa = case_when(
      IdEmpresa == 233 ~ "ZMOV",
      IdEmpresa == 231 ~ "ZMOIII",
      TRUE ~ NA_character_
    ),
    Tipo = case_when(
      Puntos == 10 ~ "Tipo_I",
      Puntos == 15 ~ "Tipo_II",
      Puntos == 30 ~ "Tipo_III",
      TRUE ~ NA_character_
    )
  )
#=========================================================
# COLORES CORPORATIVOS
#=========================================================

COLORES <- list(
  
  verde = "#27AE60",
  
  amarillo = "#F1C40F",
  
  naranja = "#F39C12",
  
  rojo = "#E74C3C",
  
  azul = "#3498DB",
  
  gris = "#7F8C8D"
  
)

#=========================================================
# CONFIGURACIÓN INICIAL DEL MODELO
#=========================================================

MODELO_DEFAULT <- list(
  
  telemetria_op = list(
    
    EV19 = 1,
    
    ALA1 = 3,
    
    ALA2 = 6,
    
    ALA3 = 10
    
  ),
  
  accidentes = list(
    
    Simple = 1,
    
    Lesionados = 3,
    
    Fatalidad = 18,
    
    Responsabilidad = 5
    
  ),
  
  infracciones = list(
    
    Tipo_I = 10,
    
    Tipo_II = 15,
    
    Tipo_III = 30
    
  ),
  
  percentiles = list(
    
    medio = 0.70,
    
    alto = 0.90
    
  ),
  
  top = 10
  
)

#=========================================================
# CARGA DE DATOS
#=========================================================
# En este punto NO modificamos nada.
# Seguiremos utilizando los dataframes que ya cargas
# actualmente en tu proyecto.
#=========================================================

# telemetria_op
# accidentes

#=========================================================
# PREPARACIÓN DE TELEMETRÍA
#=========================================================

telemetria_op <- telemetria_op %>%
  
  mutate(
    
    Fecha = as.Date(Fecha),
    
    Empresa = case_when(
      
      startsWith(as.character(IdVehiculo), "63") ~ "ZMOIII",
      
      startsWith(as.character(IdVehiculo), "67") ~ "ZMOV",
      
      TRUE ~ "Otra"
      
    )
    
  )

#=========================================================
# PREPARACIÓN DE ACCIDENTES
#=========================================================

accidentes <- accidentes %>%
  
  mutate(
    
    Fecha = as.Date(Fecha),
    Empresa = case_when(
      
      startsWith(as.character(CodigoOperador), "63") ~ "ZMOIII",
      
      startsWith(as.character(CodigoOperador), "67") ~ "ZMOV",
      
      TRUE ~ "Otra"
      
    )
  )

accidentes <- accidentes %>%
  dplyr::mutate(
    Responsabilidad = dplyr::case_when(
      
      trimws(as.character(Responsabilidad)) == "Greenmovil" ~
        "Greenmovil",
      
      TRUE ~
        "Sin responsabilidad"
    )
  )

#=========================================================
# VALORES PARA FILTROS
#=========================================================

LISTA_EMPRESAS <- sort(unique(telemetria_op$Empresa))

LISTA_RUTAS <- sort(unique(telemetria_op$IdRuta))

LISTA_TABLAS <- sort(unique(telemetria_op$Tabla))

LISTA_VARIABLES <- sort(unique(telemetria_op$Variable))

LISTA_NIVELES <- c(
  
  "Todos",
  
  "Bajo",
  
  "Medio",
  
  "Alto"
  
)

LISTA_TOP <- c(
  
  5,
  
  10,
  
  15,
  
  20,
  
  30,
  
  50,
  
  100,
  
  "Todos"
)

#=========================================================
# FIN GLOBAL
#=========================================================