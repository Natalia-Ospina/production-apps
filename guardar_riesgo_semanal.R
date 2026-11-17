#=========================================================
# PIRÁMIDE BIRD - CORTE SEMANAL AUTOMÁTICO
#=========================================================
# Calcula el riesgo de toda la planta para la semana anterior
# completa (lunes a domingo) con los pesos y percentiles de
# MODELO_DEFAULT (global.R) y lo guarda en
# op.FactRiesgoOperadores con Usuario = "AUTOMATICO".
#
# Se programa con cron para ejecutarse cada lunes.
# Si la semana ya fue guardada, no la vuelve a guardar.
#=========================================================

setwd(Sys.getenv("APP_DIR", "/srv/shiny-server"))

source("requirements.R")

invisible(
  lapply(
    packages,
    library,
    character.only = TRUE
  )
)

source("global.R")
source("funciones.R")

USUARIO_AUTOMATICO <- "AUTOMATICO"

log_msg <- function(...){
  cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "America/Bogota"),
    "-",
    ...,
    "\n"
  )
}

#---------------------------------------------------------
# PERIODO: SEMANA ANTERIOR (LUNES A DOMINGO)
#---------------------------------------------------------

hoy <- as.Date(format(Sys.time(), tz = "America/Bogota"))

lunes_actual <- lubridate::floor_date(hoy, "week", week_start = 1)

fecha_inicio <- lunes_actual - 7
fecha_fin    <- lunes_actual - 1

log_msg("Corte semanal del", format(fecha_inicio), "al", format(fecha_fin))

#---------------------------------------------------------
# EVITAR DUPLICADOS
#---------------------------------------------------------

if(existe_corte(fecha_inicio, fecha_fin, USUARIO_AUTOMATICO)){
  log_msg("Ya existe un corte automático para esta semana. No se guarda de nuevo.")
  quit(save = "no", status = 0)
}

#---------------------------------------------------------
# CÁLCULO Y GUARDADO
#---------------------------------------------------------

resultado <- tryCatch({
  
  riesgo_semana <- calcular_riesgo(
    telemetria_op = telemetria_op %>%
      dplyr::filter(Fecha >= fecha_inicio, Fecha <= fecha_fin),
    accidentes = accidentes %>%
      dplyr::filter(Fecha >= fecha_inicio, Fecha <= fecha_fin),
    infracciones = infracciones %>%
      dplyr::filter(Fecha >= fecha_inicio, Fecha <= fecha_fin),
    pesos = MODELO_DEFAULT,
    percentil_medio = MODELO_DEFAULT$percentiles$medio,
    percentil_alto = MODELO_DEFAULT$percentiles$alto
  )
  
  if(nrow(riesgo_semana) == 0){
    stop("No hay datos para el periodo.")
  }
  
  guardar_riesgo(
    riesgo = riesgo_semana,
    fecha_inicio = fecha_inicio,
    fecha_fin = fecha_fin,
    pesos = MODELO_DEFAULT,
    percentil_medio = MODELO_DEFAULT$percentiles$medio,
    percentil_alto = MODELO_DEFAULT$percentiles$alto,
    usuario = USUARIO_AUTOMATICO
  )
  
}, error = function(e){
  
  log_msg("ERROR:", conditionMessage(e))
  quit(save = "no", status = 1)
  
})

log_msg(
  "Corte", resultado$id_corte, "guardado con", resultado$filas, "operadores.",
  if(is.na(resultado$corte_anterior))
    "Sin corte de la semana anterior para comparar."
  else
    paste("Comparado contra el corte", resultado$corte_anterior)
)

try(DBI::dbDisconnect(con), silent = TRUE)
