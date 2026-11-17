#=========================================================
# PIRAMIDE BIRD
# FUNCIONES GENERALES
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
#=========================================================
# AUTENTICACION
#=========================================================

conexion2_dwh <- function() {
  
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB2_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    dbname = Sys.getenv("DB2_NAME"),
    user = Sys.getenv("DB2_USER"),
    password = Sys.getenv("DB2_PASSWORD")
  )
}

con2 <- conexion2_dwh()

validar_usuario <- function(username, password, con2) {
  
  usuario_db <- DBI::dbGetQuery(
    con2,
    "
    SELECT
      id,
      username,
      password,
      rol
    FROM public.usuarios
    WHERE username = $1
    ",
    params = list(username)
  )
  
  if (nrow(usuario_db) == 0) {
    return(FALSE)
  }
  
  hash <- usuario_db$password[1]
  
  # Adaptar hash PHP $2y$ a formato compatible
  hash <- sub(
    "^\\$2y\\$",
    "$2a$",
    hash
  )
  
  bcrypt::checkpw(
    password,
    hash
  )
}

#=========================================================
# FILTRAR TELEMETRÍA
#=========================================================

filtrar_telemetria <- function(
    datos,
    fecha_inicio = NULL,
    fecha_fin = NULL,
    empresa = "Todos",
    ruta = "Todos",
    tabla = "Todos",
    variable = "Todos"
){
  
  df <- datos
  
  if(!is.null(fecha_inicio)){
    df <- df %>%
      filter(Fecha >= fecha_inicio)
  }
  
  if(!is.null(fecha_fin)){
    df <- df %>%
      filter(Fecha <= fecha_fin)
  }
  
  if(empresa!="Todos"){
    df <- df %>%
      filter(Empresa==empresa)
  }
  
  if(ruta!="Todos"){
    df <- df %>%
      filter(IdRuta==ruta)
  }
  
  if(tabla!="Todos"){
    df <- df %>%
      filter(Tabla==tabla)
  }
  
  if(variable!="Todos"){
    df <- df %>%
      filter(Variable==variable)
  }
  
  df
  
}

#=========================================================
# FILTRAR ACCIDENTES
#=========================================================

filtrar_accidentes <- function(
    datos,
    fecha_inicio=NULL,
    fecha_fin=NULL
){
  
  df <- datos
  
  if(!is.null(fecha_inicio)){
    df <- df %>%
      filter(Fecha>=fecha_inicio)
  }
  
  if(!is.null(fecha_fin)){
    df <- df %>%
      filter(Fecha<=fecha_fin)
  }
  
  df
  
}

#=========================================================
# SCORE TELEMETRÍA
#=========================================================

calcular_score_telemetria <- function(
    telemetria_op,
    pesos
){
  
  # Conteo bruto por operador y tipo de evento
  datos <- telemetria_op %>%
    
    count(
      CodigoOperador,
      Tabla
    ) %>%
    
    pivot_wider(
      
      names_from=Tabla,
      
      values_from=n,
      
      values_fill=0
      
    )
  
  for(col in c("EV19","ALA1","ALA2","ALA3")){
    
    if(!col %in% names(datos))
      datos[[col]] <- 0
    
  }
  
  # EV19 ajustado: se cuenta por operador y día y a cada día se le
  # aplica la raíz cuadrada. Misma regla para todos: más eventos
  # siempre suman más, pero un día anómalo (p. ej. falla de cámara)
  # no domina el score.
  ev19_ajustado <- telemetria_op %>%
    
    filter(Tabla == "EV19") %>%
    
    count(
      CodigoOperador,
      Fecha,
      name = "EventosDia"
    ) %>%
    
    group_by(CodigoOperador) %>%
    
    summarise(
      EV19_ajustado = round(sum(sqrt(EventosDia)), 2),
      .groups = "drop"
    )
  
  datos %>%
    
    left_join(
      ev19_ajustado,
      by = "CodigoOperador"
    ) %>%
    
    mutate(
      
      EV19_ajustado = coalesce(EV19_ajustado, 0),
      
      ScoreTelemetria =
        
        EV19_ajustado*pesos$EV19 +
        
        ALA1*pesos$ALA1 +
        
        ALA2*pesos$ALA2 +
        
        ALA3*pesos$ALA3
      
    )
  
}

#=========================================================
# SCORE ACCIDENTES
#=========================================================

calcular_score_accidentes <- function(
    accidentes,
    pesos
){
  
  accidentes %>%
    
    mutate(
      
      Peso = case_when(
        
        TipoEvento == "Simple" ~ pesos$Simple,
        
        TipoEvento == "Lesionados" ~ pesos$Lesionados,
        
        TipoEvento == "Fatalidad" ~ pesos$Fatalidad,
        
        TRUE ~ 0
        
      ),
      
      Responsable = ifelse(
        Responsabilidad == "Greenmovil",
        1,
        0
      ),
      
      NoResponsable = ifelse(
        Responsabilidad == "Greenmovil",
        0,
        1
      )
      
    ) %>%
    
    group_by(CodigoOperador) %>%
    
    summarise(
      
      Simples = sum(TipoEvento == "Simple"),
      
      Lesionados = sum(TipoEvento == "Lesionados"),
      
      Fatales = sum(TipoEvento == "Fatalidad"),
      
      ScoreAccidentes = sum(Peso),
      
      Responsables = sum(Responsable),
      
      NoResponsables = sum(NoResponsable),
      
      .groups = "drop"
      
    ) %>%
    
    mutate(
      
      ScoreResponsabilidad =
        Responsables * pesos$Responsabilidad
      
    )
  
}

#=========================================================
# SCORE INFRACCIONES
#=========================================================

calcular_score_infracciones <- function(
    infracciones,
    pesos
){
  
  infracciones %>%
    
    mutate(
      
      Peso = case_when(
        
        Tipo == "Tipo_I" ~ pesos$Tipo_I,
        
        Tipo == "Tipo_II" ~ pesos$Tipo_II,
        
        Tipo == "Tipo_III" ~ pesos$Tipo_III,
        
        TRUE ~ 0
        
      )
      
    ) %>%
    
    group_by(CodigoOperador) %>%
    
    summarise(
      
      Tipo_I = sum(Tipo == "Tipo_I"),
      
      Tipo_II = sum(Tipo == "Tipo_II"),
      
      Tipo_III = sum(Tipo == "Tipo_III"),
      
      ScoreInfracciones = sum(Peso),
      
      .groups = "drop"
      
    )
  
}

#=========================================================
# MOTOR DE RIESGO
#=========================================================

calcular_riesgo <- function(
    
  telemetria_op,
  
  accidentes,
  
  infracciones,
  
  pesos,
  
  percentil_medio = .70,
  
  percentil_alto = .90
  
){
  
  score_telemetria <-
    
    calcular_score_telemetria(
      
      telemetria_op,
      
      pesos$telemetria_op
      
    )
  
  score_accidentes <-
    
    calcular_score_accidentes(
      
      accidentes,
      
      pesos$accidentes
      
    )
  
  score_infracciones <-
    
    calcular_score_infracciones(
      
      infracciones,
      
      pesos$infracciones
      
    )
  
  riesgo <-
    
    full_join(
      
      score_telemetria,
      
      score_accidentes,
      
      by = "CodigoOperador"
      
    ) %>%
    
    full_join(
      
      score_infracciones,
      
      by = "CodigoOperador"
      
    )
  
  riesgo <-
    
    riesgo %>%
    
    mutate(
      
      across(
        
        where(is.numeric),
        
        ~replace_na(.x, 0)
        
      )
      
    )
  
  riesgo <-
    
    riesgo %>%
    
    mutate(
      
      ScoreTotal =
        
        ScoreTelemetria +
        
        ScoreAccidentes +
        
        ScoreInfracciones +
        
        ScoreResponsabilidad
      
    )
  
  p70 <-
    
    quantile(
      
      riesgo$ScoreTotal,
      
      percentil_medio,
      
      na.rm = TRUE
      
    )
  
  p90 <-
    
    quantile(
      
      riesgo$ScoreTotal,
      
      percentil_alto,
      
      na.rm = TRUE
      
    )
  
  riesgo <-
    
    riesgo %>%
    
    mutate(
      
      Nivel =
        
        case_when(
          
          ScoreTotal >= p90 ~ "Alto",
          
          ScoreTotal >= p70 ~ "Medio",
          
          TRUE ~ "Bajo"
          
        )
      
    )
  
  # Redondeo a 2 decimales (después de calcular percentiles y
  # niveles para no alterar la clasificación)
  riesgo <-
    riesgo %>%
    mutate(
      across(
        c(EV19_ajustado, ScoreTelemetria, ScoreTotal),
        ~round(.x, 2)
      )
    )
  
  riesgo
  
}


#==========================================================
# GUARDAR DISTRIBUCIÓN DE RIESGO EN LA BASE DE DATOS
#==========================================================
# Guarda un "corte" con todos los operadores del dataframe de
# riesgo. Cada llamada crea un corte nuevo (no borra los
# anteriores), identificado por IdCorte.
#
# AltoSemanaAnterior: TRUE/FALSE si el operador estaba en
# nivel Alto en el corte de la semana anterior (el corte cuyo
# FechaFin es el día antes de FechaInicio; si hay varios se
# usa el automático). Queda vacío (NA) si no existe ese corte.
#==========================================================

guardar_riesgo <- function(
    riesgo,
    fecha_inicio,
    fecha_fin,
    pesos,
    percentil_medio,
    percentil_alto,
    usuario = NA_character_,
    esquema = Sys.getenv("INTEL_DB_SCHEMA", "public"),
    tabla = "FactRiesgoOperadores"
){
  
  insert_date <- Sys.time()
  
  id_corte <- format(
    insert_date,
    "%Y%m%d%H%M%S",
    tz = "America/Bogota"
  )
  
  fecha_inicio <- as.Date(fecha_inicio)
  fecha_fin    <- as.Date(fecha_fin)
  
  # Parámetros con los que se calculó el riesgo
  parametros <- paste0(
    "EV19=", pesos$telemetria_op$EV19,
    ";ALA1=", pesos$telemetria_op$ALA1,
    ";ALA2=", pesos$telemetria_op$ALA2,
    ";ALA3=", pesos$telemetria_op$ALA3,
    ";Simple=", pesos$accidentes$Simple,
    ";Lesionados=", pesos$accidentes$Lesionados,
    ";Fatalidad=", pesos$accidentes$Fatalidad,
    ";Responsabilidad=", pesos$accidentes$Responsabilidad,
    ";Tipo_I=", pesos$infracciones$Tipo_I,
    ";Tipo_II=", pesos$infracciones$Tipo_II,
    ";Tipo_III=", pesos$infracciones$Tipo_III,
    ";PercentilMedio=", percentil_medio,
    ";PercentilAlto=", percentil_alto
  )
  
  cols_conteo <- c(
    "EV19", "ALA1", "ALA2", "ALA3",
    "Simples", "Lesionados", "Fatales",
    "Responsables", "NoResponsables",
    "Tipo_I", "Tipo_II", "Tipo_III"
  )
  
  cols_score <- c(
    "EV19_ajustado",
    "ScoreTelemetria", "ScoreAccidentes",
    "ScoreResponsabilidad", "ScoreInfracciones",
    "ScoreTotal"
  )
  
  # Si alguna columna no existe (p. ej. sin infracciones en el
  # periodo) se crea en 0 para que la tabla siempre reciba todas
  for(col in c(cols_conteo, cols_score)){
    if(!col %in% names(riesgo))
      riesgo[[col]] <- 0
  }
  
  # Conexión propia para escribir, que se cierra al terminar
  con_w <- conexion_intelligence()
  on.exit(DBI::dbDisconnect(con_w), add = TRUE)
  
  tabla_sql <- DBI::dbQuoteIdentifier(
    con_w,
    DBI::Id(schema = esquema, table = tabla)
  )
  
  #--------------------------------------------------------
  # OPERADORES EN RIESGO ALTO LA SEMANA ANTERIOR
  #--------------------------------------------------------
  
  corte_anterior <- DBI::dbGetQuery(
    con_w,
    paste0(
      'SELECT "IdCorte" FROM ', tabla_sql,
      ' WHERE "FechaFin" = $1',
      ' GROUP BY "IdCorte", "Usuario"',
      # Se prefiere el corte automático; si no hay, el más reciente
      ' ORDER BY ("Usuario" = \'AUTOMATICO\') DESC NULLS LAST, "IdCorte" DESC',
      ' LIMIT 1'
    ),
    params = list(fecha_inicio - 1)
  )$IdCorte
  
  hay_corte_anterior <- length(corte_anterior) == 1 &&
    !is.na(corte_anterior)
  
  if(hay_corte_anterior){
    
    altos_anterior <- DBI::dbGetQuery(
      con_w,
      paste0(
        'SELECT "CodigoOperador" FROM ', tabla_sql,
        ' WHERE "IdCorte" = $1 AND "Nivel" = \'Alto\''
      ),
      params = list(corte_anterior)
    )$CodigoOperador
    
  }
  
  #--------------------------------------------------------
  # ARMAR EL CORTE
  #--------------------------------------------------------
  
  datos <- riesgo %>%
    
    arrange(desc(ScoreTotal)) %>%
    
    mutate(
      IdCorte = id_corte,
      InsertDate = insert_date,
      FechaInicio = fecha_inicio,
      FechaFin = fecha_fin,
      Usuario = if(is.null(usuario)) NA_character_ else as.character(usuario),
      Parametros = parametros,
      CodigoOperador = as.character(CodigoOperador),
      Ranking = row_number(),
      AltoSemanaAnterior = if(hay_corte_anterior)
        CodigoOperador %in% altos_anterior
      else
        NA,
      across(all_of(cols_conteo), as.integer),
      across(all_of(cols_score), ~round(as.numeric(.x), 2))
    ) %>%
    
    select(
      IdCorte, InsertDate, FechaInicio, FechaFin,
      Usuario, Parametros,
      CodigoOperador, Ranking, Nivel, AltoSemanaAnterior,
      all_of(cols_conteo),
      all_of(cols_score)
    )
  
  DBI::dbWithTransaction(con_w, {
    DBI::dbAppendTable(
      con_w,
      DBI::Id(schema = esquema, table = tabla),
      datos
    )
  })
  
  list(
    id_corte = id_corte,
    filas = nrow(datos),
    corte_anterior = if(hay_corte_anterior) corte_anterior else NA
  )
  
}


#==========================================================
# ¿YA EXISTE UN CORTE PARA ESE PERIODO Y USUARIO?
#==========================================================
# Se usa en el proceso semanal para no guardar dos veces la
# misma semana si el script se ejecuta de nuevo.
#==========================================================

existe_corte <- function(
    fecha_inicio,
    fecha_fin,
    usuario,
    esquema = Sys.getenv("INTEL_DB_SCHEMA", "public"),
    tabla = "FactRiesgoOperadores"
){
  
  con_r <- conexion_intelligence()
  on.exit(DBI::dbDisconnect(con_r), add = TRUE)
  
  tabla_sql <- DBI::dbQuoteIdentifier(
    con_r,
    DBI::Id(schema = esquema, table = tabla)
  )
  
  n <- DBI::dbGetQuery(
    con_r,
    paste0(
      'SELECT COUNT(*) AS n FROM ', tabla_sql,
      ' WHERE "FechaInicio" = $1 AND "FechaFin" = $2',
      ' AND "Usuario" = $3'
    ),
    params = list(
      as.Date(fecha_inicio),
      as.Date(fecha_fin),
      usuario
    )
  )$n
  
  as.numeric(n) > 0
  
}


#==========================================================
# CONSTRUIR PIRÁMIDE DE OPERADORES
#==========================================================

construir_piramide <- function(datos){
  
  datos <- datos %>%
    
    arrange(
      factor(
        Nivel,
        levels = c("Alto","Medio","Bajo")
      ),
      desc(ScoreTotal)
    )
  
  niveles <- c("Alto","Medio","Bajo")
  
  resultado <- data.frame()
  
  y <- 3
  
  for(nivel in niveles){
    
    grupo <- datos %>%
      filter(Nivel == nivel)
    
    n <- nrow(grupo)
    
    if(n == 0){
      
      y <- y-1
      
      next
      
    }
    
    grupo$x <- seq(
      from = -(n-1)/2,
      to = (n-1)/2,
      by = 1
    )
    
    grupo$y <- y
    
    resultado <- bind_rows(
      resultado,
      grupo
    )
    
    y <- y-1
    
  }
  
  resultado
  
}

#==========================================================
# CREA LA PIRÁMIDE EN HTML
#==========================================================

crear_tarjetas_piramide <- function(datos){
  
  niveles <- c("Alto","Medio","Bajo")
  
  colores <- c(
    Alto = "#d73027",
    Medio = "#fdae61",
    Bajo = "#1a9850"
  )
  
  filas <- lapply(niveles, function(nivel){
    
    operadores <- datos %>%
      dplyr::filter(Nivel == nivel) %>%
      dplyr::arrange(desc(ScoreTotal))
    
    # Para Medio y Bajo solo mostrar los 20 de mayor riesgo
    if(nivel != "Alto"){
      operadores <- head(operadores, 20)
    }
    
    if(nrow(operadores) == 0) return(NULL)
    
    tarjetas <- lapply(seq_len(nrow(operadores)), function(i){
      
      op <- operadores[i,]
      
      tags$div(
        
        title = paste0(
          
          "Operador: ", op$CodigoOperador,
          
          "\nNivel: ", op$Nivel,
          
          "\nScore Total: ", formatC(op$ScoreTotal, format = "f", digits = 2),
          
          "\n\n📡 TELEMETRÍA (",
          formatC(op$ScoreTelemetria, format = "f", digits = 2),
          " pts)",
          
          if(op$EV19 > 0)
            paste0(
              "\n• EV19 (Sin mirar al frente): ", op$EV19,
              " (", formatC(op$EV19_ajustado, format = "f", digits = 2), ")"
            )
          else "",
          
          if(op$ALA1 > 0)
            paste0("\n• ALA1 (Aceleración brusca): ", op$ALA1)
          else "",
          
          if(op$ALA2 > 0)
            paste0("\n• ALA2 (Frenada brusca): ", op$ALA2)
          else "",
          
          if(op$ALA3 > 0)
            paste0("\n• ALA3 (Exceso velocidad): ", op$ALA3)
          else "",
          
          
          "\n\n⚠ ACCIDENTES (",
          round(op$ScoreAccidentes, 2),
          " pts)",
          
          if(op$Simples > 0)
            paste0("\n• Simples: ", op$Simples)
          else "",
          
          if(op$Lesionados > 0)
            paste0("\n• Lesionados: ", op$Lesionados)
          else "",
          
          if(op$Fatales > 0)
            paste0("\n• Fatalidades: ", op$Fatales)
          else "",
          
          "\n\nResponsabilidad Green Móvil: ",
          op$Responsables,
          
          "\n\n🚨 INFRACCIONES (",
          round(op$ScoreInfracciones, 2),
          " pts)",
          
          if(op$Tipo_I > 0)
            paste0("\n• Tipo_I: ", op$Tipo_I)
          else "",
          
          if(op$Tipo_II > 0)
            paste0("\n• Tipo_II: ", op$Tipo_II)
          else "",
          
          if(op$Tipo_III > 0)
            paste0("\n• Tipo_III: ", op$Tipo_III)
          else ""
          
        ),
        
        style = paste0(
          "background:", colores[nivel], ";",
          "color:white;",
          "padding:8px;",
          "margin:6px;",
          "border-radius:8px;",
          "font-weight:bold;",
          "width:90px;",
          "text-align:center;",
          "display:inline-block;",
          "box-shadow:2px 2px 5px #999;"
        ),
        
        op$CodigoOperador
        
      )
      
    })
    
    tags$div(
      
      style = paste0(
        
        "display:flex;",
        "flex-direction:column;",
        "align-items:center;",
        "margin-bottom:35px;"
        
      ),
      
      tags$h3(
        nivel,
        style = "margin-bottom:15px;font-weight:bold;"
      ),
      
      tags$div(
        
        style = paste0(
          
          "display:flex;",
          "flex-wrap:wrap;",
          "justify-content:center;",
          "gap:8px;",
          "max-width:",
          
          if(nivel == "Alto"){
            
            "1500px"
            
          } else if(nivel == "Medio"){
            
            "1000px"
            
          } else {
            
            "700px"
            
          }
          
        ),
        
        tarjetas
        
      )
      
    )
    
  })
  
  tagList(filas)
  
}