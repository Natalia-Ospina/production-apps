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
  
  datos %>%
    
    mutate(
      
      ScoreTelemetria =
        
        EV19*pesos$EV19 +
        
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
  
  riesgo
  
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
          
          "\nScore Total: ", round(op$ScoreTotal, 2),
          
          "\n\n📡 TELEMETRÍA (",
          round(op$ScoreTelemetria, 2),
          " pts)",
          
          if(op$EV19 > 0)
            paste0("\n• EV19 (Sin mirar al frente): ", op$EV19)
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