
server <- function(input, output, session){
  
  #==========================================================
  # AUTENTICACIÓN
  #==========================================================
  
  autenticado <- reactiveVal(FALSE)
  
  usuario_activo <- reactiveVal(NULL)
  
  observe({
    
    if (!autenticado()) {
      
      showModal(
        modalDialog(
          
          title = NULL,
          
          div(
            
            h2(
              "Pirámide Bird",
              style = "
              text-align: center;
              font-weight: bold;
              margin-bottom: 30px;
            "
            ),
            
            textInput(
              "username",
              "Usuario",
              placeholder = "Ingrese su usuario"
            ),
            
            passwordInput(
              "password",
              "Contraseña",
              placeholder = "Ingrese su contraseña"
            ),
            
            div(
              style = "text-align: center;",
              
              actionButton(
                "btn_login",
                "Iniciar sesión",
                class = "btn-primary",
                style = "width: 100%;"
              )
            ),
            
            br(),
            
            uiOutput("mensaje_login")
            
          ),
          
          easyClose = FALSE,
          
          footer = NULL,
          
          size = "s"
        )
      )
      
    }
    
  })
  
  #==========================================================
  # VALIDAR LOGIN
  #==========================================================
  
  observeEvent(input$btn_login, {
    
    req(input$username)
    req(input$password)
    
    print("Intentando validar usuario...")
    
    valido <- validar_usuario(
      input$username,
      input$password,
      con2
    )
    
    print(paste("Resultado:", valido))
    
    if (isTRUE(valido)) {
      
      print("LOGIN CORRECTO")
      
      usuario_activo(input$username)
      
      autenticado(TRUE)
      
      removeModal()
      
    } else {
      
      print("LOGIN INCORRECTO")
      
      output$mensaje_login <- renderUI({
        
        div(
          class = "alert alert-danger",
          style = "margin-top: 15px;",
          "Usuario o contraseña incorrectos."
        )
        
      })
      
    }
    
  })
  
  #==========================================================
  # MOSTRAR USUARIO ACTIVO
  #==========================================================
  
  output$info_usuario <- renderUI({
    
    req(usuario_activo())
    
    div(
      
      style = "
      text-align: center;
      padding: 15px 10px;
      margin: 10px;
      border-radius: 8px;
      background-color: rgba(255,255,255,0.1);
      color: #343a40;
    ",
      
      div(
        icon("user-circle"),
        style = "font-size: 25px; 
        margin-bottom: 5px;
        color: #343a40;"
      ),
      
      div(
        "Usuario activo",
        style = "
        font-size: 11px;
        opacity: 0.7;
        color: #343a40;
      "
      ),
      
      tags$strong(
        usuario_activo(),
        style = "font-size: 14px;"
      )
      
    )
  })
  
  #==========================================================
  # CERRAR SESIÓN
  #==========================================================
  
  observeEvent(input$btn_logout, {
    
    usuario_activo(NULL)
    
    autenticado(FALSE)
    
  })
  
  #==========================================================
  # PARÁMETROS DEL MODELO
  #==========================================================
  
  pesos_modelo <- reactive({
    
    list(
      
      telemetria_op = list(
        
        EV19 = input$peso_ev19_piramide,
        
        ALA1 = input$peso_ala1_piramide,
        
        ALA2 = input$peso_ala2_piramide,
        
        ALA3 = input$peso_ala3_piramide
        
      ),
      
      accidentes = list(
        
        Simple = input$peso_simple_piramide,
        
        Lesionados = input$peso_lesionados_piramide,
          
        Fatalidad = input$peso_fatalidad_piramide,
        
        Responsabilidad = input$peso_responsabilidad_piramide
        
      ),
      
      infracciones = list(
        
        Tipo_I = input$peso_tipo_I_piramide,
        
        Tipo_II = input$peso_tipo_II_piramide,
        
        Tipo_III = input$peso_tipo_III_piramide
        
      )
      
    )
    
  })
  
  #==========================================================
  # PERCENTILES
  #==========================================================
  
  percentil_medio <- reactive({
    
    input$percentil_medio_piramide / 100
    
  })
  
  percentil_alto <- reactive({
    
    input$percentil_alto_piramide / 100
    
  })
  
  #==========================================================
  # TOP SELECCIONADO
  #==========================================================
  
  top_n <- reactive({
    
    req(input$top_n)
    
    if(input$top_n=="Todos"){
      
      return(999999)
      
    }
    
    as.numeric(input$top_n)
    
  })
  
  #==========================================================
  # FILTRO TELEMETRÍA
  #==========================================================
  
  telemetria_filtrada <- reactive({
    
    datos <- telemetria_op
    
    #--------------------------------------------------------
    # FECHA
    #--------------------------------------------------------
    
    if(length(input$filtro_fecha) == 2){
      
      fecha_inicio <- as.Date(input$filtro_fecha[1])
      fecha_fin    <- as.Date(input$filtro_fecha[2])
      
      datos <- datos %>%
        dplyr::filter(
          Fecha >= fecha_inicio,
          Fecha <= fecha_fin
        )
      
    }
    
    #--------------------------------------------------------
    # TABLA / TIPO EVENTO
    #--------------------------------------------------------
    
    if(!is.null(input$filtro_tabla) &&
       input$filtro_tabla != "Todos"){
      
      datos <- datos %>%
        dplyr::filter(
          as.character(Tabla) ==
            as.character(input$filtro_tabla)
        )
      
    }
    
    #--------------------------------------------------------
    # EMPRESA
    #--------------------------------------------------------
    
    if(!is.null(input$filtro_empresa) &&
       input$filtro_empresa != "Todos" &&
       "Empresa" %in% names(datos)){
      
      datos <- datos %>%
        dplyr::filter(
          as.character(Empresa) ==
            as.character(input$filtro_empresa)
        )
      
    }
    
    #--------------------------------------------------------
    #FILTRO OPERADOR
    #--------------------------------------------------------
    
    if(
      !is.null(input$filtro_operador) &&
      input$filtro_operador != "Todos"
    ){
      
      datos <- datos %>%
        dplyr::filter(
          as.character(CodigoOperador) ==
            as.character(input$filtro_operador)
        )
      
    }
    
    datos
    
  })
  
  
  #==========================================================
  # FILTRO ACCIDENTES
  #==========================================================
  
  accidentes_filtrados <- reactive({
    
    datos <- accidentes
    
    
    #--------------------------------------------------------
    # FECHA
    #--------------------------------------------------------
    
    if(length(input$filtro_fecha) == 2){
      
      datos <- datos %>%
        
        dplyr::filter(
          Fecha >= as.Date(input$filtro_fecha[1]),
          Fecha <= as.Date(input$filtro_fecha[2])
        )
      
    }
    
    
    #--------------------------------------------------------
    # EMPRESA
    #--------------------------------------------------------
    
    if(
      !is.null(input$filtro_empresa) &&
      input$filtro_empresa != "Todos"
    ){
      
      datos <- datos %>%
        
        dplyr::filter(
          as.character(Empresa) ==
            as.character(input$filtro_empresa)
        )
      
    }
    
    
    #--------------------------------------------------------
    # TIPO DE ACCIDENTE
    #--------------------------------------------------------
    
    if(
      !is.null(input$filtro_tipo_accidente) &&
      input$filtro_tipo_accidente != "Todos"
    ){
      
      datos <- datos %>%
        
        dplyr::filter(
          as.character(TipoEvento) ==
            as.character(input$filtro_tipo_accidente)
        )
      
    }
    
    
    #--------------------------------------------------------
    # RESPONSABILIDAD
    #--------------------------------------------------------
    
    if(
      !is.null(input$filtro_responsabilidad) &&
      input$filtro_responsabilidad != "Todos"
    ){
      
      datos <- datos %>%
        
        dplyr::filter(
          as.character(Responsabilidad) ==
            as.character(input$filtro_responsabilidad)
        )
      
    }
    
    #--------------------------------------------------------
    #FILTRO OPERADOR
    #--------------------------------------------------------
    
    if(
      !is.null(input$filtro_operador) &&
      input$filtro_operador != "Todos"
    ){
      
      datos <- datos %>%
        dplyr::filter(
          as.character(CodigoOperador) ==
            as.character(input$filtro_operador)
        )
      
    }
    
    datos
    
  })
  
  #==========================================================
  # FILTRO INFRACCIONES
  #==========================================================
  
  infracciones_filtradas <- reactive({
    
    datos <- infracciones
    
    #--------------------------------------------------------
    #FECHA
    #--------------------------------------------------------
    
    if(length(input$filtro_fecha) == 2){
      
      datos <- datos %>%
        
        dplyr::filter(
          Fecha >= as.Date(input$filtro_fecha[1]),
          Fecha <= as.Date(input$filtro_fecha[2])
        )
      
    }
    
    #--------------------------------------------------------
    #EMPRESA
    #--------------------------------------------------------
    
    if(
      !is.null(input$filtro_empresa) &&
      input$filtro_empresa != "Todos"
    ){
      
      datos <- datos %>%
        
        dplyr::filter(
          as.character(Empresa) ==
            as.character(input$filtro_empresa)
        )
      
    }
    
    #--------------------------------------------------------
    #TIPO INFRACCION
    #--------------------------------------------------------
    
    if(
      !is.null(input$filtro_tipo_infraccion) &&
      input$filtro_tipo_infraccion != "Todos"
    ){
      
      datos <- datos %>%
        
        dplyr::filter(
          as.character(Tipo) ==
            as.character(input$filtro_tipo_infraccion)
        )
      
    }
    
    #--------------------------------------------------------
    #FILTRO OPERADOR
    #--------------------------------------------------------
    
    if(
      !is.null(input$filtro_operador) &&
      input$filtro_operador != "Todos"
    ){
      
      datos <- datos %>%
        dplyr::filter(
          as.character(CodigoOperador) ==
            as.character(input$filtro_operador)
        )
      
    }
    
    datos
    
  })
  
  #==========================================================
  # RIESGO OPERADORES
  #==========================================================
  
  riesgo_operadores <- reactive({
    
    calcular_riesgo(
      
      telemetria_op = telemetria_filtrada(),
      
      accidentes = accidentes_filtrados(),
      
      infracciones = infracciones_filtradas(),
      
      pesos = pesos_modelo(),
      
      percentil_medio = percentil_medio(),
      
      percentil_alto = percentil_alto()
      
    )
    
  })
  
  #==========================================================
  # RIESGO OPERADORES FILTRADO
  #==========================================================
  
  riesgo_operadores_filtrado <- reactive({
    
    datos <- riesgo_operadores()
    
    
    #--------------------------------------------------------
    # FILTRO TIPO DE ACCIDENTE
    #--------------------------------------------------------
    
    if(
      !is.null(input$filtro_tipo_accidente) &&
      input$filtro_tipo_accidente != "Todos"
    ){
      
      if(input$filtro_tipo_accidente == "Simple"){
        
        datos <- datos %>%
          filter(Simples > 0)
        
      }
      
      if(input$filtro_tipo_accidente == "Lesionados"){
        
        datos <- datos %>%
          filter(Lesionados > 0)
        
      }
      
      if(input$filtro_tipo_accidente == "Fatalidad"){
        
        datos <- datos %>%
          filter(Fatales > 0)
        
      }
      
    }
    
    
    #--------------------------------------------------------
    # FILTRO RESPONSABILIDAD
    #--------------------------------------------------------
    
    if(
      !is.null(input$filtro_responsabilidad) &&
      input$filtro_responsabilidad != "Todos"
    ){
      
      if(input$filtro_responsabilidad == "Greenmovil"){
        
        datos <- datos %>%
          filter(Responsables > 0)
        
      }
      
      if(input$filtro_responsabilidad == "Sin responsabilidad"){
        
        datos <- datos %>%
          filter(Responsables == 0)
        
      }
      
    }
    
    #--------------------------------------------------------
    # FILTRO INFRACCIONES
    #--------------------------------------------------------
    
    if(
      !is.null(input$filtro_tipo_infraccion) &&
      input$filtro_tipo_infraccion != "Todos"
    ){
      
      if(input$filtro_tipo_infraccion == "Tipo_I"){
        
        datos <- datos %>%
          filter(Tipo_I > 0)
        
      }
      
      if(input$filtro_tipo_infraccion == "Tipo_II"){
        
        datos <- datos %>%
          filter(Tipo_II > 0)
        
      }
      
      if(input$filtro_tipo_infraccion == "Tipo_III"){
        
        datos <- datos %>%
          filter(Tipo_III > 0)
        
      }
      
    }
    
    #--------------------------------------------------------
    #FILTRO OPERADOR
    #--------------------------------------------------------
    
    if(
      !is.null(input$filtro_operador) &&
      input$filtro_operador != "Todos"
    ){
      
      datos <- datos %>%
        filter(
          as.character(CodigoOperador) ==
            as.character(input$filtro_operador)
        )
      
    }
    
    #--------------------------------------------------------
    # FILTRO NIVEL
    #--------------------------------------------------------
    
    if(
      !is.null(input$filtro_nivel) &&
      input$filtro_nivel != "Todos"
    ){
      
      datos <- datos %>%
        filter(
          Nivel == input$filtro_nivel
        )
      
    }
    
    
    datos
    
  })
  
  #==========================================================
  # PIRÁMIDE 1
  # RIESGO POTENCIAL
  #==========================================================
  
  riesgo_potencial <- reactive({
    
    datos <- riesgo_operadores_filtrado()
    
    datos %>%
      filter(
        (ScoreTelemetria > 0 |
           ScoreAccidentes > 0 |
           ScoreInfracciones > 0),
        Responsables == 0
      ) %>%
      arrange(
        desc(ScoreTotal)
      )
    
  })
  
  #==========================================================
  # PIRÁMIDE 2
  # RIESGO MATERIALIZADO
  #==========================================================
  
  riesgo_materializado <- reactive({
    
    riesgo_operadores_filtrado() %>%
      
      filter(
        
        Responsables > 0
        
      ) %>%
      
      arrange(desc(ScoreTotal))
    
  })
  
  #==========================================================
  # TOP OPERADORES
  #==========================================================
  
  ranking_operadores <- reactive({
    
    riesgo_operadores_filtrado() %>%
      
      arrange(
        
        desc(ScoreTotal)
        
      ) %>%
      
      slice_head(
        
        n = top_n()
        
      )
    
  })
  
  #==========================================================
  # TOP SELECCIONADO
  #==========================================================
  
  top_n <- reactive({
    
    req(input$top_n)
    
    if(input$top_n=="Todos"){
      return(999999)
    }
    
    as.numeric(input$top_n)
    
  })
  
  #==========================================================
  #OPCIONES DE LOS FILTROS
  #==========================================================
  
  observe({
    
    #--------------------------------------------------------
    #EMPRESAS
    #--------------------------------------------------------
    
    empresas <- sort(
      unique(
        c(
          as.character(telemetria_op$Empresa),
          as.character(accidentes$Empresa),
          as.character(infracciones$Empresa)
        )
      )
    )
    
    empresas <- empresas[
      !is.na(empresas) &
        empresas != "" &
        empresas != "NA"
    ]
    
    #--------------------------------------------------------
    #OPERADORES
    #--------------------------------------------------------
    
    operadores <- sort(
      unique(
        c(
          as.character(telemetria_op$CodigoOperador),
          as.character(accidentes$CodigoOperador),
          as.character(infracciones$CodigoOperador)
        )
      )
    )
    
    operadores <- operadores[
      !is.na(operadores) &
        operadores != "" &
        operadores != "NA"
    ]
    
    #--------------------------------------------------------
    #TIPO DE ACCIDENTE
    #--------------------------------------------------------
    
    tipos_accidente <- sort(
      unique(
        as.character(accidentes$TipoEvento)
      )
    )
    
    tipos_accidente <- tipos_accidente[
      !is.na(tipos_accidente) &
        tipos_accidente != "" &
        tipos_accidente != "NA"
    ]
    
    #--------------------------------------------------------
    #RESPONSABILIDAD
    #--------------------------------------------------------
    
    responsabilidades <- c(
      "Greenmovil",
      "Sin responsabilidad"
    )
    
    #--------------------------------------------------------
    #TIPO DE EVENTO
    #--------------------------------------------------------
    
    tablas <- sort(
      unique(
        as.character(telemetria_op$Tabla)
      )
    )
    
    tablas <- tablas[
      !is.na(tablas) &
        tablas != "" &
        tablas != "NA"
    ]
    
    #--------------------------------------------------------
    #TIPO DE INFRACCION
    #--------------------------------------------------------
    
    tipo_infraccion <- sort(
      unique(
        as.character(infracciones$Tipo)
      )
    )
    
    tipo_infraccion <- tipo_infraccion[
      !is.na(tipo_infraccion) &
        tipo_infraccion != "" &
        tipo_infraccion != "NA"
    ]
    
    #--------------------------------------------------------
    #ACTUALIZAR EMPRESA
    #--------------------------------------------------------
    
    updateSelectInput(
      session,
      "filtro_empresa",
      choices = c("Todos", empresas),
      selected = "Todos"
    )
    
    #--------------------------------------------------------
    #ACTUALIZAR OPERADOR
    #--------------------------------------------------------
    
    updateSelectizeInput(
      session,
      "filtro_operador",
      choices = c("Todos", operadores),
      selected = "Todos",
      server = FALSE
    )
    
    #--------------------------------------------------------
    #ACTUALIZAR TIPO DE ACCIDENTE
    #--------------------------------------------------------
    
    updateSelectInput(
      session,
      "filtro_tipo_accidente",
      choices = c("Todos", tipos_accidente),
      selected = "Todos"
    )
    
    #--------------------------------------------------------
    #ACTUALIZAR RESPONSABILIDAD
    #--------------------------------------------------------
    
    updateSelectInput(
      session,
      "filtro_responsabilidad",
      choices = c("Todos", responsabilidades),
      selected = "Todos"
    )
    
    #--------------------------------------------------------
    #ACTUALIZAR TIPO DE EVENTO
    #--------------------------------------------------------
    
    updateSelectInput(
      session,
      "filtro_tabla",
      choices = c("Todos", tablas),
      selected = "Todos"
    )
    
    #--------------------------------------------------------
    #ACTUALIZAR TIPO DE INFRACCION
    #--------------------------------------------------------
    
    updateSelectInput(
      session,
      "filtro_tipo_infraccion",
      choices = c("Todos", tipo_infraccion),
      selected = "Todos"
    )
    
  })
  
  #==========================================================
  # KPIs
  #==========================================================
  
  # Función para mostrar números con separador de miles
  formatear_numero <- function(x) {
    
    format(
      x,
      big.mark = ".",
      decimal.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
    
  }
  
  
  #==========================================================
  # KPI - EVENTOS
  #==========================================================
  
  output$kpi_eventos <- renderValueBox({
    
    datos <- telemetria_filtrada()
    
    total_eventos <- nrow(datos)
    
    total_ev19 <- sum(
      datos$Tabla == "EV19",
      na.rm = TRUE
    )
    
    total_ala1 <- sum(
      datos$Tabla == "ALA1",
      na.rm = TRUE
    )
    
    total_ala2 <- sum(
      datos$Tabla == "ALA2",
      na.rm = TRUE
    )
    
    total_ala3 <- sum(
      datos$Tabla == "ALA3",
      na.rm = TRUE
    )
    
    valueBox(
      
      value = formatear_numero(total_eventos),
      
      subtitle = tagList(
        
        "Eventos totales de telemetría",
        
        tags$div(
          
          style = "
          font-size: 12px;
          line-height: 1.6;
          margin-top: 5px;
        ",
          
          paste0(
            "EV19: ", formatear_numero(total_ev19),
            " | ALA1: ", formatear_numero(total_ala1)
          ),
          
          tags$br(),
          
          paste0(
            "ALA2: ", formatear_numero(total_ala2),
            " | ALA3: ", formatear_numero(total_ala3)
          )
          
        )
        
      ),
      
      icon = icon("triangle-exclamation"),
      
      color = "primary"
      
    )
    
  })
  
  
  #==========================================================
  # KPI - ACCIDENTES
  #==========================================================
  
  output$kpi_accidentes <- renderValueBox({
    
    datos <- accidentes_filtrados()
    
    total_accidentes <- nrow(datos)
    
    total_simple <- sum(
      datos$TipoEvento == "Simple",
      na.rm = TRUE
    )
    
    total_lesionados <- sum(
      grepl(
        "Lesion",
        datos$TipoEvento,
        ignore.case = TRUE
      ),
      na.rm = TRUE
    )
    
    total_fatalidad <- sum(
      grepl(
        "Fatal",
        datos$TipoEvento,
        ignore.case = TRUE
      ),
      na.rm = TRUE
    )
    
    valueBox(
      
      value = formatear_numero(total_accidentes),
      
      subtitle = tagList(
        
        "Accidentes totales",
        
        tags$div(
          
          style = "
          font-size: 12px;
          line-height: 1.6;
          margin-top: 5px;
        ",
          
          paste0(
            "Simple: ", formatear_numero(total_simple),
            " | Lesionados: ", formatear_numero(total_lesionados)
          ),
          
          tags$br(),
          
          paste0(
            "Fatalidad: ", formatear_numero(total_fatalidad)
          )
          
        )
        
      ),
      
      icon = icon("car-burst"),
      
      color = "danger"
      
    )
    
  })
  
  #==========================================================
  # KPI - INFRACCIONES
  #==========================================================
  
  output$kpi_infracciones <- renderValueBox({
    
    datos <- infracciones_filtradas()
    
    total_infracciones <- nrow(datos)
    
    total_tipo_I <- sum(
      datos$Tipo == "Tipo_I",
      na.rm = TRUE
    )
    
    total_tipo_II <- sum(
      datos$Tipo == "Tipo_II",
      na.rm = TRUE
    )
    
    total_tipo_III <- sum(
      datos$Tipo == "Tipo_III",
      na.rm = TRUE
    )
    
    valueBox(
      
      value = formatear_numero(total_infracciones),
      
      subtitle = tagList(
        
        "Infracciones totales",
        
        tags$div(
          
          style = "
    font-size: 12px;
    line-height: 1.6;
    margin-top: 5px;
    ",
          
          paste0(
            "Tipo_I: ", formatear_numero(total_tipo_I),
            " | Tipo_II: ", formatear_numero(total_tipo_II)
          ),
          
          tags$br(),
          
          paste0(
            "Tipo_III: ", formatear_numero(total_tipo_III)
          )
          
        )
        
      ),
      
      icon = icon("shield"),
      
      color = "warning"
      
    )
    
  })
  
  #==========================================================
  # KPI - OPERADORES
  #==========================================================
  
  output$kpi_operadores <- renderValueBox({
    
    total_operadores <- nrow(
      riesgo_operadores()
    )
    
    valueBox(
      
      value = formatear_numero(total_operadores),
      
      subtitle = tagList(
        
        "Operadores",
        
        tags$div(
          
          style = "
    font-size: 12px;
    line-height: 1.6;
    margin-top: 5px;
    min-height: 38px;
    ",
          
          ""
          
        )
        
      ),
      
      icon = icon("users"),
      
      color = "success"
      
    )
    
  })
  
  
  #==========================================================
  # KPI - RESPONSABILIDAD
  #==========================================================
  
  output$kpi_responsables <- renderValueBox({
    
    # Total de accidentes con responsabilidad
    total_responsables <- sum(
      riesgo_operadores()$Responsables,
      na.rm = TRUE
    )
    
    # Detalle de accidentes con responsabilidad por tipo
    detalle_accidentes <- accidentes_filtrados() %>%
      filter(Responsabilidad == "Greenmovil") %>%
      count(TipoEvento)
    
    
    # Obtener cantidades por tipo
    total_simple <- detalle_accidentes %>%
      filter(TipoEvento == "Simple") %>%
      pull(n)
    
    total_lesionados <- detalle_accidentes %>%
      filter(grepl("Lesion", TipoEvento, ignore.case = TRUE)) %>%
      pull(n)
    
    total_fatalidad <- detalle_accidentes %>%
      filter(grepl("Fatal", TipoEvento, ignore.case = TRUE)) %>%
      pull(n)
    
    
    # Si no existe algún tipo, asignar 0
    if (length(total_simple) == 0) total_simple <- 0
    if (length(total_lesionados) == 0) total_lesionados <- 0
    if (length(total_fatalidad) == 0) total_fatalidad <- 0
    
    
    valueBox(
      
      value = formatear_numero(total_responsables),
      
      subtitle = tagList(
        
        "Accidentes con responsabilidad",
        
        tags$div(
          
          style = "
          font-size: 12px;
          line-height: 1.6;
          margin-top: 5px;
        ",
          
          paste0(
            "Simple: ", formatear_numero(total_simple),
            " | Lesionados: ", formatear_numero(total_lesionados)
          ),
          
          tags$br(),
          
          paste0(
            "Fatalidad: ", formatear_numero(total_fatalidad)
          )
          
        )
        
      ),
      
      icon = icon("car-burst"),
      
      color = "danger"
      
    )
    
  })
  
  
  #############################################################
  # TABLA
  #############################################################
  
  output$tabla_riesgo <- renderDT({
    
    datatable(
      
      ranking_operadores(),
      
      filter = "top",
      
      extensions = "Buttons",
      
      options = list(
        
        pageLength = 20,
        
        scrollX = TRUE,
        
        dom = "Bfrtip",
        
        buttons = c("copy","csv","excel")
        
      )
      
    )
    
  })
  
  #==================================================
  # EVENTOS DE TELEMETRÍA
  #==================================================
  
  output$graf_eventos <- renderPlotly({
    
    datos <- telemetria_filtrada()
    
    req(nrow(datos) > 0)
    
    resumen <- datos %>%
      count(Tabla)
    
    p <- ggplot(
      resumen,
      aes(
        x = reorder(Tabla, n),
        y = n,
        fill = Tabla
      )
    ) +
      
      geom_col(show.legend = FALSE) +
      
      scale_y_continuous(
        labels = scales::label_number(
          big.mark = ".",
          decimal.mark = ",",
          accuracy = 1
        )
      ) +
      
      coord_flip() +
      
      labs(
        x = "",
        y = "Cantidad",
        title = "Eventos de Telemetría"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  #==================================================
  # TENDENCIA TELEMETRIA
  #==================================================
  
  output$graf_tendencia <- renderPlotly({
    
    datos <- telemetria_filtrada()
    
    req(nrow(datos) > 0)
    
    resumen <- datos %>%
      count(Fecha) %>%
      arrange(Fecha)
    
    p <- ggplot(
      resumen,
      aes(
        x = Fecha,
        y = n
      )
    ) +
      
      geom_line(
        linewidth = 0.6,
        color = "#1F4E79"
      ) +
      
      geom_point(
        size = 1.5,
        color = "#1F4E79"
      ) +
      
      labs(
        title = "Tendencia diaria de eventos",
        x = "Fecha",
        y = "Número de eventos"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  #==================================================
  # ACCIDENTES
  #==================================================
  
  output$graf_accidentes <- renderPlotly({
    
    datos <- accidentes_filtrados()
    
    req(nrow(datos) > 0)
    
    resumen <- datos %>%
      count(TipoEvento)
    
    p <- ggplot(
      resumen,
      aes(
        TipoEvento,
        n,
        fill = TipoEvento
      )
    ) +
      
      geom_col(show.legend = FALSE) +
      
      labs(
        x = "",
        y = "Cantidad",
        title = "Accidentes"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  #==================================================
  #TENDENCIA DIARIA DE ACCIDENTES
  #==================================================
  
  output$graf_tendencia_accidentes <- renderPlotly({
    
    datos <- accidentes_filtrados()
    
    req(nrow(datos) > 0)
    
    total_dia <- datos %>%
      count(Fecha) %>%
      mutate(
        Tipo = "Total"
      )

    
    detalle_dia <- datos %>%
      count(Fecha, TipoEvento) %>%
      rename(
        Tipo = TipoEvento
      )

    
    resumen <- bind_rows(
      detalle_dia,
      total_dia
    )
    
    
    p <- ggplot(
      resumen,
      aes(
        x = Fecha,
        y = n,
        group = Tipo,
        color = Tipo,
        text = paste0(
          "Fecha: ", format(Fecha, "%d/%m/%Y"),
          "<br>Tipo: ", Tipo,
          "<br>Cantidad: ", n
        )
      )
    ) +
      
      geom_line(
        linewidth = 0.6
      ) +
      
      geom_point(
        size = 1.5
      ) +
      
      scale_color_manual(
        values = c(
          setNames(
            scales::hue_pal()(length(setdiff(unique(resumen$Tipo), "Total"))),
            setdiff(unique(resumen$Tipo), "Total")
          ),
          Total = "#1F4E79"
        )
      ) +
      
      scale_y_continuous(
        labels = scales::label_number(
          big.mark = ".",
          decimal.mark = ",",
          accuracy = 1
        )
      ) +
      
      labs(
        x = "",
        y = "Cantidad",
        title = "Tendencia Diaria de Accidentes",
        color = ""
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  #==========================================================
  #GRÁFICA - INFRACCIONES POR TIPO
  #==========================================================
  
  output$graf_infracciones <- renderPlotly({
    
    datos <- infracciones_filtradas()
    
    req(nrow(datos) > 0)
    
    resumen <- datos %>%
      count(Detalle, sort = TRUE)
    
    p <- ggplot(
      resumen,
      aes(
        x = reorder(Detalle, n),
        y = n,
        text = paste0(
          "Detalle: ", Detalle,
          "<br>Cantidad: ", n
        )
      )
    ) +
      
      geom_col(
        fill = "#5BC0BE"
      ) +
      
      scale_y_continuous(
        labels = scales::label_number(
          big.mark = ".",
          decimal.mark = ",",
          accuracy = 1
        )
      ) +
      
      coord_flip() +
      
      labs(
        x = "",
        y = "Cantidad",
        title = "Infracciones por Detalle"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  #==================================================
  #TENDENCIA DIARIA DE INFRACCIONES
  #==================================================
  
  output$graf_tendencia_infracciones <- renderPlotly({
    
    datos <- infracciones_filtradas()
    
    req(nrow(datos) > 0)
    
    
    total_dia <- datos %>%
      count(Fecha) %>%
      mutate(
        Tipo = "Total"
      )
    
    
    detalle_dia <- datos %>%
      count(Fecha, Tipo)
    
    
    resumen <- bind_rows(
      detalle_dia,
      total_dia
    )
    
    p <- ggplot(
      resumen,
      aes(
        x = Fecha,
        y = n,
        group = Tipo,
        color = Tipo,
        text = paste0(
          "Fecha: ", format(Fecha, "%d/%m/%Y"),
          "<br>Tipo: ", Tipo,
          "<br>Cantidad: ", n
        )
      )
    ) +
      
      geom_line(
        linewidth = 0.6
      ) +
      
      geom_point(
        size = 1.5
      ) +
      
      scale_color_manual(
        values = c(
          setNames(
            scales::hue_pal()(length(setdiff(unique(resumen$Tipo), "Total"))),
            setdiff(unique(resumen$Tipo), "Total")
          ),
          Total = "#1F4E79"
        )
      ) +
      
      scale_y_continuous(
        labels = scales::label_number(
          big.mark = ".",
          decimal.mark = ",",
          accuracy = 1
        )
      ) +
      
      labs(
        x = "",
        y = "Cantidad",
        title = "Tendencia Diaria de Infracciones",
        color = ""
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  
  
  #==================================================
  # TOP OPERADORES
  #==================================================
  
  output$graf_operadores <- renderPlotly({
    
    datos <- ranking_operadores()
    
    req(nrow(datos) > 0)
    
    p <- ggplot(
      datos,
      aes(
        reorder(CodigoOperador, ScoreTotal),
        ScoreTotal,
        fill = Nivel
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      theme_minimal() +
      
      labs(
        x = "",
        y = "Score",
        title = paste0("Top ", nrow(datos), " Operadores con Mayor Riesgo")
      )
    
    ggplotly(p)
    
  })
  
  #==================================================
  # PORCENTAJE DE LA PLANTA POR NIVEL DE RIESGO
  #==================================================
  
  output$graf_riesgo_planta <- renderPlotly({
    
    datos <- riesgo_operadores()
    
    req(nrow(datos) > 0)
    
    resumen <- datos %>%
      count(Nivel) %>%
      mutate(
        porcentaje = n / sum(n) * 100
      ) %>%
      mutate(
        Nivel = factor(
          Nivel,
          levels = c("Alto", "Medio", "Bajo")
        )
      ) %>%
      arrange(Nivel)
    
    plot_ly(
      data = resumen,
      labels = ~Nivel,
      values = ~porcentaje,
      type = "pie",
      
      textinfo = "label+percent",
      
      hovertemplate = paste(
        "<b>Nivel:</b> %{label}<br>",
        "<b>Operadores:</b> %{customdata}<br>",
        "<b>Porcentaje:</b> %{percent}",
        "<extra></extra>"
      ),
      
      customdata = ~n,
      
      marker = list(
        colors = c(
          "#D32F2F",  # Alto - Rojo
          "#FBC02D",  # Medio - Amarillo
          "#388E3C"   # Bajo - Verde
        ),
        line = list(
          color = "#FFFFFF",
          width = 2
        )
      )
      
    ) %>%
      layout(
        title = "Distribución de la Planta por Nivel de Riesgo",
        showlegend = TRUE
      )
    
  })
  
  #==========================================================
  # PIRÁMIDE DE RIESGO POTENCIAL
  #==========================================================
  
  output$graf_piramide_potencial <- renderUI({
    
    datos <- riesgo_potencial()
    
    req(nrow(datos) > 0)
    
    
    #--------------------------------------------------------
    # COLORES
    #--------------------------------------------------------
    
    colores <- c(
      "Alto"  = "#dc3545",
      "Medio" = "#ffc107",
      "Bajo"  = "#28a745"
    )
    
    
    #--------------------------------------------------------
    # FUNCIÓN PARA CREAR TARJETAS
    #--------------------------------------------------------
    
    crear_tarjetas <- function(datos_nivel, nivel){
      
      if(nrow(datos_nivel) == 0){
        return(NULL)
      }
      
      # Ordenar de mayor a menor score
      datos_nivel <- datos_nivel %>%
        dplyr::arrange(
          dplyr::desc(ScoreTotal)
        )
      
      
      #------------------------------------------------------
      # TOP 20 POR NIVEL
      #------------------------------------------------------
      
      datos_nivel <- datos_nivel %>%
        dplyr::slice_head(n = 30)
      
      
      #------------------------------------------------------
      # CREAR TARJETAS
      #------------------------------------------------------
      
      tarjetas <- lapply(
        
        seq_len(nrow(datos_nivel)),
        
        function(i){
          
          op <- datos_nivel[i, ]
          
          
          #----------------------------------------------
          # INFORMACIÓN TELEMETRÍA
          #----------------------------------------------
          
          texto_telemetria <- paste(
            "Telemetría:",
            paste0(
              names(op)[names(op) %in%
                          c("EV19", "ALA1", "ALA2", "ALA3")],
              op[1, names(op)[names(op) %in%
                                c("EV19", "ALA1", "ALA2", "ALA3")]],
              sep = ": ",
              collapse = " | "
            )
          )
          
          
          #----------------------------------------------
          # INFORMACIÓN ACCIDENTES
          #----------------------------------------------
          
          texto_accidentes <- paste(
            "Accidentes:",
            paste0(
              names(op)[names(op) %in%
                          c("Simples", "Lesionados", "Fatales")],
              op[1, names(op)[names(op) %in%
                                c("Simples", "Lesionados", "Fatales")]],
              sep = ": ",
              collapse = " | "
            )
          )
          
          
          #----------------------------------------------
          # RESPONSABILIDAD
          #----------------------------------------------
          
          responsabilidad <- if(
            "Responsabilidad" %in% names(op)
          ){
            as.character(op$Responsabilidad)
          } else {
            "No disponible"
          }
          
          
          #--------------------------------------------------------------
          # TOOLTIP
          #--------------------------------------------------------------
          
          tooltip <- paste0(
            
            "Operador: ", op$CodigoOperador,
            "\nNivel: ", op$Nivel,
            "\nScore Total: ", round(op$ScoreTotal, 2),
            
            "\n\n📡 TELEMETRÍA (",
            round(op$ScoreTelemetria, 2),
            " pts)",
            
            if(op$EV19 > 0)
              paste0(
                "\n• EV19 (Sin mirar al frente): ",
                op$EV19
              )
            else "",
            
            if(op$ALA1 > 0)
              paste0(
                "\n• ALA1 (Aceleración brusca): ",
                op$ALA1
              )
            else "",
            
            if(op$ALA2 > 0)
              paste0(
                "\n• ALA2 (Frenada brusca): ",
                op$ALA2
              )
            else "",
            
            if(op$ALA3 > 0)
              paste0(
                "\n• ALA3 (Exceso velocidad): ",
                op$ALA3
              )
            else "",
            
            
            "\n\n⚠ ACCIDENTES (",
            round(op$ScoreAccidentes, 2),
            " pts)",
            
            if(op$Simples > 0)
              paste0(
                "\n• Simples: ",
                op$Simples
              )
            else "",
            
            if(op$Lesionados > 0)
              paste0(
                "\n• Lesionados: ",
                op$Lesionados
              )
            else "",
            
            if(op$Fatales > 0)
              paste0(
                "\n• Fatalidades: ",
                op$Fatales
              )
            else "",
            
            "\n\nResponsabilidad Green Móvil: ",
            op$Responsables,
            
            "\n\n🚨 INFRACCIONES (",
            round(op$ScoreInfracciones, 2),
            " pts)",
            
            if(op$Tipo_I > 0)
              paste0(
                "\n• Tipo I: ",
                op$Tipo_I
              )
            else "",
            
            if(op$Tipo_II > 0)
              paste0(
                "\n• Tipo II: ",
                op$Tipo_II
              )
            else "",
            
            if(op$Tipo_III > 0)
              paste0(
                "\n• Tipo III: ",
                op$Tipo_III
              )
            else ""
            
          )
          
          #----------------------------------------------
          # TARJETA
          #----------------------------------------------
          
          tags$div(
            
            title = tooltip,
            
            style = paste0(
              "background:", colores[nivel], ";",
              "color:white;",
              "padding:7px;",
              "margin:4px;",
              "border-radius:8px;",
              "font-weight:bold;",
              "width:85px;",
              "text-align:center;",
              "display:inline-block;",
              "box-shadow:2px 2px 5px #999;",
              "font-size:12px;"
            ),
            
            op$CodigoOperador
            
          )
          
        }
        
      )
      
      
      #------------------------------------------------------
      # CONTENEDOR DEL NIVEL
      #------------------------------------------------------
      
      tags$div(
        
        style = "text-align:center;",
        
        tarjetas
        
      )
      
    }
    
    
    #========================================================
    # CONSTRUIR LOS TRES NIVELES
    #========================================================
    
    tags$div(
      
      style = "
      width:100%;
      text-align:center;
      padding:15px 0 25px 0;
    ",
      
      
      #------------------------------------------------------
      # ALTO - PUNTA
      #------------------------------------------------------
      
      tags$div(
        
        style = "
        width:30%;
        min-width:250px;
        margin:0 auto;
        padding:8px 0;
        background:rgba(220,53,69,0.08);
        border-radius:12px 12px 0 0;
      ",
        
        tags$div(
          style = "
          font-weight:bold;
          font-size:15px;
          margin-bottom:5px;
          color:#dc3545;
        ",
          "🔴 ALTO"
        ),
        
        crear_tarjetas(
          datos %>%
            dplyr::filter(Nivel == "Alto"),
          "Alto"
        )
        
      ),
      
      
      #------------------------------------------------------
      # MEDIO - ZONA INTERMEDIA
      #------------------------------------------------------
      
      tags$div(
        
        style = "
        width:60%;
        min-width:450px;
        margin:0 auto;
        padding:8px 0;
        background:rgba(255,193,7,0.08);
      ",
        
        tags$div(
          style = "
          font-weight:bold;
          font-size:15px;
          margin-bottom:5px;
          color:#d39e00;
        ",
          "🟡 MEDIO"
        ),
        
        crear_tarjetas(
          datos %>%
            dplyr::filter(Nivel == "Medio"),
          "Medio"
        )
        
      ),
      
      
      #------------------------------------------------------
      # BAJO - BASE
      #------------------------------------------------------
      
      tags$div(
        
        style = "
        width:100%;
        margin:0 auto;
        padding:8px 0;
        background:rgba(40,167,69,0.08);
        border-radius:0 0 12px 12px;
      ",
        
        tags$div(
          style = "
          font-weight:bold;
          font-size:15px;
          margin-bottom:5px;
          color:#28a745;
        ",
          "🟢 BAJO"
        ),
        
        crear_tarjetas(
          datos %>%
            dplyr::filter(Nivel == "Bajo"),
          "Bajo"
        )
        
      )
      
    )
    
  })
  
  #==========================================================
  # PIRÁMIDE DE RIESGO MATERIALIZADO
  #==========================================================
  
  output$graf_piramide_materializado <- renderUI({
    
    datos <- riesgo_materializado()
    
    req(nrow(datos) > 0)
    
    
    #--------------------------------------------------------
    # COLORES
    #--------------------------------------------------------
    
    colores <- c(
      "Alto"  = "#dc3545",
      "Medio" = "#ffc107",
      "Bajo"  = "#28a745"
    )
    
    
    #--------------------------------------------------------
    # FUNCIÓN PARA CREAR TARJETAS
    #--------------------------------------------------------
    
    crear_tarjetas <- function(datos_nivel, nivel){
      
      if(nrow(datos_nivel) == 0){
        return(NULL)
      }
      
      
      # Ordenar de mayor a menor Score
      datos_nivel <- datos_nivel %>%
        dplyr::arrange(
          dplyr::desc(ScoreTotal)
        )
      
      
      #------------------------------------------------------
      # MÁXIMO 20 OPERADORES POR NIVEL
      #------------------------------------------------------
      
      datos_nivel <- datos_nivel %>%
        dplyr::slice_head(n = 20)
      
      
      #------------------------------------------------------
      # CREAR TARJETAS
      #------------------------------------------------------
      
      tarjetas <- lapply(
        
        seq_len(nrow(datos_nivel)),
        
        function(i){
          
          op <- datos_nivel[i, ]
          
          
          #--------------------------------------------------
          # TOOLTIP
          #--------------------------------------------------
          
          tooltip <- paste0(
            
            "Operador: ", op$CodigoOperador,
            "\nNivel: ", op$Nivel,
            "\nScore Total: ", round(op$ScoreTotal, 2),
            
            "\n\n📡 TELEMETRÍA (",
            round(op$ScoreTelemetria, 2),
            " pts)",
            
            if(op$EV19 > 0)
              paste0(
                "\n• EV19 (Sin mirar al frente): ",
                op$EV19
              )
            else "",
            
            if(op$ALA1 > 0)
              paste0(
                "\n• ALA1 (Aceleración brusca): ",
                op$ALA1
              )
            else "",
            
            if(op$ALA2 > 0)
              paste0(
                "\n• ALA2 (Frenada brusca): ",
                op$ALA2
              )
            else "",
            
            if(op$ALA3 > 0)
              paste0(
                "\n• ALA3 (Exceso velocidad): ",
                op$ALA3
              )
            else "",
            
            
            "\n\n⚠ ACCIDENTES (",
            round(op$ScoreAccidentes, 2),
            " pts)",
            
            if(op$Simples > 0)
              paste0(
                "\n• Simples: ",
                op$Simples
              )
            else "",
            
            if(op$Lesionados > 0)
              paste0(
                "\n• Lesionados: ",
                op$Lesionados
              )
            else "",
            
            if(op$Fatales > 0)
              paste0(
                "\n• Fatalidades: ",
                op$Fatales
              )
            else "",
            
            
            "\n\nResponsabilidad Green Móvil: ",
            op$Responsables
            
          )
          
          
          #--------------------------------------------------
          # TARJETA
          #--------------------------------------------------
          
          tags$div(
            
            title = tooltip,
            
            style = paste0(
              "background:", colores[nivel], ";",
              "color:white;",
              "padding:7px;",
              "margin:4px;",
              "border-radius:8px;",
              "font-weight:bold;",
              "width:85px;",
              "text-align:center;",
              "display:inline-block;",
              "box-shadow:2px 2px 5px #999;",
              "font-size:12px;"
            ),
            
            op$CodigoOperador
            
          )
          
        }
        
      )
      
      
      #------------------------------------------------------
      # CONTENEDOR DEL NIVEL
      #------------------------------------------------------
      
      tags$div(
        
        style = "text-align:center;",
        
        tarjetas
        
      )
      
    }
    
    
    #========================================================
    # CONSTRUIR PIRÁMIDE
    #========================================================
    
    tags$div(
      
      style = "
      width:100%;
      text-align:center;
      padding:15px 0 25px 0;
    ",
      
      
      #------------------------------------------------------
      # ALTO - PUNTA
      #------------------------------------------------------
      
      tags$div(
        
        style = "
        width:30%;
        min-width:250px;
        margin:0 auto;
        padding:8px 0;
        background:rgba(220,53,69,0.08);
        border-radius:12px 12px 0 0;
      ",
        
        tags$div(
          
          style = "
          font-weight:bold;
          font-size:15px;
          margin-bottom:5px;
          color:#dc3545;
        ",
          
          "🔴 ALTO"
          
        ),
        
        crear_tarjetas(
          
          datos %>%
            dplyr::filter(Nivel == "Alto"),
          
          "Alto"
          
        )
        
      ),
      
      
      #------------------------------------------------------
      # MEDIO - ZONA INTERMEDIA
      #------------------------------------------------------
      
      tags$div(
        
        style = "
        width:60%;
        min-width:450px;
        margin:0 auto;
        padding:8px 0;
        background:rgba(255,193,7,0.08);
      ",
        
        tags$div(
          
          style = "
          font-weight:bold;
          font-size:15px;
          margin-bottom:5px;
          color:#d39e00;
        ",
          
          "🟡 MEDIO"
          
        ),
        
        crear_tarjetas(
          
          datos %>%
            dplyr::filter(Nivel == "Medio"),
          
          "Medio"
          
        )
        
      ),
      
      
      #------------------------------------------------------
      # BAJO - BASE
      #------------------------------------------------------
      
      tags$div(
        
        style = "
        width:100%;
        margin:0 auto;
        padding:8px 0;
        background:rgba(40,167,69,0.08);
        border-radius:0 0 12px 12px;
      ",
        
        tags$div(
          
          style = "
          font-weight:bold;
          font-size:15px;
          margin-bottom:5px;
          color:#28a745;
        ",
          
          "🟢 BAJO"
          
        ),
        
        crear_tarjetas(
          
          datos %>%
            dplyr::filter(Nivel == "Bajo"),
          
          "Bajo"
          
        )
        
      )
      
    )
    
  })
  
} 
