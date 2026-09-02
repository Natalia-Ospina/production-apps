
ui <- bs4DashPage(
  
  tags$head(
    
    tags$title("Pirámide Bird"),
    
    tags$link(
      rel = "icon",
      type = "image/png",
      href = "favicon.png"
    ),
    
    tags$style(HTML("
    
    .sidebar-bottom {
      position: absolute;
      bottom: 0;
      left: 0;
      width: 100%;
      background-color: white;
      padding-top: 5px;
    }
    
  "))
  ),
  
  # HEADER
  
  header = bs4DashNavbar(
    title = "Pirámide Bird"
  ),
  
  #=========================================================
  # SIDEBAR
  #=========================================================
  
  sidebar = bs4DashSidebar(
    
    skin = "light",
    
    status = "primary",
    
    elevation = 3,
    
    # ==========================================
    # MENÚ
    # ==========================================
    
    bs4SidebarMenu(
      
      bs4SidebarMenuItem(
        text = "Dashboard",
        tabName = "dashboard",
        icon = icon("chart-line")
      ),
      
      bs4SidebarMenuItem(
        text = "Operadores",
        tabName = "operadores",
        icon = icon("users")
      ),
      
      bs4SidebarMenuItem(
        text = "Pirámides",
        tabName = "piramides",
        icon = icon("triangle-exclamation")
      )
      
    ),
    
    # ==========================================
    # USUARIO + CERRAR SESIÓN + COPYRIGHT
    # ==========================================
    
    div(
      class = "sidebar-bottom",
      
      uiOutput("info_usuario"),
      
      actionButton(
        "btn_logout",
        "Cerrar Sesión",
        icon = icon("sign-out-alt"),
        class = "btn-danger",
        style = "
      width: 90%;
      margin: 5px 5% 10px 5%;
    "
      ),
      
      div(
        HTML("© 2026 Calidad de la Operación | Operaciones<br>Green Móvil S.A.S."),
        style = "
      text-align: center;
      font-size: 9px;
      color: #6c757d;
      padding: 5px 8px 12px 8px;
      line-height: 1.3;
    "
      )
    )
    
  ),
  
  #=========================================================
  # BODY
  #=========================================================
  
  body = bs4DashBody(
    
    #=======================================================
    # ESTILOS CONFIGURACIÓN DEL MODELO
    #=======================================================
    
    tags$head(
      
      tags$style(HTML("

      /* ==========================================
         SLIDERS COMPACTOS
         ========================================== */

      .config-modelo .form-group {
        margin-bottom: 4px !important;
      }

      .config-modelo .control-label {
        font-size: 13px !important;
        font-weight: 600 !important;
        margin-bottom: 2px !important;
      }

      /* Títulos de las secciones de configuración */

      .config-modelo h4,
      .config-modelo h5 {
        font-size: 12px !important;
        font-weight: bold !important;
        margin-top: 5px !important;
        margin-bottom: 6px !important;
      }

      .config-modelo .irs {
        height: 22px !important;
        margin-top: -2px !important;
        margin-bottom: 0px !important;
      }

      .config-modelo .irs-line {
        top: 9px !important;
        height: 4px !important;
      }

      .config-modelo .irs-bar {
        top: 9px !important;
        height: 4px !important;
      }

      .config-modelo .irs-handle {
        top: 6px !important;
        width: 10px !important;
        height: 10px !important;
        border-radius: 50% !important;
      }

      .config-modelo .irs-single,
      .config-modelo .irs-from,
      .config-modelo .irs-to {
        font-size: 10px !important;
        line-height: 11px !important;
        padding: 1px 3px !important;
        top: -10px !important;
      }

      .irs-min,
      .irs-max {
        font-size: 10px !important;
        color: #333333 !important;
        font-weight: 500 !important;
        background: transparent !important;
        line-height: 14px !important;
        top: -4px !important;
      }

      .config-modelo .irs-grid {
        display: none !important;
      }

    "))
      
    ),
    
    
    #=======================================================
    # FILTROS GENERALES
    #=======================================================
    
    fluidRow(
      
      box(
        
        width = 12,
        
        title = "Filtros Generales",
        
        status = "primary",
        
        solidHeader = TRUE,
        
        collapsible = TRUE,
        
        fluidRow(
          
          column(
            2,
            
            dateRangeInput(
              "filtro_fecha",
              "Fecha",
              start = fecha_min,
              end = fecha_max,
              min = fecha_min,
              max = fecha_max
            )
            
          ),
          
          column(
            2,
            
            selectInput(
              "filtro_empresa",
              "Empresa",
              choices = c("Todos")
            )
            
          ),
          
          column(
            2,
            
            selectInput(
              "filtro_tipo_accidente",
              "Tipo de Accidente",
              choices = c("Todos")
            )
            
          ),
          
          column(
            2,
            
            selectInput(
              "filtro_responsabilidad",
              "Responsabilidad",
              choices = c("Todos")
            )
            
          ),
          
          column(
            2,
            
          selectInput(
            "filtro_tipo_infraccion",
            "Tipo de Infracción",
            choices = c("Todos")
            )
          
          ),
          
          column(
            2,
            
            selectInput(
              "filtro_tabla",
              "Tipo Evento",
              choices = c("Todos")
            )
            
          ),
          
          column(
            2,
            
            selectInput(
              "filtro_nivel",
              "Nivel",
              choices = c(
                "Todos",
                "Alto",
                "Medio",
                "Bajo"
              )
            )
          ),
            
          column(
            2,
            
            selectizeInput(
              "filtro_operador",
              "Operador",
              choices = c("Todos"),
              selected = "Todos",
              multiple = FALSE,
              options = list(
                placeholder = "Seleccione o escriba el código",
                allowEmptyOption = TRUE,
                create = FALSE
              )
            )
          )
            
        ),
        
        br(),
        
        fluidRow(
          
          column(
            3,
            
            selectInput(
              "top_n",
              "Top Operadores",
              
              choices = c(
                "5",
                "10",
                "15",
                "20",
                "50",
                "100",
                "Todos"
              ),
              
              selected = "10"
              
            )
            
          )
          
        )
        
      )
      
    ),
    
    
    #=======================================================
    # CONVENCIONES
    #=======================================================
    
    box(
      
      title = "Convenciones",
      
      width = 12,
      
      status = "primary",
      
      solidHeader = TRUE,
      
      collapsible = TRUE,
      
      collapsed = FALSE,
      
      div(
        
        style = "font-size:13px; line-height:1.6;",
        
        tags$b("🚗 Telemetría:"),
        
        tags$span(
          
          HTML(
            "&nbsp;&nbsp;
          <b>EV19-1</b>: Sin mirar al frente &nbsp;&nbsp;|&nbsp;&nbsp;
          <b>ALA1</b>: Aceleración brusca &nbsp;&nbsp;|&nbsp;&nbsp;
          <b>ALA2</b>: Frenada brusca &nbsp;&nbsp;|&nbsp;&nbsp;
          <b>ALA3</b>: Exceso de velocidad"
          )
          
        ),
        
        tags$br(),
        
        tags$b("⚠ Accidentes:"),
        
        tags$span(
          
          HTML(
            "&nbsp;&nbsp;
          <b>Simple</b>: Daños materiales &nbsp;&nbsp;|&nbsp;&nbsp;
          <b>Lesionados</b>: Accidente con lesionados &nbsp;&nbsp;|&nbsp;&nbsp;
          <b>Fatalidad</b>: Accidente fatal"
          )
          
        ),
        
        tags$br(),
        
        tags$b("📊 Nivel de Riesgo:"),
        
        tags$span(
          
          HTML(
            "&nbsp;&nbsp;
          <span style='color:#28a745;font-weight:bold;'>● Bajo</span>
          &nbsp;&nbsp;
          <span style='color:#ffc107;font-weight:bold;'>● Medio</span>
          &nbsp;&nbsp;
          <span style='color:#dc3545;font-weight:bold;'>● Alto</span>"
          )
          
        )
        
      )
      
    ),
    
    bs4TabItems(
      
      #=========================================================
      # DASHBOARD
      #=========================================================
      
      bs4TabItem(
        
        tabName = "dashboard",
        
        fluidRow(
          
          valueBoxOutput("kpi_eventos"),
          valueBoxOutput("kpi_accidentes"),
          valueBoxOutput("kpi_responsables"),
          valueBoxOutput("kpi_infracciones"),
          valueBoxOutput("kpi_operadores")
          
        ),
        
        fluidRow(
          
          box(
            
            width = 6,
            title = "Eventos de Telemetría",
            status = "primary",
            solidHeader = TRUE,
            
            plotlyOutput(
              "graf_eventos",
              height = "350px"
            )
            
          ),
          
          box(
            
            width = 6,
            title = "Tendencia de eventos de Telemetría",
            status = "primary",
            solidHeader = TRUE,
            
            plotlyOutput(
              "graf_tendencia",
              height = "350px"
            )
            
          ),
          
        fluidRow(
            
          box(
            
            width = 6,
            title = "Accidentes",
            status = "danger",
            solidHeader = TRUE,
            
            plotlyOutput(
              "graf_accidentes",
              height = "350px"
            )
            
          ),
          
          box(
            width = 6,
            title = "Tendencia de Accidentes",
            status = "danger",
            solidHeader = TRUE,
            
            plotlyOutput(
              "graf_tendencia_accidentes",
              height = "350px"
            )
          ),
        
          box(
            
            width = 6,
            title = "Infracciones",
            status = "warning",
            solidHeader = TRUE,
            
            plotlyOutput(
              "graf_infracciones",
              height = "350px"
            )
            
          ),
          
          box(
            width = 6,
            title = "Tendencia de Infracciones",
            status = "warning",
            solidHeader = TRUE,
            
            plotlyOutput(
              "graf_tendencia_infracciones",
              height = "350px"
            )
          ),
          
        
          box(
            
            width = 6,
            title = "Ranking de Operadores",
            status = "success",
            solidHeader = TRUE,
            
            plotlyOutput(
              "graf_operadores",
              height = "350px"
            )
            
          ),   
          
          box(
            width = 6,
            title = "Distribución de la Planta por Nivel de Riesgo",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput(
              "graf_riesgo_planta",
              height = "350px"
            )
          )  
        )
        
      )
      ),
      
      
      #=========================================================
      # OPERADORES
      #=========================================================
      
      bs4TabItem(
        
        tabName = "operadores",
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Ranking de Riesgo de Operadores",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            DTOutput("tabla_riesgo")
            
          )
          
        )
        
      ),
      
      
      #=========================================================
      # PIRÁMIDES
      #=========================================================
      
      bs4TabItem(
        
        tabName = "piramides",
        
        
        #=======================================================
        # CONFIGURACIÓN DEL MODELO
        #=======================================================
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "⚙ Configuración del modelo",
            
            status = "info",
            
            solidHeader = TRUE,
            
            collapsible = TRUE,
            
            collapsed = FALSE,
            
            class = "config-modelo",
            
            style = "margin-bottom: 8px;",
            
            fluidRow(
              
              #-------------------------------------------------
              # TELEMETRÍA
              #-------------------------------------------------
              
              column(
                
                width = 4,
                
                tags$div(
                  style = "font-weight:bold; font-size:13px; margin-bottom:3px;",
                  "Telemetría"
                ),
                
                sliderInput(
                  "peso_ev19_piramide",
                  "EV19",
                  min = 0,
                  max = 20,
                  value = 1,
                  step = 1,
                  width = "95%"
                ),
                
                sliderInput(
                  "peso_ala1_piramide",
                  "ALA1",
                  min = 0,
                  max = 20,
                  value = 3,
                  step = 1,
                  width = "95%"
                ),
                
                sliderInput(
                  "peso_ala2_piramide",
                  "ALA2",
                  min = 0,
                  max = 20,
                  value = 6,
                  step = 1,
                  width = "95%"
                ),
                
                sliderInput(
                  "peso_ala3_piramide",
                  "ALA3",
                  min = 0,
                  max = 20,
                  value = 10,
                  step = 1,
                  width = "95%"
                )
                
              ),
              
              
              #-------------------------------------------------
              # ACCIDENTES
              #-------------------------------------------------
              
              column(
                
                width = 4,
                
                tags$div(
                  style = "font-weight:bold; font-size:13px; margin-bottom:3px;",
                  "Accidentes"
                ),
                
                sliderInput(
                  "peso_simple_piramide",
                  "Simple",
                  min = 0,
                  max = 20,
                  value = 1,
                  step = 1,
                  width = "95%"
                ),
                
                sliderInput(
                  "peso_lesionados_piramide",
                  "Lesionados",
                  min = 0,
                  max = 30,
                  value = 3,
                  step = 1,
                  width = "95%"
                ),
                
                sliderInput(
                  "peso_fatalidad_piramide",
                  "Fatalidad",
                  min = 0,
                  max = 50,
                  value = 18,
                  step = 1,
                  width = "95%"
                ),
                
                sliderInput(
                  "peso_responsabilidad_piramide",
                  "Responsabilidad",
                  min = 0,
                  max = 20,
                  value = 5,
                  step = 1,
                  width = "95%"
                )
                
              ),
              
              #-------------------------------------------------
              # INFRACCIONES
              #-------------------------------------------------
              
              column(
                
                width = 4,
                
                tags$div(
                  style = "font-weight:bold; font-size:13px; margin-bottom:3px;",
                  "Infracciones"
                ),
                
                sliderInput(
                  "peso_tipo_I_piramide",
                  "Tipo_I",
                  min = 0,
                  max = 50,
                  value = 10,
                  step = 1,
                  width = "95%"
                ),
                
                sliderInput(
                  "peso_tipo_II_piramide",
                  "Tipo_II",
                  min = 0,
                  max = 50,
                  value = 15,
                  step = 1,
                  width = "95%"
                ),
                
                sliderInput(
                  "peso_tipo_III_piramide",
                  "Tipo_III",
                  min = 0,
                  max = 50,
                  value = 30,
                  step = 1,
                  width = "95%"
                )
                
              ),
              
              #-------------------------------------------------
              # PERCENTILES
              #-------------------------------------------------
              
              column(
                
                width = 4,
                
                tags$div(
                  style = "font-weight:bold; font-size:13px; margin-bottom:3px;",
                  "Clasificación del riesgo"
                ),
                
                sliderInput(
                  "percentil_medio_piramide",
                  "Riesgo Medio",
                  min = 40,
                  max = 95,
                  value = 70,
                  step = 1,
                  width = "95%"
                ),
                
                sliderInput(
                  "percentil_alto_piramide",
                  "Riesgo Alto",
                  min = 60,
                  max = 99,
                  value = 90,
                  step = 1,
                  width = "95%"
                )
                
              )
              
            )
            
          )
          
        ),
        
        
        #=======================================================
        # PIRÁMIDE POTENCIAL
        #=======================================================
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Pirámide de Riesgo Potencial",
            
            status = "warning",
            
            solidHeader = TRUE,
            
            p(
              "Operadores con eventos de telemetría y/o accidentes sin responsabilidad Green Móvil."
            ),
            
            uiOutput(
              "graf_piramide_potencial"
            )
            
          )
          
        ),
        
        
        #=======================================================
        # PIRÁMIDE MATERIALIZADA
        #=======================================================
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Pirámide de Riesgo Materializado",
            
            status = "danger",
            
            solidHeader = TRUE,
            
            p(
              "Operadores con accidentes donde Green Móvil fue responsable."
            ),
            
            uiOutput(
              "graf_piramide_materializado"
            )
            
          )
          
        )
        
      )   # ← cierra bs4TabItem("piramides")
      
    )     # ← cierra bs4TabItems()
    
  ),      # ← cierra bs4DashBody()
  
  #=========================================================
  # FOOTER / CONTROLBAR
  #=========================================================
  
  controlbar = bs4DashControlbar(),
  
  footer = bs4DashFooter()
  
)         # ← cierra bs4DashPage()
