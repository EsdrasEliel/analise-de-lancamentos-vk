# Instalação dos pacotes necessários
 install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "plotly", "DT", "shinyWidgets"))

# Carregando bibliotecas
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(shinyWidgets)

# Criando dados simulados baseados no relatório
set.seed(123)

# Dados de campanhas
campanhas <- data.frame(
  campanha = c("AD17", "AD19", "AD35", "AD63", "AD44", "AD28", "AD52", "AD73"),
  leads = c(3628, 1719, 369, 369, 300, 1250, 875, 1485),
  investimento = c(7320, 3390, 1090, 2150, 1800, 2800, 2200, 3100),
  conversoes = c(26, 13, 3, 2, 1, 9, 6, 11),
  cpm = c(427.81, 276.74, 258.15, 310.20, 285.35, 320.45, 295.70, 345.60),
  cpc = c(37.28, 23.50, 14.23, 18.75, 16.45, 22.30, 19.85, 24.75),
  cpl = c(2.02, 1.97, 2.95, 5.83, 6.00, 2.24, 2.51, 2.09),
  canal = sample(c("Facebook", "YouTube", "Instagram", "Google Ads", "TikTok"), 8, replace = TRUE)
)

# Dados de canais
canais <- data.frame(
  canal = c("Facebook Ads", "YouTube", "Shorts", "Manychat", "Google Ads", "Bio", "Direct", "Outros"),
  leads = c(8400, 320, 280, 600, 300, 300, 150, 245),
  taxa_conversao = c(0.8, 17.8, 14.2, 2.3, 1.9, 2.5, 5.5, 1.7)
)

# Dados demográficos
demograficos <- data.frame(
  faixa_etaria = c("18-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56+"),
  respostas = c(85, 125, 161, 210, 256, 221, 170, 76),
  genero = sample(c("Masculino", "Feminino"), 8, replace = TRUE, prob = c(0.45, 0.55))
)

# Dados de renda
renda <- data.frame(
  faixa = c("Até R$ 1.500", "R$ 1.500-3.000", "R$ 3.000-5.000", "Acima de R$ 5.000"),
  porcentagem = c(53.24, 26.16, 12.74, 7.86)
)

# Dados de tempo conhecendo marca
tempo_marca <- data.frame(
  tempo = c("Menos de 1 mês", "1-3 meses", "3-6 meses", "6-12 meses", "1-2 anos", "Mais de 2 anos"),
  respostas = c(798, 165, 98, 56, 37, 150)
)

# Definição da UI do Shiny Dashboard
ui <- dashboardPage(
  skin = "blue",
  
  # Cabeçalho
  dashboardHeader(title = "Análise de Lançamentos Digitais - VK", 
                  titleWidth = 350),
  
  # Barra lateral com menus
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Visão Geral", tabName = "overview", icon = icon("dashboard")),
      menuItem("Análise de Campanhas", tabName = "campaigns", icon = icon("ad")),
      menuItem("Funil por Canal", tabName = "channels", icon = icon("filter")),
      menuItem("Perfil da Audiência", tabName = "audience", icon = icon("users")),
      menuItem("Simulador de ROI", tabName = "simulator", icon = icon("calculator"))
    )
  ),
  
  # Corpo do dashboard
  dashboardBody(
    tabItems(
      # Aba Visão Geral
      tabItem(tabName = "overview",
              fluidRow(
                valueBox(9995, "Total de Leads", icon = icon("users"), color = "blue", width = 3),
                valueBox(71, "Vendas Realizadas", icon = icon("shopping-cart"), color = "green", width = 3),
                valueBox("0,71%", "Taxa Conversão", icon = icon("percentage"), color = "red", width = 3),
                valueBox("R$ 18.050", "Investimento Total", icon = icon("money-bill"), color = "purple", width = 3)
              ),
              fluidRow(
                box(
                  title = "Performance dos Lançamentos",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("overview_performance", height = 300)
                )
              ),
              fluidRow(
                box(
                  title = "Eficiência vs. Investimento",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("overview_efficiency", height = 300)
                ),
                box(
                  title = "Conversão por Canal",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("overview_channels", height = 300)
                )
              )
      ),
      
      # Aba Análise de Campanhas
      tabItem(tabName = "campaigns",
              fluidRow(
                box(
                  title = "Filtros",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  column(4, selectInput("canal_filter", "Canal:", 
                                        c("Todos", unique(campanhas$canal)))),
                  column(4, sliderInput("invest_filter", "Investimento:", 
                                        min = 0, max = 10000, value = c(0, 10000))),
                  column(4, sliderInput("cpl_filter", "CPL:", 
                                        min = 0, max = 10, value = c(0, 10)))
                )
              ),
              fluidRow(
                box(
                  title = "Detalhe das Campanhas",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("campaign_table")
                )
              ),
              fluidRow(
                box(
                  title = "Volume de Leads por Campanha",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("campaign_leads", height = 300)
                ),
                box(
                  title = "Custo por Lead (CPL)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("campaign_cpl", height = 300)
                )
              )
      ),
      
      # Aba Funil por Canal
      tabItem(tabName = "channels",
              fluidRow(
                box(
                  title = "Distribuição de Leads por Canal",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("channel_distribution", height = 300)
                ),
                box(
                  title = "Taxa de Conversão por Canal",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("channel_conversion", height = 300)
                )
              ),
              fluidRow(
                box(
                  title = "Análise do Funil de Conversão",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("funnel_analysis", height = 300)
                )
              )
      ),
      
      # Aba Perfil da Audiência
      tabItem(tabName = "audience",
              fluidRow(
                tabBox(
                  title = "Dados Demográficos",
                  width = 12,
                  tabPanel("Faixa Etária", plotlyOutput("demographic_age", height = 300)),
                  tabPanel("Renda", plotlyOutput("demographic_income", height = 300)),
                  tabPanel("Tempo de Relacionamento", plotlyOutput("demographic_time", height = 300))
                )
              ),
              fluidRow(
                box(
                  title = "Segmentação da Audiência",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("audience_table")
                )
              )
      ),
      
      # Aba Simulador de ROI
      tabItem(tabName = "simulator",
              fluidRow(
                box(
                  title = "Simulador de Campanhas",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(4, 
                           numericInput("sim_budget", "Orçamento (R$):", 5000, min = 1000, max = 100000),
                           selectInput("sim_channel", "Canal Principal:", 
                                       c("Facebook", "YouTube", "Instagram", "Google Ads")),
                           numericInput("sim_cpl", "CPL Esperado (R$):", 2.00, min = 0.5, max = 10)
                    ),
                    column(4,
                           sliderInput("sim_conv_rate", "Taxa de Conversão (%):", 0.7, min = 0.1, max = 10, step = 0.1),
                           numericInput("sim_ticket", "Ticket Médio (R$):", 997, min = 100, max = 10000),
                           checkboxInput("sim_upsell", "Incluir Upsell", TRUE)
                    ),
                    column(4,
                           br(),
                           actionButton("run_sim", "Calcular Projeção", 
                                        class = "btn-lg btn-primary",
                                        style = "width: 100%"),
                           br(), br(),
                           downloadButton("download_sim", "Exportar Projeção", 
                                          class = "btn-lg",
                                          style = "width: 100%")
                    )
                  )
                )
              ),
              fluidRow(
                valueBoxOutput("sim_leads", width = 3),
                valueBoxOutput("sim_sales", width = 3),
                valueBoxOutput("sim_revenue", width = 3),
                valueBoxOutput("sim_roi", width = 3)
              ),
              fluidRow(
                box(
                  title = "Projeção de Resultados",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("sim_projection", height = 300)
                )
              )
      )
    )
  )
)

# Definição do servidor Shiny
server <- function(input, output) {
  
  # ------ VISÃO GERAL ------
  
  # Gráfico de performance
  output$overview_performance <- renderPlotly({
    p <- ggplot(campanhas, aes(x = reorder(campanha, -leads), y = leads, fill = campanha)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = leads), vjust = -0.5) +
      theme_minimal() +
      labs(title = "Volume de Leads por Campanha", x = "Campanha", y = "Número de Leads") +
      theme(legend.position = "none")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Gráfico de eficiência vs investimento
  output$overview_efficiency <- renderPlotly({
    p <- ggplot(campanhas, aes(x = investimento, y = cpl, text = campanha, color = campanha)) +
      geom_point(size = 10, alpha = 0.7) +
      theme_minimal() +
      labs(title = "Eficiência vs. Investimento", 
           x = "Investimento (R$)", 
           y = "Custo por Lead (R$)") +
      theme(legend.position = "right")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Gráfico de conversão por canal
  output$overview_channels <- renderPlotly({
    p <- ggplot(canais, aes(x = reorder(canal, -taxa_conversao), y = taxa_conversao, fill = canal)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(taxa_conversao, "%")), vjust = -0.5) +
      theme_minimal() +
      labs(title = "Taxa de Conversão por Canal", x = "Canal", y = "Taxa de Conversão (%)") +
      theme(legend.position = "none") +
      coord_flip()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # ------ ANÁLISE DE CAMPANHAS ------
  
  # Filtrar dados de campanhas
  filtered_campaigns <- reactive({
    result <- campanhas
    
    if (input$canal_filter != "Todos") {
      result <- result %>% filter(canal == input$canal_filter)
    }
    
    result <- result %>%
      filter(investimento >= input$invest_filter[1] & investimento <= input$invest_filter[2]) %>%
      filter(cpl >= input$cpl_filter[1] & cpl <= input$cpl_filter[2])
    
    return(result)
  })
  
  # Tabela de campanhas
  output$campaign_table <- renderDT({
    datatable(
      filtered_campaigns() %>%
        select(campanha, leads, investimento, conversoes, cpl, canal) %>%
        mutate(
          taxa_conversao = paste0(round(conversoes/leads*100, 2), "%"),
          investimento = paste0("R$ ", format(investimento, big.mark = ".", decimal.mark = ",")),
          cpl = paste0("R$ ", format(cpl, big.mark = ".", decimal.mark = ","))
        ) %>%
        rename(
          "Campanha" = campanha,
          "Leads" = leads,
          "Investimento" = investimento,
          "Conversões" = conversoes,
          "CPL" = cpl,
          "Taxa Conv." = taxa_conversao,
          "Canal" = canal
        ),
      options = list(pageLength = 5, dom = 'ftip'),
      rownames = FALSE
    )
  })
  
  # Gráfico de leads por campanha
  output$campaign_leads <- renderPlotly({
    p <- ggplot(filtered_campaigns(), aes(x = reorder(campanha, -leads), y = leads, fill = canal)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Volume de Leads por Campanha", x = "Campanha", y = "Número de Leads")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Gráfico de CPL por campanha
  output$campaign_cpl <- renderPlotly({
    p <- ggplot(filtered_campaigns(), aes(x = reorder(campanha, cpl), y = cpl, fill = canal)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Custo por Lead (CPL)", x = "Campanha", y = "CPL (R$)") +
      coord_flip()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # ------ FUNIL POR CANAL ------
  
  # Gráfico de distribuição por canal
  output$channel_distribution <- renderPlotly({
    p <- plot_ly(canais, labels = ~canal, values = ~leads, type = 'pie',
                 textinfo = 'label+percent',
                 insidetextorientation = 'radial') %>%
      layout(title = "Distribuição de Leads por Canal")
    
    p %>% config(displayModeBar = FALSE)
  })
  
  # Gráfico de conversão por canal
  output$channel_conversion <- renderPlotly({
    p <- ggplot(canais, aes(x = reorder(canal, -taxa_conversao), y = taxa_conversao, fill = canal)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Taxa de Conversão por Canal", x = "Canal", y = "Taxa de Conversão (%)") +
      theme(legend.position = "none")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Gráfico de análise de funil
  output$funnel_analysis <- renderPlotly({
    # Criando dados para o funil
    funnel_data <- data.frame(
      stage = c("Impressões", "Cliques", "Leads", "Oportunidades", "Vendas"),
      value = c(1500000, 95000, 9995, 210, 71)
    )
    
    colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd')
    
    p <- plot_ly(
      funnel_data, 
      x = ~value, 
      y = ~stage, 
      type = "funnel",
      textposition = "inside",
      textinfo = "value+percent initial",
      opacity = 0.65,
      marker = list(color = colors),
      text = ~stage
    ) %>%
      layout(
        title = "Funil de Conversão",
        yaxis = list(categoryarray = ~stage)
      )
    
    p %>% config(displayModeBar = FALSE)
  })
  
  # ------ PERFIL DA AUDIÊNCIA ------
  
  # Gráfico de faixa etária
  output$demographic_age <- renderPlotly({
    p <- ggplot(demograficos, aes(x = faixa_etaria, y = respostas, fill = faixa_etaria)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Distribuição por Faixa Etária", x = "Faixa Etária", y = "Número de Respostas") +
      theme(legend.position = "none")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Gráfico de renda
  output$demographic_income <- renderPlotly({
    p <- plot_ly(renda, labels = ~faixa, values = ~porcentagem, type = 'pie',
                 textinfo = 'label+percent',
                 insidetextorientation = 'radial') %>%
      layout(title = "Distribuição por Faixa de Renda")
    
    p %>% config(displayModeBar = FALSE)
  })
  
  # Gráfico de tempo de relacionamento
  output$demographic_time <- renderPlotly({
    p <- ggplot(tempo_marca, aes(x = tempo, y = respostas, fill = tempo)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Tempo que Conhece a Marca", x = "Período", y = "Número de Respostas") +
      theme(legend.position = "none") +
      coord_flip()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Tabela de audiência
  output$audience_table <- renderDT({
    # Criar dados de segmentação de audiência
    segments <- data.frame(
      segmento = c("Iniciantes Curiosos", "Profissionais em Transição", "Empresários Estabelecidos", 
                   "Investidores Iniciantes", "Empreendedores Digitais"),
      idade = c("36-45", "41-50", "46-55", "31-40", "26-35"),
      renda = c("Até R$ 1.500", "R$ 1.500-3.000", "Acima de R$ 5.000", "R$ 3.000-5.000", "R$ 1.500-3.000"),
      taxa_conversao = c("0,5%", "0,9%", "2,1%", "1,2%", "1,8%"),
      interesses = c("Renda Extra", "Nova Carreira", "Crescimento", "Investimentos", "Negócios Online")
    )
    
    datatable(
      segments,
      options = list(pageLength = 5, dom = 'ft'),
      rownames = FALSE
    )
  })
  
  # ------ SIMULADOR DE ROI ------
  
  # Cálculos do simulador
  simulation_results <- eventReactive(input$run_sim, {
    # Cálculos básicos
    leads <- floor(input$sim_budget / input$sim_cpl)
    sales <- floor(leads * (input$sim_conv_rate / 100))
    base_revenue <- sales * input$sim_ticket
    
    # Adicionar upsell se selecionado
    if (input$sim_upsell) {
      upsell_rate <- 0.3 # 30% de taxa de upsell
      upsell_value <- input$sim_ticket * 0.4 # 40% do valor do ticket principal
      upsell_revenue <- sales * upsell_rate * upsell_value
      total_revenue <- base_revenue + upsell_revenue
    } else {
      total_revenue <- base_revenue
      upsell_revenue <- 0
    }
    
    # Calcular ROI
    roi <- (total_revenue - input$sim_budget) / input$sim_budget * 100
    
    # Criar dados de projeção para 6 meses
    months <- c("Mês 1", "Mês 2", "Mês 3", "Mês 4", "Mês 5", "Mês 6")
    
    # Aplicar fatores de crescimento mês a mês (simulação de otimização)
    lead_growth <- c(1, 1.05, 1.12, 1.18, 1.22, 1.25)
    conv_growth <- c(1, 1.08, 1.15, 1.20, 1.25, 1.30)
    
    monthly_leads <- floor(leads * lead_growth)
    monthly_conv_rate <- pmin(input$sim_conv_rate * conv_growth, 10) # limitar a 10%
    monthly_sales <- floor(monthly_leads * (monthly_conv_rate / 100))
    monthly_revenue <- monthly_sales * input$sim_ticket
    
    if (input$sim_upsell) {
      monthly_upsell <- monthly_sales * upsell_rate * upsell_value
      monthly_total <- monthly_revenue + monthly_upsell
    } else {
      monthly_total <- monthly_revenue
    }
    
    monthly_roi <- (monthly_total - input$sim_budget) / input$sim_budget * 100
    
    projections <- data.frame(
      month = months,
      leads = monthly_leads,
      conv_rate = monthly_conv_rate,
      sales = monthly_sales,
      revenue = monthly_total,
      roi = monthly_roi
    )
    
    return(list(
      leads = leads,
      sales = sales,
      revenue = total_revenue,
      roi = roi,
      projections = projections
    ))
  })
  
  # Output de caixas de valor
  output$sim_leads <- renderValueBox({
    sim <- simulation_results()
    valueBox(
      format(sim$leads, big.mark = ".", decimal.mark = ","),
      "Leads Estimados",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$sim_sales <- renderValueBox({
    sim <- simulation_results()
    valueBox(
      sim$sales,
      "Vendas Projetadas",
      icon = icon("shopping-cart"),
      color = "green"
    )
  })
  
  output$sim_revenue <- renderValueBox({
    sim <- simulation_results()
    valueBox(
      paste0("R$ ", format(round(sim$revenue), big.mark = ".", decimal.mark = ",")),
      "Receita Total",
      icon = icon("dollar-sign"),
      color = "purple"
    )
  })
  
  output$sim_roi <- renderValueBox({
    sim <- simulation_results()
    roi_value <- round(sim$roi, 1)
    roi_color <- ifelse(roi_value < 0, "red", ifelse(roi_value < 100, "yellow", "green"))
    
    valueBox(
      paste0(format(roi_value, big.mark = ".", decimal.mark = ","), "%"),
      "ROI Estimado",
      icon = icon("chart-line"),
      color = roi_color
    )
  })
  
  # Gráfico de projeção
  output$sim_projection <- renderPlotly({
    sim <- simulation_results()
    
    p <- plot_ly() %>%
      add_trace(
        x = sim$projections$month,
        y = sim$projections$revenue,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Receita',
        line = list(color = 'rgb(49,130,189)', width = 4)
      ) %>%
      add_trace(
        x = sim$projections$month,
        y = rep(input$sim_budget, 6),
        type = 'scatter',
        mode = 'lines',
        name = 'Investimento',
        line = list(color = 'rgb(204,204,204)', width = 2, dash = 'dash')
      ) %>%
      layout(
        title = "Projeção de Resultados (6 meses)",
        xaxis = list(title = "Período"),
        yaxis = list(title = "Valor (R$)")
      )
    
    p %>% config(displayModeBar = FALSE)
  })
  
  # Download de simulação
  output$download_sim <- downloadHandler(
    filename = function() {
      paste("simulacao-lancamento-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      sim <- simulation_results()
      write.csv(sim$projections, file, row.names = FALSE)
    }
  )
}

# Execução do aplicativo Shiny
shinyApp(ui = ui, server = server)
