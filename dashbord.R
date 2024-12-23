library(lubridate)
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(maps)
library(sf)
library(tigris)
library(htmlwidgets)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shiny)
#Read file


df <- read_excel(file.choose())



head(df)



##Tratamento dos dados


colnames(df)

#Remover colunas
df2 <- df %>% select(-`Agency Name`,-`Incident Address`,-`Cross Street 1`, -`Cross Street 2`,-`Intersection Street 1`, -`Due Date`,
                     -`Intersection Street 2`,-`Landmark`, -`Facility Type`, -`Resolution Description`, -`Community Board`, 
                     -`Park Borough`, -`Park Facility Name`, -`Vehicle Type`, -`Taxi Company Borough`, -`Taxi Pick Up Location`, 
                     -`Bridge Highway Name`, -`Bridge Highway Direction`, -`Road Ramp`, -`Bridge Highway Segment`,-`Location`,
                     -`Resolution Action Updated Date`, -`Location Type`, -`Street Name`)





#colnames(dados_limpos)

#Remover linhas com N/A
dados_limpos <- drop_na(df2)

dados_limpos$Agency[dados_limpos$Agency == 36894] <- 311

# Converter todos os valores da coluna City para maiúsculas
dados_limpos$City <- toupper(dados_limpos$City)



#View(dados_limpos)



#Conversao de datas


dados_limpos$`Created Date` <- as.POSIXct(dados_limpos$`Created Date`, format="%m/%d/%Y,%H:%M:%S")
dados_limpos$`Closed Date` <- as.POSIXct(dados_limpos$`Closed Date`, format="%m/%d/%Y,%H:%M:%S")


#nova coluna (duration)
dados_limpos$duration <- dados_limpos$`Closed Date` - dados_limpos$`Created Date`


dados_limpos <- dados_limpos[dados_limpos$duration >= 0, ]

#View(dados_limpos)


###Exploratory Data Analysis and Interesting Visualization

##Grafico da duraçao dos processos (Grafico 1)

dados_limpos$duration_days <- as.numeric(dados_limpos$duration) / (60 * 60 * 24)  # Convertendo segundos para dias


dados_limpos$duration_category <- cut(dados_limpos$duration_days, 
                                      breaks = c(seq(0, 4, by = 1), seq(5, 30, by = 5), Inf), 
                                      labels = c("0", "1", "2", "3", "4", 
                                                 "5-10", "10-15", "15-20", "20-25", "25-30", ">30"),
                                      right = FALSE)



dados_limpos <- dados_limpos %>%
  mutate(`Complaint Category` = case_when(
    grepl("Animal|Pigeon|Rodent|Harboring Bees/Wasps", `Complaint Type`, ignore.case = TRUE) ~ "Animal Control",
    grepl("Elevator|Appliance|Door|Outside Building|Maintenance|FLOORING/STAIRS|PAINT/PLASTER", `Complaint Type`, ignore.case = TRUE) ~ "Building Maintenance",
    grepl("Homeless|Senior|Public Toilet|Beach|Pool|Sauna|Public Payphone Complaint|Legal Services Provider Complaint", `Complaint Type`, ignore.case = TRUE) ~ "Community Services/Structures",
    grepl("Asbestos|Mold|Standing Water|Dirty|Sanitation|Overgrown|Dead|Dying|Damaged Tree|New Tree Request", `Complaint Type`, ignore.case = TRUE) ~ "Environmental Issues",
    grepl("Food|Poisoning|Drinking|Health", `Complaint Type`, ignore.case = TRUE) ~ "Food and Health Safety",
    grepl("Noise", `Complaint Type`, ignore.case = TRUE) ~ "Noise Complaints",
    grepl("Parking|Driveway|Derelict Vehicle|Derelict Vehicles", `Complaint Type`, ignore.case = TRUE) ~ "Parking Issues",
    grepl("Non-Emergency|Illegal|Disorderly|Panhandling|SAFETY|Other Enforcement|Violation of Park Rules|Lifeguard", `Complaint Type`, ignore.case = TRUE) ~ "Public Safety and Law Enforcement",
    grepl("Overflowing|Litter|Posting|UNSANITARY CONDITION|Urinating in Public|Adopt-A-Basket|Missed Collection|Recycling Enforcement", `Complaint Type`, ignore.case = TRUE) ~ "Sanitation",
    grepl("Sidewalk|Street|Curb|Root|Sewer", `Complaint Type`, ignore.case = TRUE) ~ "Road and Sidewalk Conditions",
    grepl("ELECTRIC|PLUMBING|WATER|HEAT|HOT", `Complaint Type`, ignore.case = TRUE) ~ "Utilities",
    grepl("Bike Rack Condition|Broken Muni Meter|Transportation Provider Complaint|Taxi Report|For Hire Vehicle Report|Public Payphone Complaint|Taxi Complaint|Bike/Roller/Skate Chronic|For Hire Vehicle Complaint|Bus Stop Shelter Complaint|Bus Stop Shelter Placement|Traffic|Derelict Bicycle", `Complaint Type`, ignore.case = TRUE) ~ "Tranportation",
    TRUE ~ "Miscellaneous"  # For any complaints that don't fit into the defined categories
  ))

dados_limpos <- dados_limpos %>% mutate(Hour = hour(`Created Date`))



# Garantir que o tigris use os dados mais recentes
options(tigris_use_cache = TRUE)

# Obter os dados de polígonos dos boroughs de Nova York
ny_counties <- counties(cb = TRUE, state = "36", year = 2022)  # Define o ano explicitamente
ny_city <- ny_counties %>%
  filter(COUNTYFP %in% c("061", "047", "005", "081", "085")) %>%  # Apenas boroughs da cidade
  st_transform(crs = 4326)  # Converter para CRS WGS84

ui <- fluidPage(
  titlePanel("Interactive Dashboard - Complaints"),
  sidebarLayout(
    sidebarPanel(
      h4("Step 1: Choose Variables"),
      selectInput("countVar", "Variable to Count (X-axis):", 
                  choices = c("Complaint Category", "Agency", "Borough"),
                  selected = "Complaint Category"),
      
      checkboxGroupInput("fixedVars", "Fixed Variables (Filters):",
                         choices = c("Complaint Category", "Agency", "Borough"),
                         selected = c("Complaint Category", "Agency", "Borough")),
      
      selectInput("fillVar", "Fill Bars by:",
                  choices = c("None", "Complaint Category", "Agency", "Borough", "Complaint Type"),
                  selected = "None"),
      
      hr(),
      h4("Step 2: Set Filter Options"),
      uiOutput("filterUI"),
      uiOutput("complaintTypeUI"),
      uiOutput("descriptorUI"),
      
      hr(),
      h4("Step 3: Bar Display Options"),
      selectInput("barPosition", "Choose Bar Position:",
                  choices = c("Stacked" = "stack", "Separate" = "dodge"),
                  selected = "stack"),
      
      hr(),
      h4("Step 4: Select Time Frames"),
      sliderInput(
        inputId = "time_range",
        label = "Select Hour Range:",
        min = 0,
        max = 23,
        value = c(0, 23),
        step = 1
      ),
      
      hr(),
      h4("Step 5: Select Close Date Range"),
      dateRangeInput(
        inputId = "date_range",
        label = "Select Closed Date Range:",
        start = min(dados_limpos$`Closed Date`, na.rm = TRUE),
        end = max(dados_limpos$`Closed Date`, na.rm = TRUE),
        min = min(dados_limpos$`Closed Date`, na.rm = TRUE),
        max = max(dados_limpos$`Closed Date`, na.rm = TRUE),
        format = "yyyy-mm-dd"
      ),
      
      hr(),
      h4("Step 5: Select Created Date Range"),
      dateRangeInput(
        inputId = "created_date_range",
        label = "Select Created Date Range:",
        start = min(dados_limpos$`Created Date`, na.rm = TRUE),
        end = max(dados_limpos$`Created Date`, na.rm = TRUE),
        min = min(dados_limpos$`Created Date`, na.rm = TRUE),
        max = max(dados_limpos$`Created Date`, na.rm = TRUE),
        format = "yyyy-mm-dd"
      ),
      
      hr(),
      actionButton("start", "Start")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Visualizations",
          fluidRow(
            column(6, plotOutput("barPlot")),
            column(6, plotOutput("clockPlot"))
          ),
          fluidRow(
            column(12, plotOutput("scatterPlot"))),
          fluidRow(
            column(12, plotOutput("timeSeriesPlot")))
        ),
        
        tabPanel("Map", leafletOutput("heatmap")),
        tabPanel(
          "Summary Table",
          tableOutput("summaryTable"),
          fluidRow(conditionalPanel(
            condition = "output.showTopDescriptors",
            column(6, plotOutput("topDescriptorsPlot")),
            column(6, plotOutput("topZipCodesPlot"))
          ))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamic filters
  output$filterUI <- renderUI({
    req(input$fixedVars)
    lapply(input$fixedVars, function(var) {
      checkboxGroupInput(
        inputId = paste0("filter_", var),
        label = paste("Select", var, "values:"),
        choices = c("All", unique(dados_limpos[[var]])),
        selected = "All"
      )
    })
  })
  
  output$complaintTypeUI <- renderUI({
    req(input$fixedVars)
    
    if ("Complaint Category" %in% input$fixedVars) {
      selectedCategories <- input[[paste0("filter_Complaint Category")]]
      
      # Gerar as opções para "Complaint Type"
      if (!is.null(selectedCategories) && "All" %in% selectedCategories) {
        filterdData <- dados_limpos
        choices <- c("All", unique(dados_limpos$`Complaint Type`))
      } else if (!is.null(selectedCategories)) {
        # Filtrar "Complaint Types" pelas categorias selecionadas
        filteredData <- dados_limpos %>% filter(`Complaint Category` %in% selectedCategories)
        choices <- c("All", unique(filteredData$`Complaint Type`))
      } else {
        choices <- c("All") # Padrão
      }
      
      # Atualizar o seletor de "Complaint Type"
      selectInput(
        inputId = "filter_Complaint.Type", 
        label = "Select Complaint Type:",
        choices = choices,
        selected = "All", # Sempre redefine para "All"
        multiple = TRUE
      )
    }
  })
  
  
  output$descriptorUI <- renderUI({
    req(input$filter_Complaint.Type)
    
    selectedCategories <- input[[paste0("filter_Complaint Category")]]
    
    if (length(selectedCategories) == 1 && !("All" %in% selectedCategories)) {
      selectedTypes <- input$filter_Complaint.Type
      
      if ("All" %in% selectedTypes) {
        filteredDataForDescriptor <- dados_limpos %>%
          filter(`Complaint Category` %in% selectedCategories)
      } else {
        filteredDataForDescriptor <- dados_limpos %>%
          filter(`Complaint Type` %in% selectedTypes)
      }
      
      descriptorChoices <- unique(filteredDataForDescriptor$Descriptor)
      
      checkboxGroupInput(
        inputId = "filter_Descriptor",
        label = "Select Descriptor values:",
        choices = descriptorChoices,
        selected = descriptorChoices
      )
    }
  })
  
  filteredData <- eventReactive(input$start, {
    data <- dados_limpos
    
  # Aplicar filtros dinâmicos com base nas variáveis fixas
    for (var in input$fixedVars) {
      selected <- input[[paste0("filter_", var)]]
      if (!is.null(selected) && !"All" %in% selected) {
        data <- data %>% filter(.data[[var]] %in% selected)
      }
    }
  
  # Filtro para "Complaint Type"
    selectedCategories <- input[[paste0("filter_Complaint Category")]]
    if (!is.null(selectedCategories) && "All" %in% selectedCategories) {
      # Ignorar filtro de "Complaint Type" quando "All" em "Complaint Category"
    } else if (!is.null(input$filter_Complaint.Type) && !"All" %in% input$filter_Complaint.Type) {
     data <- data %>% filter(`Complaint Type` %in% input$filter_Complaint.Type)
    }
      
      # Filtro para Descriptor
    if (!is.null(input$filter_Descriptor) && !"All" %in% input$filter_Descriptor) {
      data <- data %>% filter(Descriptor %in% input$filter_Descriptor)
      }
    
  #Filtro por intervalo de horas
    start_time <- input$time_range[1]
    end_time <- input$time_range[2]
    data <- data %>% filter(`Hour` >= start_time & `Hour` <= end_time)
    
    # Filtro por intervalo de datas "Closed Date"
    if (!is.null(input$date_range)) {
      start_date <- as.POSIXct(paste(input$date_range[1], "00:00:00"), tz = "UTC")
      end_date <- as.POSIXct(paste(input$date_range[2], "23:59:59"), tz = "UTC")
      
      data <- data %>%
        filter(`Closed Date` >= start_date & `Closed Date` <= end_date)
    }
    
    # Filtro por intervalo de datas "Created Date"
    if (!is.null(input$created_date_range)) {
      start_date <- as.POSIXct(paste(input$created_date_range[1], "00:00:00"), tz = "UTC")
      end_date <- as.POSIXct(paste(input$created_date_range[2], "23:59:59"), tz = "UTC")
      
      data <- data %>%
        filter(`Created Date` >= start_date & `Created Date` <= end_date)
    }
    
    data
  })
  
  
  
  # Time Series Plot
  output$timeSeriesPlot <- renderPlot({
    data <- filteredData()
    countVar <- input$countVar
    complaintTypesSelected <- input$filter_Complaint.Type
    complaintCategorySelected <- input[[paste0("filter_Complaint Category")]]
    
    if (nrow(data) == 0) {
      ggplot() +
        labs(title = "No data available for the selected filters") +
        theme_void()
    } else {
      # Se uma única Complaint Category foi selecionada, e múltiplos Complaint Types, usar Complaint Type
      if (length(complaintCategorySelected) == 1 && length(complaintTypesSelected) > 1 && countVar== "Complaint Category") {
        
        data_grouped <- data %>%
          mutate(
            data_dia = as.Date(`Created Date`),                           # Extrai a data de 'Created Date'
            fim_de_semana = weekdays(data_dia) %in% c("sábado", "domingo"), # Identifica os dias de fim de semana
            feriado_local = data_dia == as.Date("2016-09-05")             # Identifica o feriado local
          ) %>%
          group_by(data_dia, `Complaint Type`) %>%
          summarise(
            numero_casos = n(),                                           # Conta o número de casos
            fim_de_semana = any(fim_de_semana),                           # Marca se algum dia no agrupamento é fim de semana
            feriado_local = any(feriado_local),                          # Marca se o dia é o feriado local
            .groups = "drop"
          )
        
        
        
        ggplot(data_grouped, aes(x = data_dia, y = numero_casos)) +
          geom_line(aes(color = `Complaint Type`, group = `Complaint Type`), size = 1) +
          geom_point(aes(color = `Complaint Type`)) +
          geom_point(data = subset(data_grouped, fim_de_semana), shape = 16, color="black") +
          geom_point(data = subset(data_grouped, feriado_local), shape = 16, color = "grey") +# Pontos nos dias de fim de semana
          labs(
            title = "Time Series - Daily Complaints by Complaint Type",
            x = "Date",
            y = "Number of Complaints",
            color = "Complaint Type"
          ) +
          theme_minimal() +
          scale_x_date(date_labels = "%d-%m (%a)", date_breaks = "1 week") +  # Inclui os dias da semana
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
      } else {
        data_grouped <- data %>%
          mutate(
            data_dia = as.Date(`Created Date`),                           # Extrai a data de 'Created Date'
            fim_de_semana = weekdays(data_dia) %in% c("sábado", "domingo"), # Identifica os dias de fim de semana
            feriado_local = data_dia == as.Date("2016-09-05")             # Identifica o feriado local
          ) %>%
          group_by(data_dia, .data[[countVar]]) %>%
          summarise(
            numero_casos = n(),                                           # Conta o número de casos
            fim_de_semana = any(fim_de_semana),                           # Marca se algum dia no agrupamento é fim de semana
            feriado_local = any(feriado_local),                          # Marca se o dia é o feriado local
            .groups = "drop"
          )
        
        
        ggplot(data_grouped, aes(x = data_dia, y = numero_casos)) +
          geom_line(aes(color = .data[[countVar]], group = .data[[countVar]]), size = 1) +
          geom_point(aes(color = .data[[countVar]])) +
          geom_point(data = subset(data_grouped, fim_de_semana), shape = 16, color="black") +  # Pontos para fins de semana
          geom_point(data = subset(data_grouped, feriado_local), shape = 16, color = "grey") +  # Ponto no feriado local (branco)
          labs(
            title = paste("Time Series - Daily Complaints by", countVar),
            x = "Date",
            y = "Number of Complaints",
            color = countVar
          ) +
          theme_minimal() +
          scale_x_date(date_labels = "%d-%m (%a)", date_breaks = "1 week") +  # Inclui os dias da semana
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
      }
    }
  })
  
  
  # Scatter Plot
  output$scatterPlot <- renderPlot({
    data <- filteredData()
    countVar <- input$countVar
    complaintTypesSelected <- input$filter_Complaint.Type
    complaintCategorySelected <- input[[paste0("filter_Complaint Category")]]
    
    if (nrow(data) == 0) {
      ggplot() +
        labs(title = "No data available for the selected filters") +
        theme_void()
    } else {
      # Quando uma única Complaint Category for selecionada e múltiplos Complaint Types foram escolhidos
      if (length(complaintCategorySelected) == 1 && length(complaintTypesSelected) > 1 && countVar== "Complaint Category") {
        ggplot(data, aes(x = as.Date(`Created Date`), y = as.Date(`Closed Date`))) +
          geom_point(aes(color = `Complaint Type`), alpha = 0.7) +  # Usando Complaint Type para colorir
          labs(title = "Scatter Plot: Created vs. Closed Dates",
               x = "Created Date", y = "Closed Date", color = "Complaint Type") +
          theme_minimal() 
      } else {
        ggplot(data, aes(x = as.Date(`Created Date`), y = as.Date(`Closed Date`))) +
          geom_point(aes(color = .data[[countVar]]), alpha = 0.7) +  # Caso contrário, usa a variável countVar
          labs(title = "Scatter Plot: Created vs. Closed Dates",
               x = "Created Date", y = "Closed Date", color = input$countVar) +
          theme_minimal() 
      }
    }
  })
  
  
  #Grafico de Barras
  output$barPlot <- renderPlot({
    data <- filteredData()
    countVar <- input$countVar
    fillVar <- input$fillVar
    barPosition <- input$barPosition
    
    if (nrow(data) == 0) {
      ggplot() +
        labs(title = "No data available for the selected filters") +
        theme_void()
    } else {
      # Criar uma cópia da coluna do fillVar, se for igual ao countVar
      if (countVar == fillVar && fillVar != "None") {
        data <- data %>%
          mutate(fillVar_temp = .data[[fillVar]])
        fillVar <- "fillVar_temp" # Substituir temporariamente no gráfico
      }
      
      # Adicionar contagem e ordenação
      data_summary <- data %>%
        group_by(across(all_of(c(countVar, if (fillVar != "None") fillVar else NULL)))) %>%
        summarise(Count = n(), .groups = "drop") %>%
        arrange(desc(Count))
      
      # Ordenar categorias do eixo x por contagem total
      data_summary[[countVar]] <- factor(
        data_summary[[countVar]],
        levels = unique(data_summary %>%
                          group_by(.data[[countVar]]) %>%
                          summarise(TotalCount = sum(Count), .groups = "drop") %>%
                          arrange(desc(TotalCount)) %>%
                          pull(.data[[countVar]]))
      )
      
      # Ordenar categorias do eixo de preenchimento por contagem dentro de cada x
      if (fillVar != "None") {
        data_summary[[fillVar]] <- factor(
          data_summary[[fillVar]],
          levels = unique(data_summary %>%
                            group_by(.data[[countVar]], .data[[fillVar]]) %>%
                            summarise(TotalCount = sum(Count), .groups = "drop") %>%
                            arrange(desc(TotalCount)) %>%
                            pull(.data[[fillVar]]))
        )
      }
      
      # Criar gráfico
      p <- ggplot(data_summary, aes(x = .data[[countVar]], y = Count)) +
        geom_bar(
          aes(fill = if (fillVar != "None") .data[[fillVar]] else NULL),
          stat = "identity", position = barPosition,
          color = "black", linewidth = 0.1
        )
      
      # Ajustar rótulos e título da legenda
      if (fillVar != "None") {
        p <- p + scale_fill_discrete(name = input$fillVar) # Nome original do fillVar
      }
      
      p + labs(
        title = paste("Distribution of", countVar),
        x = countVar,
        y = "Count"
      ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  
  
  
  # Heatmap with Borough Markers
  output$heatmap <- renderLeaflet({
    data <- filteredData() %>%
      filter(!is.na(Longitude) & !is.na(Latitude)) %>%
      mutate(Longitude = as.numeric(Longitude), Latitude = as.numeric(Latitude))
    
    leaflet(data = ny_city) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = "black", fillColor = "lightgrey", weight = 1, opacity = 0.5) %>%
      addHeatmap(data = data,
                 lng = ~Longitude, lat = ~Latitude,
                 intensity = ~1, blur = 20, max = 0.05, radius = 15) %>%
      addLegend("bottomright", 
                colors = c("blue", "yellow", "red"), 
                labels = c("Low", "Medium", "High"),
                title = "Heatmap Density")
  })
  
  # Clock (Histogram Circular)
  output$clockPlot <- renderPlot({
    data <- filteredData()
    fillVar <- input$fillVar    # Variável para preencher as barras
    
    if (nrow(data) == 0) {
      ggplot() +
        labs(title = "No data available for the selected filters") +
        theme_void()
    } else {
      p <- ggplot(data, aes(x = Hour)) +
        geom_histogram(
          aes(fill = if (fillVar != "None") .data[[fillVar]] else NULL), 
          binwidth = 1, color = "black", linewidth = 0.1, boundary = 0, closed = "left"
        ) +
        coord_polar(start = 0) +
        scale_x_continuous(
          breaks = seq(0, 23, by = 3), 
          limits = c(0, 24), 
          minor_breaks = seq(0, 23, by = 1)
        ) +
        labs(
          x = "Hour of the Day (0-23h)", 
          y = "Number of Complaints", 
          fill = if (fillVar != "None") fillVar else NULL,
          title = paste("Distribution of Complaints by Hour")
        ) +
        theme_minimal()
      
      p
    }
  })
  
  output$summaryTable <- renderTable({
    data <- filteredData()
    if (nrow(data) == 0) {
      return(data.frame("No data available for the selected filters"))
    } else {
      data %>%
        group_by(`Complaint Type`) %>%
        summarise(
          Count = n(),
          Avg_Duration_Days = mean(as.numeric(duration), na.rm = TRUE) / (60 * 60 * 24)
        ) %>%
        arrange(desc(Count))
    }
  })
  
  # Reactive expression for top descriptors and ZIP codes
  topData <- reactive({
    data <- filteredData()
    complaintTypesSelected <- input$filter_Complaint.Type
    
    # Ensure only one Complaint Type is selected
    if (length(complaintTypesSelected) == 1 && !"All" %in% complaintTypesSelected) {
      filtered <- data %>% filter(`Complaint Type` == complaintTypesSelected)
      
      top_descriptors <- filtered %>%
        count(Descriptor, sort = TRUE) %>%
        slice_max(n, n = 5)
      
      top_zip_codes <- filtered %>%
        count(`Incident Zip`, sort = TRUE) %>%
        slice_max(n, n = 5)
      
      list(descriptors = top_descriptors, zip_codes = top_zip_codes)
    } else {
      NULL
    }
  })
  
  # Show/hide conditional panels
  output$showTopDescriptors <- reactive({
    !is.null(topData())
  })
  outputOptions(output, "showTopDescriptors", suspendWhenHidden = FALSE)
  
  # Plot for top 5 descriptors
  output$topDescriptorsPlot <- renderPlot({
    top <- topData()
    if (!is.null(top)) {
      ggplot(top$descriptors, aes(x = reorder(Descriptor, n), y = n)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(
          title = "Top 5 Descriptors for Selected Complaint Type",
          x = "Descriptor", y = "Count"
        ) +
        theme_minimal()
    }
  })
  
  # Plot for top 5 ZIP codes
  output$topZipCodesPlot <- renderPlot({
    top <- topData()
    if (!is.null(top)) {
      ggplot(top$zip_codes, aes(x = reorder(`Incident Zip`, n), y = n)) +
        geom_bar(stat = "identity", fill = "darkorange") +
        coord_flip() +
        labs(
          title = "Top 5 ZIP Codes for Selected Complaint Type",
          x = "ZIP Code", y = "Count"
        ) +
        theme_minimal()
    }
  })
  
  
}

# Run the App
shinyApp(ui, server)


