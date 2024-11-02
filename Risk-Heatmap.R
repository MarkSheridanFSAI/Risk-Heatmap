#.libPaths("R Packages")
library(shiny)
library(shinyalert)
library(ggplot2)
library(dplyr)
library(DT)

button_style <- "background-color: #007bff; color: white; border-radius: 5px;"


ui <- fluidPage(
  titlePanel("Risk Heatmap - Shiny Tool"),
  #--------------------------------------------------------
  fluidRow(
    column(width = 2),
    column(width = 8,
           tags$hr(), 
           plotOutput("heatmap"),
           tags$hr(),
           div(style = "text-align: center;",
           downloadButton("download_heatmap", "Download Heatmap", style = button_style)
           ),
           tags$hr()),
    column(width = 2),
    tags$hr(style = "border-top: 3px dashed #333; width: 90%;")),
  #--------------------------------------------------------
  fluidRow(
    column(width = 4,
      h2("1. Manual Input"),
      textInput("risk_name", "Risk Name",value = "Example Risk"),
      textInput("risk_description", "Risk Description",value = "Description of Example Risk"),  # Added description input
      numericInput("materiality", "Materiality (1-5)", value = 1, min = 1, max = 5, step = 1),
      numericInput("subjectivity", "Subjectivity (1-5)", value = 1, min = 1, max = 5, step = 1),
      div(style = "text-align: center;",
      actionButton("add_risk", "Add Risk", style = button_style)
      ),
      tags$hr(style = "border-top: 3px dashed #333; width: 90%;"),
      
      h2("2. Import Input"),
      fileInput("file", "Upload CSV File", accept = c(".csv")),
      tags$hr(style = "border-top: 3px dashed #333; width: 90%;"),

      h2("3. Remove Risk Data"),
      selectInput("risk_select", "Select Risk to Delete", choices = NULL),
      div(style = "text-align: center;",
      actionButton("delete_risk", "Delete Selected Risk", style = button_style)
      ),
      tags$hr()),
    
    column(width = 8,
      DTOutput("risk_table"),
      tags$hr(),
      div(style = "text-align: center;",
          downloadButton("download_data", "Download Risk Data", style = button_style)
      ),
      tags$hr())
  )
  #--------------------------------------------------------
  )

server <- function(input, output, session) {
  
#------------------------------------------------------
# Handling the Risk Dataset

  blank_data <- data.frame(Name = character(), Description = character(), Materiality = numeric(), Subjectivity = numeric(),Risk_Level = numeric(), stringsAsFactors = FALSE)
  risk_data <- reactiveVal(blank_data)
  observe({updateSelectInput(session, "risk_select", choices = risk_data()$Name)}) # Update choices for delete select input
  
  # Add risk to the data frame
  observeEvent(input$add_risk, {
    if (!(input$materiality %in% 1:5)) {shinyalert("Invalid Input!", "Materiality must be a whole number between 1 and 5.", type = "error")
    } else if (!(input$subjectivity %in% 1:5)) {shinyalert("Invalid Input!", "Subjectivity must be a whole number between 1 and 5.", type = "error")}
    validate(need(input$materiality %in% 1:5, "Materiality must be a whole number between 1 and 5"))
    validate(need(input$subjectivity %in% 1:5, "Subjectivity must be a whole number between 1 and 5"))
    new_risk <- data.frame(
                Name = input$risk_name,
                Description = input$risk_description,  # Include description
                Materiality = input$materiality,
                Subjectivity = input$subjectivity,
                Risk_Level = input$materiality*input$subjectivity,
                stringsAsFactors = FALSE)
    risk_data(rbind(risk_data(), new_risk)) # Append new risk to existing data
    updateTextInput(session, "risk_name", value = "Example Risk")  # Reset input fields
    updateTextInput(session, "risk_description", value = "Description of Example Risk")  
    updateNumericInput(session, "materiality", value = 1)
    updateNumericInput(session, "subjectivity", value = 1)
  })
  
  # Delete selected risk
  observeEvent(input$delete_risk, {
    selected_risk <- input$risk_select
    if (selected_risk != "") {risk_data(risk_data() %>% filter(Name != selected_risk))
    }})
  
  # Reactive expression to read the uploaded file
  observeEvent(input$file, {
    req(input$file)
    uploaded_data <- read.csv(input$file$datapath)
    if (all(c("Name", "Description", "Materiality", "Subjectivity","Risk_Level") %in% names(uploaded_data))) {
      risk_data(rbind(risk_data(), uploaded_data))
    } else {
      showModal(modalDialog(
        title = "Error",
        "The uploaded file must contain 'Name', 'Description', 'Materiality', 'Subjectivity', and 'Risk_Level' columns.",
        easyClose = TRUE,
        footer = NULL
      ))}})
  
  # Render risk data table
  output$risk_table <- renderDT({
    datatable(risk_data(), options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })
  
  # Download handler for the risk data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("risk_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(risk_data(), file, row.names = FALSE)
    })
  
  
  
  
  #------------------------------------------------------
  # Handling the Plot
  
  create_heatmap_plot <- function(risk_data_df) {
    background_data <- expand.grid(Materiality = seq(1, 5, by = 1), Subjectivity = seq(1, 5, by = 1))
    background_data$RiskLevel <- with(background_data, Materiality * Subjectivity)
    
    ggplot() +
      geom_tile(data = background_data, aes(x = Materiality, y = Subjectivity, fill = RiskLevel), color = "white", size = 2) +
      scale_fill_gradientn(colors = c("green", "yellow", "orange", "red", "darkred"), 
                           values = scales::rescale(c(1, 5, 10, 15, 25)), 
                           limits = c(1, 25), 
                           name = "Risk Level") +
      geom_text(data = risk_data_df, aes(x = Materiality, y = Subjectivity, label = Name), 
                size = 4,
                color = "black", 
                position = position_jitter(width = 0.3, height = 0.3), 
                fontface = "bold", hjust = 0.4, vjust = -0.3) +
      labs(x = "Materiality", y = "Subjectivity", title = "Risk Heatmap") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 24, face = "bold",hjust = 0.5),  
        axis.title.x = element_text(size = 20, face = "bold"),                
        axis.title.y = element_text(size = 20, face = "bold"),                
        axis.text.x = element_text(size = 16, face = "bold"),                 
        axis.text.y = element_text(size = 16, face = "bold")
      )}
  
  
  # Render heatmap plot
  output$heatmap <- renderPlot({
    risk_data_df <- risk_data()  # Get the current risk data
    create_heatmap_plot(risk_data_df)
  })
  
  # Download handler for the heatmap
  output$download_heatmap <- downloadHandler(
    filename = function() {
      paste("heatmap-", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      risk_data_df <- risk_data()  # Get the current risk data
      p <- create_heatmap_plot(risk_data_df)  # Create the plot
      ggsave(file, plot = p, width = 10, height = 10, units = "in", dpi = 300)
    })}

shinyApp(ui, server)
