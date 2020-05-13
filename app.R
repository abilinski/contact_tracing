#************************************* Shiny App Code *************************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

#### SETUP #### 

# source model code
source("contact_tracing_v4.R")

# libraries
library(shinythemes)
library(plotly)
library(RColorBrewer)
library(tidyverse)

# housekeeping
  # radio button values
  choiceNames= c("Fraction of symptomatic cases detected (community, untraced)", "Fraction of contacts successfully traced",
                 "Isolation and quarantine efficacy (first generation)", "Isolation and quarantine efficacy (subsequent generations)")
  choiceValues = c("S_prob.det", "contact_trace_prob", "adh", "adh2")

#### SERVER #### 
server <- function(input, output, session) {
  
### INPUTS ###
  
  # LOAD INPUTS
  observeEvent(input$file, {
    
    inFile = input$file
    if(!is.null(inFile)){
      # Load inputs
      uploaded_inputs <- read.csv(inFile$datapath)
      # Update each input
      for(i in 1:nrow(uploaded_inputs)){
        if(!grepl("testing|comparator|xaxis", uploaded_inputs$inputId[i])) { 
          updateSliderInput(session,
                            inputId = uploaded_inputs$inputId[i],
                            value = uploaded_inputs$value[i])
        }else{
          updateRadioButtons(session,inputId = uploaded_inputs$inputId[i],
                             selected = uploaded_inputs$value[i])
        }
      }
    }
    
  })
  
  # MAKE INPUT TABLE
  inputs = reactive({
    inFile = input$file
    if (is.null(inFile)) {
      return(NULL)
    } else{ return(read.csv(inFile$datapath))}
  })
  output$tbl2 = renderTable({ inputs() })

### MODEL ###
  # RESTORE BUTTON
  observeEvent(input$restore_all, {
    vars = c("P_RR", "P_dur", "S_RR", "S_dur",
             "A_RR", "A_dur",
             "S_prob.det", "A_prob.det", "A_prob", "contact_trace_prob",
             "R0", "Rt", "comparator",
             "baseline_S_prob.det", "baseline_A_prob.det", "test_uptake", "adh", "rel_trans", "xaxis")
    
    for(i in 1:length(vars)) reset(vars[i])
  })
  
  # RUN CONTACT TRACING MODEL
  out <- reactive({
    
    get_R(P_RR = input$P_RR, P_dur = input$P_dur, S_RR = input$S_RR, S_dur = input$S_dur,
          A_RR = input$A_RR, A_dur = input$A_dur, S_prob.det = input$S_prob.det,
          A_prob.det = input$A_prob.det, A_prob = input$A_prob, contact_trace_prob = input$contact_trace_prob,
          comparator = input$comparator,
          baseline_S_prob.det = input$baseline_S_prob.det,
          baseline_A_prob.det = input$baseline_A_prob.det, test_uptake = input$test_uptake,
          adh = input$adh, adh2 = input$adh2, rel_trans = input$rel_trans, xaxis = input$xaxis)
  
    })
  
  # MAKE TABLE
  output$tbl = renderTable({ out() })
  
  # MAKE PLOT TOP ROW
  plots = reactive({ make_plots(out(), xaxis = choiceNames[which(choiceValues==input$xaxis)], R0 = input$R0, Rt = input$Rt) })
  output$plot <- renderPlotly({
    plots()[[1]]
  })
  
  # MAKE PLOT BOTTOM ROW
  output$plot2 <- renderPlotly({
    subplot(style(plots()[[2]], showlegend = FALSE),
            plots()[[3]])
  })

  ### OUTPUTS ###
  # DOWNLOAD INPUTS
  output$download_Inputs <- downloadHandler(
    filename = function() {
      paste("inputs_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # Define inputs to save
      inputs_to_save <- c("P_RR", "P_dur", "S_RR", "S_dur",
                                  "A_RR", "A_dur",
                                  "S_prob.det", "A_prob.det", "A_prob", "contact_trace_prob", 
                                   "R0", "Rt", "comparator",
                                  "baseline_S_prob.det", "baseline_A_prob.det", "test_uptake", "adh", "adh2", "rel_trans", "xaxis")

      # Declare inputs
      inputs <- NULL
      # Append all inputs before saving to folder
      for(input.i in inputs_to_save){
        inputs <- append(inputs, input[[input.i]])
      }
      # Inputs data.frame
      inputs_data_frame <- data.frame(inputId = inputs_to_save, value = inputs)
      # Save Inputs
      write.csv(inputs_data_frame, file, row.names = FALSE)
    }  
  )
  
  # DOWNLOAD ESTIMATES
  output$download_Data <- downloadHandler(
    filename = function() {
      paste("data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(out() %>% select(-point), file)
    }  
  )

}

#### UI #### 
ui <- fluidPage(
  # THEME
  theme=shinytheme("simplex"),
  
  # TITLE
  titlePanel("COVID-19 Contact Tracing Model"),
  
  # SIDEBAR
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        
        # MAIN PARAMETERS
        tabPanel("Main", fluid=TRUE,
                               
                   # model
                   h4("Community detection"),
                   h5("Detection of individuals outside of contact tracing"),
                   sliderInput("S_prob.det", "Fraction of symptomatic cases detected", min=0, max=1, value=.5, step = 0.05),                               
                   sliderInput("A_prob.det", "Fraction of asymptomatic cases detected", min=0, max=1, value=.05, step = 0.05),  

                   h4("Contact tracing"),
                   sliderInput("contact_trace_prob", "Fraction of contacts successfully traced", min=0, max=1, value=0.8, step = 0.05), 
                   sliderInput("adh", "Isolation and quarantine efficacy (first generation)", min=0, max=.95, value=.5, step = 0.05),
                   sliderInput("adh2", "Isolation and quarantine efficacy (subsequent generations)", min=0, max=.95, value=.75, step = 0.05), 
                   
                   h4("Epidemiology"),
                   sliderInput("R0", HTML("R<sub>0</sub> (prior to physical distancing)"), min=1.5, max=3.5, value=2.5, step = 0.1),  
                   sliderInput("Rt", HTML("R(t) (after physical distancing, prior to contact tracing)"), min=.4, max=1.5, value=1, step = 0.1),    
                   
                  radioButtons("xaxis", "X-axis variable:", choiceNames= choiceNames, 
                               choiceValues = choiceValues,
                               selected = NULL,
                               inline = FALSE, width = NULL),
                   
                  # save and download
                  actionButton("restore_all", "Restore original inputs"),
                  downloadButton(outputId = "download_Inputs", 
                                 label = 'Download inputs',
                                 class= "mybutton"),
                  downloadButton(outputId = "download_Data", 
                                 label = 'Download estimates',
                                 class= "mybutton")
               
                  ),
        
        # ADVANCED PARAMETERS
        tabPanel("Advanced", fluid = TRUE,
              
               # comparator             
               radioButtons("comparator", "Analysis:", choices = c("Contact tracing only", "Testing scale-up + contact tracing"),
                            inline = FALSE, width = NULL,
                            choiceValues = NULL),
               conditionalPanel(
                 "input.comparator == 'Testing scale-up + contact tracing'",
                 sliderInput("baseline_S_prob.det", "Baseline fraction of symptomatic cases detected", min = 0, max = 1, value = .5, step = 0.01),
                 sliderInput("baseline_A_prob.det", "Baseline fraction of asymptomatic cases detected", min = 0, max = 1, value = 0, step = 0.01)
                 
               ),
               
               # program
               sliderInput("test_uptake", "Fraction of eligible contacts tested", min=0, max=1, value=0.9, step = 0.01),
               sliderInput("rel_trans", "Relative risk of transmission among detected cases", min=0, max=1, value=0.5, step = 0.01), 
               
               # epidemiology
               h4("Pre-symptomatic"),
               sliderInput("P_RR", "Relative risk of transmission", min=0, max=1, value=1, step = 0.01),
               sliderInput("P_dur", "Duration of pre-symptomatic transmission", min=0, max=3, value=1.5, step = 0.01),  
               
               h4("Symptomatic"),
               sliderInput("S_RR", "Relative risk of transmission", min=0, max=1, value=1, step = 0.01),                               
               sliderInput("S_dur", "Duration of symptomatic transmission", min=0, max=10, value=4, step = 0.01),   
                    
               h4("Asymptomatic"),
               sliderInput("A_RR", "Relative risk of transmission", min=0, max=1, value=.7, step = 0.01),                               
               sliderInput("A_dur", "Duration of asymptomatic transmission", min=0, max=10, value=5.5, step = 0.01),   

               sliderInput("A_prob", "Fraction of cases that are asymptomatic", min=0, max=1, value=0.4, step = 0.01)
               
               )
      
        ),width=3),
        
        # RESULTS          
        mainPanel(tabsetPanel(type = "tabs",
                              
              # model output                
              tabPanel("Plots", 
                       h4(" "),
                       
                       # description
                       includeMarkdown("content/start.md"),
                       
                       # output plots
                       plotlyOutput("plot", height = "450", width = "1300"),
                       plotlyOutput("plot2", height = "450", width = "1300")
                       ),
              
              # upload and view inputs
              tabPanel("Upload inputs",
                              h4("Input data"),
                              fileInput("file", "Choose CSV File",
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                              tableOutput("tbl2")),
              
              # documentation
              tabPanel("Documentation",
                       h4(""),
                       includeMarkdown("content/model.md")
                       )
              ))),
      hr(),
      includeMarkdown("content/footer.md")
    )

#### APP CALL ####
shinyApp(ui = ui, server = server)
