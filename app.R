# app.R ----

# Load packages
require(shiny)
require(tidyverse)
require(BE)

load(file = 'examples.Rdata')

analyze_ecg <- function(df){
  write_csv(df, 'df.csv')
  system('wrsamp -i df.csv -F 500 -o 1000 1 2 3 4 5 6 7 8 9 10 11 12')
  system('ecgpuwave -r 1000 -a 1000')
  system('rdann -r 1000 -a 1000 > 1000.table')
  read_table('1000.table', 
             col_names = c('time', 'index', 
                           'anno', 'no1', 
                           'no2', 'class')) %>% 
    mutate(index = index*2) %>% 
    as.data.frame()
}


# Define UI for data download app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("PIPET ECG"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      shiny::h2('1-A: 자료 선택'),
      
      # Input: Choose dataset ----
      
      radioButtons("upload_yn", NULL,
                   c("업로드" = "upload",
                     "예제" = "default"),
                   selected = 'default'),
     
      selectInput("dataset", NULL,
                  choices = c("Example ECG 1" = "case1", 
                              "Example ECG 2" = "case2")),
      
      shiny::tags$p('업로드 할 ECG 자료는 아래의 10초간 12 channel ECG를 500Hz로 sampling하여 CSV형태로 준비해주세요.'),
      
      shiny::h2('1-B: 자료 확인'),
      
      verbatimTextOutput("table"),
      
      # Button
      downloadButton("downloadData", "Download"),
      
      # Horizontal line ----
      tags$hr(),
      shiny::h2('1-B: 직접 자료 업로드'),
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"')
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      shiny::h2('3: ECG 결과 확인'),
      checkboxInput("annotate1", "Annotate the ECG record", TRUE),
      shiny::h3('Lead II signal'),
      
      plotOutput("plot_leadii"),
      #fluidRow(splitLayout(cellWidths = c("100%"), )),
      
      shiny::h3('12-lead signal'),
      plotOutput("plot_12lead", height = '1500px'),
      #fluidRow(splitLayout(cellWidths = c("100%"), )),
      shiny::verbatimTextOutput('be_result_auc'),
      
      includeMarkdown("references.md")
    )
  )
)

# Define server logic to display and download selected file ----
server <- function(input, output) {
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    
    if (input$upload_yn == 'upload') {
      read.csv(input$file1$datapath,
               header = input$header,
               sep = input$sep,
               quote = input$quote)
    } else {
      switch(input$dataset,
             "case1" = examples$case1,
             "case2" = examples$case2)
    }
  })
  
  # Table of selected dataset ----
  output$table <- renderPrint({
    as_tibble(datasetInput())
  })
  
  #output$be_result()
  output$be_result_auc <- renderPrint({ analyze_ecg(datasetInput()) }) 
  output$be_result_cmax <- renderPrint({ BE::test2x2(datasetInput(), "Cmax") }) 
  output$be_result_tmax <- renderPrint({ BE::test2x2(datasetInput(), "Tmax") })
  
  output$plot_leadii <- renderPlot({
    datasetInput() %>% 
      mutate(index = index*2) %>% 
      ggplot(aes(index, II)) +
      geom_line() +
      labs(x = 'Time (ms)', y = 'ECG Amplitude')+ theme_bw() +
      geom_vline(data = analyze_ecg(datasetInput()), 
                 aes(xintercept = analyze_ecg(datasetInput())$index, 
                 color = analyze_ecg(datasetInput())$anno))
    })
  
  output$plot_12lead <- renderPlot({
    datasetInput() %>% 
      mutate(index = index*2) %>% 
      gather(lead, value, -index) %>% 
      ggplot(aes(index, value)) +
      facet_wrap(.~lead, ncol=1) +
      geom_line() +
      labs(x = 'Time (ms)', y = 'ECG Amplitude') + theme_bw() +
      geom_vline(data = analyze_ecg(datasetInput()), 
                 aes(xintercept = analyze_ecg(datasetInput())$index, 
                     color = analyze_ecg(datasetInput())$anno))
    #geom_vline(data = anno, xintercept = anno$index, color = 'red')
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
}

# Create Shiny app ----
shinyApp(ui, server)
