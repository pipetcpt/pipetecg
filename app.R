# app.R ----

# Load packages
require(shiny)
require(tidyverse)
require(BE)

load(file = 'examples.Rdata')

# Define UI for data download app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("PIPET 생물학적동등성 분석 (2x2 crossover design, average bioequivalence)"),
  
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
                  choices = c("BE 패키지 내장: N=33" = "NCAResult4BE", 
                              "Case 1: N=18, Sauter et al. 1992" = "case1",
                              "Case 2: N=18, Clayton and Leslie 1981" = "case2",
                              "Case 3: N=13, Case 2 with imbalance between sequences" = "case3",
                              "Case 4: N=18, Case 2 with extreme range" = "case4",
                              "Case 5: N=18, Case 2 with an outlier" = "case5",
                              "Case 6: N=100, Simulated dataset, balanced,  residual is not normally distributed" = "case6",
                              "Case 7: N=1000, Simulated dataset, balanced, normal distributed residual" = "case7",
                              "Case 8: N=1000, Case 7 with outliers, imbalance, and an extreme range" = "case8"
                              )),
      
      shiny::tags$p('입력자료는 아래와 같이 준비해주세요. SUBJ에 대상자 번호를 적어줍니다. GRP에는 RT인지 TR인지 sequence 정보를 적어줍니다. PRD에는 1기 혹은 2기를 적어줍니다. TRT에는 각 기에 투여받은 약물 정보를 적어줍니다. AUClast, Cmax, Tmax는 비구획 분석으로 얻어진 값을 넣어주면 됩니다. 본 앱의 개발자는 누가 파일을 올렸는지 알 수 없고, 어떠한 경로로도 올린 파일에 접근할 수 없으므로 안심하고 사용하셔도 됩니다. (총 7개의 열로 구성됨)'),
      
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
      
      shiny::h2('3: 결과 확인'),
      shiny::h3('AUClast'),
      shiny::verbatimTextOutput('be_result_auc'), 
      fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_auc_a"), plotOutput("plot_auc_b"))),
      
      conditionalPanel(
        condition = "input.dataset == 'NCAResult4BE'",
        
        shiny::h3('Cmax'),
        shiny::verbatimTextOutput('be_result_cmax'), 
        fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_cmax_a"), plotOutput("plot_cmax_b"))),
        
        shiny::h3('Tmax'),
        shiny::verbatimTextOutput('be_result_tmax'),
        fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_tmax_a"), plotOutput("plot_tmax_b")))
        
      ),
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
             "case2" = examples$case2,
             "case3" = examples$case3,
             "case4" = examples$case4,
             "case5" = examples$case5,
             "case6" = examples$case6,
             "case7" = examples$case7,
             "case8" = examples$case8,
             "NCAResult4BE" = BE::NCAResult4BE)
    }
  })
  
  # Table of selected dataset ----
  output$table <- renderPrint({
    as_tibble(datasetInput())
  })
  
  #output$be_result()
  output$be_result_auc <- renderPrint({ BE::test2x2(datasetInput(), "AUClast") }) 
  output$be_result_cmax <- renderPrint({ BE::test2x2(datasetInput(), "Cmax") }) 
  output$be_result_tmax <- renderPrint({ BE::test2x2(datasetInput(), "Tmax") })
  
  output$plot_auc_a <- renderPlot({plot2x2a(datasetInput(), "AUClast")})
  output$plot_auc_b <- renderPlot({plot2x2b(datasetInput(), "AUClast")})
  output$plot_cmax_a <- renderPlot({plot2x2a(datasetInput(), "Cmax")})
  output$plot_cmax_b <- renderPlot({plot2x2b(datasetInput(), "Cmax")})
  output$plot_tmax_a <- renderPlot({plot2x2a(datasetInput(), "Tmax")})
  output$plot_tmax_b <- renderPlot({plot2x2b(datasetInput(), "Tmax")})
  
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
