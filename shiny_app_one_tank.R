library(shiny)

# --------------------------------------------- Functions

ex <- function(p, k_er, t){
  p * exp(-k_er * t)
}

os<- function(p, rlr, k_er, k_p, t){
  ((p * rlr * k_er) / (k_er - k_p)) * (exp(-k_p * t) - exp(- k_er * t))
}

pd <- function(p, rlr, rrf, k_er, k_p, t){
  ((p * rlr * rrf) / (k_er - k_p)) * (k_er * (1 - exp(-k_p * t)) - k_p * (1 - exp(-k_er * t)))
}

# --------------------------------------------- Shiny

ui <- shinyUI(fluidPage(
  titlePanel("Adjustments"),
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
  sliderInput(inputId = "rlr",  
              label = "RLR",
              value = 1, min = 0, max = 1),
  
  sliderInput(inputId = "rrf", 
              label = "RRF",
              value = 0.75, min = 0, max = 1),
  
  sliderInput(inputId = "p", 
              label = "P",
              value = 100, min = 0, max = 100),
  
  sliderInput(inputId = "k_er", 
              label = "K_er",
              value = 0.1, min = 0, max = 1),
  
  sliderInput(inputId = "k_p", 
              label = "K_p",
              value = 0.5, min = 0, max = 1),
  
  sliderInput("t", label = h3("Time Span"), min = 0, 
              max = 100, value = c(0, 100), step = 1),
  
                ),
  mainPanel("Visualisation",
  
  plotOutput("hist"), # Same here, "hist" is a name, also also be ABC
))))

server <- function(input, output) {
  
  time <- reactive({
    seq(from = min(input$t), to = max(input$t), by = 1)
  })
  
  data_ex <- reactive({
    ex(p = input$p, k_er = input$k_er, t = time())
  })
  
  data_os <- reactive({
    os(p = input$p, rlr = input$rlr, k_er = input$k_er, k_p = input$k_p, t = time())
  })
  
  data_pd <- reactive({
    pd(p = input$p, rlr = input$rlr, rrf = input$rrf, k_er = input$k_er, k_p = input$k_p, t = time())
  })
  
  
  output$hist <- renderPlot({ 
    plot(time(), data_ex(), type = "l", main = "Baseline Model", ylab = "State Variable Solutions")
    lines(time(), data_os(), col = 2)
    lines(time(), data_pd(), col = 4)
  })
  
}

shinyApp(ui = ui, server = server )







