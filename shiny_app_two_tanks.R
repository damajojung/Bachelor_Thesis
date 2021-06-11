
library(shiny)

# --------------------------------------------- Functions

ex <- function(p, k_er, t){
    p * exp(-k_er * t)
}

os1<- function(p, rlr, k_er, k_p1,k_p2, t){
    (((p * rlr * k_er) / (k_er - k_p1 - k_p2)) * (exp(-(k_p1 + k_p2) * t) - exp(-k_er * t)))
}

os2<- function(p, rlr, k_er,k_p1, k_p2, t){
    (((p * rlr * k_er * k_p2) / ((k_p1* (k_p2 - k_er)) * (k_er - k_p1 - k_p2))) * (
        exp(-(k_p1 + k_p2)*t) * (k_er - k_p2) - exp(-k_p2 * t)*(k_er - k_p1 - k_p2) - 
            exp(-k_er * t) * k_p1))
}

pd <- function(p, rlr, rrf, k_er, k_p1, k_p2, t){
    (((p *rlr * rrf) / (k_p1 * (k_p2 - k_er) * (k_er - k_p1 - k_p2))) * 
         ((k_p1 * (k_er * (k_p1 - k_er) - k_p2 * (k_p1 + k_p2)) + 2*k_er*k_p2) +
         exp(-(k_p1 + k_p2)*t) * (k_er * (k_er * k_p1 - k_er*k_p2 + (k_p2)^2 - k_p1*k_p2)) +
         exp(-k_p2 * t) * (k_er * k_p2*(k_er - k_p1 - k_p2)) +
         exp(-k_er * t) * (k_p1 * (k_p1* k_p2 + (k_p2)^2 - k_er*k_p1)))
    )
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
                               
                               sliderInput(inputId = "k_p1", 
                                           label = "K_p1",
                                           value = 0.5, min = 0, max = 1),
                               
                               sliderInput(inputId = "k_p2", 
                                           label = "K_p2",
                                           value = 0.5, min = 0, max = 1),
                               
                               sliderInput("t", label = h3("Time Span"), min = 0, 
                                           max = 100, value = c(0, 100), step = 1),
                               
                  ),
                  mainPanel("Visualisation",
                            
                            plotOutput("hist"), # Same here, "hist" is a name, could also be ABC
                  ))))

server <- function(input, output) {
    
    time <- reactive({
        seq(from = min(input$t), to = max(input$t), by = 1)
    })
    
    data_ex <- reactive({
        ex(p = input$p, k_er = input$k_er, t = time())
    })
    
    data_os1 <- reactive({
        os1(p = input$p, rlr = input$rlr, k_er = input$k_er, k_p1 = input$k_p1, k_p2 = input$k_p2, t = time())
    })
    
    data_os2 <- reactive({
        os2(p = input$p, rlr = input$rlr, k_er = input$k_er, k_p1 = input$k_p1, k_p2 = input$k_p2, t = time())
    })
    
    data_pd <- reactive({
        pd(p = input$p, rlr = input$rlr, rrf = input$rrf, k_er = input$k_er, k_p1 = input$k_p1, k_p2 = input$k_p2, t = time())
    })
    
    
    output$hist <- renderPlot({ 
        plot(time(), data_ex(), type = "l", main = "Two Tanks Model", ylab = "State Variable Solutions")
        lines(time(), data_os1(), col = 2)
        lines(time(), data_os2(), col = 3)
        lines(time(), data_pd(), col = 4)
    })
    
}

shinyApp(ui = ui, server = server )







