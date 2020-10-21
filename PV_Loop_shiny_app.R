library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)

ui <- fluidPage (
  titlePanel("Curva presión - volumen"),
  h6("por: Juan Camilo Cárdenas"),
  fluidRow (
    column(2,
           h3("Escenario 1"),
           sliderInput(inputId = "PAS1", label = "Presión arterial sistólica", 
                       min = 0, max = 200, value = 130),
           sliderInput(inputId = "PAD1", label = "Presión arterial diastólica", 
                       min = 0, max = 200, value = 80),
           sliderInput(inputId = "EDP1", label = "Presión de fin de diástole", 
                       min = 0, max = 200, value = 12),
           sliderInput(inputId = "EDV1", label = "Volumen de fin de diástole", 
                       min = 0, max = 200, value = 120),
           sliderInput(inputId = "ESV1", label = "Volumen de fin de sístole", 
                       min = 0, max = 200, value = 50)),
    position = "right",
    column(8,
           (plotOutput(outputId = "galleta")),
            hr(),
            column(6,
                   span(textOutput("Volsis1"), style = "color:blue"),
                   span(textOutput("EjFrac1"), style = "color:blue"),
                   span(textOutput("PAM1"), style = "color:blue"),
                   span(textOutput("PP1"), style = "color:blue")),
           column(6,
                  span(textOutput("Volsis2"), style = "color:red"),
                  span(textOutput("EjFrac2"), style = "color:red"),
                  span(textOutput("PAM2"), style = "color:red"),
                  span(textOutput("PP2"), style = "color:red"))),
    column (2,
            h3 ("Escenario 2"),
            sliderInput(inputId = "PAS2", label = "Presión arterial sistólica", 
                        min = 0, max = 200, value = 131),
            sliderInput(inputId = "PAD2", label = "Presión arterial diastólica", 
                        min = 0, max = 200, value = 81),
            sliderInput(inputId = "EDP2", label = "Presión de fin de diástole", 
                        min = 0, max = 200, value = 13),
            sliderInput(inputId = "EDV2", label = "Volumen de fin de diástole", 
                        min = 0, max = 200, value = 121),
            sliderInput(inputId = "ESV2", label = "Volumen de fin de sístole", 
                        min = 0, max = 200, value = 51))))

server <- function (input, output) {
  output$galleta <- renderPlot({
    A1 <- c(press = 10, vol = input$ESV1)
    B1 <- c(press = input$EDP1, vol = input$EDV1)
    C1 <- c(press = input$PAD1, vol = input$EDV1)
    D1 <- c(press = input$PAS1, vol = mean(c(input$ESV1, input$EDV1)))
    E1 <- c(press = mean(c(input$PAD1, input$PAS1)), vol = input$ESV1)
    
    A2 <- c(press = 10, vol = input$ESV2)
    B2 <- c(press = input$EDP2, vol = input$EDV2)
    C2 <- c(press = input$PAD2, vol = input$EDV2)
    D2 <- c(press = input$PAS2, vol = mean(c(input$ESV2, input$EDV2)))
    E2 <- c(press = mean(c(input$PAD2, input$PAS2)), vol = input$ESV2)
    plot <- data.frame(rbind(A1,B1,C1,D1,E1,A2,B2,C2,D2,E2))
    
    plot1 <- plot %>%
      ggplot (aes (vol, press)) +
      labs(title = "Curva presión volumen", y = "Presión [mmHg]", x = "Volumen [mL]") +
      geom_curve(x = A1[[2]], y = A1[[1]], xend = B1[[2]], yend = B1[[1]], 
                 curvature = 0.08, colour = "blue", size = 2) +
      geom_curve(x = B1[[2]], y = B1[[1]], xend = C1[[2]], yend = C1[[1]], 
                 curvature = 0.08, colour = "blue", size = 2) +
      geom_curve(x = C1[[2]], y = C1[[1]], xend = D1[[2]], yend = D1[[1]], 
                 curvature = 0.2, colour = "blue", size = 2) +
      geom_curve(x = D1[[2]], y = D1[[1]], xend = E1[[2]], yend = E1[[1]], 
                 curvature = 0.2, colour = "blue", size = 2) +
      geom_curve(x = E1[[2]], y = E1[[1]], xend = A1[[2]], yend = A1[[1]], 
                 curvature = 0.08, colour = "blue", size = 2)+
      geom_curve(x = A2[[2]], y = A2[[1]], xend = B2[[2]], yend = B2[[1]], 
                 curvature = 0.08, colour = "red", size = 2) +
      geom_curve(x = B2[[2]], y = B2[[1]], xend = C2[[2]], yend = C2[[1]], 
                 curvature = 0.08, colour = "red", size = 2) +
      geom_curve(x = C2[[2]], y = C2[[1]], xend = D2[[2]], yend = D2[[1]], 
                 curvature = 0.2, colour = "red", size = 2) +
      geom_curve(x = D2[[2]], y = D2[[1]], xend = E2[[2]], yend = E2[[1]], 
                 curvature = 0.2, colour = "red", size = 2) +
      geom_curve(x = E2[[2]], y = E2[[1]], xend = A2[[2]], yend = A2[[1]], 
                 curvature = 0.08, colour = "red", size = 2) +
      geom_point(size = 3) +
      coord_fixed(ratio = 0.75, xlim = c(0, 200), ylim = c(0, 200)) +
      theme_minimal()
    plot1
  }, height = 400, width = 800)
  output$Volsis1 <- renderText({paste("Volumen sistólico:", 
                                    (input$EDV1 - input$ESV1), "mL")})
  
  output$EjFrac1 <- renderText({paste("Fracción de eyección", 
                                      (substr(((input$EDV1-input$ESV1)/input$EDV1),
                                              1, 4)))})
  output$PAM1 <- renderText({paste("Presión arterial media:", 
                                   (substr(((input$PAS1 + input$PAD1 + input$PAD1)/3), 1, 5)))})
  
  output$PP1 <- renderText({paste("Presión de pulso:", input$PAS1 - input$PAD1)})
  
  output$Volsis2 <- renderText({paste("Volumen sistólico:",
                                    (input$EDV2 - input$ESV2), "mL")})
  
  output$EjFrac2 <- renderText({paste("Fracción de eyección", 
                                      (substr(((input$EDV2-input$ESV2)/input$EDV2),
                                              1, 4)))})
  
  output$PAM2 <- renderText({paste("Presión arterial media:", 
                                   (substr(((input$PAS2 + input$PAD2 + input$PAD2)/3), 1, 5)))})
  
  output$PP2 <- renderText({paste("Presión de pulso:", input$PAS2 - input$PAD2)})
  
  
}

shinyApp(ui = ui, server = server)
