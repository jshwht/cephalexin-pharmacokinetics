library(shiny)
library(shinyWidgets)

ui <- navbarPage("Cephalexin",
                 tabPanel("Main",
                          sidebarLayout(
                            sidebarPanel(
                              h2("Inputs"),
                              radioButtons(
                                inputId = "sex", label = "",
                                choices = c("Male" = 1,"Female" = 0.85),selected = 1),
                              sliderInput("dose", "Dose of cephalexin (mg)", min = 0, max = 1800, value = 500, step = 10),
                              sliderInput("CsCr", "Serum creatinine concentration (mg/dL)", min = 0, max = 2, value = 0.95, step = 0.05),
                              sliderInput("age", "Age (years)", min = 0, max = 110, value = 40, step = 1),
                              sliderInput("weight", "Weight (kg)", min = 1, max = 200, value = 50, step = 1)
                            ),
                            mainPanel(
                              chooseSliderSkin("Flat", color = "blue"),
                              plotOutput("Cp"),
                              sliderInput("sim", "Number of simulations", min = 1, max = 1000, value = 10, step = 1)
                            ))),
                 tabPanel("Simulated data",
                          fluidRow(
                            column(width=5,
                                   tableOutput("table")))
                 ),
                 tabPanel("Model information",
                          uiOutput("model_info")
                 )
)