#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  useShinyjs(),  # Set up shinyjs
  
  # Application title
  titlePanel("Critical Illumination Simulator"),
  
  plotOutput("scatterplot",
             dblclick = "plot_dblclick",
             brush = brushOpts(
               id = "plot_brush",
               resetOnNew = TRUE),
  ),
  
  fluidRow(
    column(3,
           checkboxInput('off_axis','Off axis ray', value=TRUE),
           checkboxInput('on_axis','On axis ray'),
    ),
    
    column(6,
           h4("Illumination:"),
           textOutput("result4"),
           textOutput("result5"),
           
    ),
    
    column(3,
           h4("Imaging:"),
           textOutput("result1"),
           textOutput("result2"),
           textOutput("result6"),
           textOutput("result3"),
    ),
  ),
  
  
  hr(),
  
  fluidRow(
    column(3,
           wellPanel(
             h4(tags$a(href="https://www.thorlabs.co.jp/newgrouppage9.cfm?objectgroup_id=351", "Source", target="_blank", rel="noopener noreferrer")),
             
             numericInput(inputId = "na1",
                          label = "N.A.",
                          value = 0.39,
                          min = 0,
                          max = 2,
                          step = 0.01),
             numericInput(inputId = "r1",
                          label = "Radius (mm)",
                          value = 0.5,
                          min = 0,
                          max = 30,
                          step = 0.1),
             numericInput(inputId = "wavelength",
                          label = "Wavelength (nm)",
                          value = 0.5,
                          min = 0,
                          max = 30,
                          step = 0.1),
           )
    ),
    column(3,
           wellPanel(
             h4(tags$a(href="https://www.thorlabs.co.jp/newgrouppage9.cfm?objectgroup_id=355", "Lens 1", target="_blank", rel="noopener noreferrer")),
             
             numericInput(inputId = "na2",
                          label = "N.A.",
                          value = 0.25,
                          min = 0,
                          max = 2,
                          step = 0.01),
             numericInput(inputId = "fl2",
                          label = "Focal Length (mm)",
                          value = 10.92,
                          min = 0,
                          max = 200,
                          step = 1),
           )
           
    ),
    column(3,
           wellPanel(
             h4(tags$a(href="https://www.thorlabs.co.jp/navigation.cfm?guide_id=2087", "Lens 2", target="_blank", rel="noopener noreferrer")),
             
             numericInput(inputId = "na3",
                          label = "N.A.",
                          value = 0.5,
                          min = 0,
                          max = 2,
                          step = 0.01),
             numericInput(inputId = "fl3",
                          label = "Focal Length (mm)",
                          value = 10,
                          min = 0,
                          max = 200,
                          step = 1),
             numericInput(inputId = "p3",
                          label = "Position (mm)",
                          value = 80,
                          min = 0,
                          max = 500,
                          step = 1),
           )
    ),
    column(3,
           wellPanel(
             h4(tags$a(href="https://www.thorlabs.co.jp/navigation.cfm?guide_id=2365", "Camera", target="_blank", rel="noopener noreferrer")),
             numericInput(inputId = "pixel",
                          label = "Pixel size (um)",
                          value = 3.45,
                          min = 0,
                          max = 20,
                          step = 0.1),
             numericInput(inputId = "diag",
                          label = "Sensor size (mm)",
                          value = 11,
                          min = 0,
                          max = 50,
                          step = 1),
             h4(tags$a(href="https://www.thorlabs.co.jp/navigation.cfm?guide_id=2027", "Objective lens", target="_blank", rel="noopener noreferrer")),
             
             numericInput(inputId = "na4",
                          label = "N.A.",
                          value = 0.5,
                          min = 0,
                          max = 2,
                          step = 0.01),
             
             numericInput(inputId = "obj",
                          label = "Focal length (mm)",
                          value = 10,
                          min = 0,
                          max = 50,
                          step = 1),
             
             
             h4(tags$a(href="https://www.thorlabs.co.jp/newgrouppage9.cfm?objectgroup_id=5834", "Tube lens", target="_blank", rel="noopener noreferrer")),
             
             numericInput(inputId = "tube",
                          label = "Focal length (mm)",
                          value = 200,
                          min = 0,
                          max = 500,
                          step = 10),
           )
    ),  
  ),
)
)
