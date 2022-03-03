#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  ranges <- reactiveValues(x = NULL, y = NULL)
  output$scatterplot <- renderPlot({
    
    # Data structure is radius, numerical aperture, focal length, position
    R <- c(input$r1, input$fl2*input$na2, input$fl3*input$na3, input$r1*input$fl3/input$fl2)
    N <- c(input$na1, input$na2, input$na3, NA)
    FL <- c(NA, input$fl2, input$fl3, NA)
    P <- c(0,  input$fl2, input$p3, input$p3 + input$fl3)
    G <- c("#7CAE00", "lightblue", "lightblue", "#7CAE00")
    
    df <- data.frame(Radius = R, Numerical_Aperture = N, Focal_Length = FL, Position = P, Group = G)
    df2 <- data.frame(Radius = input$diag/(input$tube/input$obj)/2, Position = input$p3 + input$fl3 + 2)
    
    p1 <- ggplot(data = df, aes(x = P, y = R)) +
      theme(text = element_text(size = 18)) +
      xlab("Distance (mm)") + 
      ylab("Radius (mm)") +
      
      # Sizes and positions of components
      geom_segment(aes(x = P, y = -R, xend = P, yend = R), color = G, size = 2) +
      geom_errorbar(data = df2, aes(x = Position, y = 0, ymin = -Radius, ymax = Radius), width = 2) +
      annotate("text", x = P[1], y = R[1] + 1, label="Source") +
      annotate("text", x = P[2], y = R[2] + 1, label="Lens 1") +
      annotate("text", x = P[3], y = R[3] + 1, label="Lens 2") +
      annotate("text", x = input$p3 + input$fl3 + 2, y = R[4] + 1, label="FOV") +
      scale_y_continuous(limits = c(ifelse(-max(df$Radius) < -R[1]/P[2]*(P[3] - P[2]) -FL[2]*ifelse(N[2] > N[1], N[2], N[1]), -max(df$Radius), -R[1]/P[2]*(P[3] - P[2]) -FL[2]*ifelse(N[2] > N[1], N[2], N[1])), max(df$Radius) + 2)) +
      scale_x_continuous(limits = c(0, P[3] + FL[3] + 3)) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)
    
    if(input$off_axis){
      p1 <- p1 +
        
        ## From light source to L1
        # Top ray 
        geom_segment(aes(x = 0, y = R[1], xend = P[2], yend = FL[2]*N[1] + R[1]), color = ifelse(FL[2]*N[1] + R[1] > FL[2]*N[2], "gray", "blue")) +
        geom_segment(aes(x = 0, y = R[1], xend = P[2], yend = FL[2]*N[2]), color = "blue", alpha = ifelse(FL[2]*N[1] + R[1] > FL[2]*N[2], 1, 0)) +
        # Middle ray
        geom_segment(aes(x = 0, y = R[1], xend = P[3], yend = -R[1]/P[2]*P[3] + R[1]), color="blue") +
        # Bottom ray
        geom_segment(aes(x = 0, y = R[1], xend = P[2], yend = -FL[2]*N[2]), color="blue", alpha = ifelse(-FL[2]*N[1] + R[1] > -FL[2]*N[2], 0, 1)) +
        geom_segment(aes(x = 0, y = R[1], xend = P[2], yend = -FL[2]*N[1] + R[1]), color = ifelse(-FL[2]*N[1] + R[1] < -FL[2]*N[2], "gray", "blue")) +
        
        ## From L1 to L2
        # Top ray
        geom_segment(aes(x = P[2], y = FL[2]*N[1] + R[1], xend = P[3], yend = -R[1]/P[2]*(P[3]-P[2]) + FL[2]*N[1] + R[1]), color = ifelse(FL[2]*N[1] + R[1] > FL[2]*N[2], "gray", "blue")) +
        geom_segment(aes(x = P[2], y = FL[2]*N[2], xend = P[3], yend = -R[1]/P[2]*(P[3]-P[2]) + FL[2]*N[2]), color="blue", alpha = ifelse(FL[2]*N[1] + R[1] > FL[2]*N[2], 1, 0)) +
        # Bottom ray
        geom_segment(aes(x = P[2], y = -FL[2]*N[2], xend = P[3], yend = -R[1]/P[2]*(P[3] - P[2]) -FL[2]*N[2]), color="blue", alpha = ifelse(-FL[2]*N[1] + R[1] > -FL[2]*N[2], 0, 1)) +
        geom_segment(aes(x = P[2], y = -FL[2]*N[1] + R[1], xend = P[3], yend = -R[1]/P[2]*(P[3] - P[2]) - FL[2]*N[1] + R[1]), color = ifelse(-FL[2]*N[1] + R[1] < -FL[2]*N[2], "gray", "blue")) +
        
        ## From L2 to image
        # Top ray
        geom_segment(aes(x = P[3], y = -R[1]/P[2]*(P[3]-P[2]) + FL[2]*N[1] + R[1], xend = P[3] + FL[3], yend = -R[1]/P[2]*FL[3]), color = ifelse(FL[2]*N[1] + R[1] > FL[2]*N[2], "gray", "blue")) +
        geom_segment(aes(x = P[3], y = -R[1]/P[2]*(P[3]-P[2]) + FL[2]*N[2], xend = P[3] + FL[3], yend = -R[1]/P[2]*FL[3]), color="blue", alpha = ifelse(FL[2]*N[1] + R[1] > FL[2]*N[2], 1, 0)) +
        # Middle ray 
        geom_segment(aes(x = P[3], y = -R[1]/P[2]*P[3] + R[1], xend = P[3] + FL[3], yend = -R[1]/P[2]*FL[3]), color="blue") +
        # Bottom ray 
        geom_segment(aes(x = P[3], y = -R[1]/P[2]*(P[3] - P[2]) -FL[2]*N[2], xend = P[3] + FL[3], yend = -R[1]/P[2]*FL[3]), color="blue", alpha = ifelse(-FL[2]*N[1] + R[1] > -FL[2]*N[2], 0, 1)) +
        geom_segment(aes(x = P[3], y = -R[1]/P[2]*(P[3] - P[2]) - FL[2]*N[1] + R[1], xend = P[3] + FL[3], yend = -R[1]/P[2]*FL[3]), color = ifelse(-FL[2]*N[1] + R[1] < -FL[2]*N[2], "gray", "blue"))
      
      
    }
    
    if(input$on_axis){
      p1 <- p1 + 
        
        ## From light source to L1
        geom_segment(aes(x = 0, y = 0, xend = P[2], yend = FL[2]*N[1]), color = ifelse(FL[2]*N[1] > FL[2]*N[2], "gray", "red")) +
        geom_segment(aes(x = 0, y = 0, xend = P[2], yend = FL[2]*N[2]), color = "red", alpha = ifelse(FL[2]*N[1] > FL[2]*N[2], 1, 0)) +
        # Middle ray
        geom_segment(aes(x = 0, y = 0, xend = P[3], yend = 0), color="red") +
        # Bottom ray
        geom_segment(aes(x = 0, y = 0, xend = P[2], yend = -FL[2]*N[2]), color = "red", alpha = ifelse(-FL[2]*N[1] > -FL[2]*N[2], 0, 1)) +
        geom_segment(aes(x = 0, y = 0, xend = P[2], yend = -FL[2]*N[1]), color = ifelse(-FL[2]*N[1] < -FL[2]*N[2], "gray", "red")) +
        
        ## From L1 to L2
        # Top ray
        geom_segment(aes(x = P[2], y = FL[2]*N[1], xend = P[3], yend = FL[2]*N[1]), color = ifelse(FL[2]*N[1] > FL[2]*N[2], "gray", "red")) +
        geom_segment(aes(x = P[2], y = FL[2]*N[2], xend = P[3], yend = FL[2]*N[2]), color = "red", alpha = ifelse(FL[2]*N[1] > FL[2]*N[2], 1, 0)) +
        # Bottom ray
        geom_segment(aes(x = P[2], y = -FL[2]*N[2], xend = P[3], yend = -FL[2]*N[2]), color = "red", alpha = ifelse(-FL[2]*N[1] > -FL[2]*N[2], 0, 1)) +
        geom_segment(aes(x = P[2], y = -FL[2]*N[1], xend = P[3], yend = -FL[2]*N[1]), color = ifelse(-FL[2]*N[1] < -FL[2]*N[2], "gray", "red")) +
        
        ## From L2 to image
        # Top ray
        geom_segment(aes(x = P[3], y = FL[2]*N[1], xend = P[3] + FL[3], yend = 0), color = ifelse(FL[2]*N[1] > FL[2]*N[2], "gray", "red")) +
        geom_segment(aes(x = P[3], y = FL[2]*N[2], xend = P[3] + FL[3], yend = 0), color = "red", alpha = ifelse(FL[2]*N[1] > FL[2]*N[2], 1, 0)) +
        # Middle ray 
        geom_segment(aes(x = P[3], y = 0, xend = P[3] + FL[3], yend = 0), color="red") +
        # Bottom ray 
        geom_segment(aes(x = P[3], y = -FL[2]*N[2], xend = P[3] + FL[3], yend = 0), color = "red", alpha = ifelse(-FL[2]*N[1] > -FL[2]*N[2], 0, 1)) +
        geom_segment(aes(x = P[3], y = -FL[2]*N[1], xend = P[3] + FL[3], yend = 0), color =  ifelse(-FL[2]*N[1] < -FL[2]*N[2], "gray", "red"))
      
    }
    
    
    p1
    
  })
  
  output$result1 <- renderText({ 
    M <- input$tube/input$obj
    paste0("Magnification: ", round(M, digits=1))
  })
  output$result2 <- renderText({ 
    d <- input$wavelength/(2*input$na4)
    paste0("Rayleigh resolution: ", round(d, digits=2), " um")
  })
  output$result3 <- renderText({ 
    FOV <- input$diag/(input$tube/input$obj)
    paste0("FOV: ", FOV, " mm")
  })
  output$result4 <- renderText({ 
    a <- 2*input$r1*(input$fl3/input$fl2)
    paste0("Spot size: ", round(a, digits=2), " mm")
  })
  output$result5 <- renderText({ 
    l_max <- input$fl2*(input$fl3*input$na3 - input$fl2*input$na2)/input$r1
    paste0("Max Lens 1 to Lens 2 distance: ", round(l_max), " mm")
  })
  output$result6 <- renderText({ 
    sample <- input$pixel/(input$tube/input$obj)
    paste0("Sampling frequency: ", round(sample, digits=2), " um")
  })
  
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  observeEvent(input$use_objective == TRUE, {
    updateNumericInput(session, "na3", value = input$na4)
    updateNumericInput(session, "fl3", value = input$obj)
  }, ignoreInit = TRUE)
  
})
