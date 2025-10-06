library(shiny)
library(bslib)
library(ggplot2)

ui <- page_fillable(
  # Add custom CSS for slider color
  tags$head(
    tags$style(HTML("
      .irs-bar {
        background: #A90533 !important;
        border-top: 1px solid #A90533 !important;
        border-bottom: 1px solid #A90533 !important;
      }
      .irs-bar-edge {
        background: #A90533 !important;
        border: 1px solid #A90533 !important;
      }
      .irs-single {
        background: #A90533 !important;
      }
      .irs-single:before {
        border-top-color: #A90533 !important;
      }
      .irs-handle {
        background: #A90533 !important;
        border: 1px solid #A90533 !important;
      }
    "))
  ),
  card(
    div(
      style = "position: relative;",
      plotOutput("beta_plot", height = "500px"),
      # Overlay slider on top of the plot
      div(
        style = "position: absolute; top: 20px; right: 20px; background: rgba(255,255,255,0.9); padding: 10px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); width: 220px; z-index: 10;",
        sliderInput("x_value", 
                    "Wait Time:", 
                    min = 0, 
                    max = 5, 
                    value = 2.5, 
                    step = 0.1,
                    width = "200px")
      ),
      # Area display letterbox - positioned to not overlap with y-axis
      div(
        style = "position: absolute; top: 20px; left: 50px; background: rgba(255,255,255,0.95); padding: 10px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.15); border: 1px solid #ddd; z-index: 10;",
        div(
          style = "font-weight: normal; font-size: 14px; color: #333; display: flex; align-items: center; gap: 8px;",
          span("Probability:"),
          span(
            style = "font-size: 14px; color: #A90533; font-weight: bold;",
            textOutput("area_value", inline = TRUE)
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for area calculation
  area_calc <- reactive({
    alpha <- 2.5
    beta <- 3.5
    beta_mode <- (alpha - 1) / (alpha + beta - 2)
    scale_factor <- 2 / beta_mode
    current_x_beta <- input$x_value / scale_factor
    area <- pbeta(pmin(current_x_beta, 1), alpha, beta)
    return(area)
  })
  
  # Output for area value in the letterbox
  output$area_value <- renderText({
    round(area_calc(), 4)
  })
  
  output$beta_plot <- renderPlot({
    # Beta distribution parameters
    alpha <- 2.5
    beta <- 3.5
    
    # The mode of beta distribution is (alpha-1)/(alpha+beta-2)
    # For alpha=2.5, beta=3.5: mode = 1.5/4 = 0.375
    # We want this mode to be at x=2, so we need to scale by 2/0.375 = 5.333
    beta_mode <- (alpha - 1) / (alpha + beta - 2)
    scale_factor <- 2 / beta_mode
    
    # Create x values for the curve (0 to 1 for beta distribution)
    x_curve <- seq(0, 1, length.out = 1000)
    y_curve <- dbeta(x_curve, alpha, beta)
    
    # Scale x values so peak occurs at x=2
    x_display <- x_curve * scale_factor
    
    # Adjust y values to account for the scaling (preserve area properties)
    y_display <- y_curve / scale_factor
    
    # Calculate y-value at current x position for limiting the vertical line
    current_x_beta <- input$x_value / scale_factor
    if (current_x_beta <= 1) {
      y_at_current_x <- dbeta(current_x_beta, alpha, beta) / scale_factor
    } else {
      y_at_current_x <- 0
    }
    
    # Create the plot
    p <- ggplot() +
      # Plot the beta curve
      geom_line(
        aes(x = x_display, y = y_display), 
        linewidth = 1.2, 
        color = "blue"
      ) +
      # Shade the area under the curve
      geom_area(data = data.frame(x = x_display[x_display <= input$x_value], 
                                  y = y_display[x_display <= input$x_value]),
                aes(x = x, y = y), fill = "skyblue", alpha = 0.7) +
      
      # Add vertical line from x-axis to curve only
      geom_segment(aes(x = input$x_value, y = 0, 
                      xend = input$x_value, yend = y_at_current_x), 
                  color = "#A90533", linewidth = 1.5, linetype = "solid") +
      
      # Customize the plot
      labs(x = "Wait Time (min)", y = "Density") +
      theme_classic() +
      theme(text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5)) +
      scale_x_continuous(breaks = seq(0, 5, by = 1)) +
      #xlim(0, 5.5) +
      ylim(0, max(y_display) * 1.1)
    
    p
  })
}

shinyApp(ui = ui, server = server)
