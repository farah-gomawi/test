library(shiny)
library(eegkit)

source('githubtest.R')
# Define the UI
ui <- fluidPage(
  titlePanel("Coherence of EEG Frequency Bands"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "band", 
        "Select Band:", 
        choices = c("Band 1: Delta", "Band 2: Theta", "Band 3: Alpha", "Band 4: Beta", "Band 5: Gamma"),
        selected = "Band 1: Delta"
      ),
      p("Hover over a channel to set it as the reference channel.")
    ),
    mainPanel(
      plotOutput("eegPlot", hover = hoverOpts("plot_hover"))
    )
  )
)

server <- function(input, output, session) {
  ref_channel <- reactiveVal(2)
    channels <- c("FP1", "FP2", "F7", "F3", "FZ", "F4", "F8", "FT7", "FC3", "FCZ", "FC4", "FT8", "T7",
                "C3", "CZ", "C4", "T8", "TP7", "CP3", "CPZ", "CP4", "TP8", "P7", "P3", "PZ", "P4", "P8", 
                "O1", "OZ", "O2")
  
  set.seed(123)
  n_bands <- 5
  n_channels <- length(channels)
  
  #removed coh_color map

  
  output$eegPlot <- renderPlot({
    band_index <- switch(input$band, 
                         "Band 1: Delta" = 1, 
                         "Band 2: Theta" = 2, 
                         "Band 3: Alpha" = 3, 
                         "Band 4: Beta" = 4, 
                         "Band 5: Gamma" = 5)
    
    # Get the current reference channel
    ref_ch <- ref_channel()
    
    # Set the colors for each channel
    col.point <- coh_colormap[band_index, ref_ch, ]

    #removed electrodes
    eegcap2d(channels,col.point = col.point)

    #text(electrodes$x, electrodes$y, labels = channels, pos = 3, cex = 0.7)
    
    # Highlight the reference channel
    points(electrodes$x[ref_ch], electrodes$y[ref_ch], col = "green", pch = 19, cex = 1.5)
  })
  
  # Update reference channel on hover
  observeEvent(input$plot_hover, {
    hover <- input$plot_hover
    
    # Find the closest channel to the hover position
    if (!is.null(hover)) {
      dist <- sqrt((hover$x - electrodes$x)^2 + (hover$y - electrodes$y)^2)
      closest_ch <- which.min(dist)
      
      ref_channel(closest_ch)
    }
  })
}

shinyApp(ui = ui, server = server)

# shinylive::export(appdir = "EEGplot", destdir = "test")
# httpuv::runStaticServer("docs/", port=8008)

