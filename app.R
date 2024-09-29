library(shiny)
library(eegkit)

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
  
  coh_colormap<-array(dim = dim(mean_coherence))
  for (b in 1:5) {
    for (c1 in 1:30){
      for (c2 in 1:30) {
        if (c1==c2){
          coh_colormap[b,c1,c2]='green'
        }else if((mean_coherence[b,c1,c2])>0.75){
          coh_colormap[b,c1,c2]='red'
        }else if((mean_coherence[b,c1,c2])>0.5){
          coh_colormap[b,c1,c2]='pink'
        }else if(mean_coherence[b,c1,c2]>0.25){
          coh_colormap[b,c1,c2]='mistyrose'
        }else{
          coh_colormap[b,c1,c2]='white'
        }
      }
    }
  }
  
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
    electrodes <- data.frame(
      name = c("FP1", "FP2",
               "F7", "F3", "FZ", "F4", "F8", 
               "FT7", "FC3", "FCZ", "FC4", "FT8", 
               "T3", "C3", "CZ", "C4", "T4", 
               "TP7", "CP3", "CPZ", "CP4", "TP8", 
               "T5", "P3", "PZ", "P4", "T6", 
               "O1", "OZ", "O2"),
      x = c(-3, 3,
            -6.2, -3, 0, 3, 6.2,
            -7, -3, 0, 3, 7,
            -8, -3, 0, 3, 8,
            -8, -3, 0, 3, 8,
            -7, -3, 0, 3, 7,  
            -3.5, 0, 3.5),
      y = c(9.5, 9.5,
            6.5, 6.5, 6.5, 6.5, 6.5,
            3.5, 3, 3.5, 3, 3.5,
            0, 0, 0, 0, 0,
            -3.5, -3, -3.5, -3, -3.5,
            -6, -6.5, -6.5, -6.5,-6,
            -9.5, -10.5, -9.5) # Occipitals
    )
    
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

