
library(shiny)
library(raster)
library(rgdal)

# Load the 1895 raster
precip1895<-raster("C:/Users/laure/OneDrive/Desktop/Remote Sensing/Average Precipiation/PRISM_ppt_stable_4kmM2_1895_bil/PRISM_ppt_stable_4kmM2_1895_bil.bil")


# Create a vector of file paths for other years
years <- seq(1895, 1980)
precip_file_paths <- paste0("C:/Users/laure/OneDrive/Desktop/Remote Sensing/Average Precipiation/PRISM_ppt_stable_4kmM2_", years, "_bil/PRISM_ppt_stable_4kmM2_", years, "_bil.bil")

# Initialize an empty list to store raster objects
raster_list_ppt <- lapply(precip_file_paths, raster)

# Combine all raster objects into a single raster stack
precip_adjusted <- stack(raster_list_ppt)


my_custom_palette <- rainbow(100)
# Define UI for application that draws a histogram









VPD_max1895<-raster("C:/Users/laure/OneDrive/Desktop/Remote Sensing/Max VPD data/PRISM_vpdmax_stable_4kmM3_1895_bil/PRISM_vpdmax_stable_4kmM3_1895_bil.bil")

max_VPD_file_paths <- paste0("C:/Users/laure/OneDrive/Desktop/Remote Sensing/Max VPD data/PRISM_vpdmax_stable_4kmM3_", years, "_bil/PRISM_vpdmax_stable_4kmM3_", years, "_bil.bil")

# Initialize an empty list to store raster objects
raster_list_VPDMax <- lapply(max_VPD_file_paths, raster)

# Combine all raster objects into a single raster stack
VPDMax_adjusted <- stack(raster_list_VPDMax)










VPD_min1895<-raster("C:/Users/laure/OneDrive/Desktop/Remote Sensing/Min VPD data/PRISM_vpdmin_stable_4kmM3_1895_bil/PRISM_vpdmin_stable_4kmM3_1895_bil.bil")

min_VPD_file_paths <- paste0("C:/Users/laure/OneDrive/Desktop/Remote Sensing/Min VPD data/PRISM_vpdmin_stable_4kmM3_", years, "_bil/PRISM_vpdmin_stable_4kmM3_", years, "_bil.bil")

# Initialize an empty list to store raster objects
raster_list_VPDMin <- lapply(min_VPD_file_paths, raster)

# Combine all raster objects into a single raster stack
VPDMin_adjusted <- stack(raster_list_VPDMin)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Climate Data Scatterplot Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "x", label = "X-axis variable:",
                  choices = c("precipitation", "VPDMax", "VPDMin"),
                  selected = "precipitation"),
      selectInput(inputId = "y", label = "Y-axis variable:",
                  choices = c("precipitation", "VPDMax", "VPDMin"),
                  selected = "VPDMax"),
      sliderInput(inputId = "year_selector", label = "Select Year:",
                  min = 1895, max = 1980, value = 1895, step = 1)
    ),
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$scatterplot <- renderPlot({
    # Filter data based on selected year
    current_year <- input$year_selector
    current_precip <- precip_adjusted[[which(years == current_year)]]
    current_VPDMax <- VPDMax_adjusted[[which(years == current_year)]]
    current_VPDMin <- VPDMin_adjusted[[which(years == current_year)]]
    
    # Create scatterplot based on user-selected variables
    if (input$x == "precipitation") {
      x_data <- current_precip
    } else if (input$x == "VPDMax") {
      x_data <- current_VPDMax
    } else {
      x_data <- current_VPDMin
    }
    
    if (input$y == "precipitation") {
      y_data <- current_precip
    } else if (input$y == "VPDMax") {
      y_data <- current_VPDMax
    } else {
      y_data <- current_VPDMin
    }
    
    ggplot(data = data.frame(x = values(x_data), y = values(y_data)),
           aes(x = x, y = y)) +
      geom_point() +
      labs(x = input$x, y = input$y) +
      theme_minimal()
  })
}

# Run the Shiny app
shinyApp(ui, server)