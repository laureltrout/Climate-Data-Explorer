#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

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






ui <- fluidPage(
  titlePanel("Climate Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_selector", "Select Year:",
                  min = 1895, max = 1980, value = 1895, step = 1)
    ),
    mainPanel(
      plotOutput("precip_plot"),   # Precipitation plot
      plotOutput("vpd_max_plot"),  # VPDMax plot
      plotOutput("vpd_min_plot")   # VPDMin plot
    )
  )
)

server <- function(input, output) {
  output$selected_plot <- renderPlot({
    year <- input$year_selector
    if (year < 1895 || year > 1980) {
      # Handle invalid year (outside available range)
      return(NULL)
    }
  })
  
  output$precip_plot <- renderPlot({
    year <- input$year_selector
    current_precip_raster <- precip_adjusted[[which(years == year)]]
    precip_plot_title <- paste("Average Precipitation", year)
    plot(current_precip_raster, col = my_custom_palette, main = precip_plot_title)
  })
  
  output$vpd_max_plot <- renderPlot({
    year <- input$year_selector
    current_vpd_max_raster <- VPDMax_adjusted[[which(years == year)]]
    vpd_max_plot_title <- paste("Maximum Vapor Pressure Deficit (VPDMax)", year)
    plot(current_vpd_max_raster, col = my_custom_palette, main = vpd_max_plot_title)
  })
  
  output$vpd_min_plot <- renderPlot({
    year <- input$year_selector
    current_vpd_min_raster <- VPDMin_adjusted[[which(years == year)]]
    vpd_min_plot_title <- paste("Minimum Vapor Pressure Deficit (VPDMin)", year)
    plot(current_vpd_min_raster, col = my_custom_palette, main = vpd_min_plot_title)
  })
}

shinyApp(ui = ui, server = server)





