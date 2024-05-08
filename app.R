### West, Williams, Glasby  Dec 2021
## A Shiny map to display and explore the estuarine macorphyte mapping data

# Last update Nov 2021

# v1.0.1

# credit needs to be given to NSW Department of Primary Industries - Fisheries

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# The following global code is run at the opening of the App and provides all the packages and data required for the global environment.

# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(raster)) install.packages("raster", repos = "http://cran.us.r-project.org")
if(!require(lazyeval)) install.packages("lazyeval", repos = "http://cran.us.r-project.org")
if(!require(htmltools)) install.packages("htmltools", repos = "http://cran.us.r-project.org")
if(!require(leaflet.esri)) install.packages("leaflet.esri", repos = "http://cran.us.r-project.org")
#if(!require(stars)) install.packages("leaflet.esri", repos = "http://cran.us.r-project.org")
if(!require(leafem)) install.packages("leaflet.esri", repos = "http://cran.us.r-project.org")
if(!require(leaflet.extras2)) install.packages("leaflet.extras2", repos = "http://cran.us.r-project.org")
if(!require(Cairo)) install.packages("Cairo", repos = "http://cran.us.r-project.org")
if(!require(png)) install.packages("png", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")



# Read in data .csv
    dat <-  read.csv("Multi_Estuary_All_Years_Grid.csv", stringsAsFactors = T)
    report_dat <-  read.csv("Estuary_Overview.csv", stringsAsFactors = T)
    presence_dat <- read.csv("NSW_Estuarine_Macrophytes_Presence.csv", stringsAsFactors = T)

# read in spatial grid data
    geojson <- sf::st_read("estuary_grids.geojson")
    
# mangrove and saltmarsh plot background image    
    mvsimg <- readPNG("www/Man_SM_Background.png")
    mvsbgimg <- rasterGrob(mvsimg, interpolate=TRUE)

dat$fMap_Year <- factor(dat$Map_Year)

# Transpose data to long format to enable filtering by species.
dat1 <- dat %>%
    gather("Habitat", "Area", 7:16)
    
dat1$Habitat <- factor(dat1$Habitat, levels = c("Seagrass", "Mangrove", "Man_Sm", "Saltmarsh","Posidonia", "Zostera","Halophila","Ruppia", "Man_Biplot", "SM_Biplot"))

# Index/select the default region based on order in the above list i.e. 3 = Port Stephens (at the test state)
# List the regions available, there are only 4 so a list here is easy.
list_region <- c("North Coast","Hunter", "Greater Sydney", "South East")
#Start with Sydney
index_region <- 3

# Index/select the default estuary based on order in the above list i.e. 3 = Port Stephens (at the test state)
# List the estuaries available
list_estuary <- c(levels(presence_dat$Estuary))
index_estuary <- 1

list_habs <- c(levels(presence_dat$Macrophyte))
index_habs <- 1
# Cell size for building rasters at 250x250m
cell_size <- 250

myproj <- "+proj=utm +zone=56 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 + towgs8"

#Labels for mangrove, mixed and saltmarsh plots
mansm.names <- as_labeller(c("Mangrove"="Mangrove", "Man_Sm"="Mixed mangrove and saltmarsh", "Saltmarsh"="Saltmarsh"))

# Theme for mangrove v saltmarsh plot layout ---- based on the function by Rodrigo Zepeda https://stackoverflow.com/questions/17753101/center-x-and-y-axis-with-ggplot2
theme_geometry <- function(xvals, yvals, xgeo = 0, ygeo = 0, 
                           color = "grey", linewidth = 0.5, 
                           xlab = "Saltmarsh", 
                           ylab = "Mangrove",
                           ticks = 10,
                           textsize = 3,
                           #xlimit = max(62500,62500),
                           #ylimit = max(62500,62500),
                           xlimit = max(abs(xvals),abs(yvals)),
                           ylimit = max(abs(yvals),abs(xvals)),
                           epsilon = max(xlimit,ylimit)/50){
    
    #INPUT:
    #xvals .- Values of x that will be plotted
    #yvals .- Values of y that will be plotted
    #xgeo  .- x intercept value for y axis
    #ygeo  .- y intercept value for x axis
    #color .- Default color for axis
    #size  .- Line size for axis
    #xlab  .- Label for x axis
    #ylab  .- Label for y axis
    #ticks .- Number of ticks to add to plot in each axis
    #textsize .- Size of text for ticks
    #xlimit .- Limit value for x axis 
    #ylimit .- Limit value for y axis
    #epsilon .- Parameter for small space
    
    
    #Create axis 
    xaxis <- data.frame(x_ax = c(-xlimit, xlimit), y_ax = rep(ygeo,2))
    yaxis <- data.frame(x_ax = rep(xgeo, 2), y_ax = c(-ylimit, ylimit))
    
    #Add axis
    theme.list <- 
        list(
            theme_void(), #Empty the current theme
            geom_line(aes(x = x_ax, y = y_ax), color = color, linewidth = linewidth, data = xaxis),
            geom_line(aes(x = x_ax, y = y_ax), color = color, linewidth = linewidth, data = yaxis),
            #annotate("text", x = xlimit + 2*epsilon, y = ygeo, label = xlab, size = 2*textsize, angle = 270),
            #annotate("text", x = xgeo, y = ylimit + 4*epsilon, label = ylab, size = 2*textsize),
            xlim(-xlimit - 7*epsilon, xlimit + 7*epsilon), #Add limits to make it square
            ylim(-ylimit - 7*epsilon, ylimit + 7*epsilon)  #Add limits to make it square
        )
    
    #Add ticks programatically
    ticks_x <- round(seq(-xlimit, xlimit, length.out = ticks),2)
    ticks_y <- round(seq(-ylimit, ylimit, length.out = ticks),2)
    
    #Add ticks of x axis
    nlist <- length(theme.list)
    for (k in 1:ticks){
        
        #Create data frame for ticks in x axis
        xtick <- data.frame(xt = rep(ticks_x[k], 2), 
                            yt = c(xgeo + epsilon, xgeo - epsilon))
        
        #Create data frame for ticks in y axis
        ytick <- data.frame(xt = c(ygeo + epsilon, ygeo - epsilon), 
                            yt = rep(ticks_y[k], 2))
        
        #Add ticks to geom line for x axis
        theme.list[[nlist + 4*k-3]] <- geom_line(aes(x = xt, y = yt), 
                                                 data = xtick, linewidth = linewidth, 
                                                 color = color)
        
        #Add labels to the x-ticks
        # theme.list[[nlist + 4*k-2]] <- annotate("text",
        #                                         x = ticks_x[k],
        #                                         y = ygeo - 2.5*epsilon,
        #                                         size = textsize,
        #                                         label = paste(ticks_x[k]))
        
        
        #Add ticks to geom line for y axis
        theme.list[[nlist + 4*k-1]] <- geom_line(aes(x = xt, y = yt), 
                                                 data = ytick, linewidth = linewidth, 
                                                 color = color)
        
        #Add labels to the y-ticks
        # theme.list[[nlist + 4*k]] <- annotate("text",
        #                                       x = xgeo - 2.5*epsilon,
        #                                       y = ticks_y[k],
        #                                       size = textsize,
        #                                       label = paste(ticks_y[k]))
    }
    
    #Add theme
    #theme.list[[3]] <- 
    return(theme.list)
}


# User Interface ----
# The User Interface code controls how the App is displayed (colours, layout, menus etc)
# The code is broken down tab by tab

# Overall layout of App.
ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                 title = div(img(src= "DPI_LOGO_left.svg","Estuarine Habitat Dashboard",
                                 style="margin-top: -15px;margin-left: -30;padding-right:10px", 
                                 height = 50)),
                 windowTitle = "NSW Estuarine Mapping",
                 position = c("fixed-top"),
                 header=singleton(tags$head(includeHTML(("google-analytics.html")))),
                 

                 
#### Habitat Explorer Tab UI                                  -----
                 tabPanel(
                     "Interactive Explorer",
                     
                     #hide red error messages
                     tags$style(type="text/css",
                                ".shiny-output-error { visibility: hidden; }",
                                ".shiny-output-error:before { visibility: hidden; }"
                     ),
                     
                     div(class="outer",
                         tags$head(includeCSS("styles.css")),
                         leafletOutput("map", width = "100%", height = "100%"),
                       
                         absolutePanel(id = "controls", class = "panel panel-default",
                                       top = 160, left = 11, width = 250, height = "auto",
                                       fixed = TRUE, draggable = FALSE,
                                       tags$div(id = 'demo',  class="collapse in",
                                       h3("Map Explorer", style="text-align:center"),
                                    
                                       h6("Select a region, estuary and the desired year to view the mapped data"),
                                       selectInput("region1", 
                                                   label = "Region:", 
                                                   choices = list_region, 
                                                   selected = list_region[index_region]),
                                       selectInput("estuary1", 
                                                   label = "Estuary:", 
                                                   choices = NULL, 
                                                   selected = "Pittwater"),
                                       selectInput("date_t1", 
                                                   label = "Year Mapped:", 
                                                   choices = NULL, 
                                                   selected = "2017"),
                                       tags$h6("A detailed list of all mapped estuaries and the features present can be viewed ",
                                              tags$a(href="NSW_Estuaries.html", target="_blank", "here.")),         
                                       tags$h6("The latest macrophyte maps for each estuary can be downloaded from the NSW DPI Fisheries Spatial Data Portal.", 
                                                        tags$br(),tags$br(),
                                                        tags$a(href="https://www.dpi.nsw.gov.au/about-us/science-and-research/spatial-data-portal", target="_blank", 
                                                tags$img(src='Fisheries_Portal.svg')) 
                                               )
                                       
                                       
                                       )
                                       
                         ),
                         absolutePanel(id = "legend1", class = "card", bottom = 25, right = 10,  fixed=TRUE, draggable = FALSE, width= "auto", height = "auto",
                                                                                tags$img(src='Explore_legend.svg',width='375'))
                     )
                 ),


                 
     

##### Change over time tab UI ----
tabPanel(
    "Change over Time",
    div(
        class = "outer",
        tags$head(includeCSS("styles.css")),
        leafletOutput("map_change", width = "100%", height = "100%"),
        
        absolutePanel(
            id = "controls",
            class = "panel panel-default",
            top = 160,
            left = 11,
            width = 250,
            height = "auto",
            fixed = TRUE,
            draggable = FALSE,
            tags$div(id = 'demo1',  class="collapse in",
            h3("Change Analysis", style="text-align:center"),
            h6("The change grid represents the change in area of mapped habitat per 250x250m between the selected times. Due to differences in mapping methods, changes relative to data mapped during the 1980s should be considered as indicative only. Comparisons of time periods after 2000 are considered the most reliable."
            ),
            
            selectInput("region3", 
                        label = "Region:", 
                        choices = list_region, 
                        selected = list_region[index_region]),
            selectInput("estuary3",
                        label = "Estuary:",
                        choices = list_estuary,
                        selected = list_estuary[index_estuary]
            ),
            selectInput("change_t1",
                        label = "Time 1: ",
                        choices = NULL,
                        selected = "1985"
            ),
            selectInput("change_t2", 
                        label = "Time 2: ", 
                        choices = NULL, 
                        selected = "2017"
            ),
            selectInput("habitat3",
                        label= "Habitat for change analysis: ",
                        choices = list_habs, 
                        selected = list_habs[index_habs]
            ),
            
        
            tags$h6("The latest macrophyte maps for each estuary can be downloaded from the NSW DPI Fisheries Spatial Data Portal.", 
                    tags$br(),tags$br(),
                    tags$a(href="https://www.dpi.nsw.gov.au/about-us/science-and-research/spatial-data-portal", target="_blank", 
                           tags$img(src='Fisheries_Portal.svg')), 
                    
                    absolutePanel(id = "legend1", class = "card", bottom = 300, right = 15,  fixed=TRUE, draggable = FALSE, width= "auto", height = "auto",
                                  tags$img(src='Macrophyte_Legend.svg',width='170'))
            ),
            absolutePanel(id = "legend1", class = "card", bottom = 20, right = 15,  fixed=TRUE, draggable = FALSE, width= "auto", height = "auto",
                          tags$img(src='Grid_Legend_Curved.svg',height='275',width='170'))
            
            )
        ),
        # Below is the code for the small disclaimer panel the displays on the Change over time tab, the commented section below can be un-commented to re-enable it
        
        # absolutePanel(
        #     id = "disclaimer",
        #     #class = "panel panel-default",
        #     top = 92,
        #     left = 280,
        #     width = 450,
        #     #height = "auto",
        # 
        #     tags$div(id = 'discDiv',  class="collapse in",
        #              h3("Disclaimer", style="text-align:center"),
        #              h6("Mapping methods were far less sophisticated (and at a coarser scale) for the earliest maps, hence changes in the extent of all habitats relative to 1980s data are indicative only.  Comparisons of time periods after 2000 are considered the most reliable.",
        #                 tags$br(),tags$br(),
        #                 "Although small patches of habitat (< 20 m",tags$sup(2), ") have been identified in some recent maps, these cannot be consistently identified.",
        #                 tags$br(),tags$br(),
        # 
        #                 actionButton('agreeBtn', 'OK', "data-toggle"='collapse', "data-target"='#disclaimer'),
        #              ),
        # 
        # 
        #     )
        # )
        
        
    )
    
),


        
                                
##### Data Summary Tab UI                  ----
tabPanel(
    "Data Summary",
    fluidPage(
        tags$style(type="text/css", "body {padding-top: 70px; width: 1140px; text-align: justify; margin: auto;}"),
        fluidRow(
            h3("Estuary overview"),
            h5("Select the region then select the estuary of interest."),
            div(style="display:flex",
            selectInput("region2", 
                        label = "Region:", 
                        choices = list_region, 
                        selected = list_region[index_region]),
            selectInput("estuary2", 
                        label = "Estuary:", 
                        choices = list_estuary, 
                        selected = list_estuary[index_estuary])
            )
        ),
        
        fluidRow(
            column(11, offset = 0,
                   
                   tags$h3(htmlOutput("summary_title")
                   ),
                   tags$p(htmlOutput("estuary_type")
                   ),
                   tags$p(htmlOutput("catchment_area")
                   ),
                   tags$p(htmlOutput("estuary_area")
                   ),
                   tags$p(htmlOutput("ave_depth")
                   ),
                   tags$b("Years mapped: "), textOutput("years_mapped"),
                   
                   
                   
                   tags$br(),
                   
                    #tags$p("Macrophyte maps for the latest time of sampling can be downloaded from the", 
                    #          tags$a(href="https://webmap.industry.nsw.gov.au/Html5Viewer/index.html?viewer=Fisheries_Data_Portal", target="_blank", "NSW Fisheries Spatial Data Portal.") ) 
                   
                   
            )),
        
        fluidRow(
            tags$br(),tags$br(),
            
            h4("Change in mapped area of seagrass"),
            
            tags$p("Change in area (hectares) of mapped seagrass extent for the selected estuary. The areas presented below represent 100% of the mapped area for each genus. 
                   This includes areas of overlap where multiple genera occur. As such, totalling the values from these figures may over estimate the total amount of seagrass present 
                   in the selected estuary. Due to differences in mapping techniques, comparison of areas from the data mapped during the 1980s (light grey columns) should be used as an indication of the direction 
                   of change (increase vs decrease) rather than the magnitude of area change.  In general, maps from the 1980s tended to overestimate areas of 
                   large habitat patches and may have missed small patches."),
            
            column(6, offset = 0,
                   style = "height:600px; ", 
                   plotOutput("habitat_plot", width = "100%"))
        ),
        
        fluidRow(
            tags$br(),tags$br(),
            
            h4("Change in mapped area of mangrove and saltmarsh"),
            tags$p("Change in area (hectares) of mapped mangrove and saltmarsh for the selected estuary. Areas in the figures below represent the total mapped area for each 
                   habitat for each time. Due to differences in mapping techniques, comparison of areas from the data mapped during the 1980s (light grey columns) should be used as an indication of the 
                   direction of change (increase vs decrease) rather than the magnitude of change area change."),
            
            column(6, offset = 0,
                   style = "height:350px; ", 
                   plotOutput("mansm_plot", width = "100%")),
            tags$br(),tags$br(),     
            ),
        
        # commented out for biplot to be added later.
        # fluidRow(
        #     tags$br(),
        #     
        #     h4("Relative change of mangrove and saltmarsh"),
        #     tags$p("Relative changes in the area of mangrove to saltmarsh over time are presented below. 
        #            This analysis represents the changes in area between these two habitats within each 250m x 250m grid in which both mangrove and saltmarsh are present for the selected times. 
        #            Areas for mangrove and saltmarsh where classified as mixed polygons have been calculated as 100% of the area for each habitat. Data are measures of relative change and as such no scale has been included on the axes."),
        #     
        #     
        # ),
    #     fluidRow(column(4,
    #                     selectInput("mvs_t1",
    #                     label = "Year (Time 1): ",
    #                     choices = NULL,
    #                     selected = "1985"
    #         
    #                     )
    #         ),
    #         column(4,
    #                 selectInput("mvs_t2", 
    #                 label = "Latest Year (Time 2): ", 
    #                 choices = NULL, 
    #                 selected = "2017"
    #         )
    #         ),
    # 
    #         tags$br(),
    #         tags$br(),
    #         
    #         column(6, offset = 2,
    #                style = "height:50px; "
    #         
    #         ),
    #         column(12, align="center",
    #                style = "height:700px; ",
    #                plotOutput("MVS", width = "100%"))
    # 
    # )
    )
    ),
    
#### Predictive Modelling Tab UI                                  -----
tabPanel(
  "Projected Wetlands",
  
  div(class="outer",
      tags$head(includeCSS("styles.css")),
      leafletOutput("slrmap", width = "100%", height = "100%"),
      
      absolutePanel(id = "controls", class = "panel panel-default",
                    top = 160, left = 11, width = 250, height = "auto",
                    fixed = TRUE, draggable = FALSE,
                    tags$div(id = 'demo',  class="collapse in",
                             h4("Projected distribution of mangrove and saltmarsh in NSW estuaries under sea level rise", style="text-align:center"),
                             
                             h6("The projected distributions of mangrove and saltmarsh in this dataset were developed using the method described in", 
                                tags$a(href="https://www.frontiersin.org/articles/10.3389/fenvs.2022.749950/full", target="_blank", "Hughes et al (2022).")),
                             
                             tags$h6("Two scenarios are presented: Present day and +0.5m sea level rise (SLR). Both assume all land use classes, except urban/developed, can be colonized by wetlands. Details of the landuse categories used can be found ", 
                                     tags$a(href="Landuse.html", target="_blank", "here"),
                                     
                                     
                                     tags$br(),
                                     
                                     h6("More information can be found on the About tab."),                               
                                     
                                     h6("Select a region and estuary to zoom to the desired area"),  
                                     
                                     selectInput("region4", 
                                                 label = "Region:", 
                                                 choices = list_region, 
                                                 selected = list_region[index_region]),
                                     selectInput("estuary4", 
                                                 label = "Estuary:", 
                                                 choices = NULL, 
                                                 selected = "Pittwater"),
                                     
                                     
                                     tags$a(href="https://datasets.seed.nsw.gov.au/dataset/projected-distribution-of-mangrove-and-saltmarsh-in-nsw-estuaries-under-sea-level-rise", target="_blank", 
                                            tags$img(src='SEED_Portal.svg')) 
                             )
                             
                    )    
      )
      
  ),
  absolutePanel(id = "legend1", class = "card", top = 205, right = 10,  fixed=TRUE, draggable = FALSE, width= "170", height = "auto",
                tags$img(src='Predicted_man_sm.svg')
  ),
  absolutePanel(id = "legend1", class = "card", bottom = 25, right = 10, dropdownButton(label = "Land use legend", circle = FALSE, size = 'sm', up = TRUE, right = TRUE,
                                                                                        tags$img(src='Landuse2017_Horizontal_legend.svg',width='650')), style = "z-index: 1000;"
  )
),

##### About Tab ----
tabPanel(
  "About",
  tags$div(
    tags$h3("Disclaimer"),
    tags$p("The information contained in this website is based on knowledge and understanding at the time of publication (July 2021) and may not be accurate, current or complete. The State of New South Wales (including the Department of Regional NSW), the authors and the publisher take no responsibility, and will accept no liability, for the accuracy, currency, reliability or correctness of any information included in the document (including material provided by third parties). Readers should make their own inquiries and rely on their own advice when making decisions related to material contained in this publication."),
    
    tags$h3("Overview"),
    tags$p("This website and the most recent maps were developed as part of the NSW Marine Estate Management Strategy (Initiative 2.5 'Delivering healthy coastal habitats with sustainable use and development'), which was funded by the NSW Government."),
    tags$p("More details on the estuarine habitats of NSW can be found on the ",
           tags$a(href="https://www.dpi.nsw.gov.au/fishing/habitat/aquatic-habitats/estuarine", target="_blank", " NSW DPI Website")),
    tags$p("A detailed list of all mapped estuaries can be viewed ",
           tags$a(href="NSW_Estuaries.html", target="_blank", "here.")),
    tags$p("A full list of changes and updates to the dashboard can be viewed in the ",
           tags$a(href="Changelog.html", target="_blank", "Changelog.")),  
    tags$p("Macrophyte maps for the latest time of mapping can be downloaded from the", 
           tags$a(href="https://www.dpi.nsw.gov.au/about-us/science-and-research/spatial-data-portal", target="_blank", "NSW Fisheries Spatial Data Portal.")),
    
    tags$h3("Mapping Details"),
    tags$p("Methods used for mapping are described in ", tags$a(href="https://www.dpi.nsw.gov.au/__data/assets/pdf_file/0004/306625/AE_2009_Output-1575_Creese-et-al_Habitat-Mapping-Final-Report-113_REPORT.pdf", target="_blank", "Creese et al. (2009)"), "and ", tags$a(href="https://doi.org/10.1007/s12237-021-01026-w", target="_blank", "West & Glasby (2021). "), "Mangrove and saltmarsh species have not been distinguished in these maps.  Saltmarshes include the low-marsh species described in Sainty (2012) and exclude higher-marsh species such as", 
           tags$span(tags$em("Phragmites australis")),
           "and ", 
           tags$span(tags$em("Baumea juncea", .noWS = "outside"),.noWS = "outside")),
    tags$p("The 2019 - 2020 Fire Damage layer represents an estimate of areas of mangrove and saltmarsh damaged in affected estuaries due to the 2020 NSW bushfires. Imagery was sourced from Nearmap and fire damage was mapped using eCognition. The layer may contain omissions or additional damage not present. More details can be found ", tags$a(href="NSW_Fire_Impacted_Estmac_metadata.html", target="_blank", "here.")),
    tags$p("Foreshore mapping was completed as part of the Mapping the habitats of NSW estuaries project. Details on the estuaries mapped can be found in ", tags$a(href="https://www.dpi.nsw.gov.au/__data/assets/pdf_file/0004/306625/AE_2009_Output-1575_Creese-et-al_Habitat-Mapping-Final-Report-113_REPORT.pdf", target="_blank", "Creese et al. (2009)")),
    tags$p("Artificial structures (pontoons and jetties) and habitat disturbances are currently being mapped and will be updated when available."),
    
    tags$h3("Projected distribution of mangrove and saltmarsh in NSW estuaries under sea level rise"),
    tags$p("The projected distributions of mangrove and saltmarsh in this dataset were developed using the method described in", 
           tags$a(href="https://www.frontiersin.org/articles/10.3389/fenvs.2022.749950/full", target="_blank", "Hughes et al (2022)."), "It’s important to note that impacts of sea level rise (SLR) on wetland distribution will depend on their ability to migrate upslope and/or grow vertically due to the gradual accumulation of new layers of sediment (accretion).  Sediment accretion was not considered in these models as data are unavailable at a regional scale. "),
    
    tags$p("Two projected scenarios are presented: Present day and +0.5m SLR. Both assume all land use classes, except urban/developed, can be colonized by wetlands. For SE Australia, +0.5m SLR is estimated to occur by 2100 under emissions scenarios RCP 2.6 to RCP 8.5", tags$a(href="https://www.nature.com/articles/s41598-018-25410-y", target="_blank", "(Hanslow et al., 2018.)")),
    
    tags$p("Details of the land use categories used to delineate urban/developed can be found ", tags$a(href="Landuse.html", target="_blank", "here."), "Due to data constraints projected distributions were only able to be created for 110 estuaries and a list of these estuaries and accuracy assessment results for the present day modelling can be found" , tags$a(href=" https://datasets.seed.nsw.gov.au/dataset/266cdf74-2da9-48cb-8ba1-acdce66928b7/resource/1d05f43d-095f-492b-9110-28a0a4b42923/download/table_1.pdf", target="_blank", "here.")),
    
    tags$h3("Acknowledgements"),
    tags$p("The data used here is derived from many decades of work by DPI Fisheries staff.  The earliest macrophyte mapping project was led by Ron West and maps were created by Trudy Walford and Carol Thorogood. See ", tags$a(href="West_1985.pdf", target="_blank", "West et al. 1985")),
    tags$p("Subsequent mapping projects were led by Rob Williams, Bob Creese, Greg West and Tim Glasby.  We thank the many staff involved in producing these maps, including, but not limited to: Peter Gibson, Roger Laird, Isabelle Thiebaud, Brooke McCartin, Chris Gallen, Danielle Morrison, Joe Neilson and Trudy Walford."),
    tags$p("This website and the most recent maps were funded by the NSW State Government via the Marine Estate Management Strategy.  Funding for older mapping projects was from numerous NSW State Government sources and short-term contracts funded by organisations including: Lake Macquarie City Council, Eurobodalla Shire Council, North Rivers Catchment Management Authority, Sydney Metro Catchment Management Authority and NSW Roads and Maritime"),
    
    tags$br(),
    tags$p(strong("Concept/design:"), "Tim Glasby & Greg West"),
    tags$p(strong("Shiny App development:"), "Greg West & Joel Williams"),
    tags$p(strong("Data compilation:"), "Greg West"),
    tags$p(strong("Last mapping update:"), "Jan 2023"),
    tags$p(strong("Last Shiny App update:"), "Jan 2023"),
    tags$br(),
    
    tags$h3("References"),
    tags$a(href="https://doi.org/10.1111/emr.12572", tags$p("Glasby, T.M., Gibson, P.T., Laird, R., Swadling, D.S. and West, G. (2023), Black summer bushfires caused extensive damage to estuarine wetlands in New South Wales, Australia. Ecol Manag Restor."), target="_blank"),
    tags$a(href="https://doi.org/10.3389/fenvs.2022.749950", tags$p("Hughes MG, Glasby TM, Hanslow DJ, West GJ, Wen L. (2022). Random Forest Classification Method for Predicting Intertidal Wetland Migration Under Sea Level Rise. Frontiers in Environmental Science 10."), target="_blank"),
    tags$a(href="https://doi.org/10.1002/aqc.3905", tags$p("Swadling DS, West GJ, Gibson PT, Laird RJ, Glasby TM. (2022). Don't go breaking apart: Anthropogenic disturbances predict meadow fragmentation of an endangered seagrass. Aquatic Conservation: Marine and Freshwater Ecosystems"), target="_blank"),
    tags$a(href="https://doi.org/10.1007/s12237-021-01026-w", tags$p("West, G., Glasby, T.M., (2021). Interpreting long-term patterns of seagrasses abundance: how seagrass variability is dependent on genus and estuary type. Estuaries and Coasts."), target="_blank"),
    tags$p("G. Sainty, J. Hosking, G. Carr, & P. Adam (Eds.). (2012). Estuary plants and what's happening to them in South-East Australia. Sainty and Associates Pty Ltd."),
    tags$a(href="https://www.dpi.nsw.gov.au/__data/assets/pdf_file/0004/306625/AE_2009_Output-1575_Creese-et-al_Habitat-Mapping-Final-Report-113_REPORT.pdf", tags$p("Creese, R.G., Glasby , T.M., West, G., Gallen, C. (2009). Mapping the habitats of NSW estuaries. Industry & Investment NSW, Nelson Bay, Australia, 95pp."), target="_blank"),
    tags$a(href="West_1985.pdf", tags$p("West, R.J., Thorogood, C.A., Walford, T.R. and Williams R.J. (1985). An estuarine inventory for New South Wales, Australia. Fisheries Bulletin 2. Department of Agriculture, New South Wales. 140 pp."), target="_blank"),
    
    tags$br(),
    tags$br("The Shiny App is created using R v4.2.1, RStudio 2023.03.0 Build 386, and Leaflet v2.1.2"),
    tags$br(),
    
    )
)

)
                             
                   

# Server logic ----
#
#
#
# THe Server codes runs in the background making all the calculation from user choice etc.

server <- function(input, output, session) {

    showModal(modalDialog(
        #title = "NSW DPI Estuarine Macrophyte Dashboard",
        HTML('<center><img src="splashscreen.svg" width="860"></center>'),
        footer = modalButton("OK"),
        size = c("l")
    ))
    
## User selections carry through to each tab forwards and backwards
    observeEvent(
        input$region1,
        updateSelectizeInput(session, "region2", selected = input$region1)
    )
    
    observeEvent(
        input$region1,
        updateSelectizeInput(session, "region3", selected = input$region1)
    )
    
    observeEvent(
        input$region2,
        updateSelectizeInput(session, "region1", selected = input$region2)
    )
    
    observeEvent(
        input$region3,
        updateSelectizeInput(session, "region1", selected = input$region3)
    )
    observeEvent(
        input$region3,
        updateSelectizeInput(session, "region2", selected = input$region3)
    )
    
    observeEvent(
        input$estuary1,
        updateSelectizeInput(session, "estuary2", selected = input$estuary1)
    )

    observeEvent(
        input$estuary1,
        updateSelectizeInput(session, "estuary3", selected = input$estuary1)
    )

    observeEvent(
        input$estuary2,
        updateSelectizeInput(session, "estuary1", selected = input$estuary2)
    )

    observeEvent(
        input$estuary3,
        updateSelectizeInput(session, "estuary1", selected = input$estuary3)
    )
    observeEvent(
        input$estuary3,
        updateSelectizeInput(session, "estuary2", selected = input$estuary3)
    )


######## HABITAT EXPLORER TAB ########
    # DATA PREPERATION
    # Update Year selector to only provide the estuaries per region
    observe({
        x0 <- dat %>% dplyr::filter(Region == input$region1) %>% dplyr::select(Estuary)
        updateSelectInput(session, 
                          "estuary1", 
                          label = "Estuary:",
                          choices = unique(x0))
    })
    # Update Year selector to only provide the years that have been mapped
    observe({
        #x1 <- dat %>% dplyr::filter(Estuary == input$estuary1) %>% dplyr::select(Map_Year)
        x1 <- dat$Map_Year[dat$Estuary ==input$estuary1]
        updateSelectInput(session, 
                          "date_t1", 
                          label = "Year:",
                          choices = sort(unique(x1) ,decreasing = TRUE))
    })

    estuary_report_dat <- reactive({ report_dat %>%
            dplyr::filter(Estuary == input$estuary1) 
    })
    

    
    raster_points <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary1) %>%
            dplyr::select(Easting, Northing)
    })    
    
    # Set raster max and min based on estuary selection
    lon_min <- reactive({min(raster_points()$Easting)-10}) 
    lon_max <- reactive({max(raster_points()$Easting)+10})
    lat_min <- reactive({min(raster_points()$Northing)-10})
    lat_max <- reactive({max(raster_points()$Northing)+10})    
    ncols <- reactive({((lon_max() - lon_min())/cell_size)}) 
    nrows <- reactive({((lat_max() - lat_min())/cell_size)})  
    
    # Create and empty raster that covers the estuary selected
    empty_raster <- reactive({
        raster(nrows=nrows(), ncols=ncols(), 
               xmn=lon_min(), xmx=lon_max(), 
               ymn=lat_min(), ymx=lat_max(), 
               res=c(cell_size,cell_size), crs=myproj)
    })

   
    # OUTPUTS  

    
    output$map <- renderLeaflet({
        
        
        leaflet() %>% 
            addMapPane("basemaps", zIndex=200) %>% #add map pane for baselayers
            addMapPane("macromap", zIndex=400) %>% #add map pane for macrophyte map
            addMapPane("othermap", zIndex=450) %>% #add map pane for all other layers
            
            addProviderTiles(providers$CartoDB.Positron, group = "Open Street Map",
                             options = pathOptions(pane="basemaps", maxZoom=17)) %>%
            addEsriTiledMapLayer("http://maps.six.nsw.gov.au/arcgis/rest/services/public/NSW_Imagery/MapServer", group = "Photo basemap",
                                 layerId = "0", pathOptions(pane="basemaps", maxZoom=17)) %>%

            addTiles(urlTemplate = "https://gregjwest.github.io/NSW_EstMacrophytes/jetties_pontoons/{z}/{x}/{y}.png", group = "Artificial structures",
                     options = pathOptions(pane="macromap", maxZoom=17),
                     attribution = "&copy; Department of Primary Industries 2021") %>% 
            #addTiles(urlTemplate = "https://gregjwest.github.io/NSW_EstMacrophytes/mooringarea/{z}/{x}/{y}.png", group = "Mooring Areas",
            #         options = pathOptions(pane="macromap", maxZoom=17, opacity=.6),
            #         attribution = "&copy; Transport for NSW 2020") %>% 
            addTiles(urlTemplate = "https://gregjwest.github.io/NSW_EstMacrophytes/fire_damage/{z}/{x}/{y}.png", group = "2019 - 2020 Fire damage",
                     options = pathOptions(pane="macromap", maxZoom=17, opacity=.8),
                     attribution = "&copy; Department of Primary Industries 2021") %>% 
            addTiles(urlTemplate = "https://gregjwest.github.io/NSW_EstMacrophytes/Disturbance/{z}/{x}/{y}.png", group = "Saltmarsh - Vehicle damage",
                     options = pathOptions(pane="macromap", maxZoom=17, opacity=.8),
                     attribution = "&copy; Department of Primary Industries 2021") %>%
            # addTiles(urlTemplate = "https://gregjwest.github.io/NSW_EstMacrophytes/moorings/{z}/{x}/{y}.png", group = "Boat moorings",
            #          options = pathOptions(pane="macromap", maxZoom=17),
            #          attribution = "&copy; Transport for NSW 2020") %>% 
            addTiles(urlTemplate = "https://gregjwest.github.io/NSW_EstMacrophytes/foreshore/{z}/{x}/{y}.png", group = "Foreshore",
                     options = pathOptions(pane="macromap", maxZoom=17),
                     attribution = "&copy; Department of Primary Industries 2021") %>% 
            addTiles(urlTemplate = "https://gregjwest.github.io/NSW_EstMacrophytes/reef/{z}/{x}/{y}.png", group = "Estuarine reef",
                     options = pathOptions(pane="othermap", maxZoom=17),
                     attribution = "&copy; Department of Primary Industries 2021") %>%
            addTiles(urlTemplate = "https://gregjwest.github.io/latest_mapping/{z}/{x}/{y}.png", group = "Latest mapped habitat",
                     options = pathOptions(pane="macromap", maxZoom=17),
                     attribution = "&copy; Department of Primary Industries 2021") %>%
            
            #Add aquaculture leases from DPI
            addEsriFeatureLayer(url="https://spatial.industry.nsw.gov.au/arcgis/rest/services/Fisheries_Portal/Aquaculture1/MapServer/1", 
                                useServiceSymbology = FALSE, options = pathOptions(pane="othermap", maxZoom=17), color = "black",  weight = "1.5", group = "Aquaculture leases")%>%
            
            # set boundaries for map
            # fitBounds(lng1 = min(raster_points()$Lon), 
            #           lat1 = min(raster_points()$Lat), 
            #           lng2 = max(raster_points()$Lon), 
            #           lat2 = max(raster_points()$Lat)) %>%
            # 
            
            
            #add empty file for map extent
            addRasterImage(empty_raster()) %>% 
            
            
            #addMiniMap() %>%
            addScaleBar(position = "bottomleft", scaleBarOptions(
                maxWidth = 100,
                metric = TRUE,
                imperial = FALSE,
                updateWhenIdle = TRUE)) %>%
            
            addLayersControl(
                baseGroups = c("Open Street Map", "Photo basemap"),
                overlayGroups = c("Latest mapped habitat", "Historic mapping (selected estuary)", "2019 - 2020 Fire damage", "Saltmarsh - Vehicle damage", "Foreshore", "Artificial structures", "Aquaculture leases", "Estuarine reef"),
                options = layersControlOptions(collapsed = FALSE)) %>%
            
            addMeasure(position = "topleft",
                       primaryLengthUnit = "meters",
                       secondaryLengthUnit = "kilometers",
                       primaryAreaUnit = "sqmeters",
                       secondaryAreaUnit = "hectares",
                       activeColor = "#3D535D",
                       completedColor = "#7D4479") %>%
            hideGroup("Foreshore") %>%
            hideGroup("2019 - 2020 Fire damage") %>%
            hideGroup("Artificial structures") %>%
            hideGroup("Saltmarsh - Vehicle damage") %>%
            hideGroup("Aquaculture leases") %>%
            hideGroup("Estuarine reef")
            #hideGroup("Mooring Areas") %>%
            #hideGroup("Boat moorings")
        
        
    })
    
    
    # watch the year selector and load macrophytes for the selected year without resetting the map
    observe({
        input$date_t1
        input$estuary1
        leafletProxy("map") %>%
            removeTiles("macmap") %>%
            addTiles(urlTemplate = stringr::str_interp("https://gregjwest.github.io/NSW_EstMacrophytes/Estuaries/${estuary_report_dat()$Est_Name}/${input$date_t1}/{z}/{x}/{y}.png"), group = "Historic mapping (selected estuary)", layerId = "macmap",
                     options = pathOptions(pane="macromap", maxZoom=17),
                     attribution = "&copy; Department of Primary Industries 2020") %>%
            hideGroup("Latest mapped habitat")
        
    })
    
##### CHANGE OVER TIME TAB #####
    
    # UI preparation
    
    # Update Year selector to only provide the years that have been mapped
    # Update Year selector to only provide the estuaries per region
    observe({
        x3_1 <- presence_dat %>% dplyr::filter(Region == input$region3) %>% dplyr::select(Estuary)
        updateSelectInput(session, "estuary3", label = "Estuary:", choices = unique(x3_1), selected = index_estuary)
    })
    
    #select habitats based on selected years and estuary, only habitats present for the selected years will be listed
    observe({
        #x3_2 <- presence_dat %>% dplyr::filter(Map_Year == input$change_t1 | Map_Year== input$change_t2) %>% dplyr::filter(Estuary == input$estuary3) %>% dplyr::select(Macrophyte)
        x3_2 <- presence_dat %>% dplyr::filter(Map_Year == input$change_t1 | Map_Year== input$change_t2)
        x3_3 <- x3_2$Macrophyte[x3_2$Estuary == input$estuary3]
        updateSelectInput(session, "habitat3", label = "Habitat for change grid:", choices = unique(x3_3))
    })
    
    observe({
        #x2 <- presence_dat %>% dplyr::filter(Estuary == input$estuary3) %>% dplyr::select(Map_Year) %>% dplyr::arrange(desc(Map_Year))
        x2 <- presence_dat$Map_Year[presence_dat$Estuary == input$estuary3 & presence_dat$Map_Year >= input$change_t1]
        updateSelectInput(session, "change_t2", "Mapped habitat - Time 2 (Right)", choices = sort(unique(x2),decreasing = TRUE))
    })
    observe({
        #x3 <- presence_dat %>% dplyr::filter(Estuary == input$estuary3) %>% dplyr::select(Map_Year)%>% dplyr::arrange(Map_Year) %>% dplyr::filter(Map_Year <= input$change_t2)
        x3 <- presence_dat$Map_Year[presence_dat$Estuary == input$estuary3] 
        updateSelectInput(session, "change_t1", "Mapped habitat - Time 1 (Left)", choices = unique(x3))
    })

    # DATA PREPERATION
    
    estuary_report_dat1 <- reactive({ report_dat %>%
            dplyr::filter(Estuary == input$estuary3) 
    })
    
    filtered_geojson <- reactive({ geojson %>%
        dplyr::filter(Estuary == input$estuary3) 
    })
    
    t2_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary3) %>%
            dplyr::filter(Map_Year == input$change_t2) %>%
            dplyr::select(GRID_ID, input$habitat3)
     
    })
    
    t1_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary3) %>%
            dplyr::filter(Map_Year == input$change_t1) %>%
            dplyr::select(GRID_ID, input$habitat3)
    })
    

    diff_dat <- reactive({
        inner_join(t2_dat(),
                   t1_dat(),
                   by = c("GRID_ID")) %>%
            mutate(Diff = .[[2]] - .[[3]])
    })
    
    #Find true zeros, time one plus time 2 will equal zero when both are zero
    Chnge_dat <- reactive({
        diff_dat() %>%
            dplyr::mutate(Chnge = .[[2]] + .[[3]])
    })    

    #Filter out the true zeros, select only the values greater than 0
    diff_dat1 <- reactive({
        Chnge_dat() %>%
            dplyr::filter(Chnge > 0)
    })

    #merge estuary change data with spatial data
    merged_data <- reactive({
      merged <- merge(filtered_geojson(), diff_dat1(), by.x = "GRID_ID", by.y = "GRID_ID")
      return(merged)
    })
    
    
    ### Create an asymmetric color range
    # scale_range <- c(-62500, 62500)
    bins <- c(65000, 32000, 3200, 320, 32, .1, -.1, -32, -320, -3200, -32000, -65000)
    grdpal <- colorBin ("RdYlBu", bins = bins, na.color = "transparent") 
    
    output$map_change <- renderLeaflet({
      leaflet(options = leafletOptions(maxZoom = 17)) %>% 
        addMapPane("basemaps", zIndex=100) %>% #add map pane for baselayers
        addMapPane("Grid", zIndex=400) %>% #add map pane for macrophyte grid
        #Add side by side comparison slider   
        addMapPane("left", zIndex = 600) %>%
        addMapPane("right", zIndex = 600) %>%        
        
        #leaflet::addTiles() %>%
        #Open Street Map base layer
        addProviderTiles(providers$CartoDB.Positron, group = "Open Street Map",
                         options = pathOptions(pane="basemaps", maxZoom=17)) %>%
        #Spatial Sevices Photo base layer
        addEsriTiledMapLayer("http://maps.six.nsw.gov.au/arcgis/rest/services/public/NSW_Imagery/MapServer", group = "Photo basemap",
                             layerId = "0", pathOptions(pane="basemaps", maxZoom=17)) %>%
        
        #add change grid polygons, and colour them based on Diff and colour palette
        addPolygons(data = merged_data(), fillColor = ~grdpal(Diff),  fillOpacity = 0.5, weight = .1, color = "black", options = pathOptions(Pane ="Grid", group = "Change Grid")) %>%
        
        #add maps to side by side comparison  
        addTiles(urlTemplate = stringr::str_interp("https://gregjwest.github.io/NSW_EstMacrophytes/Estuaries/${estuary_report_dat1()$Est_Name}/${input$change_t1}/{z}/{x}/{y}.png"), group = "Mapped habitat", layerId = "Time1",
                 options = pathOptions(pane = "left", maxZoom=17),
                 attribution = "&copy; Department of Primary Industries 2020") %>%
        addTiles(urlTemplate = stringr::str_interp("https://gregjwest.github.io/NSW_EstMacrophytes/Estuaries/${estuary_report_dat1()$Est_Name}/${input$change_t2}/{z}/{x}/{y}.png"), group = "Mapped habitat", layerId = "Time2",
                 options = pathOptions(pane = "right", maxZoom=17),
                 attribution = "&copy; Department of Primary Industries 2020") %>%
        
        addLayersControl(
          baseGroups = c("Open Street Map", "Photo basemap"),
          overlayGroups = c("Change Grid", "Mapped habitat"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        
        #Add side by Side control
        addSidebyside(layerId = "sidecontrols",
                      rightId = "Time2",
                      leftId = "Time1") %>%
        
        addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
        addMeasure(position = "bottomleft",
                   primaryLengthUnit = "meters",
                   secondaryLengthUnit = "kilometers",
                   primaryAreaUnit = "sqmeters",
                   secondaryAreaUnit = "hectares",
                   activeColor = "#3D535D",
                   completedColor = "#7D4479") %>%
        
        #change map turned on by default
        hideGroup("Mapped habitat")
      #hideGroup("Change grid")
      
      
      
    })
    
   
    
    ###### DATA SUMMARY TAB #####
    # Update Year selector to only provide the estuaries per region
    observe({
        x2_1 <- dat %>% dplyr::filter(Region == input$region2) %>% dplyr::select(Estuary)
        updateSelectInput(session, 
                          "estuary2", 
                          label = "Estuary:",
                          choices = unique(x2_1))
    })
    
    # Estuary Summary Table
    estuary_report_dat <- reactive({ report_dat %>%
            dplyr::filter(Estuary == input$estuary2) 
    })

    output$estuary_type <- renderText({ 
        paste("<b>Estuary type:</b>", unique(estuary_report_dat()$RoyEstuarytype))
    })
    
    output$catchment_area <- renderText({ 
        paste("<b>Catchment area:</b>", round(unique(estuary_report_dat()$Catchmentarea), digits = 0),"km<sup>2</sup>")
    })
    
    output$estuary_area <- renderText({ 
        paste("<b>Estuary area:</b>", round(unique(estuary_report_dat()$Estuaryarea), digits = 0),"km<sup>2</sup>")
    })
    
    output$ave_depth <- renderText({ 
        paste("<b>Average depth:</b>", round(unique(estuary_report_dat()$AveDepth), digits = 1),"m")
    })
    
    
    years_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary2)
    })
    
    
    output$years_mapped <- renderText({ 
        paste(unique(years_dat()$Map_Year))
    })
    

    # Plot estuary by habitat over time    
    plot_dat <- reactive({
        dat1 %>%
            dplyr::filter(Estuary == input$estuary2) %>%
            dplyr::group_by(Habitat,Map_Year) %>%
            dplyr::summarise(Area = sum(Area)) %>%
            dplyr::filter(!Habitat %in% c("Seagrass", "Mangrove", "Saltmarsh", "Man_Sm", "Man_Biplot", "SM_Biplot")) %>%
            dplyr::filter(Area >= 0)%>%
            dplyr::mutate(ToHighlight = ifelse(Map_Year <= 1990, "yes", "no"))
        
    })
    
    
    output$habitat_plot <- renderPlot({
        ggplot(data=plot_dat(), aes(x=factor(Map_Year), y=Area/10000, fill=ToHighlight)) + 
            geom_bar(stat="identity", color="black", linewidth = 0.2, width = 0.7)+ 
            scale_fill_manual(values = c("yes"="#EEEEEE", "no"="#B1B1B1"), guide = "none" )+ 
            labs(y="Hectares", x="Years Mapped") +
            scale_y_continuous(expand = expansion(mult = c(0, .1)))+
            facet_wrap(~Habitat, scales = "free_y") +
            theme_bw() +
            theme(axis.title.x = element_text(face="bold", size=14), 
                  axis.text.x  = element_text(vjust=0.5, size=12),
                  axis.title.y = element_text(face="bold", size=14),
                  axis.text.y  = element_text(size=14),
                  #panel.spacing.x = unit(6, "lines"),
                  strip.text.x = element_text(size = 14, face = "italic"))
    },height = 550, width = 1000 )
    
    
    
    # Change in mangrove v saltmarsh (MVS)
    
    # Update Year selector to only provide the years that have been mapped

    observe({
     #x2 <- presence_dat %>% dplyr::filter(Estuary == input$estuary3) %>% dplyr::select(Map_Year) %>% dplyr::arrange(desc(Map_Year))
           x5 <- dat$Map_Year[dat$Estuary == input$estuary2 & dat$Map_Year >= input$mvs_t1]
           updateSelectInput(session, "mvs_t2", "Time 2: ", choices = sort(unique(x5),decreasing = TRUE))
    })
    observe({
           #x3 <- presence_dat %>% dplyr::filter(Estuary == input$estuary3) %>% dplyr::select(Map_Year)%>% dplyr::arrange(Map_Year) %>% dplyr::filter(Map_Year <= input$change_t2)
           x4 <- dat$Map_Year[dat$Estuary == input$estuary2] 
           updateSelectInput(session, "mvs_t1", "Time 1: ", choices = unique(x4))
    })   
       
       
        
    #     # DATA PREPERATION
    mvs_t2_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary2) %>%
            dplyr::filter(Map_Year == input$mvs_t2) %>%
            dplyr::select(GRID_ID, Man_Biplot, SM_Biplot)
        
    })
    
    
    
    mvs_t1_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary2) %>%
            dplyr::filter(Map_Year == input$mvs_t1) %>%
            dplyr::select(GRID_ID, Man_Biplot, SM_Biplot)
    })
    
    
    
    #calculate mangrove and saltmarsh values from grid IDs
    mvs_diff_dat <- reactive({
        inner_join(mvs_t2_dat(),
                   mvs_t1_dat(),
                   by = c("GRID_ID")) %>%
            mutate(delta_mangrove = .[[2]] - .[[4]]) %>%
            mutate(delta_saltmarsh = .[[3]] - .[[5]]) %>%

        #code to filter out grids with 0 change - possibly excludes grids that have habitat but doesn't change through time
            filter(delta_saltmarsh != 0) %>%
            filter(delta_mangrove != 0)            
        
        #optional code to filter grids which don't have both mangrove or saltmarsh present at either time
            #mutate(pres_mangrove = .[[2]] + .[[4]]) %>%
            #mutate(pres_saltmarsh = .[[3]] + .[[5]]) %>%
            #filter(pres_saltmarsh != 0) %>%
            #filter(pres_mangrove != 0)    
            


    })
    
    #Filter out mangroves, mangrove_saltmarsh and saltmarsh
    mansm_dat <- reactive({
        dat1 %>%
            dplyr::filter(Estuary == input$estuary2) %>%
            dplyr::group_by(Habitat,Map_Year) %>%
            dplyr::summarise(Area = sum(Area)) %>%
            dplyr::filter(Habitat %in% c("Mangrove", "Saltmarsh", "Man_Sm")) %>%
            dplyr::filter(Area >= 0)%>%
            dplyr::mutate(ToHighlight = ifelse(Map_Year <= 1990, "yes", "no"))
        
    })
    
    
    
    # Mangrove, Mixed and Saltmarsh plots
    output$mansm_plot <- renderPlot({
        ggplot(data=mansm_dat(), aes(x=factor(Map_Year), y=Area/10000, fill=ToHighlight)) + 
            geom_bar(stat="identity", color="black", linewidth = 0.2, width = 0.7)+ 
            scale_fill_manual(values = c("yes"="#EEEEEE", "no"="#B1B1B1"), guide = "none" )+
            #geom_text(aes(label=round(Area/10000, digits = 2)), position=position_dodge(width=0.9), vjust=-0.25)+
            labs(y="Hectares", x="Years Mapped") +
            scale_y_continuous(expand = expansion(mult = c(0, .1)))+
            facet_wrap(~Habitat, labeller = mansm.names) +
            theme_bw() +
            theme(axis.title.x = element_text(face="bold", size=14), 
                  axis.text.x  = element_text(vjust=0.5, size=12),
                  axis.title.y = element_text(face="bold", size=14),
                  axis.text.y  = element_text(size=14),
                  strip.text.x = element_text(size = 14))
    },height = 350, width = 1000 )
    
    
    
    #   below Biplot commented out for later release 
    # #Mangrove vs Saltmarsh Biplot
    # output$MVS <- renderPlot({
    #     ggplot(mvs_diff_dat()) +
    #         theme_geometry(mvs_diff_dat()$delta_mangrove, mvs_diff_dat()$delta_saltmarsh) +
    #         theme_light() +
    #         theme(axis.title.x=element_blank(),
    #               axis.text.x=element_blank(),
    #               axis.ticks.x=element_blank(),
    #               axis.title.y=element_blank(),
    #               axis.text.y=element_blank(),
    #               axis.ticks.y=element_blank()) +
    #         geom_point(aes(x = delta_mangrove, y = delta_saltmarsh), size = 3, shape = 21, fill="lightgray", color = "black", alpha = .5) +
    #         annotation_custom(mvsbgimg, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
    # 
    #     
    # },height = 600, width = 600 )
    
    
 
    #### PREDICTIVE MODELLING TAB ####
    # DATA PREPERATION
    
    observe({
      x4_1 <- dat %>% dplyr::filter(Region == input$region4) %>% dplyr::select(Estuary)
      updateSelectInput(session, 
                        "estuary4", 
                        label = "Estuary:",
                        choices = unique(x4_1))
    })
    
    
    raster_slr <- reactive({
      dat %>%
        dplyr::filter(Estuary == input$estuary4) %>%
        dplyr::select(Easting, Northing)
    })    
    
    # Set raster max and min based on estuary selection
    lon_min3 <- reactive({min(raster_slr()$Easting)-10}) 
    lon_max3 <- reactive({max(raster_slr()$Easting)+10})
    lat_min3 <- reactive({min(raster_slr()$Northing)-10})
    lat_max3 <- reactive({max(raster_slr()$Northing)+10})    
    ncols <- reactive({((lon_max3() - lon_min3())/cell_size)}) 
    nrows <- reactive({((lat_max3() - lat_min3())/cell_size)})  
    
    # Create and empty raster that covers the estuary selected
    empty_slr <- reactive({
      raster(nrows=nrows(), ncols=ncols(), 
             xmn=lon_min3(), xmx=lon_max3(), 
             ymn=lat_min3(), ymx=lat_max3(), 
             res=c(cell_size,cell_size), crs=myproj)
    })
    
    
    # OUTPUTS  
    
    
    output$slrmap <- renderLeaflet({
      
      
      leaflet(options = leafletOptions(maxZoom = 17)) %>% 
        addMapPane("basemaps4", zIndex=200) %>% #add map pane for baselayers
        addMapPane("macromap4", zIndex=400) %>% #add map pane for macrophyte map
        
        addProviderTiles(providers$CartoDB.Positron, group = "Open Street Map",
                         options = pathOptions(pane="basemaps4", maxZoom=16)) %>%
        
        addWMSTiles("https://mapprod1.environment.nsw.gov.au/arcgis/services/Landuse/LandUse2017/MapServer/WMSServer", group = "Land use 2017", 
                    options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity = 0.5, pane="basemaps4"),
                    attribution = "&copy; State Government of NSW and Department of Planning, Industry and Environment 2020", layers = "0") %>%
        
        
        addEsriTiledMapLayer("http://maps.six.nsw.gov.au/arcgis/rest/services/public/NSW_Imagery/MapServer", group = "Photo basemap",
                             layerId = "0", pathOptions(pane="basemaps4", maxZoom=16)) %>%
        
        
        #addTiles(urlTemplate = "https://gregjwest.github.io/latest_mapping/{z}/{x}/{y}.png", group = "Latest mapped habitat",
        #         options = pathOptions(pane="macromap4", maxZoom=15),
        #         attribution = "&copy; Department of Primary Industries 2020") %>%
        
        
        
        
        
        
      addTiles(urlTemplate = "https://gregjwest.github.io/Sea_level_modelling/S0/{z}/{x}/{y}.png", group = "Sea level - Present day",
               options = pathOptions(pane="macromap4", maxZoom=16),
               attribution = "&copy; Department of Primary Industries 2020") %>%
        
        addTiles(urlTemplate = "https://gregjwest.github.io/Sea_level_modelling/S1/{z}/{x}/{y}.png", group = "Sea level - 0.5m increase",
                 options = pathOptions(pane="macromap4", maxZoom=16),
                 attribution = "&copy; Department of Primary Industries 2020") %>%
        #addWMSLegend("https://mapprod1.environment.nsw.gov.au/arcgis/services/Landuse/LandUse2017/MapServer/WmsServer?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=0") %>%
        # set boundaries for map
        # fitBounds(lng1 = min(raster_points()$Lon), 
        #           lat1 = min(raster_points()$Lat), 
        #           lng2 = max(raster_points()$Lon), 
        #           lat2 = max(raster_points()$Lat)) %>%
        # 
        
        
        #add empty file for map extent
        addRasterImage(empty_slr()) %>% 
        
        
        #addMiniMap() %>%
        addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
        
        addLayersControl(
          baseGroups = c("Sea level - Present day", "Sea level - 0.5m increase"),
          overlayGroups = c("Photo basemap", "Land use 2017"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        addMeasure(position = "bottomleft",
                   primaryLengthUnit = "meters",
                   secondaryLengthUnit = "kilometers",
                   primaryAreaUnit = "sqmeters",
                   secondaryAreaUnit = "hectares",
                   activeColor = "#3D535D",
                   completedColor = "#7D4479") %>%
        hideGroup("Land use 2017")%>%
        hideGroup("Photo basemap")
      
    })
    
    
    
}


shinyApp(ui, server)




