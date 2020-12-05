### Williams, West, Glasby, June 2020
## A Shiny map to display and explore the estuarine macorphyte mapping data

# Last update 18 August 2020 1312

# v0.01

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
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(lazyeval)) install.packages("lazyeval", repos = "http://cran.us.r-project.org")
if(!require(htmltools)) install.packages("htmltools", repos = "http://cran.us.r-project.org")
if(!require(leaflet.esri)) install.packages("leaflet.esri", repos = "http://cran.us.r-project.org")
if(!require(stars)) install.packages("leaflet.esri", repos = "http://cran.us.r-project.org")
if(!require(leafem)) install.packages("leaflet.esri", repos = "http://cran.us.r-project.org")
if(!require(leaflet.extras2)) install.packages("leaflet.extras2", repos = "http://cran.us.r-project.org")
if(!require(Cairo)) install.packages("Cairo", repos = "http://cran.us.r-project.org")
if(!require(png)) install.packages("png", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")



# Read in master data .csv
# THe data is supplied by Greg West and should be in the exact same layout (column names, species names and all are case sensitive. Ensure there are no unknown spaces)
dat <-  read.csv("Multi_Estuary_All_Years_Grid.csv", stringsAsFactors = T)

report_dat <-  read.csv("Estuary_Overview.csv", stringsAsFactors = T)

presence_dat <- read.csv("NSW_Estuarine_Macrophytes_Presence.csv", stringsAsFactors = T)

img <- readPNG("www/Man_SM_Background.png")

g <- rasterGrob(img, interpolate=TRUE)

dat$fMap_Year <- factor(dat$Map_Year)

# Transpose data to long format to enable filtering by species.
dat1 <- dat %>%
    gather("Habitat", "Area", 7:13) %>%
    dplyr::filter(Habitat != "Mangrove_Saltmarsh")
dat1$Habitat <- factor(dat1$Habitat, levels = c("Mangrove", "Saltmarsh","Posidonia", "Zostera","Halophila","Ruppia"))


# Create a data frame of the total area of each species by estuary and year. To be graphed and tabled in the data summary tab.
dat_summary <- dat %>%
    group_by(Estuary,Map_Year) %>%
    summarise(Mangrove = sum(Mangrove/10000),
              Saltmarsh = sum(Saltmarsh/10000)) %>%
    #mutate(Seagrass = round(Seagrass, 0)) %>%
    mutate(Mangrove = round(Mangrove, 0)) %>%
    mutate(Saltmarsh = round(Saltmarsh, 0)) %>%
    ungroup()

#List of habitats that are available for the user to select
# Can and added and removed by the host ensuring that the spelling and case is the exact same as the raw data file.
 #------list_habs <- c( "Mangrove", "Saltmarsh","Seagrass_all_species","Posidonia", "Zostera","Halophila","Ruppia") 
# Index/select the default habitat based on order in the above list i.e. 3 = Seagrass
 #-----index_habs <- 3



# Index/select the default region based on order in the above list i.e. 3 = Port Stephens (at the test state)
# List the estuaries available
list_region <- c(levels(presence_dat$Region))
index_region <- 1

# Index/select the default estuary based on order in the above list i.e. 3 = Port Stephens (at the test state)
# List the estuaries available
list_estuary <- c(levels(presence_dat$Estuary))
index_estuary <- 2

list_habs <- c(levels(presence_dat$Macrophyte))
index_habs <- 0
# Cell size for building rasters at 200x200m
cell_size <- 250

myproj <- "+proj=utm +zone=56 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 + towgs8"

### Theme for mangrove v slatmarsh plot layout ----
theme_geometry <- function(xvals, yvals, xgeo = 0, ygeo = 0, 
                           color = "grey", size = 0.5, 
                           xlab = "Saltmarsh", 
                           ylab = "Mangrove",
                           ticks = 10,
                           textsize = 3,
                           xlimit = max(62500,62500),
                           ylimit = max(62500,62500),
                           #xlimit = max(abs(xvals),abs(yvals)),
                           #ylimit = max(abs(yvals),abs(xvals)),
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
            geom_line(aes(x = x_ax, y = y_ax), color = color, size = size, data = xaxis),
            geom_line(aes(x = x_ax, y = y_ax), color = color, size = size, data = yaxis),
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
                                                 data = xtick, size = size, 
                                                 color = color)
        
        #Add labels to the x-ticks
        # theme.list[[nlist + 4*k-2]] <- annotate("text",
        #                                         x = ticks_x[k],
        #                                         y = ygeo - 2.5*epsilon,
        #                                         size = textsize,
        #                                         label = paste(ticks_x[k]))
        
        
        #Add ticks to geom line for y axis
        theme.list[[nlist + 4*k-1]] <- geom_line(aes(x = xt, y = yt), 
                                                 data = ytick, size = size, 
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
#####






#
#
#
#
#
#
#
#
#
#
# User Interface ----
# The User Interface code controls how the App is displayed (colours, layout, menus etc)
# The code is broken down tab by tab

# Overall layout of App.
ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                 title = div(img(src= "DPI_LOGO_left.png","Estuarine Habitat Mapping",
                                 style="margin-top: -15px;margin-left: -30;padding-right:10px", 
                                 height = 50)),
                 windowTitle = "NSW Estuarine Mapping",
                 position = c("fixed-top"),

                 

                 
#### Interactive Explorer Tab UI                                  -----
                 tabPanel(
                     "Interactive Explorer",
                     div(class="outer",
                         tags$head(includeCSS("styles.css")),
                         leafletOutput("map", width = "100%", height = "100%"),
                       
                         absolutePanel(id = "controls", class = "panel panel-default",
                                       top = 160, left = 11, width = 250, height = "auto",
                                       fixed = TRUE, draggable = TRUE,
                                       HTML('<button data-toggle="collapse" data-target="#demo">Show/Hide</button>'),
                                       tags$div(id = 'demo',  class="collapse in",
                                       h3("Welcome"),
                                       h4("Demonstration Version"),
                                       
                                       h6("This interactive map allows the comparison of macrophytes (seagrasses, mangroves, saltmarshes) between two times in any 
                                          NSW estuary that is known to contain macrophytes.  These habitats have been mapped by DPI Fisheries since the 1980s 
                                          and the various estuaries have been mapped different numbers of times and often in different years."),
                                       tags$br(),
                                       h6("This Interactive Explorer displays the latest mapped habitats."),
                                       tags$br(),
                                       h6("Please select a region and estuary.  The map can be panned or zoomed."),
                                       selectInput("region1", 
                                                   label = "Region:", 
                                                   choices = list_region, 
                                                   selected = list_region[index_region]),
                                       selectInput("estuary1", 
                                                   label = "Estuary:", 
                                                   choices = NULL, 
                                                   selected = "Port Stephens"),
                                       selectInput("date_t1", 
                                                   label = "Year Mapped:", 
                                                   choices = NULL, 
                                                   selected = "2017"),
                                       
                                       # selectInput("habitat1",
                                       #             label = "Habitat:",
                                       #             choices = list_habs,
                                       #             selected = list_habs[index_habs]),
                                       # selectInput("year1",
                                       #             label = "Year:",
                                       #             choices = NULL,
                                       #             selected = "2017"),
                                       tags$h6("The latest macrophyte maps for each estuary can be downloaded from the NSW DPI Fisheries Data Portal.", 
                                              tags$a(href="https://webmap.industry.nsw.gov.au/Html5Viewer/index.html?viewer=Fisheries_Data_Portal", target="_blank", 
                                                      tags$img(src='DPI_LOGO_left.svg')) 
                                               )
                                       
                                       
                                       )
                                       
                         ),
                         #absolutePanel(id = "logo", class = "card", bottom = 0, right = 290,  fixed=TRUE, draggable = FALSE, width= "auto", height = "auto",
                         #              tags$a(href='https://webmap.industry.nsw.gov.au/Html5Viewer/index.html?viewer=Fisheries_Data_Portal', target="_blank", 
                         #                     tags$img(src='Spatial Data Portal small.png',height='35',width='100'))),
                         absolutePanel(id = "legend1", class = "card", bottom = 170, right = 10,  fixed=TRUE, draggable = FALSE, width= "auto", height = "auto",
                                                                                tags$img(src='Macrophyte_Legend.svg',height='300',width='150'))
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
            draggable = TRUE,
            HTML('<button data-toggle="collapse" data-target="#demo1">Show/Hide</button>'),
            tags$div(id = 'demo1',  class="collapse in",
            h3("Where is change occuring?"
            ),
            h6("Change is represent by the area increase or decrease of habitat per 250x250m grid."
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
                        label= "Habitat: ",
                        choices = list_habs, 
                        selected = list_habs[index_habs]
            ),
            
        
            tags$h6("The latest macrophyte maps for each estuary can be downloaded from the NSW DPI Fisheries Data Portal.", 
                    tags$a(href="https://webmap.industry.nsw.gov.au/Html5Viewer/index.html?viewer=Fisheries_Data_Portal", target="_blank", 
                           tags$img(src='DPI_LOGO_left.svg')), 
            
                    absolutePanel(id = "legend1", class = "card", bottom = 300, right = 0,  fixed=TRUE, draggable = FALSE, width= "auto", height = "auto",
                                  tags$img(src='Macrophyte_Legend.svg',height='300',width='200'))
            ),
                    absolutePanel(id = "legend1", class = "card", bottom = 20, right = 15,  fixed=TRUE, draggable = FALSE, width= "auto", height = "auto",
                                tags$img(src='Grid_Legend_Curved.svg',height='275',width='170'))
          
            )
        )
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
                   tags$p("Years mapped: "), textOutput("years_mapped")
                   ,
                   
                   tags$br(),
                   
                   tags$p("Macrophyte maps for the latest time of sampling can be donwloaded from the", 
                          tags$a(href="https://webmap.industry.nsw.gov.au/Html5Viewer/index.html?viewer=Fisheries_Data_Portal", target="_blank", "NSW Fisheries Spatial Data Portal.") ) 
                   
                   
            )),
        
        fluidRow(
            tags$br(),tags$br(),tags$br(),tags$br(),
            
            h4("Change in area over time for each species"),
            
            tags$p("Area (hectares) of mapped habitat in the selected estuary are graphed for each year that mapping has been done."),
            
            column(6, offset = 0,
                   style = "height:600px; ", 
                   plotOutput("habitat_plot", width = "100%"))
        ),
        fluidRow(
            tags$br(),tags$br(),
            h4(" Relative change in Mangrove and Saltmarsh"),
            tags$p("To examine changes in the ratio of mangrove to saltmarsh over time, first select the years to compare.  The predominance of plotted points in different quadrants of the graph indicate the type of change in these two habitats, e.g.  whether mangroves have increased at the expense of saltmarshes, or some other pattern. Each point represents the change in ratio of mangrove area to saltmarsh area in a 250 x 250 m map grid. "),
            
        ),
        fluidRow(column(4,
                        selectInput("mvs_t1",
                        label = "Year (Time 1): ",
                        choices = NULL,
                        selected = "1985"
            
                        )
            ),
            column(4,
                    selectInput("mvs_t2", 
                    label = "Latest Year (Time 2): ", 
                    choices = NULL, 
                    selected = "2017"
            )
            ),

            tags$br(),
            tags$br(),
            
            column(6, offset = 2,
                   style = "height:50px; "
            
            ),
            column(6, offset = 2,
                   style = "height:700px; ",
                   plotOutput("MVS", width = "100%"))

    )
    )
    ),
    
        




##### About Tab ----
tabPanel(
    "About",
    tags$div(
        tags$h3("Overview"),
        tags$p("This website and the most recent maps were developed as part of the NSW Marine Estate Management Strategy (Initiative 2.5 'Delivering healthy coastal habitats with sustainable use and development'), which was funded by the NSW Government."),
        tags$h3("Mapping Details"),
        tags$p("Methods used for mapping are described in Creese et al. (2009) and West & Glasby (2020).  Mangrove and saltmarsh species have not been distinguished in these maps.  Saltmarshes include the low-marsh species described in Sainty (2012) and exclude higher-marsh species such as", 
               tags$span(tags$em("Phragmites australis")),
               "and ", 
               tags$span(tags$em("Baumea juncea", .noWS = "outside"),.noWS = "outside"),
               ". Seagrasses have been classified to the level of genus and meadows of seagrass have mapped according to the dominant genus in that meadow (e.g. ", 
               tags$span(tags$em("Posidonia", .noWS = "outside"),.noWS = "outside"),"-dominated, ", 
               tags$span(tags$em("Zostera", .noWS = "outside"),.noWS = "outside"),
               "-dominated).   Areas where mixed genus occur are mapped and classified as such, e.g. the class", 
               tags$span(tags$em("'Posidonia/Zostera /Halophila'", .noWS = "outside"),.noWS = "outside"),
               " contains all three genera.  For mixed habitats with two genera, the area per genus has been calculated as 50% of the total mapped area. For habitats with three genera, areas per genus one third of the total mixed habitat area."),
        
        tags$h3("Acknowledgements"),
        tags$p("The data used here is derived from many decades of work by DPI Fisheries staff.  The earliest macrophyte mapping project was led by Ron West and maps were created by Trudy Walford and Carol Thorogood (see West et al. 1985)."),
        tags$p("Subsequent mapping projects were led by Rob Williams, Bob Creese, Greg West and Tim Glasby.  We thank the many staff involved in producing these maps, including, but not limited to: Peter Gibson, Roger Laird, Isabelle Thiebaud, Brooke McCartin, Chris Gallen, Danielle Morrison, Joe Neilson and Trudy Walford."),
        tags$p("Funding for the various mapping projects has come from numerous sources, primarily via the NSW State government through a variety of funding programs and from short-term contracts funded by organisations including: Lake Macquarie City Council, Eurobodalla Shire Council, North Rivers Catchment Management Authority, Sydney Metro Catchment Management Authority and NSW Roads and Maritime"),
        tags$br(),
        tags$p(strong("Concept/design:"), "Tim Glasby & Greg West"),
        tags$p(strong("Shiny App development:"), "Joel Williams"),
        tags$p(strong("Data compilation:"), "Greg West"),
        tags$p(strong("Last mapping update:"), "2019"),
        tags$p(strong("Last Shiny App update:"), "18 August 2020"),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$h3("References"),
        tags$a(href="https://www.dpi.nsw.gov.au/__data/assets/pdf_file/0004/306625/AE_2009_Output-1575_Creese-et-al_Habitat-Mapping-Final-Report-113_REPORT.pdf", tags$p("Creese, R.G., Glasby , T.M., West, G., Gallen, C. (2009). Mapping the habitats of NSW estuaries. Industry & Investment NSW, Nelson Bay, Australia, 95pp."), target="_blank"),
        tags$p("West, G., Glasby, T.M., (in press). Interpreting long-term patterns of seagrasses abundance: how seagrass variability is dependent on genus and estuary type. Estuarine Coastal and Shelf Science."),
        tags$a(href="West (1985).pdf", tags$p("West, R.J., Thorogood, C.A., Walford, T.R. and Williams R.J. (1985). An estuarine inventory for New South Wales, Australia. Fisheries Bulletin 2. Department of Agriculture, New South Wales. 140 pp."), target="_blank"),
        tags$br(),
        tags$br("The Shiny App is produced using R v4.0.0, RStudio v1.3.959 and Leaflet v2.0.3,"),
        tags$br("v0.01")
        

    )
)

)
                             




#                             
                             
                    

# Server logic ----
#
#
#
# THe Server codes runs in the background making all the calculation from user choice etc.

server <- function(input, output, session) {

    
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

    # observeEvent(
    #     input$habitat1,
    #     updateSelectizeInput(session, "habitat3", selected = input$habitat1)
    # )
    # 
    # observeEvent(
    #     input$habitat3,
    #     updateSelectizeInput(session, "habitat1", selected = input$habitat3)
   # )
#
    #
    #
    
    
    
######## HABITAT EXPLORER TAB ########
    # DATA PREPERATION
    # Update Year selector to only provide the estuaries per region
    observe({
        x0 <- dat %>% dplyr::filter(Region == input$region1) %>% dplyr::select(Estuary)
        updateSelectInput(session, 
                          "estuary1", 
                          label = "Estuary:",
                          choices = unique(x0),
                          selected = "Port Stephens")
    })
    # Update Year selector to only provide the years that have been mapped
    observe({
        #x1 <- dat %>% dplyr::filter(Estuary == input$estuary1) %>% dplyr::select(Map_Year)
        x1 <- dat$Map_Year[dat$Estuary ==input$estuary1]
        updateSelectInput(session, 
                          "date_t1", 
                          label = "Year:",
                          choices = unique(x1))
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
    lon_min <- reactive({min(raster_points()$Easting)}) 
    lon_max <- reactive({max(raster_points()$Easting)})
    lat_min <- reactive({min(raster_points()$Northing)})
    lat_max <- reactive({max(raster_points()$Northing)})    
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
    
    # Summary table
    output$sum_stats <- renderPrint({
        summary(raster_values())
    })  

    
    output$map <- renderLeaflet({
        
        
        leaflet() %>% 
            addProviderTiles(providers$CartoDB.Positron, group = "Open Street Map",
                             options = tileOptions(maxZoom=18)) %>%
            addEsriTiledMapLayer("http://maps.six.nsw.gov.au/arcgis/rest/services/public/NSW_Imagery/MapServer", group = "Photo basemap",
                        layerId = "0",
                        options (maxZoom = 18)) %>%
                        #,WMSTileOptions(format = "image/png", 
                        #                         
                        #                        transparent = TRUE, 
                        #                        continuousWorld = TRUE, 
                        #                        tiled = TRUE,
                        #                        tms = TRUE,
                        #                        info_format = 'text/html'),
                        #attribution = "&copy; Department of Customer Service 2020") %>% 

           
            
            addTiles(urlTemplate = "Pittwater/Jetty/{z}/{x}/{y}.png", group = "Disturbance",
                       options = tileOptions(maxZoom=18, zIndex = 1000),
                       attribution = "&copy; Department of Primary Industries 2020") %>% 
            #addTiles(urlTemplate = "Pittwater/MArea/{z}/{x}/{y}.png", group = "Mooring Areas",
            #         options = tileOptions(maxZoom=18, zIndex = 1000),
            #         attribution = "&copy; Transport for NSW 2020") %>% 
            addTiles(urlTemplate = "Pittwater/Slip/{z}/{x}/{y}.png", group = "Slipways",
                     options = tileOptions(maxZoom=18, zIndex = 1000),
                     attribution = "&copy; Department of Primary Industries 2020") %>% 
            addTiles(urlTemplate = "Pittwater/Mooring/{z}/{x}/{y}.png", group = "Mooring",
                     options = tileOptions(maxZoom=18, zIndex = 1000),
                     attribution = "&copy; Transport for NSW 2020") %>% 
            addTiles(urlTemplate = "Pittwater/Shore/{z}/{x}/{y}.png", group = "Foreshore",
                     options = tileOptions(maxZoom=18, zIndex = 1000),
                     attribution = "&copy; Department of Primary Industries 2020") %>% 
            # addTiles(urlTemplate = "https://gregjwest.github.io/NSW_EstMacrophytes/Westetal1985/{z}/{x}/{y}.png", group = "West et al. 1985",
            #          options = tileOptions(maxZoom=18, zIndex = 1000),
            #          attribution = "&copy; Department of Primary Industries 2020") %>%
            addTiles(urlTemplate = stringr::str_interp("https://gregjwest.github.io/NSW_EstMacrophytes/Estuaries/${estuary_report_dat()$Est_Name}/${input$date_t1}/{z}/{x}/{y}.png"), group = "Mapped Habitat", 
                     options = tileOptions(maxZoom=18, zIndex = 1000),
                    attribution = "&copy; Department of Primary Industries 2020") %>%
            addRasterImage(empty_raster()) %>%

        
            addMiniMap() %>%
            addScaleBar(position = "bottomleft") %>%
            addLayersControl(
                baseGroups = c("Open Street Map", "Photo basemap"),
                overlayGroups = c("Mapped Habitat", "Foreshore", "Disturbance", "Slipways", "Mooring"),
                options = layersControlOptions(collapsed = FALSE)) %>%
            addMeasure(position = "bottomleft",
                       primaryLengthUnit = "meters",
                       secondaryLengthUnit = "kilometers",
                       primaryAreaUnit = "sqmeters",
                       secondaryAreaUnit = "hectares",
                       activeColor = "#3D535D",
                       completedColor = "#7D4479") %>%
            # hideGroup("West et al. 1985") %>% 
            # hideGroup("Mapped Habitat") %>%
            hideGroup("Foreshore") %>%
            hideGroup("Disturbance") %>%
            hideGroup("Slipways") %>%
            hideGroup("Mooring")

        
 
        
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
        updateSelectInput(session, "habitat3", label = "Habitat:", choices = unique(x3_3))
    })
    
    observe({
        #x2 <- presence_dat %>% dplyr::filter(Estuary == input$estuary3) %>% dplyr::select(Map_Year) %>% dplyr::arrange(desc(Map_Year))
        x2 <- presence_dat$Map_Year[presence_dat$Estuary == input$estuary3]
        updateSelectInput(session, "change_t2", "Time 2: ", choices = sort(unique(x2),decreasing = TRUE))
    })
    observe({
        #x3 <- presence_dat %>% dplyr::filter(Estuary == input$estuary3) %>% dplyr::select(Map_Year)%>% dplyr::arrange(Map_Year) %>% dplyr::filter(Map_Year <= input$change_t2)
        x3 <- presence_dat$Map_Year[presence_dat$Estuary == input$estuary3 & presence_dat$Map_Year <= input$change_t2] 
        updateSelectInput(session, "change_t1", "Time 1: ", choices = unique(x3), )
    })

    
    # DATA PREPERATION
    
    estuary_report_dat1 <- reactive({ report_dat %>%
            dplyr::filter(Estuary == input$estuary3) 
    })
    
    t2_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary3) %>%
            dplyr::filter(Map_Year == input$change_t2) %>%
            dplyr::select(GRID_ID, Easting, Northing, input$habitat3)
     
    })
    
    
    
    t1_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary3) %>%
            dplyr::filter(Map_Year == input$change_t1) %>%
            dplyr::select(GRID_ID, Easting, Northing, input$habitat3)
    })
    

    
    
    diff_dat <- reactive({
        inner_join(t2_dat(),
                   t1_dat(),
                   by = c("GRID_ID", "Easting", "Northing")) %>%
            mutate(Diff = .[[4]] - .[[5]])
    })
    
    #Find true zeros, time one plus time 2 will equal zero when both are zero
    Chnge_dat <- reactive({
        diff_dat() %>%
            dplyr::mutate(Chnge = .[[4]] + .[[5]])
    })    
    #negs <- reactive({
    #    diff_dat() %>%
    #        dplyr::filter(Diff < 0) %>%
    #        dplyr::mutate(Percent = Diff / .[[5]] * 100)
    #})

    #posi <- reactive({
    #    diff_dat() %>%
    #        dplyr::filter(Diff >= 0) %>%
    #        dplyr::mutate(Percent = Diff / .[[4]] * 100) %>%
    #        mutate_all( ~ replace(., is.na(.), 0))
    #})

    #diff_dat1 <- reactive({
    #    dplyr::union(negs(), posi())
    #})
    
    #Filter out the true zeros, select only the values greater than 0
    diff_dat1 <- reactive({
        Chnge_dat() %>%
            dplyr::filter(Chnge > 0)
    })

    #select coordinates
    raster_points1 <- reactive({
        diff_dat1() %>%
            dplyr::select(Easting, Northing)
    })

    #Filter and select values
    raster_values1 <- reactive({
        diff_dat1() %>%
            dplyr::select(Diff)
    }) 
    
    # Set raster max and min based on estuary selection
    lon_min1 <- reactive({min(raster_points1()$Easting)-(cell_size/2)}) 
    lon_max1 <- reactive({max(raster_points1()$Easting)+(cell_size/2)})
    lat_min1 <- reactive({min(raster_points1()$Northing)-(cell_size/2)})
    lat_max1 <- reactive({max(raster_points1()$Northing)+(cell_size/2)})    
    ncols1 <- reactive({((lon_max1() - lon_min1())/cell_size)}) 
    nrows1 <- reactive({((lat_max1() - lat_min1())/cell_size)}) 
    
    # Create and empty raster that covers the estuary selected
    empty_raster1 <- reactive({
        raster(nrows=nrows1(), ncols=ncols1(), 
               xmn=lon_min1(), xmx=lon_max1(), 
               ymn=lat_min1(), ymx=lat_max1(), 
               res=c(cell_size,cell_size), crs= myproj)
    })
    
    # Convert dataframe to a spatial data frame
    raster_pts1 <- reactive({
        SpatialPointsDataFrame(coords=raster_points1(), data=data.frame(raster_values1()), proj4string = CRS(myproj))

        
    }) 
    

    # Rasterize spatial data frame
    diff_raster_utm <- reactive({
        rasterize(raster_pts1(), empty_raster1(), raster_values1(), fun= mean) 
    }) 
    
    
    # diff_raster <- reactive({ 
    #     projectRaster(diff_raster_utm(), crs = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), method = "ngb")
    # })
    

    #Edzar's suggestion using stars package
    raster_utm <- reactive({ 
        st_as_stars(diff_raster_utm())
    })
    
    new <- st_crs(3857)
    
    diff_raster <- reactive({ 
        st_transform(raster_utm(), new)
    })
    
    
    
    
    
    ### Create an asymmetric color range
    # scale_range <- c(-62500, 62500)
    ## Make vector of colors for values smaller than 0 (20 colors)
    # rc1 <- colorRampPalette(colors = c("dark red", "red", "white"), space = "Lab")(10)
    
    ## Make vector of colors for values larger than 0 (180 colors)
    #rc2 <- colorRampPalette(colors = c("white","green", "dark green"), space = "Lab")(10)
    
    ## Combine the two color palettes
    #rampcols <- c(rc1, rc2)
    
    # mypal <- colorNumeric(palette = rampcols, domain = scale_range,  na.color = "transparent")
    
    bins <- c(62500, 32000, 3200, 320, 32, .1, -.1, -32, -320, -3200, -32000, -65000)
    grdpal <- colorBin ("RdYlBu", bins = bins, na.color = "transparent") 
    
    output$map_change <- renderLeaflet({
        leaflet() %>% 
        #leaflet::addTiles() %>%
            #Open Street Map base layer
            addProviderTiles(providers$CartoDB.Positron) %>%
            
            #Spatial Sevices Photo base layer
            addEsriTiledMapLayer("http://maps.six.nsw.gov.au/arcgis/rest/services/public/NSW_Imagery/MapServer", group = "Photo basemap",
                                 layerId = "0",
                                 options (maxZoom = 16)) %>%
            #Add change raster layer
            addStarsImage(diff_raster(), colors = grdpal, opacity = 0.5, group = "Change map", project = FALSE) %>%
            
            #Add side by side comparison slider   
            addMapPane("left", zIndex = 200) %>%
            addMapPane("right", zIndex = 200) %>%
            
            
            addTiles(urlTemplate = stringr::str_interp("https://gregjwest.github.io/NSW_EstMacrophytes/Estuaries/${estuary_report_dat1()$Est_Name}/${input$change_t1}/{z}/{x}/{y}.png"), group = "Time 1 - (Left)", layerId = "Time1",
                     options = pathOptions(pane = "left"),
                     attribution = "&copy; Department of Primary Industries 2020") %>%
            addTiles(urlTemplate = stringr::str_interp("https://gregjwest.github.io/NSW_EstMacrophytes/Estuaries/${estuary_report_dat1()$Est_Name}/${input$change_t2}/{z}/{x}/{y}.png"), group = "Time 2 - (Right)", layerId = "Time2",
                     options = pathOptions(pane = "right"),
                     attribution = "&copy; Department of Primary Industries 2020") %>%
            
            #addMiniMap() %>%
            # ----- addLegend(position = "bottomright", pal = grdpal, values = bins,
            # -----           title = ("Change Area m<sup>2")) %>%
            addLayersControl(
                baseGroups = c("OSM (default)", "Photo basemap"),
                overlayGroups = c("Change map", "Time 1 - (Left)", "Time 2 - (Right)"),
                options = layersControlOptions(collapsed = FALSE)) %>%
            
            #Add side by Side control
            addSidebyside(layerId = "sidecontrols",
                          rightId = "Time2",
                          leftId = "Time1") %>%
            
            addScaleBar(position = "bottomleft") %>%
            addMeasure(position = "bottomleft",
                       primaryLengthUnit = "meters",
                       secondaryLengthUnit = "kilometers",
                       primaryAreaUnit = "sqmeters",
                       secondaryAreaUnit = "hectares",
                       activeColor = "#3D535D",
                       completedColor = "#7D4479") %>%
            
            #change map turned off by default
            hideGroup("Time 1 - (Left)") %>%
            hideGroup("Time 2 - (Right)")


    })
    
   
    
    ###### DATA SUMMARY TAB #####
    # Update Year selector to only provide the estuaries per region
    observe({
        x2_1 <- dat %>% dplyr::filter(Region == input$region2) %>% dplyr::select(Estuary)
        updateSelectInput(session, 
                          "estuary2", 
                          label = "Estuary:",
                          choices = unique(x2_1),
                          selected = "Pittwater")
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
            #dplyr::filter(Habitat == c( "Mangrove", "Saltmarsh","Seagrass","Halophila","Posidonia","Ruppia", "Zostera")) %>%
            dplyr::group_by(Habitat,Map_Year) %>%
            dplyr::summarise(Area = sum(Area)) %>%
            dplyr::filter(Area >= 0)
        
    })
    
    
    output$habitat_plot <- renderPlot({
        ggplot(data=plot_dat(), aes(x=Map_Year, y=Area/10000)) +
            geom_line(colour="#989898", size=1) +
            geom_point(shape = 23, fill="lightgray", color = "black", size=3) +
            
            #geom_col(fill="#053f5c") + # change the x = fMap_Year
            #ylim(0,NA) +
            ylab("Hectares") +
            xlab("Years Mapped") +
            scale_x_continuous(breaks=seq(1985, 2020, 3)) +
            facet_wrap(~Habitat, scales = "free_y") +
            scale_y_continuous(limits = c(0, NA)) +
            theme_bw() +
            theme(axis.title.x = element_text(face="bold", size=14), axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
                    axis.title.y = element_text(face="bold", size=14),
                    axis.text.y  = element_text(size=14),
                    strip.text.x = element_text(size = 14, face = "italic"))
    },height = 550, width = 1000 )
    
    
    
    # Change in mangrove v saltmarsh (MVS)
    
    # Update Year selector to only provide the years that have been mapped

    observe({
        
        x5 <- dat %>% 
            dplyr::filter(Estuary == input$estuary2) %>% 
            dplyr::select(Map_Year) %>% 
            dplyr::arrange(desc(Map_Year))
            #dplyr::filter(Map_Year <= input$mvs_t2) 
        
        updateSelectInput(session, "mvs_t2", "Time 2: ", choices = unique(x5), selected = max(x5$Map_Year))
    })
    
       observe({
        
        x4 <- dat %>% 
            dplyr::filter(Estuary == input$estuary2) %>% 
            dplyr::select(Map_Year) %>% 
            #dplyr::arrange(desc(Map_Year)) %>%
            dplyr::filter(Map_Year <= input$mvs_t2)
        updateSelectInput(session, "mvs_t1", "Time 1: ", choices = unique(x4), selected = min(x4$Map_Year))
    })
     
    #     # DATA PREPERATION
    mvs_t2_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary2) %>%
            dplyr::filter(Map_Year == input$mvs_t2) %>%
            dplyr::select(GRID_ID, Mangrove, Saltmarsh)
        
    })
    
    
    
    mvs_t1_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary2) %>%
            dplyr::filter(Map_Year == input$mvs_t1) %>%
            dplyr::select(GRID_ID, Mangrove, Saltmarsh)
    })
    
    
    
    
    mvs_diff_dat <- reactive({
        inner_join(mvs_t2_dat(),
                   mvs_t1_dat(),
                   by = c("GRID_ID")) %>%
            mutate(delta_mangrove = .[[2]] - .[[4]]) %>%
            mutate(delta_saltmarsh = .[[3]] - .[[5]]) %>%
            filter(delta_saltmarsh != 0) %>%
            filter(delta_mangrove != 0)
    })
    
    
    
    output$MVS <- renderPlot({
        ggplot(mvs_diff_dat()) +
            theme_geometry(mvs_diff_dat()$delta_mangrove, mvs_diff_dat()$delta_saltmarsh) +
            theme_light() +
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank()) +
            geom_point(aes(x = delta_mangrove, y = delta_saltmarsh), size = 3, shape = 21, fill="lightgray", color = "black", alpha = .5) +
            annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) #+
            # annotate("text", x = -52000, y = 62000, label = "- Mangrove", color = "red", size = 5) +
            # annotate("text", x = -52000, y = 52000, label = "+ Saltmarsh", color = "green4", size = 5) +
            # annotate("text", x = -52000, y = -52000, label = "- Mangrove", color = "red", size = 5) +
            # annotate("text", x = -52000, y = -62000, label = "- Saltmarsh", color = "red", size = 5) +
            # annotate("text", x = 52000, y = 62000, label = "+ Mangrove", color = "green4", size = 5) +
            # annotate("text", x = 52000, y = 52000, label = "+ Saltmarsh", color = "green4", size = 5) +
            # annotate("text", x = 52000, y = -62000, label = "+ Mangrove", color = "green4", size = 5) +
            # annotate("text", x = 52000, y = -52000, label = "- Saltmarsh", color = "red", size = 5) #+
        
        # geom_segment(aes(x = 250000, xend = 250000, y= 1, yend= -250000),
        #              arrow=arrow(length=unit(0.2,"cm")), colour = "red") +
        # geom_segment(aes(x = 250000, xend = 250000, y= 1, yend= 250000),
        #              arrow=arrow(length=unit(0.2,"cm")), color = "green") +
        # geom_segment(aes(x = 1, xend = -250000, y= 250000, yend= 250000),
        #              arrow=arrow(length=unit(0.2,"cm")), colour = "red") +
        # geom_segment(aes(x = 1, xend = 250000, y= 250000, yend= 250000),
        #              arrow=arrow(length=unit(0.2,"cm")), color = "green") 
        
    },height = 500, width = 800 )
    
    
    
    #     
    #     
    #     
    #     
    #     
    #     
    #     
  
    
    
    
    
        
    
    
    
    
    
}


shinyApp(ui, server)





































