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

#pittwater_grid <- rgdal::readOGR("json/Pittwater_Grid.geojson")
#mooring_area <- rgdal::readOGR("json/MooringArea.geojson")



# Read in master data .csv
# THe data is supplied by Greg West and should be in the exact same layout (column names, species names and all are case sensitive. Ensure there are no unknown spaces)
dat <-  read.csv("Multi_Estuary_All_Years_Grid.csv", stringsAsFactors = T)

report_dat <-  read.csv("Estuary_Overview.csv", stringsAsFactors = T)

presence_dat <- read.csv("NSW_Estuarine_Macrophytes_Presence.csv", stringsAsFactors = T)

dat$fMap_Year <- factor(dat$Map_Year)

# Transpose data to long format to enable filtering by species.
dat1 <- dat %>%
    gather("Habitat", "Area", 7:14) %>%
    dplyr::filter(Habitat != "Mangrove_Saltmarsh")
dat1$Habitat <- factor(dat1$Habitat, levels = c("Mangrove", "Saltmarsh","Seagrass_all_species","Posidonia", "Zostera","Halophila","Ruppia"))


# Create a data frame of the total area of each species by estuary and year. To be graphed and tabled in the data summary tab.
dat_summary <- dat %>%
    group_by(Estuary,Map_Year) %>%
    summarise(Seagrass = sum(Seagrass_all_species/10000),
              Mangrove = sum(Mangrove/10000),
              Saltmarsh = sum(Saltmarsh/10000)) %>%
    mutate(Seagrass = round(Seagrass, 0)) %>%
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
            selectInput("habitat3",
                        label= "Habitat: ",
                        choices = list_habs, 
                        selected = list_habs[index_habs]
            ),
            selectInput("change_t2", 
                        label = "Year: ", 
                        choices = NULL, 
                        selected = "2017"
            ),
            selectInput("change_t1",
                        label = "Compared to: ",
                        choices = NULL,
                        selected = "1985"
            ),
            tags$h6("The latest macrophyte maps for each estuary can be downloaded from the NSW DPI Fisheries Data Portal.", 
                    tags$a(href="https://webmap.industry.nsw.gov.au/Html5Viewer/index.html?viewer=Fisheries_Data_Portal", target="_blank", 
                           tags$img(src='DPI_LOGO_left.svg')), 
            
                    absolutePanel(id = "legend1", class = "card", bottom = 475, right = 0,  fixed=TRUE, draggable = FALSE, width= "auto", height = "auto",
                                  tags$img(src='Macrophyte_Legend.svg',height='300',width='200'))
            ),
                    absolutePanel(id = "legend1", class = "card", bottom = 200, right = 15,  fixed=TRUE, draggable = FALSE, width= "auto", height = "auto",
                                tags$img(src='Grid_Legend_Curved.svg',height='275',width='170'))
          
            )
        )
    )
        
    ),


        
                                
##### Data Summary NEW Tab UI                  ----
tabPanel(
    "Data Summary",
    fluidPage(
        tags$style(type="text/css", "body {padding-top: 70px; width: 1140px; text-align: justify; margin: auto;}"),
        fluidRow(
            h3("Explore the data"),
            h5("First select the region and estuary of interest."),
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
            tags$br(),tags$br(),tags$br(),tags$br(),
            
            h4("Change in time for each species"),
            
            tags$p("Area (hectares) of mapped habitat in the selected estuary are graphed for each year that mapping has been done."),
            
            column(6, offset = 0,
                   style = "height:650px; ", 
                   plotOutput("habitat_plot", width = "100%"))
        ),
        fluidRow(
            tags$br(),tags$br(),
            h4("Change in Mangrove relative to Saltmarshes"),
            tags$p("To examine changes in the ratio of mangrove to saltmarsh over time, first select the years to compare.  The predominance of plotted points in different quadrants of the graph indicate the type of change in these two habitats, e.g.  whether mangroves have increased at the expense of saltmarshes, or some other pattern. Each point represents the change in ratio of mangrove area to saltmarsh area in a 250 x 250 m map grid. "),
            
        ),
        fluidRow(column(4,
                       
            selectInput("mvs_t2", 
                        label = "Latest Year: ", 
                        choices = NULL, 
                        selected = "2017"
            )
            ),
            column(4,
            selectInput("mvs_t1",
                        label = "Compared to previous: ",
                        choices = NULL,
                        selected = "1985"
            
                        )
            ),
            tags$br(),
            tags$br(),
            
            column(6, offset = 3,plotOutput("MVS"))

    )
    )
    ),
    
        


#### Download report tab UI -----
tabPanel("Download Report",

        fluidRow(style = "border: 4px double #bbb;",
            HTML('<button data-toggle="collapse" data-target="#demo2">Show/Hide</button>'),
            tags$div(id = 'demo2',  class="collapse in",
                     
             column(12, offset = 0.5,
                    h4("Download an Estuary Report"),
                    tags$p("This is under construction. Soon you will be able to click on a button to save a PDF version of this report. For the time being you can select a region, estuary and two years to compare, then review the report below. You can save the report as HTML by right clicking on the page and selecting 'save as'. Otherwise, this can be saved as a PDF by right clicking on the page, select print, then select 'print as PDF'. Before saving or printing, click on the hide button to minimise this menu."),
                    ),
             
             column(3, offset = 0.5,
                    
                    selectInput("region4", 
                                label = "Region:", 
                                choices = list_region, 
                                selected = list_region[index_region])
                    ),
             column(3, offset = 0.5,
                    selectInput("estuary4",
                                label = "Estuary:",
                                choices = list_estuary,
                                selected = list_estuary[index_estuary])
             ),
             
             column(2, offset = 0.5,
                    selectInput("report_t2", 
                                label = "Year: ", 
                                choices = NULL, 
                                selected = "2017")
                    ),
             
             column(2, offset = 0.5,
                    selectInput("report_t1",
                                label = "Compared to: ",
                                choices = NULL,
                                selected = "1985")
                    )
            )
             ),
        
        fluidRow(
            column(11, offset = 0.5,
                    
         tags$h3(textOutput("summary_title")
         ),
         tags$p(textOutput("estuary_type")
         ),
         tags$p(htmlOutput("catchment_area")
         ),
         tags$p(htmlOutput("estuary_area")
         ),
         tags$p(textOutput("ave_depth")
         ),
         tags$p("Years mapped: "), textOutput("years_mapped")
         ,
         
         tags$br(),

         tags$p("Macrophyte maps for the latest time of sampling can be donwloaded from the", 
                 tags$a(href="https://webmap.industry.nsw.gov.au/Html5Viewer/index.html?viewer=Fisheries_Data_Portal", target="_blank", "Spatial Data Portal.") ) 
         
         
         )),
         
         
         
         
         fluidRow(
             column(3, offset = 0.5,
                    tags$h4(tags$em("Posidonia"), "change"),
                    
                    leafletOutput("report_pos_map", height = 520, width = 520)
                    
             ),
             column(4, offset = 3,
                    tags$h4(tags$em("Zostera"), "change"),
                    leafletOutput("report_zos_map", height = 520, width = 520)
             )
         ),
         fluidRow(
             column(3, offset = 0.5,
                    tags$h4("Mangrove change"),
                    leafletOutput("report_mang_map", height = 520, width = 520)
                    
             ),
             column(4, offset = 3,
                    tags$h4("Saltmarsh change"),
                    leafletOutput("report_salt_map", height = 520, width = 520)
                    
             )
         ),
        
        tags$br(), tags$br(),        tags$br(), tags$br(),

        
        fluidRow(
            column(3, offset = 0.5,
                   tags$h4(tags$em("Posidonia"), "change over time"),

                   plotOutput("pos_plot")
            ),
                   
            column(4, offset = 3,
                    tags$h4(tags$em("Zostera"), "change over time"),
                    plotOutput("zos_plot")
            )

            
            ),
        
        fluidRow(
            column(3, offset = 0.5,
                   tags$h4("Mangrove change over time"),
                   
                   plotOutput("man_plot")
            ),
            
            column(4, offset = 3,
                   tags$h4("Saltmarsh change over time"),
                   plotOutput("sal_plot")
            )
            
            
        ),
         
        tags$br(), tags$br(),        tags$br(), 
        
        fluidRow(column(6, offset = 0.5,
                         tags$br(),
                         h4("Change in Mangrove relative to Saltmarsh"),  
                         column(8, offset = 2,
                                style = "height:500px; width:500px ",
                                tags$br(),
                                
                                plotOutput("report_MVS")
                         )
         )),
         
         fluidRow(column(6, offset = 0.5,
                         h5("Disclaimer: The information provided in this report is a guide and the authors are not responsible..... "),
                         h5("Acknowledgments.....")
                         
                         
         ))
         
                    
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

    
## User selections carry through to each tabe forwards and backwards
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
        x0 <- presence_dat %>% dplyr::filter(Region == input$region1) %>% dplyr::select(Estuary)
        updateSelectInput(session, 
                          "estuary1", 
                          label = "Estuary:",
                          choices = unique(x0),
                          selected = "Port Stephens")
    })
    # Update Year selector to only provide the years that have been mapped
    observe({
        x1 <- presence_dat %>% dplyr::filter(Estuary == input$estuary1) %>% dplyr::select(Map_Year)
        updateSelectInput(session, 
                          "year1", 
                          label = "Year:",
                          choices = unique(x1))
    })
    
    
    # Raster building based on users selection
    # Filter and select coordinates
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
            addTiles(urlTemplate = "Pittwater/Pittwater1985/{z}/{x}/{y}.png", group = "West et al. 1985",
                     options = tileOptions(maxZoom=18, zIndex = 1000),
                     attribution = "&copy; Department of Primary Industries 2020") %>%
            addTiles(urlTemplate = "Pittwater/Pittwater2000/{z}/{x}/{y}.png", group = "Pittwater 2000",
                     options = tileOptions(maxZoom=18, zIndex = 1000),
                     attribution = "&copy; Department of Primary Industries 2020") %>%
            addTiles(urlTemplate = "Pittwater/Pittwater2005/{z}/{x}/{y}.png", group = "Pittwater 2005",
                     options = tileOptions(maxZoom=18, zIndex = 1000),
                     attribution = "&copy; Department of Primary Industries 2020") %>%
            addTiles(urlTemplate = "Pittwater/Pittwater2019/{z}/{x}/{y}.png", group = "Pittwater 2019",
                     options = tileOptions(maxZoom=18, zIndex = 1000),
                     attribution = "&copy; Department of Primary Industries 2020") %>%
            addRasterImage(empty_raster()) %>%
            
            #add geojson
            # addPolygons(data = grid_shp, color = "#444", stroke = TRUE, weight = .1, smoothFactor = 0.3, fillOpacity = .1,  popup = ~htmlEscape(GRID_ID), group = "Change Grid") %>%  
            #addPolygons(data = mooring_area, fillColor = "#35D6F0", stroke = TRUE, color = "#1D7582", weight = 1, smoothFactor = 0.3, fillOpacity = .2, group = "Mooring Areas") %>%
         
        
            addMiniMap() %>%
            addScaleBar(position = "bottomleft") %>%
            addLayersControl(
                baseGroups = c("Open Street Map", "Photo basemap"),
                overlayGroups = c("West et al. 1985", "Pittwater 2000", "Pittwater 2005", "Pittwater 2019", "Foreshore", "Disturbance", "Slipways", "Mooring"),
                options = layersControlOptions(collapsed = FALSE)) %>%
            addMeasure(position = "bottomleft",
                       primaryLengthUnit = "meters",
                       secondaryLengthUnit = "kilometers",
                       primaryAreaUnit = "sqmeters",
                       secondaryAreaUnit = "hectares",
                       activeColor = "#3D535D",
                       completedColor = "#7D4479") %>%
            hideGroup("West et al. 1985") %>% 
            hideGroup("Pittwater 2000") %>%
            hideGroup("Pittwater 2005") %>%
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
        x3_2 <- presence_dat %>% dplyr::filter(Map_Year == input$change_t1 | Map_Year== input$change_t2) %>% dplyr::filter(Estuary == input$estuary3) %>% dplyr::select(Macrophyte)
        updateSelectInput(session, "habitat3", label = "Habitat:", choices = unique(x3_2))
    })
    
    observe({
        x2 <- presence_dat %>% dplyr::filter(Estuary == input$estuary3) %>% dplyr::select(Map_Year) %>% dplyr::arrange(desc(Map_Year))
        updateSelectInput(session, "change_t2", "Year: ", choices = unique(x2))
    })
    observe({
        x3 <- presence_dat %>% dplyr::filter(Estuary == input$estuary3) %>% dplyr::select(Map_Year)%>% dplyr::arrange(Map_Year) %>% dplyr::filter(Map_Year <= input$change_t2)
        updateSelectInput(session, "change_t1", "Compared to: ", choices = unique(x3))
    })

    
    # DATA PREPERATION
    
    
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
            addTiles(urlTemplate = "Pittwater/Pittwater1985/{z}/{x}/{y}.png", group = "Time 1 - (Left)", layerId = "Time1",
                     options = pathOptions(pane = "left"),
                     attribution = "&copy; Department of Primary Industries 2020") %>%
            addTiles(urlTemplate = "Pittwater/Pittwater2000/{z}/{x}/{y}.png", group = "Time 2 - (Right)", layerId = "Time2",
                     options = pathOptions(pane = "right"),
                     attribution = "&copy; Department of Primary Industries 2020") %>%
            
            addMiniMap() %>%
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
            geom_point(colour="#404040", size=1.2) +
            
            #geom_col(fill="#053f5c") + # change the x = fMap_Year
            ylim(0,NA) +
            ylab("Hectares") +
            xlab("Years Mapped") +
            scale_x_continuous(breaks=seq(1985, 2020, 3)) +
            facet_wrap(Habitat ~ . ) +
            theme_bw() +
            theme(axis.title.x = element_text(face="bold", size=14), axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
                    axis.title.y = element_text(face="bold", size=14),
                    axis.text.y  = element_text(size=14),
                    strip.text.x = element_text(size = 14, face = "italic"))
    },height = 650, width = 1000 )
    
    
    
    # Change in mangrove v saltmarsh (MVS)
    
    # Update Year selector to only provide the years that have been mapped
    observe({
        
        x4 <- dat %>% 
            dplyr::filter(Estuary == input$estuary2) %>% 
            dplyr::select(Map_Year) %>% 
            dplyr::arrange(desc(Map_Year))
        
        updateSelectInput(session, "mvs_t2", "Year: ", choices = unique(x4), selected = max(x4$Map_Year))
    })
    
    observe({
        
        x5 <- dat %>% 
            dplyr::filter(Estuary == input$estuary2) %>% 
            dplyr::select(Map_Year) %>% 
            dplyr::arrange(Map_Year) %>% 
            dplyr::filter(Map_Year <= input$mvs_t2) 
        
        updateSelectInput(session, "mvs_t1", "Compared to: ", choices = unique(x5), selected = min(x5$Map_Year))
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
            geom_point(aes(x = delta_mangrove, y = delta_saltmarsh), size = .5, color = "#053f5c") +
            annotate("text", x = -52000, y = 62000, label = "- Mangrove", color = "red", size = 5) +
            annotate("text", x = -52000, y = 52000, label = "+ Saltmarsh", color = "green4", size = 5) +
            annotate("text", x = -52000, y = -52000, label = "- Mangrove", color = "red", size = 5) +
            annotate("text", x = -52000, y = -62000, label = "- Saltmarsh", color = "red", size = 5) +
            annotate("text", x = 52000, y = 62000, label = "+ Mangrove", color = "green4", size = 5) +
            annotate("text", x = 52000, y = 52000, label = "+ Saltmarsh", color = "green4", size = 5) +
            annotate("text", x = 52000, y = -62000, label = "+ Mangrove", color = "green4", size = 5) +
            annotate("text", x = 52000, y = -52000, label = "- Saltmarsh", color = "red", size = 5) #+
        
        # geom_segment(aes(x = 250000, xend = 250000, y= 1, yend= -250000),
        #              arrow=arrow(length=unit(0.2,"cm")), colour = "red") +
        # geom_segment(aes(x = 250000, xend = 250000, y= 1, yend= 250000),
        #              arrow=arrow(length=unit(0.2,"cm")), color = "green") +
        # geom_segment(aes(x = 1, xend = -250000, y= 250000, yend= 250000),
        #              arrow=arrow(length=unit(0.2,"cm")), colour = "red") +
        # geom_segment(aes(x = 1, xend = 250000, y= 250000, yend= 250000),
        #              arrow=arrow(length=unit(0.2,"cm")), color = "green") 
        
    })
    
    
    
    #     
    #     
    #     
    #     
    #     
    #     
    #     
  
    
    
    
    
        
    
    
    
    
    
       
##### REPORT TAB ###########################################
    
    # UI preparation
    observe({
        x4_1 <- dat %>% 
            dplyr::filter(Region == input$region4) %>% 
            dplyr::select(Estuary)
        updateSelectInput(session, 
                          "estuary4", 
                          label = "Estuary:",
                          choices = unique(x4_1),
                          selected = "Pittwater")
    })
    
    observe({
        x4_2 <- dat %>% 
            dplyr::filter(Estuary == input$estuary4) %>% 
            dplyr::select(Map_Year) %>% 
            dplyr::arrange(desc(Map_Year))
        updateSelectInput(session, "report_t2", 
                          "Year: ", 
                          choices = unique(x4_2))
    })
    
    observe({
        x4_3 <- dat %>% 
            dplyr::filter(Estuary == input$estuary4) %>% 
            dplyr::select(Map_Year)%>% 
            dplyr::arrange(Map_Year) %>% 
            dplyr::filter(Map_Year <= input$report_t2)
        updateSelectInput(session, "report_t1", 
                          "Compared to: ", 
                          choices = unique(x4_3))
    })
    
    
    
    # Estuary Summary Table
    estuary_report_dat <- reactive({ report_dat %>%
            dplyr::filter(Estuary == input$estuary4) 
        })
    
    output$summary_title <- renderText({ 
        paste(input$estuary4,"- ", input$report_t2, " compared to ", input$report_t1)
    })
    
    output$estuary_type <- renderText({ 
        paste("Estuary type:", unique(estuary_report_dat()$RoyEstuarytype))
    })
    
    output$catchment_area <- renderText({ 
       paste("Catchment area:", round(unique(estuary_report_dat()$Catchmentarea), digits = 0),"km<sup>2</sup>")
    })
    
    output$estuary_area <- renderText({ 
        paste("Estuary area:", round(unique(estuary_report_dat()$Estuaryarea), digits = 0),"km<sup>2</sup>")
    })
    
    output$ave_depth <- renderText({ 
        paste("Average depth:", round(unique(estuary_report_dat()$AveDepth), digits = 1),"m")
    })
    
    
    years_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary4)
    })
    
    
    output$years_mapped <- renderText({ 
        paste(unique(years_dat()$Map_Year))
    })
    
    
    
    
    
    # #Posidonia change leaflet map
    pos_t2_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary4) %>%
            dplyr::filter(Map_Year == input$report_t2) %>%
            dplyr::select(GRID_ID, Easting, Northing, Posidonia)
    })
    pos_t1_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary4) %>%
            dplyr::filter(Map_Year == input$report_t1) %>%
            dplyr::select(GRID_ID, Easting, Northing, Posidonia)
    })
    pos_diff_dat <- reactive({
        inner_join(pos_t2_dat(),
                   pos_t1_dat(),
                   by = c("GRID_ID", "Easting", "Northing")) %>%
            mutate(Diff = .[[4]] - .[[5]])
    })
    pos_diff_negs <- reactive({
        pos_diff_dat() %>%
            dplyr::filter(Diff < 0) %>%
            dplyr::mutate(Percent = Diff / .[[5]] * 100)
    })
    pos_diff_posi <- reactive({
        pos_diff_dat() %>%
            dplyr::filter(Diff >= 0) %>%
            dplyr::mutate(Percent = Diff / .[[4]] * 100) %>%
            mutate_all( ~ replace(., is.na(.), 0))
    })
    pos_diff_dat1 <- reactive({
        dplyr::union(pos_diff_negs(), pos_diff_posi())
    })
    #select coordinates
    pos_raster_points <- reactive({
        pos_diff_dat1() %>%
            dplyr::select(Easting, Northing)
    })
    #Filter and select values
    pos_raster_values <- reactive({
        pos_diff_dat1() %>%
            dplyr::select(Diff)
    })
    # Set raster max and min based on estuary selection
    pos_lon_min <- reactive({min(pos_raster_points()$Easting)})
    pos_lon_max <- reactive({max(pos_raster_points()$Easting)})
    pos_lat_min <- reactive({min(pos_raster_points()$Northing)})
    pos_lat_max <- reactive({max(pos_raster_points()$Northing)})
    pos_ncols <- reactive({((pos_lon_max() - pos_lon_min())/cell_size)})
    pos_nrows <- reactive({((pos_lat_max() - pos_lat_min())/cell_size)})
    # Create and empty raster that covers the estuary selected
    pos_empty_raster <- reactive({
        raster(nrows=pos_nrows(), ncols=pos_ncols(),
               xmn=pos_lon_min(), xmx=pos_lon_max(),
               ymn=pos_lat_min(), ymx=pos_lat_max(),
               res=c(cell_size,cell_size), crs=myproj)
    })
    # Convert dataframe to a spatial data frame
    pos_raster_pts <- reactive({
        SpatialPointsDataFrame(coords=pos_raster_points(), data=data.frame(pos_raster_values()))
    })
    # Rasterize spatial data frame
    pos_diff_raster <- reactive({
        rasterize(pos_raster_points(), pos_empty_raster(), pos_raster_values(), fun= mean)
    })
    ### Create an asymmetric color range
    scale_range <- c(-62500, 62500)
    ## Make vector of colors for values smaller than 0 (20 colors)
    rc1 <- colorRampPalette(colors = c("dark red", "red", "white"), space = "Lab")(10)
    ## Make vector of colors for values larger than 0 (180 colors)
    rc2 <- colorRampPalette(colors = c("white","green", "dark green"), space = "Lab")(10)
    ## Combine the two color palettes
    rampcols <- c(rc1, rc2)
    mypal <- colorNumeric(palette = rampcols, domain = scale_range,  na.color = "transparent")
    map1 = reactive({
        leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        # addLegend(position = "bottomright", pal = mypal, values = scale_range,
        #           title = ("Change Area m<sup>2")) %>%
        addRasterImage(pos_diff_raster(), colors = mypal, opacity = 0.75)
    })
    output$report_pos_map = renderLeaflet(map1())
    
    
    
    
    
    # #Zostera change leaflet map
    zos_t2_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary4) %>%
            dplyr::filter(Map_Year == input$report_t2) %>%
            dplyr::select(GRID_ID, Easting, Northing, Zostera)
    })
    zos_t1_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary4) %>%
            dplyr::filter(Map_Year == input$report_t1) %>%
            dplyr::select(GRID_ID, Easting, Northing, Zostera)
    })
    zos_diff_dat <- reactive({
        inner_join(zos_t2_dat(),
                   zos_t1_dat(),
                   by = c("GRID_ID", "Easting", "Northing")) %>%
            mutate(Diff = .[[4]] - .[[5]])
    })
    zos_diff_negs <- reactive({
        zos_diff_dat() %>%
            dplyr::filter(Diff < 0) %>%
            dplyr::mutate(Percent = Diff / .[[5]] * 100)
    })
    zos_diff_posi <- reactive({
        zos_diff_dat() %>%
            dplyr::filter(Diff >= 0) %>%
            dplyr::mutate(Percent = Diff / .[[4]] * 100) %>%
            mutate_all( ~ replace(., is.na(.), 0))
    })
    zos_diff_dat1 <- reactive({
        dplyr::union(zos_diff_negs(), zos_diff_posi())
    })
    #select coordinates
    zos_raster_points <- reactive({
        zos_diff_dat1() %>%
            dplyr::select(Easting, Northing)
    })
    #Filter and select values
    zos_raster_values <- reactive({
        zos_diff_dat1() %>%
            dplyr::select(Diff)
    })
    # Set raster max and min based on estuary selection
    zos_lon_min <- reactive({min(zos_raster_points()$Easting)})
    zos_lon_max <- reactive({max(zos_raster_points()$Easting)})
    zos_lat_min <- reactive({min(zos_raster_points()$Northing)})
    zos_lat_max <- reactive({max(zos_raster_points()$Northing)})
    zos_ncols <- reactive({((zos_lon_max() - zos_lon_min())/cell_size)+1})
    zos_nrows <- reactive({((zos_lat_max() - zos_lat_min())/cell_size)+1})
    # Create and empty raster that covers the estuary selected
    zos_empty_raster <- reactive({
        raster(nrows=zos_nrows(), ncols=zos_ncols(),
               xmn=zos_lon_min(), xmx=zos_lon_max(),
               ymn=zos_lat_min(), ymx=zos_lat_max(),
               res=c(cell_size,cell_size), crs=myproj)
    })
    # Convert dataframe to a spatial data frame
    zos_raster_pts <- reactive({
        SpatialPointsDataFrame(coords=zos_raster_points(), data=data.frame(zos_raster_values()))
    })
    # Rasterize spatial data frame
    zos_diff_raster <- reactive({
        rasterize(zos_raster_points(), zos_empty_raster(), zos_raster_values(), fun= mean)
    })
    ### Create an asymmetric color range
    scale_range <- c(-62500, 62500)
    ## Make vector of colors for values smaller than 0 (20 colors)
    rc1 <- colorRampPalette(colors = c("dark red", "red", "white"), space = "Lab")(10)
    ## Make vector of colors for values larger than 0 (180 colors)
    rc2 <- colorRampPalette(colors = c("white","green", "dark green"), space = "Lab")(10)
    ## Combine the two color palettes
    rampcols <- c(rc1, rc2)
    mypal <- colorNumeric(palette = rampcols, domain = scale_range,  na.color = "transparent")
    map2 = reactive({
        leaflet() %>% 
            addProviderTiles(providers$CartoDB.Positron) %>% 
            # addLegend(position = "bottomright", pal = mypal, values = scale_range,
            #           title = ("Change Area m<sup>2")) %>%
            addRasterImage(zos_diff_raster(), colors = mypal, opacity = 0.75)
    })
    output$report_zos_map = renderLeaflet(map2())
    
    

    
    # #Mangrove change leaflet map
    mang_t2_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary4) %>%
            dplyr::filter(Map_Year == input$report_t2) %>%
            dplyr::select(GRID_ID, Easting, Northing, Mangrove)
    })
    mang_t1_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary4) %>%
            dplyr::filter(Map_Year == input$report_t1) %>%
            dplyr::select(GRID_ID, Easting, Northing, Mangrove)
    })
    mang_diff_dat <- reactive({
        inner_join(mang_t2_dat(),
                   mang_t1_dat(),
                   by = c("GRID_ID", "Easting", "Northing")) %>%
            mutate(Diff = .[[4]] - .[[5]])
    })
    mang_diff_negs <- reactive({
        mang_diff_dat() %>%
            dplyr::filter(Diff < 0) %>%
            dplyr::mutate(Percent = Diff / .[[5]] * 100)
    })
    mang_diff_posi <- reactive({
        mang_diff_dat() %>%
            dplyr::filter(Diff >= 0) %>%
            dplyr::mutate(Percent = Diff / .[[4]] * 100) %>%
            mutate_all( ~ replace(., is.na(.), 0))
    })
    mang_diff_dat1 <- reactive({
        dplyr::union(mang_diff_negs(), mang_diff_posi())
    })
    #select coordinates
    mang_raster_points <- reactive({
        mang_diff_dat1() %>%
            dplyr::select(Easting, Northing)
    })
    #Filter and select values
    mang_raster_values <- reactive({
        mang_diff_dat1() %>%
            dplyr::select(Diff)
    })
    # Set raster max and min based on estuary selection
    mang_lon_min <- reactive({min(mang_raster_points()$Easting)})
    mang_lon_max <- reactive({max(mang_raster_points()$Easting)})
    mang_lat_min <- reactive({min(mang_raster_points()$Northing)})
    mang_lat_max <- reactive({max(mang_raster_points()$Northing)})
    mang_ncols <- reactive({((mang_lon_max() - mang_lon_min())/cell_size)+1})
    mang_nrows <- reactive({((mang_lat_max() - mang_lat_min())/cell_size)+1})
    # Create and empty raster that covers the estuary selected
    mang_empty_raster <- reactive({
        raster(nrows=mang_nrows(), ncols=mang_ncols(),
               xmn=mang_lon_min(), xmx=mang_lon_max(),
               ymn=mang_lat_min(), ymx=mang_lat_max(),
               res=c(cell_size,cell_size), crs=myproj)
    })
    # Convert dataframe to a spatial data frame
    mang_raster_pts <- reactive({
        SpatialPointsDataFrame(coords=mang_raster_points(), data=data.frame(mang_raster_values()))
    })
    # Rasterize spatial data frame
    mang_diff_raster <- reactive({
        rasterize(mang_raster_points(), mang_empty_raster(), mang_raster_values(), fun= mean)
    })
    ### Create an asymmetric color range
    scale_range <- c(-62500, 62500)
    ## Make vector of colors for values smaller than 0 (20 colors)
    rc1 <- colorRampPalette(colors = c("dark red", "red", "white"), space = "Lab")(10)
    ## Make vector of colors for values larger than 0 (180 colors)
    rc2 <- colorRampPalette(colors = c("white","green", "dark green"), space = "Lab")(10)
    ## Combine the two color palettes
    rampcols <- c(rc1, rc2)
    mypal <- colorNumeric(palette = rampcols, domain = scale_range,  na.color = "transparent")
    map3 = reactive({
        leaflet() %>% 
            addProviderTiles(providers$CartoDB.Positron) %>% 
            # addLegend(position = "bottomright", pal = mypal, values = scale_range,
            #           title = ("Change Area m<sup>2")) %>%
            addRasterImage(mang_diff_raster(), colors = mypal, opacity = 0.75)
    })
    output$report_mang_map = renderLeaflet(map3())
    
    
    # #Saltmarsh change leaflet map
    salt_t2_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary4) %>%
            dplyr::filter(Map_Year == input$report_t2) %>%
            dplyr::select(GRID_ID, Easting, Northing, Saltmarsh)
    })
    salt_t1_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary4) %>%
            dplyr::filter(Map_Year == input$report_t1) %>%
            dplyr::select(GRID_ID, Easting, Northing, Saltmarsh)
    })
    salt_diff_dat <- reactive({
        inner_join(salt_t2_dat(),
                   salt_t1_dat(),
                   by = c("GRID_ID", "Easting", "Northing")) %>%
            mutate(Diff = .[[4]] - .[[5]])
    })
    salt_diff_negs <- reactive({
        salt_diff_dat() %>%
            dplyr::filter(Diff < 0) %>%
            dplyr::mutate(Percent = Diff / .[[5]] * 100)
    })
    salt_diff_posi <- reactive({
        salt_diff_dat() %>%
            dplyr::filter(Diff >= 0) %>%
            dplyr::mutate(Percent = Diff / .[[4]] * 100) %>%
            mutate_all( ~ replace(., is.na(.), 0))
    })
    salt_diff_dat1 <- reactive({
        dplyr::union(salt_diff_negs(), salt_diff_posi())
    })
    #select coordinates
    salt_raster_points <- reactive({
        salt_diff_dat1() %>%
            dplyr::select(Easting, Northing)
    })
    #Filter and select values
    salt_raster_values <- reactive({
        salt_diff_dat1() %>%
            dplyr::select(Diff)
    })
    # Set raster max and min based on estuary selection
    salt_lon_min <- reactive({min(salt_raster_points()$Easting)})
    salt_lon_max <- reactive({max(salt_raster_points()$Easting)})
    salt_lat_min <- reactive({min(salt_raster_points()$Northing)})
    salt_lat_max <- reactive({max(salt_raster_points()$Northing)})
    salt_ncols <- reactive({((salt_lon_max() - salt_lon_min())/cell_size)})
    salt_nrows <- reactive({((salt_lat_max() - salt_lat_min())/cell_size)})
    # Create and empty raster that covers the estuary selected
    salt_empty_raster <- reactive({
        raster(nrows=salt_nrows(), ncols=salt_ncols(),
               xmn=salt_lon_min(), xmx=salt_lon_max(),
               ymn=salt_lat_min(), ymx=salt_lat_max(),
               res=c(cell_size,cell_size), crs=myproj)
    })
    # Convert dataframe to a spatial data frame
    salt_raster_pts <- reactive({
        SpatialPointsDataFrame(coords=salt_raster_points(), data=data.frame(salt_raster_values()))
    })
    # Rasterize spatial data frame
    salt_diff_raster <- reactive({
        rasterize(salt_raster_points(), salt_empty_raster(), salt_raster_values(), fun= mean)
    })
    ### Create an asymmetric color range
    scale_range <- c(-62500, 62500)
    ## Make vector of colors for values smaller than 0 (20 colors)
    rc1 <- colorRampPalette(colors = c("dark red", "red", "white"), space = "Lab")(10)
    ## Make vector of colors for values larger than 0 (180 colors)
    rc2 <- colorRampPalette(colors = c("white","green", "dark green"), space = "Lab")(10)
    ## Combine the two color palettes
    rampcols <- c(rc1, rc2)
    mypal <- colorNumeric(palette = rampcols, domain = scale_range,  na.color = "transparent")
    map4 = reactive({
        leaflet() %>% 
            addProviderTiles(providers$CartoDB.Positron) %>% 
            # addLegend(position = "bottomright", pal = mypal, values = scale_range,
            #           title = ("Change Area m<sup>2")) %>%
            addRasterImage(salt_diff_raster(), colors = mypal, opacity = 0.75)
    })
    output$report_salt_map = renderLeaflet(map4())
    
    
    
    
    #### Habitat line graphs for report
    # Plot estuary by habitat over time    
    pos_plot_dat <- reactive({
        dat1 %>%
            dplyr::filter(Estuary == input$estuary4) %>%
            dplyr::filter(Habitat == "Posidonia") %>%
            dplyr::group_by(Map_Year) %>%
            dplyr::summarise(Area = sum(Area)) %>%
            dplyr::filter(Area > 0)
        
    })
    zos_plot_dat <- reactive({
        dat1 %>%
            dplyr::filter(Estuary == input$estuary4) %>%
            dplyr::filter(Habitat == "Zostera") %>%
            dplyr::group_by(Map_Year) %>%
            dplyr::summarise(Area = sum(Area)) %>%
            dplyr::filter(Area > 0)

    })
    man_plot_dat <- reactive({
        dat1 %>%
            dplyr::filter(Estuary == input$estuary4) %>%
            dplyr::filter(Habitat == "Mangrove") %>%
            dplyr::group_by(Map_Year) %>%
            dplyr::summarise(Area = sum(Area)) %>%
            dplyr::filter(Area > 0)

    })
    sal_plot_dat <- reactive({
        dat1 %>%
            dplyr::filter(Estuary == input$estuary4) %>%
            dplyr::filter(Habitat == "Saltmarsh") %>%
            dplyr::group_by(Map_Year) %>%
            dplyr::summarise(Area = sum(Area)) %>%
            dplyr::filter(Area > 0)

    })
    
    output$pos_plot <- renderPlot({
        ggplot(data=pos_plot_dat(), aes(x=Map_Year, y=Area/10000)) +
            geom_point(colour="#053f5c", size=5) +
            geom_line(colour="#053f5c", size=2) +
            #geom_col(fill="#053f5c") + # change the x = fMap_Year
            ylim(0,NA) +
            ylab("Hectares") +
            xlab("Years Mapped") +
            scale_x_continuous(breaks=seq(1985, 2019, 3)) +
            theme_classic() +
            theme(axis.title.x = element_text(face="bold", size=14),
                  axis.text.x  = element_text(size=14, angle=90, vjust=0.5),
                  axis.title.y = element_text(face="bold", size=14),
                  axis.text.y  = element_text(size=14),
                  strip.text.y = element_text(size = 12)) 
    },height = 400, width = 400)
    
    output$zos_plot <- renderPlot({
        ggplot(data=zos_plot_dat(), aes(x=Map_Year, y=Area/10000)) +
            geom_point(colour="#053f5c", size=5) +
            geom_line(colour="#053f5c", size=2) +
            #geom_col(fill="#053f5c") + # change the x = fMap_Year
            ylim(0,NA) +
            ylab("Hectares") +
            xlab("Years Mapped") +
            scale_x_continuous(breaks=seq(1985, 2019, 3)) +
            theme_classic() +
            theme(axis.title.x = element_text(face="bold", size=14),
                  axis.text.x  = element_text(size=14, angle=90, vjust=0.5),
                  axis.title.y = element_text(face="bold", size=14),
                  axis.text.y  = element_text(size=14),
                  strip.text.y = element_text(size = 12)) 
    },height = 400, width = 400)
    
    output$man_plot <- renderPlot({
        ggplot(data=man_plot_dat(), aes(x=Map_Year, y=Area/10000)) +
            geom_point(colour="#053f5c", size=5) +
            geom_line(colour="#053f5c", size=2) +
            #geom_col(fill="#053f5c") + # change the x = fMap_Year
            ylim(0,NA) +
            ylab("Hectares") +
            xlab("Years Mapped") +
            scale_x_continuous(breaks=seq(1985, 2019, 3)) +
            theme_classic() +
            theme(axis.title.x = element_text(face="bold", size=14),
                  axis.text.x  = element_text(size=14, angle=90, vjust=0.5),
                  axis.title.y = element_text(face="bold", size=14),
                  axis.text.y  = element_text(size=14),
                  strip.text.y = element_text(size = 12)) 
    },height = 400, width = 400)
    
    output$sal_plot <- renderPlot({
        ggplot(data=sal_plot_dat(), aes(x=Map_Year, y=Area/10000)) +
            geom_point(colour="#053f5c", size=5) +
            geom_line(colour="#053f5c", size=2) +
            #geom_col(fill="#053f5c") + # change the x = fMap_Year
            ylim(0,NA) +
            ylab("Hectares") +
            xlab("Years Mapped") +
            scale_x_continuous(breaks=seq(1985, 2019, 3)) +
            theme_classic() +
            theme(axis.title.x = element_text(face="bold", size=14),
                  axis.text.x  = element_text(size=14, angle=90, vjust=0.5),
                  axis.title.y = element_text(face="bold", size=14),
                  axis.text.y  = element_text(size=14),
                  strip.text.y = element_text(size = 12)) 
    },height = 400, width = 400)
    
    
    
    
    #### Mangrove relative to saltmarsh plot for report
    
    report_mvs_t2_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary4) %>%
            dplyr::filter(Map_Year == input$report_t2) %>%
            dplyr::select(GRID_ID, Mangrove, Saltmarsh)
        
    })
    
    
    
    report_mvs_t1_dat <- reactive({
        dat %>%
            dplyr::filter(Estuary == input$estuary4) %>%
            dplyr::filter(Map_Year == input$report_t1) %>%
            dplyr::select(GRID_ID, Mangrove, Saltmarsh)
    })
    
    
    
    
    report_mvs_diff_dat <- reactive({
        inner_join(report_mvs_t2_dat(),
                   report_mvs_t1_dat(),
                   by = c("GRID_ID")) %>%
            mutate(delta_mangrove = .[[2]] - .[[4]]) %>%
            mutate(delta_saltmarsh = .[[3]] - .[[5]]) %>%
            filter(delta_saltmarsh != 0) %>%
            filter(delta_mangrove != 0)
    })
    
    output$report_MVS <- renderPlot({
        ggplot(report_mvs_diff_dat()) +
            theme_geometry(report_mvs_diff_dat()$delta_mangrove, report_mvs_diff_dat()$delta_saltmarsh) +
            geom_point(aes(x = delta_mangrove, y = delta_saltmarsh), size = 1, color = "#053f5c") +
            annotate("text", x = -52000, y = 62000, label = "- Mangrove", color = "red", size = 5) +
            annotate("text", x = -52000, y = 52000, label = "+ Saltmarsh", color = "green", size = 5) +
            annotate("text", x = -52000, y = -52000, label = "- Mangrove", color = "red", size = 5) +
            annotate("text", x = -52000, y = -62000, label = "- Saltmarsh", color = "red", size = 5) +
            annotate("text", x = 52000, y = 62000, label = "+ Mangrove", color = "green", size = 5) +
            annotate("text", x = 52000, y = 52000, label = "+ Saltmarsh", color = "green", size = 5) +
            annotate("text", x = 52000, y = -62000, label = "+ Mangrove", color = "green", size = 5) +
            annotate("text", x = 52000, y = -52000, label = "- Saltmarsh", color = "red", size = 5) #+
            # geom_segment(aes(x = 250000, xend = 250000, y= 1, yend= -250000),
            #              arrow=arrow(length=unit(0.2,"cm")), colour = "red") +
            # geom_segment(aes(x = 250000, xend = 250000, y= 1, yend= 250000),
            #              arrow=arrow(length=unit(0.2,"cm")), color = "green") +
            # geom_segment(aes(x = 1, xend = -250000, y= 250000, yend= 250000),
            #              arrow=arrow(length=unit(0.2,"cm")), colour = "red") +
            # geom_segment(aes(x = 1, xend = 250000, y= 250000, yend= 250000),
            #              arrow=arrow(length=unit(0.2,"cm")), color = "green") 
        
    })
    

    
    
    
    
    
}

shinyApp(ui, server)





































