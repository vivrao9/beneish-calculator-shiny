##################################
# Biodiversity in National Parks #
# by Alessio Benedetti           #
# ui.R file                      #
##################################

library(shinydashboard)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(Rcpp)
library(dashboardthemes)
library(rjson)
library(knitr)
rmdfiles = c("www/ProjectMarkDown.rmd")
rmdfiles = sapply(rmdfiles, knit, quiet = T)

###########
# LOAD UI #
###########
px=readxl::read_excel("px.xlsx", col_names = TRUE)
comp = px$name


shinyUI(fluidPage(
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  tags$head(
    tags$style(
      HTML('
           <link rel="preconnect" href="https://fonts.googleapis.com">
           <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
           <link href="https://fonts.googleapis.com/css2?family=Source+Sans+Pro:ital,wght@0,200;0,300;0,400;0,600;0,700;0,900;1,200;1,300;1,400;1,600;1,700&display=swap" rel="stylesheet">
           '),
  
            # remove shiny "red" warning messages on GUI
            type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
  )),
  
  # load page layout
  dashboardPage(
    
    # skin = "blue",
    
    dashboardHeader(title="Beneish m-score calculator", titleWidth = 300),
    
    dashboardSidebar(width = 300,
                     sidebarMenu(
                       HTML(paste0(
                         "<br>",
                         "<a href='https://www.purdue.edu' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='https://upload.wikimedia.org/wikipedia/commons/thumb/3/35/Purdue_Boilermakers_logo.svg/1200px-Purdue_Boilermakers_logo.svg.png' width = '186'></a>",
                         "<br>"
                       )),
                       selectInput("company", "Select a company", comp),
                       menuItem("About the model", tabName = "home", icon = icon("home")),
                       menuItem("Explore", tabName = "eda", icon = icon("bar-chart")),
                       menuItem("Prediction", tabName = "pred", icon = icon("table"))
                     )
                     
    ), # end dashboardSidebar
    
    dashboardBody(
      
      ### changing theme
      shinyDashboardThemes(
        theme = "blue_gradient"
      ),
      
      tabItems(
        
        tabItem(tabName = "home",
                
              fluidRow(
                column(width=2),
                column(width=8,
                       
                # home section
                withMathJax(includeMarkdown(rmdfiles)),
                column(width = 2)
        )),
        fluidRow(column(width = 2),
          HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/gxviBoaYoUQ" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
          )
        ),
        
        tabItem(tabName = "eda",
                fluidRow(
                  column(width = 6,
                         plotOutput("stock_price")),
                  column(width = 6,
                         plotOutput("gross_profit")),
                  column(width = 6,
                         plotOutput("total_revenue")),
                  column(width = 6,
                         plotOutput("net_income"))
                )
                
        ),
        
        tabItem(# prediction section where we display probability of earnings
          # manipulation and discuss other descriptive statistics
          tabName = "pred",
          # textOutput("mscore1") %>% tagAppendAttributes(class = 'pred_prob'),
          
          valueBoxOutput("mscore1"),
          valueBoxOutput("prob"),
          valueBoxOutput("dsri"),
          valueBoxOutput("gmi"),
          valueBoxOutput("aqi"),
          valueBoxOutput("sgi"),
          valueBoxOutput("depi"),
          valueBoxOutput("sgai"),
          valueBoxOutput("lvgi"),
          
          selectInput("sector", "Select a sector:", px$sector),
          plotOutput("ben_hist"),
          DT::dataTableOutput("mytable")
          
        )
        
      ) # end dashboardBody
      
    )# end dashboardPage
    
  )))