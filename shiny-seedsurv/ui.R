library(shiny)

# Define UI
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Oak Seedling Analysis"),
  
  #Variables with user input in sidebar panel
  sidebarPanel(
    sliderInput("canopy", "Canopy Cover %:",
                min = 0, max=95,value=45, step=1),
    sliderInput("distance", "Distance Z score:",
                min = -2, max=2,value=0, step=0.1),
    radioButtons('aspect', 'Aspect:',
                 c('Northeast'=0,
                   'Southwest'=1)),
    sliderInput("start.height", "Starting height Z score):",
                min = -2, max=2,value=0, step=0.1),
    radioButtons('browse', 'Browsed in previous time step:',
                 c('No'=0,
                   'Yes'=1)),
    radioButtons('season', 'Season:',
                 c('May-October'=1,
                   'November-April'=0)),
    sliderInput("comp", "Interspecific Competition PCA Score:",
                min = -2, max=7,value=0, step=0.1),
    sliderInput("herb", "Interspecific Herbaceous Vegetation PCA Score:",
                min = -2, max=3,value=0, step=0.1),
    sliderInput("elapsed", "Number of Days since Planting:",
                min = 0, max=1047,value=483, step=1),
    radioButtons('sprout', 'Is plant a sprout?',
                 c('No'=0,
                   'Yes'=1))
  ),
  
  #Define panel layout
  mainPanel(
    tabsetPanel(
      tabPanel("Seedling Survival",plotOutput("plot"),plotOutput("fullplot")),
      tabPanel("Seedling Growth",plotOutput("growthplot"))
  ))))




