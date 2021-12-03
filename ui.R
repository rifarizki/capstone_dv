header <- dashboardHeader(title = "Suicide Rates")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Global Overview",
             tabName = "overview",
             icon = icon("globe")),
    menuItem(text = "Country Performance",
             tabName = "countries",
             icon = icon("flag")),
    menuItem(text = "Dataset",
             tabName = "dataset",
             icon = icon("table"))
  )
)

body <- dashboardBody(
  shinyDashboardThemes(theme = "flat_red"),
  
  tabItems(
    
  # PAGE 1
    
  tabItem(
    tabName = "overview",
    fluidPage(
      h2(tags$b("Global Suicide Rates from 1995-2015"))),
    
    fluidPage(
      tabBox(width = 12,
             title = tags$b("Suicide number and ratio by country"),
             id = "tabset1", side = "right",
             tabPanel(tags$b("Ratio"),
                      plotlyOutput("plot2")),
             tabPanel(tags$b("Suicide number"),
                      plotlyOutput("plot1")))),
    
    fluidPage(box(width = 6, plotlyOutput("plot3")),
              box(width = 6, plotlyOutput("plot_gen")))),
  
  # PAGE 2
  
    tabItem(
    tabName = "countries",
    
    fluidPage(
      h3(tags$b("Country Performance")),
      h4("Population and trends of suicide rates in each country (1995-2015)")),
    
    fluidPage(box(width = 8, plotlyOutput("plot4")),
              box(width = 4,
                  height = 420,
                  solidHeader = F,
                  #background = "black",
                  selectInput(
                    inputId = "country_perf1",
                    label = h5(tags$b("Select country:")),
                    choices = unique(decade$country)),
                  radioButtons(
                    inputId = "multiple_choices",
                    label = h5(tags$b("Select numerical variables:")),
                    choices = colnames(single_country[c(3,4)]))
                  )),
    fluidPage(
      h4("Suicide ratio in different age-group and sexes")
    ),
    
    fluidPage(box(width = 8, plotlyOutput("plot5")),
              box(width = 4,
                  height = 420,
                  solidHeader = F,
                  #background = "black",
                  selectInput(
                    inputId = "country_perf2",
                    label = h5(tags$b("Select country:")),
                    choices = unique(decade$country)),
                  selectInput(
                    inputId = "country_perf3",
                    label = h5(tags$b("Select year:")),
                    choices = unique(decade$year)))
              )
    ),
  
  # PAGE 3
  
  tabItem(
    tabName = "dataset",
    
    fluidPage(
      h3(tags$b("Suicide Ratio from 1995-2015"))),
    
    fluidPage(box(width = 12, plotlyOutput("plot_maps"))),
    
    fluidPage(box(width = 12, dataTableOutput(outputId = "data")))
    
    )
  )
)

dashboardPage(
  skin = "black",
  header = header,
  body = body,
  sidebar = sidebar
)