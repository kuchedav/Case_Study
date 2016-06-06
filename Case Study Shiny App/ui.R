
# Author: David Kuchelmeister
#############################


library(shinydashboard)

dashboardPage(
  dashboardHeader(
    title="Risk Engineering"),
  dashboardSidebar(
    sidebarMenu(
      # icons unter: http://fontawesome.io/icons/
      menuItem("Rpackages", tabName = "Input_data", icon = icon("coffee")),
      menuItem("Rpackages", tabName = "Case_Study", icon = icon("fa fa-paper-plane-o"))
    )
  ),
  
  
  
  dashboardBody(
    
    tabItems(
      # Second tab content
      tabItem(tabName = "Input_data",
              fluidRow(
                box(title = "Policy Records",
                    DT::dataTableOutput("Policy.Records.table")
                ),
                box(title = "Discount Yield Curve",
                    DT::dataTableOutput("Mortality.Rates.table")
                    ,width = 3),
                box(title = "Mortality Rates",
                    DT::dataTableOutput("Discount.Yield.Curve.table")
                    ,width = 3)
              )
      ), # end of Skew_kurt tab
      
      
      
      
      # Second tab content
      tabItem(tabName = "Case_Study",
              h1(paste0("Policy number:")),
              fluidRow(
                box(width=12, title="Level premium",
                    plotOutput("levelpremium.age.plot", height = 500, width = 800,
                               # Equivalent to: click = clickOpts(id = "plot_click")
                               click = "plot_click",
                               dblclick = dblclickOpts(
                                 id = "plot_dblclick"
                               ),
                               brush = brushOpts(
                                 id = "plot_brush"
                               )
                    ),
                    verbatimTextOutput("plot.brush.points")
                ),
                box(width = 12, title="Calculations", collapsed = T,collapsible = T,
                    uiOutput("policy.number.choice"),
                    tableOutput("data.table")
                )
              )
      ) # end of Case_Study
      
    ) # end tabItems 
  ) # end dashboardBody
)









