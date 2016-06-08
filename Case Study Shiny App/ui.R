
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
      # menuItem("Rpackages", tabName = "optimisation", icon = icon("fa fa-smile-o")),
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
      
      
      # tabItem(tabName = "optimisation",
      #         column(8,h1("Optimisation"),
      #                "To be able to optimise, we need to know how we should optimise.
      #         It would be possible to calculate the optimas with a equation-system,
      #         but R can not calculate analytically. Therefore,
      #         normal optimisations are probably the best choice.
      #         We have some different possibilities for optimisation codes and try to find the fastest possible"),
      #         
      #         box(width = 6,
      #             h1("Optimisation methods"),
      #             "To find the fastest optimisation, we need to
      #               calculate all the possibilites we have and compare them.
      #             The apply-loop is added to calculate the optimas multiple times
      #              this may take a while but the time predictino is more accurate
      #              This apply-loop is not built to be specifically fast, because it will be run only once.",
      #             br(),br(),br(),
      #             "The controlled methods are:",
      #             h3("Nelder-Mead",",  ", "BFGS",",  ", "CG",",  ", "L-BFGS-B",",  ", "SANN",",  ", "Brent"),
      #             "all of the methods are implemented in the optim() function",
      #             h4("optim(par= 40, fn= level.of.premium, Policy.Number= i, method= input$mathod, lower= 0, upper= 200)$par")
      #             
      #         )
      # ),
      
      
      # Second tab content
      tabItem(tabName = "Case_Study",
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
                    radioButtons("analysis", "which fit should be chosen?", c("Regression", "Spline")),
                    verbatimTextOutput("plot.brush.points")
                ),
                box(width = 12, title="Policy table",collapsible = T,
                    uiOutput("policy.number.choice"),
                    tableOutput("data.table")
                )
              )
      ) # end of Case_Study
      
    ) # end tabItems 
  ) # end dashboardBody
)









