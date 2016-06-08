
# Author: David Kuchelmeister
#############################


function(input, output) {
  
  #### Load Data / general Parameters ############################################################.
  {
    # The following code is used to load the data from different sources into the R environement
    Discount.Yield.Curve <- reactive({ read.xlsx(file = "Data/Case study data.xlsx",sheetIndex = 1,header = T) })
    Mortality.Rates <- reactive({ read.xlsx(file = "Data/Case study data.xlsx",sheetIndex = 2,header = T) })
    Policy.Records <- reactive({ read.xlsx(file = "Data/Case study data.xlsx",sheetIndex = 3,header = T) })
    
    
    # The Information about the policy chosen policy is implemented
    Chosen.Policy.Records <- reactive({ Policy.Records()[input$Policy.number,] })
    Policyholder.Age <- reactive({ Chosen.Policy.Records()$Policyholder.Age-1+projected.years })
  }
  
  
  #### Calculations of Level Premium #############################################################.
  {
    # Following are all the used functions to calculate the level Premium
    # 
    # reading out the mortality rate of the current policy
    mortality.rate <- reactive({ subset(Mortality.Rates()$Rate, match(Mortality.Rates()$Age,Policyholder.Age(),nomatch = 0) != 0 ) })
    
    # calculating percentage of alive policy holders over time
    policyholder.alive.end.year <- reactive({ cumprod(1-mortality.rate()) })
    policyholder.alive.start.year <- reactive({ c(1,head(policyholder.alive.end.year(),-1)) })
    
    # Claims Expected To Be Paid Out to Policyholders (End of year timing)
    expected.claims <- reactive({ mortality.rate() * Chosen.Policy.Records()$Sum.Assured.CHF * policyholder.alive.start.year() })
    
    # Premium Expected to be Received (Start of year timing)
    expected.premium <- reactive({ policyholder.alive.start.year() * level.premium })
    
    # Expenses Expected (End of year timing)
    expected.expenses <- reactive({ Fixed.Expense * policyholder.alive.end.year() })
    
    # Present Value of Income
    Income <- reactive({
      Income <- 0
      for(i in rev(projected.years)) Income <- c(Income, tail(Income,1)/(1+Discount.Yield.Curve()$Rate[i]) + expected.premium()[i])
      Income <- data.frame(rev(Income[-1])); colnames(Income) <- "Present Value of Income"
      Income
    })
    
    # Present Value of Outgo
    Outgo <- reactive({
      Outgo <- 0
      for(i in rev(projected.years)) Outgo <- c(Outgo,(expected.claims()[i] + expected.expenses()[i]
                                                       + tail(Outgo,1)) / (1+Discount.Yield.Curve()$Rate[i]))
      Outgo <- data.frame(rev(Outgo[-1])); colnames(Outgo) <- "Present Value of Outgo"
      Outgo
    })
    
    # optimisation of the level premium
    level.premium.optimised <- reactive({
      level.premium.list <-  foreach(i = Policy.Records()$Policy.Number) %dopar% optimise(f = level.of.premium.compiler,c(0,100),tol = 0.01, i)$minimum
      level.premium.optimised <- Reduce(c,level.premium.list)
      level.premium.optimised
    })
    
  }
  #### Build data table ##########################################################################.
  {
    # Binding the Information into one data.frame
    
    
    # Projected Year
    data.table.projected.year <- reactive({
      data.table.projected.year <- data.frame(c(0,projected.years))
      colnames(data.table.projected.year) <- "projected year"
      data.table.projected.year
    })
    
    # Bind all the calculated values (ecxept Income and Outgo)
    data.table.total <- reactive({
      data.table.total <- data.frame(Year.Interval, Policyholder.Age(), mortality.rate(),policyholder.alive.start.year(),policyholder.alive.end.year(),
                                     expected.claims(), expected.premium(), expected.expenses(), Discount.Yield.Curve()[,2])
      colnames(data.table.total) <- c("Year Interval","Policyholder Age","Mortality rate","Policyholders alive at start",
                                      "Policyholders alive at end","Expected claims", "Expected premium", "Expected expenses", "Discount rate")
      data.table.total
    })
    
    # Bind the above values and add projected year, Income and Outgo. Which all start one year earlier than the others
    data.table.Income.Outgo <- reactive({
      data.table.Income.Outgo <- data.frame(Income(), Outgo())
      colnames(data.table.Income.Outgo) <- c("Income","Outgo")
      data.table.Income.Outgo
    })
    
    # Bind all the seperate dataframes to one full data frame
    data.table <- reactive({
      data.frame(data.table.projected.year(), rbind(rep(NA,ncol(data.table.total())),data.table.total()),
                 rbind(data.table.Income.Outgo(), c(NA,NA)))
    })
  }
  
  
  #### Shiny Application User Interface ##########################################################.
  {
    
    ##### Buttons ##########################################################.
    {
      output$policy.number.choice <- renderUI({
        radioButtons(inputId = "Policy.number"
                     ,label = "Choose a policy number"
                     ,choices = as.character(Policy.Records()$Policy.Number)
                     ,inline = T
                     ,width = 500
        )
      })
      
    }
    
    ##### Input Data Tables ################################################.
    {
      # Filter data based on selections
      output$Policy.Records.table <- DT::renderDataTable(DT::datatable({
        Policy.Records()
      }))
      output$Mortality.Rates.table <- DT::renderDataTable(DT::datatable({
        Mortality.Rates()
      }))
      output$Discount.Yield.Curve.table <- DT::renderDataTable(DT::datatable({
        Discount.Yield.Curve()
      }))
    }
    
    
    output$data.table <- renderTable({
      data.table()
    })
    
    output$levelpremium.age.plot <- renderPlot({
      plot(Policy.Records()[,2],level.premium.optimised(), xlab = "Age", ylab = "Level of premium", xlim = c(0,70), ylim = c(0,130),
           main = "Comparison of the age against the Level of premium")
      
      if(input$analysis == "Spline"){
        for(i in 8:12) lines(smooth.spline(x=Policy.Records()[,2],y=level.premium.optimised(),spar = 0.65),col=2)
        lines(predict(smooth.spline(x=Policy.Records()[,2],y=level.premium.optimised(),spar = 0.65),1:20))
        lines(predict(smooth.spline(x=Policy.Records()[,2],y=level.premium.optimised(),spar = 0.65),60:70))
      }else{
        lm.erg <- lm(level.premium.optimised() ~ Policy.Records()[,2])
        summary(lm.erg)
        abline(lm.erg, col="red4")
      }
    })
    # clicked points
    output$plot.brush.points <- renderPrint({
      policy.output <- subset(Policy.Records(), Policy.Records()[,2] > input$plot_brush$xmin &  Policy.Records()[,2] < input$plot_brush$xmax)
      
      premium.ouput <- subset(level.premium.optimised(), Policy.Records()[,2] > input$plot_brush$xmin &
                                Policy.Records()[,2] < input$plot_brush$xmax)
      # premium.ouput <- subset(level.premium.optimised, Policy.Records[,2] > 30 &
      #                           Policy.Records[,2] < 60)
      # Policy.Records[premium.ouput > 40 &  premium.ouput < 100,]
      policy.output[premium.ouput > input$plot_brush$ymin &  premium.ouput < input$plot_brush$ymax,]
      policy.output
    })
    
  }
  
  
} # end function(input, output)









