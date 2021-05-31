library(shiny)
library(tidyverse)
library(reshape2)

server <- function(input, output) {
  set.seed(31)
  CrCl <- reactive({
    (((140 - input$age) * input$weight * as.numeric(input$sex)) / (72 * input$CsCr))
  })
  
  
  # Produces pharmacokinetic parameters and calculates concentration values for one individual
  #
  # Returns a list containing the parameters vector ("Parameters") and concentration values 
  # vector ("Cp")
  cpcalc <- function() {
    params = rnorm(3, c(10.73, 14.32, 0), c(1.055, 1.604, 0.165))
    V = params[1]
    Q = params[2]
    CL = exp(params[3])*13.583*CrCl()/100
    kel = CL / V
    k12 = Q / V
    k21 = Q / 5.5
    ci <- (input$dose/V)
    a = 0.5*(k12 + k21 + kel + sqrt((k12 + k21 + kel)^2 - 4*k21*kel))
    b = 0.5*(k12 + k21 + kel - sqrt((k12 + k21 + kel)^2 - 4*k21*kel))
    parameters <- c(V,Q,CL,kel,k12,k21,a,b)
    cp <- c(ci)
    for (i in 1:90)
    {
      cp <- append(cp, (((k21 - a)*input$dose)/(V*(b - a)))*exp(-(a*(i/10))) + (((k21 - b)*input$dose)/(V*(a - b)))*exp(-(b*(i/10))))
    }
    pass_to_cdf <- list("Parameters" = parameters, "Cp" = cp)
    return(pass_to_cdf)
  }
  

  # Function calls cpcalc() repeatedly
  #
  # Generates a dataframe of concentration values for all simulated individuals 
  #
  # Generates a dataframe of pharmacokinetic parameters for all simulated individuals
  #
  # Generates a vector containing subject numbers
  #
  # Returns a list containing the concentration values ("plotdata"), the individual parameters ("individuals"),
  # and the subject numbers ("sub")
  createdataframe <- function() {
    t <- c(0)
    for (i in 1:90)
    {
      t <- append(t, i/10)
    }
    cpcols <- c()
    ind_info <- c()
    sub <- c()
    for (i in 1:input$sim)
    {
      cpcalc()
      cpcols <- cbind(cpcols, cpcalc()$Cp)
      ind_info <- rbind(ind_info, cpcalc()$Parameters)
      sub <- append(sub, format(round(i, 0)))
    }
    df <- data.frame(Time = t, 
                     Cp = cpcols)
    df <- melt(df, "Time")
    datalist <- list("plotdata" = df, "merged_parameters" = ind_info, "sub" = sub)
    return(datalist)
  }
  
  
  # Renders the concentration plot for the UI 
  output$Cp <- renderPlot({
    finaldata = createdataframe()$plotdata
    colorvector <- c()
    for (i in 1:input$sim)
    {
      colorvector <- c(colorvector, "darkblue")
    }
    plot <- ggplot(data = finaldata, aes(x = Time, y = value, color = variable)) + 
      geom_line() +scale_color_manual(values=c(colorvector)) + stat_summary(fun = "mean", colour = "orange", size = 1, geom = "line") +
      labs(x = "Time (hours)", y = "Cp (mg/mL)", title = "Central compartment concentration of cephalexin after intravenous (IV) injection", subtitle = paste("CrCl = ", round(CrCl(), 0),"mL/min", sep = ""), caption = paste("N = ", input$sim, sep = "")) + 
      theme(legend.position="none") + theme(text=element_text(size=12, face = "bold", family = "serif")) + scale_x_continuous(expand = c(0, 0), limits = c(0,NA)) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      theme(panel.background = element_rect(fill = "beige"),
            plot.background = element_rect(fill = "lightblue3"),
            panel.grid.major = element_line(color = "lightblue3"),
            panel.grid = element_blank()
      ) 
    return(plot)
  })
  
  
  # Renders the table of pharmacokinetic parameters for all simulated individuals
  output$table <- renderTable({
    tab <- createdataframe()$merged_parameters
    tab <- cbind(createdataframe()$sub, tab)
    colnames(tab) <- c("Subject","V","Q","CL","kel","k12","k21","a","b")
    return(tab)
  })
  
  
  # MathJax equations and text for the "Model information" tab 
  output$model_info <- renderUI({
    withMathJax(
      helpText('$$C_p=\\frac{(k_{21}-\\alpha)D}{V(\\beta-\\alpha)}e^{-\\alpha t}\\,+\\frac{(k_{21}-\\beta)D}{V(\\alpha-\\beta)}e^{-\\beta t}$$'),
      helpText('$$\\alpha=\\frac{1}{2}[k_{12}+k_{21}+k_{el}]+\\sqrt{(k_{12}+k_{21}+k_{el})^2 -4k_{21}k_{el}}$$'),
      helpText('$$\\beta=\\frac{1}{2}[k_{12}+k_{21}+k_{el}]-\\sqrt{(k_{12}+k_{21}+k_{el})^2 -4k_{21}k_{el}}$$'),
      helpText('\\(C_p\\): central compartment concentration at time t'),
      helpText('\\(V\\): volume of distribution of the central compartment'),
      helpText('\\(D\\): administered dose'),
      helpText('\\(CL\\): drug clearance (function of creatinine clearance as calculated by the Cockcroft-Gault equation)'),
      helpText('\\(k_{el} (CL/V),\\,k_{12} (Q/V),\\,k_{21}\\): rate constants for flow between compartments'),
      helpText('_______________________________________________________________________________________'),
      helpText(strong('Reference for model:')),
      helpText("Greene, D. S., Flanagan, D. R., Quintiliani, R., & Nightingale, C. H. (1976). Pharmacokinetics of cephalexin: An evaluation of one- and two-compartment model pharmacokinetics. The Journal of Clinical Pharmacology, 16(5-6), 257-264. doi:10.1002/j.1552-4604.1976.tb02402.x")
    )
  })
}
