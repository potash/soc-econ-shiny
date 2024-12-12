#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(scales)
library(ggtext)
library(shinyjs)
source("soc.R")
library(shinyBS)

# Define UI for application that draws a histogram
ui <- fluidPage(
    #tags$head(
    #  tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
    #),
    # Application title
    titlePanel("Soil Organic Carbon Economics"),
    withMathJax(),
    useShinyjs(),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        width=5,
        navset_card_tab(
          title=h3("Parameters"),
          nav_panel(
            "Scale",
            numericInput("N",
                         "Fields \\(N\\):",
                         value = par_defaults$N),
            bsTooltip("N", "Number of fields in the project"),
            numericInput("Y",
                         "Years to remeasurement \\(Y\\):",
                         value = par_defaults$Y),
            bsTooltip("Y", "Number of years before remeasuring SOC stocks")
          ),
          nav_panel("SOC",
            numericInput("tau_tilde",
                         tags$div("Average treatment effect \\(\\tilde\\tau\\)", 
                                  HTML("(Mg ha<sup>-1</sup> y<sup>-1</sup>)")),
                         value = par_defaults$tau_tilde),
            bsTooltip("tau_tilde", "Annual SOC change difference between treatment and control, on average across fields in superpopulation"),
            numericInput("sigma_b",
                         tags$div("Between-field standard deviation \\(\\sigma_b\\)", 
                                  HTML("(Mg ha<sup>-1</sup> y<sup>-1</sup>)")),
                         value = par_defaults$sigma_b, min=0),
            bsTooltip("sigma_b", "Standard deviation of annual SOC changes across fields, in both treatment and control"),
            numericInput("sigma_w",
                         tags$div("Within-field standard deviation \\(\\sigma_w\\)", 
                                  HTML("(Mg ha<sup>-1</sup> y<sup>-1</sup>)")),
                         value = par_defaults$sigma_w, min=0),
            bsTooltip("sigma_w", "Standard deviation of annual SOC changes within a field, in both treatment and control"),
            numericInput("sigma_n",
                         tags$div("Short range standard deviation \\(\\sigma_n\\)", 
                                  HTML("(Mg ha<sup>-1</sup>)")),
                         value = par_defaults$sigma_n, min=0),
            bsTooltip("sigma_n", "Standard deviation in SOC stocks around a given a location at a fixed point in time"),
            numericInput("sigma_l",
                         tags$div("Lab standard deviation \\(\\sigma_l\\)", 
                                  HTML("(Mg ha<sup>-1</sup>)")),
                         value = par_defaults$sigma_l, min=0),
            bsTooltip("sigma_l", "Standard deviation of SOC stock of a single sample under (hypothetical) repeated measurements"),
            numericInput("A",
                         "Field area \\(A\\) (ha):",
                         value = par_defaults$A, min=0),
            bsTooltip("A", "Average area of fields in the project in hectares"),
            bsCollapse(id = "socAdvanced",
                       bsCollapsePanel("Advanced", 
                                       numericInput("sigma_tau_tilde",
                                                    tags$div("Average treatment effect uncertainty \\(\\sigma_\\tilde\\tau\\)",
                                                             HTML("(Mg ha<sup>-1</sup> y<sup>-1</sup>)")),
                                                    value = par_defaults$sigma_tau_tilde, min=0),
                                       sliderInput("rho_b",
                                                   "Between-field temporal autocorrelation \\(\\rho_b\\):",
                                                   value = par_defaults$rho_b, min=0, max=1),
                                       sliderInput("rho_w",
                                                   "Within-field temporal autocorrelation \\(\\rho_w\\):",
                                                   value = par_defaults$rho_w, min=0, max=1))
            )
          ),
          nav_panel("Economics",
            numericInput("c_T",
                         tags$div("Treatment cost \\(c_T\\)", 
                                  HTML("($ ha<sup>-1</sup> y<sup>-1</sup>)")),
                         value = par_defaults$c_T),
            bsTooltip("c_T", "Cost of treatment practice per hectare per year, paid to farmers every year independent of SOC accrual"),
            numericInput("c_C",
                         tags$div("Control cost \\(c_C\\)", 
                                  HTML("($ ha<sup>-1</sup> y<sup>-1</sup>)")),
                         value = par_defaults$c_C),
            bsTooltip("c_C", "Cost of control practice per hectare per year"),
            numericInput("P_CO2",
                         tags$div("Carbon price \\(P_{CO_2}\\)", 
                                  HTML("($ per tonne CO<sub>2</sub>)")),
                         value = par_defaults$P_CO2, min=0),
            bsTooltip("P_CO2", "Sale price of carbon credits per tonne of carbon dioxide"),
            numericInput("c_1s",
                         tags$div("Cost per field visit \\(c_{1s}\\)", 
                                  HTML("($)")),
                         value = par_defaults$c_1s, min=0),
            bsTooltip("c_1s", "Cost of visiting a field for sampling"),
            numericInput("c_2s",
                         tags$div("Cost per core \\(c_{2s}\\)", 
                                  HTML("($)")),
                         value = par_defaults$c_2s, min=0),
            bsTooltip("c_2s", "Cost of sampling and processing one core in a visited field"),
            numericInput("c_l",
                         tags$div("Cost per lab analysis \\(c_{l}\\)", 
                                  HTML("($)")),
                         value = 20, min=0),
            bsTooltip("c_l", "Cost of measuring bulk density and SOC concentration"),
            numericInput("z_deduct",
                         tags$div("Uncertainty deduction \\(z_{deduct}\\)"),
                         value = par_defaults$z_deduct, max=0),
            bsTooltip("z_deduct", "Multiplier of standard error that is deducted from carbon credit"),
            sliderInput("r_b",
                         tags$div("Alternative annual return \\(r_b\\)", 
                                  HTML("(%)")),
                         value = par_defaults$r_b, min=0, max=1),
            bsTooltip("r_b", "Alternative annual return on investment used in Sharpe ratio calculation"),
          ),
          nav_panel("Design",
                   selectInput("design",
                               "Sampling strategy",
                               c("Two stage cluster"="cluster",
                                 "Stratified"="stratified",
                                 "Simple random"="simple")),
                   numericInput("p_1C",
                                "Control fields \\(p_{1C}\\) (proportion)",
                                value = design_defaults$p_1C, min=0, max=1),
                   bsTooltip("p_1C", "Proportion of project fields N assigned to control group, all of which are sampled"),
                   numericInput("d_2C",
                                "Control sample density \\(d_{2C}\\) (cores ha\\(^{-1}\\))",
                                value = design_defaults$d_2C, min=0),
                   bsTooltip("d_2C", "Sampling density in control fields"),
                   numericInput("n_3C",
                                "Control composite size \\(n_{3C}\\) (cores)",
                                value = design_defaults$n_3C, min=0),
                   bsTooltip("n_3C", "Number of locations composited into a single lab analysis in control group"),
                   
                   numericInput("p_1T",
                                "Sampled treatment fields \\(p_{1T}\\) (proportion)",
                                value = design_defaults$p_1T, min=0, max=1),
                   bsTooltip("p_1T", "Proportion of project fields N assigned to treatment and sampled"),
                   
                   numericInput("d_2T",
                                "Treatment sample density \\(d_{2T}\\) (cores ha\\(^{-1}\\))",
                                value = design_defaults$d_2T, min=0),
                   bsTooltip("d_2T", "Sampling density in treatment fields"),
                   
                   numericInput("n_3T",
                                "Treatment composite size \\(n_{3T}\\) (cores)",
                                value = design_defaults$n_3T, min=0),
                   bsTooltip("n_3T", "Number of locations composited into a single lab analysis in treatment group"),
                   
                   strong("Equivalent soil mass (ESM) comparison"),
                   checkboxInput("esm", "Measure two depths when resampling", value=TRUE)
          ),         
          nav_panel("Optimization",
                   actionButton("optimizeSharpe", "Maximize Sharpe ratio"),
                   actionButton("optimizeSE", "Minimize standard error (experimental)"),
                   numericInput("budget", "Budget", value="1000000", min=1)
          )
        )),
        # Show a plot of the generated distribution
        mainPanel(
          width=7,
           navset_card_underline(
             title=h3("Results"),
             nav_panel("Instructions",
                       "On the left is the parameters panel. Here you can:",
                       HTML("<ul>"),
                       HTML("<li>",
                       "<h4>1. Select project scale, SOC, and economic parameters</h4>",
                       "You can choose your own parameters or use the defaults",
                       "</li>",
                       "<li>",
                       "<h4>2. Select experimental and sampling design parameters</h4>",
                       "You can choose these parameters, use the defaults, or click 'Optimize' to select these parameters to maximize the Sharpe ratio.",
                       "</li>",
                       "</ul>",
                       "On the right is the results panel where you can:",
                       "<ul>",
                       "<li>",
                       "<h4>3. View results</h4>",
                       "View all project outcomes in a table or certain outcomes as a bar chart plot.",
                       "</li>",
                       "</ul>"),
             ),
             nav_panel("Plot", 
                       plotOutput("econPlot")),
             nav_panel("Table",
                       h4("Costs and earnings"),
                       selectizeInput("unitSelect", "Unit", 
                                   c(r'(Dollars per hectare-year ($ ha<sup>-1</sup> y<sup>-1</sup>))',
                                     "Dollars ($)"#, 
                                     #r'(Dollars per tonne CO<sub>2</sub> ($ t<sup>-1</sup>))'
                                     ),
                                   # for rendering the sub/sup tags in the options
                                   options = list(render = I(
                                '{
                                  item: function(item, escape) {
                                    return "<div>" + item.value + "</div>";
                                  },
                                  option: function(item, escape) {
                                    return "<div>" + item.value + "</div>";
                                  }
                                }'))),
                       tableOutput("cbTable"), 
                       h4("Economic summary"),
                       tableOutput("summaryTable"),
                       h4("Estimation summary"),
                       tableOutput("estTable"),
                       h4("Design summary"),
                       tableOutput("designTable"))
             )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    analytic_soc_function = reactive(switch(input$design,
                                            cluster=analytic_soc_cluster,
                                            simple=analytic_soc_simple,
                                            stratified=analytic_soc_stratified))
      
    outcomes = reactive(do.call(analytic_soc_function(), 
                                copy_input(input)) %>% 
                          add_design_analysis %>%
                          as_tibble %>%
                          convertCB(input$unitSelect))
  
    output$summaryTable = renderTable(pivot_longer(outcomes(), everything()) %>%
                                 filter(name %in% names(summary_outcomes)) %>%
                                 mutate(name=recode(name, !!!summary_outcomes)))
    
    output$designTable = renderTable(pivot_longer(outcomes(), everything()) %>%
                                        filter(name %in% names(design_outcomes)) %>%
                                        mutate(name=recode(name, !!!design_outcomes)))
    
    output$cbTable = renderTable(pivot_longer(outcomes(), everything()) %>%
                                        filter(name %in% names(cb_outcomes)) %>%
                                        mutate(name=recode(name, !!!cb_outcomes)))
    
    output$estTable = renderTable(pivot_longer(outcomes(), everything()) %>%
                                        filter(name %in% names(est_outcomes)) %>%
                                        mutate(name=recode(name, !!!est_outcomes)))
    
    output$econPlot = renderPlot({
      plot_outcomes = lapply(cb_outcomes, function(name) gsub(" \\(Expected\\)", "", name))
      outcomes() %>%
        pivot_longer(c(R_credit, C_deduct, C_measure, C_implement, Profit)) %>%
        mutate(sd=ifelse(name %in% c("R_credit", "Profit"), Profit_sd, NA)) %>%
        mutate(value=ifelse(name %in% c("R_credit", "Profit"), value, -value)) %>%
        mutate(name=recode_factor(name,
                                  R_credit="Carbon credit",
                                  C_deduct="Uncertainty deduction",
                                  C_measure="Measurement cost",
                                  C_implement="Farmer payments",
                                  Profit="Profit")) %>%
        ggplot(aes(name, value, fill=name)) +
        geom_col() +
        scale_x_discrete(labels=scales::label_wrap(10)) +
        scale_y_continuous(labels=scales::label_comma()) +
        geom_errorbar(aes(ymin=value - sd,
                          ymax=value+sd),
                      width=0) +
        ylab(gsub('.*\\((.*)\\)', '\\1', input$unitSelect)) +
        xlab("") +
        theme_bw(base_size = 18) +
        theme(legend.position="none",
              axis.title.y=element_markdown())
    })
    
    observeEvent(input$optimizeSharpe, {
      pars = reactiveValuesToList(input)[names(par_defaults)]
      
      design = input$design
      design_names = get_design_names(design)
      
      o = optim(par=design_defaults[design_names],
                compose(partial(get_and_transform, name="Sharpe", a=-1, b=0), 
                        partial(eval_design, 
                                pars=pars, 
                                design_names=get_design_names(design),
                                soc_function=analytic_soc_function())),
            method="L-BFGS-B",
            lower=get_bounds(pars, "lower", design),
            upper=get_bounds(pars, "upper", design)
            )

      for(i in 1:length(o$par)) {
        updateNumericInput(session, design_names[[i]], value=o$par[[i]])
      }
    })
    
    
    observeEvent(input$optimizeSE, {
      pars = reactiveValuesToList(input)[names(par_defaults)]
      
      design = input$design
      design_names = get_design_names(design)
      soc_function = analytic_soc_function()
      
      lower = get_bounds(pars, "lower", design)
      upper = get_bounds(pars, "upper", design)
      
      args = list(pars=pars, design_names=design_names, soc_function=soc_function)
      
      if(design == "cluster") {
        lower[[1]] = 0
        lower[[4]] = 0
        upper[[1]] = 1
        upper[[4]] = 1
        
        x0_init = unlist(get_design_defaults(pars$N)[design_names])
        x0_init[1] = 0.5
        
        nlo = nloptr::slsqp(x0=unlist(x0_init) %>% scale_props2(n=pars$N),
                            fn=compose(partial(get_and_transform, name="tau_hat_se", a=1, b=0), 
                                       partial(eval_design, !!!args),
                                       partial(scale_props2, n=pars$N)),
                            lower=lower,
                            upper=upper,
                            heq=compose(partial(get_and_transform, name="Cost", a=1, b=-input$budget), 
                                        partial(eval_design, !!!args),
                                        partial(scale_props2, n=pars$N))
        )
        nlo$par = scale_props2(nlo$par, n=pars$N)
      } else {
        x0_init = design_defaults[design_names]
        x0_init[1] = 0.5
        print(unlist(x0_init) %>% scale_props3(N=pars$N, A=pars$A))
        print(lower %>% scale_props3(N=pars$N, A=pars$A))
        print(upper %>% scale_props3(N=pars$N, A=pars$A))
        
        lower = lower %>% scale_props3(N=pars$N, A=pars$A)
        upper = upper %>% scale_props3(N=pars$N, A=pars$A)
        
        lower[3] = 0.04
        upper[3] = 1
        lower[5] = 0.04
        upper[5] = 1
        
        # when c_T == c_C and design is stratified or simple
        nlo = nloptr::slsqp(x0=unlist(x0_init) %>% scale_props3(N=pars$N, A=pars$A),
                            fn=compose(
                              function(x) x^2,
                              partial(get_and_transform, name="tau_hat_se", a=1, b=0), 
                                       partial(eval_design, !!!args),
                                       partial(scale_props3_inv, N=pars$N, A=pars$A) ),
                            lower=lower,
                            upper=upper,
                            heq=compose(partial(get_and_transform, name="Cost", a=1, b=-input$budget), 
                                        partial(eval_design, !!!args),
                                        partial(scale_props3_inv, N=pars$N, A=pars$A)),
                            control=list(xtol_rel=1e-8)
        )
        # nlo = nloptr::slsqp(x0=unlist(design_defaults[design_names])[2:3],
        #                     fn=compose(partial(get_and_transform, name="tau_hat_se", a=1, b=0), 
        #                                   partial(eval_design, !!!args),
        #                                   symmetrize),
        #                     lower=get_bounds(pars, "lower", design)[2:3],
        #                     upper=get_bounds(pars, "upper", design)[2:3],
        #                     heq=compose(partial(get_and_transform, name="Cost", a=1, b=-input$budget), 
        #                                 partial(eval_design, !!!args),
        #                                 symmetrize)
        # )
        nlo$par = scale_props3_inv(nlo$par, N=pars$N, A=pars$A)
      }
      
      print(nlo)
      
      for(i in 1:length(nlo$par)) {
        updateNumericInput(session, design_names[[i]], value=nlo$par[[i]])
      }
    })
    
    # p_1T is only relevant to two stage cluster sampling design
    observeEvent(input$design, {
        toggleState("p_1T", input$design == "cluster")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
