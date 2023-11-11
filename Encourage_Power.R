suppressWarnings(suppressPackageStartupMessages(library('shiny')))
suppressWarnings(suppressPackageStartupMessages(library('ggplot2')))
suppressWarnings(suppressPackageStartupMessages(library('ggtext')))
suppressWarnings(suppressPackageStartupMessages(library('shinydashboard')))
suppressWarnings(suppressPackageStartupMessages(library('kableExtra')))
suppressWarnings(suppressPackageStartupMessages(library('scales')))

#
# DEFINE THE UI
#

ui <- shinyUI(
  dashboardPage(skin='green',
    dashboardHeader(titleWidth = 329, title=HTML("<em><b><font size='3'>Encourage_Power</font></b></em>")),
    dashboardSidebar(width=350,
      tags$style(HTML(".sidebar { height: 90vh; overflow-y: auto; }" )),
      tags$style(HTML(type='text/css', ".irs-min, .irs-max, .irs-grid-text, .irs-grid-pol { visibility: hidden !important;}")),
      sidebarMenu(

        fluidPage(
          tags$head(tags$style(HTML(".textInput {margin-left:5px}"))),    # This adds te left margin
        helpText(
          div(strong(HTML("</p>This dashboard conducts power calculations </br> for random encouragement
                          designs (see </br> Schochet, Journal of Educational and </br> Behavioral Statistics, 2024)
                          </p> After entering the inputs below, you can press </br> \"Submit\" at the bottom of the dashboard </br> to view
                          and save the results"))),
            style = "color: #27CF82;" )),  #27CF82;CAFF70

        fluidPage(
          tags$head(tags$style(HTML(".textInput {margin-left:5px}"))),
        helpText(
          div(strong(HTML("Key design information"))),
          style = "color:#33C5FF"
        )),

        # CREATE THE SIDEBAR PANEL INPUTS

        radioButtons(
          inputId = "iv_itt",
          label   = "Treatment effect estimand",
          choices = c(
            "Participation effect on long-term outcome (Complier average causal effect [CACE] analysis)",
            "Encouragement effect on participation (First-stage intent-to-treat [ITT] analysis)"),
          inline = FALSE
        ),

        radioButtons(
          inputId = "rct_design",
          label   = "Design type",
          choices = c(
            "Nonclustered design",
            "Clustered design",
            "Random block design"),
          inline = FALSE
        ),

        radioButtons(
          inputId = "samp_size",
          label   = "Type of power analysis",
          choices = c(
            "Calculate the sample size required to achieve a target minimum detectable effect size (MDE) for the CACE analysis or treatment-control difference for the ITT participation analysis",
            "Calculate the MDE for a given sample size (for the CACE analysis only)"),
          inline = FALSE
        ),

        conditionalPanel(
          'input.samp_size == "Calculate the sample size required to achieve a target minimum detectable effect size (MDE) for the CACE analysis or treatment-control difference for the ITT participation analysis"
          & input.iv_itt == "Participation effect on long-term outcome (Complier average causal effect [CACE] analysis)"',
          numericInput(
            inputId = "mde",
            label = "Target MDE value for the CACE analysis",
            value = .20,
            min = .01,
            max = 5,
            step = .01
          )
        ),

        radioButtons(
          inputId = "power_form",
          label   = "Power formula in the Schochet (2024) article for the CACE analysis",
          choices = c(
            "Eq. (12): Outcome and participation rate variances",
            "Eq. (11): Outcome variance only",
            "Eq. (13): Upper bound variance"),
          inline = FALSE
        ),

        fluidPage(
          tags$head(tags$style(HTML(".textInput {margin-left:5px}"))),
          helpText(
            div(strong(HTML("Sample size information"))),
            style = "color:#33C5FF"
          )),

        conditionalPanel(
          'input.samp_size == "Calculate the MDE for a given sample size (for the CACE analysis only)"
          & input.rct_design != "Nonclustered design"',
          numericInput(
            inputId = "mclus",
            label = "Total number of clusters or blocks",
            min = 3,
            step = 1,
            value = 20
          )
        ),
        
        conditionalPanel(
          'input.rct_design != "Nonclustered design"',
          numericInput(
            inputId = "nclus",
            label = "Number of individuals per cluster or block",
            min = 1,
            step = 1,
            value = 50
          )
        ),

        conditionalPanel(
          'input.samp_size == "Calculate the MDE for a given sample size (for the CACE analysis only)"
          & input.rct_design == "Nonclustered design"',
          numericInput(
            inputId = "ntot",
            label = "Total number of individuals",
            min = 3,
            step = 1,
            value = 200
          )
        ),

        numericInput(
          inputId = "pt",
          label = "Assignment rate to the treatment group (0 to 1)",
          min = .01,
          max = .99,
          step = .01,
          value = .50
        ),

        fluidPage(
          tags$head(tags$style(HTML(".textInput {margin-left:5px}"))),
          helpText(
            div(strong(HTML("Intraclass correlation coefficients (ICCs) </br> for clustered
                            and random block designs"))),
            style = "color:#33C5FF"
          )),

        conditionalPanel(
          'input.rct_design == "Clustered design"',
          numericInput(
            inputId = "icc",
            label = "ICC of mean outcomes between clusters (0 to 1)",
            min = 0,
            max = 1,
            step = .01,
            value = .05
          )
        ),

        conditionalPanel(
          'input.rct_design == "Random block design"',
          numericInput(
            inputId = "icc_imp",
            label = "ICC of treatment effects between blocks (0 to 1)",
            min = 0,
            max = 1,
            step = .01,
            value = .02
          )
        ),

        fluidPage(
          tags$head(tags$style(HTML(".textInput {margin-left:5px}"))),
          helpText(
            div(strong(HTML("Expected participation rates in the </br> program or activity"))),  # </br>
            style = "color:#33C5FF"
          )),

        numericInput(
          inputId = "mu1",
          label = "Treatment group participation rate (0 to 1)",
          min = .01,
          max = 1,
          step = .01,
          value = .70
        ),

        numericInput(
          inputId = "mu0",
          label = "Control group participation rate (0 to 1)",
          min = 0,
          max = .99,
          step = .01,
          value = .50
        ),

        fluidPage(
          tags$head(tags$style(HTML(".textInput {margin-left:5px}"))),
          helpText(
            div(strong(HTML("Precision gains from model covariates"))),
            style = "color:#33C5FF"
          )),

        numericInput(
          inputId = "r2yx",
          label = "Regression R2 value from model covariates (0 to 1)",
          min = 0,
          max = .99,
          step = .01,
          value = 0
        ),

        conditionalPanel(
          'input.r2yx>0',
          numericInput(
            inputId = "nx",
            label = "Number of model covariates",
            value = 1,
            min = 1,
            step = 1
          )
        ),

        fluidPage(
          tags$head(tags$style(HTML(".textInput {margin-left:5px}"))),
          helpText(
            div(strong(HTML("Hypothesis testing parameters"))),
            style = "color:#33C5FF"
          )),

        numericInput(
          inputId = "alpha",
          label = "Significance (alpha) level (0 to 1)",
          min = 0,
          max = .99,
          step = .01,
          value = 0.05
        ),

        radioButtons(
          inputId = "two_tailed",
          label   = "Two- or one-tailed test",
          choices = c(
            "Two-tailed",
            "One-tailed"),
          inline = TRUE
        ),

        sliderInput(
          inputId = "power",
          label = "Power level range: Calculations are conducted between min and max values",
          min = .50,
          max = .99,
          #step = 0.5,
          value = c(.60,.90)
        ),

        radioButtons(
          inputId = "power_by",
          label   = "Power level interval",
          choices = c(
            ".01",
            ".05",
            ".10"),
          selected = ".05",
          inline = TRUE
        ),

        div(actionButton("submit_button", "Submit"), class="form-group shiny-input-container")
       )
    ),

    # DEFINE THE MAIN PANEL ELEMENTS. A SplitLayout IS USED SO THAT THERE ARE
    # TWO PANELS FOR PRINTING THE TABLES AND GRAPHS

    dashboardBody(
     splitLayout(
       htmlOutput("tableset"),     # These are critical for printing - need html for kableExtra
       plotOutput("plotset"),      # This is for Panel 2 for the plots
       cellArgs = list(style='white-space: normal;')

       #tableOutput("table"),
       #plotOutput("plot"),
       #textOutput("vv"),
     ),

     hr(),
     textInput(
       inputId = "file_name",
       label   = "Name of file to save the graph (exclude the file path and extension)",
       value = "Encourage_Power graph"),
     radioButtons(
       inputId = "png_pdf",
       label   = "Save graph as a png or pdf file",
       choices = c("png" = "png", "pdf" = "pdf"),
       inline = FALSE),
     downloadButton('down', 'Download Graph')
     )
    )
  )

server <- shinyServer(
  function(input, output, session) {

  # The following exits into R when exiting Shiny- this is nice!
    session$onSessionEnded(function() {
      stopApp(returnValue = invisible())
      })

    #
    # THIS IS THE MAIN PROGRAM THAT DOES ALL THE CALCULATIONS AND RETURNS THE RESULTS
    # INTO POW_RES. IT IS REACTIVE SO UPDATES WHEN SIDE BAR INPUTS ARE CHANGED
    #

    pow_res <- reactive({

      # Read in inputs and save them to variables which makes it easier to refer to them

      if (input$iv_itt ==  "Participation effect on long-term outcome (Complier average causal effect [CACE] analysis)") {
        iv_itt <- 1
      } else if (input$iv_itt == "Encouragement effect on participation (First-stage intent-to-treat [ITT] analysis)") {
        iv_itt <- 2
      }

      if (input$rct_design == "Nonclustered design") {
        rct_design <- 1
      } else if (input$rct_design == "Clustered design") {
        rct_design <- 2
      } else if (input$rct_design == "Random block design") {
        rct_design <- 3
      }

      if (input$samp_size == "Calculate the sample size required to achieve a target minimum detectable effect size (MDE) for the CACE analysis or treatment-control difference for the ITT participation analysis") {
        samp_size <- 1
      } else if (input$samp_size == "Calculate the MDE for a given sample size (for the CACE analysis only)") {
        samp_size <- 2
      }

      if (input$power_form == "Eq. (12): Outcome and participation rate variances") {
        power_form <- 1
      } else if (input$power_form == "Eq. (11): Outcome variance only") {
        power_form <- 2
      } else if (input$power_form == "Eq. (13): Upper bound variance") {
        power_form <- 3
      }

      mde   <- input$mde
      mclus <- input$mclus
      nclus <- input$nclus
      ntot  <- input$ntot
      pt    <- input$pt
      icc   <- input$icc
      icc_imp <- input$icc_imp
      mu1   <- input$mu1
      mu0   <- input$mu0
      alpha <- input$alpha

      if (input$two_tailed=="Two-tailed") {
        two_tailed <- 1
      } else if (input$two_tailed=="One-tailed") {
        two_tailed <- 0
      }

      power <- input$power
      power_s <- power[1]
      power_f <- power[2]

      if (input$power_by == ".05") {
        power_by <- .05
      } else if (input$power_by == ".01") {
        power_by <- .01
      } else if (input$power_by == ".10") {
        power_by <- .10
      }

      r2yx <- input$r2yx
      nx   <- input$nx

      # CHECK FOR ERRORS

      # Set ICC and ICC_IMP to 0 for Nonclustered design and ICC_IMP to 0
      # for the Clustered design

      if (rct_design==1) {
        icc <- 0
        icc_imp <- 0
      } else if (rct_design==2) {
        icc_imp <- 0
      } else if (rct_design==3) {
      icc <- 0
      }

      # Write error messages and crash variable for key inputs
      # Test for missing values, decimal values for categories, and out of ranges

      crash <- 0
      nerr  <- 0
      err_mess <- matrix(ncol=100,nrow=1)

      # pt

      if ((!exists("pt")) | (length(pt) != 1)) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid assignment rate to the treatment group")
      } else if ((is.na(pt)) | (pt >= 1) | (pt <= 0))
      {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid assignment rate to the treatment group")
      }

      # mu1 and mu0

      badmu1 <- 0
      badmu0 <- 0

      if ((!exists("mu1")) | (length(mu1) != 1)) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid participation rate for the treatment group")
        badmu1 <- 1
      } else if ((is.na(mu1)) | (mu1 > 1) | (mu1 <= 0))
      {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid participation rate for the treatment group")
        badmu1 <- 1
      }

      if ((!exists("mu0")) | (length(mu0) != 1)) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid participation rate for the control group")
        badmu0 <- 1
      } else if ((is.na(mu0)) | (mu0 >= 1) | (mu0 < 0))
      {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid participation rate for the control group")
        badmu0 <- 1
      }

      if ((badmu1==0) & (badmu0==0) & (mu0 >= mu1)) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Participation rate must be larger for the treatment than control group")
      }

      # MCLUS and NCLUS

      if ((samp_size==2) & (rct_design > 1)) {
        if ((!exists("mclus")) | (is.na(mclus)) | (mclus<3)
            | (mclus != round(mclus))) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid total number of clusters")
        }

        if ((!exists("nclus")) | (is.na(nclus)) | (nclus<1)
            | (nclus != round(nclus))) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid number of individuals per cluster")
        }
      }

      # NTOT

      if ((samp_size==2) & (rct_design==1)) {
        if ((!exists("ntot")) | (is.na(ntot)) | (ntot<3)
            | (ntot != round(ntot))) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid total number of individuals")
        }
      }

      # ICC

      if (rct_design==2) {
        if ((!exists("icc")) | (length(icc) != 1)) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid intraclass correlation for mean outcomes")
        } else if ((is.na(icc)) | (icc > 1) | (icc < 0))
        {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid intraclass correlation for mean outcomes")
        }
      }

      # ICC_IMP

      if (rct_design==3) {
        if ((!exists("icc_imp")) | (length(icc_imp) != 1)) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid intraclass correlation for treatment effects")
        } else if ((is.na(icc_imp)) | (icc_imp > 1) | (icc_imp < 0))
        {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid intraclass correlation for treatment effects")
        }
      }

      # MDE

      if ((samp_size == 1) & (iv_itt == 1)) {
        if ((!exists("mde")) | (is.na(mde)) | (mde <= 0) | (mde > 5)) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid MDE")
        }
      }

      # ALPHA

      if ((!exists("alpha")) | (length(alpha) != 1)) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid alpha level")
      } else if ((is.na(alpha)) | (alpha>=1) | (alpha<=0))
      {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid alpha level")
      }

      # R2YX

      badr2 <- 0
      if ((!exists("r2yx")) | (length(r2yx) != 1)) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid regression R2 value from model covariates")
        badr2 <- 1
      } else if ((is.na(r2yx)) | (r2yx >= 1) | (r2yx < 0))
      {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid regression R2 value from model covariates")
        badr2 <- 1
      }

      # NX

      # Set NX to 0 if R2YX=0

      if ((badr2==0) & (r2yx==0)) {
        nx <- 0
      }

      if ((!exists("nx")) | (is.na(nx)) | (nx<0)
          | (nx != round(nx))) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid number of model covariates")
      } else if ((badr2==0) & (r2yx>0) & (nx==0)) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Number of model covariates must be greater than 0 if R2>0")
      }

      # Power_form = 3 and samp_size = 2 error

      if ((power_form==3) & (samp_size==2)) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("The program does not calculate MDEs using Eq. (13) for the CACE analysis: Change power formula
                              to Eq. (12) or Eq. (11)")
      }

      # iv_itt = 2 and samp_size = 2 error

      if ((iv_itt==2) & (samp_size==2)) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("The program does not calculate minimum detectable treatment effects for the ITT paticipation rate analysis:
        Change the type of power analysis to instead calculate the required sample size")
      }

      #
      # NOW PERFORM THE VARIANCE-COVARIANCE CALCULATIONS IF crash==0
      #

      if (crash==0) {

        # Calculate the t cutoff alpha depending on a two- or one-tailed test

        if (two_tailed==1) {
          alpha2 <- 1 - (alpha/2)
        } else if (two_tailed==0)
        {
          alpha2 <- 1 - alpha
        }

        # Set mde = mu1-mu0 for itt analysis

        if (iv_itt == 2) {
          mde <- mu1 - mu0
        }

        # Calculate the design effect

        if (rct_design==1) {
          deff <- 1
        } else if (rct_design==2) {
          deff <- 1+icc*(nclus-1)
        } else if (rct_design==3) {
          deff <- 1+icc_imp*((nclus*pt*(1-pt))-1)
        }

        # Calculate the basic variance term

        if (rct_design>1) {
          term1     <- deff/(((mu1-mu0)^2)*nclus*pt*(1-pt))  # CACE analysis
          term1itt1 <- mu1*(1-mu1)/(nclus*pt)                # ITT analysis
          term1itt2 <- mu0*(1-mu0)/(nclus*(1-pt))
          term1itt  <- deff*(term1itt1+term1itt2)

        } else if (rct_design==1) {
          term1     <- deff/(((mu1-mu0)^2)*pt*(1-pt))        # CACE analysis
          term1itt1 <- mu1*(1-mu1)/pt                        # ITT analysis
          term1itt2 <- mu0*(1-mu0)/(1-pt)
          term1itt  <- deff*(term1itt1+term1itt2)
        }

        # Calculate mustar for the random block design

        if (power_form==3) {
          absmu1 <- abs(mu1-.5)
          absmu0 <- abs(mu0-.5)

          if (absmu1 <= absmu0) {
            mustar <- mu1
          } else if (absmu1 > absmu0) {
            mustar <- mu0
          }
        }

        # Calculate variance for samp_size = 1 and 2 depending on power formula

        baddf1 <- 0
        baddf2 <- 0
        badopt <- 0

        # Do first for samp_size = 1

        if (samp_size==1) {

          term1a <- term1/(mde^2)
          term1itta <- term1itt/(mde^2)

          if (power_form==1) {
            term2 <- (1-r2yx) + (mde^2)*((mu1*(1-mu1)*(1-pt)) + (mu0*(1-mu0)*pt))
          } else if (power_form==2) {
            term2 <- (1-r2yx)
          } else if (power_form==3) {
            term2 <- (1-r2yx) + 2*mde*sqrt(mustar*(1-mustar)) + (mde^2)*mustar*(1-mustar)
          }

          varxx <- term1a*term2
          varxxitt <- term1itta*(1-r2yx)

        }

        # Function to calculate required m for SAMP_SIZE==1

        calc_m <- function(var_term) {

          # Use Secant Method to Compute M

          if (var_term>0) {

            maxiter <- 25
            converge <- .000001

            # Compute initial function values

            if (rct_design==1) {
              m_opt   <- 1000
              m_opt1  <- 1200
            } else if (rct_design > 1) {
              m_opt   <- 30
              m_opt1  <- 20
            }

            df  <- m_opt  - nx - 2
            df1 <- m_opt1 - nx - 2

            if (df<0) {
              df  <- m_opt - 2
              if (df<0) {
                df <- 1
              }
            }

            if (df1<0) {
              df1  <- m_opt1 - 2
              if (df1<0) {
                df1 <- 1
              }
            }

            inv_alpha2 <- qt(alpha2,df)
            inv_power  <- qt(power,df)

            factor <- inv_alpha2 + inv_power

            func <- m_opt - ((factor^2)*var_term)

            inv_alpha2 <- qt(alpha2,df1)
            inv_power  <- qt(power,df1)

            factor <- inv_alpha2 + inv_power

            func1 <- m_opt1 - ((factor^2)*var_term)

            # Iterate

            iter <- 1
            while ((iter <= maxiter) & (max(abs(func))>converge)) {

              delta <- func*(m_opt - m_opt1)/(func - func1)
              m_opt1 <- m_opt
              m_opt  <- m_opt - delta

              func1 <- func

              df  <- m_opt  - nx - 2

              if (df<1) {
                df <- 1
              }

              inv_alpha2 <- qt(alpha2,df)
              inv_power  <- qt(power,df)

              factor <- inv_alpha2 + inv_power

              func <- m_opt - ((factor^2)*var_term)

              iter <- iter + 1
            }

            if (iter <= maxiter) {
              m_optf <- round(m_opt)

            } else if (iter >= maxiter) {
              m_optf <- 0  #c("No Convergence")
            }

          } else if (var_term <= 0) {
            m_optf <- NA
            iter   <- NA
            func   <- NA
          }

          # Output a data frame

          m_optg <- data.frame(m_optf,iter,func)

          return(m_optg)
        }  # END of Optimization function

        ps <- round(power_s*100)
        pf <- round(power_f*100)
        pb <- round(power_by*100)

        countp <- 1
        for (powert in seq(ps,pf,pb)) {

          power <- powert/100

          mde_val  <- NA
          m_opt <- NA
          iter  <- NA
          func  <- NA

        if ((samp_size==1) & (badopt==0)) {

            if (iv_itt == 1) {
              m_opt_out <- calc_m(varxx)           # Calls optimization function
            } else if (iv_itt == 2) {
              m_opt_out <- calc_m(varxxitt)
            }

            m_opt <- m_opt_out$m_optf
            iter  <- m_opt_out$iter
            func  <- m_opt_out$func

            # Check for lack of convergence

            if ((m_opt <= 0) & (badopt==0)) {
              crash <- 1
              nerr  <- nerr + 1
              err_mess[1,nerr] <- c("Required sample size is too small: Reduce the MDE value (treatment-control contrast)")
              badopt <- 1
            }

            # Check for negative degrees of freedom

            df_chk1 <- m_opt - nx - 2

            if ((df_chk1 <= 0) & (baddf1 == 0)) {
              crash <- 1
              nerr  <- nerr + 1
              err_mess[1,nerr] <- c("Degrees of freedom are negative: Reduce the treatment-control contrast, the number of covariates,
                                    or the R2 value")
              baddf1 <- 1
            }
        }

        #
        # Now do this for samp_size = 2
        #

        if ((samp_size==2) & (iv_itt == 1)) {

          if (rct_design > 1) {
            df  <- mclus - nx - 2
            term1aa <- deff/(((mu1-mu0)^2)*mclus*nclus*pt*(1-pt))
          } else if (rct_design==1) {
            df <- ntot - nx - 2
            term1aa <- deff/(((mu1-mu0)^2)*ntot*pt*(1-pt))
          }

          # Check for negative degrees of freedom

          df_chk2 <- df
          if ((df_chk2 <= 0) & (baddf2 == 0)) {
            crash <- 1
            nerr  <- nerr + 1
            err_mess[1,nerr] <- c("Degrees of freedom are negative: Reduce the treatment-control contrast, the number of covariates,
                                  or the R2 value")
            baddf2 <- 1
          }

          if (df_chk2 <= 0) {
            df_chk2 <- 1
            }

          inv_alpha2 <- qt(alpha2,df_chk2)
          inv_power  <- qt(power,df_chk2)

          factor <- inv_alpha2 + inv_power
          term1af <- term1aa*(factor^2)

          if (power_form==1) {
            muterm <- ((mu1*(1-mu1)*(1-pt)) + (mu0*(1-mu0)*pt))
            mde_val2 <- term1af*(1-r2yx) / (1-(term1af*muterm))
            #dd_chk <- (1-(term1af*muterm))
            mde_val  <- mde_val2^.5

          } else if (power_form==2) {
            mde_val2 <- term1af*(1-r2yx)
            mde_val  <- mde_val2^.5
          }
        }

        # Write results to the data frame resgt and stack across power levels in resg

        if (samp_size==1) {
          power1 <- format(power, digits=2, nsmall=2)
          m_opt1 <- format(m_opt, digits=20, big.mark=",")

          resgt  <- data.frame(power, m_opt, power1, m_opt1)
        } else if (samp_size==2) {
          power1   <- format(power, digits=2, nsmall=2)
          mde_val1 <- format(mde_val, digits=2, nsmall = 2)
          resgt    <- data.frame(power,mde_val,power1,mde_val1)
        }

        if (countp==1) {
          resg <- resgt
        } else if (countp>1) {
          resg <- rbind(resg,resgt)
        }

        countp <- countp + 1

        }  # End of Power Level loop

      } # End of if Crash=0

      #
      # WRITE RESULTS TO POW_RES WHICH IS A DATA FRAME
      #

      if (crash==0) {
        pow_res <- resg

      } else if (crash==1) {
        err_out <- data.frame(err_mess[1:nerr])
        colnames(err_out) <- c("Errors")
        pow_res <- err_out
      }

    }) # end pow_res reactive

    #
    # RETRIEVES THE CRASH VARIABLE NEEDED TO DEFINE THE TABLES AND PLOTS
    #

    crashv <- reactive({
      if (ncol(pow_res())==1) {
        crashv <- 1
      } else if (ncol(pow_res())>1)
      {
        crashv <- 0
      }
    })

    #
    # CREATES THE KABLEEXTRA CODE FOR THE TABLES TO RUN LATER
    # DEPENDING ON THE CRASH VARIABLE
    #

    # First get caption for the tables

    cap <- reactive({

      if (input$iv_itt == "Participation effect on long-term outcome (Complier average causal effect [CACE] analysis)") {
        iv_ittz <- 1
      } else if (input$iv_itt == "Encouragement effect on participation (First-stage intent-to-treat [ITT] analysis)") {
        iv_ittz <- 2
      }

      if (input$rct_design == "Nonclustered design") {
        rct_designz <- 1
      } else if (input$rct_design == "Clustered design") {
        rct_designz <- 2
      } else if (input$rct_design == "Random block design") {
        rct_designz <- 3
      }

      if (input$samp_size == "Calculate the sample size required to achieve a target minimum detectable effect size (MDE) for the CACE analysis or treatment-control difference for the ITT participation analysis") {
        samp_sizez <- 1
      } else if (input$samp_size == "Calculate the MDE for a given sample size (for the CACE analysis only)") {
        samp_sizez <- 2
      }

      if (input$power_form == "Eq. (12): Outcome and participation rate variances") {
        pfz <- 1
      } else if (input$power_form == "Eq. (11): Outcome variance only") {
        pfz <- 2
      } else if (input$power_form == "Eq. (13): Upper bound variance") {
        pfz <- 3
      }

      if ((pfz == 3) | (iv_ittz == 2)) {
        samp_sizez <- 1
      }

      mdez <- input$mde

      mclusz  <- input$mclus
      ptz     <- input$pt
      nclusz  <- input$nclus
      ntotz   <- input$ntot
      mu1z    <- input$mu1
      mu0z    <- input$mu0

      if (iv_ittz == 2) {
        mdez <- mu1z - mu0z
      }

      if (iv_ittz == 1) {
        if (rct_designz==1) {
          dsgn <- c("Nonclustered CACE Design")
        } else if (rct_designz==2) {
          dsgn <- c("Clustered CACE Design")
        } else if (rct_designz==3) {
          dsgn <- c("Random Block CACE Design")
        }
      }

      if (iv_ittz == 2) {
        if (rct_designz==1) {
          dsgn <- c("Nonclustered Design")
        } else if (rct_designz==2) {
          dsgn <- c("Clustered Design")
        } else if (rct_designz==3) {
          dsgn <- c("Random Block Design")
        }
      }

      mdez    <- format(mdez,digits=2, nsmall=2)
      mclusz  <- format(mclusz,digits=1)
      ptz     <- format(ptz,digits=2, nsmall=2)
      nclusz  <- format(nclusz,digits=1)
      ntotz   <- format(ntotz,digits=1)
      mu1z    <- format(mu1z,digits=2, nsmall=2)
      mu0z    <- format(mu0z,digits=2, nsmall=2)

      if (crashv()==0) {
        if ((samp_sizez==2) & (rct_designz==1)) {
          cap1 <- sprintf("MDE Results for the %s",dsgn)
          cap2 <- sprintf("with %s Individuals and Treatment-Control",ntotz)
          cap3 <- sprintf("Participation Rates of %s and %s",mu1z,mu0z)
          cap  <- data.frame(cap1,cap2,cap3)

        } else if ((samp_sizez==2) & (rct_designz==2)) {
          cap1 <- sprintf("MDE Results for the %s",dsgn)
          cap2 <- sprintf("with %s Clusters, %s Individuals per Cluster, and",mclusz,nclusz)
          cap3 <- sprintf("Treatment-Control Participation Rates of %s and %s",mu1z,mu0z)
          cap  <- data.frame(cap1,cap2,cap3)
          
        } else if ((samp_sizez==2) & (rct_designz==3)) {
          cap1 <- sprintf("MDE Results for the %s",dsgn)
          cap2 <- sprintf("with %s Blocks, %s Individuals per Block, and",mclusz,nclusz)
          cap3 <- sprintf("Treatment-Control Participation Rates of %s and %s",mu1z,mu0z)
          cap  <- data.frame(cap1,cap2,cap3)

        } else if ((samp_sizez==1) & (rct_designz==1) & (iv_ittz==1)) {
          cap1 <- sprintf("Required Individual Sample Sizes for the %s to",dsgn)
          cap2 <- sprintf("Achieve an MDE of %s with Treatment-Control",mdez)
          cap3 <- sprintf("Participation Rates of %s and %s",mu1z,mu0z)
          cap  <- data.frame(cap1,cap2,cap3)

        } else if ((samp_sizez==1) & (rct_designz==2) & (iv_ittz==1)) {
          cap1 <- sprintf("Required Cluster Sample Sizes for the %s to",dsgn)
          cap2 <- sprintf("Achieve an MDE of %s with %s Individuals per Cluster and",mdez,nclusz)
          cap3 <- sprintf("Treatment-Control Participation Rates of %s and %s",mu1z,mu0z)
          cap  <- data.frame(cap1,cap2,cap3)
          
        } else if ((samp_sizez==1) & (rct_designz==3) & (iv_ittz==1)) {
          cap1 <- sprintf("Required Block Sample Sizes for the %s to",dsgn)
          cap2 <- sprintf("Achieve an MDE of %s with %s Individuals per Block and",mdez,nclusz)
          cap3 <- sprintf("Treatment-Control Participation Rates of %s and %s",mu1z,mu0z)
          cap  <- data.frame(cap1,cap2,cap3)

        } else if ((samp_sizez==1) & (rct_designz==1) & (iv_ittz==2)) {
          cap1 <- sprintf("Required Individual Sample Sizes for the %s to",dsgn)
          cap2 <- sprintf("Detect a Participation Rate Impact of %s for Treatment-Control",mdez)
          cap3 <- sprintf("Participation Rates of %s and %s",mu1z,mu0z)
          cap  <- data.frame(cap1,cap2,cap3)
          
        } else if ((samp_sizez==1) & (rct_designz==2) & (iv_ittz==2)) {
          cap1 <- sprintf("Required Cluster Sample Sizes for the %s with",dsgn)
          cap2 <- sprintf("%s Individuals per Cluster to Detect a Participation Rate Impact of %s for",nclusz,mdez)
          cap3 <- sprintf("Treatment-Control Participation Rates of %s and %s",mu1z,mu0z)
          cap  <- data.frame(cap1,cap2,cap3)
        
        } else if ((samp_sizez==1) & (rct_designz==3) & (iv_ittz==2)) {
          cap1 <- sprintf("Required Block Sample Sizes for the %s with",dsgn)
          cap2 <- sprintf("%s Individuals per Block to Detect a Participation Rate Impact of %s for",nclusz,mdez)
          cap3 <- sprintf("Treatment-Control Participation Rates of %s and %s",mu1z,mu0z)
          cap  <- data.frame(cap1,cap2,cap3)
        }

      } else NULL
    }) # end cap reactive

    cap_tab <- reactive({
      if (crashv()==0) {
        cap_tab <- paste(cap()[1],cap()[2],cap()[3])
      } else NULL
    })

    cap_grph <- reactive({
      if (crashv()==0) {
        cap_grph1 <- paste(cap()[1],"\n",cap()[2])
        cap_grph  <- paste(cap_grph1,"\n",cap()[3])
      } else NULL
    })

    # Now for column names

    cn <- reactive({

      if (input$samp_size == "Calculate the sample size required to achieve a target minimum detectable effect size (MDE) for the CACE analysis or treatment-control difference for the ITT participation analysis") {
        ssz <- 1
      } else if (input$samp_size == "Calculate the MDE for a given sample size (for the CACE analysis only)") {
        ssz <- 2
      }

      if (input$rct_design == "Nonclustered design") {
        rz <- 1
      } else if (input$rct_design == "Clustered design") {
        rz <- 2
      } else if (input$rct_design == "Random block design") {
        rz <- 3
      }

      if (input$power_form == "Eq. (12): Outcome and participation rate variances") {
        pfz <- 1
      } else if (input$power_form == "Eq. (11): Outcome variance only") {
        pfz <- 2
      } else if (input$power_form == "Eq. (13): Upper bound variance") {
        pfz <- 3
      }

      if (pfz == 3) {
        ssz <- 1
      }

      if (ssz==2) {
        cn <- c("Power Level", "MDE Value")
      } else if ((ssz==1) & (rz==2)) {
        cn <- c("Power Level", "Required Clusters")
      } else if ((ssz==1) & (rz==3)) {
        cn <- c("Power Level", "Required Blocks")
      } else if ((ssz==1) & (rz==1)) {
        cn <- c("Power Level", "Required Individuals")
      }
    })

    # Now for the footnote

    fn <- reactive({

      if (input$iv_itt == "Participation effect on long-term outcome (Complier average causal effect [CACE] analysis)") {
        iv_ittz <- 1
      } else if (input$iv_itt == "Encouragement effect on participation (First-stage intent-to-treat [ITT] analysis)") {
        iv_ittz <- 2
      }

      if (input$samp_size == "Calculate the sample size required to achieve a target minimum detectable effect size (MDE) for the CACE analysis or treatment-control difference for the ITT participation analysis") {
        ssz <- 1
      } else if (input$samp_size == "Calculate the MDE for a given sample size (for the CACE analysis only)") {
        ssz <- 2
      }

      if (input$rct_design == "Nonclustered design") {
        rz <- 1
      } else if (input$rct_design == "Clustered design") {
        rz <- 2
      } else if (input$rct_design == "Random block design") {
        rz <- 3
      }

      if (rz == 3) {
        ssz <- 1
      }

      mdez <- input$mde
      ptz  <- input$pt

      mclusz <- input$mclus
      nclusz <- input$nclus
      ntotz  <- input$ntot
      iccz   <- input$icc
      icc_impz <- input$icc_imp

      alphaz <- input$alpha
      r2yxz  <- input$r2yx
      nxz    <- input$nx

      if (input$two_tailed=="Two-tailed") {
        twotz <- c("Two-tailed")
      } else if (input$two_tailed=="One-tailed") {
        twotz <- c("One-tailed")
      }

      if (rz==1) {
        icc <- 0
        icc_imp <- 0
      } else if (rz==2) {
        icc_imp <- 0
      } else if (rz==3) {
        icc <- 0
      }

      # Set NX to 0 if R2YX=0

      if (r2yxz==0) {
        nxz <- 0
      }

      # Calculate the design effect

      if (rz==1) {
        deffz <- 1
      } else if (rz==2) {
        deffz <- 1+iccz*(nclusz-1)
      } else if (rz==3) {
        deffz <- 1+icc_impz*((nclusz*ptz*(1-ptz))-1)
      }

      # Power formula

      if (input$power_form == "Eq. (12): Outcome and participation rate variances") {
        pform <- c("(12)")
      } else if (input$power_form == "Eq. (11): Outcome variance only") {
        pform <- c("(11)")
      } else if (input$power_form == "Eq. (13): Upper bound variance") {
        pform <- c("(13)")
      }

      # Degrees of freedom

      if (rz > 1) {
        dfz  <- mclusz - nxz - 2
      } else if (rz==1) {
        dfz <- ntotz - nxz - 2
      }

      mdez    <- format(mdez,digits=2, nsmall=2)
      ptz     <- format(ptz,digits=2)
      iccz    <- format(iccz,digits=2)
      icc_impz  <- format(icc_impz,digits=2)
      r2yxz   <- format(r2yxz,digits=2, nsmall=2)
      alphaz  <- format(alphaz,digits=2, nsmall=2)
      nxz     <- format(nxz,digits=1)
      deffz   <- format(deffz,digits=4)
      df      <- format(dfz,digits=8)

      if (crashv()==0) {

        if (rz == 1) {

          txt1 <- c("Inputs: Treatment assignment rate = %s, R2yx = %s, Covariates = %s,")
          if (iv_ittz == 1) {
            txt2 <- c("Alpha = %s, %s test, Formula in article = %s")
          } else if (iv_ittz == 2) {
            txt2 <- c("Alpha = %s, %s test, Formula in article = (21)")
          }
          txtg <- paste(txt1,txt2)

          fn <- sprintf(txtg,ptz,r2yxz,nxz,alphaz,twotz,pform)
        } else if (rz == 2) {

          txt1 <- c("Inputs: Treatment assignment rate = %s, R2yx = %s, Covariates = %s,")
          txt2 <- c("Design effect = %s, ICC of mean outcomes = %s, Alpha = %s, %s test,")
          if (iv_ittz == 1) {
            txt3 <- c("Formula in article = %s")
          } else if (iv_ittz == 2) {
            txt3 <- c("Formula in article = (21)")
          }
          txtg <- paste(txt1,txt2,txt3)

          fn <- sprintf(txtg,ptz,r2yxz,nxz,deffz,iccz,alphaz,twotz,pform)
        } else if (rz == 3) {

          txt1 <- c("Inputs: Treatment assignment rate = %s, R2yx = %s, Covariates = %s,")
          txt2 <- c("Design effect = %s, ICC of treatment effects = %s,")
          if (iv_ittz == 1) {
            txt3 <- c("Alpha = %s, %s test, Formula in article = %s")
          } else if (iv_ittz == 2) {
            txt3 <- c("Alpha = %s, %s test, Formula in article = (21)")
          }
          txtg <- paste(txt1,txt2,txt3)

          fn <- sprintf(txtg,ptz,r2yxz,nxz,deffz,icc_impz,alphaz,twotz,pform)

        } else NULL
      }
    })

    ktab <- reactive({

      if (crashv() == 0) {
      kbl(pow_res()[3:4],
          caption = paste("<center><strong>", cap_tab(), "<center><strong>"),
          align=c("c","c"),escape=FALSE,col.names=cn()) %>%
          kable_styling(full_width=FALSE,position="left",
          bootstrap_options = "striped") %>%
          column_spec(1, color ="#00aff5",bold=TRUE) %>%          #007bbc
          column_spec(2, color ="#00aff5",bold=TRUE) %>%
          footnote(general = fn(), fixed_small_size = TRUE)

      } else if (crashv() == 1)
      {
      kbl(pow_res()) %>%
          kable_styling(full_width=FALSE,position="left") %>%
          column_spec(1, color = "red", width = 5)
      }
    })

    #
    # CREATES THE GGPLOT2 CODE FOR THE PLOTS TO RUN LATER
    # DEPENDING ON THE CRASH AND SAMP_SIZE VARIABLE
    #

    grph <- reactive({

      if ((crashv() == 0) & (input$samp_size=="Calculate the MDE for a given sample size (for the CACE analysis only)")
          & (input$power_form != "Eq. (14): Upper bound variance")) {
        ggplot(
          pow_res()[1:2],                     # Reads the data
          aes(x = power,                      # aes sets the axes
              y = mde_val,
              group = 1)) +
          geom_line(color="#00aff5",size=1.2) +
          labs(x = cn()[1], y = cn()[2]) +
          theme(axis.title.x = element_text(face="bold")) +
          theme(axis.title.y = element_text(face="bold"))
      } else if ((crashv() == 0) & ((input$power_form == "Eq. (14): Upper bound variance") |
                 (input$samp_size=="Calculate the sample size required to achieve a target minimum detectable effect size (MDE) for the CACE analysis or treatment-control difference for the ITT participation analysis")))
      {
        ggplot(
          pow_res()[1:2],
          aes(x = power,
              y = m_opt,
              group = 1)) +
          geom_line(color="#00aff5",size=1.2) +
          scale_y_continuous(label=comma) +
          labs(x = cn()[1], y = cn()[2]) +
          theme(axis.title.x = element_text(face="bold")) +
          theme(axis.title.y = element_text(face="bold"))
      } else NULL

    })

    # This one is for downloading

    grph1 <- reactive({

      if ((crashv() == 0) & (input$samp_size=="Calculate the MDE for a given sample size (for the CACE analysis only)")
          & (input$power_form != "Eq. (14): Upper bound variance")) {
        ggplot(
          pow_res()[1:2],
          aes(x = power,
              y = mde_val,
              group = 1)) +
          geom_line(color="#00aff5",size=1.2) +
          labs(title = cap_grph(), caption = fn(), x = cn()[1], y = cn()[2]) +
          # Allows the footnote to wrap and adds space before it
          theme(plot.caption=element_textbox_simple(hjust=0, size=10, margin=margin(10,0,0,0))) +
          theme(plot.title = element_text(hjust = 0.5)) +        # This centers the title
          theme(axis.title.x = element_text(face="bold")) +
          theme(axis.title.y = element_text(face="bold"))

      } else if ((crashv() == 0) & ((input$power_form == "Eq. (14): Upper bound variance") |
                 (input$samp_size=="Calculate the sample size required to achieve a target minimum detectable effect size (MDE) for the CACE analysis or treatment-control difference for the ITT participation analysis")))
      {
        ggplot(
          pow_res()[1:2],
          aes(x = power,
              y = m_opt,
              group = 1)) +
          geom_line(color="#00aff5",size=1.2) +
          scale_y_continuous(label=comma) +
          labs(title = cap_grph(), caption = fn(), x = cn()[1], y = cn()[2]) +
          theme(plot.caption=element_textbox_simple(hjust=0, size=10, margin=margin(10,0,0,0))) +
          theme(plot.title = element_text(hjust = 0.5)) +
          theme(axis.title.x = element_text(face="bold")) +
          theme(axis.title.y = element_text(face="bold"))
      } else NULL

    })

  #
  # CREATE SUBMIT BUTTON AND MAKE TABLES IN PANEL 1 AND PLOTS IN PANEL 2
  #

    # Need to reset the output file name for those who leave it blank or the download produces an error
    filen <- reactive({
      ifelse(input$file_name != "", input$file_name, "Encourage_Power graph")
    })

  observe({

    if (input$submit_button > 0) {
      #output$vv <- renderText({ tt() })
      #output$table <- renderTable({ crashv() })

      output$tableset <- renderText({ ktab() })

      if (is.null(grph())) {
        output$plotset <- renderPlot(NULL)
      } else
      {
        output$plotset <- renderPlot({ grph() })

        #Download table to file
        output$down <- downloadHandler(
          filename = function() {
            paste(filen(), input$png_pdf, sep=".")
          },
          content = function(file) {
            plot_function <- match.fun(input$png_pdf)

            plot_function(file)
            print( grph1() )
            #save_kable(ktab())
            dev.off()
          }
        )
      }
    }
  })

})  # End server

suppressMessages(
  suppressWarnings(
    runApp(shinyApp(ui = ui, server = server))
))




