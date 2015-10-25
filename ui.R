library(shiny)

shinyUI(
    fluidPage(
        withMathJax(),
        theme = "bootstrap.min.darkly.css",
        # Application title
        h1("Lottery Wizard"),

        # Input parameters
        sidebarLayout(
            sidebarPanel(
                selectInput("select",
                            label = h3("Random Variable"),
                            choices = list("R1" = "R1",
                                           "R2" = "R2",
                                           "R3" = "R3",
                                           "R4" = "R4",
                                           "R5" = "R5",
                                           "R6" = "R6"),
                            selected = 1),

                checkboxInput("densBox",
                              label = "Plot Density Curve",
                              value = FALSE),

                sliderInput("bins",
                            "Number of bins in Histogram:",
                            min = 1,
                            max = 50,
                            value = 10),

                radioButtons("dist",
                             "Fitting Distribution:",
                             c("Normal" = "norm",
                               "Uniform" = "unif",
                               "Gamma" = "gamma",
                               "Beta" = "beta",
                               "Logistic" = "logis",
                               "Log-normal" = "lnorm",
                               "Exponential" = "exp",
                               "Weibull" = "weibull")
                             )
            ),

            # Show results
            mainPanel(
                div(img(src="lucky.jpg", height = 100, width = 100),
                    img(src="lotto.jpg", height = 150, width = 300),
                    style="text-align: center;
                    background-color: rgba(255, 255, 255, 1.0)"
                ),

                tabsetPanel(
                    tabPanel("Documentation",uiOutput("docs")),
                    tabPanel("Histogram", plotOutput("hist")),
                    tabPanel("Models",uiOutput("models")),
                    tabPanel("Fit",plotOutput("fit")),
                    tabPanel("Play", dataTableOutput("play")),
                    tabPanel("Distribution", plotOutput("dist")),
                    tabPanel("Data", dataTableOutput("table"))
                )
            )
        )
    )
)
