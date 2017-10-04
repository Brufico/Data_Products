#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rockchalk)

# =================================================
# Aux function
#
# get the equation for a simple linear regression model

regeq1 <- function(mod, digits = 3) {
        co <- coef(mod)
        response <- attr(mod$terms, "variables")[[2]]
        predictor <- attr(mod$terms, "variables")[[3]]
        equa <- paste(response,
                      "=",
                      format(co[1], digits = digits),
                      ifelse(co[2] < 0,  "-", "+"),
                      format(abs(co[2]), digits = digits),
                      predictor
        )
        equa
}

# to rewrite: 2-covariables model
regeq2 <- function(mod, digits = 3) {
        co <- coef(mod)
        response <- attr(mod$terms, "variables")[[2]]
        predictor1 <- attr(mod$terms, "variables")[[3]]
        predictor2 <- attr(mod$terms, "variables")[[4]]
        equa <- paste(response,
                      "=",
                      format(co[1], digits = digits),
                      ifelse(co[2] < 0,  "-", "+"),
                      format(abs(co[2]), digits = digits),
                      predictor1,
                      ifelse(co[3] < 0,  "-", "+"),
                      format(abs(co[3]), digits = digits),
                      predictor2
        )
        equa
}





# =================================================
# data
datadir <- "data"
fname <- "Sales_en.csv"
vis <- read.csv2(file=file.path(datadir, fname), dec=".")
vis <- vis[-1]

# =======================================================
# Linear models
# 1-var
mod1 <- lm(Sales ~ Adv, data=vis)
sum1 <- summary(mod1)
eq1 <- regeq1(mod1)


mod1B <- lm(Sales ~ Visits, data=vis)
sum1B <- summary(mod1B)
eq1B <- regeq1(mod1B)

# 2-var
mod2 <- lm(Sales ~ Adv + Visits, data=vis)
sum2 <- summary(mod2)
eq2 <- regeq2(mod2)

# =======================================================

# Define UI for application that draws a histogram
ui <- fluidPage(

        # Application title
        titlePanel("Simple vs multiple regression"),

        # Sidebar with a slider input for number of bins
        sidebarLayout(
                sidebarPanel(
                        h2("Choose a graph"),
                        radioButtons("whichgraph", "pick one",
                                     c("A","b","c")),
                        h2("Perspective angles"),
                        sliderInput("azimut",
                                    "Horizontal (deg):",
                                    min = -90,
                                    max = 90,
                                    value = 40),
                        sliderInput("hauteur",
                                    "vertical (deg):",
                                    min = -90,
                                    max = 90,
                                    value = 8),
                        checkboxInput("showplane",
                                      "Show Regression plane",TRUE)
                ),

                # Show a plot of the generated distribution
                mainPanel(
                        h1("3D scatterplot +  regression plane"),
                        plotOutput("Plot3d")
                )
        )
)


# =======================================================

# Define server logic required to draw a histogram
server <- function(input, output) {

        output$Plot3d <- renderPlot({
                # display scatterplot3D using angles from sliders
                theta <- input$azimut
                phi <- input$hauteur
                planecol <- adjustcolor( "green",
                                         alpha.f = ifelse(input$showplane, 1, 0))

                # draw the graph
                plotPlane(mod2, # reg model
                          # x1 and x2 names in model
                          plotx1 = "Adv",
                          plotx2 = "Visits",
                          drawArrows = TRUE,
                          # angles
                          theta = theta,
                          phi=phi,
                          # floor
                          # x1lim=c(70, 110),
                          # x2lim = c(22, 33),
                          # x1floor = 10, x2floor = 10,

                          # line density in depicting the regression plane
                          npp = 20,

                          # points
                          pch = 16, pcol = "blue",
                          plwd = 3, # contour width
                          pcex = 2, # point size
                          llwd =  0.1,
                          # color of the lines of the regression plane
                          lcol = planecol,
                          llty = 1, # type of the lines of the regression plane

                          col="gray97", # color of the floor

                          ticktype = "detailed")

        })
}

# Run the application
shinyApp(ui = ui, server = server)

