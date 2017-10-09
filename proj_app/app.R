#
# This is the Shiny web application. for the course project



# Preliminary code =========================================

library(shiny)
library(rockchalk)

# ==========================================================
# Auxiliary functions
#
# get the equation (text) for a simple linear regression model

regeq <- function(mod, digits = 3) {
        co <- coef(mod)
        response <- attr(mod$terms, "variables")[[2]]
        predictors <- names(co)
        # function for constructing the body
        partialeq <- function(x, predname) {
                op <- ifelse(sign(x) >=0, "+", "-")
                paste(op, format(abs(x), digits = digits), predname)
        }
        bodyeq <- paste(mapply(partialeq, co[-1], predictors[-1]), collapse = " ")

        equa <- paste(response,
                      "=",
                      format(co[1], digits = digits),
                      bodyeq
        )
        equa
}



# panel function for the pairplot
panel.cor <- function(x, y, digits = 2, prefix = "R=", cex.cor, ...) {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- abs(cor(x, y))
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste0(prefix, txt)
        if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
        text(0.5, 0.5, txt, cex = cex.cor * r)
}


# ==========================================================
# data
datadir <- "data"
fname <- "Sales_en.csv"
vis <- read.csv2(file=file.path(datadir, fname), dec=".")
vis <- vis[-1]

# ==========================================================
# Linear models
# 1-var
mod1 <- lm(Sales ~ Adv, data=vis)
sum1 <- summary(mod1)
eq1 <- regeq(mod1)
# eq1 <- lmatheq(mod1)

mod1B <- lm(Sales ~ Visits, data=vis)
sum1B <- summary(mod1B)
eq1B <- regeq(mod1B)

# 2-var
mod2 <- lm(Sales ~ Adv + Visits, data=vis)
sum2 <- summary(mod2)
eq2 <- regeq(mod2)


# ==========================================================
# General parameters

point_color = "blue"
regcolor = "red"



# ui =======================================================


# Define UI for application that draws a histogram
ui <- fluidPage(

        # Application title
        titlePanel("VIsualizing simple/multiple regression using 3D vs 'flat' representations"),

        # begin a tabset panel (top)
        tabsetPanel(
                # first main tab
                tabPanel("Situation and Instructions",
                         #"contents is an  tablist panel inside the taPanel"
                         navlistPanel(
                                 tabPanel("Situation",
                                          p("Shiny app by Bruno Fischer Colonimos, Story and Data from: "),
                                          HTML("<center><strong>de Lagarde, Jean</strong>, 'L'analyse des données',
                                               Dunod ed (1998)</center>"),
                                          h2("Marketing Mix"),
                                          p("A company has just launched a promotional campaign.
                                            A first report lists, for each of the 8 sales sectors, the sales volume,
                                            the advertising budget and the number of visits to the distributors made
                                            by the sales representatives. The data is shown under the 'Data' tab
                                            (on the left)"),
                                          p("In order to analyze the effectiveness of the campaign, the manager
                                            has tried to explain the sales volume using two regression models:"),
                                          tags$ul(
                                                  tags$li("sales ~ advertising"),
                                                  tags$li("sales ~ visits")
                                          ),
                                          p("You can view the two corresponding scatterplots by clicking on the
                                            'simple regressions' tabs on the left).
                                            Is'nt there something very odd with one of these
                                            scatterplots/regression models ?"),
                                          p("A double regresion model has also been made. You can view and
                                            manipulate the corresponding 3D-scatterplot (and regression plane)
                                                by clicking the tab 'multiple regression' (on top)",
                                            br(),
                                            "By changing the view angles, can you match and explain the odd
                                            simple regression result we saw before?" )
                                 ),
                                 tabPanel("Data",
                                          h3("The data"),
                                          img(src="data_img.png")
                                 ),
                                 tabPanel("Simple regression: Sales ~ Advertising",
                                          h3("Simple regression: Sales ~ Advertising"),
                                          h4("Regression equation"),
                                          p(eq1),
                                          h4("Scatterplot"),
                                          plotOutput("scatter1")
                                 ),
                                 tabPanel("Simple regression: Sales ~ Visits",
                                          h3("Simple regression: Sales ~ Visits"),
                                          h4("Regression equation"),
                                          p(eq1B),
                                          h4("Scatterplot"),
                                          plotOutput("scatter1B")
                                 ),
                                 tabPanel("Scatterplot matrix",
                                          h3("Scatterplot matrix ('pairplot')"),
                                          plotOutput("pairplot"))
                         )

                ),
                # second main tab panel: the interactive part
                tabPanel("Multiple regression : Sales ~ Advertising + Visits",
                         sidebarLayout(
                                 sidebarPanel(
                                         h2("Perspective angles"),
                                         p("Rotate the perspective using the sliders"),
                                         sliderInput("azimut",
                                                     "Horizontal (deg):",
                                                     min = -90,
                                                     max = 90,
                                                     value = 40,
                                                     animate = animationOptions(interval = 200)),
                                         sliderInput("hauteur",
                                                     "vertical (deg):",
                                                     min = -90,
                                                     max = 90,
                                                     value = 8,
                                                     animate = FALSE),
                                         checkboxInput("showplane",
                                                       "Show Regression plane",TRUE)
                                 ),

                                 # Show a 3D scatterplot
                                 mainPanel(
                                         h1("3D scatterplot +  regression plane"),
                                         p(eq2),
                                         plotOutput("Plot3d")
                                 )
                         )
                )
        )
)


# server ===================================================

# Define server logic required
server <- function(input, output) {

   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   #---------------------------------------------------------
   output$Plot3d <- renderPlot({
           # display scatterplot3D using angles from sliders
           theta <- input$azimut
           phi <- input$hauteur
           # planecol <- adjustcolor(regcolor,
                                    # alpha.f = ifelse(input$showplane, 1, 0))
           planecol = regcolor
           # draw the graph
           plotPlane(mod2, # reg model
                     # x1 and x2 names in model
                     plotx1 = "Adv",
                     plotx2 = "Visits",
                     drawArrows = FALSE,
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
                     pcex = 1.5, # point size
                     llwd =  0.1,
                     # color of the lines of the regression plane
                     lcol = planecol,
                     llty = 1, # type of the lines of the regression plane

                     col="gray97", # color of the floor

                     ticktype = "detailed")

   })

   # scatter1 #---------------------------------------------------------
   output$scatter1 <- renderPlot({
           with(vis,
                {
                        plot(x = Adv, y = Sales, pch = 16, col = "blue", cex = 2)
                        abline(mod1)
                }
           )
   })

   # scatter1B #---------------------------------------------------------
   output$scatter1B <- renderPlot({
           with(vis,
                {
                        plot(x = Visits, y = Sales, pch = 16, col = "blue", cex = 2)
                        abline(mod1B, col = "green")
                }
           )
   })

   # scatterplot matrix #-----------------------------------------------
   output$pairplot <- renderPlot({
           pairs(~ Sales + Adv + Visits,
                 data = vis, pch = 16, col = "blue",
                 upper.panel = panel.smooth,
                 lower.panel = panel.cor )
   })
}

# Run the application
shinyApp(ui = ui, server = server)

