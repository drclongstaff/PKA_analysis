library(shiny)
ver_str<-"version 0.53"
fluidPage(
  
  titlePanel (h2("Analysis of PKA in albumin",  align = "right")),
 
 
                
  sidebarLayout(
    
    sidebarPanel(
   
              fluidRow(
                  column(6, fileInput("data", label = "Select dataset:")),
                           
                  column(3,radioButtons(inputId = "sep", label = "File type", choices = c(csv = ",", txt ="\t"), selected = ",")),
                  
                  column(3,checkboxInput(inputId= "header", label = "Columns have header text", value = TRUE))
                         ),
                         
              fluidRow(
                  
                           
                  column(5, numericInput("numrows",
                            label = h5("Number of rows in plots"), value = 8)),
                           
                  column(4,  uiOutput("what"))
                  
                  #column(3,checkboxInput( "names", label = "Well names", value = FALSE))
                  
                      ),

              fluidRow(
                
                column(4, numericInput("delay",label=h5("Time delay"), step = 20, value=60)),  
                
                column(4, uiOutput("maxt")),
                  
                column(4, numericInput("num", 
                                         label = h5("Maximum absorbance"), step = 0.025,
                                         value = 0.1))
                     ),
              
              fluidRow(
                  column(3, checkboxInput("sqr", label = "Use time squared", value = TRUE)),
      
                  column(3, checkboxInput("zero", label = "Zero at t0", value = TRUE)),
                         
                  column(6, radioButtons(inputId="show", label = "show what", choices = c("results", "well names", "well numbers"),
                                                                                          selected = "results"))     #"time sq rates", "rates",  
      
                  
              ),
              
            helpText(h4("Subtract kallikrein activity in PK substrate using substrate blank")),
            
           
              
              fluidRow(
                column(5, numericInput("blank", label = h5("Select a well number to use as PK substrate blank. 0 for no subtraction"), value = 0)),
                
                column(6, numericInput("blrate", label = h5("Specify a known rate (1e6 x abs change/s) for PK substrate blank. 0 for no subtraction"), value = 0))
                
                
              ),
              
              
            
            #fluidRow(
              #column(5, numericInput("drop", label = h5("drop n initial readings"), value = 0))
              
              
           # ),
            
            helpText(h4("Subtract kallikrein activity in samples using no PK substrate wells")),
            
            fluidRow(
              column(6, numericInput("north",label=h5("Subtraction block top start"), step = 1, value=13)), 
              
              column(6, numericInput("south",label=h5("Subtraction block bottom start"), step = 1, value=49))
              
            ),
            
            fluidRow(
              column(6, numericInput("west",label=h5("Subtraction block left start"), step = 1, value=1)), 
              
              column(6, numericInput("east",label=h5("Subtraction block right start"), step = 1, value=7))
              
            ),
              
               fluidRow(
                
                column(9,radioButtons(inputId = "subc", label = "Subtract one half of plate from the other", 
                choices = c("none",  "top-bottom", "bottom-top", "left-right", "right-left"), selected = "top-bottom"))
             
              ),
              
             

      helpText(h4("Please cite this page if you find it useful, Longstaff C,  Shiny App for calculating PKA activities",ver_str,", URL address, last accessed", Sys.Date()))
     
   
    
    
  ),
  mainPanel( 
    tabsetPanel(type="tab",
                tabPanel("Plots", 
                         
                         
                         plotOutput(outputId = "myplotAll"),
                         h4(textOutput("text3")),
                         #tags$br(),
                         #tags$br(),
                         
                         h4(textOutput("text4")),
                         
                         h5(""), tableOutput("resultsTable"), align = "center",
                        helpText(h4("Settings")),
                        helpText(h5(paste("Shiny app for calculating PKA activities", ver_str))),
                        helpText(h5(uiOutput("where"))),
                        
                         tableOutput("settings")
                         ),
                
                
                 
                
                   
                
                tabPanel("Curve", 
                         
                          
                         
                         plotOutput(outputId = "myplot"),
                         
                       
                         
                         #fluidRow(
                           
                         
                          column(5, 
                                 radioButtons(inputId="curveRes", label = "Results to plot", 
                                              choices = c("Absorbance", "Residual"), selected = "Absorbance")
                                 )
                          
                        
                ),
                
                          
                
              
                tabPanel("Raw data", dataTableOutput("contents")),
 
                tabPanel("Help",
                         
                  tags$blockquote(h5("►Load a data file in csv or txt fomat (tab separator)",
                                  tags$br(), 
                                  "►If the columns have headers, check the box",
                                  tags$br(),
                                 "►Select the number of rows to organise the graphs and table",
                                  tags$br(), 
                                  "►All the plots are shown or individual curves can be analysed on the next tab",
                                 tags$br(), 
                                  "►Select the cut off for maximum absorbance",
                                 tags$br(), 
                                 "►Choose how many data points you want to include",
                                 tags$br(),
                                 "►Tick the box if you want to zero each curve at the first absorbance reading",
                                 tags$br(),
                                  "►Select time squared to calculate the rate of prekallikrein activation in ⌂Abs/sec²",
                                 tags$br(),
                                 "►If time squared is not selected you can calculate simple rates of substrate hydrolysis over time",
                                 tags$br(),
                                 "►If your PK substrate has chromogenic activity, you can subtract this by selecting a representative blank well",
                                 tags$br(),
                                 "►There are options for subtracting one half of the plate from the other for background kallikrein activity in the sample",
                                 tags$br(),
                                 "►Subtractions are performed on the raw data, point by point, not on calculated rates",
                                 tags$br(),
                                 "►Options are provided to subtract rates of kallikrein activity in the PK substrate, using a blank well or including a known rate",
                                 tags$br(),
                                 "►There is an option to include a lag phase to avoid including early readings that may be non-linear",
                                 tags$br(),
                                 "►Note: Data files should not contain any gaps or spaces between characters",
                                 tags$br(),
                                 "►Avoid unusual characters such as % and ' in names",
                                 tags$br(),
                                 "►Code and detailed help notes are available in a Github respository at https://github.com/drclongstaff/"
                                 )
  
  ),
  
                
                  
                  
                  
                 # tags$img(src="screenCapSq.png", width=600, height=700)
                  tags$img(src="Plate_graphics.PNG", width=500, height=600)
                  
                )
                
               
                
    )
  )
  
)   
)
