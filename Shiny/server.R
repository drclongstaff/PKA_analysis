library(shiny)

function(input, output){


  readDatapre <- reactive({
    
    inputFile <- input$data
    if (is.null(inputFile)) 
      #return(read.csv(file.path("./Data/PKA070918CL.csv"))) 
      return(read.csv(file.path("./Data/GR Example T_B.csv")))
    else(read.table(inputFile$datapath, sep = input$sep, header = input$header))
    
    
  })
  
  
  output$maxt<-renderUI({
   
    numericInput("maxt", 
                 label = h5("Number of points"),
                 value = length(readDatapre()[,1]))
  })
  
  readData <- reactive({

    inputFile <- input$data
    if (is.null(inputFile)) 
      return(read.csv(file.path("./Data/GR Example T_B.csv"), nrows = input$maxt)) 
      else(read.table(inputFile$datapath, sep = input$sep, header = input$header, nrows = input$maxt))


  })
  
 
  
  output$where <- renderUI({
    inputFile <- input$data
    if (is.null(inputFile)) 
      return(file.path("./Data/GR Example T_B.csv")) 
    else(file.path(inputFile))
    
    
  })
  

   var<- reactive({
    
     mycols<-colnames(readData()[,-1])
  })
   
  # var1<- reactive({
     
   #  mycols1<-colnames(readData()[,-1])
     
 #  }) 
   
  
  
   
   procData <- reactive({
    
     plate0<-readData()
     plateTime<-plate0[,1]+input$delay
     
     
     if(input$blrate>0) backsub<-plateTime*input$blrate*1e-6
     else backsub<-plate0[,-1][,input$blank]
     
     
     plateabs<-plate0[,-1]
     #backsub<-plateabs[,input$blank]
     
     if(input$zero) for (j in 1:length(plateabs[1,])) {plateabs[,j]<-plateabs[,j]-plateabs[1,j]}
     else plateabs
     plateabs<-round(plateabs, 4)
     
     #top<-13:48 
     #btm<-top+36
     top<-input$north:(input$south-1)
     btm<-input$south:(97-input$north)
     Left<-input$west:(input$east-1)
     #Left<-1:6
     left<-c(Left, Left+12, Left+24, Left+36, Left+48, Left+60, Left+72, Left+84)
     right<-left+(input$east-input$west)
     #right<-left+6
     
     #n<-1
     switch(input$subc,
            "none"= if (input$blank>0 | input$blrate>0) plateabs<-plateabs-backsub
            else plateabs,
            
            "top-bottom"= if (input$blank>0 | input$blrate>0) plateabs[,top]<-(plateabs[,top]-plateabs[,btm])-backsub
            else plateabs[,top]<-plateabs[,top]-plateabs[,btm],
            
            "bottom-top"= if (input$blank>0 | input$blrate>0) plateabs[,btm]<-(plateabs[,btm]-plateabs[,top])-backsub
            else plateabs[,btm]<-plateabs[,btm]-plateabs[,top],
            
            "left-right" = if (input$blank>0 | input$blrate>0) plateabs[,left]<-(plateabs[,left]-plateabs[,right])-backsub
            else plateabs[,left]<-plateabs[,left]-plateabs[,right],
            
            "right-left" = if (input$blank>0 | input$blrate>0) plateabs[,right]<-(plateabs[,right]-plateabs[,left])-backsub
            else plateabs[,right]<-plateabs[,right]-plateabs[,left]
            
     )
     
     
     plateabs<-cbind(plateTime, plateabs)
     
   })
  
  

  output$what<-renderUI({
    
    selectInput("colmnames",
                label= h5("Data in 'Curve' tab"),
                choices = var())
  })


  output$myplot<-renderPlot({
    if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
   
    plate1<-procData()[,-1] #remove time
    Time<-procData()[,1]#+input$delay
    tsq<-Time^2
    
    if (input$sqr) tsq
    else (tsq<-Time)
    
    delta<-input$num
    yic<-plate1[,input$colmnames]
    Yi<-yic[yic<delta]
    Tsq<-tsq[1:length(Yi)]
    regrCol<-lm(Yi~Tsq)
    Res<-summary(regrCol)
    
    switch(input$curveRes,
           "Absorbance"= plot(Tsq,Yi,  pch=21, col="black", bg="red", cex=1.4, xlim=c(0,max(Tsq)), ylim=c(min(Yi),delta), xlab="Time", ylab="Absorbance", main=(if (input$sqr) paste(input$colmnames, "rate for", input$maxt, "data points and", input$num, "absorbance change =", signif(Res$coef[2]*1e9, digits=4), "Abs/s² x 1e9")
                                                                                                                                                          
                                                                                                                                                          else (paste(input$colmnames, "rate for", input$maxt, "data points and", input$num, "absorbance change =", signif(Res$coef[2]*1e6, digits=4), "Abs/s x 1e6"))), cex.lab=1.6, abline(regrCol, lwd=2, col="blue")),
           
           "Residual"= plot(residuals.lm(regrCol), pch=3, col= "red", main = paste("Residuals versus index; fit has adjusted R squared of ", signif(Res$adj.r.squared, digits=4)),  xlab="Time", ylab="Residuals", cex.lab=1.6)
           
    )
  })
  
  
  output$contents<-renderDataTable({
    procData()

  })


  
  setsTab<-reactive({
    
    setTable<-matrix(c(  
                        
                        "Date", format(Sys.Date(), "%d %b %Y"), "Time", format(Sys.time(), "%X"),
                        
                        "Read interval s", readData()[2,1]-readData()[1,1],  "Read delay s", input$delay,
                       
                       "Maximum points used", input$maxt,  "Maximum absorbance", input$num,
                      
                       "Time Squared", input$sqr,  "Zero abs at first point", input$zero,
                      
                       "Well for PKS subtraction", input$blank,  "Blank rate for PKS (1e-6 Abs/s)", input$blrate,
                      
                       #"Drop n initial points", input$drop, "Plate subtraction for no PKS", input$subc,
                       
                        "Plate subtraction for no PKS", input$subc, " "," " ,
                       
                       "Plate top", paste(input$north,":", input$south-1),"Plate bottom", paste(input$south,":",97-input$north),
                       
                       "Plate left", paste(input$west,":", input$east-1),"Plate right", paste(input$east,":",(input$east-1)+(input$east-input$west))
                       ),
                      
                       byrow=TRUE, nrow=8)
    
    colnames(setTable)<-c("Parameter", "Value", "Parameter", "Value")
    setTable
  })

  output$settings<-renderTable({
    if(is.null(input$colmnames)){return(NULL)}
    setTable<-setsTab()
    
    setTable
  }
    
  )
  
 
  output$resultsTable<-renderTable({
    
    if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
    
   
    plate1<-procData()[,-1]
    Time<-procData()[,1]#+input$delay
    tsq<-Time^2
    
    if (input$sqr) tsq
    else (tsq<-Time)
    
    delta<-input$num
    
    absCols=length(plate1)
    RowNum<-input$numrows 
    vect1<-1:absCols
    
    for(i in 1:absCols){ 
      yic<- plate1[,i]
      Yi<-yic[yic<delta]
      Tsq<-tsq[1:length(Yi)]
      regrCol<-lm(Yi~Tsq)
      Res<-summary(regrCol)
      if (input$sqr) ans<-Res$coef[2]*1e9 
      else ans<-Res$coef[2]*1e6
      vect1[i]<-round(ans, 4)
      
      
      
    }
    
   
    data<- matrix(vect1, byrow=TRUE, nrow=RowNum)
    #write.table(data, "clipboard", sep="\t", col.names=F, row.names=F) #enable this line if run locally
    colnames(data) =as.character(1:(absCols/RowNum))
    data
  })
  
  output$text3<-renderText({ print(switch(input$show,
                                               "results" = "Results",
                                               "well names"= "well names",
                                               "well numbers" = "well numbers"))
    
    })
  
  output$text4<-renderText({
    if (input$sqr) paste( "Rates Abs/s² x 1e9 for maximum absorbance ", input$num,"and", input$maxt, "data points")
    else (paste( "Rates Abs/s x 1e6 for maximum absorbance ", input$num, "and", input$maxt, "data points"))
    
  })
  
  output$myplotAll<-renderPlot({
    if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
    
    
    plate1<-procData()[,-1]
    Time<-procData()[,1]#+input$delay
    tsq<-Time^2

    if (input$sqr) tsq
    else (tsq<-Time)
    
    delta<-input$num
    
    absCols=length(plate1)
    RowNum<-input$numrows 
    
    vect1<-1:absCols
    wellnum<-1:96
    wellnames<-colnames(plate1)
  
    par(mfrow=c(RowNum,absCols/RowNum))
    par(mar=c(0.2,0.2,0.2,0.2))
    
    
    for(i in 1:absCols){ 
      yic<- plate1[,i]
      Yi<-yic[yic<delta]
      Tsq<-tsq[1:length(Yi)]
      regrCol<-lm(Yi~Tsq)
      plot(Tsq,Yi, col="red", pch=20,xaxt="n", yaxt="n", xlim=c(0,max(tsq)), ylim=c(min(Yi),delta))
      abline(regrCol)
      Res<-summary(regrCol)
      
              ans<-switch(input$show,
              "results" = ( if (input$sqr) signif(Res$coef[2]*1e9, digits=4)
                          else signif(Res$coef[2]*1e6, digits=4)),
             "well names"= wellnames[i],
             "well numbers" = wellnum[i])
      
      
      legend(0,delta,bty="n", ans)
      vect1[i]<-ans
      
      
    }
    
    
  })
}