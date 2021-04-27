source("./global.R")


ui <-  navbarPage("Anthro Brain", collapsible = TRUE, inverse = TRUE,theme = shinytheme("yeti"),
    tabPanel("Data Analysis",fluidPage(
    sidebarLayout(
    sidebarPanel(

      radioButtons("type", "Type of Data:",c("Single"="0","Cohort"="1"),selected = "0",inline = T, width = "100%"),
      radioButtons("sex", "Sex:",c("Female"="1","Male"="0"),selected = "1",inline = T, width = "100%"),
      conditionalPanel(condition = "input.type==1",fileInput("filedata", "Choose CSV File for Cohort Data",
                multiple = FALSE,
                accept = c(".csv"))),
      conditionalPanel(condition = "input.type==0", numericInput("age","Age (days)",0, min = 0, max = 6480)),
      conditionalPanel(condition = "input.type==0", numericInput("brain_vol","Brain Volume (cc)",0, min = 1, max = 25000)),
      conditionalPanel(condition = "input.type==0", numericInput("csf_vol","CSF Volume (cc)",0, min = 1, max = 25000)),
      conditionalPanel(condition = "input.type==1",downloadButton("downloadData", "Download Results")),
      div(style="margin-bottom:10px"),
      conditionalPanel(condition = "input.type==1",downloadButton("downloadPlot2", "Download Plots")),
      conditionalPanel(condition = "input.type==0 && input.brain_vol !=0 | input.csf_vol !=0",downloadButton("downloadData2", "Download Results")),
      div(style="margin-bottom:10px"),
      conditionalPanel(condition = "input.type==0 && input.brain_vol !=0 | input.csf_vol !=0",downloadButton("downloadPlot", "Download Plots")),
    )
    ,
    
  mainPanel(
               conditionalPanel(
                 condition = "input.brain_vol !=0 && input.type==0",
                 plotOutput("Plot1"),
                 h5("Brain Volume Z-score"),
                 textOutput("Zscore1")
               ),
               conditionalPanel(
                 condition =  "input.csf_vol!=0 && input.type==0",
                 plotOutput("Plot2"),
                 h5("CSF Volume Z-score"),
                 textOutput("Zscore2")
               ),
               conditionalPanel(
                 condition = "input.brain_vol !=0 && input.csf_vol!=0 && input.type==0",
                 plotOutput("Plot3"),
                 h5("Brain/CSF Ratio & Z-score"),
                 textOutput("Zscore3andRatio")
               ),
             
      conditionalPanel(condition="input.type==1",DTOutput(outputId = "table")),
               conditionalPanel(
                 condition = "input.type==1",
                 plotOutput("Plot4"),
               ),
               conditionalPanel(
                 condition =  "input.type==1",
                 plotOutput("Plot5"),
               ),
               conditionalPanel(
                 condition = "input.type==1",
                 plotOutput("Plot6"),
               )
                 )
    )
 )
  ),
 tabPanel("Instructions", fluidPage(
  tags$h4("This tool is meant to produce percentiled plots and z-scores for brain, cerebrospinal fluid (CSF), and the brain/CSF ratio using either single subject or cohort data.  The resulting plots show 3rd, 15th, 50th, 85th, and 97th percentiles (approximately 2 standard deviations above and below the median)."),
  tags$h1("Single Data Modality"),
  tags$h4("To use the single subject modality, click the single radio button, choose the sex, and manually fill in the age (in days) and brain and CSF volume (in cubic centimeters)."),
  tags$h1("Cohort Data Modality"),
  tags$h4("To use the cohort modality, click the cohort radio button and upload a .csv file containing a column titled 'Subject' (for subject numnber), a column titled 'Age' (containing subject ages in days),
        a column titled 'Brain' (with brain volume in cc), and a column titled 'CSF' (with CSF volume in cc).  Only subjects of the same sex can be analyzed at the same 
        time in the cohort modality.  Sex of the cohort can be chosen with the radio button before or after uploading the data. This modality can also be used to plot subjects over time."),
  tags$h1("Results"),
  tags$h4("The resulting plots and z-scores can be downloaded as pdf files and csv files by clicking the respective buttons for each."),
  div(style="margin-bottom:30px"),  
  tags$a(href="https://www.medrxiv.org/content/10.1101/2020.05.19.20102319v3","If using this tool for publication purposes, please cite the work of the Schiff lab at Penn State (linked here).",target="_blank"),
  tags$a(href="https://github.com/nutriverse/zscorer","A similar application, (zscorer-linked here), exists for calculating z-scores based on the reference WHO anthropomorphic measurements.",target="_blank")
  )
 )) 
  


server <- function(input, output) {

  sex_label <- reactive({
    if(input$sex == 1){
      modelS <- tissFF
      ageS <- FVff$ageff
      colorS <- "firebrick3"
      modelScsf <- csfFF
      modelSrat <- ratioF
      xnameS <- "ageff"
      sex_name<-"F"
    }else{
      modelS <- tissMM
      ageS <- MVmm$agemm
      colorS <- "dodgerblue3"
      modelScsf <- csfMM
      modelSrat <-  ratioM
      xnameS <- "agemm"
      sex_name<-"M"
    }
    list(modelS=modelS,ageS=ageS,colorS=colorS,modelScsf=modelScsf, modelSrat=modelSrat,xnameS=xnameS,sex_name=sex_name)
  })
  
  data <- reactive({
    req(input$filedata)
    inFile <- input$filedata
    Ndata <- read.csv(inFile$datapath)
    Ndata$Ratio <- Ndata$Brain/Ndata$CSF
    Ndata$Ratio <- round(Ndata$Ratio,digits=2)
    idff<-factor(Ndata$Subject)
    idmm<-factor(Ndata$Subject)
    Ndata$Sex <- c()
    Ndata$Brain_Zscore<- round(centiles.pred(sex_label()$modelS, type = c("z-scores"),xname =sex_label()$xnameS, xvalues = Ndata$Age, yval = Ndata$Brain),digits=2)
    Ndata$CSF_Zscore<- round(centiles.pred(sex_label()$modelScsf, type = c("z-scores"),xname =sex_label()$xnameS, xvalues =Ndata$Age, yval =  Ndata$CSF),digits=2)
    Ndata$Ratio_Zscore<- round(centiles.pred(sex_label()$modelSrat, type = c("z-scores"),xname =sex_label()$xnameS, xvalues =Ndata$Age, yval =  Ndata$Ratio),digits=2)
    for (i in 1:length(Ndata$Brain_Zscore))
     if (Ndata$Brain_Zscore[i]=="Inf"){Ndata$Brain_Zscore[i]=8.4}
     if (Ndata$Brain_Zscore[i]=="-Inf"){Ndata$Brain_Zscore[i]=-8.4}
     if (Ndata$CSF_Zscore[i]=="Inf"){Ndata$Brain_Zscore[i]=8.4}
     if (Ndata$CSF_Zscore[i]=="-Inf"){Ndata$Brain_Zscore[i]=-8.4}
     if (Ndata$Ratio_Zscore[i]=="Inf"){Ndata$Brain_Zscore[i]=8.4}
     if (Ndata$Ratio_Zscore[i]=="-Inf"){Ndata$Brain_Zscore[i]=-8.4}
    for (i in 1:length(Ndata$Brain_Zscore))
      Ndata$Sex[i]=sex_label()$sex_name
    Ndata
    })
   
    output$table <- renderDT(data())
  
  ratio <- reactive({
    if(input$brain_vol != 0){
      B_F<- round(input$brain_vol/input$csf_vol,digits=2)
    }else{
      B_F<- 0
    }
    list(B_F=B_F)
  })
  
  output$Plot1 <- renderPlot({
    centiles(sex_label()$modelS,sex_label()$ageS, cent, legend=FALSE, main = "Brain Volume", xlim = c(0,6570), ylim = c(200,1800),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save =FALSE, plot = TRUE, points = FALSE, pch = 13, cex = 0.2, col = sex_label()$colorS, col.centiles = sex_label()$colorS, lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
    points(input$age,input$brain_vol, pch=19,cex=2)
    grid(nx=18,ny=8)
    axis(2,at=c(200,400,600,800,1000,1200,1400,1600),labels=c("200","400","600","800","1000","1200","1400","1600"),ylab="N")
    axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
    title(ylab = "Volume (cc)", xlab = "Age (years)")})
  idff<-factor(c(1,2))

  output$Plot2 <- renderPlot({
    centiles(sex_label()$modelScsf,sex_label()$ageS, cent, legend=FALSE, main = "CSF Volume", xlim = c(0,6570), ylim = c(0,350),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save =FALSE, plot = TRUE, points = FALSE, pch = 13, cex = 0.2, col = sex_label()$colorS, col.centiles = sex_label()$colorS, lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
    points(input$age,input$csf_vol, pch=19,cex=2)
    grid(nx=18,ny=NULL)
    axis(2,at=c(0,100,200,300),labels=c("0","100","200","300"),ylab="N")
    axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
    title(ylab = "Volume (cc)", xlab = "Age (years)")})

  
  output$Plot3 <- renderPlot({
    centiles(sex_label()$modelSrat,sex_label()$ageS, cent, legend=FALSE, main = "Brain/CSF Ratio", xlim = c(0,6570), ylim = c(2,12),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save =FALSE, plot = TRUE, points = FALSE, pch = 13, cex = 0.2, col = sex_label()$colorS, col.centiles = sex_label()$colorS, lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
    points(input$age,ratio()$B_F, pch=19,cex=2)
    grid(nx=18,ny=NULL)
    axis(2,at=c(2,4,6,8,10,12),labels=c("2","4","6","8","10","12"),ylab="N")
    axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
    title(ylab = "Ratio", xlab = "Age (years)")})

  zscoreInf <- reactive({
    S <- sex_label()$sex_name
    BV <- input$brain_vol
    CV <- input$csf_vol
    RV <- ratio()$B_F
    B <- round(centiles.pred(sex_label()$modelS, type = c("z-scores"),xname =sex_label()$xnameS, xvalues =input$age, yval = input$brain_vol),digits=2)
    if (B=="Inf"){B=8.4}
    if (B=="-Inf"){B=-8.4}
    C <- round(centiles.pred(sex_label()$modelScsf, type = c("z-scores"),xname =sex_label()$xnameS, xvalues =input$age, yval = input$csf_vol),digits=2)
    if (C=="Inf"){C=8.4}
    if (C=="-Inf"){C=-8.4}
    R <- round(centiles.pred(sex_label()$modelSrat, type = c("z-scores"),xname =sex_label()$xnameS, xvalues =input$age, yval = ratio()$B_F),digits=2)
    if (R=="Inf"){R=8.4}
    if (R=="-Inf"){R=-8.4}
    list(Sex=S,Brain=BV,CSF=CV,Ratio=RV,Brain_Zscore=B,CSF_Zscore=C,Ratio_Zscore=R)
  })
  
  output$Zscore1<- renderText({zscoreInf()$Brain_Zscore})
  output$Zscore2 <- renderText({zscoreInf()$CSF_Zscore})
  output$Zscore3andRatio<- renderText({c(round(ratio()$B_F,digits=2),' & ',zscoreInf()$Ratio_Zscore)})

  
  output$Plot4 <- renderPlot({
    centiles(sex_label()$modelS,sex_label()$ageS, cent, legend=FALSE, main = "Brain Volume", xlim = c(0,6570), ylim = c(200,1800),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save =FALSE, plot = TRUE, points = FALSE, pch = 13, cex = 0.2, col = sex_label()$colorS, col.centiles = sex_label()$colorS, lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
    points(data()$Age,data()$Brain, pch=1,cex=2)
    text(data()$Age,data()$Brain, labels=data()$Subject,data=data(), cex=0.9, font=2)
    grid(nx=18,ny=8)
    axis(2,at=c(200,400,600,800,1000,1200,1400,1600),labels=c("200","400","600","800","1000","1200","1400","1600"),ylab="N")
    axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
    title(ylab = "Volume (cc)", xlab = "Age (years)")})

  output$Plot5 <- renderPlot({
    centiles(sex_label()$modelScsf,sex_label()$ageS, cent, legend=FALSE, main = "CSF Volume", xlim = c(0,6570), ylim = c(0,350),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save =FALSE, plot = TRUE, points = FALSE, pch = 13, cex = 0.2, col = sex_label()$colorS, col.centiles = sex_label()$colorS, lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
    points(data()$Age,data()$CSF, pch=1,cex=2)
    text(data()$Age,data()$CSF, labels=data()$Subject,data=data(), cex=0.9, font=2)
    grid(nx=18,ny=NULL)
    axis(2,at=c(0,100,200,300),labels=c("0","100","200","300"),ylab="N")
    axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
    title(ylab = "Volume (cc)", xlab = "Age (years)")})

  
  output$Plot6 <- renderPlot({
    centiles(sex_label()$modelSrat,sex_label()$ageS, cent, legend=FALSE, main = "Brain/CSF Ratio", xlim = c(0,6570), ylim = c(2,12),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save =FALSE, plot = TRUE, points = FALSE, pch = 13, cex = 0.2, col = sex_label()$colorS, col.centiles = sex_label()$colorS, lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
    points(data()$Age,data()$Ratio, pch=1,cex=2)
    text(data()$Age,data()$Ratio, labels=data()$Subject,data=data(), cex=0.9, font=2)
    grid(nx=18,ny=NULL)
    axis(2,at=c(2,4,6,8,10,12),labels=c("2","4","6","8","10","12"),ylab="N")
    axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
    title(ylab = "Ratio", xlab = "Age (years)")})

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Results",input$filedata, sep = "_")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("Results","csv", sep = ".")
    },
    content = function(file) {
      write.csv(zscoreInf(), file, row.names = FALSE)
    }
  )
  
  
  output$downloadPlot <- downloadHandler(
    filename =  function() {
      paste("Result",'pdf', sep=".")
    },
    content = function(file) {
      pdf(file)
      if (input$brain_vol!=0){
      centiles(sex_label()$modelS,sex_label()$ageS, cent, legend=FALSE, main = "Brain Volume", xlim = c(0,6570), ylim = c(200,1800),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save =FALSE, plot = TRUE, points = FALSE, pch = 13, cex = 0.2, col = sex_label()$colorS, col.centiles = sex_label()$colorS, lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
      points(input$age,input$brain_vol, pch=19,cex=2)
      grid(nx=18,ny=8)
      axis(2,at=c(200,400,600,800,1000,1200,1400,1600),labels=c("200","400","600","800","1000","1200","1400","1600"),ylab="N")
      axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
      title(ylab = "Volume (cc)", xlab = "Age (years)")}
      if (input$csf_vol!=0){
        centiles(sex_label()$modelScsf,sex_label()$ageS, cent, legend=FALSE, main = "CSF Volume", xlim = c(0,6570), ylim = c(0,350),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save =FALSE, plot = TRUE, points = FALSE, pch = 13, cex = 0.2, col = sex_label()$colorS, col.centiles = sex_label()$colorS, lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
        points(input$age,input$csf_vol, pch=19,cex=2)
        grid(nx=18,ny=NULL)
        axis(2,at=c(0,100,200,300),labels=c("0","100","200","300"),ylab="N")
        axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
        title(ylab = "Volume (cc)", xlab = "Age (years)")}
  if (input$brain_vol!=0 && input$csf_vol!=0){
    centiles(sex_label()$modelSrat,sex_label()$ageS, cent, legend=FALSE, main = "Brain/CSF Ratio", xlim = c(0,6570), ylim = c(2,12),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save =FALSE, plot = TRUE, points = FALSE, pch = 13, cex = 0.2, col = sex_label()$colorS, col.centiles = sex_label()$colorS, lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
    points(input$age,ratio()$B_F, pch=19,cex=2)
    grid(nx=18,ny=NULL)
    axis(2,at=c(2,4,6,8,10,12),labels=c("2","4","6","8","10","12"),ylab="N")
    axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
    title(ylab = "Ratio", xlab = "Age (years)")}
  dev.off()
      })
  output$downloadPlot2 <- downloadHandler(
    filename =  function() {
      paste("Results","pdf",sep=".")
    },
    content = function(file) {
      pdf(file) 
        centiles(sex_label()$modelS,sex_label()$ageS, cent, legend=FALSE, main = "Brain Volume", xlim = c(0,6570), ylim = c(200,1800),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save =FALSE, plot = TRUE, points = FALSE, pch = 13, cex = 0.2, col = sex_label()$colorS, col.centiles = sex_label()$colorS, lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
        points(data()$Age,data()$Brain, pch=1,cex=2)
        text(data()$Age,data()$Brain, labels=data()$Subject,data=data(), cex=0.9, font=2)
        grid(nx=18,ny=8)
        axis(2,at=c(200,400,600,800,1000,1200,1400,1600),labels=c("200","400","600","800","1000","1200","1400","1600"),ylab="N")
        axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
        title(ylab = "Volume (cc)", xlab = "Age (years)")
        centiles(sex_label()$modelScsf,sex_label()$ageS, cent, legend=FALSE, main = "CSF Volume", xlim = c(0,6570), ylim = c(0,350),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save =FALSE, plot = TRUE, points = FALSE, pch = 13, cex = 0.2, col = sex_label()$colorS, col.centiles = sex_label()$colorS, lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
        points(data()$Age,data()$CSF, pch=1,cex=2)
        text(data()$Age,data()$CSF, labels=data()$Subject,data=data(), cex=0.9, font=2)
        grid(nx=18,ny=NULL)
        axis(2,at=c(0,100,200,300),labels=c("0","100","200","300"),ylab="N")
        axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
        title(ylab = "Volume (cc)", xlab = "Age (years)")
        centiles(sex_label()$modelSrat,sex_label()$ageS, cent, legend=FALSE, main = "Brain/CSF Ratio", xlim = c(0,6570), ylim = c(2,12),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save =FALSE, plot = TRUE, points = FALSE, pch = 13, cex = 0.2, col = sex_label()$colorS, col.centiles = sex_label()$colorS, lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
        points(data()$Age,data()$Ratio, pch=1,cex=2)
        text(data()$Age,data()$Ratio, labels=data()$Subject,data=data(), cex=0.9, font=2)
        grid(nx=18,ny=NULL)
        axis(2,at=c(2,4,6,8,10,12),labels=c("2","4","6","8","10","12"),ylab="N")
        axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
        title(ylab = "Ratio", xlab = "Age (years)")
      dev.off()})

  }


shinyApp(ui, server)




