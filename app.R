library(shiny)
library(clmp)

default.tree <- "(((((T1_I_0:0.1005429103,(T2_I_0:0.1271288955,(T3_I_0:0.1457696781,T4_I_0:0.1142013539):0.004083799555):0.004881525322):0.0350056351,T5_I_0:0.1378446003):0.003750869692,(T6_I_0:0.1595083623,T7_I_0:0.1673446392):0.01053775728):0.004127849713,((((((T8_I_1:0.08372023511,(T9_I_1:0.1164518268,T10_I_1:0.06067705821):0.01415676431):0.005072340838,((T11_I_1:0.06994864007,T12_I_1:0.07330843202):0.004214991057,((T13_I_1:0.1252934067,(T14_I_1:0.1006116975,T15_I_1:0.09393882783):0.002697183799):0.003131717785,T16_I_1:0.09081271531):0.0003876637767):0.009594121983):0.005620570188,T17_I_1:0.1028883675):0.01259861286,(T18_I_1:0.1267226481,((T19_I_1:0.1130253127,T20_I_1:0.03826953865):0.0004886024334,T21_I_1:0.09900997709):0.00352699017):0.007141408251):0.002010154624,(((T22_I_1:0.1087418638,T23_I_1:0.1214572114):0.00139366348,(T24_I_1:0.01148161741,T25_I_1:0.05680770018):0.01296854835):0.009636462456,((T26_I_1:0.1093645945,T27_I_1:0.06230140684):0.0008447588462,T28_I_1:0.119783223):0.008485612411):0.01563026945):0.00170789972,((T29_I_1:0.1115219584,(((((T30_I_1:0.0919804977,(T31_I_1:0.01831967784,T32_I_1:0.05483690288):0.01500562171):0.001731538964,T33_I_1:0.07441146687):0.004363951195,T34_I_1:0.03633506194):0.01657402761,T35_I_1:0.08589029845):0.004906899101,T36_I_1:0.1246623618):0.004379468653):0.004238738005,((T37_I_1:0.1267530271,T38_I_1:0.09065722029):0.01217206716,T39_I_1:0.09917571931):0.007322372282):0.00962804166):0.03884125972):0.04715494044,(((((T40_I_0:0.138976737,T41_I_0:0.1357431295):0.05538512111,((((((T42_I_0:0.08755817937,T43_I_0:0.08706136217):0.002204403494,T44_I_0:0.08235689562):0.04533164309,T45_I_0:0.149604517):0.006877071498,(((T46_I_0:0.01575518441,T47_I_0:0.04096511144):0.08534674997,((T48_I_0:0.1192680601,T49_I_0:0.1446040589):0.008395110135,(((T50_I_0:0.1202276016,T51_I_0:0.1095926825):0.003459634304,(T52_I_0:0.1139773835,T53_I_0:0.1031011433):0.006589820262):0.01249100408,T54_I_0:0.1314471573):0.02001451988):6.328403383e-05):0.01396884616,T55_I_0:0.1063463989):0.005632079335):0.009727178957,(T56_I_0:0.1062985599,T57_I_0:0.1067445318):0.05699517329):0.0007640665602,(T58_I_0:0.1189598538,T59_I_0:0.03893871718):0.06988871343):0.02172420987):0.01002646509,(((T60_I_0:0.1375603619,T61_I_0:0.1303786598):0.0311349214,(T62_I_0:0.135017591,T63_I_0:0.1606026516):0.01583312033):0.0137567787,(T64_I_0:0.05727984653,T65_I_0:0.09937631494):0.07861042359):0.02666715957):0.00188772249,((T66_I_0:0.08923557379,T67_I_0:0.1051851602):0.06670187001,T68_I_0:0.1176855276):0.04458935306):0.003718710331,((((((T69_I_0:0.09182219132,T70_I_0:0.08941246718):0.03278451569,T71_I_0:0.1343133469):0.004481859859,(T72_I_0:0.09149450092,T73_I_0:0.1202729745):0.01961508088):0.0465752121,(T74_I_0:0.1180438372,T75_I_0:0.1225026033):0.05291591764):0.004040073655,(T76_I_0:0.1547344746,(((T77_I_0:0.1123243053,T78_I_0:0.05284458542):0.02061389784,(T79_I_0:0.1373971683,(T80_I_0:0.1066457904,T81_I_0:0.07909934787):0.03117642673):0.003551849299):0.004237227706,(T82_I_0:0.04864766987,T83_I_0:0.07183495571):0.07184723976):0.04142929935):0.00378895171):0.007149677526,((((T84_I_0:0.120730524,T85_I_0:0.1247008614):0.0232659053,(((T86_I_0:0.05004930838,T87_I_0:0.0248884179):0.08272118242,T88_I_0:0.03350589561):0.006876961516,(T89_I_0:0.09528929882,T90_I_0:0.1015030514):0.04378860007):0.007187623595):0.003586197422,(T91_I_0:0.0989121395,T92_I_0:0.1377726875):0.01691323426):0.01056856529,(T93_I_0:0.03823013525,((T94_I_0:0.02100355895,T95_I_0:0.06448456041):0.03358097762,T96_I_0:0.09574256091):0.02893682008):0.03944927439):0.02473022594):0.02182935105):0.02114672067):0.01024037781;"


ui <- fluidPage(
  #shinythemes::themeSelector(),
  #theme="united",
  
  tags$head(
    tags$style(
      HTML(
        'textarea{font-family: monospace; font-size: 9px;}'
        )
      )
    ),
  
  titlePanel("clmp: clustering with Markov-modulated Poisson processes"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Do not submit trees labeled with any potentially 
        identifying information."),
      textAreaInput(
        inputId="newick", 
        label="Input tree (Newick format)",
        height='400px',
        value=default.tree
        ),
      fileInput("nwkFile", "Upload Newick", multiple=FALSE, accept=c('text/plain')),
      actionButton(inputId="actButton", label="Submit", icon=icon("check"))
    ),
    
    mainPanel(
      h4("Tree plot:"),
      plotOutput(outputId="clmpPlot"), #, height='500px'),
      h4("Summary:"),
      textOutput("summary"),
      #h4("Model selection:"),
      textOutput("dAIC"),
      downloadButton("csv", label="Download results (CSV)"),
      br(),
      h5("If you use clmp in your work, please cite:"),
      helpText("McCloskey RM, Poon AF. A model-based clustering method to detect 
        infectious disease transmission outbreaks from sequence variation. 
        PLoS Comput Biol. 2017 Nov 13;13(11):e1005868.")
    )
  )
)

server <- function(input, output, session) {
  #output$treelength <- renderText({
  #  phy <- read.tree(text=input$newick)
  #  phy$Nnode
  #})
    v <- eventReactive(input$actButton, {
      phy <- read.tree(text=input$newick)
      res1 <- clmp(phy, nrates=1)
      res2 <- clmp(phy, nrates=2)
      list(phy=phy, res1=res1, res2=res2)
    })
    
    v <- eventReactive(input$nwkFile, {
      if (is.null(input$nwkFile)) return(NULL)
      phy <- read.tree(input$nwkFile)
      updateTextInput(session, "newick", value=write.tree(phy))
      list(phy=phy)
    })
    
    output$clmpPlot <- renderPlot({
      plot(v()$res2)
    })
    
    
    output$dAIC <- renderText({
      dAIC <- round(AIC(v()$res2)-AIC(v()$res1), digits=5)
      if (dAIC < 0) {
        paste("dAIC =", dAIC, " (AIC favours 2-rate class model)")
      } else if (dAIC > 0) {
        paste("dAIC =", dAIC, " (AIC favours 1-rate class model)")
      } else {
        # very unlikely to happen
        paste("dAIC =", dAIC, " (models have identical AIC!)")
      }
    })
    
    output$summary <- renderText({
      clu <- v()$res2$clusters
      n.clusters <- max(clu)
      
      # restrict to tips
      clu2 <- clu[is.element(names(clu), v()$res2$tip.label)]
      
      n.in.clusters <- sum(clu2>0)
      n <- length(clu2)
      
      paste("Detected ", n.clusters, " cluster", 
            ifelse(n.clusters==1, ". ", "s. "),
            n.in.clusters, " out of ", n, 
            " individuals assigned to clusters.",
            sep='')
    })
    
    output$csv <- downloadHandler(
      filename = paste('clmp-', Sys.Date(), '.csv', sep=''),
      content = function(con) {
        cls <- v()$res2$clusters
        cls <- cls[is.element(names(cls), v()$res2$tip.label)]
        df <- data.frame(
          label=names(cls)[cls>0],
          cluster=cls[cls>0]
        )
        df <- df[order(df$cluster),]
        write.csv(df, con, row.names=F)
    })
    #outputOptions(output, "clmpPlot", priority=10)
}

shinyApp(ui, server)