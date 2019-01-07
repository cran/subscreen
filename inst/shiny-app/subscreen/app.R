# load required libraries

if (!requireNamespace("shiny", quietly = TRUE)) {
  cat("Error: subscreenshow requires the package shiny to be installed")
  stop()
}
if (!requireNamespace("shinyjs", quietly = TRUE)) {
  cat("Error: subscreenshow requires the package shinyjs to be installed")
  stop()
}
if (!requireNamespace("bsplus", quietly = TRUE)) {
  cat("Error: subscreenshow requires the package bsplus to be installed")
  stop()
}
if (!requireNamespace("V8", quietly = TRUE)) {
  cat("Error: subscreenshow requires the package V8 to be installed")
  stop()
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("Error: subscreenshow requires the package jsonlite to be installed")
  stop()
}
if (!requireNamespace("colourpicker", quietly = TRUE)) {
  cat("Error: subscreenshow requires the package colourpicker to be installed")
  stop()
}
if (!requireNamespace("DT", quietly = TRUE)) {
  cat("Error: subscreenshow requires the package DT to be installed")
  stop()
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  cat("Error: subscreenshow requires the package dplyr to be installed")
  stop()
}


library(shiny)
library(shinyjs)
library(bsplus)
library(V8)
library(jsonlite)
library(colourpicker)
library(DT)
library(dplyr)


PreSelectXAxis <- "N.of.subjects"
PreSelectTarget <- ""


####.. functions to grey out tabs####

jscode <- "shinyjs.disableTab = function(name) {
var tab = $('.nav li a[data-value='+name+']');
tab.bind('click.tab', function(e) {
e.preventDefault();
return false;
});
tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
var tab = $('.nav li a[data-value='+name+']');
tab.unbind('click.tab');
tab.removeClass('disabled');
}
"

css <- ".nav li a.disabled {
background-color: #aaa !important;
color: #333 !important;
cursor: not-allowed !important;
border-color: #aaa !important;
}"

createCombinationMatrix <- function(n, k, l) {
  t(do.call(cbind, lapply(k:l, function(x) utils::combn(n, x, tabulate, nbins = n))))
}

is.integer0 <- function(x){is.integer(x) && length(x) == 0L}

parents <- function(data,SGID){
  
  if(is.null(SGID) | is.integer0(SGID)){}else{
    
    Parents_start <- NULL
    for(k in 1:length(SGID)){
      start <- data$sge[SGID[k],]
      
      if(start$nfactors>1){
        
        M1 <- as.data.frame(createCombinationMatrix(start$nfactors,start$nfactors-1,start$nfactors-1))
        
        act_val <- intersect(colnames(start[,(start != "Not used" | is.na(start))]),data$factors)
        colnames(M1) <- act_val
        
        M2 <- as.data.frame(start[act_val])
        
        
        for(j in 1:length(act_val)){
          for(i in 1:dim(M1)[1]){
            if(eval(parse(text=paste0('M1','$',act_val[i])))[j] == 1){
              M1[j,i] <- as.character(eval(parse(text=paste0('M2','$',act_val[i]))))
            }else{
              M1[j,i] <- "Not used"
            }
          }
        }
        
        M1$'nfactors'<- start$nfactors-1
        
        M <- data$sge[,intersect(colnames(M1),colnames(data$sge))]
        
        Parents <- NULL
        for(i in 1:dim(M1)[1]){
          Parents <- rbind(Parents,data$sge[eval(parse(text =c(paste0('M[,',1:(start$nfactors),']==M1[',i,',',1:(start$nfactors),'] &'),
                                                               paste0('M[,',start$nfactors+1,']==M1[',i,',',(start$nfactors+1),'] ')))),])
        }
        Parents_start <- rbind(Parents_start,Parents)
      }
    }
    return(list('Parents' = Parents_start))
  }
}



#### SGEAPP ####


  
  
  #### UI ####
  ui = navbarPage(
      ####......logo ####
      imageOutput("image2",width="auto",height="20px"),    
      
      #####SUBSCREEN EXPLORER TAB####
      tabPanel("Subscreen Explorer", value=1,           
               
               ####...... tags style navbar (cont_nav) ####
               uiOutput('cont_nav'),
               
               shiny::fluidPage( 
                 ####...... tags style (cont) ####
                 uiOutput('cont'),
                 ####...... tags style icon color (cont2)####
                 uiOutput('cont2'),         
                 
                 fluidRow(
                   column(3,
                          ####VARIABLE OPTIONS TAB####
                          tabsetPanel(type = "tabs",     
                                      tabPanel("Variable Options", 
                                               
                                               ####.. wellpanel variable ####
                                               uiOutput('Panel_Variable')
                                               ,icon=icon("wrench")),
                                      
                                      #### IMPORTANCE TAB ####
                                      tabPanel("Importance Tab", value = "ImportanceTab", 
                                               
                                               #### .. wellpanel importance ####
                                               uiOutput('Panel_Importance'),
                                               ## For grey Out Tabs
                                               useShinyjs(),
                                               extendShinyjs(text = jscode),
                                               inlineCSS(css)
                                               ,icon=icon("exclamation")),
                                      
                                      #### DISPLAY OPTIONS TAB ####
                                      tabPanel("Display Options",
                                               
                                               ####.. wellpanel display1 ####
                                               uiOutput('Panel_Display1'),   
                                               ####.. wellpanel display2 ####
                                               uiOutput('Panel_Display2')
                                               ,icon=icon('eye')),
                                      
                                      #### COLOUR OPTIONS TAB####
                                      tabPanel("Colour Options",
                                               ####.. select color uioutput ####
                                               uiOutput('select_col'),
                                               ####......wellpanel colour ####
                                               uiOutput('Panel_Colour') 
                                               ,icon=icon("paint-brush"))
                          )
                   ),
                   
                   #### .. graph1 ####
                   column(9, 
                          shiny::plotOutput("graph", click = "plot_click", 
                                            height = 700, width = 1100)
                   )
                 ),
                 
                 fluidRow(column(12, 
                                 ####..Table Output #### 
                                 tabsetPanel(type = "tabs", 
                                             tabPanel("Selected Subgroups", 
                                                      DT::dataTableOutput("selectedSG"),
                                                      icon = icon("circle")), 
                                             shiny::tabPanel("Filtered Subgroups", 
                                                             DT::dataTableOutput("filteredSG"),
                                                             icon=icon("filter")), 
                                             shiny::tabPanel("Parent Subgroups",value = "ParentSubgroup",
                                                             DT::dataTableOutput("parents"),
                                                             icon=icon("sitemap")),                                    
                                             shiny::tabPanel("Memorized Subgroups",
                                                             DT::dataTableOutput("memorizedSG"),
                                                             icon=icon("edit")))))),
               fluid = FALSE, position = c("static-top"), inverse = FALSE, icon = icon("braille")),            
      
      #### SUBSCREEN COMPARER TAB ####
      tabPanel("Subscreen Comparer", value = 2,                                                
               
               fluidRow(column(3,
                               ####.. panel variable 2 ####             
                               uiOutput('Panel_Variable2'),
                               ####.. panel variable 3 ####     
                               uiOutput('Panel_Variable3'),
                               ####.. panel variable 4 ####     
                               uiOutput("Panel_Variable4"),
                               ####.. panel variable 5 ####     
                               uiOutput("Panel_Variable5")
               ),
               
               mainPanel(                        
                 tabsetPanel(type = "tabs",     
                             ####.. graph2 ####
                             tabPanel("Compare",column(9,plotOutput("graph2", 
                                                                    click = "plot_click",
                                                                    height = 390, width = 1100)),
                                      ####.. graph2 ####
                                      column(9,plotOutput("graph3",
                                                          click = "plot_click2",
                                                          height = 390, width = 1100))),
                             ####.. bubble plot####
                             tabPanel("Bubble plot",column(9,plotOutput("graph4", 
                                                                        click = "plot_click3",
                                                                        height = 780, width = 1100)))
                             
                 ) 
               )
               ),icon=icon("object-group"))
      
      #### SUBSCREEN MOSAIC #####
      
      ,tabPanel("Subscreen Mosaic", value="SubscreenMosaic",           
                fluidPage(
                  fluidRow(
                    column(3,
                           ####.. panel mosaic####  
                           uiOutput('PanelMosaic')
                           
                    ),
                    
                    column(8,
                           ####.. plot mosaic####
                           plotOutput("mosaic",
                                      hover = hoverOpts(id='plot_hover', delay=500, delayType='debounce'), 
                                      height = 550, width=750),
                           br(),
                           ####.. table mosaic####
                           DT::dataTableOutput("tmp_info")
                           
                    )
                    
                  )
                )
                ,icon=icon("th-list"))    
      
    ) 
  
  #### SERVER #### 
  server = function(input, output, session) {
    ####.. tags styles for ui ####
    ####...... cont#####
    
    
    if (exists("apppars")) {
      scrresults <- apppars$scrresults
      variable_importance <- apppars$variable_importance
      NiceNumbers <- apppars$NiceNumbers
    }
    
    if (!exists("apppars")) {
      if (file.exists("scresults.rds")) {
        scresults <- readRDS(file="scresults.rds")
        cat("Note: Using scresults.rds from app folder")
      }
      else {
        cat("Error: Subscreenresults are missing")
        stop()
      }
    }
    
    if (!exists("apppars")) {
      if (file.exists("scrimportance.rds")) {
        variable_importance <- readRDS(file="scrimportance.rds")
        cat("Note: Using scrimportance.rds from app folder")
      }
      else {
        variable_importance = NULL
      }
    }
    
    if (!exists("apppars")) {
      NiceNumbers = c(1, 1.5, 2, 4, 5, 6, 8, 10)
    }
    
    roundUpNice <- function(x, nice = NiceNumbers) {
      if (length(x) != 1) stop("'x' must be of length 1")
      if (x >= 0) 10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) *nice)[[1]]]]
      else -1 * (roundDownNice(-x, nice = NiceNumbers))
    }
    
    roundDownNice <- function(x, nice = NiceNumbers) {
      if (length(x) != 1) stop("'x' must be of length 1")
      if (x >= 0) 10^floor(log10(x)) * nice[[max(which(x >= 10^floor(log10(x)) * nice))]]
      else -1 * (roundUpNice(-x, nice = NiceNumbers))
    }
    
    
    
    output$cont <- renderUI({
      list(shiny::tags$head(shiny::tags$style(paste("body {background-color: ", colthemeCol$col.bg, "; color: ", colthemeCol$font.col, "}", sep = ""))))
    })
    ####...... cont2#####
    output$cont2 <- renderUI({
      shiny::tags$head(shiny::tags$style(paste0(".fa-bug {color:#D30F4B}", ".fa-th-list {color:grey}",
                                                ".fa-circle {color:",colthemeCol$ColorClicked,"}","fa-info-circle {color:#DE0043FF}",".fa-filter {color:",colthemeCol$ColorSelected,"}",".fa-delicious {color:#00aaff}", ".fa-braille {color: grey}",     
                                                ".fa-sitemap {color: ",colthemeCol$ColorParents,"}",".fa-edit {color:#00aaff}",".fa-object-group {color: grey}",                                      
                                                shiny::HTML(".selectize-input.input-active, .selectize-input.input-active:hover, .selectize-control.multi .selectize-input.focus {border-color: red !important;}\n                                     .selectize-dropdown .active {background: #FF3162FF !important;}") ,sep=","))) 
    })
    ####...... cont_nav#####
    output$cont_nav <- renderUI({
      shiny::tags$head(shiny::tags$style(paste0(".navbar { background-color:",colthemeCol$col.bg," ;font-family: Arial;font-size: 15px; color: ",colthemeCol$font.col,"; }',
                                                '.navbar-default .navbar-brand {
                                                color: ",colthemeCol$font.col,";
                                                font-size: 40px;
                                                font-family: Arial;}")))
  })
    
    #### PANEL VARIABLE OPTIONS I (1)####
    ####..  PANEL ####
    output$Panel_Variable <- renderUI({
      
      style.panel <- paste('background-color: ',colthemeCol$panel.col,';padding: 9px;')
      
      wellPanel(style = style.panel,
                
                ## Different help text implementations:
                
                # div(style = "position:absolute;right:2em;",
                #     bs_embed_tooltip(tag = shiny_iconlink("question "),
                #                      title = " Choose a variable for the y-Axis.", placement = "top",expanded =TRUE)
                # ),
                # ####...... y #####        
                # selectInput("y", label='Target variable', 
                #             names(scresults$results_total), selected =start_y$val), 
                
                bs_embed_tooltip(tag = h5('Target variable'),
                                 title = " Choose a variable for the y-Axis.", placement = "top",expanded =TRUE), 
                ####...... y #####        
                selectInput("y", label=NULL, 
                            names(scresults$results_total), selected =start_y$val), 
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question"),
                                     title = " Choose a variable for the x-Axis.", placement = "top",expanded =TRUE) 
                ),
                ####...... x ####     
                selectInput("x", "Reference variable", names(scresults$results_total), selected = start_x$val ),
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question "),
                                     title = "Choose a filter variable. Filtered variable shown will shown in green and in the 
                                     filter tab below", placement = "top")
                    ),
                ####...... filter####     
                selectInput("filter","Subgroup Filter", c("no selection", scresults$factors), selected = start_filter$val),
                
                use_bs_popover(),
                use_bs_tooltip(),
                ####...... variable chosen####    
                
                shiny::conditionalPanel("input.filter != 'no selection'", 
                                        uiOutput("VarChosen")
                                        , selectize = FALSE
                ), 
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question "),
                                     title = "Adjust the Subgroups which are displayed in the plot", placement = "top",expanded =TRUE) 
                ),
                ####...... subgroup levels####     
                shiny::sliderInput("key","Subgroup level(s)", min = scresults$min_comb, max = scresults$max_comb,
                                   ticks = FALSE, value = key_val$range, step = 1),
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_popover(tag = shiny_iconlink("question"),
                                     title = "Explanation:", content = "Choose scale typ", placement = "top")
                ),
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question "),
                                     title = "Change the y-axis into a logarithmic scale", placement = "top",expanded =TRUE) 
                ),
                ####....... linear/log type ####
                radioButtons("plot_type",
                             label = "Plot Type",
                             selected = start_plot_type$val, inline = TRUE
                             ,choiceNames = list("linear", "logarithmic"), 
                             choiceValues = c("lin", "log")),
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question "),
                                     title = " Change the size of the y-axis.", placement = "top",expanded =TRUE) 
                ),
                #uiOutput("YRange"),
                ####...... y range plot####
                sliderInput("YRange","Y Range", min = roundDownNice(min(scresults$sge[, start_y$val], na.rm = TRUE)),
                            max = roundUpNice(max(scresults$sge[, start_y$val], na.rm = TRUE)), value = start_yrange$val,
                            step = roundUpNice((max(scresults$sge[, start_y$val], na.rm = TRUE) - min(scresults$sge[, start_y$val], na.rm = TRUE))/100))
                ,
                
                ####...... help texts####       
                "Help text:", 
                bs_embed_tooltip(tag = shiny_iconlink("circle"),
                                 title = "Click on the points in the plot to select Subgroups", placement = "top"),
                bs_embed_tooltip(tag = shiny_iconlink("filter"),
                                 title = "Choose a filter variable to get a list of all filtered Subgroups", placement = "top"),
                bs_embed_tooltip(tag = shiny_iconlink("sitemap"),
                                 title = "Click on the List in the 'Selected Subgroups' tab to display the parent
                                 subgroups", placement = "top"),
                bs_embed_tooltip(tag = shiny_iconlink("edit"),
                                 title = "Use the Memorize Button in the 'Selected Subgroups' tab to memorize Subgroups", placement = "top")
                )
    })
    
    ####..REACTIVE VALUES AND OBSERVE EVENTS####
    ####......start_y$val####
    start_plot_type <- reactiveValues( val = "lin")
    
    observeEvent(input$plot_type,{    
      start_plot_type$val <- input$plot_type
    })
    
    observeEvent(input$plot_type2,{           
      start_plot_type$val <- input$plot_type2
    })
    
    start_plot_type3 <- reactiveValues( val = 'lin' )
    
    observeEvent(input$plot_type3,{           
      start_plot_type3$val <- input$plot_type3
    })
    start_y <- reactiveValues( val = names(scresults$results_total)[1])
    
    observeEvent(input$y,{
      start_y$val <- input$y
    })
    
    ####....... start_y2$val####
    start_y2 <- reactiveValues( val = names(scresults$results_total)[2])
    
    observeEvent(input$y2,{
      start_y2$val <- input$y2
    })
    
    ####...... start_yrange$val/val2####
    start_yrange <- reactiveValues(
      val = c(min(scresults$sge[,names(scresults$results_total)[1]],na.rm=TRUE),max(scresults$sge[,names(scresults$results_total)[1]],na.rm=TRUE)),
      val2 = c(min(scresults$sge[,names(scresults$results_total)[2]],na.rm=TRUE),max(scresults$sge[,names(scresults$results_total)[2]],na.rm=TRUE))
    )
    
    observeEvent(input$y1,{           
      start_yrange$val <- c(min(scresults$sge[,input$y1],na.rm=TRUE),max(scresults$sge[,input$y1],na.rm=TRUE))
    })
    
    observeEvent(input$y,{           
      start_yrange$val <- c(min(scresults$sge[,input$y],na.rm=TRUE),max(scresults$sge[,input$y],na.rm=TRUE))
    })
    
    observeEvent(input$y2,{           
      start_yrange$val2 <- c(min(scresults$sge[,input$y2],na.rm=TRUE),max(scresults$sge[,input$y2],na.rm=TRUE))
    })
    
    observeEvent(input$YRange,{
      start_yrange$val <- input$YRange
    })
    
    observeEvent(input$YRange2,{
      start_yrange$val <- input$YRange2
    })
    
    observeEvent(input$YRange3,{
      start_yrange$val2 <- input$YRange3
    })
    
    ####...... key_val$range####
#    start_key1  <- reactiveValues(val=c(1,3))
#    start_key2  <- reactiveValues(val=c(1,3))
    key_val <- reactiveValues(range = c(1,3))
    
#    observeEvent(input$key,{
#      start_key2$val <- input$key2
#    })
    
#    observeEvent(input$key2,{
#      start_key1$val <- input$key
#    })

    
    observeEvent(input$key ,{
      key_val$range <- input$key
      updateSliderInput(session, "key2", value = key_val$range)
    })
    observeEvent(input$key2 ,{
      key_val$range <- input$key2
      updateSliderInput(session, "key", value = key_val$range)
    })
    
    ####...... start_x$val####
    start_x <- reactiveValues( val = c(names(scresults$results_total)[3])[1])
    
    observeEvent(input$x,{
      start_x$val <- input$x
    })
    
    ####....... start_y2$val####
    start_x2 <- reactiveValues( val = c(names(scresults$results_total)[3])[1])
    
    observeEvent(input$x2,{
      start_x$val <- input$x2
    })
    
    ####...... curr_x$cv ####
    
    curr_x <- reactiveValues( cv = c(PreSelectXAxis, names(scresults$results_total)[3])[1])
    
    observeEvent(input$x,{
      curr_x$cv <- input$x
    })
    
    
    ####...... start_filter$val####
    
    start_filter <- reactiveValues( val = c("No selection"))
    
    observeEvent(input$filter,{
      start_filter$val <- input$filter
    })
    
    ####....... start_filter2$val####
    #start_filter2 <- reactiveValues( val = c("No selection"))
    
    observeEvent(input$filter2,{
      start_filter$val <- input$filter2
    })
    
    
    ####...... filter_sel$fi####
    filter_sel <- reactiveValues(fi = c("no selection"))
    
    
    observeEvent(input$filter,{
      filter_sel$fi <- input$filter
    })
    
    ####......VarChose$choice####
    VarChose <-reactiveValues(choice = c("Not Used"))
    
    observeEvent(input$VarChosen,{
      VarChose$choice <- input$VarChosen
    })
    
    # observeEvent(input$VarChosen2,{
    #  VarChose$choice <- input$VarChosen2
    #})
    
    
    ####...... output$VarChosen####
    output$VarChosen <- renderUI({
      if (start_filter$val != 'no selection'){
        selectInput("VarChosen", "Choose a value", 
                    choices = c(as.character(unique(scresults$sge[, c(start_filter$val)]))), selected = start_varc$val)
      }
    })
    
    
    ####...... start_varc$val
    start_varc <- reactiveValues(val = c("Not Used"))
    
    observeEvent(input$VarChosen,{
      start_varc$val <- input$VarChosen
    })
    
    ####....... start_varc2$val####
    #start_varc2 <- reactiveValues( val = c("Not Used"))
    
    observeEvent(input$VarChosen2,{
      start_varc$val <- input$VarChosen2
    })
    
    
    #### PANEL IMPORTANCE VALUES (2)####
    ####.. PANEL ####
    output$Panel_Importance <- renderUI({
      
      style.panel <- paste('background-color: ',colthemeCol$panel.col,';padding: 9px;')
      
      wellPanel(style = style.panel,
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question "),
                                     title = "Use Variable Importance", placement = "top",expanded =TRUE)),
                ####...... importance buttons####        
                radioButtons("Impo_opt", label = HTML('<p style="color:white"> Importance Value Option </p>'),
                             choices = list("No Importance Value" = 0, "Use Variable Importance Values" = 1, "Use Ranking of Variable Importance Values" = 2), 
                             selected = 0), 
                
                #uiOutput("Impo_opt"),
                
                conditionalPanel("input.Impo_opt == '1'",
                                 div(style = "position:absolute;right:2em;", 
                                     bs_embed_tooltip(tag = shiny_iconlink("question "),
                                                      title = "Use the slider to set the range of Importance values which
                                                      should be colored in the plot", placement = "top",expanded =TRUE) 
                                     ),
                                 
                                 ####...... importance range slider####  
                                 if(!is.null(variable_importance$Importance)){
                                    sliderInput("impo","Choose importance Range", min(variable_importance$Importance),max(variable_importance$Importance),c(min(variable_importance$Importance),min(variable_importance$Importance)))
                                    }
                                 ),
                
                
                
                conditionalPanel("input.Impo_opt == '2'", 
                                 div(style = "position:absolute;right:2em;", 
                                     bs_embed_tooltip(tag = shiny_iconlink("question "),
                                                      title = "Use the Slider to adjust the number of Variables", placement = "top",expanded =TRUE) 
                                 ),
                                 ####....... importance rank slider####
                                 sliderInput("impo2", "Choose number of Variables which are most important", 1, length(variable_importance$Importance),
                                             1, step = 1)
                                 
                ),
                conditionalPanel("input.Impo_opt == '2'", 
                                 div(style = "position:absolute;right:2em;", 
                                     bs_embed_tooltip(tag = shiny_iconlink("question "),
                                                      title = "Change sorting order of the Variable importance values by decreasing or ascending", placement = "top",expanded =TRUE) 
                                 ),
                                 ####........... decrease or increase button####             
                                 radioButtons("decrease",  label = HTML('<p style="color:white"> Sorting order: </p>'),
                                              choices = list("Increase" = FALSE,"Decrease" = TRUE), 
                                              selected = FALSE)  
                                 
                ),
                conditionalPanel("input.Impo_opt == '1'" ,
                                 div(style = "position:absolute;right:2em;", 
                                     bs_embed_tooltip(tag = shiny_iconlink("question "),
                                                      title = "In this table all variables to the colored Points are displayed", placement = "top",expanded =TRUE) 
                                 ),
                                 
                                 ####...... importance variable list####                
                                 tableOutput('imp_var_list')
                                 
                ),
                conditionalPanel("input.Impo_opt == '2'" ,
                                 div(style = "position:absolute;right:2em;", 
                                     bs_embed_tooltip(tag = shiny_iconlink("question "),
                                                      title = "In this table all variables to the colored Points are displayed", placement = "top",expanded =TRUE) 
                                 ),
                                 ####...... importance variable list2####  
                                 tableOutput('imp_var_list2')
                ),
                
                use_bs_popover(),
                use_bs_tooltip()
                
      )
      
    })
    
    ####.. REACTIVE VALUES AND OBSERVE EVENTS####
    
    ####...... import_reac$reactive####
    if(!is.null(variable_importance$Importance)){
      import_reac <- reactiveValues(
          reactive = c(min(variable_importance$Importance),min(variable_importance$Importance))
      )
    }
    observeEvent(input$impo, {
        import_reac$reactive <- input$impo
    })
    
    ####...... import_reac2$reactive####
    import_reac2 <- reactiveValues(
      reactive = 1
    )
    
    observeEvent(input$impo2, {
      import_reac2$reactive <- input$impo2
    })
    
    ####...... Imp_opt$value
    
    Imp_opt <- reactiveValues(
      value = "0"
    )
    
    
    observeEvent(input$Impo_opt,{
      Imp_opt$value <- input$Impo_opt
    })
    
    
    
    
    
    #### PANEL DISPLAY 1(3) #####
    output$Panel_Display1 <- renderUI({
      ####.. PANEL ####
      
      style.panel <- paste('background-color: ',colthemeCol$panel.col,';padding: 9px;')
      
      wellPanel(style = style.panel,
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question "),
                                     title = "Change the Pickradius in pixel for the Selected Subgroups", placement = "top",expanded =TRUE) 
                ),
                ####...... radius slider click####         
                sliderInput("pickradius","Choose a radius for click points",min = 1, max = 30, value = 5, step = 1 , ticks = FALSE),
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question "),
                                     title = "Change the pointsize in the plot. You can combine this option
                                     with the point style option.", placement = "top",expanded =TRUE) 
                    ),
                
                ####...... point size slider ####        
                sliderInput("pointsize","Choose point size" ,0.1,3,1,step = 0.1),
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question "),
                                     title = "Use the Subgroup Size as pointsize or show all points with the same size
                                     ", placement = "top",expanded =TRUE) 
                    ),
                ####...... circle style buttons#####    
                radioButtons("circlestyle","Point Style",choiceNames = list("Standard","Subgroup size")
                             , 
                             choiceValues = c("standard", "groupsize"),
                             selected = "standard", inline = TRUE)
                ,  use_bs_popover(),
                use_bs_tooltip()
                )
    })
    
    ####.. REACTIVE VALUES AND OBSERVE EVENTS
    
    ####...... pick_rad$radius####
    pick_rad <- reactiveValues(
      radius = 5
    )
    
    observeEvent(input$pickradius,{
      pick_rad$radius <- input$pickradius
    })
    
    ####...... cs$val####
    cs <- reactiveValues( val = "standard")
    
    observeEvent(input$circlestyle,{
      cs$val <- input$circlestyle
    })
    
    ####......ps$val####
    ps <- reactiveValues(val=1)
    
    observeEvent(input$pointsize,{
      ps$val <- input$pointsize
    })
    
    
    #### PANEL DISPLAY2  (4)####
    ####.. PANEL####
    output$Panel_Display2 <- renderUI({
      
      style.panel <- paste('background-color: ',colthemeCol$panel.col,';padding: 9px;')
      
      wellPanel(style = style.panel,
                sliderInput("stripes", "Choose number of Stripes (Background)", min = 1 , max = 30, value = 7, step = 1, ticks = FALSE),
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question "),
                                     title = "Show a label on the x-axis", placement = "top",expanded =TRUE) 
                ),
                
                ####...... label x-axis####
                checkboxInput("xlabel", label = "Show label of X-Axis", value = TRUE),
                
                ####...... grid checkbox####
                checkboxInput("grid", "Display a grid", value = FALSE)
                #sliderInput("grid_y","Choose tickmark length grid on the x-axis",1,30,1,value=1)
                
                ,use_bs_popover(),
                use_bs_tooltip()
      )
    })
    
    ####.. REACTIVE VALUES AND OBSERVE EVENTS ####
    
    ####...... xl$dec
    xl <- reactiveValues(
      dec = TRUE
    )
    
    observeEvent(input$xlabel,{
      xl$dec <- input$xlabel
    })
    
    output$xlabel2 <- renderUI({
      if(xl$dec=="TRUE"){
        checkboxInput("xlabel3", label = HTML('<p style="color:white"> Show label of X-Axis </p>'), value = xl$dec)
      }else{
        checkboxInput("xlabel4", label = HTML('<p style="color:white"> Show label of X-Axis </p>'), value = xl$dec)
      }
    })  
    
    ####...... strip$nr
    strip <- reactiveValues(
      nr = 7
    )  
    observeEvent(input$stripes,{
      strip$nr <- input$stripes
    })
    
    ####...... display$grid
    
    display <- reactiveValues(
      grid = FALSE
    )
    
    observeEvent(input$grid,{
      display$grid <- input$grid
    })
    observeEvent(input$grid2,{
      display$grid <- input$grid2
    })
    
    
    #### PANEL COLOUR  (5)####
    ####.. PANEL ####
    output$Panel_Colour <- renderUI({
      
      style.panel <- paste('background-color: ',colthemeCol$panel.col,';padding: 9px;')
      
      wellPanel(style = style.panel,
                ####...... ColorClicked option####
                colourpicker::colourInput("ColorClicked","Choose a Colour for the selected Subgroup(s)", colthemeCol$ColorClicked, allowTransparent = TRUE),
                ####...... ColorSelected option####
                colourpicker::colourInput("ColorSelected","Choose a Colour for the filtered Subgroup(s)", colthemeCol$ColorSelected, allowTransparent = TRUE),
                ####...... ColorParents option####
                colourpicker::colourInput("ColorParents","Choose a Colour for the Parent Subgroup(s)", colthemeCol$ColorParents, allowTransparent = TRUE),
                ####...... ColorTabClicked option####
                colourpicker::colourInput("ColorTabClicked","Choose a Colour for the clicked Subgroup(s)", colthemeCol$ColorTabClicked, allowTransparent = TRUE),
                ####...... ColorImportance option####
                colourpicker::colourInput("ColorImportance","Choose a Colour for the Subgroup(s) with important Variable(s) ", colthemeCol$ColorImportance, allowTransparent = TRUE),
                ####...... ColorReference option####  
                colourpicker::colourInput("ColorReference","Choose a Colour for the Reference Line", colthemeCol$ColorReference, allowTransparent = TRUE),
                ####...... ColorBGplot option####    
                colourpicker::colourInput("ColorBGplot","Choose Background Colour (Plot)",colthemeCol$ColorBGplot),
                ####...... ColorStripe option####    
                colourpicker::colourInput("ColorStripe","Choose Background Colour (Plot stripes)",colthemeCol$panel.col),
                ####...... ColorPoints option####
                colourpicker::colourInput("ColorPoints","Choose a Colour for the Points",colthemeCol$ColorPoints),
                ####...... ColorFontColour button####
                radioButtons("FontColour"," Choose font Colour ", choiceNames = list("White" , "Black"), choiceValues = c("#ffffff", "#000000"), selected = colthemeCol$font.col, inline = TRUE),
                br(),
                ####.... color theme selection####
                selectInput('select_col', "Select standard color theme:", 
                            list('app version', 'print version'), selected = coltheme$col_sel),
                ####...... color theme button####
                actionButton('settheme', 'Apply / Refresh', width=NULL)
                ,use_bs_popover(),
                use_bs_tooltip()
      )
    })
    
    ####..REACTIVE VALUES AND OBSERVE EVENTS####
    
    ####...... coltheme$col_sel ####
    coltheme <- reactiveValues( col_sel = 'app version' ) 
    
    observeEvent(input$select_col,{
      coltheme$col_sel <- input$select_col
    })
    
    ####......colthemeCol$col.bg / font.col / ...####
    colthemeCol <- reactiveValues(
      col.bg = '#10384F',
      font.col = '#ffffff',
      panel.col = '#154967',
      ColorClicked = "#D30F4B",
      ColorSelected = "#89D329",
      ColorParents = "#ff6c00",
      ColorTabClicked = "gold3",
      ColorImportance = "#FA1BDC",
      ColorReference = "#0091DF",
      ColorBGplot = "#10384F",
      ColorPoints = "#FFFFFF"
    )
    observeEvent(input$settheme,{
      if (coltheme$col_sel == 'app version'){
        colthemeCol$col.bg <- '#10384F'
        colthemeCol$font.col <- '#ffffff'
        colthemeCol$panel.col <- '#154967'
        colthemeCol$ColorClicked <- "#D30F4B"
        colthemeCol$ColorSelected <- "#89D329"
        colthemeCol$ColorParents <- "#ff6c00"
        colthemeCol$ColorTabClicked <- "gold3"
        colthemeCol$ColorImportance <- "#FA1BDC"
        colthemeCol$ColorReference <- "#0091DF"
        colthemeCol$ColorBGplot <- "#10384F"
        colthemeCol$ColorPoints <- "#FFFFFF"
      }else if(coltheme$col_sel == 'print version'){
        colthemeCol$col.bg <- '#ffffff'
        colthemeCol$font.col <- '#000000'
        colthemeCol$panel.col <- '#EBEBEB'
        colthemeCol$ColorClicked <- "#D30F4B"
        colthemeCol$ColorSelected <- "#89D329"
        colthemeCol$ColorParents <- "#ff6c00"
        colthemeCol$ColorTabClicked <- "gold3"
        colthemeCol$ColorImportance <- "#FA1BDC"
        colthemeCol$ColorReference <- "#0091DF"
        colthemeCol$ColorBGplot <- "#ffffff"
        colthemeCol$ColorPoints <- "#000000"
      }
    })
    observeEvent(c(input$FontColour, input$ColorClicked, input$ColorStripe,
                   input$ColorSelected, input$ColorParents, input$ColorTabClicked,
                   input$ColorImportance, input$ColorReference, input$ColorBGplot,
                   input$ColorPoints),{
                     colthemeCol$col.bg <- input$ColorBGplot
                     colthemeCol$font.col <- input$FontColour
                     colthemeCol$panel.col <- input$ColorStripe
                     colthemeCol$ColorClicked <- input$ColorClicked
                     colthemeCol$ColorSelected <- input$ColorSelected
                     colthemeCol$ColorParents <- input$ColorParents
                     colthemeCol$ColorTabClicked <- input$ColorTabClicked
                     colthemeCol$ColorImportance <- input$ColorImportance
                     colthemeCol$ColorReference <- input$ColorReference
                     colthemeCol$ColorBGplot <- input$ColorBGplot
                     colthemeCol$ColorPoints <- input$ColorPoints 
                   })
    
    ####...... ColorPointCollect$ColorPoint1 / - ColorPoint5 ####
    ColorPointCollect <- reactiveValues(
      ColorPoint1 = grDevices::adjustcolor('#FFFFFF', alpha = 1),
      ColorPoint2 = grDevices::adjustcolor('#FFFFFF', alpha = 0.75),
      ColorPoint3 = grDevices::adjustcolor('#FFFFFF', alpha = 0.5),
      ColorPoint4 = grDevices::adjustcolor('#FFFFFF', alpha = 0.25),
      ColorPoint5 = grDevices::adjustcolor('#FFFFFF', alpha = 0.1)
    )
    
    observeEvent(input$ColorPoints,{
      ColorPointCollect$ColorPoint1 <- grDevices::adjustcolor(input$ColorPoints, alpha = 1)
      ColorPointCollect$ColorPoint2 <- grDevices::adjustcolor(input$ColorPoints, alpha = 0.75)
      ColorPointCollect$ColorPoint3<-  grDevices::adjustcolor(input$ColorPoints, alpha = 0.5)
      ColorPointCollect$ColorPoint4 <- grDevices::adjustcolor(input$ColorPoints, alpha = 0.25)
      ColorPointCollect$ColorPoint5 <- grDevices::adjustcolor(input$ColorPoints, alpha = 0.1)
    })
    
    ####...... ColorBGplotlight$col ####
    ColorBGplotlight <- reactiveValues(
      col = grDevices::adjustcolor("#10384F", red.f = 1.3, 
                                   green.f = 1.3, blue.f = 1.3)
    )
    
    observeEvent(input$ColorBGplot,{
      ColorBGplotlight$col <- grDevices::adjustcolor(colthemeCol$ColorBGplot, red.f = 1.3, green.f = 1.3, blue.f = 1.3)
    })
    
    
    #### PANEL VARIABLE OPTIONS II (6)####
    
    output$Panel_Variable2 <- renderUI({
      ####.. PANEL####
      style.panel <- paste('background-color: ',colthemeCol$panel.col,';padding: 9px;')
      
      wellPanel(style = style.panel,
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question "),
                                     title = "Select a variable for the y-axis for the plot at the top", placement = "top",expanded =TRUE) 
                ),             
                ####...... y1 #####         
                selectInput("y1", "First Target variable", 
                            names(scresults$results_total), selected = start_y$val),
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question "),
                                     title = "Change the y-axis into a log scale for the plot at the top", placement = "top",expanded =TRUE) 
                ),
                ####...... linear/log type####      
                radioButtons("plot_type2", label ="Plot Type (Compare Plot: y-axis / Bubble Plot: x-axis)", 
                             selected = start_plot_type$val, inline = TRUE
                             ,choiceNames = list("linear", "logarithmic"), 
                             choiceValues = c("lin", "log")),
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question"),
                                     title = "Choose the range of the y-axis for the plot at the top", placement = "top",expanded =TRUE) 
                ),
                #uiOutput('YRange2')
                ####...... y range 2####       
                sliderInput("YRange2","Range (Compare Plot: y-axis / Bubble Plot: x-axis)", min = roundDownNice(min(scresults$sge[, start_y$val], na.rm = TRUE)),
                            max = roundUpNice(max(scresults$sge[, start_y$val], na.rm = TRUE)), value = start_yrange$val,
                            step = roundUpNice((max(scresults$sge[, start_y$val], na.rm = TRUE) - min(scresults$sge[, start_y$val], na.rm = TRUE))/100))
                
                
                
                ,  use_bs_popover(),
                use_bs_tooltip()        
      )
      
    })
    
    ####.. REACTIVE VALUES AND OBSERVE EVENT####
    
    
    observeEvent(input$VarChosen2,{
      VarChose$choice <- input$VarChosen2
    })
    
    observeEvent(input$y1,{
      start_y$val <- input$y1
    })
    
    output$VarChosen2 <- renderUI({
      if (start_filter$val != 'no selection'){
        selectInput("VarChosen2", "Choose a value", 
                    choices = c(as.character(unique(scresults$sge[, c(start_filter$val)]))), selected = start_varc$val)
      }
    })
    
    # ####...... plot_type2$val####
    # plot_type2 <- reactiveValues(val = "")
    # 
    # observeEvent(input$plot_type2,{
    #   plot_type2$val <- input$plot_type2
    # })
    
    ####...... dec$val
    dec <- reactiveValues(
      val = FALSE
    )
    
    observeEvent(input$decrease,{
      dec$val <- input$decrease
    })
    
    
    #### PANEL VARIABLE OPTIONS III (7)####
    
    ####.. PANEL####
    output$Panel_Variable3 <- renderUI({
      
      style.panel <- paste('background-color: ',colthemeCol$panel.col,';padding: 9px;')
      
      wellPanel(style = style.panel,
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question "),
                                     title = "Select a variable for the y-axis for the plot at the bottom", placement = "top",expanded =TRUE) 
                ), 
                
                
                ####...... y2 ####
                selectInput("y2", "Second Target variable", 
                            names(scresults$results_total),start_y2$val),
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question "),
                                     title = "Change the y-axis into a log scale for the plot at the bottom", placement = "top",expanded =TRUE) 
                ),
                ####...... linear/log type 3 ####         
                shiny::radioButtons("plot_type3", "Plot Type (Compare Plot: y-axis / Bubble Plot: y-axis)", 
                                    choiceNames = list("linear", 
                                                       "logarithmic"
                                    ), 
                                    choiceValues = c("lin", "log"),
                                    selected =  start_plot_type3$val, inline = TRUE),
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question "),
                                     title = "Choose the range of the y-axis for the plot at the bottom", placement = "top",expanded =TRUE) 
                ),
                
                #shiny::uiOutput("YRange3")
                ####...... y range 3####        
                sliderInput("YRange3", "Y Range (Compare Plot: y-axis / Bubble Plot: y-axis)", min = roundDownNice(min(scresults$sge[, start_y2$val], na.rm = TRUE)),
                            max = roundUpNice(max(scresults$sge[, start_y2$val], na.rm = TRUE)), value = start_yrange$val2,
                            step = roundUpNice((max(scresults$sge[, start_y2$val], na.rm = TRUE) - min(scresults$sge[, start_y2$val], na.rm = TRUE))/100))
                
                ,use_bs_popover(),
                use_bs_tooltip()
      )
    })
    
    ####.. REACTIVE VALUES AND OBSERVE EVENTS####
    
    ####...... cur2$cv####
    curr2 <- reactiveValues(
      cv = c(PreSelectTarget, names(scresults$results_total)[6])[1]
    )
    
    observeEvent(input$y2,{
      curr2$cv <- input$y2
    })
    
    
    
    #### PANEL VARIABLE OPTIONS IV  (8)####
    
    output$Panel_Variable4 <- renderUI({
      
      style.panel <- paste('background-color: ',colthemeCol$panel.col,';padding: 9px;')
      
      wellPanel(style = style.panel,
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question "),
                                     title = "Choose a variable for the x-axis for both plots", placement = "top",expanded =TRUE) 
                ),
                ####...... x2####         
                #selectInput("x2","Reference variable", names(scresults$results_total), selected =  c(PreSelectXAxis, names(scresults$results_total)[3])[1]) 
                selectInput("x2","Reference variable", names(scresults$results_total), selected =  start_x$val) 
                ,  use_bs_popover(),
                use_bs_tooltip()
      )
      
    })
    
    ####.. REACTIVE VALUES AND OBSERVE EVENTS####
    
    observeEvent(input$x2,{
      curr_x$cv <- input$x2
    })
    
    #### PANEL VARIABLE OPTIONS V (9) ####
    
    ####.. PANEL####
    output$Panel_Variable5 <- renderUI({
      
      style.panel <- paste('background-color: ',colthemeCol$panel.col,';padding: 9px;')
      
      wellPanel(style = style.panel,
                
                div(style = "position:absolute;right:2em;", 
                    bs_embed_tooltip(tag = shiny_iconlink("question "),
                                     title = "Choose a filter variable. Filtered variable shown will shown in green and in the 
                                     filter tab below", placement = "top",expanded =TRUE) 
                    ),
                ####...... filter2####
                selectInput("filter2","Subgroup Filter", c("no selection", scresults$factors), selected = start_filter$val),
                
                shiny::conditionalPanel("input.filter2 != 'no selection'", 
                                        shiny::uiOutput("VarChosen2")
                                        # selectInput("VarChosen2","Choose a value", 
                                        #              choices = c(as.character(unique(scresults$sge[, c(start_filter$val)]))), selected = start_varc$val)
                                        , selectize = FALSE
                ), 
                ####...... key2####
                shiny::sliderInput("key2", "Subgroup level(s)", min = scresults$min_comb, max = scresults$max_comb, 
                                   ticks = FALSE, value = key_val$range, step = 1)
                ,  use_bs_popover(),
                use_bs_tooltip()
                )
      
    })
    ####.. REACTIVE VALUES AND OBSERVE EVENTS####
    
    
#    observeEvent(input$key2 ,{
#      key_val$range <- input$key2
#    })
    
    observeEvent(input$filter2,{
      filter_sel$fi <- input$filter2
    })
    
    ####...... ref_line####
    ref_line <- shiny::reactive({
      scresults$results_total[, c(start_y$val)]
    })
    
    ####...... log_type$graph1/graph2
    log_type <- shiny::reactiveValues(
      graph1 = '',
      graph3 = ''
    )
    observeEvent(input$plot_type, {
      log_type$graph1 <- ifelse(input$plot_type == "log", "y", "")
    })
    observeEvent(input$plot_type3, {
      log_type$graph3 <- ifelse(input$plot_type3 == "log", "y", "")
    })
    
    #### PANEL MOSAIC (10) ####
    ####.. PANEL####
    output$PanelMosaic <- renderUI({
      
      style.panel <- paste('background-color: ',colthemeCol$panel.col,';padding: 9px;')
      
      wellPanel(style = style.panel,
                
                ####...... first variable####    
                selectInput("var1", "First subgroup variable (x)",
                            choices=scresults$factors, selected = scresults$factors[1]),
                ####...... second variable####             
                selectInput("var2", "Second subgroup variable (y)",
                            choices=c('no selection',scresults$factors), selected = 'no selection'),
                ####...... second variable####             
                selectInput("var22", "Third subgroup variable (y2)",
                            choices=c('no selection',scresults$factors), selected = 'no selection'),
                ####...... reference variable####            
                selectInput("var3","Reference variable (color)" ,setdiff(names(scresults$results_total),'N.of.subjects'),selected=start_y$val),
                
                #sliderInput("var4","midpoint",mi,ma,1)
                ####...... linear/log mosaic####            
                radioButtons("logmosaic","Plot Type",
                             c(linear = "lin", logarithmic = "log"),selected = "lin", inline = TRUE)  #back
                
                ,  use_bs_popover(),
                use_bs_tooltip()       
      )
      
    })
    
    
    
    
    #### CLICK_POINTS_DATA #### 
    ####...... click_points_data$xy
    click_points_data <- reactiveValues(
      xy = data.frame(x = NULL, y = NULL)
    )             
    
    select_points_data <- data.frame(x = numeric(), y = numeric(), SGID = numeric())
    
    sel_SG <- data.frame(Selected = "None")
    
    color <- rep('#FFFFFF', 10)
    
    shiny::makeReactiveBinding("color")
    
    #### SETCOLOR FUNCTION #### 
    setcolor <- function() {
      
      f <- scresults$sge[which(scresults$sge$nfactors >= key_val$range[1] & scresults$sge$nfactors <= key_val$range[2]),]
      
      p.col <- colthemeCol$ColorPoints
      
      f$colour <- as.character(c(grDevices::adjustcolor(p.col, alpha = 1),
                                 grDevices::adjustcolor(p.col, alpha = 0.75), 
                                 grDevices::adjustcolor(p.col, alpha = 0.5), 
                                 grDevices::adjustcolor(p.col, alpha = 0.25),
                                 grDevices::adjustcolor(p.col, alpha = 0.1), 
                                 grDevices::adjustcolor(p.col, alpha = 0.1), 
                                 grDevices::adjustcolor(p.col, alpha = 0.1), 
                                 grDevices::adjustcolor(p.col, alpha = 0.1)
      ))[match(f$nfactors, 1:8)]
      
      if (start_filter$val != "no selection") {
        #if (filter_sel$fi != "no selection") {
        f$colour[f$SGID %in% select_points_data$SGID] <- colthemeCol$ColorSelected
        f$colour[f$colour != colthemeCol$ColorSelected] <- grDevices::adjustcolor(p.col, alpha = 0.1)
      }
      
      val <- Imp_opt$value 
      if(val == 1){
        
        ## Color Important Variable in pink 
        im <- import_reac$reactive 
        
        vek1 <- variable_importance[variable_importance$Importance >= im[1] & variable_importance$Importance <= im[2],1]
        
        if(is.integer0(as.integer(vek1)) == FALSE){
          test1 <- NULL
          
          for (i in 1:length(vek1)){
            test1 <- rbind(test1,scresults$sge[scresults$sge[,as.character(eval(parse(text='vek1[i]')))]!="Not used",]) 
            test1<- unique(test1)
          }
          if(!is.null(test1$SGID)){
            f[f$SGID %in% test1$SGID,'colour'] <- colthemeCol$ColorImportance
          }
        }
        output$imp_var_list <- renderTable({
          
          tab1 <- data.frame('Used importance variables'=vek1)
          names(tab1) <- "Used/colored importance variables"
          tab1
        },
        hover = TRUE,
        spacing = 'xs',
        na='none',
        digits=0,
        caption.placement='top')
        
      }
      
      if(val == 2){
        
        de <- dec$val
        
        im2 <- import_reac2$reactive
        vek2 <- variable_importance[order(
          variable_importance$Importance,decreasing = as.logical(de))[1:im2],1]
        
        if(is.integer0(as.integer(vek2)) == FALSE){
          test2 <- NULL
          
          for (i in 1:length(vek2)){
            test2 <- rbind(test2,scresults$sge[scresults$sge[,as.character(eval(parse(text='vek2[i]')))]!="Not used",]) 
            test2<- unique(test2)
          }
          if(!is.null(test2$SGID)){
            f[f$SGID %in% test2$SGID,'colour'] <- colthemeCol$ColorImportance
          }
        }
        output$imp_var_list2 <- renderTable({
          tab2 <- data.frame('Used importance variables' = vek2)
          names(tab2) <- "Used/colored importance variables"
          tab2
        },
        hover = TRUE,
        spacing = 'xs',
        na='none',
        digits=0,
        caption.placement='top')
      }
      
      
      #10.08.
      tmp <- NULL
      tmp <- pare$val
      
      f[f$SGID %in% parents(scresults,click_points_data$xy[(tmp),'SGID'])$Parents$SGID,'colour'] <- colthemeCol$ColorParents
      
      f[f$SGID %in% click_points_data$xy$SGID,'colour'] <- colthemeCol$ColorClicked 
      
      f[f$SGID %in% click_points_data$xy[input$selectedSG_rows_selected,'SGID'],'colour'] <- colthemeCol$ColorTabClicked 
      
      
      
      
      color <<- f$colour
    }
    
    pare <- reactiveValues(
      val = NULL
    )
    
    observeEvent(c(input$selectedSG_rows_selected,input$selectedSG_row_last_clicked),{
      pare$val <- input$selectedSG_rows_selected
    })
    
    #input$selectedSG_rows_selected,
    #input$selectedSG_row_last_clicked,
    
    #10.08
    observeEvent(c(input$plot_click),{
      pare$val <- NULL
    })
    
    #### ENABLE TABS ####
    
    ####.. importance tab####
    js$disableTab("ImportanceTab")
    if(!is.null(variable_importance)){js$enableTab("ImportanceTab")}
    
    ####.. parent subgroup tab####
    js$disableTab("ParentSubgroup")   
    if(scresults$max_comb>1){js$enableTab("ParentSubgroup")} 
    
    useShinyjs()
    shinyjs::disable("ColorImportance")
    if(!is.null(variable_importance)){shinyjs::enable("ColorImportance")}
    
    shinyjs::disable("ColorParents")
    if(scresults$max_comb>1){shinyjs::enable("ColorParents")}
    
    
    #### LOGO mit renderImage ####
    
    #  output$image2 <- renderImage({
    #   return(list(
    #     src = "www/Explorer Logo 19_6_zugeschnitten2.png",
    #     width = 155,
    #     height = 65,
    #     contentType = "image/png",
    #     alt = "Logo"
    #   ))
    # },deleteFile = FALSE)
    
    #### BUTTONS TO DATATABLE ####
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, click_points_data$xy$SGID[i]), ...))
      }
      inputs
    }
    
    shiny::observeEvent(c(input$plot_click,input$plot_click2,input$plot_click3#,input$settheme
    ), {
      
      key <- key_val$range
      start_radius <- pick_rad$radius
      clicked <- shiny::nearPoints(scresults$sge[which(scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]),],
                                   input$plot_click, xvar = curr_x$cv, yvar = start_y$val, threshold = start_radius, maxpoints = NULL)
      
      clicked2 <- shiny::nearPoints(scresults$sge[which(scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]),],
                                    input$plot_click2, xvar = curr_x$cv, yvar = curr2$cv, threshold = start_radius, maxpoints = NULL)
      
      clicked3 <- shiny::nearPoints(scresults$sge[which(scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]),],
                                    input$plot_click3, xvar = start_y$val, yvar = curr2$cv, threshold = start_radius, maxpoints = NULL)
      
      
      clicked <- subset(rbind(clicked,clicked2,clicked3), select = c("SGID", x = curr_x$cv, y = start_y$val, "nfactors", scresults$factors))
      
      click_points_data$xy <- clicked[, unlist(lapply(clicked, function(x) !all(is.na(x))))]
      #click_points_data$xy <- clicked[, unlist(lapply(clicked, function(x) !all(x=='Not used')))]
      
      setcolor()
      
      Memorize = shinyInput(actionButton, dim(click_points_data$xy)[1], 'button_', label = "Memorize", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )
      
      if (dim(click_points_data$xy)[1]==0){
        output$selectedSG <- DT::renderDataTable(DT::datatable(NULL))
      }
      if (dim(click_points_data$xy)[1]!=0){
        
        col2hide <- which(sapply(click_points_data$xy, FUN=function(x){all(x=='Not used')}))-1
        names(col2hide) <- NULL
        
        tmp <- DT::datatable(cbind(Memorize,click_points_data$xy), 
                             extensions = 'Buttons',
                             escape = FALSE, 
                             
                             options = list(
                               columnDefs = list(list(targets=col2hide+1, visible=FALSE)),
                               initComplete = JS(
                                 "function(settings, json) {",
                                 paste0('"$(this.api().table().header()).css({\'background-color\': \'',colthemeCol$ColorBGplot,', \'color\': \'',colthemeCol$font.col,'\'});"'),
                                 "}"),
                               
                               dom = 'Brtip',buttons=c('copy','print','pageLength',I('colvis')),lengthMenu = list(c(6, 12, -1),c("6", "12", "All")), pageLength = 6,
                               rowCallback = DT::JS("function(row, data) {\n                // Bold cells for those >= 5 in the first column\n                if (parseFloat(data[1]) >= 15.0)\n                $(\"td:eq(1)\", row).css(\"font-weight\", \"bold\");\n      }"))
                             ,class = 'cell-border stripe', rownames = FALSE,
                             caption = 'Table of Selected Subgroups',
                             filter='top'#, selection = 'single'
        )
        tmp <- formatStyle(tmp, columns = 1:(ncol(click_points_data$xy)+1), target = "cell", backgroundColor = colthemeCol$panel.col,
                           border = paste0('.5px solid ',colthemeCol$ColorBGplot))
        
        
        # %>% 
        #   formatStyle(columns = seq(1,101,by=2), target = "cell", backgroundColor = colthemeCol$ColorBGplot) %>% 
        #   formatStyle(columns = seq(2,100,by=2), target = "cell", backgroundColor = colthemeCol$panel.col)
        # colthemeCol$font.col,
        # colthemeCol$panel.col,
        # colthemeCol$ColorReference,
        
        
        tmp.sglev <- levels(relevel(factor(unlist(lapply(click_points_data$xy[,scresults$factors], as.character))), ref='Not used'))
        colXY <- which(colnames(click_points_data$xy)%in%c('SGID',names(scresults$results_total),'nfactors'))+1
        
        if(coltheme$col_sel =='app version'){
          col.tabFont <- 'white'
        }else{col.tabFont <- 'black'}
        tmp <- formatStyle(tmp, columns=colXY, color=col.tabFont)
        if(coltheme$col_sel =='app version'){
          tmp <- formatStyle(tmp, columns=scresults$factors,
                             color=styleEqual(
                               tmp.sglev, c('black', rep(col.tabFont,length(tmp.sglev)-1))))
        }else{
          tmp <- formatStyle(tmp, columns=scresults$factors,
                             color=styleEqual(
                               tmp.sglev, c('white', rep(col.tabFont,length(tmp.sglev)-1))))
        }
        
        #)
        
        output$selectedSG <- DT::renderDataTable(tmp)#,extensions="Responsive")
      }
      
    })
    
    
    df_m <- reactiveValues(data = data.frame(NULL
    ))
    
    
    shinyInput_remove <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, as.numeric(strsplit(input$select_button, "_")[[1]][2])), ...))
      }
      inputs
      
    }
    
    
    #10.08:
    ## 
    observeEvent(c(input$select_button), {
      if(!is.null(input$select_button)){
        
        selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
        
        del <- cbind(data.frame(
          Delete = shinyInput_remove(actionButton, 1, 'button_', label = "Remove",
                                     onclick = 'Shiny.onInputChange(\"remove_button\",  this.id)' )
        ),
        click_points_data$xy[click_points_data$xy$SGID==selectedRow,])
        
        df_m$data <- rbind(df_m$data,del)
        
      }
    })
    
    observeEvent(c(input$remove_button), {
      
      selectedRow <- as.numeric(strsplit(input$remove_button, "_")[[1]][2])
      
      df_m$data <- df_m$data[rownames(df_m$data) != as.numeric(strsplit(input$select_button, "_")[[1]][2]), ]
      
      selectRow <- NULL
    })
    
    
    
    #### PARENTS Reactive object: df_parent$data
    
    df_parent <- reactiveValues(data = data.frame(NULL))
    
    observeEvent(c(input$selectedSG_rows_selected,input$settheme), ignoreNULL = FALSE, {
      
      df_parent <- parents(scresults,click_points_data$xy[input$selectedSG_rows_selected,'SGID'])
      
      if (is.null(dim(df_parent$Parents))){
        
        tmp <- NULL
        
      }else{
        
        col2hide <- which(sapply(df_parent$Parents, FUN=function(x){all(x=='Not used')}))-1
        names(col2hide) <- NULL
        
        tmp <- DT::datatable(df_parent$Parents,extensions = 'Buttons',
                             options= list(columnDefs=list(list(targets=col2hide, visible=FALSE)) ,dom = 'Brtip',
                                           buttons=c('copy','print','pageLength',I('colvis')),
                                           lengthMenu = list(c(6, 12, -1), c("6", "12", "All")), pageLength = 6),class = 'cell-border stripe', rownames = FALSE,
                             caption = 'Table of Parent Subgroups',filter='top')
        
        tmp <- formatStyle(tmp, columns = 1:(ncol(df_parent$Parents)+1), target = "cell", backgroundColor = colthemeCol$panel.col,
                           border = paste0('.5px solid ',colthemeCol$ColorBGplot))
        #23.08.
        #formatStyle(columns = seq(1,101,by=2), target = "cell", backgroundColor = colthemeCol$ColorBGplot) %>% 
        #formatStyle(columns = seq(2,100,by=2), target = "cell", backgroundColor = colthemeCol$panel.col)
        
        
        tmp.sglev <- levels(relevel(factor(unlist(lapply(df_parent$Parents[,scresults$factors], as.character))), ref='Not used'))
        colXY <- which(colnames(df_parent$Parents)%in%c('SGID',names(scresults$results_total),'nfactors'))
        
        if(coltheme$col_sel =='app version'){
          col.tabFont <- 'white'
        }else{col.tabFont <- 'black'}
        tmp <- formatStyle(tmp, columns=colXY, color=col.tabFont)
        
        
        
        if(coltheme$col_sel =='app version'){
          tmp <- formatStyle(tmp, columns=scresults$factors,
                             color=styleEqual(
                               tmp.sglev, c('black', rep(col.tabFont,length(tmp.sglev)-1))))
        }else{
          tmp <- formatStyle(tmp, columns=scresults$factors,
                             color=styleEqual(
                               tmp.sglev, c('white', rep(col.tabFont,length(tmp.sglev)-1))))
        }
        # tmp <- formatStyle(tmp, columns=scresults$factors,
        #                    color=styleEqual(
        #                      tmp.sglev, c('black', rep(col.tabFont,length(tmp.sglev)-1)))
        # )
        
      }
      
      output$parents<- DT::renderDataTable(tmp)
      
    })
    
    
    #### FILTER  TABLE TAB####
    shiny::observeEvent(c(input$filter,input$filter2,input$VarChosen,input$VarChosen2,input$settheme  # 24.08:
    ), { 
      #filt <- filter_sel$fi
      filt <- start_filter$val 
      key <- key_val$range
      if (filt != "no selection") {
        choice <- VarChose$choice
        #choice <- c("Not Used")
        select_points_data <<- scresults$sge[which(scresults$sge$nfactors >= 
                                                     key[1] & scresults$sge$nfactors <= 
                                                     key[2] & 
                                                     scresults$sge[, c(filt)] == choice),]
      }
      else {
        select_points_data <<- data.frame(x = numeric(), y = numeric(), SGID = numeric())
      }
      
      setcolor()
      
      if (filt == "no selection"){
        output$filteredSG <- DT::renderDataTable(DT::datatable(NULL))
      }
      
      if (filt != "no selection") {
        
        df_filt <- subset(select_points_data, select = c(x = curr_x$cv, y = start_y$val, "nfactors", scresults$factors))
        
        col2hide <- which(sapply(df_filt, FUN=function(x){all(x=='Not used')}))-1
        names(col2hide) <- NULL
        
        tmp <- DT::datatable(df_filt ,
                             extensions = 'Buttons', 
                             options = list(
                               
                               initComplete = JS(
                                 "function(settings, json) {",
                                 paste0('"$(this.api().table().header()).css({\'background-color\': \'',colthemeCol$ColorBGplot,', \'color\': \'',colthemeCol$font.col,'\'});"'),
                                 "}"),
                               
                               columnDefs = list(list(targets=col2hide, visible=FALSE)),
                               dom = 'Brtip',buttons=c('copy','print','pageLength',I('colvis')),
                               lengthMenu = list(c(6, 12, -1),c("6", "12", "All")), 
                               pageLength = 6),class = 'cell-border stripe', 
                             rownames = FALSE,
                             caption = 'Table of Filtered Subgroups',filter='top')
        
        tmp <- formatStyle(tmp, columns = 1:ncol(df_filt), target = "cell", backgroundColor = colthemeCol$panel.col,
                           border = paste0('.5px solid ',colthemeCol$ColorBGplot))
        
        #formatStyle(columns = seq(1,101,by=2), target = "cell", backgroundColor = colthemeCol$ColorBGplot) %>% 
        #formatStyle(columns = seq(2,100,by=2), target = "cell", backgroundColor = colthemeCol$panel.col)#)
        
        
        if(dim(df_filt)[1] != 0){
          tmp.sglev <- levels(relevel(factor(unlist(unique(lapply(df_filt,as.character)))),ref="Not used"))
          colXY <- which(colnames(subset(df_filt, select = c(x = start_x$val, y = start_y$val, 'nfactors', 
                                                             scresults$factors)))%in%c('SGID',names(scresults$results_total),'nfactors'))
          
          if(coltheme$col_sel =='app version'){
            col.tabFont <- 'white'
          }else{col.tabFont <- 'black'}
          
          tmp <- formatStyle(tmp, columns=colXY, color=col.tabFont)
          
          if(coltheme$col_sel =='app version'){
            tmp <- formatStyle(tmp, columns=scresults$factors,
                               color=styleEqual(
                                 tmp.sglev, c('black', rep(col.tabFont,length(tmp.sglev)-1))))
          }else{
            tmp <- formatStyle(tmp, columns=scresults$factors,
                               color=styleEqual(
                                 tmp.sglev, c('white', rep(col.tabFont,length(tmp.sglev)-1))))
          }        
        }
        
        output$filteredSG <- DT::renderDataTable(tmp)
      }
    })
    
    observeEvent(c(input$select_button,input$remove_button),{
      
      col2hide <- which(sapply(df_m$data, FUN=function(x){all(x=='Not used')}))-1
      names(col2hide) <- NULL
      
      tmp<- DT::datatable(df_m$data, extensions = 'Buttons', escape = FALSE, selection = 'none', 
                          options = list(
                            initComplete = JS(
                              "function(settings, json) {",
                              paste0('"$(this.api().table().header()).css({\'background-color\': \'',colthemeCol$ColorBGplot,', \'color\': \'',colthemeCol$font.col,'\'});"'),
                              "}"),
                            
                            dom = 'Brtip', columnDefs = list(list(targets=col2hide, visible=FALSE)),
                            buttons=c('copy','print','pageLength',I('colvis')),
                            lengthMenu = list(c(6, 12, -1), 
                                              c("6", "12", "All")), pageLength = 6), class = 'cell-border stripe', rownames = FALSE,
                          caption = 'Table of Memorized Subgroups'
                          , 
                          filter = 'top'
      )
      
      tmp <- formatStyle(tmp, columns = 1:(ncol(df_m$data)+1), target = "cell", backgroundColor = colthemeCol$panel.col,
                         border = paste0('.5px solid ',colthemeCol$ColorBGplot))
      # formatStyle(columns = seq(1,101,by=2), target = "cell", backgroundColor = colthemeCol$ColorBGplot) %>% 
      #  formatStyle(columns = seq(2,100,by=2), target = "cell", backgroundColor = colthemeCol$panel.col)
      
      if(dim(df_m$data)[1] != 0){
        tmp.sglev <- levels(relevel(factor(unlist(unique(lapply(df_m$data,as.character)))),ref="Not used"))
        colXY <- which(colnames(subset(df_m$data, select = c(x = start_x$val, y = start_y$val, 'nfactors', 
                                                             scresults$factors)))%in%c('SGID',names(scresults$results_total),'nfactors'))+1
        
        if(coltheme$col_sel =='app version'){
          col.tabFont <- 'white'
        }else{col.tabFont <- 'black'}
        tmp <- formatStyle(tmp, columns=colXY, color=col.tabFont)
        
        if(coltheme$col_sel =='app version'){
          tmp <- formatStyle(tmp, columns=scresults$factors,
                             color=styleEqual(
                               tmp.sglev, c('black', rep(col.tabFont,length(tmp.sglev)-1))))
        }else{
          tmp <- formatStyle(tmp, columns=scresults$factors,
                             color=styleEqual(
                               tmp.sglev, c('white', rep(col.tabFont,length(tmp.sglev)-1))))
        }
        
        
        # tmp <- formatStyle(tmp, columns=scresults$factors,
        #                    color=styleEqual(
        #                      tmp.sglev, c('black', rep(col.tabFont,length(tmp.sglev)-1))))
      }
      if(dim(df_m$data)[1] == 0){
        tmp <- NULL
      }
      
      output$memorizedSG <- DT::renderDataTable(tmp)
    })                                      
    
    
    
    #### PLOT_POINTS_DATA ####
    
    plot_points_data <- reactive({ 
      key <- key_val$range
      data.frame(x = scresults$sge[, c(curr_x$cv)][scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]], 
                 y = scresults$sge[, c(start_y$val)][scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]])
      
    })
    
    #### PLOT_POINTS_DATA 3 ####
    
    plot_points_data3 <- shiny::reactive({
      key <- key_val$range
      data.frame(x = scresults$sge[, c(curr_x$cv)][scresults$sge$nfactors >= 
                                                     key[1] & scresults$sge$nfactors <= key[2]], 
                 y= scresults$sge[, c(input$y2)][scresults$sge$nfactors >= 
                                                   key[1] & scresults$sge$nfactors <= 
                                                   key[2]])
    })
    
    #### SG_TIT ####
    SG_tit <- shiny::reactive({
      key <- key_val$range
      if (key[1] == key[2]) paste(key[1], "-Factorial Subgroups (", length(plot_points_data()$x), ")", sep = "")
      else paste(key[1], " to ", key[2], "-Factorial Subgroups (", length(plot_points_data()$x), ")", sep = "")
    })
    
    SG_tit3 <- shiny::reactive({
      key <- key_val$range
      if (key[1] == key[2]) 
        paste(key[1], "-Factorial Subgroups (", 
              length(plot_points_data3()$x3), ")", sep = "")
      else paste(key[1], " to ", key[2], "-Factorial Subgroups (", length(plot_points_data3()$x3), 
                 ")", sep = "")
    })
    
    
    #### UPDATE COLORS setcolor() ####
    shiny::observeEvent(c(input$y,
                          input$key,
                          input$key2,
                          Imp_opt$value,
                          #input$Impo_opt,
                          input$impo, 
                          input$impo2,
                          #coltheme$col_sel,
                          colthemeCol$col.bg,
                          colthemeCol$font.col,
                          colthemeCol$panel.col,
                          colthemeCol$ColorReference,
                          colthemeCol$ColorBGplot,
                          colthemeCol$ColorPoints,
                          colthemeCol$ColorClicked, 
                          colthemeCol$ColorSelected, 
                          colthemeCol$ColorParents, 
                          colthemeCol$ColorTabClicked,
                          colthemeCol$ColorImportance,
                          input$decrease,
                          input$selectedSG_rows_selected,
                          input$selectedSG_row_last_clicked,
                          input$select_col#,
                          #input$plot_click
    ), {
      setcolor()
    })
    
    
    #### PLOTS ####
    
    
    
    ####.. Graph 1 #### 
    output$graph <- shiny::renderPlot({
      
      par(oma = c(0, 0, 0, 0), mar = c(0, 3, 0, 0), bg = colthemeCol$ColorBGplot)
      
      plot(plot_points_data(), xlab = "", ylab = "", ylim = start_yrange$val, 
           log = ifelse(start_plot_type$val == "log", "y", ""), cex.axis = 1.5, cex.lab = 1.5, 
           type = "n"
           ,axes=FALSE                                       
      )
      rect(xleft=grconvertX(0,'ndc','user'), xright=grconvertX(1,'ndc','user'),
           ybottom=grconvertY(0,'ndc','user'), ytop=grconvertY(1,'ndc','user'),
           border=NA,col=colthemeCol$ColorBGplot, xpd=TRUE)
      
      #if (log_type$graph1 == "y") {
      if ( ifelse(start_plot_type$val == "log", "y", "") == "y") {
        miniy <- 10^par("usr")[3]
        maxiy <- 10^par("usr")[4]
        lowy <- 10^(par("usr")[3] + (par("usr")[4] - par("usr")[3])/40)
        lowyp <- 10^(par("usr")[3] + (par("usr")[4] - par("usr")[3])/20)
        minplustinyy <- 10^(par("usr")[3] + (par("usr")[4] - 
                                               par("usr")[3])/1400)
      } else {
        miniy <- par("usr")[3]
        maxiy <- par("usr")[4]
        lowy <- miniy + (maxiy - miniy)/40
        lowyp <- miniy + (maxiy - miniy)/20
        minplustinyy <- miniy + (maxiy - miniy)/1400
      }
      
      minix <- roundDownNice(par("usr")[1])
      maxix <- roundUpNice(par("usr")[2])
      
      nr <- strip$nr 
      stepx <- roundUpNice((maxix - minix)/(nr +  1))
      if (minix < stepx) 
        minix <- 0
      stripesx <- 0:(nr + 1)
      stripesx <- lapply(stripesx, function(x) x * stepx)
      stripesx <- lapply(stripesx, function(x) x + minix)
      stripesxp <- lapply(stripesx, function(x) paste(floor(x/scresults$results_total[,c(curr_x$cv)] * 100), "%"))
      
      for (i in seq(1, nr, 2)) rect(stripesx[i],miniy, stripesx[i + 1], maxiy, col = colthemeCol$panel.col, border = NA)
      
      
      if(xl$dec == TRUE){
        text(stripesx, lowy, stripesx, cex = 1.5,col=colthemeCol$font.col)
        text(stripesx, lowyp, stripesxp, cex = 1.5,col=colthemeCol$font.col)
      }
      
      box(col=colthemeCol$font.col)
      
      axis(2, col=colthemeCol$font.col, col.ticks=colthemeCol$font.col, col.axis=colthemeCol$font.col, cex.axis=1)
      
      title(main = SG_tit(), line = -2, col = "#8b8b8b", col.main=colthemeCol$font.col)
      if(cs$val == "standard"){
        points(plot_points_data(), pch = 19, cex = ps$val, col = color)
      }
      if(cs$val == "groupsize"){
        points(plot_points_data(), pch = 19, cex = ps$val * sqrt(scresults$sge[, 'N.of.subjects'][scresults$sge$nfactors >= key_val$range[1] & scresults$sge$nfactors <= key_val$range[2]]/pi), col = color)
      }
      abline(h = ref_line(), lwd = 3, col = colthemeCol$ColorReference)
      
      text(x=grconvertX(0.97, from='nfc', to='user'),y = grconvertY(0.06,from='nfc', to ='user')+ref_line(),paste0(ref_line()),col = colthemeCol$ColorReference)
      
      
      if(display$grid == TRUE){
        abline(h = axTicks(2), lty = 2, col = colthemeCol$font.col, lwd = 0.3)
        
        abline(v = axTicks(1), lty = 2, col = colthemeCol$font.col, lwd = 0.3)
      }
      
    })
    
    ####.. Graph 2 ####
    output$graph2 <- shiny::renderPlot({
      
      par(oma = c(0, 0, 0, 0), mar = c(0, 3, 0, 0), bg = colthemeCol$ColorBGplot)
      plot(plot_points_data(), xlab = "", ylab = "", ylim = start_yrange$val, 
           log = ifelse(start_plot_type$val == "log", "y", ""), 
           cex.axis = 1.5, cex.lab = 1.5, 
           axes = FALSE,
           type = "n" )
      rect(xleft=grconvertX(0,'ndc','user'), xright=grconvertX(1,'ndc','user'),
           ybottom=grconvertY(0,'ndc','user'), ytop=grconvertY(1,'ndc','user'),
           border=NA,col=colthemeCol$ColorBGplot, xpd=TRUE)
      if ( ifelse(start_plot_type$val == "log", "y", "") == "y") {
        #if (plot_type2$val == "y") {
        miniy <- 10^par("usr")[3]
        maxiy <- 10^par("usr")[4]
        lowy  <- 10^(par("usr")[3] + (par("usr")[4] - par("usr")[3])/40)
        lowyp <- 10^(par("usr")[3] + (par("usr")[4] - par("usr")[3])/20)
        minplustinyy <- 10^(par("usr")[3] + (par("usr")[4] - 
                                               par("usr")[3])/1400)
      } else {
        miniy <- par("usr")[3]
        maxiy <- par("usr")[4]
        lowy  <- miniy + (maxiy - miniy)/40
        lowyp <- miniy + (maxiy - miniy)/20
        minplustinyy <- miniy + (maxiy - miniy)/1400
      }
      
      minix <- roundDownNice(par("usr")[1])
      maxix <- roundUpNice(par("usr")[2])
      nr <- strip$nr 
      stepx <- roundUpNice((maxix - minix)/(nr +  1))
      if (minix < stepx) 
        minix <- 0
      stripesx <- 0:(nr + 1)
      stripesx <- lapply(stripesx, function(x) x * stepx)
      stripesx <- lapply(stripesx, function(x) x + minix)
      stripesxp <- lapply(stripesx, function(x) paste(floor(x/scresults$results_total[,c(curr_x$cv)] * 100), "%"))
      
      for (i in seq(1, nr, 2)) rect(stripesx[i],miniy, stripesx[i + 1], maxiy, col = colthemeCol$panel.col, border = NA)
      
      if(xl$dec == TRUE){
        text(stripesx, lowy, stripesx, cex = 1.5, col=colthemeCol$font.col)
        text(stripesx, lowyp, stripesxp, cex = 1.5, col=colthemeCol$font.col)
      }
      
      box(col=colthemeCol$font.col)
      
      axis(2, col=colthemeCol$font.col, col.ticks=colthemeCol$font.col, col.axis=colthemeCol$font.col, cex.axis=1)
      
      title(main = SG_tit(), line = -2, col = "#8b8b8b",col.main=colthemeCol$font.col)
      
      if(cs$val == "standard"){
        points(plot_points_data(), pch = 19, cex = ps$val, col = color)
      }
      if(cs$val == "groupsize"){
        points(plot_points_data(), pch = 19, cex = ps$val * sqrt(scresults$sge[, 'N.of.subjects'][scresults$sge$nfactors >= key_val$range[1] & scresults$sge$nfactors <= key_val$range[2]]/pi), col = color)
      }
      
      abline(h = ref_line(), lwd = 3, col = colthemeCol$ColorReference)
      
      text(x=grconvertX(0.97, from='npc', to='user'),y = grconvertY(0.06,from='nfc', to ='user')+ref_line(),paste0(ref_line()),col=colthemeCol$ColorReference)
      #text(x=grconvertX(0.97, from='nfc', to='user'),y = ref_line(),paste0(ref_line()),col=colthemeCol$ColorReference, adj=c(1,0))
      
      if(display$grid == TRUE){
        abline(h = axTicks(2), lty = 2, col = colthemeCol$font.col, lwd = 0.3)
        
        abline(v = axTicks(1), lty = 2, col = colthemeCol$font.col, lwd = 0.3)
      }
      
      abline(h = ref_line(), lwd = 3, col = colthemeCol$ColorReference)
      
    }  )
    
    ####.. Graph 3 ####  
    output$graph3 <- shiny::renderPlot({
      req(input$YRange3)
      par(oma = c(0, 0, 0, 0), mar = c(0, 3, 0, 0), bg = colthemeCol$ColorBGplot)
      plot(plot_points_data3(), xlab = "", ylab = "", ylim = start_yrange$val2, 
           log = log_type$graph3, cex.axis = 1.5, cex.lab = 1.5, 
           axes = FALSE,
           type = "n", bg = colthemeCol$ColorBGplot)
      rect(xleft=grconvertX(0,'ndc','user'), xright=grconvertX(1,'ndc','user'),
           ybottom=grconvertY(0,'ndc','user'), ytop=grconvertY(1,'ndc','user'),
           border=NA,col=colthemeCol$ColorBGplot, xpd=TRUE)
      if (log_type$graph3 == "y") {
        miniy <- 10^par("usr")[3]
        maxiy <- 10^par("usr")[4]
        lowy  <- 10^(par("usr")[3] + (par("usr")[4] - par("usr")[3])/40)
        lowyp <- 10^(par("usr")[3] + (par("usr")[4] - par("usr")[3])/20)
        minplustinyy <- 10^(par("usr")[3] + (par("usr")[4] - 
                                               par("usr")[3])/1400)
      } else {
        miniy <- par("usr")[3]
        maxiy <- par("usr")[4]
        lowy <- miniy + (maxiy - miniy)/40
        lowyp <- miniy + (maxiy - miniy)/20
        minplustinyy <- miniy + (maxiy - miniy)/1400
      }
      
      minix <- roundDownNice(par("usr")[1])
      maxix <- roundUpNice(par("usr")[2])
      nr <- strip$nr 
      stepx <- roundUpNice((maxix - minix)/(nr +  1))
      if (minix < stepx) 
        minix <- 0
      stripesx <- 0:(nr + 1)
      stripesx <- lapply(stripesx, function(x) x * stepx)
      stripesx <- lapply(stripesx, function(x) x + minix)
      stripesxp <- lapply(stripesx, function(x) paste(floor(x/scresults$results_total[,c(curr_x$cv)] * 100), "%"))
      
      for (i in seq(1, nr, 2)) rect(stripesx[i],miniy, stripesx[i + 1], maxiy, col = colthemeCol$panel.col, border = NA)
      if(xl$dec == TRUE){
        text(stripesx, lowy, stripesx, cex = 1.5, col=colthemeCol$font.col)
        text(stripesx, lowyp, stripesxp, cex = 1.5, col=colthemeCol$font.col)
      }
      box(col=colthemeCol$font.col)
      axis(2, col=colthemeCol$font.col, col.ticks=colthemeCol$font.col, col.axis=colthemeCol$font.col, cex.axis=1)
      
      if(cs$val == "standard"){
        points(plot_points_data3(), pch = 19, cex = ps$val, col = color)
      }
      if(cs$val == "groupsize"){
        points(plot_points_data3(), pch = 19, cex = ps$val * sqrt(scresults$sge[, 'N.of.subjects'][scresults$sge$nfactors >= key_val$range[1] & scresults$sge$nfactors <= key_val$range[2]]/pi), col = color)
      }
      
      if(display$grid == TRUE){
        abline(h = axTicks(2), lty = 2, col = colthemeCol$font.col, lwd = 0.3)
        abline(v = axTicks(1), lty = 2, col = colthemeCol$font.col, lwd = 0.3)
      }
      
      abline(h = scresults$results_total[, c(curr2$cv)], lwd = 3, col = colthemeCol$ColorReference)
      text(x=grconvertX(0.97, from='nfc', to='user'),y = grconvertY(0.06,from='nfc', to ='user')+scresults$results_total[, c(curr2$cv)],paste0(scresults$results_total[, c(curr2$cv)]),col=colthemeCol$ColorReference)
      
    } )
    
    
    ####.. BUBBLE PLOT Graph 4####  
    output$graph4 <- shiny::renderPlot({
      
      key <- key_val$range
      
      #28.08
      #rect(start_yrange$val[1]-10,start_yrange$val2[1]-10,start_yrange$val[2]+10,start_yrange$val2[2]+10,col="red")
      if (ifelse(start_plot_type$val == "log", "y", "") != "y" & log_type$graph3 != "y") {
        par(oma = c(0, 0, 0, 0), mar = c(3, 3, 1, 1) 
            , bg = colthemeCol$ColorBGplot
        )
        plot(0,0, xlim=start_yrange$val,ylim=start_yrange$val2,xlab='',ylab='',type='n',axes=FALSE,log= "")
        rect(xleft=grconvertX(0,'ndc','user'), xright=grconvertX(1,'ndc','user'),
             ybottom=grconvertY(0,'ndc','user'), ytop=grconvertY(1,'ndc','user'),
             border=NA,col=colthemeCol$ColorBGplot, xpd=TRUE)
        
        suppressWarnings(symbols(main = SG_tit(), col.main = colthemeCol$font.col,plot_points_data()$y, plot_points_data3()$y, 
                                 circles = sqrt(( scresults$sge[,c('N.of.subjects')][scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]] )/ pi ), inches=1/3,
                                 xlim=start_yrange$val, ylim=start_yrange$val2, fg = "grey",bg = color,log= "",add=T))
      }
      
      if (ifelse(start_plot_type$val == "log", "y", "") == "y" & log_type$graph3 != "y") {
        par(oma = c(0, 0, 0, 0), mar = c(3, 3, 1, 1) 
            , bg = colthemeCol$ColorBGplot
        )
        plot(1,0, xlim=start_yrange$val,ylim=start_yrange$val2,xlab='',ylab='',type='n',axes=FALSE,log= "x")
        rect(xleft=grconvertX(0,'ndc','user'), xright=grconvertX(1,'ndc','user'),
             ybottom=grconvertY(0,'ndc','user'), ytop=grconvertY(1,'ndc','user'),
             border=NA,col=colthemeCol$ColorBGplot, xpd=TRUE)
        
        suppressWarnings(symbols(main = SG_tit(), col.main = colthemeCol$font.col,plot_points_data()$y, plot_points_data3()$y, 
                                 circles = sqrt(( scresults$sge[,c('N.of.subjects')][scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]] )/ pi ), inches=1/3,
                                 xlim=start_yrange$val, ylim=start_yrange$val2, fg = "grey",bg = color,log= "x",add=T))
      }
      
      if (ifelse(start_plot_type$val == "log", "y", "") != "y" & log_type$graph3 == "y") {
        par(oma = c(0, 0, 0, 0), mar = c(3, 3, 1, 1) 
            , bg = colthemeCol$ColorBGplot
        )
        plot(0,1, xlim=start_yrange$val,ylim=start_yrange$val2,xlab='',ylab='',type='n', axes=FALSE,log="y")
        
        rect(xleft=grconvertX(0,'ndc','user'), xright=grconvertX(1,'ndc','user'),
             ybottom=grconvertY(0,'ndc','user'), ytop=grconvertY(1,'ndc','user'),
             border=NA,col=colthemeCol$ColorBGplot, xpd=TRUE)
        
        suppressWarnings(symbols(main = SG_tit(), col.main = colthemeCol$font.col,plot_points_data()$y, plot_points_data3()$y, 
                                 circles = sqrt(( scresults$sge[,c('N.of.subjects')][scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]] )/ pi ),
                                 inches=1/3,
                                 xlim=start_yrange$val, ylim=start_yrange$val2, fg = "grey",bg = color,log= "y",add=T))
      }
      
      if (ifelse(start_plot_type$val == "log", "y", "") == "y" & log_type$graph3 == "y") {
        par(oma = c(0, 0, 0, 0), mar = c(3, 3, 1, 1) 
            , bg = colthemeCol$ColorBGplot
        )
        plot(1,1, xlim=start_yrange$val,ylim=start_yrange$val2,xlab='',ylab='',type='n',axes=FALSE,log="yx")
        
        rect(xleft=grconvertX(0,'ndc','user'), xright=grconvertX(1,'ndc','user'),
             ybottom=grconvertY(0,'ndc','user'), ytop=grconvertY(1,'ndc','user'),
             border=NA,col=colthemeCol$ColorBGplot, xpd=TRUE)
        
        suppressWarnings(symbols(main = SG_tit(), col.main = colthemeCol$font.col,plot_points_data()$y, plot_points_data3()$y, 
                                 circles = sqrt(( scresults$sge[,c('N.of.subjects')][scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]] )/ pi ), inches=1/3,
                                 xlim=start_yrange$val, ylim=start_yrange$val2, fg = "grey",bg = color, log= "yx",add=T))
      }
      
      box(col=colthemeCol$font.col)
      axis(1, col=colthemeCol$font.col, col.ticks=colthemeCol$font.col, col.axis=colthemeCol$font.col, cex.axis=1)
      axis(2, col=colthemeCol$font.col, col.ticks=colthemeCol$font.col, col.axis=colthemeCol$font.col, cex.axis=1)
      mtext(start_y$val, side=1, line=3, col=colthemeCol$font.col, cex=1)
      mtext(curr2$cv, side=2, line=3, col=colthemeCol$font.col, cex=1)
      
    })
    
    ####.. MOSAIC PLOT ####
    
    var_mos1 <- reactiveValues(
      val = scresults$factors[1]
    )
    var_mos2 <- reactiveValues(
      val = 'no selection'
    )
    var_mos22 <- reactiveValues(
      val = 'no selection'
    )
    var_mos3 <- reactiveValues(
      val = names(scresults$results_total)[1]
    )
    
    observeEvent(input$var1 ,{
      var_mos1$val <- input$var1
    })
    observeEvent(input$var2 ,{
      var_mos2$val <- input$var2
    })
    observeEvent(input$var22 ,{
      var_mos22$val <- input$var22
    })
    observeEvent(input$var3 ,{
      var_mos3$val <- input$var3
    })
    
    # midpoint <- reactiveValues(
    #    val = 1
    #  )
    
    
    observeEvent(input$var4,{
      midpoint$val <- input$var4
    })
    
    #  output$var4 <- renderUI({
    
    ##    mi <- min(scresults$sge[,var_mos3$val],na.rm=TRUE)
    #   ma <- max(scresults$sge[,var_mos3$val],na.rm=TRUE)
    #  
    #    sliderInput("var4",HTML('<p style="color:white"> midpoint </p>'),mi,ma,1)
    # })
    
    output$mosaic <- renderPlot({
      
      mos.x <- var_mos1$val
      mos.y <- var_mos2$val
      mos.y2 <- var_mos22$val
      mos.z <- var_mos3$val
      
      col.bg <- colthemeCol$ColorBGplot #same as App background!
      col.txt <- colthemeCol$font.col
      colrange.z <- c('#00BCFF','gray89','#89D329') #colors for min and max of z
      
      #not.used <- c('Not used','No data')
      not.used <- 'Not used'
      
      if (mos.y=='no selection'){
        mos.y <- NULL
      }
      if (mos.y2=='no selection' | is.null(mos.y)){
        mos.y2 <- NULL
      }
      if (!is.null(mos.y)){
        if (mos.x==mos.y){
          mos.y <- NULL
        }
      }
      if (!is.null(mos.y2)){
        if (mos.x==mos.y2 | mos.y==mos.y2){
          mos.y2 <- NULL
        }
      }
      
      ##x
      tmp <- scresults$sge[scresults$sge$nfactors==1 & !scresults$sge[,mos.x]%in%not.used,]
      tmp <- arrange_(tmp, .dots = c(mos.x))
      prop.x <- cumsum(tmp[,'N.of.subjects'])
      prop.x <- c(0,prop.x)/max(prop.x)
      mid.x <- (prop.x[-length(prop.x)]+prop.x[-1])/2
      names(mid.x) <- paste0(mos.x,'=',tmp[,mos.x])
      
      ##y
      prop.y <- c(0,1)
      mid.y <- 0.5
      if(!is.null(mos.y)){
        tmp <- scresults$sge[scresults$sge$nfactors==1 & !scresults$sge[,mos.y]%in%not.used,]
        tmp <- arrange_(tmp, .dots = c(mos.y))
        prop.y <- cumsum(tmp[,'N.of.subjects'])
        prop.y <- c(0,prop.y)/max(prop.y)
        mid.y <- (prop.y[-length(prop.y)]+prop.y[-1])/2
        names(mid.y) <- paste0(mos.y,'=',tmp[,mos.y])
        
        
        ##y2
        if(!is.null(mos.y2)){
          tmp <- scresults$sge[scresults$sge$nfactors==2 & !scresults$sge[,mos.y]%in%not.used &
                                 !scresults$sge[,mos.y2]%in%not.used,]
          tmp <- arrange_(tmp, .dots = c(mos.y,mos.y2))
          prop.y <- cumsum(tmp[,'N.of.subjects'])
          prop.y <- c(0,prop.y)/max(prop.y)
          mid.y <- (prop.y[-length(prop.y)]+prop.y[-1])/2
          names(mid.y) <- paste0(mos.y,'=',tmp[,mos.y],'\n',mos.y2,'=',tmp[,mos.y2])
          
        }
        
      }
      
      ##z
      if(req(input$logmosaic)=="lin"){
        rg.z <- range(scresults$sge[,mos.z], na.rm=TRUE)}
      if(req(input$logmosaic)=="log"){
        rg.z <- log(range(scresults$sge[,mos.z], na.rm=TRUE))
      }
      
      if (is.null(mos.y)){
        tmp <- scresults$sge[scresults$sge$nfactors==1 & !scresults$sge[,mos.x]%in%not.used,]
      }else{
        if (is.null(mos.y2)){
          tmp <- scresults$sge[scresults$sge$nfactors==2 & !scresults$sge[,mos.x]%in%not.used & !scresults$sge[,mos.y]%in%not.used,]
        }else{
          tmp <- scresults$sge[scresults$sge$nfactors==3 & !scresults$sge[,mos.x]%in%not.used &
                                 !scresults$sge[,mos.y]%in%not.used & !scresults$sge[,mos.y2]%in%not.used,]
        }
      }
      
      #ensure that all possible combinations are present
      if(any(sapply(tmp, class)=='character')){
        tmp[sapply(tmp, is.character)] <- lapply(tmp[sapply(tmp, is.character)], factor)
      }
      comb.full <- unique(expand.grid(tmp[,c(mos.x,mos.y,mos.y2), drop=FALSE]))
      tmp <- merge(tmp,comb.full,all.y=TRUE)
      
      tmp <- arrange_(tmp, .dots = c(mos.x,mos.y,mos.y2))
      if(req(input$logmosaic)=="lin"){
        val.z <- matrix(tmp[,mos.z], ncol=length(prop.x)-1, byrow=FALSE)}
      if(req(input$logmosaic)=="log"){
        val.z <- matrix(log(tmp[,mos.z]), ncol=length(prop.x)-1, byrow=FALSE)
      }
      
      #color function with bias to ensure that overall mean is indicated by middle color (white)
      mean.z <- ifelse(req(input$logmosaic)=="lin",
                       scresults$results_total[,mos.z],
                       log(scresults$results_total[,mos.z]))
      tr.mean.z <- (mean.z-rg.z[1])/diff(rg.z)
      f_colZ <- colorRamp(colrange.z, bias=log(tr.mean.z, base=0.5))
      
      #create plot
      par(mar=c(1,8,3,12), bg=col.bg, oma=c(0,0,0,0))
      plot(NULL, xlim=c(0,1), ylim=c(0,1), xlab='', ylab='', axes=FALSE, xaxs='i', yaxs='i')
      for (i in 1:length(mid.x)){
        for (j in 1:length(mid.y)){
          val.z.ij <- val.z[j,i]
          col.z.ij <- ifelse(is.na(val.z.ij),col.bg,
                             rgb(f_colZ((val.z.ij-rg.z[1])/diff(rg.z)), maxColorValue=255))
          rect(xleft=prop.x[i], xright=prop.x[i+1],
               ybottom=prop.y[j], ytop=prop.y[j+1],
               col=col.z.ij, border=col.bg, lwd=4)
        }
      }
      
      #add labels
      ##x
      text(x=mid.x, y=1.025, xpd=NA, adj=c(0.5,0), col=col.txt, labels=names(mid.x), cex=ifelse(is.null(mos.y2),1,0.75))
      ##y
      text(y=mid.y, x=-0.025, xpd=NA, adj=c(1,0.5), col=col.txt, labels=names(mid.y), srt=0, cex=ifelse(is.null(mos.y2),1,0.75))
      
      #add color gradient legend
      leg.x <- grconvertX(1,'npc','user')+0.5*(grconvertX(1,'ndc','user')-grconvertX(1,'npc','user'))
      leg.y <- seq(grconvertY(0.1,'npc','user'),grconvertY(0.9,'npc','user'),length.out=201)
      leg.width <- 0.05
      rect(xleft=leg.x-leg.width/2, xright=leg.x+leg.width/2, ybottom=leg.y[-1], ytop=leg.y[-length(leg.y)], xpd=NA,
           col=rgb(f_colZ(seq(0,1,length.out=length(leg.y)-1)), maxColorValue=255), border=NA)
      
      ndig <- 2
      if(req(input$logmosaic)=="lin"){
        ticks.q <- c(0,1,2,3,4)/4
        text(x=leg.x-(leg.width/2+0.01), y=quantile(leg.y, prob=ticks.q), xpd=NA, col=col.txt, adj=c(1,0.5),
             labels=round(quantile(seq(rg.z[1],rg.z[2], length.out=201), prob=ticks.q),ndig), cex=0.75)
      }
      if(req(input$logmosaic)=="log"){
        ticks.q <- c(0,1,2,3,4)/4
        text(x=leg.x-(leg.width/2+0.01), y=quantile(leg.y, prob=ticks.q), xpd=NA, col=col.txt, adj=c(1,0.5),
             labels=round(exp(quantile(seq(rg.z[1],rg.z[2], length.out=201), prob=ticks.q)),ndig), cex=0.75)
      }
      segments(x0=leg.x+(leg.width/2), x1=leg.x+(leg.width/2+0.01), y0=quantile(leg.y, prob=tr.mean.z),
               col=col.txt, lwd=2, xpd=NA)
      text(x=leg.x+(leg.width/2+0.02), y=quantile(leg.y, prob=tr.mean.z), xpd=NA, col=col.txt, adj=c(0,0.5), font=2,
           labels=paste0(ifelse(req(input$logmosaic)=="lin",round(mean.z, ndig),round(exp(mean.z), ndig)),' (total)'), cex=0.75)
      text(x=leg.x, y=grconvertY(0.925,'npc','user'), xpd=NA, col=col.txt, adj=c(0.5,0), srt=0,
           labels=mos.z, cex=1, font=2)
      
      
    }, bg = "transparent")
    
    ####...... hover functionality ####
    
    hoverlabel <- reactiveValues(
      value = NULL
    )
    
    observeEvent(c(input$plot_hover$x,input$plot_hover$y,input$var1,input$var2,input$var22,input$var3), ignoreNULL=FALSE, {
      if(!is.null(input$plot_hover$x) & !is.null(input$plot_hover$y)){
        
        mos.x <- var_mos1$val
        mos.y <- var_mos2$val
        mos.y2 <- var_mos22$val
        mos.z <- var_mos3$val
        
        #not.used <- c('Not used','No data')
        not.used <- 'Not used'
        
        if (mos.y=='no selection'){
          mos.y <- NULL
        }
        if (mos.y2=='no selection' | is.null(mos.y)){
          mos.y2 <- NULL
        }
        if (!is.null(mos.y)){
          if (mos.x==mos.y){
            mos.y <- NULL
          }
        }
        if (!is.null(mos.y2)){
          if (mos.x==mos.y2 | mos.y==mos.y2){
            mos.y2 <- NULL
          }
        }
        
        ##x
        tmp <- scresults$sge[scresults$sge$nfactors==1 & !scresults$sge[,mos.x]%in%not.used,]
        tmp <- arrange_(tmp, .dots = c(mos.x))
        prop.x <- cumsum(tmp[,'N.of.subjects'])
        prop.x <- c(0,prop.x)/max(prop.x)
        mid.x <- (prop.x[-length(prop.x)]+prop.x[-1])/2
        names(mid.x) <- paste0(mos.x,'=',tmp[,mos.x])
        hov.x <- tmp[,mos.x]
        
        ##y
        prop.y <- c(0,1)
        mid.y <- 0.5
        if(!is.null(mos.y)){
          if (is.null(mos.y2)){
            
            ##y1
            tmp <- scresults$sge[scresults$sge$nfactors==1 & !scresults$sge[,mos.y]%in%not.used,]
            tmp <- arrange_(tmp, .dots = c(mos.y))
            prop.y <- cumsum(tmp[,'N.of.subjects'])
            prop.y <- c(0,prop.y)/max(prop.y)
            mid.y <- (prop.y[-length(prop.y)]+prop.y[-1])/2
            names(mid.y) <- paste0(mos.y,'=',tmp[,mos.y])
            
            hov.y <- tmp[,mos.y]
            
          }else{
            
            ##y1 and y2
            tmp <- scresults$sge[scresults$sge$nfactors==2 & !scresults$sge[,mos.y]%in%not.used &
                                   !scresults$sge[,mos.y2]%in%not.used,]
            tmp <- arrange_(tmp, .dots = c(mos.y,mos.y2))
            prop.y <- cumsum(tmp[,'N.of.subjects'])
            prop.y <- c(0,prop.y)/max(prop.y)
            mid.y <- (prop.y[-length(prop.y)]+prop.y[-1])/2
            names(mid.y) <- paste0(mos.y,'=',tmp[,mos.y],'\n',mos.y2,'=',tmp[,mos.y2])
            
            hov.y <- tmp[,c(mos.y,mos.y2)]
            
          }
          
        }
        
        if (is.null(mos.y)){
          tmp <- scresults$sge[scresults$sge$nfactors==1 & !scresults$sge[,mos.x]%in%not.used,]
        }else{
          if (is.null(mos.y2)){
            tmp <- scresults$sge[scresults$sge$nfactors==2 & !scresults$sge[,mos.x]%in%not.used & !scresults$sge[,mos.y]%in%not.used,]
          }else{
            tmp <- scresults$sge[scresults$sge$nfactors==3 & !scresults$sge[,mos.x]%in%not.used &
                                   !scresults$sge[,mos.y]%in%not.used & !scresults$sge[,mos.y2]%in%not.used,]
          }
        }
        #ensure that all possible combinations are present
        if(any(sapply(tmp, class)=='character')){
          tmp[sapply(tmp, is.character)] <- lapply(tmp[sapply(tmp, is.character)], factor)
        }
        comb.full <- unique(expand.grid(tmp[,c(mos.x,mos.y,mos.y2), drop=FALSE]))
        tmp <- merge(tmp,comb.full,all.y=TRUE)
        
        tmp <- arrange_(tmp, .dots = c(mos.x,mos.y,mos.y2))
        
        col.disp <- unique(c(mos.x,mos.y,mos.y2,setdiff(colnames(tmp), c(scresults$factors, 'nfactors'))))
        
        if (is.null(mos.y)){
          hoverlabel$value <- tmp[tmp[,mos.x] == (hov.x[cut(input$plot_hover$x, prop.x, labels = FALSE)]), col.disp]
        }else{
          if (is.null(mos.y2)){
            hoverlabel$value <- tmp[tmp[,mos.x] == (hov.x[cut(input$plot_hover$x, prop.x, labels = FALSE)]) &
                                      tmp[,mos.y] == (hov.y[cut(input$plot_hover$y, prop.y, labels = FALSE)]),col.disp]
          }else{
            hoverlabel$value <- tmp[tmp[,mos.x] == (hov.x[cut(input$plot_hover$x, prop.x, labels = FALSE)]) &
                                      tmp[,mos.y] == (hov.y[,mos.y][cut(input$plot_hover$y, prop.y, labels = FALSE)]) &
                                      tmp[,mos.y2] == (hov.y[,mos.y2][cut(input$plot_hover$y, prop.y, labels = FALSE)]),col.disp]
          }
        }
        
        if(!is.null(hoverlabel$value)){
          dt.sginfo <- DT::datatable(hoverlabel$value, 
                                     extensions = 'Buttons',
                                     escape = FALSE, 
                                     options = list(
                                       initComplete = JS(
                                         "function(settings, json) {",
                                         paste0('"$(this.api().table().header()).css({\'background-color\': \'',colthemeCol$ColorBGplot,', \'color\': \'',colthemeCol$font.col,'\'});"'),
                                         "}"),
                                       dom = 'rtp', paging=FALSE, pageLength = 1, bSort=FALSE 
                                     ),
                                     class = 'cell-border stripe', rownames = FALSE,
                                     caption = 'Subgroup information', filter='none')
          
          dt.sginfo <- formatStyle(dt.sginfo, columns=1:ncol(hoverlabel$value),
                                   backgroundColor = colthemeCol$panel.col,
                                   border = paste0('.5px solid ',colthemeCol$ColorBGplot))
          
          output$tmp_info <- DT::renderDataTable(dt.sginfo)
        }
      }else{
        output$tmp_info <- DT::renderDataTable(NULL)
      }
    })
  }


SGEApp <- shiny::shinyApp(ui=ui, server=server)