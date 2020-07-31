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

suppressMessages(library(shiny))
suppressMessages(library(shinyjs))
suppressMessages(library(bsplus))
suppressMessages(library(V8))
suppressMessages(library(jsonlite))
suppressMessages(library(colourpicker))
suppressMessages(library(DT))
suppressMessages(library(shinyWidgets))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))

PreSelectXAxis <- "N.of.subjects"
PreSelectTarget <- ""

#### SCREENING MODULE USER INTERFACE ####
screeningModule_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(debug = TRUE),
    shiny::column(4,
      shiny::uiOutput(ns("screening_size"))
    ),
    shiny::column(4,
      shiny::uiOutput(ns("screening_satis"))
    )
  )
}

#### SCREENING MODULE SERVER ####
screeningModule_Server <- function(input, output, session, label, module_input) {
  shiny::observe({module_input})
  shiny::observe({label})
  output$screening_size <- shiny::renderUI({
    shinyWidgets::prettyRadioButtons(
      inputId = session$ns("screening_size"),
      label = "Is the Subgroup size big enough?",
      choices = c("No", "N/A", "Yes"),
      selected = module_input[which(rownames(module_input) == paste0("Subgroup ID: ", label)), 1],
      inline = TRUE,
      status = "success",
      icon = icon("check-circle")
    )
  })

  output$screening_satis <- shiny::renderUI({
    shinyWidgets::prettyRadioButtons(
      inputId = session$ns("screening_satis"),
      label = "Is the effect remarkable?",
      choices = c("No","N/A", "Yes"),
      selected = module_input[which(rownames(module_input) == paste0("Subgroup ID: ", label)), 2],
      inline = TRUE,
      status = "success",
      icon = icon("check-circle")
    )
  })
  return(
    list(
      size = shiny::reactive({input$screening_size}),
      satis = shiny::reactive({input$screening_satis}),
      label = shiny::reactive({label})
    )
  )
}

#### JavaScript Code ####
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
#### CSS code ####
css <- ".nav li a.disabled {
background-color: #aaa !important;
color: #333 !important;
cursor: not-allowed !important;
border-color: #aaa !important;
}"

#### Functions ####

interaction_plot2 <- function (
  df_data,
  fac1,
  fac2 = NULL,
  fac3 = NULL,
  response,
  bg.col ="#6B6B6B",
  bg.col2 = NULL,
  font.col = "white",
  y.min = "NA",
  y.max = "NA",
  box.col = "white",
  factor_gold = NULL,
  plot_type = "") {

  sg_green <- "#5cb85c"
  sg_blue <- "#3a6791"
  f_col <- colorRamp(c(sg_blue, "gray89", sg_green))

  if (y.min != "NA") {
    v_min <- y.min
  } else if (y.min == "NA") {
    v_min <- min(df_data[response])
  }

  if (y.max != "NA") {
    v_max <- y.max
  } else if (y.max == "NA") {
    v_max <- max(df_data[response])
  }

  lev1 <- levels(df_data[, fac1])
  lev1 <- lev1[lev1 != "Not used"]

  if (!is.null(fac2)) {
    lev2 <- levels(df_data[, fac2])
    lev2 <- lev2[lev2 != "Not used"]
  }

  if (!is.null(fac3)) {
    lev3 <- levels(df_data[, fac3])
    lev3 <- lev3[lev3 != "Not used"]
  }

  if (is.null(fac2) & is.null(fac3)) {
    plot(
      as.numeric(factor(lev1)),
      df_data[[response]],
      type = "b",
      ylim = c(v_min, v_max),
      axes = FALSE,
      log = plot_type
    )

    rect(
      xleft = grconvertX(0,'ndc','user') - 1000,
      xright = grconvertX(1,'ndc','user') + 1000,
      ybottom = grconvertY(0,'ndc','user'),
      ytop = grconvertY(1,'ndc','user'),
      border = NA,
      col = bg.col,
      xpd = TRUE
    )

    if (!is.null(bg.col2)) {
      rect(
        xleft = grconvertX(0,'npc','user'),
        xright = grconvertX(1,'npc','user'),
        ybottom = grconvertY(0,'npc','user') ,
        ytop = grconvertY(1,'npc','user'),
        border = NA,
        col = bg.col2,
        xpd = TRUE
      )
    }

    points(
      as.numeric(factor(lev1)),
      df_data[[response]],
      type = "l",
      ylim = c(v_min, v_max),
      lwd = 3,
      cex = 1.4,
      col = sg_green
    )

    if (!is.null(factor_gold)) {
      points(
        as.numeric(factor(lev1)),
        df_data[[response]],
        type = "p",
        ylim = c(v_min, v_max),
        col = "blue",
        pch = 16,
        cex = 1.4
      )
      points(
        as.numeric(factor(lev1))[which(levels(factor(lev1)) == as.character(factor_gold))],
        df_data[[response]][which(levels(factor(lev1)) == as.character(factor_gold))],
        type = "p",
        ylim = c(v_min,v_max),
        col="gold3",
        pch = 16,
        cex = 1.4
      )
    }

    box(col = box.col)

    axis(
      1,
      at = seq_along(as.numeric(factor(lev1))),
      labels = as.character(factor(lev1)),
      col = font.col,
      col.axis = font.col
    )

    axis(
      2,
      col = font.col,
      col.axis = font.col
    )

    title(ylab = response,
          xlab = fac1,
          col.main = font.col,
          col.lab = font.col
    )
  } else if (!is.null(fac2) & is.null(fac3)) {

    layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE), heights = c(8, 2))

    data_cols <- rgb(f_col(seq(0, 1, length = length(lev1))), maxColorValue = 255)

    for (i in 1:length(lev1)) {
      dat <- df_data[df_data[fac1] == lev1[i], ]

      if (i == 1) {
        plot(
          as.numeric(factor(lev2)),
          dat[[response]],
          type = "b",
          ylim = c(v_min, v_max),
          axes = FALSE,
          log = plot_type
        )
        rect(
          xleft = grconvertX(0,'ndc','user') - 1000,
          xright = grconvertX(1,'ndc','user') + 1000,
          ybottom = grconvertY(0,'ndc','user'),
          ytop = grconvertY(1,'ndc','user'),
          border = NA,
          col = bg.col,
          xpd = TRUE
        )

        if (!is.null(bg.col2)) {
          rect(
            xleft = grconvertX(0, 'npc', 'user'),
            xright = grconvertX(1, 'npc', 'user'),
            ybottom = grconvertY(0, 'npc', 'user'),
            ytop = grconvertY(1, 'npc', 'user'),
            border = NA,
            col = bg.col2,
            xpd = TRUE
          )
        }
      }
      points(
        as.numeric(factor(lev2)),
        dat[[response]],
        type = "l",
        ylim = c(v_min, v_max),
        lwd = 3,
        cex = 1.4,
        col = data_cols[i]
      )

      if (!is.null(factor_gold)) {
        points(
          as.numeric(factor(lev2)),
          dat[[response]],
          type = "p",
          ylim = c(v_min, v_max),
          col = "blue",
          bg = sg_green,
          pch = 21,
          cex = 1.5
        )

        if (factor_gold[1] == lev1[i]) {
          points(
            as.numeric(factor(lev2))[which(levels(factor(lev2)) == as.character(factor_gold[2]))],
            dat[[response]][which(levels(factor(lev2)) == as.character(factor_gold[2]))],
            type = "p",
            ylim = c(v_min,v_max),
            col = "gold3",
            bg = sg_green,
            pch = 21,
            cex = 1.5
          )
        }
      }

      if (i == 1) {
        box(col = box.col)

        axis(
          1,
          at = seq_along(as.numeric(factor(lev2))),
          labels = as.character(factor(lev2)),
          col = font.col,
          col.axis = font.col
        )

        axis(
          2,
          col = font.col,
          col.axis = font.col
        )

        title(
          ylab = response,
          xlab = fac2,
          col.main = font.col,
          col.lab = font.col
        )
      }
    }

    par(mar = c(0, 0, 0, 0))
    plot(
      NULL,
      NULL,
      xlim = c(0,1),
      ylim = c(0,1),
      bg = "grey",
      axes = FALSE
    )
    rect(
      xleft = grconvertX(0,'ndc','user') - 1000,
      xright = grconvertX(1,'ndc','user') + 1000,
      ybottom = grconvertY(0,'ndc','user'),
      ytop = grconvertY(1,'ndc','user'),
      border = NA,
      col = bg.col,
      xpd = TRUE
    )
    legend(
      "center",
      legend = paste0(fac1, " = ", lev1),
      col = data_cols,
      lwd = 3,
      horiz = FALSE,
      bg = bg.col2,
      box.col = font.col,
      text.col = font.col
    )

    par(mfrow = c(1,1), mar = c(5.1, 4.1, 4.1, 2.1))

  } else if (!is.null(fac2) & !is.null(fac3)) {
    data_cols <- rgb(f_col(seq(0, 1, length = length(lev1))), maxColorValue = 255)
    layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE) , heights = c(8, 2))
    for (j in 1:length(lev3)) {
      df_data_tmp <- df_data[df_data[fac3] == lev3[j], ]
      for (i in 1:length(lev1)) {
        dat <- df_data_tmp[df_data_tmp[fac1] == lev1[i], ]
        if (i == 1) {
          plot(
            as.numeric(factor(lev2)),
            dat[[response]],
            type = "b",
            ylim = c(v_min, v_max),
            axes = FALSE,
            log = plot_type
          )
          rect(
            xleft = grconvertX(0,'ndc','user') - 1000,
            xright = grconvertX(1,'ndc','user') + 1000,
            ybottom = grconvertY(0,'ndc','user'),
            ytop = grconvertY(1,'ndc','user'),
            border = NA,
            col = bg.col,
            xpd = TRUE
          )

          if (!is.null(bg.col2)) {
            rect(
              xleft = grconvertX(0, 'npc', 'user'),
              xright = grconvertX(1, 'npc', 'user'),
              ybottom = grconvertY(0, 'npc', 'user'),
              ytop = grconvertY(1, 'npc', 'user'),
              border = NA,
              col = bg.col2,
              xpd = TRUE
            )
          }
        }
        points(
          as.numeric(factor(lev2)),
          dat[[response]],
          type = "l",
          ylim = c(v_min, v_max),
          lwd = 3,
          cex = 1.4,
          col = data_cols[i]
        )

        if (!is.null(factor_gold)) {
          points(
            as.numeric(factor(lev2)),
            dat[[response]],
            type = "p",
            ylim = c(v_min, v_max),
            col = "blue",
            pch = 16,
            cex = 1.4
          )

          if (factor_gold[1] == lev1[i] & factor_gold[3] == lev2[j]) {
            points(
              as.numeric(factor(lev2))[which(levels(factor(lev2)) == as.character(factor_gold[2]))],
              dat[[response]][which(levels(factor(lev2)) == as.character(factor_gold[2]))],
              type = "p",
              ylim = c(v_min,v_max),
              col = "gold3",
              pch = 16,
              cex = 1.4
            )
          }
        }
        if (i == 1) {
          box(col = box.col)
          axis(
            1,
            at = seq_along(as.numeric(factor(lev2))),
            labels = as.character(factor(lev2)),
            col = font.col,
            col.axis = font.col
          )
          axis(
            2,
            col = font.col,
            col.axis = font.col
          )

          title(
            main = paste0(fac3, " = ", lev3[j]),
            ylab = response,
            xlab = fac2,
            col.main = font.col,
            col.lab = font.col
          )
        }
      }
    }
    par(mar = c(0, 0, 0, 0))
    plot(
      NULL,
      NULL,
      xlim = c(0,10),
      ylim = c(0,1),
      bg = "grey",
      axes = FALSE
    )
    rect(
      xleft = grconvertX(0,'ndc','user') - 1000,
      xright = grconvertX(1,'ndc','user') + 1000,
      ybottom = grconvertY(0,'ndc','user'),
      ytop = grconvertY(1,'ndc','user'),
      border = NA,
      col = bg.col,
      xpd = TRUE
    )
    legend(
      "center",
      legend = paste0(fac1, " = ", lev1),
      col = data_cols,
      lwd = 3,
      bg = bg.col2,
      box.col = font.col,
      text.col = font.col,
      horiz = FALSE
    )
    par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))
  }
}

createCombinationMatrix <- function(n, k, l) {
  t(do.call(cbind, lapply(k:l, function(x) utils::combn(n, x, tabulate, nbins = n))))
}

font_color <- function (hex_code) {
  ifelse(
    ((col2rgb(hex_code)[1] * 0.299) + (col2rgb(hex_code)[2] * 0.587) + (col2rgb(hex_code)[3] * 0.114) > 186),
    "#000000",
    "#ffffff"
  )
}

different_hues <- function(hex_code, value = 21) {
  ifelse(
    ((col2rgb(hex_code)[1] * 0.299) + (col2rgb(hex_code)[2] * 0.587) + (col2rgb(hex_code)[3] * 0.114) > 186),
    rgb(max(col2rgb(hex_code)[1] - value, 0), max(col2rgb(hex_code)[2] - value, 0), max(col2rgb(hex_code)[3] - value, 0), maxColorValue = 255),
    rgb(min(col2rgb(hex_code)[1] + value, 255), min(col2rgb(hex_code)[2] + value, 255), min(col2rgb(hex_code)[3] + value,255), maxColorValue = 255)
  )
}

is.integer0 <- function(x) {
  is.integer(x) && length(x) == 0L
}

factorialContext <- function(data, SGID) {
  SGID <- SGID[1]
  if (is.null(SGID) | is.integer0(SGID)){} else {

    nfac <- data$sge[which(data$sge$SGID == SGID), ]$nfactors
    tmp <- colnames(data$sge[which(data$sge$SGID == SGID), data$factors])[which(data$sge[data$sge$SGID == SGID, data$factors] != "Not used")]
    tmp2 <- data$sge[apply(data$sge[ , c("SGID","nfactors", tmp)] != "Not used", 1, sum) == (2 + nfac), ]
    tmp3 <- tmp2[tmp2$nfactors == nfac,]
    ges <- 1
    if (length(tmp) > 0) {
      for(i in 1:length(tmp)) {
        ges <- sum(levels(data$sge[[tmp[i]]]) != "Not used") * ges
      }
    } else {
       ges <- 0
    }

    status_ <- ifelse(ges == dim(tmp3)[1], "Complete", "Incomplete")

    return(list(
      'Factorial' = tmp3,
      'Number Factors' = nfac,
      'Variables' = tmp,
      'Status' = status_
      )
    )
  }
}

parents <- function(data, SGID) {
  if (is.null(SGID) | is.integer0(SGID)) {} else {
    Parents_start <- NULL
    for (k in 1:length(SGID)) {
      start <- data$sge[data$sge$SGID == SGID[k], ]
      if (start$nfactors == 1) {
        Parents_start <- NULL
      } else {
        tmp <- start[, colnames(start) %in% data$factors]
        tmp2 <- tmp[, which(start[, colnames(start) %in% data$factors] != "Not used")]
        ind <- which(colnames(start) %in% colnames(tmp2))
        M1 <- as.data.frame(
          createCombinationMatrix(
            start$nfactors,
            start$nfactors - 1,
            start$nfactors - 1
          )
        )
        for (i in 1:length(ind)) {
          M1[M1[, i] == 1, i] <- as.character(start[, ind[i]])
          M1[M1[, i] == 0, i] <- "Not used"
        }
        colnames(M1) <- colnames(tmp2)
        M_ <- data$sge[(data$sge$nfactors == start$nfactors - 1), ]
        tmp3 <- M_[, which(colnames(data$sge) %in% colnames(M1))]
        ind <- c()
        for (i in 1:length(M1)) {
          ind[i] <- as.numeric(names(which(apply(apply(tmp3, 1, '==', M1[i, ]), 2, all))))
        }
        Parents_start <- rbind(Parents_start, data$sge[data$sge$SGID %in% ind, ])
      }
    }
    return(list('Parents' = Parents_start))
  }
}

#### SGEAPP ####

#### UI ####
ui <- shiny::navbarPage(
  title = shiny::div(
    shiny::img(src = 'www/subscreen_logo.png',
        style = "margin-top: 3px; padding-right:10px;padding-bottom:10px",
        height = 55
    )
  ),
  windowTitle = "Subscreen Explorer",
  id = "navpanel",
  ##### SUBSCREEN EXPLORER TAB ####
  shiny::tabPanel(
    "Subscreen Explorer",
    value = 1,
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          '.navbar-nav > li > a, .navbar-brand {
          padding-top:4px !important;
          padding-bottom:0 !important;
          height: 60px;
          }
          .navbar {min-height:25px !important;}'
        )
      )
    ),
    shiny::tags$style(
      shiny::HTML(
        "#header4{color: #e2b007;}"
      )
    ),
    shiny::tags$style(
      shiny::HTML(
        ".navbar-default .navbar-brand:hover {color: #a3a3a3; background-color: #393939;}
        .navbar { background-color: #383838;}
        .navbar-default .navbar-nav > li > a {color:#7a7a7a;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color: #dedede; background-color: #404040;}
        .navbar-default .navbar-nav > li > a:hover {color: #999999;}
        "
      )
    ),
    chooseSliderSkin("Nice", color = "#112446"),
    ####... 1. cont_nav (uiOutput)####
    shiny::uiOutput('cont_nav'),
    shiny::fluidPage(
      ####... 2. cont (uiOutput)####
      shiny::uiOutput('cont'),
      ####... 3. cont2 (uiOutput)####
      shiny::uiOutput('cont2'),
      shiny::fluidRow(
        ####... 4. logo (uiOutput)####
        shiny::uiOutput('logo'),
        shiny::column(3,
          #### VARIABLE OPTIONS TAB ####
          shiny::tabsetPanel(type = "tabs",
            shiny::tabPanel("Variable Options",
              ####.. wellpanel variable ####
              shiny::wellPanel(class = "myclass1", id = "myid1",
                ####... 5. cont_well (uiOutput)####
                shiny::uiOutput('cont_well'),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question"),
                    title = "Variable plotted on the y-axis.",
                    placement = "top"
                  )
                ),
                ####... 6. y (selectInput)####
                shiny::selectInput(
                  inputId = "y",
                  label = "Target variable",
                  names(scresults$results_total),
                  selected = names(scresults$results_total)[1]
                ),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question"),
                    title = "Variable plotted on the x-axis.",
                    placement = "top",
                    expanded = TRUE
                  )
                ),
                ####... 7. x (selectInput)####
                shiny::selectInput(
                  inputId = "x",
                  label = "Reference variable",
                  choices = names(scresults$results_total),
                  selected = "N.of.subjects"
                ),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question "),
                    title = "Select a filter variable. Subgroups containing this variable are displayed in green (default color).",
                    placement = "top"
                  )
                ),
                ####... 8. filter (selectInput)####
                shiny::selectInput(
                  inputId = "filter",
                  label = "Subgroup Filter",
                  choices = c("no selection", scresults$factors),
                  selected = c("no selection")
                ),
                bsplus::use_bs_popover(),
                bsplus::use_bs_tooltip(),
                shiny::conditionalPanel(
                  condition = "input.filter != 'no selection'",
                  ####... 9. VarChosen (uiOutput)####
                  shiny::uiOutput("VarChosen"),
                  selectize = FALSE
                ),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question "),
                    title = "Subgroups containing selected number(s) of factor(s) are displayed in the plot.",
                    placement = "top",
                    expanded = TRUE
                  )
                ),
                ####... 10. key (sliderInput)####
                shiny::sliderInput(
                  inputId = "key",
                  label = "Subgroup level(s)",
                  min = scresults$min_comb,
                  max = scresults$max_comb,
                  ticks = FALSE,
                  value = c(1, min(c(3, scresults$max_comb), na.rm = TRUE)),
                  step = 1
                ),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_popover(
                    tag = shiny_iconlink("question"),
                    title = "Explanation:",
                    content = "Choose scale typ",
                    placement = "top"
                  )
                ),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question "),
                    title = "Change the scale on the y-axis.",
                    placement = "top",
                    expanded = TRUE
                  )
                ),
                ####... 11. plot_type (radioButtons)####
                shiny::radioButtons(
                  inputId = "plot_type",
                  label = "Plot Type",
                  selected = "lin",
                  inline = TRUE,
                  choiceNames = list("linear", "logarithmic"),
                  choiceValues = c("lin", "log")
                ),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question "),
                    title = " Change y-axis limits.",
                    placement = "top",
                    expanded = TRUE
                  )
                ),
                ####... 12. YRange (uiOutput)####
                shiny::uiOutput("YRange"),
                "Help text:",
                bsplus::bs_embed_tooltip(
                  tag = shiny_iconlink("circle"),
                  title = "Click on the plot. Then automatically the subgroups around this point will appear in the
                  'Selected Subgroups'-tab. In order to make further calculations (parent subgroups, factorial context, complement subgroup), select a subgroup by clicking on the row in the 'Selected Subgroups'-table.
                  These selected dots are now displayed in gold (default color).",
                  placement = "top"
                ),
                bsplus::bs_embed_tooltip(
                  tag = shiny_iconlink("filter"),
                  title = "Select a filter variable. Subgroups containing this variable are shown in a table in the 'Filtered Subgroups'-tab.",
                  placement = "top"
                ),
                bsplus::bs_embed_tooltip(
                  tag = shiny_iconlink("sitemap"),
                  title = "Select a subgroup by clicking on a row in the 'Selected Subgroups'-table. All parent subgroups will be displayed in the 'Parent Subgroup'-tab.
                  Parent subgroup is defined as a subgroup with the factor-combination of the selected subgroup with 1 factor less.
                  ",
                  placement = "top"
                ),
                if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(scresults$sge))) {
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("list"),
                    title = "Select a subgroup by clicking a row of the 'Selected Subgroups' table. The factorial context of this subgroup will be
                    displayed in the 'Factorial Context'-tab.",
                    placement = "top"
                  )
                },
                if (any(startsWith(colnames(scresults$sge), "Complement_"))) {
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("times-circle"),
                    title = "Select a subgroup by clicking a row of the 'Selected Subgroups' table.
                    The complement of a subgroup itself is not a subgroup and can be calculated in the within the subscreencalc()-function.
                    ",
                    placement = "top"
                  )
                },
                bsplus::bs_embed_tooltip(
                  tag = shiny_iconlink("edit"),
                  title = "Use the Memorize Button in the 'Selected Subgroups'-tab to save subgroups. Memorized subgroups can also be deleted in the 'Memorize Subgroups'-tab.",
                  placement = "top"
                )
              ), icon = icon("wrench")
            ),
            #### IMPORTANCE TAB ####
            shiny::tabPanel("Importance Tab", value = "ImportanceTab",
              #### .. wellpanel importance ####
              shiny::wellPanel(class = "myclass8", id = "myid8",
                ####... 13. cont_well8 (uiOutput)####
                shiny::uiOutput('cont_well8'),
                ####... 14. Impo_opt (radioButtons)####
                shiny::radioButtons(
                  inputId = "Impo_opt",
                  label = shiny::HTML('<p style="color:white"> Importance Value Option </p>'),
                  choices = list(
                    "No Importance Value" = 0,
                    "Use Variable Importance Values" = 1,
                    "Use Ranking of Variable Importance Values" = 2
                  ),
                  selected = 0
                ),
                ####... 15. select_importance_variable (uiOutput)####
                shiny::uiOutput("select_importance_variable"),
                shiny::conditionalPanel("input.Impo_opt == '1'",
                  shiny::div(style = "position:absolute;right:2em;",
                    bsplus::bs_embed_tooltip(
                      tag = shiny_iconlink("question "),
                      title = "Use the slider to set the range of 'Important values' which
                      can be signified through colors in the plot.",
                      placement = "top",
                      expanded = TRUE
                    )
                  ),
                  ####... 16. impo (uiOutput)####
                 shiny::uiOutput("impo")
                ),
                shiny::conditionalPanel("input.Impo_opt == '2'",
                  shiny::div(style = "position:absolute;right:2em;",
                    bsplus::bs_embed_tooltip(
                      tag = shiny_iconlink("question "),
                      title = "Use the slider to adjust the number of variables",
                      placement = "top",
                      expanded = TRUE
                    )
                  ),
                  ####... 17. impo2 (uiOutput)####
                  shiny::uiOutput("impo2")
                ),
                shiny::conditionalPanel("input.Impo_opt == '2'",
                  shiny::div(style = "position:absolute;right:2em;",
                    bsplus::bs_embed_tooltip(
                      tag = shiny_iconlink("question "),
                      title = "The variable important order can be prioritized by using increasing or decreasing values.",
                      placement = "top",
                      expanded = TRUE
                    )
                  ),
                  ####... 18. decrease (radioButtons)####
                  shiny::radioButtons(
                    inputId = "decrease",
                    label = shiny::HTML('<p style="color:white"> Sorting order: </p>'),
                    choices = list("Increase" = FALSE, "Decrease" = TRUE),
                    selected = FALSE
                  )
                ),
                shiny::conditionalPanel("input.Impo_opt == '1'" ,
                  ####... 19. imp_var_list (tableOutput)####
                  shiny::tableOutput('imp_var_list')
                ),
                shiny::conditionalPanel("input.Impo_opt == '2'" ,
                  shiny::div(style = "position:absolute;right:2em;",
                    bsplus::bs_embed_tooltip(
                      tag = shiny_iconlink("question "),
                      title = "Variables for colorized dots are displayed in this table.",
                      placement = "top",
                      expanded = TRUE
                    )
                  ),
                  ####... 20. imp_var_list2 (tableOutput)####
                  shiny::tableOutput('imp_var_list2')
                ),
                bsplus::use_bs_popover(),
                bsplus::use_bs_tooltip()
              ),
              shinyjs::useShinyjs(debug = TRUE),
              shinyjs::extendShinyjs(text = jscode),
              shinyjs::inlineCSS(css),
              icon = icon("exclamation")
            ),
            #### DISPLAY OPTIONS TAB ####
            shiny::tabPanel("Display Options",
              ####.. wellpanel display1 ####
              shiny::wellPanel(class = "myclass6", id = "myid6",
                ####... 21. cont_well6 (uiOutput)####
                shiny::uiOutput('cont_well6'),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question"),
                    title = "Maximum distance to the click dot (in pixel).",
                    placement = "top",
                    expanded = TRUE
                  )
                ),
                ####... 22. pickradius (sliderInput)####
                shiny::sliderInput(
                  inputId = "pickradius",
                  label = "Choose distance to the click point",
                  min = 1,
                  max = 30,
                  value = 5,
                  step = 1 ,
                  ticks = FALSE
                ),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question "),
                    title = "Change the dot size.
                    Combinable with dot style option.",
                    placement = "top",
                    expanded = TRUE
                  )
                ),
                ####... 23. pointsize (sliderInput)####
                shiny::sliderInput(
                  inputId = "pointsize",
                  label = "Choose dot size" ,
                  min = 0.1,
                  max = 3,
                  value = 1,
                  step = 0.1
                ),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question"),
                    title = "Use the Subgroup size as given size or display
                    all dots with equal size.",
                    placement = "top",
                    expanded = TRUE
                  )
                ),
                ####... 24. circlestyle (radioButtons)####
                shiny::radioButtons(
                  inputId = "circlestyle",
                  label = "Point Style",
                  choiceNames = list("Standard", "Subgroup size"),
                  choiceValues = c("standard", "groupsize"),
                  selected = "standard",
                  inline = TRUE
                ),
                shiny::radioButtons(
                  inputId = "pch_value",
                  label = "Plotting character",
                  choiceNames = list("Circles", "Squares (faster)"),
                  choiceValues = c(19, '.'),
                  selected = 19,
                  inline = TRUE
                ),
                shiny::div(style = "position:absolute;right:2em;",
                  bsplus::bs_embed_tooltip(
                    tag = shiny_iconlink("question "),
                    title = "Adjust brightness of unmarked dots.",
                    placement = "top",
                    expanded = TRUE
                  )
                ),
                ####... 25. point_brightness (sliderInput)####
                shiny::sliderInput(
                  inputId = "point_brightness",
                  label = "Adjust dot brightness",
                  min = 0.1,
                  max = 1,
                  value = 1,
                  step = 0.1
                ),
                bsplus::use_bs_popover(),
                bsplus::use_bs_tooltip()
              ),
              ####.. wellpanel display2 ####
              shiny::wellPanel(class = "myclass7", id = "myid7",
                ####... 26. cont_well7 (uiOutput)####
                shiny::uiOutput('cont_well7'),
                ####... 27. xlabel (checkboxInput)####
                shiny::checkboxInput(
                  inputId = "xlabel",
                  label = "Show label of X-Axis",
                  value = TRUE
                ),
                ####... 28. grid (checkboxInput)####
                shiny::checkboxInput(
                  inputId = "grid",
                  label = "Display a grid",
                  value = FALSE
                ),
                bsplus::use_bs_popover(),
                bsplus::use_bs_tooltip()
              ),
              icon = icon('eye')
            ),
            #### COLOUR OPTIONS TAB####
            shiny::tabPanel("Colour Options",
              shiny::wellPanel(class = "myclass9", id = "myid9",
                ####... 29. cont_well9 (uiOutput)####
                shiny::uiOutput('cont_well9'),
                ####... 30. Panel_Colour (uiOutput)####
                shiny::uiOutput('Panel_Colour')
              ),
              bsplus::use_bs_popover(),
              bsplus::use_bs_tooltip(),
              icon = icon("paint-brush")
            )
          )
        ),
        #### .. graph1 ####
        shiny::column(6,
          ####... 31. graph (plotOutput)####
          shiny::div(style = "position:relative",
            shiny::plotOutput(
              outputId = "graph",
              click = "plot_click",
              hover = hoverOpts("plot_hover1",
                                delay = 300,
                                delayType = "debounce"
              ),
              height = 700
            ),
            shiny::uiOutput("hover_info1")
          )
        ),
        shiny::column(3,
          ####... 32. showPanel2 (prettyToggle)####
          shinyWidgets::prettyToggle(
            inputId = 'showPanel2',
            label_off = 'Interaction Plot',
            label_on = 'Interaction Plot',
            value = FALSE,
            outline = TRUE,
            status_on = "default",
            status_off = "default",
            plain = TRUE,
            icon_off = icon("chart-line"),
            icon_on = icon ("times")
          ),
          shiny::conditionalPanel(
            condition = 'input.showPanel2',
            ####... 33. interaction_panel (uiOutput)####
            shiny::uiOutput("interaction_panel")
          ),
          bsplus::use_bs_popover(),
          bsplus::use_bs_tooltip(),
          ####... 34. legend (uiOutput)####
          shiny::uiOutput('legend')
        ),
        ####... 35. absPanel (uiOutput)####
        shiny::uiOutput('absPanel'),
        ####... 36. screeningPanel (uiOutput)####
        shiny::uiOutput('screeningPanel')
      ),
      shiny::fluidRow(
        shiny::column(12,
          ####..Table Output ####
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel(
              "Selected Subgroups",
              ####... 37. selectedSG (dataTableOutput)####
              DT::dataTableOutput("selectedSG"),
              icon = icon("circle")
            ),
            shiny::tabPanel(
              title = "Filtered Subgroups",
              ####... 38. filteredSG (dataTableOutput)####
              DT::dataTableOutput("filteredSG"),
              icon = icon("filter")
            ),
            shiny::tabPanel(
              title = "Parent Subgroups",
              value = "ParentSubgroup",
              ####... 39. parents (dataTableOutput)####
              DT::dataTableOutput("parents"),
              icon = icon("sitemap")
            ),
            shiny::tabPanel(
              title = "Factorial Contexts",
              value = "FactorialSubgroup",
              ####... 40. factorial (dataTableOutput)####
              DT::dataTableOutput("factorial"),
              icon = icon("list")
            ),
            shiny::tabPanel(
              title ="Subgroup Complement",
              value = "ComplementSubgroup",
              ####... 41. complement (dataTableOutput)####
              DT::dataTableOutput("complement"),
              icon = icon("times-circle")
            ),
            shiny::tabPanel(
              title = "Memorized Subgroups",
              ####... 42. memorizedSG (dataTableOutput)####
              DT::dataTableOutput("memorizedSG"),
              icon = icon("edit")
            )
          )
        )
      )
    ), fluid = FALSE, position = c("static-top"), inverse = FALSE, icon = icon("braille")
  ),
  #### SUBSCREEN COMPARER TAB ####
  shiny::tabPanel(
    title = "Subscreen Comparer",
    value = 2,
    shiny::fluidRow(
      shiny::column(3,
        shiny::wellPanel(
          class = "myclass2",
          id = "myid2",
          ####... 43. cont_well2 (uiOutput)####
          shiny::uiOutput('cont_well2'),
          shiny::div(style = "position:absolute;right:2em;",
            bsplus::bs_embed_tooltip(tag = shiny_iconlink("question "),
            title = "Variable plotted on the y-axis (upper plot).",
            placement = "top",
            expanded = TRUE
            )
          ),
          ####... 44. y1 (uiOutput)####
          shiny::uiOutput("y1"),
          shiny::div(style = "position:absolute;right:2em;",
            bsplus::bs_embed_tooltip(tag = shiny_iconlink("question "),
            title = "Change the scale on the y-axis (upper plot).",
            placement = "top",
            expanded = TRUE
            )
          ),
          ####... 45. plot_type2 (uiOutput)####
          shiny::uiOutput("plot_type2"),
          shiny::div(style = "position:absolute;right:2em;",
            bsplus::bs_embed_tooltip(tag = shiny_iconlink("question"),
            title = "Change the y-axis limits (upper plot).",
            placement = "top",
            expanded = TRUE
            )
          ),
          ####... 46. YRange2 (uiOutput)####
          shiny::uiOutput("YRange2"),
          bsplus::use_bs_popover(),
          bsplus::use_bs_tooltip()
        ),
        shiny::wellPanel(
          class = "myclass3",
          id = "myid3",
          ####... 47. cont_well3 (uiOutput)####
          shiny::uiOutput('cont_well3'),
          shiny::div(style = "position:absolute;right:2em;",
            bsplus::bs_embed_tooltip(tag = shiny_iconlink("question "),
            title = "Variable t obe plotted on the y-axis (lower plot).",
            placement = "top",
            expanded = TRUE
            )
          ),
          ####... 48. y2 (uiOutput)####
          shiny::uiOutput("y2"),
          shiny::div(style = "position:absolute;right:2em;",
            bsplus::bs_embed_tooltip(tag = shiny_iconlink("question "),
            title = "Change the scale on the y-axis (lower plot).",
            placement = "top",
            expanded = TRUE
            )
          ),
          ####... 49. plot_type3 (uiOutput)####
          shiny::uiOutput("plot_type3"),
          shiny::div(style = "position:absolute;right:2em;",
            bsplus::bs_embed_tooltip(tag = shiny_iconlink("question "),
            title = "Change y-axis limits (lower plot).",
            placement = "top",
            expanded = TRUE
            )
          ),
          ####... 50. YRange3 (uiOutput)####
          shiny::uiOutput("YRange3"),
          bsplus::use_bs_popover(),
          bsplus::use_bs_tooltip()
        ),
        shiny::wellPanel(
          class = "myclass4",
          id = "myid4",
          ####... 51. cont_well4 (uiOutput)####
          shiny::uiOutput('cont_well4'),
          shiny::div(style = "position:absolute;right:2em;",
            bsplus::bs_embed_tooltip(tag = shiny_iconlink("question "),
            title = "Variable plotted on the x-axes.",
            placement = "top",
            expanded = TRUE
            )
          ),
          ####... 52. x2 (uiOutput)####
          shiny::uiOutput("x2")
        ),
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(type = "tabs",
          shiny::tabPanel("Compare",
            shiny::uiOutput('legend2'),
            shiny::column(7,
              ####... 56. graph2 (plotOutput)####
              shiny::div(style = "position:relative",
                shiny::plotOutput(
                  outputId = "graph2",
                  click = "plot_click",
                  hover = hoverOpts("plot_hover2", delay = 300, delayType = "debounce"),
                  height = 390,
                  width = 1100
                ),
                ####... 57. hover_info2 (uiOutput)####
                shiny::uiOutput("hover_info2")
              )
            ),
            shiny::column(7,
              ####... 58. graph3 (plotOutput)####
              shiny::div(
                style = "position:relative",
                shiny::plotOutput(
                  outputId = "graph3",
                  click = "plot_click2",
                  hover = hoverOpts("plot_hover3", delay = 300, delayType = "debounce"),
                  height = 390,
                  width = 1100
                ),
                ####59. hover_info3 (uiOutput)####
                shiny::uiOutput("hover_info3")
              )
            ),
            ####... 60. absPanel2 (uiOutput)####
            shiny::uiOutput('absPanel2')
          ),
          ####.. Bubble plot####
          shiny::tabPanel("Bubble plot",
            ####... 61. legend3 (uiOutput)####
            shiny::uiOutput('legend3'),
            shiny::column(7,
              ####... 62. graph4 (plotOutput)####
              shiny::div(style = "position:relative",
                shiny::plotOutput(
                  outputId = "graph4",
                  click = "plot_click3",
                  hover = hoverOpts("plot_hover4",
                                    delay = 300,
                                    delayType = "debounce"
                  ),
                  height = 780,
                  width = 1100
                ),
                ####... 63. hover_info4 (uiOutput)####
                shiny::uiOutput("hover_info4")
              )
            )
          )
        )
      )
    ), icon = icon("object-group")
  ),
  #### SUBSCREEN MOSAIC #####
  shiny::tabPanel(
    title = "Subscreen Mosaic",
    value = "SubscreenMosaic",
    shiny::fluidPage(
      shiny::fluidRow(
          shiny::column(3,
          ####... 64. PanelMosaic (uiOutput)####
          shiny::uiOutput('PanelMosaic')
        ),
        shiny::column(8,
          ####... 65. mosaic (plotOutput)####
          shiny::div(style = "position:relative",
            shiny::plotOutput(
              outputId = "mosaic",
              hover = hoverOpts(id = 'plot_hover', delay = 500, delayType = 'debounce'),
              height = 550,
              width = 750
            )
          ),
          shiny::br(),
          ####... 66. tmp_info (dataTableOutput)####
          DT::dataTableOutput("tmp_info")
        )
      )
    ), icon = icon("th-list")
  ),
  shiny::tabPanel(
    "Subscreen ASMUS",
    value = "subscreenasmus",
    shiny::fluidRow(
      shiny::column(8,
        shiny::HTML("<h3> <b style='color: #e2b007'>A</b>utomatic <b style='color: #e2b007'>S</b>creening of one- or <b style='color: #e2b007'>MU</b>lti-factorial <b style='color: #e2b007'>S</b>ubgroups - <b style='color: #e2b007'>ASMUS</b> </h3>")
      ),
      shiny::column(1,
       shiny::tags$style(".btn-custom {background-color: #e2b007; color: #FFF;}"),
        ####... 67. mydropdown_bgcolor (uiOutput)####
        shiny::uiOutput('mydropdown_bgcolor'),
        ####... 68. MyDropDown (dropdownButton)####
        shinyWidgets::dropdownButton(
          inputId = "MyDropDown",
          shiny::tags$h3("Settings"),
          ####... 69. plot_type_asmus (uiOutput)####
          shiny::uiOutput('plot_type_asmus'),
          ####... 70. yrange_asmus (uiOutput)####
          shiny::uiOutput('yrange_asmus'),
          ####... 71. keys_asmus (sliderInput)####
          shiny::sliderInput(
            inputId = "keys_asmus",
            label = "Subgroup level(s)",
            min = scresults$min_comb,
            max = scresults$max_comb,
            ticks = FALSE,
            value = c(1, min(c(3, scresults$max_comb), na.rm = TRUE)),
            step = 1
          ),
          ####... 72. y_Interaction_Button2 (uiOutput)####
          shiny::uiOutput('y_Interaction_Button2'),
          circle = TRUE,
          status = "custom",
          icon = icon("gear"),
          width = "300px",
          tooltip = tooltipOptions(title = "Click to see inputs!")
        )
      ),
      shiny::column(3,
        shiny::tags$style(".btn-custom {background-color: #e2b007; color: #FFF;}"),
        ####... 73. mydropdown_bgcolor2 (uiOutput)####
        shiny::uiOutput('mydropdown_bgcolor2'),
        ####... 74. MyDropDown2 (dropdownButton)####
        shinyWidgets::dropdownButton(
          inputId = "MyDropDown2",
          shiny::tags$h4("About ASMUS:"),
          shiny::tags$h5(shiny::tags$b(shiny::tags$u("When is a subgroup interesting?"))),
          "If the treatment effect is remarkable or noticeable and if the size of the subgroup is not too small to give a reliable estimate",
          shiny::tags$h5(shiny::tags$b(shiny::tags$u("What is a factorial context?"))),
          "For a subgroup defined by one factor the context consists of all levels of that factor. For multi-factorial subgroups the context is the set of all combinations of levels of the respective factors.",
          shiny::tags$h5(shiny::tags$b(shiny::tags$u("Completeness of factorial contexts"))),
          "Complete factorial context: For all possible factor level combinations there is an estimate",
          "Incomplete factorial context: For at least one factor level combination no estimate is available",
          "Pseudo(-complete) factorial context: An incomplete factorial context that can be made complete ignoring certain factor levels",
          circle = TRUE,
          status = "custom",
          icon = icon("info"),
          width = "300px",
          tooltip = tooltipOptions(title = "Click to see further Information!")
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(7,
        ####... 75. graph5 (plotOutput)####
        shiny::div(style = "position:relative",
          shiny::plotOutput(
            outputId = "graph5",
            hover = hoverOpts("plot_hover5", delay = 300, delayType = "debounce")
          ),
          ####... 76. hover_info5 (uiOutput)####
          shiny::uiOutput("hover_info5")
        )
      ),
      shiny::column(5,
       ####... 77. interaction2 (plotOutput)####
       shiny::plotOutput(outputId = 'interaction2')
      )
    ),
    ####... 78. legend4 (uiOutput)####
    shiny::uiOutput('legend4'),
    shiny::fluidRow(
      shiny::wellPanel(class = "myclass10", id = "myid10",
        ####... 79. cont_well3 (uiOutput)####
        shiny::uiOutput('cont_well10'),
        shiny::fluidRow(
          shiny::column(1,
            shiny::column(6,
              ####... 80. screening_backward (circleButton)####
              shinyWidgets::circleButton(
                inputId = "screening_backward",
                icon = icon("step-backward"),
                size = "sm",
                status = "default"
              )
            ),
            shiny::column(6,
              ####... 81. screening_forward (circleButton)####
              shinyWidgets::circleButton(
                inputId = "screening_forward",
                icon = icon("step-forward"),
                size = "sm",
                status = "default"
              )
            )
          ),
          shiny::column(2,
            ####... 82. header1 (uiOutput)####
            shiny::uiOutput('header1'),
            ####... 83. header2 (uiOutput)####
            shiny::uiOutput('header2')
          ),
          ####... 84. screening_ui (uiOutput)####
          shiny::column(8,
            shiny::uiOutput('screening_ui'),
            shiny::radioButtons(
              inputId = "direction",
              label = "Sorting direction",
              choices = c("Descending" = "desc",
                          "Ascending" = "asc"),
              selected = "desc"
            )
          )
        )
      )
    ),
    shiny::tabPanel("Subgroup Assessment",
      ####... 85. assessment (dataTableOutput)####
      DT::dataTableOutput("assessment"),
      icon = icon("clipboard")
    ), icon = icon("tasks")
  )
)

#### SERVER ####
server <- function(input, output, session) {

  if (exists("apppars")) {
    scrresults <- apppars$scrresults
    variable_importance <- apppars$variable_importance
    NiceNumbers <- apppars$NiceNumbers
  }

  if (!exists("apppars")) {
    if (file.exists("scresults.rds")) {
      scresults <- readRDS(file = "scresults.rds")
      cat("Note: Using scresults.rds from app folder")
    } else {
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

  shinyInput_remove <- function (FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, as.numeric(strsplit(input$select_button, "_")[[1]][2])), ...))
    }
    inputs
  }

  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, click_points_data$xy$SGID[i]), ...))
    }
    inputs
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

  #### DATA FRAMES ####
  click_points_data <- shiny::reactiveValues(xy = data.frame(x = NULL, y = NULL))

  select_points_data <- data.frame(x = numeric(), y = numeric(), SGID = numeric())

  sel_SG <- data.frame(Selected = "None")

  plot_points_data_complement <- shiny::reactive({
    shiny::req(input$y)
    if (input$y != "N.of.subjects" & any(startsWith(colnames(scresults$sge),"Complement_"))) {
      IDs <- click_points_data$xy[input$selectedSG_rows_selected,]$SGID
      data.frame(
        x = scresults$results_total$N.of.subjects - scresults$sge[which(scresults$sge ==IDs),  c(input$x)],
        y = scresults$sge[which(scresults$sge ==IDs), c(paste0("Complement_", input$y))]
      )
    } else {
      NULL
    }
  })

  plot_points_data <- shiny::reactive({
    shiny::req(input$x, input$y, input$key)
    data.frame(
      x = scresults$sge[, c(input$x)][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]],
      y = scresults$sge[, c(input$y)][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]],
      ID = scresults$sge[, "SGID"][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]]
    )
  })

  shiny::observe({plot_points_data()})

  plot_points_data2 <- shiny::reactive({
    shiny::req(input$key, input$x2, input$y1)
    data.frame(
      x = scresults$sge[, c(input$x2)][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]],
      y = scresults$sge[, c(input$y1)][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]],
      ID = scresults$sge[, "SGID"][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]]
    )
  })

  plot_points_data3 <- shiny::reactive({
    shiny::req(input$key, input$x2, input$y2)
    data.frame(
      x = scresults$sge[, c(input$x2)][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]],
      y = scresults$sge[, c(input$y2)][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]],
      ID = scresults$sge[, "SGID"][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]]
    )
  })

  plot_points_data4 <- shiny::reactive({
    shiny::req(input$key, input$y1, input$y2)
    data.frame(
      x = scresults$sge[, c(input$y1)][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]],
      y = scresults$sge[, c(input$y2)][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]],
      ID = scresults$sge[, "SGID"][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]]
    )
  })

  plot_points_data5 <- shiny::reactive({
    data.frame(
      x = scresults$sge[, c(input$x)][scresults$sge$nfactors >= input$keys_asmus[1] & scresults$sge$nfactors <= input$keys_asmus[2]],
      y = scresults$sge[, c(input$y)][scresults$sge$nfactors >= input$keys_asmus[1] & scresults$sge$nfactors <= input$keys_asmus[2]],
      ID = scresults$sge[, "SGID"][scresults$sge$nfactors >= input$keys_asmus[1] & scresults$sge$nfactors <= input$keys_asmus[2]])
  })

  SG_tit <- shiny::reactive({
    key <- shiny::req(input$key)
    if (key[1] == key[2]) paste(key[1], "-Factorial Subgroups (", length(plot_points_data()$x), ")", sep = "")
    else paste(key[1], " to ", key[2], "-Factorial Subgroups (", length(plot_points_data()$x), ")", sep = "")
  })

  SG_tit3 <- shiny::reactive({
    key <- shiny::req(input$keys_asmus)
    if (key[1] == key[2])
      paste(key[1], "-Factorial Subgroups (",
            length(plot_points_data5()$x), ")", sep = "")
    else paste(key[1], " to ", key[2], "-Factorial Subgroups (", length(plot_points_data5()$x),
               ")", sep = "")
  })

  log_type <- shiny::reactiveValues(
    graph1 = '',
    graph3 = ''
  )

  shiny::observeEvent(input$plot_type, {
    log_type$graph1 <- ifelse(input$plot_type == "log", "y", "")
  })

  log_type_asmus <- shiny::reactiveValues(graph5 = '')

  shiny::observeEvent(input$plot_type_asmus, {
    log_type_asmus$graph5 <- ifelse(input$plot_type_asmus == "log", "y", "")
  })

  vi_variable <- shiny::reactive({
    shiny::req(input$select_importance_variable)
    if (is.null(variable_importance)) {
      NULL
    } else if (!is.null(variable_importance) & input$select_importance_variable == "NULL") {
      variable_importance
    } else if (!is.null(variable_importance) & input$select_importance_variable != "NULL") {
      variable_importance[[input$select_importance_variable]]
    }
  })

  shiny::observeEvent(vi_variable(), {
    import_reac$reactive <- c(min(vi_variable()$Importance),
                              max(vi_variable()$Importance))
  })

  setcolor <- function() {

    key <- shiny::isolate(input$key)
    filter <- shiny::isolate(input$filter)

    f <- scresults$sge[which(scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]), ]
    p.col <- colthemeCol$ColorPoints
    bright <- input$point_brightness
    f$colour <- as.character(
      c(
        grDevices::adjustcolor(p.col, alpha = 1 * bright),
        grDevices::adjustcolor(p.col, alpha = 0.75 * bright),
        grDevices::adjustcolor(p.col, alpha = 0.5 * bright),
        grDevices::adjustcolor(p.col, alpha = 0.25 * bright),
        grDevices::adjustcolor(p.col, alpha = 0.1 * bright),
        grDevices::adjustcolor(p.col, alpha = 0.1 * bright),
        grDevices::adjustcolor(p.col, alpha = 0.1 * bright),
        grDevices::adjustcolor(p.col, alpha = 0.1 * bright)
      )
    )[match(f$nfactors, 1:8)]

    if (shiny::isolate(input$navpanel) == "1") {
      if (shiny::isolate(input$filter) != "no selection") {
        f$colour[f$SGID %in% select_points_data$SGID] <- shiny::isolate(colthemeCol$ColorSelected)
        f$colour[f$colour != shiny::isolate(colthemeCol$ColorSelected)] <- grDevices::adjustcolor(p.col, alpha = 0.1)
      }
    }

    val <- input$Impo_opt
    if (val == 1) {
      im <- import_reac$reactive
      if (!is.null(im)) {
        vek1 <- shiny::isolate(vi_variable())[shiny::isolate(vi_variable()$Importance) >= im[1] & shiny::isolate(vi_variable()$Importance) <= im[2], 1]
        tmp1 <- NULL

        for (i in 1:length(vek1)) {
          tmp1 <- rbind(tmp1, scresults$sge[scresults$sge[, as.character(eval(parse(text = 'vek1[i]')))] != "Not used", ])
          tmp1<- unique(tmp1)
        }
        if(!is.null(tmp1$SGID)) {
          f[f$SGID %in% tmp1$SGID, 'colour'] <- shiny::isolate(colthemeCol$ColorImportance)
        }
        ####... 20. imp_var_list ####
        output$imp_var_list <- shiny::renderTable({
          tab1 <- data.frame('Used importance variables' = vek1)
          names(tab1) <- "Used/colored importance variables"
          tab1
          },
          hover = TRUE,
          spacing = 'xs',
          na = 'none',
          digits = 0,
          caption.placement = 'top'
        )
      }
    }
    if (val == 2) {
      de <- input$decrease
      im2 <- input$impo2
      if(!is.null(im2)){
        vek2 <- shiny::isolate(vi_variable())[
          order(
            shiny::isolate(vi_variable()$Importance), decreasing = as.logical(de)
          )[1:im2], 1]
        tmp2 <- NULL
        for (i in 1:length(vek2)) {
          tmp2 <- rbind(
            tmp2,
            scresults$sge[scresults$sge[, as.character(eval(parse(text = 'vek2[i]')))] != "Not used", ]
          )
          tmp2<- unique(tmp2)
        }
        if (!is.null(tmp2$SGID)) {
          f[f$SGID %in% tmp2$SGID, 'colour'] <- shiny::isolate(colthemeCol$ColorImportance)
        }
        ####... 21. imp_var_list2 ####
        output$imp_var_list2 <- shiny::renderTable({
          tab2 <- data.frame('Used importance variables' = vek2)
          names(tab2) <- "Used/colored importance variables"
          tab2
          },
          hover = TRUE,
          spacing = 'xs',
          na = 'none',
          digits = 0,
          caption.placement = 'top'
        )
      }
    }

    factorialContext_result <- factorialContext(scresults, click_points_data$xy[shiny::isolate(pare$val),'SGID'])
    if (!is.null(factorialContext_result$Status)) {
      if (!any(is.na(factorialContext_result$Factorial[[shiny::isolate(input$y)]])) & factorialContext_result$Status == "Complete") {
        f[f$SGID %in% factorialContext_result$Factorial$SGID, 'colour'] <- shiny::isolate(colthemeCol$ColorFactCont)
      } else {
        f[f$SGID %in% factorialContext_result$Factorial$SGID, 'colour'] <- different_hues(colthemeCol$ColorFactCont, value = 89)
      }
    }
    f[f$SGID %in% parents(scresults,shiny::isolate(click_points_data$xy)[(pare$val),'SGID'])$Parents$SGID,'colour'] <- shiny::isolate(colthemeCol$ColorParents)
    f[f$SGID %in% shiny::isolate(click_points_data$xy$SGID),'colour'] <- shiny::isolate(colthemeCol$ColorClicked)

    f[f$SGID %in% shiny::isolate(click_points_data$xy)[shiny::isolate(pare$val),'SGID'],'colour'] <- shiny::isolate(colthemeCol$ColorTabClicked)
    color <<- f$colour
  }

  setcolor2 <- function() {
    if (screening_index$val > 0 ) {
      f <- scresults$sge[which(scresults$sge$nfactors >= input$keys_asmus[1] & scresults$sge$nfactors <= input$keys_asmus[2]),]
      p.col <- colthemeCol$ColorPoints
      bright <- 1
      f$colour <- as.character(
        c(
          grDevices::adjustcolor(p.col, alpha = 1 * bright),
          grDevices::adjustcolor(p.col, alpha = 0.75 * bright),
          grDevices::adjustcolor(p.col, alpha = 0.5 * bright),
          grDevices::adjustcolor(p.col, alpha = 0.25 * bright),
          grDevices::adjustcolor(p.col, alpha = 0.1 * bright),
          grDevices::adjustcolor(p.col, alpha = 0.1 * bright),
          grDevices::adjustcolor(p.col, alpha = 0.1 * bright),
          grDevices::adjustcolor(p.col, alpha = 0.1 * bright)
        )
      )[match(f$nfactors, 1:8)]

      tmp <- factorialContext(scresults, sorting_index()[screening_index$val])
      if (all(tmp$Factorial$FCID_incomplete == "Complete")) {
        f[f$SGID %in% tmp$Factorial$SGID, 'colour'] <- colthemeCol$ColorFactCont
      } else {
        f[f$SGID %in% tmp$Factorial$SGID, 'colour'] <- different_hues(colthemeCol$ColorFactCont, value = 89)
      }
      f[f$SGID %in% parents(scresults, sorting_index()[screening_index$val])$Parents$SGID, 'colour'] <- colthemeCol$ColorParents
      f[f$SGID %in% sorting_index()[screening_index$val], 'colour'] <- colthemeCol$ColorTabClicked
      color2 <<- f$colour
    }
  }

  pare <- shiny::reactiveValues(val = NULL)

  shiny::observeEvent(c(input$selectedSG_rows_selected,input$selectedSG_row_last_clicked), {
    pare$val <- input$selectedSG_rows_selected
  })

  shiny::observeEvent(c(input$plot_click, input$plot_click2, input$plot_click3), {
    pare$val <- 0
  })

  shiny::observeEvent(c(input$plot_click, input$plot_click2, input$plot_click3), {

    shiny::req(input$key, click_points_data$xy)

    if(input$navpanel == "1") {
      key <- shiny::req(input$key)
      curr_x <- shiny::req(input$x)
    } else if (input$navpanel == "2") {
      key <- shiny::req(input$key)
      curr_x <- shiny::req(input$x2)
    }

    start_radius <- input$pickradius

    clicked <- shiny::nearPoints(
      scresults$sge[which(scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]),],
      input$plot_click,
      xvar = curr_x,
      yvar = ifelse(input$navpanel == "1", input$y, input$y1),
      threshold = start_radius,
      maxpoints = NULL
    )

    clicked2 <- shiny::nearPoints(
      scresults$sge[which(scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]),],
      input$plot_click2,
      xvar = curr_x,
      yvar = input$y2,
      threshold = start_radius,
      maxpoints = NULL
    )

    clicked3 <- shiny::nearPoints(
      scresults$sge[which(scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]),],
      input$plot_click3,
      xvar = input$y1,
      yvar = input$y2,
      threshold = start_radius,
      maxpoints = NULL
    )

    clicked <- subset(
      rbind(clicked, clicked2, clicked3),
      select = c("SGID", x = curr_x, y = input$y, "nfactors", scresults$factors)
    )

    click_points_data$xy <- clicked[, unlist(lapply(clicked, function(x) !all(is.na(x))))]

    Memorize = shinyInput(
      actionButton,
      dim(click_points_data$xy)[1],
      'button_',
      label = "Memorize",
      onclick = 'Shiny.onInputChange(\"select_button\",  this.id)'
    )

    if (dim(click_points_data$xy)[1] == 0) {
      output$selectedSG <- DT::renderDataTable(DT::datatable(NULL))
    }

    if (dim(click_points_data$xy)[1] != 0) {

      col2hide <- which(sapply(click_points_data$xy, FUN = function(x){all(x == 'Not used')})) - 1

      names(col2hide) <- NULL

      tmp <- DT::datatable(
        data = cbind(Memorize, click_points_data$xy),
        extensions = 'Buttons',
        escape = FALSE,
        options = list(
          columnDefs = list(list(targets = col2hide + 1, visible = FALSE)),
          initComplete = DT::JS(
            "function(settings, json) {",
            paste0("$(this.api().table().header()).css({'background-color': '",
                   colthemeCol$col.bg,
                   "', 'color': '",
                   font_color(different_hues(colthemeCol$col.bg)),
                   "'});"
            ),"}"
          ),
          dom = 'Brtip',
          buttons = c('copy','print','pageLength', I('colvis')),
          lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
          pageLength = 6,
          rowCallback = DT::JS(
            "function(row, data) {
            \n
            // Bold cells for those >= 5 in the first column\n
            if (parseFloat(data[1]) >= 15.0)\n
            $(\"td:eq(1)\", row).css(\"font-weight\", \"bold\");\n
            }"
          )
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of Selected Subgroups',
        filter='top'
      )


      tmp <- DT::formatStyle(
        table = tmp,
        columns = 1:(ncol(click_points_data$xy) + 1),
        target = "cell",
        backgroundColor = different_hues(colthemeCol$col.bg),
        border = paste0('.5px solid ', colthemeCol$col.bg)
      )

      tmp.sglev <- levels(
        relevel(
          factor(unlist(lapply(click_points_data$xy[, scresults$factors], as.character))),
                ref = 'Not used'
        )
      )

      colXY <- which(colnames(click_points_data$xy) %in% c('SGID', names(scresults$results_total), 'nfactors')) + 1

      col.tabFont <- font_color(different_hues(colthemeCol$col.bg))
      col.tabBack <- colthemeCol$col.bg

      tmp <- DT::formatStyle(
        table = tmp,
        columns = names(click_points_data$xy),
        color = col.tabFont
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = scresults$factors,
        color = DT::styleEqual(
          tmp.sglev,
          c(col.tabBack, rep(col.tabFont, length(tmp.sglev) - 1))
        )
      )

      output$selectedSG <- DT::renderDataTable(tmp)
    }
  })

  df_parent <- shiny::reactiveValues(data = data.frame(NULL))

  shiny::observeEvent(c(input$selectedSG_rows_selected, input$settheme), ignoreNULL = FALSE, {

    df_parent <- parents(scresults,click_points_data$xy[input$selectedSG_rows_selected,'SGID'])

    if (is.null(dim(df_parent$Parents))){
      tmp <- NULL
    }else{
      if(input$navpanel == "1") {
        curr_x <- shiny::req(input$x)
      } else if (input$navpanel == "2") {
        curr_x <- shiny::req(input$x2)
      }

      df_par <- subset(
        df_parent$Parents,
        select = c("SGID", x = curr_x, y = input$y, "nfactors", scresults$factors)
      )

      col2hide <- which(sapply(df_par, FUN = function(x){all(x == 'Not used')})) - 1
      names(col2hide) <- NULL

      tmp <- DT::datatable(
        data = df_par,
        extensions = 'Buttons',
        options = list(initComplete = JS(
          "function(settings, json) {",
          paste0("$(this.api().table().header()).css({'background-color': '",
                 colthemeCol$col.bg,
                 "', 'color': '",
                 font_color(different_hues(colthemeCol$col.bg)),
                 "'});"
          ),"}"
        ),
        columnDefs = list(list(targets = col2hide, visible = FALSE)),
        dom = 'Brtip',
        buttons = c('copy','print','pageLength',I('colvis')),
        lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
        pageLength = 6
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of Parent Subgroups',
        filter = 'top'
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = 1:(ncol(df_par) + 1),
        target = "cell",
        backgroundColor = different_hues(colthemeCol$col.bg),
        border = paste0('.5px solid ', colthemeCol$ColorBGplot)
      )

      tmp.sglev <- levels(relevel(factor(unlist(lapply(df_par[, scresults$factors], as.character))), ref = 'Not used'))
      colXY <- which(colnames(df_par) %in% c('SGID', names(scresults$results_total), 'nfactors'))

      col.tabFont <- font_color(different_hues(colthemeCol$col.bg))
      col.tabBack <- colthemeCol$col.bg
      tmp <- DT::formatStyle(
        table = tmp,
        columns = colXY,
        color = col.tabFont
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = scresults$factors,
        color = DT::styleEqual(
          tmp.sglev, c(col.tabBack, rep(col.tabFont, length(tmp.sglev) - 1))
        )
      )
    }
    ####... 45. parents ####
    output$parents<- DT::renderDataTable(tmp)
  })

  output$interaction_panel <- shiny::renderUI({
    shiny::wellPanel(
      style = paste0("background:" , colthemeCol$col.bg),
      shiny::fluidRow(
        ####... 39. interaction ####
        shiny::plotOutput(outputId = 'interaction')
      ),
      shiny::fluidRow(
        shiny::column(12,
          ####... 40. y_Interaction_Button ####
          shiny::radioButtons(
            inputId = 'y_Interaction_Button',
            label = 'Synchronise y-axes with main plot',
            selected = ("Synchron"),
            choices = c("Synchron","Optimal"),
            inline = TRUE
          )
        )
      )
    )
  })

  color <- rep('#FFFFFF', 10)

  shiny::makeReactiveBinding("color")

  js$disableTab("ImportanceTab")
  if (!is.null(variable_importance)) {
    js$enableTab("ImportanceTab")
  }

  js$disableTab("subscreenasmus")
  if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(scresults$sge))) {
    js$enableTab("subscreenasmus")
  }
  js$disableTab("ComplementSubgroup")
  if (any(startsWith(colnames(scresults$sge), "Complement_"))) {
    js$enableTab("ComplementSubgroup")
  }

  js$disableTab("ParentSubgroup")
  if (scresults$max_comb > 1) {
    js$enableTab("ParentSubgroup")
  }

  shinyjs::useShinyjs(debug = TRUE)
  shinyjs::disable("ColorImportance")
  if (!is.null(variable_importance)) {
    shinyjs::enable("ColorImportance")
  }

  shinyjs::disable("ColorParents")
  if (scresults$max_comb > 1) {
    shinyjs::enable("ColorParents")
  }

  ####... 1. cont_nav #####
  output$cont_nav <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        paste0(
          ".navbar { background-color:",
          colthemeCol$col.bg,
          " ;font-family: Arial;font-size: 15px; color: ",
          font_color(colthemeCol$col.bg),
          "; }',
          '.navbar-default .navbar-brand {
          color: ",
          font_color(colthemeCol$col.bg),
          ";
          font-size: 40px;
          font-family: Arial;}"
        )
      )
    )
  })

  ####... 2. cont ####
  output$cont <- shiny::renderUI({
    list(
      shiny::tags$head(
        shiny::tags$style(
          paste(
            "body {background-color: ",
            colthemeCol$col.bg,
            "; color: ",
            font_color(colthemeCol$col.bg),
            "}",
            sep = ""
          )
        )
      )
    )
  })

  ####... 3. cont2 ####
  output$cont2 <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        paste0(
          ".fa-bug {color:#D30F4B}",
          ".fa-times-circle{color: #fffb00}",
          ".fa-th-list {color:grey}",
          ".fa-circle {color:", colthemeCol$ColorClicked, "}",
          "fa-info-circle {color:#DE0043FF}",
          ".fa-filter {color:", colthemeCol$ColorSelected, "}",
          ".fa-delicious {color:#00aaff}",
          ".fa-braille {color: grey}",
          ".fa-list {color:", colthemeCol$ColorFact, "}",
          ".fa-sitemap {color: ", colthemeCol$ColorParents, "}",
          ".fa-clipboard {color: #e2b007}",
          ".fa-edit {color:#00aaff}",
          ".fa-object-group {color: grey}",
          shiny::HTML(
            ".selectize-input.input-active, .selectize-input.input-active:hover, .selectize-control.multi .selectize-input.focus {border-color: red !important;}\n                                     .selectize-dropdown .active {background: #FF3162FF !important;}"
          ), sep = ","
        )
      )
    )
  })

  ####... 5. cont_well ####
  output$cont_well  <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        type = 'text/css',
        paste0(".myclass1 {background-color: ", colthemeCol$col.bg, ";}")
      )
    )
  })

  ####... 9. VarChosen ####
  output$VarChosen <- shiny::renderUI({
    if (input$filter != 'no selection') {
      choices <- c(as.character(unique(scresults$sge[, input$filter])))
      choices <- choices[-which(choices == "Not used")]
      selected <- choices[1]
      shiny::selectInput(
        inputId = "VarChosen",
        label = "Choose a value",
        choices = choices,
        selected = selected
      )
    }
  })

  output$YRange <- shiny::renderUI({
    shiny::req(input$y)
     if (input$plot_type == "lin") {
      shiny::sliderInput(
        inputId = "YRange",
        label = "Y Range",
        min = roundDownNice(min(scresults$sge[, input$y], na.rm = TRUE)),
        max = roundUpNice(max(scresults$sge[, input$y], na.rm = TRUE)),
        value = c(min(scresults$sge[, names(scresults$results_total)[1]], na.rm = TRUE), max(scresults$sge[, input$y], na.rm = TRUE)),
        step = roundUpNice((max(scresults$sge[, input$y], na.rm = TRUE) - min(scresults$sge[, input$y], na.rm = TRUE))/100)
      )
     } else {
      rg.z <- log(
        range(roundDownNice(
                min(scresults$sge[, input$y], na.rm = TRUE)
              ),
              roundUpNice(
                max(scresults$sge[, input$y], na.rm = TRUE)
              )
        )
      )
      choices <- unique(unlist(lapply(exp(seq(rg.z[1], rg.z[2], length.out = 20)), function(x){signif(x, 2)})))
      shinyWidgets::sliderTextInput(
        inputId = "YRange",
        label = "Log Y Range:",
        hide_min_max = TRUE,
        choices = choices,
        selected = c(choices[1],choices[length(choices)]),
        grid = TRUE
      )
     }
  })

  ####... 14. cont_well8 ####
  output$cont_well8 <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        type = 'text/css',
        paste0(".myclass8 {background-color: ", colthemeCol$col.bg, ";}"
        )
      )
    )
  })

  ####... 16. select_importance_variable ####
  vi_names <- shiny::reactive({
    if (is.data.frame(variable_importance)) {
      "NULL"
    } else if (is.list(variable_importance)) {
      names(variable_importance)
    } else  {
      "NULL"
    }
  })

  ####... select_importance_variable ####
  output$select_importance_variable <- shiny::renderUI({
    if (is.data.frame(variable_importance)) {
     choices <- "NULL"
    } else if (is.list(variable_importance)) {
     choices <- names(variable_importance)
    } else  {
     choices <-"NULL"
    }
    shiny::selectInput(
      inputId = "select_importance_variable",
      "Select Variable",
      choices = choices,
      selected = choices[1]
    )
  })

  ####... 17. impo ####
  output$impo <- shiny::renderUI({
    shiny::req(vi_variable())
    shiny::sliderInput(
      inputId = "impo",
      label = "Choose importance Range",
      min = min(vi_variable()$Importance),
      max = max(vi_variable()$Importance),
      value = c(min(vi_variable()$Importance), min(vi_variable()$Importance))
    )
  })

  import_reac <- shiny::reactiveValues(
    reactive = c(NULL, NULL)
  )
  shiny::observeEvent(input$impo, {
    import_reac$reactive <- input$impo
  })

  ####... 18. impo2 ####
  output$impo2 <- shiny::renderUI({
    shiny::sliderInput(
      inputId = "impo2",
      label = "Choose number of Variables which are most important",
      min = 1,
      max = length(vi_variable()$Importance),
      value = 1,
      step = 1
    )
  })

  ####... 22. cont_well6 ####
  output$cont_well6 <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        type = 'text/css',
        paste0(".myclass6 {background-color: ",
               colthemeCol$col.bg,";}"
        )
      )
    )
  })

  ####... 26. cont_well7 ####
  output$cont_well7 <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        type = 'text/css',
        paste0(".myclass7 {background-color: ", colthemeCol$col.bg, ";}")
      )
    )
  })

  ####... 31. Panel_Colour ####
  output$cont_well9 <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        type = 'text/css',
        paste0(".myclass9 {background-color: ", colthemeCol$col.bg, ";}")
      )
    )
  })

  output$Panel_Colour <- shiny::renderUI({
    shiny::tagList(
      ####... 31. (I) ColorClicked####
      shiny::fluidRow(
        shiny::column(6,
          colourpicker::colourInput(
            inputId = "ColorClicked",
            label = "Choose a Colour for the selected Subgroup(s)",
            colthemeCol$ColorClicked,
            allowTransparent = TRUE

          )
        ),
        ####... 31. (II) ColorSelected ####
        shiny::column(6,
          colourpicker::colourInput(
            inputId = "ColorSelected",
            label = "Choose a Colour for the filtered Subgroup(s)",
            value = colthemeCol$ColorSelected,
            allowTransparent = TRUE
          )
        )
      ),
      ####... 31. (III) ColorParents####
      shiny::fluidRow(
        shiny::column(6,
          colourpicker::colourInput(
            inputId = "ColorParents",
            label = "Choose a Colour for the Parent Subgroup(s)",
            value = colthemeCol$ColorParents,
            allowTransparent = TRUE
          )
        ),
        ####... 31. (IV) ColorTabClicked ####
        shiny::column(6,
          colourpicker::colourInput(
            inputId = "ColorTabClicked",
            label = "Choose a Colour for the clicked Subgroup(s)",
            value = colthemeCol$ColorTabClicked,
            allowTransparent = TRUE
          )
        )
      ),
      ####... 31. (V) ColorImpportance ####
      shiny::fluidRow(
        shiny::column(6,
          colourpicker::colourInput(
            inputId = "ColorImportance",
            label = "Choose a Colour for the Subgroup(s) with important Variable(s) ",
            value = colthemeCol$ColorImportance,
            allowTransparent = TRUE
          )
        ),
        ####... 31. (VI) ColorReference ####
        shiny::column(6,
          colourpicker::colourInput(
            inputId = "ColorReference",
            label = "Choose a Colour for the Reference Line",
            value = colthemeCol$ColorReference,
            allowTransparent = TRUE
          )
        )
      ),
      ####... 31. (VII) ColorFactCont ####
      shiny::fluidRow(
        shiny::column(6,
          colourpicker::colourInput(
            inputId = "ColorFactCont",
            label = "Choose a Colour for the Factorial Context",
            value = colthemeCol$ColorFactCont,
            allowTransparent = TRUE
          )
        ),
        ####... 31. (VIII) ColorBGplot ####
        shiny::column(6,
          colourpicker::colourInput(
            inputId = "ColorBGplot",
            label = "Choose Background Colour (Plot)",
            colthemeCol$ColorBGplot
          )
        )
      ),
      shiny::fluidRow(
      ####... 31. (X) ColorPoints ####
        shiny::column(6,
          colourpicker::colourInput("ColorPoints",
                                "Choose a Colour for the Points",
                                colthemeCol$ColorPoints

          )
        ),
        shiny::column(6,
          shiny::selectInput(
            inputId = 'select_col',
            label = "Select standard color theme:",
            choices = list('app version', 'print version'),
            selected = 'app version'
          )
        )
      ),
      shiny::fluidRow(
      ####... 31. (XII) select_col ####
        shiny::column(6, offset = 6,
          ####... 31. (XIII) settheme ####
          shiny::actionButton(
            inputId = 'settheme',
            label = 'Apply / Refresh',
            width = NULL
          )
        )
      ),
      use_bs_popover(),
      use_bs_tooltip()
    )
  })

  colthemeCol <- shiny::reactiveValues(
    col.bg = '#383838',
    font.col = '#ffffff',
    panel.col = '#6b6b6b',
    ColorClicked = "#D30F4B",
    ColorSelected = "#89D329",
    ColorParents = "#ff6c00",
    ColorTabClicked = "#e2b007",
    ColorImportance = "#FA1BDC",
    ColorReference = "#0091DF60",
    ColorFactCont = "#0350E0",
    ColorBGplot = "#383838",
    ColorPoints = "#FFFFFF"
  )

  shiny::observeEvent(input$settheme, {
    if (input$select_col == 'app version') {
      colthemeCol$col.bg <- '#383838'
      colthemeCol$ColorBGplot <- "#383838"
      colthemeCol$ColorPoints <- "#FFFFFF"
    } else if (input$select_col == 'print version') {
      colthemeCol$col.bg <- '#ffffff'
      colthemeCol$ColorReference <- "#0091DF"
      colthemeCol$ColorBGplot <- "#ffffff"
      colthemeCol$ColorPoints <- "#000000"
    }
  })

  shiny::observeEvent(
    c(input$FontColour,
      input$ColorClicked,
      input$ColorSelected,
      input$ColorParents,
      input$ColorTabClicked,
      input$ColorImportance,
      input$ColorReference,
      input$ColorBGplot,
      input$ColorPoints,
      input$ColorFactCont
    ), {
      colthemeCol$col.bg <- input$ColorBGplot
      colthemeCol$ColorClicked <- input$ColorClicked
      colthemeCol$ColorSelected <- input$ColorSelected
      colthemeCol$ColorFactCont <- input$ColorFactCont
      colthemeCol$ColorParents <- input$ColorParents
      colthemeCol$ColorTabClicked <- input$ColorTabClicked
      colthemeCol$ColorImportance <- input$ColorImportance
      colthemeCol$ColorReference <- input$ColorReference
      colthemeCol$ColorBGplot <- input$ColorBGplot
      colthemeCol$ColorPoints <- input$ColorPoints
    }
  )

  ColorBGplotlight <- shiny::reactiveValues(
    col = grDevices::adjustcolor(
      "#383838",
      red.f = 1.3,
      green.f = 1.3,
      blue.f = 1.3
    )
  )

  shiny::observeEvent(input$ColorBGplot, {
    ColorBGplotlight$col <- grDevices::adjustcolor(
      colthemeCol$ColorBGplot,
      red.f = 1.3,
      green.f = 1.3,
      blue.f = 1.3
    )
  })

  ####... 32. graph ####
  output$graph <- shiny::renderPlot({
    shiny::req(plot_points_data(), input$YRange, input$plot_type, input$pointsize)

    par(oma = c(0, 0, 0, 0), mar = c(0, 3, 0, 0), bg = colthemeCol$ColorBGplot)

    plot_point <- plot_points_data()
    input$VarChosen

    colthemeCol$ColorParents
    colthemeCol$ColorClicked
    colthemeCol$Importance
    colthemeCol$ColorFactCont
    setcolor()

    all_points <- cbind(plot_point, color, stringsAsFactors = FALSE)
    gold_points_ID <- all_points[all_points$color %in% colthemeCol$ColorTabClicked,]$ID

    white_points <- all_points[all_points$color %in% c("#FFFFFFFF", "#FFFFFFBF", "#FFFFFF80", "#FFFFFF40", "#FFFFFF1A"),]

    colored_points <- all_points[!all_points$color %in% c("#FFFFFFFF", "#FFFFFFBF", "#FFFFFF80", "#FFFFFF40", "#FFFFFF1A"),]

    plot(
      x = all_points$x,
      y = all_points$y,
      xlab = "",
      ylab = "",
      ylim = shiny::isolate(input$YRange),
      log = ifelse(shiny::isolate(input$plot_type) == "log", "y", ""),
      cex.axis = 1.4, cex.lab = 1.4,
      type = "n",
      axes = FALSE
    )

    rect(
      xleft = grconvertX(0,'ndc','user') - ifelse(input$plot_type == "lin", 1000, 0),
      xright = grconvertX(1,'ndc','user') + ifelse(input$plot_type == "lin", 1000, 0),
      ybottom = grconvertY(0,'ndc','user') - ifelse(input$plot_type == "lin", 1000, 0),
      ytop = grconvertY(1,'ndc','user') + ifelse(input$plot_type == "lin", 1000, 0),
      border = NA,
      col = colthemeCol$ColorBGplot,
      xpd = TRUE
    )

    if (ifelse(shiny::isolate(input$plot_type) == "log", "y", "") == "y") {
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

    nr <- 7
    stepx <- roundUpNice((maxix - minix)/(nr +  1))

    if (minix < stepx)
      minix <- 0
    stripesx <- 0:(nr + 1)
    stripesx <- lapply(stripesx, function(x) x * stepx)
    stripesx <- lapply(stripesx, function(x) x + minix)
    stripesxp <- lapply(stripesx, function(x) paste(floor(x/scresults$results_total[,c(input$x)] * 100), "%"))
    for (i in seq(1, nr, 2)) rect(stripesx[i],miniy, stripesx[i + 1], maxiy, col = different_hues(colthemeCol$col.bg), border = NA)


    if(input$xlabel == TRUE) {
      text(stripesx, lowy, stripesx, cex = 1.5,col = font_color(colthemeCol$col.bg))
      text(stripesx, lowyp, stripesxp, cex = 1.5,col = font_color(colthemeCol$col.bg))
    }

    box(col = font_color(colthemeCol$col.bg))

    axis(
      2,
      col = font_color(colthemeCol$col.bg),
      col.ticks = font_color(colthemeCol$col.bg),
      col.axis = font_color(colthemeCol$col.bg),
      cex.axis = 1
    )

    title(main = SG_tit(), line = -2, col = "#8b8b8b", col.main = font_color(colthemeCol$col.bg))

    pch_ <- ifelse(input$pch_value == "19", 19, input$pch_value)

    if(shiny::isolate(input$circlestyle) == "standard"){
      points(white_points$x, white_points$y, pch = pch_, cex = shiny::isolate(input$pointsize), col = white_points$color)
      points(colored_points$x, colored_points$y, pch = pch_, cex = shiny::isolate(input$pointsize), col = colored_points$color)
    }
    if(input$circlestyle == "groupsize") {
      points(white_points$x, white_points$y, pch = pch_, cex = shiny::isolate(input$pointsize) * sqrt(scresults$sge[scresults$sge$SGID %in% white_points$ID, 'N.of.subjects'][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]]/pi), col = white_points$color)
      points(colored_points$x, colored_points$y, pch = pch_, cex = shiny::isolate(input$pointsize) * sqrt(scresults$sge[scresults$sge$SGID %in% colored_points$ID , 'N.of.subjects'][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]]/pi), col = colored_points$color)
    }

    abline(h = ref_line(), lwd = 3, col = colthemeCol$ColorReference)

    points(shiny::isolate(plot_points_data_complement()),
           pch = ifelse(input$pch_value == "19", 13, '.'),
           cex = ifelse(input$circlestyle == "groupsize",
                        shiny::isolate(input$pointsize) * sqrt(plot_points_data_complement()$x/pi),
                        shiny::isolate(input$pointsize)
                        ),
           col = "#fffb00")

    text(
      x = grconvertX(0.97, from = 'nfc', to = 'user'),
      y = ref_line() + diff(input$YRange)/50,
      paste0(shiny::isolate(ref_line())),
      col = colthemeCol$ColorReference
    )

    if (input$grid == TRUE) {
      abline(h = axTicks(2), lty = 2, col = font_color(colthemeCol$col.bg), lwd = 0.3)
      abline(v = axTicks(1), lty = 2, col = font_color(colthemeCol$col.bg), lwd = 0.3)
    }
  })

  ####... 34. cont_well5 ####
  output$header1 <- shiny::renderUI({
    shiny::req(screening_index$val)
    shiny::tags$h5(
      id = "header4",
      paste0("Subgroup: ", screening_index_new$val)
    )
  })

  output$header2 <- shiny::renderUI({
    shiny::req(screening_index$val)
    tmp1 <- colnames(scresults$sge[which(scresults$sge$SGID == screening_index_new$val), scresults$factors])[which(scresults$sge[which(scresults$sge$SGID == screening_index_new$val), scresults$factors] != "Not used")]
    tmp2 <- scresults$sge %>%
      dplyr::filter(SGID %in% screening_index_new$val) %>%
      dplyr::select(colnames(scresults$sge[which(scresults$sge$SGID == screening_index_new$val), scresults$factors])[which(scresults$sge[which(scresults$sge$SGID == screening_index_new$val), scresults$factors] != "Not used")])
    tmp2 <- data.frame(lapply(tmp2, as.character), stringsAsFactors = FALSE)
    shiny::tags$h5(
      id = "header4",
      paste0("Factors(", length(tmp1),"): ", paste(paste0(tmp1, " = ", tmp2), collapse = ", "))
    )
  })

  ####... 35. screening_ui ####
  output$screening_ui <- shiny::renderUI({
    purrr::map(screening_index_new$val, ~ screeningModule_UI(id = .x))
  })

  ####... 39.+46. interaction+factorial ####

  df_factorial <- shiny::reactiveValues(data = data.frame(NULL))
  shiny::observeEvent(
    c(input$showPanel1, input$screening_forward, input$screening_backward), ignoreNULL = FALSE, {
      if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(scresults$sge))) {
    shiny::req(screening_index$val)
    if (screening_index$val != 0) {
      df_factorial <- factorialContext(scresults, screening_index$val)
      if (is.null(dim(df_factorial$Factorial))) {
        tmp <- NULL
      } else {

        df_fac <- subset(
          df_factorial$Factorial,
          select = c("SGID", x = input$x, y = input$y, "nfactors", scresults$factors)
        )

        tmp <- DT::datatable(
          data = df_fac,
          extensions = 'Buttons',
          options = list(
            initComplete = JS(
              "function(settings, json) {",
              paste0("$(this.api().table().header()).css({'background-color': '",
                     colthemeCol$col.bg,
                     "', 'color': '",
                     font_color(different_hues(colthemeCol$col.bg)),
                     "'});"
              ),
              "}"
            ),
            dom = 'Brtip',
            buttons = c('copy','print','pageLength',I('colvis')),
            lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
            pageLength = 6
          ),
          class = 'cell-border stripe',
          rownames = FALSE,
          caption = '',
          filter = 'top'
        )

        tmp <- DT::formatStyle(
          table = tmp,
          columns = 1:(ncol(df_fac)
                       ),
          target = "cell",
          backgroundColor = different_hues(colthemeCol$col.bg),
          border = paste0('.5px solid ', colthemeCol$ColorBGplot)
        )
      }
    }
    }
    ####... 46. factorial ####

    y_axe_Int <- shiny::reactive({
      shiny::req(input$y_Interaction_Button)

      if (input$y_Interaction_Button == "Synchron") {
        tmp <- c(input$YRange[1], input$YRange[2])
      }
      if (input$y_Interaction_Button == "Optimal") {
        tmp <- c("NA","NA")
      }
      tmp
    })

    ####... 38b. legend ####
    output$legend <- shiny::renderUI({
      shiny::req(plot_points_data())
      plot_point <- plot_points_data()

      if (length(color) == dim(plot_point)[1]) {

      all_points <- cbind(plot_point, color, stringsAsFactors = FALSE)
      gold_points_ID <- all_points[all_points$color %in% colthemeCol$ColorTabClicked,]$ID
      colored_points <- all_points[!startsWith(all_points$color, colthemeCol$ColorPoints),]
      active_colors <- unique(colored_points$color)
      shiny::tagList(
        if (colthemeCol$ColorClicked %in% active_colors) {
               shiny::p(
                 shiny::span(
                  shiny::tagList(
                    shiny::tags$i(
                      class = "fa fa-circle",
                      style = paste0("color: ",
                                     colthemeCol$ColorClicked
                      )
                    ),"Clicked Subgroup(s)"
                  )
                 )
               )
        },
        if (length(gold_points_ID) > 0) {
               shiny::p(
                 shiny::span(
                   shiny::tagList(
                    shiny::tags$i(
                      class = "fa fa-circle",
                      style = paste0("color: ", colthemeCol$ColorTabClicked)
                    ),"Selected Subgroup(s)"
                  )
                 )
               )
        },
        if (colthemeCol$ColorSelected %in% active_colors) {
               shiny::p(
                 shiny::span(
                  shiny::tagList(
                    shiny::tags$i(
                      class = "fa fa-circle",
                      style = paste0("color: ", colthemeCol$ColorSelected)
                    ),"Filtered Subgroup(s)"
                  )
                 )
           )
        },
        if (colthemeCol$ColorImportance%in% active_colors) {
               shiny::p(
                 shiny::span(
                  shiny::tagList(
                    shiny::tags$i(
                      class = "fa fa-circle",
                      style = paste0("color: ", colthemeCol$ColorImportance)
                    ),"Importance"
                  )
                 )
           )
        },
        if (colthemeCol$ColorParents %in% active_colors) {
               shiny::h5(
                 shiny::span(
                  shiny::tagList(
                    shiny::tags$i(
                      class = "fa fa-circle",
                      style = paste0("color: ", colthemeCol$ColorParents)
                    ), "Parent Subgroup(s)"
                  )
                 )
           )
        },
        if (colthemeCol$ColorFactCont %in% active_colors) {
             tag = shiny::p(
               shiny::span(
                shiny::tagList(
                  shiny::tags$i(
                    class = "fa fa-circle",
                    style = paste0("color: ", colthemeCol$ColorFactCont)
                  ),"Factorial Context"
                )
               )
         )
        },
        if (!is.null(plot_points_data_complement())) {
          if (dim(plot_points_data_complement())[1] > 0) {
               tag = shiny::p(
                 shiny::span(
                  shiny::tagList(
                    shiny::tags$i(
                      class = "fa fa-times-circle",
                      style = paste0("color: #fffb00")
                    ),"Subgroup Complement"
                  )
                 )
             )
          }
        },
        if (different_hues(colthemeCol$ColorFactCont, value = 89) %in% active_colors) {
          tag = shiny::p(
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa fa-circle",
                  style = paste0("color: ", different_hues(colthemeCol$ColorFactCont, value = 89))
                ),"Incomplete factorial Context"
              )
           )
          )
        })
      }
    })


    output$legend2 <- shiny::renderUI({
      shiny::req(plot_points_data())
      plot_point <- plot_points_data()
      all_points <- cbind(plot_point, color, stringsAsFactors = FALSE)
      gold_points_ID <- all_points[all_points$color %in% colthemeCol$ColorTabClicked,]$ID
      colored_points <- all_points[!startsWith(all_points$color, colthemeCol$ColorPoints),]
      active_colors <- unique(colored_points$color)
      shiny::tagList(
        if (colthemeCol$ColorClicked %in% active_colors) {
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa fa-circle", style = paste0("color: ", colthemeCol$ColorClicked)
                ),"Clicked Subgroup(s)"
           )
          )
        },
        if (length(gold_points_ID) > 0) {
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa fa-circle", style = paste0("color: ", colthemeCol$ColorTabClicked)
                ),"Selected Subgroup(s)"
           )
            )
        },
        if (colthemeCol$ColorSelected %in% active_colors) {
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa fa-circle",
                  style = paste0("color: ", colthemeCol$ColorSelected)
                ),"Filtered Subgroup(s)"
           )
          )
        },
        if (colthemeCol$ColorImportance%in% active_colors) {
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa fa-circle",
                  style = paste0("color: ", colthemeCol$ColorImportance)
                ),"Importance"
              )
          )
        },
        if (colthemeCol$ColorParents %in% active_colors) {
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa fa-circle",
                  style = paste0("color: ", colthemeCol$ColorParents)
                ),"Parent Subgroup(s)"
           )
          )
        },
        if (colthemeCol$ColorFactCont %in% active_colors) {
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa fa-circle",
                  style = paste0("color: ", colthemeCol$ColorFactCont)
                ),"Factorial Context"
           )
            )
        },
        if (different_hues(colthemeCol$ColorFactCont, value = 89) %in% active_colors) {
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa fa-circle",
                  style = paste0("color: ", different_hues(colthemeCol$ColorFactCont, value = 89))
                ),"Incomplete factorial Context"
           )
          )
        }
      )
    })

    output$legend3 <- shiny::renderUI({
      shiny::req(plot_points_data())
      plot_point <- plot_points_data()
      all_points <- cbind(plot_point, color, stringsAsFactors = FALSE)
      gold_points_ID <- all_points[all_points$color %in% colthemeCol$ColorTabClicked,]$ID
      colored_points <- all_points[!startsWith(all_points$color, colthemeCol$ColorPoints),]
      active_colors <- unique(colored_points$color)
      shiny::tagList(
        if (colthemeCol$ColorClicked %in% active_colors) {
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa fa-circle",
                  style = paste0("color: ", colthemeCol$ColorClicked)
                ),"Clicked Subgroup(s)"
              )
           )
        },
        if (length(gold_points_ID) > 0) {
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa fa-circle",
                  style = paste0("color: ", colthemeCol$ColorTabClicked)
                ),"Selected Subgroup(s)"
              )
            )
        },
        if (colthemeCol$ColorSelected %in% active_colors) {
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa fa-circle",
                  style = paste0("color: ", colthemeCol$ColorSelected)
                ),"Filtered Subgroup(s)"
              )
          )
        },
        if (colthemeCol$ColorImportance %in% active_colors) {
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa fa-circle",
                  style = paste0("color: ", colthemeCol$ColorImportance)
                ), "Importance"
              )
          )
        },
        if (colthemeCol$ColorParents %in% active_colors) {
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa fa-circle",
                  style = paste0("color: ", colthemeCol$ColorParents)
                ),"Parent Subgroup(s)"
              )
          )
        },
        if (colthemeCol$ColorFactCont %in% active_colors) {
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa fa-circle",
                  style = paste0("color: ", colthemeCol$ColorFactCont)
                ),"Factorial Context"
              )
           )
        },
        if (different_hues(colthemeCol$ColorFactCont, value = 89) %in% active_colors) {
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa fa-circle",
                  style = paste0("color: ", different_hues(colthemeCol$ColorFactCont, value = 89))
                ),"Incomplete factorial Context"
              )
            )
        }
      )
    })

    ####... 39. interaction ####
    output$interaction <- shiny::renderPlot({

      shiny::req(y_axe_Int())
      y_axe <- y_axe_Int()
      df_factorial <- factorialContext(
        scresults,
        click_points_data$xy[input$selectedSG_rows_selected,'SGID']
      )

      if (is.null(df_factorial$Variables[1]) || is.na(df_factorial$Variables[1])) {
        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          axes = FALSE,
          xlab = "",
          ylab = ""
        )
        rect(
          xleft = grconvertX(0,'ndc','user') - 1000,
          xright = grconvertX(1,'ndc','user') + 1000,
          ybottom = grconvertY(0,'ndc','user') - 1000,
          ytop = grconvertY(1,'ndc','user') + 1000,
          border = NA,
          col = colthemeCol$ColorBGplot,
          xpd = TRUE
        )
        text(
          0.5,
          0.5,
          "Please select a Subgroup!",
          col = font_color(colthemeCol$col.bg),
          cex = 1.4
        )
        text(
          0.5,
          0.4,
          "(Click on a point in the graphic",
          col = font_color(colthemeCol$col.bg),
          cex = 0.9
        )
        text(
          0.5,
          0.3,
          "and then select a subgroup in the",
          col = font_color(colthemeCol$col.bg),
          cex = 0.9
        )

        text(
          0.5,
          0.2,
          "'Selected Subgroup'-table by clicking on)",
          col = font_color(colthemeCol$col.bg),
          cex = 0.9
        )

      } else if (!is.null(df_factorial$Variables[1]) &
                 !is.na(df_factorial$Variables[1]) &
                 any(is.na(df_factorial$Factorial[input$y])) &
                 df_factorial$`Number Factors` <= 3) {
        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          axes = FALSE,
          xlab = "",
          ylab = ""
        )
        rect(
          xleft = grconvertX(0,'ndc','user') - 1000,
          xright = grconvertX(1, 'ndc', 'user') + 1000,
          ybottom = grconvertY(0,'ndc','user') - 1000,
          ytop = grconvertY(1, 'ndc', 'user') + 1000,
          border = NA,
          col = colthemeCol$ColorBGplot,
          xpd = TRUE
        )
        text(
          0.5,
          0.5,
          "Incomplete factorial context!",
          col = font_color(colthemeCol$col.bg),
          cex = 1.4
        )
        text(
          0.5,
          0.4,
          "(This graphic is not available",
          col = font_color(colthemeCol$col.bg),
          cex = 0.9
        )
        text(
          0.5,
          0.3,
          "for pseudo factorial contexts)",
          col = font_color(colthemeCol$col.bg),
          cex = 0.9
        )

      } else if (df_factorial$`Number Factors` > 3) {
        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          axes = FALSE,
          xlab = "",
          ylab = ""
        )

        rect(
          xleft = grconvertX(0,'ndc','user') - 1000,
          xright = grconvertX(1, 'ndc', 'user') + 1000,
          ybottom = grconvertY(0,'ndc','user') - 1000,
          ytop = grconvertY(1, 'ndc', 'user') + 1000,
          border = NA,
          col = colthemeCol$ColorBGplot,
          xpd = TRUE
        )
        text(
          0.5,
          0.5,
          "Too many factors!",
          col = font_color(colthemeCol$col.bg),
          cex = 1.4
        )
        text(
          0.5,
          0.4,
          "(This graphic is not available",
          col = font_color(colthemeCol$col.bg),
          cex = 0.9
        )
        text(
          0.5,
          0.3,
          "for 4 or more subgroup levels)",
          col = font_color(colthemeCol$col.bg),
          cex = 0.9
        )

      } else if (!any(is.na(df_factorial$Factorial[input$y])) &
                 df_factorial$`Number Factors` == 1 &
                 df_factorial$Status == "Complete") {

        interaction_plot2(
          df_data = df_factorial$Factorial,
          fac1 = df_factorial$Variables[1],
          response = input$y,
          bg.col = colthemeCol$ColorBGplot,
          bg.col2 = different_hues(colthemeCol$col.bg),
          font.col = font_color(colthemeCol$col.bg),
          y.min = y_axe[1],
          y.max = y_axe[2],
          box.col = font_color(colthemeCol$col.bg)
        )

      } else if (!any(is.na(df_factorial$Factorial[input$y])) &
                 df_factorial$`Number Factors` == 2 &
                 df_factorial$Status == "Complete") {

        interaction_plot2(
          df_data = df_factorial$Factorial,
          fac1 = df_factorial$Variables[1],
          fac2 = df_factorial$Variables[2],
          response = input$y,
          bg.col = colthemeCol$ColorBGplot,
          bg.col2 = different_hues(colthemeCol$col.bg),
          font.col = font_color(colthemeCol$col.bg),
          y.min = y_axe[1], y.max = y_axe[2],
          box.col = font_color(colthemeCol$col.bg)
        )
      } else if (!any(is.na(df_factorial$Factorial[input$y])) &
                 df_factorial$`Number Factors` == 3 &
                 df_factorial$Status == "Complete") {
        interaction_plot2(
          df_data = df_factorial$Factorial,
          fac1 = df_factorial$Variables[1],
          fac2 = df_factorial$Variables[2],
          fac3 = df_factorial$Variables[3],
          response = input$y,
          bg.col = colthemeCol$ColorBGplot,
          bg.col2 = different_hues(colthemeCol$col.bg),
          font.col = font_color(colthemeCol$col.bg),
          y.min = y_axe[1],
          y.max = y_axe[2]
        )
      } else if (!any(is.na(df_factorial$Factorial[input$y])) & df_factorial$Status == "Incomplete") {
        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          axes = FALSE,
          xlab = "",
          ylab = ""
        )

        rect(
          xleft = grconvertX(0,'ndc','user') - 100,
          xright = grconvertX(1, 'ndc', 'user') + 100,
          ybottom = grconvertY(0,'ndc','user') - 100,
          ytop = grconvertY(1, 'ndc', 'user') + 100,
          border = NA,
          col = colthemeCol$ColorBGplot,
          xpd = TRUE
        )
        text(
          0.5,
          0.5,
          "Incomplete factorial context!",
          col = font_color(colthemeCol$col.bg),
          cex = 1.4
        )
        text(
          0.5,
          0.4,
          "(This graphic is not available",
          col = font_color(colthemeCol$col.bg),
          cex = 0.9
        )
        text(
          0.5,
          0.3,
          "for pseudo factorial contexts)",
          col = font_color(colthemeCol$col.bg),
          cex = 0.9
        )
      }
    })
  })

  output$cont_well10  <- shiny::renderUI({
   shiny::tags$head(
     shiny::tags$style(
        type = 'text/css',
        paste0(
          ".myclass10 {background-color: ",
          colthemeCol$col.bg,";}"
        )
      )
    )
  })

  ####... XX. interaction2 ####
  output$interaction2 <- shiny::renderPlot({
    shiny::req(screening_index_new$val)
    if (is.null(input$y_Interaction_Button2)) {
      y_axe <- c(input$YRange[1],input$YRange[2])
    } else {
      if (input$y_Interaction_Button2 == "Synchron") {
        if (is.null(input$yrange_asmus)) {
          y_axe <- input$YRange
        } else {
          y_axe <- input$yrange_asmus
        }
      }
      if (input$y_Interaction_Button2 == "Optimal") {
        y_axe <- c("NA","NA")
      }
    }

    if (is.null(input$plot_type_asmus)) {
      pl_typ <- "lin"
    } else {
      pl_typ <- input$plot_type_asmus
    }
    tmp1 <- colnames(
      scresults$sge[which(scresults$sge$SGID == screening_index_new$val), scresults$factors]
    )[which(scresults$sge[which(scresults$sge$SGID == screening_index_new$val), scresults$factors] != "Not used")]

    tmp2 <- scresults$sge %>%
      dplyr::filter(SGID %in% screening_index_new$val) %>%
      dplyr::select(colnames(scresults$sge[which(scresults$sge$SGID == screening_index_new$val), scresults$factors])[which(scresults$sge[which(scresults$sge$SGID == screening_index_new$val), scresults$factors] != "Not used")])
    tmp2 <- data.frame(lapply(tmp2, as.character), stringsAsFactors = FALSE)
    df_factorial <- factorialContext(scresults, screening_index_new$val)

    if(is.null(df_factorial$Variables[1]) || is.na(df_factorial$Variables[1])) {
      plot(
        NULL,
        xlim = c(0, 1),
        ylim = c(0, 1),
        axes = FALSE,
        xlab = "",
        ylab = ""
      )
      rect(
        xleft = grconvertX(0, 'ndc', 'user') - 1000,
        xright = grconvertX(1, 'ndc', 'user') + 1000,
        ybottom = grconvertY(0, 'ndc', 'user') - 1000,
        ytop = grconvertY(1,'ndc','user') + 1000,
        border = NA,
        col = colthemeCol$ColorBGplot,
        xpd = TRUE
      )

      text(
        0.5,
        0.5,
        "Please select a Subgroup!",
        col = font_color(colthemeCol$col.bg),
        cex = 1.4
      )
      text(
        0.5,
        0.4,
        "(Click on a point in the graphic",
        col = font_color(colthemeCol$col.bg),
        cex = 0.9
      )
      text(
        0.5,
        0.3,
        "and then select a subgroup in the",
        col = font_color(colthemeCol$col.bg),
        cex = 0.9
      )

      text(
        0.5,
        0.2,
        "'Selected Subgroup'-table by clicking on)",
        col = font_color(colthemeCol$col.bg),
        cex = 0.9
      )


    } else if (!is.null(df_factorial$Variables[1]) &
              !is.na(df_factorial$Variables[1]) &
              any(is.na(df_factorial$Factorial[input$y])) &
              df_factorial$`Number Factors` <= 3) {
      plot(
        NULL,
        xlim = c(0, 1),
        ylim = c(0, 1),
        axes = FALSE,
        xlab = "",
        ylab = ""
      )

      rect(
        xleft = grconvertX(0, 'ndc', 'user') - 1000,
        xright = grconvertX(1, 'ndc', 'user') + 1000,
        ybottom = grconvertY(0,'ndc','user') - 1000,
        ytop = grconvertY(1, 'ndc', 'user') + 1000,
        border = NA,
        col = colthemeCol$ColorBGplot,
        xpd = TRUE
      )
      text(
        0.5,
        0.5,
        "Incomplete factorial context!",
        col = font_color(colthemeCol$col.bg),
        cex = 1.4
      )
      text(
        0.5,
        0.4,
        "(This graphic is not available",
        col = font_color(colthemeCol$col.bg),
        cex = 0.9
      )
      text(
        0.5,
        0.3,
        "for pseudo factorial contexts)",
        col = font_color(colthemeCol$col.bg),
        cex = 0.9
      )

    } else if (df_factorial$`Number Factors` > 3) {
      plot(
        NULL,
        xlim = c(0, 1),
        ylim = c(0, 1),
        axes = FALSE,
        xlab = "",
        ylab = ""
      )
      rect(
        xleft = grconvertX(0, 'ndc', 'user') - 1000,
        xright = grconvertX(1, 'ndc', 'user') + 1000,
        ybottom = grconvertY(0, 'ndc', 'user') - 1000,
        ytop = grconvertY(1, 'ndc', 'user') + 1000,
        border = NA,
        col = colthemeCol$ColorBGplot,
        xpd = TRUE
      )
      text(
        0.5,
        0.6,
        "Interaction plots are only implemented ",
        col = font_color(colthemeCol$col.bg)
      )
      text(
        0.5,
        0.4,
        "for 3 or less Subgroup levels!",
        col = font_color(colthemeCol$col.bg)
      )

    } else if (!any(is.na(df_factorial$Factorial[input$y])) &
              df_factorial$`Number Factors` == 1) {

      interaction_plot2(
        df_data = df_factorial$Factorial,
        fac1 = df_factorial$Variables[1],
        response = input$y,
        bg.col = colthemeCol$ColorBGplot,
        bg.col2 = different_hues(colthemeCol$col.bg),
        font.col = font_color(colthemeCol$col.bg),
        y.min = y_axe[1],
        y.max = y_axe[2],
        box.col = font_color(colthemeCol$col.bg),
        plot_type = ifelse(pl_typ == "log", "y", "")
      )

    } else if (!any(is.na(df_factorial$Factorial[input$y])) &
              df_factorial$`Number Factors` == 2) {
      interaction_plot2(
        df_data = df_factorial$Factorial,
        fac1 = df_factorial$Variables[1],
        fac2 = df_factorial$Variables[2],
        response = input$y,
        bg.col = colthemeCol$ColorBGplot,
        bg.col2 = different_hues(colthemeCol$col.bg),
        font.col = font_color(colthemeCol$col.bg),
        y.min = y_axe[1],
        y.max = y_axe[2],
        box.col = font_color(colthemeCol$col.bg),
        plot_type = ifelse(pl_typ == "log", "y", "")
      )
    } else if (!any(is.na(df_factorial$Factorial[input$y])) &
               df_factorial$`Number Factors` == 3) {

      interaction_plot2(
        df_data = df_factorial$Factorial,
          fac1 = df_factorial$Variables[1],
          fac2 = df_factorial$Variables[2],
          fac3 = df_factorial$Variables[3],
          response = input$y,
          bg.col = colthemeCol$ColorBGplot,
          bg.col2 = different_hues(colthemeCol$col.bg),
          font.col = font_color(colthemeCol$col.bg),
          y.min = y_axe[1],
          y.max = y_axe[2]
      )
    }
  })

  shiny::observeEvent(c(input$selectedSG_rows_selected, input$settheme), ignoreNULL = FALSE, {

    df_factorial <- factorialContext(scresults,click_points_data$xy[input$selectedSG_rows_selected,'SGID'])

    if (is.null(dim(df_factorial$Factorial)) | all(is.na(df_factorial$Factorial))) {

      tmp <- NULL

    } else {

      tmp.sglev <- levels(
        relevel(
          factor(
            unlist(
              lapply(df_factorial$Factorial[, scresults$factors], as.character)
            )
          ), ref = 'Not used'
        )
      )

      col.tabFont <- font_color(different_hues(colthemeCol$col.bg))
      col.tabBack <- colthemeCol$col.bg

      if(input$navpanel == "1") {
        curr_x <- shiny::req(input$x)
      } else if (input$navpanel == "2") {
        curr_x <- shiny::req(input$x2)
      }

      df_fac <- subset(
        df_factorial$Factorial,
        select = c("SGID", x = curr_x, y = input$y, "nfactors", scresults$factors)
      )

      colXY <- which(colnames(df_fac) %in% c('SGID', names(scresults$results_total), 'nfactors'))

      tmp <- DT::datatable(
        data = df_fac,
        extensions = 'Buttons',
        options = list(
          initComplete = JS(
           "function(settings, json) {",
           paste0("$(this.api().table().header()).css({'background-color': '",
                  colthemeCol$col.bg,
                  "', 'color': '",
                  font_color(different_hues(colthemeCol$col.bg)),
                  "'});"
           ),
           "}"
          ),
          dom = 'Brtip',
          buttons = c('copy','print','pageLength',I('colvis')),
          lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
          pageLength = 6
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of Factorial Contexts',
        filter = 'top'
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = colXY,
        color = col.tabFont
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = scresults$factors,
        color = DT::styleEqual(
          tmp.sglev,
          c(col.tabBack, rep(col.tabFont,length(tmp.sglev) - 1))
        )
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = 1:(ncol(df_fac)),
        target = "cell",
        backgroundColor = different_hues(colthemeCol$col.bg),
        border = paste0('.5px solid ', colthemeCol$ColorBGplot)
      )
    }
    ####... 46. factorial ####
    output$factorial <- DT::renderDataTable(tmp)

    y_axe_Int <- shiny::reactive({
      shiny::req(input$y_Interaction_Button)

      if (input$y_Interaction_Button == "Synchron") {
        tmp <- c(input$YRange[1], input$YRange[2])
      }
      if (input$y_Interaction_Button == "Optimal") {
        tmp <- c("NA", "NA")
      }
      tmp
    })
  })

  output$y_Interaction_Button2 <- shiny::renderUI({
    shiny::radioButtons(
      inputId = 'y_Interaction_Button2',
      label = 'Synchronise y-axes with main plot',
      selected = ("Synchron"),
      choices = c("Synchron", "Optimal"),
      inline = TRUE
    )
  })

  ####... 44. filteredSG ####
  shiny::observeEvent(input$filter, {
      setcolor()
  })

  shiny::observeEvent(c(input$VarChosen), {
    filt <- input$filter
    key <- shiny::req(input$key)
    if (filt != "no selection") {
      choice <- input$VarChosen
      select_points_data <<- scresults$sge[which(scresults$sge$nfactors >=
                                                   input$key[1] & scresults$sge$nfactors <=
                                                   input$key[2] &
                                                   scresults$sge[, c(filt)] == choice),]
    } else {
      select_points_data <<- data.frame(x = numeric(), y = numeric(), SGID = numeric())
    }

    if (filt == "no selection"){
      ####... 44. filteredSG ####
      output$filteredSG <- DT::renderDataTable(DT::datatable(NULL))
    }

    if (filt != "no selection") {
      df_filt <- subset(select_points_data, select = c(x = input$x, y = input$y, "nfactors", scresults$factors))

      col2hide <- which(sapply(df_filt, FUN = function(x){all(x == 'Not used')})) - 1
      names(col2hide) <- NULL

      tmp <- DT::datatable(
        data = df_filt ,
        extensions = 'Buttons',
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            paste0("$(this.api().table().header()).css({'background-color': '",
              colthemeCol$col.bg,
              "', 'color': '",
              font_color(different_hues(colthemeCol$col.bg)),
              "'});"
            ),
            "}"
          ),
          columnDefs = list(list(targets = col2hide, visible = FALSE)),
          dom = 'Brtip',buttons = c('copy', 'print', 'pageLength', I('colvis')),
          lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
          pageLength = 6
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of Filtered Subgroups',
        filter = 'top'
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = 1:ncol(df_filt),
        target = "cell",
        backgroundColor = different_hues(colthemeCol$col.bg),
        border = paste0('.5px solid ',colthemeCol$ColorBGplot)
      )

      if (dim(df_filt)[1] != 0) {
        tmp.sglev <- levels(
          relevel(
            factor(
              unlist(
                unique(
                  lapply(df_filt, as.character)
                )
              )
            ), ref = "Not used"
          )
        )
        colXY <- which(
          colnames(
            subset(
              df_filt,
              select = c(x = shiny::req(input$x), y = input$y, 'nfactors', scresults$factors)
            )
          ) %in% c('SGID', names(scresults$results_total), 'nfactors')
        )

        col.tabFont <- font_color(different_hues(colthemeCol$col.bg))
        col.tabBack <- colthemeCol$col.bg

        tmp <- DT::formatStyle(
          table = tmp,
          columns = colXY,
          color = col.tabFont
        )

        tmp <- DT::formatStyle(
          table = tmp,
          columns = scresults$factors,
          color = DT::styleEqual(
            tmp.sglev, c('black', rep(col.tabFont, length(tmp.sglev) - 1))
          )
        )
      }
      ####... 44. filteredSG ####
      output$filteredSG <- DT::renderDataTable(tmp)
    }
  })

  ####... 45.+47. parents + complement ####
  shiny::observeEvent(c(input$selectedSG_rows_selected, input$settheme), ignoreNULL = FALSE, {

    if (shiny::req(input$y) != "N.of.subjects") {

      shiny::req(input$selectedSG_rows_selected)


      IDs <- click_points_data$xy[input$selectedSG_rows_selected,]$SGID
      dat <- scresults$sge[IDs, ]

      if (input$navpanel == "1") {
        curr_x <- shiny::req(input$x)
      } else if (input$navpanel == "2") {
        curr_x <- shiny::req(input$x2)
      }

      dat <- subset(
        dat,
        select = c("SGID", x = curr_x, y = input$y, "nfactors", scresults$factors)
      )

      tmp <- DT::datatable(
        data = dat,
        extensions = 'Buttons',
        options= list(
          initComplete = JS(
            "function(settings, json) {",
            paste0("$(this.api().table().header()).css({'background-color': '",
                   colthemeCol$col.bg,
                   "', 'color': '",
                   font_color(different_hues(colthemeCol$col.bg)),
                   "'});"
            ),
            "}"
          ),
          dom = 'Brtip',
          buttons=c('copy','print','pageLength',I('colvis')),
          lengthMenu = list(c(6, 12, -1), c("6", "12", "All")), pageLength = 6
        ),
        class = 'cell-border stripe', rownames = FALSE,
        caption = 'Table of Complement Subgroup(s)', filter = 'top'
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = 1:(ncol(dat) + 1),
        target = "cell",
        backgroundColor = different_hues(colthemeCol$col.bg),
        border = paste0('.5px solid ', colthemeCol$ColorBGplot)
      )

      tmp.sglev <- levels(
        relevel(
          factor(
            unlist(
              lapply(click_points_data$xy[, scresults$factors], as.character)
            )
          ),
          ref = 'Not used'
        )
      )

      colXY <- which(
        colnames(click_points_data$xy) %in% c('SGID', names(scresults$results_total), 'nfactors')
      )

      col.tabFont <- font_color(different_hues(colthemeCol$col.bg))
      col.tabBack <- colthemeCol$col.bg

      tmp <- DT::formatStyle(
        table = tmp,
        columns = colXY,
        color = col.tabFont
      )
      tmp <- DT::formatStyle(
        table = tmp,
        columns = scresults$factors,
        color = DT::styleEqual(
          tmp.sglev, c(col.tabBack, rep(col.tabFont, length(tmp.sglev) - 1))
        )
      )

    } else {
      NULL
    }
    ####... 47. complement ####
    output$complement <- DT::renderDataTable(tmp)
  })

  ####... 48. memorizedSG ####
  df_m <- shiny::reactiveValues(data = data.frame(NULL))

  shiny::observeEvent(c(input$remove_button), {
    selectedRow <- as.numeric(strsplit(input$remove_button, "_")[[1]][2])
    df_m$data <- df_m$data[rownames(df_m$data) != as.numeric(strsplit(input$select_button, "_")[[1]][2]), ]
    selectRow <- NULL
  })

  shiny::observeEvent(c(input$select_button), {
    if(!is.null(input$select_button)){
      selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      del <- cbind(data.frame(
        Delete = shinyInput_remove(actionButton, 1, 'button_', label = "Remove",
                                   onclick = 'Shiny.onInputChange(\"remove_button\",  this.id)' )
      ),
      click_points_data$xy[click_points_data$xy$SGID == selectedRow, ])
      df_m$data <- rbind(df_m$data, del)
    }
  })

  shiny::observeEvent(c(input$select_button, input$remove_button), {

    col2hide <- which(sapply(df_m$data[,-1], FUN = function(x){all(x == 'Not used')})) - 1
    names(col2hide) <- NULL

    tmp <- DT::datatable(
      data = df_m$data,
      extensions = 'Buttons',
      escape = FALSE,
      selection = 'none',
      options = list(
        initComplete = JS(
          "function(settings, json) {",
          paste0("$(this.api().table().header()).css({'background-color': '",
            colthemeCol$col.bg,
            "', 'color': '",
            font_color(different_hues(colthemeCol$col.bg)),
            "'});"
          ),
          "}"
        ),
        dom = 'Brtip',
        columnDefs = list(list(targets = col2hide, visible = FALSE)),
        buttons = c('copy', 'print', 'pageLength', I('colvis')),
        lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
        pageLength = 6
      ),
      class = 'cell-border stripe',
      rownames = FALSE,
      caption = 'Table of Memorized Subgroups',
      filter = 'top'
    )

    if (dim(df_m$data)[1] != 0) {

      tmp <- DT::formatStyle(
        table = tmp,
        columns = 1:(ncol(df_m$data[, -1]) + 1),
        target = "cell",
        backgroundColor = different_hues(colthemeCol$col.bg),
        border = paste0('.5px solid ',colthemeCol$ColorBGplot)
      )

      tmp.sglev <- levels(
        relevel(
          factor(
            unlist(
              lapply(df_m$data[, scresults$factors], as.character)
            )
          ),
          ref = 'Not used'
        )
      )

      colXY <- which(colnames(df_m$data[, -1]) %in% c('SGID', names(scresults$results_total), 'nfactors')) + 1

      col.tabFont <- font_color(different_hues(colthemeCol$col.bg))
      col.tabBack <- colthemeCol$col.bg

      tmp <- DT::formatStyle(
        table = tmp,
        columns = names(df_m$data[, -1]),
        color = col.tabFont
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = scresults$factors,
        color = DT::styleEqual(
          tmp.sglev,
          c(col.tabBack, rep(col.tabFont, length(tmp.sglev) - 1))
        )
      )
    }
    if (dim(df_m$data)[1] == 0) {
      tmp <- NULL
    }
    ####... 48. memorizedSG ####
    output$memorizedSG <- DT::renderDataTable(tmp)
  })

  ####... 49. assessment ####
  if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(scresults$sge))) {
  output$legend4 <- shiny::renderUI({
    shiny::req(plot_points_data5())
    plot_point <- plot_points_data5()
    setcolor2()
    all_points <- cbind(plot_point, color2, stringsAsFactors = FALSE)
    gold_points_ID <- all_points[all_points$color %in% colthemeCol$ColorTabClicked,]$ID
    colored_points <- all_points[!startsWith(all_points$color, colthemeCol$ColorPoints),]
    active_colors <- unique(colored_points$color)
    shiny::tagList(
      if (length(gold_points_ID) > 0) {
        bsplus::bs_embed_tooltip(
          tag = shiny::span(shiny::tagList(shiny::tags$i(class = "fa fa-circle", style = paste0("color: ", colthemeCol$ColorTabClicked)),"Selected Subgroup")),
          title = ".",
          placement = "top",
          expanded = TRUE
        )
      },
      if (colthemeCol$ColorParents %in% active_colors) {
        bsplus::bs_embed_tooltip(
          tag = shiny::span(shiny::tagList(shiny::tags$i(class = "fa fa-circle", style = paste0("color: ", colthemeCol$ColorParents)),"Parent Subgroup(s)")),
          title = ".",
          placement = "top",
          expanded = TRUE
        )
      },


      if (colthemeCol$ColorFactCont %in% active_colors) {
        bsplus::bs_embed_tooltip(
          tag = shiny::span(shiny::tagList(shiny::tags$i(class = "fa fa-circle", style = paste0("color: ", colthemeCol$ColorFactCont)),"Factorial Context")),
          title = ".",
          placement = "top",
          expanded = TRUE
        )
      },
      if (different_hues(colthemeCol$ColorFactCont, value = 89) %in% active_colors) {
        bsplus::bs_embed_tooltip(
          tag = shiny::span(shiny::tagList(shiny::tags$i(class = "fa fa-circle", style = paste0("color: ", different_hues(colthemeCol$ColorFactCont, value = 89))),"Pseudo factorial Context")),
          title = ".",
          placement = "top",
          expanded = TRUE
        )
      }
    )
  })
  if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(scresults$sge))) {
    sorting_index <- shiny::reactive({
     sort_df <- rbind(scresults$sge[scresults$sge$nfactors %in% c(1, 2, 3, 4) & scresults$sge$FCID_incomplete == "Complete",],
            scresults$sge[scresults$sge$nfactors %in% c(2, 3, 4) & scresults$sge$FCID_pseudo != "No Pseudo",]) %>%
        dplyr::arrange(nfactors, !! rlang::sym(shiny::req(input$y)))
      if (!is.null(input$direction) & input$direction == "desc") {
        sort_df <- sort_df %>%
          dplyr::arrange(nfactors, desc(!! rlang::sym(shiny::req(input$y))))
     }
     sort_df$SGID
    })
  }


  DT_values <- shiny::reactive({
    if (input$navpanel == "subscreenasmus") {
    shinyInput_goto <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }

    df <- as.data.frame(Module_input2())
    df_add <- rbind(scresults$sge[scresults$sge$nfactors %in% c(1, 2, 3, 4) & scresults$sge$FCID_incomplete == "Complete",],
                    scresults$sge[scresults$sge$nfactors %in% c(2, 3, 4) & scresults$sge$FCID_pseudo != "No Pseudo",])$nfactors
    df_add2 <- df_add[order(as.numeric(Module_input$dat[,3]))]
    df <- cbind(df, df_add2)
    colnames(df)[colnames(df) == "df_add2"] <- "Factors"
    df <- df %>%
      dplyr::select("Factors", dplyr::everything())
    df <- cbind(df, data.frame(
      Change_to_subgroup_ID = shinyInput_goto(actionButton, dim(df)[1], 'button_', label = "Switch to Subgroup",
                             onclick = 'Shiny.onInputChange(\"goto_button\",  this.id)' )
    ))
    df
    }
  })

  output$assessment <- DT::renderDataTable(

    DT::datatable(
      shiny::isolate(DT_values()),
      escape = FALSE,
      filter = 'top',
      selection = 'none',
      extensions = 'Buttons',
      options = list(
        initComplete = JS(
          "function(settings, json) {",
          paste0("$(this.api().table().header()).css({'background-color': '",
                 colthemeCol$col.bg,
                 "', 'color': '",
                 font_color(different_hues(colthemeCol$col.bg)),
                 "'});"
          ),
          "}"
        ),
        processing = FALSE,
        deferRender = TRUE
        ,  dom = 'Brtip', buttons = c('copy','print','pageLength', I('colvis')),
        lengthMenu = list(c(6, 12, -1),
        c("6", "12", "All")),
        pageLength = 6,
        rowCallback = DT::JS(
          "function(row, data) {\n
          // Bold cells for those >= 5 in the first column\n
          if (parseFloat(data[1]) >= 15.0)\n
          $(\"td:eq(1)\", row).css(\"font-weight\", \"bold\");\n
          }"
        )
      )
    ) %>%
    DT::formatStyle(
      1, target = "row",
      backgroundColor = different_hues(colthemeCol$col.bg)
    ) %>%
    DT::formatStyle(
      c(names(shiny::isolate(DT_values()))),
      backgroundColor = DT::styleEqual(
        levels = c("No","N/A","Yes"),
        values = c("#6b5050", different_hues(colthemeCol$col.bg), "#506b50")
      ),
      color =  font_color(different_hues(colthemeCol$col.bg))
    )
  )

  proxy = DT::dataTableProxy('assessment')

  shiny::observe({
    shiny::req(DT_values())
    DT::replaceData(
      proxy,
      data = DT_values()
    )
  })

  shiny::observeEvent(input$goto_button, {
    screening_index$val <- as.numeric(strsplit(input$goto_button, "_")[[1]][2])
  })
}
  ####... 50. cont_well2 ####
  output$cont_well2 <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        type = 'text/css',
        paste0(".myclass2 {background-color: ",colthemeCol$col.bg,";}")
      )
    )
  })

  ####... 51. y1 ####
  output$y1 <- shiny::renderUI({
    start_y <- names(scresults$results_total)[1]
    shiny::selectInput(
      inputId = "y1",
      label = "First Target variable",
      choices = names(scresults$results_total),
      selected = start_y
    )
  })

  ####... 52. plot_type2 ####
  output$plot_type2 <- shiny::renderUI({
    shiny::radioButtons(
      inputId = "plot_type2",
      label ="Plot Type (Compare Plot: y-axis / Bubble Plot: x-axis)",
      selected = "lin",
      inline = TRUE,
      choiceNames = list("linear", "logarithmic"),
      choiceValues = c("lin", "log")
    )
  })

  ####... 53. YRange2 ####
  output$YRange2 <- shiny::renderUI({
    shiny::req(input$y1)
    if (input$plot_type2 == "lin") {
    shiny::sliderInput(
      inputId = "YRange2",
      label = "Range (Compare Plot: y-axis / Bubble Plot: x-axis)",
      min = roundDownNice(min(scresults$sge[, input$y1], na.rm = TRUE)),
      max = roundUpNice(max(scresults$sge[, input$y1], na.rm = TRUE)),
      value = c(min(scresults$sge[, names(scresults$results_total)[1]], na.rm = TRUE), max(scresults$sge[, input$y1], na.rm = TRUE)),
      step = roundUpNice((max(scresults$sge[, input$y1], na.rm = TRUE) - min(scresults$sge[, input$y1], na.rm = TRUE))/100)
    )

  } else {
      rg.z <- log(
        range(
          roundDownNice(min(scresults$sge[, input$y1], na.rm = TRUE)),
          roundUpNice(max(scresults$sge[, input$y1], na.rm = TRUE))
        )
      )
      choices <- unique(
        unlist(
          lapply(
            exp(seq(rg.z[1], rg.z[2], length.out = 20)),
            function(x) {roundUpNice(x = x, nice = c(2,4,8,16))}
          )
        )
      )
      shinyWidgets::sliderTextInput(
        inputId = "YRange2",
        label = "Log Range (Compare Plot: y-axis / Bubble Plot: x-axis)",
        hide_min_max = TRUE,
        choices = choices,
        selected = c(choices[1], choices[length(choices)]),
        grid = TRUE
      )
     }
  })

  ####... 54. cont_well3 ####
  output$cont_well3 <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        type = 'text/css',
        paste0(".myclass3 {background-color: ", colthemeCol$col.bg,";}")
      )
    )
  })
  ####... 55. y2 ####
  output$y2 <- shiny::renderUI({
    shiny::selectInput(
      inputId = "y2",
      label = "Second Target variable",
      choices = names(scresults$results_total),
      selected = names(scresults$results_total)[2]
    )
  })

    ####... 56. plot_type3 ####
  output$plot_type3 <- shiny::renderUI({
    shiny::radioButtons(
      inputId = "plot_type3",
      label = "Plot Type (Compare Plot: y-axis / Bubble Plot: y-axis)",
      choiceNames = list("linear", "logarithmic"),
      choiceValues = c("lin", "log"),
      selected = "lin",
      inline = TRUE
    )
  })

  shiny::observeEvent(input$plot_type3, {
    log_type$graph3 <- ifelse(input$plot_type3 == "log", "y", "")
  })

  ####... 57. YRange3 ####
  output$YRange3 <- shiny::renderUI({
    shiny::req(input$y2)
    if (input$plot_type3 == "lin") {
    shiny::sliderInput(
      inputId = "YRange3",
      label = "Y Range (Compare Plot: y-axis / Bubble Plot: y-axis)",
      min = roundDownNice(min(scresults$sge[, input$y2], na.rm = TRUE)),
      max = roundUpNice(max(scresults$sge[, input$y2], na.rm = TRUE)),
      value = c(min(scresults$sge[, input$y2], na.rm = TRUE), max(scresults$sge[, input$y2], na.rm = TRUE)),
      step = roundUpNice((max(scresults$sge[, input$y2], na.rm = TRUE) - min(scresults$sge[, names(scresults$results_total)[2]], na.rm = TRUE))/100)
    )
  } else {
      rg.z <- log(
        range(
          roundDownNice(min(scresults$sge[, input$y2], na.rm = TRUE)),
          roundUpNice(max(scresults$sge[, input$y2], na.rm = TRUE))
        )
      )

      choices <- unique(
        unlist(
          lapply(
            exp(seq(rg.z[1], rg.z[2], length.out = 20)),
            function(x) {roundUpNice(x = x, nice = c(2,4,8,16))}
          )
        )
      )

      shinyWidgets::sliderTextInput(
        inputId = "YRange3",
        label = "Log Range (Compare Plot: y-axis / Bubble Plot: y-axis)",
        hide_min_max = TRUE,
        choices =choices,
        selected = c(choices[1], choices[length(choices)]),
        grid = TRUE
      )
     }
  })

  ####... 58. cont_well4 ####
  output$cont_well4 <- shiny::renderUI({
   shiny::tags$head(
     shiny::tags$style(
        type = 'text/css',
        paste0(".myclass4 {background-color: ",
               colthemeCol$col.bg,";}"
        )
      )
    )
  })
  ####... 59. x2 ####
  output$x2 <- shiny::renderUI({
    shiny::selectInput(
      inputId = "x2",
      label = "Reference variable",
      names(scresults$results_total),
      selected =  "N.of.subjects"
    )
  })

  ####... 60. cont_well5 ####
  output$cont_well5 <- shiny::renderUI({
   shiny::tags$head(
     shiny::tags$style(
        type = 'text/css',
        paste0(".myclass5 {background-color: ",colthemeCol$col.bg,";}")
      )
    )
  })

  ####... 63. graph2 ####
  output$graph2 <- shiny::renderPlot({

    shiny::req(plot_points_data2(), input$YRange2, input$plot_type, input$pointsize)

    par(oma = c(0, 0, 0, 0), mar = c(0, 3, 0, 0), bg = colthemeCol$ColorBGplot)

    plot_point <- plot_points_data2()
    all_points <- cbind(plot_point, color, stringsAsFactors = FALSE)
    white_points <- all_points[all_points$color %in% c("#FFFFFFFF", "#FFFFFFBF", "#FFFFFF80", "#FFFFFF40", "#FFFFFF1A"), ]
    colored_points <- all_points[!all_points$color %in% c("#FFFFFFFF", "#FFFFFFBF", "#FFFFFF80", "#FFFFFF40", "#FFFFFF1A"), ]

    setcolor()

    plot(
      x = all_points$x,
      y = all_points$y,
      xlab = "",
      ylab = "",
      ylim = input$YRange2,
      log = ifelse(input$plot_type2 == "log", "y", ""),
      cex.axis = 1.5,
      cex.lab = 1.5,
      axes = FALSE,
      type = "n"
    )
    rect(
      xleft = grconvertX(0,'ndc','user') - ifelse(input$plot_type2 == "lin", 1000, 0),
      xright = grconvertX(1,'ndc','user') + ifelse(input$plot_type2 == "lin", 1000, 0),
      ybottom = grconvertY(0,'ndc','user') - ifelse(input$plot_type2 == "lin", 1000, 0),
      ytop = grconvertY(1,'ndc','user') + ifelse(input$plot_type2 == "lin", 1000, 0),
      border = NA,
      col = colthemeCol$ColorBGplot,
      xpd = TRUE
    )
    if (ifelse(input$plot_type2 == "log", "y", "") == "y") {
      miniy <- 10^par("usr")[3]
      maxiy <- 10^par("usr")[4]
      lowy  <- 10^(par("usr")[3] + (par("usr")[4] - par("usr")[3])/40)
      lowyp <- 10^(par("usr")[3] + (par("usr")[4] - par("usr")[3])/15)
      minplustinyy <- 10^(par("usr")[3] + (par("usr")[4] -
                                             par("usr")[3])/1400)
    } else {
      miniy <- par("usr")[3]
      maxiy <- par("usr")[4]
      lowy  <- miniy + (maxiy - miniy)/40
      lowyp <- miniy + (maxiy - miniy)/15
      minplustinyy <- miniy + (maxiy - miniy)/1400
    }

    minix <- roundDownNice(par("usr")[1])
    maxix <- roundUpNice(par("usr")[2])
    nr <- 7
    stepx <- roundUpNice((maxix - minix)/(nr +  1))
    if (minix < stepx)
      minix <- 0
    stripesx <- 0:(nr + 1)
    stripesx <- lapply(stripesx, function(x) x * stepx)
    stripesx <- lapply(stripesx, function(x) x + minix)
    stripesxp <- lapply(stripesx, function(x) paste(floor(x/scresults$results_total[,c(input$x2)] * 100), "%"))

    for (i in seq(1, nr, 2)) {
      rect(
        stripesx[i],
        miniy,
        stripesx[i + 1],
        maxiy,
        col = different_hues(colthemeCol$col.bg),
        border = NA
      )
    }
    if(input$xlabel == TRUE){
      text(stripesx, lowy, stripesx, cex = 1.2, col = font_color(colthemeCol$col.bg))
      text(stripesx, lowyp, stripesxp, cex = 1.2, col = font_color(colthemeCol$col.bg))
    }

    box(col = font_color(colthemeCol$col.bg))

    graphics::axis(
      side = 2,
      col = font_color(colthemeCol$col.bg),
      col.ticks = font_color(colthemeCol$col.bg),
      col.axis = font_color(colthemeCol$col.bg),
      cex.axis = 1
    )

    title(
      main = SG_tit(),
      line = -2,
      col = "#8b8b8b",
      col.main = font_color(colthemeCol$col.bg)
    )

    abline(
      h = ref_line(),
      lwd = 3,
      col = colthemeCol$ColorReference
    )

    pch_ <- ifelse(input$pch_value == "19", 19, input$pch_value)

    if (input$circlestyle == "standard") {
      points(white_points$x, white_points$y, pch = pch_, cex = input$pointsize, col = white_points$color)
      points(colored_points$x, colored_points$y, pch = pch_, cex = input$pointsize, col = colored_points$color)
    }
    if (input$circlestyle == "groupsize") {
      points(
        white_points$x,
        white_points$y,
        pch = pch_,
        cex = input$pointsize * sqrt(scresults$sge[scresults$sge$SGID %in% white_points$ID, 'N.of.subjects'][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]]/pi), col = white_points$color)
      points(
        colored_points$x,
        colored_points$y,
        pch = pch_,
        cex = input$pointsize * sqrt(scresults$sge[scresults$sge$SGID %in% colored_points$ID , 'N.of.subjects'][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]]/pi), col = colored_points$color)
    }

    text(
      x = grconvertX(0.97, from = 'npc', to = 'user'),
      y = grconvertY(0.06, from = 'nfc', to = 'user') + ref_line(),
      paste0(ref_line()), col = colthemeCol$ColorReference
    )

    points(shiny::isolate(plot_points_data_complement()),
           pch = ifelse(input$pch_value == "19", 13, '.'),
           cex = ifelse(input$circlestyle == "groupsize",
                        shiny::isolate(input$pointsize) * sqrt(plot_points_data_complement()$x/pi),
                        shiny::isolate(input$pointsize)
           ),
           col = "#fffb00")

    if(input$grid == TRUE){
      abline(h = axTicks(2), lty = 2, col = font_color(colthemeCol$col.bg), lwd = 0.3)
      abline(v = axTicks(1), lty = 2, col = font_color(colthemeCol$col.bg), lwd = 0.3)
    }

  })

  ####... XY. plot_type ####
  output$plot_type_asmus <- shiny::renderUI({
    shiny::radioButtons(
      inputId = "plot_type_asmus",
      label = "Plot Type",
      selected = "lin",
      inline = TRUE,
      choiceNames = list("linear", "logarithmic"),
      choiceValues = c("lin", "log")
    )
  })

  output$yrange_asmus <- shiny::renderUI({
    shiny::req(input$y)

    if (req(input$plot_type_asmus) == "lin") {
    shiny::sliderInput(
      inputId = "yrange_asmus",
      label = "Y Range",
      min = roundDownNice(min(scresults$sge[, input$y], na.rm = TRUE)),
      max = roundUpNice(max(scresults$sge[, input$y], na.rm = TRUE)),
      value = c(min(scresults$sge[, names(scresults$results_total)[1]], na.rm = TRUE), max(scresults$sge[, names(scresults$results_total)[1]], na.rm = TRUE)),
      step = roundUpNice((max(scresults$sge[, input$y], na.rm = TRUE) - min(scresults$sge[,shiny::req(input$y)], na.rm = TRUE))/100)
    )
    } else {
      rg.z <- log(
        range(
          roundDownNice(min(scresults$sge[, input$y], na.rm = TRUE)),
          roundUpNice(max(scresults$sge[, input$y], na.rm = TRUE))
        )
      )

      choices <- unique(
        unlist(
          lapply(
            exp(seq(rg.z[1], rg.z[2], length.out = 20)),
            function(x) {roundUpNice(x = x, nice = c(2,4,8,16))}
          )
        )
      )

      shinyWidgets::sliderTextInput(
        inputId = "yrange_asmus",
        label = "Y Range",
        hide_min_max = TRUE,
        choices = choices,
        selected = c(choices[1], choices[length(choices)]),
        grid = TRUE
      )
    }
  })

  ####... 64. graph3 ####
  output$graph3 <- shiny::renderPlot({
    shiny::req(input$YRange3)

    plot_point <- plot_points_data3()
    all_points <- cbind(plot_point, color, stringsAsFactors = FALSE)
    white_points <- all_points[all_points$color %in% c("#FFFFFFFF", "#FFFFFFBF", "#FFFFFF80", "#FFFFFF40", "#FFFFFF1A"),]
    colored_points <- all_points[!all_points$color %in% c("#FFFFFFFF", "#FFFFFFBF", "#FFFFFF80", "#FFFFFF40", "#FFFFFF1A"),]

    par(oma = c(0, 0, 0, 0), mar = c(0, 3, 0, 0), bg = colthemeCol$ColorBGplot)
    plot(
      all_points$x,
      all_points$y,
      xlab = "",
      ylab = "",
      ylim = input$YRange3,
      log = log_type$graph3,
      cex.axis = 1.5,
      cex.lab = 1.5,
      axes = FALSE,
      type = "n",
      bg = colthemeCol$ColorBGplot
    )

    rect(
      xleft = grconvertX(0, 'ndc', 'user'),
      xright = grconvertX(1, 'ndc', 'user'),
      ybottom = grconvertY(0, 'ndc', 'user'),
      ytop = grconvertY(1, 'ndc', 'user'),
      border = NA,
      col = colthemeCol$ColorBGplot,
      xpd = TRUE
    )
    if (log_type$graph3 == "y") {
      miniy <- 10^par("usr")[3]
      maxiy <- 10^par("usr")[4]
      lowy  <- 10^(par("usr")[3] + (par("usr")[4] - par("usr")[3])/40)
      lowyp <- 10^(par("usr")[3] + (par("usr")[4] - par("usr")[3])/15)
      minplustinyy <- 10^(par("usr")[3] + (par("usr")[4] -
                                             par("usr")[3])/1400)
    } else {
      miniy <- par("usr")[3]
      maxiy <- par("usr")[4]
      lowy <- miniy + (maxiy - miniy)/40
      lowyp <- miniy + (maxiy - miniy)/15
      minplustinyy <- miniy + (maxiy - miniy)/1400
    }

    minix <- roundDownNice(par("usr")[1])
    maxix <- roundUpNice(par("usr")[2])
    nr <- 7
    stepx <- roundUpNice((maxix - minix)/(nr +  1))
    if (minix < stepx)
      minix <- 0
    stripesx <- 0:(nr + 1)
    stripesx <- lapply(stripesx, function(x) x * stepx)
    stripesx <- lapply(stripesx, function(x) x + minix)
    stripesxp <- lapply(stripesx, function(x) paste(floor(x/scresults$results_total[,c(input$x2)] * 100), "%"))

    for (i in seq(1, nr, 2)) rect(stripesx[i],miniy, stripesx[i + 1], maxiy, col = different_hues(colthemeCol$col.bg), border = NA)
    if (input$xlabel == TRUE) {
      text(stripesx, lowy, stripesx, cex = 1.2, col=font_color(colthemeCol$col.bg))
      text(stripesx, lowyp, stripesxp, cex = 1.2, col=font_color(colthemeCol$col.bg))
    }
    box(col = font_color(colthemeCol$col.bg))
    axis(
      2,
      col = font_color(colthemeCol$col.bg),
      col.ticks = font_color(colthemeCol$col.bg),
      col.axis = font_color(colthemeCol$col.bg),
      cex.axis = 1
    )
    abline(
      h = scresults$results_total[, c(input$y2)],
      lwd = 3,
      col = colthemeCol$ColorReference
    )

    pch_ <- ifelse(input$pch_value == "19", 19, input$pch_value)

    if (input$circlestyle == "standard") {
      points(
        white_points$x,
        white_points$y,
        pch = pch_,
        cex = input$pointsize,
        col = white_points$color
      )
      points(
        colored_points$x,
        colored_points$y,
        pch = pch_,
        cex = input$pointsize,
        col = colored_points$color
      )
    }
    if (input$circlestyle == "groupsize") {
      points(
        white_points$x,
        white_points$y,
        pch = pch_,
        cex = input$pointsize * sqrt(scresults$sge[scresults$sge$SGID %in% white_points$ID, 'N.of.subjects'][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]]/pi),
        col = white_points$color
      )
      points(
        colored_points$x,
        colored_points$y,
        pch = pch_,
        cex = input$pointsize * sqrt(scresults$sge[scresults$sge$SGID %in% colored_points$ID, 'N.of.subjects'][scresults$sge$nfactors >= input$key[1] & scresults$sge$nfactors <= input$key[2]]/pi),
        col = colored_points$color
      )
    }

    if (input$grid == TRUE) {
      abline(
        h = axTicks(2),
        lty = 2,
        col = font_color(colthemeCol$col.bg),
        lwd = 0.3
      )
      abline(
        v = axTicks(1),
        lty = 2,
        col = font_color(colthemeCol$col.bg),
        lwd = 0.3
      )
    }

    points(
      shiny::isolate(plot_points_data_complement()),
      pch = ifelse(input$pch_value == "19", 13, '.'),
      cex = ifelse(input$circlestyle == "groupsize",
                  shiny::isolate(input$pointsize) * sqrt(plot_points_data_complement()$x / pi),
                  shiny::isolate(input$pointsize)
      ),
      col = "#fffb00"
    )

    text(
      x = grconvertX(0.97, from = 'nfc', to = 'user'),
      y = grconvertY(0.06,from = 'nfc', to = 'user') + scresults$results_total[, c(input$y2)], paste0(scresults$results_total[, c(input$y2)]),
      col = colthemeCol$ColorReference
    )
  })
  ####... 66. graph4 ####

  output$graph4 <- shiny::renderPlot({

    click_points_data$xy

    key <- shiny::req(input$key)
    if (ifelse(input$plot_type2 == "log", "y", "") != "y" & log_type$graph3 != "y") {
      par(
        oma = c(0, 0, 0, 0),
        mar = c(3, 3, 1, 1),
        bg = colthemeCol$ColorBGplot
      )

      setcolor()

      plot(
        x = 0,
        y = 0,
        xlim = input$YRange2,
        ylim = input$YRange3,
        xlab = '',
        ylab = '',
        type = 'n',
        axes = FALSE,
        log = ""
      )

      rect(
        xleft = grconvertX(0,'ndc','user'),
        xright = grconvertX(1,'ndc','user'),
        ybottom = grconvertY(0,'ndc','user') ,
        ytop = grconvertY(1,'ndc','user') ,
        border = NA,
        col = colthemeCol$ColorBGplot,
        xpd = TRUE
      )

      suppressWarnings(
        graphics::symbols(
          main = SG_tit(),
          col.main = font_color(colthemeCol$col.bg),
          plot_points_data4()$x,
          plot_points_data4()$y,
          circles = sqrt((scresults$sge[, c('N.of.subjects')][scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]] )/ pi ),
          inches = 1/3,
          xlim = input$YRange2,
          ylim = input$YRange3,
          fg = "grey",
          bg = color,
          log = "",
          add = TRUE
        )
      )
    }

    if (ifelse(input$plot_type2 == "log", "y", "") == "y" & log_type$graph3 != "y") {
      par(
        oma = c(0, 0, 0, 0),
        mar = c(3, 3, 1, 1),
        bg = colthemeCol$ColorBGplot
      )

      setcolor()

      plot(
        x = 1,
        y = 0,
        xlim = input$YRange2,
        ylim = input$YRange3,
        xlab = '',
        ylab = '',
        type = 'n',
        axes = FALSE,
        log = "x"
      )

      rect(
        xleft = grconvertX(0,'ndc','user') - ifelse(input$plot_type2 == "lin", 1000, 0),
        xright = grconvertX(1,'ndc','user') + ifelse(input$plot_type2 == "lin", 1000, 0),
        ybottom = grconvertY(0,'ndc','user') - ifelse(input$plot_type2 == "lin", 1000, 0),
        ytop = grconvertY(1,'ndc','user') + ifelse(input$plot_type2 == "lin", 1000, 0),
        border = NA,
        col = colthemeCol$ColorBGplot,
        xpd = TRUE
      )

      suppressWarnings(
        graphics::symbols(
          main = SG_tit(),
          col.main = font_color(colthemeCol$col.bg),
          plot_points_data2()$y,
          plot_points_data3()$y,
          circles = sqrt(( scresults$sge[,c('N.of.subjects')][scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]] )/ pi),
          inches = 1/3,
          xlim = input$YRange2,
          ylim = input$YRange3,
          fg = "grey",
          bg = color,
          log = "x",
          add = TRUE
        )
      )
    }

    if (ifelse(input$plot_type2 == "log", "y", "") != "y" & log_type$graph3 == "y") {
      par(
        oma = c(0, 0, 0, 0),
        mar = c(3, 3, 1, 1),
        bg = colthemeCol$ColorBGplot
      )

      setcolor()

      plot(
        x = 0,
        y = 1,
        xlim = input$YRange2,
        ylim = input$YRange3,
        xlab = '',
        ylab = '',
        type = 'n',
        axes = FALSE,
        log = "y"
      )

      rect(
        xleft = grconvertX(0,'ndc','user'),
        xright = grconvertX(1,'ndc','user'),
        ybottom = grconvertY(0,'ndc','user'),
        ytop = grconvertY(1,'ndc','user'),
        border = NA,
        col = colthemeCol$ColorBGplot,
        xpd = TRUE
      )

      suppressWarnings(
        graphics::symbols(
          main = SG_tit(),
          col.main = font_color(colthemeCol$col.bg),
          plot_points_data4()$x,
          plot_points_data4()$y,
          circles = sqrt(( scresults$sge[,c('N.of.subjects')][scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]] )/ pi ),
          inches = 1/3,
          xlim = input$YRange2,
          ylim = input$YRange3,
          fg = "grey",
          bg = color,
          log =  "y",
          add = TRUE
        )
      )
    }

    if (ifelse(input$plot_type2 == "log", "y", "") == "y" & log_type$graph3 == "y") {
      par(
        oma = c(0, 0, 0, 0),
        mar = c(3, 3, 1, 1),
        bg = colthemeCol$ColorBGplot
      )

      setcolor()

      plot(
        x = 1,
        y = 1,
        xlim = input$YRange2,
        ylim = input$YRange3,
        xlab = '',
        ylab = '',
        type = 'n',
        axes = FALSE,
        log = "yx"
      )

      rect(
        xleft = grconvertX(0, 'ndc', 'user'),
        xright = grconvertX(1, 'ndc', 'user'),
        ybottom = grconvertY(0, 'ndc', 'user'),
        ytop = grconvertY(1, 'ndc', 'user'),
        border = NA,
        col = colthemeCol$ColorBGplot,
        xpd = TRUE
      )

      suppressWarnings(
        graphics::symbols(
          main = SG_tit(),
          col.main = font_color(colthemeCol$col.bg),
          x = plot_points_data4()$y,
          y = plot_points_data4()$x,
          circles = sqrt((scresults$sge[, c('N.of.subjects')][scresults$sge$nfactors >= key[1] & scresults$sge$nfactors <= key[2]] )/ pi),
          inches = 1/3,
          xlim = input$YRange2,
          ylim = input$YRange3,
          fg = "grey",
          bg = color,
          log = "yx",
          add = TRUE
        )
      )
    }

    box(col = font_color(colthemeCol$col.bg))
    axis(
      1,
      col = font_color(colthemeCol$col.bg),
      col.ticks = font_color(colthemeCol$col.bg),
      col.axis = font_color(colthemeCol$col.bg),
      cex.axis = 1
    )
    axis(
      2,
      col = font_color(colthemeCol$col.bg),
      col.ticks = font_color(colthemeCol$col.bg),
      col.axis = font_color(colthemeCol$col.bg),
      cex.axis = 1
    )
    graphics::mtext(
      text = input$y,
      side = 1,
      line = 3,
      col = font_color(colthemeCol$col.bg),
      cex = 1
    )

    graphics::mtext(
      text = input$y2,
      side = 2,
      line = 3,
      col = font_color(colthemeCol$col.bg),
      cex = 1
    )
  })

  ####... 67. PanelMosaic ####
  output$PanelMosaic <- shiny::renderUI({
    style.panel <- paste('background-color: ', colthemeCol$ColorBGplot, ';padding: 9px;')
    shiny::wellPanel(
      style = style.panel,
      ####... 67. (I) var1 ####
      shiny::selectInput(
        inputId = "var1",
        label = "First subgroup variable (x)",
        choices = scresults$factors,
        selected = scresults$factors[1]
      ),
      ####... 67. (II) var2 ####
      shiny::selectInput(
        inputId = "var2",
        label = "Second subgroup variable (y)",
        choices = c('no selection', scresults$factors),
        selected = 'no selection'
      ),
      ####... 67. (III) var22 ####
      shiny::selectInput(
        inputId = "var22",
        label = "Third subgroup variable (y2)",
        choices = c('no selection', scresults$factors),
        selected = 'no selection'
      ),
      ####... 67. (IV) var3 ####
      shiny::selectInput(
        inputId = "var3",
        label = "Reference variable (color)",
        choices = setdiff(names(scresults$results_total),'N.of.subjects'),
        selected = input$y
      ),
      ####... 67. (V) logmosaic ####
      shiny::radioButtons(
        inputId = "logmosaic",
        label = "Plot Type",
        choices = c(linear = "lin", logarithmic = "log"),
        selected = "lin",
        inline = TRUE
      ),
      "Use mouse hover to get further information about the subgroup(s)!",
      bsplus::use_bs_popover(),
      bsplus::use_bs_tooltip()
    )
  })

  ####... 68. mosaic ####
  output$mosaic <- shiny::renderPlot({
    mos.x <- shiny::req(input$var1)
    mos.y <- shiny::req(input$var2)
    mos.y2 <- shiny::req(input$var22)
    mos.z <- shiny::req(input$var3)
    col.bg <- colthemeCol$ColorBGplot
    col.txt <- font_color(colthemeCol$col.bg)
    colrange.z <- c('#00BCFF','gray89','#89D329')
    not.used <- 'Not used'
    if (mos.y == 'no selection') {
      mos.y <- NULL
    }
    if (mos.y2 == 'no selection' | is.null(mos.y)) {
      mos.y2 <- NULL
    }
    if (!is.null(mos.y)) {
      if (mos.x == mos.y) {
        mos.y <- NULL
      }
    }
    if (!is.null(mos.y2)) {
      if (mos.x == mos.y2 | mos.y == mos.y2) {
        mos.y2 <- NULL
      }
    }
    tmp <- scresults$sge[scresults$sge$nfactors == 1 & !scresults$sge[, mos.x] %in% not.used, ]
    tmp <- arrange_(tmp, .dots = c(mos.x))
    prop.x <- cumsum(tmp[, 'N.of.subjects'])
    prop.x <- c(0,prop.x) / max(prop.x)
    mid.x <- (prop.x[-length(prop.x)] + prop.x[-1])/2
    names(mid.x) <- paste0(mos.x, ' = ', tmp[, mos.x])
    prop.y <- c(0, 1)
    mid.y <- 0.5
    if (!is.null(mos.y)) {
      tmp <- scresults$sge[scresults$sge$nfactors == 1 & !scresults$sge[, mos.y] %in% not.used, ]
      tmp <- arrange_(tmp, .dots = c(mos.y))
      prop.y <- cumsum(tmp[, 'N.of.subjects'])
      prop.y <- c(0,prop.y) / max(prop.y)
      mid.y <- (prop.y[-length(prop.y)] + prop.y[-1])/2
      names(mid.y) <- paste0(mos.y, ' = ',tmp[, mos.y])
      if (!is.null(mos.y2)) {
        tmp <- scresults$sge[scresults$sge$nfactors == 2 & !scresults$sge[, mos.y] %in% not.used &
                               !scresults$sge[, mos.y2] %in% not.used, ]
        tmp <- arrange_(tmp, .dots = c(mos.y,mos.y2))
        prop.y <- cumsum(tmp[, 'N.of.subjects'])
        prop.y <- c(0, prop.y)/max(prop.y)
        mid.y <- (prop.y[-length(prop.y)] + prop.y[-1])/2
        names(mid.y) <- paste0(mos.y, ' = ', tmp[, mos.y], '\n', mos.y2, ' = ', tmp[,mos.y2])
      }
    }
    if (shiny::req(input$logmosaic) == "lin") {
      rg.z <- range(scresults$sge[, mos.z], na.rm = TRUE)
    }
    if (shiny::req(input$logmosaic) == "log") {
      rg.z <- log(
        range(
          scresults$sge[, mos.z], na.rm = TRUE
        )
      )
    }

    if (is.null(mos.y)) {
      tmp <- scresults$sge[scresults$sge$nfactors == 1 & !scresults$sge[, mos.x] %in% not.used, ]
    } else {
      if (is.null(mos.y2)) {
        tmp <- scresults$sge[scresults$sge$nfactors == 2 & !scresults$sge[, mos.x] %in% not.used & !scresults$sge[, mos.y] %in% not.used,]
      } else {
        tmp <- scresults$sge[scresults$sge$nfactors == 3 & !scresults$sge[, mos.x] %in% not.used &
                               !scresults$sge[, mos.y] %in% not.used & !scresults$sge[, mos.y2] %in% not.used, ]
      }
    }

    if(any(sapply(tmp, class) == 'character')) {
      tmp[sapply(tmp, is.character)] <- lapply(tmp[sapply(tmp, is.character)], factor)
    }
    comb.full <- unique(expand.grid(tmp[, c(mos.x, mos.y, mos.y2), drop = FALSE]))
    tmp <- merge(tmp, comb.full, all.y = TRUE)

    tmp <- arrange_(tmp, .dots = c(mos.x, mos.y, mos.y2))
    if(shiny::req(input$logmosaic) == "lin") {
      val.z <- matrix(tmp[, mos.z], ncol = length(prop.x) - 1, byrow = FALSE)
    }
    if(shiny::req(input$logmosaic)=="log") {
      val.z <- matrix(log(tmp[, mos.z]), ncol = length(prop.x) - 1, byrow = FALSE)
    }

    mean.z <- ifelse(shiny::req(input$logmosaic) == "lin",
                     scresults$results_total[,mos.z],
                     log(scresults$results_total[,mos.z]))
    tr.mean.z <- (mean.z-rg.z[1])/diff(rg.z)
    f_colZ <- colorRamp(colrange.z, bias = log(tr.mean.z, base = 0.5))

    par(
      mar = c(1, 8, 3, 12),
      bg = col.bg,
      oma = c(0, 0, 0, 0)
    )
    plot(
      NULL,
      xlim = c(0, 1),
      ylim = c(0,1),
      xlab = '',
      ylab = '',
      axes = FALSE,
      xaxs = 'i',
      yaxs = 'i'
    )

    for (i in 1:length(mid.x)) {
      for (j in 1:length(mid.y)) {
        val.z.ij <- val.z[j,i]
        col.z.ij <- ifelse(
          is.na(val.z.ij),
          col.bg,
          rgb(f_colZ((val.z.ij - rg.z[1])/diff(rg.z)), maxColorValue = 255)
        )
        rect(
          xleft = prop.x[i],
          xright = prop.x[i + 1],
          ybottom = prop.y[j],
          ytop = prop.y[j + 1],
          col = col.z.ij,
          border = col.bg,
          lwd = 4
        )
      }
    }

    text(
      x = mid.x,
      y = 1.025,
      xpd = NA,
      adj = c(0.5, 0),
      col = col.txt,
      labels = names(mid.x),
      cex = ifelse(is.null(mos.y2), 1, 0.75)
    )

    text(
      y = mid.y,
      x = -0.025,
      xpd = NA,
      adj = c(1, 0.5),
      col = col.txt,
      labels = names(mid.y),
      srt = 0,
      cex = ifelse(is.null(mos.y2), 1, 0.75)
    )

    leg.x <- grconvertX(1,'npc','user') + 0.5 * (grconvertX(1, 'ndc', 'user') - grconvertX(1, 'npc', 'user'))
    leg.y <- seq(grconvertY(0.1, 'npc', 'user'), grconvertY(0.9, 'npc', 'user'), length.out = 201)
    leg.width <- 0.05
    rect(
      xleft = leg.x - leg.width / 2,
      xright = leg.x + leg.width / 2,
      ybottom = leg.y[-1],
      ytop = leg.y[-length(leg.y)],
      xpd = NA,
      col = rgb(f_colZ(seq(0, 1, length.out = length(leg.y) - 1)), maxColorValue = 255), border = NA)

    ndig <- 2
    if(shiny::req(input$logmosaic) == "lin") {
      ticks.q <- c(0, 1, 2, 3, 4) / 4
      text(
        x = leg.x - (leg.width / 2 + 0.01),
        y = quantile(leg.y, prob = ticks.q),
        xpd = NA,
        col = col.txt,
        adj = c(1, 0.5),
        labels = round(quantile(seq(rg.z[1], rg.z[2], length.out = 201), prob = ticks.q), ndig),
        cex = 0.75
      )
    }
    if (shiny::req(input$logmosaic) == "log") {
      ticks.q <- c(0, 1, 2, 3, 4) / 4
      text(
        x = leg.x - (leg.width / 2 + 0.01),
        y = quantile(leg.y, prob = ticks.q),
        xpd = NA,
        col = col.txt,
        adj = c(1, 0.5),
        labels = round(exp(quantile(seq(rg.z[1], rg.z[2], length.out = 201), prob = ticks.q)), ndig),
        cex = 0.75
      )
    }

    segments(
      x0 = leg.x + (leg.width / 2),
      x1 = leg.x + (leg.width / 2 + 0.01),
      y0 = quantile(leg.y, prob = tr.mean.z),
      col = col.txt,
      lwd = 2,
      xpd = NA
    )

    text(
      x = leg.x + (leg.width / 2 + 0.02),
      y = quantile(leg.y, prob = tr.mean.z),
      xpd = NA,
      col = col.txt,
      adj = c(0, 0.5),
      font = 2,
      labels = paste0(ifelse(shiny::req(input$logmosaic) == "lin", round(mean.z, ndig), round(exp(mean.z), ndig)),' (total)'),
      cex = 0.75
    )

    text(
      x = leg.x,
      y = grconvertY(0.925, 'npc', 'user'),
      xpd = NA,
      col = col.txt,
      adj = c(0.5, 0),
      srt = 0,
      labels = mos.z,
      cex = 1,
      font = 2
    )

  }, bg = "transparent"
  )

  ####... 69. tmp_info ####
  hoverlabel <- shiny::reactiveValues(value = NULL)
  shiny::observeEvent(c(input$plot_hover$x, input$plot_hover$y, input$var1, input$var2, input$var22, input$var3), ignoreNULL = FALSE, {
    if (!is.null(input$plot_hover$x) & !is.null(input$plot_hover$y)) {

      mos.x <- input$var1
      mos.y <- input$var2
      mos.y2 <- input$var22
      mos.z <- input$var3

      not.used <- 'Not used'

      if (mos.y == 'no selection') {
        mos.y <- NULL
      }
      if (mos.y2 == 'no selection' | is.null(mos.y)) {
        mos.y2 <- NULL
      }
      if (!is.null(mos.y)) {
        if (mos.x == mos.y) {
          mos.y <- NULL
        }
      }
      if (!is.null(mos.y2)) {
        if (mos.x == mos.y2 | mos.y == mos.y2) {
          mos.y2 <- NULL
        }
      }

      tmp <- scresults$sge[scresults$sge$nfactors == 1 & !scresults$sge[,mos.x] %in% not.used, ]
      tmp <- arrange_(tmp, .dots = c(mos.x))
      prop.x <- cumsum(tmp[, 'N.of.subjects'])
      prop.x <- c(0, prop.x) / max(prop.x)
      mid.x <- (prop.x[-length(prop.x)] + prop.x[-1]) / 2
      names(mid.x) <- paste0(mos.x,' = ', tmp[, mos.x])
      hov.x <- tmp[, mos.x]

      prop.y <- c(0, 1)
      mid.y <- 0.5
      if (!is.null(mos.y)) {
        if (is.null(mos.y2)) {
          tmp <- scresults$sge[scresults$sge$nfactors == 1 & !scresults$sge[, mos.y] %in% not.used, ]
          tmp <- arrange_(tmp, .dots = c(mos.y))
          prop.y <- cumsum(tmp[, 'N.of.subjects'])
          prop.y <- c(0, prop.y) / max(prop.y)
          mid.y <- (prop.y[-length(prop.y)] + prop.y[-1]) / 2
          names(mid.y) <- paste0(mos.y,' = ', tmp[, mos.y])

          hov.y <- tmp[, mos.y]

        } else {
          tmp <- scresults$sge[scresults$sge$nfactors == 2 & !scresults$sge[, mos.y] %in% not.used &
                                 !scresults$sge[,mos.y2] %in% not.used, ]
          tmp <- arrange_(tmp, .dots = c(mos.y, mos.y2))
          prop.y <- cumsum(tmp[, 'N.of.subjects'])
          prop.y <- c(0, prop.y) / max(prop.y)
          mid.y <- (prop.y[-length(prop.y)] + prop.y[-1]) / 2
          names(mid.y) <- paste0(mos.y, ' = ', tmp[, mos.y], '\n', mos.y2, ' = ', tmp[, mos.y2])
          hov.y <- tmp[, c(mos.y, mos.y2)]
        }
      }
      if (is.null(mos.y)) {
        tmp <- scresults$sge[scresults$sge$nfactors == 1 & !scresults$sge[, mos.x] %in% not.used, ]
      } else {
        if (is.null(mos.y2)) {
          tmp <- scresults$sge[scresults$sge$nfactors == 2 & !scresults$sge[, mos.x] %in% not.used & !scresults$sge[, mos.y] %in% not.used, ]
        } else {
          tmp <- scresults$sge[scresults$sge$nfactors == 3 & !scresults$sge[, mos.x] %in% not.used &
                                 !scresults$sge[, mos.y] %in% not.used & !scresults$sge[, mos.y2] %in% not.used, ]
        }
      }
      if (any(sapply(tmp, class) == 'character')) {
        tmp[sapply(tmp, is.character)] <- lapply(tmp[sapply(tmp, is.character)], factor)
      }
      comb.full <- unique(expand.grid(tmp[,c(mos.x,mos.y,mos.y2), drop=FALSE]))

      tmp <- merge(tmp, comb.full, all.y = TRUE)

      tmp <- arrange_(tmp, .dots = c(mos.x, mos.y, mos.y2))

      col.disp <- unique(c(mos.x, mos.y, mos.y2, setdiff(colnames(tmp), c(scresults$factors, 'nfactors'))))

      if (is.null(mos.y)) {
        hoverlabel$value <- tmp[tmp[, mos.x] == (hov.x[cut(input$plot_hover$x, prop.x, labels = FALSE)]), col.disp]
      } else {
        if (is.null(mos.y2)) {
          hoverlabel$value <- tmp[tmp[,mos.x] == (hov.x[cut(input$plot_hover$x, prop.x, labels = FALSE)]) &
                                    tmp[,mos.y] == (hov.y[cut(input$plot_hover$y, prop.y, labels = FALSE)]),col.disp]
        }else{
          hoverlabel$value <- tmp[tmp[,mos.x] == (hov.x[cut(input$plot_hover$x, prop.x, labels = FALSE)]) &
                              tmp[,mos.y] == (hov.y[,mos.y][cut(input$plot_hover$y, prop.y, labels = FALSE)]) &
                              tmp[,mos.y2] == (hov.y[,mos.y2][cut(input$plot_hover$y, prop.y, labels = FALSE)]),col.disp]
        }
      }

      hoverlabel$value <- hoverlabel$value[, !startsWith(colnames(hoverlabel$value), "FCID_")]
      hoverlabel$value <- hoverlabel$value[, !startsWith(colnames(hoverlabel$value), "Complement_")]

      if (!is.null(hoverlabel$value)) {
        dt.sginfo <- DT::datatable(
          data = hoverlabel$value,
          extensions = 'Buttons',
          escape = FALSE,
          options = list(
            initComplete = JS(
              "function(settings, json) {",
              paste0(
                "$(this.api().table().header()).css({'background-color': '",
                 colthemeCol$col.bg,
                 "', 'color': '",
                 font_color(different_hues(colthemeCol$col.bg)),
                 "'});"
              ),
              "}"
            ),
            dom = 'rtp',
            paging = FALSE,
            pageLength = 1,
            bSort = FALSE
          ),
          class = 'cell-border stripe',
          rownames = FALSE,
          caption = 'Subgroup information',
          filter = 'none'
        )

        col.tabFont <- font_color(different_hues(colthemeCol$col.bg))
        dt.sginfo <- DT::formatStyle(
          table = dt.sginfo,
          columns = 1:ncol(hoverlabel$value),
          backgroundColor = different_hues(colthemeCol$col.bg),
          border = paste0('.5px solid ', colthemeCol$ColorBGplot),
          color = col.tabFont
        )
        ####... 69. tmp_info ####
        output$tmp_info <- DT::renderDataTable(dt.sginfo)
      }
    } else {
      ####... 69. tmp_info ####
      output$tmp_info <- DT::renderDataTable(NULL)
    }
  })

  output$mydropdown_bgcolor <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          paste0(
            "#dropdown-menu-MyDropDown {
            background-color: ",
            colthemeCol$ColorBGplot,
            " !important;}
            "
          )
        )
      )
    )
  })

  output$mydropdown_bgcolor2 <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          paste0(
            "#dropdown-menu-MyDropDown2 {
             background-color: ",
             colthemeCol$ColorBGplot,
             " !important;}
             "
          )
        )
      )
    )
  })

  ####... 70. graph5 ####
  if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(scresults$sge))) {
  output$graph5 <- shiny::renderPlot({
    shiny::req(plot_points_data5())
    input$screening_forward
    input$screening_backward
    par(
      oma = c(0, 0, 0, 0),
      mar = c(0, 3, 0, 0),
      bg = colthemeCol$ColorBGplot
    )
    plot_point <- plot_points_data5()
    setcolor2()
    all_points <- cbind(plot_point, color2, stringsAsFactors = FALSE)

    gold_points_ID <- all_points[all_points$color %in% colthemeCol$ColorTabClicked, ]$ID

    white_points <- all_points[all_points$color %in% c("#FFFFFFFF", "#FFFFFFBF", "#FFFFFF80", "#FFFFFF40", "#FFFFFF1A"), ]
    colored_points <- all_points[!all_points$color %in% c("#FFFFFFFF", "#FFFFFFBF", "#FFFFFF80", "#FFFFFF40", "#FFFFFF1A"), ]

    if (is.null(input$yrange_asmus)) {
      y_lim <- input$YRange
    } else {
      y_lim <- input$yrange_asmus
    }

    if (is.null(input$plot_type_asmus)) {
      pl_typ <- "lin"
    } else {
      pl_typ <- input$plot_type_asmus
    }

    plot(
      all_points$x,
      all_points$y,
      xlab = "",
      ylab = "",
      ylim = y_lim,
      log = ifelse(pl_typ == "log", "y", ""),
      cex.axis = 1.5,
      cex.lab = 1.5,
      type = "n",
      axes = FALSE
    )
    rect(
      xleft = grconvertX(0,'ndc','user') - ifelse(pl_typ == "lin", 1000, 0),
      xright = grconvertX(1,'ndc','user') + ifelse(pl_typ == "lin", 1000, 0),
      ybottom = grconvertY(0,'ndc','user') - ifelse(pl_typ == "lin", 1000, 0),
      ytop = grconvertY(1,'ndc','user') + ifelse(pl_typ == "lin", 1000, 0),
      border = NA,
      col = colthemeCol$ColorBGplot,
      xpd = TRUE
    )
    if (ifelse(pl_typ == "log", "y", "") == "y") {
      miniy <- 10^par("usr")[3]
      maxiy <- 10^par("usr")[4]
      lowy <- 10^(par("usr")[3] + (par("usr")[4] - par("usr")[3])/40)
      lowyp <- 10^(par("usr")[3] + (par("usr")[4] - par("usr")[3])/15)
      minplustinyy <- 10^(par("usr")[3] + (par("usr")[4] -
                                             par("usr")[3])/1400)
    } else {
      miniy <- par("usr")[3]
      maxiy <- par("usr")[4]
      lowy <- miniy + (maxiy - miniy)/40
      lowyp <- miniy + (maxiy - miniy)/15
      minplustinyy <- miniy + (maxiy - miniy)/1400
    }

    minix <- roundDownNice(par("usr")[1])
    maxix <- roundUpNice(par("usr")[2])

    nr <- 7
    stepx <- roundUpNice((maxix - minix)/(nr +  1))
    if (minix < stepx)
      minix <- 0
    stripesx <- 0:(nr + 1)
    stripesx <- lapply(stripesx, function(x) x * stepx)
    stripesx <- lapply(stripesx, function(x) x + minix)
    stripesxp <- lapply(stripesx, function(x) paste(floor(x/scresults$results_total[,c(input$x)] * 100), "%"))

    for (i in seq(1, nr, 2)) rect(stripesx[i],miniy, stripesx[i + 1], maxiy, col = different_hues(colthemeCol$col.bg), border = NA)


    if (input$xlabel == TRUE) {
      text(stripesx, lowy, stripesx, cex = 1.2, col = font_color(colthemeCol$col.bg))
      text(stripesx, lowyp, stripesxp, cex = 1.2, col = font_color(colthemeCol$col.bg))
    }

    box(col = font_color(colthemeCol$col.bg))

    axis(
      2,
      col = font_color(colthemeCol$col.bg),
      col.ticks = font_color(colthemeCol$col.bg),
      col.axis = font_color(colthemeCol$col.bg),
      cex.axis = 1
    )

    title(
      main = SG_tit3(),
      line = -2,
      col = "#8b8b8b",
      col.main = font_color(colthemeCol$col.bg)
    )

    pch_ <- ifelse(input$pch_value == "19", 19, input$pch_value)


    if (input$circlestyle == "standard") {
      points(
        white_points$x,
        white_points$y,
        pch = pch_,
        cex = input$pointsize,
        col = white_points$color
      )
      points(
        colored_points$x,
        colored_points$y,
        pch = pch_,
        cex = input$pointsize,
        col = colored_points$color
      )
    }

    if (input$circlestyle == "groupsize") {
      points(
        white_points$x,
        white_points$y,
        pch = pch_,
        cex = input$pointsize * sqrt(scresults$sge[scresults$sge$SGID %in% white_points$ID, 'N.of.subjects'][scresults$sge$nfactors >= input$keys_asmus[1] & scresults$sge$nfactors <= input$keys_asmus[2]]/pi),
        col = white_points$color
      )
      points(
        colored_points$x,
        colored_points$y,
        pch = pch_,
        cex = input$pointsize * sqrt(scresults$sge[scresults$sge$SGID %in% colored_points$ID , 'N.of.subjects'][scresults$sge$nfactors >= input$keys_asmus[1] & scresults$sge$nfactors <= input$keys_asmus[2]]/pi),
        col = colored_points$color
      )
    }

    abline(
      h = ref_line(),
      lwd = 3,
      col = colthemeCol$ColorReference
    )
    text(
      x = grconvertX(0.97, from = 'nfc', to = 'user'),
      y = ref_line() + diff(input$YRange)/50,
      col = colthemeCol$ColorReference
    )

    if (input$grid == TRUE) {
      abline(h = axTicks(2), lty = 2, col = font_color(colthemeCol$col.bg), lwd = 0.3)
      abline(v = axTicks(1), lty = 2, col = font_color(colthemeCol$col.bg), lwd = 0.3)
    }
  })
  }

  output$hover_info1 <- shiny::renderUI({
    shiny::req(input$plot_hover1, plot_points_data())
    input$plot_hover1

    plot_point <- plot_points_data()

    all_points <- cbind(plot_point, color, stringsAsFactors = FALSE)
    colored_points <- all_points[!startsWith(all_points$color, colthemeCol$ColorPoints),  ]
    hover <- input$plot_hover1
    hover$mapping <- list(xintercept = "xintercept", x = "x", y = "y")

    tmp1 <- plot_points_data_complement()
    if (!is.null(tmp1)) {
      if (dim(tmp1)[1] > 0) {
        tmp1$color <- "#fffb00"
        tmp1$ID <- NA
        colored_points <- rbind(colored_points, tmp1)
      }
    }


    point <- nearPoints(colored_points, hover)
    if (nrow(point) == 0) return(NULL)

    left_pct <- (hover$coords_img$x - hover$range$left) / (hover$range$right - hover$range$left)
    top_pct <- (hover$domain$top - ifelse(input$plot_type == "lin", hover$y, log10(hover$y))) / (hover$domain$top - hover$domain$bottom)

    left_px <- ifelse(left_pct <= 0.75,
                      20 + hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x,
                      - 175 + hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x)

    top_px <- ifelse(top_pct <= 0.5,
                     20 + hover$range$top + top_pct * (hover$range$bottom - hover$range$top),
                     - 115 + hover$range$top + top_pct * (hover$range$bottom - hover$range$top))
    style <- paste0("position:absolute; z-index:100;background-color: rgba(",col2rgb(point$color)[1],",",col2rgb(point$color)[2],",",col2rgb(point$color)[3],",0.85); ",
                    "left:", left_px, "px; top:", top_px, "px; border: 0px;")
    point <- point[1,]

    tmp1 <- colnames(scresults$sge[which(scresults$sge$SGID == point$ID), scresults$factors])[which(scresults$sge[which(scresults$sge$SGID == point$ID), scresults$factors] != "Not used")]

    tmp2 <- scresults$sge %>%
      dplyr::filter(SGID %in% point$ID) %>%
      dplyr::select(colnames(scresults$sge[which(scresults$sge$SGID == point$ID), scresults$factors])[which(scresults$sge[which(scresults$sge$SGID == point$ID), scresults$factors] != "Not used")])

    tmp2 <- data.frame(lapply(tmp2, as.character), stringsAsFactors = FALSE)

    shiny::wellPanel(
      style = style,
     shiny::p(
        shiny::HTML(
          ifelse(length(tmp1)>0,
          paste0(
            "<b style = 'color: ",
            font_color(point$color),
            "'> ",
            input$x,
            ": ",
            point$x,
            "</br>",
            "<b style = 'color: ",
            font_color(point$color),
            "'> ",
            input$y,
            ": ",
            point$y,
            "</br>",
            "<b style = 'color: ",
            font_color(point$color),
            "'> Factors(",
            length(tmp1),
            "): ",
            paste(
              paste0(
                tmp1," = ", tmp2
              ), collapse = ", "
            ),
            "</br>"
          ),
          paste0(
            "<b style = 'color: ",
            font_color(point$color),
            "'> ",
            input$x,
            ": ",
            point$x,
            "</br>",
            "<b style = 'color: ",
            font_color(point$color),
            "'> ",
            input$y,
            ": ",
            point$y
          )
          )
        )
      )
    )
  })

  output$hover_info2 <- shiny::renderUI({
    shiny::req(input$plot_hover2, plot_points_data())
    input$plot_hover2

    plot_point <- plot_points_data2()

    all_points <- cbind(plot_point, color, stringsAsFactors = FALSE)

    colored_points <- all_points[!startsWith(all_points$color, colthemeCol$ColorPoints), ]

    hover <- input$plot_hover2
    hover$mapping <- list(xintercept = "xintercept", x = "x", y = "y")

    tmp1 <- plot_points_data_complement()
    if(!is.null(tmp1)) {
      if (dim(tmp1)[1] > 0) {
        tmp1$color <- "#fffb00"
        tmp1$ID <- NA
        colored_points <- rbind(colored_points, tmp1)
      }
    }
    point <- nearPoints(colored_points, hover)

    if (nrow(point) == 0) return(NULL)

    left_pct <- (hover$coords_img$x - hover$range$left) / (hover$range$right - hover$range$left)


    top_pct <- (hover$domain$top - ifelse(input$plot_type == "lin", hover$y, log10(hover$y))) / (hover$domain$top - hover$domain$bottom)

    left_px <- ifelse(left_pct <= 0.75,
                      20 + hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x,
                      - 175 + hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x)

    top_px <- ifelse(top_pct <= 0.5,
                     20 + hover$range$top + top_pct * (hover$range$bottom - hover$range$top),
                     - 115 + hover$range$top + top_pct * (hover$range$bottom - hover$range$top))

    style <- paste0(
      "position:absolute; z-index:100;background-color: rgba(",
      col2rgb(point$color)[1],
      ",",
      col2rgb(point$color)[2],
      ",",
      col2rgb(point$color)[3],
      ",0.85); ",
      "left:",
      left_px,
      "px; top:",
      top_px,
      "px; border: 0px;"
    )
    point <- point[1,]
    tmp1 <- colnames(scresults$sge[point$ID, scresults$factors])[which(scresults$sge[point$ID, scresults$factors] != "Not used")]

    tmp2 <- scresults$sge %>%
      dplyr::filter(SGID %in% point$ID) %>%
      dplyr::select(colnames(scresults$sge[point$ID, scresults$factors])[which(scresults$sge[point$ID, scresults$factors] != "Not used")])

    tmp2 <- data.frame(lapply(tmp2, as.character), stringsAsFactors = FALSE)
    shiny::wellPanel(
      style = style,
     shiny::p(shiny::HTML(
        ifelse(length(tmp1)>0,
               paste0(
                 "<b style = 'color: ",
                 font_color(point$color),
                 "'> ",
                 input$x,
                 ": ",
                 point$x,
                 "</br>",
                 "<b style = 'color: ",
                 font_color(point$color),
                 "'> ",
                 input$y,
                 ": ",
                 point$y,
                 "</br>",
                 "<b style = 'color: ",
                 font_color(point$color),
                 "'> Factors(",
                 length(tmp1),
                 "): ",
                 paste(
                   paste0(
                     tmp1," = ", tmp2
                   ), collapse = ", "
                 ),
                 "</br>"
               ),
               paste0(
                 "<b style = 'color: ",
                 font_color(point$color),
                 "'> ",
                 input$x,
                 ": ",
                 point$x,
                 "</br>",
                 "<b style = 'color: ",
                 font_color(point$color),
                 "'> ",
                 input$y,
                 ": ",
                 point$y
               )
        )
        ))
    )
  })

  output$hover_info3 <- shiny::renderUI({
   shiny::req(input$plot_hover3, plot_points_data())
    input$plot_hover3

    plot_point <- plot_points_data3()

    all_points <- cbind(plot_point, color, stringsAsFactors = FALSE)

    colored_points <- all_points[!startsWith(all_points$color, colthemeCol$ColorPoints),]

    hover <- input$plot_hover3
    hover$mapping <- list(xintercept = "xintercept", x = "x", y = "y")

    tmp1 <- plot_points_data_complement()
    if(!is.null(tmp1)) {
      if (dim(tmp1)[1] > 0) {
        tmp1$color <- "#fffb00"
        tmp1$ID <- NA
        colored_points <- rbind(colored_points, tmp1)
      }
    }
    point <- nearPoints(colored_points, hover)

    if (nrow(point) == 0) return(NULL)

    left_pct <- (hover$coords_img$x - hover$range$left) / (hover$range$right - hover$range$left)

    top_pct <- (hover$domain$top - ifelse(input$plot_type == "lin", hover$y, log10(hover$y))) / (hover$domain$top - hover$domain$bottom)

    left_px <- ifelse(left_pct <= 0.75,
                      20 + hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x,
                      - 175 + hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x)

    top_px <- ifelse(top_pct <= 0.5,
                     20 + hover$range$top + top_pct * (hover$range$bottom - hover$range$top),
                     - 115 + hover$range$top + top_pct * (hover$range$bottom - hover$range$top))
    style <- paste0(
      "position:absolute; z-index:100;background-color: rgba(",
      col2rgb(point$color)[1],
      ",",
      col2rgb(point$color)[2],
      ",",
      col2rgb(point$color)[3],
      ",0.85); ",
      "left:",
      left_px,
      "px; top:",
      top_px,
      "px; border: 0px;"
    )
    point <- point[1,]
    tmp1 <- colnames(scresults$sge[point$ID, scresults$factors])[which(scresults$sge[point$ID, scresults$factors] != "Not used")]

    tmp2 <- scresults$sge %>%
      dplyr::filter(SGID %in% point$ID) %>%
      dplyr::select(colnames(scresults$sge[point$ID, scresults$factors])[which(scresults$sge[point$ID, scresults$factors] != "Not used")])

    tmp2 <- data.frame(lapply(tmp2, as.character), stringsAsFactors = FALSE)
    shiny::wellPanel(
      style = style,
      shiny::p(shiny::HTML(
        ifelse(length(tmp1)>0,
             paste0(
               "<b style = 'color: ",
               font_color(point$color),
               "'> ",
               input$x,
               ": ",
               point$x,
               "</br>",
               "<b style = 'color: ",
               font_color(point$color),
               "'> ",
               input$y,
               ": ",
               point$y,
               "</br>",
               "<b style = 'color: ",
               font_color(point$color),
               "'> Factors(",
               length(tmp1),
               "): ",
               paste(
                 paste0(
                   tmp1," = ", tmp2
                 ), collapse = ", "
               ),
               "</br>"
             ),
             paste0(
               "<b style = 'color: ",
               font_color(point$color),
               "'> ",
               input$x,
               ": ",
               point$x,
               "</br>",
               "<b style = 'color: ",
               font_color(point$color),
               "'> ",
               input$y,
               ": ",
               point$y
             )
          )
        )
      )
    )
  })

  output$hover_info4 <- shiny::renderUI({
   shiny::req(input$plot_hover4, plot_points_data())
    input$plot_hover4

    plot_point <- plot_points_data4()

    all_points <- cbind(plot_point, color, stringsAsFactors = FALSE)

    colored_points <- all_points[!startsWith(all_points$color, colthemeCol$ColorPoints), ]

    hover <- input$plot_hover4
    hover$mapping <- list(xintercept = "xintercept", x = "x", y = "y")

    tmp1 <- plot_points_data_complement()
    if(!is.null(tmp1)) {
      if (dim(tmp1)[1] > 0) {
        tmp1$color <- "#fffb00"
        tmp1$ID <- NA
        colored_points <- rbind(colored_points, tmp1)
      }
    }
    point <- nearPoints(colored_points, hover)

    if (nrow(point) == 0) return(NULL)

    left_pct <- (hover$coords_img$x - hover$range$left) / (hover$range$right - hover$range$left)
    top_pct <- (hover$domain$top - ifelse(input$plot_type3 == "lin", hover$y, log10(hover$y))) / (hover$domain$top - hover$domain$bottom)

    left_px <- ifelse(left_pct <= 0.75,
                      20 + hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x,
                      - 175 + hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x)

    top_px <- ifelse(top_pct <= 0.5,
                     20 + hover$range$top + top_pct * (hover$range$bottom - hover$range$top),
                     - 115 + hover$range$top + top_pct * (hover$range$bottom - hover$range$top))
    style <- paste0("position:absolute; z-index:100;background-color: rgba(",col2rgb(point$color)[1],",",col2rgb(point$color)[2],",",col2rgb(point$color)[3],",0.85); ",
                    "left:", left_px, "px; top:", top_px, "px; border: 0px;")
    point <- point[1,]

    tmp1 <- colnames(scresults$sge[which(scresults$sge$SGID == point$ID), scresults$factors])[which(scresults$sge[which(scresults$sge$SGID == point$ID), scresults$factors] != "Not used")]

    tmp2 <- scresults$sge %>%
      dplyr::filter(SGID %in% point$ID) %>%
      dplyr::select(colnames(scresults$sge[which(scresults$sge$SGID == point$ID), scresults$factors])[which(scresults$sge[which(scresults$sge$SGID == point$ID), scresults$factors] != "Not used")])

    tmp2 <- data.frame(lapply(tmp2, as.character), stringsAsFactors = FALSE)

    shiny::wellPanel(
      style = style,
      shiny::p(shiny::HTML(paste0("<b style = 'color: ", font_color(point$color),"'> ", input$y1,": ", point$x, "</br>",
                    "<b style = 'color: ", font_color(point$color),"'> ", input$y2,": ", point$y, "</br>",
                    "<b style = 'color: ", font_color(point$color),"'> Factors(",
                    length(tmp1),
                    "): ", paste(paste0(tmp1," = ", tmp2), collapse = ", "), "</br>"))
      )
    )
  })


  output$hover_info5 <- shiny::renderUI({
    shiny::req(input$plot_hover5, plot_points_data())
    input$plot_hover5

    plot_point <- plot_points_data5()

    all_points <- cbind(plot_point, color2, stringsAsFactors = FALSE)

    colored_points <- all_points[!startsWith(all_points$color, colthemeCol$ColorPoints),]

    hover <- input$plot_hover5
    hover$mapping <- list(xintercept = "xintercept", x = "x", y = "y")

    point <- nearPoints(colored_points, hover)

    if (nrow(point) == 0) return(NULL)

    left_pct <- (hover$coords_img$x - hover$range$left) / (hover$range$right - hover$range$left)

    top_pct <- (hover$domain$top - ifelse(input$plot_type_asmus == "log",log10(hover$y), hover$y)) / (hover$domain$top - hover$domain$bottom)
    left_px <- ifelse(left_pct <= 0.75,
                      20 + hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x,
                      - 175 + hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x)

    top_px <- ifelse(top_pct <= 0.5,
                     20 + hover$range$top + top_pct * (hover$range$bottom - hover$range$top),
                     - 115 + hover$range$top + top_pct * (hover$range$bottom - hover$range$top))
    style <- paste0("position:absolute; z-index:100; background-color: rgba(", col2rgb(point$color)[1],",", col2rgb(point$color)[2],",",col2rgb(point$color)[3],",0.85); ",
                    "left:", left_px, "px; top:", top_px, "px; border: 0px;")
    point <- point[1,]


    tmp1 <- colnames(scresults$sge[which(scresults$sge$SGID == point$ID), scresults$factors])[which(scresults$sge[which(scresults$sge$SGID == point$ID), scresults$factors] != "Not used")]

    tmp2 <- scresults$sge %>%
      dplyr::filter(SGID %in% point$ID) %>%
      dplyr::select(colnames(scresults$sge[which(scresults$sge$SGID == point$ID), scresults$factors])[which(scresults$sge[which(scresults$sge$SGID == point$ID), scresults$factors] != "Not used")])
    tmp2 <- data.frame(lapply(tmp2,as.character), stringsAsFactors = FALSE)
    wellPanel(
      style = style,
      shiny::p(shiny::HTML(paste0("<b style = 'color: ", font_color(point$color),"'> ", input$x,": ", point$x, "</br>",
                    "<b style = 'color: ", font_color(point$color),"'> ", input$y,": ", point$y, "</br>",
                    "<b style = 'color: ", font_color(point$color),"'> Factors(",
                    length(tmp1),
                    "): ", paste(paste0(tmp1," = ", tmp2), collapse = ", "), "</br>")))
    )
  })

  context_ids <- shiny::reactive({
    tmp <- rbind(scresults$sge[scresults$sge$nfactors %in% c(1, 2, 3, 4) & scresults$sge$FCID_incomplete == "Complete",],
                 scresults$sge[scresults$sge$nfactors %in% c(2, 3, 4) & scresults$sge$FCID_pseudo != "No Pseudo",]
    )
    list('SGID' = tmp$SGID, 'FCID' = unique(tmp$FCID_all))
  })


  screening_ids <- shiny::reactive({
    shiny::req(context_ids())
    if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(scresults$sge))) {
    ind <- sorting_index()
    }
    tmp <- context_ids()$SGID
    if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(scresults$sge))) {
    tmp <- tmp[rank(ind)]
    }
    tmp
  })
  if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(scresults$sge))) {
  screening_index <- shiny::reactiveValues(val = 0)

  screening_index_new <- shiny::reactiveValues(val = NULL)

  shiny::observeEvent(c(screening_index$val,input$direction), {
   shiny::req(sorting_index())
    sort_ind <- sorting_index()
    screening_index_new$val <- sort_ind[screening_index$val]
  })

  shiny::observeEvent(input$screening_backward, {
    if (screening_index$val > 1) {
      screening_index$val <- screening_index$val - 1
    }
  })

  shiny::observeEvent(c(input$screening_forward), {
   shiny::req(sorting_index())
    sort_ind <- sorting_index()
    if(screening_index$val < length(sort_ind)) {
      screening_index$val <- screening_index$val + 1
    }
  })

  shiny::observeEvent(c(input$screening_forward, input$screening_backward, input$goto_button), {
    if (input$navpanel == "subscreenasmus") {
      setcolor2()
    } else if (input$navpanel %in% c("1", "2")) {

    }
  })
  }
  #### MODULE ####
  if (all(c("FCID_all", "FCID_complete", "FCID_incomplete", "FCID_pseudo") %in% colnames(scresults$sge))) {
  Module_input <- shiny::reactiveValues(
    dat = matrix(
      c(rep("N/A",
          2 * length(rbind(scresults$sge[scresults$sge$nfactors %in% c(1, 2, 3, 4) & scresults$sge$FCID_incomplete == "Complete",],
          scresults$sge[scresults$sge$nfactors %in% c(2, 3, 4) & scresults$sge$FCID_pseudo != "No Pseudo",])$SGID)
      ),rbind(scresults$sge[scresults$sge$nfactors %in% c(1, 2, 3, 4) & scresults$sge$FCID_incomplete == "Complete",],
              scresults$sge[scresults$sge$nfactors %in% c(2, 3, 4) & scresults$sge$FCID_pseudo != "No Pseudo",])$SGID),
      length(rbind(scresults$sge[scresults$sge$nfactors %in% c(1, 2, 3, 4) & scresults$sge$FCID_incomplete == "Complete",],
                   scresults$sge[scresults$sge$nfactors %in% c(2, 3, 4) & scresults$sge$FCID_pseudo != "No Pseudo",])$SGID
      ),
      3,
      dimnames = list(
        paste0(
          "Subgroup ID: ",
          rbind(scresults$sge[scresults$sge$nfactors %in% c(1, 2, 3, 4) & scresults$sge$FCID_incomplete == "Complete",],
          scresults$sge[scresults$sge$nfactors %in% c(2, 3, 4) & scresults$sge$FCID_pseudo != "No Pseudo",])$SGID
        ),
        c("Is the Subgroup size big enough?",
          "Is the effect remarkable?",
          "Subgroup_ID"
        )
      )
    )
  )

  Module_input2 <- shiny::reactive({
    Module_input <- Module_input$dat[order(as.numeric(Module_input$dat[,3])),]
    Module_input[rank(sorting_index()),]
  })

  shiny::observeEvent(call_Mod()$size(), {
    if (!is.null(call_Mod()$size())) {
      Module_input$dat[which(Module_input$dat[, 3] == as.character(screening_index_new$val)) , 1] <- call_Mod()$size()
    }
  }, ignoreNULL = TRUE
  )

  shiny::observeEvent(call_Mod()$satis(), {
    if (!is.null(call_Mod()$satis())) {
      Module_input$dat[which(Module_input$dat[, 3] == as.character(screening_index_new$val)), 2] <- call_Mod()$satis()
    }
  }, ignoreNULL = TRUE
  )

  call_Mod <- shiny::reactive({
    Val <- shiny::callModule(screeningModule_Server,
                             id = as.character(screening_index_new$val),
                             label = screening_index_new$val,
                             module_input = Module_input2()
    )
    Val
  })

  shiny::observe({(call_Mod())})
  }



  ref_line <- shiny::reactive({scresults$results_total[, c(input$y)]})
}


SGEApp <- shiny::shinyApp(ui = ui, server = server)
