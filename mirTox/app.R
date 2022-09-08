library(BiocManager)
options(repos = BiocManager::repositories())
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(shinycustomloader) 
#library(shinyalert)
library(bs4Dash)
library(fresh)
library(DT)
library(dplyr)
library(ggplot2)
library(htmlwidgets)
library(corrplot)
#library(visNetwork)
#library(gridExtra)
#library(EnvStats)
library(paletteer)
library(plotly)
library(EnhancedVolcano)
library(upsetjs)
library(tippy)
library(fmsb)
library(gt)
library(matrixStats)
library(pheatmap)
library(miRNetR)
library(clusterProfiler)
library(org.Hs.eg.db)
library(ReactomePA)
library(DOSE)
library(viridis)
library(ds4psy)
library(unikn)

set.seed(99)

# load data used in app
counts = readRDS("./data/counts_new.rds") # this is the normalized counts by DESeq2
DEG = readRDS("./data/DEG_list.rds") # this is a list of the DEG for the 9 treatments.



# create a theme for the app
mytheme <- create_theme(
  bs4dash_vars(
    navbar_light_color = "#bec5cb",
    navbar_light_active_color = "#17a2b8",
    navbar_light_hover_color = "#FFF"
  ),
  bs4dash_yiq(
    contrasted_threshold = 10,
    text_dark = "#FFF", 
    text_light = "#272c30"
  ),
  bs4dash_layout(
    main_bg = "#353c42"
  ),
  bs4dash_sidebar_light(
    bg = "#272c30", 
    color = "#bec5cb",
    hover_color = "#FFF",
    submenu_bg = "#272c30", 
    submenu_color = "#FFF", 
    submenu_hover_color = "#FFF"
  ),
  bs4dash_status(
    primary = "#5E81AC", danger = "#BF616A", light = "#272c30", success = '#55AD89', info = '#17a2b8'
  ),
  bs4dash_color(
    gray_900 = "#FFF"
  )
)

# to add color to the spinner 
options(spinner.color="#ffffff")

### SIDEBAR #################################


sidebar <- bs4DashSidebar(skin = "light", 
                          bs4SidebarMenu(id = "sidebar", # id important for updateTabItems,
                                                      bs4SidebarMenuItem("Introduction", tabName = "Intro", icon = icon("info")),
                                                      bs4SidebarMenuItem("miRNA expression", tabName = "mirs", icon = icon("monero"))
                                                      #bs4SidebarMenuItem("mRNA expression", tabName = "mrna", icon = icon("openid")),
                                                      #bs4SidebarMenuItem("Correlation with RNA-seq", tabName = "plot", icon = icon("braille")),
                                                      #bs4SidebarMenuItem("Pathway analysis", tabName = "pathways", icon = icon("connectdevelop"))
))


### BODY #################################


body <- bs4DashBody(
  fresh::use_theme(mytheme),
  useShinyjs(),
  tags$head(
    tags$script(
      "$(function() {
            function resizeBoxContent(trigger, target) {
              $(trigger).on('click', function() {
                setTimeout(function() {
                  var isMaximized = $('html').hasClass('maximized-card');
                    if (isMaximized) {
                      $(target).css('height', '100%');
                    } else {
                      $(target).css('height', '600px');
                    }
                }, 300);
                $(target).trigger('resize');
              });
            }
            
            setTimeout(function() {
              resizeBoxContent('#count_box [data-card-widget=\"maximize\"]', '#plot1');
              resizeBoxContent('#vol_box [data-card-widget=\"maximize\"]', '#volcano');
            }, 500);
            
          });
          "
    )
  ),

  bs4TabItems(
    
    #### INTRO TAB #######
    bs4TabItem("Intro",
               fluidPage(
                 h2("MicroRNAs in Osteogenic Differentiation"),
                 h4("Objective"),
                 p("Skeletal birth defects are among the most common congenital anomalies. These defects often arise from both environmental and genetic factors but are often attributed to misregulation in gene expression. Identifying biomarkers that can be assessed prenatally can aid in detection and possibly used as targets for therapies. MicroRNAs (miRNAs) are one such biomarker that could be predictive of environmental toxicity exposure. By treating human embryonic stem cells with toxicants known to interfere with early osteogenesis we aim to study the dysregulated repertoire of miRNAs and identify subsets that are involved with bone developmental pathways."),
                 h4("Approach"),
                 p("Human embryonic stem cells (hESCs - H9) are induced to osteogenic differentiation and simultaneously treated with the toxicants in the below table, known to inhibit neural crest differentiation leading to craniofacial defects or mesoderm differentiation (limb skeleton)."),
                 gt_output("toxtable") %>% withSpinner(type = 6, size=1),
                 br(),
                 p("This app will visualize the analysis of both miRNA-sequencing and mRNA-sequencing individually to explore the differentially regulated miRNAs and genes in the treatments. Then both datasets are integrated to display any inverse correlation of miRNA to its known or predicted mRNA target. The mRNA hits can be used in pathway analysis to identify involvement in osteogenic differentiation."),
                 h4("Samples"),
                 p("The principal component analysis (PCA) for each sequencing project is displayed below."),
                 fluidRow(column(6,
                                 img(height = "100%", width="100%",style="display: block; margin-left: auto; margin-right: auto;",src="PCA_miRNAs.png")),
                          column(6,
                                 img(height = "100%", width="100%",style="display: block; margin-left: auto; margin-right: auto;",src="PCA_mRNA.png")))
               )), 

    
    #### DIFF TAB #######
    bs4TabItem("mirs",
            fluidPage(
              h3("microRNA Expression"),
              br(),
              fluidRow(column(4,
                              pickerInput("mir", label = "Choice of miRNA", choices= sort(unique(counts$Geneid)), multiple = F, options = list(style = "btn-light", `live-search` = TRUE, `actions-box` = TRUE)))
              ),
              fluidRow(column(6,
                              box(id = "count_box", title = "Counts Plot", width = 12, collapsible = TRUE, collapsed = FALSE, maximizable = T, status = "primary", solidHeader = TRUE, background = "gray-dark",
                                  plotlyOutput("plot1"))),
                       column(6,
                              box(id="count_tab", title = "Normalized Counts", width = 12, collapsible = TRUE, collapsed = FALSE, maximizable = T, status = "primary", solidHeader = TRUE, background = "white",
                                  DT::dataTableOutput("count_table") %>% withSpinner(type =6 , size=1, color = "#343a40")))),
              fluidRow(column(6,
                              box(id = "vol_box", title = "Volcano Plot", width = 12, collapsible = TRUE, collapsed = FALSE, maximizable = T, status = "primary", solidHeader = TRUE, background = "gray-dark",
                                  label = boxLabel(text = "?", status = "danger") %>% tippy(tooltip = "By default the miRNA chosen above will be labeled in the volcano plot.", interactive = TRUE, placement = "bottom"),
                                  sidebar = boxSidebar(id = "box2side", 
                                                       sliderTextInput("FC", "Log2Fold-Change (absolute value)", choices = seq(from= 0, to= 4, by=0.5), grid = TRUE, selected = 1),
                                                       pickerInput("FDR", "False Discovery Rate", choices = c(0.1, 0.05, 0.01), selected = 0.05),
                                                       radioGroupButtons("all", "Label Points", choices = c("Default", "All Points", "Selected microRNAs"), selected = "Default",  individual = TRUE, checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"), no = tags$i(class = "fa fa-circle-o", style = "color: steelblue"))),
                                                       shinyjs::hidden(div(id="list", pickerInput("mir2", label = "Choice of miRNA", choices= "", multiple = T, options = list(style = "btn-light", `live-search` = TRUE, `actions-box` = TRUE))))
                                                       
                                  ),
                                  pickerInput("tox", label = "Choice of toxicant", choices = names(DEG), selected = "Cyclopamine", multiple = F, options = list(style = "btn-light", `live-search` = TRUE)),
                                  plotOutput("volcano"),
                                  downloadButton("save", "Download Plot", icon=icon("download", lib = "font-awesome")))),
                       column(6,
                              box(id = "table1", title = "Table of DEGs in volcano plot", width = 12, collapsible = TRUE, collapsed = FALSE, maximizable = T, status = "primary", solidHeader = TRUE, background = "white", 
                                  DT::dataTableOutput("voltable") %>% withSpinner(type =6 , size=1, color = "#343a40"))
                              )),
              fluidRow(column(6,
                              box(id = "upbox", title = "Overlapping MicroRNAs", width = 12, collapsible = TRUE, collapsed = FALSE, maximizable = T, status = "primary", solidHeader = TRUE, background = "white",
                                  label = boxLabel(text = "?", status = "danger") %>% tippy(tooltip = "Select a column in the plot to display the names of the overlapping miRNAs. These miRNAs will be displayed in the adjacent table.", interactive = TRUE, placement = "bottom"),
                                  sidebar = boxSidebar(id = "box3side", 
                                                       sliderTextInput("FC_2", "Log2Fold-Change (absolute value)", choices = seq(from= 0, to= 4, by=0.5), grid = TRUE, selected = 1),
                                                       pickerInput("FDR_2", "False Discovery Rate", choices = c(0.1, 0.05, 0.01), selected = 0.05),
                                                       radioGroupButtons("reg", "Choose:", choices = c("Up-regulated", "Down-regulated"), individual = TRUE, checkIcon = list(
                                                         yes = tags$i(class = "fa fa-circle", 
                                                                      style = "color: teal"),
                                                         no = tags$i(class = "fa fa-circle-o", 
                                                                     style = "color: teal")))
                                  ),
                                  fluidRow(
                                    column(3, pickerInput("combo", label = HTML('<h6 style="color:black;">Choice of Toxicants</h6>'), choices = c("Cyclopamine","Methoxyacetic acid", "Ogremorphin", "Triademenol","Cyclophosphamide" ,"Methotrexate","Valproic acid","5-Flurouracil" ,"Hydrogen Peroxide"), multiple = T, options = list(style = "btn-light", `actions-box` = TRUE))),
                                    column(3, actionBttn("go", "Plot", style = "jelly", color="danger"))
                                  ),
                                  upsetjsOutput("upset1", width = "100%", height = "600px") %>% withSpinner(type = 6, size=1, color = "#343a40"),
                                  fluidRow(
                                    column(2, "Intersections"),
                                    column(3, span(textOutput("clicked"), style ="color:black")),
                                    column(7, span(textOutput("clickedElements"), style ="color:black"))
                                  ))),
                       column(6,
                              box(id = "table3", title = "Overlapping microRNAs", width = 12, collapsible = TRUE, collapsed = FALSE, maximizable = T, status = "primary", solidHeader = TRUE, background = "white",
                                  DT::dataTableOutput("upsettable") %>% withSpinner(type =6 , size=1, color = "#343a40")))
                       ),
              fluidRow(column(12,
                              box(id="heatbox1", title = "Heatmaps", width = 12, height = "1000px", collapsible = TRUE, collapsed = FALSE, maximizable = T, status = "primary", solidHeader = TRUE, background = "gray-dark",
                                  label = boxLabel(text = "?", status = "danger") %>% tippy(tooltip = "Maximize box to view plot. <br>DESeq2's normalized counts are used to plot the heatmaps.", interactive = TRUE, placement = "top", allowHTML = TRUE, arrow = TRUE),
                                  tabBox(id = "tabbox", width = 12, type = "pills", status = "gray-dark", solidHeader = T,  collapsible = F, background = "gray-dark",
                                         tabPanel("All Counts",
                                                  plotOutput("heatmap1", width = "100%", height = "800px") %>% withSpinner(type = 6, size=1, color = "#343a40"),
                                                  downloadButton("downheat1", "Download Plot", icon=icon("download", lib = "font-awesome"))),
                                         tabPanel("Averaged Counts",
                                                  plotOutput("heatmap2", width = "100%", height = "800px") %>% withSpinner(type = 6, size=1, color = "#343a40"),
                                                  downloadButton("downheat2", "Download Plot", icon=icon("download", lib = "font-awesome"))),
                                         tabPanel("Selected miRNAs",
                                                  fluidRow(column(6,
                                                                  pickerInput("mirs_choice", label = "Choice of microRNA", choices = unique(counts$Geneid), multiple = T, options = list(style = "btn-light", `live-search` = TRUE, `actions-box` = TRUE))),
                                                           column(3,
                                                                  actionBttn("go2", "Plot", style = "jelly", color="danger"))),
                                                  plotOutput("heatmap3", width = "100%", height = "700px") %>% withSpinner(type = 6, size=1, color = "#343a40"),
                                                  downloadButton("downheat3", "Download Plot", icon=icon("download", lib = "font-awesome"))))
                                  ))),
              fluidRow(column(12,
                              box(id = "radarbox", title = "Radar Plots", width = 12, height = "1000px", collapsible = TRUE, collapsed = FALSE, maximizable = T, status = "primary", solidHeader = TRUE, background = "gray-dark",
                                  tabBox(id = "tabbox2", width = 12, type = "pills", status = "gray-dark", solidHeader = T,  collapsible = F, background = "gray-dark",
                                         tabPanel("Toxicants",
                                                  fluidRow(column(4,
                                                                  pickerInput("mirs_choice2", label = "Choice of microRNA", choices = unique(counts$Geneid), multiple = T, options = list(style = "btn-light", `live-search` = TRUE, `actions-box` = TRUE))),
                                                           column(4,
                                                                  pickerInput("combo2", label = "Choice of toxicant", choices = c("Cyclopamine","Methoxyacetic acid", "Ogremorphin", "Triademenol","Cyclophosphamide" ,"Methotrexate","Valproic acid","5-Flurouracil" ,"Hydrogen Peroxide"), multiple = T, options = list(style = "btn-light", `actions-box` = TRUE))),
                                                           column(3,
                                                                  actionBttn("go3", "Plot", style = "jelly", color="danger"))),
                                                  plotOutput("radar1", width = "100%", height = "800px") %>% withSpinner(type = 6),
                                                  downloadButton("downrad1", "Download Plot", icon=icon("download", lib = "font-awesome"))),
                                         tabPanel("MiRs",
                                                  fluidRow(column(4,
                                                                  pickerInput("mirs_choice3", label = "Choice of microRNA", choices = unique(counts$Geneid), multiple = T, options = list("maxOptions" = 10, "maxOptionsText" = "Too many miRNAs, choose less.", "style" = "btn-light", `live-search` = TRUE, `actions-box` = TRUE))),
                                                           column(4,
                                                                  pickerInput("combo3", label = "Choice of toxicant", choices = c("Cyclopamine","Methoxyacetic acid", "Ogremorphin", "Triademenol","Cyclophosphamide" ,"Methotrexate","Valproic acid","5-Flurouracil" ,"Hydrogen Peroxide"), multiple = T, options = list(style = "btn-light", `actions-box` = TRUE))),
                                                           column(3,
                                                                  actionBttn("go4", "Plot", style = "jelly", color="danger"))),
                                                  plotOutput("radar2", width = "100%", height = "800px") %>% withSpinner(type = 6),
                                                  downloadButton("downrad2", "Download Plot", icon=icon("download", lib = "font-awesome"))))))),
              fluidRow(column(12, div(id = "go_box",
                                      tabBox(id = "tabbox3",title = "Gene Target Ontology", side= "right", width = 12, height = "1100px", type = "tabs", status = "primary", solidHeader = T,  collapsible = T, maximizable = T, background = "gray",
                                             label = boxLabel(text = "?", status = "danger") %>% tippy(tooltip = "Choose a toxicant or enter your own list of microRNAs to get the predicted gene targets. <br>Subsequent tabs will appear and the predicted targets will be used in choice of ontology and can be plotted in the other tabs.", interactive = TRUE, placement = "top", allowHTML = TRUE, arrow = TRUE),
                                             tabPanel(HTML('<h6 style="color:black;">miR Targets</h6>'),
                                                      fluidRow(column(3,
                                                                      radioGroupButtons("go_choice", "Choice of input", choices = c("Toxicant", "Custom List"), status = "primary", individual = T, checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("xmark", lib = "glyphicon"))))),
                                                      shinyjs::hidden(div(id = "choice_a",
                                                                          fluidRow(column(3,
                                                                                          pickerInput("tox2", label = "Choice of toxicant", choices = names(DEG), multiple = F, options = list(style = "btn-light", `live-search` = TRUE))),
                                                                                   column(4,
                                                                                          radioGroupButtons("reg2", "Choose:", choices = c("Up-regulated", "Down-regulated"), individual = TRUE, checkIcon = list(
                                                                                            yes = tags$i(class = "fa fa-circle", 
                                                                                                         style = "color: teal"),
                                                                                            no = tags$i(class = "fa fa-circle-o", 
                                                                                                        style = "color: teal"))))),
                                                                          fluidRow(column(3,
                                                                                          sliderTextInput("FC_3", "Log2Fold-Change (absolute value)", choices = seq(from= 0, to= 4, by=0.5), grid = TRUE, selected = 1)),
                                                                                   column(3,
                                                                                          pickerInput("FDR_3", "False Discovery Rate", choices = c(0.1, 0.05, 0.01), selected = 0.05))))),
                                                      shinyjs::hidden(div(id = "choice_b",
                                                                          fluidRow(column(4,
                                                                                          textAreaInput("mir_list", "Input custom microRNAs", placeholder = "hsa-miR-21-5p", cols=17, rows = 4, resize = "both"))))),
                                                      fluidRow(column(3,
                                                                      actionBttn("go5", "Get Table", style = "jelly", color="danger")),
                                                               actionButton("resetAll", "Reset all")),
                                                      br(),
                                                      shinycustomloader::withLoader(DT::DTOutput("targetstable",  height = "500px"), type = 'image', loader = 'coffee_loading.gif')),
                                             tabPanel(HTML('<h6 style="color:black;">Ontology</h6>'), value = "tab2",
                                                      fluidRow(column(4,
                                                                      radioGroupButtons("ont", "Choice of Ontology Database", choices = c("KEGG", "REACTOME", "Disease"), status = "success", checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("xmark", lib = "glyphicon")))),
                                                               column(3,
                                                                      actionBttn("go6", "Get Table", style = "jelly", color="danger"))),
                                                      br(),
                                                      shinycustomloader::withLoader(DT::DTOutput("GOtable", height = "500px"), type = 'image', loader = 'loading.gif')),
                                             tabPanel(HTML('<h6 style="color:black;">Dot Plot</h6>'), value = "tab3",
                                                      fluidRow(column(5,
                                                                      pickerInput("ont_pick", label = "Choose Ontology", choices= "", multiple = T, options = list(style = "btn-light", `live-search` = TRUE, `actions-box` = TRUE))),
                                                               column(3,
                                                                      actionBttn("go7", "Plot", style = "jelly", color="danger"))),
                                                      plotOutput("ont_plot", width = "80%", height = "800px") %>% withSpinner(type =6 , size=1),
                                                      downloadButton("downgo", "Download Plot", icon=icon("download", lib = "font-awesome"))),
                                             tabPanel(HTML('<h6 style="color:black;">Bar Plot</h6>'), value = "tab4",
                                                      fluidRow(column(5,
                                                                      pickerInput("ont_pick2", label = "Choose Ontology", choices= "", multiple = T, options = list(style = "btn-light", `live-search` = TRUE, `actions-box` = TRUE))),
                                                               column(3,
                                                                      actionBttn("go8", "Plot", style = "jelly", color="danger"))),
                                                      plotOutput("ont_barplot", width = "80%", height = "800px") %>% withSpinner(type =6 , size=1),
                                                      downloadButton("downgo2", "Download Plot", icon=icon("download", lib = "font-awesome")))
                                      ))# div go_box

                                  )) # GO 
              )
            ) # Tab2
    
  )# tabItems
  
) # body

#footer = dashboardFooter(left = "Desiree Williams", right = "2022")

ui <- bs4DashPage(header = bs4DashNavbar(title = dashboardBrand(title = "  MicroRNA Tox App", color = "secondary"), skin = "light", status = "gray-dark"),
                  sidebar = sidebar,
                  body = body,
                  #footer = footer,
                  dark = NULL
)

### SERVER #################################

server <- function(input, output, session) {
  
  #### INTRO TAB #######
  
  output$toxtable <- render_gt({
    tox_df = data.frame(Chemical = c("Cyclopamine", "Methoxyacetic acid", "Ogremorphin", "Triademenol", "Cyclophosphamide", "Methotrexate", "Valproic acid", "5-Flurouracil", "Hydrogen Peroxide"),
                        Abbreviation = c("CYCLO", "MAA", "OGM", "MENOL", "CPA", "MTX", "VPA", "5FU", "H2O2"),
                        "micrograms per mL" = c(41.16, 9, 0.0309, 29.58, 10, 0.01, 10, 0.0025, NA), 
                        "µM" = c(100, 100, 0.1, 100, 35.8, 0.022, 58.9, 0.0192, NA),
                        check.names = F)
    tox_tbl = gt(tox_df, rowname_col = "Chemical") %>%
      tab_header(title = "Developmental Toxicants") %>%
      tab_footnote(footnote = "Neural crest (NC) and mesoderm", locations = cells_body(columns = Chemical, rows = 2)) %>%
      tab_row_group(label = "Neural Crest Toxicants", rows = 1:4) %>%
      tab_row_group(label = "Mesoderm Toxicants", rows = 5:8) %>%
      cols_align( align = "center", columns = Abbreviation) %>%
      fmt_number(columns = c("micrograms per mL", µM), decimals = 4, drop_trailing_zeros = T)
    tox_tbl
  })
  

  
  #### DIFF TAB #######
  
  output$plot1 <- renderPlotly({
    df = counts %>% filter(Geneid %in% input$mir)
    color2 = paletteer_d("ggthemes::Superfishel_Stone")
    cols = as.character(color2)
    xform <- list(categoryorder = "array", categoryarray = c("Untreated", "Cyclopamine","Methoxyacetic acid","Ogremorphin","Triademenol", "Cyclophosphamide","Methotrexate","Valproic acid","5-Flurouracil","Hydrogen Peroxide"), title ="", tickangle=45)
    p = plot_ly(
      data=df,
      x = ~sample_id,
      y = ~norm_count,
      color = ~sample_id,
      colors = cols,
      type= 'box',
      boxpoints = "all",
      pointpos = 0,
      text = ~paste("Sample: ", sample_rep , "<br>Count: ", round(norm_count, 3)),
      hoverinfo = "text"
    ) %>%
      layout(title = paste0('microRNA expression for ', input$mir), xaxis = xform, yaxis = list(title = 'Normalized Counts (DESeq2)'), legend = list(title=list(text='<b> Treatments </b>'), orientation = 'h', x = 0.1, y = -0.5))
  })
  
  output$count_table <- renderDataTable({
    df = counts %>% filter(Geneid %in% input$mir)
  }, extensions =c("Buttons"), rownames = FALSE, filter = "top", escape = F, options = list("dom" = 'T<"clear">lBrtip', buttons = list('copy', list(extend = "collection",
                                                                                                                                                    buttons = c("csv", "excel", "pdf"),
                                                                                                                                                    text = "Download")),
                                                                                            pageLength = 10, lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All"))))
  
  
  vol_1 <- reactive({
    df = DEG[[input$tox]]
    df$log2FoldChange = as.numeric(df$log2FoldChange)
    
    if(input$all == "All Points"){
      opt = NULL
    } else if(input$all == "Selected microRNAs"){
      opt = input$mir2
    } else if(input$all == "Default"){
      opt = input$mir
    }
    
    EnhancedVolcano(df,
                    lab = df$microRNA,
                    x = 'log2FoldChange',
                    y = 'padj',
                    selectLab = opt,
                    xlab = bquote(~Log[2]~ 'fold change'),
                    ylab = bquote(~-Log[10] ~ italic(P-adjusted(BH))),
                    axisLabSize = 12,
                    pCutoff = as.numeric(input$FDR),
                    FCcutoff = input$FC,
                    pointSize = 4.0,
                    labSize = 6,
                    labFace = 'bold',
                    colAlpha = 0.5,
                    boxedLabels = TRUE,
                    legendPosition ='bottom',
                    legendLabSize = 12,
                    legendIconSize = 4.0,
                    drawConnectors = TRUE,
                    max.overlaps = 20,
                    widthConnectors = 1,
                    lengthConnectors = unit(0.02, "npc"),
                    colConnectors = 'black',
                    title = paste0("DEG for ", input$tox), subtitle = "microRNA-seq",
                    titleLabSize = 14, subtitleLabSize = 9, captionLabSize = 12)
  })
  
  observe({
    shinyjs::toggle(id = "list", condition = {input$all == "Selected microRNAs"})
  })
  
  observe({
    df = DEG[[input$tox]]
    df = df %>% dplyr::filter(padj < as.numeric(input$FDR) & (log2FoldChange > input$FC | log2FoldChange < -input$FC))
    updatePickerInput(session, "mir2", choices = unique(df$microRNA))
  })
  
  output$volcano <- renderPlot({
    vol_1()
  })
  
  #download handler to generate plotdownload
  output$save <- downloadHandler(
    filename = function() { paste("mir_volcano", "png", sep =".")},
    content = function(file) {
      ggsave(file, plot = vol_1(), width = 13, height = 7, units = "in", device = "png")
    }
  )
  
  output$voltable <- renderDataTable({
    df = DEG[[input$tox]]
    df = df %>% dplyr::filter(padj < as.numeric(input$FDR) & (log2FoldChange > input$FC | log2FoldChange < -input$FC))
  }, extensions =c("Buttons"), rownames = FALSE, filter = "top", escape = F, options = list("dom" = 'T<"clear">lBrtip', buttons = list('copy', list(extend = "collection",
                                                                                                                                         buttons = c("csv", "excel", "pdf"),
                                                                                                                                         text = "Download")),
                                                                            pageLength = 10, lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All"))))
  
  # upset plot for mirs overlapping in treatments
  
  combo_df <- eventReactive(input$go, {
    df = DEG[input$combo]
    if (input$reg == "Up-regulated"){
      filtered_list = lapply(df, dplyr::filter, padj < as.numeric(input$FDR_2) & (log2FoldChange > input$FC_2))
    } else {
      filtered_list = lapply(df, dplyr::filter, padj < as.numeric(input$FDR_2) & (log2FoldChange < input$FC_2))
    }
    filtered_list
  })
  
  upset_df <- eventReactive(input$go,{
    filtered_final = lapply(combo_df(), dplyr::select, microRNA)
    filtered_final = lapply(filtered_final, as.list)
    filtered_final = lapply(filtered_final, unlist, use.names= FALSE)
    color_g = list("Cyclopamine" = '#65A479',
                   "Methoxyacetic acid" = '#65A479',
                   "Ogremorphin" = '#65A479',
                   "Triademenol" = '#65A479',
                   "Cyclophosphamide" = '#5D8CA8',
                   "Methotrexate" = '#5D8CA8',
                   "Valproic acid" = '#5D8CA8',
                   "5-Flurouracil" = '#D3BA68',
                   "Hydrogen Peroxide" = '#D5695D')
    upsetjs() %>% fromList(filtered_final, colors = color_g) %>% generateDistinctIntersections() %>% chartLabels(title = paste0(input$reg, " microRNAs")) %>% chartLayout(set.label.alignment = "left")  %>% interactiveChart()
  })
  
  output$upset1 <- renderUpsetjs({
    upset_df()
  })

  
  output$clicked <- renderText({
    # click event: <id>_hover -> list(name="NAME" or NULL, elems=c(...))
    input$upset1_click$name
  })
  output$clickedElements <- renderText({
    as.character(input$upset1_click$elems)
  })
  
  output$upsettable <- renderDataTable({
    df = bind_rows(combo_df(), .id = "column_label")
    df2 = df %>% dplyr::filter(microRNA %in% as.character(input$upset1_click$elems))
  }, extensions =c("Buttons"), rownames = FALSE, filter = "top", escape = F, options = list("dom" = 'T<"clear">lBrtip', buttons = list('copy', list(extend = "collection",
                                                                                                                                                    buttons = c("csv", "excel", "pdf"),
                                                                                                                                                    text = "Download")),
                                                                                            pageLength = 10, lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All"))))
  
  # heatmaps
  
  mat_df <- reactive({
    counts %>% dplyr::select(-sample_id) %>% tidyr::pivot_wider(names_from = sample_rep, values_from = norm_count) %>% tibble::column_to_rownames(var = "Geneid") %>% as.matrix()
  })
  
  heat_df1 <- reactive({
    heat = t(scale(t(mat_df()[,1:ncol(mat_df())]))) # calculates the z-score of samples starting in column 1  
    anno = data.frame(Treatment = c(rep("Cyclopamine", 3), rep("Methoxyacetic acid", 3), rep("Ogremorphin", 3), rep("Triademenol", 3), rep("Cyclophosphamide", 3), rep("Methotrexate", 3), rep("Valproic acid", 3), rep("5-Flurouracil", 3), rep("Untreated", 3), rep("Hydrogen Peroxide", 3)), check.names = F)
    row.names(anno) <- colnames(heat)
    pheatmap(heat, annotation_col = anno, show_rownames = F, treeheight_row = 0, main = "Heatmap for all miRNAs")
  })
  
  output$heatmap1 <- renderPlot({
    heat_df1()
  })
  
  #download handler to generate plotdownload
  output$downheat1 <- downloadHandler(
    filename = function() { paste("mir_heatmap_all", "png", sep =".")},
    content = function(file) {
      ggsave(file, plot = heat_df1(), width = 11, height = 14, units = "in", device = "png")
    }
  )
  
  heat_df2 <- reactive({
    mat = mat_df()
    df_ave = data.frame(CYCLO = rowMeans(mat[,1:3]),
                        MAA =  rowMeans(mat[,4:6]),
                        OGM =  rowMeans(mat[,7:9]),
                        MENOL = rowMeans(mat[,10:12]),
                        CPA = rowMeans(mat[,13:15]),
                        MTX = rowMeans(mat[,16:18]),
                        VPA = rowMeans(mat[,19:21]),
                        `5FU` = rowMeans(mat[,22:24]),
                        UNT = rowMeans(mat[,25:27]),
                        H2O2 = rowMeans(mat[,28:30]),
                        check.names = F)
    mat2 = as.matrix(df_ave)
    heat = t(scale(t(mat2[,1:ncol(mat2)]))) # calculates the z-score of samples starting in column 1  
    anno = data.frame(Treatment = c("Cyclopamine","Methoxyacetic acid","Ogremorphin","Triademenol","Cyclophosphamide","Methotrexate", "Valproic acid", "5-Flurouracil","Untreated", "Hydrogen Peroxide"), check.names = F)
    row.names(anno) <- colnames(heat)
    pheatmap(heat, annotation_col = anno, show_rownames = F, treeheight_row = 0, main = "Heatmap for all miRNAs")
  })
  
  output$heatmap2 <- renderPlot({
    heat_df2()
  })
  
  #download handler to generate plotdownload
  output$downheat2 <- downloadHandler(
    filename = function() { paste("mir_heatmap_ave", "png", sep =".")},
    content = function(file) {
      ggsave(file, plot = heat_df2(), width = 11, height = 14, units = "in", device = "png")
    }
  )
  
  heat_df3 <- eventReactive(input$go2, {
    mat = mat_df()
    df_ave = data.frame(CYCLO = rowMeans(mat[,1:3]),
                        MAA =  rowMeans(mat[,4:6]),
                        OGM =  rowMeans(mat[,7:9]),
                        MENOL = rowMeans(mat[,10:12]),
                        CPA = rowMeans(mat[,13:15]),
                        MTX = rowMeans(mat[,16:18]),
                        VPA = rowMeans(mat[,19:21]),
                        `5FU` = rowMeans(mat[,22:24]),
                        UNT = rowMeans(mat[,25:27]),
                        H2O2 = rowMeans(mat[,28:30]),
                        check.names = F)
    df_ave = df_ave %>% tibble::rownames_to_column(var = "gene") %>% dplyr::filter(gene %in% input$mirs_choice) %>% tibble::column_to_rownames(var = "gene")
    mat2 = as.matrix(df_ave)
    heat = t(scale(t(mat2[,1:ncol(mat2)]))) # calculates the z-score of samples starting in column 1  
    anno = data.frame(Treatment = c("Cyclopamine","Methoxyacetic acid","Ogremorphin","Triademenol","Cyclophosphamide","Methotrexate", "Valproic acid", "5-Flurouracil","Untreated", "Hydrogen Peroxide"), check.names = F)
    row.names(anno) <- colnames(heat)
    pheatmap(heat, annotation_col = anno, show_rownames = T, treeheight_row = 0, main = "miRNAs")
  })
  
  output$heatmap3 <- renderPlot({
    heat_df3()
  })
  
  #download handler to generate plotdownload
  output$downheat3 <- downloadHandler(
    filename = function() { paste("mir_heatmap_ave", "png", sep =".")},
    content = function(file) {
      ggsave(file, plot = heat_df3(), width = 11, height = 14, units = "in", device = "png")
    }
  )
  
  
  # radar plot
  
  rad1 <- eventReactive(input$go3,{
    DEG = bind_rows(DEG, .id = "toxicants")
    DEG = DEG %>% filter(microRNA %in% input$mirs_choice2 & toxicants %in% input$combo2)
    DEG = DEG %>% dplyr::arrange(desc(log2FoldChange))
    DEG = DEG %>% dplyr::select(-padj) %>% tidyr::pivot_wider(names_from = microRNA, values_from = log2FoldChange) %>% tibble::column_to_rownames(var = "toxicants")
    DEG <- rbind(rep(max(DEG),ncol(DEG)) , rep(min(DEG),ncol(DEG)) , DEG)
    DEG
  })
  
  output$radar1 <- renderPlot({
    validate(
      need(input$mirs_choice2 >= 3, "Select at least three microRNAs.")
    )
    colors = paletteer_d("ggthemes::Tableau_10")
    radarchart(rad1(), axistype=1,
               #custom polygon
               pcol=colors , plwd=2 , plty=1, pfcol = scales::alpha(colors, 0.25),
               #custom the grid
               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
               #custom labels
               vlcex=1.3,
               title = "Generated using log2FoldChange")
    # Add a legend
    legend(x=1.3, y=1, legend = rownames(rad1()[-c(1,2),]), bty = "n", pch=20 , col=colors , text.col = "black", cex=1.2, pt.cex=3)
  })
  
  output$downrad1 <- downloadHandler(
    filename = function() { paste("mir_radar_tox", "png", sep =".")},
    content = function(file) {
      png(file, width = 1200, height = 800, units = "px", res = 72, type = "cairo-png")
      colors = paletteer_d("ggthemes::Tableau_10")
      radarchart(rad1(), axistype=1,
                 #custom polygon
                 pcol=colors , plwd=2 , plty=1, pfcol = scales::alpha(colors, 0.25),
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                 #custom labels
                 vlcex=1.3,
                 title = "Generated using log2FoldChange")
      # Add a legend
      legend(x=1.3, y=1, legend = rownames(rad1()[-c(1,2),]), bty = "n", pch=20 , col=colors , text.col = "black", cex=1.2, pt.cex=3)
      dev.off()
    }
  )
  
  rad2 <- eventReactive(input$go4,{
    DEG = bind_rows(DEG, .id = "toxicants")
    DEG = DEG %>% filter(microRNA %in% input$mirs_choice3 & toxicants %in% input$combo3)
    DEG = DEG %>% dplyr::arrange(desc(log2FoldChange))
    DEG = DEG %>% dplyr::select(-padj) %>% tidyr::pivot_wider(names_from = toxicants, values_from = log2FoldChange) %>% tibble::column_to_rownames(var = "microRNA")
    DEG <- rbind(rep(max(DEG),ncol(DEG)) , rep(min(DEG),ncol(DEG)) , DEG)
    DEG
  })  
  

  output$radar2 <- renderPlot({
    validate(
      need(input$combo3 >= 3, "Select at least three toxicants.")
    )
    colors = paletteer_d("ggthemes::Tableau_10")
    radarchart(rad2()  , axistype=1 , 
                #custom polygon
                pcol=colors , plwd=2 , plty=1, pfcol = scales::alpha(colors, 0.25),
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                #custom labels
                vlcex=1.5,
                title = "Generated using log2FoldChange")
    
    # Add a legend
    legend(x=1.3, y=1, legend = rownames(rad2()[-c(1,2),]), bty = "n", pch=20 , col=colors , text.col = "black", cex=1.2, pt.cex=3)
  })
  
  output$downrad2 <- downloadHandler(
    filename = function() { paste("mir_radar_tox", "png", sep =".")},
    content = function(file) {
      png(file, width = 1200, height = 800, units = "px", res = 72, type = "cairo-png")
      colors = paletteer_d("ggthemes::Tableau_10")
      radarchart( rad2()  , axistype=1 , 
                  #custom polygon
                  pcol=colors , plwd=2 , plty=1, pfcol = scales::alpha(colors, 0.25),
                  #custom the grid
                  cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                  #custom labels
                  vlcex=1.3,
                  title = "Generated using log2FoldChange")
      
      # Add a legend
      legend(x=1.3, y=1, legend = rownames(rad2()[-c(1,2),]), bty = "n", pch=20 , col=colors , text.col = "black", cex=1.2, pt.cex=3)
      dev.off()
    }
  )
  
  # GO analysis
  
  # hide the other two tabs that depend on values generated in the first tab
  observe({
    hide(selector = "#tabbox3 li a[data-value=tab2]")
    hide(selector = "#tabbox3 li a[data-value=tab3]")
    hide(selector = "#tabbox3 li a[data-value=tab4]")
  })
  
  observe({
    toggle(id = "choice_a", condition = {input$go_choice %in% "Toxicant"})
    toggle(id = "choice_b", condition = {input$go_choice %in% "Custom List"})
  })
  
  observeEvent(input$resetAll, {
    reset("go_box")
    hide(selector = "#tabbox3 li a[data-value=tab2]")
    hide(selector = "#tabbox3 li a[data-value=tab3]")
    hide(selector = "#tabbox3 li a[data-value=tab4]")
  })
  
  mirs_choice <- reactive({
    if(input$go_choice %in% "Toxicant"){
      df = DEG[[input$tox2]]
      if (input$reg2 == "Up-regulated"){
        filtered_list = df %>% dplyr::filter(padj < as.numeric(input$FDR_3) & (log2FoldChange > input$FC_3))
      } else {
        filtered_list = df %>% dplyr::filter(padj < as.numeric(input$FDR_3) & (log2FoldChange < input$FC_3))
      }
      mirs = tolower(filtered_list$microRNA)
    } else {
      mirs = scan(text = input$mir_list, what = "")
      mirs = tolower(mirs)
    }
    mirs
  })
  

  # get the microRNA targets for a specific toxicant
  .on.public.web <- FALSE; # only TRUE when on mirnet web server
  # function to set up the type of mir targets needed. 
  setdatamulti <- function () 
  {
    dataSet$type <- c("gene")
    dataSet <<- dataSet
    if (.on.public.web) {
      return(1)
    }
    else {
      return(paste("Targets were entered!"))
    }
  }
  
  mirnet_targets <- eventReactive(input$go5, {
    mirs = mirs_choice()
    #### Step 1. Initiate the dataSet object
    Init.Data("mir", "mirlist")
    #### Step 2. Set up the user input data
    SetupMirListData(mirs = mirs, orgType = "hsa", idType = "mir_id", tissue = "na")
    #### Step 3. Set up targets
    setdatamulti()
    #### Step 4. Perform miRNAs to multiple targets mapping, results are downloaded in your working directory
    QueryMultiListMir()
    res = mir.resu %>% dplyr::select(-Tissue)
    res
  })
  
  output$targetstable <- renderDT({
    datatable(mirnet_targets(), rownames=F, filter="top",
              extensions =c("Buttons"), options = list(dom = 'lBrtip', buttons = list('copy', list(extend = "collection", buttons = c("csv", "excel", "pdf"), text = "Download")),
                                                       pageLength = 10, lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All"))))
  })
  
  # once the get table button on the first tab is clicked, reveal the other three tabs.
  observeEvent(input$go5, {
    shinyjs::toggle(selector = "#tabbox3 li a[data-value=tab2]")
    shinyjs::toggle(selector = "#tabbox3 li a[data-value=tab3]")
    shinyjs::toggle(selector = "#tabbox3 li a[data-value=tab4]")
  })
  
  
  go_table <- eventReactive(input$go6, {
    if (input$ont %in% "KEGG"){
      res <- enrichKEGG(gene         = unique(mirnet_targets()$TargetID),
                        organism     = 'hsa',
                        pvalueCutoff = 0.05)
      
      res <- setReadable(res, OrgDb = org.Hs.eg.db, keyType="ENTREZID")
    } else if (input$ont %in% "REACTOME"){
      res <- enrichPathway(gene=unique(mirnet_targets()$TargetID), pvalueCutoff = 0.05, readable=TRUE)
    } else {
      res <- enrichDO(gene          = unique(mirnet_targets()$TargetID),
                      ont           = "DO",
                      pvalueCutoff  = 0.05,
                      pAdjustMethod = "BH",
                      minGSSize     = 5,
                      maxGSSize     = 500,
                      qvalueCutoff  = 0.05,
                      readable      = TRUE)
    }
    res <- as.data.frame(res)
    res <- res %>% dplyr::filter(p.adjust < 0.05) %>% dplyr::select(ID, Description, GeneRatio, pvalue, p.adjust, geneID, Count) %>% dplyr::mutate(GeneRatio = DOSE::parse_ratio(GeneRatio))
  })
  
  output$GOtable <- renderDT({
    datatable(go_table(), extensions =c("Buttons", 'Responsive'), rownames = FALSE, filter = "top", escape = F, options = list("dom" = 'T<"clear">lBrtip', buttons = list('copy', list(extend = "collection",
                                                                                                                                                                         buttons = c("csv", "excel", "pdf"),
                                                                                                                                                                         text = "Download")),
                                                                                                                 pageLength = 10, lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All"))))
    
  })
  
  # use the ontologies generated above as choices to plot
  observe({
    df = go_table()$Description
    updatePickerInput(session, "ont_pick", choices = unique(df))
  })
  
  go_plot <- eventReactive(input$go7, {
    path.table = go_table() %>% dplyr::filter(Description %in% input$ont_pick)
    p <- ggplot(path.table, aes(GeneRatio, forcats::fct_reorder(Description, GeneRatio))) +
      geom_point(aes(color=p.adjust, size = Count), position = position_jitter(width = 0.01, height = 0.01)) + 
      scale_color_viridis() +
      scale_size(range = c(2,10)) +
      theme_bw() +
      xlab("GeneRatio") + ylab(NULL) +
      ggtitle(paste0(ifelse(input$go_choice %in% "Toxicant", input$tox2, input$go_choice), " ", ifelse(input$go_choice %in% "Toxicant", input$reg2, "of"), " microRNAs in ", input$ont, " ontologies"))
    p
  })
  
  output$ont_plot <- renderPlot({
    go_plot()
  })
  
  #download handler to generate plotdownload
  output$downgo <- downloadHandler(
    filename = function(){ paste0("GO_dotplot", ".png")},
    content = function(file) {
    ggplot2::ggsave(file, plot = go_plot(), width = 8, height = 11, units = "in", device = "png")
    }
  )
  
  # use the ontologies generated as choices to plot
  observe({
    df = go_table()$Description
    updatePickerInput(session, "ont_pick2", choices = unique(df))
  })
  
  go_plotbar <- eventReactive(input$go8, {
    path.table = go_table() %>% dplyr::filter(Description %in% input$ont_pick2) %>% dplyr::mutate(negative_log10_of_adjusted_p_value = -log10(p.adjust))
    colors <- unikn::usecol(pal = pal_petrol, n=19)
    p =  ggplot(path.table, aes(forcats::fct_reorder(Description, negative_log10_of_adjusted_p_value), negative_log10_of_adjusted_p_value)) +
      geom_col(aes(fill=negative_log10_of_adjusted_p_value)) +
      scale_fill_gradientn(colors = colors) +
      coord_flip() +
      labs(title = paste0(ifelse(input$go_choice %in% "Toxicant", input$tox2, input$go_choice), " ", ifelse(input$go_choice %in% "Toxicant", input$reg2, "of"), " microRNAs in ", input$ont, " ontologies"),
           x= NULL, y = expression("-Log"[10]*"(adjusted p-value)"), fill = expression("-Log"[10]*"(p.adjust)")) +
      ds4psy::theme_ds4psy(col_title = "#37474f" , col_brdrs = "#37474f")
    p
  })
  
  output$ont_barplot <- renderPlot({
    go_plotbar()
  })
  
  #download handler to generate plotdownload
  output$downgo2 <- downloadHandler(
    filename = function(){ paste0("GO_barplot", ".png")},
    content = function(file) {
      ggplot2::ggsave(file, plot = go_plotbar(), width = 14, height = 11, units = "in", device = "png", bg = "white")
    }
  )

} #server



shinyApp(ui, server)

