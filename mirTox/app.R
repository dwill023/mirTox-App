library(shiny)
library(shinyjs)
library(bs4Dash)
library(fresh)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(dplyr)
library(ggplot2)
library(htmlwidgets)
library(corrplot)
#library(visNetwork)
#library(gridExtra)
#library(EnvStats)
#library(highcharter)
library(paletteer)
library(plotly)
library(EnhancedVolcano)
library(upsetjs)
library(tippy)
library(fmsb)
library(gt)
library(matrixStats)
library(pheatmap)


# load data used in app
counts = readRDS("./data/counts_new.rds") # this is the normalized counts by DESeq2
DEG = readRDS("./data/DEG_list.rds") # this is a list of the DEG for the 9 treatments.

# create a theme for the app
mytheme <- create_theme(
  bs4dash_vars(
    navbar_light_color = "#bec5cb",
    navbar_light_active_color = "#FFF",
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
    primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
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
                                                       prettyCheckbox("all", "Label all points", value = FALSE, status = "danger", shape = "curve", outline = TRUE)
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
                                  upsetjsOutput("upset1", width = "100%", height = "600px") %>% withSpinner(type = 6, size=1),
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
                              box(id="heatbox1", title = "Heatmap", width = 12, collapsible = TRUE, collapsed = FALSE, maximizable = T, status = "primary", solidHeader = TRUE, background = "white",
                                  sidebar = boxSidebar(id = "box5side",
                                                       ),
                                  plotOutput("heatmap", width = "100%", height = "1000px") %>% withSpinner(type = 6),
                                  downloadButton("downheat", "Download Plot", icon=icon("download", lib = "font-awesome"))))

            ))
            )
    
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
    if(input$all == TRUE){
      opt = NULL
    } else{
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
                    labSize = 4.0,
                    colAlpha = 0.5,
                    boxedLabels = FALSE,
                    legendPosition ='bottom',
                    legendLabSize = 12,
                    legendIconSize = 4.0,
                    drawConnectors = TRUE,
                    widthConnectors = 0.75,
                    title = paste0("DEG for ", input$tox), subtitle = "microRNA-seq",
                    titleLabSize = 14, subtitleLabSize = 9, captionLabSize = 12)
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
    df$log2FoldChange = as.numeric(df$log2FoldChange)
    df$padj = as.numeric(df$padj)
    df = df %>% dplyr::filter(padj < input$FDR & (log2FoldChange > input$FC | log2FoldChange < -input$FC))
  }, extensions =c("Buttons"), rownames = FALSE, filter = "top", escape = F, options = list("dom" = 'T<"clear">lBrtip', buttons = list('copy', list(extend = "collection",
                                                                                                                                         buttons = c("csv", "excel", "pdf"),
                                                                                                                                         text = "Download")),
                                                                            pageLength = 10, lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All"))))
  
  # upset plot for mirs overlapping in treatments
  
  upset_df <- reactive({
    if (input$reg == "Up-regulated"){
      filtered_list = lapply(DEG, dplyr::filter, padj < as.numeric(input$FDR_2) & (log2FoldChange > input$FC_2))
    } else {
      filtered_list = lapply(DEG, dplyr::filter, padj < as.numeric(input$FDR_2) & (log2FoldChange < input$FC_2))
    }
    filtered_list
  })
  
  output$upset1 <- renderUpsetjs({
    filtered_final = lapply(upset_df() , dplyr::select, microRNA)
    list_f = list("Cyclopamine" = c(filtered_final$Cyclopamine$microRNA),
                  "Methoxyacetic acid" = c(filtered_final$`Methoxyacetic acid`$microRNA),
                  "Ogremorphin" = c(filtered_final$Ogremorphin$microRNA),
                  "Triademenol" = c(filtered_final$Triademenol$microRNA),
                  "Cyclophosphamide" = c(filtered_final$Cyclophosphamide$microRNA),
                  "Methotrexate" = c(filtered_final$Methotrexate$microRNA),
                  "Valproic acid" = c(filtered_final$`Valproic acid`$microRNA),
                  "5-Flurouracil" = c(filtered_final$`5-Flurouracil`$microRNA),
                  "Hydrogen Peroxide" = c(filtered_final$`Hydrogen Peroxide`$microRNA))
    color_g = list("Cyclopamine" = '#00798c',
                   "Methoxyacetic acid" = '#00798c',
                   "Ogremorphin" = '#00798c',
                   "Triademenol" = '#00798c',
                   "Cyclophosphamide" = '#52489c',
                   "Methotrexate" = '#52489c',
                   "Valproic acid" = '#52489c',
                   "5-Flurouracil" = '#52489c',
                   "Hydrogen Peroxide" = '#d1495b')
    upsetjs() %>% fromList(list_f, colors = color_g) %>% generateDistinctIntersections(min = 3) %>%
      chartLabels(title = paste0("Upset plot for ", input$reg, " microRNAs")) %>% chartLayout(set.label.alignment = "left")  %>% interactiveChart()
  })
  
  output$clicked <- renderText({
    # click event: <id>_hover -> list(name="NAME" or NULL, elems=c(...))
    input$upset1_click$name
  })
  output$clickedElements <- renderText({
    as.character(input$upset1_click$elems)
  })
  
  output$upsettable <- renderDataTable({
    df = bind_rows(upset_df(), .id = "column_label")
    df2 = df %>% dplyr::filter(microRNA %in% as.character(input$upset1_click$elems))
  }, extensions =c("Buttons"), rownames = FALSE, filter = "top", escape = F, options = list("dom" = 'T<"clear">lBrtip', buttons = list('copy', list(extend = "collection",
                                                                                                                                                    buttons = c("csv", "excel", "pdf"),
                                                                                                                                                    text = "Download")),
                                                                                            pageLength = 10, lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All"))))
  
  # heatmap
  
  heat_df <- reactive({
    mat = counts %>% dplyr::select(-sample_id) %>% tidyr::pivot_wider(names_from = sample_rep, values_from = norm_count) %>% tibble::column_to_rownames(var = "Geneid") %>% as.matrix()
    topVarGenes <- head(order(rowVars(mat), decreasing = TRUE), 100) #a subset of the most highly variable genes.
    mat2 = mat[topVarGenes,]
    heat = t(scale(t(mat2[,1:ncol(mat2)]))) # calculates the z-score of samples starting in column 1
    anno = data.frame(Treatment = c(rep("Cyclopamine", 3), rep("Methoxyacetic acid", 3), rep("Ogremorphin", 3), rep("Triademenol", 3), rep("Cyclophosphamide", 3), rep("Methotrexate", 3), rep("Valproic acid", 3), rep("5-Flurouracil", 3), rep("Untreated", 3), rep("Hydrogen Peroxide", 3)), check.names = F)
    row.names(anno) <- colnames(heat)
    pheatmap(heat, annotation_col = anno, main = HTML(paste0("Heatmap for miRNAs", '<br>', '<sup>Top 100 most variable miRNAs</sup>')))
  })
  
  output$heatmap <- renderPlot({
    heat_df()
  })
  
  #download handler to generate plotdownload
  output$downheat <- downloadHandler(
    filename = function() { paste("mir_heatmap", "png", sep =".")},
    content = function(file) {
      ggsave(file, plot = heat_df(), width = 11, height = 14, units = "in", device = "png")
    }
  )
  


  

  
  
} #server



shinyApp(ui, server)

