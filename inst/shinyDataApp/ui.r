## No upload progress bar
fileInput1 <-
function (inputId, label, multiple = FALSE, accept = NULL)
{
  inputTag <- tags$input(id = inputId, name = inputId, type = "file")
  if (multiple)
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0)
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  tagList(tags$label(label), inputTag)
}

textareaInput <- function(inputId, label, value="", placeholder="", rows=2){
  tagList(
    div(strong(label), style="margin-top: 5px;"),
    tags$style(type="text/css", "textarea {width:100%; margin-top: 5px;}"),
    tags$textarea(id = inputId, placeholder = placeholder, rows = rows, value))
}


shinyUI(navbarPage(
  id='mainNavBar',
  title=img(src="http://i.imgur.com/hG7Ltn2.png", alt="shinyData"),
  windowTitle="shinyData",

  tabPanel(title='Project',

           div(selectInput('sampleProj',
                                list(actionButton('openSampleProj', 'Open', styleclass="primary", size="small"), 'Sample Project:'),
                                choices=list.files('samples')),
               class = "pull-right"),
           br(),

           downloadButton('downloadProject', 'Save Project to File'),

           tags$hr(),

           fileInput1('loadProject', 'Import Project from File', accept=c('.sData')),
           radioButtons('loadProjectAction', '',
                        choices=c('Replace existing work'='replace',
                                  'Merge with existing work'='merge'),
                        selected='replace', inline=FALSE),

           tags$hr(),
           includeMarkdown('md/about.md')
  ),



  tabPanel(title="Data",

           sidebarLayout(
             sidebarPanel(

               selectInput(inputId="datList", label=NULL, choices=NULL),

               tags$hr(),
               fileInput1('file', 'Add Data Source from Text File',
                         accept=c('text/csv',
                                  'text/comma-separated-values,text/plain',
                                  '.csv')),

               tags$hr(),
               actionButton('addDatCode', 'Add Data Source with R Code', styleclass="primary")
             ),
             mainPanel(
               textInput('datName', 'Data Source Name'),

               tags$hr(),

               conditionalPanel('output.currentDatType=="code"',
                                bsAlert('datCodeAlert'),
                        aceEditor('datCode', mode='r', value='', cursorId="datCodeCursor",
                                  selectionId='datCodeSelection', wordWrap=TRUE),
                        actionButton('runDatCode', 'Run', styleclass="primary"),
                        tags$hr()
               ),

               selectizeInput(inputId="measures", label="Measures",
                              choices=NULL, multiple=TRUE,
                              options=list(
                                placeholder = '',
                                plugins = I("['remove_button']"))),

               tags$hr(),

               selectizeInput(inputId="fieldsList", label="Fields Details",
                              choices=NULL),
               textInput('fieldName', 'Field Name'),

               tags$hr(),

               h4('Preview'),
               dataTableOutput('datPreview')



               )
             )
           ),

  tabPanel(title='Visualize',

           sidebarLayout(
             sidebarPanel(
               fluidRow(
                 column(6, selectInput(inputId='sheetList', label=NULL, choices=NULL, selected='')),
                 column(6, fluidRow(
                   actionButton(inputId='addSheet', label='Add Sheet', styleclass="primary", size="small", css.class='btn-aligned-select'),
                   actionButton(inputId='deleteSheet', label='Delete Sheet', styleclass="danger", size="small", css.class='btn-aligned-select')
                 ))
               ),
               fluidRow(
                 column(6, selectInput(inputId='layerList', label=NULL, choices=NULL, selected='', selectize=FALSE, size=3)),
                 column(6, fluidRow(
                   actionButton(inputId='addLayer', label='Add Overlay', styleclass="primary", size="small", css.class='btn-aligned-select'),
                   actionButton(inputId='bringToTop', label='Bring to Top', styleclass="primary", size="small", css.class='btn-aligned-select'),
                   conditionalPanel('input.layerList!="Plot"',
                                    actionButton(inputId='deleteLayer', label='Delete Overlay', styleclass="danger", size="small", css.class='btn-aligned-select')
                                    )
                   ))
                 ),

               tabsetPanel(id='sheetControlTab',
                 tabPanel('Type', value='sheetTabType',
                          fluidRow(
                            column(6,
                                   selectInput(inputId='markList', label='Mark Type',
                                               choices=GeomChoices, selected='point'),
                                   selectInput(inputId='layerPositionType', label='Positioning',
                                               choices=c('Stack'='stack','Dodge'='dodge','Fill'='fill',
                                                         'Identity'='identity','Jitter'='jitter'),
                                               selected='identity'),
                                   fluidRow(
                                     column(6,
                                            textInput('layerPositionWidth', label='Width')
                                            ),
                                     column(6,
                                            textInput('layerPositionHeight', label='Height')
                                            )
                                     )
                            ),
                            column(6,
                                   selectInput(inputId='statTypeList', label='Stat',
                                               choices=StatChoices, selected='identity'),
                                   conditionalPanel('input.statTypeList=="summary"',
                                                    selectizeInput(inputId='yFunList', label='Summarize Y with',
                                                                   choices=YFunChoices,
                                                                   selected='sum', multiple=FALSE,
                                                                   options = list(create = TRUE)))
                            )
                          ),
                          br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
                          ),
                 tabPanel('Mapping', value='sheetTabMapping',

                          fluidRow(
                            column(4,
                                   selectInput(inputId='aesList', label=NULL,
                                               choices=NULL, selectize=FALSE, size=15)
                                   ),
                            column(8,
                                   conditionalPanel('input.layerList != "Plot" ||
                                                    (input.aesList!="aesX" && input.aesList!="aesY")',
                                                    radioButtons('aesMapOrSet', '', choices=c('Map to variable'='map',
                                                                                              'Set to fixed value'='set'),
                                                                 selected='', inline=TRUE)
                                                    ),

                                   uiOutput('mapOrSetUI')
                                   )
                            ),
                          br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
                        ),
                  #                  tabPanel('Filters', value='sheetTabFilters',
                  #                           selectizeInput(inputId='filterField', label='Field',
                  #                                          choices=NULL, multiple=FALSE),
                  #                           br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
                  #                   ),

                 tabPanel('Customize', value='sheetTabCustomize',
                          textareaInput(inputId = 'plotTitle', label="Plot Title", value="",
                                        placeholder = 'Enter Plot Title here', rows = 2),
                          fluidRow(
                            column(6,
                                   textInput('plotXlab', 'X Axis Title')
                            ),
                            column(6,
                                   textInput('plotYlab', 'Y Axis Title')
                            )),
                          h4('Formatting'),
                          fluidRow(
                            column(6,
                                   shinyTree('customizeItem', search=TRUE)
                                   ),
                            column(6,
                                   conditionalPanel('output.ggElementType!="unit" && output.ggElementType!="character" && output.ggElementType!=""',
                                                    checkboxInput('elementBlank', 'Hide Element', value=FALSE)),
                                   conditionalPanel('output.ggElementType=="element_text"',
                                                    selectInput('textFamily','Font Family', choices=FontFamilyChoices),
                                                    selectInput('textFace', 'Font Face', choices=FontFaceChoices),
                                                    colorInput('textColor', 'Font Color'),
                                                    numericInput('textSize', 'Font Size (pts)', value=NULL, step=0.1),
                                                    numericInput('textHjust', 'Horizontal Adjustment', value=NULL, step=0.1),
                                                    numericInput('textVjust', 'Vertical Adjustment', value=NULL, step=0.1),
                                                    numericInput('textAngle', 'Angle (in [0,360])', value=NULL, step=1),
                                                    numericInput('textLineheight', 'Text Line Height', value=NULL, step=0.1)
                                   ),
                                   conditionalPanel('output.ggElementType=="element_rect"',
                                                    colorInput('rectColor', 'Border Color'),
                                                    colorInput('rectFill', 'Fill'),
                                                    numericInput('rectSize', 'Border Line Width (pts)', value=NULL, step=0.1),
                                                    numericInput('rectLinetype', 'Border Line Type', value=NULL, step=1)
                                   ),
                                   conditionalPanel('output.ggElementType=="element_line"',
                                                    colorInput('lineColor', 'Line Color'),
                                                    numericInput('lineSize', 'Line Width (pts)', value=NULL, step=0.1),
                                                    numericInput('lineLinetype', 'Line Type', value=NULL, step=1),
                                                    numericInput('lineLineend', 'Line End', value=NULL, step=1)
                                   ),
                                   conditionalPanel('output.ggElementType=="unit"',
                                                    numericInput('unitX', 'Value', value=NULL, step=0.1),
                                                    selectInput('unitUnits', 'Unit', choices=UnitChoices)
                                   ),
                                   conditionalPanel('output.ggElementType=="character"',
                                                    uiOutput('charSetting')
                                   )
                                   )
                            ),
                          br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
                          )
               )
               ),
             mainPanel(
               textInput('sheetName', label=''),
               tags$hr(),
               fluidRow(
                 column(4,
                        #selectInput(inputId='outputTypeList', label='Output Type',
                        #            choices=c('Table'='table','Plot'='plot'), selected='plot'),
                        radioButtons('autoRefresh', label='',
                                     choices=c('Auto Refresh'='refresh','Pause Refreshing'='pause'), selected='refresh')
                        ),
                 column(4,
                        selectInput(inputId='sheetDatList', label='Data', choices=NULL),
                        checkboxInput('combineMeasures', label='Combine Measures')
                        ),
                 column(4,
                        selectizeInput(inputId="columns", label="Facet Columns",
                                       choices=NULL, multiple=TRUE,
                                       options=list(
                                         placeholder = '',
                                         plugins = I("['remove_button','drag_drop']"))),
                        selectizeInput(inputId="rows", label="Facet Rows",
                                       choices=NULL, multiple=TRUE,
                                       options=list(
                                         placeholder = '',
                                         plugins = I("['remove_button','drag_drop']")))
                        )
                 ),
               tags$hr(),
               uiOutput('sheetOutput')
               )
             )
           ),


  tabPanel(title='Presentation',

           sidebarLayout(
             sidebarPanel(
               fluidRow(
                 column(3, selectInput(inputId='docList', label=NULL, choices=NULL, selected='')),
                 column(9, fluidRow(
                   actionButton(inputId='addDoc', label='Add Document', styleclass="primary", size="small", css.class='btn-aligned-select'),
                   actionButton(inputId='deleteDoc', label='Delete Document', styleclass="danger", size="small", css.class='btn-aligned-select')
                 ))
               ),

               tabsetPanel(
                 tabPanel('Instructions',

                          br(),
                          includeMarkdown('md/rmdInstructions.md'),

                          checkboxInput('withRChunk', label='Insert with R chunk enclosure', value=TRUE),
                          fluidRow(
                            column(6, selectInput(inputId='datNameToInsert', label=NULL, choices=NULL, selected='')),
                            column(6, fluidRow(
                              actionButton(inputId='insertDatName', label='Insert Data', styleclass="primary", size="small", css.class='btn-aligned-select')
                            ))
                          ),
                          fluidRow(
                            column(6, selectInput(inputId='sheetNameToInsert', label=NULL, choices=NULL, selected='')),
                            column(6, fluidRow(
                              actionButton(inputId='insertSheetName', label='Insert Sheet', styleclass="primary", size="small", css.class='btn-aligned-select')
                            ))
                          ),
                          br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
                          )
                 )
               ),
             mainPanel(
               textInput('docName', label=''),
               tags$hr(),
               div(downloadButton('downloadRmdOutput', 'Generate Output'), class = "pull-right"),
               selectInput('rmdOuputFormat','Output Format',
                           choices=c('HTML'='html_document', 'PDF'='pdf_document',
                                     'Word'='word_document', 'Markdown'='md_document',
                                     'ioslides'='ioslides_presentation',
                                     'Slidy'='slidy_presentation',
                                     'Beamer'='beamer_presentation'),
                           selected='pdf_document'),

               tags$hr(),
               tabsetPanel(id='rmdTabs',
                 tabPanel('R_Markdown',
                          aceEditor('rmd', mode='markdown', value='', cursorId="rmdCursor",
                                    selectionId='rmdSelection', wordWrap=TRUE)
                          ),
                 tabPanel('Preview',
                          uiOutput('rmdOutput')
                          )
                 )
               )
             )
           ),


  #   tabPanel(title='Settings',
  #
  #            if(!extrafontsImported){
  #              list(actionButton('importFonts', 'Import System Fonts', styleclass="primary"),
  #                   helpText('Import fonts from the operating system so that they are available for shinyData. This can take a few minutes.'))
  #            }
  #
  #   ),

  #

  tags$head(tags$script(src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/jquery-ui.min.js"),
            tags$style(type='text/css', ".btn-aligned-select { margin-bottom: 10px; } .btn-small {font-size:13px; padding:5px;}")
            )


))

