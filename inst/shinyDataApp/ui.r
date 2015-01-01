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


shinyUI(navbarPage(
  id='mainNavBar',
  title="shinyData (Beta)",

  tabPanel(title='Project',

           downloadButton('downloadProject', 'Save Project to File'),

           tags$hr(),

           fileInput1('loadProject', 'Import Project from File', accept=c('.sData')),
           radioButtons('loadProjectAction', '',
                        choices=c('Replace existing work'='replace',
                                  'Merge with existing work'='merge'),
                        selected='replace', inline=FALSE),

           tags$hr(),
           fluidRow(
             column(2, selectInput('sampleProj', 'Sample Projects', choices=list.files('samples'))),
             actionButton('openSampleProj', 'Open', styleclass="primary", size="small")
             )
  ),



  tabPanel(title="Data",

           sidebarLayout(
             sidebarPanel(

               selectInput(inputId="datList", label="", choices=NULL),

               tags$hr(),

               fileInput1('file', 'Add Text File',
                         accept=c('text/csv',
                                  'text/comma-separated-values,text/plain',
                                  '.csv')),
               tags$hr(),

               conditionalPanel("output.uploadingTextFile==true",
                                checkboxInput('header', 'Header', TRUE),
                                radioButtons('sep', 'Separator',
                                             c(Comma=',',
                                               Semicolon=';',
                                               Tab='\t'),
                                             ','),
                                radioButtons('quote', 'Quote',
                                             c(None='',
                                               'Double Quote'='"',
                                               'Single Quote'="'"),
                                             '"')
                                )


             ),
             mainPanel(
               textInput('datName', 'Data Source Name'),

               tags$hr(),

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
                 column(3, selectInput(inputId='sheetList', label='', choices=NULL, selected='')),
                 column(9, fluidRow(
                   actionButton(inputId='addSheet', label='Add Sheet', styleclass="primary", size="small"),
                   actionButton(inputId='deleteSheet', label='Delete Sheet', styleclass="danger", size="small")
                 ))
               ),
               fluidRow(
                 column(3, selectInput(inputId='layerList', label='', choices=NULL, selected='')),
                 column(9, fluidRow(
                   actionButton(inputId='addLayer', label='Add Overlay', styleclass="primary", size="small"),
                   actionButton(inputId='bringToTop', label='Bring to Top', styleclass="primary", size="small"),
                   conditionalPanel('input.layerList!="Plot"',
                                    actionButton(inputId='deleteLayer', label='Delete Overlay', styleclass="danger", size="small")
                                    )
                   ))
                 ),

               tabsetPanel(
                 tabPanel('Type',
                          fluidRow(
                            column(6,
                                   selectInput(inputId='markList', label='Mark Type',
                                               choices=GeomChoices, selected='bar'),
                                   selectInput(inputId='layerPositionType', label='Positioning',
                                               choices=c('Stack'='stack','Dodge'='dodge','Fill'='fill',
                                                         'Identity'='identity','Jitter'='jitter'),
                                               selected='stack'),
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
                          ), tags$h1('.'), tags$h1('.'), tags$h1('.')
                          ),
                 tabPanel('Mapping',

                          fluidRow(
                            column(6,
                                   selectInput(inputId='aesList', label='',
                                               choices=NULL)
                                   ),
                            column(6,
                                   conditionalPanel('input.layerList != "Plot" ||
                                                    (input.aesList!="aesX" && input.aesList!="aesY")',
                                                    radioButtons('aesMapOrSet', '', choices=c('Map to variable'='map',
                                                                                              'Set to fixed value'='set'),
                                                                 selected='map')
                                                    )

                                   )
                            ),

                          uiOutput('mapOrSetUI'),
                          conditionalPanel('(input.aesList=="aesColor" || input.aesList=="aesBorderColor") &&
                                           input.aesMapOrSet=="set"',
                                           jscolorInput('aesValueColor')),

                          tags$h1('.'), tags$h1('.'), tags$h1('.'), tags$h1('.'), tags$h1('.')
                        ),
                 tabPanel('Filters',
                          selectizeInput(inputId='filterField', label='Field',
                                         choices=NULL, multiple=FALSE)
                  )
               )
               ),
             mainPanel(
               textInput('sheetName', label=''),
               tags$hr(),
               fluidRow(
                 column(4,
                        selectInput(inputId='outputTypeList', label='Output Type',
                                    choices=c('Table'='table','Plot'='plot'), selected='plot'),
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
                 column(3, selectInput(inputId='docList', label='', choices=NULL, selected='')),
                 column(9, fluidRow(
                   actionButton(inputId='addDoc', label='Add Document', styleclass="primary", size="small"),
                   actionButton(inputId='deleteDoc', label='Delete Document', styleclass="danger", size="small")
                 ))
               )
               ),
             mainPanel(
               tabsetPanel(id='rmdTabs',
                 tabPanel('R_Markdown',
                          aceEditor('rmd', mode='markdown', value='')
                          ),
                 tabPanel('Preview',
                          uiOutput('rmdOutput')
                          )
                 )
               )
             )
           ),



  tags$head(tags$script(src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/jquery-ui.min.js"),
            tags$style(type='text/css', "button { margin-top: 10px; }"),
            tags$style(type='text/css', "#openSampleProj { margin-top: 30px; }")
            )


))

