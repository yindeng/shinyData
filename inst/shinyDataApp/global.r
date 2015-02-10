
## conditional calculated field: mutate and ddply; see documentation for ddply
## groups: use selectInput with multiple=TRUE and selectize = FALSE
## http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information

## GitHub Hosting example: https://gist.github.com/mattbrehmer/5645155
## Alternative to ggplot2: https://github.com/ramnathv/rCharts

#options(error = browser)
# NULL, browser, etc.
options(shiny.error=NULL)
# options(shiny.error=function() {
#   ## skip validation errors
#   if(!inherits(eval.parent(expression(e)), "validation")) browser()
# })
options(shiny.trace = FALSE)  # change to TRUE for trace
#options(shiny.reactlog=TRUE)
options(shiny.maxRequestSize = 100*1024^2)  # Set the upload limit to 100MB
## see https://groups.google.com/forum/#!topic/shiny-discuss/2wgIG3dOEZI

require(shiny); require(reshape); require(ggplot2); require(Hmisc); require(uuid); #require(plotly);
require(tables); require(tools); require(png); require(data.table); require(shinysky); require(Cairo)
require(knitr); require(rmarkdown); require(shinyAce)

options(shiny.usecairo=TRUE)


MoltenMeasuresName <- 'value'
lengthUnique <<- function(x) { length(unique(x)) }
YFunChoices <- c('Sum'='sum','Mean'='mean','Median'='median','Min'='min','Max'='max',
                 'Standard Deviation'='sd','Variance'='var','Count'='length', 'Count (Distinct)'='lengthUnique')
AggFunChoicesDimension <- c('Min'='min','Max'='max',
                            'Count'='length', 'Count (Distinct)'='lengthUnique')
InternalY <- '..y..'


GeomChoices <- c('Text'='text', 'Bar'='bar','Line'='line',
                 'Area'='area',  'Point'='point',
                 'Path'='path','Polygon'='polygon',
                 'Boxplot'='boxplot')
StatChoices <- c('Identity'='identity','Count'='bin','Summary'='summary','Boxplot'='boxplot')


getAesChoices <- function(geom, stat='identity'){
  switch(geom,
         'text'=switch(stat,
                     'bin'=list('Coordinates'=c('X'='aesX'),
                                'Common'=c('Label'='aesLabel','Color'='aesColor','Size'='aesSize',
                                           'Shape'='aesShape','Line Type'='aesLineType','Angle'='aesAngle'),
                                'Color'=c('Alpha'='aesAlpha'),
                                'Label'=c('Font Family'='aesFamily','Font Face'='aesFontface','Line Height'='aesLineheight'),
                                'Justification'=c('Horizontal Adjustment'='aesHjust','Vertical Adjustment'='aesVjust')
                     ),
                     'identity'=list('Coordinates'=c('X'='aesX','Y'='aesY'),
                                     'Common'=c('Label'='aesLabel','Color'='aesColor','Size'='aesSize',
                                                'Shape'='aesShape','Line Type'='aesLineType','Angle'='aesAngle'),
                                     'Color'=c('Alpha'='aesAlpha'),
                                     'Label'=c('Font Family'='aesFamily','Font Face'='aesFontface','Line Height'='aesLineheight'),
                                     'Justification'=c('Horizontal Adjustment'='aesHjust','Vertical Adjustment'='aesVjust')
                     )
        ),

        'bar'=switch(stat,
                     'bin'=list('Coordinates'=c('X'='aesX'),
                                'Common'=c('Color'='aesColor','Size'='aesSize',
                                           'Line Type'='aesLineType','Weight'='aesWeight'),
                                'Color'=c('Border Color'='aesBorderColor',
                                          'Alpha'='aesAlpha')
                     ),
                     'identity'=list('Coordinates'=c('X'='aesX','Y'='aesY'),
                                     'Common'=c('Color'='aesColor','Size'='aesSize',
                                                'Line Type'='aesLineType','Weight'='aesWeight'),
                                     'Color'=c('Border Color'='aesBorderColor',
                                               'Alpha'='aesAlpha')
                     )
        ),

        'line'=switch(stat,
                     'bin'=list('Coordinates'=c('X'='aesX'),
                                'Common'=c('Color'='aesColor','Size'='aesSize',
                                           'Line Type'='aesLineType',
                                           'Grouping'='aesGroup'),
                                'Color'=c('Alpha'='aesAlpha')
                     ),
                     'identity'=list('Coordinates'=c('X'='aesX','Y'='aesY'),
                                     'Common'=c('Color'='aesColor','Size'='aesSize',
                                                'Line Type'='aesLineType',
                                                'Grouping'='aesGroup'),
                                     'Color'=c('Alpha'='aesAlpha')
                     )
        ),

        'area'=switch(stat,
                     'bin'=list('Coordinates'=c('X'='aesX'),
                                'Common'=c('Color'='aesColor','Size'='aesSize',
                                           'Line Type'='aesLineType'),
                                'Color'=c('Border Color'='aesBorderColor',
                                          'Alpha'='aesAlpha')
                     ),
                     'identity'=list('Coordinates'=c('X'='aesX','Y'='aesY'),
                                     'Common'=c('Color'='aesColor','Size'='aesSize',
                                                'Line Type'='aesLineType'),
                                     'Color'=c('Border Color'='aesBorderColor',
                                               'Alpha'='aesAlpha')
                     )
        ),

        'point'=switch(stat,
                      'bin'=list('Coordinates'=c('X'='aesX'),
                                 'Common'=c('Color'='aesColor','Size'='aesSize',
                                            'Shape'='aesShape'),
                                 'Color'=c('Border Color'='aesBorderColor','Alpha'='aesAlpha')
                      ),
                      'identity'=list('Coordinates'=c('X'='aesX','Y'='aesY'),
                                      'Common'=c('Color'='aesColor','Size'='aesSize',
                                                 'Shape'='aesShape'),
                                      'Color'=c('Border Color'='aesBorderColor','Alpha'='aesAlpha')
                      )
        ),

        'boxplot'=switch(stat,
                         'boxplot'=list('Coordinates'=c('X'='aesX','Y'='aesY'),
                                        'Common'=c('Color'='aesColor','Size'='aesSize',
                                                   'Shape'='aesShape','Line Type'='aesLineType','Weight'='aesWeight'),
                                        'Color'=c('Border Color'='aesBorderColor',
                                                  'Alpha'='aesAlpha')
                         ),
                         'identity'=list('Coordinates'=c('X'='aesX','Y Middle'='aesMiddle',
                                                         'Y Lower'='aesLower','Y Upper'='aesUpper',
                                                         'Y Min'='aesYmin','Y Max'='aesYmax'),
                                         'Common'=c('Color'='aesColor','Size'='aesSize',
                                                    'Shape'='aesShape','Line Type'='aesLineType','Weight'='aesWeight'),
                                         'Color'=c('Border Color'='aesBorderColor',
                                                   'Alpha'='aesAlpha')
                         )
        )

  )
}

AesChoicesSimpleList <- unique(unlist(lapply(GeomChoices, getAesChoices), use.names=FALSE))

extrafontsImported <- (system.file("fontmap/fonttable.csv", package = "extrafontdb")!="")
FontFamilyChoices <- c("AvantGarde", "Bookman", "Courier", "Helvetica",
  "Helvetica-Narrow", "NewCenturySchoolbook", "Palatino", "Times")
if(extrafontsImported) FontFamilyChoices <- extrafont::fonts()


FontFaceChoices <- c("Plain"="plain","Bold"="bold","Italic"="italic","Bold & Italic"="bold.italic")


