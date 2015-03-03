
####################################################
## Reshaping data
####################################################


## Sheet input interdependence
observe({
  currentSheet <- projProperties[['activeSheet']]
  if(!isEmpty(currentSheet)){
    currentLayer <- sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']]
    if(!isEmpty(currentLayer)){
      markType <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['geom']]
      stat <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['statType']]
      isolate({
        # control stat choices
        sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['statChoices']] <<-
          StatChoices[sapply(StatChoices, function(n) !is.null(getAesChoices(geom=markType, stat=n)))]
        # control aesthetics choices
        sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesChoices']] <<-
          getAesChoices(markType, stat)
      })
    }

    cc <- sheetList[[currentSheet]][['dynamicProperties']][['columns']]
    rr <- sheetList[[currentSheet]][['dynamicProperties']][['rows']]
    mDat <- sheetList[[currentSheet]][['datR']]()
    fields <- sheetList[[currentSheet]][['fieldNames']]()
    measures <- sheetList[[currentSheet]][['measuresR']]()

    cc1 <- ''; rr1 <- ''; cChoices <- ''; rChoices <- ''
    outTable <- NULL; outDf <- NULL;
    cc.measures <- ''; cc.dims <- '';
    rr.measures <- ''; rr.dims <- ''
    if(!is.null(fields)){
      dims <- setdiff.c(fields, measures)
      cc1 <- intersect(cc,dims); rr1 <- intersect(rr,dims)
      #         cc.measures <- intersect(cc1,measures); rr.measures <- intersect(rr1,measures)
      #         cc.dims <- setdiff(cc1,cc.measures); rr.dims <- setdiff(rr1,rr.measures)

      #         cc1 <- c(cc.dims,cc.measures); rr1 <- c(rr.dims,rr.measures)

      all.x <- sapply(sheetList[[currentSheet]][['dynamicProperties']][['layerList']],
                      function(z) z[['aesList']][['aesX']][['aesField']])
      all.y <- sapply(sheetList[[currentSheet]][['dynamicProperties']][['layerList']],
                      function(z) z[['aesList']][['aesY']][['aesField']])

      cChoices <- setdiff.c(dims, c(all.x, all.y, rr1))
      rChoices <- setdiff.c(dims, c(all.x, all.y, cc1))
      if(!isEmpty(currentLayer)){
        isolate({
          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][['aesX']][['fieldChoices']] <<-
            setdiff.c(fields, c(rr1, cc1))
          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][['aesY']][['fieldChoices']] <<-
            setdiff.c(fields, c(rr1, cc1))
        })
      }


      if(!is.null(mDat)){
        outType <- ifnull(sheetList[[currentSheet]][['dynamicProperties']][['outputType']], 'plot')
        if(outType=='table'){
          #             if(combineMeasures && !is.null(facets)){
          #               outTable <- cast(mDat, facets, sum)
          #               outDf <- as.data.frame(outTable)
          #             }

        } else {

        }

        if(!is.null(outDf)){
          ## add to dataList
          isolate({
            sheetNameDat <- convertSheetNameToDatName(sheetList[[currentSheet]][['dynamicProperties']][['name']])
            if(is.null(datList[[currentSheet]])){
              datList[[currentSheet]] <<- createNewDatClassObj(outDf, name=sheetNameDat, type='sheet')
            } else {
              datList[[currentSheet]][['dynamicProperties']][['dat']] <<- outDf
              datList[[currentSheet]][['dynamicProperties']][['measures']] <<- intersect(datList[[currentSheet]][['dynamicProperties']][['measures']],
                                                                                         getDefaultMeasures(outDf))
              ## add new fields, delete outdated fields, leave common fields alone since they might have user customizations
              newFields <- getDefaultFieldsList(outDf)
              oldList <- names(datList[[currentSheet]][['dynamicProperties']][['fieldsList']])
              newList <- names(newFields)
              for(n in setdiff(newList, oldList)){
                datList[[currentSheet]][['dynamicProperties']][['fieldsList']][[n]] <<- newFields[[n]]
              }
              for(n in setdiff(oldList, newList)){
                datList[[currentSheet]][['dynamicProperties']][['fieldsList']][[n]] <<- NULL
              }
            }
          })
        }
      }

    }
    isolate({
      #       sheetList[[currentSheet]][['dynamicProperties']][['dat']] <<- mDat
      #       sheetList[[currentSheet]][['dynamicProperties']][['cMeasures']] <<- cc.measures
      #       sheetList[[currentSheet]][['dynamicProperties']][['cDims']] <<- cc.dims
      #       sheetList[[currentSheet]][['dynamicProperties']][['rMeasures']] <<- rr.measures
      #       sheetList[[currentSheet]][['dynamicProperties']][['rDims']] <<- rr.dims
      if(are.vectors.different(cc1, sheetList[[currentSheet]][['dynamicProperties']][['columns']])){
        triggerUpdateInput('sheetColumns')
      }
      if(are.vectors.different(rr1, sheetList[[currentSheet]][['dynamicProperties']][['rows']])){
        triggerUpdateInput('sheetRows')
      }
      sheetList[[currentSheet]][['dynamicProperties']][['columns']] <<- cc1
      sheetList[[currentSheet]][['dynamicProperties']][['rows']] <<- rr1
      sheetList[[currentSheet]][['dynamicProperties']][['colChoices']] <<- cChoices
      sheetList[[currentSheet]][['dynamicProperties']][['rowChoices']] <<- rChoices
      sheetList[[currentSheet]][['dynamicProperties']][['outputTable']] <<- outTable
      sheetList[[currentSheet]][['dynamicProperties']][['outputDataframe']] <<- outDf
    })
  }
}, priority=1)




## Switching sheet
observe({
  v <- input$sheetList
  isolate({
    if(!isEmpty(v)) projProperties[['activeSheet']] <<- v
  })

})
observe({
  updateInput[['activeSheet']]
  updateSelectInput(session, 'sheetList', choices=(sheetListNames()),
                    selected=isolate(projProperties[['activeSheet']]))
})


## modify sheet name
observe({
  v <- input$sheetName
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!isEmpty(currentSheet)){
      if(!isEmpty(v) && isEmpty(sheetListNames()[v])){
        ## the second condition makes sure v is different
        ## update doc's rmd
        oldName <- paste('`', sheetList[[currentSheet]][['dynamicProperties']][['name']], '`', sep='')
        newName <- paste('`', v, '`', sep='')
        sapply(names(docList), function(currentDoc){
          docList[[currentDoc]][['rmd']] <<- gsub(oldName, newName, docList[[currentDoc]][['rmd']], fixed=TRUE)
          NULL
        })
        triggerUpdateInput('docRmd')

        sheetList[[currentSheet]][['dynamicProperties']][['name']] <<- v
      }
    }
  })

})
observe({
  updateInput[['sheetName']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!isEmpty(currentSheet)){
    isolate(sheetList[[currentSheet]][['dynamicProperties']][['name']])
  } else ''
  updateTextInput(session, 'sheetName', value=null2String(s))
})

## link sheet name to corresponding data name
observe({
  currentSheet <- projProperties[['activeSheet']]
  if(!isEmpty(currentSheet)){
    s <- sheetList[[currentSheet]][['dynamicProperties']][['name']]
    isolate({
      if(!is.null(datList[[currentSheet]])){
        datList[[currentSheet]][['dynamicProperties']][['name']] <<- convertSheetNameToDatName(s)
        triggerUpdateInput('datName')
      }
    })
  }
})

## sheet control tab
observe({
  v <- input$sheetControlTab
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!isEmpty(currentSheet)) sheetList[[currentSheet]][['dynamicProperties']][['sheetControlTab']] <<- v
  })

})
observe({
  updateInput[['sheetControlTab']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!isEmpty(currentSheet)){
    isolate(sheetList[[currentSheet]][['dynamicProperties']][['sheetControlTab']])
  } else ''
  updateTabsetPanel(session, 'sheetControlTab', selected=ifempty(s, 'sheetTabType'))
})

## Selecting data for sheet
observe({
  v <- input$sheetDatList
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!isEmpty(currentSheet)) sheetList[[currentSheet]][['dynamicProperties']][['datId']] <<- v
  })

})
observe({
  updateInput[['sheetDatId']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!isEmpty(currentSheet)){
    isolate(sheetList[[currentSheet]][['dynamicProperties']][['datId']])
  } else ''
  choices <- datListNames()
  choices <- choices[!sapply(choices, isDatBasedonSheet, sheetId=currentSheet)]
  updateSelectInput(session, 'sheetDatList', choices=null2String(choices),
                    selected=null2String(s))
})

## whether use molten data?
observe({
  v <- input$combineMeasures
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!isEmpty(currentSheet)){
      sheetList[[currentSheet]][['dynamicProperties']][['combineMeasures']] <<- as.logical(v)
    }
  })

})
observe({
  updateInput[['combineMeasures']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!isEmpty(currentSheet)){
    isolate(sheetList[[currentSheet]][['dynamicProperties']][['combineMeasures']])
  } else FALSE
  updateCheckboxInput(session, 'combineMeasures', value=null2String(s))
})

## Manipulating columns
observe({
  v <- input$columns
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!isEmpty(currentSheet)) sheetList[[currentSheet]][['dynamicProperties']][['columns']] <<- v
  })

})
observe({
  updateInput[['sheetColumns']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!isEmpty(currentSheet)){
    isolate(sheetList[[currentSheet]][['dynamicProperties']][['columns']])
  } else ''
  choices <- if(!isEmpty(currentSheet)){sheetList[[currentSheet]][['dynamicProperties']][['colChoices']]} else ''
  updateSelectizeInput(session, 'columns', choices=null2String(choices), selected=null2String(s))
})

## Manipulating rows
observe({
  v <- input$rows
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!isEmpty(currentSheet)) sheetList[[currentSheet]][['dynamicProperties']][['rows']] <<- v
  })

})
observe({
  updateInput[['sheetRows']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!isEmpty(currentSheet)){
    isolate(sheetList[[currentSheet]][['dynamicProperties']][['rows']])
  } else ''
  choices <- if(!isEmpty(currentSheet)){sheetList[[currentSheet]][['dynamicProperties']][['rowChoices']]} else ''
  updateSelectizeInput(session, 'rows', choices=null2String(choices), selected=null2String(s))
})

## Manipulating output type
observe({
  v <- input$outputTypeList
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!isEmpty(currentSheet)) sheetList[[currentSheet]][['dynamicProperties']][['outputType']] <<- v
  })

})
observe({
  updateInput[['sheetOutput']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!isEmpty(currentSheet)){
    isolate(sheetList[[currentSheet]][['dynamicProperties']][['outputType']])
  } else ''
  updateSelectInput(session, 'outputTypeList', selected=null2String(s))
})

## Selecting ggplot layer for sheet
observe({
  v <- input$layerList
  isolate({
    if(!isEmpty(v)){
      currentSheet <- (projProperties[['activeSheet']])
      if(!isEmpty(currentSheet)) sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']] <<- v
    }
  })
})
observe({
  updateInput[['sheetPlotLayer']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!isEmpty(currentSheet)){
    isolate(sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
  } else ''
  choices <- if(!isEmpty(currentSheet)) sheetList[[currentSheet]][['layerNames']]()
  updateSelectInput(session, 'layerList', choices=null2String(choices),
                    selected=null2String(s))
})

## Selecting aesthetic
observe({
  v <- input$aesList
  isolate({
    if(!isEmpty(v)){
      currentSheet <- (projProperties[['activeSheet']])
      if(!isEmpty(currentSheet)) {
        currentLayer <- sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']]
        if(!isEmpty(currentLayer)){
          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']] <<- v
        }
      }
    }
  })
})
observe({
  updateInput[['sheetLayerAes']]
  currentSheet <- projProperties[['activeSheet']]
  s <- choices <- ''
  if(!isEmpty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!isEmpty(currentLayer)){
      s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']])
      choices <- (sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesChoices']])
    }
  }

  updateSelectInput(session, 'aesList', choices=null2String(choices),
                    selected=null2String(s))
})

## set aes map or set option
observe({
  v <- input$aesMapOrSet
  if(!is.null(v)){
    isolate({
      currentSheet <- (projProperties[['activeSheet']])
      if(!isEmpty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!isEmpty(currentLayer)){
          currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
          if(!isEmpty(currentAes)){
            sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesMapOrSet']] <<- v

            ## set default value
            if(v=='set' && isEmpty(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesValue']])){
              sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesValue']] <<-
                switch(currentAes,
                       'aesLabel'='My Label', 'aesFamily'='Times', 'aesFontface'='plain',
                       'aesColor'=, 'aesBorderColor'='',
                       'aesSize'=8, 'aesLineheight'=1,
                       0
                )

              triggerUpdateInput('aesValue')
            }
          }
        }
      }
    })
  }

})
observe({
  updateInput[['aesMapOrSet']]
  currentSheet <- (projProperties[['activeSheet']])
  s <- 'map'
  if(!isEmpty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!isEmpty(currentLayer)){
      currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
      if(!isEmpty(currentAes)){
        s1 <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesMapOrSet']])
        if(!are.vectors.different('set', s1)) s <- 'set'
      }
    }
  }
  updateRadioButtons(session, 'aesMapOrSet', selected=s)
})

## map or set UI
output$mapOrSetUI <- renderUI({
  currentSheet <- (projProperties[['activeSheet']])
  if(!isEmpty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!isEmpty(currentLayer)){
      currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
      if(!isEmpty(currentAes)){
        useMapping <- are.vectors.different('set',
                                            sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesMapOrSet']])
        isolate({
          aes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]]
          if(useMapping){
            s <- null2String(aes[['aesField']])
            choices <- if(currentAes %in% c('aesX','aesY')) {
              aes[['fieldChoices']]
            } else {
              sheetList[[currentSheet]][['fieldNames']]()
            }
            if(!(s %in% choices)) choices[s]=s ## this is needed when s="", otherwise selected will defaults to the first value

            aggFun <- null2String(aes[['aesAggFun']])
            aggFunchoices <- YFunChoices
            if(!(aggFun %in% aggFunchoices)) aggFunchoices[aggFun]=aggFun

            list(
              fluidRow(
                column(4,
                       selectizeInput(inputId='aesField', label='Field',
                                      choices=choices, selected=s, multiple=FALSE,
                                      options = list(create = TRUE))
                ),
                column(4,
                       checkboxInput(inputId='aesAggregate', label='Aggregate Field',
                                     value=aes[['aesAggregate']]),
                       conditionalPanel('input.aesAggregate==true',
                                        selectizeInput(inputId='aesAggFun', label='By',
                                                       choices=aggFunchoices,
                                                       selected=aggFun, multiple=FALSE,
                                                       options = list(create = TRUE))
                       )

                ),
                column(4,
                       conditionalPanel('output.canAesFieldBeContinuous==true',
                                        radioButtons('aesDiscrete', 'Treat Field as',
                                                     choices=c('Continuous'='continuous',
                                                               'Discrete'='discrete'),
                                                     selected=ifelse(aes[['aesDiscrete']], 'discrete', 'continuous'),
                                                     inline=FALSE)
                       )


                )
              ),

              tags$hr(),

              switch(currentAes,
                     'aesShape'=checkboxInput('shapeSolid', 'Solid Shapes', value=empty2TRUE(aes[['scale']][['shapeSolid']])),
                     'aesSize'=sliderInput('sizeRange', 'Size Range', min=0, max=50, value=ifempty(aes[['scale']][['sizeRange']], c(1,6))),
                     'aesBorderColor'=, 'aesColor'=list(
                       h4('Color Specification'),
                       conditionalPanel('input.aesDiscrete==null || input.aesDiscrete=="discrete"',
                                        selectInput('discreteColorScaleType', 'Type of Scale',
                                                    selected=aes[['scale']][['discreteColorScaleType']],
                                                    choices=c('Default'='', 'Hue'='hue', 'Brewer'='brewer')),
                                        selectInput('colorBrewerPallete', 'Palette',
                                                    selected=aes[['scale']][['colorBrewerPallete']],
                                                    choices=BrewerPaletteChoices)
                                      ),
                       conditionalPanel('input.aesDiscrete=="continuous"',
                                        checkboxInput('colorDiverging', 'Diverging Colors', value=aes[['scale']][['colorDiverging']]),
                                        fluidRow(column(4,
                                                        colorInput('colorLow', 'Low', value=aes[['scale']][['colorLow']])),
                                                 column(4,
                                                        conditionalPanel('input.colorDiverging == true',
                                                                         colorInput('colorMid', 'Middle', value=aes[['scale']][['colorMid']]),
                                                                         numericInput('colorMidpoint', 'Scale Center',
                                                                                      value=aes[['scale']][['colorMidpoint']]))),
                                                 column(4, colorInput('colorHigh', 'High', value=aes[['scale']][['colorHigh']]),
                                                        colorInput('colorNA_value', 'Missing Data', value=aes[['scale']][['colorNA_value']]))
                                                 )
                       )
                      )

              ),
              if(currentAes != 'aesX' && currentAes != 'aesY' && currentAes != 'aesGroup'){
                list(
                  radioButtons('legendType', 'Legend:', selected=aes[['scale']][['legendType']],
                               choices=c('Default'='','Custom'='custom','Hide'='none'), inline=TRUE),
                  conditionalPanel('input.legendType != "none"',
                                   textInput('legendTitle', 'Legend Title', value=aes[['scale']][['legendTitle']])),
                  conditionalPanel('input.legendType == "custom"',
                                   fluidRow(
                                    column(6,
                                           selectInput('title_position', 'Title Position', selected=aes[['scale']][['legend']][['title_position']],
                                                       choices=c('Default'='', 'Top'='top', 'Bottom'='bottom', 'Left'='left', 'Right'='right')),
                                           selectInput('direction', 'Layout Direction', selected=aes[['scale']][['legend']][['direction']],
                                                       choices=c('Default'='', 'Horizontal'='horizontal', 'Vertical'='vertical')),
                                           checkboxInput('reverse', 'Reverse', value=empty2FALSE(aes[['scale']][['legend']][['reverse']]))
                                           ),
                                    column(6,
                                           selectInput('label_position', 'Label Position', selected=aes[['scale']][['legend']][['label_position']],
                                                       choices=getLegendLabelPositionChoices(aes[['scale']][['legend']][['direction']])),
                                           conditionalPanel('input.direction=="horizontal"',
                                                            numericInput('label_hjust', 'Label Horizontal Adjust',
                                                                         value=aes[['scale']][['legend']][['label_hjust']])),
                                           conditionalPanel('input.direction=="" || input.direction=="vertical"',
                                                            numericInput('label_vjust', 'Label Vertical Adjust',
                                                                         value=aes[['scale']][['legend']][['label_vjust']]))
                                           )
                                    )
                                   )
                )
              }
            )

          } else {
            switch(currentAes,
                   'aesLabel'=textInput('aesValue', '', value=aes[['aesValue']]),
                   'aesFontface'=selectInput('aesValue','',choices=FontFaceChoices, selected=aes[['aesValue']]),
                   'aesFamily'=selectInput('aesValue','',choices=FontFamilyChoices, selected=aes[['aesValue']]),
                   'aesColor'=, 'aesBorderColor'=colorInput('aesValue', '', value=aes[['aesValue']]),
                   'aesShape'=, 'aesLineType'=numericInput('aesValue', 'Value', value=aes[['aesValue']], step=1),
                   numericInput('aesValue', 'Value', value=aes[['aesValue']], step=0.1)
            )
          }
        })
      }
    }
  }
})


## upsert basic aes properties
lapply(list(list(inputId='aesValue', inputType=''),
            list(inputId='aesAggregate', inputType=''),
            list(inputId='aesAggFun', inputType='')),
       function(x){
         assign(paste0('observer_', x$inputId, '_push'),
                observeEvent(input[[x$inputId]], {
                  v <- input[[x$inputId]]
                  currentSheet <- (projProperties[['activeSheet']])
                  if(!isEmpty(currentSheet)){
                    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
                    if(!isEmpty(currentLayer)){
                      currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
                      if(!isEmpty(currentAes)){
                        if(are.vectors.different(v,
                                                 sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][[x$inputId]]))
                          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][[x$inputId]] <<- v
                      }
                    }
                  }
                }),
                sessionEnv)

       })

## upsert aes scales
lapply(list(list(inputId='legendType', inputType='', storage='scale'),
            list(inputId='legendTitle', inputType='text', storage='scale'),
            list(inputId='discreteColorScaleType', inputType='', storage='scale'),
            list(inputId='colorBrewerPallete', inputType='', storage='scale'),
            list(inputId='shapeSolid', inputType='', storage='scale'),
            list(inputId='sizeRange', inputType='', storage='scale'),
            list(inputId='colorDiverging', inputType='', storage='scale'),
            list(inputId='colorLow', inputType='', storage='scale'),
            list(inputId='colorMid', inputType='', storage='scale'),
            list(inputId='colorMidpoint', inputType='', storage='scale'),
            list(inputId='colorHigh', inputType='', storage='scale'),
            list(inputId='colorNA_value', inputType='', storage='scale'),
            list(inputId='title_position', inputType='', storage='legend'),
            list(inputId='direction', inputType='', storage='legend'),
            list(inputId='reverse', inputType='', storage='legend'),
            list(inputId='label_position', inputType='select', storage='legend'),
            list(inputId='label_hjust', inputType='', storage='legend'),
            list(inputId='label_vjust', inputType='', storage='legend')
            ),
       function(x){
         push.label <- paste0('observer_', x$inputId, '_push')
         assign(push.label,
                observeEvent(input[[x$inputId]], label=push.label, {
                  v <- input[[x$inputId]]
                  currentSheet <- (projProperties[['activeSheet']])
                  if(!isEmpty(currentSheet)){
                    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
                    if(!isEmpty(currentLayer)){
                      currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
                      if(!isEmpty(currentAes)){
                        if(is.null(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['scale']]))
                          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['scale']] <<- list()
                        if(x$storage=='scale'){
                          if(are.vectors.different(v,
                              sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][[
                                'aesList']][[currentAes]][['scale']][[x$inputId]]))
                            sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][[
                              'aesList']][[currentAes]][['scale']][[x$inputId]] <<- v
                        } else {  # legend
                          if(is.null(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][[
                            'aesList']][[currentAes]][['scale']][['legend']]))
                            sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][[
                              'aesList']][[currentAes]][['scale']][['legend']] <<- list()
                          if(are.vectors.different(v,
                                                   sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][[
                                                     'aesList']][[currentAes]][['scale']][['legend']][[x$inputId]]))
                            sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][[
                              'aesList']][[currentAes]][['scale']][['legend']][[x$inputId]] <<- v

                          if(x$inputId=='direction'){
                            label.pos <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][[
                              'aesList']][[currentAes]][['scale']][['legend']][['label_position']]
                            choices <- getLegendLabelPositionChoices(v)
                            if(!isEmpty(label.pos) && !label.pos %in% choices){
                              label.pos <- choices[1]
                              sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][[
                                'aesList']][[currentAes]][['scale']][['legend']][['label_position']] <<- label.pos
                            }
                            updateSelectInput(session, 'label_position', selected=label.pos, choices=choices)
                          }
                        }


                      }
                    }
                  }
                }),
                sessionEnv)

         if(isEmpty(x$inputType)) return()
         assign(paste0('observer_', x$inputId, '_pull'),
                observeEvent(updateInput[[x$inputId]], {
                  currentSheet <- projProperties[['activeSheet']]
                  s <- ''
                  if(!isEmpty(currentSheet)){
                    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
                    if(!isEmpty(currentLayer)){
                      currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
                      if(!isEmpty(currentAes)){
                        scales <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['scale']]
                        s <- if(x$storage=='scale') scales[[x$inputId]] else scales[['legend']][[x$inputId]]
                      }
                    }
                  }

                  updateInput(session, x$inputType, x$inputId, s)

                }),
                sessionEnv)
       })

## set aes field
observe({
  v <- input$aesField
  if(!is.null(v)){
    isolate({
      currentSheet <- (projProperties[['activeSheet']])
      if(!isEmpty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!isEmpty(currentLayer)){
          currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
          if(!isEmpty(currentAes)){
            if(are.vectors.different(v, sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesField']])){
              sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesField']] <<- v

              ## set default field properties
              if(isEmpty(v) && currentLayer != 'Plot'){
                v <- null2String(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][['Plot']][['aesList']][[currentAes]][['aesField']])
              }
              is.measure <- v %in% sheetList[[currentSheet]][['measuresR']]()
              statType <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['statType']]
              notAggregate <- are.vectors.different(statType, 'identity') && (currentAes=='aesX' || currentAes=='aesY')
              sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesIsFieldMeasure']] <<-
                is.measure  ## just for capturing this information to customize choices for agg fun
              sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[
                currentLayer]][['aesList']][[currentAes]][['aesAggregate']] <<- is.measure && (!notAggregate)
              sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesAggFun']] <<- if(is.measure) 'sum' else 'length'
              sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesDiscrete']] <<- !is.measure
              sapply(c('aesAggregate','aesAggFun','aesDiscrete'), triggerUpdateInput)

              ## set default xlab, ylab
              if(!isEmpty(v)){
                fieldNames <- sheetList[[currentSheet]][['fieldNames']]()
                i.match <- match(v, fieldNames)
                if(!is.na(i.match)){
                  fieldName <- names(fieldNames)[i.match]
                  switch(currentAes,
                         'aesX'={
                           sheetList[[currentSheet]][['dynamicProperties']][['plotXlab']] <<- fieldName
                           triggerUpdateInput('plotXlab')
                         },
                         'aesY'={
                           sheetList[[currentSheet]][['dynamicProperties']][['plotYlab']] <<- fieldName
                           triggerUpdateInput('plotYlab')
                         },
{
  if(is.null(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['scale']]))
    sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['scale']] <<- list()
  sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['scale']][['legendTitle']] <<- fieldName
  triggerUpdateInput('legendTitle')
}
                         )

                }
              }

            }
          }
        }
      }
    })
  }

})


output$canAesFieldBeContinuous <- reactive({
  ans <- FALSE
  currentSheet <- (projProperties[['activeSheet']])
  if(!isEmpty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!isEmpty(currentLayer)){
      currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
      if(!isEmpty(currentAes)){
        ans <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['canFieldBeContinuous']]()
      }
    }
  }
  ans
})
outputOptions(output, "canAesFieldBeContinuous", suspendWhenHidden=FALSE)

## set aes aggregate
observe({
  updateInput[['aesAggregate']]
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    s <- FALSE
    if(!isEmpty(currentSheet)){
      currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
      if(!isEmpty(currentLayer)){
        currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
        if(!isEmpty(currentAes)){
          s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesAggregate']])

        }
      }
    }
    updateCheckboxInput(session, 'aesAggregate', value=s)
  })

})


## set aes agg fun
observe({
  updateInput[['aesAggFun']]
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    s <- ''
    choices <- YFunChoices
    if(!isEmpty(currentSheet)){
      currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
      if(!isEmpty(currentLayer)){
        currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
        if(!isEmpty(currentAes)){
          s <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesAggFun']]
          is.measure <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesIsFieldMeasure']]
          if(!is.null(is.measure) && !is.measure) choices <- AggFunChoicesDimension
        }
      }
    }

    if(!isEmpty(s) && !(s %in% choices)) choices[s]=s
    updateSelectizeInput(session, 'aesAggFun', choices=null2String(choices), selected=null2String(s))
  })
})

## set aes discrete
observe({
  v <- input$aesDiscrete
  if(!is.null(v)){
    isolate({
      currentSheet <- (projProperties[['activeSheet']])
      if(!isEmpty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!isEmpty(currentLayer)){
          currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
          if(!isEmpty(currentAes)){
            isDiscrete <- (v=='discrete')
            if(currentAes %in% c('aesX', 'aesY')){
              ## can't mix discrete with continuous scales on x or y, so need to keep all layers the same
              for(layer in names(sheetList[[currentSheet]][['dynamicProperties']][['layerList']])){
                sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[layer]][['aesList']][[currentAes]][['aesDiscrete']] <<- isDiscrete
              }
            } else {
              sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesDiscrete']] <<- isDiscrete
            }
          }
        }
      }
    })
  }

})
observe({
  updateInput[['aesDiscrete']]
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    s <- 'discrete'
    if(!isEmpty(currentSheet)){
      currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
      if(!isEmpty(currentLayer)){
        currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
        if(!isEmpty(currentAes)){
          d <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesDiscrete']])
          if(!isEmpty(d) && !d){
            s <- 'continuous'
          }

        }
      }
    }
    updateRadioButtons(session, 'aesDiscrete', selected=s)
  })
})

setStatType <- function(currentSheet, currentLayer, value){
  sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['statType']] <<- value
  if(!isEmpty(value) && value!='identity'){
    ## no aggregation on x, y when stat is not identity
    sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][['aesX']][['aesAggregate']] <<- FALSE
    sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][['aesY']][['aesAggregate']] <<- FALSE
    triggerUpdateInput('aesAggregate')
  }
}

## set Mark Type / geom
observe({
  v <- input$markList
  isolate({
    if(!isEmpty(v)){
      currentSheet <- (projProperties[['activeSheet']])
      if(!isEmpty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!isEmpty(currentLayer)){
          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['geom']] <<- v
          # set default stat type & position
          setStatType(currentSheet, currentLayer, switch(v, 'boxplot'='boxplot', 'density'='density', 'smooth'='smooth', 'identity'))
          triggerUpdateInput('layerStatType')
          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['layerPositionType']] <<-
            switch(v, 'bar'='dodge', 'identity')
          triggerUpdateInput('layerPositionType')
        }
      }
    }
  })
})
observe({
  updateInput[['layerGeom']]
  currentSheet <- (projProperties[['activeSheet']])
  s <- choices <- ''
  if(!isEmpty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!isEmpty(currentLayer)){
      s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['geom']])
      #choices <- (sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['geomChoices']])
    }
  }
  updateSelectInput(session, 'markList', selected=null2String(s))
})

## set Stat Type
observe({
  v <- input$statTypeList
  isolate({
    if(!isEmpty(v)){
      currentSheet <- (projProperties[['activeSheet']])
      if(!isEmpty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!isEmpty(currentLayer)){
          setStatType(currentSheet, currentLayer, v)
        }
      }
    }
  })
})
observe({
  updateInput[['layerStatType']]
  currentSheet <- (projProperties[['activeSheet']])
  s <- choices <- ''
  if(!isEmpty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!isEmpty(currentLayer)){
      s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['statType']])
      choices <- (sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['statChoices']])
    }
  }

  updateSelectInput(session, 'statTypeList', choices=null2String(choices), selected=null2String(s))
})

## set y fun
observe({
  v <- input$yFunList
  isolate({
    if(!isEmpty(v)){
      currentSheet <- (projProperties[['activeSheet']])
      if(!isEmpty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!isEmpty(currentLayer)){
          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['yFun']] <<- v
        }
      }
    }
  })
})
observe({
  updateInput[['layerYFun']]
  currentSheet <- (projProperties[['activeSheet']])
  s <- ''
  if(!isEmpty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!isEmpty(currentLayer)){
      s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['yFun']])
    }
  }
  choices <- YFunChoices
  if(!isEmpty(s) && !(s %in% choices)) choices[s]=s
  updateSelectizeInput(session, 'yFunList', choices=null2String(choices), selected=null2String(s))
})



## set PositionType
observe({
  v <- input$layerPositionType
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!isEmpty(currentSheet)){
      currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
      if(!isEmpty(currentLayer)){
        sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['layerPositionType']] <<- v
      }
    }
  })

})
observe({
  updateInput[['layerPositionType']]
  currentSheet <- (projProperties[['activeSheet']])
  s <- ''
  if(!isEmpty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!isEmpty(currentLayer)){
      s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['layerPositionType']])
    }
  }
  updateSelectizeInput(session, 'layerPositionType', selected=null2String(s))
})

## set Position Height
observe({
  v <- empty2NULL(as.numeric(input$layerPositionHeight))
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!isEmpty(currentSheet)){
      currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
      if(!isEmpty(currentLayer)){
        sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['layerPositionHeight']] <<- v
      }
    }
  })

})
observe({
  updateInput[['layerPositionHeight']]
  currentSheet <- (projProperties[['activeSheet']])
  s <- ''
  if(!isEmpty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!isEmpty(currentLayer)){
      s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['layerPositionHeight']])
    }
  }
  updateTextInput(session, 'layerPositionHeight', value=null2String(s))
})

## set Position Width
observe({
  v <- empty2NULL(as.numeric(input$layerPositionWidth))
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!isEmpty(currentSheet)){
      currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
      if(!isEmpty(currentLayer)){
        sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['layerPositionWidth']] <<- v
      }
    }
  })

})
observe({
  updateInput[['layerPositionWidth']]
  currentSheet <- (projProperties[['activeSheet']])
  s <- ''
  if(!isEmpty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!isEmpty(currentLayer)){
      s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['layerPositionWidth']])
    }
  }
  updateTextInput(session, 'layerPositionWidth', value=null2String(s))
})




tabS <- tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
                   (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )
canLatexPNG <- tryCatch(length(make.png(tabS)), error=function(e) FALSE)


output$sheetOutput <- renderUI({
  currentSheet <- projProperties[['activeSheet']]
  if(!isEmpty(currentSheet)){
    switch(null2String(sheetList[[currentSheet]][['dynamicProperties']][['outputType']]),
           'table'=if(canLatexPNG) imageOutput('sheetOutputTable') else {
             tags$div(
               HTML(paste(capture.output(Hmisc::html(sheetList[[currentSheet]][['tableR']]())), collapse=" "))
             )
           },
           plotOutput('ggplot'))
  }

})


output$sheetOutputTable <- renderImage(if(canLatexPNG) {
  currentSheet <- projProperties[['activeSheet']]
  if(!isEmpty(currentSheet)){
    tab <- sheetList[[currentSheet]][['tableR']]()

    if(!is.null(tab)){
      #width  <- session$clientData$output_test_width
      #height <- session$clientData$output_test_height

      # For high-res displays, this will be greater than 1
      pixelratio <- session$clientData$pixelratio
      fileName <- make.png(tab, resolution=72*pixelratio)
      pngFile <- readPNG(fileName)

      # Return a list containing the filename
      list(src = normalizePath(fileName),
           width = dim(pngFile)[2],
           height = dim(pngFile)[1],
           alt = "Output not available")
    }
  }
}, deleteFile = TRUE)

# x <- tabular( (Species + 1) ~ (n=1) + Format(digits=3)*
#                 (Sepal.Length + Sepal.Width)*(mean + Justify(r)*sd), data=iris )
# tags$div(
#   HTML(paste(capture.output(Hmisc::html(x)), collapse=" "))
# )

## Reshaped output
output$reshapedDat <- renderTable({
  currentSheet <- projProperties[['activeSheet']]
  if(!isEmpty(currentSheet)){sheetList[[currentSheet]][['dynamicProperties']][['outputTable']]}
})

output$ggplot <- renderPlot({
  if(input$autoRefresh=='refresh'){
    currentSheet <- projProperties[['activeSheet']]
    if(!isEmpty(currentSheet)){
      sheetList[[currentSheet]][['plotR']]()
    }
  } else {
    gg <- last_plot()
    if(!is.null(gg)) gg <- gg  + theme_grey()
    gg
  }

})



## add Layer
observe({
  v <- input$addLayer
  isolate({
    if(v){
      currentSheet <- (projProperties[['activeSheet']])
      if(!isEmpty(currentSheet)){
        existingNames <- names(sheetList[[currentSheet]][['dynamicProperties']][['layerList']])
        layerName <- make.unique(c(existingNames, 'Overlay'), sep='_')[length(existingNames)+1]

        newLayer <- createNewLayer()
        sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[layerName]] <<- newLayer
        sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']] <<- layerName

        #copy from Plot layer
        plotLayer <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][['Plot']]
        names1 <- names(plotLayer)
        for(n1 in names1){
          if(n1=='aesList'){
            names2 <- names(plotLayer[[n1]])
            for(n2 in names2){
              names3 <- names(plotLayer[[n1]][[n2]])
              for(n3 in names3){
                if(n3 != 'aesField' && typeof(plotLayer[[n1]][[n2]][[n3]]) != 'closure'){#
                  sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[layerName]][[n1]][[n2]][[n3]] <<- plotLayer[[n1]][[n2]][[n3]]
                }
              }
              setAesReactives(currentSheet, layerName, n2)
            }
          } else {
            sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[layerName]][[n1]] <<- plotLayer[[n1]]
          }
        }

      }
    }
  })
})
## delete Layer
observe({
  v <- input$deleteLayer
  isolate({
    if(v){
      currentSheet <- (projProperties[['activeSheet']])
      if(!isEmpty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!isEmpty(currentLayer) && currentLayer!='Plot'){
          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]] <<- NULL
          sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']] <<- 'Plot'
        }
      }
    }
  })
})
## bring Layer to top
observe({
  v <- input$bringToTop
  isolate({
    if(v){
      currentSheet <- (projProperties[['activeSheet']])
      if(!isEmpty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!isEmpty(currentLayer)){
          temp <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]]
          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]] <<- NULL
          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]] <<- temp
        }
      }
    }
  })
})

## add sheet
observe({
  v <- input$addSheet
  isolate({
    if(v){
      addSheet()
    }
  })
})
## delete sheet
observe({
  v <- input$deleteSheet
  isolate({
    if(v){
      currentSheet <- (projProperties[['activeSheet']])
      if(!isEmpty(currentSheet)){
        sheets <- names(sheetList)
        i <- match(currentSheet, sheets)
        sheetList[[currentSheet]] <<- NULL
        projProperties[['activeSheet']] <<- ifelse(length(sheets)>i, sheets[i+1],
                                                   ifelse(i>1, sheets[i-1], ''))
      }
    }
  })
})

