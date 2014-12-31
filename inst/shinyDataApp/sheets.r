
####################################################
## Reshaping data
####################################################


## Sheet input interdependence
observe({
  currentSheet <- projProperties[['activeSheet']]
  if(!is.empty(currentSheet)){    
    currentLayer <- sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']]
    if(!is.empty(currentLayer)){
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
      if(!is.empty(currentLayer)){
        isolate({
          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][['aesX']][['fieldChoices']] <<- 
            setdiff.c(fields, c(rr1, cc1))
          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][['aesY']][['fieldChoices']] <<- 
            setdiff.c(fields, c(rr1, cc1))
        })
      }
      
      
      if(!is.null(mDat)){
        outType <- (sheetList[[currentSheet]][['dynamicProperties']][['outputType']])
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
    if(!is.empty(v)) projProperties[['activeSheet']] <<- v
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
    if(!is.empty(currentSheet)){
      if(!is.empty(v) && is.empty(isolate(sheetListNames())[v])){
        sheetList[[currentSheet]][['dynamicProperties']][['name']] <<- v
      } 
    }    
  })
  
})
observe({
  updateInput[['sheetName']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!is.empty(currentSheet)){
    isolate(sheetList[[currentSheet]][['dynamicProperties']][['name']])
  } else ''
  updateTextInput(session, 'sheetName', value=null2String(s))
})

## link sheet name to corresponding data name
observe({
  currentSheet <- projProperties[['activeSheet']]
  if(!is.empty(currentSheet)){
    s <- sheetList[[currentSheet]][['dynamicProperties']][['name']]
    isolate({
      if(!is.null(datList[[currentSheet]])){
        datList[[currentSheet]][['dynamicProperties']][['name']] <<- convertSheetNameToDatName(s)
        triggerUpdateInput('datName')
      }
    })      
  }
})

## Selecting data for sheet
observe({
  v <- input$sheetDatList
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!is.empty(currentSheet)) sheetList[[currentSheet]][['dynamicProperties']][['datId']] <<- v
  })
  
})
observe({
  updateInput[['sheetDatId']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!is.empty(currentSheet)){
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
    if(!is.empty(currentSheet)){
      sheetList[[currentSheet]][['dynamicProperties']][['combineMeasures']] <<- as.logical(v)
    } 
  })
  
})
observe({
  updateInput[['combineMeasures']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!is.empty(currentSheet)){
    isolate(sheetList[[currentSheet]][['dynamicProperties']][['combineMeasures']])
  } else F
  updateCheckboxInput(session, 'combineMeasures', value=null2String(s))
})

## Manipulating columns
observe({
  v <- input$columns
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!is.empty(currentSheet)) sheetList[[currentSheet]][['dynamicProperties']][['columns']] <<- v
  })
  
})
observe({
  updateInput[['sheetColumns']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!is.empty(currentSheet)){
    isolate(sheetList[[currentSheet]][['dynamicProperties']][['columns']])
  } else ''
  choices <- if(!is.empty(currentSheet)){sheetList[[currentSheet]][['dynamicProperties']][['colChoices']]} else ''   
  updateSelectizeInput(session, 'columns', choices=null2String(choices), selected=null2String(s))
})

## Manipulating rows
observe({
  v <- input$rows
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!is.empty(currentSheet)) sheetList[[currentSheet]][['dynamicProperties']][['rows']] <<- v
  })
  
})
observe({
  updateInput[['sheetRows']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!is.empty(currentSheet)){
    isolate(sheetList[[currentSheet]][['dynamicProperties']][['rows']])
  } else ''
  choices <- if(!is.empty(currentSheet)){sheetList[[currentSheet]][['dynamicProperties']][['rowChoices']]} else ''
  updateSelectizeInput(session, 'rows', choices=null2String(choices), selected=null2String(s))
})  

## Manipulating output type
observe({
  v <- input$outputTypeList
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!is.empty(currentSheet)) sheetList[[currentSheet]][['dynamicProperties']][['outputType']] <<- v
  })
  
})
observe({
  updateInput[['sheetOutput']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!is.empty(currentSheet)){
    isolate(sheetList[[currentSheet]][['dynamicProperties']][['outputType']])
  } else ''
  updateSelectInput(session, 'outputTypeList', selected=null2String(s))
})  

## Selecting ggplot layer for sheet
observe({
  v <- input$layerList
  isolate({
    if(!is.empty(v)){
      currentSheet <- (projProperties[['activeSheet']])
      if(!is.empty(currentSheet)) sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']] <<- v
    }    
  })  
})
observe({
  updateInput[['sheetPlotLayer']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!is.empty(currentSheet)){
    isolate(sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
  } else ''
  choices <- if(!is.empty(currentSheet)) sheetList[[currentSheet]][['layerNames']]()
  updateSelectInput(session, 'layerList', choices=null2String(choices), 
                    selected=null2String(s))
})

## Selecting aesthetic
observe({
  v <- input$aesList
  isolate({
    if(!is.empty(v)){
      currentSheet <- (projProperties[['activeSheet']])
      if(!is.empty(currentSheet)) {
        currentLayer <- sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']]
        if(!is.empty(currentLayer)){
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
  if(!is.empty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!is.empty(currentLayer)){
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
      if(!is.empty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!is.empty(currentLayer)){
          currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
          if(!is.empty(currentAes)){
            sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesMapOrSet']] <<- v
            
            ## set default value
            if(v=='set' && is.empty(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesValue']])){
              sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesValue']] <<- 
                switch(currentAes,
                       'aesLabel'='My Label', 'aesFamily'='Times', 'aesFontface'='plain',
                       'aesColor'=, 'aesBorderColor'='darkblue',
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
  if(!is.empty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!is.empty(currentLayer)){
      currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
      if(!is.empty(currentAes)){
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
  if(!is.empty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!is.empty(currentLayer)){
      currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
      if(!is.empty(currentAes)){
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
            
            fluidRow(
              column(4,
                     selectizeInput(inputId='aesField', label='Field', 
                                    choices=choices, selected=s, multiple=F,
                                    options = list(create = TRUE))  
              ),
              column(4, 
                     checkboxInput(inputId='aesAggregate', label='Aggregate Field',
                                   value=aes[['aesAggregate']]),
                     conditionalPanel('input.aesAggregate==true', 
                                      selectizeInput(inputId='aesAggFun', label='By', 
                                                     choices=aggFunchoices,
                                                     selected=aggFun, multiple=F,
                                                     options = list(create = TRUE))
                     )
                     
              ),
              column(4,
                     conditionalPanel('output.canAesFieldBeContinuous==true',
                                      radioButtons('aesDiscrete', 'Treat Field as', 
                                                   choices=c('Continuous'='continuous',
                                                             'Discrete'='discrete'),
                                                   selected=ifelse(aes[['aesDiscrete']], 'discrete', 'continuous'), 
                                                   inline=F)
                     )
                     
                     
              )
            )
          } else {            
            switch(currentAes,
                   'aesLabel'=textInput('aesValue', '', value=aes[['aesValue']]), 
                   'aesFontface'=selectInput('aesValue','',choices=FontFaceChoices, selected=aes[['aesValue']]),
                   'aesFamily'=selectInput('aesValue','',choices=FontFamilyChoices, selected=aes[['aesValue']]), 
                   'aesColor'=, 'aesBorderColor'=NULL,                   
                   numericInput('aesValue', 'Value', value=aes[['aesValue']], step=0.1)
            )      
          }
        })
      }      
    }    
  }
})

## set aesValue
observe({
  v <- input$aesValue
  vColor <- input$aesValueColor
  ## using is.empty instead of is.null below will cause aesValue not being able to be deleted by user
  ## not ideal, but due to the inability of setting value on jscolorPicker
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!is.empty(currentSheet)){
      currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
      if(!is.empty(currentLayer)){
        currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
        if(!is.empty(currentAes)){
          if((currentAes=='aesColor' || currentAes=='aesBorderColor') && !is.empty(vColor)) v <- paste('#', vColor, sep='')
          if(!is.empty(v)) sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesValue']] <<- v
        }        
      }      
    }
  }) 
  
})
# observe({
#   updateInput[['aesValue']]
#   currentSheet <- (projProperties[['activeSheet']])
#   s <- 0
#   if(!is.empty(currentSheet)){
#     currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
#     if(!is.empty(currentLayer)){
#       currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
#       if(!is.empty(currentAes)){
#         s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesValue']])
#         switch(currentAes,
#                'aesLabel'=updateTextInput(session, 'aesValue', value=s), 
#                'aesFamily'=, 'aesFontface'=updateSelectInput(session, 'aesValue', selected=s),
#                'aesColor'=, 'aesBorderColor'=
#                  NULL, # no update jscolorInput available
#                
#                updateNumericInput(session, 'aesValue', value=s)
#         )
#       }      
#     }    
#   }
#   
# })

## set aes field
observe({
  v <- input$aesField
  if(!is.null(v)){
    isolate({
      currentSheet <- (projProperties[['activeSheet']])
      if(!is.empty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!is.empty(currentLayer)){
          currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
          if(!is.empty(currentAes)){
            if(are.vectors.different(v, sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesField']])){                      
              sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesField']] <<- v
              
              ## set default field properties
              if(is.empty(v) && currentLayer != 'Plot'){
                v <- null2String(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][['Plot']][['aesList']][[currentAes]][['aesField']])
              }
              is.measure <- v %in% sheetList[[currentSheet]][['measuresR']]()
              sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesAggregate']] <<- is.measure
              sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesAggFun']] <<- 'sum'
              sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesDiscrete']] <<- !is.measure
              sapply(c('aesAggregate','aesAggFun','aesDiscrete'), triggerUpdateInput)
            }          
          }        
        }      
      }
    })  
  }
  
})
# observe({
#   updateInput[['aesField']]
#   currentSheet <- (projProperties[['activeSheet']])
#   s <- choices <- ''
#   if(!is.empty(currentSheet)){
#     currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
#     if(!is.empty(currentLayer)){
#       currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
#       if(!is.empty(currentAes)){
#         s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesField']])
#         choices <- if(currentAes %in% c('aesX','aesY')) {
#           sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['fieldChoices']]
#         } else {
#           sheetList[[currentSheet]][['fieldNames']]()
#         }          
#       }      
#     }    
#   }
#   
#   updateSelectizeInput(session, 'aesField', choices=null2String(choices), selected=null2String(s))
# })

output$canAesFieldBeContinuous <- reactive({
  ans <- F
  currentSheet <- (projProperties[['activeSheet']])
  if(!is.empty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!is.empty(currentLayer)){
      currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
      if(!is.empty(currentAes)){
        ans <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['canFieldBeContinuous']]()
      }      
    }    
  }
  ans
})
outputOptions(output, "canAesFieldBeContinuous", suspendWhenHidden=FALSE)

## set aes aggregate
observe({
  v <- input$aesAggregate
  if(!is.null(v)){
    isolate({
      currentSheet <- (projProperties[['activeSheet']])
      if(!is.empty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!is.empty(currentLayer)){
          currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
          if(!is.empty(currentAes)){
            sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesAggregate']] <<- v
          }        
        }      
      }
    })  
  }
  
})
observe({
  updateInput[['aesAggregate']]
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    s <- F
    if(!is.empty(currentSheet)){
      currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
      if(!is.empty(currentLayer)){
        currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
        if(!is.empty(currentAes)){
          s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesAggregate']])
          
        }      
      }    
    }
    updateCheckboxInput(session, 'aesAggregate', value=s)
  })
  
})


## set aes agg fun
observe({
  v <- input$aesAggFun
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!is.empty(currentSheet)){
      currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
      if(!is.empty(currentLayer)){
        currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
        if(!is.empty(currentAes)){
          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesAggFun']] <<- v         
        }        
      }      
    }
  })  
})
observe({
  updateInput[['aesAggFun']]
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    s <- ''
    if(!is.empty(currentSheet)){
      currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
      if(!is.empty(currentLayer)){
        currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
        if(!is.empty(currentAes)){
          s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesAggFun']])
          
        }      
      }    
    }
    choices <- YFunChoices
    if(!is.empty(s) && !(s %in% choices)) choices[s]=s
    updateSelectizeInput(session, 'aesAggFun', choices=null2String(choices), selected=null2String(s))
  })  
})

## set aes discrete
observe({
  v <- input$aesDiscrete
  if(!is.null(v)){
    isolate({
      currentSheet <- (projProperties[['activeSheet']])
      if(!is.empty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!is.empty(currentLayer)){
          currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
          if(!is.empty(currentAes)){
            sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesDiscrete']] <<- (v=='discrete')
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
    if(!is.empty(currentSheet)){
      currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
      if(!is.empty(currentLayer)){
        currentAes <- sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['activeAes']]
        if(!is.empty(currentAes)){
          d <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['aesList']][[currentAes]][['aesDiscrete']])
          if(!is.empty(d) && !d){
            s <- 'continuous'
          }
          
        }      
      }    
    }
    updateRadioButtons(session, 'aesDiscrete', selected=s)
  })  
})

## set Mark Type / geom
observe({
  v <- input$markList
  isolate({
    if(!is.empty(v)){
      currentSheet <- (projProperties[['activeSheet']])
      if(!is.empty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!is.empty(currentLayer)){
          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['geom']] <<- v
          # set default stat type & position
          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['statType']] <<-
            switch(v, 'boxplot'='boxplot', 'identity')
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
  if(!is.empty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!is.empty(currentLayer)){
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
    if(!is.empty(v)){
      currentSheet <- (projProperties[['activeSheet']])
      if(!is.empty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!is.empty(currentLayer)){
          
          sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['statType']] <<- v
        }      
      }
    }    
  })  
})
observe({
  updateInput[['layerStatType']]
  currentSheet <- (projProperties[['activeSheet']])
  s <- choices <- ''
  if(!is.empty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!is.empty(currentLayer)){
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
    if(!is.empty(v)){
      currentSheet <- (projProperties[['activeSheet']])
      if(!is.empty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!is.empty(currentLayer)){
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
  if(!is.empty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!is.empty(currentLayer)){
      s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['yFun']])
    }    
  }
  choices <- YFunChoices
  if(!is.empty(s) && !(s %in% choices)) choices[s]=s
  updateSelectizeInput(session, 'yFunList', choices=null2String(choices), selected=null2String(s))
})



## set PositionType
observe({
  v <- input$layerPositionType
  isolate({
    currentSheet <- (projProperties[['activeSheet']])
    if(!is.empty(currentSheet)){
      currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
      if(!is.empty(currentLayer)){
        sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['layerPositionType']] <<- v
      }      
    }
  })
  
})
observe({
  updateInput[['layerPositionType']]
  currentSheet <- (projProperties[['activeSheet']])
  s <- ''
  if(!is.empty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!is.empty(currentLayer)){
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
    if(!is.empty(currentSheet)){
      currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
      if(!is.empty(currentLayer)){
        sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['layerPositionHeight']] <<- v
      }      
    }
  })
  
})
observe({
  updateInput[['layerPositionHeight']]
  currentSheet <- (projProperties[['activeSheet']])
  s <- ''
  if(!is.empty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!is.empty(currentLayer)){
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
    if(!is.empty(currentSheet)){
      currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
      if(!is.empty(currentLayer)){
        sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['layerPositionWidth']] <<- v
      }      
    }
  })
  
})
observe({
  updateInput[['layerPositionWidth']]
  currentSheet <- (projProperties[['activeSheet']])
  s <- ''
  if(!is.empty(currentSheet)){
    currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
    if(!is.empty(currentLayer)){
      s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][['layerList']][[currentLayer]][['layerPositionWidth']])      
    }    
  }
  updateTextInput(session, 'layerPositionWidth', value=null2String(s))
})




tabS <- tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
                   (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )
canLatexPNG <- tryCatch(length(make.png(tabS)), error=function(e) F)


output$sheetOutput <- renderUI({
  currentSheet <- projProperties[['activeSheet']]
  if(!is.empty(currentSheet)){
    switch(sheetList[[currentSheet]][['dynamicProperties']][['outputType']], 
           'table'=if(canLatexPNG) imageOutput('sheetOutputTable') else {
             tags$div(
               HTML(paste(capture.output(Hmisc::html(sheetList[[currentSheet]][['tableR']]())), collapse=" "))
             )
           },
           'plot'=plotOutput('ggplot'))
  }
  
})


output$sheetOutputTable <- renderImage(if(canLatexPNG) {
  currentSheet <- projProperties[['activeSheet']]
  if(!is.empty(currentSheet)){
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
  if(!is.empty(currentSheet)){sheetList[[currentSheet]][['dynamicProperties']][['outputTable']]}    
})

output$ggplot <- renderPlot({
  if(input$autoRefresh=='refresh'){
    currentSheet <- projProperties[['activeSheet']]
    if(!is.empty(currentSheet)){
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
      if(!is.empty(currentSheet)){
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
      if(!is.empty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!is.empty(currentLayer) && currentLayer!='Plot'){
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
      if(!is.empty(currentSheet)){
        currentLayer <- (sheetList[[currentSheet]][['dynamicProperties']][['activeLayer']])
        if(!is.empty(currentLayer)){
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
      if(!is.empty(currentSheet)){
        sheets <- names(sheetList)
        i <- match(currentSheet, sheets)
        sheetList[[currentSheet]] <<- NULL
        projProperties[['activeSheet']] <<- ifelse(length(sheets)>i, sheets[i+1],
                                                   ifelse(i>1, sheets[i-1], ''))
      } 
    }    
  })
})

