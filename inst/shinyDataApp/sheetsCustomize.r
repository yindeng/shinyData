gg_element_tree <- ggplot2:::.element_tree

observe({
  v <- get_selected(input[['customizeItem']])
  isolate({
    if(!isEmpty(v)){
      v <- v[[1]]
      currentSheet <- (projProperties[['activeSheet']])
      if(!isEmpty(currentSheet)){
        if(are.vectors.different(v, sheetList[[currentSheet]][['dynamicProperties']][['customizeItem']])){
          sheetList[[currentSheet]][['dynamicProperties']][['customizeItem']] <<- v

          if(is.null(sheetList[[currentSheet]][['dynamicProperties']][['formatting']])){
            sheetList[[currentSheet]][['dynamicProperties']][['formatting']] <<- list()
          }
          if(is.null(sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[v]])){
            formats <- structure(list(), 'type'=gg_element_tree[[v]][['class']])
            if(attr(formats, 'type')=='unit'){
              ## set default unit
              formats[['unitUnits']] <- 'char'
            }
            sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[v]] <<- formats
          }
        }
      }
    }
  })
})
output$customizeItem <- renderTree({
  updateInput[['customizeItem']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!isEmpty(currentSheet)){
    isolate(sheetList[[currentSheet]][['dynamicProperties']][['customizeItem']])
  } else ''

  get_children <- function(node){
    children <- gg_element_tree[sapply(gg_element_tree, function(x) !are.vectors.different(node, x$inherit))]
    if(length(children)){
      sapply(names(children), simplify = FALSE, USE.NAMES = TRUE,
             function(n){
               ans <- get_children(n)
               if(n==null2String(s)) attr(ans, 'stselected') <- TRUE
               ans
             })
    } else {
      ''
    }
  }
  get_children(NULL)
})


## upsert sheet dynamicProperties
lapply(list(list(inputId='plotTitle', inputType='text'),
            list(inputId='plotXlab', inputType='text'),
            list(inputId='plotYlab', inputType='text')),
       function(x){
         assign(paste0('observer_', x$inputId, '_push'),
                observe({
                  v <- input[[x$inputId]]
                  isolate({
                    currentSheet <- (projProperties[['activeSheet']])
                    if(!isEmpty(currentSheet)){
                      if(are.vectors.different(v, sheetList[[currentSheet]][['dynamicProperties']][[x$inputId]])){
                        sheetList[[currentSheet]][['dynamicProperties']][[x$inputId]] <<- v
                      }
                    }
                  })
                }),
                sessionEnv)
         assign(paste0('observer_', x$inputId, '_pull'),
                observe({
                  updateInput[[x$inputId]]
                  currentSheet <- projProperties[['activeSheet']]
                  s <- if(!isEmpty(currentSheet)){
                    isolate(sheetList[[currentSheet]][['dynamicProperties']][[x$inputId]])
                  } else ''
                  updateInput(session, x$inputType, x$inputId, s)
                }),
                sessionEnv)
       })


output$ggElementType <- reactive({
  currentSheet <- projProperties[['activeSheet']]
  s <- ''
  if(!isEmpty(currentSheet)){
    customizeItem <- (sheetList[[currentSheet]][['dynamicProperties']][['customizeItem']])
    isolate({
      if(!isEmpty(customizeItem) && !is.null(sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[customizeItem]])){
        s <- attr(sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[customizeItem]], 'type')
      }
    })
  }
  null2String(s)
})
outputOptions(output, "ggElementType", suspendWhenHidden=FALSE)

###########################
## Formatting

output$charSetting <- renderUI({
  currentSheet <- projProperties[['activeSheet']]
  if(!isEmpty(currentSheet)){
    s <- (sheetList[[currentSheet]][['dynamicProperties']][['customizeItem']])
    isolate({
      vMain <- sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[s]][['charMainValue']]
      v1 <- sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[s]][['charAltValue1']]
      v2 <- sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[s]][['charAltValue2']]
      switch(s,
             'legend.text.align'=, 'legend.title.align'=list(
               selectInput('charMainValue', 'Alignment', selected=vMain,
                           choices=c('Choose'='', 'Left'=0, 'Right'=1,'Center'=0.5,
                                     'Custom'='custom_')),
               conditionalPanel('input.charMainValue=="custom_"',
                                sliderInput('charAltValue1', 'Anchor', min=0, max=1, value=v1))
               ),
             'legend.direction'=list(
               helpText('Layout of items in legends:'),
               selectInput('charMainValue', '', selected=vMain,
                           choices=c('Choose'='', 'Horizontal'='horizontal', 'Vertical'='vertical'))
               ),
             'legend.box'=list(
               helpText('Arrangement of multiple legends:'),
               selectInput('charMainValue', '', selected=vMain,
                           choices=c('Choose'='', 'Horizontal'='horizontal', 'Vertical'='vertical'))
             ),
             'legend.position'=list(
               selectInput('charMainValue', '', selected=vMain,
                           choices=c('Choose'='', 'Right'='right','Bottom'='bottom',
                                     'Top'='top','Left'='left','Custom'='custom_')),
               conditionalPanel('input.charMainValue=="custom_"',
                                sliderInput('charAltValue1', 'X', min=0, max=1, value=v1),
                                sliderInput('charAltValue2', 'Y', min=0, max=1, value=v2))
             ),
             'legend.justification'=list(
               helpText('Anchor point for positioning legend inside plot:'),
               selectInput('charMainValue', '', selected=vMain,
                           choices=c('Choose'='', 'Center'='center','Custom'='custom_')),
               conditionalPanel('input.charMainValue=="custom_"',
                                sliderInput('charAltValue1', 'X', min=0, max=1, value=v1),
                                sliderInput('charAltValue2', 'Y', min=0, max=1, value=v2))
             ),
             'legend.box.just'=list(
               helpText('Justification of each legend within the overall bounding box, when there are multiple legends.'),
               selectInput('charMainValue', '', selected=vMain,
                           choices=c('Choose'='', 'Right'='right','Bottom'='bottom',
                                     'Top'='top','Left'='left'))
             ),
             'aspect.ratio'=list(
               numericInput('charMainValue', 'Plot Aspect Ratio', value=vMain, step=0.1)
               )
      )
    })
  }
})

lapply(list(list(inputId='textFamily', inputType='select'),
            list(inputId='textFace', inputType='select'),
            list(inputId='textColor', inputType='color'),
            list(inputId='textSize', inputType='numeric'),
            list(inputId='textHjust', inputType='numeric'),
            list(inputId='textVjust', inputType='numeric'),
            list(inputId='textAngle', inputType='numeric'),
            list(inputId='textLineheight', inputType='numeric'),

            list(inputId='rectColor', inputType='color'),
            list(inputId='rectFill', inputType='color'),
            list(inputId='rectSize', inputType='numeric'),
            list(inputId='rectLinetype', inputType='numeric'),

            list(inputId='lineColor', inputType='color'),
            list(inputId='lineSize', inputType='numeric'),
            list(inputId='lineLinetype', inputType='numeric'),
            list(inputId='lineLineend', inputType='numeric'),

            list(inputId='unitX', inputType='numeric'),
            list(inputId='unitUnits', inputType='select'),

            list(inputId='charMainValue', inputType='dynamic'), # no updating for dynamic UIs
            list(inputId='charAltValue1', inputType='dynamic'),
            list(inputId='charAltValue2', inputType='dynamic'),

            list(inputId='elementBlank', inputType='checkbox')
            ),
       function(x){
         assign(paste0('observer_', x$inputId, '_push'),
                observe({
                  v <- input[[x$inputId]]
                  isolate({
                    if(!is.null(v)){
                      currentSheet <- (projProperties[['activeSheet']])
                      if(!isEmpty(currentSheet)) {
                        customizeItem <- sheetList[[currentSheet]][['dynamicProperties']][['customizeItem']]
                        if(!isEmpty(customizeItem)){
                          if(x$inputId=='elementBlank'){
                            if(!isEmpty(v)) attr(sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[customizeItem]],
                                                 'elementBlank') <<- v
                          } else {
                            if(are.vectors.different(v, sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[customizeItem]][[x$inputId]])){
                              sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[customizeItem]][[x$inputId]] <<- v

                              if(x$inputId=='charMainValue' && !isEmpty(v) && v=='custom_' &&
                                   isEmpty(sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[customizeItem]][['charAltValue1']])){
                                sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[customizeItem]][['charAltValue1']] <<- 0
                                if(customizeItem %in% c('legend.position','legend.justification')){
                                  sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[customizeItem]][['charAltValue2']] <<- 0
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  })
                }),
                sessionEnv)
         if(x$inputType=='dynamic') return()
         assign(paste0('observer_', x$inputId, '_pull'),
                observe({
                  updateInput[[x$inputId]]
                  currentSheet <- projProperties[['activeSheet']]
                  s <- ''
                  if(!isEmpty(currentSheet)){
                    customizeItem <- (sheetList[[currentSheet]][['dynamicProperties']][['customizeItem']])
                    isolate({
                      if(!isEmpty(customizeItem) && !is.null(sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[customizeItem]])){
                        if(x$inputId=='elementBlank'){
                          s <- attr(sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[customizeItem]],
                                    'elementBlank')
                        } else {
                          s <- sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[customizeItem]][[x$inputId]]
                        }
                      }
                    })
                  }

                  updateInput(session, x$inputType, x$inputId, s)
                }),
                sessionEnv)
       })


## End of Formatting
###########################

