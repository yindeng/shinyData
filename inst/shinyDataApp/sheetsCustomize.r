## Selecting plot element for customizaiton
observe({
  v <- input$customizeItem
  isolate({
    if(!isEmpty(v)){
      currentSheet <- (projProperties[['activeSheet']])
      if(!isEmpty(currentSheet)) sheetList[[currentSheet]][['dynamicProperties']][['customizeItem']] <<- v
    }
  })
})
observe({
  updateInput[['customizeItem']]
  currentSheet <- projProperties[['activeSheet']]
  s <- if(!isEmpty(currentSheet)){
    isolate(sheetList[[currentSheet]][['dynamicProperties']][['customizeItem']])
  } else ''
  updateTabsetPanel(session, 'customizeItem', selected=null2String(s))
})

## modify plotTitle and other text fields
lapply(c('plotTitle', 'plotXlab', 'plotYlab'),
       function(inputId){
         assign(paste0('observer_', inputId, '_push'),
                observe({
                  v <- input[[inputId]]
                  isolate({
                    currentSheet <- (projProperties[['activeSheet']])
                    if(!isEmpty(currentSheet)){
                      sheetList[[currentSheet]][['dynamicProperties']][[inputId]] <<- v
                    }
                  })

                }),
                sessionEnv)
         assign(paste0('observer_', inputId, '_pull'),
                observe({
                  updateInput[[inputId]]
                  currentSheet <- projProperties[['activeSheet']]
                  s <- if(!isEmpty(currentSheet)){
                    isolate(sheetList[[currentSheet]][['dynamicProperties']][[inputId]])
                  } else ''
                  updateTextInput(session, inputId, value=null2String(s))
                }),
                sessionEnv)
       })



###########################
## Text Formatting

lapply(list(list(inputId='textFamily', inputType='select'),
            list(inputId='textFace', inputType='select'),
            list(inputId='textColor', inputType='color'),
            list(inputId='textSize', inputType='numeric'),
            list(inputId='textHjust', inputType='numeric'),
            list(inputId='textVjust', inputType='numeric'),
            list(inputId='textAngle', inputType='numeric'),
            list(inputId='textLineheight', inputType='numeric')),
       function(x){
         assign(paste0('observer_', x$inputId, '_push'),
                observe({
                  v <- input[[x$inputId]]
                  isolate({
                    currentSheet <- (projProperties[['activeSheet']])
                    if(!isEmpty(currentSheet)) {
                      customizeItem <- sheetList[[currentSheet]][['dynamicProperties']][['customizeItem']]
                      if(!isEmpty(customizeItem)){
                        if(is.null(sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]])){
                          sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]] <<- list()
                        }
                        if(x$inputId=='textColor' && !isEmpty(v)) v <- paste0("#", v)
                        if(are.vectors.different(v, sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]][[x$inputId]]))
                          sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]][[x$inputId]] <<- v
                      }
                    }
                  })
                }),
                sessionEnv)
         if(x$inputId=='textColor') return() ## no updateJsColorInput is available yet
         assign(paste0('observer_', x$inputId, '_pull'),
                observe({
                  updateInput[[x$inputId]]
                  currentSheet <- projProperties[['activeSheet']]
                  s <- ''
                  if(!isEmpty(currentSheet)){
                    customizeItem <- (sheetList[[currentSheet]][['dynamicProperties']][['customizeItem']])
                    if(!isEmpty(customizeItem) && !is.null(sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]])){
                      s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]][[x$inputId]])
                    }
                  }

                  switch(x$inputType,
                         'numeric'=updateNumericInput(session, x$inputId, value=null2String(s)),
                         'select'=updateSelectInput(session, x$inputId, selected=null2String(s)))

                }),
                sessionEnv)
       })


## End of Text Formatting
###########################

