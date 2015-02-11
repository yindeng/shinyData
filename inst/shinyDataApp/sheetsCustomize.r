## upsert sheet dynamicProperties
lapply(list(list(inputId='customizeItem', inputType='tabsetPanel'),
            list(inputId='plotTitle', inputType='text'),
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

                        if(x$inputId=='customizeItem'){
                          if(is.null(sheetList[[currentSheet]][['dynamicProperties']][['formatting']])){
                            sheetList[[currentSheet]][['dynamicProperties']][['formatting']] <<- list()
                          }
                          if(is.null(sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[v]])){
                            sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[v]] <<-
                              structure(list(), 'type'='element_text')
                          }
                        }
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
                  switch(x$inputType,
                         'text'=updateTextInput(session, x$inputId, value=null2String(s)),
                         'tabsetPanel'=updateTabsetPanel(session, x$inputId, selected=null2String(s)))
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
                        if(x$inputId=='textColor' && !isEmpty(v)) v <- paste0("#", v)
                        if(are.vectors.different(v, sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[customizeItem]][[x$inputId]])){
                          sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[customizeItem]][[x$inputId]] <<- v
                        }
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
                    isolate({
                      if(!isEmpty(customizeItem) && !is.null(sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[customizeItem]])){
                        s <- sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[customizeItem]][[x$inputId]]
                      }
                    })
                  }

                  switch(x$inputType,
                         'numeric'=updateNumericInput(session, x$inputId, value=null2String(s)),
                         'select'=updateSelectInput(session, x$inputId, selected=null2String(s)))

                }),
                sessionEnv)
       })


## End of Text Formatting
###########################

