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
            sheetList[[currentSheet]][['dynamicProperties']][['formatting']][[v]] <<-
              structure(list(), 'type'=gg_element_tree[[v]][['class']])
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

