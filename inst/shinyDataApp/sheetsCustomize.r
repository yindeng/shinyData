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


#
#
#
# ## modify xlab
# observe({
#   v <- input$plotXlab
#   isolate({
#     currentSheet <- (projProperties[['activeSheet']])
#     if(!isEmpty(currentSheet)){
#       sheetList[[currentSheet]][['dynamicProperties']][['plotXlab']] <<- v
#     }
#   })
#
# })
# observe({
#   updateInput[['plotXlab']]
#   currentSheet <- projProperties[['activeSheet']]
#   s <- if(!isEmpty(currentSheet)){
#     isolate(sheetList[[currentSheet]][['dynamicProperties']][['plotXlab']])
#   } else ''
#   updateTextInput(session, 'plotXlab', value=null2String(s))
# })
#
# ## modify ylab
# observe({
#   v <- input$plotYlab
#   isolate({
#     currentSheet <- (projProperties[['activeSheet']])
#     if(!isEmpty(currentSheet)){
#       sheetList[[currentSheet]][['dynamicProperties']][['plotYlab']] <<- v
#     }
#   })
#
# })
# observe({
#   updateInput[['plotYlab']]
#   currentSheet <- projProperties[['activeSheet']]
#   s <- if(!isEmpty(currentSheet)){
#     isolate(sheetList[[currentSheet]][['dynamicProperties']][['plotYlab']])
#   } else ''
#   updateTextInput(session, 'plotYlab', value=null2String(s))
# })


###########################
## Text Formatting
#
# ## Selecting font family
# observe({
#   v <- input$textFamily
#   isolate({
#     if(!isEmpty(v)){
#       currentSheet <- (projProperties[['activeSheet']])
#       if(!isEmpty(currentSheet)) {
#         customizeItem <- sheetList[[currentSheet]][['dynamicProperties']][['customizeItem']]
#         if(!isEmpty(customizeItem)){
#           if(is.null(sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]])){
#             sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]] <<- list()
#           }
#           sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]][['textFamily']] <<- v
#         }
#       }
#     }
#   })
# })
# observe({
#   updateInput[['textFamily']]
#   currentSheet <- projProperties[['activeSheet']]
#   s <- ''
#   if(!isEmpty(currentSheet)){
#     customizeItem <- (sheetList[[currentSheet]][['dynamicProperties']][['customizeItem']])
#     if(!isEmpty(customizeItem) && !is.null(sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]])){
#       s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]][['textFamily']])
#     }
#   }
#
#   updateSelectInput(session, 'textFamily', selected=null2String(s))
# })
#
# ## Selecting font face
# observe({
#   v <- input$textFace
#   isolate({
#     if(!isEmpty(v)){
#       currentSheet <- (projProperties[['activeSheet']])
#       if(!isEmpty(currentSheet)) {
#         customizeItem <- sheetList[[currentSheet]][['dynamicProperties']][['customizeItem']]
#         if(!isEmpty(customizeItem)){
#           if(is.null(sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]])){
#             sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]] <<- list()
#           }
#           sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]][['textFace']] <<- v
#         }
#       }
#     }
#   })
# })
# observe({
#   updateInput[['textFace']]
#   currentSheet <- projProperties[['activeSheet']]
#   s <- ''
#   if(!isEmpty(currentSheet)){
#     customizeItem <- (sheetList[[currentSheet]][['dynamicProperties']][['customizeItem']])
#     if(!isEmpty(customizeItem) && !is.null(sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]])){
#       s <- isolate(sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]][['textFace']])
#     }
#   }
#
#   updateSelectInput(session, 'textFace', selected=null2String(s))
# })

# ## Selecting font color
# observe({
#   v <- input$textColor
#   isolate({
#     currentSheet <- (projProperties[['activeSheet']])
#     if(!isEmpty(currentSheet)) {
#       customizeItem <- sheetList[[currentSheet]][['dynamicProperties']][['customizeItem']]
#       if(!isEmpty(customizeItem)){
#         if(is.null(sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]])){
#           sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]] <<- list()
#         }
#         if(!isEmpty(v)) v <- paste0("#", v)
#         sheetList[[currentSheet]][['dynamicProperties']][[customizeItem]][['textColor']] <<- v
#       }
#     }
#   })
# })

## font size, etc.
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

