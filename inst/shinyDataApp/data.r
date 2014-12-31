
## switching data source
observe({
  v <- input$datList
  isolate({
    if(!is.empty(input$datList)) projProperties[['activeDat']] <<- v
  })    
})
observe({
  updateInput[['activeDat']]
  updateSelectInput(session, 'datList', choices=(datListNames()), 
                    selected=isolate(projProperties[['activeDat']]))
})


## modify dat source name
observe({    
  v <- input$datName
  isolate({
    currentDat <- (projProperties[['activeDat']])
    if(!is.empty(currentDat)){
      if(!is.empty(v) && is.empty(datListNames()[v])){
        datList[[currentDat]][['dynamicProperties']][['name']] <<- v
      } 
    }
  })
  
})
observe({
  updateInput[['datName']]
  currentDat <- projProperties[['activeDat']]
  s <- if(!is.empty(currentDat)){
    isolate(datList[[currentDat]][['dynamicProperties']][['name']])
  } else ''
  updateTextInput(session, 'datName', value=null2String(s))
})

## Selecting fields
observe({
  activeField <- input$fieldsList
  isolate({
    currentDat <- (projProperties[['activeDat']])
    if(!is.empty(currentDat)){
      datList[[currentDat]][['dynamicProperties']][['activeField']] <<- activeField
    }
  })
  
})
observe({
  updateInput[['activeField']]
  currentDat <- projProperties[['activeDat']]
  s <- if(!is.empty(currentDat)){
    isolate(datList[[currentDat]][['dynamicProperties']][['activeField']])
  } else ''
  choices <- if(!is.empty(currentDat)) datList[[currentDat]][['fieldNames']]()
  updateSelectizeInput(session, "fieldsList", choices=null2String(choices), 
                       selected=null2String(s))
})

## modify field name
observe({
  v <- (input$fieldName) #make.names
  isolate({
    currentDat <- (projProperties[['activeDat']])    
    if(!is.empty(currentDat)){
      currentField <- (datList[[currentDat]][['dynamicProperties']][['activeField']])
      if(!is.empty(currentField)){
        if(!is.empty(v) && is.empty((datList[[currentDat]][['fieldNames']]())[v])){
          datList[[currentDat]][['dynamicProperties']][['fieldsList']][[currentField]][['name']] <<- v
          if(v!=input$fieldName) triggerUpdateInput('fieldName')
        } 
      }      
    }
  })
  
})
observe({
  updateInput[['fieldName']]
  currentDat <- projProperties[['activeDat']]
  s <- ''
  if(!is.empty(currentDat)){
    currentField <- datList[[currentDat]][['dynamicProperties']][['activeField']]
    if(!is.empty(currentField)){
      s <- isolate(datList[[currentDat]][['dynamicProperties']][['fieldsList']][[currentField]][['name']])
    }
  }
  updateTextInput(session, 'fieldName', value=null2String(s))
})

## Manipulating set of measures
observe({
  newMeasures <- input$measures
  isolate({
    currentDat <- (projProperties[['activeDat']])
    if(!is.empty(currentDat)){
      datList[[currentDat]][['dynamicProperties']][['measures']] <<- newMeasures
    } 
  })    
})
observe({
  updateInput[['measures']]
  currentDat <- projProperties[['activeDat']]
  s <- if(!is.empty(currentDat)){
    isolate(datList[[currentDat]][['dynamicProperties']][['measures']])
  } else ''
  choices <- if(!is.empty(currentDat)) datList[[currentDat]][['fieldNames']]()
  updateSelectizeInput(session, "measures", choices=null2String(choices), 
                       selected=null2String(s))
})

## Add data source from text file
observe({
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.    
  inFile <- input[['file']]  
  isolate({
    if (!is.null(inFile)){      
      dat  <- (read.csv(inFile$datapath, 
                        header=input[['header']], 
                        sep=input[['sep']], 
                        quote=input[['quote']]))
      fileN <- paste('file_',newGuid(),sep='')
      datList[[fileN]] <<- createNewDatClassObj(dat, name=inFile$name, 
                                                nameOriginal=inFile$name, type='file')         
      projProperties[['activeDat']] <<- fileN   
      triggerUpdateInput('activeDat')
    }
  })
  
})

output$uploadingTextFile <- reactive({
  T
})
outputOptions(output, "uploadingTextFile", suspendWhenHidden=FALSE)

output$datPreview <- renderDataTable({
  currentDat <- projProperties[['activeDat']]
  if(!is.empty(currentDat)){
    datPrev <- datList[[currentDat]][['datR']]()
    names(datPrev) <- names(datList[[currentDat]][['fieldNames']]())
    datPrev
  }
})


## Text file import calibration

#   observe({
#     hh <- input[['header']]; ss <- input[['sep']]; qq <- input[['quote']]
#     if(uploadingData){
#       inFile <- isolate(input[['file']])
#       
#       if (!is.null(inFile)){
#         
#         dat  <- read.csv(inFile$datapath, header=hh, sep=ss, quote=qq)
#         fileN <- isolate(input$datList)
#         datList[[fileN]] <<- dat
#         
#         defaultMeasures <- colnames(dat)[apply(dat,2,is.numeric)]
#         updateSelectizeInput(session, "measures", choices=colnames(dat), selected=defaultMeasures)
#         
#         output$datPreview <- renderDataTable({
#           (dat)
#         })
#         
#         metaDataSources[[fileN]][['data']] <<- dat
#         metaDataSources[[fileN]][['measures']] <<- defaultMeasures
#         
#       }
#     }
#   })

