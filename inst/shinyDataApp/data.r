
## switching data source
observe({
  v <- input$datList
  isolate({
    if(!isEmpty(input$datList)) projProperties[['activeDat']] <<- v
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
    if(!isEmpty(currentDat)){
      if(!isEmpty(v) && isEmpty(datListNames()[v])){
        ## the second condition makes sure v is different
        ## update doc's rmd
        oldName <- paste('`', datList[[currentDat]][['dynamicProperties']][['name']], '`', sep='')
        newName <- paste('`', v, '`', sep='')
        sapply(names(docList), function(currentDoc){
          docList[[currentDoc]][['rmd']] <<- gsub(oldName, newName, docList[[currentDoc]][['rmd']], fixed=TRUE)
          NULL
        })
        triggerUpdateInput('docRmd')

        datList[[currentDat]][['dynamicProperties']][['name']] <<- v
      }
    }
  })

})
observe({
  updateInput[['datName']]
  currentDat <- projProperties[['activeDat']]
  s <- if(!isEmpty(currentDat)){
    isolate(datList[[currentDat]][['dynamicProperties']][['name']])
  } else ''
  updateTextInput(session, 'datName', value=null2String(s))
})

## Selecting fields
observe({
  activeField <- input$fieldsList
  isolate({
    currentDat <- (projProperties[['activeDat']])
    if(!isEmpty(currentDat)){
      datList[[currentDat]][['dynamicProperties']][['activeField']] <<- activeField
    }
  })

})
observe({
  updateInput[['activeField']]
  currentDat <- projProperties[['activeDat']]
  s <- if(!isEmpty(currentDat)){
    isolate(datList[[currentDat]][['dynamicProperties']][['activeField']])
  } else ''
  choices <- if(!isEmpty(currentDat)) datList[[currentDat]][['fieldNames']]()
  updateSelectizeInput(session, "fieldsList", choices=null2String(choices),
                       selected=null2String(s))
})

## modify field name
observe({
  v <- (input$fieldName) #make.names
  isolate({
    currentDat <- (projProperties[['activeDat']])
    if(!isEmpty(currentDat)){
      currentField <- (datList[[currentDat]][['dynamicProperties']][['activeField']])
      if(!isEmpty(currentField)){
        if(!isEmpty(v) && isEmpty((datList[[currentDat]][['fieldNames']]())[v])){
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
  if(!isEmpty(currentDat)){
    currentField <- datList[[currentDat]][['dynamicProperties']][['activeField']]
    if(!isEmpty(currentField)){
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
    if(!isEmpty(currentDat)){
      datList[[currentDat]][['dynamicProperties']][['measures']] <<- newMeasures
    }
  })
})
observe({
  updateInput[['measures']]
  currentDat <- projProperties[['activeDat']]
  s <- if(!isEmpty(currentDat)){
    isolate(datList[[currentDat]][['dynamicProperties']][['measures']])
  } else ''
  choices <- if(!isEmpty(currentDat)) datList[[currentDat]][['fieldNames']]()
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
      dat  <- fread(inFile$datapath, header="auto", sep="auto")
      fileN <- paste('file_',newGuid(),sep='')
      existingNames <- names(datListNames())
      ## make sure the new name is different
      newName <- make.unique(c(existingNames, inFile$name), sep='_')[length(existingNames)+1]

      datList[[fileN]] <<- createNewDatClassObj(dat, name=newName,
                                                nameOriginal=inFile$name, type='file')
      projProperties[['activeDat']] <<- fileN
      triggerUpdateInput('activeDat')
    }
  })

})

output$uploadingTextFile <- reactive({
  TRUE
})
outputOptions(output, "uploadingTextFile", suspendWhenHidden=FALSE)

output$datPreview <- renderDataTable({
  currentDat <- projProperties[['activeDat']]
  if(!isEmpty(currentDat)){
    datPrev <- copy(datList[[currentDat]][['datR']]())  # use of copy is necessary since setnames modify by reference
    setnames(datPrev, names(datList[[currentDat]][['fieldNames']]()))
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

