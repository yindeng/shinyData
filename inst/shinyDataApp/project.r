

####################################################
## Project saving and loading
####################################################
output$downloadProject <- downloadHandler(
  filename = function() { 'MyProject.sData' },
  content = function(file) {
    isolate({
      for(di in names(datList)){
        datList[[di]]$removeDatDependencies()
      }
      allData <- list(pp=reactiveValuesToList(projProperties),
                      dl=datList,
                      sl=sheetList)
      save(allData, file=file)
      for(di in names(datList)){
        datList[[di]]$setDatDependencies()
      }
    })
  }
)


loadProject <- function(file, replaceOrMerge='replace'){
  load(file)

  if(replaceOrMerge=='replace'){
    for(n in names(datList)) datList[[n]] <<- NULL
    for(n in names(sheetList)) sheetList[[n]] <<- NULL
  }

  for(n in names(allData$pp)){
    if(is.null(projProperties[[n]]) || projProperties[[n]] != allData$pp[[n]]){
      projProperties[[n]] <<- allData$pp[[n]]
    }
  }

  for(di in names(allData$dl)){
    if(is.null(datList[[di]])){ # new data
      datList[[di]] <<- DatClass$new('staticProperties'=allData$dl[[di]][['staticProperties']])
      datList[[di]][['dynamicProperties']] <<- reactiveValues()

    }
    for(n in names(allData$dl[[di]][['dynamicProperties']])){
      x <- allData$dl[[di]][['dynamicProperties']][[n]]
      if(n=='fieldsList'){
        if(is.null(datList[[di]][['dynamicProperties']][[n]])) datList[[di]][['dynamicProperties']][[n]] <<- list()
        names1 <- names(x)
        for(n1 in names1){
          if(is.reactivevalues(x[[n1]])){
            if(is.null(datList[[di]][['dynamicProperties']][[n]][[n1]])) datList[[di]][['dynamicProperties']][[n]][[n1]] <<- reactiveValues()
            names2 <- names(x[[n1]])
            for(n2 in names2){
              datList[[di]][['dynamicProperties']][[n]][[n1]][[n2]] <<- x[[n1]][[n2]]
            }
          } else {
            datList[[di]][['dynamicProperties']][[n]][[n1]] <<- x[[n1]]
          }
        }
      } else {
        datList[[di]][['dynamicProperties']][[n]] <<- x
      }
    }
    datList[[di]]$setDatDependencies()
  }

  for(si in names(allData$sl)){
    if(is.null(sheetList[[si]])){ # new sheet
      sheetList[[si]] <<- createNewSheetObj()
    }
    for(n in names(allData$sl[[si]][['dynamicProperties']])){
      x <- allData$sl[[si]][['dynamicProperties']][[n]]
      if(n=='layerList'){
        if(is.null(sheetList[[si]][['dynamicProperties']][[n]])) sheetList[[si]][['dynamicProperties']][[n]] <<- list()
        names1 <- names(x)
        for(n1 in names1){
          if(is.reactivevalues(x[[n1]])){
            if(is.null(sheetList[[si]][['dynamicProperties']][[n]][[n1]]))
              sheetList[[si]][['dynamicProperties']][[n]][[n1]] <<- createNewLayer()
            names2 <- names(x[[n1]])
            for(n2 in names2){
              if(n2=='aesList'){
                names3 <- names(x[[n1]][[n2]])
                for(n3 in names3){
                  if(is.reactivevalues(x[[n1]][[n2]][[n3]])){
                    names4 <- names(x[[n1]][[n2]][[n3]])
                    for(n4 in names4){
                      if(typeof(x[[n1]][[n2]][[n3]][[n4]]) != 'closure')
                        sheetList[[si]][['dynamicProperties']][[n]][[n1]][[n2]][[n3]][[n4]] <<- x[[n1]][[n2]][[n3]][[n4]]
                    }
                    setAesReactives(si,n1,n3)
                  } else {
                    sheetList[[si]][['dynamicProperties']][[n]][[n1]][[n2]][[n3]] <<- x[[n1]][[n2]][[n3]]
                  }
                }
              } else {
                sheetList[[si]][['dynamicProperties']][[n]][[n1]][[n2]] <<- x[[n1]][[n2]]
              }
            }
          } else {
            sheetList[[si]][['dynamicProperties']][[n]][[n1]] <<- x[[n1]]
          }
        }
      } else {
        sheetList[[si]][['dynamicProperties']][[n]] <<- x
      }
    }
  }

  ## update all UI
  sapply(names(updateInput), triggerUpdateInput)
  updateTabsetPanel(session, 'mainNavBar', selected='Visualize')
}

observe({
  inFile <- input[['loadProject']]
  isolate({
    if (!is.null(inFile)){
      loadProject(file=inFile$datapath, replaceOrMerge=input[['loadProjectAction']])
    }
  })

})

observe({
  v <- input$openSampleProj
  isolate({
    file <- paste('samples', input[['sampleProj']], sep='/')
    if(v && file.exists(file)){
      loadProject(file=file, 'replace')
    }
  })
})
