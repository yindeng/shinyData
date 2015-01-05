## Switching doc
observe({
  v <- input$docList
  isolate({
    if(!isEmpty(v)) projProperties[['activeDoc']] <<- v
  })

})
observe({
  updateInput[['activeDoc']]
  updateSelectInput(session, 'docList', choices=(docListNames()),
                    selected=isolate(projProperties[['activeDoc']]))
})

## modify doc name
observe({
  v <- input$docName
  isolate({
    currentDoc <- (projProperties[['activeDoc']])
    if(!isEmpty(currentDoc)){
      if(!isEmpty(v) && isEmpty(isolate(docListNames())[v])){
        docList[[currentDoc]][['name']] <<- v
      }
    }
  })

})
observe({
  updateInput[['docName']]
  currentDoc <- projProperties[['activeDoc']]
  s <- if(!isEmpty(currentDoc)){
    isolate(docList[[currentDoc]][['name']])
  } else ''
  updateTextInput(session, 'docName', value=null2String(s))
})

## modify rmd
observe({
  v <- input$rmd
  isolate({
    currentDoc <- (projProperties[['activeDoc']])
    if(!isEmpty(currentDoc)){
      docList[[currentDoc]][['rmd']] <<- v
    }
  })

})
observe({
  updateInput[['docRmd']]
  currentDoc <- projProperties[['activeDoc']]
  s <- if(!isEmpty(currentDoc)){
    isolate(docList[[currentDoc]][['rmd']])
  } else ''
  updateAceEditor(session, 'rmd', value=null2String(s))
})


# substituteSheetName <- function(text){
#   nn <- sheetListNames()
#   for(n in names(nn)){
#     text <- gsub(paste('[',n,']',sep=''),
#                  paste('sheetList[["',nn[n],'"]][["plotR"]]()',sep=''), text, fixed=TRUE)
#   }
#   text
# }

getDatSheetEnv <- function(){
  env <- new.env(parent = globalenv())
  nn <- sheetListNames()
  for(n in names(nn)){
    ## using local is essential to make delayedAssign work
    local({
      nId <- nn[n]
      local(delayedAssign(n, sheetList[[nId]][['plotR']](), assign.env=env))
    })
  }
  nn <- datListNames()
  for(n in names(nn)){
    local({
      nId <- nn[n]
      local(delayedAssign(n, datList[[nId]][['datR']](), assign.env=env))
    })
  }
  env
}

## preview output
output$rmdOutput <- renderUI({
  currentDoc <- (projProperties[['activeDoc']])
  if(input$rmdTabs=='Preview' && !isEmpty(currentDoc)){
    isolate({
      srcCode <- docList[[currentDoc]][['rmd']]
      if(!isEmpty(srcCode)){
        HTML(knit2html(text = srcCode, fragment.only = TRUE, quiet = TRUE,
                       envir=getDatSheetEnv()))
      }
    })
  }
})



## add doc
observe({
  v <- input$addDoc
  isolate({
    if(v){
      addDoc()
    }
  })
})
## delete doc
observe({
  v <- input$deleteDoc
  isolate({
    if(v){
      currentDoc <- (projProperties[['activeDoc']])
      if(!isEmpty(currentDoc)){
        docs <- names(docList)
        i <- match(currentDoc, docs)
        docList[[currentDoc]] <<- NULL
        projProperties[['activeDoc']] <<- ifelse(length(docs)>i, docs[i+1],
                                                   ifelse(i>1, docs[i-1], ''))
      }
    }
  })
})

