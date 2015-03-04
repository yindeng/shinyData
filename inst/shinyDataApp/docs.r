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


## modify output format
observe({
  v <- input$rmdOuputFormat
  isolate({
    currentDoc <- (projProperties[['activeDoc']])
    if(!isEmpty(currentDoc)){
      docList[[currentDoc]][['rmdOuputFormat']] <<- v
    }
  })

})
observe({
  updateInput[['rmdOuputFormat']]
  currentDoc <- projProperties[['activeDoc']]
  s <- if(!isEmpty(currentDoc)){
    isolate(docList[[currentDoc]][['rmdOuputFormat']])
  } else ''
  updateSelectInput(session, 'rmdOuputFormat', selected=null2String(s))
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
  updateAceEditor(session, 'rmd', value=ifempty(s, '\n')) # bug in updateAceEditor: won't update with ""
})


# substituteSheetName <- function(text){
#   nn <- sheetListNames()
#   for(n in names(nn)){
#     text <- gsub(paste('[',n,']',sep=''),
#                  paste('sheetList[["',nn[n],'"]][["plotR"]]()',sep=''), text, fixed=TRUE)
#   }
#   text
# }



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

getOutputExtension <- function (outputFormat) {
  switch(outputFormat,
         'html_document'=, 'ioslides_presentation'=, 'slidy_presentation'='html',
         'pdf_document'=, 'beamer_presentation'='pdf',
         'word_document'='docx', 'md_document'='md', '')
}

output$downloadRmdOutput <- downloadHandler(
  filename = function() {
    isolate({
      currentDoc <- projProperties[['activeDoc']]
      s <- if(!isEmpty(currentDoc)){
        getOutputExtension(docList[[currentDoc]][['rmdOuputFormat']])
      } else ''
      paste(docList[[currentDoc]][['name']], s, sep='.')
    })
    },
  content = function(file) {
    ## file is a temp file without extension
    ## unfortunately pandoc assumes it's html when no extension and not explicit format option is specified.
    ## this causes problems when the intended output is a pdf.
    isolate({
      inputFile <- tempfile(fileext='.Rmd')
      on.exit(file.remove(inputFile), add=TRUE)
      currentDoc <- projProperties[['activeDoc']]
      if(!isEmpty(currentDoc) && !isEmpty(docList[[currentDoc]][['rmd']])){
        cat(docList[[currentDoc]][['rmd']], file=inputFile)

        ## latex runs into problem with short form path, so use normalizePath to convert file to long form
        cat(" ", file=file) # just to create file so normalizePath can work
        file <- normalizePath(file)
        file.remove(file)
        outFile <- paste(file,
                         getOutputExtension(docList[[currentDoc]][['rmdOuputFormat']]), sep='.')

        rmarkdown::render(normalizePath(inputFile), output_format=docList[[currentDoc]][['rmdOuputFormat']],
                          output_file=outFile, envir=getDatSheetEnv(), quiet = TRUE, clean=TRUE)

        file.rename(outFile, file)
      }
    })
  }
)

#' Insert string into text at the cursor position
#'
#' @param insert The string to insert
#' @param text The text to insert into
#' @param cursor A list with row and column given as 0-based indexes indicating the cursor position in text
#'
#' @examples
#' insertAtCursor('ss', 'ab\nc', list(row=1, column=1))
#' insertAtCursor('ss', 'aa\n', list(row=1, column=0))
insertAtCursor <- function(insert, text, cursor){
  textLines <- unlist(strsplit(text, "\n", fixed=TRUE))
  ## note strsplit doesn't capture the last line if it's empty
  if(text=='' || substring(text, nchar(text))=="\n") textLines <- c(textLines, "")
  row <- cursor$row + 1
  ans <- text
  if(row<=length(textLines)){
    nn <- nchar(textLines[row])
    if(cursor$column<=nn){
      sub1 <- if(cursor$column==0) "" else substr(textLines[row], 1, cursor$column)
      sub2 <- if(cursor$column==nn) "" else substr(textLines[row], cursor$column+1, nn)
      textLines[row] <- paste0(sub1, insert, sub2)
      ans <- paste(textLines, collapse="\n")
    }
  }
  ans
}

observe({
  v <- input$insertDatName
  if(v){
    isolate({
      if(!isEmpty(input$datNameToInsert) && !is.null(input$rmdCursor)){
        sNames <- datListNames()
        sheetName <- paste0('`', names(sNames)[match(input$datNameToInsert, sNames)], '`')
        if(input$withRChunk){
          sheetName <- paste('```{r, echo=FALSE}', sheetName, '```', sep='\n')
        }
        rmdNew <- insertAtCursor(sheetName, input$rmd, input$rmdCursor)
        updateAceEditor(session, 'rmd', value=rmdNew)
      }
    })
  }
})
observe({
  updateSelectInput(session, 'datNameToInsert', choices=datListNames())
})
observe({
  v <- input$insertSheetName
  if(v){
    isolate({
      if(!isEmpty(input$sheetNameToInsert) && !is.null(input$rmdCursor)){
        sNames <- sheetListNames()
        sheetName <- paste0('`', names(sNames)[match(input$sheetNameToInsert, sNames)], '`')
        if(input$withRChunk){
          sheetName <- paste('```{r, echo=FALSE}', sheetName, '```', sep='\n')
        }
        rmdNew <- insertAtCursor(sheetName, input$rmd, input$rmdCursor)
        updateAceEditor(session, 'rmd', value=rmdNew)
      }
    })
  }
})
observe({
  updateSelectInput(session, 'sheetNameToInsert', choices=sheetListNames())
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

