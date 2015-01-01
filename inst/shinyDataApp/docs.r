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


## preview output
output$rmdOutput <- renderUI({
  if(input$rmdTabs=='Preview'){
    isolate({
      srcCode <- input$rmd
      if(!isEmpty(srcCode)){
        HTML(knit2html(text = srcCode, fragment.only = TRUE, quiet = TRUE))
      }
    })
  }
})
