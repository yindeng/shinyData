## a simple color picker
# AllColors <- rgb(t(col2rgb(colors())), maxColorValue=255)
# names(AllColors) <- colors()
AllColors <- colors()
colorInput <- function(inputId, label, value=NULL){
  selectInput(inputId, label, choices=c('Choose'='', AllColors), selected=value)
}

updateColorInput <- function(session, inputId, label = NULL, value = NULL){
  updateSelectInput(session, inputId, label, selected=value)
}

#rgb(t(col2rgb(head(colors()))), maxColorValue=255)
