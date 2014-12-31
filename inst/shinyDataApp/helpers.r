## see: http://stackoverflow.com/questions/9298765/print-latex-table-directly-to-an-image-png-or-other
## also maybe useful: http://tex.stackexchange.com/questions/11866/compile-a-latex-document-into-a-png-image-thats-as-short-as-possible
make.png <- function(obj, resolution=NULL) { 
  name <- tempfile('x')
  texFile <- paste(name,".tex",sep="")
  pngFile <- paste(name,".png",sep="")
  sink(file=texFile)
  cat('
      \\documentclass[12pt]{report}
      \\usepackage[paperwidth=11in,paperheight=8in,noheadfoot,margin=0in]{geometry}
      \\usepackage[T1]{fontenc}
      \\usepackage{booktabs}
      \\begin{document}\\pagestyle{empty}
      {\\Large 
      ')
  save <- booktabs(); on.exit(table_options(save))
  latex(obj)
  table_options(save)
  cat('
      }\\end{document}
      ')
  sink()
  wd <- setwd(tempdir()); on.exit(setwd(wd))
  texi2dvi(file=texFile, index=FALSE)
  
  cmd <- paste("dvipng -T tight -o", 
               shQuote(pngFile), 
               if(!is.null(resolution)) paste("-D",resolution) else "",
               shQuote(paste(name,".dvi",sep="")))
  invisible(sys(cmd))
  cleaner <- c(".tex",".aux",".log",".dvi")
  invisible(file.remove(paste(name,cleaner,sep="")))
  pngFile
}

newGuid <- function(){
  gsub("-", "_", UUIDgenerate(), fixed=TRUE)
  
  #paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")
}

## preserve names of x
setdiff.c <<- function(x, y){
  z <- setdiff(x, y)
  x1 <- names(x)
  if(!is.null(x1)){
    names(x1) <- x
    names(z) <- x1[z]
    z
  } else z  
}
isEmpty <<- function(x){
  is.null(x) || length(x)==0 || all(is.na(x)) || all(x=='')
}

ifnull <- function(x, d){
  if(is.null(x)) d else x
}
ifempty <- function(x, d){
  if(isEmpty(x)) d else x
}

null2String <- function(x){
  ifnull(x,"")
}
empty2NULL <- function(x){
  ifempty(x,NULL)
}

getDefaultMeasures <- function(dat){
  names(dat)[sapply(dat, function(x) typeof(x)=="double")]
}

getDefaultFieldsList <- function(dat){
  x <- lapply(names(dat), function(n) reactiveValues('name'=n, 'type'=typeof(dat[[n]])))
  names(x) <- names(dat)
  x
}

convertSheetNameToDatName <- function(sheetName){
  paste(sheetName, '(Aggregated Data)', sep=' ')
}

names2formula <- function(nms){
  if(!isEmpty(nms)){
    paste(nms, collapse=" + ")
  } else " . "
}

isFieldUninitialized <- function(obj, field){
  class(obj[[field]])=="uninitializedField"
}

are.vectors.different <- function(x, y){
  if(isEmpty(x)){
    !isEmpty(y)
  } else {
    isEmpty(y) || any(x!=y)
  }
}

## convert d so that the specified columns of d are measures and the rest are dims
## measures: can be either logical or charecter vector
forceMeasures <<- function(d, measures){
  if(!is.logical(measures)) measures <- names(d) %in% measures
  for(n in seq_along(measures)){
    if(measures[n]){
      if(!is.numeric(d[[n]])) d[[n]] <- as.numeric(d[[n]])
    } else {
      if(!is.factor(d[[n]])) d[[n]] <- as.factor(d[[n]])
    }                                
  }
  d
}

DatClass <- setRefClass("DatClass", fields=c("staticProperties","dynamicProperties","datR","fieldNames","moltenDat","moltenNames"),
                        methods=list(setDatDependencies=function(){
                          fieldNames <<- reactive({
                            if(length(dynamicProperties[['fieldsList']])){
                              x <- names(dynamicProperties[['fieldsList']])
                              names(x) <- make.unique(sapply(dynamicProperties[['fieldsList']], 
                                                             function(y) y[['name']]), sep='_')
                              x
                            } else c() 
                          })
                          datR <<- reactive({
                            if(is.null(dynamicProperties[['dat']])){
                              ## fetch from database etc.
                              
                            } else {
                              forceMeasures(dynamicProperties[['dat']],
                                             dynamicProperties[['measures']])
                            }
                          })
                          
                          measureName <- 'MeasureNames'
                          moltenDat <<- reactive({
                            if(!isEmpty(dynamicProperties[['measures']])){
                              melt(datR(), measure.vars=dynamicProperties[['measures']], 
                                   variable_name=measureName)
                            }                            
                          })
                          moltenNames <<- reactive({
                            x <- setdiff.c(fieldNames(), dynamicProperties[['measures']])
                            x[measureName] <- measureName
                            x['MeasureValues'] <- MoltenMeasuresName
                            x
                          })
                        },
                        removeDatDependencies=function(){
                          datR <<- NULL; fieldNames <<- NULL; moltenDat <<- NULL; moltenNames <<- NULL
                        }))

createNewDatClassObj <- function(dat=NULL, name='Data', nameOriginal=NULL, type='file'){
  x <- DatClass$new('staticProperties'=list('type'=type, 'nameOriginal'=nameOriginal))
  if(is.null(dat)){
    x[['dynamicProperties']] <- reactiveValues('fieldsList'=list())
  } else {
    activeField <- if(length(names(dat))) names(dat)[1] else ''
    x[['dynamicProperties']] <- reactiveValues('dat'=dat, 'name'=name, 
                                               'fieldsList'=getDefaultFieldsList(dat),
                                               'activeField'=activeField,
                                               'measures'=getDefaultMeasures(dat))
  }
  x$setDatDependencies() 
  x
}


SheetClass <- setRefClass("SheetClass", 
                          fields=c("dynamicProperties","datR",
                                   "fieldNames","measuresR","plotCore","plotR",
                                   "tableR","layerNames"))
createNewLayer <- function(){
  reactiveValues('geom'='point', 'statType'='identity', 'yFun'='sum', 'layerPositionType'='identity',
                 'activeAes'='aesX',
                 'aesList'=sapply(AesChoicesSimpleList, 
                                  function(x) reactiveValues('aesAggregate'=FALSE,'aesDiscrete'=TRUE,'aesMapOrSet'='map'), simplify=FALSE))
}
createNewSheetObj <- function(name='Sheet'){
  SheetClass$new(
    'dynamicProperties'=reactiveValues(
      'name'=name, 
      'datId'='', 'combineMeasures'=FALSE, 'outputType'='plot',
      'columns'='', 'colChoices'='',
      'rows'='', 'rowChoices'='',
      'outputTable'=NULL,
      'outputDataframe'=NULL,
      'layerList'=list(
        'Plot'=createNewLayer()),
      'activeLayer'='Plot'))
}



