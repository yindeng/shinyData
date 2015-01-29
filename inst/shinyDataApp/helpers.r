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
  save <- booktabs(); on.exit(table_options(save), add=TRUE)
  latex(obj)

  cat('
      }\\end{document}
      ')
  sink()
  wd <- setwd(tempdir()); on.exit(setwd(wd), add=TRUE)
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

## list with reference semantics
## for later use (challenge: how to delete a field dynamically similarly to deleting a list item by setting it to NULL)
refList <- setRefClass("refList")
names.refList <- function(x) ls(x)[-1] # get rid of "getClass"

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
                              dat <- forceMeasures(dynamicProperties[['dat']],
                                             dynamicProperties[['measures']])
                              label(dat, self=FALSE) <- names(fieldNames())
                              if(is.data.table(dat)) dat else as.data.table(dat)
                            }
                          })

                          measureName <- 'MeasureNames'
                          moltenDat <<- reactive({
                            if(!isEmpty(dynamicProperties[['measures']])){
                              melt(datR(), measure.vars=dynamicProperties[['measures']],
                                   variable.name=measureName)
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
  geom <- 'point'
  stat <- 'identity'
  reactiveValues('geom'=geom, 'statType'=stat, 'yFun'='sum', 'layerPositionType'='identity',
                 'activeAes'='aesX',
                 'aesChoices'=getAesChoices(geom, stat),
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



# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <<- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


