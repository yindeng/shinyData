
source('helpers.r', local=TRUE)

shinyServer(function(input, output, session) {

  sessionEnv <- environment()
  projProperties <- reactiveValues('activeDat'='')
  sessionProperties <- reactiveValues('runDatCode'=0)
  updateInput <- reactiveValues('activeDat'=0,'datName'=0,'activeField'=0,'fieldName'=0,'measures'=0,
                                'activeSheet'=0,'sheetName'=0,'sheetDatId'=0,'combineMeasures'=0,
                                'sheetColumns'=0,'sheetRows'=0,'sheetOutput'=0,'sheetPlotLayer'=0,
                                'sheetLayerAes'=0,'aesField'=0,'aesAggregate'=0,'aesAggFun'=0,'aesDiscrete'=0,
                                'layerGeom'=0,'layerStatType'=0,'layerYFun'=0,
                                'layerPositionType'=0, 'layerPositionWidth'=0, 'layerPositionHeight'=0,
                                'activeDoc'=0,'docName'=0,'docRmd'=0,'rmdOuputFormat'=0,
                                'customizeItem'=0, 'plotXlab'=0, 'plotYlab'=0, 'plotTitle'=0,
                                'textFamily'=0, 'textFace'=0, 'textColor'=0,'textSize'=0, 'textHjust'=0, 'textVjust'=0,
                                'textAngle'=0, 'textLineheight'=0)

  triggerUpdateInput <- function(inputId){
    if(is.null(updateInput[[inputId]])) updateInput[[inputId]] <- 0
    updateInput[[inputId]] <- updateInput[[inputId]] + 1
  }

  datList <- list()
  makeReactiveBinding('datList')
  datListNames <- reactive({
    if(length(datList)){
      x <- names(datList)
      names(x) <- sapply(datList, function(y) y[['dynamicProperties']][['name']])
      x
    } else c('')
  })

  docList <- list()
  makeReactiveBinding('docList')
  docListNames <- reactive({
    if(length(docList)){
      x <- names(docList)
      names(x) <- sapply(docList, function(y) y[['name']])
      x
    } else c('')
  })
  addDoc <- function(){
    newDoc <- paste("Doc_",newGuid(),sep="")
    existingNames <- names(docListNames())
    ## make sure the new name is different
    newName <- make.unique(c(existingNames, 'Doc'), sep='_')[length(existingNames)+1]

    docObj <- reactiveValues('name'=newName, 'rmdOuputFormat'='pdf_document')
    docList[[newDoc]] <<- docObj
    projProperties[['activeDoc']] <- newDoc
  }
  isolate(addDoc())



  sheetList <- list()
  makeReactiveBinding('sheetList')

  sheetListNames <- reactive({
    if(length(sheetList)){
      x <- names(sheetList)
      names(x) <- sapply(sheetList, function(y) y[['dynamicProperties']][['name']])
      x
    } else c('')
  })

  getDatSheetEnv <- function(excludeDat=NULL){
    env <- new.env(parent = globalenv())
    nn <- sheetListNames()
    for(n in names(nn)){
      ## using local is essential to make delayedAssign work
      local({
        nId <- nn[n]
        local(delayedAssign(n, sheetList[[nId]][['plotR']](), assign.env=env))
      })
    }
    nn <- setdiff.c(datListNames(), excludeDat)
    for(n in names(nn)){
      local({
        nId <- nn[n]
        local(delayedAssign(n, datList[[nId]][['datR']](), assign.env=env))
      })
    }
    env
  }


  datUpdated <- function(currentDat){
    dat <- datList[[currentDat]][['datRaw']]()
    if(!isEmpty(dat)){
      ## need to adjust fieldsList to be consistent with the new dat
      newFields <- names(dat)
      if(length(newFields)){
        fieldsList <- datList[[currentDat]][['dynamicProperties']][['fieldsList']]
        measures <- datList[[currentDat]][['dynamicProperties']][['measures']]
        if(is.null(fieldsList)) fieldsList <- list()
        if(is.null(measures)) measures <- c()

        # delete deprecated fields
        deleted <- setdiff(names(fieldsList), newFields)
        for(nn in deleted){
          fieldsList[[nn]] <- NULL
        }
        measures <- setdiff(measures, deleted)

        # add new fields
        added <- setdiff(newFields, names(fieldsList))
        for(nn in added){
          fieldsList[[nn]] <- reactiveValues('name'=nn, 'type'=typeof(dat[[nn]]))
        }
        measures <- c(measures, getDefaultMeasures(dat, added))

        datList[[currentDat]][['dynamicProperties']][['fieldsList']] <<- fieldsList
        datList[[currentDat]][['dynamicProperties']][['measures']] <<- measures

        activeField <- datList[[currentDat]][['dynamicProperties']][['activeField']]
        if(isEmpty(activeField) || !(activeField %in% newFields)){
          datList[[currentDat]][['dynamicProperties']][['activeField']] <<- names(fieldsList)[1]
        }
      }
    }
  }

  setDatReactives <- function(currentDat){
    datList[[currentDat]][['fieldNames']] <<- reactive({
      if(length(datList[[currentDat]][['dynamicProperties']][['fieldsList']])){
        x <- names(datList[[currentDat]][['dynamicProperties']][['fieldsList']])
        names(x) <- make.unique(sapply(datList[[currentDat]][['dynamicProperties']][['fieldsList']],
                                       function(y) y[['name']]), sep='_')
        x
      } else c()
    })

    datList[[currentDat]][['datRaw']] <<- reactive({
      dat <- NULL
      if(datList[[currentDat]][['staticProperties']][['type']]=='code'){
        sessionProperties$runDatCode
        isolate({
          code <- datList[[currentDat]][['dynamicProperties']][['datCode']]
          if(!isEmpty(code)){
            dat <- tryCatch(eval(parse(text=code), envir=getDatSheetEnv(excludeDat=currentDat)),
                            error=function(e) {
                              createAlert(session,'datCodeAlert',
                                          title='Error in R code:',
                                          message=e$message,
                                          type='warning', dismiss=TRUE, append=FALSE)
                              NULL
                            })
          }
        })
      } else {
        dat <- datList[[currentDat]][['dynamicProperties']][['dat']]
      }
      dat
    })
    datList[[currentDat]][['datR']] <<- reactive({
      dat <- datList[[currentDat]][['datRaw']]()
      if(!is.null(dat)){
        dat <- forceMeasures(dat,
                             datList[[currentDat]][['dynamicProperties']][['measures']])
        label(dat, self=FALSE) <- names(datList[[currentDat]][['fieldNames']]())
        if(is.data.table(dat)) dat else as.data.table(dat)
      }
    })

    measureName <- 'MeasureNames'
    datList[[currentDat]][['moltenDat']] <<- reactive({
      if(!isEmpty(datList[[currentDat]][['dynamicProperties']][['measures']])){
        melt(datList[[currentDat]][['datR']](),
             measure.vars=datList[[currentDat]][['dynamicProperties']][['measures']],
             variable.name=measureName)
      }
    })
    datList[[currentDat]][['moltenNames']] <<- reactive({
      x <- setdiff.c(datList[[currentDat]][['fieldNames']](),
                     datList[[currentDat]][['dynamicProperties']][['measures']])
      x[measureName] <- measureName
      x['MeasureValues'] <- MoltenMeasuresName
      x
    })
  }

  setAesReactives <- function(currentSheet, currentLayer, currentAes){
      sheetList[[currentSheet]][['dynamicProperties'
                                 ]][['layerList']][[currentLayer]][['aesList']][[currentAes]][['canFieldBeContinuous']] <<- reactive({
                                   aes <- sheetList[[currentSheet]][['dynamicProperties'
                                                                     ]][['layerList']][[currentLayer]][['aesList']][[currentAes]]
                                   measures <- sheetList[[currentSheet]][['measuresR']]()
                                   field <- aes[['aesField']]
                                   if(isEmpty(field)){
                                     field <- sheetList[[currentSheet]][['dynamicProperties'
                                               ]][['layerList']][['Plot']][['aesList']][[currentAes]][['aesField']]
                                   }
                                   aes[['aesAggregate']] || (!isEmpty(field) && field %in% measures)
                                 })
  }


  ## create sheet reactives and defaults
  setSheetReactives <- function(currentSheet){
    if(isFieldUninitialized(sheetList[[currentSheet]],'layerNames')){
      sheetList[[currentSheet]][['layerNames']] <<- reactive({
        sl <- isolate(sheetList)
        if(!isEmpty(sl[[currentSheet]][['dynamicProperties']][['layerList']])){
          names(sl[[currentSheet]][['dynamicProperties']][['layerList']])
        } else c()
      })
    }


    if(isFieldUninitialized(sheetList[[currentSheet]],'fieldNames')){
      sheetList[[currentSheet]][['fieldNames']] <<- reactive({
        sl <- isolate(sheetList)
        dl <- isolate(datList)
        currentDat <- sl[[currentSheet]][['dynamicProperties']][['datId']]
        combineMeasures <- sl[[currentSheet]][['dynamicProperties']][['combineMeasures']]

        if(!isEmpty(currentDat)){
          currentDatObj <- dl[[currentDat]]
          if(combineMeasures) currentDatObj[['moltenNames']]() else currentDatObj[['fieldNames']]()
        }
      })
    }

    if(isFieldUninitialized(sheetList[[currentSheet]],'measuresR')){
      sheetList[[currentSheet]][['measuresR']] <<- reactive({
        sl <- isolate(sheetList)
        dl <- isolate(datList)
        currentDat <- sl[[currentSheet]][['dynamicProperties']][['datId']]
        combineMeasures <- sl[[currentSheet]][['dynamicProperties']][['combineMeasures']]

        if(!isEmpty(currentDat)){
          currentDatObj <- dl[[currentDat]]
          if(combineMeasures) c(MoltenMeasuresName) else currentDatObj[['dynamicProperties']][['measures']]
        }
      })
    }

    if(isFieldUninitialized(sheetList[[currentSheet]],'datR')){
      sheetList[[currentSheet]][['datR']] <<- reactive({
        sl <- isolate(sheetList)
        dl <- isolate(datList)
        currentDat <- sl[[currentSheet]][['dynamicProperties']][['datId']]
        combineMeasures <- sl[[currentSheet]][['dynamicProperties']][['combineMeasures']]

        if(!isEmpty(currentDat)){
          currentDatObj <- dl[[currentDat]]
          if(combineMeasures) currentDatObj[['moltenDat']]() else currentDatObj[['datR']]()
        }
      })
    }

    if(isFieldUninitialized(sheetList[[currentSheet]],'tableR')){
      sheetList[[currentSheet]][['tableR']] <<- reactive({
        sl <- isolate(sheetList)
        tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
                   (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )
      })
    }



    if(isFieldUninitialized(sheetList[[currentSheet]],'plotCore')){
      ## Helpful ggplot references:
      ## http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/
      ## http://www.ling.upenn.edu/~joseff/rstudy/summer2010_ggplot2_intro.html
      ## http://learnr.wordpress.com/2009/03/17/ggplot2-barplots/
      ## http://sape.inf.usi.ch/quick-reference/ggplot2
      ## http://ggplot2.org/book/

      ## http://stackoverflow.com/questions/20249653/insert-layer-underneath-existing-layers-in-ggplot2-object

      ## current solutions:
      ## http://blog.ouseful.info/2011/08/03/working-visually-with-the-ggplot2-web-interface/  (no support for saving project)
      ## Deducer


      sheetList[[currentSheet]][['plotCore']] <<- reactive({
        sl <- isolate(sheetList)
        layerList <- sl[[currentSheet]][['dynamicProperties']][['layerList']]
        cc <- empty2NULL(sl[[currentSheet]][['dynamicProperties']][['columns']])
        rr <- empty2NULL(sl[[currentSheet]][['dynamicProperties']][['rows']])
        datSheet <- sl[[currentSheet]][['datR']]()
        validate(need(!isEmpty(datSheet), label='Data'))

        gg <- NULL
        for(i in c('bar','line','point')) update_geom_defaults(i, list(colour = "darkblue", fill = "darkblue"))

        aes.base <- layerList[['Plot']][['aesList']]
        for(currentLayer in names(layerList)){
          layer.current <- layerList[[currentLayer]]

          stat <- empty2NULL(layer.current[['statType']])
          geom <- empty2NULL(layer.current[['geom']])
          fun.y <- empty2NULL(layer.current[['yFun']])
          position <- empty2NULL(layer.current[['layerPositionType']])
          pWidth <- empty2NULL(layer.current[['layerPositionWidth']])
          pHeight <- empty2NULL(layer.current[['layerPositionHeight']])

          if(!is.null(geom) && !is.null(stat) && !is.null(position)){
            ## get effective aesthetics taking into account of inheritance
            aes.current <- layer.current[['aesList']][isolate(unlist(layer.current[['aesChoices']], use.names=FALSE))]
            aes.current <- sapply(names(aes.current), function(n){
              temp <- reactiveValuesToList(aes.current[[n]]) # converting to list so we can modify it
              if(are.vectors.different(temp[['aesMapOrSet']],'set')){
                temp[['aesField']] <- ifempty(temp[['aesField']], empty2NULL(aes.base[[n]][['aesField']]))
                if(!is.null(temp[['aesField']])){
                  if(temp[['aesAggregate']]){
                    temp[['aesFieldOriginal']] <- temp[['aesField']]
                    temp[['aesField']] <- paste(temp[['aesFieldOriginal']], temp[['aesAggFun']], sep='_')
                  }
                }
              }
              temp
            }, simplify=FALSE)
            #browser()
            aes.current <- aes.current[sapply(aes.current,
                                              function(x) {
                                                isSetting <- !are.vectors.different(x[['aesMapOrSet']],'set')
                                                (isSetting && !isEmpty(x[['aesValue']])) || (!isSetting && !isEmpty(x[['aesField']]))
                                              })]

            borderColor <- aes.current[['aesBorderColor']]
            aes.current[['aesBorderColor']] <- NULL
            if(geom %in% c('bar','area','boxplot','density','smooth')){
              aes.current[['aesFill']] <- aes.current[['aesColor']]
              aes.current[['aesColor']] <- borderColor
            }

            ## aesthetics validation
            validate(
              need(!is.null(aes.current[['aesX']]), label='X')
            )
            if(stat %in% c('identity', 'boxplot', 'smooth')){
              validate(
                need(!is.null(aes.current[['aesY']]), label='Y')
              )
            }
            if(geom=='text'){
              validate(
                need(!is.null(aes.current[['aesLabel']]), label='Label')
              )
            }
            if(geom=='boxplot' && stat=='identity'){
              validate(
                need(!is.null(aes.current[['aesYmin']]), label='Y Min')
              )
              validate(
                need(!is.null(aes.current[['aesYmax']]), label='Y Max')
              )
              validate(
                need(!is.null(aes.current[['aesLower']]), label='Y Lower')
              )
              validate(
                need(!is.null(aes.current[['aesMiddle']]), label='Y Middle')
              )
              validate(
                need(!is.null(aes.current[['aesUpper']]), label='Y Upper')
              )
            }
            if(position=='fill'){
              validate(
                need(!is.null(aes.current[['aesYmax']]), label='Y Max')
              )
            }

            i.map <- sapply(aes.current, function(x) are.vectors.different(x[['aesMapOrSet']],'set'))
            aes.map <- aes.current[i.map]
            aes.set <- aes.current[!i.map]

            ## aggregate data for layer
            datLayer <- datSheet
            aes.toAgg <- aes.map[sapply(aes.map, function(x) x[['aesAggregate']])]
            if(length(aes.toAgg)){
              # get rid of duplicates to avoid aggregating the same field the same way twice
              dups <- duplicated(sapply(aes.toAgg, function(x) x[['aesField']]))
              aes.toAgg <- aes.toAgg[!dups]

              # some validation
              agg.fields <- sapply(aes.toAgg, function(x) x[['aesFieldOriginal']])
              overlaps <- intersect(agg.fields, c(rr,cc))
              validate(need(isEmpty(overlaps), 'Can not aggregate fields used in faceting.'))

              # build the j-expression from string; there may be a better way
              agg.str <- sapply(aes.toAgg, function(x) paste(x[['aesAggFun']], '(', x[['aesFieldOriginal']], ')', sep=''))
              agg.str <- paste(sapply(aes.toAgg, function(x) x[['aesField']]), agg.str, sep='=', collapse=', ')
              agg.str <- paste0('list(', agg.str, ')')
              agg.exp <- parse(text=agg.str)[[1]]
              groupBy <- unique(c(rr,cc,sapply(aes.map[sapply(aes.map, function(x) !(x[['aesAggregate']]))],
                                               function(x) x[['aesField']])))
              #validate(need(!isEmpty(groupBy), 'Please provide at least one field that is not being aggregated.'))
              datLayer <- eval(bquote(datSheet[, .(agg.exp), by=.(groupBy)]))

              if(currentLayer=='Plot'){
                ## add to dataList
                outDf <- datLayer
                isolate({
                  sheetNameDat <- convertSheetNameToDatName(sheetList[[currentSheet]][['dynamicProperties']][['name']])
                  if(is.null(datList[[currentSheet]])){
                    addDat(outDf, name=sheetNameDat, type='sheet', id=currentSheet)
                  } else {
                    datList[[currentSheet]][['dynamicProperties']][['dat']] <<- outDf
                    datUpdated(currentSheet)
                  }
                })
              }
            }


            ## get ready to ggplot
            if(stat=='summary'){ # need to re-point all the other aesthetics to ..y..
              sapply(setdiff(names(aes.map), 'aesY'),
                     function(x){
                       if(aes.map[[x]][['aesField']]==aes.map[['aesY']][['aesField']]){
                         aes.map[[x]][['aesField']] <<- InternalY
                       }
                     })
            }


            ## list of set values
            aes.set.args <- list()
            if(length(aes.set)){
              aes.set.args <- lapply(aes.set,
                                     function(x) {
                                       x[['aesValue']]
                                     }
              )
              names(aes.set.args) <- tolower(substring(names(aes.set), 4)) # get rid of the 'aes' prefix
            }

            aes.args <- lapply(aes.map,
                               function(x) {
                                 if(x[['canFieldBeContinuous']]()){
                                   paste(ifelse(x[['aesDiscrete']], 'as.factor(', 'as.numeric('),
                                         x[['aesField']], ')', sep='')
                                 } else {
                                   x[['aesField']]
                                 }
                               }
            )
            names(aes.args) <- tolower(substring(names(aes.map), 4)) # get rid of the 'aes' prefix

            aess <- do.call('aes_string', aes.args)
            position <- do.call(paste('position', position, sep='_'),
                                list(width=pWidth, height=pHeight))
            #browser()
            if(is.null(gg)) gg <- ggplot()
            gg <- gg + do.call(paste('geom',geom,sep='_'),
                               c(aes.set.args, list(mapping=aess, data=datLayer, stat=stat, fun.y=fun.y, position=position)))

            ## scales
            allScales <- lapply(aes.map, function(x) x[['scale']])
            if(!isEmpty(allScales)){
              allScales <- allScales[!sapply(allScales, isEmpty)]
              for(a in names(allScales)){
                scale.args.mandatory <- list('name'=null2String(allScales[[a]][['legendTitle']]))
                legend.type <- null2String(allScales[[a]][['legendType']])
                scale.args.optional <- list('guide'=if(legend.type != 'custom') legend.type else {
                  guide <- allScales[[a]][['legend']]
                  if(!isEmpty(guide)){
                    guide <- guide[!sapply(guide, isEmpty)]
                    names(guide) <- make.names(names(guide), allow_=FALSE)  # convert _ to .
                    guide.name <- if((a=='aesColor' || a=='aesFill') && !aes.map[[a]][['aesDiscrete']]) 'guide_colorbar' else 'guide_legend'
                    do.call(guide.name, guide)
                  }
                })
                call.name <- ''
                call.name12 <- paste('scale', tolower(substring(a, 4)), sep='_')
                call.name.default <- paste(call.name12, if(aes.map[[a]][['aesDiscrete']]) 'discrete' else 'continuous', sep='_')
                switch(a,
                       'aesLineType'={call.name <- call.name.default},
                       'aesShape'={call.name <- call.name.default
                                   scale.args.optional <- c(scale.args.optional, list('solid'=allScales[[a]][['shapeSolid']]))},
                       'aesSize'={call.name <- call.name.default
                                  scale.args.optional <- c(scale.args.optional, list('range'=allScales[[a]][['sizeRange']]))},
                       'aesColor'=, 'aesFill'=
                         if(aes.map[[a]][['aesDiscrete']]){ # discrete
                           call.name <- paste(call.name12,
                                              if(isEmpty(allScales[[a]][['discreteColorScaleType']])) 'discrete' else allScales[[a]][['discreteColorScaleType']],
                                              sep='_')
                           switch(null2String(allScales[[a]][['discreteColorScaleType']]),
                                  'brewer'={
                                    scale.args.optional <- c(scale.args.optional, list('palette'=allScales[[a]][['colorBrewerPallete']]))
                                  }
                           )
                         } else {  # continuous
                           diverging <- allScales[[a]][['colorDiverging']]
                           call.name <- paste(call.name12, if(diverging) 'gradient2' else 'gradient', sep='_')
                           scale.args.optional <- c(scale.args.optional, list('low'=allScales[[a]][['colorLow']], 'high'=allScales[[a]][['colorHigh']],
                                                                              'na.value'=allScales[[a]][['colorNA_value']]))
                           if(diverging) scale.args.optional <- c(scale.args.optional, list('mid'=allScales[[a]][['colorMid']],
                                                                                            'midpoint'=allScales[[a]][['colorMidpoint']]))

                         }
                )

                if(call.name!=''){
                  scale.args.optional <- scale.args.optional[!sapply(scale.args.optional, isEmpty)]
                  scale.call <- do.call(call.name, c(scale.args.mandatory, scale.args.optional))
                  gg <- gg + scale.call
                }
              }
            }


          }
        }
        if(!is.null(gg)){
          if(!isEmpty(cc) || !isEmpty(rr)){
            gg <- gg + facet_grid(as.formula(paste(names2formula(rr), names2formula(cc), sep=" ~ ")))
          }
          gg <- gg + theme_bw()
        }
        gg
      })
    }

    if(isFieldUninitialized(sheetList[[currentSheet]],'plotR')){
      sheetList[[currentSheet]][['plotR']] <<- reactive({
        sl <- isolate(sheetList)
        layerList <- sl[[currentSheet]][['dynamicProperties']][['layerList']]
        aes.base <- layerList[['Plot']][['aesList']]

        fieldNames <- sl[[currentSheet]][['fieldNames']]()

        gg <- sl[[currentSheet]][['plotCore']]()
        if(!is.null(gg)){
          themeElementCalls <-
            sapply(sheetList[[currentSheet]][['dynamicProperties']][['formatting']],
                   simplify = FALSE, USE.NAMES = TRUE,
                   function(cus){
                     eleBlank <- attr(cus, 'elementBlank')
                     if(!is.null(eleBlank) && eleBlank) return(element_blank())
                     if(!isEmpty(cus)){
                       cus1 <- cus[!sapply(cus, isEmpty)]
                       names(cus1) <- tolower(substring(names(cus1), 5)) # get rid of the 4-char prefix like 'text', 'rect', etc.
                       switch(attr(cus, 'type'),
                              'unit'=if(!isEmpty(cus1$x) && !isEmpty(cus1$units)) do.call('unit', cus1),
                              'character'=if(!isEmpty(cus1$mainvalue)){
                                if(cus1$mainvalue!='custom_'){
                                  cus1$mainvalue
                                } else {
                                  c(cus1$altvalue1, cus1$altvalue2)
                                }
                              },
                              do.call(attr(cus, 'type'), cus1))
                     }
                   })
          themeElementCalls <- themeElementCalls[!sapply(themeElementCalls, is.null)]


          gg <- gg + xlab(sheetList[[currentSheet]][['dynamicProperties']][['plotXlab']]) +
            ylab(sheetList[[currentSheet]][['dynamicProperties']][['plotYlab']]) +
            ggtitle(sheetList[[currentSheet]][['dynamicProperties']][['plotTitle']])
          if(length(themeElementCalls)){
            gg <- gg + do.call('theme', themeElementCalls)
          }

        }
        gg
      })
    }

  }

  addDat <- function(dat=NULL, name=NULL, type='file', id=NULL){
    currentDat <- ifnull(id, paste('dat_',newGuid(),sep=''))
    existingNames <- names(datListNames())
    ## make sure the new name is different
    newName <- make.unique(c(existingNames, ifempty(name, 'Data')), sep='_')[length(existingNames)+1]

    datList[[currentDat]] <<- createNewDatClassObj(dat, name=newName,
                                              nameOriginal=name, type=type)
    setDatReactives(currentDat)
    datUpdated(currentDat)
    if(is.null(id)){
      projProperties[['activeDat']] <<- currentDat
      triggerUpdateInput('activeDat')
    }
  }
  addSheet <- function(){
    newSheet <- paste("Sheet_",newGuid(),sep="")
    existingNames <- names(sheetListNames())
    ## make sure the new name is different
    newName <- make.unique(c(existingNames, 'Sheet'), sep='_')[length(existingNames)+1]

    sheetObj <- createNewSheetObj(newName)
    sheetList[[newSheet]] <<- sheetObj
    setSheetReactives(newSheet)
    projProperties[['activeSheet']] <- newSheet

    for(currentLayer in names(sheetObj[['dynamicProperties']][['layerList']])){
      for(currentAes in names(sheetObj[['dynamicProperties']][['layerList']][[currentLayer]][['aesList']])){
        setAesReactives(newSheet, currentLayer, currentAes)
      }
    }
  }



  isDatBasedonSheet <- function(datId, sheetId){
    if(!is.null(datList[[datId]])){
      while(datList[[datId]][['staticProperties']][['type']] == 'sheet'){
        if(datId==sheetId) return(TRUE)
        datId <- sheetList[[datId]][['dynamicProperties']][['datId']]
      }
    }
    FALSE
  }



  source('data.r', local=TRUE)
  source('sheets.r', local=TRUE)
  source('sheetsCustomize.r', local=TRUE)
  isolate(addSheet())
  source('project.r', local=TRUE)
  source('docs.r', local=TRUE)

  observe({
    v <- input$importFonts
    isolate({
      if(!is.null(v) && v==1){  # so it's executed the first time the button is clicked
        if(!require(extrafont)) install.packages('extrafont')
        extrafont::font_import(prompt=FALSE) # this only needs run once but takes a long time
        # todo: alert user
      }
    })
  })

})


