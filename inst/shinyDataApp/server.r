
source('helpers.r', local=TRUE)

shinyServer(function(input, output, session) {

  projProperties <- reactiveValues('activeDat'='')
  sessionProperties <- reactiveValues()
  updateInput <- reactiveValues('activeDat'=0,'datName'=0,'activeField'=0,'fieldName'=0,'measures'=0,
                                'activeSheet'=0,'sheetName'=0,'sheetDatId'=0,'combineMeasures'=0,
                                'sheetColumns'=0,'sheetRows'=0,'sheetOutput'=0,'sheetPlotLayer'=0,
                                'sheetLayerAes'=0,'aesField'=0,'aesAggregate'=0,'aesAggFun'=0,'aesDiscrete'=0,
                                'layerGeom'=0,'layerStatType'=0,'layerYFun'=0,
                                'layerPositionType'=0, 'layerPositionWidth'=0, 'layerPositionHeight'=0,
                                'activeDoc'=0,'docName'=0)
  #                                 'layerX'=0,'layerY'=0,
  #                                 'layerColor'=0,'layerFill'=0,'layerSize'=0,'layerAlpha'=0,'layerLabel'=0,
  #                                 'layerLineType'=0)

  triggerUpdateInput <- function(inputId){
    updateInput[[inputId]] <<- updateInput[[inputId]] + 1
  }

  datList <- list()
  makeReactiveBinding('datList')
  datListNames <- reactive({
    if(length(datList)){
      x <- names(datList)
      names(x) <- make.unique(sapply(datList, function(y) y[['dynamicProperties']][['name']]), sep='_')
      x
    } else c('')
  })

  docList <- list()
  makeReactiveBinding('docList')
  docListNames <- reactive({
    if(length(docList)){
      x <- names(docList)
      names(x) <- make.unique(sapply(docList, function(y) y[['name']]), sep='_')
      x
    } else c('')
  })

  sheetList <- list()
  setAesReactives <- function(currentSheet, currentLayer, currentAes){
    local({
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
    })
  }
  addSheet <- function(){
    newSheet <- paste("Sheet_",newGuid(),sep="")
    sheetObj <- createNewSheetObj()
    sheetList[[newSheet]] <<- sheetObj
    projProperties[['activeSheet']] <- newSheet

    for(currentLayer in names(sheetObj[['dynamicProperties']][['layerList']])){
      for(currentAes in names(sheetObj[['dynamicProperties']][['layerList']][[currentLayer]][['aesList']])){
        setAesReactives(newSheet, currentLayer, currentAes)
      }
    }
  }
  isolate(addSheet())

  makeReactiveBinding('sheetList')



  ## create sheet reactives and defaults
  observe({#browser()
    for(currentSheet in names(sheetList)){
      isolate(local({

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
                aes.current <- aes.current[sapply(aes.current,
                     function(x) !are.vectors.different(x[['aesMapOrSet']],'set') || !is.null(x[['aesField']]))]

                borderColor <- aes.current[['aesBorderColor']]
                aes.current[['aesBorderColor']] <- NULL
                if(geom %in% c('bar','area','boxplot')){
                  aes.current[['aesFill']] <- aes.current[['aesColor']]
                  aes.current[['aesColor']] <- borderColor
                }

                i.map <- sapply(aes.current, function(x) are.vectors.different(x[['aesMapOrSet']],'set'))
                aes.map <- aes.current[i.map]
                aes.set <- aes.current[!i.map]

                ## aesthetics validation
                validate(
                  need(!is.null(aes.current[['aesX']]), label='X')
                )
                if(stat=='identity'){
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

                  # build the call for ddply
                  .args <- lapply(aes.toAgg, function(x) parse(text=paste(x[['aesAggFun']], '(', x[['aesFieldOriginal']], ')', sep=''))[[1]])
                  names(.args) <- sapply(aes.toAgg, function(x) x[['aesField']])
                  .args[['.data']] <- datSheet
                  .args[['.variables']] <- unique(c(rr,cc,sapply(aes.map[sapply(aes.map, function(x) !(x[['aesAggregate']]))],
                                                                 function(x) x[['aesField']])))
                  .args[['.fun']] <- summarize
                  datLayer <- do.call('ddply', .args)

                  if(currentLayer=='Plot'){
                    ## add to dataList
                    outDf <- datLayer
                    isolate({
                      sheetNameDat <- convertSheetNameToDatName(sheetList[[currentSheet]][['dynamicProperties']][['name']])
                      if(is.null(datList[[currentSheet]])){
                        datList[[currentSheet]] <<- createNewDatClassObj(outDf, name=sheetNameDat, type='sheet')
                      } else {
                        datList[[currentSheet]][['dynamicProperties']][['dat']] <<- outDf
                        datList[[currentSheet]][['dynamicProperties']][['measures']] <<- intersect(datList[[currentSheet]][['dynamicProperties']][['measures']],
                                                                                                   getDefaultMeasures(outDf))
                        ## add new fields, delete outdated fields, leave common fields alone since they might have user customizations
                        newFields <- getDefaultFieldsList(outDf)
                        oldList <- names(datList[[currentSheet]][['dynamicProperties']][['fieldsList']])
                        newList <- names(newFields)
                        for(n in setdiff(newList, oldList)){
                          datList[[currentSheet]][['dynamicProperties']][['fieldsList']][[n]] <<- newFields[[n]]
                        }
                        for(n in setdiff(oldList, newList)){
                          datList[[currentSheet]][['dynamicProperties']][['fieldsList']][[n]] <<- NULL
                        }
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
                #                 if(currentLayer=='Plot'){
                #                   gg <- ggplot(data=datLayer, aess) +
                #                     do.call(paste('geom',geom,sep='_'),
                #                             c(aes.set.args, list(stat=stat, fun.y=fun.y, position=position)))
                #                 } else {
                #                   if(!is.null(gg)){
                #                     gg <- gg + do.call(paste('geom',geom,sep='_'),
                #                                        c(aes.set.args, list(mapping=aess, data=datLayer, stat=stat, fun.y=fun.y, position=position)))
                #                   }
                #                 }
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
              if(!isEmpty(aes.base[['aesX']][['aesField']])){
                i.match <- match(aes.base[['aesX']][['aesField']], fieldNames)
                if(!is.na(i.match)){
                  gg <- gg + xlab(names(fieldNames)[i.match])
                }
              }
              if(!isEmpty(aes.base[['aesY']][['aesField']])){
                i.match <- match(aes.base[['aesY']][['aesField']], fieldNames)
                if(!is.na(i.match)){
                  gg <- gg + ylab(names(fieldNames)[i.match])
                }
              }
            }
            gg
          })
        }

      }))
    }
  }, priority=10)

  sheetListNames <- reactive({
    if(length(sheetList)){
      x <- names(sheetList)
      names(x) <- make.unique(sapply(sheetList, function(y) y[['dynamicProperties']][['name']]), sep='_')
      x
    } else c('')
  })


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
  source('project.r', local=TRUE)
  source('docs.r', local=TRUE)

})


