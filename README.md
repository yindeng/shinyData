shinyData (Beta)
=========

[shinyData](https://github.com/yindeng/shinyData) is intended to be an easy to use tool for interactive data analysis, visualization and presentation. It leverages the power of R and its vast collection of packages to allow users to efficiently perform common data tasks, such as slicing and dicing, aggregation, visualization and more (usually referred to as "business intelligence"). Almost no knowledge of R programming is required to use shinyData.

Note: shinyData is currently under active development. Interested users are encouraged to try it out, but should not use it for production purposes.

# Installation
To run the web based version of shinyData without installing anything, simply go to https://roose.shinyapps.io/shinyData/. 
To install the package locally, execute the following R code (you can use the same code to get updates as well): 
```
if(!require(devtools)) install.packages("devtools")
devtools::install_github("trestletech/shinyAce@a2268d545e0ea30eb44c9dca517aec1165b06a51")
devtools::install_github("AnalytixWare/ShinySky@15c29bec4e7c5e694625e571656515a8ace7f376")
devtools::install_github("trestletech/shinyTree@522f8a7e28565bec0f634faf5aa1e75da247de44")
devtools::install_github("yindeng/shinyData")
```

# Usage
```
shinyData::shinyData()
```
This will open your default browser and run shinyData locally on your computer.

To quickly get a flavor of what shinyData can do, simply open one of the sample projects in the dropdown box on the "Project" page.

# Feature Overview

## Data extraction and basic manipulation
Currently you can only load data from a text file. However, shinyData can auto-detect the presence of header row and common text delimiters (such as comma or tab), as well as skipping banners at the beginning of the file if any, thus requiring minimal input from the user.

After data is loaded, user can preview the data, customize the data source name and field names, and specify which fields should be considered measures. The implication of setting a field as measure is that it will allow for numerical aggregation on the field.

## Data aggregation
The data aggregation in each plot layer is independent of each other, so it is possible to have different levels of granularity in the same plot. When you map a visual element (like X, Y, Color, etc) to a field in the data, you have the option to aggregate the field with the function selected from a list (or type in any R function that takes a vector and returns a single value). The aggregation is done conditional on all the fields that are mapped to in the current layer but not being aggregated, as well as any fields specified in facet columns or rows.

When there is any aggregation done in the base "Plot" layer, the aggregated data table is automatically added to the list of data sources. This is very useful when a secondary aggregation is desired.

## Visualization
In general, visual elements can either be mapped to a field or set to a fixed value. The appearance of the graph is fully customizable. Customization can be specified at different levels and inherit through a tree-like structure. For example, "axis.title" inherits from "title", which in turn inherits from "text", so fonts set for "text" will automatically apply to "title" and "axis.title", but can be overwritten.

## Presentation
Thanks to the simplicity and flexibility of [R Markdown](http://rmarkdown.rstudio.com/), user can easily combine the plots to create beautiful reports and presentations. And if you are a R programmer, you can add arbitrary R scripts to include analysis results that are not supported by the shinyData UI. 

## Project management
By saving the project to a file, user can pick up where he left off. User can also merge two projects together by selecting "Merge with existing work" when loading a project file.

