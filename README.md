![shinyData](http://i.imgur.com/hG7Ltn2.png)
=========

[shinyData](https://github.com/yindeng/shinyData) is an easy to use tool for interactive data analysis, visualization and presentation. It leverages the power of R and its vast collection of packages to allow users to efficiently perform common data tasks, such as slicing and dicing, aggregation, visualization and more (usually referred to as "business intelligence"). Almost no knowledge of R programming is required to use shinyData.

**Current stable version**: v0.1.1

**Demo**: https://roose.shinyapps.io/shinyData/

![](http://i.imgur.com/bkmylo0.png?1)

# Installation
To run the web based version of shinyData without installing anything, simply go to https://roose.shinyapps.io/shinyData/. 
To install the package locally, execute the following R code (you can use the same code to get updates as well): 
```
if(!require(devtools)) install.packages("devtools")
devtools::install_github("trestletech/shinyAce@a2268d545e0ea30eb44c9dca517aec1165b06a51")
devtools::install_github("AnalytixWare/ShinySky@15c29bec4e7c5e694625e571656515a8ace7f376")
devtools::install_github("trestletech/shinyTree@522f8a7e28565bec0f634faf5aa1e75da247de44")
devtools::install_github("ebailey78/shinyBS", ref = "shinyBS3")
devtools::install_github("yindeng/shinyData")
```

# Usage
```
shinyData::shinyData()
```
This will open your default browser and run shinyData locally on your computer.

To quickly get a flavor of what shinyData can do, simply open one of the sample projects in the dropdown box on the "Project" page.

# Feature Overview

## Data extraction and manipulation (Tab "Data")
shinyData supports loading data from a text file. It can auto-detect the presence of header row and common text delimiters (such as comma or tab), as well as skipping banners at the beginning of the file if any, thus requiring minimal input from the user.

The other option of loading is by writing some R code. This gives the user a tremendous amount of flexibility. For example, by using the RODBC library, one can submit a SQL query to a database using an ODBC connection and get results back as a R data frame, ready to be processed by shinyData. The R code can also include names (when quoted in backticks) of other data sources in the same project, which will be evaluated to the corresponding [`data.table`](https://rawgit.com/wiki/Rdatatable/data.table/vignettes/datatable-intro-vignette.html) objects. This makes it easy to pre-process (like adding a derived column) a data source or join two data sources together before creating the desired visualizations. Examples of these customizations will be added to the sample projects over time.

>To load data from an Excel file, refer to this [blog post](http://www.milanor.net/blog/?p=779) for a good comparison of a variety of different approaches available.

>We are working on adding more GUI support for loading data.

After data is loaded, user can preview the data, customize the data source name and field names, and specify which fields should be considered measures. The implication of setting a field as measure is that it will allow for numerical aggregation on the field.

## Data aggregation (Tab "Visualize")
It's often useful to aggregate numerical data (or measures). For example, one might be interested in the average store sales per region. shinyData integrates common data aggregations with visualization, so you can see the results quickly. When you map a visual element (like X, Y, Color, etc) to a field in the selected data source, you have the option to aggregate the field with a function selected from a dropdown list (or type in any R function that takes a vector and returns a single value). The aggregation is done conditional on all the fields that are mapped to in the current layer but not being aggregated, as well as any fields specified in facet columns or rows. Additionally, the data aggregation in each plot layer is independent of each other, so it is possible to have different levels of granularity in the same plot. Another special type of aggregation is sometimes also applied to a layer when "Stat" under Tab "Type" is not "Identity". This is useful for creating statistical charts like box plots, for which you need to aggregate the data to get the quartiles. This type of statistical aggregation is done after the previously mentioned aggregations are done.

>Tip: if you need to create a type of aggregation that's not supported by either of the techniques mentioned above, you can create an intermediate data source with R code under Tab "Data" (see above), do whatever aggregation there, and use that data source for your visualization instead.

When there is any aggregation done in the base "Plot" layer, the aggregated data table is automatically added to the list of data sources, and it will be kept in sync with the changes made to the "Plot" layer. This is useful when a secondary aggregation is desired.

## Visualization (Tab "Visualize")
The following chart types are supported: Text (for adding data labels), bar chart, line chart, area chart, scatter plot, path plot, polygon plot, box plot, density plot, and smoother (or trend line with confidence bands). Visual elements can either be mapped to a field or set to a fixed value. You can add as many layers to the plot as you want, and they will be plotted on top of each other in the order shown. To make sure a layer is not hidden behind other layers (ie, plotted last), click on "Bring to Top" when that layer is selected.

The appearance of the plot is fully customizable. Customization can be specified at different levels and inherit through a tree-like structure. For example, "axis.title" inherits from "title", which in turn inherits from "text", so fonts set for "text" will automatically apply to "title" and "axis.title", but can be overwritten. More information can be found [here](http://docs.ggplot2.org/current/theme.html).

>If you are familiar with the R library [ggplot2](http://docs.ggplot2.org/current/index.html), you should recognize the semantics right away since the back end of shinyData visualization is exactly ggplot2.

## Presentation (Tab "Presentation")
Thanks to the simplicity and flexibility of [R Markdown](http://rmarkdown.rstudio.com/), user can easily combine the plots to create beautiful reports and presentations. And if you are a R programmer, you can add arbitrary R scripts to include analysis results that are not supported by the shinyData UI. Again see the sample projects for examples of creating presentations.

## Project management (Tab "Project")
By saving the project to a file, user can pick up where he left off. User can also merge two projects together by selecting "Merge with existing work" when loading a project file.

>Due to the web-based nature of shinyData, currently you cannot save changes to an existing project file. Instead you need to download the project as a new file. We are working on overcoming this inconvenience when shinyData is run locally, so it essentially behaves more like a desktop application.



# Links
shinyData Blog: https://shinydata.wordpress.com/

ggplot2 Online Reference: http://docs.ggplot2.org/current/index.html

# To Potential Contributors
Please make your changes to the "develop" branch since we intend to keep the "master" branch only for stable releases. Feel free to submit your pull requests!

