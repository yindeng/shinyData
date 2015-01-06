shinyData (Beta)
=========

shinyData is intended to be an easy to use tool for interactive data analysis, visualization and presentation.
  It leverages the power of R and its vast collection of packages to allow users to efficiently perform common
  data tasks, such as slicing and dicing, aggregation, pattern recognition, visualization and more.
  Almost no knowledge of R programming is required to use shinyData.
  
  Note: shinyData is currently under active development. Interested users are encouraged to try it out, but should not 
  use it for production purposes.

# Installation
To run the web based version of shinyData without installing anything, simply go to https://roose.shinyapps.io/shinyData/. 
To install the package locally, execute the following R code (you can use the same code to get updates as well): 
```
if(!require(devtools)) install.packages("devtools")
devtools::install_github("yindeng/shinyData")
```

# Usage
```
shinyData::shinyData()
```
This will open your default browser and run shinyData locally on your computer.

To quickly get a flavor of what shinyData can do, simply open one of the sample projects in the dropdown box on the "Project" page.

