---
title: "Front-end for ggplot2"
author: "Yindeng Jiang"
date: "Wednesday, January 07, 2015"
---

`ggplot2` is a very powerful R package for data visualization, but mastering it is like learning another language in addition to R. Wouldn't it be nice if there's a simple GUI to `ggplot2`? That's one major motivation behind the R package [`shinyData`](https://github.com/yindeng/shinyData).

Leveraging the power of [`shiny`](http://shiny.rstudio.com/) (which is another R package), majority of the functionalities offered by `ggplot2` can be captured by a web-based, highly interactive GUI. Below I will walk through a simple example of creating some basic charts with `shinyData`.

Since `shiny` is a web-based framework, `shinyData` can be run locally just as other R packages, but it can also be deployed over the web. Thanks to http://www.shinyapps.io/, `shinyData` is available to anyone with a web browser by simply clicking [here](https://roose.shinyapps.io/shinyData/).

![](../images//sDproject.png)

Once you're on the homepage of `shinyData` either by following the link above or by installing `shinyData` as a package (see the instructions at https://github.com/yindeng/shinyData), a sample project named **mtcars.sData** should be visible near the top right corner. Click "Open" to load the project, which will take you to Tab "Visualize" displaying a boxplot (together with the raw data dots) of the `mtcars` data set. 

![](../images//sDboxplot.png)

The second drop-down box on the left shows there are two overlays (or layers in `ggplot2`) to this chart. The controls below provide specifications for the selected layer. For example, when the "Overlay" layer is selected, the "Mark Type" (corresponding to `geom` in `ggplot2`) shows "Boxplot" and "Stat" also shows "Boxplot". Just to experiment with the interactivity, if you change the "Mark Type" from "Boxplot" to "Bar", you should see the chart immediately updated to something not quite useful. This is because "Stat" automatically defaulted to "Identity", which makes the bars cover all the dots. To make the new chart a little more useful, you can change "Stat" to "Count" (corresponding to `bin` in `ggplot2`), so now the bars will represent the count of data points in each category. 

Let's change "Mark Type" back to "Boxplot" before we move on (note how "Stat" also defaulted back to "Boxplot"). Now switch to "Plot" layer. "Mark Type" should show "Point" and "Stat" shows "Identity", which tells you this is the layer that generated the data dots. If you notice the data dots are being covered by the boxes of the boxplots, you can uncover them by simply clicking on the button "Bring to Top", which changes the order of the layers and plots the current layer last.

Lastly, the "Mapping" tab lets you specify how data are plotted in each layer. One important feature to point out is, since the "Plot" layer is considered the foundation of the chart, other layers automatically inherit its data mappings if not overwritten explicitly. In this example, you can see "X" and "Y" are not mapped to anything in layer "Overlay" since they are the same as the "Plot" layer. 

This should serve as a quick start to using `shinyData` as a front-end to `ggplot2` to easily make charts. To load your own data, go to the "Data" tab. I will cover the specifics of that and more about `shinyData` in follow-up posts.
