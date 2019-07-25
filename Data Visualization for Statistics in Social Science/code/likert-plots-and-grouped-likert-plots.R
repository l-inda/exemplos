library(sjPlot)
library(sjmisc)
library(snakecase)
library(cowplot)
data(efc)

# find all variables from COPE-Index, which all 
# have a "cop" in their variable name, and then
# plot the items as likert-plot
mydf <- find_var(efc, pattern = "cop", out = "df")
##Package `snakecase` needs to be installed for case-conversion.
plot_likert(mydf)

##The plot is not perfect, because for those values with just a few answers, we have overlapping values.
# However, there are quite some options to tweak the plot. 
#For instance, we can increase the axis-range (grid.range), show cumulative percentage-values only at the 
#ende of the bars (values = "sum.outside") and show the percentage-sign (show.prc.sign = TRUE).
plot_likert(
    mydf,
    grid.range = c(1.2, 1.4),
    expand.grid = FALSE,
    values = "sum.outside",
    show.prc.sign = TRUE
)

## The interesting question is, whether we can reduce the dimensions of this 
#scale and try to extract principle components, in order to group single items 
#into different sub-scales. To do that, we first run a PCA on the data. 
#This can be done, e.g., with sjt.pca() or sjp.pca().

# creates a HTML-table of the results of an PCA.
sjt.pca(mydf)

#As we can see, six items are associated with component one, while three items 
#mainly load on the second component. The indices that indicate which items is 
#associated with which component is returned by the function in the element 
#$factor.index. So we save this in an object that can be used to create a 
#grouped Likert-plot.

groups <- sjt.pca(mydf)$factor.index
plot_likert(mydf, groups = groups, values = "sum.outside")
#Erro: plot_likert_grp: Please install the package "cowplot"

