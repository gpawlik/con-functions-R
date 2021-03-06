# TODO: implement a way to use line plots, for example for time series plots, 
#       so you can overlap timeline plots. 
    
#===============================================================================
#                                                    PLOT_BY_CAT
#===============================================================================
#' @title plot_by_cat
#' @description Draws a visual separation for how y relates to x when grouped  
#'              by some category variable. You can separate by color coding 
#'              into the same plot (overlaping) or by separating into several 
#'              subplots. 
#' @details You can specify if the subplots all share the same x and y limits 
#'          (default), or if you want each subplot to automatically scale to 
#'          fit its sub-data with the least amount of shite space. 
#'          
#' @param x (vector of numerics) The independent variable
#' @param y (vector of numerics) The dependent variable
#' @param cat (factors) The factors to do grouping with
#' @param col Colors of the scatter points
#' @param overlap (logical) Overlap the subplots over each other to create a 
#'        single plot that is color coded for each category? 
#'        
#'        If FALSE, then each category is plotted as a separate subplot.  
#' @param legend (vector of characters) If you selected overlap=TRUE, then a 
#'        color coded legend will be created. You can optionally specify the 
#'        strings to be used in the legend here. 
#'        
#'        If you do not provide any value here, it will default to using the 
#'        levels in cat. 
#' 
#' @param hlims (logical) Homogenous Limits?
#' 
#'      TRUE = each subplot is set up with the exact same x and y limits, to 
#'             make it easy to compare each one
#'      
#'      FALSE = each subplot automatically scales its limits to fill up the 
#'              space from its own data points  
#'      
#'      (DEFAULT is TRUE)
#'
#' @author Ronny Restrepo
#' @examples
#' data(mtcars)
#' col = c("magenta", "blue", "green")
#' 
#' # Separate subplot for each category (number of cylinders)
#' plot_by_cat(mtcars$disp, mtcars$mpg, cat=mtcars$cyl, overlap=F, col=col)
#' 
#' # Scatter points overlapped, but color coded for different cylinders
#' plot_by_cat(mtcars$disp, mtcars$mpg, cat=mtcars$cyl, overlap=T, col=col, 
#'             legend=c("4 Cylinders", "6 Cylinders", "8 Cylinders"))
#' 
#' @seealso plot.cols plot
#' @keywords plot_by_cat category group
#' @export plot_by_cat
#===============================================================================

plot_by_cat <- function(x, y, cat, col="blue", alpha=0.3, overlap=FALSE, 
                        legend=NA, hlims=TRUE){
    cats = levels(as.factor(cat))   # Levels of the categories
    ncats = length(cats)            # Number of categories
    
    #--------------------------------------------------------------------------
    #                                                          Sanity Checking
    #--------------------------------------------------------------------------    
    # If overlapping is requested, make sure each category is a different color
    if (overlap & length(col) == 1){
        col = 1:ncats
    }
    
    # Make sure there are enough color elements for each category
    col = rep_len(col, ncats)
    
    #--------------------------------------------------------------------------
    #                                            Set up Plot Parameter Settings
    #--------------------------------------------------------------------------
    # Take a snapshot of the current global plotting settings
    BU.par = par(c("mfrow", "mar", "mgp", "tck", "oma"))
    
    if (overlap){
        # Parameters to use to allow legend placement to the right of the plot
        par(mar=c(5.1, 4.1, 4.1, 5.1))
    } else {
        # Set New global plotting settings for a grid layout
        set_par_for_n_subplots(ncats)
    }
    #--------------------------------------------------------------------------
    #                                                            Plot the Cells
    #--------------------------------------------------------------------------
    labelCex = 1
    for (i in 1:ncats){
        rows = cat == cats[i]
        if ((hlims & !overlap) | (overlap & i == 1)){
            plot(y ~ x, type="n")
        }
        points(x[rows], y[rows], pch=19, col=scales::alpha(col[i], alpha))
        if (!overlap) {
            mtext(cats[i], side=3, line=0.5, cex = labelCex)
        }
    }
    #--------------------------------------------------------------------------
    #                                                    Add a Legend if Needed
    #--------------------------------------------------------------------------
    if (overlap){
        # Set default legend
        if (is.na(legend[1])){
            legend = cats
        }
        
        # Trim legend to 12 characters
        legend = substr(legend, 1, 12)
        
        # Add legend to the right hand side
        legend("topleft", inset=c(1,0), legend=legend, 
               col=scales::alpha(col, 2*alpha), 
               pch=19, cex=0.7, xpd=TRUE, bty="n")
    }
    
    # Return the global plot parameters to their previous settings
    par(BU.par)
}
