# TODO: Implement option to plot as grid of cells. 
# TODO: Option to chose which predictor variable(s) to use for models with  
#       multiple predictor variables (specified bey either index, or name) , 
#       along with each predictor variable as separate cell in a grid plot. 
#       Multiple models are overlayed in each cell for the same predictor.
# TODO: FIx axis labels to give useful labels. Maybe add a main title too. 
# TODO: implement plotting of residuals like in ggplot_residuals()


# =====================================
# RESET PAR TO FACTORY DEFAULTS
# =====================================
# resetPar <- function() {
#     dev.new()
#     op <- par(no.readonly = TRUE)
#     dev.off()
#     op
# }
# =====================================


#===============================================================================
#                                                                    PLOT.MODELS
#===============================================================================
#' @title plot_models
#' @description Plots multiple models fitted to the same data. By default it 
#'          overlaps each model over the others. But you can specify to have 
#'          each in a separate cell in a grid plot. 
#' @details By default the plot of each model will be the same color. But you 
#'          can specify the colour of each individualy. 
#'          
#'          Alternatively, a feature that will be added is to have all plots 
#'          the same color except for a master model (needs to be placed as the 
#'          first model in the list of models) 
#'          , or to have a master and a set of slaves (where
#'          one is brightly colored, and the others are grey)
#'          
#'          You can make the models plot with a shaded Confidence Interval. By 
#'          default, the confidence interval is 0.95, but you can set it to 
#'          something else.   
#' @note Make sure that when you are creating models, that you avoid using the 
#'       "$" operator. 
#'       
#'       BAD:  lm(myData$y ~ myData$x)
#'       
#'       GOOD: lm(y ~ x, data=myData)
#'       
#'       Otherwise you will get error messages like  "'x' and 'y' lengths differ" 
#'       when you try running the predict() function which is used by 
#'       plot_models()
#'       
#' @param models a list of model objects
#' @param scatter (logical) 
#' 
#'      TRUE = plots scatterplot of the datapoints used in the training set. 
#'      
#'      FALSE =  only plots the models without any of the original data points.
#' @param scatter_color the color for the scatter plot of the original data. 
#' 
#'      See here for a chart of possible colors: 
#'      \url{http://research.stowers-institute.org/efg/R/Color/Chart/}
#' @param scatter_alpha (numeric) The alpha of the scatter plots
#' @param scatter_size (numeric) Size of the scatter points (Default is 2)
#' @param model_color either a single color for all models to be. Or a vector 
#'      of colors to cycle through. 
#' @param model_alpha (numeric) The alpha of the model lines
#' @param modelWidth (numeric) width of the model lines. You can wither 
#'      pass a single value in which case all models will be the same width, 
#'      or you can specify a vector of numerics to  specify the width of each 
#'      individual model. 
#' @param ci (logical) Plot the Confidence Interval? 
#' 
#'      (Default is TRUE) 
#' @param ci_level (numeric) confidence level to use. Must be a value between 
#'      0 and 1.
#' 
#'      (Default is 0.95)
#' @param ci_color Color of the Confidence Interval Shaded Region
#'      
#'      (Default is to use same color as model_color) 
#' @param ci_alpha (numeric) Alpha level of Confidence Interval Shaded Region.
#' 
#'      (Default is model_alpha/4)
#' @param draw_grid (logical) should it include a grid? 
#' 
#'      (Default is TRUE)
#' @param ... other arguments to be passed on to the parent plot() function.
#' 
#'          use ?plot() to see what other arguments can be passed on.  
#' 
#' @author Ronny Restrepo
#' @examples
#' library(ElemStatLearn)
#' data(ozone,package="ElemStatLearn")
#' 
#' # create models
#' model1 = loess(temperature ~ ozone, data=ozone,span=0.15)
#' model2 = lm(temperature ~ ozone, data=ozone)
#' model3 = loess(temperature ~ ozone, data=ozone, span=0.2)
#' model4 = loess(temperature ~ ozone, data=ozone, span=1)
#' model5 = loess(temperature ~ ozone, data=ozone, span=20)
#' models = list(model1, model2, model3, model4, model5)
#' 
#' plot_models(models)
#' plot_models(models, xlab="Ozone", ylab="Temperature", 
#'             main="Models of Temperature as function of Ozone")
#' 
#' # Use different colors for each model
#' modcols = c("darkorchid2","chartreuse","brown2","aquamarine2","darkorange") 
#'
#' # Use different line widths for each model
#' plot_models(models, model_color=modcols, modelWidth=c(8,2,4,8,3))
#' 
#' # Plot models with Confidence intervals
#' models = list(model1, model2)
#' modcols = c("darkorchid2", "darkorange")
#' plot_models(models, model_color=modcols, ci=T)
#' 
#' 
#' @seealso \code{\link{plot_residuals}} \code{\link{plot.cols}} \code{\link{plot}}
#' @keywords plot plot_models plotting modelling models model 
#'           plot.convenience
#' @import scales
#' @export plot_models 
#===============================================================================
plot_models <- function(models, scatter=TRUE, 
                        scatter_color="blue", scatter_alpha=0.2, scatter_size=2,
                        model_color="darkorange", model_alpha=1, modelWidth=2, 
                        ci=FALSE, ci_level=0.95, ci_color=model_color, 
                        ci_alpha = model_alpha/4, 
                        draw_grid=TRUE, ...){
    
    #---------------------------------------------------------------------------
    #                                                            Sanity Checking 
    #---------------------------------------------------------------------------
    # make sure that models is a list of models
    if (class(models) != "list"){
        models = list(models)
    }
    
    # Make sure there are enough color and width elements for each model
    model_color = rep_len(model_color, length(models))
    ci_color = rep_len(ci_color, length(models))
    modelWidth = rep_len(modelWidth, length(models))
    
    #---------------------------------------------------------------------------
    #                                                                Preparation
    #---------------------------------------------------------------------------
    # Get x and y points of original data
    xy = model.frame(models[[1]])
    
    # Prepare x axis values for Model
    model.x = data.frame(seq(min(xy[,2]), max(xy[,2]), length.out=100))
    names(model.x) = names(xy)[2]
    
    #---------------------------------------------------------------------------
    #                      Prepare the Cavas, and plot scatter plot if requested
    #---------------------------------------------------------------------------
    scatterType = ifelse(scatter, "p", "n")
    plot(xy[,2],xy[,1], col=scales::alpha(scatter_color, scatter_alpha), pch=19, 
         cex=scatter_size, type=scatterType, ...)
    
    #---------------------------------------------------------------------------
    #                                      Draw grid lines if they are requested
    #---------------------------------------------------------------------------
    if (draw_grid){
        grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
             lwd = 1, equilogs = TRUE)
    }
    
    #---------------------------------------------------------------------------
    #                                                           Plot the models
    #---------------------------------------------------------------------------
    for (i in 1:length(models)){
        # TODO: check that the se=TRUE option is available on all model types
        model.y = predict(models[[i]], newdata=model.x, se=TRUE)
        lines(as.vector(model.x[,1]), 
              as.vector(model.y$fit), 
              type="l", 
              col=scales::alpha(model_color[i], model_alpha), 
              lwd=modelWidth[i])
        
        #-----------------------------------------------------------------------
        #                               Plot Confidence Intervals for each model
        #-----------------------------------------------------------------------
        if (ci){
            # Upper and lower thresholds
            ci_threshold = (ci_level / 2) + 0.5
            lower = model.y$fit - qt(ci_threshold, model.y$df)*model.y$se.fit
            upper = model.y$fit + qt(ci_threshold, model.y$df)*model.y$se.fit
            
            # create the x and y coordinates to create a polygon
            ci.x = c(as.vector(model.x[,1]), rev(as.vector(model.x[,1])))
            ci.y = c(upper, rev(lower))
            
            # Shade the points as a polygon
            polygon(ci.x, ci.y, col=scales::alpha(ci_color[i], ci_alpha), 
                    border=0, lty=0)
        }
    }
}

