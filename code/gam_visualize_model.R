


# From https://github.com/bart-larsen/GAMM-Tutorial/blob/master/GAMM_tutorial.Rmd#L291-L544
### function to extract derivative, confidence interval, significance, and plot ###
### This works for single smooth terms and factor-smooth interactions. Does not work for bivariate smooths.
### This part is still under development.
### If you want to plot derivatives for a by-variable factor model, and you want all plots to have the same scaling, see the note below. Right now you will have to manually set the max value (sorry)

get_derivs_and_plot <- function(modobj, smooth_var, low_color=NULL, hi_color=NULL){
  this_font_size = font_size*1.25
  if (is.null(low_color)){low_color = "white"}
  if (is.null(hi_color)){hi_color = "grey20"}
  derv<-derivatives(modobj, select = "s(age)") # use "select = s(smooth_term"), "term" parameter is depracated 
  derv<- derv %>%
    mutate(sig = !(0 > .lower_ci & 0 < .upper_ci))
  derv$sig_deriv = derv$.derivative*derv$sig # only plot deriv on the color bar if it is significant
  # cat(sprintf("\nSig change: %1.2f - %1.2f\n",min(derv$data[derv$sig==T]),max(derv$data[derv$sig==T])))
  d1<- ggplot(data=derv) + geom_tile(aes(x = age, y = 0, fill = sig_deriv, height = 5))
  
  # Set the gradient colors
  if (min(derv$.derivative) > 0) {
    d1 <- d1 + scale_fill_gradient(low = low_color, high = hi_color, limits = c(0, max(derv$.derivative)))
    # If you want all plots to have the same scaling, this code can be used instead-- This is desirable if you have a factor-smooth model.
    ## max_val = .5
    ## scale_fill_gradient(low = low_color,high = hi_color,limits = c(0,max_val),oob = squish)
  } else if (min(derv$.derivative) < 0 & max(derv$.derivative) < 0) {
    d1 <- d1 + scale_fill_gradient(low = hi_color, high = low_color,limits = c(min(derv$.derivative),0))
  }else {
    d1 <- d1 +
      scale_fill_gradient2(low = "steelblue", midpoint = 0, mid = "white",
                           high = "firebrick",limits = c(min(derv$.derivative),max(derv$.derivative)))
  }
  
  d1 <- d1 + 
    labs(x = smooth_var,fill = sprintf("\u0394%s",smooth_var)) + 
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = this_font_size),
          axis.line = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size=this_font_size),
          legend.text = element_text(size = this_font_size),
          axis.title = element_text(size = this_font_size),
          legend.key.width = unit(1,"cm"),
          legend.position = "off",
          plot.margin = unit(c(0, 0, 0, 0), "cm"))+
    guides(fill = guide_colorbar(reverse = F,direction = "horizontal",title.position = "top")) +
    geom_rect(aes(ymin=0, ymax=5, xmin=min(age), xmax=max(age)),color="black",fill="white",alpha = 0)
  return(d1)
}



# code from: https://github.com/bart-larsen/GAMM-Tutorial/blob/master/GAMM_tutorial.Rmd#L291-L544
# adapted by audrey
visualize_model <- function(modobj,smooth_var, int_var, group_var, plabels = NULL,check_diagnostics = F, derivative_plot = F, custom_color, print_on=TRUE){
  this_font_size = font_size*1.25
  if (any(class(modobj)=="gam")) {
    model <- modobj
  } else if (class(modobj$gam)=="gam") {
    model <- modobj$gam
  } else {
    stop("Can't find a gam object to plot")
  }
  s<-summary(model)
  
  ## Generate custom line plot
  np <- 10000 #number of predicted values
  df = model$model
  
  theseVars <- attr(model$terms,"term.labels")
  varClasses <- attr(model$terms,"dataClasses")
  thisResp <- as.character(model$terms[[2]])
  
  
  # No interaction variable, just produce a single line plot
  thisPred <- data.frame(init = rep(0,np))
  
  for (v in c(1:length(theseVars))) {
    thisVar <- theseVars[[v]]
    thisClass <- varClasses[thisVar]
    if (thisVar == smooth_var) {
      thisPred[,smooth_var] = seq(min(df[,smooth_var],na.rm = T),max(df[,smooth_var],na.rm = T), length.out = np)
    } else {
      switch (thisClass,
              "numeric" = {thisPred[,thisVar] = median(df[,thisVar])},
              "factor" = {thisPred[,thisVar] = levels(df[,thisVar])[[1]]},
              "ordered" = {thisPred[,thisVar] = levels(df[,thisVar])[[1]]}
      )
    }
  }
  pred <- thisPred %>% select(-init)
  p<-data.frame(predict(model,pred,se.fit = T))
  pred <- cbind(pred,p)
  pred$selo <- pred$fit - 2*pred$se.fit
  pred$sehi <- pred$fit + 2*pred$se.fit
  pred[,group_var] = NA
  pred[,thisResp] = 1
  
  p1 <- ggplot(data = df, aes_string(x = smooth_var,y = thisResp)) +
    geom_point(alpha = .3,stroke = 0, size = point_size, fill = custom_color, colour = custom_color) +
    geom_ribbon(data = pred,aes_string(x = smooth_var , ymin = "selo",ymax = "sehi"),alpha = .5, linetype = 0, fill=custom_color) + 
    geom_line(data = pred,aes_string(x = smooth_var, y = "fit"),size = line_size, color=custom_color) +
    labs(title = plabels)
  
  
  if (derivative_plot == T) {
    # We will add a bar that shows where the derivative is significant.
    # First make some adjustments to the line plot.
    p1<- p1+theme(text = element_text(size=this_font_size),
                  axis.text = element_text(size = this_font_size),
                  axis.title.y = element_text(size = this_font_size),
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  legend.text = element_text(size = this_font_size),
                  legend.title = element_text(size = this_font_size),
                  axis.title = element_text(size = this_font_size),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_rect(fill = "transparent",colour = NA),
                  plot.background = element_rect(fill = "transparent",colour = NA),
                  plot.margin = unit(c(.2, .2, 0, .2), "cm")) #Top, left,Bottom, right
    scatter <- list(p1)
    
    # Now add the plots using the derivative plotting function
    if (any(grepl(x = row.names(s$s.table),pattern =  ":") & grepl(x=row.names(s$s.table),pattern = int_var))) {
      # Factor levels separately if there is an interaction in the model.
      f<-formula(model) # current formula
      fterms <- terms(f)
      fac <- attr(fterms, "factors")
      idx <- which(as.logical(colSums(fac[grep(x=row.names(fac),pattern = int_var),])))
      new_terms <- drop.terms(fterms, dropx = idx, keep.response = TRUE)
      new_formula <- formula(new_terms) # Formula without any interaction terms in the model.
      
      #add derivative gradients for each level of the factor
      num_levels <- length(levels(df[,int_var]))
      level_colors <- suppressWarnings(RColorBrewer::brewer.pal(num_levels,"Set1")) #use the same palette as the line plot
      plotlist = vector(mode = "list",length = num_levels+1) # we will be building a list of plots
      plotlist[1] = scatter # first the scatter plot
      
      for (fcount in 1:num_levels) {
        this_level <- levels(df[,int_var])[fcount]
        df$subset <- df[,int_var] == this_level
        df$group_var <- df[,group_var]
        this_mod <- gam(formula = new_formula,data = df,subset = subset,random=list(group_var=~1))
        # this_d <- get_derivs_and_plot(modobj = this_mod,smooth_var = smooth_var,low_color = "white",hi_color = level_colors[fcount])
        this_d <- get_derivs_and_plot(modobj = this_mod,smooth_var = smooth_var,low_color = "white",hi_color = level_colors[fcount])
        
        if (fcount != num_levels & fcount != 1){
          # get rid of redundant junk
          this_d$theme$axis.title = element_blank()
          this_d$theme$axis.text.x = element_blank()
          this_d$theme$axis.ticks=element_blank()
          this_d$theme$legend.background=element_blank()
          this_d$theme$legend.box.background = element_blank()
          this_d$theme$legend.key = element_blank()
          this_d$theme$legend.title = element_blank()
          this_d$theme$legend.text = element_blank()
        }
        if (fcount == 1) {
          this_d$theme$axis.title = element_blank()
          this_d$theme$axis.text.x = element_blank()
          this_d$theme$axis.ticks=element_blank()
          this_d$theme$legend.background=element_blank()
          this_d$theme$legend.box.background = element_blank()
          this_d$theme$legend.key = element_blank()
          this_d$theme$legend.text = element_blank()
        }
        if (fcount == num_levels) {
          this_d$theme$legend.background=element_blank()
          this_d$theme$legend.box.background = element_blank()
          this_d$theme$legend.key = element_blank()
          this_d$theme$legend.title = element_blank()
        }
        this_d$labels$fill=NULL
        plotlist[fcount+1] = list(this_d)
      }
      pg<-plot_grid(rel_heights = c(16*num_levels,rep(num_levels,num_levels-1),3*num_levels),plotlist = plotlist,align = "v",axis = "lr",ncol = 1)
      final_plot <- pg
      if(print_on) {
        print(final_plot)
      }
    } else {
      # No need to split
      d1 <- get_derivs_and_plot(modobj = modobj,smooth_var = smooth_var)
      scatter <- list(p1)
      bar <- list(d1)
      allplots <- c(scatter,bar)
      pg<-plot_grid(rel_heights = c(16,3),plotlist = allplots,align = "v",axis = "lr",ncol = 1)
      final_plot <- pg
      if(print_on) {
        print(final_plot)
      }
    }
    
  }    else {
    # No derivative plot
    p1<- p1+theme(text = element_text(size=font_size),
                  axis.text = element_text(size = font_size),
                  legend.text = element_text(size = font_size),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  plot.background = element_blank())
    final_plot <- p1
    if(print_on) {
      print(final_plot)
    }
  }
  
  if (check_diagnostics == T) {
    cp <- check(b,
                a.qq = list(method = "tnorm",
                            a.cipoly = list(fill = "light blue")),
                a.respoi = list(size = 0.5),
                a.hist = list(bins = 10))
    print(cp)
  }
  return(final_plot)
}

plot_zero.centered_smooths <- function(smooth_fits, ylab, thickness) {
  plot <- ggplot(smooth_fits, aes(age, .estimate, group = bundle, color = color, text = bundle)) +
    geom_line(alpha = .6, size = thickness) +  # size=.7
    scale_color_identity() +
    theme(
      axis.line = element_line(color = "black"),
      axis.text = element_text(size = 18, color = "black"),
      panel.background = element_blank(),
      legend.position = "right") + xlab("Age") + ylab(ylab) + ggtitle(deparse(substitute(smooth_fits)))
  plotly_plot <- ggplotly(plot, tooltip = "text") # include hover data, also add text=bundle in geom_line(aes)
  return(plotly_plot)

}