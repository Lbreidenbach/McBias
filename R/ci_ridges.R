#' Creates a ggridges plot for fast visualization of outputs from varied_runs() or reparse_runs()
#'
#' Each ridgeline plots out the calculated effect size for each method in the output. The set effect size appears as a dashed line through the ridgeline so the user can see how close the calculated
#' estimates are. If the calculated effect size contains the true effect size in it's 95% confidence intervals, it is shaded blue, otherwise it is shaded grey. This allows users to visualize a simulations coverage.
#'
#' @param run a list object as returned by varied_runs() or reparse_runs()
#'
#' @param title The title of the ggridges plot.
#'
#' @param subtitle The subtitle of the ggridges plot
#'
#' @returns A ggridges plot
#' @example examples/varied_methods_example.R
#' @seealso [varied_runs()], [reparse_runs()]
#' @export

ci_ridges = function(run, title =NULL, subtitle=NULL){
  ate_val = as.data.frame(run[[2]])
  drawn_ci = beta_sum(run)

  if(is.null(dim(run[[1]]))==T){
    colnames(ate_val) = "regression"

  }

  all_in_ci = unlist(lapply(c(1:ncol(ate_val)), function(x){
    if(drawn_ci[7,x]==0&drawn_ci[8,x]==0){
      return(TRUE)
    }else{
      return(FALSE)
    }
  } ))

  drawn_ci[drawn_ci==0] =NA
  drawn_ci[drawn_ci==1] =NA

  # drawn_ci$tag = c(1,1,0,0)
  # drawn_ci = drawn_ci[!(duplicated(drawn_ci[1:3]) | duplicated(drawn_ci[1:3], fromLast = TRUE)), ]
  # drawn_ci = drawn_ci[drawn_ci$tag==1,]
  # drawn_ci = drawn_ci[-length(drawn_ci)]
  upper = sapply(drawn_ci[7,], function(x) rep(x, length(ate_val[[1]])))
  lower = sapply(drawn_ci[8,], function(x) rep(x, length(ate_val[[1]])))
  #rep(drawn_ci[,1],length(comp_df[[1]]))


  names= unlist(lapply(colnames(ate_val), function(y) rep(y, nrow(ate_val))))
  value= as.numeric(unlist(ate_val))
  upper = 1- as.numeric(unlist(upper))
  lower = as.numeric(unlist(lower))

  data=dplyr::tibble(names,value, upper, lower)
  data$tag =0
  data[data$value>0,]$tag = 1
  data$names = as.factor(data$names)
  data$tag = as.character(data$tag)

  lines = drawn_ci[1:2, , drop = FALSE]
  nas_df = drawn_ci[7:8, , drop = FALSE]
  #na_dex
  inf_dex = which(is.na(nas_df[1,]))
  neg_inf_dex = which(is.na(nas_df[2,]))
  all_in_dex = which(all_in_ci==TRUE)

  lines[1, inf_dex]= Inf
  lines[2, neg_inf_dex]= -Inf



  factor_df = as.data.frame(unique(as.integer(data$names)))
  factor_df = t(factor_df)
  colnames(factor_df) = unique(as.character(data$names))
  rownames(factor_df) = "i"

  #FIX HERE, col name problem
  lines = rbind(lines, factor_df, all_in_ci)
  # rownames(lines[4, ]) = "all" #perhaps unneseccary??

  #lines = lines[,order(lines[nrow(lines),])]

  drawn_ci = rbind(drawn_ci, factor_df)
  #drawn_ci = drawn_ci[,order(drawn_ci[nrow(drawn_ci),])]

  ###WORKING LINES
  p <- ggplot2::ggplot(data, aes(x=value, y=names)) +
    ggridges::stat_density_ridges(scale = 0.95,
                        quantile_lines = TRUE,
                        quantile_fun = function(x, ...) quantile(x, probs =
                                                                   c(sort(c(mean(data[data$value == x,]$lower))), sort(c(mean(data[data$value == x,]$upper)))), na.rm = TRUE)
    ) +
    ggridges::theme_ridges(center = TRUE) +
    ggplot2::ylab("Anaylsis Performed") +
    ggplot2::xlab("Estimated Beta") +
    ggplot2::ggtitle(label = paste(title),
            subtitle = paste(subtitle)) +
    ggplot2::theme(plot.title = element_text(size = 18, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.text = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 14, face = "bold")) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = expansion(mult = c(0.01, .1))) +
    if(max(data$value) > 10 | min(data$value < -10)){
      ggplot2::xlim(limits[1],limits[2])
    }
  p

  d <- ggplot_build(p)$data[[1]]
  ribbon = function(upper, lower, i, all) {
    if(upper == Inf & lower == -Inf & all == 0){
      return()
    }
    q = ggplot2::geom_ribbon(
      data = transform(subset(d, x <= upper & x >= lower & ymin == i), names = group),
      ggplot2::aes(x, ymin = ymin, ymax = ymax, group = group),
      fill = "lightblue2")
    return(q)
  }
  # i = 3
  # p + ribbon(lines[1,i], lines[2,i], lines[3,i])
  # p + ribbon(-0.1, -0.2, 2)
  q = p + lapply(c(1:ncol(lines)), function(x) ribbon(lines[1,x],lines[2,x],lines[3,x], lines[4,x]))

  # q =p + geom_ribbon(
  #   data = transform(subset(d, x <= 0.0373545924 & x >= -0.0412792948 & ymin == 3), names = group),
  #   aes(x, ymin = ymin, ymax = ymax, group = group),
  #   fill = "lightblue2") +
  #   geom_ribbon(
  #     data = transform(subset(d, x <= 0.0348230349 & x >= -0.0357772047  & ymin == 2), names = group),
  #     aes(x, ymin = ymin, ymax = ymax, group = group),
  #     fill = "lightblue2") +
  #   geom_ribbon(
  #     data = transform(subset(d, ymin == 1), names = group),
  #     aes(x, ymin = ymin, ymax = ymax, group = group),
  #     fill = "lightblue2") +
  #   geom_segment( aes(y=1, yend=length(colnames(ate_val))+1, x=run[[8]][1], xend=run[[8]][1]), color="navy", linetype = "dashed", lwd = 1)

  r = q+ ggridges::stat_density_ridges(scale = 0.95,
                             quantile_lines = TRUE,
                             quantile_fun = function(x, ...) quantile(x, probs =
                                                                        c(sort(c(mean(data[data$value == x,]$lower))), sort(c(mean(data[data$value == x,]$upper)))), na.rm = TRUE),
                             fill = "lightblue2",
                             alpha= 0.01) +
    ggplot2::geom_vline( xintercept = run[[9]][1], color="navy", linetype = "dashed", lwd = 1)




  ###



  # Construct the six grobs - three symbols and three labels
  L1 = grid::rectGrob(height = 0.8, width = 0.8, y=.41,gp = gpar(fill = "lightblue2", col = "black", lwd = 2))
  L2 = grid::rectGrob(height = 0.8, width = 0.8,y=.41, gp = gpar(fill = "grey50",  col = "black", lwd = 2))
  L3 = grid::linesGrob(x = unit(c(0.05, 0.9), "npc"), y = unit(c(.3, .3), "npc"), gp = gpar( col = "navy", lwd = 3.8, lty = "dashed"))
  T1 = grid::textGrob("= Yes", x = 0, just = "left",y=.4,gp = gpar(fontsize=10))
  T2 = grid::textGrob("= No", x = 0, just = "left",y=.4, gp = gpar(fontsize=10))
  T3 = grid::textGrob("Set true effect size =", x = .36,y=.3, just = "left",gp = gpar(fontsize=10, fontface = "bold"))


  # Construct a gtable - 2 columns X 4 rows
  leg = gtable::gtable(width = unit(c(5.4,.8,1,.8,1), "cm"), height = unit(c(1,.7), "cm"))

  #gtable_show_layout(leg)
  # Place the six grob into the table
  leg = gtable::gtable_add_grob(leg, L1, t=1, l=2)
  leg = gtable::gtable_add_grob(leg, L2, t=1, l=4)
  leg = gtable::gtable_add_grob(leg, L3, t=2,l=2, r=5)
  leg = gtable::gtable_add_grob(leg, T1, t=1, l=3)
  leg = gtable::gtable_add_grob(leg, T2, t=1, l=5)
  leg = gtable::gtable_add_grob(leg, T3, t=2,l=1)
  leg = gtable::gtable_add_grob(leg, grid::textGrob(expression(bold("Does effect size estimate have\ntrue effect size in its 95% C.I.?")), vjust = 1.2,gp = gpar(fontsize=10),y=.4 ), t=1,l=1)


  # Give it a title (if needed)


  # Get the ggplot grob for plot1
  g = ggplot2::ggplotGrob(r)
  gtable_show_layout(g)
  # Get the position of the panel,
  # add a column to the right of the panel,
  # put the legend into that column,
  # and then add another spacing column
  pos = g$layout[grepl("panel", g$layout$name), c('t', 'l')]
  g = gtable::gtable_add_rows(g, sum(leg$heights), pos$t +2 )
  g = gtable::gtable_add_grob(g, leg, t = pos$t +3, l=pos$l-1,r = pos$l)


  # Draw it
  grid::grid.newpage()
  grid::grid.draw(g)
  return(grid::grid.draw(g))

}

