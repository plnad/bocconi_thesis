
##### PLOT CONTOUR OR SURFACE (wrapper over plotly)
# type: a string which is either "contour" or "surface"
# color_scale: a vector of hex color codes (ex: a=c("#006600")) or the name as string of a color scale from the package RColorBrewer
# labels and title: must be strings
plot_3d <- function(data,plot_type,color_scale, x_label, y_label, z_label, title, cluster_color="#006600", levels, bar_incr, z_min, z_max,cluster_stop=0.1)
{
  # Colors management
  colors_contour = list(list(0,cluster_color),
                        list(cluster_stop,"#ffffff"),
                        list(0.5,"#808080"),
                        list(0.75,"#404040"),
                        list(1,"#000000"))
  # Font parameters
  f <- list(
    family = "Latin Modern Roman",
    size = 11,
    color = "#000000"
  )
  
  x = as.numeric(colnames(data))
  y = as.numeric(rownames(data))
  z = data
  if (missing(z_min) && missing(z_max)){
    plot = plot_ly(z=z, x=x, y=y, type=plot_type, colorscale = colors_contour, ncontours=levels, colorbar=list(title=z_label,dtick=bar_incr))%>% 
    layout(title = title, xaxis = list(title = x_label), 
           yaxis = list(title = y_label), zaxis = list(title = z_label),
           scene = list(
             xaxis = list(title = x_label), 
             yaxis = list(title = y_label), 
             zaxis = list(title = z_label)),
             font = f)
  }else{  plot = plot_ly(z=z, x=x, y=y, type=plot_type, colorscale = colors_contour, ncontours=levels, colorbar=list(title=z_label,dtick=bar_incr), zmin=z_min, zmax=z_max)%>% 
    layout(title = title, xaxis = list(title = x_label), 
           yaxis = list(title = y_label), zaxis = list(title = z_label),
           scene = list(
             xaxis = list(title = x_label), 
             yaxis = list(title = y_label), 
             zaxis = list(title = z_label)),
             font = f)}
  return(plot)
}
