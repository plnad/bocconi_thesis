
# Load the data required to plot. 
# This data has already been computed and it took quite a long time.
# Here, we just plot based on this data.
# It is available in the Output subfolder of the main folder.
load(file = file.path(path2output, "optimal_m.rda")) # will load under the name results in data objects

plot_lines_m <- function(row_of_output, type="none", levels=50, cluster_color="#0033cc", bar_incr=0.1, cluster_stop=0.075)
{
# Nice colors to use: Blue: #0033cc, Red #b30000, Green: #006600
  
# Access one vector of errors like this example: results[[1,1]][3,30]
# The data is organized as follows: results[[k's,n's]][vectors of errors, iteration]
# We just need the 1st row of every element of the results matrix (row 2 and row 3 were used to compute row 1)
  
# Average over:
iterations = 30
f <- function(j)
{
curves_row = lapply(1:dim(results)[2], function(i) Reduce("+",results[[j,i]][1,])/30)
return(curves_row)
}

# RESULTS CONTAINS 8 ROWS
curves_all_both = lapply(1:8, function(j) f(j)) # You can access the elements like this: curves_all[[1]][[1]] will give you the averaged curve of the element (1,1) of results

# Standardize
n_s = as.numeric(rownames(results))
g <- function(j)
{
curves_std_row = lapply(1:dim(results)[2], function(i) rbind( curves_all_both[[j]][[i]], as.numeric(names( curves_all_both[[j]][[i]] ))/n_s[j])   )
return(curves_std_row)  
}
curves_std_both = lapply(1:8, function(j) g(j))

# Lines chart
n = row_of_output
k_s = colnames(results)

# repackage the data into a dataframe
data = curves_std_both[[n]][[1]]
data = t(data)
data = data.frame(data[,2], data[,1])
names(data)[1] = "% of n"
names(data)[2] = k_s[1]
for (i in 2:8)
{
  data_temp = curves_std_both[[n]][[i]]
  data_temp = t(data_temp)
  data_temp = data.frame(data_temp[,1])
  names(data_temp)[1] = k_s[i]
  data = cbind(data, data_temp)
}

to_plot = data

# Font parameters
f <- list(
  family = "Latin Modern Roman",
  size = 11,
  color = "#000000"
)

# Colors management
# Reds to Greys
colors = c("#990000","#cc0000","#ff3333","#ff8080","#b3b3b3","#737373","#404040","#000000")
# Greys
colors_contour = list(list(0,cluster_color),
                      list(cluster_stop/2,"#ffffff"),
                      list(0.5,"#808080"),
                      list(0.75,"#404040"),
                      list(1,"#000000"))

# Repackaging for the contour plot
rownames(data)=data[,1]
data=data[,-1]
data_contour=data.matrix(data)

if(type=="lines")
{
  # Lines plot (surface's slices)
  
  x=to_plot$`% of n`
  my_plot = plot_ly(x=x, y = data[,1],name = paste("k =",k_s[1]), marker = list(color =colors[1]))%>%
    add_trace(x=x,y = data[,2], name = paste("k =",k_s[2]), marker = list(color =colors[2]))%>%
    add_trace(x=x,y = data[,3], name = paste("k =",k_s[3]), marker = list(color =colors[3]))%>%
    add_trace(x=x,y = data[,4], name = paste("k =",k_s[4]), marker = list(color =colors[4]))%>%
    add_trace(x=x,y = data[,5], name = paste("k =",k_s[5]), marker = list(color =colors[5]))%>%
    add_trace(x=x,y = data[,6], name = paste("k =",k_s[6]), marker = list(color =colors[6]))%>%
    add_trace(x=x,y = data[,7], name = paste("k =",k_s[7]), marker = list(color =colors[7]))%>%
    add_trace(x=x,y = data[,8], name = paste("k =",k_s[8]), marker = list(color =colors[8]))%>%
    layout(xaxis = list(title="m (as a % of n)"), yaxis = list(title="Errors"), title = paste("Errors when varying m with n =",rownames(results)[n]), font = f)
  return(my_plot)
}else if (type=="contour")
{
  # Contour plot
  
  title = paste("Errors when varying m with n =",rownames(results)[n])
  x_label="Number of stocks"
  y_label="m (as a % of n)"
  z_label="l1-norm"
  temp = unlist(curves_std_both)
  z_min = (min(temp)<0)*- max(abs(min(temp)), abs(max(temp)))
  z_max = (max(temp)>0)* max(abs(min(temp)), abs(max(temp)))
  my_plot = plot_ly(x=colnames(data),y=rownames(data),z=data_contour,type="contour", ncontours=levels,
                    zmin = z_min, zmax = z_max, colorbar=list(title=z_label,dtick=bar_incr), colorscale = colors_contour)%>%
    layout(title = title, xaxis = list(title = x_label), 
                             yaxis = list(title = y_label), zaxis = list(title = z_label),
                             scene = list(
                               xaxis = list(title = x_label), 
                               yaxis = list(title = y_label), 
                               zaxis = list(title = z_label)),
                             font = f)
  return(my_plot)
}else{print("Invalid type. Choose between 'lines' or 'contour'.")}
}


# Example
a=plot_lines_m(1,"lines")
a
b=plot_lines_m(6,"contour")
b

