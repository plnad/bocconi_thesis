# Clean the workspace
rm(list = ls())

# Path management
path = getwd()
setwd(path)
path2output = file.path(path, "Output")

##### GO HAVE A LOOK IN THIS FILE AND UNCOMMENT THE INSTALLATION OF THE PACKAGES TO HAVE THE REQUIRE PACKAGES FOR THIS CODE
header=source("header.R")

# The only required dataset
load(file = file.path(path, "sp500crsp.rda"))


###########################################################################################################################
color_3_RWB = c("#990000","#ffffff","#000000")
split_cluster = 0.1

option1 = 1
option2 = 0
option3 = 0
k_s=seq(35,420,35)
n_s=seq(70,420,70)

# COMPUTATIONS COMMENTED. TAKES A SIGNIFICANT AMOUNT OF TIME TO RUN
# timepoint = proc.time() # Start the clock
# results_cov_3 = collect_results_cov(n_s, k_s)
# proc.time() - timepoint # Stop the clock
# save(results_cov, file=file.path(path2output, "results_cov_3.rda"))
load(file = file.path(path2output, "results_cov_3.rda"))

##### FIGURE 1 LEFT and FIGURE 5 LEFT
subset_cov = results_cov_3
to_plot = subset_cov
title = "Classical approach"
z_min = 0
z_max = max(subset_cov)
plot_3d(to_plot,"contour","Greys","Number of stocks", 
        "Number of days", "l2-norm", title,"#0033cc", 30, 2, z_min, z_max, split_cluster)


# COMPUTATIONS COMMENTED. TAKES A SIGNIFICANT AMOUNT OF TIME TO RUN
# timepoint = proc.time() # Start the clock
# results_shrink_3 = collect_results_shrink(n_s, k_s)
# proc.time() - timepoint # Stop the clock
# save(results_shrink_3, file=file.path(path2output, "results_shrink_3.rda"))
load(file = file.path(path2output, "results_shrink_3.rda"))

##### FIGURE 1 RIGHT
subset_shrink = results_shrink_3
to_plot = subset_shrink
title = "Shrinkage approach (Wolf & Ledoit)"
plot_3d(to_plot,"contour","Greys","Number of stocks", 
        "Number of days", "l2-norm", title, "#0033cc", 75, 2, z_min, z_max, split_cluster)

##### FIGURE 2
subset_shrink = results_shrink_3
to_plot = subset_shrink
title = "Shrinkage approach (Wolf & Ledoit)"
rangex=c(35,420)
rangey=c(70,420)
plot_3d(to_plot,"contour","Greys","Number of stocks",
        "Number of days", "l2-norm", title, "#0033cc", 30, 0.2,cluster_stop = split_cluster)%>%
  add_trace(x = c(70,420), y = c(70,420), name = "linear",
            marker=list(color="#000000", dash="dot"), line=list(dash="dot",width=1))%>%
  layout(xaxis = list( showgrid = FALSE, range = rangex), yaxis = list( showgrid = FALSE, range=rangey), margin=list( pad = 0))



option1 = 1
option2 = 0
option3 = 0
k_s=seq(75,450,50)
n_s=seq(50,520,65)
b = 30 # number_of_bootstraps
b = max(b,3) # MUST BE >=3, OTHERWISE THE CODE DOESNT WORK
iterations = 30
# COMPUTATIONS COMMENTED. TAKES A SIGNIFICANT AMOUNT OF TIME TO RUN
# timepoint = proc.time() # Start the clock
# results = collect_results_m_s(n_s, k_s, option1,option2,option3, b,iterations)
# proc.time() - timepoint # Stop the clock
# save(results, file=file.path(path2output, "optimal_m.rda"))
load(file = file.path(path2output, "optimal_m.rda"))

##### FIGURE 3
plot_lines_m(1,"lines")
plot_lines_m(8,"lines")

##### FIGURE 4
cluster_color = "#0033cc"
split = split_cluster
plot_lines_m(1,"contour",30,cluster_color, 0.1, cluster_stop=split)
plot_lines_m(8,"contour",30,cluster_color, 0.1, cluster_stop=split)



s = 30
m_lower = 0.2
m_upper = 0.4
granularity = 1/25
# COMPUTATIONS COMMENTED. TAKES A SIGNIFICANT AMOUNT OF TIME TO RUN
# timepoint = proc.time() # Start the clock
# results_generalized_m_8 = collect_results_generalized_m(n_s, k_s, option1,option2,option3,  s,m_lower,m_upper,granularity)
# proc.time() - timepoint # Stop the clock
# save(results_generalized_m_8, file=file.path(path2output, "results_generalized_m_8.rda"))
load(file = file.path(path2output, "results_generalized_m_8.rda"))

##### FIGURE 5 RIGHT
subset_abadir_8 = results_generalized_m_8
to_plot = subset_abadir_8
title = "New approach (Abadir et al.)"
plot_3d(to_plot,"contour","Greys","Number of stocks", 
        "Number of days", "l2-norm", title, "#0033cc", 75, 2, z_min, z_max, split_cluster)

##### FIGURE 6
to_plot = subset_abadir_8
title = "New approach (Abadir et al.)"
rangex=c(35,420)
rangey=c(70,420)
plot_3d(to_plot,"contour","Greys","Number of stocks",
        "Number of days", "l2-norm", title, "#0033cc", 30, 0.2,cluster_stop = split_cluster)%>%
  add_trace(x = c(70,420), y = c(70,420), name = "linear",
            marker=list(color="#000000", dash="dot"), line=list(dash="dot",width=1))%>%
  layout(xaxis = list( showgrid = FALSE, range = rangex), yaxis = list( showgrid = FALSE, range=rangey), margin=list( pad = 0))


##### FIGURE 7
subset_shrink = results_shrink_3
subset_abadir_8 = results_generalized_m_8
# Find the color borders to compare them on the same scale
subset_all = c(subset_shrink,subset_abadir_8)
to_plot = subset_all
z_min = (min(to_plot)<0)*- max(abs(min(to_plot)), abs(max(to_plot)))
z_max = (max(to_plot)>0)* max(abs(min(to_plot)), abs(max(to_plot)))
rangex=c(35,420)
rangey=c(70,420)

# LEFT
to_plot = subset_shrink
title = "Shrinkage approach (Wolf & Ledoit)"
plot_3d(to_plot,"contour","Greys","Number of stocks", 
        "Number of days", "l2-norm", title, "#0033cc", 60, 0.25, z_min, z_max, split_cluster)%>%
  add_trace(x =c(70,420), y =  c(70,420), name = "linear", 
            marker=list(color="#000000", dash="dot"), line=list(dash="dot",width=1))%>%
  layout(xaxis = list( showgrid = FALSE, range = rangex), yaxis = list( showgrid = FALSE, range=rangey), margin=list( pad = 0))

# RIGHT
to_plot = subset_abadir_8
title = "New approach (Abadir et al.)"
plot_3d(to_plot,"contour","Greys","Number of stocks", 
        "Number of days", "l2-norm", title, "#0033cc", 60, 0.25, z_min, z_max, split_cluster)%>%
  add_trace(x =c(70,420), y =  c(70,420), name = "linear", 
            marker=list(color="#000000", dash="dot"), line=list(dash="dot",width=1))%>%
  layout(xaxis = list( showgrid = FALSE, range = rangex), yaxis = list( showgrid = FALSE, range=rangey), margin=list( pad = 0))



##### FIGURE 8
shrink_abadir = (subset_shrink / subset_abadir_8)-1
to_plot = shrink_abadir
title = "% of excess errors: shrinkage approach over new approach"
# Find the color borders to compare them on the same scale
split = abs(min(to_plot))/(abs(min(to_plot))+max(to_plot))
rangex=c(35,420)
rangey=c(70,420)
plot_3d(to_plot,"contour",color_3_RWB,"Number of stocks", "Number of days",
        "%Excess l2-norm", title,"#b30000", 30, 0.25, cluster_stop = split)%>%
  add_trace(x = c(70,420), y =  c(70,420), name = "linear", 
            marker=list(color="#000000", dash="dot"), line=list(dash="dot",width=1))%>%
  layout(xaxis = list( showgrid = FALSE, range = rangex), yaxis = list( showgrid = FALSE, range=rangey), margin=list( pad = 0))

##### FIGURE 9
shrink_abadir = (subset_shrink - subset_abadir_8)
to_plot = shrink_abadir
title = "Excess errors: shrinkage approach - new approach"
# Find the color borders to compare them on the same scale
split = abs(min(to_plot))/(abs(min(to_plot))+max(to_plot))
rangex=c(35,420)
rangey=c(70,420)
plot_3d(to_plot,"contour",color_3_RWB,"Number of stocks", "Number of days",
        "Excess l2-norm", title,"#b30000", 30, 0.25, cluster_stop = split)%>%
  add_trace(x = c(70,420), y =  c(70,420), name = "linear", 
            marker=list(color="#000000", dash="dot"), line=list(dash="dot",width=1))%>%
  layout(xaxis = list( showgrid = FALSE, range = rangex), yaxis = list( showgrid = FALSE, range=rangey), margin=list( pad = 0))



m_lower = 0.3
# COMPUTATIONS COMMENTED. TAKES A SIGNIFICANT AMOUNT OF TIME TO RUN
# timepoint = proc.time() # Start the clock
# results_generalized_m_11 = collect_results_generalized_m(n_s, k_s, option1,option2,option3,  s,m_lower,m_upper,granularity)
# proc.time() - timepoint # Stop the clock
# save(results_generalized_m_11, file=file.path(path2output, "results_generalized_m_11.rda"))
load(file = file.path(path2output, "results_generalized_m_11.rda"))

##### FIGURE 9
rangex=c(35,420)
rangey=c(70,420)

subset_abadir_8 = results_generalized_m_8
subset_abadir_11 = results_generalized_m_11
shrink_abadir_2 = (subset_shrink / subset_abadir_8)-1
shrink_abadir_3 = (subset_shrink / subset_abadir_11)-1
z_min = min(c(shrink_abadir_2,shrink_abadir_3))
z_max = max(c(shrink_abadir_2,shrink_abadir_3))

split = abs(z_min)/(abs(z_min)+z_max)

# LEFT
to_plot = shrink_abadir_3
title = "Shrinkage approach over m region [0.3n,0.4n]"
plot_3d(to_plot,"contour",color_3_RWB,"Number of stocks", "Number of days", "% Excess l2-norm", 
        title, cluster_color="#b30000", 30, 0.25, z_min = z_min, z_max = z_max, cluster_stop=split)%>%
  add_trace(x =c(70,420), y =  c(70,420), name = "linear",
            marker=list(color="#000000", dash="dot"), line=list(dash="dot",width=1))%>%
  layout(xaxis = list( showgrid = FALSE, range = rangex), yaxis = list( showgrid = FALSE, range=rangey), margin=list( pad = 0))

# RIGHT
to_plot = shrink_abadir_2
title = "Shrinkage approach over m region [0.2n,0.4n]"
plot_3d(to_plot,"contour",color_3_RWB,"Number of stocks", "Number of days", "% Excess l2-norm",
        title, cluster_color="#b30000", 30, 0.25, z_min = z_min, z_max = z_max, cluster_stop=split)%>%
  add_trace(x =c(70,420), y =  c(70,420), name = "linear",
            marker=list(color="#000000", dash="dot"), line=list(dash="dot",width=1))%>%
  layout(xaxis = list( showgrid = FALSE, range = rangex), yaxis = list( showgrid = FALSE, range=rangey), margin=list( pad = 0))



##### FIGURE 10
rangex=c(35,420)
rangey=c(70,420)

shrink_abadir_2 = (subset_shrink - subset_abadir_8)
shrink_abadir_3 = (subset_shrink - subset_abadir_11)
z_min = min(c(shrink_abadir_2,shrink_abadir_3))
z_max = max(c(shrink_abadir_2,shrink_abadir_3))

split = abs(z_min)/(abs(z_min)+z_max)

# LEFT
to_plot = shrink_abadir_3
title = "Shrinkage approach - m region [0.3n,0.4n]"
plot_3d(to_plot,"contour",color_3_RWB,"Number of stocks", "Number of days", "Excess l2-norm", 
        title, cluster_color="#b30000", 30, 0.25, z_min = z_min, z_max = z_max, cluster_stop=split)%>%
  add_trace(x =c(70,420), y =  c(70,420), name = "linear",
            marker=list(color="#000000", dash="dot"), line=list(dash="dot",width=1))%>%
  layout(xaxis = list( showgrid = FALSE, range = rangex), yaxis = list( showgrid = FALSE, range=rangey), margin=list( pad = 0))

# RIGHT
to_plot = shrink_abadir_2
title = "Shrinkage approach - m region [0.2n,0.4n]"
plot_3d(to_plot,"contour",color_3_RWB,"Number of stocks", "Number of days", "Excess l2-norm",
        title, cluster_color="#b30000", 30, 0.25, z_min = z_min, z_max = z_max, cluster_stop=split)%>%
  add_trace(x =c(70,420), y =  c(70,420), name = "linear",
            marker=list(color="#000000", dash="dot"), line=list(dash="dot",width=1))%>%
  layout(xaxis = list( showgrid = FALSE, range = rangex), yaxis = list( showgrid = FALSE, range=rangey), margin=list( pad = 0))



##### APPENDIX
cluster_color = "#0033cc"
split = split_cluster
plot_lines_m(1,"contour",30,cluster_color, 0.1, cluster_stop=split)
plot_lines_m(2,"contour",30,cluster_color, 0.1, cluster_stop=split)
plot_lines_m(3,"contour",30,cluster_color, 0.1, cluster_stop=split)
plot_lines_m(4,"contour",30,cluster_color, 0.1, cluster_stop=split)
plot_lines_m(5,"contour",30,cluster_color, 0.1, cluster_stop=split)
plot_lines_m(6,"contour",30,cluster_color, 0.1, cluster_stop=split)
plot_lines_m(7,"contour",30,cluster_color, 0.1, cluster_stop=split)
plot_lines_m(8,"contour",30,cluster_color, 0.1, cluster_stop=split)

plot_lines_m(2,"lines")
plot_lines_m(3,"lines")
plot_lines_m(4,"lines")
plot_lines_m(5,"lines")
plot_lines_m(6,"lines")
plot_lines_m(7,"lines")
plot_lines_m(8,"lines")

