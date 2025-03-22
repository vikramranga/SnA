# 
##
### writting this function to plot SnA relationships
##
#

SA_data_1 <- read.csv("SA_data_1.csv")

sna_plot <- function(s_var, a_var, data = NULL, papers = 'rangaNpani', ...){
  if(data){
    s_var <- data[s_var]
    a_var <- data[a_var]
  } else {
  s_var = s_var
  s_var = a_var
  }
  g <- ggplot(data = data) +
    geom_line()
}