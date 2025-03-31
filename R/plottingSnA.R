#
##
### check the graphs from Schumm & Begin
##
#

snaData <- readxl::read_xlsx('Data/Data_SnA_ajijul.xlsx')

#<--------------------------- Functions --------------------------------->

# function to plot logarithmic minor breaks
# from https://stackoverflow.com/questions/30179442/plotting-minor-breaks-on-a-log-scale-with-ggplot


log_breaks = function(maj, radix=10) {
  function(x) {
    minx         = floor(min(logb(x,radix), na.rm=T)) - 1
    maxx         = ceiling(max(logb(x,radix), na.rm=T)) + 1
    n_major      = maxx - minx + 1
    major_breaks = seq(minx, maxx, by=1)
    
    if (maj) {
      breaks = major_breaks
    } else {
      steps = logb(1:(radix-1),radix)
      breaks = rep(steps, times=n_major) +
        rep(major_breaks, each=radix-1)
    }
    radix^breaks
  }
}


scale_x_log_eng = function(..., radix=10) {
  scale_x_continuous(...,
                     trans=scales::log_trans(radix),
                     breaks=log_breaks(TRUE, radix),
                     minor_breaks=log_breaks(FALSE, radix))
}
scale_y_log_eng = function(..., radix=10) {
  scale_y_continuous(...,
                     trans=scales::log_trans(radix),
                     breaks=log_breaks(TRUE, radix),
                     minor_breaks=log_breaks(FALSE, radix))
}

# Plotting

ggplot() + 
  scale_x_log_eng(name = "A (ha)", limits = c(0.00003, 0.03),
                          labels = scales::comma) +
  scale_y_log_eng(name = "S (m/m)", limits = c(0.01, 1),
                  labels = scales::comma) + 
  theme_bw() +
  geom_segment(aes(x = 0.00023, 
                   xend = 0.0077, 
                   y = 0.1217413 * 0.00023^-0.086, 
                   yend = 0.1217413 * 0.0077^-0.086), 
               linewidth =  1, 
               lty = 2) + 
  annotation_logticks() 
