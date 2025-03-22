### Libraries first ###

library(pacman)
p_load(tidyverse, deming, scales)

sa_rel <- readxl::read_xlsx("SA_data_1.xlsx")
sa_rel <- sa_rel[complete.cases(sa_rel),]
#Changing this computation using tidyverse style
sa_rel <- sa_rel |> 
  mutate(S_mm = S/100,
        A_ha = `CA_(m2)`/10000,
        sqft_criterion = Av_width * Av_depth)

sa_rel <- sa_rel |> 
  filter(sqft_criterion > 929) |> # critical cross section must be over 929cm2 hence the condition.
  mutate(w_d_all = "WDR = All")
sa_rel_ltone <- subset(sa_rel, w.d <= 1)
sa_rel_gtone <- subset(sa_rel, w.d >= 1)

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

# Function to write linear regression and r2 on ggplot2
# I am not using this but I put these values using Inkscape.
lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

# Let's first see the correlation between log10(S_mm) & log10(A_ha)
cor.test(log10(sa_rel$S_mm), log10(sa_rel$A_ha))


#	Pearson's product-moment correlation

#data:  log10(sa_rel$S_mm) and log10(sa_rel$A_ha)
#t = -0.51092, df = 27, p-value = 0.6136
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.4482828  0.2786446
#sample estimates:
#        cor 
#-0.09785443

# Calculate the linear regression formula:


#<---------------------- Fit a deming regression --------------------------->

dem_reg_all <- deming(log10(sa_rel$S_mm) ~ log10(sa_rel$A_ha))
print(dem_reg_all)

#Call:
#deming(formula = log10(sa_rel$S_mm) ~ log10(sa_rel$A_ha))
#
#n= 29
#                 Coef  se(coef) lower 0.95 upper 0.95
#Intercept -0.91456221 0.5669427 -2.0257496  0.1966252
#Slope     -0.08619501 0.1854332 -0.4496375  0.2772475
#
#   Scale= 0.2201013 

#10^-0.91456221 = 0.1217413
#10^-1.9684802 = 0.01075276 lower 0.95 CI #not using them.
#10^0.02801468 = 1.066632 upper 0.95 CI #not using them.

p <- ggplot(sa_rel, aes(x = A_ha, y = S_mm, group= Width.Depth.Ratio)) + 
  geom_point(aes(shape = Width.Depth.Ratio)) + scale_shape_manual(values=c(0, 16)) 
p <- p  + scale_x_log_eng(name = "A (ha)", limits = c(0.00003, 0.03),
                          labels = scales::comma) +
  scale_y_log_eng(name = "S (m/m)", limits = c(0.01, 1),
                  labels = scales::comma) + facet_wrap(.~w_d_all) + theme_light() + theme(legend.position = c(0.8, 0.095)) +
  geom_segment(aes(x = 0.00023, xend = 0.0077, y = 0.1217413 * 0.00023^-0.086, yend = 0.1217413 * 0.0077^-0.086), size = 1, lty = 2) + annotation_logticks()
#geom_abline(intercept = -1.52, slope = - 0.136)+ annotation_logticks() +
#xlab("A (ha)") + ylab("S (m/m)")
p_dem_all <- p + geom_segment(aes(x = 0.00023, xend = 0.0077, y = 0.041 * 0.00023^-0.086, yend = 0.041 * 0.0077^-0.086), size = 1, lty = 1)


#ggsave("E:/Literature Survey Badlands/slope catchment area threshold/Figures/Figures_new/SA_all_deming.svg", plot = p_dem_all, width=8, height=6)


sa_rel_ltone_1 <- sa_rel_ltone[-5,]	

# Let's keep the outlier in the plot but regression equation does not include it.

# I am using OLS regression here also 


cor.test(log10(sa_rel_ltone$S_mm), log10(sa_rel_ltone$A_ha)) # this one is reported in publication.
#	Pearson's product-moment correlation

#data:  log10(sa_rel_ltone$S_mm) and log10(sa_rel_ltone$A_ha)
#t = 0.1464, df = 9, p-value = 0.8868
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.5677321  0.6301916
#sample estimates:
#       cor 
#0.04874334 

cor.test(log10(sa_rel_ltone_1$S_mm), log10(sa_rel_ltone_1$A_ha))

#	Pearson's product-moment correlation
#
#data:  log10(sa_rel_ltone_1$S_mm) and log10(sa_rel_ltone_1$A_ha)
#t = 1.1018, df = 7, p-value = 0.307
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.3755721  0.8352971
#sample estimates:
#      cor 
#0.3844384 


#<-------------- Fit a deming regression for WDR < 1 - here gully no. 18 is added.--------------->


deming_reg_ltone_1 <- deming(log10(sa_rel_ltone_1$S_mm)~ log10(sa_rel_ltone_1$A_ha))

#Call:
#deming(formula = log10(sa_rel_ltone_1$S_mm) ~ log10(sa_rel_ltone_1$A_ha))

#n= 10
#                 Coef  se(coef) lower 0.95 upper 0.95
#Intercept -0.51632691 0.7360947 -1.9590460  0.9263922
#Slope      0.04855582 0.2458482 -0.4332978  0.5304095
#
#   Scale= 0.1847613 
# transform intercept using antilog 10^-0.51632691 = 0.3045602

p <- ggplot(sa_rel_ltone, aes(x = A_ha, y = S_mm)) + 
  geom_point(shape = 0) 
p <- p  + scale_x_log_eng(name = "A (ha)", limits = c(0.00003, 0.03),
                          labels = scales::comma) +
  scale_y_log_eng(name = "S (m/m)", , limits = c(0.01, 1),
                  labels = scales::comma)+ facet_wrap(Width.Depth.Ratio~.) +
  geom_segment(aes(x = 0.00028, xend = 0.0048, y = 0.304 * 0.00028 ^0.048 , yend = 0.304 * 0.0048^0.048), size = 1, lty = 2) + theme_light() + annotation_logticks()
#geom_abline(intercept = -1.52, slope = - 0.136)+ annotation_logticks() +
#xlab("A (ha)") + ylab("S (m/m)")
p_ltone <- p + geom_segment(aes(x = 0.00028, xend = 0.0048, y = 0.18* 0.00028 ^0.048 , yend = 0.18 * 0.0048 ^0.048), size = 1, lty = 1)


#ggsave("E:/Literature Survey Badlands/slope catchment area threshold/Figures/SA_ltone_deming.svg", plot = p, width=8, height=6)



#deming_reg_gtone <- deming(log10(sa_rel_gtone$S_mm)~ log10(sa_rel_gtone$CA.m2.))

cor.test(log10(sa_rel_gtone$S_mm), log10(sa_rel_gtone$A_ha))
#	Pearson's product-moment correlation

#data:  log10(sa_rel_gtone$S_mm) and log10(sa_rel_gtone$A_ha)
#t = -0.56916, df = 16, p-value = 0.5772
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.5702370  0.3489495
#sample estimates:
#       cor 
#-0.1408704 


#################################################################################
#
#
# Fit a deming regression for WDR > 1
#
#
#################################################################################

deming_reg_gtone <- deming(log10(sa_rel_gtone$S_mm)~ log10(sa_rel_gtone$A_ha))

#Call:
#deming(formula = log10(sa_rel_gtone$S_mm) ~ log10(sa_rel_gtone$A_ha))
#
#n= 18
#                Coef se(coef) lower 0.95 upper 0.95
#Intercept -1.0113197 1.058312  -3.085574  1.0629342
#Slope     -0.1250759 0.336519  -0.784641  0.5344892#
#
#   Scale= 0.2263624 
# intercept = 10^-1.011 = 0.09749896

p <- ggplot(sa_rel_gtone, aes(x = A_ha, y = S_mm)) + 
  geom_point() 
p <- p  + scale_x_log_eng(name = "A (ha)", limits = c(0.00003, 0.03),
                          labels = scales::comma) +
  scale_y_log_eng(name = "S (m/m)", , limits = c(0.01, 1),
                  labels = scales::comma)+ facet_wrap(.~Width.Depth.Ratio)+
  geom_segment(aes(x = 0.00023, xend = 0.0077, y = 0.097*0.00023^-0.125, yend = 0.097*0.0077^-0.125), size = 1, lty = 2) + theme_light() + annotation_logticks()
#geom_abline(intercept = -1.52, slope = - 0.136)+ annotation_logticks() +
#xlab("A (ha)") + ylab("S (m/m)")
p_dem_wdrgt1 <- p + geom_segment(aes(x = 0.00023, xend = 0.0077, y = 0.032*0.00023^-0.125, yend = 0.032*0.0077^-0.125), size = 1, lty = 1)


ggsave("E:/Literature Survey Badlands/slope catchment area threshold/Figures/SA_gtone_deming.svg", plot = p, width=15, height=6)

p <- grid.arrange(p_dem_all, p_dem_wdrgt1, p_ltone, # use library(gridExtra)
                  ncol = 3, nrow = 1)


ggsave("E:/Literature Survey Badlands/slope catchment area threshold/Figures/Figures_new/SA_all_deming.svg", plot = p, width=10, height=6)


###############################################################################################################################################################
###############################################################################################################################################################
###############################################################################################################################################################
###############################################################################################################################################################
###############################################################################################################################################################


#################################################################################
summary(lm(sa_rel$Av_depth ~ sa_rel$Av_width))

#Call:
#lm(formula = sa_rel$Av_depth ~ sa_rel$Av_width)

#Residuals:
#    Min      1Q  Median      3Q     Max 
#-60.604 -19.004  -4.818  20.020 110.356 
#
#Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      -3.4811    19.7883  -0.176  0.86167    
#sa_rel$Av_width   0.9133     0.2095   4.360  0.00017 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#
#
#Residual standard error: 36.31 on 27 degrees of freedom
#Multiple R-squared:  0.4132,	Adjusted R-squared:  0.3915 
#F-statistic: 19.01 on 1 and 27 DF,  p-value: 0.0001697



summary(lm(sa_rel_ltone$Av_depth ~ sa_rel_ltone$Av_width))
#Call:
#lm(formula = sa_rel_ltone$Av_depth ~ sa_rel_ltone$Av_width)

#Residuals:
#    Min      1Q  Median      3Q     Max 
#-33.828 -13.690  -5.496   7.591  62.216 
#
#Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             3.4581    20.4208   0.169 0.869271    
#sa_rel_ltone$Av_width   1.2348     0.2139   5.774 0.000268 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 26.93 on 9 degrees of freedom
#Multiple R-squared:  0.7874,	Adjusted R-squared:  0.7638 
#F-statistic: 33.34 on 1 and 9 DF,  p-value: 0.00026


summary(lm(sa_rel_gtone$Av_depth ~ sa_rel_gtone$Av_width))

#Call:
#lm(formula = sa_rel_gtone$Av_depth ~ sa_rel_gtone$Av_width)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-23.090 -11.846  -4.085  12.106  33.922 
#
#Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             2.5335    12.8467   0.197  0.84614    
#sa_rel_gtone$Av_width   0.6065     0.1369   4.431  0.00042 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 16.3 on 16 degrees of freedom
#Multiple R-squared:  0.551,	Adjusted R-squared:  0.5229 
#F-statistic: 19.63 on 1 and 16 DF,  p-value: 0.0004196

#################################################################################
#
#     Facet wrap for Width.Depth Ratio.
#
#################################################################################

p_wdr <- ggplot(sa_rel, aes(x = Av_width, y = Av_depth)) + 
  geom_point() + xlim(10, 160) + ylim(-10, 250) +
  #geom_segment(aes(x = 10, xend = 160, y = 0.206 + 0.865*10, yend = 0.206 + 0.865*160), size = 1) + 
  geom_smooth(method=lm, color="black", se=FALSE, linetype="solid") +
  xlab("Average Width (cm)") + ylab("Average Depth (cm)") + facet_wrap(.~Width.Depth.Ratio) + theme_light()
p_wdr

ggsave("E:/Literature Survey Badlands/slope catchment area threshold/Figures/Figures_new/AvWnAvD.svg", plot = p)


p_wdr_all <- ggplot(sa_rel, aes(x = Av_width, y = Av_depth)) + 
  geom_point() + xlim(10, 160) + ylim(-10, 250) +
  #geom_segment(aes(x = 10, xend = 160, y = 0.206 + 0.865*10, yend = 0.206 + 0.865*160), size = 1) + 
  geom_smooth(method=lm, color="black", se=FALSE, linetype="solid") +
  xlab("Average Width (cm)") + ylab("Average Depth (cm)") + facet_wrap(.~w_d_all) + theme_light()
p_wdr_all

#ggarrange(p_wdr_all, p_wdr,
#          ncol = 2, nrow = 1)
#
p <- grid.arrange(p_wdr_all, p_wdr, # use library(gridExtra)
                  ncol = 1, nrow = 2)

ggsave("E:/Literature Survey Badlands/slope catchment area threshold/Figures/Figures_new/AvWnAvD_all.svg", plot = p, width=6, height=8)



###############################################################################################################################################################
###############################################################################################################################################################


#################################################################################
#
#     Facet wrap for Gully head depth
#
#################################################################################


cor.test(sa_rel_gtone$A_ha, sa_rel_gtone$Hhc)

#
#	Pearson's product-moment correlation
#
#data:  sa_rel_gtone$A_ha and sa_rel_gtone$Hfc
#t = 4.4957, df = 18, p-value = 0.0002796
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.4198963 0.8849811
#sample estimates:
#      cor 
#0.7272793 

cor.test(sa_rel_ltone$A_ha, sa_rel_ltone$Hhc)

#	Pearson's product-moment correlation
#
#data:  sa_rel_ltone$A_ha and sa_rel_ltone$Hfc
#t = 0.2355, df = 8, p-value = 0.8197
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.5767835  0.6772218
#sample estimates:
#       cor 
#0.08297626

summary(lm(sa_rel$Hhc~sa_rel$A_ha))

#Call:
#lm(formula = sa_rel$Hhc ~ sa_rel$A_ha)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-49.606 -27.015  -6.801  16.580 148.579 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    53.27      10.47   5.088  2.4e-05 ***
#sa_rel$A_ha 10984.68    4888.45   2.247    0.033 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#
#
#Residual standard error: 40.66 on 27 degrees of freedom
#Multiple R-squared:  0.1575,	Adjusted R-squared:  0.1263 
#F-statistic: 5.049 on 1 and 27 DF,  p-value: 0.03301


summary(lm(sa_rel_gtone$Hhc~sa_rel_gtone$A_ha))

#Call:
#lm(formula = sa_rel_gtone$Hhc ~ sa_rel_gtone$A_ha)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-34.614 -10.640  -4.997  17.297  30.283 
#
#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         40.334      5.741   7.026 2.86e-06 ***
#sa_rel_gtone$A_ha 9353.154   2672.790   3.499  0.00297 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1##
#
#Residual standard error: 18.66 on 16 degrees of freedom
#Multiple R-squared:  0.4335,	Adjusted R-squared:  0.3981 
#F-statistic: 12.25 on 1 and 16 DF,  p-value: 0.002967

summary(lm(sa_rel_ltone$Hhc~sa_rel_ltone$A_ha))

#Call:
#lm(formula = sa_rel_ltone$Hhc ~ sa_rel_ltone$A_ha)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-65.256 -36.598   6.255  18.958 123.821 

#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)          77.68      26.60   2.920    0.017 *
#sa_rel_ltone$A_ha 11257.30   12483.24   0.902    0.391  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 55.76 on 9 degrees of freedom
#Multiple R-squared:  0.08287,	Adjusted R-squared:  -0.01903 
#F-statistic: 0.8132 on 1 and 9 DF,  p-value: 0.3907

p <- ggplot(sa_rel, aes(x = A_ha, y = Hhc)) + 
  geom_point() +
  geom_smooth(method=lm, color="black", se=FALSE, linetype="solid") +
  xlab("A (ha)") + ylab("Gully Head Height") + facet_wrap(.~Width.Depth.Ratio) + theme_bw()
p

#################################################################################

cor.test(sa_rel_gtone$S_mm, sa_rel_gtone$Hfc)

#cor.test(sa_rel_gtone$S_mm, sa_rel_gtone$Hfc)
#
#	Pearson's product-moment correlation
#
#data:  sa_rel_gtone$S_mm and sa_rel_gtone$Hfc
#t = 0.039099, df = 18, p-value = 0.9692
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.4350797  0.4499015
#sample estimates:
#        cor 
#0.009215374


cor.test(sa_rel_ltone$S_mm, sa_rel_ltone$Hfc)

#	Pearson's product-moment correlation
#
#data:  sa_rel_ltone$S_mm and sa_rel_ltone$Hfc
#t = -1.395, df = 8, p-value = 0.2005
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.8384447  0.2596002
#sample estimates:
#       cor 
#-0.4423245

summary(lm(sa_rel$Hhc~sa_rel$S_mm))

#Call:
#lm(formula = sa_rel$Hhc ~ sa_rel$S_mm)
#
#Residuals:
#   Min     1Q Median     3Q    Max 
#-59.54 -29.51 -12.04  19.26 134.81 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    86.73      18.63   4.655  7.7e-05 ***
#sa_rel$S_mm   -69.20      67.72  -1.022    0.316    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 43.46 on 27 degrees of freedom
#Multiple R-squared:  0.03723,	Adjusted R-squared:  0.001568 
#F-statistic: 1.044 on 1 and 27 DF,  p-value: 0.316




summary(lm(sa_rel_gtone$Hhc~sa_rel_gtone$S_mm))

#Call:
#lm(formula = sa_rel_gtone$Hhc ~ sa_rel_gtone$S_mm)

#Residuals:
#    Min      1Q  Median      3Q     Max 
#-29.076 -14.404  -1.109   6.911  68.552 
#
#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)          38.97      13.40   2.908   0.0103 *
#sa_rel_gtone$S_mm    54.36      46.31   1.174   0.2577  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 23.79 on 16 degrees of freedom
#Multiple R-squared:  0.07927,	Adjusted R-squared:  0.02173 
#F-statistic: 1.378 on 1 and 16 DF,  p-value: 0.2577


summary(lm(sa_rel_ltone$Hhc~sa_rel_ltone$S_mm))

#Call:
#lm(formula = sa_rel_ltone$Hhc ~ sa_rel_ltone$S_mm)

#Residuals:
#    Min      1Q  Median      3Q     Max 
#-80.761 -29.377   0.453  32.377  87.024 
#
#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)         147.19      34.35   4.285  0.00203 **
#sa_rel_ltone$S_mm  -227.68     137.36  -1.658  0.13179   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 50.96 on 9 degrees of freedom
#Multiple R-squared:  0.2339,	Adjusted R-squared:  0.1488 
#F-statistic: 2.747 on 1 and 9 DF,  p-value: 0.1318


p <- ggplot(sa_rel, aes(x = S_mm, y = Hfc)) + 
  geom_point() +
  #geom_segment(aes(x = 10, xend = 160, y = 0.206 + 0.865*10, yend = 0.206 + 0.865*160), size = 1) + 
  geom_smooth(method=lm, color="black", se=FALSE, linetype="solid") +
  xlab("S (m/m)") + ylab("Gully Head Height") + facet_wrap(.~Width.Depth.Ratio) + theme_bw()
p



#########################################################################################################################################
#
#		Comparison of SA from LCV to other reported SA relationships
#
#########################################################################################################################################

p <- ggplot() 
p <- p  + scale_x_log_eng(name = "A (ha)", limits = c(0.0001, 10^3),
                          labels = scales::comma) +
  scale_y_log_eng(name = "S (m/m)", , limits = c(0.01, 1),
                  labels = scales::comma)+ 
  geom_segment(aes(x = 0.00023, xend = 0.0077, y = 0.032 * 0.00023^-0.125, yend = 0.032 * 0.0077^-0.125), size = 1, lty = 1) + theme_light() + annotation_logticks()
# This study WDR > 1
#geom_abline(intercept = -1.52, slope = - 0.136)+ annotation_logticks() +
#xlab("A (ha)") + ylab("S (m/m)")
p <- p + geom_segment(aes(x = 0.0001, xend = 0.04, y = 0.01 * 0.0002 ^-0.4 , yend = 0.01 * 0.04^-0.4), size = 1, lty = 1) # Badlands As per Torri et al. 2018
p <- p + geom_segment(aes(x = 0.1, xend = 10, y = 0.118 * 0.1 ^-0.111 , yend =  0.118 * 10^-0.111), size = 1, lty = 1) # As per Majhi et al. 2021
p <- p + geom_segment(aes(x = 0.1, xend = 1, y = 0.31 * 0.1 ^-0.25 , yend =  0.31 * 1^-0.25), size = 1, lty = 1) # As per Ghosh and Gucchait et al. 2016
p <- p + geom_segment(aes(x = 0.5, xend = 20, y = 0.080 * 0.5 ^-0.329 , yend =  0.080 * 20^-0.329), size = 1, lty = 1) # Cropland As per Torri and Poesen 2014
p <- p + geom_segment(aes(x = 10, xend = 500, y = 0.201 * 10 ^-0.295 , yend = 0.201 * 500^-0.295), size = 1, lty = 1) # Rangeland/Pasture As per Torri and Poesen 2014
p <- p + geom_segment(aes(x = 4, xend = 70, y = 0.557 * 4^-0.391 , yend =  0.557 * 70^-0.391), size = 1, lty = 1) # Forest/Grassland As per Torri and Poesen 2014
p <- p + geom_segment(aes(x = 0.01, xend = 20, y = 0.017 * 0.2^-0.123 , yend =  0.017 * 20^-0.123), size = 1, lty = 1) # As per Verachtert et al. 2010, specifically for piping.

#scale_color_manual(values=c("Chambal Badlands, WDR>1"="green", "Chambal Badlands (All)"="black","Italian Badlands"="chartreuse"))


ggsave("E:/Literature Survey Badlands/slope catchment area threshold/Figures/Figures_new/SA_comparison.svg", plot = p, width=8, height=6)



###########################################################################################################
#
#				Summary stats of sa_rel
#
###########################################################################################################

summary(sa_rel) # super easy 

#Gully No.        Status             CA_(m2)            S(%)       
#Min.   : 2.00   Length:29          Min.   : 2.203   Min.   :0.0700  
#1st Qu.:11.00   Class :character   1st Qu.: 6.058   1st Qu.:0.1500  
#Median :20.00   Mode  :character   Median : 7.990   Median :0.2400  
#Mean   :20.38                      Mean   :14.837   Mean   :0.2479  
#3rd Qu.:29.00                      3rd Qu.:17.391   3rd Qu.:0.2900  
#Max.   :39.00                      Max.   :70.438   Max.   :0.5600  
#      S            Av_width         Av_depth           w.d        
#Min.   : 7.00   Min.   : 32.12   Min.   : 22.88   Min.   :0.5723  
#1st Qu.:15.00   1st Qu.: 61.00   1st Qu.: 42.11   1st Qu.:0.8722  
#Median :24.00   Median : 91.00   Median : 65.78   Median :1.2562  
#Mean   :24.79   Mean   : 88.82   Mean   : 77.64   Mean   :1.3450  
#3rd Qu.:29.00   3rd Qu.:103.50   3rd Qu.: 96.50   3rd Qu.:1.7489  
#Max.   :56.00   Max.   :151.38   Max.   :223.89   Max.   :2.2201  
#  max_length         Hhc         Width.Depth.Ratio       S_mm       
#Min.   : 20.0   Min.   : 17.50   Length:29          Min.   :0.0700  
#1st Qu.: 60.0   1st Qu.: 40.00   Class :character   1st Qu.:0.1500  
#Median :120.0   Median : 54.00   Mode  :character   Median :0.2400  
#Mean   :132.1   Mean   : 69.57                      Mean   :0.2479  
#3rd Qu.:182.0   3rd Qu.: 80.00                      3rd Qu.:0.2900  
#Max.   :270.0   Max.   :216.00                      Max.   :0.5600  
#     A_ha           sqft_criterion    w_d_all         
#Min.   :0.0002203   Min.   : 1040   Length:29         
#1st Qu.:0.0006058   1st Qu.: 2601   Class :character  
#Median :0.0007990   Median : 5569   Mode  :character  
#Mean   :0.0014837   Mean   : 7842                     
#3rd Qu.:0.0017391   3rd Qu.:10040                     
#Max.   :0.0070438   Max.   :28686 

