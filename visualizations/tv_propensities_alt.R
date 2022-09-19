library(lubridate)
library(MMWRweek)
library(ggplot2)
library("RColorBrewer")

propensities = readRDS("../data/smoothedpropensities_alt_08262021.RDS")

propensities$date = MMWRweek::MMWRweek2Date(MMWRyear = propensities$year,
                                            MMWRweek = propensities$week,
                                            MMWRday = 1)

my_palette <- brewer.pal(name="Greys",n=9)[3:(3+5)]

## Example propensity: Fever + Not H + White + Male + 35to44
x = as.matrix(c(0,1,0,1,0,0,1,0,1,0,0,0,0), ncol = 1)
propensities$probs = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x)))

png(filename = "../figs/tvprop_alt_fig1.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = propensities, aes(x = date, y = probs)) +
  geom_point(size = 5) +
  labs(x = "Date",
       y = "COVID-19 Testing Propensity",
       title = "35-44, non-Hispanic, White Male with Symptoms") +
  theme(text = element_text(size=20))

dev.off()

## ALL AGES
x_age1 = as.matrix(c(0,1,0,1,0,0,1,1,0,0,0,0,0), ncol = 1)
x_age2 = as.matrix(c(0,1,0,1,0,0,1,0,1,0,0,0,0), ncol = 1)
x_age3 = as.matrix(c(0,1,0,1,0,0,1,0,0,1,0,0,0), ncol = 1)
x_age4 = as.matrix(c(0,1,0,1,0,0,1,0,0,0,1,0,0), ncol = 1)
x_age5 = as.matrix(c(0,1,0,1,0,0,1,0,0,0,0,1,0), ncol = 1)
x_age6 = as.matrix(c(0,1,0,1,0,0,1,0,0,0,0,0,1), ncol = 1)
propensities$probs1 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age1)))
propensities$probs2 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age2)))
propensities$probs3 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age3)))
propensities$probs4 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age4)))
propensities$probs5 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age5)))
propensities$probs6 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age6)))

propensities_long = rbind(propensities, propensities, propensities,
                          propensities, propensities, propensities)

propensities_long$age = c(rep("25to34", nrow(propensities)),
                          rep("35to44", nrow(propensities)),
                          rep("45to54", nrow(propensities)),
                          rep("55to64", nrow(propensities)),
                          rep("65to74", nrow(propensities)),
                          rep("75plus", nrow(propensities))
                          )

propensities_long$propensity = propensities_long$probs1 * (propensities_long$age == "25to34") +
  propensities_long$probs2 * (propensities_long$age == "35to44") +
  propensities_long$probs3 * (propensities_long$age == "45to54") +
  propensities_long$probs4 * (propensities_long$age == "55to64") +
  propensities_long$probs5 * (propensities_long$age == "65to74") +
  propensities_long$probs6 * (propensities_long$age == "75plus")

propensities_long_fever = propensities_long

my_palette <- brewer.pal(name="Greys",n=9)[3:(3+5)]

png(filename = "../figs/tvprop_alt_fig1_mainpaper.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = propensities_long, aes(x = date, y = propensity, col = age)) +
  geom_point(size = 5) +
  theme_classic() +
  labs(x = "Date",
       y = "COVID-19 Testing Propensity",
       title = "Non-Hispanic, White Male with Symptoms") +
  theme(text = element_text(size=25)) +
  scale_color_manual(values=my_palette) 

dev.off()


## Example propensity: Not Fever + Not H + White + Male + 65to74
x = as.matrix(c(1,0,1,1,0,0,1,0,0,0,0,1,0), ncol = 1)
propensities$probs = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x)))


png(filename = "../figs/tvprop_alt_fig2.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = propensities, aes(x = date, y = probs)) +
  geom_point(size = 5) +
  labs(x = "Date",
       y = "Testing Propensity",
       title = "65-74, non-Hispanic, White Female without Symptoms") +
  theme(text = element_text(size=20))

dev.off()

## ALL AGES
x_age1 = as.matrix(c(1,0,0,1,0,0,1,1,0,0,0,0,0), ncol = 1)
x_age2 = as.matrix(c(1,0,0,1,0,0,1,0,1,0,0,0,0), ncol = 1)
x_age3 = as.matrix(c(1,0,0,1,0,0,1,0,0,1,0,0,0), ncol = 1)
x_age4 = as.matrix(c(1,0,0,1,0,0,1,0,0,0,1,0,0), ncol = 1)
x_age5 = as.matrix(c(1,0,0,1,0,0,1,0,0,0,0,1,0), ncol = 1)
x_age6 = as.matrix(c(1,0,0,1,0,0,1,0,0,0,0,0,1), ncol = 1)
propensities$probs1 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age1)))
propensities$probs2 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age2)))
propensities$probs3 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age3)))
propensities$probs4 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age4)))
propensities$probs5 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age5)))
propensities$probs6 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age6)))

propensities_long = rbind(propensities, propensities, propensities,
                          propensities, propensities, propensities)

propensities_long$age = c(rep("25to34", nrow(propensities)),
                          rep("35to44", nrow(propensities)),
                          rep("45to54", nrow(propensities)),
                          rep("55to64", nrow(propensities)),
                          rep("65to74", nrow(propensities)),
                          rep("75plus", nrow(propensities))
)

propensities_long$propensity = propensities_long$probs1 * (propensities_long$age == "25to34") +
  propensities_long$probs2 * (propensities_long$age == "35to44") +
  propensities_long$probs3 * (propensities_long$age == "45to54") +
  propensities_long$probs4 * (propensities_long$age == "55to64") +
  propensities_long$probs5 * (propensities_long$age == "65to74") +
  propensities_long$probs6 * (propensities_long$age == "75plus")

propensities_long_withoutfever = propensities_long

png(filename = "../figs/tvprop_alt_fig2_mainpaper.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = propensities_long, aes(x = date, y = propensity, col = age)) +
  geom_point(size = 5) +
  theme_classic() +
  labs(x = "Date",
       y = "COVID-19 Testing Propensity",
       title = "Non-Hispanic, White Male without Symptoms") +
  theme(text = element_text(size=25)) +
  scale_color_manual(values=my_palette) 

dev.off()


propensities_long_fever$propensity/propensities_long_withoutfever$propensity

## APPENDIX VERSIONS OF MAIN PAPER PLOT

## NotHoL, AFrican American

## Fig1
x_age1 = as.matrix(c(0,1,0,0,1,0,0,1,0,0,0,0,0), ncol = 1)
x_age2 = as.matrix(c(0,1,0,0,1,0,0,0,1,0,0,0,0), ncol = 1)
x_age3 = as.matrix(c(0,1,0,0,1,0,0,0,0,1,0,0,0), ncol = 1)
x_age4 = as.matrix(c(0,1,0,0,1,0,0,0,0,0,1,0,0), ncol = 1)
x_age5 = as.matrix(c(0,1,0,0,1,0,0,0,0,0,0,1,0), ncol = 1)
x_age6 = as.matrix(c(0,1,0,0,1,0,0,0,0,0,0,0,1), ncol = 1)
propensities$probs1 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age1)))
propensities$probs2 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age2)))
propensities$probs3 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age3)))
propensities$probs4 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age4)))
propensities$probs5 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age5)))
propensities$probs6 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age6)))

propensities_long = rbind(propensities, propensities, propensities,
                          propensities, propensities, propensities)

propensities_long$age = c(rep("25to34", nrow(propensities)),
                          rep("35to44", nrow(propensities)),
                          rep("45to54", nrow(propensities)),
                          rep("55to64", nrow(propensities)),
                          rep("65to74", nrow(propensities)),
                          rep("75plus", nrow(propensities))
)

propensities_long$propensity = propensities_long$probs1 * (propensities_long$age == "25to34") +
  propensities_long$probs2 * (propensities_long$age == "35to44") +
  propensities_long$probs3 * (propensities_long$age == "45to54") +
  propensities_long$probs4 * (propensities_long$age == "55to64") +
  propensities_long$probs5 * (propensities_long$age == "65to74") +
  propensities_long$probs6 * (propensities_long$age == "75plus")

propensities_long_fever = propensities_long

my_palette <- brewer.pal(name="Greys",n=9)[3:(3+5)]

png(filename = "../figs/tvprop_alt_fig1_supp1.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = propensities_long, aes(x = date, y = propensity, col = age)) +
  geom_point(size = 5) +
  theme_classic() + 
  labs(x = "Date",
       y = "COVID-19 Testing Propensity",
       title = "Non-Hispanic, African American Male with Symptoms") +
  theme(text = element_text(size=25)) +
  scale_color_manual(values=my_palette)

dev.off()

## Fig 2
x_age1 = as.matrix(c(1,0,0,0,1,0,0,1,0,0,0,0,0), ncol = 1)
x_age2 = as.matrix(c(1,0,0,0,1,0,0,0,1,0,0,0,0), ncol = 1)
x_age3 = as.matrix(c(1,0,0,0,1,0,0,0,0,1,0,0,0), ncol = 1)
x_age4 = as.matrix(c(1,0,0,0,1,0,0,0,0,0,1,0,0), ncol = 1)
x_age5 = as.matrix(c(1,0,0,0,1,0,0,0,0,0,0,1,0), ncol = 1)
x_age6 = as.matrix(c(1,0,0,0,1,0,0,0,0,0,0,0,1), ncol = 1)
propensities$probs1 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age1)))
propensities$probs2 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age2)))
propensities$probs3 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age3)))
propensities$probs4 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age4)))
propensities$probs5 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age5)))
propensities$probs6 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age6)))

propensities_long = rbind(propensities, propensities, propensities,
                          propensities, propensities, propensities)

propensities_long$age = c(rep("25to34", nrow(propensities)),
                          rep("35to44", nrow(propensities)),
                          rep("45to54", nrow(propensities)),
                          rep("55to64", nrow(propensities)),
                          rep("65to74", nrow(propensities)),
                          rep("75plus", nrow(propensities))
)

propensities_long$propensity = propensities_long$probs1 * (propensities_long$age == "25to34") +
  propensities_long$probs2 * (propensities_long$age == "35to44") +
  propensities_long$probs3 * (propensities_long$age == "45to54") +
  propensities_long$probs4 * (propensities_long$age == "55to64") +
  propensities_long$probs5 * (propensities_long$age == "65to74") +
  propensities_long$probs6 * (propensities_long$age == "75plus")

propensities_long_withoutfever = propensities_long

png(filename = "../figs/tvprop_alt_fig2_supp1.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = propensities_long, aes(x = date, y = propensity, col = age)) +
  geom_point(size = 5) +
  theme_classic() +
  labs(x = "Date",
       y = "COVID-19 Testing Propensity",
       title = "Non-Hispanic, African American Male without Symptoms") +
  theme(text = element_text(size=25)) +
  scale_color_manual(values=my_palette)

dev.off()

## HoL, White
x_age1 = as.matrix(c(0,1,0,0,0,0,1,1,0,0,0,0,0), ncol = 1)
x_age2 = as.matrix(c(0,1,0,0,0,0,1,0,1,0,0,0,0), ncol = 1)
x_age3 = as.matrix(c(0,1,0,0,0,0,1,0,0,1,0,0,0), ncol = 1)
x_age4 = as.matrix(c(0,1,0,0,0,0,1,0,0,0,1,0,0), ncol = 1)
x_age5 = as.matrix(c(0,1,0,0,0,0,1,0,0,0,0,1,0), ncol = 1)
x_age6 = as.matrix(c(0,1,0,0,0,0,1,0,0,0,0,0,1), ncol = 1)
propensities$probs1 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age1)))
propensities$probs2 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age2)))
propensities$probs3 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age3)))
propensities$probs4 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age4)))
propensities$probs5 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age5)))
propensities$probs6 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age6)))

propensities_long = rbind(propensities, propensities, propensities,
                          propensities, propensities, propensities)

propensities_long$age = c(rep("25to34", nrow(propensities)),
                          rep("35to44", nrow(propensities)),
                          rep("45to54", nrow(propensities)),
                          rep("55to64", nrow(propensities)),
                          rep("65to74", nrow(propensities)),
                          rep("75plus", nrow(propensities))
)

propensities_long$propensity = propensities_long$probs1 * (propensities_long$age == "25to34") +
  propensities_long$probs2 * (propensities_long$age == "35to44") +
  propensities_long$probs3 * (propensities_long$age == "45to54") +
  propensities_long$probs4 * (propensities_long$age == "55to64") +
  propensities_long$probs5 * (propensities_long$age == "65to74") +
  propensities_long$probs6 * (propensities_long$age == "75plus")

propensities_long_fever = propensities_long

my_palette <- brewer.pal(name="Greys",n=9)[3:(3+5)]

png(filename = "../figs/tvprop_alt_fig1_supp2.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = propensities_long, aes(x = date, y = propensity, col = age)) +
  geom_point(size = 5) +
  theme_classic() +
  labs(x = "Date",
       y = "COVID-19 Testing Propensity",
       title = "Hispanic, White Male with Symptoms") +
  theme(text = element_text(size=25)) +
  scale_color_manual(values=my_palette)

dev.off()

## Fig 2
x_age1 = as.matrix(c(1,0,0,0,0,0,1,1,0,0,0,0,0), ncol = 1)
x_age2 = as.matrix(c(1,0,0,0,0,0,1,0,1,0,0,0,0), ncol = 1)
x_age3 = as.matrix(c(1,0,0,0,0,0,1,0,0,1,0,0,0), ncol = 1)
x_age4 = as.matrix(c(1,0,0,0,0,0,1,0,0,0,1,0,0), ncol = 1)
x_age5 = as.matrix(c(1,0,0,0,0,0,1,0,0,0,0,1,0), ncol = 1)
x_age6 = as.matrix(c(1,0,0,0,0,0,1,0,0,0,0,0,1), ncol = 1)
propensities$probs1 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age1)))
propensities$probs2 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age2)))
propensities$probs3 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age3)))
propensities$probs4 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age4)))
propensities$probs5 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age5)))
propensities$probs6 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age6)))

propensities_long = rbind(propensities, propensities, propensities,
                          propensities, propensities, propensities)

propensities_long$age = c(rep("25to34", nrow(propensities)),
                          rep("35to44", nrow(propensities)),
                          rep("45to54", nrow(propensities)),
                          rep("55to64", nrow(propensities)),
                          rep("65to74", nrow(propensities)),
                          rep("75plus", nrow(propensities))
)

propensities_long$propensity = propensities_long$probs1 * (propensities_long$age == "25to34") +
  propensities_long$probs2 * (propensities_long$age == "35to44") +
  propensities_long$probs3 * (propensities_long$age == "45to54") +
  propensities_long$probs4 * (propensities_long$age == "55to64") +
  propensities_long$probs5 * (propensities_long$age == "65to74") +
  propensities_long$probs6 * (propensities_long$age == "75plus")

propensities_long_withoutfever = propensities_long

png(filename = "../figs/tvprop_alt_fig2_supp2.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = propensities_long, aes(x = date, y = propensity, col = age)) +
  geom_point(size = 5) +
  theme_classic() + 
  labs(x = "Date",
       y = "COVID-19 Testing Propensity",
       title = "Hispanic, White Male without Symptoms") +
  theme(text = element_text(size=25)) +
  scale_color_manual(values=my_palette)

dev.off()

## NotHoL, Other
x_age1 = as.matrix(c(0,1,0,0,0,1,0,1,0,0,0,0,0), ncol = 1)
x_age2 = as.matrix(c(0,1,0,0,0,1,0,0,1,0,0,0,0), ncol = 1)
x_age3 = as.matrix(c(0,1,0,0,0,1,0,0,0,1,0,0,0), ncol = 1)
x_age4 = as.matrix(c(0,1,0,0,0,1,0,0,0,0,1,0,0), ncol = 1)
x_age5 = as.matrix(c(0,1,0,0,0,1,0,0,0,0,0,1,0), ncol = 1)
x_age6 = as.matrix(c(0,1,0,0,0,1,0,0,0,0,0,0,1), ncol = 1)
propensities$probs1 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age1)))
propensities$probs2 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age2)))
propensities$probs3 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age3)))
propensities$probs4 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age4)))
propensities$probs5 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age5)))
propensities$probs6 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age6)))

propensities_long = rbind(propensities, propensities, propensities,
                          propensities, propensities, propensities)

propensities_long$age = c(rep("25to34", nrow(propensities)),
                          rep("35to44", nrow(propensities)),
                          rep("45to54", nrow(propensities)),
                          rep("55to64", nrow(propensities)),
                          rep("65to74", nrow(propensities)),
                          rep("75plus", nrow(propensities))
)

propensities_long$propensity = propensities_long$probs1 * (propensities_long$age == "25to34") +
  propensities_long$probs2 * (propensities_long$age == "35to44") +
  propensities_long$probs3 * (propensities_long$age == "45to54") +
  propensities_long$probs4 * (propensities_long$age == "55to64") +
  propensities_long$probs5 * (propensities_long$age == "65to74") +
  propensities_long$probs6 * (propensities_long$age == "75plus")

propensities_long_fever = propensities_long

my_palette <- brewer.pal(name="Greys",n=9)[3:(3+5)]

png(filename = "../figs/tvprop_alt_fig1_supp3.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = propensities_long, aes(x = date, y = propensity, col = age)) +
  geom_point(size = 5) +
  theme_classic() + 
  labs(x = "Date",
       y = "COVID-19 Testing Propensity",
       title = "Hispanic, 'Other Race' Male with Symptoms") +
  theme(text = element_text(size=25)) +
  scale_color_manual(values=my_palette)

dev.off()

## Fig 2
x_age1 = as.matrix(c(1,0,0,0,0,1,0,1,0,0,0,0,0), ncol = 1)
x_age2 = as.matrix(c(1,0,0,0,0,1,0,0,1,0,0,0,0), ncol = 1)
x_age3 = as.matrix(c(1,0,0,0,0,1,0,0,0,1,0,0,0), ncol = 1)
x_age4 = as.matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0), ncol = 1)
x_age5 = as.matrix(c(1,0,0,0,0,1,0,0,0,0,0,1,0), ncol = 1)
x_age6 = as.matrix(c(1,0,0,0,0,1,0,0,0,0,0,0,1), ncol = 1)
propensities$probs1 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age1)))
propensities$probs2 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age2)))
propensities$probs3 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age3)))
propensities$probs4 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age4)))
propensities$probs5 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age5)))
propensities$probs6 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age6)))

propensities_long = rbind(propensities, propensities, propensities,
                          propensities, propensities, propensities)

propensities_long$age = c(rep("25to34", nrow(propensities)),
                          rep("35to44", nrow(propensities)),
                          rep("45to54", nrow(propensities)),
                          rep("55to64", nrow(propensities)),
                          rep("65to74", nrow(propensities)),
                          rep("75plus", nrow(propensities))
)

propensities_long$propensity = propensities_long$probs1 * (propensities_long$age == "25to34") +
  propensities_long$probs2 * (propensities_long$age == "35to44") +
  propensities_long$probs3 * (propensities_long$age == "45to54") +
  propensities_long$probs4 * (propensities_long$age == "55to64") +
  propensities_long$probs5 * (propensities_long$age == "65to74") +
  propensities_long$probs6 * (propensities_long$age == "75plus")

propensities_long_withoutfever = propensities_long

png(filename = "../figs/tvprop_alt_fig2_supp3.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = propensities_long, aes(x = date, y = propensity, col = age)) +
  geom_point(size = 5) +
  theme_classic() + 
  labs(x = "Date",
       y = "COVID-19 Testing Propensity",
       title = "Hispanic, 'Other Race' Male without Symptoms") +
  theme(text = element_text(size=25)) +
  scale_color_manual(values=my_palette)

dev.off()

## NotHoL, White, Female
x_age1 = as.matrix(c(0,1,1,1,0,0,1,1,0,0,0,0,0), ncol = 1)
x_age2 = as.matrix(c(0,1,1,1,0,0,1,0,1,0,0,0,0), ncol = 1)
x_age3 = as.matrix(c(0,1,1,1,0,0,1,0,0,1,0,0,0), ncol = 1)
x_age4 = as.matrix(c(0,1,1,1,0,0,1,0,0,0,1,0,0), ncol = 1)
x_age5 = as.matrix(c(0,1,1,1,0,0,1,0,0,0,0,1,0), ncol = 1)
x_age6 = as.matrix(c(0,1,1,1,0,0,1,0,0,0,0,0,1), ncol = 1)
propensities$probs1 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age1)))
propensities$probs2 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age2)))
propensities$probs3 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age3)))
propensities$probs4 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age4)))
propensities$probs5 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age5)))
propensities$probs6 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age6)))

propensities_long = rbind(propensities, propensities, propensities,
                          propensities, propensities, propensities)

propensities_long$age = c(rep("25to34", nrow(propensities)),
                          rep("35to44", nrow(propensities)),
                          rep("45to54", nrow(propensities)),
                          rep("55to64", nrow(propensities)),
                          rep("65to74", nrow(propensities)),
                          rep("75plus", nrow(propensities))
)

propensities_long$propensity = propensities_long$probs1 * (propensities_long$age == "25to34") +
  propensities_long$probs2 * (propensities_long$age == "35to44") +
  propensities_long$probs3 * (propensities_long$age == "45to54") +
  propensities_long$probs4 * (propensities_long$age == "55to64") +
  propensities_long$probs5 * (propensities_long$age == "65to74") +
  propensities_long$probs6 * (propensities_long$age == "75plus")

propensities_long_fever = propensities_long

my_palette <- brewer.pal(name="Greys",n=9)[3:(3+5)]

png(filename = "../figs/tvprop_alt_fig1_supp4.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = propensities_long, aes(x = date, y = propensity, col = age)) +
  geom_point(size = 5) +
  theme_classic() + 
  labs(x = "Date",
       y = "COVID-19 Testing Propensity",
       title = "Not Hispanic, White Female with Symptoms") +
  theme(text = element_text(size=25)) +
  scale_color_manual(values=my_palette)

dev.off()

## Fig 2
x_age1 = as.matrix(c(1,0,1,1,0,0,1,1,0,0,0,0,0), ncol = 1)
x_age2 = as.matrix(c(1,0,1,1,0,0,1,0,1,0,0,0,0), ncol = 1)
x_age3 = as.matrix(c(1,0,1,1,0,0,1,0,0,1,0,0,0), ncol = 1)
x_age4 = as.matrix(c(1,0,1,1,0,0,1,0,0,0,1,0,0), ncol = 1)
x_age5 = as.matrix(c(1,0,1,1,0,0,1,0,0,0,0,1,0), ncol = 1)
x_age6 = as.matrix(c(1,0,1,1,0,0,1,0,0,0,0,0,1), ncol = 1)
propensities$probs1 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age1)))
propensities$probs2 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age2)))
propensities$probs3 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age3)))
propensities$probs4 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age4)))
propensities$probs5 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age5)))
propensities$probs6 = as.vector(1/(1+exp(-as.matrix(propensities[,3:15])%*%x_age6)))

propensities_long = rbind(propensities, propensities, propensities,
                          propensities, propensities, propensities)

propensities_long$age = c(rep("25to34", nrow(propensities)),
                          rep("35to44", nrow(propensities)),
                          rep("45to54", nrow(propensities)),
                          rep("55to64", nrow(propensities)),
                          rep("65to74", nrow(propensities)),
                          rep("75plus", nrow(propensities))
)

propensities_long$propensity = propensities_long$probs1 * (propensities_long$age == "25to34") +
  propensities_long$probs2 * (propensities_long$age == "35to44") +
  propensities_long$probs3 * (propensities_long$age == "45to54") +
  propensities_long$probs4 * (propensities_long$age == "55to64") +
  propensities_long$probs5 * (propensities_long$age == "65to74") +
  propensities_long$probs6 * (propensities_long$age == "75plus")

propensities_long_withoutfever = propensities_long

png(filename = "../figs/tvprop_alt_fig2_supp4.png",
    width = 960, height = 480, units = "px", pointsize = 25)

ggplot(data = propensities_long, aes(x = date, y = propensity, col = age)) +
  geom_point(size = 5) +
  theme_classic() +
  labs(x = "Date",
       y = "COVID-19 Testing Propensity",
       title = "Not Hispanic, White Female without Symptoms") +
  theme(text = element_text(size=25)) +
  scale_color_manual(values=my_palette) 

dev.off()