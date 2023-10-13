###
# Macro Survey 
###

rm(list = ls())
library(dplyr)
library(ggplot2)

macro_survey <- read.csv('./data/macro_survey.csv')


# Summarize by group

macro_by_group <- macro_survey |> 
  group_by(Tolerance, Site, Survey) |> 
  summarize(count = sum(Count))

##
# Run t-tests for each group #############
##

# |- first need to filter by group -------------
moderate_group <- macro_by_group |> 
  filter(Tolerance == 'Moderate')

# check assumption of normality
moderate_group$count[moderate_group$Site == 'ShoppingPlaza'] |> 
  density() |> 
  plot()

moderate_group$count[moderate_group$Site == 'Upstream'] |> 
  density() |> 
  plot()

wilcox.test(count ~ Site, data = moderate_group,
            exact = FALSE, conf.int = T)

sensitive_group <- macro_by_group |> 
  filter(Tolerance == 'Sensitive')

# check assumption of normality
sensitive_group$count[sensitive_group$Site == 'ShoppingPlaza'] |> 
  density() |> 
  plot()

sensitive_group$count[sensitive_group$Site == 'Upstream'] |> 
  density() |> 
  plot()

wilcox.test(count ~ Site, data = sensitive_group,
            exact = FALSE, conf.int = T)


Tolerant_group <- macro_by_group |> 
  filter(Tolerance == 'Tolerant')

# check assumption of normality
Tolerant_group$count[Tolerant_group$Site == 'ShoppingPlaza'] |> 
  density() |> 
  plot()

Tolerant_group$count[Tolerant_group$Site == 'Upstream'] |> 
  density() |> 
  plot()

wilcox.test(count ~ Site, data = Tolerant_group,
            exact = FALSE, conf.int = T)


###
# Create plots #############
###

# create sumamry df
macro_plot_df <- macro_by_group |> 
  group_by(Tolerance, Site) |> 
  summarize(mean_count = mean(count),
            sd_count = sd(count))

ggplot(macro_plot_df) +
  geom_bar(aes(x = Tolerance, y = mean_count,
               fill = Site),
           stat = 'identity', position = 'dodge') +
  geom_errorbar(aes(x = Tolerance, ymin = mean_count,
                    ymax = mean_count + sd_count,
                    color = Site),
                position = 'dodge') +
  scale_color_manual(values = c('lightblue', 'darkblue')) +
  scale_fill_manual(values = c('lightblue', 'darkblue')) +
  labs(x = 'Invertebrate Tolerance Level',
       y = 'Mean Abundance',
       fill = "", color = "") +
  theme_classic()



