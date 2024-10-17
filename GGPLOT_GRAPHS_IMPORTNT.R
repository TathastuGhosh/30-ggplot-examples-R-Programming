# ---------- REGRESSION RELATED --------------------#
# 1
# regression curve
CO2 %>% 
  ggplot(aes(conc,uptake,colour = Treatment))+
  geom_point()+
  geom_smooth()

# ----------------------------------------------------
# 2
# regression line
CO2 %>% 
  ggplot(aes(conc,uptake,colour = Treatment))+
  geom_point()+
  geom_smooth(method=lm,se=FALSE)+
  facet_wrap(~Type)+
  labs(title = "Concentration of CO2")+
  theme_bw()  

# ------------------------------------------------------
# 3
# regression curve with standard error
msleep %>% 
  filter(bodywt<2) %>% 
  ggplot(aes(bodywt,brainwt))+
  geom_point(aes(colour = sleep_total,
                 size = awake))+
  geom_smooth()+
  labs(x = "Body weight",
       y = "Brain weight",
       title = "Brain and body weight")+
  theme_bw()

# ------------------------------------------------------
# 4
# regression using faceting
library(gapminder)
gapminder %>% 
  filter(gdpPercap < 40000 & continent != "Oceania") %>% 
  ggplot(aes(gdpPercap,lifeExp,colour = year))+
  geom_point(shape="circle",alpha = 0.8)+
  geom_smooth(se = F)+
  labs(title = "GDP vs Life expectancy plot")+
  facet_wrap(~continent)

# ------------------------------------------------------

##########################################################################
# ---------- BOXPLOT RELATED --------------------#

# 5
# boxplot with points
CO2 %>% 
  ggplot(aes(Treatment,uptake))+
  geom_boxplot()+
  geom_point(alpha = 0.5, 
             aes(colour = Plant,
                 size=conc))+
  theme_bw()+
  facet_wrap(~Type)+
  labs(title = "Boxplot of Uptake",subtitle = "(For each treatment & for each Type)")

# -------------------------------------------------------
# 6
# side by side boxplot
Salaries %>% 
  filter(salary < 150000) %>% 
  ggplot(aes(x = rank, y = salary, fill = sex))+
  geom_boxplot(alpha = 0.5)+
  scale_x_discrete(breaks = c("AsstProf","AssocProf","Prof"),
                   labels = c("Assistant\nProfessor","Associate\nProfessor","Professor"))+
  scale_y_continuous(breaks = c(50000,75000,100000,125000,150000),
                     labels = c("$50K","$75K","$100K","$125K","$150K"))+
  labs(title = "Faculty salary by Rank and Gender",
       x = "",
       y = "Salary",
       fill = "Gender")+
  theme(legend.position = c(.10,.80))

# ------------------------------------------------------


####################################################################
# ---------------DENSITY PLOT --------------------#.

# 7
# density curve faceting
msleep %>%
  drop_na(vore) %>% 
  ggplot(aes(sleep_total))+
  geom_density(aes(fill = vore),alpha = 0.4)+
  facet_wrap(~vore)

# ------------------------------------------------------

# 8
#density curve together
msleep %>% 
  drop_na(vore) %>%
  ggplot(aes(sleep_total,fill=vore))+
  geom_density(alpha = 0.2)+
  theme_bw()

# ------------------------------------------------------

# 9
# symmetric density plot
library(ggplot2)
library(hrbrthemes)

# Dummy data
data <- data.frame(
  var1 = rnorm(1000),
  var2 = rnorm(1000, mean=2)
)

# Chart
p <- ggplot(data, aes(x=var1) ) +
  # Top
  geom_density( aes(x = var1, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=4.5, y=0.25, label="variable1"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = var2, y = -..density..), fill= "#404080") +
  geom_label( aes(x=4.5, y=-0.25, label="variable2"), color="#404080") +
  xlab("value of x")
p

# ------------------------------------------------------------
# 10

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)

# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

# sample size
sample_size = data %>% group_by(name) %>% summarize(num=n())

# Plot
data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(name, "\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=value, fill=name)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey",fill = "red", alpha=0.8) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A Violin wrapping a boxplot") +
  xlab("")
# -----------------------------------------------------------------
################################################################

# ------------------- HISTOGRAM --------------------------#

# 11
# histogram using faceting
library(forcats)
library(gapminder)
gss_cat %>% 
  filter(partyid %in% c("Strong democrat","Strong republican","Independent")) %>% 
  ggplot(aes(age))+
  geom_histogram(binwidth = 5,fill = "steelblue",alpha = 0.8)+
  facet_wrap(~partyid,nrow = 3,
             strip.position = "left")+ # top, bottom, left & right 
  labs(title = "Age distribution by political affiliation",
       x = "AGE",
       y = "")

################################################################3
# ----------- AREA & RASTER PLOT  & LEVEL PLOT & HEATMAPS---------------------3
# 12
# area plot
mpg %>% 
  ggplot(aes(x=displ))+
  geom_area(aes(y=hwy,fill="Highway"))+
  geom_area(aes(y=cty,fill="City"))+
  labs(title = "Highway vs Driving",
       x = "Engine displacement (L)",
       y = "Miles per gallon",
       fill = "")

# ----------------------------------------------------------

# 13
# raster plot
faithfuld %>% 
  ggplot(aes(x = waiting,
             y = eruptions,
             fill = density))+
  geom_raster()

# ----------------------------------------------------------
# 14
# level plot
library(latticeExtra) 
set.seed(1) 
data <- data.frame(x = rnorm(100), y = rnorm(100)) 
data$z <- with(data, x * y + rnorm(100, sd = 1)) 
levelplot(z ~ x * y, data, 
          panel = panel.levelplot.points, cex = 1.2
) + 
  layer_(panel.2dsmoother(..., n = 200))

# ------------------------------------------------------------------

# 15
# heatmaps(tile plot)

library(ggplot2)
library(hrbrthemes)
library(plotly)
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)

# new column: text for tooltip:
data <- data %>%
  mutate(text = paste0("x: ", x, "\n", "y: ", y, "\n", "Value: ",round(Z,2), "\n", "What else?"))

# classic ggplot, with text in aes
p <- ggplot(data, aes(X, Y, fill= Z, text=text)) + 
  geom_tile() 

ggplotly(p, tooltip="text")

# -----------------------------------------------------------------------


######################################################################3
# -------------- BAR PLOT ---------------------3

# 16
# multiple bar plot(side by side)
starwars %>% 
  filter(hair_color %in% c("black","brown")) %>% 
  drop_na(sex) %>% 
  ggplot(aes(hair_color,fill=sex))+
  geom_bar(position = "dodge",alpha = 0.5)+
  theme_bw()

# ------------------------------------------------------

# 17
# stacked bar plot
starwars %>% 
  filter(hair_color %in% c("black","brown")) %>% 
  drop_na(sex) %>% 
  ggplot(aes(hair_color,fill=sex))+
  geom_bar(alpha = 0.5)+
  theme_bw()

# -------------------------------------------------------

# 18
#bar plot with error bar
ToothGrowth %>% 
  filter(supp == "VC") %>% 
  mutate(dose = as.factor(dose)) %>% 
  group_by(dose) %>% 
  summarise(mean_len = mean(len),
            sd_len = sd(len)) %>% 
  ggplot(aes(dose,mean_len))+
  geom_point(size = 3,colour = "blue")+
  geom_bar(stat = "identity",fill = "skyblue",alpha = 0.7)+
  geom_errorbar(aes(x=dose,
                    ymin = mean_len - sd_len,
                    ymax = mean_len + sd_len,
                    width = 0.05))+
  theme_bw()

# -----------------------------------------------------

# 19
# circular barplot
library(tidyverse)

# Create dataset
data <- data.frame(
  id=seq(1,60),
  individual=paste( "Mister ", seq(1,60), sep=""),
  value=sample( seq(10,100), 60, replace=T)
)

# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data <- data

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


# Start the plot
p <- ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p



# ------------------------------------------------------------------------------------
#####################################################################3
# ---------------------- Lollipop Graph ----------------------

# 20
# Lollipop Graph
msleep %>% 
  group_by(order) %>% 
  summarise(mean_sleep = mean(sleep_total)) %>% 
  mutate(order = fct_reorder(order,mean_sleep)) %>% 
  ggplot(aes(x = order,y = mean_sleep))+
  geom_point(size = 3,
             colour = "orange")+
  geom_segment(aes(x = order,
                   y = mean(msleep$sleep_total),
                   xend = order,
                   yend = mean_sleep),
               colour = "black")+
  geom_hline(yintercept = mean(msleep$sleep_total),
             colour = "red",
             size = 1)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Average sleep time of mammals by order",
       x = "",
       y = "Hours")

# -----------------------------------------------------------
############################################################################
# -------------- ANNOTATIONS --------------------#
# 21
ggplot(mtcars,aes(wt,mpg,color = factor(gear)))+
  geom_point(size = 3,alpha = 0.6)+
  labs(title = "Weight vs Miles per gallon",
       x = "Weight",
       y = "Miles per gallon",
       color = "Gears")+
  theme_bw()+
  theme(plot.title = 
          element_text(size = 18,
                       face = "bold",
                       color = "steelblue",
                       hjust = 0.5),
        axis.text = 
          element_text(size = 10,
                       color ="red",
                       face = "bold"),
        axis.title = 
          element_text(size = 12,
                       color = "steelblue"))+
  theme(legend.position = c(0.9,0.8))+
  annotate("text",x = 4,y = 30,
           label = "Notice the difference\nbetween the cars with\n three and four gears",
           color = "black",
           face = "bold",
           size = 4)+
  geom_segment(x=4.5,y=27,
               xend = 5,yend = 17,
               arrow = arrow(length = unit(0.5,"cm")),
               color = "darkred",
               size = 1.5)+
  geom_segment(x=3.4,y=30,
               xend = 2.4,yend = 27,
               arrow = arrow(length = unit(0.5,"cm")),
               color = "darkgreen",
               size = 1.5)
# ----------------------------------------------------------
#######################################################################3

# -------------- PIE CHART -----------------#

# 22
# pie chart
ggplot(mtcars, aes(x="", y=mpg, fill=cyl)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

# -------------------------------------------------------------

library(ggplot2)
bar <- ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = cut),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)
bar + coord_polar()

# -------------------------------------------------------------
########################################################################
# ------------------ RIDGELINE PLOT ----------------------#
# 23
# Ridgeline plot
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)

# Plot
ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016') +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# ---------------------------------------------------------------
###########################################################################

# --------------- PLOT MATRIX -----------------------#
# 24
# overview of the variable
library(GGally)
mtcars$cyl = as.factor(mtcars$cyl)
ggpairs(mtcars,columns = 1:4,aes(color = cyl, alpha = 0.5))

# --------------------------------------------------------------
# 25
library(GGally)
data(flea)
ggpairs(flea, columns = 2:4, ggplot2::aes(colour=species)) 

# ------------------------------------------------------------------------
# 26
# correlation matrix with pie chart
library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main="Car Milage Data in PC2/PC1 Order") 


# ------------------------------------------------------------------------
# 27
# scatterplot with density plot at margins
library(ggpubr)
# Grouped Scatter plot with marginal density plots
ggscatterhist(
  iris, x = "Sepal.Length", y = "Sepal.Width",
  color = "Species", size = 3, alpha = 0.6,
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  margin.params = list(fill = "Species", color = "black", size = 0.2)
)

# ---------------------------------------------------------------------------
############################################################################
# --------------------- MULTIPLE LINE PLOT ----------------------- #
# 28
# multiple line plot
df <- economics %>%
  select(date, psavert, uempmed) %>%
  gather(key = "variable", value = "value", -date)
head(df, 3)

ggplot(df, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

# ------------------------------------------------------------------------

#############################################################################
# ------------------------ QQPLOT --------------------------------3
# 29
# qqplot
library(ggpubr)
ggqqplot(iris, x = "Sepal.Length",col = "red",
         ggtheme = theme_bw())



#######################################################
# ------------------- Background change ----------------#
# 30

df <- data.frame(x=c(1, 3, 3, 4, 5, 5, 6, 9, 12, 15),
                 y=c(13, 14, 14, 12, 17, 21, 22, 28, 30, 31))
p <- ggplot(df, aes(x=x, y=y)) +
  geom_point()
p + theme(panel.background = element_rect(fill = 'lightblue', color = 'purple'),
          panel.grid.major = element_line(color = 'red', linetype = 'dotted'),
          panel.grid.minor = element_line(color = 'green', size = 2))



















