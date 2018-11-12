library(ggplot2)
 
# Aesthetic  shape , size , x - axis, y - axis , alpha- tranpiracy, 

ggplot(diamonds, aes(x= carat , y = price, col = color, alpha = 0.01) )+geom_point()
ggplot(diamonds, aes(x= carat , y = price, col = color, alpha = 0.01 , size = 5))+geom_point()
ggplot(diamonds, aes(x= carat , y = price, fill = color ,alpha = 0.01 ))+geom_point()
ggplot(diamonds, aes(x= carat , y = price, alpha = 0.01 ))+geom_point(aes(col =color ))
ggplot(diamonds, aes(x= carat , y = price, alpha = 0.01 ))+geom_point(fill = color)

# the aes( ) is working the same way in ggplot and geom_ 
ggplot(diamonds, aes(x= carat , y = price, alpha = 0.001 ))+
  geom_point(aes(col = color))+ geom_smooth()
# span = 0 to 1 hogh to low 

ggplot(diamonds, aes(x= carat , y = price, alpha = 0.001 ))+
  geom_point(aes(col = color))

# Assignment question
ggplot(USArrests, aes(x = row.names(USArrests), y = USArrests$Murder, lab)) + geom_bar() 
+ theme(axis.text.x=element_text(angle=90, hjust=1))

ggplot(USArrests, aes(x = row.names(USArrests), y = USArrests$Murder, lab)) + geom_col() 
+ theme(axis.text.x=element_text(angle=90, hjust=1))

ggplot(USArrests, aes(x = row.names(USArrests), y = USArrests$Murder, lab)) + geom_bar() 
+ theme(axis.angle.x = 90)

ggplot(sleep, aes(x = ID, y = extra, fill = group)) + geom_col()

ggplot(diamonds, aes( x= carat))+ geom_histogram(binwidth = 0.1)


# box plots and jitter 
ggplot(diamonds, aes(x= color , y = price/carat) )+geom_boxplot()
ggplot(diamonds, aes(x= color , y = price/carat) )+geom_jitter()
ggplot(diamonds, aes(x= color , y = price/carat, alpha = 1/50) )+geom_jitter()
ggplot(diamonds, aes(x= color , y = price/carat, alpha = 1/5000) )+geom_jitter()

# Histogram and Desity 
ggplot(diamonds, aes(x= carat, col  = color) )+geom_histogram(binwidth = 1 )
ggplot(diamonds, aes(x= carat, fill  = color) )+geom_histogram()
ggplot(diamonds, aes(x= carat, col  = color) )+geom_density()
ggplot(diamonds, aes(x= carat, fill  = color, alpha = 0.01))+geom_density()

View(diamonds)
ggplot(diamonds, aes(x=carat ,y =price, col = cut ))+geom_point()

p <- ggplot(mtcars, aes(x = mpg, y = wt))
p+geom_point()
p+geom_point(col = "red")           # Setting the value
p+geom_point(aes(col = factor(cyl)))  # aes() to map the value to variable
p+geom_point(aes(col = factor(cyl))) + geom_line()
p+geom_point(aes(y = disp))     
# Overriding the values but showing wt only but scale is changed 

# to see the list of colours available - colors()
ggplot(economics,aes(economics$date, economics$unemploy))+geom_line( col = "red")

# library("nlme", lib.loc="C:/Program Files/R/R-3.5.0/library")
a<-(Oxboys) 
ggplot(a,aes(a$age,height, group = a$Subject))+geom_line()

ggplot(a,aes(a$age,height))+ geom_line()
geom_smooth(aes(group = Subject), method="lm", se = F)

ggplot(a,aes(a$age,height))+ geom_line()+
geom_smooth(aes(group = 1), method="lm", size = 2, se = F)

ggplot(midwest, aes(x=area, y=poptotal)) + geom_point(col = "red") + 
  geom_smooth(method="lm", se = FALSE, size = 2)+
coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))+   # zooms in 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")


ggplot(midwest, aes(x=area, y=poptotal)) + geom_point(aes(col = midwest$state)) + 
  geom_smooth(method="lm", se = FALSE, size = 2 , col = "firebrick")+
  coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))

library(RColorBrewer)    #we can see other set of colors.
head(brewer.pal.info, 10)

# All the basic formatting of labels and scale
ggplot(midwest, aes(x=area, y=poptotal)) + geom_point(aes(col = midwest$state)) + 
  geom_smooth(method="lm", se = FALSE, size = 2 , col = "firebrick")+
  coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))+
  scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = letters[1:11])+  # changing the labels
  scale_y_continuous(breaks=seq(0, 1000000, 200000))

# to reverse the scale - scale_x_reverse()















