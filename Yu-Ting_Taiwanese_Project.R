# Code to check the balance between the control and treatment
# Suppose the experiment column is named: expGSL
# Reading Data:
setwd("~/Desktop")
rm(list=ls())
require(data.table)
require(lfe)
require(AER)
require(ggplot2)
require(reldist)
require(grid)
rawdata <- data.table(fread("appended.csv", stringsAsFactors = FALSE));
#View(head(rawdata))
# Data cleaning
#nrow(subset(rawdata, work_lw == "full_time"))
#nrow(subset(rawdata, work_lw == "not in labor force"))
#View(head(rawdata[work_lw == "not in labor force"]))
#nrow(rawdata[work_lw == "not in labor force" & sex =="female"])
#nrow(rawdata[work_lw == "not in labor force" & sex =="male"])

#hist(rawdata[work_lw == "not in labor force"]$age)
#hist(rawdata[work_lw == "full_time"]$age)
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
Data <- subset(rawdata, work_lw == "full_time" & hour > 0 & wageMonthly > 0);

#View(head(Data))

# x: wage monthly, group by $5,000; y: head counts adjusted by weight

Year <- c(1981, 2000, 2016) - 1911;
wage_group <- c(0,1000*seq(1,150,1),Inf);
N_y <- length(Year)
N_w <- length(wage_group)-1;

wage_dist_wholesample <- array(N_y*N_w, dim = c(N_y,N_w));

for (t in 1:N_y){
  for (i in 1:N_w){
    wage_dist_wholesample[t,i] <- sum(Data[wage_group[i] < wageMonthly &
                                    wageMonthly <= wage_group[i+1] & year == Year[t],weight])
  }
}

plot(c(1,N_w),c(0,0.2),typ="n")
lines(1:N_w, wage_dist_wholesample[1,]/sum(wage_dist_wholesample[1,]),col = 1, type = "h")
lines(1:N_w,wage_dist_wholesample[2,]/sum(wage_dist_wholesample[2,]),col = 2, type = "h")
lines(1:N_w,wage_dist_wholesample[3,]/sum(wage_dist_wholesample[3,]),col = 3, type = "h")


# x: hourly wage, group by $5,000; y: head counts adjusted by weight

Year <- c(1981, 2000, 2016) - 1911;
wage_group <- c(0,20*seq(1,20,1),Inf);
N_y <- length(Year)
N_w <- length(wage_group)-1;

hourly_wage_dist_wholesample <- array(N_y*N_w, dim = c(N_y,N_w));

for (t in 1:N_y){
  for (i in 1:N_w){
    hourly_wage_dist_wholesample[t,i] <- sum(Data[wage_group[i] < wageMonthly/(hour*4.5) &
                                              wageMonthly/(hour*4.5) <= wage_group[i+1] & year == Year[t],weight]);
  }
}

plot(c(1,N_w),c(0,0.2),typ="n")
lines(1:N_w, hourly_wage_dist_wholesample[1,]/sum(hourly_wage_dist_wholesample[1,]),col = 1, type = "h")
lines(1:N_w,hourly_wage_dist_wholesample[2,]/sum(hourly_wage_dist_wholesample[2,]),col = 2, type = "h")
lines(1:N_w,hourly_wage_dist_wholesample[3,]/sum(hourly_wage_dist_wholesample[3,]),col = 3, type = "h")



# x: wage, group by $5,000; y: head counts adjusted by weight; z: edu

Year <- c(1981, 2000, 2016) - 1911;
wage_group <- c(0,10*seq(1,50,1),Inf);
edu_group <- list(c("國(初)中","專科(五專前三年劃記高職)","高中","小學","自修","高職","不識字"),c("大學以上","碩士或博士","大學","碩士", "博士" ))
N_y <- length(Year)
N_w <- length(wage_group)-1;
N_e <- length(edu_group);

hourly_wage_dist_by_edu<- array(N_y*N_w*N_e, dim = c(N_y,N_w,N_e));

for (e in 1:N_e){
  for (t in 1:N_y){
    for (i in 1:N_w){
      hourly_wage_dist_by_edu[t,i,e] <- sum(Data[edu %in% edu_group[[e]] & wage_group[i] < wageMonthly/(hour*4.5) &
                                                wageMonthly/(hour*4.5) <= wage_group[i+1] & year == Year[t],weight]);
    }
  }
}

plot(c(1,N_w),c(0,0.2),typ="n")
lines(1:N_w, hourly_wage_dist_by_edu[1,,1]/sum(hourly_wage_dist_by_edu[1,,1]),col = 1, type = "h")
lines(1:N_w,hourly_wage_dist_by_edu[2,,1]/sum(hourly_wage_dist_by_edu[2,,1]),col = 2, type = "h")
lines(1:N_w,hourly_wage_dist_by_edu[3,,1]/sum(hourly_wage_dist_by_edu[3,,1]),col = 3, type = "h")

plot(c(1,N_w),c(0,0.2),typ="n")
lines(1:N_w, hourly_wage_dist_by_edu[1,,2]/sum(hourly_wage_dist_by_edu[1,,2]),col = 1, type = "h")
lines(1:N_w,hourly_wage_dist_by_edu[2,,2]/sum(hourly_wage_dist_by_edu[2,,2]),col = 2, type = "h")
lines(1:N_w,hourly_wage_dist_by_edu[3,,2]/sum(hourly_wage_dist_by_edu[3,,2]),col = 3, type = "h")



# x: wage, group by $5,000; y: head counts adjusted by weight; z: num_emp

Year <- c(1981, 2000, 2016) - 1911;
wage_group <- c(0,10*seq(1,50,1),Inf);
firm_size_group <- unique(Data$num_emp);
N_y <- length(Year)
N_w <- length(wage_group)-1;
N_f <- length(firm_size_group);

hourly_wage_dist_by_firm_size<- array(N_y*N_w*N_f, dim = c(N_y,N_w,N_f));

for (f in 1:N_f){
  for (t in 1:N_y){
    for (i in 1:N_w){
      hourly_wage_dist_by_firm_size[t,i,f] <- sum(Data[num_emp == firm_size_group[f] & wage_group[i] < wageMonthly/(hour*4.5) &
                                                   wageMonthly/(hour*4.5) <= wage_group[i+1] & year == Year[t],weight]);
    }
  }
}

f = 2;
  plot(c(1,N_w),c(0,0.2),typ="n")
  lines(1:N_w, hourly_wage_dist_by_firm_size[1,,f]/sum(hourly_wage_dist_by_firm_size[1,,f]),col = 1, type = "h")
  lines(1:N_w,hourly_wage_dist_by_firm_size[2,,f]/sum(hourly_wage_dist_by_firm_size[2,,f]),col = 2, type = "h")
  lines(1:N_w,hourly_wage_dist_by_firm_size[3,,f]/sum(hourly_wage_dist_by_firm_size[3,,f]),col = 3, type = "h")

f = N_f;
  plot(c(1,N_w),c(0,0.2),typ="n")
  lines(1:N_w, hourly_wage_dist_by_firm_size[1,,f]/sum(hourly_wage_dist_by_firm_size[1,,f]),col = 1, type = "h")
  lines(1:N_w,hourly_wage_dist_by_firm_size[2,,f]/sum(hourly_wage_dist_by_firm_size[2,,f]),col = 2, type = "h")
  lines(1:N_w,hourly_wage_dist_by_firm_size[3,,f]/sum(hourly_wage_dist_by_firm_size[3,,f]),col = 3, type = "h")
  

  
###################################################################################################  
Data$year_factor <- factor(Data$year);
Data[,hourly_wage:= wageMonthly/(hour*4.5)]
Data[,weight_adj:= weight/sum(weight), by = year]
Year <- c(1981, 2000, 2016) - 1911;
top_wage <- wtd.quantile(Data$hourly_wage,0.99,weight = Data$weight)
bottom_wage <- wtd.quantile(Data$hourly_wage,0.01,weight = Data$weight)

p_whole_sample <- ggplot(subset(Data, year %in% Year & hourly_wage < top_wage & hourly_wage > bottom_wage), aes(x=hourly_wage, colour=year_factor, weight = weight_adj)) +
    geom_density() +
    ggtitle("hourly wage distribution")
plot(p_whole_sample)


###################################################################################################  

edu_group <- list(c("國(初)中","專科(五專前三年劃記高職)","高中","小學","自修","高職","不識字"),c("大學以上","碩士或博士","大學","碩士", "博士" ))
Data_edu_1<- subset(Data, year %in% Year & edu %in% edu_group[[1]] & hourly_wage < top_wage & hourly_wage > bottom_wage);
Data_edu_1[,weight_adj:=weight/sum(weight),by = year];
p_edu1 <- ggplot(Data_edu_1, aes(x=hourly_wage, colour=year_factor, weight = weight_adj)) +
  geom_density() +
  ggtitle("hourly wage distribution, by edu");

Data_edu_2<- subset(Data, year %in% Year & edu %in% edu_group[[2]] & hourly_wage < top_wage & hourly_wage > bottom_wage);
Data_edu_2[,weight_adj:=weight/sum(weight),by = year];
p_edu2 <- ggplot(Data_edu_2, aes(x=hourly_wage, colour=year_factor, weight = weight_adj)) +
  geom_density() +
  ggtitle("hourly wage distribution, by edu");

multiplot(p_edu1, p_edu2, cols=2)

###################################################################################################  

firm_size_group <- unique(Data$num_emp);

Data_firm_2<- subset(Data, year %in% Year & num_emp %in% firm_size_group[2] & hourly_wage < top_wage & hourly_wage > bottom_wage);
Data_firm_2[,weight_adj:=weight/sum(weight),by = year];
p_firmsize2 <- ggplot(Data_firm_2, aes(x=hourly_wage, colour=year_factor, weight = weight_adj)) +
  geom_density() +
  ggtitle("hourly wage distribution, by firm size")

Data_firm_5<- subset(Data, year %in% Year & num_emp %in% firm_size_group[5] & hourly_wage < top_wage & hourly_wage > bottom_wage);
Data_firm_5[,weight_adj:=weight/sum(weight),by = year];
p_firmsize5 <- ggplot(Data_firm_5, aes(x=hourly_wage, colour=year_factor, weight = weight_adj)) +
  geom_density() +
  ggtitle("hourly wage distribution, by firm size")

Data_firm_9<- subset(Data, year %in% Year & num_emp %in% firm_size_group[9] & hourly_wage < top_wage & hourly_wage > bottom_wage);
Data_firm_9[,weight_adj:=weight/sum(weight),by = year];
p_firmsize9 <- ggplot(Data_firm_9, aes(x=hourly_wage, colour=year_factor, weight = weight_adj)) +
  geom_density() +
  ggtitle("hourly wage distribution, by firm size")

multiplot(p_firmsize2, p_firmsize5, p_firmsize9, cols=3)

###################################################################################################  
