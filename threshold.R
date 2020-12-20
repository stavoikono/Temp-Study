plot_peak_count <- function(Code_order,threshold,data){
  x <- data[which(data$Code_order == Code_order),]
  x$Index1<-1:nrow(x)
  loessMod5 <- loess(x$Temperature~Index1, data=x, span=0.1) # 10% smoothing span
  x$smoothed5 <- predict(loessMod5) 
  
  pp <- findpeaks (x$smoothed5, nups = 1, ndowns = 0, minpeakheight = 0, 
                   minpeakdistance = 1, threshold = threshold, npeaks = 0, sortstr = FALSE)
  data.peaks<-data.frame(data.smoothed5=pp)
  z<<-nrow(data.peaks)
  return(z)
}

#########################################################################

plot_peak_count("T16-S1", 0.2,data_air)


co_listA <- c("T10-A1","T11-A1","T12-A1","T13-A1","T14-A1","T15-A1","T16-A1","T17-A1","T18-A1","T19-A1",
              "T21-A1","T22-A1","T23-A1","T24-A1","T26-A1","T29-A1","T31-A1","T33-A1","T34-A1",
              "T35-A1","T37-A1","T38-A1","T46-A1","T47-A1","T51-A1","T52-A1",
              "T53-A1","T56-A1","T60-A1","T61-A1","T64-A1","T66-A1","T67-A1",
              "T10-A2","T11-A2","T12-A2","T16-A2","T17-A2","T18-A2","T19-A2","T24-A2","T26-A2",
              "T35-A2","T37-A2","T38-A2","T42-A2","T43-A2","T45-A2","T60-A2","T42-A1","T54-A1","T45-A1")

co_listS <- c("T10-S1","T11-S1","T12-S1","T13-S1","T14-S1","T15-S1","T16-S1","T18-S1","T19-S1","T21-S1","T22-S1",
              "T23-S1","T24-S1","T26-S1","T29-S1","T31-S1","T33-S1","T34-S1","T35-S1","T37-S1",
              "T38-S1","T45-S1","T46-S1","T47-S1","T51-S1","T52-S1","T53-S1",
              "T56-S1","T60-S1","T61-S1","T64-S1","T66-S1","T67-S1",
              "T10-S2","T11-S2","T12-S2","T16-S2","T17-S2","T24-S2","T26-S2","T29-S2",
              "T31-S2","T33-S2","T34-S2","T35-S2","T38-S2","T43-S2","T45-S2","T60-S2")

########################### AIR #############################

a01 <- vector()
for(i in co_listA){
  k <- plot_peak_count(i,0.1,data_air)
  a01 <- append(a01,k)
}

a02 <- vector()
for(i in co_listA){
  k <- plot_peak_count(i,0.2,data_air)
  a02 <- append(a02,k)
}

a03 <- vector()
for(i in co_listA){
  k <- plot_peak_count(i,0.3,data_air)
  a03 <- append(a03,k)
}

a04 <- vector()
for(i in co_listA){
  k <- plot_peak_count(i,0.4,data_air)
  a04 <- append(a04,k)
}

a05 <- vector()
for(i in co_listA){
  k <- plot_peak_count(i,0.5,data_air)
  a05 <- append(a05,k)
}


a06 <- vector()
for(i in co_listA){
  k <- plot_peak_count(i,0.6,data_air)
  a06 <- append(a06,k)
}

a07 <- vector()
for(i in co_listA){
  k <- plot_peak_count(i,0.7,data_air)
  a07 <- append(a07,k)
}

thrs_A <- rbind.data.frame(a01,a02,a03,a04,a05,a06,a07)
names(tnumthr) <- co_listA


apply(thrs_A,1, median)



############################# SKIN ###########################

t01 <- vector()
for(i in co_listS){
  k <- plot_peak_count(i,0.1,data_skin)
  t01 <- append(t01,k)
}

t02 <- vector()
for(i in co_listS){
  k <- plot_peak_count(i,0.2,data_skin)
  t02 <- append(t02,k)
}

t03 <- vector()
for(i in co_listS){
  k <- plot_peak_count(i,0.3,data_skin)
  t03 <- append(t03,k)
}

t04 <- vector()
for(i in co_listS){
  k <- plot_peak_count(i,0.4,data_skin)
  t04 <- append(t04,k)
}

t05 <- vector()
for(i in co_listS){
  k <- plot_peak_count(i,0.5,data_skin)
  t05 <- append(t05,k)
}


t06 <- vector()
for(i in co_listS){
  k <- plot_peak_count(i,0.6,data_skin)
  t06 <- append(t06,k)
}

t07 <- vector()
for(i in co_listS){
  k <- plot_peak_count(i,0.7,data_skin)
  t07 <- append(t07,k)
}

thrs_S <- rbind.data.frame(t01,t02,t03,t04,t05,t06,t07)
names(thrs_S) <- co_listS


apply(tnumthr,1, median)
