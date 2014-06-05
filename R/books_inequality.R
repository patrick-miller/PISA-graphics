library("dplyr")
library("ggplot2")
library("gbm")
library("reshape2")
library("grid")
library("gridExtra")

#Set BaseDir
BaseDir <- "/media/pwmiller/store/Projects/R/PISA/data"

# 
# Download the student2012.rda and the school2012.rda into the BaseDir
#  
# http://beta.icm.edu.pl/PISAcontest/data/school2012.rda
# http://beta.icm.edu.pl/PISAcontest/data/student2012.rda
#

BooksGraphic <- function(){
  
  FileNames <- c("student2012") #, "student2012dict")
  
  for(fn in FileNames){
    load(paste0(BaseDir, "/", fn, ".rda"))
  }
  
  keepColumns <- c(   
    "CNT",        # "Country code 3-character"
    "ST28Q01",    # "How many books at home"
    "ST13Q01",    # "Mother<Highest Schooling>"
    "ST17Q01",    # "Father<Highest Schooling>"
    
    "PV1MATH", 
    "PV1READ", 
    "PV1SCIE")
  
  student2012 <- student2012[, keepColumns]
  
  student2012$AvgScore <- (student2012$PV1MATH + student2012$PV1READ + student2012$PV1SCIE) / 3
  
  student2012$Books <- "> 25 books"
  student2012$Books[is.element(as.character(student2012$ST28Q01), c("0-10 books ", "11-25 books "))] <-
    "<= 25 books"
  
  theme_pisa <- theme_grey() + theme(panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color="darkgray", linetype="dashed", size=.3),
    panel.background = element_rect(color="black"),
    plot.title = element_text(size = 20, vjust=2),
    axis.title = element_text(vjust=-.1, size = 12),
    axis.title.x = element_text(vjust=-.11), axis.title.y = element_text(angle=90),
    axis.text = element_text(size=11, color="black"), plot.margin = unit(c(1,1,1,1), "cm"),
    legend.position = "bottom", legend.background = element_rect(color="black"),
    legend.title = element_blank(), legend.text = element_text(size = 11),
    strip.background = element_rect(color="black", fill="slategray1"), 
    strip.text = element_text(size = 12))
  
  booksPlot <- ggplot(data=student2012, aes(x=AvgScore, y = ..density..)) +
    
    geom_histogram(data=student2012[student2012$Books=="<= 25 books", ],
                   aes(fill="navy"), color="darkgrey", alpha=0.4) +
    geom_histogram(data=student2012[student2012$Books=="> 25 books", ],
                   aes(fill="orange"), color="darkgrey", alpha=0.4) +
    
    labs(x="Student score (overall)", y=NULL, 
         title="Students with more books at home score better on tests") +
    
    scale_fill_identity(name="", guide="legend",labels = c("<= 25 books", "> 25 books")) +
    
    theme_pisa + theme(
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank())
  
  return(booksPlot)
  
}

#
# Create books inequality .png
#
png(paste0(BaseDir, "/BooksInequality.png"), width=900, height=900)
BooksGraphic()
dev.off()