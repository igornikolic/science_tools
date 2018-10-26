#by Igor Nikolic
#Inspired by Bloom-plots by Benjamin Purzycki

#Bloomy is meant to plot dat that are created by the AnthroTools library. For this example, we are not loading them, but
#use a example data frame created by it  
#library(AnthroTools)

library("plotrix")

#data input
df = read.csv("example.csv", sep=",",skip = 0, head=TRUE) 

#We assume that the entire data frame is to be plotted, and that the last petal is in the "other" category
# In the AnthroTool language
# CODE is the petalNames
# SmithsS is the weights
# Num bold, the number of petals, starting with the first, to make bold

#define the bloomy function
bloomy = function(plotName, petalNames, weights, numBold){
  numBalls = nrow(df)
  radiusCenter = 80   #Adjust if needed to fit the text
  radiusLeafBall = 80 #Adjust if needed to fit the text
  
  #The balls needd to be separated enough so that they  do not overlap
  #total length of all radiuses of leaf balls added together, add some 20% more to make space, 
  #and then find radius of the cirle that makes that circumference
  bigCircumference = (2*radiusLeafBall)*numBalls*1.2
  lengthStalks = bigCircumference / (pi * 2)
  
  #distribute them uniformly
  degreeStep = 360 / numBalls

  #we need to change the size of the plot, as we are changing the length of the stalks 
  plotDim = radiusCenter + 2* radiusLeafBall+ lengthStalks
  
  #make the center
  plot(c(-plotDim, plotDim), c(-plotDim, plotDim), type = "n", xlab = "", ylab = "", axes=FALSE, asp=1, family = "Calibri")
  draw.circle(0,0,radiusCenter)
  text(0,0, label = plotName)

  #lets make the petals, one by one
    for(i in 0:(numBalls-1)){             # we do not want to end back at the beginning, but one step before, so -1.
    #detrmine angle ( sin/cosin dont work on degrees)
    angle = i*degreeStep*(pi/180)

   #we first draw segments
    x0 = radiusCenter*sin(angle)
    x1 = (radiusCenter+lengthStalks)*sin(angle)
    y0 = radiusCenter* cos(angle)
    y1 = (radiusCenter+lengthStalks)*cos(angle)

    if (i < numBold) {
      segments(x0, y0, x1, y1, lwd = 2)}                   #the first numBold are bold
    else  if( i == (numBalls-1) ){                         #if this is the last ball, make it dashed
      segments(x0, y0, x1, y1, lwd = 1, lty = 3)
    }
      
    else {
      segments(x0, y0, x1, y1, lwd = 1)
    }
    
    #now the text on the lines
    x4 = (radiusCenter+lengthStalks/2)*sin(angle+degreeStep*(pi/180)*0.2) #lets add 20% of the angle  to offset the tetxt. We do not use the angle variable, as it is 0 at the first step.
    y4 = (radiusCenter+lengthStalks/2)*cos(angle+degreeStep*(pi/180)*0.2)
    text(x4,y4, labels=round(weights[i+1],digits=2), font = 1, cex = 0.8) #cex is the text point size, make them smaller than label text

    
   #now add the balls
    x3 = (radiusCenter+lengthStalks+radiusLeafBall)*sin(angle)
    y3 = (radiusCenter+lengthStalks+radiusLeafBall)*cos(angle)
    if (i < numBold) {
      draw.circle(x3,y3,radiusLeafBall, lwd = 2)
      
    }  
    else if( i == (numBalls-1) ){                         #if this is the last ball, make it dashed
    draw.circle(x3,y3,radiusLeafBall, lty=3 )
      } 
    else {
      draw.circle(x3,y3,radiusLeafBall)
      }
    
    #now the text in the balls
    text(x3,y3, labels=petalNames[i+1], font = 1, cex=0.9)
  
    }

  #save the file
  dev.copy(png,filename = paste(plotName,"_bloomy.png", sep=""));
  dev.off()
}

#lets make a plot, with the CODE, SmithsS and 3 bold petals
bloomy("test plot",df$CODE, df$SmithsS,  3)

