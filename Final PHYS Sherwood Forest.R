
NumberOfArrows <- 200
MontecarloMethod <- function(NumberOfArrows) {
  
  # Part One: Creating Sherwood Forest
  NumberOfTrees <- 5000
  
  xtree <- runif(NumberOfTrees, min=-500, max=500)
  ytree <- runif(NumberOfTrees, min=-500, max=500)

  
  # Part Two: Shooting the Arrows
  
  theta <- runif(NumberOfArrows, min=0,max=2*pi)
  #I learned that R works in radians, so I had to adjust from original pseudocode
  
  xarrow <- 10*cos(theta)
  yarrow <- 10*sin(theta)
  SlopeOfArrow <- yarrow/xarrow
  
  # Part Four: Finding the Distance Between Each Tree and Arrow
  DistanceFromMatrix <- matrix(nrow=NumberOfArrows,ncol=NumberOfTrees)
  for(i in 1:NumberOfArrows){
    for(j in 1:NumberOfTrees){
      DistanceFromMatrix[i,j]= abs(((SlopeOfArrow[i])*(xtree[j]) - ytree[j])/((SlopeOfArrow[i]*SlopeOfArrow[i])+1)**(1/2))
    }} 
  
  # Part Five: Determine if Hit or Miss
  BooleanDistanceFromMatrix <- matrix(nrow=NumberOfArrows,ncol=NumberOfTrees)
  
  for(i in 1:NumberOfArrows){
    for(j in 1:NumberOfTrees){
      if(DistanceFromMatrix[i,j]< 1){
        BooleanDistanceFromMatrix[i,j]= 1}
      else {
        BooleanDistanceFromMatrix[i,j]= 0}
    }}
  
  
  #Part Six: Determining Distance Arrow Traveled
  
  DistanceTravelledMatrix <- matrix(nrow=NumberOfArrows,ncol=NumberOfTrees)
  for(i in 1:NumberOfArrows){
    for(j in 1:NumberOfTrees){
      if(theta[i] < pi/2 & theta[i] > 0 & xtree[j]>0 & ytree[j]>0){DistanceTravelledMatrix[i,j] = ((xtree[j])**2 + (ytree[j])**2)**(1/2)}
      if(theta[i] < pi & theta[i] > pi/2 & xtree[j]<0 & ytree[j]>0){DistanceTravelledMatrix[i,j] = ((xtree[j])**2 + (ytree[j])**2)**(1/2)}
      if(theta[i] < 3*pi/2 & theta[i] > pi & xtree[j]<0 & ytree[j]<0){DistanceTravelledMatrix[i,j] = ((xtree[j])**2 + (ytree[j])**2)**(1/2)}
      if(theta[i] < 2*pi & theta[i] > 3*pi/2 & xtree[j]>0 & ytree[j]<0){DistanceTravelledMatrix[i,j] = ((xtree[j])**2 + (ytree[j])**2)**(1/2)}
      else {DistanceTravelledMatrix[i,j] = 100000}
    }}
  
  
  #Part Seven: Eliminate Duplicates
  
  DummyMatrix <- matrix(nrow=NumberOfArrows,ncol=NumberOfTrees)
  
  for(i in 1:NumberOfArrows){
    for(j in 1:NumberOfTrees){
      if(BooleanDistanceFromMatrix[i,j] == 1){DummyMatrix[i,j]= DistanceTravelledMatrix[i,j]}
      else {DummyMatrix[i,j]= 100000}
    }}
  
  DummyDataFrame <- as.data.frame(DummyMatrix)
  CondensedDistanceDataFrame <- apply(DummyDataFrame, 1, FUN=min)
  
  CondensedDistanceMatrix <- data.matrix(CondensedDistanceDataFrame, rownames.force = NA)
  
  #Part Eight: Eliminate Escaped Arrows and Calculate the Mean
  FinalMean <- mean(CondensedDistanceDataFrame[CondensedDistanceDataFrame < 10000])
  return(FinalMean)
} 

NumberOfForests <- 50

FinalMeanMatrix <- matrix(nrow=NumberOfForests,ncol=1)

for (i in 1:NumberOfForests) {
  FinalMeanMatrix[i] = MontecarloMethod(NumberOfArrows)
}
#The above allows me to run multiple forests through the loop 

FinalMeanDataFrame <- as.data.frame(FinalMeanMatrix)
View(FinalMeanDataFrame)

lapply(FinalMeanDataFrame, mean, na.rm = TRUE)


