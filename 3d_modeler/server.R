library(devtools)
library(shiny)
library(shinyRGL)
library(reshape2)
library(rgl)


############# CODE FOR THE IRIS DATA ################################
data(iris)

dfiris <- data.frame(Sepal.Length = iris$Sepal.Length,Sepal.Width = iris$Sepal.Width, 
                     Petal.Area = iris$Petal.Length*iris$Petal.Width )

#Create the regression model for the data
irisfit <- lm(Petal.Area~Sepal.Length+Sepal.Width,dfiris)

#Save the predicted values to the dataframe
dfiris$pred <- predict(irisfit)

#Create a matrix to represent the heights for the surface
#I will need a vector for both the X and Y axes that covers many points
Sepal.Length <- seq(min(dfiris$Sepal.Length),max(dfiris$Sepal.Length),len=50)
Sepal.Width <- seq(min(dfiris$Sepal.Width),max(dfiris$Sepal.Width),len=50)
#Next I make every combination that can be made with the X,Y coords
plot.dfiris <- expand.grid(Sepal.Length = Sepal.Length,Sepal.Width = Sepal.Width)
#I need a vector to represent the heights of the predictions
plot.dfiris$Petal.Area.Pred <- predict(irisfit,newdata=plot.dfiris)
#I need to rework the data frame so that I can plot it
#The dcast method will create the first column to be the Sepal.Lengths 
#and the other columns will be each width. Then the inside is the combinations
irisheight <- dcast(plot.dfiris,Sepal.Length~Sepal.Width,value.var="Petal.Area.Pred")[-1]

############# CODE FOR THE IRIS INTERACTION DATA ################################


data(iris)

dfIrisInt <- iris
dfIrisInt $ Petal.Area = dfIrisInt $ Petal.Width * dfIrisInt $ Petal.Length

fitIrisInt <- lm(Petal.Area~Sepal.Length*Sepal.Width,dfIrisInt)

dfIrisInt$pred <- predict(fit)

Sepal.Length.IrisInt <- seq(min(dfIrisInt$Sepal.Length),max(dfIrisInt$Sepal.Length),len=50)
Sepal.Width.IrisInt <- seq(min(dfIrisInt$Sepal.Width),max(dfIrisInt$Sepal.Width),len=50)
plot.dfIrisInt <- expand.grid(Sepal.Length = Sepal.Length.IrisInt,Sepal.Width = Sepal.Width.IrisInt)
plot.dfIrisInt$Petal.Area.Pred <- predict(fitIrisInt,newdata=plot.dfIrisInt)
heightIrisInt <- dcast(plot.dfIrisInt,Sepal.Length~Sepal.Width,value.var="Petal.Area.Pred")[-1]


############# CODE FOR THE IRIS CATEGORICAL DATA ################################
#Combin all the data into a data frame
data(iris)

dfIrisCat <- iris
dfIrisCat $ Petal.Area = dfIrisCat $ Petal.Width * dfIrisCat $ Petal.Length

colorsIrisCat = array(dim =length(iris$Species))

k = 1
choice = c("red","blue","darkgreen")
for (i in c("setosa","versicolor","virginica"))
{
  ind = which(iris$Species==i)
  for (j in ind) 
  {
    colorsIrisCat[j]=choice[k]
  }
  k=k+1
}

fitIrisCat <- lm(Petal.Area ~ Sepal.Width + Sepal.Length + Species, data=dfIrisCat)

dfIrisCat$pred <- predict(fitIrisCat)

Sepal.Length.IrisCat <- seq(min(dfIrisCat$Sepal.Length),max(dfIrisCat$Sepal.Length),len=50)
Sepal.Width.IrisCat <- seq(min(dfIrisCat$Sepal.Width),max(dfIrisCat$Sepal.Width),len=50)

plot.dfIrisCat1 <- expand.grid(Sepal.Length = Sepal.Length.IrisCat,Sepal.Width = Sepal.Width.IrisCat,Species = "setosa")
plot.dfIrisCat1$Petal.Area.Pred <- predict(fitIrisCat,newdata=plot.dfIrisCat1)
heightIrisCat1 <- dcast(plot.dfIrisCat1,Sepal.Length~Sepal.Width,value.var="Petal.Area.Pred")[-1]

plot.dfIrisCat2 <- expand.grid(Sepal.Length = Sepal.Length.IrisCat,Sepal.Width = Sepal.Width.IrisCat,Species = "versicolor")
plot.dfIrisCat2$Petal.Area.Pred <- predict(fitIrisCat,newdata=plot.dfIrisCat2)
heightIrisCat2 <- dcast(plot.dfIrisCat2,Sepal.Length~Sepal.Width,value.var="Petal.Area.Pred")[-1]

plot.dfIrisCat3 <- expand.grid(Sepal.Length = Sepal.Length.IrisCat,Sepal.Width = Sepal.Width.IrisCat,Species = "virginica")
plot.dfIrisCat3$Petal.Area.Pred <- predict(fitIrisCat,newdata=plot.dfIrisCat3)
heightIrisCat3 <- dcast(plot.dfIrisCat3,Sepal.Length~Sepal.Width,value.var="Petal.Area.Pred")[-1]


##########CODE FOR THE MTCARS DATA AND MODEL####################
data(mtcars)

dfcars<- data.frame(mpg = mtcars$mpg,hp = mtcars$hp, wt = mtcars$wt)

carsfit <- lm(mpg~hp+wt,dfcars)

dfcars$pred <- predict(carsfit)

hp <- seq(min(dfcars$hp),max(dfcars$hp),len=50)
wt <- seq(min(dfcars$wt),max(dfcars$wt),len=50)

plot.dfcars <- expand.grid(hp = hp,wt = wt)

plot.dfcars$mpgcars.pred <- predict(carsfit,newdata=plot.dfcars)

carsheight <- dcast(plot.dfcars,hp~wt,value.var="mpgcars.pred")[-1]

#############CODE FOT THE STATEX.77 DATA#########

theData <- as.data.frame(state.x77)
names(theData)[4] = "LifeExp"
names(theData)[6] = "HSGrad"


dfstate<- data.frame(LifeExp = theData$LifeExp, Murder = theData$Murder, HSGrad = theData$HSGrad)

statefit <- lm(LifeExp ~ Murder + HSGrad, data = dfstate)

dfstate$pred <- predict(statefit)

Murder <- seq(min(dfstate$Murder),max(dfstate$Murder),len=50)
HSGrad <- seq(min(dfstate$HSGrad),max(dfstate$HSGrad),len=50)

plot.dfstate <- expand.grid(Murder = Murder,HSGrad = HSGrad)

plot.dfstate$LifeExpState.pred <- predict(statefit,newdata=plot.dfstate)

stateheight <- dcast(plot.dfstate,Murder~HSGrad,value.var="LifeExpState.pred")[-1]


shinyServer(function(input, output){
  
  
  output$troisPlot <- renderWebGL({

    
    ###########CODE TO CREATE SURFACES AND PLOT#########################
#     if (input$surfaceOrNah == TRUE){
      if (input$dataset == "iris")
      {
        if (input$expTypes1 == 4){
          par3d(scale=c(1,1,0.2),cex=.5)
          points3d(dfiris$Sepal.Length,dfiris$Sepal.Width,dfiris$Petal.Area)
          axes3d()
          title3d(xlab="Sepal Length",ylab="Sepal Width",zlab="Petal Area")
          #Plot a surface and add axes...
        }else if (input$expTypes1 == 1){
          par3d(scale=c(1,1,0.2),cex=.5)
          points3d(dfiris$Sepal.Length,dfiris$Sepal.Width,dfiris$Petal.Area)
          surface3d(Sepal.Length,Sepal.Width,as.matrix(irisheight),col="blue",alpha=.2)
          axes3d()
          title3d(xlab="Sepal Length",ylab="Sepal Width",zlab="Petal Area")
        }else if (input$expTypes1 == 2){
          par3d(scale=c(1,1,0.2),cex=.5)
          points3d(dfIrisInt$Sepal.Length,dfIrisInt$Sepal.Width,dfIrisInt$Petal.Area)
          surface3d(Sepal.Length.IrisInt,Sepal.Width.IrisInt,as.matrix(heightIrisInt),col="blue",alpha=.2)
          axes3d()
          title3d(xlab="Sepal Length",ylab="Sepal Width",zlab="Sepal Area")
        }else{
          par3d(scale=c(1,1,0.2),cex=.5)
          points3d(df$Sepal.Length,df$Sepal.Width,df$Petal.Area,col=colors)
          surface3d(Sepal.Length.IrisCat,Sepal.Width.IrisCat,as.matrix(heightIrisCat1),col="blue",alpha=.2)
          surface3d(Sepal.Length.IrisCat,Sepal.Width.IrisCat,as.matrix(heightIrisCat2),col="blue",alpha=.2)
          surface3d(Sepal.Length.IrisCat,Sepal.Width.IrisCat,as.matrix(heightIrisCat3),col="blue",alpha=.2)
          axes3d()
          title3d(xlab="Sepal Length",ylab="Sepal Width",zlab="Petal Area")
        }

      }else if (input$dataset == "mtcars"){
        par3d(scale=c(0.02,1,0.2),cex=.5)
        points3d(dfcars$hp,dfcars$wt,dfcars$mpg)
        surface3d(hp,wt,as.matrix(carsheight),col="blue",alpha=.2)
        axes3d()
        title3d(xlab="Gross Horsepower",ylab="Weight (lb/1000)",zlab="Miles / (US) Gallon")
      } else {
        par3d(scale=c(1,.5,2),cex=.5)
        points3d(dfstate$Murder,dfstate$HSGrad,dfstate$LifeExp)
        surface3d(Murder,HSGrad,as.matrix(stateheight),col="blue",alpha=.2)
        axes3d()
        title3d(xlab="Murders per 100,000",ylab="Precent High-School Graduates",zlab="Life Expectancy")
      }

#     }else{
#       if (input$dataset == "iris"){
#         #Plot a surface and add axes...
#         par3d(scale=c(1,1,0.2),cex=.5)
#         points3d(dfiris$Sepal.Length,dfiris$Sepal.Width,dfiris$Petal.Area)
#         axes3d()
#         title3d(xlab="Sepal Length",ylab="Sepal Width",zlab="Petal Area")
#       }else if (input$dataset == "mtcars"){
#         par3d(scale=c(0.02,1,0.2),cex=.5)
#         points3d(dfcars$hp,dfcars$wt,dfcars$mpg)
#         axes3d()
#         title3d(xlab="Gross Horsepower",ylab="Weight (lb/1000)",zlab="Miles / (US) Gallon")
#       }else {
#         par3d(scale=c(1,.5,2),cex=.5)
#         points3d(dfstate$Murder,dfstate$HSGrad,dfstate$LifeExp)
#         axes3d()
#         title3d(xlab="Murders per 100,000",ylab="Precent High-School Graduates",zlab="Life Expectancy")
#       }

#     }

    
  })
  
  output$responseVar <- renderPrint({
    if (input$dataset == "iris"){
      paste("Petal Area")
    }else if (input$dataset == "mtcars"){
      paste("Miles Per Gallon")
    }else {
      paste("Life Expectancy")
    }    
  })
  
  output$modelEQ <- renderPrint({
    if (input$dataset == "iris"){
      summary(irisfit)$coefficients
    }else if (input$dataset == "mtcars"){
      summary(carsfit)$coefficients
    }else {
      summary(statefit)$coefficients
    }
  })
  
  
  output$modelRsq <- renderText({
    if (input$dataset == "iris"){
      summary(irisfit)$adj.r.squared
    }else if (input$dataset == "mtcars"){
      summary(carsfit)$adj.r.squared
    }else {
      summary(statefit)$adj.r.squared 
    }
  })

})