library(ggtern)

NBA<-read.csv("NBA_train.csv")

# show dataset
table(NBA$Playoffs,NBA$W)

# show Team、Playoffs、W three variables about the relationship   
ggplot(NBA, aes(x = W,y = Team , colour = Playoffs)) +geom_point()

# Add a list of variables
NBA$PTSdiff<-NBA$PTS-NBA$oppPTS  
#Look at the difference in points versus the number of courts won
plot(NBA$PTSdiff,NBA$W)  


#Establish regression model
WinsReg <-lm(W~PTSdiff,data = NBA)
summary(WinsReg)


PointsReg<-lm(PTS~X2PA+X3PA+FTA+ORB+AST+STL,data = NBA)
summary(PointsReg)

# Test
NBA_test<-read.csv("NBA_test.csv")
str(NBA_test)

# predict test
PointsPredictions<-predict(PointsReg,newdata = NBA_test)
PointsPredictions

SSE<-sum((NBA_test$PTS-PointsPredictions)^2)
SST<-sum((NBA_test$PTS-mean(NBA$PTS))^2)
R2<-1-SSE/SST
R2