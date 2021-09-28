# Pruebas simples que ayudaron en la creación del documento final
Input = ("
 Factor.A  Factor.B  Response
 a         x          0.79
 a         y          0.75
 b         x          0.84
 b         y          0.74
 c         x          0.77
 c         y          0.73
 d         x          0.78
 d         y          0.65
 e         x          0.80
 e         y          0.65
 f         x          0.91
 f         y          0.70
 g         x          0.87
 g         y          0.73
 h         x          0.87
 h         y          0.92
")

Data = read.table(textConnection(Input),header=TRUE)
pbad2way(Response ~ Factor.A + Factor.B,
         data = Data,
         est = "mom",    # modified M-estimator
         nboot = 50)

#Permutaciones
set.seed(1203)
response <- c ( rpois (12 ,4) , rpois (12 ,0.5) , rnorm (12 ,3 ,1) )
fact1 <- gl (3 ,12 , labels = LETTERS [1:3])
block <- gl (2 ,6 ,36 , labels = letters [1:2])
data.ej <- data.frame ( response , fact1 ,block )
View(data.ej)
perm.anova(response~fact1 | block , nperm =49)