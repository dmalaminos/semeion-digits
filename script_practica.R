##
## Lectura y preparación de los datos
##

semeion <- read.table("semeion.data", quote="\"")

semeion$digit <- NA
semeion$digit[which(semeion$V257 == 1)] <- 0
semeion$digit[which(semeion$V258 == 1)] <- 1
semeion$digit[which(semeion$V259 == 1)] <- 2
semeion$digit[which(semeion$V260 == 1)] <- 3
semeion$digit[which(semeion$V261 == 1)] <- 4
semeion$digit[which(semeion$V262 == 1)] <- 5
semeion$digit[which(semeion$V263 == 1)] <- 6
semeion$digit[which(semeion$V264 == 1)] <- 7
semeion$digit[which(semeion$V265 == 1)] <- 8
semeion$digit[which(semeion$V266 == 1)] <- 9

semeion <- semeion[,-c(257,258,259,260,261,262,263,264,265,266)]
semeion$digit <- factor(semeion$digit)

semeion <- semeion[sample(nrow(semeion)),] #desordenamos filas


##
## Feature extraction
##

# Number of black pixels in the window
pixNegros.digit <- function (data.digit) {
    (sum(data.digit==1))
}

pixNegros.all <- function (data.all) {
    (apply(data.all,1,pixNegros.digit))
}


# Pixel density between upper and lower contour
upper.lower.density.digit <- function (data.digit) {
    upper <- data.digit[1:127]
    lower <- data.digit[128:256]
    (pixNegros.digit(upper)/pixNegros.digit(lower))
}

upper.lower.density.all <- function (data.all) {
    (apply(data.all,1,upper.lower.density.digit))
}


# Pixel density between left and right contour
right.left.density.digit <- function (data.digit) {
    indexs <- c(1:8,17:24,33:40,49:56,65:72,81:88,97:104,113:120,129:136,
                145:152,161:168,177:184,193:200,209:216,225:232,241:248)
    left <- data.digit[indexs]
    right <- data.digit[-indexs]
    (pixNegros.digit(left)/pixNegros.digit(right))
}

right.left.density.all <- function (data.all) {
    (apply(data.all,1,right.left.density.digit))
}


# Maximum vertical transitions
vert.transitions.digit.col <- function (data.digit.col) {
    digit <- data.digit.col[1]
    i = 2
    max = 0
    (data.digit.col[1] == 0)
    while (i<=16){
        if (data.digit.col[i] > 0 && digit==0) { max=max+1; digit <- 1; }
        else if (data.digit.col[i] == 0 && digit==1)  { max=max+1; digit <- 0 }
        i=i+1
    }
    (max)
}

vert.transitions.digit <- function (data.digit) {
    r <- t(matrix(data.digit, nrow = 16, ncol = 16))
    num <- apply(r,2,vert.transitions.digit.col)
    (max(num))
}

vert.transitions.all <- function (data.all) {
    (apply(data.all,1,vert.transitions.digit))
}


# Number of cycles
num.cycles.digit <- function(data.digit) {
    digMatrix <- paste(data.digit, sep = " ", collapse = " ")
    cmd <- paste("echo",digMatrix,"| ./cycles") #no funcionará en Windows
    res <- system(cmd, intern=TRUE)
    as.numeric(res)
}

num.cycles.all <- function(data.all) {
    (apply(data.all,1,num.cycles.digit))
}


# Marginal sums
marginal.sums.digit <- function(data.digit) {
    r <- t(apply(matrix(data.digit, nrow = 16, ncol = 16), c(1,2), as.numeric))
    c(colSums(r), rowSums(r))
}

marginal.sums.all <- function(data.all) {
    t(apply(data.all,1,marginal.sums.digit))
}


# Number of edges
num.edges.digit <- function(data.digit) {
    digMatrix <- paste(data.digit, sep = " ", collapse = " ")
    cmd <- paste("echo",digMatrix,"| ./edges") #no funcionará en Windows
    res <- system(cmd, intern=TRUE)
    as.numeric(res)
}

num.edges.all <- function(data.all) {
    (apply(data.all,1,num.edges.digit))
}



##
## Obtención de los conjuntos de training/test
##

N <- nrow(semeion)
learn <- sample(1:N, round(N/2))
nlearn <- length(learn)
ntest <- N - nlearn
semeion.train <- semeion[learn,]
semeion.test <- semeion[-learn,]

#Ahora extraemos las nuevas variables
px.n <- pixNegros.all(semeion[,-257])
ul.d <- upper.lower.density.all(semeion[,-257])
rl.d <- right.left.density.all(semeion[,-257])
tr.v <- vert.transitions.all(semeion[,-257])
n.cy <- num.cycles.all(semeion[,-257])
mg.s <- marginal.sums.all(semeion[,-257])
n.ed <- num.edges.all(semeion[,-257])

features <- data.frame(n.ed, mg.s, px.n, ul.d, rl.d, tr.v, n.cy, matrix=semeion[,-257], digit = semeion$digit)

semeion.train.feat <- features[learn,]
semeion.test.feat <- features[-learn,]

library(caret)    	#para hacer 5x2CV con train, trainControl



##
## Linear Discriminant Analysis
##

library(MASS)       #para lda

trc <- trainControl (method="repeatedcv", number=2, repeats=5)
model.lda <- train(digit ~ ., data = semeion.train.feat, method='lda', MaxNWts=3000, maxit=100, trControl=trc, trace = FALSE)

p1 <- as.factor(predict (model.lda, type = "raw"))
t1 <- table(p1,semeion.train.feat$digit)
(error_rate.learn.lda <- 100*(1-sum(diag(t1))/nlearn)) #~0.25%

p2 <- as.factor(predict (model.lda, newdata = semeion.test.feat, type = "raw"))
(t2 <- table(p2, semeion.test.feat$digit))
(error_rate.test.lda <- 100*(1-sum(diag(t2))/ntest)) #~13.05%



##
## Multinomial logistic regression
##

library(nnet)     	#para multinom

# Utilizamos trainControl para hacer 5x2CV; regularizamos con el parámetro decay
trc <- trainControl (method="repeatedcv", number=2, repeats=5)
model.mnm <- train(digit ~ ., data = semeion.train.feat, method='multinom', MaxNWts=3000, maxit=100, trControl=trc, trace = FALSE,
              tuneGrid = expand.grid(decay=seq(0.1,0.9,0.1)))

model.mnm$results
model.mnm$bestTune #decay: 0.7
plot(model.mnm)

p1 <- as.factor(predict (model.mnm, type = "raw"))
t1 <- table(p1, semeion.train.feat$digit)
(error_rate.learn.mnm <- 100*(1-sum(diag(t1))/nlearn)) #~0%

p2 <- as.factor(predict (model.mnm, newdata = semeion.test.feat, type = "raw"))
(t2 <- table(p2, semeion.test.feat$digit))
(error_rate.test.mnm <- 100*(1-sum(diag(t2))/ntest)) #~7.15%



##
## Multilayer Perceptron
##

trc <- trainControl (method="repeatedcv", number=2, repeats=5)
model.mlp <- train(digit ~ ., data = semeion.train.feat, method='nnet', MaxNWts=15000, maxit=50, trace = FALSE,
                       tuneGrid = expand.grid(.size=seq(20,40,by=5),.decay=0), trControl=trc)

model.mlp$results
model.mlp$bestTune #size 35
plot(model.mlp)

bestSize <- model.mlp$bestTune$size

trc <- trainControl (method="repeatedcv", number=2, repeats=5)
model.mlp <- train(digit ~ ., data = semeion.train.feat, method='nnet', MaxNWts=15000, maxit=50, trace = FALSE,
                   tuneGrid = expand.grid(.size=bestSize,.decay=seq(0.1,0.9,0.1)), trControl=trc)

model.mlp$results
model.mlp$bestTune #size: 35, decay: 0.3
plot(model.mlp)

p1 <- as.factor(predict (model.mlp, type = "raw"))
t1 <- table(p1, semeion.train.feat$digit)
(error_rate.learn.mlp <- 100*(1-sum(diag(t1))/nlearn)) #~7.28%

p2 <- as.factor(predict (model.mlp, newdata = semeion.test.feat, type = "raw"))
(t2 <- table(p2, semeion.test.feat$digit))
(error_rate.test.mlp <- 100*(1-sum(diag(t2))/ntest)) #~20.57%



##
## Support Vector Machine (RBF Kernel)
##

trc <- trainControl (method="repeatedcv", number=2, repeats=5)
model.svm <- train(digit ~ ., data = semeion.train.feat, method='svmRadialCost', MaxNWts=10000, maxit=100, trace = FALSE,
               tuneGrid = expand.grid(.C=seq(1,20)), trControl=trc)

model.svm$results
model.svm$bestTune #C: 2, sigma: 0.001798363
plot(model.svm)

p1 <- as.factor(predict (model.svm, type = "raw"))
t1 <- table(p1, semeion.train.feat$digit)
(error_rate.learn.svm <- 100*(1-sum(diag(t1))/nlearn)) #~0.25%

p2 <- as.factor(predict (model.svm, newdata = semeion.test.feat, type = "raw"))
(t2 <- table(p2, semeion.test.feat$digit))
(error_rate.test.svm <- 100*(1-sum(diag(t2))/ntest)) #~3.88%

which(p2 != semeion.test.feat$digit) #dígitos clasificados erróneamente
model.svm$finalModel@coef #pesos del modelo

## Elegimos este modelo como clasificador final



##
## Gráficos y funciones auxiliares
##

bp <- barplot(t2[row(t2)==col(t2)]/colSums(t2)*100, names.arg = c(0,1,2,3,4,5,6,7,8,9),
        ylim = c(80,100.1), xpd = FALSE, xlab = "Dígito", ylab = "Porcentaje de precisión",
        main = "Precisión con SVM (kernel RBF)")
axis(1, at=bp, labels=FALSE)


errors <- c(error_rate.test.lda, error_rate.test.mnm, error_rate.test.mlp, error_rate.test.svm)
c1 <- colors()[562]
c2 <- colors()[555]
barplot(errors, names.arg = c("LDA","Mnm","MLP","SVM"), col = c(c2,c2,c1,c1), ylim = c(0,21),
        xlab = "Modelo de clasificador", ylab = "Error de generalización (estimado)",
        main = "Estimación del error de generalización \nde los modelos estudiados")



outputDigit <- function(data, index, hidezeros=TRUE) {
    l <- data[index,]
    n <- l$digit
    if (hidezeros) l[l == 0] = ' '
    message(sprintf("%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n",
                    paste(l[1:16], collapse = ''), paste(l[17:32], collapse = ''),
                    paste(l[33:48], collapse = ''), paste(l[49:64], collapse = ''),
                    paste(l[65:80], collapse = ''), paste(l[81:96], collapse = ''),
                    paste(l[97:112], collapse = ''), paste(l[113:128], collapse = ''),
                    paste(l[129:144], collapse = ''), paste(l[145:160], collapse = ''),
                    paste(l[161:176], collapse = ''), paste(l[177:192], collapse = ''),
                    paste(l[193:208], collapse = ''), paste(l[209:224], collapse = ''),
                    paste(l[225:240], collapse = ''), paste(l[241:256], collapse = '')))
    message(sprintf("(%s)\n", n))
}

outputDigit(semeion, 1)
