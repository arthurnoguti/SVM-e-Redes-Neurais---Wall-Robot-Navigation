library('tidyverse')
library('e1071') #SVM
library('nnet') #Neural nets
library('NeuralNetTools')
library('tidymodels')
library('rgl')

#### Análise descritiva ####
wall_robot <- read.csv2(choose.files())
wall_robot <- as.tibble(wall_robot)
wrn <- mutate(wall_robot, X1 = as.numeric(X1), X2 = as.numeric(X2), Y = as.factor(Y))

summary(wrn)
table(wrn$Y) #Queremos descobrir o valor de Y a partir de X1 e X2

wrn %>% 
  plot3d()

head(wrn)

max(wrn$X1)
min(wrn$X1)
max(wrn$X2)
min(wrn$X2)


##### Validação k-fold ####
#Método de Validacao Cruzada k-fold k = 5

set.seed(3)

y <- c(1:5456)
sorteio1 <- sample(y, 1091)
sorteio2 <- sample(y[-sorteio1], 1091)
sorteio3 <- sample(y[c(-sorteio1, - sorteio2)], 1091)
sorteio4 <- sample(y[c(-sorteio1, - sorteio2, -sorteio3)], 1091)
sorteio5 <- sample(y[c(-sorteio1, - sorteio2, -sorteio3, - sorteio4)], 1092)

wrn_treinamento1 <- wrn[-sorteio1, ]
wrn_teste1 <- wrn[sorteio1, ]

wrn_treinamento2 <- wrn[-sorteio2, ]
wrn_teste2 <- wrn[sorteio2, ]

wrn_treinamento3 <- wrn[-sorteio3, ]
wrn_teste3 <- wrn[sorteio3, ]

wrn_treinamento4 <- wrn[-sorteio4, ]
wrn_teste4 <- wrn[sorteio4, ]

wrn_treinamento5 <- wrn[-sorteio5, ]
wrn_teste5 <- wrn[sorteio5, ] 


##### SVM #### 
#### RODADA SVM 1 ####

modelo_svm_radial1 <- svm(Y ~ ., data = wrn_treinamento1, kernel = "radial")

summary(modelo_svm_radial1)  

pred_radial1 <- predict(modelo_svm_radial1, wrn_teste1)

tab_radial1 <- table(Predict = pred_radial1, Actual = wrn_teste1$Y)

matriz_conf1 <- tab_radial1
#### RODADA SVM 2 ####

modelo_svm_radial2 <- svm(Y ~ ., data = wrn_treinamento2, kernel = "radial")

summary(modelo_svm_radial2)  

pred_radial2 <- predict(modelo_svm_radial2, wrn_teste2)

tab_radial2 <- table(Predict = pred_radial2, Actual = wrn_teste2$Y)

matriz_conf2 <- tab_radial2

#### RODADA SVM 3 ####

modelo_svm_radial3 <- svm(Y ~ ., data = wrn_treinamento3, kernel = "radial")

summary(modelo_svm_radial3)  

pred_radial3 <- predict(modelo_svm_radial3, wrn_teste3)

tab_radial3 <- table(Predict = pred_radial3, Actual = wrn_teste3$Y)

matriz_conf3 <- tab_radial3

#### RODADA SVM 4 ####

modelo_svm_radial4 <- svm(Y ~ ., data = wrn_treinamento4, kernel = "radial")
  
summary(modelo_svm_radial4)  

pred_radial4 <- predict(modelo_svm_radial4, wrn_teste4)

tab_radial4 <- table(Predict = pred_radial4, Actual = wrn_teste4$Y)

matriz_conf4 <- tab_radial4

#### RODADA SVM 5 ####

modelo_svm_radial5 <- svm(Y ~ ., data = wrn_treinamento5, kernel = "radial")

summary(modelo_svm_radial5)  

pred_radial5 <- predict(modelo_svm_radial5, wrn_teste5)

tab_radial5 <- table(Predict = pred_radial5, Actual = wrn_teste5$Y)

matriz_conf5 <- tab_radial5



#### Desempenho SVM ####
####medidas desemp SVM rod 1 ####
mc1.1 <- matriz_conf1[c(1,2),c(1,2)]
mc2.1 <- matriz_conf1[c(1,3),c(1,3)]
mc3.1 <- matriz_conf1[c(1,4),c(1,4)]
mc4.1 <- matriz_conf1[c(2,3),c(2,3)]
mc5.1 <- matriz_conf1[c(2,4),c(2,4)]
mc6.1 <- matriz_conf1[c(3,4),c(3,4)]
list_matriz1 <- list(mc1.1,mc2.1,mc3.1,mc4.1,mc5.1,mc6.1)

#MACRO 
#acuracia
acc_macro <- function(x){
  (x[1,1]+x[2,2])/sum(x)
}
acc_macro1 <- sum(sapply(list_matriz1, acc_macro))/6
acc_macro1

#sensibilidade ou recall
sen_macro <- function(x){
  x[1,1]/(x[1,1]+x[1,2])
}
sen_macro1 <- sum(sapply(list_matriz1, sen_macro))/6
sen_macro1

#especificidade
esp_macro <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}
esp_macro1 <- sum(sapply(list_matriz1, esp_macro))/6
esp_macro1

#acuracia balanceada

BA_macro1 <- (sen_macro1+esp_macro1)/2
BA_macro1

#precisao
p_macro <- function(x){
  x[1,1]/(x[1,1]+x[2,1])
}
p_macro1 <- sum(sapply(list_matriz1, p_macro))/6
p_macro1

#f1-score
f1_macro1 <- (2*p_macro1*sen_macro1)/(p_macro1+sen_macro1)
f1_macro1

#MICRO
sum_mc1 <- mc1.1+mc2.1+mc3.1+mc4.1+mc5.1+mc6.1
sum_mc1
acc_micro1 <- (sum_mc1[1,1]+sum_mc1[2,2])/sum(sum_mc1) #acuracia
sen_micro1 <- sum_mc1[1,1]/(sum_mc1[1,1]+sum_mc1[1,2]) #sensibilidade ou recall
esp_micro1 <- sum_mc1[2,2]/(sum_mc1[2,2]+sum_mc1[2,1]) #especificidade
BA_micro1 <- (sen_micro1+esp_micro1)/2
p_micro1 <- sum_mc1[1,1]/(sum_mc1[1,1]+sum_mc1[2,1])
f1_micro1 <- (2*p_micro1*sen_micro1)/(p_micro1+sen_micro1)

####medidas desemp SVM rod 2 ####
mc1.2 <- matriz_conf2[c(1,2),c(1,2)]
mc2.2 <- matriz_conf2[c(1,3),c(1,3)]
mc3.2 <- matriz_conf2[c(1,4),c(1,4)]
mc4.2 <- matriz_conf2[c(2,3),c(2,3)]
mc5.2 <- matriz_conf2[c(2,4),c(2,4)]
mc6.2 <- matriz_conf2[c(3,4),c(3,4)]
list_matriz2 <- list(mc1.2,mc2.2,mc3.2,mc4.2,mc5.2,mc6.2)

#MACRO 
#acuracia
acc_macro <- function(x){
  (x[1,1]+x[2,2])/sum(x)
}
acc_macro2 <- sum(sapply(list_matriz2, acc_macro))/6
acc_macro2

#sensibilidade ou recall
sen_macro <- function(x){
  x[1,1]/(x[1,1]+x[1,2])
}
sen_macro2 <- sum(sapply(list_matriz2, sen_macro))/6
sen_macro2

#especificidade
esp_macro <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}
esp_macro2 <- sum(sapply(list_matriz2, esp_macro))/6
esp_macro2

#acuracia balanceada

BA_macro2 <- (sen_macro2+esp_macro2)/2
BA_macro2

#precisao
p_macro <- function(x){
  x[1,1]/(x[1,1]+x[2,1])
}
p_macro2 <- sum(sapply(list_matriz2, p_macro))/6
p_macro2

#f1-score
f1_macro2 <- (2*p_macro2*sen_macro2)/(p_macro2+sen_macro2)
f1_macro2

#MICRO
sum_mc2 <- mc1.2+mc2.2+mc3.2+mc4.2+mc5.2+mc6.2
sum_mc2
acc_micro2 <- (sum_mc2[1,1]+sum_mc2[2,2])/sum(sum_mc2) #acuracia
sen_micro2 <- sum_mc2[1,1]/(sum_mc2[1,1]+sum_mc2[1,2]) #sensibilidade ou recall
esp_micro2 <- sum_mc2[2,2]/(sum_mc2[2,2]+sum_mc2[2,1]) #especificidade
BA_micro2 <- (sen_micro2+esp_micro2)/2
p_micro2 <- sum_mc2[1,1]/(sum_mc2[1,1]+sum_mc2[2,1])
f1_micro2 <- (2*p_micro2*sen_micro2)/(p_micro2+sen_micro2)

####medidas desemp SVM rod 3 ####
mc1.3 <- matriz_conf3[c(1,2),c(1,2)]
mc2.3 <- matriz_conf3[c(1,3),c(1,3)]
mc3.3 <- matriz_conf3[c(1,4),c(1,4)]
mc4.3 <- matriz_conf3[c(2,3),c(2,3)]
mc5.3 <- matriz_conf3[c(2,4),c(2,4)]
mc6.3 <- matriz_conf3[c(3,4),c(3,4)]
list_matriz3 <- list(mc1.3,mc2.3,mc3.3,mc4.3,mc5.3,mc6.3)

#MACRO 
#acuracia
acc_macro <- function(x){
  (x[1,1]+x[2,2])/sum(x)
}
acc_macro3 <- sum(sapply(list_matriz3, acc_macro))/6
acc_macro3

#sensibilidade ou recall
sen_macro <- function(x){
  x[1,1]/(x[1,1]+x[1,2])
}
sen_macro3 <- sum(sapply(list_matriz3, sen_macro))/6
sen_macro3

#especificidade
esp_macro <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}
esp_macro3 <- sum(sapply(list_matriz3, esp_macro))/6
esp_macro3

#acuracia balanceada

BA_macro3 <- (sen_macro3+esp_macro3)/2
BA_macro3

#precisao
p_macro <- function(x){
  x[1,1]/(x[1,1]+x[2,1])
}
p_macro3 <- sum(sapply(list_matriz3, p_macro))/6
p_macro3

#f1-score
f1_macro3 <- (2*p_macro3*sen_macro3)/(p_macro3+sen_macro3)
f1_macro3

#MICRO
sum_mc3 <- mc1.3+mc2.3+mc3.3+mc4.3+mc5.3+mc6.3
sum_mc3
acc_micro3 <- (sum_mc3[1,1]+sum_mc3[2,2])/sum(sum_mc3) #acuracia
sen_micro3 <- sum_mc3[1,1]/(sum_mc3[1,1]+sum_mc3[1,2]) #sensibilidade ou recall
esp_micro3 <- sum_mc3[2,2]/(sum_mc3[2,2]+sum_mc3[2,1]) #especificidade
BA_micro3 <- (sen_micro3+esp_micro3)/2
p_micro3 <- sum_mc3[1,1]/(sum_mc3[1,1]+sum_mc3[2,1])
f1_micro3 <- (2*p_micro3*sen_micro3)/(p_micro3+sen_micro3)
####medidas desemp SVM rod 4 ####
mc1.4 <- matriz_conf4[c(1,2),c(1,2)]
mc2.4 <- matriz_conf4[c(1,3),c(1,3)]
mc3.4 <- matriz_conf4[c(1,4),c(1,4)]
mc4.4 <- matriz_conf4[c(2,3),c(2,3)]
mc5.4 <- matriz_conf4[c(2,4),c(2,4)]
mc6.4 <- matriz_conf4[c(3,4),c(3,4)]
list_matriz4 <- list(mc1.4,mc2.4,mc3.4,mc4.4,mc5.4,mc6.4)



#MACRO 
#acuracia
acc_macro <- function(x){
  (x[1,1]+x[2,2])/sum(x)
}
acc_macro4 <- sum(sapply(list_matriz4, acc_macro))/6
acc_macro4

#sensibilidade ou recall
sen_macro <- function(x){
  x[1,1]/(x[1,1]+x[1,2])
}
sen_macro4 <- sum(sapply(list_matriz4, sen_macro))/6
sen_macro4

#especificidade
esp_macro <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}
esp_macro4 <- sum(sapply(list_matriz4, esp_macro))/6
esp_macro4

#acuracia balanceada

BA_macro4 <- (sen_macro4+esp_macro4)/2
BA_macro4

#precisao
p_macro <- function(x){
  x[1,1]/(x[1,1]+x[2,1])
}
p_macro4 <- sum(sapply(list_matriz4, p_macro))/6
p_macro4

#f1-score
f1_macro4 <- (2*p_macro4*sen_macro4)/(p_macro4+sen_macro4)
f1_macro4

#MICRO
sum_mc4 <- mc1.4+mc2.4+mc3.4+mc4.4+mc5.4+mc6.4
sum_mc4
acc_micro4 <- (sum_mc4[1,1]+sum_mc4[2,2])/sum(sum_mc4) #acuracia
sen_micro4 <- sum_mc4[1,1]/(sum_mc4[1,1]+sum_mc4[1,2]) #sensibilidade ou recall
esp_micro4 <- sum_mc4[2,2]/(sum_mc4[2,2]+sum_mc4[2,1]) #especificidade
BA_micro4 <- (sen_micro4+esp_micro4)/2
p_micro4 <- sum_mc4[1,1]/(sum_mc4[1,1]+sum_mc4[2,1])
f1_micro4 <- (2*p_micro4*sen_micro4)/(p_micro4+sen_micro4)

####medidas desemp SVM rod 5 ####
mc1.5 <- matriz_conf5[c(1,2),c(1,2)]
mc2.5 <- matriz_conf5[c(1,3),c(1,3)]
mc3.5 <- matriz_conf5[c(1,4),c(1,4)]
mc4.5 <- matriz_conf5[c(2,3),c(2,3)]
mc5.5 <- matriz_conf5[c(2,4),c(2,4)]
mc6.5 <- matriz_conf5[c(3,4),c(3,4)]
list_matriz5 <- list(mc1.5,mc2.5,mc3.5,mc4.5,mc5.5,mc6.5)



#MACRO 
#acuracia
acc_macro <- function(x){
  (x[1,1]+x[2,2])/sum(x)
}
acc_macro5 <- sum(sapply(list_matriz5, acc_macro))/6
acc_macro5

#sensibilidade ou recall
sen_macro <- function(x){
  x[1,1]/(x[1,1]+x[1,2])
}
sen_macro5 <- sum(sapply(list_matriz5, sen_macro))/6
sen_macro5

#especificidade
esp_macro <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}
esp_macro5 <- sum(sapply(list_matriz5, esp_macro))/6
esp_macro5

#acuracia balanceada

BA_macro5 <- (sen_macro5+esp_macro5)/2
BA_macro5

#precisao
p_macro <- function(x){
  x[1,1]/(x[1,1]+x[2,1])
}
p_macro5 <- sum(sapply(list_matriz5, p_macro))/6
p_macro5

#f1-score
f1_macro5 <- (2*p_macro5*sen_macro5)/(p_macro5+sen_macro5)
f1_macro5

#MICRO
sum_mc5 <- mc1.5+mc2.5+mc3.5+mc4.5+mc5.5+mc6.5
sum_mc5
acc_micro5 <- (sum_mc5[1,1]+sum_mc5[2,2])/sum(sum_mc5) #acuracia
sen_micro5 <- sum_mc5[1,1]/(sum_mc5[1,1]+sum_mc5[1,2]) #sensibilidade ou recall
esp_micro5 <- sum_mc5[2,2]/(sum_mc5[2,2]+sum_mc5[2,1]) #especificidade
BA_micro5 <- (sen_micro5+esp_micro5)/2
p_micro5 <- sum_mc5[1,1]/(sum_mc5[1,1]+sum_mc5[2,1])
f1_micro5 <- (2*p_micro5*sen_micro5)/(p_micro5+sen_micro5)


#### medida de desempenho SVM geral ####
acc_macro_geral <- (acc_macro1+acc_macro2+acc_macro3+acc_macro4+acc_macro5)/5
sen_macro_geral <- (sen_macro1+sen_macro2+sen_macro3+sen_macro4+sen_macro5)/5
esp_macro_geral <- (esp_macro1+esp_macro2+esp_macro3+esp_macro4+esp_macro5)/5
BA_macro_geral <- (BA_macro1+BA_macro2+BA_macro3+BA_macro4+BA_macro5)/5
p_macro_geral <- (p_macro1+p_macro2+p_macro3+p_macro4+p_macro5)/5
f1_macro_geral <- (f1_macro1+f1_macro2+f1_macro3+f1_macro4+f1_macro5)/5


acc_micro_geral <- (acc_micro1+acc_micro2+acc_micro3+acc_micro4+acc_micro5)/5
sen_micro_geral <- (sen_micro1+sen_micro2+sen_micro3+sen_micro4+sen_micro5)/5
esp_micro_geral <- (esp_micro1+esp_micro2+esp_micro3+esp_micro4+esp_micro5)/5
BA_micro_geral <- (BA_micro1+BA_micro2+BA_micro3+BA_micro4+BA_micro5)/5
p_micro_geral <- (p_micro1+p_micro2+p_micro3+p_micro4+p_micro5)/5
f1_micro_geral <- (f1_micro1+f1_micro2+f1_micro3+f1_micro4+f1_micro5)/5

coluna_macro <- c(acc_macro_geral,sen_macro_geral,esp_macro_geral,BA_macro_geral,p_macro_geral,f1_macro_geral)
coluna_micro <- c(acc_micro_geral,sen_micro_geral,esp_micro_geral,BA_micro_geral,p_micro_geral,f1_micro_geral)

metricas_svm <- data.frame(
  desempenho = c("acuracia","sensibilidade","especificidade","acuracia balanceada","precisao","f1-score"),
  macro = coluna_macro,
  micro = coluna_micro
)
metricas_svm





##### Redes Neurais ####
#### RODADA 1 REDES NEURAIS ####
mdl_fit_nn_wall_robot1 <- nnet(Y ~., data = wrn_treinamento1, size = 4)

NeuralNetTools::plotnet(mdl_fit_nn_wall_robot1)

previsao_nn_wall_robot1 <- predict(mdl_fit_nn_wall_robot1, newdata = wrn_teste1[,-3], type = "class")

matriz_conf1 <- table(previsao_nn_wall_robot1, wrn_teste1$Y)

#### RODADA 2 REDES NEURAIS ####
mdl_fit_nn_wall_robot2 <- nnet(Y ~., data = wrn_treinamento2, size = 4)

NeuralNetTools::plotnet(mdl_fit_nn_wall_robot2)

previsao_nn_wall_robot2 <- predict(mdl_fit_nn_wall_robot2, newdata = wrn_teste2[,-3], type = "class")

matriz_conf2 <- table(previsao_nn_wall_robot2, wrn_teste2$Y)

#### RODADA 3 REDES NEURAIS ####
mdl_fit_nn_wall_robot3 <- nnet(Y ~., data = wrn_treinamento3, size = 4)

NeuralNetTools::plotnet(mdl_fit_nn_wall_robot3)

previsao_nn_wall_robot3 <- predict(mdl_fit_nn_wall_robot3, newdata = wrn_teste3[,-3], type = "class")

matriz_conf3 <- table(previsao_nn_wall_robot3, wrn_teste3$Y)

#### RODADA 4 REDES NEURAIS ####
mdl_fit_nn_wall_robot4 <- nnet(Y ~., data = wrn_treinamento4, size = 4)

NeuralNetTools::plotnet(mdl_fit_nn_wall_robot4)

previsao_nn_wall_robot4 <- predict(mdl_fit_nn_wall_robot4, newdata = wrn_teste4[,-3], type = "class")

matriz_conf4 <- table(previsao_nn_wall_robot4, wrn_teste4$Y)            

#### RODADA 5 REDES NEURAIS ####
mdl_fit_nn_wall_robot5 <- nnet(Y ~., data = wrn_treinamento5, size = 4)

NeuralNetTools::plotnet(mdl_fit_nn_wall_robot5)

previsao_nn_wall_robot5 <- predict(mdl_fit_nn_wall_robot5, newdata = wrn_teste5[,-3], type = "class")

matriz_conf5 <- table(previsao_nn_wall_robot5, wrn_teste5$Y)



#### Desempenho Redes Neurais ####
####medidas desemp Redes Neurais rod 1 ####
mc1.1 <- matriz_conf1[c(1,2),c(1,2)]
mc2.1 <- matriz_conf1[c(1,3),c(1,3)]
mc3.1 <- matriz_conf1[c(1,4),c(1,4)]
mc4.1 <- matriz_conf1[c(2,3),c(2,3)]
mc5.1 <- matriz_conf1[c(2,4),c(2,4)]
mc6.1 <- matriz_conf1[c(3,4),c(3,4)]
list_matriz1 <- list(mc1.1,mc2.1,mc3.1,mc4.1,mc5.1,mc6.1)

#MACRO 
#acuracia
acc_macro <- function(x){
  (x[1,1]+x[2,2])/sum(x)
}
acc_macro1 <- sum(sapply(list_matriz1, acc_macro))/6
acc_macro1

#sensibilidade ou recall
sen_macro <- function(x){
  x[1,1]/(x[1,1]+x[1,2])
}
sen_macro1 <- sum(sapply(list_matriz1, sen_macro))/6
sen_macro1

#especificidade
esp_macro <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}
esp_macro1 <- sum(sapply(list_matriz1, esp_macro))/6
esp_macro1

#acuracia balanceada

BA_macro1 <- (sen_macro1+esp_macro1)/2
BA_macro1

#precisao
p_macro <- function(x){
  x[1,1]/(x[1,1]+x[2,1])
}
p_macro1 <- sum(sapply(list_matriz1, p_macro))/6
p_macro1

#f1-score
f1_macro1 <- (2*p_macro1*sen_macro1)/(p_macro1+sen_macro1)
f1_macro1

#MICRO
sum_mc1 <- mc1.1+mc2.1+mc3.1+mc4.1+mc5.1+mc6.1
sum_mc1
acc_micro1 <- (sum_mc1[1,1]+sum_mc1[2,2])/sum(sum_mc1) #acuracia
sen_micro1 <- sum_mc1[1,1]/(sum_mc1[1,1]+sum_mc1[1,2]) #sensibilidade ou recall
esp_micro1 <- sum_mc1[2,2]/(sum_mc1[2,2]+sum_mc1[2,1]) #especificidade
BA_micro1 <- (sen_micro1+esp_micro1)/2
p_micro1 <- sum_mc1[1,1]/(sum_mc1[1,1]+sum_mc1[2,1])
f1_micro1 <- (2*p_micro1*sen_micro1)/(p_micro1+sen_micro1)

####medidas desemp Redes Neurais rod 2 ####
mc1.2 <- matriz_conf2[c(1,2),c(1,2)]
mc2.2 <- matriz_conf2[c(1,3),c(1,3)]
mc3.2 <- matriz_conf2[c(1,4),c(1,4)]
mc4.2 <- matriz_conf2[c(2,3),c(2,3)]
mc5.2 <- matriz_conf2[c(2,4),c(2,4)]
mc6.2 <- matriz_conf2[c(3,4),c(3,4)]
list_matriz2 <- list(mc1.2,mc2.2,mc3.2,mc4.2,mc5.2,mc6.2)

#MACRO 
#acuracia
acc_macro <- function(x){
  (x[1,1]+x[2,2])/sum(x)
}
acc_macro2 <- sum(sapply(list_matriz2, acc_macro))/6
acc_macro2

#sensibilidade ou recall
sen_macro <- function(x){
  x[1,1]/(x[1,1]+x[1,2])
}
sen_macro2 <- sum(sapply(list_matriz2, sen_macro))/6
sen_macro2

#especificidade
esp_macro <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}
esp_macro2 <- sum(sapply(list_matriz2, esp_macro))/6
esp_macro2

#acuracia balanceada

BA_macro2 <- (sen_macro2+esp_macro2)/2
BA_macro2

#precisao
p_macro <- function(x){
  x[1,1]/(x[1,1]+x[2,1])
}
p_macro2 <- sum(sapply(list_matriz2, p_macro))/6
p_macro2

#f1-score
f1_macro2 <- (2*p_macro2*sen_macro2)/(p_macro2+sen_macro2)
f1_macro2

#MICRO
sum_mc2 <- mc1.2+mc2.2+mc3.2+mc4.2+mc5.2+mc6.2
sum_mc2
acc_micro2 <- (sum_mc2[1,1]+sum_mc2[2,2])/sum(sum_mc2) #acuracia
sen_micro2 <- sum_mc2[1,1]/(sum_mc2[1,1]+sum_mc2[1,2]) #sensibilidade ou recall
esp_micro2 <- sum_mc2[2,2]/(sum_mc2[2,2]+sum_mc2[2,1]) #especificidade
BA_micro2 <- (sen_micro2+esp_micro2)/2
p_micro2 <- sum_mc2[1,1]/(sum_mc2[1,1]+sum_mc2[2,1])
f1_micro2 <- (2*p_micro2*sen_micro2)/(p_micro2+sen_micro2)

####medidas desemp Redes Neurais rod 3 ####
mc1.3 <- matriz_conf3[c(1,2),c(1,2)]
mc2.3 <- matriz_conf3[c(1,3),c(1,3)]
mc3.3 <- matriz_conf3[c(1,4),c(1,4)]
mc4.3 <- matriz_conf3[c(2,3),c(2,3)]
mc5.3 <- matriz_conf3[c(2,4),c(2,4)]
mc6.3 <- matriz_conf3[c(3,4),c(3,4)]
list_matriz3 <- list(mc1.3,mc2.3,mc3.3,mc4.3,mc5.3,mc6.3)

#MACRO 
#acuracia
acc_macro <- function(x){
  (x[1,1]+x[2,2])/sum(x)
}
acc_macro3 <- sum(sapply(list_matriz3, acc_macro))/6
acc_macro3

#sensibilidade ou recall
sen_macro <- function(x){
  x[1,1]/(x[1,1]+x[1,2])
}
sen_macro3 <- sum(sapply(list_matriz3, sen_macro))/6
sen_macro3

#especificidade
esp_macro <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}
esp_macro3 <- sum(sapply(list_matriz3, esp_macro))/6
esp_macro3

#acuracia balanceada

BA_macro3 <- (sen_macro3+esp_macro3)/2
BA_macro3

#precisao
p_macro <- function(x){
  x[1,1]/(x[1,1]+x[2,1])
}
p_macro3 <- sum(sapply(list_matriz3, p_macro))/6
p_macro3

#f1-score
f1_macro3 <- (2*p_macro3*sen_macro3)/(p_macro3+sen_macro3)
f1_macro3

#MICRO
sum_mc3 <- mc1.3+mc2.3+mc3.3+mc4.3+mc5.3+mc6.3
sum_mc3
acc_micro3 <- (sum_mc3[1,1]+sum_mc3[2,2])/sum(sum_mc3) #acuracia
sen_micro3 <- sum_mc3[1,1]/(sum_mc3[1,1]+sum_mc3[1,2]) #sensibilidade ou recall
esp_micro3 <- sum_mc3[2,2]/(sum_mc3[2,2]+sum_mc3[2,1]) #especificidade
BA_micro3 <- (sen_micro3+esp_micro3)/2
p_micro3 <- sum_mc3[1,1]/(sum_mc3[1,1]+sum_mc3[2,1])
f1_micro3 <- (2*p_micro3*sen_micro3)/(p_micro3+sen_micro3)
####medidas desemp Redes Neurais rod 4 ####
mc1.4 <- matriz_conf4[c(1,2),c(1,2)]
mc2.4 <- matriz_conf4[c(1,3),c(1,3)]
mc3.4 <- matriz_conf4[c(1,4),c(1,4)]
mc4.4 <- matriz_conf4[c(2,3),c(2,3)]
mc5.4 <- matriz_conf4[c(2,4),c(2,4)]
mc6.4 <- matriz_conf4[c(3,4),c(3,4)]
list_matriz4 <- list(mc1.4,mc2.4,mc3.4,mc4.4,mc5.4,mc6.4)



#MACRO 
#acuracia
acc_macro <- function(x){
  (x[1,1]+x[2,2])/sum(x)
}
acc_macro4 <- sum(sapply(list_matriz4, acc_macro))/6
acc_macro4

#sensibilidade ou recall
sen_macro <- function(x){
  x[1,1]/(x[1,1]+x[1,2])
}
sen_macro4 <- sum(sapply(list_matriz4, sen_macro))/6
sen_macro4

#especificidade
esp_macro <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}
esp_macro4 <- sum(sapply(list_matriz4, esp_macro))/6
esp_macro4

#acuracia balanceada

BA_macro4 <- (sen_macro4+esp_macro4)/2
BA_macro4

#precisao
p_macro <- function(x){
  x[1,1]/(x[1,1]+x[2,1])
}
p_macro4 <- sum(sapply(list_matriz4, p_macro))/6
p_macro4

#f1-score
f1_macro4 <- (2*p_macro4*sen_macro4)/(p_macro4+sen_macro4)
f1_macro4

#MICRO
sum_mc4 <- mc1.4+mc2.4+mc3.4+mc4.4+mc5.4+mc6.4
sum_mc4
acc_micro4 <- (sum_mc4[1,1]+sum_mc4[2,2])/sum(sum_mc4) #acuracia
sen_micro4 <- sum_mc4[1,1]/(sum_mc4[1,1]+sum_mc4[1,2]) #sensibilidade ou recall
esp_micro4 <- sum_mc4[2,2]/(sum_mc4[2,2]+sum_mc4[2,1]) #especificidade
BA_micro4 <- (sen_micro4+esp_micro4)/2
p_micro4 <- sum_mc4[1,1]/(sum_mc4[1,1]+sum_mc4[2,1])
f1_micro4 <- (2*p_micro4*sen_micro4)/(p_micro4+sen_micro4)

####medidas desemp Redes Neurais rod 5 ####
mc1.5 <- matriz_conf5[c(1,2),c(1,2)]
mc2.5 <- matriz_conf5[c(1,3),c(1,3)]
mc3.5 <- matriz_conf5[c(1,4),c(1,4)]
mc4.5 <- matriz_conf5[c(2,3),c(2,3)]
mc5.5 <- matriz_conf5[c(2,4),c(2,4)]
mc6.5 <- matriz_conf5[c(3,4),c(3,4)]
list_matriz5 <- list(mc1.5,mc2.5,mc3.5,mc4.5,mc5.5,mc6.5)



#MACRO 
#acuracia
acc_macro <- function(x){
  (x[1,1]+x[2,2])/sum(x)
}
acc_macro5 <- sum(sapply(list_matriz5, acc_macro))/6
acc_macro5

#sensibilidade ou recall
sen_macro <- function(x){
  x[1,1]/(x[1,1]+x[1,2])
}
sen_macro5 <- sum(sapply(list_matriz5, sen_macro))/6
sen_macro5

#especificidade
esp_macro <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}
esp_macro5 <- sum(sapply(list_matriz5, esp_macro))/6
esp_macro5

#acuracia balanceada

BA_macro5 <- (sen_macro5+esp_macro5)/2
BA_macro5

#precisao
p_macro <- function(x){
  x[1,1]/(x[1,1]+x[2,1])
}
p_macro5 <- sum(sapply(list_matriz5, p_macro))/6
p_macro5

#f1-score
f1_macro5 <- (2*p_macro5*sen_macro5)/(p_macro5+sen_macro5)
f1_macro5

#MICRO
sum_mc5 <- mc1.5+mc2.5+mc3.5+mc4.5+mc5.5+mc6.5
sum_mc5
acc_micro5 <- (sum_mc5[1,1]+sum_mc5[2,2])/sum(sum_mc5) #acuracia
sen_micro5 <- sum_mc5[1,1]/(sum_mc5[1,1]+sum_mc5[1,2]) #sensibilidade ou recall
esp_micro5 <- sum_mc5[2,2]/(sum_mc5[2,2]+sum_mc5[2,1]) #especificidade
BA_micro5 <- (sen_micro5+esp_micro5)/2
p_micro5 <- sum_mc5[1,1]/(sum_mc5[1,1]+sum_mc5[2,1])
f1_micro5 <- (2*p_micro5*sen_micro5)/(p_micro5+sen_micro5)


#### medida de desempenho Redes Neurais geral ####
acc_macro_geral <- (acc_macro1+acc_macro2+acc_macro3+acc_macro4+acc_macro5)/5
sen_macro_geral <- (sen_macro1+sen_macro2+sen_macro3+sen_macro4+sen_macro5)/5
esp_macro_geral <- (esp_macro1+esp_macro2+esp_macro3+esp_macro4+esp_macro5)/5
BA_macro_geral <- (BA_macro1+BA_macro2+BA_macro3+BA_macro4+BA_macro5)/5
p_macro_geral <- (p_macro1+p_macro2+p_macro3+p_macro4+p_macro5)/5
f1_macro_geral <- (f1_macro1+f1_macro2+f1_macro3+f1_macro4+f1_macro5)/5


acc_micro_geral <- (acc_micro1+acc_micro2+acc_micro3+acc_micro4+acc_micro5)/5
sen_micro_geral <- (sen_micro1+sen_micro2+sen_micro3+sen_micro4+sen_micro5)/5
esp_micro_geral <- (esp_micro1+esp_micro2+esp_micro3+esp_micro4+esp_micro5)/5
BA_micro_geral <- (BA_micro1+BA_micro2+BA_micro3+BA_micro4+BA_micro5)/5
p_micro_geral <- (p_micro1+p_micro2+p_micro3+p_micro4+p_micro5)/5
f1_micro_geral <- (f1_micro1+f1_micro2+f1_micro3+f1_micro4+f1_micro5)/5

coluna_macro <- c(acc_macro_geral,sen_macro_geral,esp_macro_geral,BA_macro_geral,p_macro_geral,f1_macro_geral)
coluna_micro <- c(acc_micro_geral,sen_micro_geral,esp_micro_geral,BA_micro_geral,p_micro_geral,f1_micro_geral)

metricas_nn <- data.frame(
  desempenho = c("acuracia","sensibilidade","especificidade","acuracia balanceada","precisao","f1-score"),
  macro = coluna_macro,
  micro = coluna_micro
)



