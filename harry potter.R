rm(list=ls())
setwd("C:/Users/sofia/OneDrive - Università degli Studi di Milano-Bicocca/uni/TERZO ANNO/STATISTICA COMPUTAZIONALE/homework")
library(tidyverse)
# elimino dal dataset i partecipanti minorenni e il caso con età anomala
datraw = read.csv('harry_all.csv', sep = ';') %>% 
  mutate(age = ifelse(age == 1987, 2016 - 1987, age)) %>% 
  filter(age >= 18 & age <= 100)
# seleziono i partecipanti assegnati ad una sola casa
nr_houses <- sapply(str_split(as.character(datraw$Sorting_house), ';'), length)
dat <- datraw[nr_houses == 1, ] %>% 
  mutate(Sorting_house = as.factor(as.character(Sorting_house)))
# creo una funzione che mi indica se il soggetto è in quella casa,
# verificando un'uguaglianza e dando in output un factor (1 se è nella casa, 0 altrimenti)
recode <- function(house, sorted) as.factor(as.numeric(house == sorted))
center_score <- function(score, dat) {
  # mean score across all values for each participant
  mean_score <- rowMeans(dat[, -seq(280-57)])
  value_score <- rowMeans(select(dat, one_of(paste0('PVQ_', score))))
  
  # centered score (following Schwartz)
  value_score - mean_score
}
# PVQ -> Portrait Values Questionnaire
# diviso in maschi e femmine, sono 57 oggetti
# che descrivono l'obiettivo che un individuo considera importante
# e in che misura si sentono simili a ciò, da 1 a 6 (molto me)
# si vuole così misurare i valori umani
# per esempio bisogno di aiutare gli altri, importanza del successo nel lavoro
# rispetto delle regole...
SELF_SCORE <- c('01', '23', '39', '16', '30', '56')
STIM_SCORE <- c('10', '28', '43')
HEDON_SCORE <- c('03', '36', '46')
ACHIEVEMENT_SCORE <- c('17', '32', '48')
POWER_SCORE <- c('06', '29', '41', '12', '20', '44')
SECUR_SCORE <- c('13', '26', '53', '02', '35', '50')
CONFOR_SCORE <- c('15', '31', '42', '04', '22', '51')
TRADIT_SCORE <- c('18', '33', '40', '07', '38', '54')
BENEV_SCORE <- c('11', '25', '47', '19', '27', '55')
UNIV_SCORE <- c('08', '21', '45', '05', '37', '52', '14', '34', '57')

dat <- dat %>%
  mutate(
    match = Sorting_house == Sorting_house_wish,
    Gryffindor = recode('Gryffindor', Sorting_house),
    Hufflepuff = recode('Hufflepuff', Sorting_house),
    Ravenclar  = recode('Ravenclaw', Sorting_house),
    Slytherin  = recode('Slytherin', Sorting_house),
    
    self = center_score(SELF_SCORE, .),
    stim = center_score(STIM_SCORE, .),
    hedon = center_score(HEDON_SCORE, .),
    achievement = center_score(ACHIEVEMENT_SCORE, .),
    power = center_score(POWER_SCORE, .),
    secur = center_score(SECUR_SCORE, .),
    confor = center_score(CONFOR_SCORE, .),
    tradit = center_score(TRADIT_SCORE, .),
    benev = center_score(BENEV_SCORE, .),
    univ = center_score(UNIV_SCORE, .)
  ) %>%
  # IPIP indica il test per la personalità, per i 5 big five: 
  # Estroversione, Coscienziosità, Gradevolezza, Stabilità Emotiva e Apertura all’Esperienza,
  # che utilizza una scala con 10 item per ciascun tratto e ci assegna una
  # misura da 1 a 5 (molto accurato)
  
  # SD misura la dark triad, quindi i tratti narcisismo, machiavellismo, psicopatia
  # tramite 9 item per tratto, ognuno con un punteggio da 1 a 5
  #( 5 fortemente d'accordo) 
  
  # Sia in IPIP che in SD i punteggi totali corrispondono alla somma
  # dei punteggi dei rispettivi item
  rename(
    ach = achievement,
    Extraversion = IPIP_Extraversion,
    Conscientiousness = IPIP_Conscientiousness,
    Agreeableness = IPIP_Agreeableness,
    Intellect = IPIP_Intellect,
    EmStability = IPIP_EmStability,
    Machiavellianism = SD3_Machiavellianism,
    Psychopathy = SD3_Psychopathy,
    Narcissism = SD3_Narcissism
  ) %>% 
  select(-starts_with('PVQ_'))


# rimozione variabili qualitative e non significative
data_labels = dat %>%
  select(c(7,102:109))
data = data_labels[,-1]

# analisi esplorativa -----------------------------------------------------
data_long <- data_labels %>%
  pivot_longer(cols = -Sorting_house_wish, 
               names_to = "Variable", 
               values_to = "Value")

# boxplot di tutte le variabili
ggplot(data_long, aes(x = Sorting_house_wish, y = Value, fill = Sorting_house_wish)) +
  geom_boxplot() +
  facet_wrap(~Variable) +
  labs(x = "Casa", y = "Valore", title = "Distribuzione delle variabili per Casa", fill = "Casa") +
  theme_minimal()

# correlazioni
cor=cor(data)
library(ggcorrplot)
ggcorrplot(cor, lab=T, hc.order=T)

# analisi delle componenti principali
pca = princomp(data, cor=T)
cumsum(pca$sdev^2)/sum(pca$sdev^2) # >70% con 4 componenti
pca$loadings[,1:4]
colnames(data)[apply(pca$loadings[,1:4], 2, function(x) which(x**2==max(x**2)))] # variabili associate ai loadings più alti
data=data[,c(3,4,7,8)]

library(GGally)
ggpairs(data) # correlazioni, densità univariate e scatterplot

data_labels = data_labels[,c(1,4,5,8,9)]
data_labels$Sorting_house_wish = as.factor(data_labels$Sorting_house_wish)

ggpairs(data, aes(color = factor(data_labels$Sorting_house_wish), alpha = 0.5)) # densità, correlazioni e scatterplot delle classi vere

# clustering --------------------------------------------------------------
library(mclust)
clust = Mclust(data)
summary(clust)
clust$BIC
# EEE,4     VVE,2     VEE,4 --> migliori modelli secondo BIC

par(mfrow=c(1,1))
plot(clust, what = "BIC", ylim = c(-22450, -22340))

mclustICL(data)
# EEE,1     EEV,1     EVE,1 --> migliori modelli secondo ICL

entropia <- -(clust$icl-clust$bic) # 366
entropia/(nrow(data)*log(4)) # entropia relativa

# grafici della classificazione
plot(clust, what = "classification")
plot(clust, what = "uncertainty")

# bontà della classificazione
(type.cluster <- clust$classification) # cluster assegnato
(type.true <- data_labels$Sorting_house_wish) # gruppo reale
miss_class = classError(+type.cluster, class=type.true)$misclassified # etichette mal classificate

# CER
(CER <- classError (type.cluster , type.true)[2])

# ARI
(ARI <- adjustedRandIndex(type.cluster, type.true))

library("caret")
cluster = as.factor(type.cluster)
levels(cluster)=c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin") # assegno i nomi delle etichette
true = as.factor(type.true)
confusionMatrix(cluster, true) # matrice di confusione

# medie e varianze dei cluster creati dal modello
summary(clust, parameters = TRUE)
# medie delle classi con etichette
data_labels %>%
  group_by(Sorting_house_wish) %>%
  summarise(
    Psy = mean(Psychopathy),
    Extrav = mean(Extraversion),
    Emstab =mean(EmStability),
    Intell = mean(Intellect)
  )
colMeans(data)

df<-cbind(data, type.true, type.cluster)
df$misclassified <- FALSE
df$misclassified[miss_class] <- TRUE

ggtrue = ggplot(df, aes(x=Extraversion, y=EmStability, color = factor(type.true))) +
  geom_point()+
  labs(subtitle = "(a) true classification",
       color = "True Class")

ggclass = ggplot(df, aes(x=Extraversion, y=EmStability, color = factor(type.cluster))) +
  geom_point()+
  geom_point(data = df %>% filter(misclassified==T),
             aes(x = Extraversion, y = EmStability),
             color = "black") +
  labs(subtitle = "(a) cluster classification",
       color = "Cluster Class")

library(patchwork)
ggtrue + ggclass

par(mfrow = c(1, 1))
uncerPlot (z=clust$z) # grafico dell’incertezza

# classification ----------------------------------------------------------
library(Rmixmod)
data.class <- unlist (data_labels [1]) # salvo le etichette

# considero solo i modelli con mixture probabilities variabili
listmod=c("Gaussian_pk_L_I","Gaussian_pk_Lk_I","Gaussian_pk_L_B","Gaussian_pk_Lk_B","Gaussian_pk_L_Bk",
                    "Gaussian_pk_Lk_Bk","Gaussian_pk_L_C","Gaussian_pk_Lk_C","Gaussian_pk_L_D_Ak_D","Gaussian_pk_Lk_D_Ak_D",
                   "Gaussian_pk_L_Dk_A_Dk","Gaussian_pk_Lk_Dk_A_Dk","Gaussian_pk_L_Ck","Gaussian_pk_Lk_Ck")
B = 50
blocks = seq(2,16,2)
results = data.frame(
  iter = numeric(),
  blocks = numeric(),
  model = character(),
  CV = numeric(),
  BIC = numeric()
)
set.seed(123)
for(b in 1:B){ # ripeto il test più volte
  for(h in blocks){ # provo diversi numeri di blocchi della CV
    res = mixmodLearn(data, data.class, 
                  models=mixmodGaussianModel(listModels = listmod),
                  criterion=c('CV','BIC'), nbCVBlocks=h)
    for (i in 1: length(res@models@listModels)){
      results = rbind(results,
        data.frame(
          iter = b,
          blocks = h,
          model = res@results[[i]]@model, # modello selezionato
          CV = res@results[[i]]@criterionValue[1], # CV del modello all'i-esima iterazione
          BIC = res@results[[i]]@criterionValue[2])) # BIC del modello all'i-esima iterazione
      }
  }
}

# calcolo medie di CV e BIC per ogni modello
means <- results %>%
  group_by(model) %>%
  summarize(CV_mean = mean(CV, na.rm = TRUE), BIC_mean = mean(BIC, na.rm = TRUE))

ggplot(means, aes(x=model, y=BIC_mean)) +
  geom_point(color="red")+
  geom_line(color="red",group=1,)+
  geom_vline(xintercept = which.min(means$BIC_mean), linetype = "dashed")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(y = "media BIC", x = "")

ggplot(means, aes(x=model, y=CV_mean)) +
  geom_point(color="green")+
  geom_line(color="green",group=1,)+
  geom_vline(xintercept = which.min(means$CV_mean), linetype = "dashed")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(y = "media CV", x = "")
  

# secondo step della classificazione
n = 50
noss = as.integer(nrow(data)*20/100) # prendo il 20% di oss

set.seed(123)
test.set.labels<-sample(1:nrow(data),noss) # uso le osservazioni come test-set 
# tolgo il test-set e alleno il classificatore
classificazione<-mixmodLearn(data[-test.set.labels,], data.class[-test.set.labels], 
                               models=mixmodGaussianModel(family="all",equal.proportions=FALSE),seed=123)
# utilizzo il test-set
previsione<- mixmodPredict(data=data[test.set.labels,], 
                             classificationRule=classificazione["bestResult"])

# quante etichette stimate corrispondono a quelle reali?
library("caret")
type.class = as.factor(previsione@partition)
levels(type.class)=c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin") # assegno i nomi delle etichette
true = factor(data.class[test.set.labels])
confusionMatrix(type.class, true)

# regressione -------------------------------------------------------------

library(flexmix)
set.seed(123)

# final = stepFlexmix(Sorting_house_wish ~ Psychopathy + Extraversion + EmStability + Intellect,
#             data = data_labels,
#             k = 1:4,
#             model = FLXMRmultinom(),
#             nrep = 5,verbose = TRUE, drop = F, unique = FALSE)
# --> cerca di stimare troppi parametri quindi è opportuno ridurre il numero di covariate


library(nnet)
# il nostro modello ha risposta multinomiale, tramite selezione backward
full = multinom(Sorting_house_wish ~ Psychopathy + Extraversion + EmStability + Intellect, 
                data = data_labels)
step_model = step(full, direction="backward") # tolgo EmStability

final = stepFlexmix(Sorting_house_wish ~  Psychopathy + Extraversion + Intellect,
                    data = data_labels,
                    k = 1:4, # testo il numero di componenti
                    model = FLXMRmultinom(), # le covariate entrano nella regressione
                    nrep = 5,verbose = TRUE, drop = F, unique = FALSE)

# ottengo come modello con minore ICL un modello a 1 componente, perciò non una mistura ma una regressione multinomiale
model = multinom(Sorting_house_wish ~ Psychopathy + Extraversion + Intellect, 
                 data = data_labels)
summary(model) # modello selezionato 

coefs <- summary(model)$coefficients # coefficienti del modello
serr <- summary(model)$standard.errors # std error del modello
