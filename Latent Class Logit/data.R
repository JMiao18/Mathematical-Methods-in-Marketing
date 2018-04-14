## Assignment for Mathematical Models in Marketing
## Programmer: Jin Miao
## Following Freen & Srinivasan (1978), a mixed model that captures the advantages of Vector Model, Ideal-point Model and Part-worth Model will be implemented. Brand is catogorical so 4 dummies are created. Size, battery life and price levels are treated as continuous variables. Every two levels of storage and RAM have the same ratio  -- 2, which are natural transformed and used as continuous variables.

install.packages("lpSolve")
library(lpSolve)

data = read.csv(file = "M:/A Master of Science in Marketing Sciences/Mathematical Models in Marketing (Kohli)/latent/data_ipad.csv",head = TRUE)
attach(data)
summary(data)

## Modelling Step 1: Set up Training Set and Dev Set
random_factor = sample(1:15,137,replace = TRUE)
determinant_factor = (1:137)*15 - 15
## The index for Dev Set is the summation of Random and Deterministic.
index = random_factor + determinant_factor
dev = data[which(choice_set_id  %in%  index) ,]
train = data[which(!(choice_set_id  %in%  index)) ,]

## Model 1: Fit an aggregate model without any segment
## In the training set,
X1_train = subset(train, alternative_id_in_set == 1)
X2_train = subset(train, alternative_id_in_set == 2)
X3_train = subset(train, alternative_id_in_set == 3)
## In the test set,``
X1_test = subset(dev, alternative_id_in_set == 1)
X2_test = subset(dev, alternative_id_in_set == 2)
X3_test = subset(dev, alternative_id_in_set == 3)
## The 


train_set = cbind(X1_train[,5:22],X2_train[,5:22],X3_train[,5:22])
test_set = cbind(X1_test[,5:22],X2_test[,5:22],X3_test[,5:22])

## Multi-Nomial Logit Estimation

par = rnorm(18)
N = 1918
M1 = as.matrix(train_set)[,1:18] %*% par
M2 = as.matrix(train_set)[,19:36] %*% par
M3 = as.matrix(train_set)[,37:54] %*% par
M = cbind(exp(M1),exp(M2),exp(M3))
M = M / rowSums(M)

ll <- function(beta)
{
  res = 0
  for (i in 1:N)
  {
    res = res - log(M[choice[i]])	
  }
  return (res)
}

ML = nlm(ll,par,hessian = TRUE)
X1_predict = as.matrix(X1_test)[,5:22] %*% ML$estimate 
X2_predict = as.matrix(X2_test)[,5:22] %*% ML$estimate 
X3_predict = as.matrix(X3_test)[,5:22] %*% ML$estimate 


attach(X1)
## Create dummy variables for each level of each attribute
Brand_Nexus = ifelse( Nexus == 1 , 1 , 0)
Brand_Kindle = ifelse( Kindle == 1 , 1 , 0)
Brand_iPad = ifelse( iPad == 1 , 1 , 0)
Brand_Galaxy = ifelse( Galaxy == 1 , 1 , 0)
Brand_Surface = ifelse( Surface == 1 , 1 , 0)

Size_a7 = ifelse(a7inch == 1 , 1 , 0)
Size_a8 = ifelse(a8inch == 1 , 1 , 0)
Size_a9 = ifelse(a9inch == 1 , 1 , 0)
Size_a10 = ifelse(a10inch == 1 , 1 , 0)

Size = Size_a7 * 7
Size [Size_a8 == 1] = 8
Size [Size_a9 == 1] = 9
Size [Size_a10 == 1] = 10

Storage_128gb = ifelse(a128gb == 1 , 1 , 0)
Storage_64gb = ifelse(a128gb == 0 & a64gb == 1, 1 , 0)
Storage_32gb = ifelse(a64gb == 0 & a32gb == 1 , 1 , 0)
Storage_16gb = ifelse(a32gb == 0 , 1 , 0)

Storage = Storage_16gb * 16
Storage[Storage_32gb == 1] = 32
Storage[Storage_64gb == 1] = 64
Storage[Storage_128gb == 1] = 128

RAM_4gb = ifelse(a4gb == 1 , 1 , 0)
RAM_2gb = ifelse(a4gb == 0 & a2gb == 1 , 1 , 0)
RAM_1gb = ifelse(a2gb == 0 & a1gb == 1, 1 , 0)

RAM = RAM_1gb 
RAM[RAM_2gb == 1] = 2
RAM[RAM_4gb == 1] = 4

Battery_9h = ifelse(a9h == 1 , 1 , 0)
Battery_8h = ifelse(a9h == 0 & a8h == 1, 1 , 0)
Battery_7h = ifelse(a8h == 0 & a7h == 1, 1 , 0)

Battery = Battery_7h * 7
Battery[Battery_8h == 1] = 8
Battery[Battery_9h == 1] = 9

Price_169 = ifelse(p169 == 1 , 1 , 0)
Price_199 = ifelse(p169 == 0 & p199 == 1 , 1 , 0)
Price_299 = ifelse(p199 == 0 & p299 == 1 , 1 , 0)
Price_399 = ifelse(p299 == 0 & p399 == 1 , 1 , 0)
Price_499 = ifelse(p399 == 0 , 1 , 0)
Price = Price_169 * 169
Price[Price_199 == 1] = 199
Price[Price_299 == 1] = 299
Price[Price_399 == 1] = 399
Price[Price_499 == 1] = 499

X1_brand = cbind(Brand_Kindle,Brand_iPad,Brand_Galaxy,Brand_Surface)
X1_size = Size
X1_storage = log(Storage)
X1_ram = log(RAM)
X1_battery = Battery
X1_price = Price
X1 = cbind(X1_brand, X1_size, X1_storage, X1_ram, X1_battery, X1_price)
detach(X1)

attach(X2)
## Create dummy variables for each level of each attribute
Brand_Nexus = ifelse( Nexus == 1 , 1 , 0)
Brand_Kindle = ifelse( Kindle == 1 , 1 , 0)
Brand_iPad = ifelse( iPad == 1 , 1 , 0)
Brand_Galaxy = ifelse( Galaxy == 1 , 1 , 0)
Brand_Surface = ifelse( Surface == 1 , 1 , 0)

Size_a7 = ifelse(a7inch == 1 , 1 , 0)
Size_a8 = ifelse(a8inch == 1 , 1 , 0)
Size_a9 = ifelse(a9inch == 1 , 1 , 0)
Size_a10 = ifelse(a10inch == 1 , 1 , 0)

Size = Size_a7 * 7
Size [Size_a8 == 1] = 8
Size [Size_a9 == 1] = 9
Size [Size_a10 == 1] = 10

Storage_128gb = ifelse(a128gb == 1 , 1 , 0)
Storage_64gb = ifelse(a128gb == 0 & a64gb == 1, 1 , 0)
Storage_32gb = ifelse(a64gb == 0 & a32gb == 1 , 1 , 0)
Storage_16gb = ifelse(a32gb == 0 , 1 , 0)

Storage = Storage_16gb * 16
Storage[Storage_32gb == 1] = 32
Storage[Storage_64gb == 1] = 64
Storage[Storage_128gb == 1] = 128

RAM_4gb = ifelse(a4gb == 1 , 1 , 0)
RAM_2gb = ifelse(a4gb == 0 & a2gb == 1 , 1 , 0)
RAM_1gb = ifelse(a2gb == 0 & a1gb == 1, 1 , 0)

RAM = RAM_1gb 
RAM[RAM_2gb == 1] = 2
RAM[RAM_4gb == 1] = 4

Battery_9h = ifelse(a9h == 1 , 1 , 0)
Battery_8h = ifelse(a9h == 0 & a8h == 1, 1 , 0)
Battery_7h = ifelse(a8h == 0 & a7h == 1, 1 , 0)

Battery = Battery_7h * 7
Battery[Battery_8h == 1] = 8
Battery[Battery_9h == 1] = 9

Price_169 = ifelse(p169 == 1 , 1 , 0)
Price_199 = ifelse(p169 == 0 & p199 == 1 , 1 , 0)
Price_299 = ifelse(p199 == 0 & p299 == 1 , 1 , 0)
Price_399 = ifelse(p299 == 0 & p399 == 1 , 1 , 0)
Price_499 = ifelse(p399 == 0 , 1 , 0)
Price = Price_169 * 169
Price[Price_199 == 1] = 199
Price[Price_299 == 1] = 299
Price[Price_399 == 1] = 399
Price[Price_499 == 1] = 499

X2_brand = cbind(Brand_Kindle,Brand_iPad,Brand_Galaxy,Brand_Surface)
X2_size = Size
X2_storage = log(Storage)
X2_ram = log(RAM)
X2_battery = Battery
X2_price = Price
X2 = cbind(X2_brand, X2_size, X2_storage, X2_ram, X2_battery, X2_price)
detach(X2)

attach(X3)
## Create dummy variables for each level of each attribute
Brand_Nexus = ifelse( Nexus == 1 , 1 , 0)
Brand_Kindle = ifelse( Kindle == 1 , 1 , 0)
Brand_iPad = ifelse( iPad == 1 , 1 , 0)
Brand_Galaxy = ifelse( Galaxy == 1 , 1 , 0)
Brand_Surface = ifelse( Surface == 1 , 1 , 0)

Size_a7 = ifelse(a7inch == 1 , 1 , 0)
Size_a8 = ifelse(a8inch == 1 , 1 , 0)
Size_a9 = ifelse(a9inch == 1 , 1 , 0)
Size_a10 = ifelse(a10inch == 1 , 1 , 0)

Size = Size_a7 * 7
Size [Size_a8 == 1] = 8
Size [Size_a9 == 1] = 9
Size [Size_a10 == 1] = 10

Storage_128gb = ifelse(a128gb == 1 , 1 , 0)
Storage_64gb = ifelse(a128gb == 0 & a64gb == 1, 1 , 0)
Storage_32gb = ifelse(a64gb == 0 & a32gb == 1 , 1 , 0)
Storage_16gb = ifelse(a32gb == 0 , 1 , 0)

Storage = Storage_16gb * 16
Storage[Storage_32gb == 1] = 32
Storage[Storage_64gb == 1] = 64
Storage[Storage_128gb == 1] = 128

RAM_4gb = ifelse(a4gb == 1 , 1 , 0)
RAM_2gb = ifelse(a4gb == 0 & a2gb == 1 , 1 , 0)
RAM_1gb = ifelse(a2gb == 0 & a1gb == 1, 1 , 0)

RAM = RAM_1gb 
RAM[RAM_2gb == 1] = 2
RAM[RAM_4gb == 1] = 4

Battery_9h = ifelse(a9h == 1 , 1 , 0)
Battery_8h = ifelse(a9h == 0 & a8h == 1, 1 , 0)
Battery_7h = ifelse(a8h == 0 & a7h == 1, 1 , 0)

Battery = Battery_7h * 7
Battery[Battery_8h == 1] = 8
Battery[Battery_9h == 1] = 9

Price_169 = ifelse(p169 == 1 , 1 , 0)
Price_199 = ifelse(p169 == 0 & p199 == 1 , 1 , 0)
Price_299 = ifelse(p199 == 0 & p299 == 1 , 1 , 0)
Price_399 = ifelse(p299 == 0 & p399 == 1 , 1 , 0)
Price_499 = ifelse(p399 == 0 , 1 , 0)
Price = Price_169 * 169
Price[Price_199 == 1] = 199
Price[Price_299 == 1] = 299
Price[Price_399 == 1] = 399
Price[Price_499 == 1] = 499

X3_brand = cbind(Brand_Kindle,Brand_iPad,Brand_Galaxy,Brand_Surface)
X3_size = Size
X3_storage = log(Storage)
X3_ram = log(RAM)
X3_battery = Battery
X3_price = Price
X3 = cbind(X3_brand, X3_size, X3_storage, X3_ram, X3_battery, X3_price)
detach(X3)

X = cbind(X1,X2,X3)
X1sample = X1[1:15, ]
X2sample = X2[1:15, ]
X3sample = X3[1:15, ]
Xsample = X[1:15, ]

choice = data$choice
choice_new = c()
for (i in 1 : length(choice)/3)
{
  choice_new[i] = choice[3 * (i - 1) + 1]
}
samplechoice = choice_new[1:15]
