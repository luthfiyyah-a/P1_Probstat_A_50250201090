# Praktikum 1 Probstat (A)
# Luthfiyyah hanifah amari
# 5025201090


# nomor 1
#-- a
x = 3
p = 0.20
dgeom(x, prob = p)
dgeom(x, prob = p)
dgeom(x, prob = p)

#-- b
n = 1000
prob = 0.20
mean(rgeom(n, prob) == 3)
mean(rgeom(n, prob) == 3)
mean(rgeom(n, prob) == 3)

#-- c
# berdasarkan beberapa kali percobaan yang saya lakukan, untuk soal (b), hasil yang didapatkan dapat berbeda2. untuk soal (a) hasil yang didapat sama (presisi).

#-- d
library(dplyr)
library(ggplot2)

p = 0.2
n = 3

data.frame(x = 0:10, prob = dgeom(x = 0:10, prob = p)) %>% 
  mutate(Failures = ifelse(x == n, n, "other")) %>% 
  ggplot(aes(x = factor(x), y = prob, fill = Failures)) +
  geom_col() + 
  geom_text(
    aes(label = round(prob, 2), y = prob +0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) + 
  labs(title = "probibabilitas dari X = 3 distribusi geometri",
       x = "jumlah kegagalan hingga dapat keberhasilan pertama",
       y = "probabilitas")

#-- e
p = 0.20
mean = (1-p)/p
mean

varians = (1-p)/p^2
varians

# nomor 2

#-- a
# dbinom(x, n, prob)
dbinom(x=4, size=12, prob=0.2)

#-- b
x <- rbinom(1000, size=20, prob=0.2)
hist(x, main="distirbusi binomial", xlab="n=20 dan p=0.2")

#-- c
n = 20
p = 0.2
q = 1 - p

mean = n*p
varians = sqrt(n*p*q)

# nomor 3

#-- a
dpois(x=6, lambda=4.5)

#-- b
rpois(n=365, lambda=4.5)

#-- d
V = rpois(n=365, lambda=4.5)
mean(V)
sd(V)

# nomor 4

#-- a
# df = degree of freedom = v
v = 10
dchisq(x=2, df=v)

#-- b
x <- rchisq(n=100, df=10)
hist(x)

#-- c
x <- rchisq(n=100, df=10)
mean(x)
var(x)


# nomor 5

#-- a
# f(x) = 3e ^(-3x)

#-- b
x <- rexp(10, 3)
hist(x)
x <- rexp(100, 3)
hist(x)
x <- rexp(1000, 3)
hist(x)
x <- rexp(10000, 3)
hist(x)

#-- c
set.seed(1)

mean(rexp(100, 3))
sd(rexp(100, 3))


# nomor 6

#-- b
x <- rnorm(n=100, mean=50, sd=8)
hist(x, main="5025201090_Luthfiyyah_Probstat_A_DNhistogram")

#-- c
x <- rnorm(n=100, mean=50, sd=8)
var(x)
