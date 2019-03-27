where <- function(name, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    stop("Can't find ", name, call. = FALSE)
  } else if(exists(name, envir = env, inherits = FALSE)) {
    env
  } else {
    where(name, parent.env(env))
  }
}

# Performanz ----

# > Performanz vs. Flexibilität ---- 

x <- 0L 
for(i in 1:1e6) x <- x + 1 

# dynamic lookup
a <- 1
f <- function() {
  g <- function() {
    print(a)
    assign("a", 2, envir = parent.frame())
    print(a)
    a <- 3
    print(a)
  }
  g()
}
f()

`{`
where("(")
search()

# lazy evaluation

# > Profiling ----

# system.time
x <- runif(100)
sqrt(x)
exp(log(x) / 2)
system.time(sqrt(x))
system.time(x^0.5)

n <- 1e6
system.time(for(ii in 1:n) sqrt(x))
system.time(for(ii in 1:n) x^0.5)

ptm <- proc.time()
for(ii in 1:n) sqrt(x)
proc.time() - ptm

# microbenchmark
library(microbenchmark)
x <- runif(100)
microbenchmark(sqrt(x))
microbenchmark(sqrt(x), x^0.5) 

microbenchmark(sqrt(x), x^0.5, times = 1000) 

# Rprof
?Rprof ## for sourcefiles
# devtools::install_github("hadley/lineprof")

tmp <- tempfile()
library(here)
source(here("04a_profiling-source-01.R"))
Rprof(tmp, interval = .01)
f()
Rprof(NULL)
summaryRprof(tmp)

# > Optimieren ----

# Vorhandene Lösungen?

# sparsam programmieren
x <- 1:100
microbenchmark(any(x == 10), 10 %in% x) 

x <- rep(letters[1:3], each = 50)
microbenchmark(factor(x), factor(x, levels = c("a", "b", "c")))

df <- data.frame(x = runif(100), y = runif(100))

microbenchmark({
  sub <- df[sample(nrow(df), 10), ]
  cor(sub$x, sub$y)
},{
  i <- sample(nrow(df), 10)
  cor(df$x[i], df$y[i])
})

# vektorisieren
x <- matrix(runif(100), ncol = 5)
r <- rep(1, nrow(x))

microbenchmark(
  {
    res <- numeric(ncol(x))
    for(i in 1:ncol(x)) res[i] <- sum(x[, i])
  },
  apply(x, 2, sum),
  colSums(x),
  r %*% x
)

all.equal(c(r %*% x), colSums(x))


# - rowsum
x <- matrix(runif(100), ncol = 5)
group <- sample(1:8, 20, TRUE)
microbenchmark(
  aggregate(x, list(group), sum),
  rowsum(x, group)
)
rowsum
rowsum.default

# - integer-Matrix
mx <- matrix(runif(1e6), ncol = 2)
bmx<- matrix(NA, nrow(mx) / 2, ncol(mx))
for(i in 1:nrow(bmx)) bmx[i, ] <- mx[2 * i - 1, ] * mx[2 * i, ]
all.equal(bmx,
          mx[seq(1, nrow(mx), by = 2), ] * mx[seq(2, nrow(mx), by = 2), ])

# - vectorized if
hit <- NA 
for(i in 1:1e6) if(runif(1) < .3) hit[i] <- TRUE 
fasthit <- ifelse(runif(1e6) < .3, TRUE, NA)

# Kopieren vermeiden
n <- 1000
microbenchmark(
  {
    vec <- numeric(0) # oder c(), oder NULL
    for(i in 1:n) vec <- c(vec, i)
  },
  {
    vec <- numeric(n)
    for(i in 1:n) vec[i] <- i
  },
  vec <- 1:n # vektorisiert
)

# - cbind/rbind
n <- 10
my.df <- data.frame(a = character(0), 
                    b = numeric(0))
for(i in 1:n){
  my.df<- rbind(my.df, data.frame(a = sample(letters, 1, replace = TRUE),
                                  b = runif(1)))
}
my.df

# - rbind naiv
f1 <- function(n) {
  my.df <- data.frame(a = character(0), 
                      b = numeric(0))
  for(i in 1:n){
    this.N <- rpois(1, 10)
    my.df<- rbind(my.df, data.frame(a = sample(letters, this.N, replace = TRUE),
                                    b = runif(this.N)))
  }
  my.df
}

# - rbind schlauer
f2 <- function(n) {
  current.N <- 10 * n
  my.df<- data.frame(a = character(current.N),
                     b = numeric(current.N))
  count <- 0
  for(i in 1:n) {
    this.N <- rpois(1,10)
    if(count + this.N > current.N) { 
      old.df <- my.df
      current.N <- round(1.5 * (current.N + this.N))
      my.df<- data.frame(a = character(current.N), b = numeric(current.N))
      my.df[1:count, ] <- old.df[1:count,]
    }
    my.df[count + 1:this.N, ] <- data.frame(a = sample(letters, this.N, replace = TRUE),
                                            b = runif(this.N))
    count<- count + this.N
  }
  my.df <- my.df[1:count,]
  my.df
}

# - rbind: do.call
f3 <- function(n) {
  my.list<- vector('list', n)
  for(i in 1:n) { 
    this.N <- rpois(1, 10)
    my.list[[i]] <- data.frame(a = sample(letters, this.N, replace = TRUE),
                               b = runif(this.N))
  }
  my.df<- do.call('rbind', my.list) 
  my.df
}
microbenchmark(f1(10), f2(10), f3(10))

# Byte-compile
loadNamespace

my_sum <- function(x){
  out <- 0
  for(ii in 1:length(x)) out <- out + x[ii]
} 
bcmp_my_sum <- compiler::cmpfun(my_sum)

x <- 1:1000
microbenchmark(my_sum(x), bcmp_my_sum(x), sum(x))

# Paralleles programmieren

library(parallel)
detectCores()
phys_cores <- detectCores(logical = FALSE) 

# - start
cl <- makeCluster(phys_cores, type = "PSOCK") 
cl
typeof(cl)
length(cl)
cl[[1]]
names(cl[[1]])
stopCluster(cl)

# - auf Cluster durchführen
clusterApply(cl, rep(1000, phys_cores), fun = function(x) mean(rnorm(x, mean = 5)))
clusterApply(cl, rep(1000, 25), fun = function(x) mean(rnorm(x, mean = 5))) 

# - library
library(mvtnorm)
clusterEvalQ(cl, exists("dmvnorm"))
clusterEvalQ(cl, {
  library(mvtnorm)
  exists("dmvnorm")
})

# - auf Cluster kopieren
x <- 1:10
clusterEvalQ(cl, x) 
clusterExport(cl, "x")
clusterEvalQ(cl, x) 

# > Auslagern ----

library(Rcpp)

cppFunction('int add(int x, int y, int z) {
               int sum = x + y + z;
               return sum;
             }') 
add
add(1, 2, 3)

# Vektor-input
cppFunction('int sumC(NumericVector x) {
               int n = x.size();
               double tot = 0;
               for(int i = 0; i < n; i++){
                 tot += x[i];
               }
               return tot;
             }') 

sumR <- function(x) {
  tot <- 0
  for(i in 1:length(x)) tot <- tot + x[i]
  tot
}

x <- 1:100
microbenchmark(sumR(x), sumC(x), sum(x))

# SourceCpp
sourceCpp(here("04b_sourceCpp.cpp"))

# Rcpp sugar
cppFunction('int sumC2(NumericVector x) {
               return sum(x);
             }')
microbenchmark(sumR(x), sumC2(x), sumC(x), sum(x))

