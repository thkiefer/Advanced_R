f <- function(x) x^2
formals(f)
body(f)
environment(f)

sum
formals(sum)

# Suchpfad ----

# > lexical scoping ----

x <- 10
x
search()

rm(x)

# name masking
f <- function() {
  x <- 1
  y <- 1
  c(x, y)
}

f()
rm(f)

#
f <- function() {

  y <- 1
  c(x, y)
}

f()

# Dynamisches Nachschlagen
x <- 2 
f()

rm(f, x)

#
x <- 2 
f <- function() {
  y <- 3
  g <- function() {
    z <- 4
    c(x, y, z)
  }
  g()
}
f()
rm(f, x)

#
f <- function(x) {
  y <- 3
  function() { # return
    c(x, y)    
  }
}

g <- f(1)
g()

rm(f, g)

# 
f <- function(y) {
  z <- 3
  function() {
    c(x, y, z)
  }
}

g <- f(1)
g() 

x <- 2
g()

x <- 3
g()

h <- f(5)
h()

#
environment(g)
ls(envir = environment(g)) # beachte: x existiert hier nicht

rm(f, g, h)

# Funktionen und Variablen

f <- function(x) x + 1
g <- function() {
  f <- function(x) x * 2
  f(10)
}
f(10)
g()

rm(f, g)


f <- function(x) x + 1
g <- function() {
  f <- 3
  f(f) # R ignoriert alles was keine Funktion ist durch das '('
}

g()
rm(f, g)

# Neuanfang

f <- function() {
  if(!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  a
}
f()
f()

# > Umgebungen ----

# Grundlegendes
e <- new.env()

e$a <- 1
e$b <- "a"
e$c <- FALSE
e$d <- 1:3

# Referenzsemantik
f <- function(x) {
  x$d <- 4:6
  invisible()
}
f(e)
e$d

#
ls(name = e)
e[[1]]      
e$a
ls.str(e)

#
parent.env(e) 
              
identical(parent.env(e), globalenv())
search()
parent.env(globalenv())
parent.frame() 

identical(globalenv(), environment())
identical(baseenv(), as.environment("package:base"))
identical(emptyenv(), parent.env(as.environment("package:base")))

# Zugriffsoperatoren
get("d", e)
x <- 1
get("x", e)

# Entfernen
rm("a", envir = e)
ls.str(e)

# 
exists("x", envir = e)
exists("x", envir = e, inherits = FALSE)

# Zusammenfassung
where <- function(name, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    stop("Can't find ", name, call. = FALSE)
  } else if(exists(name, envir = env, inherits = FALSE)) {
    env
  } else {
    where(name, parent.env(env))
  }
}

where("x")
where("b", e)
try(where("a", e))

# > Funktions-Umgebungen / closures ---- 

# Einschließende Umgebungen
environment(where)
ls(environment(where))
rm("x")
ls(environment(where))


# Bindende Umgebungen 
e$f <- function() 1
environment(e$f)
where("f", e)

# Einschließende vs. Bindende Umgebungen 
environment(sd) 
where("var")

sd(1:10)
var <- function(x, na.rm = TRUE) 100
sd(1:10)
        

# Ausführende Umgebung
g <- function(x) {
  if(!exists("a", inherits = FALSE)) {
    a <- 1
  } else { 
    a <- a + 1
  }
  a
}
g(10)
g(10)

#
f <- function(x) {
  a <- 2
  x + a
}
x <- 2
f()
f(4) # hier wird x festgelegt

#
plus <- function(x) {
  function(y) x + y
}
f <- plus(1) # hier wird x festgelegt
new_e <- environment(f)
ls(new_e)
new_e$x

# Aufrufende Umgebung
f <- function() {
  x <- 10
  function() {
    x
  }
}

i <- f()
x <- 20
i()

f <- function() {
  x <- 10
  function() {
    e1 <- get("x", environment())
    e2 <- get("x", parent.frame())
    list("defined" = e1, "called" = e2)
  }
}
i <- f()
x <- 20
i()
x <- 11
i()

# Elemente von Funktionen ----

# > Argumente ----

# formelle vs. tatsächliche Argumente und wie werden die aufeinander abgebildet

f <- function(abcdef, bcde1, bcde2) {
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}
f(1, 2, 3)
f(1, 2, "abcdef" = 3)
f(1, 2, "a" = 3)
f(1, 2, "b" = 3)
formals(f)

do.call(f, list(1, 2, 3))

# Default
f <- function(abcdef, bcde1, bcde2 = 3) {
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}

f(1, 2)

# Lazy Evaluation -- defaults können von anderen Argumenten abhängen
f <- function(abcdef, bcde1, bcde2 = 3 + abcdef) {
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}

f(1, 2)

# Nicht zu empfehlen, aber möglich
f <- function(abcdef = bcde1 / 4, bcde1, bcde2 = 3 + abcdef) {
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}
f(bcde1 = 2)

# Nicht zu empfehlen, aber möglich
f <- function(abcdef, bcde1, bcde2 = efg) {
  efg <- abcdef + bcde1^2
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}
f(1, 2)

# mehr zu lazy eval
f <- function(x) {
  10
}
f(stop("give me an error"))

f <- function(x) {
  force(x)
  10
}
f(stop("give me an error"))

f <- function(x = ls()) {
  a <- 10
  x
}
f()
f(ls())

# - logischen Operatoren
x <- NULL
if(x > 0){}
if(!is.null(x) && x > 0){}
if(!is.null(x)) if(x > 0){}
if(is.null(x)) stop("x is null")
!is.null(x) || stop("x is null")


# ... 
?plot
?plot.default

f <- function(...) {
  names(list(...))
}
f("a" = 1, "b" = 2)

sum(1, 2, 3, NA, na.mr = TRUE)
sum(1, 2, 3, NA, na.rm = TRUE)
  
# > Infix- und Ersetzungs-Funktionen ----

# Infix
'%+%' <- function(a, b) paste0(a, b) ## must have %
"new " %+% "string"

# Ersetzung
'second<-' <- function(x, value) {
  x[2] <- value
  x
}
x <- 1:5
second(x) <- 4
x
`[<-`

'modify<-' <- function(x, index, value) {
  x[index] <- value
  x
}

# > return, on.exit, closures ----

# return
f <- function(x) {
  if(x < 10) 0 else 10
}
f(5)
f(12)

f <- function(x) {
  if(x < 10) invisible(0) else invisible(10)
}
a <- f(5)
a
(f(5))

# on.exit
my_plot <- function() { 
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(pch = 19)
  plot(cars)
}
my_plot()
par("pch")

# closures
f <- function() {
  x <- 0
  function() {
    x <<- x + 1
    x
  }
}
i <- f()
j <- f()
i()
j()

# - reduziere Code beim Programmieren vieler ähnlicher Funktionen
power <- function(exponent) {
  function(x) {
    x^exponent
  }
}
zero <- power(0)
ident <- power(1)
square <- power(2)
zero(5)
ident(5)
square(5)

# - Liste von Funktionen
f <- function(x, ...) {
  l <- list("mean" = mean(x, ..., na.rm = TRUE),
            "median" = median(x, ...,  na.rm = TRUE),
            "sd" = sd(x,  ..., na.rm = TRUE))
  lapply(l, function(f, ...) f(x))  
}
f(1:10)

x <- 1:10
l <- list("mean" = mean, "median" = median, "sd" = sd)
lapply(l, function(f) f(x, na.rm = TRUE))

# - anonyme Aufrufe
(function(x) x + 3)(10) 

# - Funktionale Programmierung
Negate
?Negate

# Objektorientierung (S3) ----

# base-Typen
f <- function() {}
typeof(f)
is.function(f)
is.closure(f)
typeof(sum)
is.primitive(sum) # andere: is.logical, is.numeric, is.character
typeof("a")

# die meisten Objekte sind S3-Objekte 
df <- data.frame("x" = 1:10, y = letters[1:10])
class(df)
is.object(df)
is.object("a") # base-Typen sind keine Objekte
is.object(df$x) # base-Typen sind keine Objekte
is.object(df$y) # Faktoren sind keine base-Typen
isS4(df) # data.frame ist kein S4-Objekt

# generische Funktionen
mean
sum 
?"internal generic"

methods("plot")
methods(class = "data.frame")

# Klasse festlegen
x <- structure(list(), class = "my_class")

x <- list()
class(x) <- "my_class"

class(x)
inherits(x, "my_class")

glm

# Konstruktor
my_class <- function(x) {
  if(!is.numeric(x)) stop("x must be numeric")
  structure(list(x), class = "my_class")
}

df
class(df) <- "lm"
print(df)
         
str(df)  

# Eigene Generische Funktionen
my_generic <- function(x) UseMethod("my_generic")

x <- my_class(1:5)
my_generic.my_class <- function(x) "meine Klasse"

my_generic(x)
my_generic(df)

my_generic.default <- function(x) "eine andere Klasse" 
class(df)
my_generic(df)

my_generic.data.frame <- function(x) "Data Frame" 
class(df) <- "data.frame"
my_generic(df)

class(df) <- c("lm", "data.frame")
print(df)
my_generic(df)
print.data.frame(df)

# S4
library("lme4")
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
isS4(fm1)
is(fm1)
class?merMod # man muss mit setClass eine solche Klasse erstellen und mit new() 
             # ein Objekt initialisieren
slotNames(fm1) # slots functionieren wie listen
fm1@call
slot(fm1, "call")

# Debugging ----

# > debugging ----

# nützlich
ii <- 0
ii
for (ii in 1:100) if (ii == 35) message()
ii

for (ii in 1:100) if (ii == 35) stop()
ii

# Error Inspector / traceback() 
f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) "a" + d
f(10)
traceback()

source(here("03a_error-source-01.R"))
f(10)
traceback()

# rerun with debug / options(error = browser)
 ## [n]ext step in the function, 
 ## [s]tep into function to work line by line, 
 ## [f]inish current loop
 ## [Q]uit
f(10)

# (optional) in R-Konsole
options("error" = browser) 
f(10)
options("error" = NULL)

# breakpoints / browser() 
source(here("03b_error-source-02.R"))
f(10)

# debug
debug(f)
f(10)
undebug(f)

# > Condition handling ----

# conditions
f <- function(x) if(!is.numeric(x)) stop("x muss numerisch sein")
g <- function(x) if(!is.numeric(x)) warning("x sollte numerisch sein; ich mach' das")
h <- function(x) if(!is.numeric(x)) message("x ist ", typeof(x))
i <- function(x) if(!is.numeric(x)) cat("x ist", typeof(x)) # ggf. print

f("a") 
g("a")
h("a")
i("a")

# handling
options("warn" = 2) ## default 0
g("a")
options("warn" = 0)

try(f("a"))
try(f("a"), silent = TRUE)
try({
  a <- 1
  b <- "a"
  a + b
})
a
b

# - Behandeln von Fehlern
res <- try(f("a"), silent = TRUE)
class(res)
class(try(1 + 2))
if(inherits(res, "try-error")) message("do something else")

#
res <- NULL
try(res <- f("a"), silent = TRUE)
res

# tryCatch(expr, message = expr, warning = expr, error = expr, finally = expr)

# Unterdrücken von Ausgabe
options("warn" = 0)
try(f("a"), silent = TRUE)
suppressWarnings(g("a"))
suppressMessages(h("a"))
suppressMessages(i("a")) 

# > Defensives programmieren ----
 