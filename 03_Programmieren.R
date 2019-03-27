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

f <- function() {

  y <- 1
  c(x, y)
}

f()

# note: sucht im Suchpfad zum Zeitpunkt des calls, nicht zum Zeitpunkt 
# der Definition --  sollte wenn möglich vermieden werden (siehe debugging)
x <- 2 
f()

rm(f, x)

# gilt auch bei geschachtelten Funktionen
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

# gilt auch bei closures (Funktionen die von andere Funktionen erzeugt werden)
# auf die komme ich gleich nochmal
f <- function(x) {
  y <- 3
  function() { ## letztes Statement in der Funktion wird als Ergebnis zurück
    c(x, y)    ## returnt; hier: eine Funktion
  }
}

g <- f(1) ## erzeuge Funktion für die gilt, dass x = 1 und y = 3 ist
g()

rm(f, g)

# oh je
f <- function(y) {
  z <- 3
  function() { ## letztes Statement in der Funktion wird als Ergebnis zurück
    c(x, y, z) ## returnt; hier: eine Funktion
  }
}

g <- f(1) ## erzeuge Funktion für die gilt, dass y = 1 und z = 3 ist
g() 

x <- 2
g()

x <- 3
g()

h <- f(5) ## erzeuge Funktion für die gilt, dass y = 1 und z = 3 ist
h()


environment(g)
ls(envir = environment(g)) # beachte: x existiert hier nicht

rm(f, g, h)

# gilt auch unabhängig vom zu suchenden Typ 
# --> suchpfad beachten 
# --> maskieren von Funktionen (reihenfolge des pakete ladens kann Bedeutung 
#  haben)
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

# > environments ----

# neues env
e <- new.env()

e$a <- 1
e$b <- "a"
e$c <- FALSE
e$d <- 1:3

# env have reference semantics: when modifying a binding (obj in an env) the 
# objects are not copied but are modified in place
f <- function(x) {
  x$d <- 4:6
  invisible()
}
f(e)
e$d

#
ls(name = e) # every object in an environment has a unique name (this is different from lists)
e[[1]]       # there is no order among the objects (this is different from lists)
e$a
ls.str(e)

parent.env(e) # an environment has a parent (this is different from lists)
              # parents are used to implement lexical scoping

identical(parent.env(e), globalenv())
search()
parent.env(globalenv())
parent.frame() # gives you the calling environment
identical(globalenv(), environment())
identical(baseenv(), as.environment("package:base"))
identical(emptyenv(), parent.env(as.environment("package:base")))

# $, [[ only work in the env; use get() for regular scoping rules
get("d", e)
x <- 1
get("x", e)

# deletion via rm; not <-NULL (this is different from lists)
rm("a", envir = e)
ls.str(e)

# exists similar to get but returns boolean; possible to only look in  e
exists("x", envir = e)
exists("x", envir = e, inherits = FALSE)

#
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

# > function environments / closures ---- 

# enclosing environment
environment(where)
ls(environment(where))
rm("x")
ls(environment(where))


# binding environment # where is the name of the method bound?
e$f <- function() 1
environment(e$f)
where("f", e)

environment(sd) 
where("var")

sd(1:10) # uses var
var <- function(x, na.rm = TRUE) 100 # if we overwrite var
sd(1:10) ## sd still uses var from the enclosing env. not the binding env 
         ## namespaces werden wir später noch benutzen, wenn wir Pakete bauen

# execution environment
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

f <- function(x) {
  a <- 2
  x + a
}
x <- 2
f()
f(4) # hier wird x festgelegt

plus <- function(x) {
  function(y) x + y
}
f <- plus(1) # hier wird x festgelegt
new_e <- environment(f)
ls(new_e)
new_e$x

# calling environment
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


# formelle vs. tatsächliche argumente und wie werden die aufeinander gemappt

f <- function(abcdef, bcde1, bcde2) {
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}
f(1, 2, 3)
f(1, 2, "abcdef" = 3)
f(1, 2, "a" = 3)
f(1, 2, "b" = 3)
formals(f)

 ## use positional matching only for the first one or two arguments; often those 
 ## are the arguments without defaults

do.call(f, list(1, 2, 3))

 ## defaults
f <- function(abcdef, bcde1, bcde2 = 3) {
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}

f(1, 2)

 ## defaults can be functions of previous arguments (due to lazy evaluation)
f <- function(abcdef, bcde1, bcde2 = 3 + abcdef) {
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}

f(1, 2)

 ## nicht empfehlenswert, aber möglich auch vorhergehende Argumente in beziehungne 
 ## zu späteren Argumenten zusetzen
f <- function(abcdef = bcde1 / 4, bcde1, bcde2 = 3 + abcdef) {
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}

f(bcde1 = 2)

f <- function(abcdef, bcde1, bcde2 = efg) {
  efg <- abcdef + bcde1^2
  c("a" = abcdef, "b1" = bcde1, "b2" = bcde2) 
}
f(1, 2)

# mehr zu lazy eval
f <- function(x) {
  10
}
f(stop("give me an error")) # imput x wird nie ausgewertet

f <- function(x) {
  force(x) # umgehe lazy eval
  10
}
f(stop("give me an error"))

 ##
f <- function(x = ls()) {
  a <- 10
  x
}
f()
f(ls())

 ## lazyness usefull in logical operations
x <- NULL
if(x > 0){}
if(!is.null(x) && x > 0){}
if(!is.null(x)) if(x > 0){}
if(is.null(x)) stop("x is null")
!is.null(x) || stop("x is null")


# ... 
?plot
?plot.default # flexible but not transparent

 ## am einfachsten mit list(...) abfangen
f <- function(...) {
  names(list(...))
}
f("a" = 1, "b" = 2)
sum(1, 2, 3, NA, na.mr = TRUE) #misspelled arguments still fall into ...
sum(1, 2, 3, NA, na.rm = TRUE) #misspelled arguments still fall into ...
  
# > infix/ replacement ----

# infix
'%+%' <- function(a, b) paste0(a, b) ## must have %
"new " %+% "string"

# replacement
'second<-' <- function(x, value) {
  x[2] <- value
  x
}
x <- 1:5
second(x) <- 4 # not .primitive -> copy semantics
x

'modify<-' <- function(x, index, value) { # additional arguments go in between
  x[index] <- value
  x
}

# > return, on.exit, closures ----

# last expression evaluated is returned
f <- function(x) {
  if(x < 10) 0 else 10
}
f(5)
f(12)

# invisible
f <- function(x) {
  if(x < 10) invisible(0) else invisible(10)
}
a <- f(5)
a
(f(5))


# side - effects

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

 ## reduziere Code beim programmieren vieler ähnlicher Funktionen
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

 ## Funktionen als liste von functionen
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


 ## use functions as a return argument of (
 ## to call it anonymously
(function(x) x + 3)(10) 

 ## functional programming
Negate
?Negate

# Objektorientierung (S3) ----

# base-typen
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

# um herauszufinden, ob eine Funktion eine generische Funktion ist, schaun wir 
mean
sum # ?"internal generic"

# für eine Klasse ist es aufgabe der generischen Funktion die richtige Methode 

# welche methoden es für eine generic gibt, findet man mit methods()
methods("plot")
methods(class = "data.frame")

# define classes
x <- structure(list(), class = "my_class")
 ## oder
x <- list()
class(x) <- "my_class"

class(x)
inherits(x, "my_class")

# Es können auch mehrere Klassen vorgegeben werden wodurch eine Vererbungsstruktur 
glm

# Häufig existiert eine Konstruktor-Funktion (z.B. auch Modellanpassung)
my_class <- function(x) {
  if(!is.numeric(x)) stop("x must be numeric")
  structure(list(x), class = "my_class")
} # oder siehe glm

# darüber hinaus gibt es keinen check ob die Klassenzuweisung sinnvoll ist
df
class(df) <- "lm"
print(df) # kann nicht ausgegeben werden, da die print-Methode der lm-Klasse hier 
          # keinen Sinn ergibt, aber 
str(df)   # die Informationen sind noch alle da

# neue generics: use UseMethod()
my_generic <- function(x) UseMethod("my_generic")

x <- my_class(1:5)
my_generic.my_class <- function(x) "meine Klasse"

my_generic(x)
my_generic(df) # usemethod sucht nach funktionen paste0(generic, ".", c(class(x), "default"))
               # hist ist class(df) "lm" -- haben wir oben so gesetzt
               # für die gibt's die generic nicht und eine default-generic gibt's auch nicht

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

# nützlich: Schleifenparameter sind Variablen in der globalen Umgebung
ii <- 0
ii
for (ii in 1:100) if (ii == 35) message()
ii

for (ii in 1:100) if (ii == 35) stop()
ii

# error inspector / traceback() 
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
 ## open interactive session where error occured
 ## Rstudio menu "Debug" -> On Error -> Break in Code
 ## [n]ext step in the function, 
 ## [s]tep into function to work line by line, 
 ## [f]inish current loop
 ## [Q]uit
f(10)
 ## R-Console
options("error" = browser) 
f(10)
options("error" = NULL) # reset in console

# breakpoints / browser() 
source(here("03b_error-source-02.R"))
f(10)

# set breakpoint at beginning of function
debug(f)
f(10)
undebug(f)

# > communicating / condition handling ----

# neben Fehlern werden von Funktionen oft noch warnings oder messages ausgeworfen
f <- function(x) if(!is.numeric(x)) stop("x muss numerisch sein")
g <- function(x) if(!is.numeric(x)) warning("x sollte numerisch sein; ich handle das")
h <- function(x) if(!is.numeric(x)) message("x ist ", typeof(x))
i <- function(x) if(!is.numeric(x)) cat("x ist ", typeof(x)) # oder print(paste0("x ist ", typeof(x)))

f("a") 
g("a")
h("a")
i("a")

options("warn" = 2) ## default 0
g("a")

try(f("a"))
try(f("a"), silent = TRUE)
try({
  a <- 1
  b <- "a"
  a + b
})
a
b

# handling
res <- try(f("a"), silent = TRUE)
class(res)
class(try(1 + 2))
if(inherits(res, "try-error")) message("do something else")

# default-werte
res <- NULL
try(res <- f("a"), silent = TRUE)
res

 ## tryCatch(expr, message = expr, warning = expr, error = expr, finally = expr)

# andere handlings
options("warn" = 0)
try(f("a"), silent = TRUE)
suppressWarnings(g("a"))
suppressMessages(h("a"))
suppressMessages(i("a")) 

# defensiv programming
 