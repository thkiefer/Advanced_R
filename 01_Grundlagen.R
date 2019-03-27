# Motivation ----

# organisatorisches
# Warm Up
RXKCD::getXKCD(1954) # Danke, Gabi!
base::rowsum
tensor::tensor

# Ressourcen

# Grundlagen ----

str(as.numeric(Sys.Date()))

# 1) Datenformat, 1 dimensional ----

# > Vektoren ----

dbl <- c(1, 2.5, 4.5) 
int <- c(1L, 6L, 10L)
log <- c(TRUE, FALSE, T, F)
chr <- c("mein", "string", "vector")
 
c(1, c(2, 3)) 

# Typen

typeof(int)
is.integer(int)
is.numeric(int)
is.atomic(int)
is.double(int)

# NA

str(NA)
str(c(NA, 3.4))
str(c(NA_real_, 3.4)) # NA, NA_integer_, NA_real_, NA_character_
str(NA_character_)

# Verbindungslogic (en: Coercion): logical -> integer -> double -> character

c(TRUE, "a")
as.numeric(c(TRUE, TRUE, FALSE))
sum(c(TRUE, TRUE, FALSE))
as.logical(c(0, 1, 2.4))
as.logical(c("a", "1", "b"))

# > Listen ----

x <- list(dbl, int, log, chr)
str(x)
typeof(x)
is.list(x)

y <- list(list(list(1))) 
str(y)
is.recursive(y)
length(y)

# Kombinieren von Listen
list(list(1, 2), c(3, 4))
c(list(1, 2), c(3, 4))


# > Attribute ----

x <- 1:10
attr(x, "my_attr") <- "mein Attributtext"
attributes(x)
attr(x, "my_attr")

x <- structure(1:10, "my_attr" = "mein Attributtext")

attributes(x[1]) 

# Element-Namen
x <- c(a = 1, "b" = 2)

x <- 1:3
names(x) <- c("a")
x

names(x) <- c("a", "b", "c")
x

setNames(x, c("d", "e", "f"))

unname(x)
names(x) <- NULL

# Faktoren
x <- factor(c("a", "b", "c", "a"))
x
class(x)
levels(x)

x[2] <- "d"
x

c(x, factor("d"))

table(factor(c("m", "m", "m"), levels = c("m", "f")))

z <- read.csv(text = "value\n12\n1\n.\n9")
z
str(z)
as.numeric(z$value)
as.numeric(as.character(z$value))

z <- read.csv(text = "value\n12\n1\n.\n9", stringsAsFactors = FALSE)
str(z)

z <- read.csv(text = "value\n12\n1\n.\n9", na.strings = ".")
str(z)

# 2) Datenformat, 2- bzw. n-dimensional ----

# > Matrix/Array ----

a <- matrix(1:6, ncol = 3, nrow = 2) 
b <- array(1:12, dim = c(2, 3, 2))

c <- 1:6
dim(c) <- c(3, 2)

d <- structure(1:6, dim = c(3, 2))

dim(c) <- c(3, 3)
matrix(1:6, ncol = 3, nrow = 3) 

# Eigenschaften von Matrizen 
a
length(a)
ncol(a)
nrow(a)

rownames(a) <- c("A", "B")
colnames(a) <- c("a", "b", "c")

dimnames(b) <- list(c("one", "two"), c("a", "b", "c"), c("A", "B"))
b

names(b)
names(b) <- c("A", "b")
c(b)

class(a)
class(b)
is.array(a)
is.matrix(b)

attributes(a)

str(1:3)
str(matrix(1:3, ncol = 1))
str(matrix(1:3, nrow = 1))
str(array(1:3, dim = 3))

l <- structure(list(1:3, "a", TRUE, 1.0), dim = c(2, 2))
l 
l[1, 2]

# > Data frame ----

# Erstellung geht mit data.frame
df <- data.frame(x = 1:3, y = c("a", "b", "c"))
str(df)

df <- data.frame(x = 1:3, y = c("a", "b", "c"),
                 stringsAsFactors = FALSE)
str(df)

# Attribute von Data frames sind 
attributes(df)
names(df)
class(df)
row.names(df)
 
rownames(df) # warum geht das?
rownames
dimnames.data.frame


# Die Kombinierung von Dateframes geht via cbind und rbind (das geht für 
cbind(df, data.frame(z = 1:3))
str(cbind(df, z = 1:3)) 
rbind(df, data.frame(y = "z", x = 10))

str(data.frame(cind(a = 1:2, b = c("a", "b")))) 

str(data.frame(a = 1:2, b = c("a", "b"), stringsAsFactors = FALSE))

# Feature
df$z <- list(1:2, 1:3, 1:4)
df

data.frame(x = 1:3, y = c("a", "b", "c"), z = list(1:2, 1:3, 1:4))
data.frame(x = 1:3, y = c("a", "b", "c"), z = I(list(1:2, 1:3, 1:4)))


# 3) Zugriffslogik ----

# > Vektoren ----

x <- c(2.1, 4.2, 3.3, 5.4)
x[c(3, 1)]
x[order(x)]
sort.default
x[c(1, 1)]
x[c(2.1, 2.9)] 

x[-c(1, 2)]
x[c(-1, 2)]

x[c(TRUE, TRUE, FALSE, FALSE)]
x[x > 3]
x[c(TRUE, FALSE)]
x[c(TRUE, FALSE, TRUE)]
x[c(TRUE, FALSE, NA, TRUE)]

x[]
 
x[0]
str(0)

y <- setNames(x, c("a", "b", "c", "d"))
y[c("a", "b")]
y[c("a", "a")]

y <- setNames(x, c("abc", "def", "geh", "ijk"))
y[c("a", "d")]

# > Matrix ----

mx <- matrix(1:9, nrow = 3, dimnames = list(NULL, c("A", "B", "C")))
mx[1:2, ] 
mx[c(TRUE, FALSE, TRUE), c("B", "A")]  
mx[0, -2]
dim(mx[1, -2]) 

vals <- outer(1:5, 1:5, FUN = "paste", sep = ",")
vals
vals[c(4, 15)]

sel_mx <- rbind(c(1, 1),
                c(3, 1),
                c(2, 4))
vals[sel_mx]
which(vals == "4,1")
which(vals == "4,1", arr.ind = TRUE)
 
# > Data Frame (& Liste) ----

df <- data.frame(x = 1:3, y = 3:1, z = c("a", "b", "c"))

df[df$x == 2,] 
str(df[df$x == 2,])
str(df[, c("x", "z")]) 

str(df[c("x", "z")]) 

str(df["x"])
str(df[, "x"]) 

# weitere Einelementige zugriffe mit [[]] und $
l <- list(a = 1, b = 2)
l[1]
l[[1]]
l[["a"]]
l[[c("a", "b")]] 

l <- list(a = 1, b = list(c = list(d = 1)))
l[[c("b", "c")]] #  entspricht l[["b"]][["c"]]

 ## $ mit Partiell-Erkennung, [[ nicht
l <- list(abc = 1, def = 2)
l$a
l[["a"]]
l$a <- 3
l

# > Struktur-vereinfachend/-erhaltend ----

# Vektor
x <- setNames(1:4, letters[1:4])
x
x[1]
x[[1]]
x$a

# Faktor
f <- factor(letters[1:4])
f
f[1]
f[[1]]
f[1, drop = TRUE]
f$a

# Matrix
mx <- matrix(1:9, nrow = 3, dimnames = list(letters[1:3], LETTERS[1:3]))
mx
str(mx[1, ])
str(mx[1, , drop = FALSE])
mx$a

# Liste
l <- list(a = 1, b = 2, c = 3, d = 4)
l
l[1]
l[[1]]
l$a

# Data Frame
df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
str(df[1])
str(df[, 1])
str(df[, 1, drop = FALSE])
str(df[[1]])
str(df$a)

# > Zugriff und Zuweisung ----

# Mit Integereinträgen
x <- 1:5
x[c(1, 2)] <- 2:3
x

x[-1] <- 3:1 
x

x[c(1, 1)] <- 2:3
x

x[c(1, NA)] <- c(1, 2) 

x[c(TRUE, FALSE, NA)] # auffüllen
x[c(TRUE, FALSE, NA, TRUE, FALSE)]
x[c(TRUE, FALSE, NA)] <- 1 
x[c(TRUE, FALSE, NA)]
x

df <- data.frame(a = c(1, 10, NA))
df
df$a < 5
df$a[df$a < 5] <- 0 
df

# leer 
str(lapply(df, '*', 2))
df[] <- lapply(df, '*', 2)
df <- lapply(df, '*', 2)

# Anwendung ----

# Rekodierung
x <- c("m", "f", "u", "f", "f", "m", "m")
table(c("m" = "bekannt", "f" = "bekannt", "u" = "unbekannt")[x])
table(c("m" = "male", "f" = "female", "u" = NA)[x], useNA = "always")

# Matching
grades <- c(1, 2, 2, 3, 1)
info <- data.frame(
  grade = 3:1,
  desc = c("Excellent", "Good", "Poor"),
  fail = c(F, F, T)
)
info[match(grades, info$grade), ]

rownames(info) <- info$grade
info[as.character(grades), ]

# Bootstrap-Stichproben ziehen
df <- data.frame(x = rep(1:3, each = 2), y = 6:1, z = letters[1:6])
set.seed(10)
df[sample(nrow(df), replace = TRUE), ]

# Sortieren
x <- c("b", "c", "a")
order(x)
x[order(x)]

# Ausweiten eines Datensatzes nach Gewichten
df <- data.frame(x = c(2, 4, 1), y = c(9, 11, 6), weight = c(3, 5, 1))
df
rep(1:nrow(df), df$weight)
df[rep(1:nrow(df), df$weight), ]

# Entfernen von Elementen
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df[c("x", "y")]
df
df$z <- NULL
df

# mehrstufige Logische Operatoren
mtcars[mtcars$gear == 5 & mtcars$cyl == 4, ] # |, &, ! (don't use &&, ||)
                                             # logic laws: !(X & Y) == !X | !Y
                                             #             !(X | Y) == !X & !Y
                                             # Aufgabe: simplify !((X & Y) | !Z)
                                             # !(X & Y) & !!Z == !X | !Y & Z



