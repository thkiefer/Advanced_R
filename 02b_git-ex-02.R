# Thomas Kiefer, 18.03.2019
# IRT Analyse von Rasch-Daten

# 1 Einlesen ----
lf <- list.files(here("roh"))
resp <- read.csv2(here("roh", lf)) # oder file.path(here("roh"), lf)

# 2 Vorbereiten ----
head(resp)
apply(resp, 2, table)
colMeans(resp)

# 3 Analyse ----
mod1 <- TAM::tam.mml(resp = resp)
wle1 <- TAM::tam.wle(mod1)

# 4 Schreiben ----
if(schreiben){
  write.csv2(wle1, here("ergebnis", paste0("personen_", Sys.Date(), ".csv")), 
             row.names = FALSE, na = "")
}
#
