
library("support.CEs")

des1 <- rotation.design(attribute.names = list(
  Salida = c("6:45am","6:55am", "7:05am"),
  VarTiempo = c("5", "10", "15"),
  ProbCong = c("Baja", "Media", "Alta")),
  nalternatives = 3, nblocks =3 , row.renames = FALSE,
  randomize = TRUE, seed = 987)

des1

questionnaire(choice.experiment.design = des1)


des2 <- Lma.design(attribute.names = list(
  Salida = c("6:45am","6:55am", "7:05am"),
  VarTiempo = c("5", "10", "15"),
  ProbCong = c("Baja", "Media", "Alta")),
  nalternatives = 3, nblocks = 3, row.renames = FALSE, seed = 987)

des2

questionnaire(choice.experiment.design = des2)

des3 <- rotation.design(attribute.names = list(
  Salida = c("17:15pm","17:25pm", "17:35pm"),
  VarTiempo = c("5", "10", "15"),
  ProbCong = c("Baja", "Media", "Alta")),
  nalternatives = 3, nblocks =3 , row.renames = FALSE,
  randomize = TRUE, seed = 987)

des3

questionnaire(choice.experiment.design = des3)


data("syn.res2")
