# Uso Paraiba: scalebar(c(-38.5,-8.7), mapa = mapa)
# Uso Paraiba: scalebar(shape = "pb", mapa = mapa)
# Uso Joao Pessoa: scalebar(c(-35,-7.25), mapa = mapa)
# Uso Joao Pessoa: scalebar(shape="jp", mapa = mapa)

scalebar <- function(loc = NA,shape = "pb", unit="km",division.cex=.8, mapa=mapa) {
  
  if(any(is.na(loc))==T){
    if(shape=="pb"){
      #Para Paraíba (Localização)
      loc = c(-38.5,-8.7)
    }
    if(shape == "jp"){
      #Para João Pessoa (Localização)
      loc = c(-35,-7.25)
      
    }
  }
  
    if(missing(loc)) stop("loc is missing")
  maximo <- sapply(1:length(mapa), function(i){
    max(mapa[[i]][[1]][1][[1]][,1])
  })
  minimo <- sapply(1:length(mapa), function(i){
    min(mapa[[i]][[1]][1][[1]][,1])
  })
  length<-as.numeric(
    sprintf(
      "%0.2f", abs(abs(max(maximo))-abs(min(minimo)))/2
    )
  )
  x <- c(0,length/c(4,2,4/3,1),length*1.1)+loc[1]
  y <- c(0,length/(10*3:1))+loc[2]
  cols <- rep(c("black","white"),2)
  for (i in 1:4) rect(x[i],y[1],x[i+1],y[2],col=cols[i])
  for (i in 1:5) segments(x[i],y[2],x[i],y[3])
  labels <- x[c(1)]-loc[1]
  # labels<- append(labels, sprintf("%0f",length*111.12/2))
  # labels<- append(labels,paste(sprintf("%0f",length*111.12),unit))
  labels<- append(labels, round(length*111.12/2))
  labels<- append(labels,paste(round(length*111.12),unit))
  text(x[c(1,3,5)],y[4],labels=labels,adj=.5,cex=division.cex)
}
# A localidade da Escala ficou manual, colocá-la de acordo com as
# coordenadas do SHAPE.


scalebarggplot <- function(loc = NA, shape = "pb", unit = "km", division.cex = 0.8, mapa = NULL) {
  # Define localizações padrão se loc não for fornecido
  if (any(is.na(loc))) {
    if (shape == "pb") {
      loc <- c(-38.5, -8.7)  # Localização para Paraíba
    }
    if (shape == "jp") {
      loc <- c(-35, -7.25)  # Localização para João Pessoa
    }
  }
  
  # Verifica se o mapa foi passado
  if (is.null(mapa)) stop("Mapa precisa ser fornecido")
  
  # Determina o comprimento da escala
  maximo <- sapply(1:length(mapa), function(i) {
    max(mapa[[i]][[1]][1][[1]][, 1])
  })
  
  minimo <- sapply(1:length(mapa), function(i) {
    min(mapa[[i]][[1]][1][[1]][, 1])
  })
  
  length <- abs(abs(max(maximo)) - abs(min(minimo))) / 2
  x <- c(0, length / c(4, 2, 4 / 3, 1), length * 1.1) + loc[1]
  y <- c(0, length / (10 * 3:1)) + loc[2]
  
  cols <- rep(c("black", "white"), 2)
  
  # Gráficos de retângulos e linhas
  scalebar_grob <- gList()
  for (i in 1:4) {
    scalebar_grob <- gList(
      scalebar_grob,
      rectGrob(x = unit(x[i], "npc"), y = unit(y[1], "npc"),
               width = unit(x[i + 1] - x[i], "npc"), height = unit(y[2] - y[1], "npc"),
               gp = gpar(fill = cols[i]))
    )
  }
  
  for (i in 1:5) {
    scalebar_grob <- gList(
      scalebar_grob,
      segmentsGrob(x0 = unit(x[i], "npc"), y0 = unit(y[2], "npc"),
                   x1 = unit(x[i], "npc"), y1 = unit(y[3], "npc"))
    )
  }
  
  labels <- c(round(length * 111.12 / 2), paste(round(length * 111.12), unit))
  scalebar_grob <- gList(
    scalebar_grob,
    textGrob(labels[1], x = unit(x[1], "npc"), y = unit(y[4], "npc"), gp = gpar(cex = division.cex)),
    textGrob(labels[2], x = unit(x[3], "npc"), y = unit(y[4], "npc"), gp = gpar(cex = division.cex))
  )
  
  return(scalebar_grob)
}
