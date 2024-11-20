#Uso: compassRose(-35.1,-5.9,rot=0, cex=0.4)

compassRose<-function(x,y,rot=0,cex=0.4) {
  oldcex<-par(cex=cex)
  mheight<-strheight("M")
  xylim<-par("usr")
  plotdim<-par("pin")
  xmult<-(xylim[2]-xylim[1])/(xylim[4]-xylim[3])*plotdim[2]/plotdim[1]
  point.angles<-seq(0,2*pi,by=pi/4)+pi*rot/180
  crspans<-rep(c(mheight*3,mheight/2),length.out=9)
  xpoints<-cos(point.angles)*crspans*xmult+x
  ypoints<-sin(point.angles)*crspans+y
  for(point in 1:8) {
    pcol<-ifelse(point%%2,"black","white")
    polygon(c(xpoints[c(point,point+1)],x),c(ypoints[c(point,point+1)],y),col=pcol)
  }
  txtxpoints<-cos(point.angles[c(1,3,5,7)])*1.2*crspans[1]*xmult+x
  txtypoints<-sin(point.angles[c(1,3,5,7)])*1.2*crspans[1]+y
  text(txtxpoints,txtypoints,c("L","N","O","S"))
  par(oldcex)
}


library(grid)

compassRoseGrob <- function(x, y, rot = 0, cex = 0.4, scale = 1) {
  # Ajustes iniciais
  mheight <- unit(0.05 * scale, "npc") # Altura base ajustada pela escala
  crspans <- rep(c(mheight * 3, mheight / 2), length.out = 9) # Tamanho proporcional
  point_angles <- seq(0, 2 * pi, by = pi / 4) + pi * rot / 180
  xpoints <- cos(point_angles)*.70 * crspans + x
  ypoints <- sin(point_angles)*.90 * crspans + y 
  
  # Criar os polígonos
  polygons <- gList()
  for (i in 1:8) {
    color <- ifelse(i %% 2 == 1, "black", "white")
    polygons <- gList(
      polygons,
      polygonGrob(
        x = unit(c(xpoints[i], xpoints[i + 1], x), "npc"),
        y = unit(c(ypoints[i], ypoints[i + 1], y), "npc"),
        gp = gpar(fill = color, col = "black")
      )
    )
  }
  
  # Adicionar os textos
  txt_angles <- point_angles[c(1, 3, 5, 7)]
  txt_x <- cos(txt_angles) * crspans[1] * .9 + x
  txt_y <- sin(txt_angles) * crspans[1] * 1.1 + y
  labels <- c("L", "N", "O", "S")
  texts <- gList()
  for (i in seq_along(labels)) {
    texts <- gList(
      texts,
      textGrob(
        labels[i],
        x = unit(txt_x[i], "npc"),
        y = unit(txt_y[i], "npc"),
        gp = gpar(cex = cex * scale, col = "black") # Escala ajustada
      )
    )
  }
  
  # Combinar tudo em um grobTree
  grobTree(polygons, texts)
}
