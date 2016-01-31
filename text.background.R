# Add Text with Background Color to a Plot
# By Christophe Cariou, April 2015
# text_bg use strwidth() and strheight() to draw a rect() background for a text()


text.background <- function(
			x,y,texte,family="serif",cex=1,pos=NULL,col="white", # text() arguments
			bg="black",lwd=NULL # rect() arguments
) {

	# Width and Height of texte with adjustement x-height
	
		Note_w <- strwidth(texte,cex=cex,family=family)
		Note_h <- strheight(texte,cex=cex,family=family)*1.5

	# Default values for rectangle
		x0 <- x-Note_w/2
		y0 <- y-Note_h/2
		x1 <- x+Note_w/2
		y1 <- y+Note_h/2

		if (!is.null(pos)) {
		if (pos==1) {y0 <- y-Note_h}
		if (pos==3) {y0 <- y}
		if (pos==2) {
			x0 <- x-Note_w
			x1 <- x
			}
		if (pos==4) {
			x0 <- x
			x1 <- x+Note_w
			}
		}
	
		
		if (!is.null(lwd)) { lwd=lwd } else { lwd <- 0 }

	# Background with text
	rect(x0,y0,x1,y1,col=bg,border=bg, lwd=lwd)
	text(x,y, texte,cex=cex,family=family,col=col,pos=pos,offset=0)
}

