# text.columns function
# Christophe Cariou
# January 2016

require(stringr)
if(!require(stringr)) {
	install.packages("stringr")
	if (require(stringr)) { 
		print("text_par() has installed stringr with success")
	} else { 
		stop("text_par() could not install stringr")
	}
}






text.columns <- function(xR,yR,texte,family="serif",cex=1,col="black",posR=NULL,justR=NULL,
			w,lheight=NULL,bg=NULL) {


W <- w
Lignes <- "erreur"

# Separate paragraph and do one by one

Paragraph <- paste(str_split(texte,"[\n]")[[1]]," ",sep="") # Separate paragraph and add white space at the end
Paragraph <- str_replace_all(Paragraph,"[(]","<")
Paragraph <- str_replace_all(Paragraph,"[)]",">")
Paragraph <- str_replace_all(Paragraph,"[+]","#")
Paragraph <- str_replace_all(Paragraph,"[*]","ç")
Paragraph <- str_replace_all(Paragraph,"[?]","çç")


ParagraphN <- length(Paragraph) # Do paragraph by paragraph



for (j in 1: ParagraphN) {
	texte <- Paragraph[j]
	for (i in 1:200) { # Loop with break, 200 is not important
	if (texte!=" ") { # The break is here

		BetweenW <- (str_locate_all(texte," ")[[1]])[,1] # Locate all white space
		Max <- round(w/(2*(strwidth("a ",cex=cex,family=family))),0) # To reduce following vector 
		SentenceW <- str_sub(texte,1,BetweenW[1:Max]-1) # Compose differents sentences
		N <- length(SentenceW)
		SentenceW1 <- strwidth(SentenceW,cex=cex,family=family)
		id <- length(subset(SentenceW1, SentenceW1<w & SentenceW1!=0))
		Ligne <- SentenceW[id]
		Lignes <- c(Lignes,Ligne)
		texte <- paste(str_trim(str_replace(texte,Ligne,""))," ",sep="")

	} else { break }
	}
}

	
	LignesN <- length(Lignes)
	Lignes <- Lignes[2:LignesN]
	LignesN <- length(Lignes)



Lignes <- str_replace_all(Lignes,"<","(")
Lignes <- str_replace_all(Lignes,">",")")
Lignes <- str_replace_all(Lignes,"#","+")
Lignes <- str_replace_all(Lignes,"çç","?")

Lignes <- str_replace_all(Lignes,"ç","*")



# From xR / yR / posR / justR to x / y / pos

	h <- strheight("cc",cex=cex,family=family)
	if (is.null(lheight))  {lheight <- h*1.5}
	H <- (h+lheight)*(LignesN-1)



	# pos

		if (!is.null(justR)) {
			if (justR==2) {pos <- NULL}
			if (justR==3) {pos <- 2}
		} else {
			justR <- 1
			pos <- 4
		}

	# x/y

	if (is.null(posR)) {posR <- 1}
	
	y <- yR-H/2
	if (!is.null(posR)) {
		if (posR %in% c(3,4)) {y <- yR+H/2}
	}

	x <- xR
	if (!is.null(posR)) {
		if ((posR %in% c(2,3)) & (justR==1)) {x <- xR-W}
		if ((posR %in% c(1,4)) & (justR==3)) {x <- xR+W}
		if ((posR %in% c(1,4)) & (justR==2)) {x <- xR+W/2}
		if ((posR %in% c(2,3)) & (justR==2)) {x <- xR-W/2}
	}


# The y_list for the lignes

	
	y_start <- y+H/2
	y <- y_start-seq(0,(LignesN-1),1)*(h+lheight)

# With a background or not

	if (!is.null(bg)) {
		x_rect <- xR
		if (posR %in% c(2,3)) {x_rect <- xR-W}
		y_rect <- yR+h
		if (posR %in% c(3,4)) {y_rect <- yR+H+h}
		rect(x_rect, y_rect,x_rect+W,y_rect-H-lheight,col=bg,border=bg,lwd=2,lend="round")
	}
# Draw the text

text(x, y,Lignes,cex=cex,family=family,col=col,pos=pos,offset=0)


}
