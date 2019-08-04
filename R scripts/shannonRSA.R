# --- Stalnakerian RSA --- #

sem <- function(w, u) {
	if (u=='some') { # If the utterance is 'some', it's always true
		return(1)
	} else if (u=='all') {
		if (w=='a') {
			return(1)
		} else if (w=='sbna') {
			return(0)
		} else {
		print('something is wrong')			
		}
	} else {
		print('something is wrong')
	}
}

surprisal <- function(p) {
	if (p==0) {
		return(0)
	} else {
		return(-log(p)*p)
	}
}


# Literal listener
L0 <- function(w, u, prob) {
	(sem(w, u) * surprisal(prob(w)))/((sem('a', u) * surprisal(prob('a'))) + (sem('sbna', u) * surprisal(prob('sbna'))))
}

# Pragmatic listener
L <- function(n, w, u, prob) {	
	if (n==0) {
		return(L0(w, u, prob))
	} else {
		return((S(n, u, w, prob) * surprisal(prob(w)))/((S(n, u, 'a', prob) * surprisal(prob('a')))+(S(n, u, 'sbna', prob) * surprisal(prob('sbna')))))
	}
}

# Speaker
lambda <- 1
k <- 0.3

eta <- function(u, n, prob) {
	return((L(n, 'a', u, prob) * -log(prob('a'))) + (L(n, 'sbna', u, prob) * -log(prob('sbna'))))
}

S <- function(n, u, w, prob) {
	return(exp(lambda * (log(L(n-1, w, u, prob)) + (k*eta(u, n-1, prob))))/(exp(lambda * (log(L(n-1, w, 'all', prob)) + (k*eta('all', n-1, prob)))) + exp(lambda * (log(L(n-1, w, 'some', prob)) + (k*eta('some', n-1, prob))))))
}



# --- Graphs --- #

# grid with 20 chunks
p_grid <- seq(from=0, to=1, length.out=21)
p_grid <- p_grid[-1]
p_grid <- p_grid[-length(p_grid)]


# Make functions that generate prior distributions
all_probs <- function(y) {
	function(x) {
		if (x=='a') {
			return(y)
		} else {
			return(1-y)
		}
	}
}


library(reshape2)

# Listener

#L1to3 <- sapply(p_grid, function(w) sapply(1:3, function(x) L(x, 'a', 'some', all_probs(w))))
L1to4 <- sapply(p_grid, function(w) sapply(1:4, function(x) L(x, 'a', 'some', all_probs(w))))


L1to4df <- data.frame(L1to4)
names(L1to4df) <- as.numeric(p_grid)

L1to4dfplot <- melt(L1to4df)
L1to4dfplot$n <- 1:4


library(ggplot2)

cls <- rev(colorRampPalette(c("red", "white"))(5))
cls <- cls[-1]

png(file="shannonRSA-L.png")
ggplot(L1to4dfplot, aes(as.numeric(as.character(variable)), as.numeric(value), group=as.factor(n))) + geom_line(aes(color=as.factor(n))) + scale_color_manual(values=cls) + theme_minimal() + ggtitle("Listener") + xlab("P(all)") + ylab("L(all|'some')") + theme(legend.position=c(0,1), legend.justification=c(0,1), legend.title=element_blank()) + ylim(c(0,1))
dev.off()




# Speaker

S1to4 <- sapply(p_grid, function(w) sapply(1:4, function(x) S(x, 'some', 'a', all_probs(w))))
S1to4df <- data.frame(S1to4)
names(S1to4df) <- p_grid

S1to4dfplot <- melt(S1to4df)
S1to4dfplot$n <- 1:4

cls <- rev(colorRampPalette(c("royalblue", "white"))(5))
cls <- cls[-1]

png(file="shannonRSA-S.png")
ggplot(S1to4dfplot, aes(as.numeric(as.character(variable)), as.numeric(value), group=as.factor(n))) + geom_line(aes(color=as.factor(n))) + scale_color_manual(values=cls) + theme_minimal() + ggtitle("Speaker") + xlab("P(all)") + ylab("S('some'|all)") + theme(legend.position=c(0,1), legend.justification=c(0,1), legend.title=element_blank()) + ylim(c(0,1)) + xlim(c(0,1))
dev.off()


