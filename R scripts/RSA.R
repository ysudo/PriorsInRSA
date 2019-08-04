# --- RSA --- #

# Lexicon
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

# Literal listener
L0 <- function(w, u, prob) {
	(sem(w, u) * prob(w))/((sem('a', u) * prob('a')) + (sem('sbna', u) * prob('sbna')))
}

# Pragmatic listener
L <- function(n, w, u, prob) {	
	if (n==0) {
		return(L0(w, u, prob))
	} else {
		return((S(n, u, w, prob) * prob(w)/((S(n, u, 'a', prob) * prob('a'))+(S(n, u, 'sbna', prob) * prob('sbna')))))
	}
}

# Speaker
lambda <- 1

S <- function(n, u, w, prob) {
	return(exp(lambda * log(L(n-1, w, u, prob)))/(exp(lambda * log(L(n-1, w, 'all', prob))) + exp(lambda * log(L(n-1, w, 'some', prob)))))
}

# --- Example Priors --- #

# Uniform prior
unip <- function(x) {
	return(0.5)
}

# Skewed prior
skew <- function(x) {
	if (x=='a') {
		return(0.9)
	} else if (x=='sbna') {
		return(0.1)
	} else {
		print('something is wrong')
	}
}

# Inverse skewed prior
invskew <- function(x) {
	if (x=='a') {
		return(0.1)
	} else if (x=='sbna') {
		return(0.9)
	} else {
		print('something is wrong')
	}
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

L1to5 <- sapply(p_grid, function(w) sapply(1:5, function(x) L(x, 'a', 'some', all_probs(w))))
L1to5df <- data.frame(L1to5)
names(L1to5df) <- p_grid

L1to5dfplot <- melt(L1to5df)
L1to5dfplot$n <- 1:5


library(ggplot2)

cls <- rev(colorRampPalette(c("red", "white"))(6))
cls <- cls[-1]

png(file="RSA-L.png")
ggplot(L1to5dfplot, aes(as.numeric(as.character(variable)), as.numeric(value), group=as.factor(n))) + geom_line(aes(color=as.factor(n))) + scale_color_manual(values=cls) + theme_minimal() + ggtitle("Listener") + xlab("P(all)") + ylab("L(all|'some')") + theme(legend.position=c(0,1), legend.justification=c(0,1), legend.title=element_blank()) + ylim(c(0,1)) + xlim(c(0,1))
dev.off()

# Speaker

S1to5 <- sapply(p_grid, function(w) sapply(1:5, function(x) S(x, 'some', 'a', all_probs(w))))
S1to5df <- data.frame(S1to5)
names(S1to5df) <- p_grid

S1to5dfplot <- melt(S1to5df)
S1to5dfplot$n <- 1:5

cls <- rev(colorRampPalette(c("royalblue", "white"))(6))
cls <- cls[-1]

png(file="RSA-S.png")
ggplot(S1to5dfplot, aes(as.numeric(as.character(variable)), as.numeric(value), group=as.factor(n))) + geom_line(aes(color=as.factor(n))) + scale_color_manual(values=cls) + theme_minimal() + ggtitle("Speaker") + xlab("P(all)") + ylab("S('some'|all)") + theme(legend.position=c(0,1), legend.justification=c(0,1), legend.title=element_blank()) + ylim(c(0,1)) + xlim(c(0,1))
dev.off()




