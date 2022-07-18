#Q1 Please	solve	the	following	questions
#a.If	X	follows	N(0,	1),	then	to	find	P(X	≤	1.20).

pnorm(q=1.25, mean = 0, sd= 1, lower.tail = T)

#b.If	X	follows	an	exponential	distribution	with	parameter	λ	=	5,	then	to	compute	P(X	> 2).

1 - pexp(q=2, 5)

#c.If	X	∼ N(µ	=	1,	σ	=	2),	then	to	find	F(1.8)	=	P(X	≤	1.8).

pnorm(q=1.8, mean = 1, sd= 2, lower.tail = T)

#d.Find	the	20th	percentile,	that	is,	the	value	q	such	that	P(X	≤	q)	=	.20 for	X	from	N(0,1).

qnorm(p=0.2, mean = 0, sd= 1, lower.tail = T)

#e.Find	the	80th	percentile for	N(2,	3).

qnorm(p=0.8, mean = 2, sd= 3, lower.tail = T)

#f.Generate	100	random	numbers	from	the	normal	distribution	N(0,	1),	and	plot	the	
#pdf for	−2.5 ≤	x	≤	2.5.

set.seed(1)
x <- rnorm(n = 100, mean = 0, sd = 1)
curve(dt(x, df=10), from=-2.5, to=2.5,
      ylab = "Probability", 
      main = "100 Random Numbers Normal	 Distribution") 


#g.Draw	a	plot	of	the	N(0,1)	PDF and CDF 

curve(dnorm(x),
      xlim = c(-4, 4),
      ylab = "Density", 
      main = "Standard Normal Density Function") 

curve(pnorm(x), 
      xlim = c(-4, 4), 
      ylab = "Probability", 
      main = "Standard Normal Cumulative Distribution Function")



#h.Compute	density	at	x=-1.96,	x=0	and	x=1.96.

dnorm(x = c(-1.96, 0, 1.96))




#Q2 Suppose	you	have	a	biased	coin	that	has	a	probability	of	0.75 of	coming	up	heads
#a.Find	the	probability	of	getting	8 heads	in	20 tosses	of	this	coin.

dbinom(x = 8, size = 20, prob = 0.75)

#b.Find the	probability	of	getting	at	most	8 heads	in	20 tosses.

dbinom(x = 8, size = 20, prob = 0.75)+ dbinom(x = 7, size = 20, prob = 0.75)+
  dbinom(x = 6, size = 20, prob = 0.75)+ dbinom(x = 5, size = 20, prob = 0.75)+
  dbinom(x = 4, size = 20, prob = 0.75)+ dbinom(x = 3, size = 20, prob = 0.75)+
  dbinom(x = 2, size = 20, prob = 0.75)+ dbinom(x = 1, size = 20, prob = 0.75)+
  dbinom(x = 0, size = 20, prob = 0.75)

#c.Find	the	0.25	quantile.
set.seed(1)
trails <- rbinom(n = 1, size = 20, prob = .75)
quantile(trails,0.25)

#Q3.	Suppose	the	East region	of	Turkey experiences	about	4 earthquakes	a	year.	Assume	
#occurrences	follow	a	Poisson	distribution.	What	is	the	probability	of	3	earthquakes	in	a	
#given	two	year?	

dpois(3 , 8) 

#Q4.	 Suppose	you	have	a	biased	coin	that	has	a	probability	of	0.6 of	coming	up	heads. Toss	
#this	coin	20 times.	

#a+b. Lets	run	this	experiment	10	times	and	record	the	number	of	successes. , lower.tail= TRUE (set.seed(0), this	sets	the	seed	for	the	random	number	generator	so
# that	you all	get	the	same	results.). Create	the	bar	chart	of	successes

set.seed(0)
H <- c()
for(i in 1:10) {
  coin_tosses <- rbinom(n = 1, size = 20, prob = .6)
  cat( "Success:", coin_tosses, " ")
  H <- append(H, coin_tosses)
}
barplot(H, xlab="Successes",col="blue")

#c.Repeat	the	experiment	100 times	and	solve	for	questions	(a.)	and	(b.)

set.seed(0)
H <- c()
for(i in 1:100) {
  coin_tosses <- rbinom(n = 1, size = 10, prob = .6)
  cat( "Success:", coin_tosses, " ")
  H <- append(H, coin_tosses)
}
barplot(H, xlab="Successes",col="gray")
