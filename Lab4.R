df = read.csv('~/Desktop/out.csv')
map('state', regions=df[,'names'], col=df[,'labels'], fill=T)
