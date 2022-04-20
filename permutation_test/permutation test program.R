n11=150
n12=372-150
n21=200
n22=428
A= matrix(c(n11,n12,n21,n22),2,2)
n1.=sum(A[1,])#sum of first row of table
n2.=sum(A[2,])#sum of second row of table
n.1=sum(A[,1])#sum of first column of table
n.2=sum(A[,2])#sum of second column of table
m = matrix(0,2,2)
m[1,1] = (n1.*n.1)/1000
m[1,2] = (n1.*n.2)/1000
m[2,1] = (n2.*n.1)/1000
m[2,2] = (n2.*n.2)/1000
chi.sqt = sum((A-m)^2/m)

#################Permutation test##############################
m = matrix(0,2,2)
mother = c(rep(1,n11+n12), rep(0, n21+n22))
chi.sq=numeric(1000)
for(i in 1:1000){
children= rep(0,1000)
places.for.1 =sample(1:1000, n11+n21)
children[places.for.1] = 1
tab=table(mother, children)
n1.=sum(tab[1,])#sum of first row of table
n2.=sum(tab[2,])#sum of second row of table
n.1=sum(tab[,1])#sum of first column of table
n.2=sum(tab[,2])#sum of second column of table

m[1,1] = (n1.*n.1)/1000
m[1,2] = (n1.*n.2)/1000
m[2,1] = (n2.*n.1)/1000
m[2,2] = (n2.*n.2)/1000
chi.sq[i] = sum((tab-m)^2/m)
}
sum(chi.sq>=chi.sqt)
