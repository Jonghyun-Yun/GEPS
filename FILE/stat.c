#ifndef __STAT_H_
#define __STAT_H_

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define TINY 1.0e-20;

void nrerror(char error_text[]);
float *vector(int nl,int nh);
int *ivector(int nl,int nh);
double *dvector(int nl,int nh);
double **dmatrix(int nrl,int nrh,int ncl,int nch);

void free_vector(float *v,int nl,int nh);
void free_ivector(int *v,int nl,int nh);
void free_dvector(double *v,int nl,int nh);
void free_dmatrix(double **m,int nrl,int nrh,int ncl,int nch);

void ludcmp(a,n,indx,d)
int n,*indx;
double **a,*d;
{
	int i,imax,j,k;
	double big,dum,sum,temp;
	double *vv;

	vv=dvector(1,n);
	*d=1.0;
	for (i=1;i<=n;i++) {
		big=0.0;
		for (j=1;j<=n;j++)
			if ((temp=fabs(a[i][j])) > big) big=temp;
		if (big == 0.0) nrerror("Singular matrix in routine LUDCMP");
		vv[i]=1.0/big;
	}
	for (j=1;j<=n;j++) {
		for (i=1;i<j;i++) {
			sum=a[i][j];
			for (k=1;k<i;k++) sum -= a[i][k]*a[k][j];
			a[i][j]=sum;
		}
		big=0.0;
		for (i=j;i<=n;i++) {
			sum=a[i][j];
			for (k=1;k<j;k++)
				sum -= a[i][k]*a[k][j];
			a[i][j]=sum;
			if ( (dum=vv[i]*fabs(sum)) >= big) {
				big=dum;
				imax=i;
			}
		}
		if (j != imax) {
			for (k=1;k<=n;k++) {
				dum=a[imax][k];
				a[imax][k]=a[j][k];
				a[j][k]=dum;
			}
			*d = -(*d);
			vv[imax]=vv[j];
		}
		indx[j]=imax;
		if (a[j][j] == 0.0) a[j][j]=TINY;
		if (j != n) {
			dum=1.0/(a[j][j]);
			for (i=j+1;i<=n;i++) a[i][j] *= dum;
		}
	}
	free_dvector(vv,1,n);
}

#undef TINY

void lubksb(a,n,indx,b)
double **a,*b;
int n,*indx;
{
	int i,ii=0,ip,j;
	double sum;

	for (i=1;i<=n;i++) {
		ip=indx[i];
		sum=b[ip];
		b[ip]=b[i];
		if (ii)
			for (j=ii;j<=i-1;j++) sum -= a[i][j]*b[j];
		else if (sum) ii=i;
		b[i]=sum;
	}
	for (i=n;i>=1;i--) {
		sum=b[i];
		for (j=i+1;j<=n;j++) sum -= a[i][j]*b[j];
		b[i]=sum/a[i][i];
	}
}


int choldc(a, n, D)
double **a;
int n;
double **D;
{
int i, j, k;
double sum, *p;

p=dvector(1,n);
for (i=1; i<=n; i++) {
    for (j=i; j<=n; j++) {
      for (sum=a[i][j], k=i-1; k>=1; k--) sum -= a[i][k]*a[j][k];
      if (i==j) {
          if (sum <=0.0){ return 1; }
          p[i]=sqrt(sum);
         } else a[j][i]=sum/p[i];
       }
    }
/* Transfer the lower triangular part of A to the lower triangular matrix D */
for(i=1; i<=n; i++){
  D[i][i]=p[i];
  for(j=1; j<i; j++){ D[i][j]=a[i][j]; D[j][i]=0.0; }
 }
 free_dvector(p,1,n);
 return 0;
}


double matrix_logdet(X, n)
double **X;
int n;
{
int j, *indx;
double d, logdet;

  indx=ivector(1,n);
  ludcmp(X, n, indx, &d);
  for(logdet=0.0,j=1; j<=n; j++) logdet+=log(fabs(X[j][j]));

  printf("logdet=%g\n", logdet);

  return logdet;
}


/* Y=inv(X), return d=log(det(X)) */
double matrix_inverse(X, Y, n)
double **X, **Y;
int n;
{
double d, *col;
int i, j, *indx;
double logdet;

col=dvector(1,n);
indx=ivector(1,n);


ludcmp(X, n, indx, &d);
for(logdet=0.0,j=1; j<=n; j++) logdet+=log(fabs(X[j][j]));
/*
if(d==0.0){ printf("Singular matrix\n");  return; }
*/

for(j=1; j<=n; j++){
  for(i=1; i<=n; i++) col[i]=0.0;
  col[j]=1.0;
  lubksb(X, n, indx, col);
  for(i=1; i<=n; i++) Y[i][j]=col[i];
 }

for(i=1; i<=n; i++)
 for(j=1; j<=n; j++) { Y[i][j]=(Y[i][j]+Y[j][i])*0.5; Y[j][i]=Y[i][j]; }

free_dvector(col,1,n); free_ivector(indx,1,n);

return logdet;
}

double matrix_trace(A,p)
double **A;
int p;
{
int i;
double sum;

for(sum=0.0, i=1; i<=p; i++) sum+=A[i][i];

return sum;
}


int matrix_sum(A,B,C,n,p)
double **A, **B, **C;
int n, p;
{
int i, j;
for(i=1; i<=n; i++)
  for(j=1; j<=p; j++) C[i][j]=A[i][j]+B[i][j];
return 0;
}


int matrix_scalarmultiply(A,b,C,n,p)
double **A, b, **C;
int n, p;
{
int i, j;
for(i=1; i<=n; i++)
  for(j=1; j<=p; j++) C[i][j]=b*A[i][j];
return 0;
}


/* Matrix: A: n by p; B: p by m;  C: n by m */
int matrix_multiply(A,B,C,n,p,m)
double **A, **B, **C;
int n, p, m;
{
int i, j, k;
for(i=1; i<=n; i++)
   for(j=1; j<=m; j++){
       C[i][j]=.0;
       for(k=1; k<=p; k++) C[i][j]+=A[i][k]*B[k][j];
      }
return 0;
}


int matrix_vector_prod(A,b,d,n,p)
double **A, *b, *d;
int n,p;
{
int i,j;
for(i=1; i<=n; i++){
    d[i]=0.0;
    for(j=1; j<=p; j++) d[i]+=A[i][j]*b[j];
   }
return 0;
}


void copy_vector(a,b,p)
double *a, *b;
int p;
{
int i;
for(i=1; i<=p; i++) b[i]=a[i];
}

void copy_matrix(a,b,n,p)
double **a,**b;
int n, p;
{
int i, j;
for(i=1; i<=n; i++)
  for(j=1; j<=p; j++) b[i][j]=a[i][j];
}



/* calculate log(Gamma(s))  */
double loggamma(xx)
double xx;
{
        double x,tmp,ser;
        static double cof[6]={76.18009173,-86.50532033,24.01409822,
                -1.231739516,0.120858003e-2,-0.536382e-5};
        int j;

        x=xx-1.0;
        tmp=x+5.5;
        tmp -= (x+0.5)*log(tmp);
        ser=1.0;
        for (j=0;j<=5;j++) {
                x += 1.0;
                ser += cof[j]/x;
        }
        return -tmp+log(2.50662827465*ser);
}

double dlogbeta(double xx, double alpha, double beta)
{
	return loggamma(alpha + beta) - loggamma(alpha) - loggamma(beta) + (alpha - 1.0) * log(xx) + (beta - 1.0) * log(1.0 - xx);

}

/* calculate log(k!) */
double logpum(k)
int k;
{
double value;
int i;

for(value=0.0, i=1; i<=k; i++) value+=log(1.0*i);

return value;
}


/* generate the random variable form Gamma(a,b) */
double Rgamma(a,b)
double a, b;
{
int ok;
double d, q, un, u1, y, z;

if(a<=0.0 || b<=0.0) { printf("Gamma parameter error (<0.0)\n"); return 0.0; }

if(a<1.0){  /* Ahrens, P.213 */
 ok=0;
while(ok==0){
  un=0.0;
  while(un<=0.0 || un>=1.0) un=rand()*1.0/RAND_MAX;
  d=(2.718282+a)/2.718282;
  q=d*un;

  if(q<=1.0){
    z=exp(1.0/a*log(q));
    u1=rand()*1.0/RAND_MAX;
    if(u1<exp(-z)) ok=1;
             }
   else{
     z=-1.0*log((d-q)/a);
     u1=rand()*1.0/RAND_MAX;
     if(u1<exp((a-1)*log(z))) ok=1;
       }
            } /* end ok */
   }
 else {  /* a>=1.0 Fishman, P.214 */
  ok=0;
  while(ok==0){
    un=0.0;
    while(un<=0.0 || un>=1.0) un=rand()*1.0/RAND_MAX;
    y=-1.0*log(un);
    u1=rand()*1.0/RAND_MAX;
    if(u1<exp((a-1)*(log(y)-(y-1)))) { z=a*y; ok=1; }
             }
      }

return z/b;
}


/* Generate a random variable from Beta(1,k), where
  the first parameter is 1, the second parameter is b */
double Rbeta(b)
double b;
{
double un;
un=0.0;
while(un<=0.0 || un>=1.0) un=rand()*1.0/RAND_MAX;
return 1.0-exp(1.0/b*log(un));
}

/* Generate deviates from Dirichlet(a1,a2,\ldots,a_k) */
int RDirichlet(w,a,k)
double *w,*a;
int k;
{
double sum;
int i;
for(sum=0.0,i=1; i<=k; i++){
    w[i]=Rgamma(a[i],1.0);
    sum+=w[i];
   }
for(i=1; i<=k; i++) w[i]/=sum;
return 0;
}


double gasdev()
{
        static int iset=0;
        static double gset;
        double fac,r,v1,v2;

        if  (iset == 0) {
                do {
                        v1=rand()*2.0/RAND_MAX-1.0;
                        v2=rand()*2.0/RAND_MAX-1.0;
                        r=v1*v1+v2*v2;
                } while (r >= 1.0);
                fac=sqrt(-2.0*log(r)/r);
                gset=v1*fac;
                iset=1;
                return v2*fac;
        }
        else {
                iset=0;
                return gset;
        }
}


double Rgausdev(mean,variance)
double mean,variance;
{
        static int iset=0;
        static double gset;
        double fac,r,v1,v2;

        if  (iset == 0) {
                do {
                        v1=rand()*2.0/RAND_MAX-1.0;
                        v2=rand()*2.0/RAND_MAX-1.0;
                        r=v1*v1+v2*v2;
                } while (r >= 1.0);
                fac=sqrt(-2.0*log(r)/r);
                gset=v1*fac;
                iset=1;
                return v2*fac*sqrt(variance)+mean;
        } else {
                iset=0;
                return gset*sqrt(variance)+mean;
        }
}


int RNORM(x,mu,Sigma,p)
double *x,*mu,**Sigma;
int p;
{
int i, j;
double **D, *z;

D=dmatrix(1,p,1,p);
z=dvector(1,p);

choldc(Sigma,p,D);

for(i=1; i<=p; i++) z[i]=gasdev();
for(i=1; i<=p; i++){
    x[i]=mu[i];
    for(j=1; j<=i; j++) x[i]+=D[i][j]*z[j];
   }

free_dmatrix(D,1,p,1,p);
free_dvector(z,1,p);

return 0;
}


int Rwishart(B,df,Sigma,p)
double **B,df,**Sigma;
int p;
{
double **Z, **A, *Y;
int i, j, k;

Z=dmatrix(1,p,1,p);
A=dmatrix(1,p,1,p);
Y=dvector(1,p);

for(i=1; i<=p; i++)
  for(j=1; j<=p; j++) Z[i][j]=A[i][j]=B[i][j]=0.0;

choldc(Sigma, p, A);

for(j=1; j<=p; j++)
  for(i=1; i<=p; i++) Z[i][j]=gasdev();
for(i=1; i<=p; i++) Y[i]=Rgamma(0.5*df,0.5);

B[1][1]=Y[1];
for(j=2; j<=p; j++){
    B[j][j]=Y[j];
    for(i=1; i<j; i++) B[j][j]+=Z[i][j]*Z[i][j];
    B[1][j]=Z[1][j]*sqrt(Y[1]);
   }
 for(j=2; j<=p; j++)
    for(i=2; i<j; i++){
        B[i][j]=Z[i][j]*sqrt(Y[i]);
        for(k=1; k<=i-1; k++) B[i][j]+=Z[k][i]*Z[k][j];
       }
for(i=1; i<=p; i++)
   for(j=1; j<i; j++) B[i][j]=B[j][i];

matrix_multiply(A,B,Z,p,p,p);

for(i=1; i<=p; i++)
  for(j=1; j<i; j++){ A[j][i]=A[i][j]; A[i][j]=0.0; }

matrix_multiply(Z,A,B,p,p,p);

free_dmatrix(Z,1,p,1,p);
free_dmatrix(A,1,p,1,p);
free_dvector(Y,1,p);

return 0;
}


/* calculated the log-density of  z~gamma(a,b) */
double dloggamma(x,a,b)
double x,a,b;
{
double logcon, den;
logcon=loggamma(a);
den=log(b)-b*x+(a-1)*log(b*x)-logcon;
return den;
}


double dlogstudent(z,k)
double z;
int k;  /* the degree of freedom */
{
double logprob;
logprob=-0.5*(k+1)*log(1.0+z*z/k);
return logprob;
}



double dlognorm(double x, double mu, double sigma)
{
	double pi = 3.14159265;
	double dens, temp;

	temp = (-1.0 / (2 * sigma * sigma)) * ((x - mu) * (x - mu));
	dens = (1 / (sqrt(2 * pi) * sigma)) * exp(temp);

	return log(dens);

}

double DLOGGAUSS(z,mean,variance,p)
double *z, *mean, **variance;
int p;
{
double pii =3.141593;
int i,j;
double logdet, sum, **mat,*vect, *mu;

mat=dmatrix(1,p,1,p);
vect=dvector(1,p);
mu=dvector(1,p);

for(i=1; i<=p; i++) mu[i]=z[i]-mean[i];
logdet=matrix_inverse(variance,mat,p);
matrix_vector_prod(mat,mu,vect,p,p);

for(sum=0.0,i=1; i<=p; i++) sum+=mu[i]*vect[i];
sum*=-0.5;
sum+=-0.5*logdet-0.5*p*log(2.0*pii);

free_dmatrix(mat,1,p,1,p);
free_dvector(vect,1,p);
free_dvector(mu,1,p);

return sum;
}


double Dlogwishart(D,df,Sigma,p)
double **D,df,**Sigma;
int p;
{
int i, j;
double a, sum, logdet1, logdet2, **mt1, **mt2;
double pii =3.141593;

mt1=dmatrix(1,p,1,p);
mt2=dmatrix(1,p,1,p);

for(i=1; i<=p; i++)
   for(j=1; j<=p; j++) mt1[i][j]=D[i][j];
logdet1=matrix_inverse(mt1,mt2,p);

for(i=1; i<=p; i++)
   for(j=1; j<=p; j++) mt1[i][j]=Sigma[i][j];
logdet2=matrix_inverse(mt1,mt2,p);

matrix_multiply(mt2,D,mt1,p,p,p);

for(sum=0.0,i=1; i<=p; i++) sum+=mt1[i][i];
sum*=-0.5;
sum+=0.5*(df-p-1)*logdet1;
sum+=-0.5*df*logdet2;
sum+=-0.5*df*p*log(2.0);
sum+=-0.25*p*(p-1)*log(pii);
for(i=1; i<=p; i++){
    a=df-i+1;
    if(a<0.0001) a=0.0001;
    sum+=-loggamma(0.5*a);
   }

free_dmatrix(mt1,1,p,1,p);
free_dmatrix(mt2,1,p,1,p);

return sum;
}


int uniform_direction(d, n)
double *d;
int n;
{
double sum;
int k;

for(sum=0, k=1; k<=n; k++){
   d[k]=gasdev();
   sum+=d[k]*d[k];
 }
 for(k=1; k<=n; k++)
    d[k]=d[k]/sqrt(sum);

return 0;
 }


int dmaxclass(z,n)
double *z;
int n;
{
int i, maxi;
double maxd;

maxd=z[1]; maxi=1;
for(i=2; i<=n; i++)
  if(z[i]>maxd){ maxd=z[i]; maxi=i; }

return maxi;
}

int imaxclass(z,n)
int *z, n;
{
int i, maxi;
int maxd;

maxd=z[1]; maxi=1;
for(i=2; i<=n; i++)
  if(z[i]>maxd){ maxd=z[i]; maxi=i; }

return maxi;
}


int binary_trans(k,l,d)
int k, l,*d;
{
int i, j;
for(i=1; i<=l; i++) d[i]=0;
j=l;
while(k>0){
   d[j]=k%2;
   k=k/2;
   j--;
 }
return 0;
}


double logsum(a,b)
        double a, b;
{
        double sum;
        if(a>b) sum=a+log(1.0+exp(b-a));
           else sum=b+log(1.0+exp(a-b));
        return sum;
}


double maxvector(x,n)
 double *x;
 int n;
{
double max;
int i;
  max=x[1];
  for(i=2; i<=n; i++)
     if(x[i]>max) max=x[i];
  return max;
 }


double minvector(x,n)
 double *x;
 int n;
{
double min;
int i;
  min=x[1];
  for(i=2; i<=n; i++)
     if(x[i]<min) min=x[i];
  return min;
 }



double sample_variance(x,n)
double *x;
int n;
{
int i;
double sum1, sum2, mean, var;

sum1=sum2=0.0;
for(i=1; i<=n; i++){
   sum1+=x[i];
   sum2+=x[i]*x[i];
  }
mean=sum1/n;
var=(sum2-n*mean*mean)/(n-1);

return var;
}




/* Return the value ln[Gamma(xx)] for xx>0 */
double gammln(xx)
double xx;
{
        double x,tmp,ser;
        static double cof[6]={76.18009173,-86.50532033,24.01409822,
                -1.231739516,0.120858003e-2,-0.536382e-5};
        int j;

        x=xx-1.0;
        tmp=x+5.5;
        tmp -= (x+0.5)*log(tmp);
        ser=1.0;
        for (j=0;j<=5;j++) {
                x += 1.0;
                ser += cof[j]/x;
        }
        return -tmp+log(2.50662827465*ser);
}


#define ITMAX 100
#define EPS 3.0e-7
void gser(gamser,a,x,gln)
double a,x,*gamser,*gln;
{
	int n;
	double sum,del,ap;

	*gln=gammln(a);
	if (x <= 0.0) {
		if (x < 0.0) nrerror("x less than 0 in routine GSER");
		*gamser=0.0;
		return;
	} else {
		ap=a;
		del=sum=1.0/a;
		for (n=1;n<=ITMAX;n++) {
			ap += 1.0;
			del *= x/ap;
			sum += del;
			if (fabs(del) < fabs(sum)*EPS) {
				*gamser=sum*exp(-x+a*log(x)-(*gln));
				return;
			}
		}
		nrerror("a too large, ITMAX too small in routine GSER");
		return;
	}
}

#undef ITMAX
#undef EPS


#define ITMAX 100
#define EPS 3.0e-7
void gcf(gammcf,a,x,gln)
double a,x,*gammcf,*gln;
{
	int n;
	double gold=0.0,g,fac=1.0,b1=1.0;
	double b0=0.0,anf,ana,an,a1,a0=1.0;
	/* float gammln();
	void nrerror(); */

	*gln=gammln(a);
	a1=x;
	for (n=1;n<=ITMAX;n++) {
		an=(float) n;
		ana=an-a;
		a0=(a1+a0*ana)*fac;
		b0=(b1+b0*ana)*fac;
		anf=an*fac;
		a1=x*a0+anf*a1;
		b1=x*b0+anf*b1;
		if (a1) {
			fac=1.0/a1;
			g=b1*fac;
			if (fabs((g-gold)/g) < EPS) {
				*gammcf=exp(-x+a*log(x)-(*gln))*g;
				return;
			}
			gold=g;
		}
	}
	nrerror("a too large, ITMAX too small in routine GCF");
}
#undef ITMAX
#undef EPS


double gammp(a,x)
double a,x;
{
        double gamser,gammcf,gln;
        /* void gser(),gcf(),nrerror(); */


        if (x < 0.0 || a <= 0.0) nrerror("Invalid arguments in routine GAMMP");
        if (x < (a+1.0)) {
                gser(&gamser,a,x,&gln);
                return gamser;
        } else {
                gcf(&gammcf,a,x,&gln);
                return 1.0-gammcf;
        }
}


/* Return the CDF of the standard normal distribution */
double Gaussp(x)
double x;
{
double s, prob;

s=0.5*x*x;
if(x>0) prob=0.5+0.5*gammp(0.5,s);
 else prob=0.5-0.5*gammp(0.5,s);

return prob;
}


/* return Gamma'(z)/Gamma(z)   */
/* Refer to "mathematics handbook pp.287" */
double diGamma(z)
double z;
{
int i;
double sum, delta, epsilon=3.0e-7;

sum=-1.0/z-0.5772156649;

delta=1.0-1.0/(1+z);
sum+=delta;
i=1;
while(delta>epsilon){
   i++;
   delta=1.0/i-1.0/(i+z);
   sum+=delta;
  }

return sum;
}

void indexx(int n, double *arrin, int *indx)
{
	int l, j, ir, indxt, i;
	double q;

        for (j=1;j<=n;j++) indx[j]=j;
        l=(n >> 1) + 1;
        ir=n;
        for (;;) {
                if (l > 1)
                        q=arrin[(indxt=indx[--l])];
                else {
                        q=arrin[(indxt=indx[ir])];
                        indx[ir]=indx[1];
                        if (--ir == 1) {
                                indx[1]=indxt;
                                return;
                        }
                }
                i=l;
                j=l << 1;
                while (j <= ir) {
                        if (j < ir && arrin[indx[j]] < arrin[indx[j+1]]) j++;
                        if (q < arrin[indx[j]]) {
                                indx[i]=indx[j];
                                j += (i=j);
                        }
                        else j=ir+1;
                }
                indx[i]=indxt;
        }
}

void nrerror(error_text)
char error_text[];
{
	void exit();

	fprintf(stderr,"Numerical Recipes run-time error...\n");
	fprintf(stderr,"%s\n",error_text);
	fprintf(stderr,"...now exiting to system...\n");
	exit(1);
}



float *vector(nl,nh)
int nl,nh;
{
	float *v;

	v=(float *)malloc((unsigned) (nh-nl+1)*sizeof(float));
	if (!v) nrerror("allocation failure in vector()");
	return v-nl;
}

int *ivector(nl,nh)
int nl,nh;
{
	int *v;

	v=(int *)malloc((unsigned) (nh-nl+1)*sizeof(int));
	if (!v) nrerror("allocation failure in ivector()");
	return v-nl;
}

double *dvector(nl,nh)
int nl,nh;
{
	double *v;

	v=(double *)malloc((unsigned) (nh-nl+1)*sizeof(double));
	if (!v) nrerror("allocation failure in dvector()");
	return v-nl;
}



float **matrix(nrl,nrh,ncl,nch)
int nrl,nrh,ncl,nch;
{
	int i;
	float **m;

	m=(float **) malloc((unsigned) (nrh-nrl+1)*sizeof(float*));
	if (!m) nrerror("allocation failure 1 in matrix()");
	m -= nrl;

	for(i=nrl;i<=nrh;i++) {
		m[i]=(float *) malloc((unsigned) (nch-ncl+1)*sizeof(float));
		if (!m[i]) nrerror("allocation failure 2 in matrix()");
		m[i] -= ncl;
	}
	return m;
}

double **dmatrix(nrl,nrh,ncl,nch)
int nrl,nrh,ncl,nch;
{
	int i;
	double **m;

	m=(double **) malloc((unsigned) (nrh-nrl+1)*sizeof(double*));
	if (!m) nrerror("allocation failure 1 in dmatrix()");
	m -= nrl;

	for(i=nrl;i<=nrh;i++) {
		m[i]=(double *) malloc((unsigned) (nch-ncl+1)*sizeof(double));
		if (!m[i]) nrerror("allocation failure 2 in dmatrix()");
		m[i] -= ncl;
	}
	return m;
}

int **imatrix(nrl,nrh,ncl,nch)
int nrl,nrh,ncl,nch;
{
	int i,**m;

	m=(int **)malloc((unsigned) (nrh-nrl+1)*sizeof(int*));
	if (!m) nrerror("allocation failure 1 in imatrix()");
	m -= nrl;

	for(i=nrl;i<=nrh;i++) {
		m[i]=(int *)malloc((unsigned) (nch-ncl+1)*sizeof(int));
		if (!m[i]) nrerror("allocation failure 2 in imatrix()");
		m[i] -= ncl;
	}
	return m;
}



float **submatrix(a,oldrl,oldrh,oldcl,oldch,newrl,newcl)
float **a;
int oldrl,oldrh,oldcl,oldch,newrl,newcl;
{
	int i,j;
	float **m;

	m=(float **) malloc((unsigned) (oldrh-oldrl+1)*sizeof(float*));
	if (!m) nrerror("allocation failure in submatrix()");
	m -= newrl;

	for(i=oldrl,j=newrl;i<=oldrh;i++,j++) m[j]=a[i]+oldcl-newcl;

	return m;
}


void free_vector(v,nl,nh)
float *v;
int nl,nh;
{
	free((char*) (v+nl));
}

void free_ivector(v,nl,nh)
int *v,nl,nh;
{
	free((char*) (v+nl));
}

void free_dvector(v,nl,nh)
double *v;
int nl,nh;
{
	free((char*) (v+nl));
}


void free_matrix(m,nrl,nrh,ncl,nch)
float **m;
int nrl,nrh,ncl,nch;
{
	int i;

	for(i=nrh;i>=nrl;i--) free((char*) (m[i]+ncl));
	free((char*) (m+nrl));
}

void free_dmatrix(m,nrl,nrh,ncl,nch)
double **m;
int nrl,nrh,ncl,nch;
{
	int i;

	for(i=nrh;i>=nrl;i--) free((char*) (m[i]+ncl));
	free((char*) (m+nrl));
}

void free_imatrix(m,nrl,nrh,ncl,nch)
int **m;
int nrl,nrh,ncl,nch;
{
	int i;

	for(i=nrh;i>=nrl;i--) free((char*) (m[i]+ncl));
	free((char*) (m+nrl));
}



void free_submatrix(b,nrl,nrh,ncl,nch)
float **b;
int nrl,nrh,ncl,nch;
{
	free((char*) (b+nrl));
}



float **convert_matrix(a,nrl,nrh,ncl,nch)
float *a;
int nrl,nrh,ncl,nch;
{
	int i,j,nrow,ncol;
	float **m;

	nrow=nrh-nrl+1;
	ncol=nch-ncl+1;
	m = (float **) malloc((unsigned) (nrow)*sizeof(float*));
	if (!m) nrerror("allocation failure in convert_matrix()");
	m -= nrl;
	for(i=0,j=nrl;i<=nrow-1;i++,j++) m[j]=a+ncol*i-ncl;
	return m;
}



void free_convert_matrix(b,nrl,nrh,ncl,nch)
float **b;
int nrl,nrh,ncl,nch;
{
	free((char*) (b+nrl));
}


#endif // __STAT_H_
