#include "stat.c"

/* Generate a random variable from Beta(a,b) */
double beta_rng(double a, double b) {
  if (a < 0.0 || b < 0.0) {
    fprintf(stderr, "error: beta parameter(s) is negative! Exiting...\n");
    exit(-1);
  }
  double ga, gb;
  ga = Rgamma(a, 1.0);
  gb = Rgamma(b, 1.0);
  return ga / (ga + gb);
}

/* mu, sigma: mean and standard deviation of the distribution on the log scale
 */
double lognormal_rng(double mu, double sigma) {
  if (sigma < 0) {
    fprintf(stderr, "sigma should be positive! Exiting...\n");
    exit(-1);
  }
  return exp(mu + sigma * gasdev());
}

/* https://people.sc.fsu.edu/~jburkardt/c_src/log_normal/log_normal.c */
/******************************************************************************/
double lognormal_lpdf(double x, double mu, double sigma)
/******************************************************************************/
/*
  Purpose:

    LOGNORMAL_LPDF evaluates the log of Log Normal PDF.

  Discussion:

    PDF(A,B;X)
      = exp ( - 0.5 * ( ( log ( X ) - MU ) / SIGMA )^2 )
        / ( SIGMA * X * sqrt ( 2 * PI ) )

    The Log Normal PDF is also known as the Cobb-Douglas PDF,
    and as the Antilog_normal PDF.

    The Log Normal PDF describes a variable X whose logarithm
    is normally distributed.

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    19 September 2004

  Author:

    John Burkardt

  Parameters:

    Input, double X, the argument of the PDF.
    0.0 < X

    Input, double MU, SIGMA, the parameters of the PDF.
    0.0 < SIGMA.

    Output, double log(PDF), the value of the PDF.
*/
{
  double lpdf;
  const double r8_pi = 3.14159265358979323;
  double y;

  if (x <= 0.0) {
    fprintf(stderr, "lognormal: x should be positive! Exiting...\n");
    exit(-1);
  } else if (sigma < 0) {
    fprintf(stderr, "lognormal: sigma should be positive! Exiting...\n");
    exit(-1);
  } else {
    y = (log(x) - mu) / sigma;
    /* pdf = exp ( - 0.5 * y * y ) / ( sigma * x * sqrt ( 2.0 * r8_pi ) ); */
    lpdf = -0.5 * y * y - log(sigma) - log(x) - 0.5 * log(2.0 * r8_pi);
  }
  return lpdf;
}

int bernoulli_rng(double p) {
  int x = 0;
  if (p > 1 || p < 0) {
    fprintf(stderr, "p should be between 0 and 1! Exiting...\n");
    exit(-1);
  }
  if (p > rand() / RAND_MAX)
    x = 1;
  return x;
}
