#include "header.h"
#include "FILE/nrutil.h"
#include "FILE/stat.c"
#include "FILE/fun_stat.c"
#include "cost.c"

int main(int argc, const char * argv[]) {
  /// Declare Variables
  FILE *inp, *JIN, *HUR, *OUT, *PRT, *JYW, *ASA, *IMS, *JJW;
  // model selection results
  FILE *FMS;
  char buf[255], frname[255];
  int stime;
  long ltime;
  int ind, ite, a, b, i, j, k, l, v, accept, gcount, mcount, mutmp, *count,
      show1, show2;
  double num, den, un, ratio;
  double old_like_beta, new_like_beta, old_like_theta, new_like_theta;
  double old_like_eta, new_like_eta;
  double update_like_samp, update_like_item, tmp_oldmu, tmp_newmu;
  double post_a, post_b, school_a, school_b;
  double *old_samp_distance, *new_samp_distance, *sample_samp_like;
  double *old_item_distance, *new_item_distance, *sample_item_like;
  double **sum_mu, **mu_dist, **sum_mu_dist;
  double **sample_tau, *sum_tau, *var_tau;
  double **sample_sigma, *sum_sigma, *var_sigma;
  double **sample_delta, *sum_delta, *var_delta;
  double **sample_gamma, *sum_gamma, *var_gamma;
  double **sample_varphi, *sum_varphi, *var_varphi;
  double var_fix, avg_fix, *var_ran, *avg_ran, avg_beta, var_beta;

  double **sample_eta, *sum_eta, *var_eta;
  double **sample_xi, *sum_xi, *var_xi;
  double **sample_pi, *sum_pi, *var_pi;

  // # CPUs ?? nah.. some index for sampling
  MM = atoi(argv[1]);

  // instead, do
  // export OMP_NUM_THREADS=4
  // computation time got halved with OMP_NUM_THREADS=4
  // for compile use (don't use clang)
  // gcc-11 main.c -o main -fopenmp

  /* model selection */
  int DO_MS = atoi(argv[2]);
  if (DO_MS > 1 || DO_MS < 0) {
    printf("invalid arguemnt for DO_MS.\n");
    return 0;
  }

  // Set Random Seed
  ltime = time(NULL);
  stime = (unsigned int)ltime / 2;
  srand(stime);
  printf("nseed = %d\n", stime);

// Input Number of Thread
#pragma omp parallel
  {
#if defined(_OPENMP)
    k = omp_get_num_threads();
    printf("k = %d\n", k);
    srand(((unsigned int)time(NULL)) ^ k);
#endif
  }

  /// Input Parameters. See header.h
  inp = fopen("DATA/parameter.txt", "r");
  if (inp == NULL) {
    printf("Can't open data file\n");
    return 0;
  }
  fscanf(inp, "%d", &niter);
  fscanf(inp, "%d", &nburn);
  fscanf(inp, "%d", &thin);
  fscanf(inp, "%d", &print);  // interval to print outputs
  fscanf(inp, "%d", &repeat); // # chains
  fscanf(inp, "%lf", &jump_beta);
  fscanf(inp, "%lf", &jump_theta);
  fscanf(inp, "%lf", &jump_mu);
  fscanf(inp, "%lf", &jump_W);
  fscanf(inp, "%lf", &jump_eta);
  fclose(inp);

  // The Number of Respondents by Schools
  ncount = ivector(1, nSCHOOL);
  inp = fopen("DATA/count.txt", "r");
  for (i = 1; i <= nSCHOOL; i++)
    fscanf(inp, "%d", &ncount[i]);
  fclose(inp);

  // Load Jumping Rule
  // NOTE: z? person?, size = (nITEM - 0 + 1): tuned by count_samp,
  // ???????????? ??????????????? ????????? ?????? ????????? ????????? ??? ?????????...
  jump_Z = dvector(0, nITEM);
  inp = fopen("DATA/jump.txt", "r");
  for (i = 0; i <= nITEM; i++) {
    fscanf(inp, "%lf", &jump_Z[i]);
    jump_Z[i] *= 1.5;
  }
  fclose(inp);

  // nMAX: max(nSample)
  // Declare typedef structure and set array of variables in typedef structure
  totalsize = sizeof(SCHOOL) + sizeof(int) * (nMAX + 1) * (nITEM + 1);
  totalsize += sizeof(int) * (nMAX + 1) + sizeof(int) * (nITEM + 1);
  totalsize += sizeof(int) * (nITEM + 1) * (nMAX + 1) * (nMAX + 1);
  totalsize += sizeof(int) * (nMAX + 1) * (nITEM + 1) * (nITEM + 1);
  totalsize += sizeof(double) * ((nITEM + 1) * 2 + (nMAX + 1) * 2) +
               sizeof(double) * ((nITEM + 1) * (nITEM + 1) * 2);
  totalsize += sizeof(double) *
               ((nMAX + 1) * (nDIM + 1) * 4 + (nITEM + 1) * (nDIM + 1) * 2);
  totalsize += sizeof(double) *
               (((niter - nburn) / thin + 1) * (nITEM + 1) + (nITEM + 1) * 3);
  totalsize += sizeof(double) *
               (((niter - nburn) / thin + 1) * (nMAX + 1) + (nMAX + 1) * 3);
  totalsize +=
      sizeof(double) * (((niter - nburn) / thin + 1) *
                        ((nMAX + 1) * (nDIM + 1) + (nITEM + 1) * (nDIM + 1)));
  totalsize += sizeof(double) * (((niter - nburn) / thin + 1) + (nDIM + 1));
  totalsize += sizeof(double) *
               ((nMAX + 1) * (nDIM + 1) * 2 + (nITEM + 1) * (nDIM + 1) * 2 +
                (nMAX + 1) + (nITEM + 1));
  totalsize += sizeof(double) * ((nITEM + 1) * (nITEM + 1) * 3);
  SCHOOL = (YEWON *)malloc(totalsize * (nSCHOOL + 1));

  // memory alloc
  for (k = 0; k <= nSCHOOL; k++) {
    SCHOOL[k].cbsize = totalsize;
    // NOTE: **dataset: item response dataset (nSAMPLE X nITEM)
    SCHOOL[k].dataset = (int **)malloc(sizeof(int *) * (nMAX + 1));
    // NOTE: (nMAX+1) array, not matrix, ???????????? ???????????? ?????????
    // ??????????????? ?????? ???, colsum or rowsum of item matrix
    SCHOOL[k].count_samp = (int *)malloc(sizeof(int) * (nMAX + 1));
    // ???????????? ???????????? ????????? ??????????????? ??????
    // ???, colsum or rowsum of item matrix
    SCHOOL[k].count_item = (int *)malloc(sizeof(int) * (nITEM + 1));
    // NOTE: Yi,n??n for item i
    SCHOOL[k].Y = (int ***)malloc(sizeof(int **) * (nITEM + 1));
    // NOTE: Uk,p??p for respondent k
    SCHOOL[k].U = (int ***)malloc(sizeof(int **) * (nMAX + 1));
    SCHOOL[k].oldbeta = (double *)malloc(sizeof(double) * (nITEM + 1));
    SCHOOL[k].newbeta = (double *)malloc(sizeof(double) * (nITEM + 1));
    SCHOOL[k].oldtheta = (double *)malloc(sizeof(double) * (nMAX + 1));
    SCHOOL[k].newtheta = (double *)malloc(sizeof(double) * (nMAX + 1));
    SCHOOL[k].old_Zsamp = (double **)malloc(sizeof(double *) * (nMAX + 1));
    SCHOOL[k].new_Zsamp = (double **)malloc(sizeof(double *) * (nMAX + 1));
    SCHOOL[k].old_Zmean = (double **)malloc(sizeof(double *) * (nMAX + 1));
    SCHOOL[k].new_Zmean = (double **)malloc(sizeof(double *) * (nMAX + 1));
    SCHOOL[k].old_Zitem = (double **)malloc(sizeof(double *) * (nITEM + 1));
    SCHOOL[k].new_Zitem = (double **)malloc(sizeof(double *) * (nITEM + 1));
    SCHOOL[k].mean_Z = (double *)malloc(sizeof(double) * (nDIM + 1));
    SCHOOL[k].sample_beta =
        (double **)malloc(sizeof(double *) * ((niter - nburn) / thin + 1));
    SCHOOL[k].sample_theta =
        (double **)malloc(sizeof(double *) * ((niter - nburn) / thin + 1));
    SCHOOL[k].sample_sigma =
        (double *)malloc(sizeof(double) * ((niter - nburn) / thin + 1));
    SCHOOL[k].sum_beta = (double *)malloc(sizeof(double) * (nITEM + 1));
    SCHOOL[k].var_beta = (double *)malloc(sizeof(double) * (nITEM + 1));
    SCHOOL[k].acc_beta = (double *)malloc(sizeof(double) * (nITEM + 1));
    SCHOOL[k].sum_theta = (double *)malloc(sizeof(double) * (nMAX + 1));
    SCHOOL[k].var_theta = (double *)malloc(sizeof(double) * (nMAX + 1));
    SCHOOL[k].acc_theta = (double *)malloc(sizeof(double) * (nMAX + 1));
    SCHOOL[k].sample_Zsamp =
        (double ***)malloc(sizeof(double **) * ((niter - nburn) / thin + 1));
    SCHOOL[k].sample_Zitem =
        (double ***)malloc(sizeof(double **) * ((niter - nburn) / thin + 1));
    SCHOOL[k].sample_item_mat =
        (double **)malloc(sizeof(double *) * (nITEM + 1));
    SCHOOL[k].sum_Zsamp = (double **)malloc(sizeof(double *) * (nMAX + 1));
    SCHOOL[k].var_Zsamp = (double **)malloc(sizeof(double *) * (nMAX + 1));
    SCHOOL[k].acc_Zsamp = (double *)malloc(sizeof(double) * (nMAX + 1));
    SCHOOL[k].sum_Zitem = (double **)malloc(sizeof(double *) * (nITEM + 1));
    SCHOOL[k].var_Zitem = (double **)malloc(sizeof(double *) * (nITEM + 1));
    SCHOOL[k].acc_Zitem = (double *)malloc(sizeof(double) * (nITEM + 1));
    SCHOOL[k].old_item_mat = (double **)malloc(sizeof(double *) * (nITEM + 1));
    SCHOOL[k].new_item_mat = (double **)malloc(sizeof(double *) * (nITEM + 1));
    SCHOOL[k].sum_item_mat = (double **)malloc(sizeof(double *) * (nITEM + 1));
    SCHOOL[k].var_item_mat = (double **)malloc(sizeof(double *) * (nITEM + 1));
    for (i = 0; i <= nMAX; i++)
      SCHOOL[k].dataset[i] = (int *)malloc(sizeof(int) * (nITEM + 1));
    for (i = 0; i <= nITEM; i++) {
      SCHOOL[k].Y[i] = (int **)malloc(sizeof(int *) * (nMAX + 1));
      for (a = 0; a <= nMAX; a++)
        SCHOOL[k].Y[i][a] = (int *)malloc(sizeof(int) * (nMAX + 1));
    }
    for (i = 0; i <= nMAX; i++) {
      SCHOOL[k].U[i] = (int **)malloc(sizeof(int *) * (nITEM + 1));
      for (a = 0; a <= nITEM; a++)
        SCHOOL[k].U[i][a] = (int *)malloc(sizeof(int) * (nITEM + 1));
    }
    for (i = 0; i <= nMAX; i++) {
      SCHOOL[k].old_Zsamp[i] = (double *)malloc(sizeof(double) * (nDIM + 1));
      SCHOOL[k].new_Zsamp[i] = (double *)malloc(sizeof(double) * (nDIM + 1));
      SCHOOL[k].old_Zmean[i] = (double *)malloc(sizeof(double) * (nDIM + 1));
      SCHOOL[k].new_Zmean[i] = (double *)malloc(sizeof(double) * (nDIM + 1));
    }
    for (i = 0; i <= nITEM; i++) {
      SCHOOL[k].old_Zitem[i] = (double *)malloc(sizeof(double) * (nDIM + 1));
      SCHOOL[k].new_Zitem[i] = (double *)malloc(sizeof(double) * (nDIM + 1));
    }
    for (i = 0; i <= (niter - nburn) / thin; i++) {
      SCHOOL[k].sample_beta[i] = (double *)malloc(sizeof(double) * (nITEM + 1));
      SCHOOL[k].sample_theta[i] = (double *)malloc(sizeof(double) * (nMAX + 1));
      SCHOOL[k].sample_Zsamp[i] =
          (double **)malloc(sizeof(double *) * (nMAX + 1));
      SCHOOL[k].sample_Zitem[i] =
          (double **)malloc(sizeof(double *) * (nITEM + 1));
      for (j = 0; j <= nMAX; j++)
        SCHOOL[k].sample_Zsamp[i][j] =
            (double *)malloc(sizeof(double) * (nDIM + 1));
      for (j = 0; j <= nITEM; j++)
        SCHOOL[k].sample_Zitem[i][j] =
            (double *)malloc(sizeof(double) * (nDIM + 1));
    }
    for (i = 0; i <= nMAX; i++) {
      SCHOOL[k].sum_Zsamp[i] = (double *)malloc(sizeof(double) * (nDIM + 1));
      SCHOOL[k].var_Zsamp[i] = (double *)malloc(sizeof(double) * (nDIM + 1));
    }
    for (i = 0; i <= nITEM; i++) {
      SCHOOL[k].sum_Zitem[i] = (double *)malloc(sizeof(double) * (nDIM + 1));
      SCHOOL[k].var_Zitem[i] = (double *)malloc(sizeof(double) * (nDIM + 1));
    }
    for (i = 0; i <= nITEM; i++) {
      SCHOOL[k].sample_item_mat[i] =
          (double *)malloc(sizeof(double) * (nITEM + 1));
      SCHOOL[k].old_item_mat[i] =
          (double *)malloc(sizeof(double *) * (nITEM + 1));
      SCHOOL[k].new_item_mat[i] =
          (double *)malloc(sizeof(double *) * (nITEM + 1));
      SCHOOL[k].sum_item_mat[i] =
          (double *)malloc(sizeof(double *) * (nITEM + 1));
      SCHOOL[k].var_item_mat[i] =
          (double *)malloc(sizeof(double *) * (nITEM + 1));
    }
    printf("MEMORY SETTING: %.2d\n", k);
  }

  // NOTE: not in namespace...
  count = ivector(1, nSCHOOL);

  oldmu = dmatrix(1, nITEM * (nITEM - 1) / 2, 0, nSCHOOL);
  olddelta = dvector(1, nITEM * (nITEM - 1) / 2);
  oldsigma = dvector(1, nSCHOOL);
  oldtau = dvector(1, nITEM * (nITEM - 1) / 2);
  oldgamma = dvector(1, nITEM);
  oldvarphi = dvector(1, nITEM);

  /* model selection param and summary */
  oldeta = dvector(1, nSCHOOL);
  neweta = dvector(1, nSCHOOL);
  oldeta_or_one = dvector(1, nSCHOOL);
  xi = dvector(1, nSCHOOL);
  pi = dvector(1, nSCHOOL);
  pi_prob = dvector(1, nSCHOOL);
  sample_eta = dmatrix(1, (niter - nburn) / thin, 1, nSCHOOL);
  sample_xi = dmatrix(1, (niter - nburn) / thin, 1, nSCHOOL);
  sample_pi = dmatrix(1, (niter - nburn) / thin, 1, nSCHOOL);
  sum_eta = dvector(1, nSCHOOL);
  var_eta = dvector(1, nSCHOOL);
  sum_xi = dvector(1, nSCHOOL);
  var_xi = dvector(1, nSCHOOL);
  sum_pi = dvector(1, nSCHOOL);
  var_pi = dvector(1, nSCHOOL);

  sample_sigma = dmatrix(1, (niter - nburn) / thin, 1, nSCHOOL);
  sample_delta = dmatrix(1, (niter - nburn) / thin, 1, nITEM * (nITEM - 1) / 2);
  sample_tau = dmatrix(1, (niter - nburn) / thin, 1, nITEM * (nITEM - 1) / 2);
  sample_gamma = dmatrix(1, (niter - nburn) / thin, 1, nITEM);
  sample_varphi = dmatrix(1, (niter - nburn) / thin, 1, nITEM);
  sum_mu = dmatrix(1, nITEM * (nITEM - 1) / 2, 0, nSCHOOL);
  sum_tau = dvector(1, nITEM * (nITEM - 1) / 2);
  var_tau = dvector(1, nITEM * (nITEM - 1) / 2);
  sum_sigma = dvector(1, nSCHOOL);
  var_sigma = dvector(1, nSCHOOL);
  sum_delta = dvector(1, nITEM * (nITEM - 1) / 2);
  var_delta = dvector(1, nITEM * (nITEM - 1) / 2);
  sum_gamma = dvector(1, nITEM);
  var_gamma = dvector(1, nITEM);
  sum_varphi = dvector(1, nITEM);
  var_varphi = dvector(1, nITEM);

  mu_dist = dmatrix(1, nSCHOOL, 1, nSCHOOL);
  sum_mu_dist = dmatrix(1, nSCHOOL, 1, nSCHOOL);
  avg_ran = dvector(1, nSCHOOL);
  var_ran = dvector(1, nSCHOOL);

  // NOTE: ??? char buf[255], frname[255];
  frname[0] = 'D';
  frname[1] = 'A';
  frname[2] = 'T';
  frname[3] = 'A';
  frname[4] = '/';
  frname[5] = 'i';
  frname[6] = 't';
  frname[7] = 'e';
  frname[8] = 'm';
  frname[11] = '.';
  frname[12] = 't';
  frname[13] = 'x';
  frname[14] = 't';
  frname[15] = '\0';

  //// init
  for (k = 0; k <= nSCHOOL; k++) {
    for (i = 0; i <= nMAX; i++)
      SCHOOL[k].count_samp[i] = 0;
    for (i = 0; i <= nITEM; i++)
      SCHOOL[k].count_item[i] = 0;
    for (i = 0; i <= nMAX; i++)
      for (j = 0; j <= nITEM; j++)
        SCHOOL[k].dataset[i][j] = 0;
    for (i = 0; i <= nITEM; i++)
      SCHOOL[k].oldbeta[i] = SCHOOL[k].newbeta[i] = 0.0;
    for (i = 0; i <= nMAX; i++)
      SCHOOL[k].oldtheta[i] = SCHOOL[k].newtheta[i] = 0.0;
    for (i = 0; i <= nITEM; i++)
      for (j = 0; j <= nITEM; j++)
        SCHOOL[k].old_item_mat[i][j] = SCHOOL[k].new_item_mat[i][j] = 0.0;
    for (i = 0; i <= nITEM; i++)
      for (a = 0; a <= nMAX; a++)
        for (b = 0; b <= nMAX; b++)
          SCHOOL[k].Y[i][a][b] = 0;
    for (i = 0; i <= nMAX; i++)
      for (a = 0; a <= nITEM; a++)
        for (b = 0; b <= nITEM; b++)
          SCHOOL[k].U[i][a][b] = 0;
    for (i = 0; i <= nMAX; i++)
      for (j = 0; j <= nDIM; j++)
        SCHOOL[k].old_Zsamp[i][j] = SCHOOL[k].new_Zsamp[i][j] =
            SCHOOL[k].old_Zmean[i][j] = SCHOOL[k].new_Zmean[i][j] = 0.0;
    for (i = 0; i <= nITEM; i++)
      for (j = 0; j <= nDIM; j++)
        SCHOOL[k].old_Zitem[i][j] = SCHOOL[k].new_Zitem[i][j] = 0.0;
    for (i = 0; i <= (niter - nburn) / thin; i++) {
      SCHOOL[k].sample_sigma[i] = 0.0;
      for (j = 0; j <= nITEM; j++)
        SCHOOL[k].sample_beta[i][j] = 0.0;
      for (j = 0; j <= nMAX; j++)
        SCHOOL[k].sample_theta[i][j] = 0.0;
      for (a = 0; a <= nMAX; a++)
        for (b = 0; b <= nDIM; b++)
          SCHOOL[k].sample_Zsamp[i][a][b] = 0.0;
      for (a = 0; a <= nITEM; a++)
        for (b = 0; b <= nDIM; b++)
          SCHOOL[k].sample_Zitem[i][a][b] = 0.0;
    }
    SCHOOL[k].oldsigma = 0.0;
    SCHOOL[k].sum_sigma = SCHOOL[k].var_sigma = 0.0;
    for (i = 0; i <= nDIM; i++)
      SCHOOL[k].mean_Z[i] = 0.0;
    for (i = 0; i <= nITEM; i++)
      SCHOOL[k].var_beta[i] = SCHOOL[k].sum_beta[i] = SCHOOL[k].acc_beta[i] =
          0.0;
    for (i = 0; i <= nMAX; i++)
      SCHOOL[k].var_theta[i] = SCHOOL[k].sum_theta[i] = SCHOOL[k].acc_theta[i] =
          0.0;
    for (i = 0; i <= nMAX; i++)
      for (j = 0; j <= nDIM; j++)
        SCHOOL[k].sum_Zsamp[i][j] = SCHOOL[k].var_Zsamp[i][j] = 0.0;
    for (i = 0; i <= nITEM; i++)
      for (j = 0; j <= nDIM; j++)
        SCHOOL[k].sum_Zitem[i][j] = SCHOOL[k].var_Zitem[i][j] = 0.0;
    for (i = 0; i <= nITEM; i++)
      for (j = 0; j <= nITEM; j++)
        SCHOOL[k].sample_item_mat[i][j] = SCHOOL[k].sum_item_mat[i][j] =
            SCHOOL[k].var_item_mat[i][j] = 0.0;
    for (i = 0; i <= nMAX; i++)
      SCHOOL[k].acc_Zsamp[i] = 0.0;
    for (i = 0; i <= nITEM; i++)
      SCHOOL[k].acc_Zitem[i] = 0.0;
    if (k != 0)
      count[k] = 0;

    // NOTE: (char)(48) character code for 0
    if (k != 0) {
      if (k < 10) {
        frname[9] = (char)(48);
        frname[10] = (char)(k + 48);
      } else {
        frname[9] = (char)(k / 10 + 48);
        frname[10] = (char)(k % 10 + 48);
      }

      inp = fopen(frname, "r");
      printf("Currently Reading %s\n", frname);
      if (inp == NULL) {
        printf("Cannot open data file\n");
        return 0;
      }
      for (i = 1; i <= ncount[k]; i++)
        for (j = 1; j <= nITEM; j++) {
          fscanf(inp, "%d", &SCHOOL[k].dataset[i][j]);
          SCHOOL[k].count_samp[i] += SCHOOL[k].dataset[i][j];
          SCHOOL[k].count_item[j] += SCHOOL[k].dataset[i][j];
        }
      fclose(inp);

      printf("%.2d\n", k);
      for (i = 1; i <= ncount[k]; i++) {
        for (j = 1; j <= nITEM; j++)
          printf("%d ", SCHOOL[k].dataset[i][j]);
        printf("\n");
      }

      for (i = 1; i <= nITEM; i++)
        for (a = 2; a <= ncount[k]; a++)
          for (b = 1; b < a; b++) {
            SCHOOL[k].Y[i][a][b] =
                SCHOOL[k].dataset[a][i] * SCHOOL[k].dataset[b][i];
            SCHOOL[k].Y[i][b][a] = SCHOOL[k].Y[i][a][b];
          }

      for (a = 1; a <= ncount[k]; a++)
        for (i = 2; i <= nITEM; i++)
          for (j = 1; j < i; j++) {
            SCHOOL[k].U[a][i][j] =
                SCHOOL[k].dataset[a][i] * SCHOOL[k].dataset[a][j];
            SCHOOL[k].U[a][j][i] = SCHOOL[k].U[a][i][j];
          }
    } /// end of if(k != 0){
    printf("INITIALIZATION AND DATA LOADING: %.2d\n", k);
  }
  for (i = 1; i <= nITEM * (nITEM - 1) / 2; i++) {
    oldtau[i] = olddelta[i] = 0.0;
    for (j = 1; j <= nSCHOOL; j++)
      oldmu[i][j] = 0.0;
  }
  for (i = 1; i <= nSCHOOL; i++)
    oldsigma[i] = 0.0;
  for (i = 1; i <= nSCHOOL; i++) {
    oldeta[i] = 1.0;
    xi[i] = 0.0;
    pi[i] = 0.0;
    pi[i] = 0.0;
  }

  // Declare Additional Variables
  sample_samp_like = dvector(1, nMAX);
  old_samp_distance = dvector(1, nMAX);
  new_samp_distance = dvector(1, nMAX);

  sample_item_like = dvector(1, nITEM);
  old_item_distance = dvector(1, nITEM);
  new_item_distance = dvector(1, nITEM);
  pr_var_Z = sqrt(2.0);

  for (v = 0; v < repeat; v++) {
    // Initialize Variables
    // all random-walk MH samplers
    for (k = 1; k <= nSCHOOL; k++) {
      for (i = 1; i <= nITEM; i++)
        SCHOOL[k].oldbeta[i] = SCHOOL[k].newbeta[i] = 0.0;
      for (i = 1; i <= ncount[k]; i++)
        SCHOOL[k].oldtheta[i] = SCHOOL[k].newtheta[i] = 0.0;
      for (i = 1; i <= ncount[k]; i++)
        for (j = 1; j <= nDIM; j++)
          SCHOOL[k].old_Zsamp[i][j] = SCHOOL[k].new_Zsamp[i][j] =
              SCHOOL[k].old_Zmean[i][j] = SCHOOL[k].new_Zmean[i][j] = 0.0;
      for (i = 1; i <= nITEM; i++)
        for (j = 1; j <= nDIM; j++)
          SCHOOL[k].old_Zitem[i][j] = SCHOOL[k].new_Zitem[i][j] = 0.0;
      for (i = 1; i <= (niter - nburn) / thin; i++) {
        SCHOOL[k].sample_sigma[i] = 0.0;
        for (j = 1; j <= nITEM; j++)
          SCHOOL[k].sample_beta[i][j] = 0.0;
        for (j = 1; j <= ncount[k]; j++)
          SCHOOL[k].sample_theta[i][j] = 0.0;
        for (a = 1; a <= ncount[k]; a++)
          for (b = 1; b <= nDIM; b++)
            SCHOOL[k].sample_Zsamp[i][a][b] = 0.0;
        for (a = 1; a <= nITEM; a++)
          for (b = 1; b <= nDIM; b++)
            SCHOOL[k].sample_Zitem[i][a][b] = 0.0;
      }
      for (i = 1; i <= nITEM; i++)
        SCHOOL[k].var_beta[i] = SCHOOL[k].sum_beta[i] = SCHOOL[k].acc_beta[i] =
            0.0;
      for (i = 1; i <= ncount[k]; i++)
        SCHOOL[k].var_theta[i] = SCHOOL[k].sum_theta[i] =
            SCHOOL[k].acc_theta[i] = 0.0;
      for (i = 1; i <= ncount[k]; i++)
        for (j = 1; j <= nDIM; j++)
          SCHOOL[k].sum_Zsamp[i][j] = SCHOOL[k].var_Zsamp[i][j] = 0.0;
      for (i = 1; i <= nITEM; i++)
        for (j = 1; j <= nDIM; j++)
          SCHOOL[k].sum_Zitem[i][j] = SCHOOL[k].var_Zitem[i][j] = 0.0;
      for (i = 1; i <= nITEM; i++)
        SCHOOL[k].acc_Zitem[i] = 0.0;
      for (i = 1; i <= nMAX; i++)
        SCHOOL[k].acc_Zsamp[i] = 0.0;
      for (i = 1; i <= nITEM; i++)
        for (j = 1; j <= nITEM; j++) {
          SCHOOL[k].sample_item_mat[i][j] = 0.0;
          SCHOOL[k].old_item_mat[i][j] = SCHOOL[k].new_item_mat[i][j] = 0.0;
          SCHOOL[k].sum_item_mat[i][j] = SCHOOL[k].var_item_mat[i][j] = 0.0;
        }
      for (i = 0; i <= nDIM; i++)
        SCHOOL[k].mean_Z[i] = 0.0;
      SCHOOL[k].oldsigma = SCHOOL[k].sum_sigma = SCHOOL[k].var_sigma = 0.0;
      count[k] = 0;
    }
    for (i = 1; i <= nITEM * (nITEM - 1) / 2; i++) {
      olddelta[i] = oldtau[i] = 0.0;
      sum_delta[i] = var_delta[i] = 0.0;
      sum_tau[i] = var_tau[i] = 0.0;
      for (j = 1; j <= (niter - nburn) / thin; j++)
        sample_tau[j][i] = sample_delta[j][i] = 0.0;
    }
    for (i = 1; i <= nSCHOOL; i++) {
      oldsigma[i] = 0.0;
      sum_sigma[i] = var_sigma[i] = 0.0;
      oldeta[i] = 1.0;
      sum_eta[i] = var_eta[i] = 0.0;
      xi[i] = 0.0;
      sum_xi[i] = var_xi[i] = 0.0;
      pi[i] = 0.0;
      sum_pi[i] = var_pi[i] = 0.0;
      for (j = 1; j <= (niter - nburn) / thin; j++) {
        sample_sigma[j][i] = 0.0;
        sample_eta[j][i] = 0.0;
        sample_xi[j][i] = 0.0;
        sample_pi[j][i] = 0.0;
      }
    }
    for (k = 1; k <= nSCHOOL; k++)
      for (i = 1; i <= nITEM * (nITEM - 1) / 2; i++)
        oldmu[i][k] = sum_mu[i][k] = 0.0;
    for (i = 1; i <= nITEM; i++) {
      oldgamma[i] = oldvarphi[i] = 0.0;
      sum_gamma[i] = var_gamma[i] = 0.0;
      sum_varphi[i] = var_varphi[i] = 0.0;
      for (j = 1; j <= (niter - nburn) / thin; j++)
        sample_gamma[j][i] = sample_varphi[j][i] = 0.0;
    }
    for (i = 1; i <= nSCHOOL; i++)
      for (j = 1; j <= nSCHOOL; j++)
        sum_mu_dist[i][j] = 0.0;

    // Generate Initial Values for beta, Z, sigma
    for (i = 1; i <= nITEM * (nITEM - 1) / 2; i++) {
      // U(-1.5,1.5)
      // delta = mu in (8)
      olddelta[i] = -1.5 + 3.0 * rand() / RAND_MAX;
      // NOTE: what's tau?: delta var in Eq (8)
      oldtau[i] = 100.0;
      for (j = 1; j <= nSCHOOL; j++)
        // NOTE: mu for each school?? == delta in (7)
        oldmu[i][j] = -1.5 + 3.0 * rand() / RAND_MAX;
    }
    for (i = 1; i <= nSCHOOL; i++) {
      // variance in (7)
      oldsigma[i] = fixed_oldsigma;
    }
    for (i = 1; i <= nITEM; i++) {
      oldgamma[i] = -1.5 + 3.0 * rand() / RAND_MAX;
      // NOTE: what's varphi? sigma_i in (6)
      oldvarphi[i] = 100.0;
    }

    for (k = 1; k <= nSCHOOL; k++) {
      // sigma in the last Eq before (9)
      SCHOOL[k].oldsigma = 0.05 * 0.05;
      for (i = 1; i <= nITEM; i++)
        SCHOOL[k].oldbeta[i] = -1.5 + 3.0 * rand() / RAND_MAX;
      for (i = 1; i <= ncount[k]; i++)
        SCHOOL[k].oldtheta[i] = -1.5 + 3.0 * rand() / RAND_MAX;
      for (i = 1; i <= nITEM; i++)
        for (j = 1; j <= nDIM; j++)
          SCHOOL[k].old_Zitem[i][j] = SCHOOL[k].new_Zitem[i][j] =
              -1.5 + 3.0 * rand() / RAND_MAX;
      for (i = 1; i <= nITEM; i++)
        for (j = 1; j <= nDIM; j++)
          for (a = 1; a <= ncount[k]; a++)
            if (SCHOOL[k].dataset[a][i] == 1)
              SCHOOL[k].old_Zmean[a][j] +=
                  SCHOOL[k].old_Zitem[i][j] / (SCHOOL[k].count_samp[a] * 1.0);
      for (i = 1; i <= ncount[k]; i++)
        for (j = 1; j <= nDIM; j++)
          SCHOOL[k].new_Zmean[i][j] = SCHOOL[k].old_Zmean[i][j];
      for (i = 1; i <= ncount[k]; i++)
        for (j = 1; j <= nDIM; j++)
          // gasdev(): N(0,1)
          SCHOOL[k].new_Zsamp[i][j] = SCHOOL[k].old_Zsamp[i][j] =
              SCHOOL[k].old_Zmean[i][j] + sqrt(SCHOOL[k].oldsigma) * gasdev();
      for (i = 2; i <= nITEM; i++)
        for (j = 1; j < i; j++) {
          for (l = 1; l <= nDIM; l++)
            // item distance matrix
            SCHOOL[k].old_item_mat[i][j] += pow(
                (SCHOOL[k].old_Zitem[i][l] - SCHOOL[k].old_Zitem[j][l]), 2.0);
          SCHOOL[k].old_item_mat[i][j] = sqrt(SCHOOL[k].old_item_mat[i][j]);
          SCHOOL[k].old_item_mat[j][i] = SCHOOL[k].old_item_mat[i][j];
        }
      for (i = 1; i <= nITEM; i++)
        for (j = 1; j <= nITEM; j++)
          SCHOOL[k].new_item_mat[i][j] = SCHOOL[k].old_item_mat[i][j];
    }

    // MCMC Implementation for Parameter Estimation
    // write samples to files
    frname[0] = 'R';
    frname[1] = 'E';
    frname[2] = 'S';
    frname[3] = 'U';
    frname[4] = 'L';
    frname[5] = 'T';
    frname[6] = '/';
    frname[7] = 's';
    frname[8] = 'i';
    frname[9] = 'm';
    frname[10] = '_';
    frname[12] = (char)(48 + MM);
    frname[13] = '.';
    frname[14] = 'l';
    frname[15] = 'o';
    frname[16] = 'g';
    frname[17] = '\0';
    frname[11] = 's';
    HUR = fopen(frname, "a");
    frname[11] = 'l';
    JYW = fopen(frname, "a");
    frname[11] = 'u';
    OUT = fopen(frname, "a");
    frname[11] = 'g';
    JIN = fopen(frname, "a");
    frname[11] = 'p';
    PRT = fopen(frname, "a");
    frname[11] = 'a';
    JJW = fopen(frname, "a");
    frname[11] = 'e';
    FMS = fopen(frname, "a");

    gcount = mcount = 0;
    for (iter = 1; iter <= niter; iter++) {
      for (a = 1; a <= nSCHOOL; a++) {
        oldeta_or_one[a] = DO_MS * oldeta[a];
        for (i = 1; i <= nITEM; i++) {

#pragma omp parallel for private(j, k) default(shared)
          for (j = 1; j <= nDIM; j++) {
            SCHOOL[a].new_Zitem[i][j] =
                SCHOOL[a].old_Zitem[i][j] + jump_mu * gasdev();
            for (k = 1; k <= ncount[a]; k++)
              if (SCHOOL[a].dataset[k][i] == 1) {
                // NOTE: it seems Eq (5)
                SCHOOL[a].new_Zmean[k][j] -=
                    SCHOOL[a].old_Zitem[i][j] / (SCHOOL[a].count_samp[k] * 1.0);
                SCHOOL[a].new_Zmean[k][j] +=
                    SCHOOL[a].new_Zitem[i][j] / (SCHOOL[a].count_samp[k] * 1.0);
              }
          }
          // item distance matrix
          for (ind = 1; ind <= nITEM; ind++)
            sample_item_like[ind] = old_item_distance[ind] =
                new_item_distance[ind] = 0.0;
#pragma omp parallel for private(ind, k, l) default(shared)
          for (ind = 1; ind <= nITEM; ind++)
            if (ind != i) {
              for (l = 1; l <= nDIM; l++) {
                old_item_distance[ind] += pow(
                    (SCHOOL[a].old_Zitem[ind][l] - SCHOOL[a].old_Zitem[i][l]),
                    2.0);
                new_item_distance[ind] += pow(
                    (SCHOOL[a].new_Zitem[ind][l] - SCHOOL[a].new_Zitem[i][l]),
                    2.0);
              }
              old_item_distance[ind] = sqrt(old_item_distance[ind]);
              new_item_distance[ind] = sqrt(new_item_distance[ind]);
              // i: item, ind: item
              SCHOOL[a].new_item_mat[ind][i] = new_item_distance[ind];
              SCHOOL[a].new_item_mat[i][ind] = SCHOOL[a].new_item_mat[ind][i];
              SCHOOL[a].old_item_mat[ind][i] = old_item_distance[ind];
              SCHOOL[a].old_item_mat[i][ind] = SCHOOL[a].old_item_mat[ind][i];
              // item likelihood + fix for model selection eta
              for (k = 1; k <= ncount[a]; k++) {
                if (SCHOOL[a].U[k][ind][i] == 1) {
                  sample_item_like[ind] -= -log(
                      1.0 + exp(-(SCHOOL[a].oldtheta[k] -
                                  oldeta_or_one[a] * old_item_distance[ind])));
                  sample_item_like[ind] += -log(
                      1.0 + exp(-(SCHOOL[a].oldtheta[k] -
                                  oldeta_or_one[a] * new_item_distance[ind])));
                } else {
                  sample_item_like[ind] -= -log(
                      1.0 + exp(SCHOOL[a].oldtheta[k] -
                                oldeta_or_one[a] * old_item_distance[ind]));
                  sample_item_like[ind] += -log(
                      1.0 + exp(SCHOOL[a].oldtheta[k] -
                                oldeta_or_one[a] * new_item_distance[ind]));
                }
              }
            }
          update_like_item = 0.0; // sum: log like
          for (ind = 1; ind <= nITEM; ind++)
            update_like_item += sample_item_like[ind];

          num = den = 0.0;
          // NOTE: j = 2 b/c [1][1] = 0.0
          for (j = 2; j <= nITEM; j++)
            for (k = 1; k < j; k++) {
              if (SCHOOL[a].new_item_mat[j][k] > 0.0001)
                // dlognorm: normal log-density
                // oldmu: ??_{m,p * p} in Eq (7). (1st index is linearized)
                num += dlognorm(log(SCHOOL[a].new_item_mat[j][k]),
                                oldmu[((j - 1) * (j - 2) / 2 + k)][a],
                                sqrt(oldsigma[a]));
              else
                num +=
                    dlognorm(log(0.0001), oldmu[((j - 1) * (j - 2) / 2 + k)][a],
                             sqrt(oldsigma[a]));
              if (SCHOOL[a].old_item_mat[j][k] > 0.0001)
                den += dlognorm(log(SCHOOL[a].old_item_mat[j][k]),
                                oldmu[((j - 1) * (j - 2) / 2 + k)][a],
                                sqrt(oldsigma[a]));
              else
                den +=
                    dlognorm(log(0.0001), oldmu[((j - 1) * (j - 2) / 2 + k)][a],
                             sqrt(oldsigma[a]));
              // printf("%d %d-%.3f %.3f %.3f %.3f %.3f\n", j, k, num, den,
              // oldmu[((j-1)*(j-2)/2+k)][a],
              // log(SCHOOL[a].new_item_mat[j][k]),
              // log(SCHOOL[a].old_item_mat[j][k]));
            }

#pragma omp parallel for private(j, k) default(shared)
          for (k = 1; k <= ncount[a]; k++)
            if (SCHOOL[a].dataset[k][i] == 1)
              for (j = 1; j <= nDIM; j++) {
                num += dlognorm(SCHOOL[a].old_Zsamp[k][j],
                                SCHOOL[a].new_Zmean[k][j],
                                sqrt(SCHOOL[a].oldsigma));
                den += dlognorm(SCHOOL[a].old_Zsamp[k][j],
                                SCHOOL[a].old_Zmean[k][j],
                                sqrt(SCHOOL[a].oldsigma));
              }

          ratio = update_like_item + (num - den);
          // printf("SCHOOL-%.2d, ITEM-%.2d: Num-%.3f, Den-%.3f\n", a, i,
          // num, den);

          if (ratio > 0.0)
            accept = 1;
          else {
            un = rand() * 1.0 / RAND_MAX;
            if (log(un) < ratio)
              accept = 1;
            else
              accept = 0;
          }

          if (accept == 1) {
            for (j = 1; j <= nDIM; j++) {
              SCHOOL[a].old_Zitem[i][j] = SCHOOL[a].new_Zitem[i][j];
              for (k = 1; k <= ncount[a]; k++)
                if (SCHOOL[a].dataset[k][i] == 1)
                  SCHOOL[a].old_Zmean[k][j] = SCHOOL[a].new_Zmean[k][j];
            }
            SCHOOL[a].acc_Zitem[i] += 1.0 / niter;
            for (j = 1; j <= nITEM; j++)
              for (k = 1; k <= nITEM; k++)
                SCHOOL[a].old_item_mat[j][k] = SCHOOL[a].new_item_mat[j][k];
          } else {
            for (j = 1; j <= nDIM; j++) {
              SCHOOL[a].new_Zitem[i][j] = SCHOOL[a].old_Zitem[i][j];
              for (k = 1; k <= ncount[a]; k++)
                if (SCHOOL[a].dataset[k][i] == 1)
                  SCHOOL[a].new_Zmean[k][j] = SCHOOL[a].old_Zmean[k][j];
            }
            for (j = 1; j <= nITEM; j++)
              for (k = 1; k <= nITEM; k++)
                SCHOOL[a].new_item_mat[j][k] = SCHOOL[a].old_item_mat[j][k];
          }
        }

        for (i = 1; i <= ncount[a]; i++) {
          for (j = 1; j <= nDIM; j++)
            SCHOOL[a].new_Zsamp[i][j] =
                SCHOOL[a].old_Zsamp[i][j] + jump_W * gasdev();

          for (ind = 1; ind <= ncount[a]; ind++)
            sample_samp_like[ind] = old_samp_distance[ind] =
                new_samp_distance[ind] = 0.0;
#pragma omp parallel for private(ind, k, l) default(shared)
          for (ind = 1; ind <= ncount[a]; ind++)
            if (ind != i) {
              for (l = 1; l <= nDIM; l++) {
                old_samp_distance[ind] += pow(
                    (SCHOOL[a].old_Zsamp[ind][l] - SCHOOL[a].old_Zsamp[i][l]),
                    2.0);
                new_samp_distance[ind] += pow(
                    (SCHOOL[a].old_Zsamp[ind][l] - SCHOOL[a].new_Zsamp[i][l]),
                    2.0);
              }
              old_samp_distance[ind] = sqrt(old_samp_distance[ind]);
              new_samp_distance[ind] = sqrt(new_samp_distance[ind]);
              for (k = 1; k <= nITEM; k++) {
                if (SCHOOL[a].Y[k][ind][i] == 1) {
                  sample_samp_like[ind] -= -log(
                      1.0 +
                      exp(-(SCHOOL[a].oldbeta[k] - old_samp_distance[ind])));
                  sample_samp_like[ind] += -log(
                      1.0 +
                      exp(-(SCHOOL[a].oldbeta[k] - new_samp_distance[ind])));
                } else {
                  sample_samp_like[ind] -= -log(
                      1.0 + exp(SCHOOL[a].oldbeta[k] - old_samp_distance[ind]));
                  sample_samp_like[ind] += -log(
                      1.0 + exp(SCHOOL[a].oldbeta[k] - new_samp_distance[ind]));
                }
              }
            }
          update_like_samp = 0.0;
          for (ind = 1; ind <= ncount[a]; ind++)
            update_like_samp += sample_samp_like[ind];
          // printf("SCHOOL-%.2d, PERSON-%.2d:
          // LIKELIHOOD_PERSON-%.3f\n", a, i, update_like_samp);

          num = den = 0.0;
          // printf("SCHOOL-%.2d, PERSON-%.2d: Num-%.3f, Den-%.3f\n", a,
          // i, num, den);
          for (j = 1; j <= nDIM; j++) {
            num +=
                dlognorm(SCHOOL[a].new_Zsamp[i][j], SCHOOL[a].old_Zmean[i][j],
                         sqrt(SCHOOL[a].oldsigma));
            den +=
                dlognorm(SCHOOL[a].old_Zsamp[i][j], SCHOOL[a].old_Zmean[i][j],
                         sqrt(SCHOOL[a].oldsigma));
          }
          ratio = update_like_samp + (num - den);
          // printf("SCHOOL-%.2d, PERSON-%.2d: Num-%.3f, Den-%.3f\n", a,
          // i, num, den);

          if (ratio > 0.0)
            accept = 1;
          else {
            un = rand() * 1.0 / RAND_MAX;
            if (log(un) < ratio)
              accept = 1;
            else
              accept = 0;
          }

          if (accept == 1) {
            for (j = 1; j <= nDIM; j++)
              // i: for (i = 1; i <= ncount[a]; i++)
              SCHOOL[a].old_Zsamp[i][j] = SCHOOL[a].new_Zsamp[i][j];
            SCHOOL[a].acc_Zsamp[i] += 1.0 / niter;
          } else {
            for (j = 1; j <= nDIM; j++)
              SCHOOL[a].new_Zsamp[i][j] = SCHOOL[a].old_Zsamp[i][j];
          }
        } // end of for (i = 1; i <= ncount[a]; i++)

        SCHOOL[a].post_a = prior_a;
        SCHOOL[a].post_b = prior_b;
        for (i = 1; i <= ncount[a]; i++)
          for (j = 1; j <= nDIM; j++) {
            SCHOOL[a].post_a += 0.5;
            SCHOOL[a].post_b +=
                0.5 * (SCHOOL[a].old_Zsamp[i][j] - SCHOOL[a].old_Zmean[i][j]) *
                (SCHOOL[a].old_Zsamp[i][j] - SCHOOL[a].old_Zmean[i][j]);
          }
        // NOTE: person position var (Eq 5)
        SCHOOL[a].oldsigma = 1.0 / Rgamma(SCHOOL[a].post_a, SCHOOL[a].post_b);


// 2. Update $\beta_i$ from the proposal distribution $\phi_2(\cdot)$
#pragma omp parallel for private(i, j, k, old_like_beta, new_like_beta, num,   \
                                 den, accept, ratio, un) default(shared)
        for (i = 1; i <= nITEM; i++) {
          old_like_beta = cost_beta(i, SCHOOL[a].oldbeta[i], a); // log-like
          SCHOOL[a].newbeta[i] = SCHOOL[a].oldbeta[i] + jump_beta * gasdev();
          if (fabs(SCHOOL[a].newbeta[i]) < 7.0) {
            new_like_beta = cost_beta(i, SCHOOL[a].newbeta[i], a);
            num = new_like_beta;
            den = old_like_beta;

            // oldvarphi: beta var Eq (6)
            num +=
                dlognorm(SCHOOL[a].oldbeta[i], oldgamma[i], sqrt(oldvarphi[i]));
            den +=
                dlognorm(SCHOOL[a].newbeta[i], oldgamma[i], sqrt(oldvarphi[i]));
            ratio = num - den;

            if (ratio > 0.0)
              accept = 1;
            else {
              un = rand() * 1.0 / RAND_MAX;
              if (log(un) < ratio)
                accept = 1;
              else
                accept = 0;
            }
          } else
            accept = 0;

          if (accept == 1) {
            SCHOOL[a].oldbeta[i] = SCHOOL[a].newbeta[i];
            SCHOOL[a].acc_beta[i] += 1.0 / niter;
          } else
            SCHOOL[a].newbeta[i] = SCHOOL[a].oldbeta[i];
        }

#pragma omp parallel for private(i, old_like_theta, new_like_theta, num, den,  \
                                 accept, ratio, un) default(shared)
        for (i = 1; i <= ncount[a]; i++) {
          old_like_theta = cost_theta(i, SCHOOL[a].oldtheta[i], a);
          SCHOOL[a].newtheta[i] = SCHOOL[a].oldtheta[i] + jump_theta * gasdev();
          new_like_theta = cost_theta(i, SCHOOL[a].newtheta[i], a);

          num = dlognorm(SCHOOL[a].newtheta[i], pr_mean_theta, pr_var_theta) +
                new_like_theta;
          den = dlognorm(SCHOOL[a].oldtheta[i], pr_mean_theta, pr_var_theta) +
                old_like_theta;
          ratio = num - den;

          if (ratio > 0.0)
            accept = 1;
          else {
            un = rand() * 1.0 / RAND_MAX;
            if (log(un) < ratio)
              accept = 1;
            else
              accept = 0;
          }

          if (accept == 1) {
            SCHOOL[a].oldtheta[i] = SCHOOL[a].newtheta[i];
            SCHOOL[a].acc_theta[i] += 1.0 / niter;
          } else
            SCHOOL[a].newtheta[i] = SCHOOL[a].oldtheta[i];
        }

        // Save MCMC Results to Files and Repository Variables
        // z, w, beta, theta, sigma (beta var)...
        if (iter > nburn && iter % thin == 0) {
          count[a]++;
          for (i = 1; i <= ncount[a]; i++)
            for (j = 1; j <= nDIM; j++)
              SCHOOL[a].sample_Zsamp[count[a]][i][j] =
                  SCHOOL[a].old_Zsamp[i][j];
          for (i = 1; i <= nITEM; i++)
            for (j = 1; j <= nDIM; j++)
              SCHOOL[a].sample_Zitem[count[a]][i][j] =
                  SCHOOL[a].old_Zitem[i][j];
          for (i = 1; i <= nITEM; i++)
            SCHOOL[a].sample_beta[count[a]][i] = SCHOOL[a].oldbeta[i];
          for (i = 1; i <= ncount[a]; i++)
            SCHOOL[a].sample_theta[count[a]][i] = SCHOOL[a].oldtheta[i];
          SCHOOL[a].sample_sigma[count[a]] = SCHOOL[a].oldsigma;
        }

        // Print MCMC Results to Screen
        if (iter % print == 0) {
          printf("%.5d-BETA%.2d  ", iter, a);
          for (i = 1; i <= nITEM; i++)
            printf("% .4f ", SCHOOL[a].oldbeta[i]);
          printf("%.4f\n", SCHOOL[a].oldsigma);
        }
      }

      // beta var update
#pragma omp parallel for private(i, j, school_a, school_b, avg_beta,           \
                                 var_beta) default(shared)
      for (i = 1; i <= nITEM; i++) {
        school_a = prior_a;
        school_b = prior_b;
        for (j = 1; j <= nSCHOOL; j++) {
          school_a += 0.5;
          school_b += 0.5 * (SCHOOL[j].oldbeta[i] - oldgamma[i]) *
                      (SCHOOL[j].oldbeta[i] - oldgamma[i]);
        }
        oldvarphi[i] = 1.0 / Rgamma(school_a, school_b);

        var_beta = 1.0 / (1.0 / pr_var_gamma + nSCHOOL / oldvarphi[i]);
        avg_beta = 0.0;
        for (j = 1; j <= nSCHOOL; j++)
          avg_beta += SCHOOL[j].oldbeta[i] / nSCHOOL;
        avg_beta *= var_beta * (nSCHOOL / oldvarphi[i]);
        oldgamma[i] = avg_beta + sqrt(var_beta) * gasdev();
      }

      // dist mat var, Eq (7), update
#pragma omp parallel for private(i, j, k, post_a, post_b) default(shared)
      for (k = 1; k <= nSCHOOL; k++) {
        if (DO_MS == 0) {
          // no variance update (7) if model selection in on
          post_a = prior_a;
          post_b = prior_b;
          for (i = 2; i <= nITEM; i++)
            for (j = 1; j < i; j++) {
              post_a += 0.5;
              if (SCHOOL[k].old_item_mat[i][j] > 0.0001)
                post_b += 0.5 *
                          (log(SCHOOL[k].old_item_mat[i][j]) -
                           oldmu[((i - 1) * (i - 2) / 2 + j)][k]) *
                          (log(SCHOOL[k].old_item_mat[i][j]) -
                           oldmu[((i - 1) * (i - 2) / 2 + j)][k]);
              else
                post_b +=
                    0.5 *
                    (log(0.0001) - oldmu[((i - 1) * (i - 2) / 2 + j)][k]) *
                    (log(0.0001) - oldmu[((i - 1) * (i - 2) / 2 + j)][k]);
            }
          oldsigma[k] =
              1.0 /
              Rgamma(post_a, post_b); // Change Array of Variables oldsigma
        }
      }

      for (i = 2; i <= nITEM; i++)
        for (j = 1; j < i; j++) {
          post_a = prior_a;
          post_b = prior_b;
#pragma omp parallel for private(k) default(shared)
          for (k = 1; k <= nSCHOOL; k++) {
            post_a += 0.5;
            post_b += 0.5 *
                      (oldmu[((i - 1) * (i - 2) / 2 + j)][k] -
                       olddelta[((i - 1) * (i - 2) / 2 + j)]) *
                      (oldmu[((i - 1) * (i - 2) / 2 + j)][k] -
                       olddelta[((i - 1) * (i - 2) / 2 + j)]);
          }
          // Rgamma: gen gamma random number
          // ????_?? update Eq (8)
          oldtau[((i - 1) * (i - 2) / 2 + j)] = 1.0 / Rgamma(post_a, post_b);

#pragma omp parallel for private(k) default(shared)
          for (k = 1; k <= nSCHOOL; k++) {
            // dist mat prec + its prior precision
            var_ran[k] =
                1.0 / (1.0 / oldtau[((i - 1) * (i - 2) / 2 + j)] +
                       1.0 / oldsigma[k]); // Change Array of Variable  var_ran
            avg_ran[k] = (1.0 / oldtau[((i - 1) * (i - 2) / 2 + j)]) *
                         olddelta[((i - 1) * (i - 2) / 2 + j)];
            if (SCHOOL[k].old_item_mat[i][j] > 0.0001)
              avg_ran[k] +=
                  (1.0 / oldsigma[k]) * log(SCHOOL[k].old_item_mat[i][j]);
            else
              avg_ran[k] += (1.0 / oldsigma[k]) * log(0.0001);
            avg_ran[k] *= var_ran[k];
            // NOTE: update ?? in Eq 7 ((log)-normal + normal model)
            oldmu[((i - 1) * (i - 2) / 2 + j)][k] =
                avg_ran[k] + sqrt(var_ran[k]) * gasdev();
          }

          var_fix = 1.0 / (1.0 / pr_var_delta +
                           nSCHOOL / oldtau[((i - 1) * (i - 2) / 2 + j)]);
          avg_fix = 0.0;
#pragma omp parallel for private(k) default(shared)
          for (k = 1; k <= nSCHOOL; k++)
            avg_fix += oldmu[((i - 1) * (i - 2) / 2 + j)][k] / nSCHOOL;
          avg_fix *= var_fix * (nSCHOOL / oldtau[((i - 1) * (i - 2) / 2 + j)]);
          // update ?? in Eq 8
          olddelta[((i - 1) * (i - 2) / 2 + j)] =
              avg_fix + sqrt(var_fix) * gasdev();
        }

      if (DO_MS == 1) {
        // model selection update: eta
        for (a = 1; a <= nSCHOOL; a++) {
          neweta[a] = lognormal_rng(log(oldeta[a]), jump_eta);
          old_like_eta = new_like_eta = 0.0;

          for (ind = 1; ind <= nITEM; ind++) {
            sample_item_like[ind] = 0.0;
#pragma omp parallel for private(ind, k, l) default(shared)
            for (ind = 1; ind <= nITEM; ind++)
              if (ind != i) {
                // item likelihood + fix for model selection eta
                for (k = 1; k <= ncount[a]; k++) {
                  if (SCHOOL[a].U[k][ind][i] == 1) {
                    sample_item_like[ind] -=
                        -log(1.0 + exp(-(SCHOOL[a].oldtheta[k] -
                                         oldeta[a] * old_item_distance[ind])));
                    sample_item_like[ind] +=
                        -log(1.0 + exp(-(SCHOOL[a].oldtheta[k] -
                                         neweta[a] * old_item_distance[ind])));
                  } else {
                    sample_item_like[ind] -=
                        -log(1.0 + exp(SCHOOL[a].oldtheta[k] -
                                       oldeta[a] * old_item_distance[ind]));
                    sample_item_like[ind] +=
                        -log(1.0 + exp(SCHOOL[a].oldtheta[k] -
                                       neweta[a] * old_item_distance[ind]));
                  }
                }
              }
            update_like_item = 0.0; // sum: log like
            for (ind = 1; ind <= nITEM; ind++)
              update_like_item += sample_item_like[ind];

            num = lognormal_lpdf(oldeta[a], log(neweta[a]), jump_eta);
            den = lognormal_lpdf(neweta[a], log(oldeta[a]), jump_eta);
            if (pi[a] == 1) {
              num += lognormal_lpdf(neweta[a], pr_slab_mean, pr_slab_sd);
              den += lognormal_lpdf(oldeta[a], pr_slab_mean, pr_slab_sd);
            } else {
              num += lognormal_lpdf(neweta[a], pr_spike_mean, pr_spike_sd);
              den += lognormal_lpdf(oldeta[a], pr_spike_mean, pr_spike_sd);
            }

            ratio = update_like_item + num - den;
            // printf("SCHOOL-%.2d, ITEM-%.2d: Num-%.3f, Den-%.3f\n", a, i,
            // num, den);

            if (ratio > 0.0)
              accept = 1;
            else {
              un = rand() * 1.0 / RAND_MAX;
              if (log(un) < ratio)
                accept = 1;
              else
                accept = 0;
            }

            if (accept == 1) {
              oldeta[a] = neweta[a];
            } else {
              neweta[a] = oldeta[a];
            }
          }

          /* xi update */
          // pi update(spike slab parameter)
          pi_prob[a] =
              xi[a] * exp(lognormal_lpdf(oldeta[a], pr_slab_mean, pr_slab_sd));
          pi_prob[a] /=
              (1.0 - xi[a]) *
                  exp(lognormal_lpdf(oldeta[a], pr_spike_mean, pr_spike_sd)) +
              xi[a] * exp(lognormal_lpdf(oldeta[a], pr_slab_mean, pr_slab_sd));
          pi[a] = bernoulli_rng(pi_prob[a]);

          // xi[a] update(spi[a]ke slab parameter)
          /* printf("%.4f\n", pi[a]); */
          /* printf("%.4f\n", pr_beta_a + pi[a]); */
          /* printf("%.4f\n",  pr_beta_b + 1.0 - pi[a]); */
          // FIXME: Gamma parameter error (<0.0)
          // only during parallelization
          xi[a] = beta_rng(pr_beta_a + pi[a], pr_beta_b + 1.0 - pi[a]);
        }
      }
      if (iter % print == 0)
        for (i = 1; i <= nITEM; i++) {
          printf("%.5d-GAMMA, VARPHI, ITEM%.2d: ", iter, i);
          printf("% .4f %.4f\n", oldgamma[i], oldvarphi[i]);
        }

      if (iter > nburn && iter % thin == 0) {
        gcount++;
        for (i = 1; i <= nITEM * (nITEM - 1) / 2; i++) {
          sample_tau[gcount][i] = sqrt(oldtau[i]);
          sample_delta[gcount][i] = olddelta[i];
          fprintf(JYW, "% .4f ", sample_delta[gcount][i]);
          fprintf(OUT, "%.4f ", sample_tau[gcount][i]);
        }
        for (i = 1; i <= nSCHOOL; i++) {
          sample_sigma[gcount][i] = sqrt(oldsigma[i]);
          fprintf(HUR, "%.4f ", sample_sigma[gcount][i]);
          if (DO_MS == 1) {
          sample_eta[gcount][i] = oldeta[i];
          fprintf(FMS, "%.4f ", sample_eta[gcount][i]);
          sample_xi[gcount][i] = xi[i];
          fprintf(FMS, "%.4f ", sample_xi[gcount][i]);
          sample_pi[gcount][i] = pi[i];
          fprintf(FMS, "%.4f ", sample_pi[gcount][i]);
          }
        }
        for (k = 1; k <= nSCHOOL; k++)
          for (i = 1; i <= nITEM * (nITEM - 1) / 2; i++)
            sum_mu[i][k] += oldmu[i][k] / ((niter - nburn) / thin);
        for (i = 1; i <= nITEM; i++) {
          sample_gamma[gcount][i] = oldgamma[i];
          sample_varphi[gcount][i] = sqrt(oldvarphi[i]);
          fprintf(JIN, "% .4f ", sample_gamma[gcount][i]);
          fprintf(PRT, "%.4f ", sample_varphi[gcount][i]);
        }
        for (i = 1; i <= nSCHOOL; i++)
          for (j = 1; j <= nSCHOOL; j++)
            mu_dist[i][j] = 0.0;
        for (k = 1; k <= nITEM * (nITEM - 1) / 2; k++)
          for (i = 2; i <= nSCHOOL; i++)
            for (j = 1; j < i; j++)
              mu_dist[i][j] +=
                  (oldmu[k][i] - oldmu[k][j]) * (oldmu[k][i] - oldmu[k][j]);
        for (i = 2; i <= nSCHOOL; i++)
          for (j = 1; j < i; j++)
            mu_dist[j][i] = mu_dist[i][j];
        for (i = 1; i <= nSCHOOL; i++)
          for (j = 1; j <= nSCHOOL; j++)
            // averaged S_qr in P10
            sum_mu_dist[i][j] += sqrt(mu_dist[i][j]) / ((niter - nburn) / thin);
        for (i = 2; i <= nSCHOOL; i++)
          for (j = 1; j < i; j++)
            fprintf(JJW, "%.4f ", sqrt(mu_dist[i][j]));

        fprintf(HUR, "\n");
        fprintf(OUT, "\n");
        fprintf(JYW, "\n");
        fprintf(JIN, "\n");
        fprintf(PRT, "\n");
        fprintf(JJW, "\n");
        fprintf(FMS, "\n");
      }
    }
    fclose(HUR);
    fclose(JYW);
    fclose(OUT);
    fclose(JIN);
    fclose(PRT);
    fclose(JJW);
    fclose(FMS);

    // write MCMC samples (school level)
    frname[0] = 'R';
    frname[1] = 'E';
    frname[2] = 'S';
    frname[3] = 'U';
    frname[4] = 'L';
    frname[5] = 'T';
    frname[6] = '/';
    frname[7] = 's';
    frname[8] = 'i';
    frname[9] = 'm';
    frname[12] = '_';
    frname[14] = (char)(48 + MM);
    frname[15] = '.';
    frname[16] = 'l';
    frname[17] = 'o';
    frname[18] = 'g';
    frname[19] = '\0';
    for (a = 1; a <= nSCHOOL; a++) {
      if (a < 10) {
        frname[10] = (char)(48);
        frname[11] = (char)(a + 48);
      } else {
        frname[10] = (char)(a / 10 + 48);
        frname[11] = (char)(a % 10 + 48);
      }

      frname[13] = 'z';
      JIN = fopen(frname, "a");
      frname[13] = 'b';
      HUR = fopen(frname, "a");
      frname[13] = 't';
      OUT = fopen(frname, "a");
      frname[13] = 'i';
      JYW = fopen(frname, "a");
      frname[13] = 'h';
      ASA = fopen(frname, "a");

      for (k = 1; k <= count[a]; k++) {
        for (i = 1; i <= ncount[a]; i++)
          for (j = 1; j <= nDIM; j++)
            fprintf(JIN, "% .4f ", SCHOOL[a].sample_Zsamp[k][i][j]);
        fprintf(JIN, "\n");

        for (i = 1; i <= nITEM; i++)
          for (j = 1; j <= nDIM; j++)
            fprintf(JYW, "% .4f ", SCHOOL[a].sample_Zitem[k][i][j]);
        fprintf(JYW, "\n");

        for (i = 1; i <= nITEM; i++)
          fprintf(HUR, "% .4f ", SCHOOL[a].sample_beta[k][i]);
        fprintf(HUR, "\n");

        for (i = 1; i <= ncount[a]; i++)
          fprintf(OUT, "% .4f ", SCHOOL[a].sample_theta[k][i]);
        fprintf(OUT, "\n");
        fprintf(ASA, "%.4f\n", SCHOOL[a].sample_sigma[k]);
      }

      fclose(JIN);
      fclose(HUR);
      fclose(OUT);
      fclose(JYW);
      fclose(ASA);
    }

    // Calculate Mean and Variance of MCMC Estimators
    for (a = 1; a <= nSCHOOL; a++) {
      for (i = 1; i <= count[a]; i++) {
        SCHOOL[a].sum_sigma += SCHOOL[a].sample_sigma[i] / count[a];
        SCHOOL[a].var_sigma += SCHOOL[a].sample_sigma[i] *
                               SCHOOL[a].sample_sigma[i] / (count[a] - 1);
        for (j = 1; j <= nITEM; j++) {
          SCHOOL[a].sum_beta[j] += SCHOOL[a].sample_beta[i][j] / count[a];
          SCHOOL[a].var_beta[j] += SCHOOL[a].sample_beta[i][j] *
                                   SCHOOL[a].sample_beta[i][j] / (count[a] - 1);
        }
        for (j = 1; j <= ncount[a]; j++) {
          SCHOOL[a].sum_theta[j] += SCHOOL[a].sample_theta[i][j] / count[a];
          SCHOOL[a].var_theta[j] += SCHOOL[a].sample_theta[i][j] *
                                    SCHOOL[a].sample_theta[i][j] /
                                    (count[a] - 1);
        }
        for (j = 1; j <= ncount[a]; j++)
          for (k = 1; k <= nDIM; k++) {
            SCHOOL[a].sum_Zsamp[j][k] +=
                SCHOOL[a].sample_Zsamp[i][j][k] / count[a];
            SCHOOL[a].var_Zsamp[j][k] += SCHOOL[a].sample_Zsamp[i][j][k] *
                                         SCHOOL[a].sample_Zsamp[i][j][k] /
                                         (count[a] - 1);
          }
        for (j = 1; j <= nITEM; j++)
          for (k = 1; k <= nDIM; k++) {
            SCHOOL[a].sum_Zitem[j][k] +=
                SCHOOL[a].sample_Zitem[i][j][k] / count[a];
            SCHOOL[a].var_Zitem[j][k] += SCHOOL[a].sample_Zitem[i][j][k] *
                                         SCHOOL[a].sample_Zitem[i][j][k] /
                                         (count[a] - 1);
          }
        for (j = 1; j <= nITEM; j++)
          for (k = 1; k <= nITEM; k++)
            SCHOOL[a].sample_item_mat[j][k] = 0.0;
        for (j = 2; j <= nITEM; j++)
          for (k = 1; k < j; k++)
            for (l = 1; l <= nDIM; l++)
              SCHOOL[a].sample_item_mat[j][k] +=
                  pow((SCHOOL[a].sample_Zitem[i][j][l] -
                       SCHOOL[a].sample_Zitem[i][k][l]),
                      2.0);
        for (j = 2; j <= nITEM; j++)
          for (k = 1; k < j; k++)
            SCHOOL[a].sample_item_mat[k][j] = SCHOOL[a].sample_item_mat[j][k];
        for (j = 1; j <= nITEM; j++)
          for (k = 1; k <= nITEM; k++) {
            SCHOOL[a].sum_item_mat[j][k] +=
                SCHOOL[a].sample_item_mat[j][k] / count[a];
            SCHOOL[a].var_item_mat[j][k] += SCHOOL[a].sample_item_mat[j][k] *
                                            SCHOOL[a].sample_item_mat[j][k] /
                                            (count[a] - 1);
          }
      }
      SCHOOL[a].var_sigma -=
          SCHOOL[a].sum_sigma * SCHOOL[a].sum_sigma * count[a] / (count[a] - 1);
      for (i = 1; i <= nITEM; i++)
        SCHOOL[a].var_beta[i] -= SCHOOL[a].sum_beta[i] * SCHOOL[a].sum_beta[i] *
                                 count[a] / (count[a] - 1);
      for (i = 1; i <= ncount[a]; i++)
        SCHOOL[a].var_theta[i] -= SCHOOL[a].sum_theta[i] *
                                  SCHOOL[a].sum_theta[i] * count[a] /
                                  (count[a] - 1);
      for (i = 1; i <= ncount[a]; i++)
        for (j = 1; j <= nDIM; j++)
          SCHOOL[a].var_Zsamp[i][j] -= SCHOOL[a].sum_Zsamp[i][j] *
                                       SCHOOL[a].sum_Zsamp[i][j] * count[a] /
                                       (count[a] - 1);
      for (i = 1; i <= nITEM; i++)
        for (j = 1; j <= nDIM; j++)
          SCHOOL[a].var_Zitem[i][j] -= SCHOOL[a].sum_Zitem[i][j] *
                                       SCHOOL[a].sum_Zitem[i][j] * count[a] /
                                       (count[a] - 1);
      for (i = 1; i <= nITEM; i++)
        for (j = 1; j <= nITEM; j++)
          SCHOOL[a].var_item_mat[i][j] -= SCHOOL[a].sum_item_mat[i][j] *
                                          SCHOOL[a].sum_item_mat[i][j] *
                                          count[a] / (count[a] - 1);
    }

    for (i = 1; i <= gcount; i++) {
      for (j = 1; j <= nITEM * (nITEM - 1) / 2; j++) {
        sum_tau[j] += sample_tau[i][j] / gcount;
        sum_delta[j] += sample_delta[i][j] / gcount;
        var_tau[j] += sample_tau[i][j] * sample_tau[i][j] / (gcount - 1);
        var_delta[j] += sample_delta[i][j] * sample_delta[i][j] / (gcount - 1);
      }
      for (j = 1; j <= nSCHOOL; j++) {
        sum_sigma[j] += sample_sigma[i][j] / gcount;
        var_sigma[j] += sample_sigma[i][j] * sample_sigma[i][j] / (gcount - 1);
        if (DO_MS == 1) {
        sum_eta[j] += sample_eta[i][j] / gcount;
        var_eta[j] += sample_eta[i][j] * sample_eta[i][j] / (gcount - 1);
        sum_xi[j] += sample_xi[i][j] / gcount;
        var_xi[j] += sample_xi[i][j] * sample_xi[i][j] / (gcount - 1);
        sum_pi[j] += sample_pi[i][j] / gcount;
        var_pi[j] += sample_pi[i][j] * sample_pi[i][j] / (gcount - 1);
        }
      }
      for (j = 1; j <= nITEM; j++) {
        sum_gamma[j] += sample_gamma[i][j] / gcount;
        sum_varphi[j] += sample_varphi[i][j] / gcount;
        var_gamma[j] += sample_gamma[i][j] * sample_gamma[i][j] / (gcount - 1);
        var_varphi[j] +=
            sample_varphi[i][j] * sample_varphi[i][j] / (gcount - 1);
      }
    }

    for (i = 1; i <= nITEM * (nITEM - 1) / 2; i++) {
      var_tau[i] -= sum_tau[i] * sum_tau[i] * gcount / (gcount - 1);
      var_delta[i] -= sum_delta[i] * sum_delta[i] * gcount / (gcount - 1);
    }
    for (i = 1; i <= nSCHOOL; i++)
      var_sigma[i] -= sum_sigma[i] * sum_sigma[i] * gcount / (gcount - 1);
    for (i = 1; i <= nITEM; i++) {
      var_gamma[i] -= sum_gamma[i] * sum_gamma[i] * gcount / (gcount - 1);
      var_varphi[i] -= sum_varphi[i] * sum_varphi[i] * gcount / (gcount - 1);
    }

    // Save Parameter Estimates
    // write summary statistics (school level)
    frname[0] = 'R';
    frname[1] = 'E';
    frname[2] = 'S';
    frname[3] = 'U';
    frname[4] = 'L';
    frname[5] = 'T';
    frname[6] = '/';
    frname[7] = 's';
    frname[8] = 'u';
    frname[9] = 'm';
    frname[12] = '_';
    frname[14] = (char)(48 + MM);
    frname[15] = '.';
    frname[16] = 'l';
    frname[17] = 'o';
    frname[18] = 'g';
    frname[19] = '\0';

    for (a = 1; a <= nSCHOOL; a++) {
      if (a < 10) {
        frname[10] = (char)(48);
        frname[11] = (char)(a + 48);
      } else {
        frname[10] = (char)(a / 10 + 48);
        frname[11] = (char)(a % 10 + 48);
      }

      frname[13] = 'z';
      JIN = fopen(frname, "a");
      frname[13] = 'b';
      HUR = fopen(frname, "a");
      frname[13] = 't';
      OUT = fopen(frname, "a");
      frname[13] = 'i';
      JYW = fopen(frname, "a");
      frname[13] = 'd';
      PRT = fopen(frname, "a");

      for (i = 1; i <= nITEM; i++)
        fprintf(HUR, "%.4f ", SCHOOL[a].sum_beta[i]);
      fprintf(HUR, "\n");
      for (i = 1; i <= nITEM; i++)
        fprintf(HUR, "%.4f ", SCHOOL[a].var_beta[i]);
      fprintf(HUR, "\n");
      for (i = 1; i <= nITEM; i++)
        fprintf(HUR, "%.4f ", SCHOOL[a].acc_beta[i]);
      fprintf(HUR, "\n");

      for (i = 1; i <= ncount[a]; i++)
        fprintf(OUT, "%.4f ", SCHOOL[a].sum_theta[i]);
      fprintf(OUT, "\n");
      for (i = 1; i <= ncount[a]; i++)
        fprintf(OUT, "%.4f ", SCHOOL[a].var_theta[i]);
      fprintf(OUT, "\n");
      for (i = 1; i <= ncount[a]; i++)
        fprintf(OUT, "%.4f ", SCHOOL[a].acc_theta[i]);
      fprintf(OUT, "\n");

      for (i = 1; i <= ncount[a]; i++)
        for (j = 1; j <= nDIM; j++)
          fprintf(JIN, "%.4f ", SCHOOL[a].sum_Zsamp[i][j]);
      fprintf(JIN, "\n");
      for (i = 1; i <= ncount[a]; i++)
        for (j = 1; j <= nDIM; j++)
          fprintf(JIN, "%.4f ", SCHOOL[a].var_Zsamp[i][j]);
      fprintf(JIN, "\n");
      for (i = 1; i <= ncount[a]; i++)
        for (j = 1; j <= nDIM; j++)
          fprintf(JIN, "%.4f ", SCHOOL[a].acc_Zsamp[i]);
      fprintf(JIN, "\n");

      for (i = 1; i <= nITEM; i++)
        for (j = 1; j <= nDIM; j++)
          fprintf(JYW, "%.4f ", SCHOOL[a].sum_Zitem[i][j]);
      fprintf(JYW, "\n");
      for (i = 1; i <= nITEM; i++)
        for (j = 1; j <= nDIM; j++)
          fprintf(JYW, "%.4f ", SCHOOL[a].var_Zitem[i][j]);
      fprintf(JYW, "\n");
      for (i = 1; i <= nITEM; i++)
        for (j = 1; j <= nDIM; j++)
          fprintf(JYW, "%.4f ", SCHOOL[a].acc_Zitem[i]);
      fprintf(JYW, "\n");

      for (i = 2; i <= nITEM; i++)
        for (j = 1; j < i; j++)
          fprintf(PRT, "%.4f ", SCHOOL[a].sum_item_mat[i][j]);
      fprintf(PRT, "\n");
      for (i = 2; i <= nITEM; i++)
        for (j = 1; j < i; j++)
          fprintf(PRT, "%.4f ", SCHOOL[a].var_item_mat[i][j]);
      fprintf(PRT, "\n");

        fclose(JIN);
        fclose(HUR);
        fclose(OUT);
        fclose(JYW);
        fclose(PRT);
      }

    // writw summary statistics
    frname[0] = 'R';
    frname[1] = 'E';
    frname[2] = 'S';
    frname[3] = 'U';
    frname[4] = 'L';
    frname[5] = 'T';
    frname[6] = '/';
    frname[7] = 's';
    frname[8] = 'u';
    frname[9] = 'm';
    frname[10] = '_';
    frname[12] = (char)(48 + MM);
    frname[13] = '.';
    frname[14] = 'l';
    frname[15] = 'o';
    frname[16] = 'g';
    frname[17] = '\0';

    frname[11] = 'm';
    JIN = fopen(frname, "a");
    frname[11] = 's';
    HUR = fopen(frname, "a");
    frname[11] = 'l';
    JYW = fopen(frname, "a");
    frname[11] = 'u';
    OUT = fopen(frname, "a");
    frname[11] = 'g';
    ASA = fopen(frname, "a");
    frname[11] = 'p';
    PRT = fopen(frname, "a");
    frname[11] = 'h';
    IMS = fopen(frname, "a");
    frname[11] = 'a';
    JJW = fopen(frname, "a");
    frname[11] = 'e';
    FMS = fopen(frname, "a");

    for (i = 1; i <= nSCHOOL; i++)
      fprintf(HUR, "% .4f ", sum_sigma[i]);
    fprintf(HUR, "\n");
    for (i = 1; i <= nSCHOOL; i++)
      fprintf(HUR, "% .4f ", var_sigma[i]);
    fprintf(HUR, "\n");

    for (i = 1; i <= nITEM * (nITEM - 1) / 2; i++)
      fprintf(OUT, "% .4f ", sum_tau[i]);
    fprintf(OUT, "\n");
    for (i = 1; i <= nITEM * (nITEM - 1) / 2; i++)
      fprintf(OUT, "% .4f ", var_tau[i]);
    fprintf(OUT, "\n");

    for (i = 1; i <= nITEM * (nITEM - 1) / 2; i++)
      fprintf(JYW, "% .4f ", sum_delta[i]);
    fprintf(JYW, "\n");
    for (i = 1; i <= nITEM * (nITEM - 1) / 2; i++)
      fprintf(JYW, "% .4f ", var_delta[i]);
    fprintf(JYW, "\n");

    for (k = 1; k <= nSCHOOL; k++) {
      for (i = 1; i <= nITEM * (nITEM - 1) / 2; i++)
        fprintf(JIN, "% .4f ", sum_mu[i][k]);
      fprintf(JIN, "\n");
    }

    for (i = 1; i <= nITEM; i++)
      fprintf(ASA, "% .4f ", sum_gamma[i]);
    fprintf(ASA, "\n");
    for (i = 1; i <= nITEM; i++)
      fprintf(ASA, "% .4f ", var_gamma[i]);
    fprintf(ASA, "\n");
    for (i = 1; i <= nITEM; i++)
      fprintf(PRT, "%.4f ", sum_varphi[i]);
    fprintf(PRT, "\n");
    for (i = 1; i <= nITEM; i++)
      fprintf(PRT, "%.4f ", var_varphi[i]);
    fprintf(PRT, "\n");

    for (k = 1; k <= nSCHOOL; k++)
      fprintf(IMS, "%.4f ", SCHOOL[k].sum_sigma);
    fprintf(IMS, "\n");
    for (k = 1; k <= nSCHOOL; k++)
      fprintf(IMS, "%.4f ", SCHOOL[k].var_sigma);
    fprintf(IMS, "\n");

    for (i = 1; i <= nSCHOOL; i++) {
      for (j = 1; j <= nSCHOOL; j++)
        fprintf(JJW, "%.4f ", sum_mu_dist[i][j]);
      fprintf(JJW, "\n");
    }

    if (DO_MS == 1) {
      for (k = 1; k <= nSCHOOL; k++) {
        fprintf(FMS, "%.4f ", sum_eta[k]);
        fprintf(FMS, "%.4f ", sum_xi[k]);
        fprintf(FMS, "%.4f ", sum_pi[k]);
        fprintf(FMS, "%.4f ", var_eta[k]);
        fprintf(FMS, "%.4f ", var_xi[k]);
        fprintf(FMS, "%.4f ", var_pi[k]);
        fprintf(FMS, "\n");
      }
    }

    fclose(JIN);
    fclose(HUR);
    fclose(JYW);
    fclose(OUT);
    fclose(ASA);
    fclose(PRT);
    fclose(IMS);
    fclose(JJW);
    fclose(FMS);
  }

  /*
  free_ivector(ncount, 1, nSCHOOL);
  free_dvector(jump_Z, 0, nITEM);

  for(k = 0; k <= nSCHOOL; k++){
                  for(i = 0; i <= nMAX; i++) free(SCHOOL[k].dataset[i]);
                  for(i = 0; i <= nITEM; i++){
                                  for(a = 0; a <= nMAX; a++)
  free(SCHOOL[k].Y[i][a]); free(SCHOOL[k].Y[i]);
                  }
                  for(i = 0; i <= nMAX; i++){
                                  for(a = 0; a <= nITEM; a++)
  free(SCHOOL[k].U[i][a]); free(SCHOOL[k].U[i]);
                  }
                  for(i = 0; i <= nMAX; i++){free(SCHOOL[k].old_Zsamp[i]);
  free(SCHOOL[k].new_Zsamp[i]);} for(i = 0; i <= nITEM;
  i++){free(SCHOOL[k].old_Zitem[i]); free(SCHOOL[k].new_Zitem[i]);} for(i = 0; i
  <= (niter-nburn)/thin; i++){ for(j = 0; j <= nMAX; j++)
  free(SCHOOL[k].sample_Zsamp[i][j]); for(j = 0; j <= nITEM; j++)
  free(SCHOOL[k].sample_Zitem[i][j]); free(SCHOOL[k].sample_beta[i]);
  free(SCHOOL[k].sample_theta[i]); free(SCHOOL[k].sample_Zsamp[i]);
  free(SCHOOL[k].sample_Zitem[i]);
                  }
                  for(i = 0; i <= nMAX; i++){free(SCHOOL[k].sum_Zsamp[i]);
  free(SCHOOL[k].var_Zsamp[i]);} for(i = 0; i <= nITEM;
  i++){free(SCHOOL[k].sum_Zitem[i]); free(SCHOOL[k].var_Zitem[i]);} for(i = 0; i
  <= nITEM; i++){free(SCHOOL[k].sum_item_mat[i]);
  free(SCHOOL[k].var_item_mat[i]);} for(i = 0; i <= nITEM;
  i++){free(SCHOOL[k].old_item_mat[i]); free(SCHOOL[k].new_item_mat[i]);
  free(SCHOOL[k].sample_item_mat[i]);} free(SCHOOL[k].old_item_mat);
  free(SCHOOL[k].new_item_mat); free(SCHOOL[k].oldbeta);
  free(SCHOOL[k].newbeta); free(SCHOOL[k].oldtheta); free(SCHOOL[k].newtheta);
                  free(SCHOOL[k].count_item); free(SCHOOL[k].count_samp);
                  free(SCHOOL[k].Y); free(SCHOOL[k].U); free(SCHOOL[k].dataset);
                  free(SCHOOL[k].old_Zsamp); free(SCHOOL[k].new_Zsamp);
                  free(SCHOOL[k].old_Zitem); free(SCHOOL[k].new_Zitem);
                  free(SCHOOL[k].sample_beta); free(SCHOOL[k].sample_theta);
                  free(SCHOOL[k].sum_beta); free(SCHOOL[k].var_beta);
  free(SCHOOL[k].acc_beta); free(SCHOOL[k].sum_theta);
  free(SCHOOL[k].var_theta); free(SCHOOL[k].acc_theta);
                  free(SCHOOL[k].sample_Zsamp); free(SCHOOL[k].sample_Zitem);
  free(SCHOOL[k].sample_item_mat); free(SCHOOL[k].sum_Zsamp);
  free(SCHOOL[k].var_Zsamp); free(SCHOOL[k].acc_Zsamp);
                  free(SCHOOL[k].sum_Zitem); free(SCHOOL[k].var_Zitem);
                  free(SCHOOL[k].sum_item_mat); free(SCHOOL[k].var_item_mat);
                  free(SCHOOL[k].sample_sigma); free(SCHOOL[k].mean_Z);
  }
  free(SCHOOL);

  free_dmatrix(sample_sigma, 1, (niter - nburn) / thin, 1, nSCHOOL);
  free_dmatrix(sample_delta, 1, (niter - nburn) / thin, 1, nITEM * nDIM);
  free_dmatrix(sample_tau, 1, (niter - nburn) / thin, 1, nITEM * nDIM);
  free_dmatrix(sample_gamma, 1, (niter-nburn)/thin, 1, nITEM);
  free_dmatrix(sample_varphi, 1, (niter-nburn)/thin, 1, nITEM);
  free_dmatrix(sum_mu, 1, nITEM * nDIM, 0, nSCHOOL);
  free_dvector(sum_tau, 1, nITEM * nDIM);
  free_dvector(var_tau, 1, nITEM * nDIM);
  free_dvector(sum_sigma, 1, nSCHOOL);
  free_dvector(var_sigma, 1, nSCHOOL);
  free_dvector(sum_delta, 1, nITEM * nDIM);
  free_dvector(var_delta, 1, nITEM * nDIM);
  free_dvector(sum_gamma, 1, nITEM);
  free_dvector(var_gamma, 1, nITEM);
  free_dvector(sum_varphi, 1, nITEM);
  free_dvector(var_varphi, 1, nITEM);

  free_dvector(oldsigma, 1, nSCHOOL);
  free_dvector(olddelta, 1, nITEM * nDIM);
  free_dvector(oldtau, 1, nITEM * nDIM);
  free_dmatrix(oldmu, 1, nITEM * nDIM, 0, nSCHOOL);
  free_dvector(oldgamma, 1, nITEM);
  free_dvector(oldvarphi, 1, nITEM);

  free_dmatrix(mu_dist, 1, nSCHOOL, 1, nSCHOOL);
  free_dmatrix(sum_mu_dist, 1, nSCHOOL, 1, nSCHOOL);
  free_dvector(avg_ran, 1, nSCHOOL);
  free_dvector(var_ran, 1, nSCHOOL);

  free_ivector(count, 1, nSCHOOL);

  free_dvector(sample_samp_like, 1, nMAX);
  free_dvector(new_samp_distance, 1, nMAX);
  free_dvector(old_samp_distance, 1, nMAX);

  free_dvector(sample_item_like, 1, nMAX);
  free_dmatrix(new_item_distance, 1, nITEM, 1, nITEM);
  free_dmatrix(old_item_distance, 1, nITEM, 1, nITEM);
  */

  return 0;
}
