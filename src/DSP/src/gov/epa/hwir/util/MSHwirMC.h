#ifndef STAT_H
#define STAT_H

#ifdef __cplusplus
extern "C" {
#endif
void __declspec(dllimport) _STAT_mp_seed(int newSeed);
void __declspec(dllimport) _STAT_mp_debugon();
void __declspec(dllimport) _STAT_mp_clear();
void __declspec(dllimport) _STAT_mp_numdist(int NumDist);
void __declspec(dllimport) _STAT_mp_numcor(int NumCor);
void __declspec(dllimport) _STAT_mp_cor(int Index1,int Index2,float Cor);
int __declspec(dllimport) _STAT_mp_normal(double mean,double sd,double min,double max);
int __declspec(dllimport) _STAT_mp_lognormal(double mean,double sd,double min,double max);
int __declspec(dllimport) _STAT_mp_exponential(double ct,double min,double max);
int __declspec(dllimport) _STAT_mp_uniform(double min,double max);
int __declspec(dllimport) _STAT_mp_sb(double ct,double var,double min,double max);
int __declspec(dllimport) _STAT_mp_su(double ct,double var,double min,double max);
int __declspec(dllimport) _STAT_mp_empirical(double min,double max,int Num,double *Values,double *CumProb);
int __declspec(dllimport) _STAT_mp_triangular(double mode,double min,double max);
int __declspec(dllimport) _STAT_mp_intuniform(double min,double max);
void __declspec(dllimport) _STAT_mp_sample(int Num,double *Values);

int __declspec(dllimport) _STAT_mp_trnlognormal(double tmean,double tsd,double min,double max);
int __declspec(dllimport) _STAT_mp_gamma(double shape,double scale,double min,double max);
int __declspec(dllimport) _STAT_mp_weibull(double shape,double scale,double min,double max);
int __declspec(dllimport) _STAT_mp_johnsonsb(double mean,double sd,double min,double max);

#ifdef __cplusplus
}
#endif
#define StatSeed _STAT_mp_seed
#define StatDebugOn _STAT_mp_debugon
#define StatClear _STAT_mp_clear
#define StatNumDist _STAT_mp_numdist
#define StatNumCor _STAT_mp_numcor
#define StatCor _STAT_mp_cor
#define StatNormal _STAT_mp_normal
#define StatLogNormal _STAT_mp_lognormal
#define StatExponential _STAT_mp_exponential
#define StatUniform _STAT_mp_uniform
#define StatSB _STAT_mp_sb
#define StatSU _STAT_mp_su
#define StatEmpirical _STAT_mp_empirical
#define StatTriangular _STAT_mp_triangular
#define StatIntUniform _STAT_mp_intuniform
#define StatSample _STAT_mp_sample

#define StatTrnLogNormal _STAT_mp_trnlognormal
#define StatGamma _STAT_mp_gamma
#define StatWeibull _STAT_mp_weibull
#define StatJohnsonSB _STAT_mp_johnsonsb

#endif
