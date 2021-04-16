#ifndef REDUCE_H
#define REDUCE_H

int NextUsed(int Start,int Max,int *Used);
float Integrate(int Num,int Index,float *Times,float **Flux,int *Used);
void Reduce(int NumType,int *NumFlux,float **Times,float ***Fluxes,float ratio);

#endif