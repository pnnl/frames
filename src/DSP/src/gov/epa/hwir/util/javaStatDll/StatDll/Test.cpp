#include "..\..\MSHwirMC.h"

int main(){

	
	double *number =  new double[5];

    number[0] = 643.18;
    number[1] = 9.64;
    number[2] = 48.05;
    number[3] = 14.26;
    number[4] = 62.55;


	StatSample( 5, number );

}