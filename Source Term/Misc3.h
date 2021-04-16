#ifndef MISC3_H
	#define MISC3_H
	char *dateoutst();
	char *timeoutst();
	float calc_vapor_cons(float henrys_const, float sat_vapor_cons, float mole_fract, float alpha) ;
//float calc_vapor_cons(float sat_conc, float thick, float elap_time, int count,float wz_bulkd);
	float batemn(int parent,int child, float elapsed_time);
	int chain_mass(int parent, int progprod,float elapsed_time);
	int NAPL_state(float org_moles, float gamma, int e_time);
#endif
