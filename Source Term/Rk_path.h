#ifndef RK_PATH_H
#define RK_PATH_H

#include"strm1.h"

void rk_calc(float *rk_inst, float *rk_path, float *dzdt,
        int num, Remediate *darcy_inf,
        float thickness,float new_soil_1,
				float dry_layer, float delta_dry, float start_dry,
        float elap_time, Remediate *wind_erosion,
				Remediate *water_erosion, float soil_lost, float gamma, float gamma_t);

void rk_step(float *step, float *dzdt, float dzdt_mass,
        int cont_num, float mass, Remediate *darcy_inf,
        float thick, float new_soil,
				float dry_layer, float delta_dry, float start_dry,
        float elap_time, Remediate *wind, Remediate *water);

float recedingV1(int contnum, float waste_thick, float net_soil_loss, float mass);
float recedingV2(int contnum, float waste_thick, float depth_dry_layer, float delta_dry_layer, float net_soil_loss, float mass);
void distributeGamma(float gammaWind, float gammaWater, float dz_dt, float *rk_pathway);




#endif
