#ifndef STRM1_H
#define STRM1_H

#include<new.h>
#include<dir.h>
#include"param3.h"
#include "sls.h"
#include "series.h"

#define DECAY               0
#define INFIL               1
#define WIND                2
#define WATER               3
#define VOLATILIZE          4   // 4 means: must have an initial value in the param file
#define KD                  4
#define SRC_SNK             0
#define GLASS               4
#define SURF_VOL            4
#define CLEAN_LAYER         4
#define TOTPOR              4
#define AIRSPC              4
#define REINITIALIZE				5

#define NUMPATH             5        // Number of pathways
#define MONTHS_PER_YEAR     12
#define MAX_ITER            100
#define BUF_SIZE            64
#define GAS_CONST           8.203e-5  // Universal Gas Law constant in (Atm m^3)/(moles K)
#define MAX_HENRYS          1.0e-7    // Any contaminant with a Henry's Const. below this value is not a volatile
#define MAX_FLUX_CT         10        // increased from 5 on Aug 29,2000
#define MAX_REPORT_TIME     10001

#define KNOWN_REL           1
#define KNOWN_FLX           2
#define SECOND_SRC          4         // obtain source/sink data from secondary source

//#define UNIX
#ifdef UNIX
 #define EXIT_FAILURE     1
 #define SEEK_SET         0
 #define strcmpi		strcasecmp
 #define min(a,b)	(((a) < (b)) ? (a) : (b))
#endif

new_handler set_new_handler(new_handler mem_out( ));

class Series_param
	{
	protected:
   int count;		//	Counter used to hold the index for the flux array
    float value;          // hold last value obtained from get_value -- to test change
	int num_values;		//	Number of flux values
	float *time;			//	Pointer to an array of time values, one less element than the fluxes since initial
						//	value is at time zero
	float *flux;			//	Pointer to an array of num_values elements containing flux values
	public:
	Series_param();
	unsigned int changed;	//	Flag which is set to one if the count was changed in the last call to get_value()
	void get_series( ParamFile &data_file, char *buf1,int c1,int c3,int c4,int c5,int c6);
	float get_value( float elapsed_time );
	unsigned int nonzero(void);
    unsigned int get_count();
    void output_series(fcsv *wff);
	~Series_param();
	};

class Remediate: public Series_param
	{
	public:
    float nfactor[3]; // factors for computation of the emissions (3 classes)
    Remediate()
    {
       Series_param();
       /* nfactor is distribution of flux to particle sizes
           -- assuming input (to MUI) is per EPA guidance
           -- in the known flux case, the default is assumed to be
                   .4 for 0-2.5 PM, .6 for 2.5-10PM and 0 for >10PM
           -- otherwise these factors are computed
       */
       nfactor[0]=.4;
       nfactor[1]=.6;
       nfactor[2]=0.0;
   }

	int init_param( ParamFile &data,char *buf,int c1,int c3,int c4,int c5,int c6,int flag);
	int set_series(int set_type, float set_value);
	};

class Pathway: public Series_param
	{
	public:
	int known_flux;
   float lasttime; // time of last non-zero value;
	int init_param( ParamFile &data,char *buf,int c1,int c3,int c4,int c5,int c6);
	int set_series(int set_type, float set_value);
        int init_param(int contnum, Series *TS);
//	leaching
	float dm_dt(Remediate *data, int contnum, float mass, float thick, float net_soil_lost,
					float delta_dry_layer, float start_dry, float elapsed_time);
//	wind or water
	float dm_dt(Remediate *w_eros, float init_waste_thick, float net_soil_lost, float start_dry, float elapsed_time);
// volatilize
	float dm_dt(float *dzdt, float dzdt_mass, float mass, int contnum,
          float elapsed_time, float net_dry_layer, float thick,
					float dry_layer, float start_dry);
	};

class SiteDef
	{
	public:
	float area, theta,
		theta_pond,	//	The water content of a pond source, volume - suspended solids
		pond_out, length, width, thick, fin_flux, temp, delta_t, org_moles_new, rel_hght, struc_hght,
		exit_vel, exit_temp, max_time, ovl_flux,
    wz_bulkd; // BLH 6/98
	int idx, numtotcon, num_contam, num_media, active;
  int modidx;
  // BLH 2/2001 - changed gid and run to MAXPATH
	char gid[MAXPATH], run[MAXPATH], name[BUF_SIZE], media_name[BUF_SIZE], arel_type[BUF_SIZE],
		media_type[BUF_SIZE];	//	Type of contaminated media
	};


class properties
	{
	public:
	float lambda,         // First order decay constant (1/y)
		grout_coef,     // = sqrt( Diffusion/Pi ) * (S/V)
		vapor_diff_coef, vol_ratio,
		adj_aqu_sol,    // adj_aqu_sol = waste_area * aqueous_sol
		mole_wt,        // Molecular Weight of contaminant (g/mole)
		molecule, unmod_henry,    //
		t_half,			// Half-life in years
		spec_act;       // Specific Activity, (Ci/g)

	char name[BUF_SIZE], cas_id[BUF_SIZE], p_name[BUF_SIZE], p_id[BUF_SIZE];

	int progeny, typeflag, type;
	};


class contaminant
	{
	public:

  // for computing dz/dt
  int bv;       // true(1) if bounding_value used in computing dmVol
  double vol_mass;

  float rk_pathway[6];
	float	rk_instant[6];
  float dzdt;
  int napl;

	float 		//	Runga-Kutta:  Mass in the waste zone  (g or Ci)
		air_vp_diff,	//	Vapor diffusion coefficient of contaminant in air (cm^2/s)
		adj_conc,		//	adj_conc = waste_area * conc_org_liq / octonal_h2o
		aq_conc, vapor_conc,	//	Concentration of the volatile in the vapor phase  (g/cm^3)
		k_g,			//	Gas-phase mass transfer coeffcient
		k_l,			//	Liquid-phase mass transfer coefficient
		k_i, retard, glass, grout, gdecay, init_mass, adj_org_conc, //
		mole_fract,	//	Mole fraction of constituent of organic mixture (unitless)
		henrys_const,	//	Henry's Law Constant
    henrys_org, //  used for partitioning from an organic liquid phase
    liq_dens; // pure contaminant density

  double mass, min_mass;
    contaminant();
	};

class ContFlags
	{
	public:

	int  aquifer, bigverb, decay, grt, napl, no_erode, pond, vit, inf, ovl, susp, vol, src, annual;
	};

extern contaminant *contam;
extern properties *props;
extern fcsv err, wrn, sdl;
extern SLS *sls;
extern SiteDef site;
extern ContFlags flg;
extern ParamFile data;
extern Pathway *leach, *suspend, *overland, *volate, *source_sink, *k_d;
extern Remediate	init_dry_layer, darcy_inf, water_eros, wind_eros, surf_vol, glass_series,
						air_space, total_porosity, sed_load, over_flow;
extern int *parent, *child, *lst_time;
extern float *aqueous_sol, *waste_thick, *sat_vapor_conc, *org_liq_conc, *depth_dry_layer;
extern bool warnings;
float *volate_tot(ParamFile *data_file, int num_contam);

float Blaney_Criddle(float latitude, int month, float temperature, float rhmin, float nratio, float u_day);

float Penman(float saturation_vp, float mean_vp, float max_humid, float cloud_percnt, float wind_height,
             float temperature, float mean_wind_speed, float elevation, float lattitude, int month, char corrected);

float melt_snow(float precip, float mean_vp, float mean_unadj_wind, float cloud_percnt,float temperature,
                 float num_days, float clear_melt, float wind_height, float site_elev);

float runoff(float total_precip, float num_precip, float temperature, float curve_num);

float soil_moist_table(float water_loss, float water_holding_capacity);

float water_loss_table(float soil_moist_ret, float water_holding_capacity);

#endif


