// StatDll.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"
#include "..\..\StatDll.h"
#include "..\..\MSHwirMC.h"


BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
    return TRUE;
}



JNIEXPORT void JNICALL Java_gov_epa_hwir_util_StatDll_StatSeed
  (JNIEnv *env, jobject object, jint seed){

	StatSeed ( seed );
	return;

  }

  JNIEXPORT void JNICALL Java_gov_epa_hwir_util_StatDll_StatDebugOn
  (JNIEnv *env, jobject object){

	StatDebugOn();
	return;

 }

 JNIEXPORT void JNICALL Java_gov_epa_hwir_util_StatDll_StatClear
  (JNIEnv *env, jobject object){

	  StatClear ();
	  return;
  }


  JNIEXPORT void JNICALL Java_gov_epa_hwir_util_StatDll_StatNumDist
  (JNIEnv *env, jobject object , jint numDist){

	StatNumDist ( numDist );
	return;
 }


JNIEXPORT void JNICALL Java_gov_epa_hwir_util_StatDll_StatNumCor
  (JNIEnv *env, jobject object , jint numCor){

	StatNumCor ( numCor );
	return;

  }


JNIEXPORT void JNICALL Java_gov_epa_hwir_util_StatDll_StatCor
  (JNIEnv *env, jobject object, jint int1, jint int2 , jfloat float1){

	StatCor ( int1, int2, float1 );
	return;

 }


JNIEXPORT jint JNICALL Java_gov_epa_hwir_util_StatDll_StatNormal
  (JNIEnv *env, jobject obj, jdouble jd1, jdouble jd2, jdouble jd3, jdouble jd4){

	  return (jint) StatNormal ( jd1, jd2, jd3, jd4 );

  }


JNIEXPORT jint JNICALL Java_gov_epa_hwir_util_StatDll_StatLogNormal
  (JNIEnv *env, jobject obj, jdouble jd1, jdouble jd2, jdouble jd3, jdouble jd4){

	return (jint) StatLogNormal ( jd1, jd2, jd3, jd4 );

}


JNIEXPORT jint JNICALL Java_gov_epa_hwir_util_StatDll_StatExponential
  (JNIEnv *env, jobject obj, jdouble jd1 , jdouble jd2 , jdouble jd3){

	  return (jint) StatExponential ( jd1, jd2 , jd3 );

  }


JNIEXPORT jint JNICALL Java_gov_epa_hwir_util_StatDll_StatUniform
  (JNIEnv *env, jobject obj, jdouble jd1 , jdouble jd2){

	  return (jint) StatUniform ( jd1, jd2 );

  }


JNIEXPORT jint JNICALL Java_gov_epa_hwir_util_StatDll_StatSB
  (JNIEnv *env, jobject obj, jdouble jd1, jdouble jd2, jdouble jd3, jdouble jd4){

	return (jint) StatSB (jd1, jd2, jd3, jd4 );
  }

  JNIEXPORT jint JNICALL Java_gov_epa_hwir_util_StatDll_StatSU
  (JNIEnv *env, jobject obj, jdouble jd1, jdouble jd2, jdouble jd3, jdouble jd4){

	  return (jint) StatSU (jd1, jd2, jd3, jd4);

  }



//  JNIEXPORT jint JNICALL Java_StatDll_StatEmpirical
//  (JNIEnv *, jobject, jdouble, jdouble, jint, jdouble, jdouble){
//
//	return StatEmpirical ( jdouble, jdouble, jint, jdouble, jdouble );
//
//  }
//

  JNIEXPORT jint JNICALL Java_gov_epa_hwir_util_StatDll_StatIntUniform
  (JNIEnv *env, jobject obj, jdouble jd1, jdouble jd2 ){

	return (jint) StatIntUniform ( jd1, jd2);

  }

  JNIEXPORT jint JNICALL Java_gov_epa_hwir_util_StatDll_StatTriangular
  (JNIEnv *env, jobject obj, jdouble jd1, jdouble jd2, jdouble jd3){

	  return (jint) StatTriangular ( jd1, jd2, jd3 );

  }

  JNIEXPORT jint JNICALL Java_gov_epa_hwir_util_StatDll_StatTrnLogNormal
  (JNIEnv *env, jobject obj, jdouble jd1, jdouble jd2, jdouble jd3, jdouble jd4){

	  return (jint) StatTrnLogNormal ( jd1, jd2, jd3, jd4  );

  }


  JNIEXPORT jint JNICALL Java_gov_epa_hwir_util_StatDll_StatGamma
    (JNIEnv *env, jobject obj, jdouble jd1, jdouble jd2, jdouble jd3, jdouble jd4){

		return (jint) StatGamma ( jd1, jd2, jd3, jd4 );

	}


  /*
   * Class:     StatDll
   * Method:    StatWeibull
   * Signature: (DDDD)I
   */
  JNIEXPORT jint JNICALL Java_gov_epa_hwir_util_StatDll_StatWeibull
    (JNIEnv *env, jobject obj, jdouble jd1, jdouble jd2, jdouble jd3, jdouble jd4){

		return (jint) StatWeibull ( jd1, jd2, jd3, jd4 );
	}

  /*
   * Class:     StatDll
   * Method:    StatJohnsonSB
   * Signature: (DDDD)I
   */
  JNIEXPORT jint JNICALL Java_StatDll_StatJohnsonSB
  (JNIEnv *env, jobject obj, jdouble jd1, jdouble jd2, jdouble jd3, jdouble jd4){


	  return (jint) StatJohnsonSB ( jd1, jd2, jd3, jd4 );


  }


  /*
 * Class:     gov_epa_hwir_util_StatDll
 * Method:    StatEmpirical
 * Signature: (DDI[D[D)I
 */
JNIEXPORT jint JNICALL Java_gov_epa_hwir_util_StatDll_StatEmpirical
(JNIEnv *env, jobject obj, jdouble jd1, jdouble jd2, jint intNum, jdoubleArray array1, jdoubleArray array2){
	jdouble *Values = env->GetDoubleArrayElements(array1, 0);
	jdouble *CumProb = env->GetDoubleArrayElements(array2, 0);

	jint returnValue = (jint) StatEmpirical ( jd1, jd2, intNum, Values, CumProb );

	env->ReleaseDoubleArrayElements( array1, Values, 0);
	env->ReleaseDoubleArrayElements( array2, CumProb, 0);

	return returnValue;
}

/*
 * Class:     gov_epa_hwir_util_StatDll
 * Method:    StatSample
 * Signature: (I[D)V
 */
JNIEXPORT void JNICALL Java_gov_epa_hwir_util_StatDll_StatSample
(JNIEnv *env, jobject obj, jint Num, jdoubleArray array){
	jdouble *Values = env->GetDoubleArrayElements( array, 0 );	
	StatSample ( Num, Values );	
	env->ReleaseDoubleArrayElements( array, Values, 0 );	
	return;
}

