#include "StatDll.h"
#include <iostream.h>
#include "MsHwirMC.h"

JNIEXPORT void JNICALL Java_StatDll_StatSeed
  (JNIEnv *, jobject, jint){

	StatSeed ( jint );
	return;

  }

  JNIEXPORT void JNICALL Java_StatDll_StatDebugOn
  (JNIEnv *, jobject){

	StatDebugOn();
	return;

 }

 JNIEXPORT void JNICALL Java_gov_epa_hwir_util_StatDll_StatClear
  (JNIEnv *, jobject){

	  StatClear ();
	  return;
  }


  JNIEXPORT void JNICALL Java_StatDll_StatNumDist
  (JNIEnv *, jobject, jint){

	StatNumDist ( jint );
	return;
 }


JNIEXPORT void JNICALL Java_StatDll_StatNumCor
  (JNIEnv *, jobject, jint){

	  StatNumCor ( jint );
	return;

  }


JNIEXPORT void JNICALL Java_gov_epa_hwir_util_StatDll_StatCor
  (JNIEnv *, jobject, jint, jint, jfloat){

	StatCor ( jint, jint, jfloat );
	return;

 }


JNIEXPORT jint JNICALL Java_StatDll_StatNormal
  (JNIEnv *, jobject, jdouble, jdouble, jdouble, jdouble){

	  return StatNormal ( jdouble, jdouble ,jdouble ,jdouble );

  }


JNIEXPORT jint JNICALL Java_StatDll_StatLogNormal
  (JNIEnv *, jobject, jdouble, jdouble, jdouble, jdouble){

	return StatLogNormal ( jdouble, jdouble, jdouble, jdouble);

}


JNIEXPORT jint JNICALL Java_StatDll_StatExponential
  (JNIEnv *, jobject, jdouble, jdouble, jdouble){

	  return StatExponential ( jdouble, jdouble, jdouble );

  }


JNIEXPORT jint JNICALL Java_StatDll_StatUniform
  (JNIEnv *, jobject, jdouble, jdouble){

	  return StatUniform ( jdouble, jdouble );

  }


JNIEXPORT jint JNICALL Java_StatDll_StatSB
  (JNIEnv *, jobject, jdouble, jdouble, jdouble, jdouble){

	return StatSB (jdouble, jdouble, jdouble, jdouble);
  }

  JNIEXPORT jint JNICALL Java_StatDll_StatSU
  (JNIEnv *, jobject, jdouble, jdouble, jdouble, jdouble){

	  return StatSU (jdouble, jdouble, jdouble, jdouble);

  }



//  JNIEXPORT jint JNICALL Java_StatDll_StatEmpirical
//  (JNIEnv *, jobject, jdouble, jdouble, jint, jdouble, jdouble){
//
//	return StatEmpirical ( jdouble, jdouble, jint, jdouble, jdouble );
//
//  }
//

  JNIEXPORT jint JNICALL Java_StatDll_StatIntUniform
  (JNIEnv *, jobject, jdouble, jdouble){

	return StatIntUniform ( jdouble, jdouble );

  }

  JNIEXPORT jint JNICALL Java_StatDll_StatTriangular
  (JNIEnv *, jobject, jdouble, jdouble, jdouble){

	  return StatTriangular ( jdouble, jdouble, jdouble );

  }

  JNIEXPORT jint JNICALL Java_StatDll_StatTrnLogNormal
  (JNIEnv *, jobject, jdouble, jdouble, jdouble, jdouble){

	  return StatTrnLogNormal ( jdouble, jdouble, jdouble, jdouble );

  }


  JNIEXPORT jint JNICALL Java_StatDll_StatGamma
    (JNIEnv *, jobject, jdouble, jdouble, jdouble, jdouble){

		return StatGamme ( jdouble, jdouble, jdouble, jdouble );

	}


  /*
   * Class:     StatDll
   * Method:    StatWeibull
   * Signature: (DDDD)I
   */
  JNIEXPORT jint JNICALL Java_StatDll_StatWeibull
    (JNIEnv *, jobject, jdouble, jdouble, jdouble, jdouble){

		return StatWeibull ( jdouble, jdouble, jdouble, jdouble );
	}

  /*
   * Class:     StatDll
   * Method:    StatJohnsonSB
   * Signature: (DDDD)I
   */
  JNIEXPORT jint JNICALL Java_StatDll_StatJohnsonSB
  (JNIEnv *, jobject, jdouble, jdouble, jdouble, jdouble){


	  return StatJohnsonSB ( jdouble, jdouble, jdouble, jdouble );


  }

