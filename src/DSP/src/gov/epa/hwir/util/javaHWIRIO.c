/* DO NOT EDIT THIS FILE - it is machine generated */
#include "javahwirio.h"
#include "mshwirio.h"
#include "string.h"

int errorflag;
char allargs[1024];

/*
  Helper function to convert a jstring to a normal c string
*/
void getCString(JNIEnv *env,jstring string,char *buf,int size)
{
  const jchar *UTFString;
  int count,i;
  jboolean isCopy;
  UTFString=(*env)->GetStringChars(env,string,&isCopy);
  count=(*env)->GetStringLength(env,string);
  if (count > size-1) count=size; // do not get bigger than the buffer
  for (i=0;i<count;i++)
	buf[i]=(char)UTFString[i];
  buf[i]='\0';
  (*env)->ReleaseStringChars(env,string,UTFString);
}

typedef struct
{
	char grp[32];
	char var[32];
	char units[32];
} indices;

/*
  helper function to convert the group, variable name, and variable units to c strings
*/
indices *getCStrings(JNIEnv *env,jstring group,jstring varName, jstring varUnits)
{
	static indices idx;
	getCString(env,group,idx.grp,32);
	getCString(env,varName,idx.var,32);
	getCString(env,varUnits,idx.units,32);
	return &idx;
}

void myCallBack()
{
	errorflag=1;
}

jboolean JNICALL Java_gov_epa_hwir_util_hwirio_IOOk(JNIEnv *env, jobject this)
{
	return (jboolean)(IOOk()==1);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_setArgsSystem
  (JNIEnv * env, jobject this, jstring args)
{
	char cargs[1024];

	getCString(env,args,cargs,1024);
	strncpy(allargs,cargs,1024);
    errorflag=0;
	SetArgs(cargs,myCallBack,1); // Leave callback and leave warn alone for now
}

void JNICALL Java_gov_epa_hwir_util_hwirio_setArgs
  (JNIEnv * env, jobject this, jstring args)
{
	char cargs[1024];
    strncpy(allargs,"\0",1);
	getCString(env,args,cargs,1024);
	SetArgs(cargs,NULL,0); // Leave callback and leave warn alone for now
}

jboolean JNICALL Java_gov_epa_hwir_util_hwirio_getStatus
  (JNIEnv *env, jobject this)
{
	return (jboolean)(errorflag==0);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_resetArgs
  (JNIEnv *env , jobject this)
{
	errorflag=0;
	if (strlen(allargs)>0)
	  SetArgs(allargs,myCallBack,1); // Leave callback and leave warn alone for now
}

void JNICALL Java_gov_epa_hwir_util_hwirio_addGroup
  (JNIEnv * env, jobject this, jstring group)
{
	char grp[32];
	getCString(env,group,grp,32);
	AddGroup(grp);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_addRWGroup
  (JNIEnv * env, jobject this, jstring group)
{
	char grp[32];
	getCString(env,group,grp,32);
	AddRWGroup(grp);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_removeGroup
  (JNIEnv * env, jobject this, jstring group)
{
	char grp[32];
	getCString(env,group,grp,32);
    RemoveGroup(grp);
}

jint JNICALL Java_gov_epa_hwir_util_hwirio_numArgs
  (JNIEnv * env, jobject this)
{
	return (jint)NumArgs();
}

jint JNICALL Java_gov_epa_hwir_util_hwirio_getArgInt
  (JNIEnv * env, jobject this, jint index)
{
	return (jint)GetArgInt(index);
}

jstring JNICALL Java_gov_epa_hwir_util_hwirio_getArgString
  (JNIEnv * env, jobject this, jint index)
{
	jstring value;
	unsigned char arg[80];
	GetArgString(index,arg);
	value=(*env)->NewStringUTF(env,arg);
	return value;
}

void JNICALL Java_gov_epa_hwir_util_hwirio_openGroups
  (JNIEnv * env, jobject this)
{
	OpenGroups();
}

void JNICALL Java_gov_epa_hwir_util_hwirio_closeGroups
  (JNIEnv * env, jobject this)
{
	CloseGroups();
}

void JNICALL Java_gov_epa_hwir_util_hwirio_error
  (JNIEnv * env, jobject this, jstring description)
{
	char message[80];
	getCString(env,description,message,80);
	Error(message);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_warning
  (JNIEnv * env, jobject this, jstring description)
{
	char message[80];
	getCString(env,description,message,80);
	Warning(message);
}

jint JNICALL Java_gov_epa_hwir_util_hwirio_readInt__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env , jobject this, jstring group, jstring varName, jstring varUnits)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	return ReadInt(idx->grp,idx->var,idx->units);
}

jint JNICALL Java_gov_epa_hwir_util_hwirio_readInt__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2I
  (JNIEnv *env , jobject this, jstring group, jstring varName,  jstring varUnits, jint index1)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	return ReadInt1(idx->grp,idx->var,idx->units,index1);
}

jint JNICALL Java_gov_epa_hwir_util_hwirio_readInt__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2II
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	return ReadInt2(idx->grp,idx->var,idx->units,index1,index2);
}

jint JNICALL Java_gov_epa_hwir_util_hwirio_readInt__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2III
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	return ReadInt3(idx->grp,idx->var,idx->units,index1,index2,index3);
}

jint JNICALL Java_gov_epa_hwir_util_hwirio_readInt__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIII
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	return ReadInt4(idx->grp,idx->var,idx->units,index1,index2,index3,index4);
}

jint JNICALL Java_gov_epa_hwir_util_hwirio_readInt__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIII
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jint index5)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	return ReadInt5(idx->grp,idx->var,idx->units,index1,index2,index3,index4,index5);
}

jint JNICALL Java_gov_epa_hwir_util_hwirio_readInt__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIIII
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jint index5, jint index6)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	return ReadInt6(idx->grp,idx->var,idx->units,index1,index2,index3,index4,index5,index6);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_writeInt__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2I
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint val)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteInt(idx->grp,idx->var,idx->units,val);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_writeInt__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2II
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint val)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteInt1(idx->grp,idx->var,idx->units,index1,val);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_writeInt__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2III
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint val)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteInt2(idx->grp,idx->var,idx->units,index1,index2,val);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_writeInt__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIII
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint val)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteInt3(idx->grp,idx->var,idx->units,index1,index2,index3,val);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_writeInt__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIII
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4,jint val)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteInt4(idx->grp,idx->var,idx->units,index1,index2,index3,index4,val);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_writeInt__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIIII
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jint index5,jint val)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteInt5(idx->grp,idx->var,idx->units,index1,index2,index3,index4,index5,val);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_writeInt__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIIIII
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jint index5, jint index6, jint val)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteInt6(idx->grp,idx->var,idx->units,index1,index2,index3,index4,index5,index6,val);
}

jdouble JNICALL Java_gov_epa_hwir_util_hwirio_readReal__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	return ReadReal(idx->grp,idx->var,idx->units);
}

jdouble JNICALL Java_gov_epa_hwir_util_hwirio_readReal__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2I
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	return ReadReal1(idx->grp,idx->var,idx->units,index1);
}

jdouble JNICALL Java_gov_epa_hwir_util_hwirio_readReal__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2II
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	return ReadReal2(idx->grp,idx->var,idx->units,index1,index2);
}

jdouble JNICALL Java_gov_epa_hwir_util_hwirio_readReal__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2III
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	return ReadReal3(idx->grp,idx->var,idx->units,index1,index2,index3);
}

jdouble JNICALL Java_gov_epa_hwir_util_hwirio_readReal__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIII
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	return ReadReal4(idx->grp,idx->var,idx->units,index1,index2,index3,index4);
}

jdouble JNICALL Java_gov_epa_hwir_util_hwirio_readReal__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIII
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jint index5)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	return ReadReal5(idx->grp,idx->var,idx->units,index1,index2,index3,index4,index5);
}

jdouble JNICALL Java_gov_epa_hwir_util_hwirio_readReal__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIIII
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jint index5, jint index6)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	return ReadReal6(idx->grp,idx->var,idx->units,index1,index2,index3,index4,index5,index6);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_writeReal__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2D
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jdouble val)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteReal(idx->grp,idx->var,idx->units,val);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_writeReal__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2ID
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jdouble val)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteReal1(idx->grp,idx->var,idx->units,index1,val);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_writeReal__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IID
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jdouble val)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteReal2(idx->grp,idx->var,idx->units,index1,index2,val);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_writeReal__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIID
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jdouble val)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteReal3(idx->grp,idx->var,idx->units,index1,index2,index3,val);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_writeReal__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIID
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jdouble val)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteReal4(idx->grp,idx->var,idx->units,index1,index2,index3,index4,val);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_writeReal__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIIID
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jint index5, jdouble val)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteReal5(idx->grp,idx->var,idx->units,index1,index2,index3,index4,index5,val);
}

void JNICALL Java_gov_epa_hwir_util_hwirio_writeReal__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIIIID
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jint index5, jint index6, jdouble val)
{
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteReal6(idx->grp,idx->var,idx->units,index1,index2,index3,index4,index5,index6,val);
}

 jboolean JNICALL Java_gov_epa_hwir_util_hwirio_readLog__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits)
 {
	indices *idx=getCStrings(env,group,varName,varUnits);
	return (jint)ReadLog(idx->grp,idx->var,idx->units) & 0x0ff;
 }

 jboolean JNICALL Java_gov_epa_hwir_util_hwirio_readLog__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2I
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1)
 {
 	indices *idx=getCStrings(env,group,varName,varUnits);
	return (jint)ReadLog1(idx->grp,idx->var,idx->units,index1) & 0x0ff;
}

 jboolean JNICALL Java_gov_epa_hwir_util_hwirio_readLog__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2II
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2)
 {
	indices *idx=getCStrings(env,group,varName,varUnits);
	return (jint)ReadLog2(idx->grp,idx->var,idx->units,index1,index2) & 0x0ff;
 }

 jboolean JNICALL Java_gov_epa_hwir_util_hwirio_readLog__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2III
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3)
 {
	indices *idx=getCStrings(env,group,varName,varUnits);
	return (jint)ReadLog3(idx->grp,idx->var,idx->units,index1,index2,index3) & 0x0ff;
 }

 jboolean JNICALL Java_gov_epa_hwir_util_hwirio_readLog__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIII
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4)
 {
	indices *idx=getCStrings(env,group,varName,varUnits);
	return (jint)ReadLog4(idx->grp,idx->var,idx->units,index1,index2,index3,index4) & 0x0ff;
 }

 jboolean JNICALL Java_gov_epa_hwir_util_hwirio_readLog__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIII
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jint index5)
 {
	indices *idx=getCStrings(env,group,varName,varUnits);
	return (jint)ReadLog5(idx->grp,idx->var,idx->units,index1,index2,index3,index4,index5) & 0x0ff;
 }

 jboolean JNICALL Java_gov_epa_hwir_util_hwirio_readLog__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIIII
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jint index5, jint index6)
 {
	indices *idx=getCStrings(env,group,varName,varUnits);
	return (jint)ReadLog6(idx->grp,idx->var,idx->units,index1,index2,index3,index4,index5,index6) & 0x0ff;
 }

 void JNICALL Java_gov_epa_hwir_util_hwirio_writeLog__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2Z
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jboolean val)
 {
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteLog(idx->grp,idx->var,idx->units,(int)val);
 }

 void JNICALL Java_gov_epa_hwir_util_hwirio_writeLog__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IZ
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jboolean val)
 {
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteLog1(idx->grp,idx->var,idx->units,index1,(int)val);
 }

 void JNICALL Java_gov_epa_hwir_util_hwirio_writeLog__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIZ
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jboolean val)
 {
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteLog2(idx->grp,idx->var,idx->units,index1,index2,(int)val);
 }

 void JNICALL Java_gov_epa_hwir_util_hwirio_writeLog__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIZ
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jboolean val)
 {
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteLog3(idx->grp,idx->var,idx->units,index1,index2,index3,(int)val);
 }

 void JNICALL Java_gov_epa_hwir_util_hwirio_writeLog__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIIZ
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jboolean val)
 {
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteLog4(idx->grp,idx->var,idx->units,index1,index2,index3,index4,(int)val);
 }

 void JNICALL Java_gov_epa_hwir_util_hwirio_writeLog__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIIIZ
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jint index5, jboolean val)
 {
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteLog5(idx->grp,idx->var,idx->units,index1,index2,index3,index4,index5,(int)val);
 }

 void JNICALL Java_gov_epa_hwir_util_hwirio_writeLog__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIIIIZ
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jint index5, jint index6, jboolean val)
 {
	indices *idx=getCStrings(env,group,varName,varUnits);
	WriteLog6(idx->grp,idx->var,idx->units,index1,index2,index3,index4,index5,index6,(int)val);
 }

 jstring JNICALL Java_gov_epa_hwir_util_hwirio_readString__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits)
 {
	char cval[80];
 	indices *idx=getCStrings(env,group,varName,varUnits);
	ReadString(idx->grp,idx->var,idx->units,cval);
	return (*env)->NewStringUTF(env,cval);
 }

 jstring JNICALL Java_gov_epa_hwir_util_hwirio_readString__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2I
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1)
 {
	char cval[80];
 	indices *idx=getCStrings(env,group,varName,varUnits);
	ReadString1(idx->grp,idx->var,idx->units,index1,cval);
	return (*env)->NewStringUTF(env,cval);
 }

 jstring JNICALL Java_gov_epa_hwir_util_hwirio_readString__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2II
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2)
 {
 	char cval[80];
 	indices *idx=getCStrings(env,group,varName,varUnits);
	ReadString2(idx->grp,idx->var,idx->units,index1,index2,cval);
	return (*env)->NewStringUTF(env,cval);
}

 jstring JNICALL Java_gov_epa_hwir_util_hwirio_readString__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2III
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3)
 {
	char cval[80];
 	indices *idx=getCStrings(env,group,varName,varUnits);
	ReadString3(idx->grp,idx->var,idx->units,index1,index2,index3,cval);
	return (*env)->NewStringUTF(env,cval);
 }

 jstring JNICALL Java_gov_epa_hwir_util_hwirio_readString__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIII
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3,jint index4)
 {
	char cval[80];
 	indices *idx=getCStrings(env,group,varName,varUnits);
	ReadString4(idx->grp,idx->var,idx->units,index1,index2,index3,index4,cval);
	return (*env)->NewStringUTF(env,cval);
 }

 jstring JNICALL Java_gov_epa_hwir_util_hwirio_readString__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIII
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jint index5)
 {
 	char cval[80];
 	indices *idx=getCStrings(env,group,varName,varUnits);
	ReadString5(idx->grp,idx->var,idx->units,index1,index2,index3,index4,index5,cval);
	return (*env)->NewStringUTF(env,cval);
}

 jstring JNICALL Java_gov_epa_hwir_util_hwirio_readString__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIIII
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jint index5, jint index6)
 {
	char cval[80];
 	indices *idx=getCStrings(env,group,varName,varUnits);
	ReadString6(idx->grp,idx->var,idx->units,index1,index2,index3,index4,index5,index6,cval);
	return (*env)->NewStringUTF(env,cval);
 }

 void JNICALL Java_gov_epa_hwir_util_hwirio_writeString__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jstring val)
 {
 	char cval[80];
	indices *idx=getCStrings(env,group,varName,varUnits);
	getCString(env,val,cval,80);
	WriteString(idx->grp,idx->var,idx->units,cval);
}

 void JNICALL Java_gov_epa_hwir_util_hwirio_writeString__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2ILjava_lang_String_2
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jstring val)
 {
 	char cval[80];
	indices *idx=getCStrings(env,group,varName,varUnits);
	getCString(env,val,cval,80);
	WriteString1(idx->grp,idx->var,idx->units,index1,cval);
}

 void JNICALL Java_gov_epa_hwir_util_hwirio_writeString__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IILjava_lang_String_2
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jstring val)
 {
	char cval[80];
	indices *idx=getCStrings(env,group,varName,varUnits);
	getCString(env,val,cval,80);
	WriteString2(idx->grp,idx->var,idx->units,index1,index2,cval);
 }

 void JNICALL Java_gov_epa_hwir_util_hwirio_writeString__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIILjava_lang_String_2
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jstring val)
 {
	char cval[80];
	indices *idx=getCStrings(env,group,varName,varUnits);
	getCString(env,val,cval,80);
	WriteString3(idx->grp,idx->var,idx->units,index1,index2,index3,cval);
 }

 void JNICALL Java_gov_epa_hwir_util_hwirio_writeString__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIILjava_lang_String_2
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jstring val)
 {
	char cval[80];
	indices *idx=getCStrings(env,group,varName,varUnits);
	getCString(env,val,cval,80);
	WriteString4(idx->grp,idx->var,idx->units,index1,index2,index3,index4,cval);
 }

 void JNICALL Java_gov_epa_hwir_util_hwirio_writeString__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIIILjava_lang_String_2
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jint index5, jstring val)
 {
	char cval[80];
	indices *idx=getCStrings(env,group,varName,varUnits);
	getCString(env,val,cval,80);
	WriteString5(idx->grp,idx->var,idx->units,index1,index2,index3,index4,index5,cval);
 }

 void JNICALL Java_gov_epa_hwir_util_hwirio_writeString__Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2IIIIIILjava_lang_String_2
  (JNIEnv *env, jobject this, jstring group, jstring varName, jstring varUnits, jint index1, jint index2, jint index3, jint index4, jint index5, jint index6, jstring val)
 {
	char cval[80];
	indices *idx=getCStrings(env,group,varName,varUnits);
	getCString(env,val,cval,80);
	WriteString6(idx->grp,idx->var,idx->units,index1,index2,index3,index4,index5,index6,cval);
 }
