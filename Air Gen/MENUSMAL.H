#ifndef MENUSMALL_H
#define MENUSMALL_H

void ExitApplication(int status);
void InitApplication(void);
void Message(int NumItems,char **Text);
int TempMessage(int NumItems,char **Text);
void ClearTempMessage(int Handle);

#endif

