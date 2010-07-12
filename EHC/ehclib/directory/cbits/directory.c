#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__) || defined(__UHC__)
/* 
 * (c) The University of Glasgow 2002
 *
 */

#define INLINE
#include "HsDirectory.h"

/*
 * Function: __hscore_getFolderPath()
 *
 * Late-bound version of SHGetFolderPath(), coping with OS versions
 * that have shell32's lacking that particular API.
 *
 */
#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
typedef HRESULT (*HSCORE_GETAPPFOLDERFUNTY)(HWND,int,HANDLE,DWORD,char*);
int
__hscore_getFolderPath(HWND hwndOwner,
		       int nFolder,
		       HANDLE hToken,
		       DWORD dwFlags,
		       char*  pszPath)
{
    static int loaded_dll = 0;
    static HMODULE hMod = (HMODULE)NULL;
    static HSCORE_GETAPPFOLDERFUNTY funcPtr = NULL;
    /* The DLLs to try loading entry point from */
    char* dlls[] = { "shell32.dll", "shfolder.dll" };
    
    if (loaded_dll < 0) {
	return (-1);
    } else if (loaded_dll == 0) {
	int i;
	for(i=0;i < sizeof(dlls); i++) {
	    hMod = LoadLibrary(dlls[i]);
	    if ( hMod != NULL &&
		 (funcPtr = (HSCORE_GETAPPFOLDERFUNTY)GetProcAddress(hMod, "SHGetFolderPathA")) ) {
		loaded_dll = 1;
		break;
	    }
	}
	if (loaded_dll == 0) {
	    loaded_dll = (-1);
	    return (-1);
	}
    }
    /* OK, if we got this far the function has been bound */
    return (int)funcPtr(hwndOwner,nFolder,hToken,dwFlags,pszPath);
    /* ToDo: unload the DLL on shutdown? */
}
#endif /* WIN32 */
#endif /* !__NHC__ */

