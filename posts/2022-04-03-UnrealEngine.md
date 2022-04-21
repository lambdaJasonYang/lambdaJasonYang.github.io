---
title: Unreal Engine and Android Setup
tags: prog
---


For Windows, we will fix 3 errors

1st Error

```bash
Unable to locate local Android SDK location. Did you run Android Studio after installing?
```

2st Error

```bash
Android Studio Path: D:\Program Files\Android\Android Studio
Android Studio SDK Path: C:\Users\MyPC\AppData\Local\Android\Sdk
\Nmap ...
```

3nd Error

```bash
Exception in thread "main" java.lang.NoClassDefFoundError: javax/xml/bind/annotation/XmlSchema
        at com.android.repository.api.SchemaModule$SchemaModuleVersion.<init>(SchemaModule.java:156)
        at com.android.repository.api.SchemaModule.<init>(SchemaModule.java:75)
        at com.android.sdklib.repository.AndroidSdkHandler.<clinit>(AndroidSdkHandler.java:81)
        at com.android.sdklib.tool.sdkmanager.SdkManagerCli.main(SdkManagerCli.java:73)
        at com.android.sdklib.tool.sdkmanager.SdkManagerCli.main(SdkManagerCli.java:48)
Caused by: java.lang.ClassNotFoundException: javax.xml.bind.annotation.XmlSchema
        at java.base/jdk.internal.loader.BuiltinClassLoader.loadClass(BuiltinClassLoader.java:581)
        at java.base/jdk.internal.loader.ClassLoaders$AppClassLoader.loadClass(ClassLoaders.java:178)
        at java.base/java.lang.ClassLoader.loadClass(ClassLoader.java:522)
        ... 5 more
Update failed. Please check the Android Studio install.
```

# Fix

## Fix 1

```bat
set ANDROID_LOCAL=%LOCALAPPDATA%\Android\Sdk
set ANDROID_LOCAL=D:\AppData\Local\Android\Sdk
```



## Fix 2

In the `SetupAndroid.bat` file add usebackq, to the 

```bat
FOR /F "tokens=2* usebackq" %%A IN ('REG.exe query "%KEY_NAME%" /v "%VALUE_NAME%"') DO (set USERPATH=%%B)
```

## Fix 3

Remove JAVA_HOME from environmental variables path.

```bat
set SDKMANAGER=%STUDIO_SDK_PATH%\tools\bin\sdkmanager.bat
set SDKMANAGER=%STUDIO_SDK_PATH%\cmdline-tools\latest\bin\sdkmanager.bat
```


```bat
@echo off
setlocal ENABLEEXTENSIONS
setlocal ENABLEDELAYEDEXPANSION
set KEY_NAME=HKLM\SOFTWARE\Android Studio
set VALUE_NAME=Path
set STUDIO_PATH=

IF "%5" == "-noninteractive" (
	set PAUSE=
) ELSE (
	set PAUSE=pause
)

SET PLATFORMS_VERSION=%1
SET BUILDTOOLS_VERSION=%2
SET CMAKE_VERSION=%3
SET NDK_VERSION=%4

rem hardcoded versions for compatibility with non-Turnkey manual running
if "%PLATFORMS_VERSION%" == "" SET PLATFORMS_VERSION=android-30
if "%BUILDTOOLS_VERSION%" == "" SET BUILDTOOLS_VERSION=30.0.3
if "%CMAKE_VERSION%" == "" SET CMAKE_VERSION=3.10.2.4988404
if "%NDK_VERSION%" == "" SET NDK_VERSION=21.4.7075529


FOR /F "tokens=2*" %%A IN ('REG.exe query "%KEY_NAME%" /v "%VALUE_NAME%"') DO (set STUDIO_PATH=%%B)

IF EXIST "%STUDIO_PATH%" (
	echo.
	) ELSE (
	echo Android Studio not installed, please download Android Studio 3.5.3 from https://developer.android.com/studio
	%PAUSE%
	exit /b 1
)
echo Android Studio Path: %STUDIO_PATH%

set VALUE_NAME=SdkPath
set STUDIO_SDK_PATH=
FOR /F "tokens=2*" %%A IN ('REG.exe query "%KEY_NAME%" /v "%VALUE_NAME%"') DO (set STUDIO_SDK_PATH=%%B)

set ANDROID_LOCAL=%LOCALAPPDATA%\Android\Sdk
set ANDROID_LOCAL=D:\AppData\Local\Android\Sdk

if "%STUDIO_SDK_PATH%" == "" (
	IF EXIST "%ANDROID_LOCAL%" (
		set STUDIO_SDK_PATH=%ANDROID_LOCAL%
	) ELSE (
		IF EXIST "%ANDROID_HOME%" (
			set STUDIO_SDK_PATH=%ANDROID_HOME%
		) ELSE (
			echo Unable to locate local Android SDK location. Did you run Android Studio after installing?
			%PAUSE%
			exit /b 2
		)
	)
)
echo Android Studio SDK Path: %STUDIO_SDK_PATH%

if DEFINED ANDROID_HOME (set a=1) ELSE (
	set ANDROID_HOME=%STUDIO_SDK_PATH%
	setx ANDROID_HOME "%STUDIO_SDK_PATH%"
)
if DEFINED JAVA_HOME (set a=1) ELSE (
	set JAVA_HOME=%STUDIO_PATH%\jre
	setx JAVA_HOME "%STUDIO_PATH%\jre"
)
set NDKINSTALLPATH=%STUDIO_SDK_PATH%\ndk\%NDK_VERSION%
set PLATFORMTOOLS=%STUDIO_SDK_PATH%\platform-tools;%STUDIO_SDK_PATH%\tools

set KEY_NAME=HKCU\Environment
set VALUE_NAME=Path
set USERPATH=

FOR /F "tokens=2* usebackq" %%A IN ('REG.exe query "%KEY_NAME%" /v "%VALUE_NAME%"') DO (set USERPATH=%%B)

where.exe /Q adb.exe

echo "%ERRORLEVEL%"
IF /I "%ERRORLEVEL%" NEQ "0" (
	echo Current user path: %USERPATH%
	setx PATH "%USERPATH%;%PLATFORMTOOLS%"
	echo Added %PLATFORMTOOLS% to path

)
echo %STUDIO_SDK_PATH%
echo %PLATFORMTOOLS%
set SDKMANAGER=%STUDIO_SDK_PATH%\tools\bin\sdkmanager.bat
set SDKMANAGER=%STUDIO_SDK_PATH%\cmdline-tools\latest\bin\sdkmanager.bat
IF EXIST "%SDKMANAGER%" (
	echo Using sdkmanager: %SDKMANAGER%
) ELSE (
	set SDKMANAGER=%STUDIO_SDK_PATH%\cmdline-tools\latest\bin\sdkmanager.bat
	IF EXIST "!SDKMANAGER!" (
		echo Using sdkmanager: !SDKMANAGER!
	) ELSE (
		echo Unable to locate sdkmanager.bat. Did you run Android Studio and install cmdline-tools after installing?
		%PAUSE%
		exit /b 3
	)
)

call "%SDKMANAGER%" "platform-tools" "platforms;%PLATFORMS_VERSION%" "build-tools;%BUILDTOOLS_VERSION%" "cmake;%CMAKE_VERSION%" "ndk;%NDK_VERSION%"

IF /I "%ERRORLEVEL%" NEQ "0" (
	echo Update failed. Please check the Android Studio install.
	%PAUSE%
	exit /b 4
)

if EXIST "%NDKINSTALLPATH%" (
	echo Success!
	setx NDKROOT "%NDKINSTALLPATH%"
	setx NDK_ROOT "%NDKINSTALLPATH%"
) ELSE (
	echo Update failed. Did you accept the license agreement?
	%PAUSE%
	exit /b 5
)

%PAUSE%
exit /b 0

```

# Unreal engine settings

Go to Edit >> Project Settings >> Platforms >> Android SDK

Location of Android SDK: D:/AppData/Local/Android/Sdk  
Location of Android NDK: D:/AppData/Local/Android/Sdk/ndk/21.4.7075529  
Location of JAVA: D:/Program Files/Android/Android Studio/jre  

WARNING - Location of Android NDK cannot be just the ndk folder, it must be the specific subfolder 21.4.7075529 which represents the version of NDK that unreal engine uses.  


# exporting apk

1. Generate Signing 

```bash
D:
cd D:\Program Files\Android\Android Studio\jre\bin
keytool -genkey -v -keystore key.keystore -alias Mykey -keyalg RSA -keysize 2048 -validity 10000 
#this will generate a key.keystore file
#copy this key.keystore file to your unreal project build android folder
#C:\Users\MyPC\Documents\Unreal Projects\Bleh\Build\Android
```
Remember:  
alias: Mykey  
password: *****  

Modify Unreal engine settings to include the keystore  

Go to Settings  
Build Configuration: Shipping  
Full Rebuild: Checked    
For Distribution: Checked   

Go to Platform >> Android >> Use Project Setting(Shipping) Checked   

## For playstore

In Project Settings >> Android:  

* **YOU MUST UNCHECK "Package game data inside .apk"** or else the size will be 6 times bigger which will be rejected by play store.  
* Target SDK verion : 30 , as of 2022 or else play store rejects  
* Check "Disable verify OBB on first start/update"
* Remember to increase "Store Version" if updating app


## Build

Platform >> Android >> Click Package Project  
Make a new folder for the output files  


# Aside

double-check if your keystore pass 

```bash
keytool -list -keystore <keystorefile> -storepass <passwordtocheck>
```