NEXUS=Nexus_10_API_21
LD_LIBRARY_PATH=/local/android-sdk-linux/tools/lib/
ASDK_PATH=/home/hesa/opt/Android

default:
	./gradlew assembleDebug

all: clean
	./gradlew assembleDebug

clean:
	./gradlew clean

uninstall: 
	$(ASDK_PATH)/platform-tools/adb  uninstall com.sandklef.coachapp

install: 
	$(ASDK_PATH)/platform-tools/adb  install ./app/build/outputs/apk/app-debug.apk 

startapp:
	$(ASDK_PATH)/platform-tools/adb  shell am start -n com.sandklef.coachapp/com.sandklef.coachapp.MainActivity

logcat:
	gnome-terminal gnome-terminal  --hide-menubar --geometry 88x100-0-0 -e "adb logcat -b com.sandklef.coachapp "

run: clean default uninstall install startapp 

nexus:
	export LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) ; \
            $(ASDK_PATH)tools/emulator64-x86 -avd $(NEXUS) -netspeed full -netdelay none&





