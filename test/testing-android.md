# Installing 

## Preparation on server

## Preparation on app

* Force stop
* Clean up data
* Uninstall app
* Clean up directory 

Probably easiest done with the following commands:

  `adb shell am force-stop coachassistant.sandklef.com.coachapp`

  `adb shell pm clear coachassistant.sandklef.com.coachapp`

  `adb shell pm uninstall coachassistant.sandklef.com.coachapp`

  `adb shell rm -fr /mnt/sdcard/com.sandklef.coachapp/`

## Installation

## Verify more information link

* Click more information and make sure a browser is started with the url: www.tranarappen.se

## Login with faulty credentials

* file wrong username => should not work

* file wrong password => should not work

## Verify club

* Go to Club information (top menu->Club information)

* Make sure you can see the example club "AS Roma" listed

* Go back to "training mode"

## Verify teams

* Refresh by clicking refresh information (top menu->Refresh)

* The example teams shall be visible (F01, P01)

## Verify training phases

* Click on each team and verify that the following training phases shall be seen:

** Bogdan
** Hoppskott
** Kantavslut
** Kullager  
** Stegisättning

