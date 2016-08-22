/******************************************************************************
 *      Copyright (c) 2015 - 2016 Henrik Sandklef
 *
 *  This file is part of Coach Assistant
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package com.sandklef.coachapp.activities;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.provider.MediaStore;

import com.sandklef.coachapp.fragments.VideoCapture;
import com.sandklef.coachapp.json.JsonAccess;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.Member;
import com.sandklef.coachapp.model.Team;
import com.sandklef.coachapp.model.TrainingPhase;
import com.sandklef.coachapp.storage.LocalStorage;
import com.sandklef.coachapp.storage.Storage;
import com.sandklef.coachapp.storage.StorageNoClubException;

import java.io.File;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

/**
 * Created by hesa on 2016-02-27.
 */
public class ActivitySwitcher {

    private final static String LOG_TAG = ActivitySwitcher.class.getSimpleName();

    // TODO: MOVE somewhere
    public final static String VIDEO_FILE_DATE_FORMAT = "yyyyMMdd-HHmmss";
    public  final static String VIDEO_FILE_TYPE_SUFFIX = ".mp4";
    public final static int VIDEO_FILE_DEFAULT_TIME = 5000;


    private static void startActivityImpl(Activity a, Class c) {
        Intent intent = new Intent(a, c);
        a.startActivity(intent);
    }

    private static void startActivityImpl(Activity a, Class c, Bundle b) {
        Intent intent = new Intent(a, c);
        intent.putExtras(b);
        a.startActivity(intent);
    }

    private static void startActivityImpl(Context con, Class c) {
        Intent intent = new Intent(con, c);
        con.startActivity(intent);
    }

    // TODO: remove this method. should not be needed
    public static void startTrainingActivity(Activity a) {
        startActivityImpl(a, com.sandklef.coachapp.activities.TeamsActivity.class);
    }

    public static void startSplashActivity(Activity a) {
        LocalStorage.getInstance().setSplashDelay(100);
        startActivityImpl(a, com.sandklef.coachapp.activities.SplashActivity.class);
    }

    public static void startLoginActivity(Activity a) {
        // TODO: make sure back stack is deleted
        startActivityImpl(a, com.sandklef.coachapp.activities.LoginActivity.class);
    }

    public static void startMediaRecorderActivity(Activity a, String file) {
        // TODO: make sure back stack is deleted
        Bundle mBundle = new Bundle();
        mBundle.putString("file",file);
        startActivityImpl(a, com.sandklef.coachapp.activities.MediaRecorderActivity.class, mBundle);
    }

    public static void startTeamActivity(Activity a) {
        startActivityImpl(a, com.sandklef.coachapp.activities.TeamsActivity.class);
    }

    public static void startTeamActivity(Context c) {
        Intent intent = new Intent(c, com.sandklef.coachapp.activities.TeamsActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_MULTIPLE_TASK);
        c.startActivity(intent);
    }


    public static void startTrainingPhaseActivity(Activity a) {
        startActivityImpl(a, com.sandklef.coachapp.activities.TrainingPhasesActivity.class);
    }

    public static void startMemberActivity(Activity a) {
        startActivityImpl(a, com.sandklef.coachapp.activities.MemberActivity.class);
    }

    public static void startLocalMediaManager(Activity a) {
        startActivityImpl(a, com.sandklef.coachapp.activities.LocalMediaManager.class);
    }

    public static void startLogMessageActivity(Activity a) {
        startActivityImpl(a, com.sandklef.coachapp.activities.LogActivity.class);
    }

    public static void startSettingsActivity(Activity a) {
        startActivityImpl(a, com.sandklef.coachapp.activities.SettingsActivity.class);
    }

    public static void startClubInfoActivity(Activity a) {
        startActivityImpl(a, com.sandklef.coachapp.activities.ClubInfoActivity.class);
    }

    public static boolean startRecording(Activity a) {

        DateFormat df = new SimpleDateFormat(VIDEO_FILE_DATE_FORMAT);
        Date today = Calendar.getInstance().getTime();
        String mediaDate = df.format(today);

        String newFileName = LocalStorage.getInstance().getNewMediaDir() + "/" +
                mediaDate + VIDEO_FILE_TYPE_SUFFIX;

        File newFile = new File(newFileName);
        String dirName = newFile.getParent();
        File dir = new File(dirName);

        Log.d(LOG_TAG, "  Dir:  " + dir.getPath());
        boolean created = dir.mkdirs();

        Log.d(LOG_TAG, "RECORD TO NEW FILE: " + newFile);

        Uri uri = Uri.fromFile(newFile);

        // create Intent to take a picture and return control to the calling application
        Intent intent = new Intent(MediaStore.ACTION_VIDEO_CAPTURE);

        Log.d(LOG_TAG, "  file: " + newFile.getParent() + " " + newFile + " " + uri);

        intent.putExtra(MediaStore.EXTRA_OUTPUT, uri);
        intent.putExtra("android.intent.extra.durationLimit", 5);
        intent.putExtra(MediaStore.EXTRA_FINISH_ON_COMPLETION, true);
        intent.putExtra(MediaStore.EXTRA_DURATION_LIMIT, 5);
        intent.putExtra(MediaStore.EXTRA_VIDEO_QUALITY, 1); // set the video image quality to high
        // start the image capture Intent
        //context.startActivity(intent);
        a.startActivityForResult(intent, VideoCapture.VIDEO_CAPTURE);
        return true;
    }


    public static void printDbFull() {
        printDbImpl(true, "");
    }

    public static void printDb() {
        printDbImpl(false, "");
    }

    public static void printDbFull(String s) {
        printDbImpl(true, s);
    }

    public static void printDb(String s) {
        printDbImpl(false, s);
    }

    private static void printDbImpl(boolean full, String prefix) {
        try {
            if (Storage.getInstance()!=null) {
                Log.d(LOG_TAG, prefix + "Teams:         " + Storage.getInstance().getTeams().size());
                return;
            }

            if (full) {
                for (Team t : Storage.getInstance().getTeams()) {
                    Log.d(LOG_TAG, " * " + t);
                }
            }
            Log.d(LOG_TAG, prefix + "Trainingphases:" + Storage.getInstance().getTrainingPhases().size());
            if (full) {
                for (TrainingPhase t : Storage.getInstance().getTrainingPhases()) {
                    Log.d(LOG_TAG, " * " + t);
                }
            }
            Log.d(LOG_TAG, prefix + "Members:      " + Storage.getInstance().getMembers().size());
            if (full) {
                for (Member m : Storage.getInstance().getMembers()) {
                    Log.d(LOG_TAG, " * " + m);
                }
            }
        } catch (StorageNoClubException e) {
            e.printStackTrace();
        }


    }

}
