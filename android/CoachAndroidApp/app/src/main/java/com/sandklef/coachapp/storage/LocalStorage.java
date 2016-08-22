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

package com.sandklef.coachapp.storage;

import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.fragments.VideoCapture;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.*;
import com.sandklef.coachapp.report.ReportUser;

import android.content.Context;
import android.content.SharedPreferences;
import android.os.Environment;
import android.preference.PreferenceManager;

import java.io.File;
import java.io.IOException;
import java.security.acl.LastOwnerException;

import coachassistant.sandklef.com.coachapp.R;

public class LocalStorage {

    private final static String LOG_TAG = LocalStorage.class.getSimpleName();

    private static LocalStorage localStore;
    private final static String sharedPrefname = "session";
    private SharedPreferences settings;

    private static final String SESSION_KEY               = "session-key";
    private static final String CURRENT_CLUB_KEY          = "current-club";
    private static final String CURRENT_CLUB_NAME_KEY     = "current-club-name";
    private static final String CURRENT_TEAM_KEY          = "current-team";
    private static final String CURRENT_TRAININGPHASE_KEY = "current-trainingphase";
    private static final String CURRENT_MEMBER_KEY        = "current-member";
    private static final String LATEST_USER_KEY           = "latest-user";
    private static final String LATEST_USER_EMAIL_KEY     = "latest-user-email";
    private static final String LATEST_USER_TOKEN_KEY     = "latest-user-token";
    private static final String CURRENT_CONNECTION_STATUS = "current-satus";
    private static final String TP_RECORDING_TIME         = "tp-recording-time"; // secs to record a member
    private static final String INSTR_RECORDING_TIME      = "instructional-recording-time"; // secs to record a performed tp
    private static final String VIDEO_RECORDINNG_TIME     = "video-record-time";
    private static final String LOG_MESSAGE_LIMIT         = "log-message-limit";
    private static final String SYNC_ON_WIFI_ONLY         = "wifi-sync-only";
    private static final String SPLASH_SCREEN_DELAY       = "splash-delay";
    private static final String NETWORK_TIMEOUT           = "network_timeout";

    private String urlBase;

    private Context c;

    //    private static String currentClub;
    //private String currentTeam;
    private String currentTrainingPhase;
    private String currentMember;


/*
    public static final String[] imageExtensions = {".png", ".jpg"};
    public static final String[] videoExtensions = {".mp4", ".ogg"};

    public static String COACHAPP_IMAGE_SUFFIX = ".png";
    public static String COACHAPP_VIDEO_SUFFIX = ".mp4";
*/

    /*
    public final static String TEAM_TAG          = "team";
    public final static String TEAMS_TAG         = "teams";
    public final static String TRAININGPHASE_TAG = "trainingphase";
    public final static String TRAINING_TAG      = "training";
*/


    private LocalStorage(Context c) {
        settings = c.getSharedPreferences(sharedPrefname, 0);
        this.c = c;

        // Create dirs in advance
        getNewMediaDir();
        getDeletableMediaDir();
        getDownloadMediaDir();
    }

    public static LocalStorage newInstance(Context c) {
        Log.d(LOG_TAG, "newInstance(Context (" + c + ")");
        if (localStore == null) {
            localStore = new LocalStorage(c);
        }
        return localStore;
    }

    private void setKeyValueString(String key, String value) {
        SharedPreferences.Editor editor = settings.edit();
        editor.putString(key, value);
        editor.commit();
    }

    private void setKeyValueString(String key, int value) {
        SharedPreferences.Editor editor = settings.edit();
        editor.putInt(key, value);
        editor.commit();
    }

    public String getServerUrl() {
        return urlBase;
    }

    public void setServerUrl(String url) {
        urlBase = url;
    }

   /* public void storeSessionToken(String sessId) {
        setKeyValueString(SESSION_KEY, sessId);
    }

    public void resetSessionToken() {
        setKeyValueString(SESSION_KEY, "");
    }*/

    /*
    public void setLatestUser(int id) {
        setKeyValueInt(LATEST_USER_KEY, id);
    }

    public int getLatestUser() {
        return getKeyValueInt(LATEST_USER_KEY);
    }
    */

    public void setLatestUserEmail(String email) {
        setKeyValueString(LATEST_USER_EMAIL_KEY, email);
    }

    public String getLatestUserEmail() {
        return getKeyValueString(LATEST_USER_EMAIL_KEY);
    }

    public void setLatestUserToken(String token) {
        Log.d(LOG_TAG, "setLatestUserToken: '" + token + "'");
        setKeyValueString(LATEST_USER_TOKEN_KEY, token);
    }

    public String getLatestUserToken() {
        return getKeyValueString(LATEST_USER_TOKEN_KEY);
    }

/*    public String getSessionToken()               {
        return getKeyValueString(SESSION_KEY);
    }
*/

    public String getKeyValueString(String key) {
        return settings.getString(key, "");
    }

    public void setKeyValueInt(String key, int value) {
        SharedPreferences.Editor editor = settings.edit();
        editor.putInt(key, value);
        editor.commit();

    }

    public int getKeyValueInt(String key) {
        return settings.getInt(key, 0);
    }

    public static LocalStorage getInstance() {
        return localStore;
    }



/*    public void setCurrentClub(String uuid) {
        Log.d(LOG_TAG, "Set current club uuid: " + uuid);
        setKeyValueString(CURRENT_CLUB_KEY, uuid);
    }

    public String getCurrentClub() {
        Log.d(LOG_TAG, "Get current club uuid: " + getKeyValueString(CURRENT_CLUB_KEY));
        return getKeyValueString(CURRENT_CLUB_KEY);
    }
    */

    public void setSplashDelay(int delay) {
        setKeyValueInt(SPLASH_SCREEN_DELAY, delay);
    }
    public int  getSplashDelay()          {
        return getKeyValueInt(SPLASH_SCREEN_DELAY);
    }

    public void setCurrentClub(String t) {
        setKeyValueString(CURRENT_CLUB_KEY, t);
    }

    public String getCurrentClub() {
        return getKeyValueString(CURRENT_CLUB_KEY);
    }

    public void setCurrentClubName(String t) {
        setKeyValueString(CURRENT_CLUB_NAME_KEY, t);
    }

    public String getCurrentClubName() {
        return getKeyValueString(CURRENT_CLUB_NAME_KEY);
    }

    public void setCurrentTeam(String t) {
      //  currentTeam = t;
        Log.d(LOG_TAG, "Storing team: " + t);
        setKeyValueString(CURRENT_TEAM_KEY, t);
        Log.d(LOG_TAG, "Storing team: " + getKeyValueString(CURRENT_TEAM_KEY));
    }

    public String getCurrentTeam() {
        //return currentTeam;
        return getKeyValueString(CURRENT_TEAM_KEY);
    }

    public void setCurrentTrainingPhase(String tp) {
        currentTrainingPhase = tp;
    }


    public String getCurrentTrainingPhase() {
        return currentTrainingPhase;
    }

    public void setCurrentMember(String m) {
        currentMember = m;
    }

    public String getCurrentMember() {
        return currentMember;
    }



/*
    public void setCurrentMember(String uuid) {
        setKeyValueString(CURRENT_MEMBER_KEY, uuid);
    }

    public String getCurrentMember() {
        return getKeyValueString(CURRENT_MEMBER_KEY);
    }

    public void setCurrentTeam(String uuid) {
        setKeyValueString(CURRENT_TEAM_KEY, uuid);
    }

    public String getCurrentTeam() {
        return getKeyValueString(CURRENT_TEAM_KEY);
    }

    public void setCurrentTrainingPhase(String uuid) {
        setKeyValueString(CURRENT_TRAININGPHASE_KEY, uuid);
   }
    public String getCurrentTrainingPhase() {
        return getKeyValueString(CURRENT_TRAININGPHASE_KEY);
    }
*/

    public void setConnectionStatus(int mode) {
        setKeyValueInt(CURRENT_CONNECTION_STATUS, mode);
    }

    public int getConnectionStatus() {
        return getKeyValueInt(CURRENT_CONNECTION_STATUS);
    }




    public String getAppDir() {
// try {
        String dir = Environment.getExternalStorageDirectory().toString() +  "/com.sandklef.coachapp/" + getCurrentClub() + "/";
  /*      } catch (IOException e) {
            Log.w(LOG_TAG, "Failed getting default path");
            ReportUser.warning(c, "Could not get default internal file storage directory\nThis app might not work. Please report this as a bug");
            return null;
        }*/
        Log.d(LOG_TAG, "  getAppDir() => " + dir);
        return dir;
    }

    private String getMediaDirImpl(String extra) {
        if (extra == null) { return null; }
        File f = new File(getAppDir() + extra);
        if (! f.exists()) {
            f.mkdirs();
        }
        return f.getAbsolutePath();
    }

    public String getNewMediaDir() {
        return getMediaDirImpl(c.getString(R.string.NEW_MEDIA_DIR));
    }

    public String getDownloadMediaDir() {
        return getMediaDirImpl(c.getString(R.string.DOWNLOAD_MEDIA_DIR));
    }

    public String getDeletableMediaDir() {
        return getMediaDirImpl(c.getString(R.string.DELETABLE_MEDIA_DIR));
    }

    public boolean replaceLocalWithDownloaded(Media m, String newFileName) {
        Log.d(LOG_TAG, "replaceLocalWithDownloaded " + m +  " with: " + newFileName);
        if (m.fileName()!=null) {
            File oldFile = new File(m.fileName());
            if (Storage.getInstance().updateMediaReplaceDownloadedFile(m, newFileName)) {
                String replaceFileName = m.fileName().replace(c.getString(R.string.NEW_MEDIA_DIR),
                        c.getString(R.string.DELETABLE_MEDIA_DIR));
                File newFile =
                        new File(replaceFileName);
                // Create deletable dir first
                Log.d(LOG_TAG, "Move " + oldFile.getAbsolutePath() + " => " + newFile.getAbsolutePath());
                return oldFile.renameTo(newFile);
            }
        }else {
            if (Storage.getInstance().updateMediaReplaceDownloadedFile(m, newFileName)) {
                return true;
            }
        }
        return false;
    }


    public void setInstructionalRecordingTime(int value) {
        setKeyValueInt(INSTR_RECORDING_TIME, value);
    }

/*    public void setTPRecordingTime(int value) {
        setKeyValueInt(TP_RECORDING_TIME, value);
    }
*/

    private String getDefaultPrefValue(String key, String defaultValue) {
        SharedPreferences sharedPref = PreferenceManager.getDefaultSharedPreferences(CoachAppSession.getInstance().getContext());
        return sharedPref.getString(key, defaultValue);
    }

    private boolean getDefaultPrefValue(String key, boolean def) {
        SharedPreferences sharedPref = PreferenceManager.getDefaultSharedPreferences(CoachAppSession.getInstance().getContext());
        return sharedPref.getBoolean(key, def);
    }

    private int stringToIntDefault(String key, int defaultValue) {
        try {
            String value = getDefaultPrefValue(key, ""+defaultValue);
            return Integer.parseInt(value);
        } catch (NumberFormatException e ) {
            return defaultValue;
        }
    }

    public int getVideoRecordingTime() {
        return stringToIntDefault(VIDEO_RECORDINNG_TIME, VideoCapture.DEFAULT_INSTR_RECORDING_TIME);
    }

    public int getLogMessageLimit() {
        return stringToIntDefault(LOG_MESSAGE_LIMIT, 100);
    }

    public int getnetworkTimeout() {
        return stringToIntDefault(NETWORK_TIMEOUT, 5000);
    }

    public boolean getSyncOnWifiOnly() {
        return getDefaultPrefValue(SYNC_ON_WIFI_ONLY, true);
    }




/*
    public int getTPRecordingTime(){
        int ret = getKeyValueInt(TP_RECORDING_TIME);
        if (ret==0) {
            return VideoCapture.DEFAULT_TP_RECORDING_TIME;
        }
        return ret;
    }
*/
}
