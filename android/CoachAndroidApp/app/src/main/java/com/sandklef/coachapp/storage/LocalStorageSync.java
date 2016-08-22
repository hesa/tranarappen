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

import android.content.Context;
import android.os.AsyncTask;

import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.json.JsonAccessException;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.Media;

import java.security.acl.LastOwnerException;

/**
 * Created by hesa on 2016-03-01.
 */
public class LocalStorageSync {

    private static LocalStorageSync instance;
    private final static String LOG_TAG = LocalStorageSync.class.getSimpleName();
    private Context context;

    private LocalStorageSync(Context context) {
        this.context = context;
    }

    public static LocalStorageSync newInstance(Context context) {
        Log.d(LOG_TAG, "LocalStorageSync newInstance()");

        if (instance==null) {
            instance = new LocalStorageSync(context);
        }
        return instance;
    }

    public static LocalStorageSync getInstance() {
        return instance;
    }

    /*

                case CONTEXT_MENU_CREATE:
                Log.d(LOG_TAG, "Create media (file): " + m.fileName());
                break;
            case CONTEXT_MENU_UPLOAD:
                Log.d(LOG_TAG, "Upload media (file): " + media.get(position).fileName());
                Storage.getInstance().uploadMediaToServer(getApplicationContext(), m);
                break;
            case CONTEXT_MENU_DOWNLOAD:
                Log.d(LOG_TAG, "Download media (file): " + media.get(position).fileName());
                Storage.getInstance().downMediaFromServer(getApplicationContext(), m);


     */



    public void handleMedia(Media m) {

        int uploaded;

        Log.d(LOG_TAG, "handleMedia() m: " + m.getStatus() + " " + m.getName());
        //TODO: bring back code below

        if (m.getStatus() == Media.MEDIA_STATUS_NEW ) {
            Log.d(LOG_TAG, " new, will create        " + m.fileName());

            Storage.getInstance().createMediaOnServer(context, m);
        }
        if (m.getStatus() == Media.MEDIA_STATUS_CREATED ) {
            Log.d(LOG_TAG, " created, will upload    " + " " + m.fileName());
            Storage.getInstance().uploadMediaToServer(context, m);
        }
    }

    public void handleMediaSync(Media m) throws StorageException {
        try {
            int uploaded;
            Log.d(LOG_TAG, "handleMedia() m: " + m.getStatus() + " " + m.getName());
            if (m.getStatus() == Media.MEDIA_STATUS_NEW ) {
                Log.d(LOG_TAG, " new, will create        " + m.fileName());
                new StorageRemoteWorker().createMedia(m);
            }

            if (m.getStatus() == Media.MEDIA_STATUS_CREATED ) {
                Log.d(LOG_TAG, " created, will upload    " + " " + m.fileName());
                new StorageRemoteWorker().uploadMedia(m);
            //    Storage.getInstance().uploadMediaToServer(context, m);
            }
        } catch (JsonAccessException e) {
            Log.d(LOG_TAG, "handleMediaSync, got an exception... handle me better: ´" + e.getMessage() + "´");
            e.printStackTrace();
            throw new StorageException(e.getMessage());
        }
    }

    public void syncLocalMedia() {
        try {
            Log.d(LOG_TAG, "syncLocalMedia");
            for (Media m: Storage.getInstance().getMedia()) {
                if (CoachAppSession.getInstance().getSyncMode()) {
                    handleMedia(m);
                } else {
                    Log.d(LOG_TAG, "Sync mode not set, discarding media: " + m);
                }
            }
        } catch (StorageNoClubException e) {
            e.printStackTrace();
        }
    }



    public void syncLocalStorage() {
        Log.d(LOG_TAG, "syncLocalStorage()");
        Log.d(LOG_TAG, "syncLocalStorage()");
        new LocalSyncTask().execute(new LocalSyncTaskSettings());
    }

    private class LocalSyncTaskSettings {
    }

    private class LocalSyncTaskReturn {
    }

    private class LocalSyncTask extends AsyncTask <LocalSyncTaskSettings, Void, LocalSyncTaskReturn> {

        @Override
        protected LocalSyncTaskReturn doInBackground(LocalSyncTaskSettings... params) {
            Log.d(LOG_TAG, "LocalSyncTaskReturn doInBackground()");

            CoachAppSession.getInstance().addDialoguser();
            syncLocalMedia();

            return null;
        }

        @Override
        protected void onPostExecute(LocalSyncTaskReturn localSyncTaskReturn) {
            super.onPostExecute(localSyncTaskReturn);
        }
    }

}
