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

import android.os.AsyncTask;
import android.widget.ProgressBar;

import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.http.HttpAccessException;
import com.sandklef.coachapp.json.JsonAccessException;
import com.sandklef.coachapp.json.JsonSettings;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.Media;
import com.sandklef.coachapp.model.TrainingPhase;
import com.sandklef.coachapp.report.ReportUser;

import java.io.File;
import java.util.List;
import java.util.Random;

import coachassistant.sandklef.com.coachapp.R;

/**
 * Created by hesa on 2016-05-30.
 */
public class StorageSync extends AsyncTask<Void, StorageSync.StorageSyncBundle, StorageSync.StorageSyncBundle> {

    private static int OK           = 0;
    private static int NOT_DONE     = 1;
    private static int INTERRUPTED  = 2;
    private static int STORAGE_ERR  = 4;
    private static int CLUB_NOT_SET = 3;

    private static int STORAGE_SYNC_DOWNLOAD = 0;
    private static int STORAGE_SYNC_UPLOAD   = 1;
    private static int STORAGE_SYNC_DONE     = 2;

    private ProgressBar progressBar;

    private final static String LOG_TAG = StorageSync.class.getSimpleName();
    private StorageSyncListener listener;

    List<TrainingPhase> trainingPhases;
    List<Media>         mediaToUpload;

    private int uploadedTPs;
    private int downloadedInstructions;

    public StorageSync(StorageSyncListener l) throws StorageNoClubException {
        listener       = l;


        trainingPhases = Storage.getInstance().getTrainingPhases();
        mediaToUpload  = Storage.getInstance().getLocalMedia();

        CoachAppSession.getInstance().setSyncDialog(trainingPhases.size()+mediaToUpload.size(),
                CoachAppSession.getInstance().getContext().getString(R.string.sync_in_progress));
    }

    @Override
    protected void onPostExecute(StorageSyncBundle  bundle) {
        Log.d(LOG_TAG, " ------------------------------------------------------------------ onPostExecute");
        if (listener!=null) {
            listener.syncFinished(bundle);
        }

        int toDo = bundle.fileToDownload + bundle.fileToUpload;
        int done = bundle.getFilesDownloaded() + bundle.getFilesUploaded();

        if (bundle.getErrorCode()!=OK) {
            String msg = bundle.getErrorMessage();
            if (msg==null) {
                msg = " ";
            }

            String errMsg = CoachAppSession.getInstance().getString(R.string.sync_interrupted);

            Log.d(LOG_TAG, "error: '" + errMsg + "´");
            Log.d(LOG_TAG, "msgr:  '" + msg    + "´");

            ReportUser.informError(CoachAppSession.getInstance().getContext(), errMsg + " (" + msg + ")");
            ReportUser.log("Error:" + errMsg,msg);
        }


        ReportUser.log(done + " of " + toDo + " synced",
                "Synchronisation with server finished. " +
                        "Downloaded:"  + bundle.getFilesDownloaded() + "/" + bundle.fileToDownload +
                        "  Uploaded: " + bundle.getFilesUploaded() + "/" + bundle.fileToUpload);
    }


    public StorageSyncBundle syncAllMedia() throws StorageException {

        downloadedInstructions = 0;
        uploadedTPs = syncLocalMediaSync();

        Log.d(LOG_TAG, "Local files synced: " + uploadedTPs);

        // Download TrainingPhase videos
        int downloadedTPs = downloadTrainingPhaseFiles();

        // TODO: remove or not???
        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }


        Storage.getInstance().reReadMedia();


        /*


        public StorageSyncBundle(
                int     mode,
                int     fileToUpload,
                int     filesUploaded,
                int     fileToDownload,
                int     filesDownloaded,
                int     errorCode)


         */

        StorageSync.StorageSyncBundle bundle = new StorageSync.StorageSyncBundle(
                STORAGE_SYNC_DONE,
                mediaToUpload.size(),
                uploadedTPs,
                trainingPhases.size(),
                downloadedTPs,
                OK);
        return bundle;
    }


    private void updateProgress(int mode, int downloadCnt, int uploadCnt) {

        StorageSync.StorageSyncBundle bundle = new StorageSync.StorageSyncBundle(
                mode,
                trainingPhases.size(),
                downloadCnt,
                mediaToUpload.size(),
                uploadCnt,
                OK);

       // onProgressUpdate(bundle);


        publishProgress(bundle);
    }

    public int downloadTrainingPhaseFiles() {
        Log.d(LOG_TAG, "download tp videos");
//        StorageRemoteWorker srw = null;


        if (trainingPhases == null) {
            Log.d(LOG_TAG, "No data downlaoded.... Missing information about your club and team(s). Refresh");
            return 0;
        }
        int cnt = 0;
        Log.d(LOG_TAG, "download tp videos: " + trainingPhases.size());

        for (TrainingPhase tp : trainingPhases) {

            cnt++;

            Log.d(LOG_TAG, "Syncing (dload) file: " + tp);
            Log.d(LOG_TAG, "Syncing (dload) file: " + tp.getVideoUuid());

            if (!CoachAppSession.getInstance().getSyncMode()) {
                Log.d(LOG_TAG, "Uh oh download tp interrupted");
                return cnt;
            }

            updateProgress(STORAGE_SYNC_DOWNLOAD, cnt, uploadedTPs);

            Log.d(LOG_TAG, "Syncing (dload) file: " + cnt + " of " + trainingPhases.size() + " files");

            Media m = Storage.getInstance().getInstructionalMedia(tp.getUuid());

//            String currentMediaUuid = new File(m.fileName()).getName().replace(JsonSettings.SERVER_VIDEO_SUFFIX,"");

            if (m == null || m.fileName() == null ) {
                Log.d(LOG_TAG, "Syncing (dload) file: " + cnt + " of " + trainingPhases.size() + " files");

                try {
//                Storage.getInstance().downloadMediaFromServer(CoachAppSession.getInstance().getContext(), m);
                    new StorageRemoteWorker().downloadMedia(m);
                } catch (JsonAccessException e) {
                    e.printStackTrace();
                    Log.d(LOG_TAG, "Failed downloading: " + e);
                    Log.d(LOG_TAG, "Failed downloading: " + e.getMode());
                    Log.d(LOG_TAG, "Failed downloading: " + e.getMessage());
                    if (e.getMode()== HttpAccessException.FILE_NOT_FOUND) {
                        Storage.getInstance().removeMediaFromDb(m);
                    }
                } catch (StorageException e) {
                    e.printStackTrace();
                    Log.d(LOG_TAG, "Failed downloading: " + e);
                    Log.d(LOG_TAG, "Failed downloading: " + e.getMessage());
                }

            } else {
                Log.d(LOG_TAG, "No need to download: " + tp);
                Log.d(LOG_TAG, "No need to download: " + m.fileName());
            }
        }
        return cnt;
    }


    public int syncLocalMediaSync() throws StorageException {
        int cnt=0;
        try {
            Storage.getInstance().reReadMedia();
            Log.d(LOG_TAG, "syncLocalMediaSync");
            for (Media m: mediaToUpload) {

                if (!CoachAppSession.getInstance().getSyncMode()){
                    ReportUser.Log(R.string.sync_interrupted, R.string.sync_interrupted_user_detail);
                    return cnt;
                }

                cnt++;
                Log.d(LOG_TAG, "Syncing file: " + cnt + " of " + Storage.getInstance().getMedia().size() + " files");
//                publishProgress ("Syncing. Uploading local video " + cnt + " of " + Storage.getInstance().getMedia().size());

                updateProgress(STORAGE_SYNC_UPLOAD, 0, cnt);
                LocalStorageSync.getInstance().handleMediaSync(m);
            }
        } catch (StorageNoClubException e) {
            e.printStackTrace();
            if (!CoachAppSession.getInstance().getSyncMode()){
                ReportUser.Log(R.string.sync_interrupted, R.string.sync_interrupted_exc_detail);
                return cnt;
            }
        }
        return cnt;
    }


    @Override
    protected StorageSyncBundle doInBackground(Void... params) {
        StorageSyncBundle bundle;
        if (!CoachAppSession.getInstance().isSyncAllowed()) {
            return new StorageSyncBundle(OK);
        }
        ReportUser.Log(R.string.sync_started_msg, R.string.sync_started_detail);// "Synchronisation with server started");
        try {

            int localMediaCount = 0;
            int tpMediaCount = 0;

            try {
                localMediaCount = Storage.getInstance().getLocalMedia().size();
            } catch (StorageNoClubException e) {
                return new StorageSyncBundle(CLUB_NOT_SET);
            }

            // Set sync mode
            CoachAppSession.getInstance().setSyncMode();

            // Upload local media
            bundle = syncAllMedia();

            // LEAVE SYNC MODE
            CoachAppSession.getInstance().unsetSyncMode();


        } catch (StorageException e) {
            if (!CoachAppSession.getInstance().getSyncMode()){
                ReportUser.Log(R.string.sync_interrupted, R.string.sync_interrupted_exc_detail);
            }
            return new StorageSyncBundle(STORAGE_ERR, e.getMessage());
        }
        return bundle;
    }


    protected void onProgressUpdate(StorageSync.StorageSyncBundle ... progress) {

        if (CoachAppSession.getInstance().getSyncMode()) {

            StorageSync.StorageSyncBundle bundle = progress[0];

            Log.d(LOG_TAG, "Bundle: " + bundle + "  => " + bundle.getFilesDownloaded() + bundle.getFilesUploaded());

            String info = "";
            if (bundle.getMode() == STORAGE_SYNC_DOWNLOAD) {
                info += "Downloading " + bundle.getFilesDownloaded() + " / " + bundle.getFileToDownload() + "\n";
            } else {
                info = "Uploading " + bundle.getFilesUploaded() + " / " + bundle.getFileToUpload() + "\n";
            }
            Log.d(LOG_TAG, "onProgressUpdate() " + info);

            CoachAppSession.getInstance().setDialogInfo(bundle.getFilesDownloaded() + bundle.getFilesUploaded(),
                    CoachAppSession.getInstance().getContext().getString(R.string.sync_in_progress));
        }

    }

    public class StorageSyncBundle {

        private int     mode;
        private int     fileToUpload;
        private int     filesUploaded;
        private int     fileToDownload;
        private int     filesDownloaded;
        private int     errorCode;
        private String  errorMsg;

        public StorageSyncBundle(
                int     mode,
                int     fileToUpload,
                int     filesUploaded,
                int     fileToDownload,
                int     filesDownloaded,
                int     errorCode) {
            this.mode             = mode;
            this.fileToUpload     = fileToUpload;
            this.filesUploaded    = filesUploaded;
            this.fileToDownload   = fileToDownload;
            this.filesDownloaded  = filesDownloaded;
            this.errorCode        = errorCode;
        }

        public StorageSyncBundle(int     errorCode) {
            this.errorCode        = errorCode;
            this.errorMsg         = "";
        }

        public StorageSyncBundle(int     errorCode, String errorMsg) {
            this.errorCode        = errorCode;
            this.errorMsg         = errorMsg;
        }

        public int     getMode()            {return mode;}
        public int     getFileToUpload()    {return fileToUpload;}
        public int     getFilesUploaded()   {return filesUploaded;}
        public int     getFileToDownload()  {return fileToDownload;}
        public int     getFilesDownloaded() {return filesDownloaded;}
        public int     getErrorCode()       {return errorCode;}
        public String  getErrorMessage()    {return errorMsg;}

        public boolean getSuccess()         {return filesDownloaded==fileToDownload && filesUploaded==fileToDownload; }

        public String toString() {
            return "mode: " + mode + " " + filesUploaded+ "/" + fileToUpload + "  " + filesDownloaded + "/" + fileToDownload ;
        }

    }



}
