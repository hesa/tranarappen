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

import android.app.ProgressDialog;
import android.content.DialogInterface;
import android.os.AsyncTask;
import android.support.v7.app.AlertDialog;

import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.json.JsonAccess;
import com.sandklef.coachapp.json.JsonAccessException;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.Media;
import com.sandklef.coachapp.model.Member;
import com.sandklef.coachapp.model.Team;
import com.sandklef.coachapp.model.TrainingPhase;

import java.io.IOException;
import java.util.List;


import com.sandklef.coachapp.json.JsonSettings;

import coachassistant.sandklef.com.coachapp.R;

/**
 * Created by hesa on 2016-02-25.
 */
public class StorageRemoteWorker extends AsyncTask<StorageRemoteWorker.AsyncBundle, Void, StorageRemoteWorker.AsyncBundle> {

    private final static String LOG_TAG = StorageRemoteWorker.class.getSimpleName();

    private JsonAccess ja;

    public StorageRemoteWorker() throws StorageException {
        try {
            ja = new JsonAccess();
        } catch (JsonAccessException e) {
            throw new StorageException("Failed to create JsonAccess instance", e);
        }
    }

    /*
     Log.d(LOG_TAG, " onPostExecute mode create");
                String uuid = sam.getUuid();
                Media m = sam.getMedia();
                Log.d(LOG_TAG, "Newly created video on server: " + uuid);
                Storage.getInstance().updateMediaStateCreated(m, uuid);
            } else if (mode == Storage.MODE_UPLOAD) {
                Log.d(LOG_TAG, " onPostExecute mode upload");
                Media m = sam.getMedia();
                boolean res = Storage.getInstance().updateMediaState(m, Media.MEDIA_STATUS_UPLOADED);
            } else if (mode == Storage.MODE_DOWNLOAD) {
                Log.d(LOG_TAG, " onPostExecute mode download");
                Media m = sam.getMedia();
                boolean res = Storage.getInstance().updateMediaState(m, Media.MEDIA_STATUS_DOWNLOADED);

     */

    public void uploadMedia(Media m) throws JsonAccessException {
        ja.uploadTrainingPhaseVideo(LocalStorage.getInstance().getCurrentClub(), m);
        Log.d(LOG_TAG, " upload seems to have worked with media: " + m);
        boolean res = Storage.getInstance().updateMediaState(m, Media.MEDIA_STATUS_UPLOADED);
    }

    public AsyncBundle createMedia(Media m) throws  JsonAccessException {
        String uuid = ja.createVideoOnServer(LocalStorage.getInstance().getCurrentClub(), m);
        Log.d(LOG_TAG, " creation seems to have work, uuid: " + uuid + "  and media: " + m);
        Log.d(LOG_TAG, "Newly created video on server: " + uuid);

        Storage.getInstance().updateMediaStateCreated(m, uuid);
        return new AsyncBundle(Storage.MODE_CREATE, new SimpleAsyncBundle(0, m, uuid), null,  null);
    }

    public void downloadMedia(Media m) throws JsonAccessException{
        Log.d(LOG_TAG, "doInBackground  mode: DOWNLOAD");
        if (m != null) {
            String videoUuid = m.getUuid();
            String file = LocalStorage.getInstance().getDownloadMediaDir() + "/" + videoUuid + JsonSettings.SERVER_VIDEO_SUFFIX;
            ja.downloadVideo(LocalStorage.getInstance().getCurrentClub(), file, videoUuid);
            LocalStorage.getInstance().replaceLocalWithDownloaded(m, file);
            Log.d(LOG_TAG, " Finished downloading video from server ");
            boolean res = Storage.getInstance().updateMediaState(m, Media.MEDIA_STATUS_DOWNLOADED);
        } else {
            Log.d(LOG_TAG, "Can't download null media");
        }
    }

    @Override
    protected AsyncBundle doInBackground(AsyncBundle... bundles) {
        AsyncBundle cbundle = bundles[0];
        SimpleAsyncBundle bundle = cbundle.getSimpleAsyncBundle();
        int mode = cbundle.getMode();
        Log.d(LOG_TAG, "doInBackground  mode: " + mode);
        Log.d(LOG_TAG, "doInBackground  modes: " + Storage.MODE_COMPOSITE + " | " + Storage.MODE_CREATE + " | " + Storage.MODE_UPLOAD + " | " + Storage.MODE_DOWNLOAD);
        Log.d(LOG_TAG, "doInBackground  mode: " + mode);
        Log.d(LOG_TAG, "doInBackground  club: " + LocalStorage.getInstance().getCurrentClub());

        CoachAppSession.getInstance().addDialoguser();

        if (mode == Storage.MODE_COMPOSITE) {
            Log.d(LOG_TAG, "doInBackground  mode: COMPOSITE");
            Log.d(LOG_TAG, " fetching JSON data in background");
            try {
                JsonAccess.CompositeBundle cb = ja.update(LocalStorage.getInstance().getCurrentClub());
                Log.d(LOG_TAG, " fetching JSON data in background" + cb);
                Log.d(LOG_TAG, " fetching JSON data in background" + cb.members.size());

                Storage.getInstance().updateDB(cb.members,
                        cb.teams,
/*                        cb.media,*/
                        cb.tps);
                return new AsyncBundle(Storage.MODE_COMPOSITE,
                        JsonAccessException.OK,
                        cbundle.listener,
                        cbundle.connListener);
            } catch (JsonAccessException e) {
                Log.d(LOG_TAG, "Failed getting Json data from server" + e.getMessage());
// TODO: Fix ... make it possible to report to user
//                ReportUser.warning(c, "Failed getting data from server");
                return new AsyncBundle(Storage.MODE_FAILURE, e.getMode(), cbundle.listener, cbundle.connListener);
            }

        } else if (mode == Storage.MODE_CREATE) {
            Log.d(LOG_TAG, "doInBackground  mode: CREATE");
            try {
                return createMedia(bundle.getMedia());
            } catch (JsonAccessException e) {
                // TODO: store errors in log?
                Log.e(LOG_TAG, " Could not create video on server" + e.getMessage());
                return new AsyncBundle(Storage.MODE_FAILURE, e.getMode(), cbundle.listener, cbundle.connListener);
            }
        } else if (mode == Storage.MODE_UPLOAD) {
            Log.d(LOG_TAG, "doInBackground  mode: UPLOAD");
            try {
                uploadMedia(bundle.getMedia());
                return new AsyncBundle(mode, new SimpleAsyncBundle(0, bundle.getMedia()),null, null);
            } catch (JsonAccessException e) {
                Log.e(LOG_TAG, " Failed uploading video to server: " + e.getMessage());
                Storage.getInstance().log("Upload failed", "Failed uploading video from server");
                return new AsyncBundle(Storage.MODE_FAILURE, e.getMode(), cbundle.listener, cbundle.connListener);
            }
        } else if (mode == Storage.MODE_DOWNLOAD) {
            Media m = bundle.getMedia();
            try {
                downloadMedia(m);
            } catch (JsonAccessException e) {
                // TODO: store errors in log?
                Log.e(LOG_TAG, " Failed downloading video from server; " + e.getMessage());
                e.printStackTrace();
                Storage.getInstance().log("Download failed", "Failed downloading video from server");
                return new AsyncBundle(Storage.MODE_FAILURE, e.getMode(), cbundle.listener, cbundle.connListener);
            }

        } else {
            Log.d(LOG_TAG, "doInBackground  mode: NULL");

        }
//        return new SimpleAsyncBundle(mode, 0, bundle.getMedia());
        return null;
    }

    protected void onPostExecute(AsyncBundle bundle) {
        Log.d(LOG_TAG, " onPostExecute " + bundle);


        if (bundle!=null) {
            Log.d(LOG_TAG, " onPostExecute " + bundle.getMode());
        }


        if (bundle != null) {
            int mode = bundle.getMode();
            Log.d(LOG_TAG, " onPostExecute " + mode);
            CompositeAsyncBundle cam = bundle.getCompositeAsyncBundle();
            SimpleAsyncBundle sam    = bundle.getSimpleAsyncBundle();
            /*
             *
             *  Store in DB
             *
             */
            if (mode == Storage.MODE_FAILURE) {
                Log.d(LOG_TAG, " onPostExecute, will inform about status: " + bundle.connectionStatus);
                if (bundle.connListener != null) {
                    bundle.connListener.onConnectionStatusUpdate(bundle.connectionStatus);
                }
                return;
            } else {
                if (bundle.connListener!=null) {
                    bundle.connListener.onConnectionStatusUpdate(JsonAccessException.OK);
                }
            }
            if (mode == Storage.MODE_COMPOSITE) {
                Log.d(LOG_TAG, " update storagelistener????");
                Log.d(LOG_TAG, " update storagelistener???? " + bundle.listener);
                if (bundle!=null && bundle.listener!=null) {
                    Log.d(LOG_TAG, " update storagelistener");
                    bundle.listener.onStorageUpdate();
                }
            } else if (mode == Storage.MODE_CREATE) {
                Log.d(LOG_TAG, " onPostExecute mode create");
                String uuid = sam.getUuid();
                Media m = sam.getMedia();
                Log.d(LOG_TAG, "Newly created video on server: " + uuid);
                Storage.getInstance().updateMediaStateCreated(m, uuid);
            } else if (mode == Storage.MODE_UPLOAD) {
                Log.d(LOG_TAG, " onPostExecute mode upload");
                Media m = sam.getMedia();
                boolean res = Storage.getInstance().updateMediaState(m, Media.MEDIA_STATUS_UPLOADED);
            } else if (mode == Storage.MODE_DOWNLOAD) {
                Log.d(LOG_TAG, " onPostExecute mode download");
                Media m = sam.getMedia();
                boolean res = Storage.getInstance().updateMediaState(m, Media.MEDIA_STATUS_DOWNLOADED);



            }


        } else {
            Log.d(LOG_TAG, "Bundle null, indicating server communication failure or similar");
        }
        // TODO: throw exception
    }

    /*    @Override
        protected void onPostExecute(SimpleAsyncBundle bundle) {
            Log.d(LOG_TAG, "onPostExecute");
        }
    */

    public static class SimpleAsyncBundle {
        private int status;
        private Media media;
        private String uuid;

        public SimpleAsyncBundle(int status, Media m) {
            this.status = status;
            this.media = m;
        }

        public SimpleAsyncBundle(int status, Media m, String uuid) {
            this.status = status;
            this.media = m;
            this.uuid = uuid;
        }

        public int getStatus() {
            return status;
        }

        public Media getMedia() {
            return media;
        }

        public String getUuid() {
            return uuid;
        }
    }

    public static class CompositeAsyncBundle {
        private List<Member> members;
        private List<Team> teams;
        private List<Media> media;
        private List<TrainingPhase> tps;


        public CompositeAsyncBundle(List<Member> members,
                                    List<Team> teams,
                                    List<Media> media,
                                    List<TrainingPhase> tps) {
            this.members = members;
            this.teams = teams;
            this.media = media;
            this.tps = tps;
        }

        public List<Member> getMembers() {
            return members;
        }

        public List<Media> getMedia() {
            return media;
        }

        public List<TrainingPhase> getTrainingPhases() {
            return tps;
        }

        public List<Team> getTeams() {
            return teams;
        }

    }

    public static class AsyncBundle {
        private int mode;
        private int connectionStatus;
        private SimpleAsyncBundle sam;
        private CompositeAsyncBundle cam;
        private StorageUpdateListener listener;
        private ConnectionStatusListener connListener;

        public AsyncBundle(int mode,
                           List<Member> members,
                           List<Team> teams,
                           List<Media> media,
                           List<TrainingPhase> tps,
                           StorageUpdateListener listener,
                           ConnectionStatusListener connListener) {
            this.cam = new CompositeAsyncBundle(members, teams, media, tps);
            this.mode = mode;
            this.sam = null;
            this.listener = listener;
            this.connListener = connListener;
        }

        public AsyncBundle(int mode,
                           SimpleAsyncBundle sam,
                           StorageUpdateListener listener,
                           ConnectionStatusListener connListener) {
            this.mode = mode;
            this.cam = null;
            this.sam = sam;
            this.listener = listener;
            this.connListener = connListener;
        }

        public AsyncBundle(int mode,
                           int connectionStatus,
                           StorageUpdateListener listener,
                           ConnectionStatusListener connListener) {
            this.connectionStatus = connectionStatus;
            this.mode = mode;
            this.cam = null;
            this.sam = null;
            this.listener = listener;
            this.connListener = connListener;
        }

        public int getMode() {
            return mode;
        }

        public CompositeAsyncBundle getCompositeAsyncBundle() {
            return cam;
        }

        public SimpleAsyncBundle getSimpleAsyncBundle() {
            return sam;
        }
    }


}
