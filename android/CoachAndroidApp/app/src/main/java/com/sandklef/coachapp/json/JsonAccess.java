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

package com.sandklef.coachapp.json;

import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.http.HttpAccessException;
import com.sandklef.coachapp.model.*;
import com.sandklef.coachapp.misc.*;
import com.sandklef.coachapp.report.ReportUser;
import com.sandklef.coachapp.storage.*;
import com.sandklef.coachapp.http.HttpAccess;

import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONException;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import android.content.Context;
import android.os.AsyncTask;

import coachassistant.sandklef.com.coachapp.R;


public class JsonAccess  {


    private final static String LOG_TAG = JsonAccess.class.getSimpleName();



    //    private Context context;
    private HttpAccess httpAccess;

    private String jsonString;

    public JsonAccess() throws JsonAccessException {
        try {
            this.httpAccess =
                    new HttpAccess(LocalStorage.getInstance().getServerUrl());
        } catch (HttpAccessException e) {
            throw new JsonAccessException("Could not access Http", e, JsonAccessException.NETWORK_ERROR);
        }
    }

    public class CompositeBundle {
        public List<Member> members;
        public List<Team> teams;
//        public List<Media> media;
        public List<TrainingPhase> tps;
    }


    public  List<Club> getUserClubs(String token) throws JsonAccessException {
        Log.d(LOG_TAG, "getUserClubs() ----> ");
        List<Club> clubs = new ArrayList<Club>();
        try {
            String jsonData = httpAccess.getUserInfo(token);

            Log.d(LOG_TAG, "data: " + jsonData);
            JSONObject json = new JSONObject(jsonData);

//            String clubJson = json.getJSONArray(JsonSettings.ITEMS_TAG);
            //          Log.d(LOG_TAG, " clubs (json):  " + clubJson);

            JSONArray instancesArray = json.getJSONArray(JsonSettings.INSTANCES_TAG);
            Log.d(LOG_TAG, " instances (array): " + instancesArray);

            for (int i = 0; i < instancesArray.length(); i++) {
                JSONObject jo = instancesArray.getJSONObject(i);
                String uuid = jo.getString(JsonSettings.ID_TAG);
                String name = jo.getString(JsonSettings.NAME_TAG);
                Log.d(LOG_TAG, " * " + uuid + " - " + name);
                clubs.add(new Club(uuid, name));
            }
        } catch (JSONException e) {
            Log.d(LOG_TAG, "getUserClubs() <---- throwing JsonAccessException");
            throw new JsonAccessException("Failed parsing JSON", e, JsonAccessException.ACCESS_ERROR);
        } catch (HttpAccessException e) {
            Log.d(LOG_TAG, "getUserClubs() <---- throwing JsonAccessException");
            throw new JsonAccessException(e, e.getMode());
        }

        Log.d(LOG_TAG, "getUserClubs() <---- ");
        return clubs;
    }

    public CompositeBundle update(String clubUri) throws JsonAccessException {
        CompositeBundle bundle = new CompositeBundle();
        try {
                /*
                 *
                 *  Get data from server
                 *
                 */
            Log.d(LOG_TAG, "Read entire coach server for club: " + clubUri);
            jsonString = readEntireCoachServer(clubUri);
            JSONObject json = new JSONObject(jsonString);

            Log.d(LOG_TAG, " json: " + jsonString);
                /*
                 *
                 *  Extract the data
                 *
                 */

            bundle.members = extractMembers(json);
            Log.d(LOG_TAG, "Members:  " + bundle.members.size() + "   " + bundle.members);
            for (Member m: bundle.members) {
                Log.d(LOG_TAG, " * " + m);
            }

            bundle.teams = extractTeams(json);
            Log.d(LOG_TAG, "Teams:  " + bundle.teams.size() + "   " + bundle.teams);

  /*          bundle.media = extractVideos(json);
            Log.d(LOG_TAG, "Media:  " + bundle.media.size() + "   " + bundle.media);
*/
            bundle.tps = extractTrainingPhases(json);
            Log.d(LOG_TAG, "TrainingPhase:  " + bundle.tps.size() + "   " + bundle.tps);
        } catch (JSONException e) {
            e.printStackTrace();
            throw new JsonAccessException("Failed receiving data from server", e, JsonAccessException.ACCESS_ERROR);
        }
        return bundle;
    }


    public String readEntireCoachServer(String clubUri) throws JsonAccessException {
        try {
            Log.d(LOG_TAG, "readEntireCoachServer(...):  token:" + LocalStorage.getInstance().getLatestUserToken());
            return httpAccess.readEntireCoachServer(LocalStorage.getInstance().getLatestUserToken(), clubUri);
        } catch (HttpAccessException e) {
            e.printStackTrace();
            throw new JsonAccessException("Failed reading composite view", e, e.getMode());
        }
    }


    public String getToken(String user, String password)  throws JsonAccessException {
        try {
            // 'Content-Type: application/json'
            String header   = "application/json";
            String data     = "{ \"user\": \"" + user + "\", \"password\": \"" + password + "\" }";
            String jsonData = httpAccess.getToken(header, data);

            Log.d(LOG_TAG, "Token: " + jsonData);
            JSONObject json = new JSONObject(jsonData);
            String token    = json.getString(JsonSettings.TOKEN_TAG);
            return new JSONObject(token).getString(JsonSettings.TOKEN_TAG);
        } catch (Exception e) {
            e.printStackTrace();
            throw new JsonAccessException("Failed getting token: ", e, JsonAccessException.ACCESS_ERROR);
        }
    }

    public static void printElement(JSONArray jArray) {
        for (int i = 0; i < jArray.length(); i++) {
            try {
                JSONObject jo = jArray.getJSONObject(i);
                String uuid = jo.getString(JsonSettings.UUID_TAG);
                String name = jo.getString(JsonSettings.NAME_TAG);
                System.out.println(name + " " + uuid);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public List<Member> extractMembers(JSONObject json) {
        List<Member> members = new ArrayList<Member>();
        try {
            JSONArray memberArray = json.getJSONArray(JsonSettings.MEMBERS_TAG);

            for (int i = 0; i < memberArray.length(); i++) {
                JSONObject jo = memberArray.getJSONObject(i);
                String uuid = jo.getString(JsonSettings.UUID_TAG);
                String name = jo.getString(JsonSettings.NAME_TAG);
//                String clubUuid = jo.getString(JsonSettings.CLUB_TAG);
                String teamUuid = "";
                if (jo.has(JsonSettings.TEAM_TAG)) {
                    teamUuid = jo.getString(JsonSettings.TEAM_TAG);
                } else {
                    Log.d(LOG_TAG, " NEW MEMEBER: no team");
                }
                Log.d(LOG_TAG, " NEW MEMEBER: club: " + LocalStorage.getInstance().getCurrentClub());
                Member m = new Member(uuid, name, LocalStorage.getInstance().getCurrentClub(), teamUuid);
                members.add(m);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return members;
    }

    public List<Team> extractTeams(JSONObject json) {
        List<Team> teams = new ArrayList<Team>();
        try {
            JSONArray memberArray = json.getJSONArray(JsonSettings.TEAMS_TAG);

            for (int i = 0; i < memberArray.length(); i++) {
                JSONObject jo = memberArray.getJSONObject(i);
                String uuid = jo.getString(JsonSettings.UUID_TAG);
                String name = jo.getString(JsonSettings.NAME_TAG);
                String clubUuid = LocalStorage.getInstance().getCurrentClub(); //jo.getString(JsonSettings.CLUB_TAG);
                Team m = new Team(uuid, name, clubUuid);
                teams.add(m);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return teams;
    }


    public List<TrainingPhase> extractTrainingPhases(JSONObject json) {
        List<TrainingPhase> tps = new ArrayList<TrainingPhase>();
        try {
            JSONArray tpArray = json.getJSONArray(JsonSettings.TRAINING_PHASES_TAG);

            for (int i = 0; i < tpArray.length(); i++) {
                JSONObject jo = tpArray.getJSONObject(i);
                String uuid = jo.getString(JsonSettings.UUID_TAG);
                String name = jo.getString(JsonSettings.NAME_TAG);
                String clubUuid = LocalStorage.getInstance().getCurrentClub();
                        //jo.getString(JsonSettings.CLUB_TAG);
                String videoUuid="";
                if (!jo.isNull(JsonSettings.VIDEO_TAG)) {
                    videoUuid = jo.getString(JsonSettings.VIDEO_TAG);
                }
                TrainingPhase m = new TrainingPhase(uuid, name, clubUuid, videoUuid);
                tps.add(m);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return tps;
    }


    private int jsonStatusToMediaStatus(String status) {
        Log.d(LOG_TAG, "jsonStatusToMediaStatus ( " + status + ")");
        switch (status) {
            case JsonSettings.SERVER_VIDEO_EMPTY_TAG:
                return Media.MEDIA_STATUS_UNDEFINED;
            case JsonSettings.SERVER_VIDEO_PROCESSING_TAG:
                return Media.MEDIA_STATUS_UPLOADED;
            case JsonSettings.SERVER_VIDEO_COMPLETE_TAG:
                return Media.MEDIA_STATUS_AVAILABLE;
            case JsonSettings.SERVER_VIDEO_FAILURE_TAG:
                return Media.MEDIA_STATUS_UPLOAD_FAILED;
        }
        return Media.MEDIA_STATUS_UPLOAD_FAILED;
    }


    public void downloadVideo(String clubUri, String file, String videoUuid) throws JsonAccessException {
        try {
            httpAccess.downloadVideo(clubUri, file, videoUuid);
        } catch (HttpAccessException e) {
            e.printStackTrace();
            throw new JsonAccessException("Failed downloading video: " + videoUuid, e.getMode());
        }
    }

    public void uploadTrainingPhaseVideo(String clubUri, Media m) throws JsonAccessException {

        Log.d(LOG_TAG, "   upload file: " + m.fileName());
        Log.d(LOG_TAG, "      uuid    : " + m.getUuid());
        try {
            httpAccess.uploadTrainingPhaseVideo(clubUri, m.getUuid(), m.fileName());
        } catch (IOException e) {
            Log.d(LOG_TAG, "Missing file ... deleting Media from db");
            ReportUser.Log(
                    CoachAppSession.getInstance().getCurrentActivity().getString(R.string.missing_file),
                    "Missing file: " + m.fileName());
            Storage.getInstance().removeMediaFromDb(m);
        } catch (HttpAccessException e) {
            if (e.getMode()==HttpAccessException.CONFLICT_ERROR) {
                Log.d(LOG_TAG, "Conflict uploading file ... deleting Media from db");
                ReportUser.Log(
                        CoachAppSession.getInstance().getCurrentActivity().getString(R.string.network_timed_out),
                        "Network timed out, upload of " + m.fileName() + " may have worked. We will retry." +
                " Perhaps increase the timeout in settings.");
                Storage.getInstance().removeMediaFromDb(m);
            } else if (e.getMode()==HttpAccessException.NETWORK_SLOW) {
                throw new JsonAccessException(CoachAppSession.getInstance().getCurrentActivity().getString(R.string.network_timed_out),
                        e, e.getMode());
            } else if (e.getMode()==HttpAccessException.CONFLICT_ERROR) {
                Log.d(LOG_TAG, "Conflict uploading file ... deleting Media from db");
                ReportUser.Log(
                        CoachAppSession.getInstance().getCurrentActivity().getString(R.string.network_timed_out),
                        "file conflict, most likely already uploaded: " + m.fileName() + ". You can discard this");
                Storage.getInstance().removeMediaFromDb(m);
            } else {
                e.printStackTrace();
                Log.d(LOG_TAG, "  exception: " + e.getMessage());
                throw new JsonAccessException("Failed to access http", e, e.getMode());
            }
        }
    }




    public String createVideoOnServer(String clubUri, Media m) throws JsonAccessException {
        Log.d(LOG_TAG, "createVideoOnServer()");
        String trainingPhaseUuid = m.getTrainingPhase();
        if (trainingPhaseUuid.length() < 3) {
            throw new JsonAccessException("No TrainingPhase id", JsonAccessException.ACCESS_ERROR);
        }


        String dateString = CADateFormat.getDateStringForServerUTC(m.getDate());
        String jsonData = "{ \"" + JsonSettings.TRAININGPHASE_TAG + "\": \"" + trainingPhaseUuid + "\" , " +
                "\"" + JsonSettings.RECORDED_DATE_TAG + "\": \"" + dateString + "\"" ;
        if ( m.getMember()!=null ) {
            if (!m.getMember().equals("")) {
                jsonData = jsonData + " , \"" + JsonSettings.MEMBER_TAG + "\": \"" + m.getMember() + "\"";
            }
        }
        jsonData = jsonData + "}";

        Log.d(LOG_TAG, "createVideoOnServer(), m.time (milliseconds): " + dateString);

        Log.d(LOG_TAG, "DEBUG upload: " + jsonData);


        String header = "application/json";

        try {
            String jsonString = httpAccess.createVideo(LocalStorage.getInstance().getLatestUserToken(), clubUri, jsonData, header);
            Log.d(LOG_TAG, jsonString);
            JSONObject jo = new JSONObject(jsonString);
            String uuid = jo.getString("uuid");
            String state = jo.getString("status");
            Log.d(LOG_TAG, "created (id):    " + uuid);
            Log.d(LOG_TAG, "created (state): " + state);
//            if (state.equals("empty")) {
            return uuid;
        } catch (JSONException e) {
            e.printStackTrace();
            throw new JsonAccessException("Failed requesting new uuid", e, JsonAccessException.ACCESS_ERROR);
        } catch (HttpAccessException e) {
            e.printStackTrace();
            throw new JsonAccessException("Failed requesting new uuid", e.getMode());
        }
    }



}