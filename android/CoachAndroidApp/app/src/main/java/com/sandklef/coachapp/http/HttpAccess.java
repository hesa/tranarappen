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

package com.sandklef.coachapp.http;

import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.misc.*;
import com.sandklef.coachapp.storage.LocalStorage;
import com.sandklef.coachapp.storage.Storage;

import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.StatusLine;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.InputStreamEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.entity.StringEntity;
import org.apache.http.util.EntityUtils;

import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URL;


import java.io.BufferedReader;
import java.io.BufferedInputStream;
import java.io.DataOutputStream;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import java.net.MalformedURLException;
import java.security.acl.LastOwnerException;

public class HttpAccess {

    private final static String LOG_TAG = HttpAccess.class.getSimpleName();

    private static int maxBufferSize = 1 * 1024 * 1024;

    private String urlBase;
    //  private String serverUrl;
    private String serverUrlShort;
//    private String clubUri;



    public HttpAccess(String baseUrl)throws HttpAccessException  {
        if (baseUrl==null ) {
            throw new HttpAccessException("NULL pointer passed to constructor (" + baseUrl + ")", HttpAccessException.NETWORK_ERROR);
        }
        this.urlBase   = baseUrl;
        //    this.clubUri   = "";
        //    this.serverUrl = urlBase + HttpSettings.CLUB_PATH + HttpSettings.PATH_SEPARATOR ;
    }


    private String sendHttpPost(String token,
                                StringEntity data,
                                String header,
                                String path)  throws HttpAccessException {
        HttpClient client = new DefaultHttpClient();
        String url = urlBase + path;
        HttpResponse resp = null;
        String response=null;
        Log.d(LOG_TAG, "sendHttpPost()  url:     " + urlBase);
        Log.d(LOG_TAG, "sendHttpPost()  path:    " + path);
        Log.d(LOG_TAG, "sendHttpPost()  url:     " + url);
        Log.d(LOG_TAG, "sendHttpPost()  club:    " + LocalStorage.getInstance().getCurrentClub());
        Log.d(LOG_TAG, "sendHttpPost()  header: " + header);
        try {
            Log.d(LOG_TAG, "sendHttpPost()  data:   " + data.getContent().toString());
        } catch (Exception e) {;}

        HttpPost httpPost = new HttpPost(url);
        try {
            httpPost.setHeader(HttpSettings.CONTENT_STATUS, header);
            httpPost.setEntity(data);
            if (token!=null) {
                httpPost.setHeader("X-Token",    token);
                httpPost.setHeader("X-instance", LocalStorage.getInstance().getCurrentClub());
                Log.d(LOG_TAG, "Using token: " + token);
            } else {
                Log.d(LOG_TAG, "Not using token");
            }
            resp = client.execute(httpPost);
            Log.d(LOG_TAG, resp + "  <==  sendHttpPost");
            Log.d(LOG_TAG, resp.getStatusLine().getStatusCode() + "  <==  sendHttpPost");
            if (HttpSettings.isResponseOk(resp.getStatusLine().getStatusCode())) {
                Log.d(LOG_TAG, " server response ok, returning data");
                response = EntityUtils.toString(resp.getEntity());
            } else {
                Log.d(LOG_TAG, "Server response OK but bad password");
                HttpAccessException e = new HttpAccessException("sendHttpPost failed", HttpAccessException.ACCESS_ERROR);
                Log.d(LOG_TAG, "Will throw: " + e.getMode());
                throw e;
            }
        } catch (IOException e) {
            throw new HttpAccessException("sendHttpPost failed", e, HttpAccessException.NETWORK_ERROR);
        }


        try {
            Log.d(LOG_TAG, " consuming entity");
            if (resp.getEntity()!=null) {
                resp.getEntity().consumeContent();
            }
        } catch (IOException ex) {
            throw new HttpAccessException("sendHttpPost failed", ex, HttpAccessException.NETWORK_ERROR);
        }

        return response;
    }



    private String getFromServer(String token, String path, String clubUri) throws HttpAccessException {
        StringBuilder builder = new StringBuilder();
        HttpClient client     = new DefaultHttpClient();


        String url            = urlBase + HttpSettings.API_VERSION + path;
        if (clubUri==null) {
            url               = urlBase + path;
        }

        HttpGet httpGet = new HttpGet(url);
        httpGet.setHeader("X-Token", token);
        if (clubUri!=null) {
            Log.d(LOG_TAG, "Server instance: " +  clubUri + " ");
            httpGet.addHeader("X-Instance", clubUri);
        } else {
            Log.d(LOG_TAG, "Server instance not set in header");
        }
        Log.d(LOG_TAG, "Server url:   " + url + "  in getFromServer()");
        Log.d(LOG_TAG, "Server token: " + token + " ");

        Log.d(LOG_TAG, "Headers:");
        for (Header h : httpGet.getAllHeaders() ){
            Log.d(LOG_TAG, " * header: " + h);
        }

        try {
            HttpResponse response = client.execute(httpGet);
            StatusLine statusLine = response.getStatusLine();
            int statusCode        = statusLine.getStatusCode();
            if (HttpSettings.isResponseOk(statusCode)) {
                HttpEntity entity = response.getEntity();
                InputStream content = entity.getContent();
                BufferedReader reader = new BufferedReader(new InputStreamReader(content));
                String line;
                while ((line = reader.readLine()) != null) {
                    builder.append(line);
                }
            } else {
                Log.d(LOG_TAG, "Failed to download file");
                Log.d(LOG_TAG, " * " + response.getStatusLine().getStatusCode());
                Log.d(LOG_TAG, " * " + response.getStatusLine());
                Log.d(LOG_TAG, "Failed to download file" + response);
                Log.w("Failed reading data from server", "Failed to download: " + path);
                throw new HttpAccessException("readEntireCoachServer failed", HttpAccessException.ACCESS_ERROR);
            }
        } catch (ClientProtocolException e) {
            throw new HttpAccessException("readEntireCoachServer failed", e, HttpAccessException.NETWORK_ERROR);
        } catch (IOException e) {
            throw new HttpAccessException("readEntireCoachServer failed", e, HttpAccessException.NETWORK_ERROR);
        }
        return builder.toString();
    }

    public String readEntireCoachServer(String token, String clubUri) throws HttpAccessException {
        return getFromServer(token, HttpSettings.PATH_SEPARATOR + HttpSettings.COMPOSITE_PATH, clubUri);
    }


    public String getUserInfo(String token) throws HttpAccessException {
        return getFromServer(token, HttpSettings.USER_PATH, null);
    }

    public String getToken(String header, String data) throws HttpAccessException{
        HttpResponse resp = null;
        // POST /api/login { "user": "my_email@example.com", "password": "my_password" }
        try {
            Log.d(LOG_TAG, "getToken(" + data + ", " + header + ")");
            return sendHttpPost(null, new StringEntity(data), header, HttpSettings.PATH_SEPARATOR + HttpSettings.LOGIN_PATH);
        } catch (UnsupportedEncodingException e) {
            throw new HttpAccessException("Failed encoding", e, HttpAccessException.NETWORK_ERROR);
        }
    }




    public String createVideo(String token, String clubUri, String data, String header) throws HttpAccessException {
        HttpResponse resp = null;
        //$ curl --data "{ \"trainingPhaseUuid\": \"$TRAINING_PHASE_UUID\" }" --header "Content-Type: application/json" --request POST localhost:3000/0.0.0/clubs/$CLUB_UUID/videos
        try {
            Log.d(LOG_TAG, "createVideo(" + data + ", " + header + ")");
            return sendHttpPost(token, new StringEntity(data), header, HttpSettings.API_VERSION +  HttpSettings.VIDEO_URL_PATH);
//            return sendHttpPost(token, new StringEntity(data), header, HttpSettings.API_VERSION + HttpSettings.CLUB_PATH + HttpSettings.PATH_SEPARATOR + clubUri + HttpSettings.PATH_SEPARATOR + HttpSettings.PATH_SEPARATOR + HttpSettings.VIDEO_URL_PATH);
        } catch (UnsupportedEncodingException e) {
            throw new HttpAccessException("Failed encoding", e, HttpAccessException.NETWORK_ERROR);
        }
    }



    public void uploadTrainingPhaseVideo(String clubUri, String videoUuid,
                                         String fileName) throws HttpAccessException, IOException {

        HttpURLConnection connection = null;
        DataOutputStream outputStream = null;
        DataInputStream inputStream = null;

        String pathToOurFile = fileName;
        String urlServer = urlBase + HttpSettings.API_VERSION + HttpSettings.PATH_SEPARATOR + HttpSettings.VIDEO_URL_PATH +
                HttpSettings.UUID_PATH + videoUuid + HttpSettings.PATH_SEPARATOR +
                HttpSettings.UPLOAD_PATH;

        Log.d(LOG_TAG, "Upload server url: " + urlServer);
        Log.d(LOG_TAG, "Upload file:       " + fileName);

        int bytesRead, bytesAvailable, bufferSize;
        byte[] buffer;

        FileInputStream fileInputStream = new FileInputStream(new File(pathToOurFile));

        URL url    = new URL(urlServer);
        connection = (HttpURLConnection) url.openConnection();

        Log.d(LOG_TAG, "connection: " + connection + "  uploading data to video: " + videoUuid);

        // Allow Inputs & Outputs
        connection.setDoInput(true);
        connection.setDoOutput(true);
        connection.setUseCaches(false);
        connection.setRequestMethod(HttpSettings.HTTP_POST);

        //
        int timeout = LocalStorage.getInstance().getnetworkTimeout();
        Log.d(LOG_TAG, "timeout: " + timeout);
        connection.setConnectTimeout(timeout);
        connection.setReadTimeout(timeout);

        connection.setRequestProperty("X-Token", LocalStorage.getInstance().getLatestUserToken());
        connection.addRequestProperty("X-Instance", LocalStorage.getInstance().getCurrentClub());
        connection.setRequestProperty("Connection", "close");

        Log.d(LOG_TAG, " upload propoerties: " + connection.getRequestProperties());

        outputStream = new DataOutputStream(connection.getOutputStream());
        bytesAvailable = fileInputStream.available();
        bufferSize = Math.min(bytesAvailable, maxBufferSize);
        buffer = new byte[bufferSize];

        // Read file
        bytesRead = fileInputStream.read(buffer, 0, bufferSize);

        while (bytesRead > 0) {
            Log.d(LOG_TAG, " writing data to stream  (" + bytesRead + " / " + bytesAvailable + ")");
            outputStream.write(buffer, 0, bufferSize);
            Log.d(LOG_TAG, " writing data to stream  -");
            bytesAvailable = fileInputStream.available();
            Log.d(LOG_TAG, " writing data to stream  -");
            bufferSize = Math.min(bytesAvailable, maxBufferSize);
            bytesRead = fileInputStream.read(buffer, 0, bufferSize);
        }
        outputStream.flush();
        outputStream.close();

        // Responses from the server (code and message)
        fileInputStream.close();

        int serverResponseCode = 0 ;

        long before = System.currentTimeMillis();
        try {
            Log.d(LOG_TAG, " ... writing done, getting response code");
            serverResponseCode = connection.getResponseCode();
            Log.d(LOG_TAG, " ... writing done, getting response message");
            String serverResponseMessage = connection.getResponseMessage();
            Log.d(LOG_TAG, "ServerCode:" + serverResponseCode);
            Log.d(LOG_TAG, "serverResponseMessage:" + serverResponseMessage);
        } catch (java.net.SocketTimeoutException e) {
            Log.d(LOG_TAG, " ... getting response code, got an exception, print stacktrace");
            e.printStackTrace();
            throw new HttpAccessException("Network timeout reached", e, HttpAccessException.NETWORK_SLOW);
        }
        long after = System.currentTimeMillis();
        long diff = after - before;
        Log.d(LOG_TAG, "diff: " + diff  + "ms   " + diff/1000 + "s" );

        if (diff > LocalStorage.getInstance().getnetworkTimeout()) {
            throw new HttpAccessException("Network timeout reached", HttpAccessException.NETWORK_SLOW);
        }

        if (serverResponseCode>=409) {
            throw new HttpAccessException("Failed uploading trainingphase video, access denied", HttpAccessException.CONFLICT_ERROR);
        }

        if (serverResponseCode<500 && serverResponseCode>=400) {
            throw new HttpAccessException("Failed uploading trainingphase video, access denied", HttpAccessException.ACCESS_ERROR);
        }

        try {
            Log.d(LOG_TAG, " ... disconnecting");
            connection.disconnect();
        } catch (Exception e) {
            Log.d(LOG_TAG, " ... disconnecting, got an exception, print stacktrace");
            e.printStackTrace();
        }
    }


    public void downloadVideo(String clubUri, String file, String videoUuid)  throws HttpAccessException{
        try {
            //$ GET http://localhost:3000/v0.0.0/clubs/<ID>/videos/uuid/<ID>/download
//            String file = LocalStorage.getInstance().getDownloadMediaDir() + "/" + videoUuid + SERVER_VIDEO_SUFFIX;
            int TIMEOUT_CONNECTION = 5000;//5sec
            int TIMEOUT_SOCKET = 30000;//30sec
            String urlServer = urlBase + HttpSettings.API_VERSION + HttpSettings.PATH_SEPARATOR + HttpSettings.VIDEO_URL_PATH +
                    HttpSettings.UUID_PATH  + videoUuid +
                    HttpSettings.PATH_SEPARATOR + HttpSettings.DOWNLOAD_PATH ;
/*            String urlServer = urlBase + HttpSettings.API_VERSION + HttpSettings.CLUB_PATH + HttpSettings.PATH_SEPARATOR + clubUri + HttpSettings.PATH_SEPARATOR + HttpSettings.VIDEO_URL_PATH +
                    HttpSettings.UUID_PATH  + videoUuid +
                    HttpSettings.PATH_SEPARATOR + HttpSettings.DOWNLOAD_PATH ;
*/
            URL url = new URL(urlServer);
            long startTime = System.currentTimeMillis();
            Log.i(LOG_TAG, "video download beginning: " + urlServer);

            //Open a connection to that URL.
            HttpURLConnection ucon = (HttpURLConnection) url.openConnection();

            ucon.setRequestProperty("X-Token", LocalStorage.getInstance().getLatestUserToken());
            ucon.addRequestProperty("X-Instance", LocalStorage.getInstance().getCurrentClub());
            ucon.setRequestMethod(HttpSettings.HTTP_GET);

            //this timeout affects how long it takes for the app to realize there's a connection problem
            ucon.setReadTimeout(TIMEOUT_CONNECTION);
            ucon.setConnectTimeout(TIMEOUT_SOCKET);

            Log.d(LOG_TAG, " and now to ...1");

            //Define InputStreams to read from the HttpURLConnection
            // uses 3KB download buffer
            InputStream is = ucon.getInputStream();
            Log.d(LOG_TAG, " and now to ...1");
            BufferedInputStream inStream = new BufferedInputStream(is, 1024 * 5);
            Log.d(LOG_TAG, " and now to ...2 to file: " + file);
            FileOutputStream outStream = new FileOutputStream(file);
            Log.d(LOG_TAG, " and now to ..31");
            byte[] buff = new byte[5 * 1024];

            System.err.println("For those about to ... file: " + file);
            //Read bytes (and store them) until there is nothing more to read(-1)
            int len;
            while ((len = inStream.read(buff)) != -1) {
                outStream.write(buff, 0, len);
            }

            Log.d(LOG_TAG, "response code: " +            ucon.getResponseCode());

            //clean up
            outStream.flush();
            outStream.close();
            inStream.close();

            Log.d(LOG_TAG, "For those about to ...");

            Log.d(LOG_TAG, "download completed in "
                    + ((System.currentTimeMillis() - startTime) / 1000)
                    + " sec");
            if (!HttpSettings.isResponseOk(ucon.getResponseCode())) {
                throw new HttpAccessException("Failed downloading video, response from server " + ucon.getResponseCode(), HttpAccessException.ACCESS_ERROR);
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            throw new HttpAccessException("Failed downloading video", e, HttpAccessException.FILE_NOT_FOUND);
        } catch (Exception e) {
            e.printStackTrace();
            throw new HttpAccessException("Failed downloading video", e, HttpAccessException.NETWORK_ERROR);
        }
    }
}
