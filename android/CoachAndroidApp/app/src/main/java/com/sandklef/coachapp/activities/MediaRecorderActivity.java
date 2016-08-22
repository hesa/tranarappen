/*
 * Copyright (C) 2013 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.sandklef.coachapp.activities;

import android.annotation.TargetApi;
import android.app.Activity;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.res.Configuration;
import android.hardware.Camera;
import android.hardware.SensorManager;
import android.media.CamcorderProfile;
import android.media.MediaRecorder;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.view.Menu;
import android.view.OrientationEventListener;
import android.view.Surface;
import android.view.SurfaceView;
import android.view.TextureView;
import android.view.View;
import android.widget.Button;
import android.widget.Toast;

import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.camera.CameraHelper;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.report.ReportUser;
import com.sandklef.coachapp.storage.LocalStorage;

import java.io.File;
import java.io.IOException;
import java.util.List;

import coachassistant.sandklef.com.coachapp.R;

/**
 *  This activity uses the camera/camcorder as the A/V source for the {@link android.media.MediaRecorder} API.
 *  A {@link android.view.TextureView} is used as the camera preview which limits the code to API 14+. This
 *  can be easily replaced with a {@link android.view.SurfaceView} to run on older devices.
 */
public class MediaRecorderActivity extends Activity {

    private Camera mCamera;
    private TextureView mPreview;
    private MediaRecorder mMediaRecorder;
    private File mOutputFile;

    private boolean cancelled ; // defaults to false
    private int     cancelCause = CANCEL_CAUSE_UNUSED;

    public static int CANCEL_CAUSE_UNUSED        = 0;
    public static int CANCEL_CAUSE_USER          = 1;
    public static int CANCEL_CAUSE_SCREEN_CHANGE = 2;
    public static int CANCEL_CAUSE_EXCEPTION     = 3 ;


    private boolean isRecording = false;
 //   private static final String TAG = "Recorder";
//    private Button captureButton;

    private final static String LOG_TAG = MediaRecorderActivity.class.getSimpleName();

    private String fileName ;

    OrientationEventListener mOrientationListener;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.media_recorder_activity);

        mPreview = (TextureView) findViewById(R.id.surface_view);
  //      captureButton = (Button) findViewById(R.id.button_capture);

        fileName = getIntent().getExtras().getString("file");

    }

    @Override
    protected void onStart() {
        super.onStart();
        startRecording();
    }


    public void startRecording() {
        onCaptureClick(null);
    }



    /**
     * The capture button controls all user interaction. When recording, the button click
     * stops recording, releases {@link android.media.MediaRecorder} and {@link android.hardware.Camera}. When not recording,
     * it prepares the {@link android.media.MediaRecorder} and starts recording.
     *
     * @param view the view generating the event.
     */
    public void onCaptureClick(View view) {
        if (isRecording) {
            // BEGIN_INCLUDE(stop_release_media_recorder)

            // stop recording and release camera
            try {
                mMediaRecorder.stop();  // stop the recording
            } catch (RuntimeException e) {
                // RuntimeException is thrown when stop() is called immediately after start().
                // In this case the output file is not properly constructed ans should be deleted.
                Log.d(LOG_TAG, "RuntimeException: stop() is called immediately after start()");
                //noinspection ResultOfMethodCallIgnored
                mOutputFile.delete();
            }
            releaseMediaRecorder(); // release the MediaRecorder object
            mCamera.lock();         // take camera access back from MediaRecorder

            // inform the user that recording has stopped
            setCaptureButtonText("Capture");
            isRecording = false;
            releaseCamera();

            Log.d(LOG_TAG, "finish here???");

            // END_INCLUDE(stop_release_media_recorder)

        } else {

            // BEGIN_INCLUDE(prepare_start_media_recorder)

            new MediaPrepareTask().execute(null, null, null);

            // END_INCLUDE(prepare_start_media_recorder)

        }
    }

    private void setCaptureButtonText(String title) {
    //    captureButton.setText(title);
    }

    @Override
    protected void onPause() {
        super.onPause();
        // if we are using MediaRecorder, release it first
        releaseMediaRecorder();
        // release the camera immediately on pause event
        releaseCamera();
    }

    private void releaseMediaRecorder(){
        if (mMediaRecorder != null) {
            // clear recorder configuration
            mMediaRecorder.reset();
            // release the recorder object
            mMediaRecorder.release();
            mMediaRecorder = null;
            // Lock camera for later use i.e taking it back from MediaRecorder.
            // MediaRecorder doesn't need it anymore and we will release it if the activity pauses.
            mCamera.lock();
        }
    }

    public static void setCameraDisplayOrientation(Activity activity,
                                                   int cameraId, android.hardware.Camera camera) {
        android.hardware.Camera.CameraInfo info =
                new android.hardware.Camera.CameraInfo();
        android.hardware.Camera.getCameraInfo(cameraId, info);
        int rotation = activity.getWindowManager().getDefaultDisplay()
                .getRotation();
        int degrees = 0;
        switch (rotation) {
            case Surface.ROTATION_0: degrees = 0; Log.d(LOG_TAG, "deg: 0");break;
            case Surface.ROTATION_90: degrees = 90; Log.d(LOG_TAG, "deg: 90");break;
            case Surface.ROTATION_180: degrees = 180; Log.d(LOG_TAG, "deg: 180");break;
            case Surface.ROTATION_270: degrees = 270; Log.d(LOG_TAG, "deg: 270");break;
        }

        int result;
        if (info.facing == Camera.CameraInfo.CAMERA_FACING_FRONT) {
            result = (info.orientation + degrees) % 360;
            result = (360 - result) % 360;  // compensate the mirror
        } else {  // back-facing
            result = (info.orientation - degrees + 360) % 360;
        }
        Log.d(LOG_TAG, "deg: result: " + result);
        camera.setDisplayOrientation(result);

    }

    private void releaseCamera(){
        if (mCamera != null){
            // release the camera for other applications
            mCamera.release();
            mCamera = null;
        }
    }

    @TargetApi(Build.VERSION_CODES.HONEYCOMB)
    private boolean prepareVideoRecorder(){

        // BEGIN_INCLUDE (configure_preview)
        mCamera = CameraHelper.getDefaultCameraInstance();


        int orientation = CoachAppSession.getInstance().getScreenOrientation();
        int orientationDegrees = CoachAppSession.getInstance().orientationDegrees(orientation);
        mCamera.setDisplayOrientation(orientationDegrees);

        Log.d(LOG_TAG, "deg: orientation: " + orientation + "  ===> hint: " + orientationDegrees);
     //   orientationDegrees = 90;



        // We need to make sure that our preview and recording video size are supported by the
        // camera. Query camera to find all the sizes and choose the optimal size given the
        // dimensions of our preview surface.
        Camera.Parameters parameters = mCamera.getParameters();


        List<Camera.Size> mSupportedPreviewSizes = parameters.getSupportedPreviewSizes();
        List<Camera.Size> mSupportedVideoSizes = parameters.getSupportedVideoSizes();
        Camera.Size optimalSize = CameraHelper.getOptimalVideoSize(mSupportedVideoSizes,
                mSupportedPreviewSizes, mPreview.getWidth(), mPreview.getHeight());

        // Use the same size for recording profile.
        CamcorderProfile profile = CamcorderProfile.get(CamcorderProfile.QUALITY_HIGH);
        profile.videoFrameWidth = optimalSize.width;
        profile.videoFrameHeight = optimalSize.height;

        // likewise for the camera object itself.
        parameters.setPreviewSize(profile.videoFrameWidth, profile.videoFrameHeight);
        mCamera.setParameters(parameters);
        try {
            // Requires API level 11+, For backward compatibility use {@link setPreviewDisplay}
            // with {@link SurfaceView}
            mCamera.setPreviewTexture(mPreview.getSurfaceTexture());
        } catch (IOException e) {
            Log.e(LOG_TAG, "Surface texture is unavailable or unsuitable" + e.getMessage());
            return false;
        }
        // END_INCLUDE (configure_preview)


        // BEGIN_INCLUDE (configure_media_recorder)
        mMediaRecorder = new MediaRecorder();

        // Step 1: Unlock and set camera to MediaRecorder
        mCamera.unlock();
        mMediaRecorder.setCamera(mCamera);

        // Step 2: Set sources
        mMediaRecorder.setAudioSource(MediaRecorder.AudioSource.DEFAULT );
        mMediaRecorder.setVideoSource(MediaRecorder.VideoSource.CAMERA);



//        mMediaRecorder.setOutputFormat(MediaRecorder.OutputFormat.THREE_GPP);

        // Step 3: Set a CamcorderProfile (requires API Level 8 or higher)
        mMediaRecorder.setProfile(profile);


        // Step 4: Set output file
        mOutputFile = CameraHelper.getOutputMediaFile(CameraHelper.MEDIA_TYPE_VIDEO, fileName);
        if (mOutputFile == null) {
            return false;
        }
        mMediaRecorder.setOutputFile(mOutputFile.getPath());
        // END_INCLUDE (configure_media_recorder)

        mMediaRecorder.setOrientationHint(orientationDegrees);


/*        SurfaceView surf = (SurfaceView)findViewById(R.id.surface_view);
       mMediaRecorder.setPreviewDisplay(surf.getHolder().getSurface());
  */      //mMediaRecorder.setOrientationHint(orientationDegrees);



        // Step 5: Prepare configured MediaRecorder
        try {
            mMediaRecorder.prepare();
        } catch (IllegalStateException e) {
            Log.d(LOG_TAG, "IllegalStateException preparing MediaRecorder: " + e.getMessage());
            releaseMediaRecorder();
            return false;
        } catch (IOException e) {
            Log.d(LOG_TAG, "IOException preparing MediaRecorder: " + e.getMessage());
            releaseMediaRecorder();
            return false;
        }
        return true;
    }

    public void onCancelClick(View view) {
        cancelled=true;
        cancelCause = CANCEL_CAUSE_USER;
        Log.d(LOG_TAG, "   cancelled, file: " + mOutputFile);
    }

    private void removeFile(File f) {
        f.delete();
    }


    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);

        Log.d(LOG_TAG, "onConfigurationChanged   orientation change");

        if (newConfig.orientation == Configuration.ORIENTATION_LANDSCAPE) {
            cancelled=true;
            cancelCause = CANCEL_CAUSE_SCREEN_CHANGE;

//            setRequestedOrientation (ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
        } else if (newConfig.orientation == Configuration.ORIENTATION_PORTRAIT){
            cancelled=true;
            cancelCause = CANCEL_CAUSE_SCREEN_CHANGE;
//            setRequestedOrientation (ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
        }

        /*
        // Checks the orientation of the screen for landscape and portrait and set portrait mode always
        */
    }

    /**
     * Asynchronous task for preparing the {@link android.media.MediaRecorder} since it's a long blocking
     * operation.
     */
    class MediaPrepareTask extends AsyncTask<Void, Void, Boolean> {

        @Override
        protected Boolean doInBackground(Void... voids) {
            // initialize video camera
            if (prepareVideoRecorder()) {
                // Camera is available and unlocked, MediaRecorder is prepared,
                // now you can start recording
                mMediaRecorder.start();

                isRecording = true;

                Log.d(LOG_TAG, "before sleep: " + LocalStorage.getInstance().getVideoRecordingTime());

                int loopsTodo = LocalStorage.getInstance().getVideoRecordingTime()*10;
                int loopCnt = 0;
                try {
                    while (loopCnt++ < loopsTodo) {
                        Thread.sleep(100);
                        if (cancelled) {
                            Log.d(LOG_TAG, "User cancelled recording");
                            releaseMediaRecorder();
                            return false;
                        }
                    }
                }catch(Exception e){
                    e.getLocalizedMessage();
                    removeFile(mOutputFile);
                    releaseMediaRecorder();
                    return false;
                }

                Log.d(LOG_TAG, "after sleep");

            } else {
                // prepare didn't work, release the camera
                releaseMediaRecorder();
                return false;
            }
            return true;
        }

        @Override
        protected void onPostExecute(Boolean result) {
            Intent intent=new Intent();
            intent.putExtra("file", mOutputFile.getAbsolutePath());
            intent.putExtra("cancel-cause", cancelCause);
            if (!result) {
                setResult(Activity.RESULT_CANCELED, intent);
                intent.putExtra("result", Activity.RESULT_CANCELED);
                MediaRecorderActivity.this.finish();
            }
            // inform the user that recording has started
            setCaptureButtonText("Stop");
            Log.d(LOG_TAG, "onPostExecute()");
            intent.putExtra("result", Activity.RESULT_OK);
            setResult(Activity.RESULT_OK, intent);
            finish();
        }
    }

}