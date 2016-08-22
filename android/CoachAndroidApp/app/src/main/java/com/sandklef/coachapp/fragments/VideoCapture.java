package com.sandklef.coachapp.fragments;


import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.hardware.Camera;
import android.media.CamcorderProfile;
import android.media.MediaRecorder;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.SystemClock;
import android.provider.MediaStore;
import android.util.AttributeSet;
import android.view.Surface;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.animation.Animation;

import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.activities.ActivitySwitcher;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.Media;
import com.sandklef.coachapp.report.ReportUser;
import com.sandklef.coachapp.storage.LocalMediaStorage;
import com.sandklef.coachapp.storage.LocalStorage;
import com.sandklef.coachapp.storage.Storage;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class VideoCapture extends SurfaceView implements SurfaceHolder.Callback {

    public static final int DEFAULT_TP_RECORDING_TIME    = 5; // secs to record a member
    public static final int DEFAULT_INSTR_RECORDING_TIME = 10; // secs to record an instruction

    private final static String LOG_TAG = VideoCapture.class.getSimpleName();
    private static final int CAPTURE_IMAGE_ACTIVITY_REQUEST_CODE = 100;
    private Uri fileUri;
    private SurfaceHolder mHolder;
    private Camera mCamera;
    private Context context;
    private List<Camera.Size> mSupportedPreviewSizes;

    private MediaRecorder mediaRecorder;
    private final int maxDurationInMs = 20000;
    private final long maxFileSizeInBytes = 500000;
    private final int videoFramesPerSecond = 20;

    public static final int VIDEO_CAPTURE = 101;

    private static VideoCapture vc;

    private enum CAMERA_MODE {
        VC_UNDEFINED,
        VC_OPEN,
        VC_CLOSED,
        VC_READY,
        VC_PREVIEW,
        VC_RECORD
    }

    private CAMERA_MODE cameraMode;
    private Camera.Size mPreviewSize;

    public VideoCapture(Context context, AttributeSet attrs) {
        super(context, attrs);
        // Install a SurfaceHolder.Callback so we get notified when the
        // underlying surface is created and destroyed.
        mHolder = getHolder();
        mHolder.addCallback(this);
        // deprecated setting, but required on Android versions prior to 3.0
        mHolder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);
        cameraMode = CAMERA_MODE.VC_UNDEFINED;
        this.context = context;
        Log.d(LOG_TAG, "VideoCapture(Context context, attrs)");
        vc = this;
    }

    public static VideoCapture getInstance() {
        return vc;
    }

    public void openCamera() {
        Log.d(LOG_TAG, "openCamere()  open camera()");
        if (mCamera != null) {
            return;
        }
        try {
            mCamera = Camera.open(Camera.CameraInfo.CAMERA_FACING_BACK);
            if (mCamera == null) {
                Log.d(LOG_TAG, "openCamere()  NULL, so open camera(0)");
                mCamera.open(Camera.CameraInfo.CAMERA_FACING_FRONT);
            }
            mSupportedPreviewSizes = mCamera.getParameters().getSupportedPreviewSizes();
            /*for (Camera.Size str : mSupportedPreviewSizes)
                Log.d(LOG_TAG, str.width + "/" + str.height);
            */
            Camera.Parameters params = mCamera.getParameters();
            params.setRecordingHint(true);
            mCamera.setParameters(params);
            Log.d(LOG_TAG, "openCamere()  camera: " + mCamera);
            return;
        } catch (RuntimeException re) {
            Log.d(LOG_TAG, "Failed to get hold of camera");
            re.printStackTrace();
        }
        ReportUser.warning(CoachAppSession.getInstance().getContext(),
                "Could not open Camera", "Failed to get hold the camera. ");
        CoachAppSession.getInstance().getCurrentActivity().finish();
        //ActivitySwitcher.startTrainingActivity(CoachAppSession.getInstance().getCurrentActivity());

    }

    public void startPreview() {
        openCamera();
        try {
            mCamera.setPreviewDisplay(mHolder);
            mCamera.startPreview();
            cameraMode = CAMERA_MODE.VC_PREVIEW;
        } catch (IOException e) {
            // ignore: tried to stop a non-existent preview
            Log.d(LOG_TAG, "startPreview: IOException:   " + e.getMessage());
        }
    }

    public void stopPreview() {
        Log.d(LOG_TAG, "stopPreview()");
        try {
            Log.d(LOG_TAG, " mCamera stopPreview()");
            mCamera.stopPreview();
            mCamera.release();
            mCamera = null;
            cameraMode = CAMERA_MODE.VC_CLOSED;
        } catch (Exception e) {
            // ignore: tried to stop a non-existent preview
        }
    }
    public void startRecordInstructional(String fileName) {
        RecordTaskSettings rts =
                new RecordTaskSettings(fileName,
                        LocalStorage.getInstance().getVideoRecordingTime(),
                        REC_TYPE_INSTR);
        new RecordTask().execute(rts);
    }

    public void startRecordTP(String fileName) {
        Log.d(LOG_TAG, "startRecordTP()");
        RecordTaskSettings rts =
                new RecordTaskSettings(fileName,
                        LocalStorage.getInstance().getVideoRecordingTime(),
                        REC_TYPE_TP);
        new RecordTask().execute(rts);
//        startRecordImpl(rts);

    }

    private static final int REC_TYPE_TP    = 0 ;
    private static final int REC_TYPE_INSTR = 1 ;

    private class RecordTaskSettings {
        String fileName;
        int    recTime;
        int    recType;

        private  RecordTaskSettings(String file, int rec, int type) {
            fileName = file;
            recTime  = rec;
            recType  = type;
        }
    }

    private class RecordTaskResult {
    }

    private class RecordTask extends AsyncTask<RecordTaskSettings, Void, RecordTaskResult> {

        @Override
        protected RecordTaskResult doInBackground(RecordTaskSettings... params) {
            Log.d(LOG_TAG, "RecordTask doInBackground()");
            RecordTaskSettings rts = params[0];
            int ret = startRecord(rts);

            //
            return new RecordTaskResult();
        }

        @Override
        protected void onPostExecute(RecordTaskResult rtr) {
            ReportUser.warning(CoachAppSession.getInstance().getCurrentActivity(),
                    "Video recorded", "Finished recording video");
        }

        private int startRecord(RecordTaskSettings rt) {
            Log.d(LOG_TAG, "RecordTaskResult startRecord()");
            return startRecordImpl(rt);
        }
    }


    private int startRecordImpl(RecordTaskSettings rt) {
        Log.d(LOG_TAG, "startRecordImpl()");
        String fileName = rt.fileName;
        int    recTime  = rt.recTime;

        Log.d(LOG_TAG, "startRecordImpl: file: " + fileName);
        Log.d(LOG_TAG, "startRecordImpl: time: " + recTime);


        int delay = 1;
    /*
    Open Camera - Use the Camera.open() to get an instance of the camera object.
    */
        // already done
        openCamera();

/* Connect Preview - Prepare a live camera image preview by connecting a SurfaceView to the camera using Camera.setPreviewDisplay().
Start Preview - Call Camera.startPreview() to begin displaying the live camera images.
*/
        // already done
        /*
Start Recording Video - The following steps must be completed in order to successfully record video:
Unlock the Camera - Unlock the camera for use by MediaRecorder by calling Camera.unlock().
*/
        mCamera.unlock();

        mediaRecorder = new MediaRecorder();
        /*
Configure MediaRecorder - Call in the following MediaRecorder methods in this order. For more information, see the MediaRecorder reference documentation.
setCamera() - Set the camera to be used for video capture, use your application's current instance of Camera.
setAudioSource() - Set the audio source, use MediaRecorder.AudioSource.CAMCORDER.
setVideoSource() - Set the video source, use MediaRecorder.VideoSource.CAMERA.
Set the video output format and encoding. For Android 2.2 (API Level 8) and higher, use the MediaRecorder.setProfile method, and get a profile instance using CamcorderProfile.get(). For versions of Android prior to 2.2, you must set the video output format and encoding parameters:
setOutputFormat() - Set the output format, specify the default setting or MediaRecorder.OutputFormat.MPEG_4.
setAudioEncoder() - Set the sound encoding type, specify the default setting or MediaRecorder.AudioEncoder.AMR_NB.
setVideoEncoder() - Set the video encoding type, specify the default setting or MediaRecorder.VideoEncoder.MPEG_4_SP.
setOutputFile() - Set the output file, use getOutputMediaFile(MEDIA_TYPE_VIDEO).toString() from the example method in the Saving Media Files section.
setPreviewDisplay() - Specify the SurfaceView preview layout element for your application. Use the same object you specified for Connect Preview.
Caution: You must call these MediaRecorder configuration methods in this order, otherwise your application will encounter errors and the recording will fail.
*/

        mediaRecorder.setCamera(mCamera);
        mediaRecorder.setAudioSource(MediaRecorder.AudioSource.CAMCORDER);

        // NEW
//        mediaRecorder.setProfile(CamcorderProfile.get(CamcorderProfile.QUALITY_HIGH));


        mediaRecorder.setVideoSource(MediaRecorder.VideoSource.CAMERA);
        mediaRecorder.setOutputFormat(MediaRecorder.OutputFormat.MPEG_4);
        mediaRecorder.setAudioEncoder(MediaRecorder.AudioEncoder.AMR_NB);
        mediaRecorder.setVideoEncoder(MediaRecorder.VideoEncoder.MPEG_4_SP);
        mediaRecorder.setOutputFile(fileName);
        //  mediaRecorder.setPreviewDisplay((Surface) mHolder);

        /*
Prepare MediaRecorder - Prepare the MediaRecorder with provided configuration settings by calling MediaRecorder.prepare().
Start MediaRecorder - Start recording video by calling MediaRecorder.start().
*/
        try {
            mediaRecorder.prepare();
        } catch (IOException e) {
            Log.d(LOG_TAG, "IOException in VideoCapture");
            e.printStackTrace();
            return -1;
        }

        /*
        Log.d(LOG_TAG, "sleeping secs: " + delay);
        try {
            Thread.sleep(delay*1000);
        } catch (Exception e) {
            e.getLocalizedMessage();
        }
*/

        Log.d(LOG_TAG, "start recording");
        mediaRecorder.start();

        Log.d(LOG_TAG, "record for secs: " + recTime);
        try {
            Thread.sleep(recTime*1000);
        } catch (Exception e) {
            e.getLocalizedMessage();
            return -2;
        }
        Log.d(LOG_TAG, "record for secs: " + recTime + " done");
//        SystemClock.sleep(recTime);

        Log.d(LOG_TAG, "stop recording");
        stopRecord();
        startPreview();

        return 0;
    }


    private void stopRecord() {
        Log.d(LOG_TAG, "stopRecord()");
        // Numbered comments from API description:
        //     http://developer.android.com/guide/topics/media/camera.html#capture-video

        // 5 Stop Recording Video
        // 5.a Stop MediaRecorder
        mediaRecorder.stop();
        // 5.b Reset MediaRecorder
        mediaRecorder.reset();
        // 5.c Release MediaRecorder
        mediaRecorder.release();
        // 5.d lock to make sure future sessions can use camera
        mCamera.lock();
        // 6 Stop the Preview
        mCamera.stopPreview();
        // 7 Release Camera
        mCamera.release();

        mCamera = null;
        cameraMode = CAMERA_MODE.VC_CLOSED;
    }


    private void stopCamera() {
        Log.d(LOG_TAG, "stopCamera()");
        if (cameraMode == CAMERA_MODE.VC_PREVIEW) {
            Log.d(LOG_TAG, "stopCamera()");
            stopPreview();
        } else if (cameraMode == CAMERA_MODE.VC_RECORD) {
            stopRecord();
        } else {
            if (mCamera != null) {
                mCamera.release();
                mCamera = null;
            }
        }
    }

    @Override
    public void surfaceCreated(SurfaceHolder holder) {
        Log.d(LOG_TAG, "surfaceCreated()  1");
//        Log.d(LOG_TAG, " mCamera Camera.open  (surfaceCreated)");
        //      cameraMode = CAMERA_MODE.VC_OPEN;


/*        try {
            Log.d(LOG_TAG, " mCamera start preview (surfaceCreated)");
            mCamera.setPreviewDisplay(holder);
            mCamera.startPreview();
        } catch (IOException e) {
            Log.d(LOG_TAG, "Error setting camera preview: " + e.getMessage());
        }
  */

    }

    @Override
    public void surfaceChanged(SurfaceHolder holder, int format, int w, int h) {

        if (mHolder.getSurface() == null) {
            // preview surface does not exist
            return;
        }
//        stopPreview();
        startPreview();
        setCameraDisplayOrientation();
    }


    @Override
    public void surfaceDestroyed(SurfaceHolder holder) {
        Log.d(LOG_TAG, " mCamera stopCamera (surfaceDestroyed)   mode: " + cameraMode);
        stopCamera();
    }

    public void stopRecording() {
        stopRecord();
    }


    public boolean startRecording_OLD(File f, int msecs) {

        Uri uri = Uri.fromFile(f);

        // create Intent to take a picture and return control to the calling application
        Intent intent = new Intent(MediaStore.ACTION_VIDEO_CAPTURE);

        Log.d(LOG_TAG, "  file: " + f.getParent() + " " + f + " " + uri);

        intent.putExtra(MediaStore.EXTRA_OUTPUT, uri);
        intent.putExtra("android.intent.extra.durationLimit", 5);
        intent.putExtra(MediaStore.EXTRA_FINISH_ON_COMPLETION, true);
        intent.putExtra(MediaStore.EXTRA_DURATION_LIMIT, 5);
        intent.putExtra(MediaStore.EXTRA_VIDEO_QUALITY, 1); // set the video image quality to high
        // start the image capture Intent
        //context.startActivity(intent);
        Activity activity = (Activity) context;
        activity.startActivityForResult(intent, VIDEO_CAPTURE);
        return true;
    }


    public void setCameraDisplayOrientation() {
        Log.d(LOG_TAG, "setCameraDisplayOrientation:  ");

        android.hardware.Camera.CameraInfo info =
                new android.hardware.Camera.CameraInfo();

        Log.d(LOG_TAG, "setCameraDisplayOrientation:  " + Camera.getNumberOfCameras());

        android.hardware.Camera.getCameraInfo(0, info);


        int rotation = ((Activity) context).getWindowManager().getDefaultDisplay().getRotation();

        int degrees = 0;

        Log.d(LOG_TAG, "setCameraDisplay:  " + rotation);
        switch (rotation) {
            case Surface.ROTATION_0:
                degrees = 0;
                break;
            case Surface.ROTATION_90:
                degrees = 90;
                break;
            case Surface.ROTATION_180:
                degrees = 180;
                break;
            case Surface.ROTATION_270:
                degrees = 270;
                break;
        }



        int result;
        if (info.facing == Camera.CameraInfo.CAMERA_FACING_FRONT) {
            result = (info.orientation + degrees) % 360;
            result = (360 - result) % 360;  // compensate the mirror
        } else {  // back-facing
            result = (info.orientation - degrees + 360) % 360;
        }
        Camera.Parameters parameters = mCamera.getParameters();
     //   parameters.setPreviewSize(mPreviewSize.width, mPreviewSize.height);

        Camera.Size bestSize = null;

        List<Camera.Size> sizeList = mCamera.getParameters().getSupportedPreviewSizes();
        bestSize = sizeList.get(0);

        for(int i = 1; i < sizeList.size(); i++){
            Log.d(LOG_TAG, "Preview size:"  + sizeList.get(i).width + " x " + sizeList.get(i).height);
            if((sizeList.get(i).width * sizeList.get(i).height) >
                    (bestSize.width * bestSize.height)){
                bestSize = sizeList.get(i);
            }
        }


        Log.d(LOG_TAG, "Setting Preview size: " + mPreviewSize.width + " " + mPreviewSize.height);
        mCamera.setParameters(parameters);
//        mCamera.setDisplayOrientation(result);
        degrees = CoachAppSession.getInstance().orientationDegrees();
        mCamera.setDisplayOrientation(degrees);
    }

    //@Override

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        final int width = resolveSize(getSuggestedMinimumWidth(), widthMeasureSpec) / 4;
        final int height = resolveSize(getSuggestedMinimumHeight(), heightMeasureSpec) / 4;
        openCamera();
    //    Log.d(LOG_TAG, "onMeassure()");
      //  Log.d(LOG_TAG, "onMeassure(): preview sizes" + mSupportedPreviewSizes);

        if (mSupportedPreviewSizes != null) {
        //    Log.d(LOG_TAG, "onMeassure(): looking for preview size");
            mPreviewSize = getOptimalPreviewSize(mSupportedPreviewSizes, width, height);
        } else {
          //  Log.d(LOG_TAG, "onMeassure(): could not find preview sizes....");
            CoachAppSession.getInstance().getCurrentActivity().finish();
            return;
        }

//        Log.d(LOG_TAG, "onMeassure():    " + mPreviewSize);
        if (mPreviewSize==null) {
  //          Log.d(LOG_TAG, "onMeassure(): could not find preview size: null");
            return;
        }

        float ratio;
        if (mPreviewSize.height >= mPreviewSize.width)
            ratio = (float) mPreviewSize.height / (float) mPreviewSize.width;
        else
            ratio = (float) mPreviewSize.width / (float) mPreviewSize.height;

        // One of these methods should be used, second method squishes preview slightly
        setMeasuredDimension(width, (int) (width * ratio));
//        setMeasuredDimension((int) (width * ratio), height);
    }


    private Camera.Size getOptimalPreviewSize(List<Camera.Size> sizes, int w, int h) {
        final double ASPECT_TOLERANCE = 0.1;
        double targetRatio = (double) h / w;

        if (sizes == null)
            return null;

        Camera.Size optimalSize = null;
        double minDiff = Double.MAX_VALUE;

        int targetHeight = h;

        for (Camera.Size size : sizes) {
            double ratio = (double) size.height / size.width;
            if (Math.abs(ratio - targetRatio) > ASPECT_TOLERANCE) {
               // Log.d(LOG_TAG, " continue");
                continue;
            }

            if (Math.abs(size.height - targetHeight) < minDiff) {
  //              Log.d(LOG_TAG, " if ...");
                optimalSize = size;
                minDiff = Math.abs(size.height - targetHeight);
            }
        }

        if (optimalSize == null) {
//            Log.d(LOG_TAG, " after ...");
            minDiff = Double.MAX_VALUE;
            for (Camera.Size size : sizes) {
                if (Math.abs(size.height - targetHeight) < minDiff) {
                    optimalSize = size;
                    minDiff = Math.abs(size.height - targetHeight);
                }
            }
        }

        /*
        int i=0;

        for (Camera.Size size : sizes) {
            Log.d(LOG_TAG, "Find via list: " + i + " " + sizes.size() +  " " + size.width + "x" + size.height) ;
            if (i++>(sizes.size()-3)) {
                Log.d(LOG_TAG, "Find via list: return: " + size.width + "x" + size.height) ;
                return size;
            }
        }
*/
        return optimalSize;
    }
}