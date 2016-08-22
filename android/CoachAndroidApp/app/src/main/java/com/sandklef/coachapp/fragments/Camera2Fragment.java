package com.sandklef.coachapp.fragments;

import android.net.Uri;
import android.support.v4.app.Fragment;
import android.view.View.OnClickListener;
import android.annotation.SuppressLint;
import android.content.DialogInterface;
import android.content.res.Configuration;
import android.media.MediaRecorder;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.View;
import android.view.ViewGroup;
//import android.widget.Button;

import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.Media;
import com.sandklef.coachapp.storage.LocalMediaStorage;
import com.sandklef.coachapp.storage.LocalStorage;
import com.sandklef.coachapp.storage.Storage;

import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Random;

import coachassistant.sandklef.com.coachapp.R;

public class Camera2Fragment extends Fragment implements SurfaceHolder.Callback {

    private final static String LOG_TAG = TopFragment.class.getSimpleName();

    //    private Button btnStartRec;
    MediaRecorder recorder;
    SurfaceHolder holder;
    boolean recording = false;
    private int randomNum;

    public static Camera2Fragment newInstance() {
        Bundle args = new Bundle();
        Camera2Fragment fragment = new Camera2Fragment();
        fragment.setArguments(args);
        return fragment;
    }

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

    }

    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);

    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

        View view001 = inflater.inflate(R.layout.capture_video, container, false);
        recorder = new MediaRecorder();
        initRecorder();
/*        btnStartRec = (Button) view001.findViewById(R.id.btnCaptureVideo);
        btnStartRec.setOnClickListener(this);
        */
        SurfaceView cameraView = (SurfaceView) view001.findViewById(R.id.surfaceCamera);
        holder = cameraView.getHolder();
        holder.addCallback(this);
        holder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);
/*
        cameraView.setClickable(true);
        cameraView.setOnClickListener(this);
 */

        return view001;
    }

    @SuppressLint({"SdCardPath", "NewApi"})
    private void initRecorder() {

//        Log.d(LOG_TAG, "initRecorder()  current member: " + Storage.getInstance().getCurrentMember());

        recorder.setVideoSource(MediaRecorder.VideoSource.CAMERA);

        recorder.setOutputFormat(MediaRecorder.OutputFormat.MPEG_4);
        recorder.setVideoEncoder(MediaRecorder.VideoEncoder.MPEG_4_SP);

        if (this.getResources().getConfiguration().orientation != Configuration.ORIENTATION_PORTRAIT) {
            recorder.setOrientationHint(90);//plays the video correctly
        } else {
            recorder.setOrientationHint(180);
        }

        recorder.setOutputFile("/sdcard/MediaAppVideos/" + randomNum + ".mp4");

    }

    private void prepareRecorder() {
        recorder.setPreviewDisplay(holder.getSurface());
        try {
            recorder.prepare();
        } catch (IllegalStateException e) {
            e.printStackTrace();
            //finish();
        } catch (IOException e) {
            e.printStackTrace();
            //finish();
        }
    }

    public void onStartStop(Media m) {
        recorder.setOutputFile(m.fileName());
        try {
            if (recording) {
                Log.d(LOG_TAG, "Stop recording....");
                recorder.stop();
                recording = false;

                // Let's initRecorder so we can record again
                //initRecorder();
                //prepareRecorder();
            } else {
                Log.d(LOG_TAG, "Start recording....");
                recording = true;
                recorder.start();
            }

        } catch (Exception e) {

        }


    }



    public void surfaceCreated(SurfaceHolder holder) {
        prepareRecorder();
    }


    public void surfaceChanged(SurfaceHolder holder, int format, int width,
                               int height) {
    }

    public void surfaceDestroyed(SurfaceHolder holder) {
        try {
            if (recording) {
                recorder.stop();
                recording = false;
            }
            recorder.release();
            // finish();
        } catch (Exception e) {

        }

    }
}