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
import android.content.Intent;
import android.content.res.Configuration;
import android.net.Uri;
import android.os.Bundle;
import android.provider.MediaStore;
import android.support.v7.app.ActionBar;
import android.support.v7.app.ActionBarActivity;
import android.support.v7.widget.Toolbar;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.GridView;
import android.widget.ImageButton;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.TextView;

import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.fragments.TrainingPhasesFragment;
import com.sandklef.coachapp.fragments.VideoCapture;
import com.sandklef.coachapp.json.JsonAccess;
import com.sandklef.coachapp.misc.CADateFormat;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.misc.ViewSetter;
import com.sandklef.coachapp.model.Media;
import com.sandklef.coachapp.model.Member;
import com.sandklef.coachapp.model.Team;
import com.sandklef.coachapp.model.TrainingPhase;
import com.sandklef.coachapp.report.ReportUser;
import com.sandklef.coachapp.storage.ConnectionStatusListener;
import com.sandklef.coachapp.storage.LocalStorage;
import com.sandklef.coachapp.storage.LocalStorageSync;
import com.sandklef.coachapp.storage.Storage;
import com.sandklef.coachapp.storage.StorageNoClubException;
import com.sandklef.coachapp.storage.StorageUpdateListener;

import java.io.File;
import java.net.Inet4Address;
import java.security.acl.LastOwnerException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import coachassistant.sandklef.com.coachapp.R;

public class MemberActivity extends ActionBarActivity
        implements AbsListView.OnItemClickListener, StorageUpdateListener, View.OnLongClickListener {

    private GridView list;
    private ArrayAdapter<String> adapter;
    private ArrayList<String> arrayList;

    private AbsListView mListView;
    private ListAdapter mAdapter;
    private final static String LOG_TAG = MemberActivity.class.getSimpleName();

    private List<Member> teamMembers;

    private Media instructionalVideo;


    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Log.d(LOG_TAG, "onCreate()");

        setContentView(R.layout.activity_member);

        Log.d(LOG_TAG, "0. club");
        if (CoachAppSession.getInstance()==null) {
            ActivitySwitcher.startLoginActivity(this);
        }
        CoachAppSession.getInstance().setupActivity(this);
        Log.d(LOG_TAG, "1. club: " + LocalStorage.getInstance().getCurrentTeam());

        String teamUuid = LocalStorage.getInstance().getCurrentTeam();
        teamMembers = Storage.getInstance().getMembersTeam(LocalStorage.getInstance().getCurrentTeam());
//getMembersTeamFromDB
        Log.d(LOG_TAG, "2. club: " + LocalStorage.getInstance().getCurrentTeam());

        Log.d(LOG_TAG, "Members in club " + teamUuid + ": ");
        for (Member m: teamMembers) {
            Log.d(LOG_TAG, " * " + m);
        }


        mAdapter = new ArrayAdapter<Member>(this,
                android.R.layout.simple_list_item_1,
                android.R.id.text1,
                teamMembers);


        Toolbar myToolbar = (Toolbar) findViewById(R.id.my_toolbar);
        setSupportActionBar(myToolbar);
        ActionBar ab = getSupportActionBar();

        ab.setDisplayHomeAsUpEnabled(true);
        ab.setTitle(getResources().getString(R.string.member_list_title));

        ViewSetter.setViewText(this,
                R.id.team_text,
                getResources().getString(R.string.team_column) + Storage.getInstance().getTeam(LocalStorage.getInstance().getCurrentTeam()));

        ViewSetter.setViewText(this,
                R.id.trainingphase_text,
                getResources().getString(R.string.trainingphase_column) +
                        Storage.getInstance().getTrainingPhase(LocalStorage.getInstance().getCurrentTrainingPhase()));
    }


    @Override
    protected void onResume() {
        super.onResume();
        Log.d(LOG_TAG, "onResume()");
        if (CoachAppSession.getInstance()==null) {
            ActivitySwitcher.startLoginActivity(this);
        }
        if (CoachAppSession.getInstance()==null) {
            ActivitySwitcher.startLoginActivity(this);
        }
        CoachAppSession.getInstance().setupActivity(this);
        LocalStorage.getInstance().setCurrentMember(null);

    }

    @Override
    protected void onStart() {
        super.onStart();

        if (CoachAppSession.getInstance() == null) {
            ActivitySwitcher.startLoginActivity(this);
        }
        CoachAppSession.getInstance().setupActivity(this);

        // Set the adapter
        mListView = (AbsListView) findViewById(R.id.member_list);
        ((AdapterView<ListAdapter>) mListView).setAdapter(mAdapter);

        // Set OnItemClickListener so we can be notified on item clicks
        mListView.setOnItemClickListener(this);

    }


    private Uri newFileUri() {
        File f = new File(CoachAppSession.getInstance().newFileName());
        return  Uri.fromFile(f);
    }




    @Override
    public void onBackPressed(){
        Log.d(LOG_TAG, "onBackPressed()");
        ActivitySwitcher.startTrainingPhaseActivity(this);
    }


    public void recordInstructionalVideo(View v) {
        Log.d(LOG_TAG, "recordInstructionalVideo()");
        Intent intent = new Intent(MediaStore.ACTION_VIDEO_CAPTURE);
        String file = CoachAppSession.getInstance().newFileName();

        Log.d(LOG_TAG, "  file: " + file);
        Uri uri = Uri.fromFile(new File(file));

        Log.d(LOG_TAG, "  creating intent");

        intent.putExtra(MediaStore.EXTRA_OUTPUT, uri);
/*
        intent.putExtra("android.intent.extra.durationLimit", 5);
        intent.putExtra(MediaStore.EXTRA_FINISH_ON_COMPLETION, true);

        intent.putExtra(MediaStore.EXTRA_DURATION_LIMIT, 5);
  */
        intent.putExtra(MediaStore.EXTRA_VIDEO_QUALITY, 1); // set the video image quality to high
        Log.d(LOG_TAG, "  starting activity");
        startActivityForResult(intent, VideoCapture.VIDEO_CAPTURE);
       // saveinstructionMedia(Uri.fromFile(new File(file)));
        Log.d(LOG_TAG, "  done");

    }




    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        Member m = teamMembers.get((int) id);
        Log.d(LOG_TAG, " member clicked: " + m.getUuid() + "  " + m);

        LocalStorage.getInstance().setCurrentMember(m.getUuid());

//        ActivitySwitcher.startRecording(this);
        String file = CoachAppSession.getInstance().newFileName();
        File f = new File(file);

        try {
//        VideoCapture vc = (VideoCapture) videoView;
// BRING BACK?            VideoCapture.getInstance().startRecordTP(file);
//            ActivitySwitcher.startMediaRecorderActivity(this, file);

            Bundle mBundle = new Bundle();
            mBundle.putString("file",file);

            Intent intent = new Intent(this, com.sandklef.coachapp.activities.MediaRecorderActivity.class);
            intent.putExtra("file",file);

            startActivityForResult(intent, 0);

        } catch (java.lang.RuntimeException e) {
            Log.d(LOG_TAG, "Recording failed....");
            new File(file).delete();
            ReportUser.Log(R.string.video_dload_failed, e.getMessage());
            return;
        }


    }


    private void saveMediaImpl(Uri uri, String member) {
        Log.d(LOG_TAG, "saveMediaImpl " + uri + " " + member);
        String club = LocalStorage.getInstance().getCurrentClub();
        String team = LocalStorage.getInstance().getCurrentTeam();
        String tp   = LocalStorage.getInstance().getCurrentTrainingPhase();

        Member mem = Storage.getInstance().getMemberUUid(member);
        String memberName = "";
        if (mem!=null) { memberName = mem.getName();}
        String teamName   = Storage.getInstance().getTeam(LocalStorage.getInstance().getCurrentTeam()).getName();
        String tpName     = Storage.getInstance().getTrainingPhase(LocalStorage.getInstance().getCurrentTrainingPhase()).getName();

        Media m = new Media("temp-uuid-"+System.currentTimeMillis(),
                "",
                club,
                uri.getPath(),
                Media.MEDIA_STATUS_NEW,
                System.currentTimeMillis(),
                team,
                tp,
                member);

        if (member!=null) {
            // TODO: get member name instaed of UUID
            Storage.getInstance().log("Recorded " + memberName,
                    "Recorded video:\n" +
                            "Team: " + teamName + "\n" +
                            "TraingingPhase: " + tpName +"\n" +
                            "Member: " + memberName + "\n" +
                            "Time: " + CADateFormat.getDateString(m.getDate())
            );
        } else {
            // TODO: get member name instaed of UUID
            Storage.getInstance().log("Recorded instructional video" ,
                    "Recorded video:\n" +
                            "Team: " + teamName + "\n" +
                            "TraingingPhase: " + tpName +"\n" +
                            "Member: " + memberName+ "\n" +
                            "Time: " + CADateFormat.getDateString(m.getDate())
            );
        }


        Log.d(LOG_TAG, "Calling store: Media:  File: " + uri.getPath());
        Storage.getInstance().saveMedia(m);
    }


    private void saveMedia(Uri uri) {
        Log.d(LOG_TAG, "SaveMedia()");
        String member = LocalStorage.getInstance().getCurrentMember();
        saveMediaImpl(uri, member);
    }

    private void saveinstructionMedia(Uri uri) {
        saveMediaImpl(uri, null);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.member_menu, menu);
     //   findInstructionalVideo(menu);

        CoachAppSession.getInstance().setupActivity(this, menu, R.id.member_sync);

        return true;
    }

    public boolean onOptionsItemSelected(MenuItem item) {
        Log.d(LOG_TAG, "  onOptionsItemSelected: " + item.getItemId());

        switch (item.getItemId()){
            case R.id.instructionalPlay:
                Log.d(LOG_TAG, "handle instrucionalitem");
                handleInstructionalVideo(null);
                break;
            case R.id.instructionalRecord:
                Log.d(LOG_TAG, "handle instrucionalitem");
                recordInstructionalVideo(null);
                break;
            case R.id.member_sync:
                com.sandklef.coachapp.misc.Log.d(LOG_TAG, "  sync");
                CoachAppSession.getInstance().syncAll();
                break;
            default:
                CoachAppSession.getInstance().goBackToActivity();
                break;
        }

        return true;
    }


    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        Log.d(LOG_TAG, "Video callback: " + requestCode + " " + resultCode + " " + data);
        Log.d(LOG_TAG, "Video callback: file:" + data.getStringExtra("file"));
        Log.d(LOG_TAG, "Video callback: file:" + data.getData());


        String fileName ;
        int cancelCause = data.getIntExtra("cancel-cause", MediaRecorderActivity.CANCEL_CAUSE_UNUSED);


        if (requestCode == VideoCapture.VIDEO_CAPTURE) {
            // instructional video
            Uri uri = data.getData();
            fileName = uri.getPath();
        } else {
            fileName = data.getStringExtra("file");
        }

        if (fileName==null) {
            Log.d(LOG_TAG, "Video callback: fileName null");
            return;
        }
        if (resultCode==Activity.RESULT_OK) {
            saveMedia(Uri.fromFile(new File(fileName)));
        } else {
            String club   = LocalStorage.getInstance().getCurrentClub();
            String team   = LocalStorage.getInstance().getCurrentTeam();
            String tp     = LocalStorage.getInstance().getCurrentTrainingPhase();
            String member = LocalStorage.getInstance().getCurrentMember();
            String memberName = Storage.getInstance().getMemberUUid(member).getName();
            String teamName   = Storage.getInstance().getTeam(LocalStorage.getInstance().getCurrentTeam()).getName();
            String tpName     = Storage.getInstance().getTrainingPhase(LocalStorage.getInstance().getCurrentTrainingPhase()).getName();

            String cause;
            if (cancelCause==MediaRecorderActivity.CANCEL_CAUSE_USER) {
                cause = CoachAppSession.getInstance().getString(R.string.media_rec_cancel_user);
            } else if (cancelCause==MediaRecorderActivity.CANCEL_CAUSE_EXCEPTION) {
                cause = CoachAppSession.getInstance().getString(R.string.media_rec_cancel_exception);
            } else if (cancelCause==MediaRecorderActivity.CANCEL_CAUSE_SCREEN_CHANGE) {
                cause = CoachAppSession.getInstance().getString(R.string.media_rec_cancel_screen);
            } else {
                cause = "unknown";
            }



            Storage.getInstance().log("Cancelled:" + memberName,
                    "Cancelled video recording:\n" +
                            "Cause: " + cause + "\n" +
                            "Team: " + teamName + "\n" +
                            "TraingingPhase: " + tpName +"\n" +
                            "Member: " + memberName
            );
        }
        Log.d(LOG_TAG, "Video callback <----");



    }



    public void watchInstructionalVideo(View v) {
        String tpUuid = LocalStorage.getInstance().getCurrentTrainingPhase();
        List<Media> tpMedia = Storage.getInstance().getMediaTrainingPhase(tpUuid);
        Media mediaToWatch = null;

        // TODO: decide a strategy for choosing between (possibly) many tp media
        for (Media m : tpMedia) {
            Log.d(LOG_TAG, " * choosing tp media: " + m.fileName());
            // this means we're choosing the last one
            mediaToWatch = m;
        }

        Log.d(LOG_TAG, "watch video: .... tp: " + tpUuid);
        if (mediaToWatch != null) {
            Log.d(LOG_TAG, "watch video: " + mediaToWatch.getUuid());
        } else {
            Log.d(LOG_TAG, "watch video: .... null");
        }

        if (mediaToWatch != null) {
            Log.d(LOG_TAG, "   video item: " + mediaToWatch.fileName() + ", " + mediaToWatch.getUuid() + ", " + mediaToWatch.getClubUuid() + ", ");
            Intent intent = new Intent(Intent.ACTION_VIEW);
            intent.setDataAndType(Uri.fromFile(new File(mediaToWatch.fileName())), "video/*");
            startActivity(intent);
        }
    }


    @Override
    public void onStorageUpdate() {
        Log.d(LOG_TAG, "onStorageUpdate()");

    }

    @Override
    public boolean onLongClick(View v) {
        Log.d(LOG_TAG, "onLongClick()");
        //recordInstructionalVideo(v);
        return true;
    }


    private Media getInstructionalMedia() {
        String tpUuid    = LocalStorage.getInstance().getCurrentTrainingPhase();
        TrainingPhase tp = Storage.getInstance().getTrainingPhase(tpUuid);
        Log.d(LOG_TAG, "findInstructionalVideo() tp:    " + tpUuid);
        Log.d(LOG_TAG, "findInstructionalVideo() video: " + tp.getVideoUuid());

        Media media= Storage.getInstance().getInstructionalMedia(tpUuid);
        return media;
    }


    public void handleInstructionalVideo(View v) {
        Log.d(LOG_TAG, "handleInstructionalVideo()");
        Media m = getInstructionalMedia();
        if (m==null || m.fileName()==null) {
            Log.d(LOG_TAG, "Nothing to show");
            ReportUser.inform(this, R.string.video_missing_sync);
        } else {
            Intent intent = new Intent(Intent.ACTION_VIEW);
            Log.d(LOG_TAG, "Will show: " + m.fileName());
            intent.setDataAndType(Uri.fromFile(new File(m.fileName())), "video/*");
            startActivity(intent);
        }
    }


    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);

        Log.d(LOG_TAG, "onConfigurationChanged   orientation change");

        if (newConfig.orientation == Configuration.ORIENTATION_LANDSCAPE) {
            CoachAppSession.getInstance().unsetSyncModeSoft();
        } else if (newConfig.orientation == Configuration.ORIENTATION_PORTRAIT){
            CoachAppSession.getInstance().unsetSyncModeSoft();
        }
    }

    @Override
    protected void onStop() {
        super.onStop();
        CoachAppSession.getInstance().unsetSyncMode();
        Log.d(LOG_TAG, "onStop()");
    }
    @Override
    protected void onDestroy() {
        super.onDestroy();
        Log.d(LOG_TAG, "onDestroy()");

    }

}
