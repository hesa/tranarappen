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
import android.net.Uri;
import android.os.StrictMode;
/*
import android.provider.MediaStore;
import android.support.v4.app.FragmentActivity;

import com.sandklef.coachapp.fragments.Camera2Fragment;
*/
import com.sandklef.coachapp.filters.MediaFilterEngine;
import com.sandklef.coachapp.filters.MediaStatusNameFilter;
import com.sandklef.coachapp.fragments.MemberFragment;
import com.sandklef.coachapp.fragments.SimpleVideoFragment;
import com.sandklef.coachapp.fragments.TeamFragment;
import com.sandklef.coachapp.fragments.TopFragment;
import com.sandklef.coachapp.fragments.TrainingPhasesFragment;
//import com.sandklef.coachapp.fragments.UserFragment;
import com.sandklef.coachapp.fragments.VideoCapture;
import com.sandklef.coachapp.json.JsonAccess;
import com.sandklef.coachapp.misc.Log;
//import com.sandklef.coachapp.model.Club;
import com.sandklef.coachapp.model.Club;
import com.sandklef.coachapp.model.Media;
import com.sandklef.coachapp.model.Member;
import com.sandklef.coachapp.model.Team;
import com.sandklef.coachapp.model.TrainingPhase;
import com.sandklef.coachapp.storage.LocalStorage;
import com.sandklef.coachapp.storage.Storage;
import com.sandklef.coachapp.storage.StorageNoClubException;


import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentTransaction;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.view.Menu;
import android.view.MenuItem;

/*
import android.view.View;

import java.io.File;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
*/

import coachassistant.sandklef.com.coachapp.R;


public class TopActivity extends AppCompatActivity implements
        TopFragment.OnFragmentInteractionListener,
        TeamFragment.TeamFragmentListener,
        MemberFragment.MemberInteractionListener,
        TrainingPhasesFragment.TrainingPhasesFragmentListener,
        SimpleVideoFragment.OnSimpleVideoListener {

    private final static String LOG_TAG = TopActivity.class.getSimpleName();
    private static Storage storage;
    private TopFragment topFragment;
    private Toolbar     toolbar;
    private Club        currentClub;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // TEMP Settings  TODO: Make this flexible,
        // Should be received via the bundle instead
//       // Club c11 = new Club("e0b7098f-b7e1-4fe4-89bb-22c4d83f1141", "IK Nord");
       // currentClub = new Club("c04b2bdd-9fef-4123-b4cb-e122081e1868", "AHK");
       currentClub = new Club("e0b7098f-b7e1-4fe4-89bb-22c4d83f1141", "IK Nord");


        Storage.newInstance(getApplicationContext());
        LocalStorage.newInstance(getApplicationContext());
        LocalStorage.getInstance().setServerUrl("http://192.168.1.118:3000/0.0.0/");
        LocalStorage.getInstance().setCurrentClub(currentClub.getUuid());

        try {

        // TEST
        Log.d(LOG_TAG, "Media new:");
        for (Media media: MediaFilterEngine.apply(Storage.getInstance().getMedia(), MediaStatusNameFilter.newMediaFilterStatus(Media.MEDIA_STATUS_NEW))){
            Log.d(LOG_TAG, " * " + media.toString());
        }
        Log.d(LOG_TAG, "Media created:");
        for (Media media: MediaFilterEngine.apply(Storage.getInstance().getMedia(), MediaStatusNameFilter.newMediaFilterStatus(Media.MEDIA_STATUS_CREATED))){
            Log.d(LOG_TAG, " * " + media.toString());
        }
        Log.d(LOG_TAG, "Media uploaded:");
        for (Media media: MediaFilterEngine.apply(Storage.getInstance().getMedia(), MediaStatusNameFilter.newMediaFilterStatus(Media.MEDIA_STATUS_UPLOADED))){
            Log.d(LOG_TAG, " * " + media.toString());
        }
        Log.d(LOG_TAG, "Media downloaded:");
        for (Media media: MediaFilterEngine.apply(Storage.getInstance().getMedia(), MediaStatusNameFilter.newMediaFilterStatus(Media.MEDIA_STATUS_DOWNLOADED))){
            Log.d(LOG_TAG, " * " + media.toString());
        }


        StrictMode.ThreadPolicy policy = new StrictMode.ThreadPolicy.Builder().permitAll().build();
        StrictMode.setThreadPolicy(policy);

        String tag = "com.sandklef.coachapp.fragments.TopFragment";
        FragmentManager fm = getSupportFragmentManager();
        Fragment fragment = fm.findFragmentByTag(tag);

        if (fragment == null) {
            fragment = Fragment.instantiate(this, tag);
            FragmentTransaction ft = fm.beginTransaction();
            ft.add(android.R.id.content, fragment, tag);
            ft.commit();
        }

        topFragment = (TopFragment) fragment;

//        updateFromServer();

        toolbar = (Toolbar) findViewById(R.id.toolbar);
        if (toolbar != null) {
            setSupportActionBar(toolbar);
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setDisplayShowHomeEnabled(true);
            getSupportActionBar().setHomeButtonEnabled(true);
        }
        } catch (StorageNoClubException e) {
            e.printStackTrace();
        }


    }


    /*

    public void updateFromServer() {
        Log.d(LOG_TAG, "Initiate update from server");
        Storage.getInstance().update(getApplicationContext(), this, null);
    }
    */

    @Override
    public void onBackPressed() {
        int fragmentIndex = topFragment.getCurrentBottomFragmentIndex();
        Log.d(LOG_TAG, "onBackPressed() " + fragmentIndex);

        if (fragmentIndex != 0) {
            fragmentIndex--;
            topFragment.setBottomFragmentIndex(fragmentIndex);
        }
    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.top_menu, menu);
        return true;
    }


    public void onFragmentInteraction(Uri uri) {
        Log.d(LOG_TAG, "click ...kinda 1");
    }

    public void onTeamFragmentInteraction(Team t) {
        Log.d(LOG_TAG, "onTrainingphasesFragmentInteraction " + t);
        if (topFragment != null) {
            topFragment.onTeamFragmentInteraction(t);
        }
    }

    public void onTrainingphasesFragmentInteraction(TrainingPhase tp) {
        Log.d(LOG_TAG, "onTrainingphasesFragmentInteraction " + tp);
        if (topFragment != null) {
            topFragment.onTrainingphasesFragmentInteraction(tp);
        }
    }

    public void onMemberInteraction(Member m) {
        Log.d(LOG_TAG, " onMemberInteraction " + m);
        if (topFragment != null) {
            topFragment.onMemberInteraction(m);
        }
    }

    public void onMediaInteraction(long id) {
        Log.d(LOG_TAG, " onMediaInteraction " + id);
    }

    public void onSimpleVideoInteraction(Uri uri) {
        Log.d(LOG_TAG, " onSimpleVideoInteraction() " + uri);
    }


    private void saveMedia(Uri uri) {
        String club = LocalStorage.getInstance().getCurrentClub();
        String team = LocalStorage.getInstance().getCurrentTeam();
        String member = LocalStorage.getInstance().getCurrentMember();
        String tp = LocalStorage.getInstance().getCurrentTrainingPhase();
        // TODO: get member name instaed of UUID
        Storage.getInstance().log("Recorded " + member.toString(), "");
        Media m = new Media(null,
                "",
                club,
                uri.getPath(),
                Media.MEDIA_STATUS_NEW,
                System.currentTimeMillis(),
                team,
                tp,
                member);

        Log.d(LOG_TAG, "Calling storage to store Media.  File: " + uri.getPath());
        Storage.getInstance().saveMedia(m);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        Log.d(LOG_TAG, "Video callback: " + requestCode + " " + resultCode + " " + data);
        if (requestCode == TrainingPhasesFragment.VIDEO_CAPTURE) {
            Log.d(LOG_TAG, "instructional video found....");
        } else if (requestCode == VideoCapture.VIDEO_CAPTURE) {
            if (resultCode == Activity.RESULT_OK) {
                Log.d(LOG_TAG, "Video saved to: " +
                        data.getData());
                Log.d(LOG_TAG, "Saving media object...");
                saveMedia(data.getData());
            } else if (resultCode == Activity.RESULT_CANCELED) {
                Log.d(LOG_TAG, "Video recording cancelled.");
            } else {
                Log.d(LOG_TAG, "Failed to record video");
            }
        }
    }


    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle item selection
        Log.d(LOG_TAG, "onOptionsItemSelected()");
        switch (item.getItemId()) {
            /*case R.id.menu_media_manager:
                ActivitySwitcher.startLocalMediaManager(this);
                return true;
            */
            case android.R.id.home:
                getSupportFragmentManager().popBackStack();
//                NavUtils.navigateUpFromSameTask(this);
                return true;
            default:
                Log.d(LOG_TAG, "  doin nada");
                return true;
        }
    }

}



