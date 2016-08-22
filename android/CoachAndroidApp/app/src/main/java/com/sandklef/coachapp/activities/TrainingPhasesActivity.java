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
import android.view.ContextMenu;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.TextView;

import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.fragments.VideoCapture;
import com.sandklef.coachapp.json.JsonSettings;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.Media;
import com.sandklef.coachapp.model.Team;
import com.sandklef.coachapp.model.TrainingPhase;
import com.sandklef.coachapp.storage.LocalStorage;
import com.sandklef.coachapp.storage.Storage;
import com.sandklef.coachapp.storage.StorageNoClubException;

import java.io.File;
import java.util.ArrayList;

import coachassistant.sandklef.com.coachapp.R;

public class TrainingPhasesActivity extends ActionBarActivity implements AbsListView.OnItemClickListener{

    private ListView list;
    private ArrayAdapter<String> adapter;
    private ArrayList<String> arrayList;

    private AbsListView mListView;
    private ListAdapter mAdapter;
    private final static String LOG_TAG = TrainingPhasesActivity.class.getSimpleName();

    private String currentTPId = null;

    @Override
    public void onBackPressed(){
        Log.d(LOG_TAG, "onBackPressed()");
        ActivitySwitcher.startTeamActivity(this);
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_training_phases);

        Log.d(LOG_TAG, "onCreate()");

        if (CoachAppSession.getInstance()==null) {
            ActivitySwitcher.startLoginActivity(this);
            Log.d(LOG_TAG, " onCreate clicked, current team: " + LocalStorage.getInstance().getCurrentTeam());
        }
        CoachAppSession.getInstance().setupActivity(this);

        Log.d(LOG_TAG, " onCreate clicked, current club: " + LocalStorage.getInstance().getCurrentClub());
        Log.d(LOG_TAG, " onCreate clicked, current team: " + LocalStorage.getInstance().getCurrentTeam());

        Log.d(LOG_TAG, "onCreate() storage:" + Storage.getInstance());


        try {
            mAdapter = new ArrayAdapter<TrainingPhase>(this,
                    android.R.layout.simple_list_item_1,
                    android.R.id.text1,
                    Storage.getInstance().getTrainingPhases() );
            Log.d(LOG_TAG, "onCreate()  adapter:" + mAdapter);

            Toolbar myToolbar = (Toolbar) findViewById(R.id.my_toolbar);
            setSupportActionBar(myToolbar);
            ActionBar ab = getSupportActionBar();
            ab.setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle(getResources().getString(R.string.trainingphase_list_title));

            LocalStorage.getInstance().setCurrentMember(null);
        } catch (StorageNoClubException e) {
            e.printStackTrace();
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        Log.d(LOG_TAG, "deg: ORIENTATION:  " + this.getWindowManager().getDefaultDisplay().getRotation());
        Log.d(LOG_TAG, "onResume()");
        if (CoachAppSession.getInstance()==null) {
            ActivitySwitcher.startLoginActivity(this);
        }
        CoachAppSession.getInstance().setupActivity(this);

        LocalStorage.getInstance().setCurrentTrainingPhase(null);
        LocalStorage.getInstance().setCurrentMember(null);

    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(CoachAppSession.getInstance().getTopMenuId(), menu);

        Log.d(LOG_TAG, " find menu: " + menu);

        CoachAppSession.getInstance().setupActivity(this, menu, R.id.topsync);

        return true;
    }


    @Override
    protected void onStart(){
        super.onStart();

        Log.d(LOG_TAG, "deg: ORIENTATION:  " + this.getWindowManager().getDefaultDisplay().getRotation());


        Log.d(LOG_TAG, "onStart()");
        if (CoachAppSession.getInstance() == null) {
            ActivitySwitcher.startLoginActivity(this);
        }
        CoachAppSession.getInstance().setupActivity(this);

        // Set the adapter
        mListView = (AbsListView) findViewById(R.id.tp_list);
        Log.d(LOG_TAG, "onStart() listview: " + mListView);
        ((AdapterView<ListAdapter>) mListView).setAdapter(mAdapter);

        Log.d(LOG_TAG, " onStart, current club: " + LocalStorage.getInstance().getCurrentClub());
        Log.d(LOG_TAG, " onStart, current team: " + LocalStorage.getInstance().getCurrentTeam());

        // Set OnItemClickListener so we can be notified on item clicks
        mListView.setOnItemClickListener(this);

       // registerForContextMenu(mListView);
    }



    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        return CoachAppSession.getInstance().handleTopMenu(item, null);
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        try {
            TrainingPhase tp = Storage.getInstance().getTrainingPhases().get((int)id);
            Log.d(LOG_TAG, " training phase clicked: " + tp.getUuid() + "  " + tp);
            Log.d(LOG_TAG, " training phase clicked, current club: " + LocalStorage.getInstance().getCurrentClub());
            Log.d(LOG_TAG, " training phase clicked, current team: " + LocalStorage.getInstance().getCurrentTeam());

            LocalStorage.getInstance().setCurrentTrainingPhase(tp.getUuid());
            ActivitySwitcher.startMemberActivity(this);
        } catch (StorageNoClubException e) {
            e.printStackTrace();
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
