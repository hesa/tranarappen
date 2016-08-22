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

import android.content.Intent;
import android.content.SharedPreferences;
import android.content.res.Configuration;
import android.net.Uri;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListAdapter;
import android.widget.ListView;

import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.json.JsonAccessException;
import com.sandklef.coachapp.misc.CADateFormat;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.Club;
import com.sandklef.coachapp.model.Team;
import com.sandklef.coachapp.storage.ConnectionStatusListener;
import com.sandklef.coachapp.storage.LocalStorage;
import com.sandklef.coachapp.storage.Storage;
import com.sandklef.coachapp.storage.StorageNoClubException;
import com.sandklef.coachapp.storage.StorageUpdateListener;

import java.security.acl.LastOwnerException;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import coachassistant.sandklef.com.coachapp.R;

public class TeamsActivity
        extends AppCompatActivity
        implements AbsListView.OnItemClickListener, StorageUpdateListener {


    //    private ListView list;
    private ArrayAdapter<String> adapter;
    private ArrayList<String> arrayList;

    private ListView mListView;
    private ArrayAdapter mAdapter;

    private final static String LOG_TAG = TeamsActivity.class.getSimpleName();

    private int backPressCounter;

    //    private Club currentClub;

    @Override
    public void onBackPressed() {

        if (backPressCounter>0) {
            //TODO: Use this.finishAffinity(); instead?????
            Log.d(LOG_TAG, "onBackPressed(), will finish");
            Intent a = new Intent(Intent.ACTION_MAIN);
            a.addCategory(Intent.CATEGORY_HOME);
            a.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            startActivity(a);

        } else {
            Log.d(LOG_TAG, "onBackPressed(), ignoring back press");
        }
        backPressCounter++;
    }



    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_teams);

        Log.d(LOG_TAG, "onCreate, checking storage");
        if (CoachAppSession.getInstance() == null) {
            Log.d(LOG_TAG, "onCreate, checking storage, start login");
            ActivitySwitcher.startLoginActivity(this);
        }
        Log.d(LOG_TAG, "onCreate, checking storage, setup activity");
        CoachAppSession.getInstance().setupActivity(this);
        Log.d(LOG_TAG, "onCreate, checking storage, setup activity done");

        //        Log.d(LOG_TAG, "video length: " + LocalStorage.getInstance().getVideoRecordingTime());
//        ActivitySwitcher.printDb("TeamsActivity");

        Log.d(LOG_TAG, "orientation: " + CoachAppSession.getInstance().getScreenOrientation());
        Log.d(LOG_TAG, "Current club:   " + LocalStorage.getInstance().getCurrentClub() );

        Log.d(LOG_TAG, "Available clubs:");
        List<Club> clubs = CoachAppSession.getInstance().getClubs();
        if (clubs!=null) {
            for (Club c : clubs) {
                Log.d(LOG_TAG, " * " + c);
            }
        }

        try {

            Log.d(LOG_TAG, "onCreate()  storage:" + Storage.getInstance().getTeams().size() + " teams");

            mAdapter = new ArrayAdapter<Team>(this,
                    android.R.layout.simple_list_item_1,
                    android.R.id.text1,
                    Storage.getInstance().getTeams());
            Log.d(LOG_TAG, "onCreate()  adapter:" + mAdapter);


            Toolbar myToolbar = (Toolbar) findViewById(R.id.my_toolbar);
            setSupportActionBar(myToolbar);
            getSupportActionBar().setTitle(getResources().getString(R.string.team_list_header));



        } catch (StorageNoClubException e) {
            e.printStackTrace();
        }
        backPressCounter=0;
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
    protected void onStart() {
        super.onStart();
        // ATTENTION: This was auto-generated to implement the App Indexing API.
        // See https://g.co/AppIndexing/AndroidStudio for more information.

        long t = System.currentTimeMillis() ;
        //new Date().getDate();

        Log.d(LOG_TAG, "Date with CADate:  " + CADateFormat.getDateStringForServerUTC(t));
        Log.d(LOG_TAG, "Date with :        " + DateFormat.getDateInstance(DateFormat.LONG).format(t));
        Log.d(LOG_TAG, "Date with :        " + DateFormat.getDateTimeInstance(DateFormat.LONG,DateFormat.LONG).format(t));
        Log.d(LOG_TAG, "Date with :        " + DateFormat.getDateTimeInstance(DateFormat.LONG,DateFormat.LONG).format(t));



        backPressCounter=0;
        if (CoachAppSession.getInstance() == null) {
            ActivitySwitcher.startLoginActivity(this);
        }
        CoachAppSession.getInstance().setupActivity(this);

        LocalStorage.getInstance().setCurrentTeam(null);
        LocalStorage.getInstance().setCurrentTrainingPhase(null);
        LocalStorage.getInstance().setCurrentMember(null);

        Log.d(LOG_TAG, "onStart()");
        Log.d(LOG_TAG, "onStart() " + LocalStorage.getInstance().getCurrentClub());
        // Set the adapter
        mListView = (ListView) findViewById(R.id.team_list);

        Log.d(LOG_TAG, "onStart() listview: " + mListView);
        ((AdapterView<ListAdapter>) mListView).setAdapter(mAdapter);

        // Set OnItemClickListener so we can be notified on item clicks
        mListView.setOnItemClickListener(this);
    }


    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        try {
            Log.d(LOG_TAG, " team clicked: " + id);
            Team t = Storage.getInstance().getTeams().get((int) id);
            Log.d(LOG_TAG, " team clicked: " + t.getUuid() + "  " + t);

            LocalStorage.getInstance().setCurrentTeam(t.getUuid());
            Log.d(LOG_TAG, " team clicked, current club: " + LocalStorage.getInstance().getCurrentClub());
            Log.d(LOG_TAG, " team clicked, current team: " + LocalStorage.getInstance().getCurrentTeam());
            ActivitySwitcher.startTrainingPhaseActivity(this);
        } catch (StorageNoClubException e) {
            e.printStackTrace();
        }
    }


    public boolean onOptionsItemSelected(MenuItem item) {
        return CoachAppSession.getInstance().handleTopMenu(item, this);
    }


    @Override
    public void onStorageUpdate() {
        Log.d(LOG_TAG, "onStorageUpdate()");
        try {
            List<Team> teams = Storage.getInstance().getTeams();
            Log.d(LOG_TAG, "refresh()  teams: " + teams.size());
            for (Team t : Storage.getInstance().getTeams()) {
                Log.d(LOG_TAG, " * " + t.getName() + " " + t.getUuid());
            }

            mListView.setAdapter(null);
            mAdapter.clear();
            mAdapter = new ArrayAdapter<Team>(this,
                    android.R.layout.simple_list_item_1,
                    android.R.id.text1,
                    teams);
            mListView.setAdapter(mAdapter);
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
