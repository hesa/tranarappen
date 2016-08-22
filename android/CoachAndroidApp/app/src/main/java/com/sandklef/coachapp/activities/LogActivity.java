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

import android.app.Dialog;
import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;
import android.support.v7.app.AlertDialog;
import android.support.v7.widget.Toolbar;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ExpandableListView;
import android.widget.ListAdapter;

import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.adapters.ExpandableListAdapter;
import com.sandklef.coachapp.misc.CADateFormat;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.Club;
import com.sandklef.coachapp.model.LogMessage;
import com.sandklef.coachapp.model.Member;
import com.sandklef.coachapp.storage.LocalStorage;
import com.sandklef.coachapp.storage.Storage;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import coachassistant.sandklef.com.coachapp.R;

public class LogActivity extends ActionBarActivity implements AbsListView.OnItemClickListener {

    private final static String LOG_TAG = LogMessage.class.getSimpleName();

    private ExpandableListAdapter mAdapter;
    private ExpandableListView    mListView;
    private List<LogMessage> logs;
    private Club             currentClub;
    private Toolbar          toolbar;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.activity_log);

        if (CoachAppSession.getInstance()==null) {
            ActivitySwitcher.startLoginActivity(this);
        }
        CoachAppSession.getInstance().setupActivity(this);

        logs = Storage.getInstance().getLogMessages(LocalStorage.getInstance().getLogMessageLimit());

        // Set the adapter
        mListView = (ExpandableListView) findViewById(R.id.local_log_list);
        mAdapter = new ExpandableListAdapter(this,logs);

        mListView.setAdapter(mAdapter);

        registerForContextMenu(mListView);

        toolbar = (Toolbar) findViewById(R.id.toolbar);
        if (toolbar != null) {
            setSupportActionBar(toolbar);
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle(getTitle());
        }
    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.log_media_menu, menu);

        mListView.setOnItemClickListener(this);

        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        Log.d(LOG_TAG, "  onOptionsItemSelected: " + item.getItemId());
        // Handle item selection
        switch (item.getItemId()) {
            case R.id.menu_training_mode:
                ActivitySwitcher.startTrainingActivity(this);
                return true;
            case R.id.log_menu_localmedia:
                ActivitySwitcher.startLocalMediaManager(this);
            default:
                Log.d(LOG_TAG, "  doin nada");
                return true;
        }
    }


    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        Log.d(LOG_TAG, " log clicked");
        Log.d(LOG_TAG, " detail:" + logs.get((int)id).getDetail());

        AlertDialog.Builder builder = new AlertDialog.Builder(CoachAppSession.getInstance().getContext());
        builder.setMessage(logs.get((int)id).toString())
                .setTitle(logs.get((int)id).getDetail());
        AlertDialog dialog = builder.create();
        dialog.setIcon(R.drawable.ic_sync_alert_black_24dp);
        dialog.show();

    }
}
