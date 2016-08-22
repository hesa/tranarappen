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

import android.graphics.Color;
import android.os.Bundle;
import android.support.v7.app.ActionBar;
import android.support.v7.app.ActionBarActivity;
import android.support.v7.widget.LinearLayoutCompat;
import android.text.Layout;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.ExpandableListView;
import android.widget.LinearLayout;
import android.widget.ListAdapter;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.adapters.CoachappExpandableList;
import com.sandklef.coachapp.adapters.ExpandableListAdapter;
import com.sandklef.coachapp.filters.MediaFilterEngine;
import com.sandklef.coachapp.filters.MediaMemberFilter;
import com.sandklef.coachapp.filters.MediaStatusNameFilter;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.Club;
import com.sandklef.coachapp.model.CoachAppBase;
import com.sandklef.coachapp.model.Media;
import com.sandklef.coachapp.model.Team;
import com.sandklef.coachapp.storage.LocalStorage;
import com.sandklef.coachapp.storage.Storage;
import com.sandklef.coachapp.storage.StorageNoClubException;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import coachassistant.sandklef.com.coachapp.R;

public class ClubInfoActivity extends ActionBarActivity {

    private final static String LOG_TAG = ClubInfoActivity.class.getSimpleName();


    private ExpandableListView     mListView;
    private CoachappExpandableList mAdapter;



    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_club_info);
        mListView = (ExpandableListView) findViewById(R.id.club_info_team_list);
    }

    private void setTextViewText(int id, String text) {
        TextView tv = (TextView) findViewById(id);
        tv.setText(text);
    }

    public void onStart() {
        super.onStart();
        try {

            String clubNames="";
            List<Club> clubs = CoachAppSession.getInstance().getClubs();
            if (clubs!=null) {
                for (Club c : clubs) {
                    clubNames = clubNames  + "\n * "                         ;
                    clubNames = clubNames + c;
                }
            }
/*            setTextViewText(R.id.club_names,
                    getResources().getString(R.string.club_names) + ":  " + clubNames);
*/
            setTextViewText(R.id.club_name,
                    getResources().getString(R.string.club_name) + ":  " + LocalStorage.getInstance().getCurrentClubName());

            setTextViewText(R.id.teams_info,
                    getResources().getString(R.string.info_teams) + ":  " + Storage.getInstance().getTeams().size());

            setTextViewText(R.id.trainingphases_info,
                    getResources().getString(R.string.info_trainingphases) + ":  " + Storage.getInstance().getTrainingPhases().size());

            setTextViewText(R.id.members_info,
                    getResources().getString(R.string.info_members) + ":  " + Storage.getInstance().getMembers().size());

            setTextViewText(R.id.local_media_info,
                    getResources().getString(R.string.info_media_uploadable) + ":  " + Storage.getInstance().getLocalMedia().size());

            setTextViewText(R.id.server_media_info,
                    getResources().getString(R.string.info_media_downloaded) + ":  " +
                            MediaFilterEngine.apply(Storage.getInstance().getMedia(),
                                    MediaStatusNameFilter.newMediaFilterStatus(Media.MEDIA_STATUS_DOWNLOADED)).size());

            setTextViewText(R.id.deletable_media_info,
                    getResources().getString(R.string.info_media_deletable) + ":  " +
                            new File(LocalStorage.getInstance().getDeletableMediaDir()).listFiles().length);

            setTextViewText(R.id.instructional_media_info,
                    getResources().getString(R.string.info_media_instructional) + ":  " +
                            MediaFilterEngine.apply(Storage.getInstance().getMedia(),
                                    new MediaMemberFilter()).size());

            /*
            List<String> teams = new ArrayList<String>();
            for (Team t: Storage.getInstance().getTeams()) {
                Log.d(LOG_TAG, " " + t.getName());
                teams.add(t.getName()+ " (" + Storage.getInstance().getMembersTeam(t.getUuid()).size() + ")");
            }
            Log.d(LOG_TAG, "teams : " + teams.size());
*/


            Log.d(LOG_TAG, "Adding " + Storage.getInstance().getTeams().size() + " teams");
            mAdapter = new CoachappExpandableList(this, (List<CoachAppBase>) (List<? extends CoachAppBase>)Storage.getInstance().getTeams());
            mListView.setAdapter(mAdapter);
            registerForContextMenu(mListView);

//            ((AdapterView<ListAdapter>) mListView).setAdapter(mAdapter);

        } catch (StorageNoClubException e) {
            e.printStackTrace();
        }

    }


}


