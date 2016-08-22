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

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.view.ContextMenu;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListAdapter;
import android.widget.TextView;
import android.widget.Toast;

import com.sandklef.coachapp.filters.MediaFilterEngine;
import com.sandklef.coachapp.filters.MediaStatusNameFilter;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.Media;
import com.sandklef.coachapp.storage.LocalStorage;
import com.sandklef.coachapp.storage.LocalStorageSync;
import com.sandklef.coachapp.storage.Storage;
import com.sandklef.coachapp.storage.StorageNoClubException;

import java.io.File;
import java.util.List;

import coachassistant.sandklef.com.coachapp.R;


public class LocalMediaManager extends AppCompatActivity implements AdapterView.OnItemClickListener {

    private final static String LOG_TAG = LocalMediaManager.class.getSimpleName();

    /**
     * The fragment's ListView/GridView.
     */
    private AbsListView mListView;

    /**
     * The Adapter which will be used to populate the ListView/GridView with
     * Views.
     */
    private ListAdapter mAdapter;
    //private MediaListAdapter mAdapter;
    private Toolbar toolbar;
    private List<Media> media;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_local_media_manager);

        try {
            Storage.newInstance(this);

            media = Storage.getInstance().getMedia();

            mAdapter = new ArrayAdapter<Media>(this,
                    android.R.layout.simple_list_item_1, android.R.id.text1, media);


            //        public MediaListAdapter(Context context, int textViewResourceId, List<Media> media)
/*
        mAdapter = new MediaListAdapter(getApplicationContext(),
                R.layout.media_list, media);
*/
            // Set the adapter
            mListView = (AbsListView) findViewById(R.id.local_media_list);
            ((AdapterView<ListAdapter>) mListView).setAdapter(mAdapter);

            // Set OnItemClickListener so we can be notified on item clicks
            mListView.setOnItemClickListener(this);

            registerForContextMenu(mListView);

            toolbar = (Toolbar) findViewById(R.id.toolbar);
            if (toolbar != null) {
                setSupportActionBar(toolbar);
                getSupportActionBar().setDisplayHomeAsUpEnabled(true);
                getSupportActionBar().setTitle(getTitle());
            }
        } catch (StorageNoClubException e) {
            e.printStackTrace();
        }
    }

    private static final String CONTEXT_MENU_EDIT     = "Edit";
    private static final String CONTEXT_MENU_WATCH    = "Watch";
    private static final String CONTEXT_MENU_DELETE   = "Delete";
    private static final String CONTEXT_MENU_CREATE   = "Create ";
    private static final String CONTEXT_MENU_UPLOAD   = "Upload";
    private static final String CONTEXT_MENU_DOWNLOAD = "Download";

    @Override
    public void onCreateContextMenu(ContextMenu menu, View v, ContextMenu.ContextMenuInfo menuInfo) {
        super.onCreateContextMenu(menu, v, menuInfo);

        AdapterView.AdapterContextMenuInfo info =
                (AdapterView.AdapterContextMenuInfo) menuInfo;
        String selectedWord = ((TextView) info.targetView).getText().toString();
        long selectedWordId = info.id;

        Log.d(LOG_TAG, "  Add context for v: " + v);
        Log.d(LOG_TAG, "   * " + v.getId());
        Log.d(LOG_TAG, "   * " + selectedWordId);

        menu.setHeaderTitle("Select");
        menu.add(0, v.getId(), 0, CONTEXT_MENU_EDIT);
        menu.add(0, v.getId(), 0, CONTEXT_MENU_DELETE);
        menu.add(0, v.getId(), 0, CONTEXT_MENU_WATCH);
        if (media.get((int) selectedWordId).getStatus() == Media.MEDIA_STATUS_NEW) {
            menu.add(0, v.getId(), 0, CONTEXT_MENU_CREATE);
        } else if (media.get((int) selectedWordId).getStatus() == Media.MEDIA_STATUS_CREATED) {
            menu.add(0, v.getId(), 0, CONTEXT_MENU_UPLOAD);
        } else if (media.get((int) selectedWordId).getStatus() == Media.MEDIA_STATUS_UPLOADED) {
            menu.add(0, v.getId(), 0, CONTEXT_MENU_DOWNLOAD);
        } else
//        menu.add(0, v.getId(), 0, CONTEXT_MENU_UPLOAD);
            if (selectedWordId == 0) {
                menu.add(0, v.getId(), 0, "0:e elementet");
            }

/*
        Log.d(LOG_TAG, "You chose video: " +
                media.get((int) selectedWordId).getUuid());
*/
    }


    @Override
    public boolean onContextItemSelected(MenuItem item) {

        AdapterView.AdapterContextMenuInfo acmi = (AdapterView.AdapterContextMenuInfo) item.getMenuInfo();
        int position = acmi.position;

        Log.d(LOG_TAG, "onContextItemSelected: " + position);

        Media m = media.get(position);
/*
        JsonAccess.AsyncBundle bundle;
        JsonAccess jsend;
*/
        switch (item.getTitle().toString()) {
            case CONTEXT_MENU_EDIT:
                Toast.makeText(getApplicationContext(), "Choice: " + CONTEXT_MENU_EDIT, Toast.LENGTH_LONG).show();
                break;
            case CONTEXT_MENU_DELETE:
                Toast.makeText(getApplicationContext(), "Choice: " + CONTEXT_MENU_DELETE, Toast.LENGTH_LONG).show();
                break;
            case CONTEXT_MENU_WATCH:
                Toast.makeText(getApplicationContext(), "Choice: " + CONTEXT_MENU_WATCH, Toast.LENGTH_LONG).show();
                String file = m.fileName();

                Log.d(LOG_TAG, "   video item: " + m.fileName() + ", " + m.getUuid() + ", " + m.getClubUuid() + ", ");
                Intent intent = new Intent(Intent.ACTION_VIEW);
                intent.setDataAndType(Uri.fromFile(new File(m.fileName())), "video/*");
                startActivity(intent);
                break;
            case CONTEXT_MENU_CREATE:
                Log.d(LOG_TAG, "Create media (file): " + m.fileName());
                Storage.getInstance().createMediaOnServer(getApplicationContext(), m);
                break;
            case CONTEXT_MENU_UPLOAD:
                Log.d(LOG_TAG, "Upload media (file): " + media.get(position).fileName());
                Storage.getInstance().uploadMediaToServer(getApplicationContext(), m);
                break;
            case CONTEXT_MENU_DOWNLOAD:
                Log.d(LOG_TAG, "Download media (file): " + media.get(position).fileName());
                Storage.getInstance().downloadMediaFromServer(getApplicationContext(), m);
            default:
                return false;
        }
        return true;
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        Media m = media.get((int) id);
        Log.d(LOG_TAG, " media clicked: " + m.getUuid() + "  " + m);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.local_media_menu, menu);
        return true;
    }


    private void filteredUpdatedList(MediaStatusNameFilter mf) {
        try {
            Log.d(LOG_TAG, "  filter " + mf + "on deletable media " + media.size());
            media = MediaFilterEngine.apply(Storage.getInstance().getMedia(),
                    mf);
            Log.d(LOG_TAG, "  filter " + mf + "on deletable media " + media.size());

            ArrayAdapter<Media> ma = ((ArrayAdapter) mAdapter);
            ma.clear();
            ma.addAll(media);
            ma.notifyDataSetChanged();
        } catch (StorageNoClubException e) {
            e.printStackTrace();
        }
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        Log.d(LOG_TAG, "  onOptionsItemSelected: " + item.getItemId());


        switch (item.getItemId()) {
            case R.id.menu_training:
                ActivitySwitcher.startTrainingActivity(this);
                return true;
            case R.id.menu_log:
                ActivitySwitcher.startClubInfoActivity(this);
//                showLogMessageMode();
                return true;
            case R.id.menu_info:
                ActivitySwitcher.startClubInfoActivity(this);
                return true;
            case R.id.menu_deletable_media:
                filteredUpdatedList(MediaStatusNameFilter.newMediaFilterStatus(Media.MEDIA_STATUS_DELETABLE));
                return true;
            case R.id.menu_all_media:
                filteredUpdatedList(new MediaStatusNameFilter());
                return true;
            case R.id.menu_new_media:
                filteredUpdatedList(MediaStatusNameFilter.newMediaFilterStatus(Media.MEDIA_STATUS_NEW));
                return true;
            case R.id.menu_created_media:
                filteredUpdatedList(MediaStatusNameFilter.newMediaFilterStatus(Media.MEDIA_STATUS_CREATED));
                return true;
            case R.id.menu_uploaded_media:
                filteredUpdatedList(MediaStatusNameFilter.newMediaFilterStatus(Media.MEDIA_STATUS_UPLOADED));
                return true;
            default:
                Log.d(LOG_TAG, "  doin nada");
                return true;
        }
    }


    private class MediaListAdapter extends ArrayAdapter {

        private Context mContext;
        private int id;
        private List<Media> media;

        public MediaListAdapter(Context context, int textViewResourceId, List<Media> media) {
            super(context, textViewResourceId, media);
            mContext = context;
            id = textViewResourceId;
            this.media = media;
        }

        @Override
        public View getView(int position, View v, ViewGroup parent) {
            View mView = v;

            if (mView == null) {
                LayoutInflater vi = (LayoutInflater) mContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
                mView = vi.inflate(id, null);
            }

            TextView text = (TextView) mView.findViewById(R.id.media_list_text);

            if (media.size() > position && media.get(position) != null) {
                text.setText(media.get(position).toString());
            }
            return mView;
        }

        @Override
        public boolean hasStableIds() {
            return true;
        }

    }
}

