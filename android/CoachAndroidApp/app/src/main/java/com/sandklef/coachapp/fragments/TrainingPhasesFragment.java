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

package com.sandklef.coachapp.fragments;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
import android.provider.MediaStore;
import android.support.v4.app.Fragment;
import android.view.ContextMenu;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListAdapter;
import android.widget.TextView;

import com.sandklef.coachapp.filters.MediaFilterEngine;
import com.sandklef.coachapp.filters.MediaMemberFilter;
import com.sandklef.coachapp.json.JsonSettings;
import com.sandklef.coachapp.misc.CADateFormat;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.Media;
import com.sandklef.coachapp.model.Member;
import com.sandklef.coachapp.model.TrainingPhase;
import com.sandklef.coachapp.storage.LocalStorage;
import com.sandklef.coachapp.storage.Storage;
import com.sandklef.coachapp.storage.StorageNoClubException;

import java.io.File;

import coachassistant.sandklef.com.coachapp.R;


public class TrainingPhasesFragment extends Fragment implements AbsListView.OnItemClickListener {


    private String currentTPId = null;

    private TrainingPhasesFragmentListener mListener;
    private final static String LOG_TAG = TrainingPhasesFragment.class.getSimpleName();

    public static final int VIDEO_CAPTURE = 102;

    /**
     * The fragment's ListView/GridView.
     */
    private AbsListView mListView;

    private Activity activity;

    /**
     * The Adapter which will be used to populate the ListView/GridView with
     * Views.
     */
    private ListAdapter mAdapter;

    public static TrainingPhasesFragment newInstance() {
        Log.d(LOG_TAG, LOG_TAG + "()");

        TrainingPhasesFragment fragment = new TrainingPhasesFragment();
        Bundle args = new Bundle();
/*        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        */
        fragment.setArguments(args);
        return fragment;
    }

    /**
     * Mandatory empty constructor for the fragment manager to instantiate the
     * fragment (e.g. upon screen orientation changes).
     */
    public TrainingPhasesFragment() {
        Log.d(LOG_TAG, LOG_TAG + "()");
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        try {
            mAdapter = new ArrayAdapter<TrainingPhase>(getActivity(),
                    android.R.layout.simple_list_item_1,
                    android.R.id.text1,
                    Storage.getInstance().getTrainingPhases());
            activity = getActivity();
        } catch (StorageNoClubException e) {
            e.printStackTrace();
        }
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_tp, container, false);


        // Set the adapter
        mListView = (AbsListView) view.findViewById(android.R.id.list);

        if (mListView != null) {
            ((AdapterView<ListAdapter>) mListView).setAdapter(mAdapter);

            // Set OnItemClickListener so we can be notified on item clicks
            mListView.setOnItemClickListener(this);
        }

        registerForContextMenu(mListView);

        return view;
    }


    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        try {
            mListener = (TrainingPhasesFragmentListener) activity;
        } catch (ClassCastException e) {
            throw new ClassCastException(activity.toString()
                    + " must implement TrainingPhasesFragmentListener");
        }
    }

    @Override
    public void onDetach() {
        super.onDetach();
        mListener = null;
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        try {
            if (null != mListener) {
                TrainingPhase tp = Storage.getInstance().getTrainingPhases().get((int) id);
                Log.d(LOG_TAG, " member clicked: " + tp.getUuid() + "  " + tp);

                // Notify the active callbacks interface (the activity, if the
                // fragment is attached to one) that an item has been selected.
                mListener.onTrainingphasesFragmentInteraction(tp);
            }
        } catch(StorageNoClubException e){
            e.printStackTrace();
        }
    }

    /**
     * The default content for this Fragment has a TextView that is shown when
     * the list is empty. If you would like to change the text, call this method
     * to supply the text it should use.
     */
    public void setEmptyText(CharSequence emptyText) {
        View emptyView = mListView.getEmptyView();

        if (emptyView instanceof TextView) {
            ((TextView) emptyView).setText(emptyText);
        }
    }

    /**
     * This interface must be implemented by activities that contain this
     * fragment to allow an interaction in this fragment to be communicated
     * to the activity and potentially other fragments contained in that
     * activity.
     * <p/>
     * See the Android Training lesson <a href=
     * "http://developer.android.com/training/basics/fragments/communicating.html"
     * >Communicating with Other Fragments</a> for more information.
     */
    public interface TrainingPhasesFragmentListener {
        // TODO: Update argument type and name
        public void onTrainingphasesFragmentInteraction(TrainingPhase tp);
    }


    @Override
    public void onCreateContextMenu(ContextMenu menu, View v, ContextMenu.ContextMenuInfo menuInfo) {
        super.onCreateContextMenu(menu, v, menuInfo);
        Log.d(LOG_TAG, "  onCreateContextMenu()");

        AdapterView.AdapterContextMenuInfo info =
                (AdapterView.AdapterContextMenuInfo) menuInfo;
        String word = ((TextView) info.targetView).getText().toString();
        long id = info.id;

        menu.setHeaderTitle("Select");
        try {
            currentTPId = Storage.getInstance().getTrainingPhases().get((int) id).getUuid();
            menu.add(0, v.getId(), 0, "Create instruction video");
        } catch (StorageNoClubException e) {
            e.printStackTrace();
        }

    }

    @Override
    public boolean onContextItemSelected(MenuItem item) {
        AdapterView.AdapterContextMenuInfo acmi = (AdapterView.AdapterContextMenuInfo) item.getMenuInfo();
        int position = acmi.position;

        String fileName = LocalStorage.getInstance().getNewMediaDir() + "/tp-" + currentTPId + JsonSettings.SERVER_VIDEO_SUFFIX;
        Media m = Media.newInstructionVideo(fileName, currentTPId);
        currentTPId = null;
        Uri uri = Uri.fromFile(new File(fileName));


        if (m!=null) {
            Log.d(LOG_TAG, "   instruction video item: " + fileName);
            Intent intent = new Intent(MediaStore.ACTION_VIDEO_CAPTURE);

            Log.d(LOG_TAG, "  file: " + fileName + " uri: " + uri);

            intent.putExtra(MediaStore.EXTRA_OUTPUT, uri);
            intent.putExtra("android.intent.extra.durationLimit", 5);
            intent.putExtra(MediaStore.EXTRA_FINISH_ON_COMPLETION, true);
            intent.putExtra(MediaStore.EXTRA_DURATION_LIMIT, 5);
            intent.putExtra(MediaStore.EXTRA_VIDEO_QUALITY, 1); // set the video image quality to high
            // start the image capture Intent
            //context.startActivity(intent);
//            activity.startActivityForResult(intent, com.sandklef.coachapp.fragments.VideoCapture.VIDEO_CAPTURE);
            ((Activity)getContext()).startActivityForResult(intent, VIDEO_CAPTURE);
        }
        Log.d(LOG_TAG, "  new instruction video wanted creation: " + fileName);

        return true;
    }

}
