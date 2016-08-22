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
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
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
import android.widget.ListView;
import android.widget.TextView;

import coachassistant.sandklef.com.coachapp.R;

import com.sandklef.coachapp.filters.MediaFilterEngine;
import com.sandklef.coachapp.filters.MediaMemberFilter;
import com.sandklef.coachapp.filters.MediaStatusNameFilter;
import com.sandklef.coachapp.filters.MemberFilter;
import com.sandklef.coachapp.filters.MemberFilterEngine;
import com.sandklef.coachapp.filters.MemberTeamFilter;
import com.sandklef.coachapp.misc.CADateFormat;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.Media;
import com.sandklef.coachapp.model.Member;
import com.sandklef.coachapp.storage.LocalStorage;
import com.sandklef.coachapp.storage.Storage;
import com.sandklef.coachapp.storage.StorageNoClubException;

import java.io.File;
import java.util.List;

public class MemberFragment extends Fragment implements AbsListView.OnItemClickListener {

    private MemberInteractionListener mListener;
    private final static String LOG_TAG = Member.class.getSimpleName();

    private List<Media> contextMedia;

    /**
     * The fragment's ListView/GridView.
     */
    private AbsListView mListView;

    /**
     * The Adapter which will be used to populate the ListView/GridView with
     * Views.
     */
    private ListAdapter mAdapter;

    // TODO: Rename and change types of parameters
    public static MemberFragment newInstance() {
        MemberFragment fragment = new MemberFragment();
        Bundle args = new Bundle();
/*
        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        */
        fragment.setArguments(args);
        return fragment;
    }

    /**
     * Mandatory empty constructor for the fragment manager to instantiate the
     * fragment (e.g. upon screen orientation changes).
     */
    public MemberFragment() {
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
/*
        if (getArguments() != null) {
            mParam1 = getArguments().getString(ARG_PARAM1);
            mParam2 = getArguments().getString(ARG_PARAM2);
        }
*/

        Log.d(LOG_TAG, " Looking for members using team: " + LocalStorage.getInstance().getCurrentTeam());
        try {
            List<Member> members =
                    MemberFilterEngine.apply(
                            Storage.getInstance().getMembers(),
                            MemberTeamFilter.newMemberTeamFilter(LocalStorage.getInstance().getCurrentTeam()));

            // TODO: Change Adapter to display your content
            mAdapter = new ArrayAdapter<Member>(getActivity(),
                    android.R.layout.simple_list_item_1, android.R.id.text1, members);
        } catch (StorageNoClubException e) {
            e.printStackTrace();
        }
    }


    public void updateMemberList() {
        Log.d(LOG_TAG, " updateMemberList() for team: " + LocalStorage.getInstance().getCurrentTeam());
        try {
            ArrayAdapter<Member> ma = ((ArrayAdapter) mAdapter);
            ma.clear();
            List<Member> members =
                    MemberFilterEngine.apply(
                            Storage.getInstance().getMembers(),
                            MemberTeamFilter.newMemberTeamFilter(LocalStorage.getInstance().getCurrentTeam()));

            ma.addAll(members);
            ma.notifyDataSetChanged();
        } catch (StorageNoClubException e) {
            e.printStackTrace();
        }
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_item, container, false);

        // Set the adapter
        mListView = (AbsListView) view.findViewById(android.R.id.list);
        ((AdapterView<ListAdapter>) mListView).setAdapter(mAdapter);

        // Set OnItemClickListener so we can be notified on item clicks
        mListView.setOnItemClickListener(this);

        TextView header = new TextView(getContext());
        header.setText("Members");
        ((ListView) mListView).addHeaderView(header);

        registerForContextMenu(mListView);

        return view;
    }

    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        try {
            mListener = (MemberInteractionListener) activity;
        } catch (ClassCastException e) {
            throw new ClassCastException(activity.toString()
                    + " must implement OnFragmentInteractionListener");
        }
    }

    @Override
    public void onDetach() {
        super.onDetach();
        mListener = null;
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        if (null != mListener) {

            try {
                // Notify the active callbacks interface (the activity, if the
                // fragment is attached to one) that an item has been selected.
                Member m = Storage.getInstance().getMembers().get((int) id);
                Log.d(LOG_TAG, " member clicked: " + m.getUuid() + "  " + m);
                mListener.onMemberInteraction(m);
            } catch (StorageNoClubException e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    public void onCreateContextMenu(ContextMenu menu, View v, ContextMenu.ContextMenuInfo menuInfo) {
        super.onCreateContextMenu(menu, v, menuInfo);
        Log.d(LOG_TAG, "  onCreateContextMenu()");

        AdapterView.AdapterContextMenuInfo info =
                (AdapterView.AdapterContextMenuInfo) menuInfo;
        String word = ((TextView) info.targetView).getText().toString();
        long id = info.id;

        try {
            menu.setHeaderTitle("Select");
            String memberUuid = Storage.getInstance().getMembers().get((int) id).getUuid();

            contextMedia = MediaFilterEngine.apply(
                    Storage.getInstance().getMedia(),
                    MediaMemberFilter.newMediaMemberFilter(memberUuid));

            for (Media media : contextMedia) {
                Log.d(LOG_TAG, " * " + media.getUuid());
                menu.add(0, v.getId(), 0, CADateFormat.getDateString(media.getDate()));
            }
        }catch (StorageNoClubException e) {
            e.printStackTrace();
        }
    }



    @Override
    public boolean onContextItemSelected(MenuItem item) {
        AdapterView.AdapterContextMenuInfo acmi = (AdapterView.AdapterContextMenuInfo) item.getMenuInfo();
        int position = acmi.position;

        Log.d(LOG_TAG, "  context menu item, position: " + position);
        Log.d(LOG_TAG, "  item:  " + item.getTitle().toString());

        Media m = Storage.getInstance().getMediaDate(item.getTitle().toString());

        if (m!=null) {
            Log.d(LOG_TAG, "   video item: " + m.fileName() + ", " + m.getUuid() + ", " + m.getClubUuid() + ", ");
            Intent intent = new Intent(Intent.ACTION_VIEW);
            intent.setDataAndType(Uri.fromFile(new File(m.fileName())), "video/*");
            startActivity(intent);
        }


        return true;
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
    public interface MemberInteractionListener {
        // TODO: Update argument type and name
        public void onMemberInteraction(Member m);
    }

}
