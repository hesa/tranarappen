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


package com.sandklef.coachapp.adapters;

import android.app.Activity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.CheckedTextView;
import android.widget.TextView;

import coachassistant.sandklef.com.coachapp.R;
import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.CoachAppBase;
import com.sandklef.coachapp.model.Member;
import com.sandklef.coachapp.model.Team;
import com.sandklef.coachapp.storage.Storage;
import com.sandklef.coachapp.storage.StorageNoClubException;

import java.security.acl.LastOwnerException;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by hesa on 2016-06-12.
 */
public class CoachappExpandableList extends BaseExpandableListAdapter {

    private List<CoachAppBase> elements;
    public LayoutInflater inflater;
    public Activity activity;

    private final static String LOG_TAG = CoachappExpandableList.class.getSimpleName();

    public CoachappExpandableList(Activity activity, List<CoachAppBase> elements) {
        this.activity = activity;
        this.elements = elements;
        inflater      = activity.getLayoutInflater();

        Log.d(LOG_TAG, "Adding elements: " + elements.size());
    }


    @Override
    public Object getChild(int groupPosition, int childPosition) {
        Log.d(LOG_TAG, "getChild " + groupPosition + " " + childPosition);
        StringBuilder sb      = new StringBuilder();
        String        players = "";
        try {
            String teamUuid = Storage.getInstance().getTeamUuid(elements.get(groupPosition).getName());
            List<Member> members = Storage.getInstance().getMembersTeam(teamUuid);
            if (members.size()>0) {
                for (Member m : members) {
                    sb.append(m.getName() + "\n");
                }
                players = sb.toString();
            } else {
                players = CoachAppSession.getInstance().getString(R.string.no_players);
            }
        } catch (StorageNoClubException e) {
            e.printStackTrace();
        }

        return players;
        //return elements.get(groupPosition).getName();
    }

    @Override
    public long getChildId(int groupPosition, int childPosition) {
        return 0;
    }

    @Override
    public View getChildView(int groupPosition, final int childPosition,
                             boolean isLastChild, View convertView, ViewGroup parent) {
        final String children = (String) getChild(groupPosition, childPosition);
        TextView text = null;
        if (convertView == null) {
            convertView = inflater.inflate(R.layout.coachapp_list_item, null);
        }
        text = (TextView) convertView.findViewById(R.id.list_item);
        text.setText(children);
        /*
        convertView.setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View v) {
                Toast.makeText(activity, children,
                        Toast.LENGTH_SHORT).show();
            }
        });
        */
        return convertView;
    }


    @Override
    public int getChildrenCount(int groupPosition) {
        return 1;
    }

    @Override
    public Object getGroup(int groupPosition) {
        return elements.get(groupPosition);
    }

    @Override
    public int getGroupCount() {
        return elements.size();
    }

    @Override
    public void onGroupCollapsed(int groupPosition) {
        super.onGroupCollapsed(groupPosition);
    }

    @Override
    public void onGroupExpanded(int groupPosition) {
        super.onGroupExpanded(groupPosition);
    }

    @Override
    public long getGroupId(int groupPosition) {
        return 0;
    }

    @Override
    public View getGroupView(int groupPosition, boolean isExpanded,
                             View convertView, ViewGroup parent) {
        if (convertView == null) {
            convertView = inflater.inflate(R.layout.coachapp_list_group, null);
        }

        CoachAppBase element = (CoachAppBase) getGroup(groupPosition);
        ((CheckedTextView) convertView).setText(element.toString());
        ((CheckedTextView) convertView).setChecked(isExpanded);
        return convertView;
    }

    @Override
    public boolean hasStableIds() {
        return false;
    }

    @Override
    public boolean isChildSelectable(int groupPosition, int childPosition) {
        return false;
    }


}
