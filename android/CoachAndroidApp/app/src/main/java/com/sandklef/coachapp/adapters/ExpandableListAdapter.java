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
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.CheckedTextView;
import android.widget.TextView;

import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.LogMessage;

import java.util.HashMap;
import java.util.List;

import coachassistant.sandklef.com.coachapp.R;

/**
 * Created by hesa on 2016-06-12.
 */
public class ExpandableListAdapter extends BaseExpandableListAdapter {

    private List<LogMessage> messages;
    public LayoutInflater inflater;
    public Activity activity;

    private final static String LOG_TAG = ExpandableListAdapter.class.getSimpleName();

    public ExpandableListAdapter(Activity activity, List<LogMessage> messages) {
        this.activity = activity;
        this.messages = messages;
        inflater      = activity.getLayoutInflater();

        Log.d(LOG_TAG, "Adding messages: " + messages.size());
    }


    @Override
    public Object getChild(int groupPosition, int childPosition) {
        return messages.get(groupPosition).getDetail();
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
            convertView = inflater.inflate(R.layout.log_list_item, null);
        }
        text = (TextView) convertView.findViewById(R.id.log_list_item);
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
        return messages.get(groupPosition);
    }

    @Override
    public int getGroupCount() {
        return messages.size();
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
            convertView = inflater.inflate(R.layout.log_list_group, null);
        }
        LogMessage msg= (LogMessage) getGroup(groupPosition);
        ((CheckedTextView) convertView).setText(msg.toString());
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
