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

import android.content.Context;
import android.support.v4.view.ViewPager;
import android.util.AttributeSet;
import android.view.MotionEvent;

import com.sandklef.coachapp.misc.Log;

/**
 * Created by hesa on 2016-02-15.
 */
public class AdaptedSwipeViewPager extends ViewPager {
    private final static String LOG_TAG = AdaptedSwipeViewPager.class.getSimpleName();
    private double lastX;

    //private boolean swipeLocked;
    private int maxIndex;

    public AdaptedSwipeViewPager(Context context) {
        super(context);
        lastX=0.0;
        maxIndex = Integer.MAX_VALUE;
        Log.d(LOG_TAG, " constructor()");
    }

    public AdaptedSwipeViewPager(Context context, AttributeSet attrs) {
        super(context, attrs);
        maxIndex = Integer.MAX_VALUE;
        lastX=0.0;
        Log.d(LOG_TAG, " constructor()");
    }

    public void setPagingMax(int limit) {
        maxIndex = limit;
    }

    public boolean swipeLocked() {
        boolean result = getCurrentItem()<=maxIndex;
        Log.d(LOG_TAG, " " + result + "  <==== " + getCurrentItem() + " < " + maxIndex);
        return result;
    }

    private boolean goingLeft(MotionEvent event) {
        boolean left  = false;
        double  X      = event.getX(0);
        if (lastX!=0.0) {
            left = X > lastX;
        }
        lastX = X;
        return left;
    }

    public boolean allowSwipe(MotionEvent event) {
        return (!swipeLocked()) || goingLeft(event);
    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent event) {
        // Never allow swiping to switch between pages
        return false;
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        // Never allow swiping to switch between pages
        return false;
    }

/*
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        return allowSwipe(event) && super.onTouchEvent(event);
    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent event) {
        return allowSwipe(event)  && super.onInterceptTouchEvent(event);
    }

    @Override
    public boolean canScrollHorizontally(int direction) {
        return !swipeLocked() && super.canScrollHorizontally(direction);
    }

*/
}
