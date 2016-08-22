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

import android.annotation.SuppressLint;
import android.app.Activity;
import android.support.v7.app.ActionBar;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.os.Handler;
import android.view.MotionEvent;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;

import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.storage.LocalStorage;

import coachassistant.sandklef.com.coachapp.R;

public class SplashActivity extends AppCompatActivity {

    final static String LOG_TAG = com.sandklef.coachapp.activities.SplashActivity.class.getSimpleName();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Log.d(LOG_TAG, "Starting splash");

        final Activity me = this;

        requestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN,
                WindowManager.LayoutParams.FLAG_FULLSCREEN);
        setContentView(R.layout.activity_splash);


        if (getSupportActionBar()!=null) {
            getSupportActionBar().hide();
        }

       // getActionBar().hide();
        CoachAppSession.getInstance().init(this);

        int _delay=5;
        Log.d(LOG_TAG, "  local storage: " +LocalStorage.getInstance());
        if ( LocalStorage.getInstance()!=null) {
            _delay = LocalStorage.getInstance().getSplashDelay();
            LocalStorage.getInstance().setSplashDelay(5);
        }

        final int delay = _delay;
        Log.d(LOG_TAG, "  delay: " + delay);

        Thread timerThread = new Thread(){
            public void run(){
                try{
                    sleep(delay*1000);
                }catch(InterruptedException e){
                    e.printStackTrace();
                }finally{
                    ActivitySwitcher.startLoginActivity(me);
                }
            }
        };
        timerThread.start();


    }

}
