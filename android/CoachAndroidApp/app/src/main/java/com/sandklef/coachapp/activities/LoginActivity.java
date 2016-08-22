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

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.annotation.TargetApi;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.support.v7.app.ActionBarActivity;
import android.app.LoaderManager.LoaderCallbacks;

import android.content.CursorLoader;
import android.content.Loader;
import android.database.Cursor;
import android.net.Uri;
import android.os.AsyncTask;

import android.os.Build;
import android.os.Bundle;
import android.provider.ContactsContract;
import android.support.v7.app.AlertDialog;
import android.text.TextUtils;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.inputmethod.EditorInfo;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;

import com.sandklef.coachapp.Auth.Authenticator;
import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.json.JsonAccess;
import com.sandklef.coachapp.json.JsonAccessException;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.Club;
import com.sandklef.coachapp.model.LocalUser;
import com.sandklef.coachapp.model.Team;
import com.sandklef.coachapp.model.TrainingPhase;
import com.sandklef.coachapp.report.ReportUser;
import com.sandklef.coachapp.storage.LocalStorage;
import com.sandklef.coachapp.storage.Storage;
import com.sandklef.coachapp.storage.StorageNoClubException;

import org.w3c.dom.Text;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import coachassistant.sandklef.com.coachapp.R;

public class LoginActivity extends ActionBarActivity implements LoaderCallbacks<Cursor> {


    private String loginFailureMessage = "";


    private final static String LOG_TAG = LoginActivity.class.getSimpleName();

    /**
     * Keep track of the login task to ensure we can cancel it if requested.
     */
    private UserLoginTask mAuthTask = null;

    // UI references.
    private AutoCompleteTextView mEmailView;
    private EditText mPasswordView;
    private View mProgressView;
    private View mLoginFormView;

    public Context context ;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Log.d(LOG_TAG, "onCreate()");

        setContentView(R.layout.activity_login);
        // Set up the login form.
        mEmailView = (AutoCompleteTextView) findViewById(R.id.email);
        populateAutoComplete();

        mPasswordView = (EditText) findViewById(R.id.password);
        mPasswordView.setOnEditorActionListener(new TextView.OnEditorActionListener() {
            @Override
            public boolean onEditorAction(TextView textView, int id, KeyEvent keyEvent) {
                if (id == R.id.login || id == EditorInfo.IME_NULL) {
                    attemptLogin();
                    return true;
                }
                return false;
            }
        });

        TextView more_info = (TextView) findViewById(R.id.more_info_link);
        more_info.setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View v) {
                String url = "https://www.tranarappen.se";
                Intent i = new Intent(Intent.ACTION_VIEW);
                i.setData(Uri.parse(url));
                startActivity(i);
            }
        });

        Button mEmailSignInButton = (Button) findViewById(R.id.email_sign_in_button);
        mEmailSignInButton.setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View view) {
                attemptLogin();
            }
        });

        mLoginFormView = findViewById(R.id.login_form);
        mProgressView = findViewById(R.id.login_progress);

        Log.d(LOG_TAG, "init : " + this);
        context = this;
        Log.d(LOG_TAG, "init : " + context);
    }


    @Override
    public void onResume() {
        super.onResume();
        CoachAppSession.getInstance().init(context);
    }


    @Override
    public void onStart() {
        super.onStart();
        CoachAppSession.getInstance().init(context);

        Log.d(LOG_TAG, "  saved token=? " + LocalStorage.getInstance().getLatestUserToken());
        final String token = LocalStorage.getInstance().getLatestUserToken();
        if (token != null && token.length()>8) {

            LocalStorage.getInstance().setConnectionStatus(CoachAppSession.COACHAPP_SESSION_STATUS_UNDEFINED);
            CoachAppSession.getInstance().verifyToken(token);
            int counter = 0;
            while (true) {
                int status = LocalStorage.getInstance().getConnectionStatus();
                if (status == CoachAppSession.COACHAPP_SESSION_STATUS_UNDEFINED) {
                    Log.d(LOG_TAG, "Logging in: undefined status: counter: " + counter++);
                    try {
                        Thread.sleep(200);
                    } catch (Exception e) {

                    }
                } else if (status == CoachAppSession.COACHAPP_SESSION_STATUS_INVALID_TOKEN) {
                    LocalStorage.getInstance().setLatestUserToken(null);
                    ReportUser.warning(this, R.string.token_invalid, R.string.token_invalid_detailed);
                    break;
                } else if (status == CoachAppSession.COACHAPP_SESSION_STATUS_NO_NETWORK) {
                    ActivitySwitcher.startTeamActivity(context);
                    ReportUser.warning(this, "No network", "No network");
                    break;
                } else if (status == CoachAppSession.COACHAPP_SESSION_STATUS_OK) {
                    ActivitySwitcher.startTeamActivity(context);
                    ReportUser.log("User logged in", "User logged in using token");
                    break;
                }
            }


        }

    }

    private void populateAutoComplete() {
        Log.d(LOG_TAG, "populateAutoComplete()");
        getLoaderManager().initLoader(0, null, this);
    }


    /**
     * Attempts to sign in or register the account specified by the login form.
     * If there are form errors (invalid email, missing fields, etc.), the
     * errors are presented and no actual login attempt is made.
     */
    private void attemptLogin() {
        Log.d(LOG_TAG, "attemptLogin()");
        if (mAuthTask != null) {
            return;
        }

        // Reset errors.
        mEmailView.setError(null);
        mPasswordView.setError(null);

        // Store values at the time of the login attempt.
        String email = mEmailView.getText().toString();
        String password = mPasswordView.getText().toString();

        boolean cancel = false;
        View focusView = null;

        // Check for a valid password, if the user entered one.
        if (!TextUtils.isEmpty(password) && !isPasswordValid(password)) {
            mPasswordView.setError(getString(R.string.error_invalid_password));
            focusView = mPasswordView;
            cancel = true;
        }

        // Check for a valid email address.
        if (TextUtils.isEmpty(email)) {
            mEmailView.setError(getString(R.string.error_field_required));
            focusView = mEmailView;
            cancel = true;
        } else if (!isEmailValid(email)) {
            mEmailView.setError(getString(R.string.error_invalid_email));
            focusView = mEmailView;
            cancel = true;
        }

        if (cancel) {
            // There was an error; don't attempt login and focus the first
            // form field with an error.
            focusView.requestFocus();
        } else {
            // Show a progress spinner, and kick off a background task to
            // perform the user login attempt.
            showProgress(true);
            mAuthTask = new UserLoginTask(email, password, context);
            mAuthTask.execute((Void) null);
        }
    }

    private boolean isEmailValid(String email) {
        Log.d(LOG_TAG, "isEmailValid()");
        //TODO: Replace this with your own logic
        return email.contains("@");
    }

    private boolean isPasswordValid(String password) {
        Log.d(LOG_TAG, "isPasswordValid()");
        //TODO: Replace this with your own logic
        return password.length() > 4;
    }

    /**
     * Shows the progress UI and hides the login form.
     */
    @TargetApi(Build.VERSION_CODES.HONEYCOMB_MR2)
    private void showProgress(final boolean show) {
        Log.d(LOG_TAG, "showProgress()");

        // On Honeycomb MR2 we have the ViewPropertyAnimator APIs, which allow
        // for very easy animations. If available, use these APIs to fade-in
        // the progress spinner.
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB_MR2) {
            int shortAnimTime = getResources().getInteger(android.R.integer.config_shortAnimTime);

            mLoginFormView.setVisibility(show ? View.GONE : View.VISIBLE);
            mLoginFormView.animate().setDuration(shortAnimTime).alpha(
                    show ? 0 : 1).setListener(new AnimatorListenerAdapter() {
                @Override
                public void onAnimationEnd(Animator animation) {
                    mLoginFormView.setVisibility(show ? View.GONE : View.VISIBLE);
                }
            });

            mProgressView.setVisibility(show ? View.VISIBLE : View.GONE);
            mProgressView.animate().setDuration(shortAnimTime).alpha(
                    show ? 1 : 0).setListener(new AnimatorListenerAdapter() {
                @Override
                public void onAnimationEnd(Animator animation) {
                    mProgressView.setVisibility(show ? View.VISIBLE : View.GONE);
                }
            });
        } else {
            // The ViewPropertyAnimator APIs are not available, so simply show
            // and hide the relevant UI components.
            mProgressView.setVisibility(show ? View.VISIBLE : View.GONE);
            mLoginFormView.setVisibility(show ? View.GONE : View.VISIBLE);
        }
    }

    @Override
    public Loader<Cursor> onCreateLoader(int i, Bundle bundle) {
        Log.d(LOG_TAG, "onCreateLoader()");
        return new CursorLoader(this,
                // Retrieve data rows for the device user's 'profile' contact.
                Uri.withAppendedPath(ContactsContract.Profile.CONTENT_URI,
                        ContactsContract.Contacts.Data.CONTENT_DIRECTORY), ProfileQuery.PROJECTION,

                // Select only email addresses.
                ContactsContract.Contacts.Data.MIMETYPE +
                        " = ?", new String[]{ContactsContract.CommonDataKinds.Email
                .CONTENT_ITEM_TYPE},

                // Show primary email addresses first. Note that there won't be
                // a primary email address if the user hasn't specified one.
                ContactsContract.Contacts.Data.IS_PRIMARY + " DESC");
    }

    @Override
    public void onLoadFinished(Loader<Cursor> cursorLoader, Cursor cursor) {
        Log.d(LOG_TAG, "onLoadFinished()");
        List<String> emails = new ArrayList<>();
        cursor.moveToFirst();
        while (!cursor.isAfterLast()) {
            emails.add(cursor.getString(ProfileQuery.ADDRESS));
            cursor.moveToNext();
        }

        addEmailsToAutoComplete(emails);
    }

    @Override
    public void onLoaderReset(Loader<Cursor> cursorLoader) {
        Log.d(LOG_TAG, "onLoaderReset()");
    }

    private void addEmailsToAutoComplete(List<String> emailAddressCollection) {
        Log.d(LOG_TAG, "addEmailsToAutoComplete()");
        //Create adapter to tell the AutoCompleteTextView what to show in its dropdown list.
        ArrayAdapter<String> adapter =
                new ArrayAdapter<>(LoginActivity.this,
                        android.R.layout.simple_dropdown_item_1line, emailAddressCollection);

        mEmailView.setAdapter(adapter);
    }



    private interface ProfileQuery {
        String[] PROJECTION = {
                ContactsContract.CommonDataKinds.Email.ADDRESS,
                ContactsContract.CommonDataKinds.Email.IS_PRIMARY,
        };

        int ADDRESS = 0;
        int IS_PRIMARY = 1;
    }

    /**
     * Represents an asynchronous login/registration task used to authenticate
     * the user.
     */
    public class UserLoginTask extends AsyncTask<Void, Void, Boolean> {

        private final String   mEmail;
        private final String   mPassword;
        private final Context  context;

        UserLoginTask(String email, String password, Context inContext) {
            mEmail       = email;
            mPassword    = password;
            this.context = inContext.getApplicationContext();
        }

        @Override
        protected Boolean doInBackground(Void... params) {
            Log.d(LOG_TAG, "doInBackground()");
            // TODO: attempt authentication against a network service.

            try {
                // For now, only one user
                // TODO: support multiple users
                String userEmail = LocalStorage.getInstance().getLatestUserEmail();
                Log.d(LOG_TAG, "LatestUser email: " + userEmail) ;

                String token = LocalStorage.getInstance().getLatestUserToken();
                Log.d(LOG_TAG, "LatestUser token: " + token) ;


                JsonAccess jsa    = new JsonAccess();

                if (token!=null && !token.equals("")) {
                    Log.d(LOG_TAG, "Using saved token: " + token);
                } else {
                    Log.d(LOG_TAG, "Not using saved token: " + token);
//                    token = jsa.getToken("hesa@sandklef.com", "F23bzx%d");
                    token = jsa.getToken(mEmail, mPassword);
                    LocalStorage.getInstance().setLatestUserToken(token);
                    Log.d(LOG_TAG, "Token: " + token);
                    if (token==null || token.equals("")){
                        Log.d(LOG_TAG, "returning false since token seems odd");
                        return false;
                    }
                }
                CoachAppSession.getInstance().verifyToken(token);
                CoachAppSession.getInstance().startUp(mEmail, token);
                return true;
            } catch (JsonAccessException e) {
                Log.d(LOG_TAG, "Failed to get token." + e.getMessage());
                Log.d(LOG_TAG, "Failed to get token." + e.getMode());
                if(e.getMode()==JsonAccessException.NETWORK_ERROR){
                    ReportUser.log("Network is down", "Network is down");
                    loginFailureMessage = "Network seems to be down";
                }
                else if(e.getMode()==JsonAccessException.ACCESS_ERROR)  {
                    ReportUser.log("Password incorrect", "Password incorrect");
                    loginFailureMessage = "Password incorrect";
                } else {
                    loginFailureMessage = "Unknown reason";
                }
                e.printStackTrace();
            }


            return false;
        }

        @Override
        protected void onPostExecute(final Boolean success) {
            Log.d(LOG_TAG, "onPostExecute()");
            mAuthTask = null;
            showProgress(false);

            Log.d(LOG_TAG, "onPostExecute() success: " + success);
            if (success) {
                //finish();
                ActivitySwitcher.startTeamActivity(context);
                ReportUser.log(mEmail + " logged in", "User " + mEmail + " logged in correctly with email and password");
            } else {
                Log.d(LOG_TAG, "onPostExecute() deleting token");
                LocalStorage.getInstance().setLatestUserToken(null);
                mPasswordView.setError(loginFailureMessage);
                mPasswordView.requestFocus();
            }
        }

        @Override
        protected void onCancelled() {
            Log.d(LOG_TAG, "onCancelled()");
            mAuthTask = null;
            showProgress(false);
        }
    }


}

