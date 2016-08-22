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
import android.net.Uri;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.view.ViewPager;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.support.v4.app.FragmentManager;
import android.widget.TextView;
import android.widget.Toast;

import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.Member;
import com.sandklef.coachapp.model.Team;
import com.sandklef.coachapp.model.TrainingPhase;
import com.sandklef.coachapp.storage.LocalMediaStorage;
import com.sandklef.coachapp.storage.LocalStorage;
import com.sandklef.coachapp.storage.Storage;

import java.io.File;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import coachassistant.sandklef.com.coachapp.R;

/**
 * A simple {@link Fragment} subclass.
 * Activities that contain this fragment must implement the
 * {@link TopFragment.OnFragmentInteractionListener} interface
 * to handle interaction events.
 * Use the {@link TopFragment#newInstance} factory method to
 * create an instance of this fragment.
 */
public class TopFragment extends Fragment {
    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER

    private final static String LOG_TAG = TopFragment.class.getSimpleName();
    private final static String VIDEO_FILE_DATE_FORMAT = "yyyyMMdd-HHmmss";
    private final static String VIDEO_FILE_TYPE_SUFFIX = ".mp4";
    private final static int VIDEO_FILE_DEFAULT_TIME = 5000;


//    private MainActivity.SectionsPagerAdapter mSectionsPagerAdapter;

    private int currentBottomIndex;

    private ViewPager mTopViewPager;
    private ViewPager mBottomViewPager;

    private BottomFragmentAdapter bottomPagerAdapter;
    private TopFragmentAdapter topPagerAdapter;

    private OnFragmentInteractionListener mListener;


    public static int BOTTOM_FRAGMENT_TEAM_INDEX = 0;
    public static int BOTTOM_FRAGMENT_TRAININGPHASE_INDEX = 1;
    public static int BOTTOM_FRAGMENT_MEMBER_INDEX = 2;
    public static int BOTTOM_FRAGMENT_LAST_INDEX = 3;

    public static int TOP_FRAGMENT_USER_INDEX = 0;
    public static int TOP_FRAGMENT_VIDEO_INDEX = 1;
    public static int TOP_FRAGMENT_LAST_INDEX = 2;

    private int maxPage = 0;
    private double lastX;

    public static TopFragment newInstance() {
        TopFragment fragment = new TopFragment();
        Bundle args = new Bundle();
        Log.d(LOG_TAG, "  newInstance()");
        fragment.setArguments(args);
        return fragment;
    }

    public int getCurrentBottomFragmentIndex() {
        return currentBottomIndex;
    }

    public TopFragment() {
        // Required empty public constructor
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Log.d(LOG_TAG, "  onCreate");

    }

    public void onTeamFragmentInteraction(Team t) {
        Log.d(LOG_TAG, "onTrainingphasesFragmentInteraction " + t + "  team: '" + LocalStorage.getInstance().getCurrentTeam() + "'");
        if (t != null) {
            LocalStorage.getInstance().setCurrentTeam(t.getUuid());
            showTrainingPhases();
        } else {
            Log.d(LOG_TAG, "You must chose a team first");
        }
    }

    public void onTrainingphasesFragmentInteraction(TrainingPhase tp) {
        Log.d(LOG_TAG, "onTrainingphasesFragmentInteraction " + tp);
        LocalStorage.getInstance().setCurrentTrainingPhase(tp.getUuid());
        showMember();
    }

    private boolean recordVideo() {
        DateFormat df = new SimpleDateFormat(VIDEO_FILE_DATE_FORMAT);
        Date today = Calendar.getInstance().getTime();
        String mediaDate = df.format(today);

        String newFileName = LocalStorage.getInstance().getNewMediaDir() + "/" +
                mediaDate + VIDEO_FILE_TYPE_SUFFIX;

        File newFile = new File(newFileName);
        String dirName = newFile.getParent();
        File dir = new File(dirName);
        Log.d(LOG_TAG, "  Dir:  " + dir.getPath());
        boolean created = dir.mkdirs();

        Log.d(LOG_TAG, "RECORD TO NEW FILE: " + newFile);
       // topPagerAdapter.getSimpleVideoFragment().getVideoCapture().startRecording(newFile, VIDEO_FILE_DEFAULT_TIME);
        return true;
    }


    public void onMemberInteraction(Member m) {
        Log.d(LOG_TAG, " onMemberInteraction " + m);
        LocalStorage.getInstance().setCurrentMember(m.getUuid());
        showVideo();

        recordVideo();
    }

    public void onMediaInteraction(long id) {
        Log.d(LOG_TAG, " onMediaInteraction " + id);
    }


    public void showTeams() {
        Log.d(LOG_TAG, "showTeams()");
        setBottomFragmentIndex(BOTTOM_FRAGMENT_TEAM_INDEX);
    }

    public void showTrainingPhases() {
        Log.d(LOG_TAG, "showTrainingPhases()    team: '" + LocalStorage.getInstance().getCurrentTeam() + "'");
        setBottomFragmentIndex(BOTTOM_FRAGMENT_TRAININGPHASE_INDEX);
    }

    public void showMember() {
        Log.d(LOG_TAG, "showMembers()");
        setBottomFragmentIndex(BOTTOM_FRAGMENT_MEMBER_INDEX);
    }

    public void showVideo() {
        Log.d(LOG_TAG, "showVideos()");
        setTopFragmentIndex(TOP_FRAGMENT_VIDEO_INDEX);
    }

    public void showUser() {
        Log.d(LOG_TAG, "showUser()");
        setTopFragmentIndex(TOP_FRAGMENT_USER_INDEX);
    }

    public void setTopFragmentIndex(int index) {
        mTopViewPager.setCurrentItem(index, true);
        topPagerAdapter.notifyDataSetChanged();
    }

    public void setBottomFragmentIndex(int index) {
        currentBottomIndex = index;
        if (index == 0) {
            LocalStorage.getInstance().setCurrentTeam("");
        } else if (index == 1) {
            LocalStorage.getInstance().setCurrentTrainingPhase("");
        }
        Log.d(LOG_TAG, "setBottomFragmentIndex(" + index + ")    team: '" + LocalStorage.getInstance().getCurrentTeam() + "'");
        mBottomViewPager.setCurrentItem(index, true);
        bottomPagerAdapter.notifyDataSetChanged();
        bottomPagerAdapter.updateMemberFragment();
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment

        Log.d(LOG_TAG, "  onCreateView()");
        View root = inflater.inflate(R.layout.fragment_top, container, false);

        mBottomViewPager = (ViewPager) root.findViewById(R.id.bottom_pager);
        mTopViewPager = (ViewPager) root.findViewById(R.id.top_pager);
        Log.d(LOG_TAG, "  onCreateView()  vp: " + R.id.bottom_pager);
        Log.d(LOG_TAG, "  onCreateView()  vp: " + mBottomViewPager);

        bottomPagerAdapter = new BottomFragmentAdapter(getChildFragmentManager(), root, this);
        topPagerAdapter = new TopFragmentAdapter(getChildFragmentManager());

        mBottomViewPager.setAdapter(bottomPagerAdapter);
        mBottomViewPager.setOffscreenPageLimit(5);


        mTopViewPager.setAdapter(topPagerAdapter);
        mTopViewPager.setOffscreenPageLimit(5);

        setSwipeListener(mBottomViewPager, bottomPagerAdapter);

        return root;
    }

    public void unSetTeam() {
        //     ((AdaptedSwipeViewPager)mBottomViewPager).setPagingEnabled(true);
        ((AdaptedSwipeViewPager) mBottomViewPager).setPagingMax(0);
        maxPage = 0;
        LocalStorage.getInstance().setCurrentTeam("");
    }

    public void unSetTrainingPhase() {
        // ((AdaptedSwipeViewPager)mBottomViewPager).setPagingEnabled(true);
        ((AdaptedSwipeViewPager) mBottomViewPager).setPagingMax(1);
        maxPage = 1;
        LocalStorage.getInstance().setCurrentTrainingPhase("");
    }

    public void unSetMember() {
        // ((AdaptedSwipeViewPager)mBottomViewPager).setPagingEnabled(true);
        ((AdaptedSwipeViewPager) mBottomViewPager).setPagingMax(2);
        maxPage = 2;
        LocalStorage.getInstance().setCurrentMember("");
    }

    private void setSwipeListener(final ViewPager bottomPager, final BottomFragmentAdapter bottomAdapter) {
        bottomPager.setOnPageChangeListener(new ViewPager.OnPageChangeListener() {
            @Override
            public void onPageScrolled(int position, float positionOffset,
                                       int positionOffsetPixels) {
                Log.d(LOG_TAG, "  onPageScrolled " + position + "( '"
                        + LocalStorage.getInstance().getCurrentTeam() + "'  '"
                        + LocalStorage.getInstance().getCurrentTrainingPhase() + "'  '"
                        + LocalStorage.getInstance().getCurrentMember() + ") ");

            }

            @Override
            public void onPageSelected(final int i) {
                Fragment fragment = (Fragment) bottomAdapter.instantiateItem(bottomPager, i);
                Log.d(LOG_TAG, "  onPageSelected " + i + "( '"
                        + LocalStorage.getInstance().getCurrentTeam() + "'  '"
                        + LocalStorage.getInstance().getCurrentTrainingPhase() + "'  '"
                        + LocalStorage.getInstance().getCurrentMember() + ") ");


                if (i == BOTTOM_FRAGMENT_TEAM_INDEX) {
                    unSetTeam();
                } else if (i == BOTTOM_FRAGMENT_TRAININGPHASE_INDEX) {
                    unSetTrainingPhase();
                } else if (i == BOTTOM_FRAGMENT_MEMBER_INDEX) {
                    unSetMember();
                }

                if (i == BOTTOM_FRAGMENT_MEMBER_INDEX) {
                    showVideo();
                } else {
                    showUser();
                }
            }

            @Override
            public void onPageScrollStateChanged(final int i) {
                Log.d(LOG_TAG, "  onPageScrollStateChanged " + i);
            }
        });
    }


    // TODO: Rename method, update argument and hook method into UI event
    public void onButtonPressed(Uri uri) {
        if (mListener != null) {
            mListener.onFragmentInteraction(uri);
        }
    }

    @Override
    public void onAttach(Activity activity) {
        //    Log.d(LOG_TAG, "  ---> onAttach()");
        super.onAttach(activity);
        //Log.d(LOG_TAG, "  --- onAttach()");
        try {
            //      Log.d(LOG_TAG, "  --- onAttach() 1");
            mListener = (OnFragmentInteractionListener) activity;
            //    Log.d(LOG_TAG, "  --- onAttach() 2");
        } catch (ClassCastException e) {
            //  Log.d(LOG_TAG, "  --- onAttach() exception: " + e);

            throw new ClassCastException(activity.toString()
                    + " must implement OnFragmentInteractionListener");
        }
        //Log.d(LOG_TAG, "  <--- onAttach()");

    }


    @Override
    public void onDetach() {
        super.onDetach();
        mListener = null;
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
    public interface OnFragmentInteractionListener {
        // TODO: Update argument type and name
        public void onFragmentInteraction(Uri uri);
    }

    public static class BottomFragmentAdapter extends FragmentPagerAdapter {
        private final static String LOG_TAG = BottomFragmentAdapter.class.getSimpleName();
        private int currentItemPosition;
        private View rootView;
        private Fragment f;
        private MemberFragment mFragment;


        public BottomFragmentAdapter(FragmentManager fm, View root, Fragment f) {
            super(fm);
            this.f = f;
            rootView = root;
            currentItemPosition = 0;
        }


        public void updateMemberFragment() {
            mFragment.updateMemberList();
        }


        @Override
        public int getCount() {
//            Log.d(LOG_TAG, "  getCount()   team: '" + LocalStorage.getInstance().getCurrentTeam() + "'");

            //          Log.d(LOG_TAG, "  getCount()  position: " + currentItemPosition + "  max: " + BOTTOM_FRAGMENT_LAST_INDEX);
/*
            */
            return BOTTOM_FRAGMENT_LAST_INDEX;
        }

        private void toastMessage(int id) {
            Context context = rootView.getContext();
            String text = f.getActivity().getString(id);
            toastMessage(text);
        }

        private void toastMessage(String text) {
            Context context = rootView.getContext();
            int duration = Toast.LENGTH_SHORT;
            Log.d(LOG_TAG, text);

            Toast toast = Toast.makeText(context, text, duration);
            toast.show();

        }

        @Override
        public Fragment getItem(int position) {
            Bundle args = new Bundle();
//            args.putInt(ChildFragment.POSITION_KEY, position);

            String teamIdx = LocalStorage.getInstance().getCurrentTeam();


/*            if (teamIdx.equals("")) {
                toastMessage(R.string.MUST_CHOSE_TEAM);
                return TeamFragment.newInstance();
            }
*/

            Log.d(LOG_TAG, "  getItem(" + position + ")   team: '" + teamIdx + "'  " + (teamIdx.length() == 0));

            if (position == 0) {
                return TeamFragment.newInstance();
            } else if (position == 1) {
                return TrainingPhasesFragment.newInstance();
            } else {
                mFragment = MemberFragment.newInstance();
                return mFragment;
            }
        }


        @Override
        public CharSequence getPageTitle(int position) {
            return position + "Child Fragment " + position;
        }

    }

    public static class TopFragmentAdapter extends FragmentPagerAdapter {
        private final static String LOG_TAG = TopFragmentAdapter.class.getSimpleName();
        private SimpleVideoFragment videoFragment;

        public TopFragmentAdapter(FragmentManager fm) {
            super(fm);
        }

        @Override
        public int getCount() {
            //   Log.d(LOG_TAG, "  getCount()");
            return TOP_FRAGMENT_LAST_INDEX;
        }

        @Override
        public Fragment getItem(int position) {
            Bundle args = new Bundle();
//            args.putInt(ChildFragment.POSITION_KEY, position);
            Log.d(LOG_TAG, "  getItem()");
            if (position == 0)
                return UserFragment.newInstance();

            videoFragment = SimpleVideoFragment.newInstance();
            return videoFragment;
//            return Camera2Fragment.newInstance();

        }

        public SimpleVideoFragment getSimpleVideoFragment() {
            return videoFragment;
        }

        @Override
        public CharSequence getPageTitle(int position) {
            return position + "Child Fragment " + position;
        }

    }

}
