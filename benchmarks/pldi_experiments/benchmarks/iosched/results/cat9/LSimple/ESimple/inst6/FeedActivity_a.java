/*
 * Copyright (c) 2017 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package com.google.samples.apps.iosched.feed;

import android.os.Bundle;
import android.support.v7.widget.RecyclerView;

import com.google.firebase.database.DatabaseReference;
import com.google.firebase.database.FirebaseDatabase;
import com.google.samples.apps.iosched.feed.data.FeedMessage;
import com.google.samples.apps.iosched.lib.R;
import com.google.samples.apps.iosched.navigation.NavigationModel;
import com.google.samples.apps.iosched.ui.BaseActivity;

/**
 * This is the host Activity for a list of cards that present key updates on the conference.
 * The cards are shown in a {@link RecyclerView} and the content in each card comes from a Firebase
 * Real-Time Database.
 */
public class FeedActivity extends BaseActivity {

    private static final String SCREEN_LABEL = "Feed";

    private FeedContract.Presenter mPresenter;
    private DatabaseReference mDatabaseReference;

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        FeedMessage.initCategoryColorMap(getResources());
        setContentView(R.layout.feed_act);
        setFullscreenLayout();
        disableActionBarTitle();
        FeedFragment feedFragment = (FeedFragment) getSupportFragmentManager()
                .findFragmentById(R.id.main_content);
        mPresenter = new FeedPresenter(feedFragment);
        feedFragment.setPresenter(mPresenter);
        mDatabaseReference = FirebaseDatabase.getInstance().getReference().child("feed");
        mPresenter.initializeDataListener(mDatabaseReference);
    }

    @Override
    protected void onStop() {
        mPresenter.removeDataListener(mDatabaseReference);
        super.onStop();
    }

    @Override
    protected NavigationModel.NavigationItemEnum getSelfNavDrawerItem() {
        return NavigationModel.NavigationItemEnum.FEED;
    }

    @Override
    public boolean canSwipeRefreshChildScrollUp() {
        return true;
    }

    @Override
    protected String getScreenLabel() {
        return SCREEN_LABEL;
    }
}
