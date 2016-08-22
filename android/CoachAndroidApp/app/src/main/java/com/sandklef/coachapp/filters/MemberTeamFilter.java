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

package com.sandklef.coachapp.filters;


import com.sandklef.coachapp.model.Member;

public class MemberTeamFilter implements MemberFilter {

    private final static String LOG_TAG = MemberTeamFilter.class.getSimpleName();

    private String teamUuid;

    public MemberTeamFilter() {
        teamUuid=null;
    }

    static public MemberTeamFilter newMemberTeamFilter(String uuid) {
        MemberTeamFilter bf = new MemberTeamFilter();
        bf.teamUuid=uuid;
        return bf;
    }


    @Override
    public boolean check(Member m) {
        return (teamUuid.equals(m.getTeamUuid()));
    }

}
