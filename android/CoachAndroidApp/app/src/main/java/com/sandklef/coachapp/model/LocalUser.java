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

package com.sandklef.coachapp.model;

import java.util.List;

public class LocalUser  {

    private int          id;
    private String       name;
    private String       email;
    private String       password; // currently not used
    private List<String> clubUuids;
    private String       latestClubUuid;
    private String       token;

    public LocalUser(int          id,
                     String       name,
                     String       email,
                     String       password,
                     List<String> clubUuids,
                     String       latestClubUuid,
                     String       token) {
        this.name           = name;
        //    super(uuid,name, "");
        this.email          = email;
        this.password       = password;
        this.clubUuids      = clubUuids;
        this.latestClubUuid = latestClubUuid;
        this.token          = token;
    }

    public LocalUser(String       name,
                     String       email,
                     String       password,
                     List<String> clubUuids,
                     String       latestClubUuid,
                     String       token) {
        this(-1, name, email, password, clubUuids, latestClubUuid, token);
    }


    public int    getId()               { return id;        }
    public String getName()             { return name;      }
    public String getEmail()            { return email;     }
    public String getPassword()         { return password;  }// currently not used
    public List<String> getClubUuids()  { return clubUuids; }
    public String getLatestClubUuid()   { return latestClubUuid;}
    public String getToken()            { return token;}

    public String toString() {
        return "[ " + id + " | " +
                name + " | " +
                email + " | " +
                password + " | " +
                clubUuids + " | " +
                latestClubUuid + " | " +
                token + " ]" ;
    }

}
