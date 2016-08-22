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

package com.sandklef.coachapp.json;

public class JsonAccessException extends Exception {

    public final static int OK            = 0;  // no problemos
    public final static int NETWORK_ERROR = 1;  // netowrk down, http problem
    public final static int ACCESS_ERROR  = 2;  // server responds "not ok"

    private int mode  ;

    public JsonAccessException(String msg, int mode) {
        super(msg);
        this.mode=mode;
    }

    public JsonAccessException(String msg, Exception e, int mode) {
        super(msg, e);
        this.mode=mode;
    }

    public JsonAccessException(Exception e, int mode) {
        super(e);
        this.mode=mode;
    }

    public int getMode() {
        return mode;
    }

}
