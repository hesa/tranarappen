#!/bin/bash

. $(dirname $0)/members.sh

declare -a CLUBS=("IK Nord" "AHK" )
#"Azalea BK" "Balic" )
CLUBS_LEN=${#CLUBS[@]}

declare -a TEAMS_M=("H-Junior" "A-Pojk" "P01" )
#"P02" "P03" "P04" "P05" "P06" "P07" "P08" "P09"   )
TEAMS_M_LEN=${#TEAMS_M[@]}
declare -a TEAMS_W=("D-Junior" "A-Flick" "F01")
#    "F02" "F03" "F04" "F05" "F06" "F07" "F08" "F09"   )
TEAMS_W_LEN=${#TEAMS_W[@]}

export URLBASE="localhost:3000/0.0.0"

#
# CLUB
#
get_club_id() {
    curl --request GET $URLBASE/clubs 2>/dev/null | jshon | grep -B 2 "$1" | grep uuid | awk '{print $2}' | sed 's/,//g'| sed 's/\"//g'
}

get_club_ids() {
    curl --request GET $URLBASE/clubs 2>/dev/null | jshon | grep uuid | awk '{print $2}' | sed 's/,//g'| sed 's/\"//g'
}

add_club() {
    curl --data "{ \"name\": \"$1\" }" --header "Content-Type: application/json" --request POST $URLBASE/clubs 2>/dev/null| sed 's/\"//g'
}

#
# TEAM
#
get_team_id() {
    curl --request GET $URLBASE/clubs/$CLUBID/teams 2>/dev/null | jshon | grep -B 3 "$TEAM" | grep uuid  | awk '{print $2}' | sed 's/,//g'| sed 's/\"//g'
}

get_team_ids() {
    curl --request GET $URLBASE/clubs/$CLUBID/teams 2>/dev/null | jshon | grep uuid  | awk '{print $2}' | sed 's/,//g'| sed 's/\"//g'
}

add_team() {
    curl --data "{ \"name\": \"$2\" }" --header "Content-Type: application/json" --request POST $URLBASE/clubs/$CLUBID/teams 2>/dev/null
}


#
# MEMBER
#
get_member_id() {
    curl --request GET $URLBASE/clubs/$CLUBID/members 2>/dev/null | jshon | grep -B 3 "$MEMBER" | grep uuid  | awk '{print $2}' | sed 's/,//g'| sed 's/\"//g'
}

get_member_ids() {
    curl --request GET $URLBASE/clubs/$CLUBID/members 2>/dev/null | jshon | grep uuid  | awk '{print $2}' | sed 's/,//g'| sed 's/\"//g'
}

add_member() {
    echo curl --data "{ \"name\": \"$1\", \"teamUuid\": \"$TEAMID\" }" --header "Content-Type: application/json" --request POST $URLBASE/clubs/$CLUBID/members
    curl --data "{ \"name\": \"$1\", \"teamUuid\": \"$TEAMID\" }" --header "Content-Type: application/json" --request POST $URLBASE/clubs/$CLUBID/members
}


create_team() {
    export TEAM=$1
    export TEAMID=$(add_team $CLUBID "$TEAM")
    echo -n "     |--TEAM / $TEAM"
    if [ "$(echo $TEAMID | grep customReason | wc -l)" != "0" ]
    then
	TEAMID=$(get_team_id "$CLUB" "$TEAM")
    fi
    echo " / $TEAMID"
    #    echo "addedded '$CLUBID'  '$TEAM' => $TEAMID"
#    echo "  ** $TEAM $TEAMID"
 
    GENDER=$2
#    echo " * $1"
#    PLAYERS=$(( 10 + $(( $RANDOM % 20 )) ))
    PLAYERS=10
    
    PLAYER_CNT=0
    while [ $PLAYER_CNT -lt $PLAYERS ]
    do
	if [ "$GENDER" = "woman" ]
	then
	    NAME=$(getWoman)
	elif [ "$GENDER" = "man" ]
	then
	    NAME=$(getMan)
	else
	    echo "uh oh...."
	    exit 1
	fi

	echo -n "           |--MEMBER:$NAME"
	add_member "$NAME"
	#2>/dev/null >/dev/null
	echo
	PLAYER_CNT=$(( $PLAYER_CNT + 1 ))
    done   
}


create_club() {

    echo -n "|--CLUB: $1"
    
    export CLUB=$1
    export CLUBID=$(add_club "$CLUB")
    if [ "$(echo $CLUBID | grep customReason | wc -l)" != "0" ]
    then
	export CLUBID=$(get_club_id "$CLUB")
    fi
    echo " / $CLUBID"
    
    TEAM_CNT=0
    while [ $TEAM_CNT -lt $TEAMS_M_LEN ]
    do
	create_team "${TEAMS_M[$TEAM_CNT]}"  "man"
	TEAM_CNT=$(( $TEAM_CNT + 1 ))
    done
    
    TEAM_CNT=0
    while [ $TEAM_CNT -lt $TEAMS_W_LEN ]
    do
	create_team "${TEAMS_W[$TEAM_CNT]}" "woman"
	TEAM_CNT=$(( $TEAM_CNT + 1 ))
    done
    
}

create_content() {
    
    CLUB_CNT=0
    while [ $CLUB_CNT -lt $CLUBS_LEN ]
    do
	create_club "${CLUBS[$CLUB_CNT]}"
	CLUB_CNT=$(( $CLUB_CNT + 1 ))
    done
}

clean_web() {
    for club in $(get_club_ids)
    do
	CLUBID=$club
	for team in $(get_team_ids)
	do
	    for member in $(get_member_ids)
	    do
		echo "C: $club  T: $team ---> $member"
		curl --request DELETE $URLBASE/clubs/$CLUBID/members/$member
	    done
	    curl --request DELETE $URLBASE/clubs/$CLUBID/teams/$team
	done
	curl --request DELETE $URLBASE/clubs/$CLUBID
    done
}

list_web() {
    for club in $(get_club_ids)
    do
	CLUBID=$club
	for team in $(get_team_ids)
	do
	    for member in $(get_member_ids)
	    do
		echo "C: $club  T: $team ---> $member"
#		curl --request DELETE localhost:3000/0.0.0/clubs/$CLUBID/members/$member
	    done
	done
    done
}

#getMan
#getWoman

if [ "$1" = "--clean" ]
then
    clean_web
    exit 
elif [ "$1" = "--list" ]
then
    list_web
    exit 
fi

create_content
