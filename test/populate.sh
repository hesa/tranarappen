#!/bin/bash

. $(dirname $0)/members.sh

declare -a CLUBS=("IK Nord" "AHK" "Azalea BK" "Balic" )
CLUBS_LEN=${#CLUBS[@]}

declare -a TEAMS_M=("H-Junior" "A-Pojk" "P01" "P02" "P03" "P04" "P05" "P06" "P07" "P08" "P09"   )
TEAMS_M_LEN=${#TEAMS_M[@]}
declare -a TEAMS_W=("H-Junior" "A-Flick" "F01" "F02" "F03" "F04" "F05" "F06" "F07" "F08" "F09"   )
TEAMS_W_LEN=${#TEAMS_W[@]}


create_team() {
    GENDER=$2
    echo " * $1"
    PLAYERS=$(( 10 + $(( $RANDOM % 20 )) ))

    PLAYER_CNT=0
    while [ $PLAYER_CNT -lt $PLAYERS ]
    do
	echo -n "  ** "
	if [ "$GENDER" = "woman" ]
	then
	    getWoman
	elif [ "$GENDER" = "man" ]
	then
	    getMan
	else
	    echo "uh oh...."
	    exit 1
	fi
	    
	PLAYER_CNT=$(( $PLAYER_CNT + 1 ))
    done
    
    
}

create_club() {

    echo "$1"
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

#getMan
#getWoman

create_content
