#!/bin/bash
# (c) 2008 Andreas Pohl

USE_DBUS=y
LOCALNAME="bash"

SSH_COMMAND=$1
shift

if [ -z $SSH_COMMAND ]; then
    echo "Usage: $0 <session title> <ssh command> [<parametes>]"
    exit;
fi

MACHINE=`basename $SSH_COMMAND|sed 's/^.*+//'|sed -e's/:.*//'`
if [ "_local_" = "$MACHINE" ]; then
    MACHINE=$LOCALNAME
    SESSION_TITLE=$LOCALNAME
    SSH_COMMAND="bash"
else
    SESSION_TITLE=`echo $MACHINE|sed 's/[\.:_].*$//'`
    if [ ! -z `echo $MACHINE|grep -i dc2` ]; then
	SESSION_TITLE="$SESSION_TITLE.dc2"
    elif [ ! -z `echo $MACHINE|grep -i us-ec` ]; then
	SESSION_TITLE="$SESSION_TITLE.East"
    elif [ ! -z `echo $MACHINE|grep -i us-wc` ]; then
	SESSION_TITLE="$SESSION_TITLE.West"
    fi
fi

echo "title: $SESSION_TITLE" > /tmp/opensession.log
echo "machine: $MACHINE" >> /tmp/opensession.log
echo "ssh cmd: $SSH_COMMAND" >> /tmp/opensession.log

if [ "$USE_DBUS" = "y" ]; then
    # first try to find an existing session
    for s in `qdbus org.kde.konsole | grep "MainWindow" | grep -v "actions"`; do
	NAME=`qdbus org.kde.konsole $s org.freedesktop.DBus.Properties.Get 1 windowTitle 2>/dev/null`
	if [ -n "$NAME" ] ; then
	    MATCH=`perl -e "if (index('$SSH_COMMAND', '$NAME')>-1) {print 1} else {print 0}"`
	    echo "NAME: $NAME MATCH: $MATCH"
	    if [ "$MATCH" = "1" ]; then
		echo "found: $s Name: $NAME" >> /tmp/opensession.log
                # existing session found, raise the window 
		qdbus org.kde.konsole $s com.trolltech.Qt.QWidget.raise
		exit
	    fi
	fi
    done
else
    # first try to find an existing session
    for k in `dcop konsole-*`; do
	for s in `dcop $k session-*`; do
	    NAME=`dcop $k $s sessionName`
	    if [ "$SESSION_TITLE" = "$NAME" ]; then
		echo "found: $k $s" >> /tmp/opensession.log
	        # existing session found, raise the window 
	        #dcop $k konsole-mainwindow#1 raise
		dcop $k konsole-mainwindow#1 hide
		dcop $k konsole-mainwindow#1 show
		exit
	    fi
	done
    done
fi

echo "not found" >> /tmp/opensession.log

if [ -n "$SSH_COMMAND" ]; then
    echo "executing: $SSH_COMMAND" >> /tmp/opensession.log
    # no session found, create a new one
    konsole -e $SSH_COMMAND $* &
fi
