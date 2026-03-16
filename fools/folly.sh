#!/bin/bash

# Script to start the Forth language server.
#
# We connect stdout to file descriptor 5 because we want to use normal
# Forth output words, like TYPE, EMIT, etc. for debugging without
# messing up the communication channel with the client.  This is easy
# to do in a shell script but hard in portable Forth.

FORTH=gforth
PORT=''

function usage {
  echo "Usage: $0 [<option> ...] "
  echo "Options:"
  echo "  --forth <program>   Forth program (default: $FORTH)"
  echo "  --port <port>       Port number for socket as communication channel"
  exit 2
}

function parse_options {
  while [ $# != 0 ] ; do
    case "$1" in
      --forth)
	if (( $# < 2 )) ; then
	  echo "--forth argument missing"
	  usage;
	fi
	FORTH="$2"
	shift 2
	;;
      --port)
	if (( $# < 2 )) ; then
	  echo "--port argument missing"
	  usage;
	fi
	PORT="$2"
	shift 2
	;;
      *)
	echo "Unrecognized option: $1"
	usage
    esac
  done
}

function start {
  cd "$(dirname $(realpath $0))"
  case "$FORTH" in
    vfx*) exec "$FORTH" include main.fth ;;
    *) exec "$FORTH" main.fth ;;
  esac
}

parse_options $*

if [ "$PORT" == "" ]; then
  start 5>&1 1>&2
else
  exec socat -r /tmp/folly.log -R /tmp/folly.log \
       tcp-listen:"$PORT",bind=127.0.0.1,reuseaddr \
       EXEC:"$0 --forth '$FORTH'"
fi
