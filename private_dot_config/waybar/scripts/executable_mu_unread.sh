#!/bin/bash

# Run mu and capture the output
messages=$(mu find flag:unread)

# Check the exit code of mu
exit_code=$?

case $exit_code in
    0)
        # If successful, count the messages
        unread_count=$(echo "$messages" | wc -l)
        echo "{\"text\":\"$unread_count\", \"alt\":\"maybe_mail\"}"
        ;;

    4)
        # Handle no matches
        echo "{\"text\":\"0\", \"alt\":\"no_mail\"}"
        ;;

    *)
        # Handle other errors
        echo "{\"text\":\"?\", \"alt\":\"maybe_mail\"}"
        ;;
esac
