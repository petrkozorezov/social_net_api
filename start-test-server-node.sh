#!/bin/sh
exec erl -pa ebin -bool start_sasl -s social_net_api -s reloader -sname social_net_api@`hostname` -config default
