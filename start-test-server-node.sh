#!/bin/sh
cd `dirname $0`
exec erl -pa ebin deps/*/ebin -boot start_sasl -config default.config  -sname social_net_api@`hostname` -s reloader -s social_net_api
