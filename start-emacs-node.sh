#!/bin/sh
cd `dirname $0`

exec erl -pa $PWD/ebin -s reloader -eval "application:load(social_net_api)" -sname emacs
