#!/usr/bin/wish
wm withdraw .
set host localhost
if {[llength $argv] == 2} {
    set host [lindex $argv 0]
    set port [lindex $argv 1]} else {
	set port [lindex $argv 0]}
puts "connecting to $host $port"

set server [socket $host $port]
set wi [open "|wish" RDWR]
fconfigure $server -blocking 0
fconfigure $wi -blocking 0


fileevent $server readable {set txt [read $server];puts $wi "$txt";flush $wi}
fileevent $wi readable {
    if {[eof $wi]} {
	close $wi
	exit} else {	    
	    set txt [read $wi]; puts "sending:$txt"; puts -nonewline $server $txt;flush $server}}

