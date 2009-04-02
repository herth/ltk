#!/usr/bin/env wish8.5
#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.
#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


package require Tk
wm geometry . 900x600
wm title . "LTk remote"

set host localhost
set port 19790
set server ""


if {[llength $argv] > 0} {
    set host [lindex $argv 0]
}

if {[llength $argv] > 1} {
    set port [lindex $argv 1]
}



label .status -text "Connecting to server $host port $port." -font 24
pack .status 
tkwait visibility .status

if {[catch {global server; global host; global port; set server [socket $host $port]}]} {
    tk_messageBox -icon error -type ok -title "Connection failed!" -message "Cannot connect to server $host port $port."
    exit
}


fconfigure $server -blocking 0 -translation binary -encoding utf-8
fileevent $server readable [list sread $server]

.status configure -text "Connected, waiting for data"

set buffer ""

proc getcount {s} { 
    if {[regexp {^\s*(\d+) } $s match num]} {
        return $num
    }
}

proc getstring {s} { 
    if {[regexp {^\s*(\d+) } $s match]} {
        return [string range $s [string length $match] end]
    }
}

proc process_buffer {} {
    global buffer
    global server

    set count [getcount $buffer]
    set tmp_buf [getstring $buffer]

    while {($count > 0) && ([string length $tmp_buf] >= $count)} {
        set cmd [string range $tmp_buf 0 $count]
        set buffer [string range $tmp_buf [expr $count+1] end]

        if {[catch $cmd result]>0} {
            tk_messageBox -icon error -type ok -title "Error!" -message $result
            puts $server "(error: \"$result\")"
            flush $server
            close $server
            exit
        }
        set count [getcount $buffer]
        set tmp_buf [getstring $buffer]
    }
}

proc sread {server} {
    global buffer
    if {[eof $server]} {
        tk_messageBox -icon info -type ok -title "Connection closed" -message "The connection has been closed by the server."
        close $server
        exit
    } else {
        set txt [read $server];
        set buffer "$buffer$txt"
        process_buffer
    }
}
