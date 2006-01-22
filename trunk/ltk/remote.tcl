#!/usr/bin/wish

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


wm withdraw .
set host localhost
if {[llength $argv] == 2} {
    set host [lindex $argv 0]
    set port [lindex $argv 1]} else {
	set port [lindex $argv 0]}
#puts "connecting to $host $port"

set server [socket $host $port]
set wi [open "|wish" RDWR]
fconfigure $server -blocking 0
fconfigure $wi -blocking 0


fileevent $server readable {set txt [read $server];puts $wi "$txt";flush $wi}
fileevent $wi readable {
    if {[eof $wi]} {
	close $wi
	exit} else {	    
	    set txt [read $wi]; puts -nonewline $server $txt;flush $server}}

