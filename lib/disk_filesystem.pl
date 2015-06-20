/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(disk_filesystem,[]).

end_of_file.  

Why mankind will always control AI?


% Inital template (A copy of Gitty)

:- module(disk_filesystem,
	  [ disk_filesystem_file/3,		% +Store, ?Name, ?Hash
	    disk_filesystem_create/5,		% +Store, +Name, +Data, +Meta, -Commit
	    disk_filesystem_update/5,		% +Store, +Name, +Data, +Meta, -Commit
	    disk_filesystem_commit/3,		% +Store, +Name, -Meta
	    disk_filesystem_data/4,		% +Store, +Name, -Data, -Meta
	    disk_filesystem_history/4,		% +Store, +Name, -History, +Options
	 %   disk_filesystem_diff/4,		% +Store, ?Start, +End, -Diff

            is_disk_filesystem_path/4             % +PathInfo, +FName, -Store, -Name
	  ]).



is_disk_filesystem_path(DirIn,NameIn,DirOut,NameOut):- 
     DirIn = filesystem,
     directory_file_path(DirIn,NameIn,PathInfo),directory_file_path(DirOut,NameOut,PathInfo).

:- if(exists_source(swish_hooks)).
:- include(swish_hooks).
swish_hooks:storage_plugin_commit(DirStore, FName, Meta):- is_disk_filesystem_path(DirStore,FName,Store,Name), disk_filesystem_commit(Store, Name, Meta).
swish_hooks:storage_plugin_create(DirStore, FName, Data, Meta, Commit):- is_disk_filesystem_path(DirStore,FName,Store,Name), disk_filesystem_create(Store, Name, Data, Meta, Commit).
swish_hooks:storage_plugin_data(DirStore, FName, Data, Meta):- is_disk_filesystem_path(DirStore,FName,Store,Name), disk_filesystem_data(Store, Name, Data, Meta).
swish_hooks:storage_plugin_history(DirStore, FName, History, Options):- is_disk_filesystem_path(DirStore,FName,Store,Name), disk_filesystem_history(Store, Name, History, Options).
swish_hooks:storage_plugin_update(DirStore, FName, Data, Meta, Commit):- is_disk_filesystem_path(DirStore,FName,Store,Name), disk_filesystem_update(Store, Name, Data, Meta, Commit).
swish_hooks:storage_plugin_file(DirStore, FName, Hash):- is_disk_filesystem_path(DirStore,FName,Store,Name), disk_filesystem_file(Store, Name, Hash).
% swish_hooks:storage_plugin_diff(DirStore, Start, End, Diff):- is_disk_filesystem_path(DirStore,_FName,Store,_Name), disk_filesystem_diff(Store, Start, End, Diff).    
swish_hooks:storage_plugin_load_source_data( PathInfo, Code, Options):- directory_file_path(DirStore,FName,PathInfo),
     is_disk_filesystem_path(DirStore,FName,Store,Name), disk_filesystem_data(Store, Name, Code, Options).

:- endif.
