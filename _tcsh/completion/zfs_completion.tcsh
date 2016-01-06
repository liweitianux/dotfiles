# CDDL HEADER START
#
# The contents of this file are subject to the terms of the
# Common Development and Distribution License (the "License").
# You may not use this file except in compliance with the License.
#
# You can obtain a copy of the license at usr/src/OPENSOLARIS.LICENSE
# or http://www.opensolaris.org/os/licensing.
# See the License for the specific language governing permissions
# and limitations under the License.
#
# When distributing Covered Code, include this CDDL HEADER in each
# file and include the License file at usr/src/OPENSOLARIS.LICENSE.
# If applicable, add the following below this CDDL HEADER, with the
# fields enclosed by brackets "[]" replaced with your own identifying
# information: Portions Copyright [yyyy] [name of copyright owner]
#
# CDDL HEADER END
#

#
# Copyright 2007 Cyril Plisko.  All rights reserved.
# Use is subject to license terms.
#

# This file contains a tcsh completion definitions for zfs command

set zpool_cmd =	(create destroy add remove list iostat status\
		online offline clear attach detach replace scrub\
		import export upgrade history get set)

set zfs_cmd =	(create destroy snapshot rollback clone promote\
		rename list set get inherit mount unmount\
		share unshare send receive)

complete zpool \
	'p/1/$zpool_cmd/'\
	'n/destroy/`zpool list -H -o name`/'\
	'n/add/`zpool list -H -o name`/'\
	'n/remove/`zpool list -H -o name`/'\
	'n/list/`zpool list -H -o name`/'\
	'n/iostat/`zpool list -H -o name`/'\
	'n/status/`zpool list -H -o name`/'\
	'n/online/`zpool list -H -o name`/'\
	'n/offline/`zpool list -H -o name`/'\
	'n/clear/`zpool list -H -o name`/'\
	'n/attach/`zpool list -H -o name`/'\
	'n/detach/`zpool list -H -o name`/'\
	'n/replace/`zpool list -H -o name`/'\
	'n/scrub/`zpool list -H -o name`/'\
	'n/import/`zpool list -H -o name`/'\
	'n/export/`zpool list -H -o name`/'\
	'n/upgrade/`zpool list -H -o name`/'\
	'n/history/`zpool list -H -o name`/'\
	'N/get/`zpool list -H -o name`/'\
	'N/set/`zpool list -H -o name`/'
	
complete zfs \
	'p/1/$zfs_cmd/'\
	'n/create/`zfs list -H -o name`/'\
	'n/destroy/`zfs list -H -o name`/'\
	'n/snapshot/`zfs list -H -o name`/'\
	'n/rollback/`zfs list -H -t snapshot -o name`/'\
	'n/clone/`zfs list -H -t snapshot -o name`/'\
	'n/promote/`zfs list -H -o name`/'\
	'n/rename/`zfs list -H -o name`/'\
	'n/list/`zfs list -H -o name`/'\
	'N/set/`zfs list -H -o name`/'\
	'N/get/`zfs list -H -o name`/'\
	'n/inherit/`zfs list -H -o name`/'\
	'n/mount/`zfs list -H -o name`/'\
	'n/unmount/`zfs list -H -o name`/'\
	'n/share/`zfs list -H -o name`/'\
	'n/unshare/`zfs list -H -o name`/'\
	'n/send/`zfs list -H -t snapshot -o name`/'\
	'n/receive/`zfs list -H -o name`/'

