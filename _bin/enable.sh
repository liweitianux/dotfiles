#!/bin/sh
#
# Enalbe specified dotfiles by linking the dotfiles to home directory.
#
# Aaron LI
# Created: 2015-01-06
# Updated: 2015-01-06
#

usage() {
    echo "Usage:"
    echo "    `basename $0` [ -fhknv ] [ -H <home> ] <df1> ..."
    echo ""
    echo "    -h: show this help"
    echo "    -f: overwrite target links of already exists"
    echo "    -H: use the specified target home instead of $HOME"
    echo "    -k: keep the beginning underscore '_'"
    echo "    -n: dry-run"
    echo "    -v: verbose"
    echo "    df*: dotfiles or directory; NOTE: dot not use '.' or '..'"
}

enable_dotfile() {
    df="$1"
    home="$2"
    df_abs=`realpath ${df}`
    if [ "x${arg_keep_us}" = "xyes" ]; then
        df_dir=`dirname   ${df}`
        df_base=`basename ${df}`
    else
        df_dir=`dirname   ${df} | sed 's|^_|.|'`
        df_base=`basename ${df} | sed 's|^_|.|'`
    fi
    ( cd ${home}; curdir=`realpath .`; \
        df_rel=`${this_dir}/relpath.sh "${curdir}" "${df_abs}"`; \
        if [ ! -d "${df_dir}" ]; then mkdir -p "${df_dir}"; fi; \
        eval ${enable_cmd} "${df_rel}" "${df_dir}/${df_base}"; \
    )
}


this=`realpath $0`
this_dir=`dirname ${this}`

# default arguments
arg_force=no
arg_home="$HOME"
arg_keep_us=no
arg_dry=no
arg_verbose=no

# should NOT use "$@" here
args=`getopt fhH:knv $*`
if [ $? != 0 ]; then
    usage
    exit 1
fi
set -- ${args}
for i; do
    case "$i" in
        -h)
            usage
            exit 0;;
        -f)
            arg_force=yes
            shift;;
        -H)
            arg_home="$2"; shift;
            shift;;
        -k)
            arg_keep_us=yes
            shift;;
        -n)
            arg_dry=yes
            shift;;
        -v)
            arg_verbose=yes
            shift;;
        --)
            shift; break;;
    esac
done

if [ $# -eq 0 ]; then
    usage
    exit 2
fi

echo "force: ${arg_force}"
echo "target_home: ${arg_home}"
echo "keep_underscore: ${arg_keep_us}"
echo "dry_run: ${arg_dry}"
#echo "dotfiles: $@"

[ "x${arg_force}"   = "xyes" ] && arg_ln="-f"
[ "x${arg_verbose}" = "xyes" ] && arg_ln="${arg_ln} -v"
enable_cmd="ln -s ${arg_ln}"
if [ "x${arg_dry}" = "xyes" ]; then
    enable_cmd="echo DRY_RUN: ${enable_cmd}"
fi

for dotfile in $@; do
    for df in `find "${dotfile}" \( -type f -o -type l \)`; do
        # strip the beginning './'
        df=`echo "${df}" | sed 's|^\./||'`
        echo "enabling: '${df}'"
        enable_dotfile "${df}" "${arg_home}"
    done
done

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=sh: #
