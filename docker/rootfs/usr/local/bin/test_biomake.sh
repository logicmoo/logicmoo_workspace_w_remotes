# get the absolute path of the executable Symlink resolution: http://stackoverflow.com/a/697552/726581
SELF_PATH=$(cd -P -- "$(dirname -- "$0")" && pwd -P) && SELF_PATH=$SELF_PATH/$(basename -- "$0")
# resolve symlinks
while [[ -h $SELF_PATH ]]; do
    # 1) cd to directory of the symlink
    # 2) cd to the directory of where the symlink points
    # 3) get the pwd
    # 4) append the basename
    DIR=$(dirname -- "$SELF_PATH")
    SYM=$(readlink "$SELF_PATH")
    SELF_PATH=$(cd "$DIR" && cd "$(dirname -- "$SYM")" && pwd)/$(basename -- "$SYM")
done
PATH_TO_ME=`dirname $SELF_PATH`;


if [ -z "$BIOMAKE_PATH" ]; then
    BIOMAKE_PATH=$PATH_TO_ME/../prolog;
fi

$PATH_TO_ME/swipl_wrap -L0 -G0 -T0 -q -p library=$BIOMAKE_PATH  -g 'assert(biomake_prog("'$0'")),main,halt' -t halt -s $BIOMAKE_PATH/biomake/cli -- "$@"

