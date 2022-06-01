SYSTEM_SETUP_FILES=()
# SYSTEM_SETUP_FILES+=(/usr/lib64/ros/setup.zsh)
SYSTEM_SETUP_FILES+=(/opt/ros/{foxy,noetic,melodic}/setup.zsh)

USER_SETUP_FILES=()
USER_SETUP_FILES=(~/${CATKIN_WS:-catkin_ws}/devel{,-isolated}/setup.zsh)

for setup_file in $SYSTEM_SETUP_FILES
do
    if [[ -r $setup_file ]]
    then
        source $setup_file
        break
    fi
done

for setup_file in $USER_SETUP_FILES
do
    if [[ -r $setup_file ]]
    then
        source $setup_file
        break
    fi
done
