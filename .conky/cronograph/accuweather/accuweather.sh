#!/bin/sh

## get the directory of this script
SCRIPT_PATH=`readlink -f $0`
SCRIPT_DIR=`dirname ${SCRIPT_PATH}`

## address (Accuweather url)
address="http://www.accuweather.com/en/cn/shanghai/106577/weather-forecast/106577"   # Shanghai


## function: test_image()
test_image() {
    case $1 in
        1)
            echo a
            ;;
        2|3)
            echo b 
            ;;
        4|5)
            echo c
            ;;
        6)
            echo d
            ;;
        7)
            echo e
            ;;
        8)
            echo f
            ;;
        11)
            echo 0
            ;;
        12)
            echo h
            ;;
        13|14)
            echo g
            ;;
        15)
            echo m
            ;;
        16|17)
            echo k
            ;;
        18)
            echo i
            ;;
        19)
            echo q
            ;;
        20|21|23)
            echo o
            ;;
        22)
            echo r
            ;;
        24|31)
            echo E
            ;;
        25)
            echo v
            ;;
        26)
            echo x
            ;;
        29)
            echo y
            ;;
        30)
            echo 5
            ;;
        32)
            echo 6
            ;;
        33)
            echo A
            ;;
        34|35)
            echo B
            ;;
        36|37)
            echo C
            ;;
        38)
            echo D
            ;;
        39|40)
            echo G
            ;;
        41|42)
            echo K
            ;;
        43|44)
            echo O
            ;;
        *)
            echo -
            ;;   
    esac
}


#kill -STOP $(pidof conky)
#killall wget

## urls and wget weather data
loc_id=$(echo ${address} | sed 's/\/weather-forecast.*$//' | sed 's/^.*\///')
last_number=$(echo ${address} | sed 's/^.*\///')

curr_addr="$(echo ${address} | sed 's/weather-forecast.*$//')current-weather/${last_number}"
wget -O ${SCRIPT_DIR}/curr_cond_raw "${curr_addr}"

addr1="$(echo ${address} | sed 's/weather-forecast.*$//')daily-weather-forecast/${last_number}"
wget -O ${SCRIPT_DIR}/tod_ton_raw "${addr1}"

addr2="${addr1}?day=6"
wget -O ${SCRIPT_DIR}/last_days_raw "${addr2}"

## current conditions
if [ -s ${SCRIPT_DIR}/curr_cond_raw ]; then
    sed -i '/detail-now/,/#details/!d' ${SCRIPT_DIR}/curr_cond_raw
    egrep -i '"cond"|icon i-|detail-tab-panel' ${SCRIPT_DIR}/curr_cond_raw > ${SCRIPT_DIR}/curr_cond
    sed -i -e 's/^.*detail-tab-panel //g' -e 's/^.*icon i-//g' -e 's/"><\/div>.*$//g' ${SCRIPT_DIR}/curr_cond
    sed -i -e 's/^.*"cond">//g' -e 's/&deg/\n/g' -e 's/<\/span>.*"temp">/\n/g' -e 's/<.*>//g' ${SCRIPT_DIR}/curr_cond
    sed -i -e 's/">//g' -e 's/-->//g' -e 's/\r$//g' -e 's/ i-alarm.*$//g' -e 's/-[a-z].*$//g' ${SCRIPT_DIR}/curr_cond
    image=$(sed -n 2p ${SCRIPT_DIR}/curr_cond)
    sed -i 2s/${image}/$(test_image ${image})/ ${SCRIPT_DIR}/curr_cond
fi

## First 5 days
if [ -s ${SCRIPT_DIR}/tod_ton_raw ]; then
    sed -i '/feed-tabs/,/\.feed-tabs/!d' ${SCRIPT_DIR}/tod_ton_raw
    egrep -i 'Early AM|Today|Tonight|Overnight|icon i-|cond|temp|Mon|Tue|Wed|Thu|Fri|Sat|Sun' ${SCRIPT_DIR}/tod_ton_raw > ${SCRIPT_DIR}/tod_ton
    sed -i -e 's/^.*#">//g' -e 's/^.*icon i-//g' -e 's/^.*cond">//g' -e 's/^.*temp">//g' ${SCRIPT_DIR}/tod_ton
    sed -i -e 's/Lo<\/span> /\n/g' -e 's/<\/a>.*$//g' -e 's/ "><.*$//g' -e 's/&#.*$//g' -e 's/teo//g' ${SCRIPT_DIR}/tod_ton
    sed -i -e 's/<span>.*$//g' -e 's/<\/span>//g' -e 's/\r$//g' -e 's/ i-alarm.*$//g' ${SCRIPT_DIR}/tod_ton
    sed -i -e 's/Early AM/EARLY AM/' -e 's/Today/TODAY/' -e 's/Tonight/TONIGHT/' -e 's/Overnight/OVERNIGHT/' -e 's/Mon/MON/' -e 's/Tue/TUE/' -e 's/Wed/WED/' -e 's/Thu/THU/' -e 's/Fri/FRI/' -e 's/Sat/SAT/' -e 's/Sun/SUN/' -e 's/-[a-z]*$//g' ${SCRIPT_DIR}/tod_ton
    time=$(sed -n 1p ${SCRIPT_DIR}/tod_ton)
    image=$(sed -n 2p ${SCRIPT_DIR}/tod_ton)
    #
    if [ "${time}" = "TODAY" ]; then
        sed -i 2s/${image}/$(test_image ${image})/ ${SCRIPT_DIR}/tod_ton
    elif [ "${time}" = "TONIGHT" -o "${time}" = "OVERNIGHT" -o "${time}" = "EARLY AM" ]; then
        sed -i 2s/${image}/$(test_image ${image})/ ${SCRIPT_DIR}/tod_ton
        sed -i 3a- ${SCRIPT_DIR}/tod_ton
    fi
    #
    for i in $(seq 7 5 22); do
        image=$(sed -n "${i}"p ${SCRIPT_DIR}/tod_ton)
        sed -i ${i}s/${image}/$(test_image ${image})/ ${SCRIPT_DIR}/tod_ton
    done
fi

## Next 5 days
if [ -s ${SCRIPT_DIR}/last_days_raw ]; then
    sed -i '/feed-tabs/,/\.feed-tabs/!d' ${SCRIPT_DIR}/last_days_raw
    egrep -i 'icon i-|cond|temp|Mon|Tue|Wed|Thu|Fri|Sat|Sun' ${SCRIPT_DIR}/last_days_raw > ${SCRIPT_DIR}/last_days
    sed -i -e 's/^.*#">//g' -e 's/^.*icon i-//g' -e 's/^.*cond">//g' -e 's/^.*temp">//g' ${SCRIPT_DIR}/last_days
    sed -i -e 's/Lo<\/span> /\n/g' -e 's/<\/a>.*$//g' -e 's/ "><.*$//g' -e 's/&#.*$//g' -e 's/teo//g' ${SCRIPT_DIR}/last_days
    sed -i -e 's/<span>.*$//g' -e 's/<\/span>//g' -e 's/\r$//g' -e 's/ i-alarm.*$//g' ${SCRIPT_DIR}/last_days
    sed -i -e 's/Mon/MON/' -e 's/Tue/TUE/' -e 's/Wed/WED/' -e 's/Thu/THU/' -e 's/Fri/FRI/' -e 's/Sat/SAT/' -e 's/Sun/SUN/' -e 's/-[a-z]*$//g' ${SCRIPT_DIR}/last_days
    #
    for i in $(seq 7 5 22); do
        image=$(sed -n "${i}"p ${SCRIPT_DIR}/last_days)
        sed -i ${i}s/${image}/$(test_image ${image})/ ${SCRIPT_DIR}/last_days
    done
fi

#kill -CONT $(pidof conky)

