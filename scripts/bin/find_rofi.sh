!/bin/bash

cd ~/3ro_Carrera

selection=$(ls ~/3ro_Carrera | rofi -dmenu -p "Select Folder-File")
cd $selection

selection=$(ls $selection | rofi -dmenu -p "Select Folder-File")
cd $selection

selection=$(ls ~/3ro_Carrera | rofi -dmenu -p "Select Folder-File")
cd $selection



