#!/bin/bash

basedir="~/R-Drive/Bartolotti_J/3T_Scan_Logs"
ilabdir="($basedir)/iLab_Reports"
xnatdir_backup="/xnatdata/backup"
xnatdir_prearch="/xnatdata/prearch"
xnatdir_arch="/xnatdata/arch"
reportdir="($basedir)/Reports"
figuredir="($basedir)/Figures"
admin_email="jbartolotti2@kumc.edu"

#This didn't seem to work? I just put it into a /usr/local/bin/dicomcount_quick instead
#
#dicomcount_quick(){
#  start_dir="$1"
#  scans_dir=$(find $start_dir -type d -name SCANS)
#  for dir in $(find "$scans_dir" -mindepth 1 -maxdepth 1 -type d | while read dir; do echo "${dir##*/} $dir"; done | sort -n | cut -d' ' -f2-)
#  do
#    # Remove the trailing slash from dir
#    dir=${dir%/}
#    # Count the number of files with .dcm extension in dir and its subfolders
#    num=$(find "$dir" -type f -name '*.dcm' | wc -l)
#    # Print the folder name, plus that folder's subfolder, and the file count
#    for subdir in "$dir"/*/
#    do
#      # Remove the trailing slash from subdir
#      scandir=$(echo "$subdir" | awk -F'/' '{print $(NF-2)}')
#      # Print only the first subdirectory of dir (without file count)
#      printf "%s:%s," "$scandir" "$num"
#      # Break out of the loop after printing one subdirectory
#      break
#    done
#  done
#}


# First, check whether the Report folder on the R-Drive is readable.
if [ -r "$basedir" ]; then
  # Second, run the iLab processing R script to check for new iLab Reports
  # and process if one exists.

  # Check if $ilabdir/.processing exists
  if [ -f "$ilabdir/.processing" ]; then
    # Send an email alert to the admin that a job is in progress
    echo "A new iLab report can not be run because a job is already in progress. If you believe this to be an error, remove the file $ilabdir/.processing to resume." | mutt -s "iLabLog Processing" "$admin_email"
  else
    # Create the file $ilabdir/.processing
    touch "$ilabdir/.processing"

    # Use Rscript to run $reportdir/ilabprocess.R
    Rscript "$basedir/HBIC_Scan_Reporter.R" "$basedir" "$ilabdir" "$xnatdir_backup" "$xnatdir_prearch" "$xnatdir_arch" "$reportdir" "$admin_email"
  fi


else
  # If reportdir is not readable:

  # Check if ~/.iLabLog_Unreadable exists and is older than 24 hours
  if [ -f ~/.iLabLog_Unreadable ] && [ $(find ~/.iLabLog_Unreadable -mtime +1) ]; then
    # Delete ~/.iLabLog_Unreadable
    rm ~/.iLabLog_Unreadable
  fi

  # Check if ~/.iLabLog_Unreadable does not exist. Note that if it was
  # older than 24 hours, it was deleted above.
  if [ ! -f ~/.iLabLog_Unreadable ]; then
    # Send an email alert to admin_email
    echo "iLabLog processing cannot be run because $basedir is not readable. If this is a network drive, check that the drive is mounted." | mutt -s "ALERT: iLabLog Unreadable" "$admin_email"

    # Create a new file called ~/.iLabLog_Unreadable so that
    # no emails will be sent for the next 24 hours
    touch ~/.iLabLog_Unreadable
  fi
fi



  # If there is a file iLab_Reports/.processing
    # Send an email alert that a new iLab report can not be run because a job is already in progress. If you believe this to be an error, remove the file iLab_Reports/.processing
  # If not, then:
    # read the contents of the iLab_Reports folder and get dateranges
    # read the contents of the Reports folder and get dateranges
    # If there is a daterange in iLab_Reports not present in Reports, then:
      # Create a iLab_Reports/.processing file
      # Use Rscript to run new_scanlog(ilab_path, ilab_filename, xnat_path, config) for processing the iLab Report
        # config fields: admin_email, allowed_usage_type




