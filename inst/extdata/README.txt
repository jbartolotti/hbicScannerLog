Create a folder structure to keep reports data in, e.g.: 
~/R-Drive/NAME/3T_Scan_Logs
~/R-Drive/NAME/3T_Scan_Logs/Reports
~/R-Drive/NAME/3T_Scan_Logs/iLab_Reports

Make HBIC_Scan_Reporter.sh writeable and put it in e.g., /usr/local/bin

Create a cron job to regularly run the HBIC_Scan_Reporter.sh script that checks for new iLab Reports.
Edit your crontab with: ****
Add an entry as follows to check for ilab reports every hour: ***

