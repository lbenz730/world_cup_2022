date_path=$(date +%Y-%m-%d)
/usr/local/bin/Rscript auto_update.R
rm Rplots.pdf
git add *
git commit -m "Tournament Update ${date_path}" 
git push -u origin main
