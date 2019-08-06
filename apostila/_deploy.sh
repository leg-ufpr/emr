#!/bin/sh

# Upload.
echo "------------------------------------------------------------------------"
echo "Uploading files to server.\n"
rsync -avzp \
      ./_book/ \
      --progress \
      --rsh="ssh -p$PATAXOP" \
      "$WEBLEG@$PATAXO:/home/$WEBLEG/ensino/EMR/apostila/"

# Vist the homepage.
echo "------------------------------------------------------------------------"
echo "Visiting the webpage.\n"
firefox http://web.leg.ufpr.br/ensino/EMR/apostila

exit 0
